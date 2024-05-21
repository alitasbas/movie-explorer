# Dependencies

library(shinythemes)
library(shiny)
library(DT)
library(plotly)
library(magrittr)
library(stringr)
library(dplyr)
library(rlang)

set.seed(295)

tmdb <- read.csv("tmdb_5000_movies.csv")
tmdb <- subset(tmdb, select = -c(2, 3, 4, 5, 6, 8, 10, 11, 15, 16, 17, 18), revenue != 0)

index <- sample(nrow(tmdb), 1750)
tmdb <- tmdb[index,]
tmdb <- tmdb %>% mutate(release_date = as.integer(format(as.Date(tmdb$release_date), "%Y")),
                ln_budget = log(budget),
                ln_popularity = log(popularity),
                ln_vote_count = log(vote_count),
                sqrt_revenue = sqrt(revenue))

data <- read.csv("netflix_titles.csv")

data <- data %>% 
  filter(release_year > 1960) %>% 
  mutate(duration_cat = str_extract_all(duration, "\\d+")) %>% 
  mutate(duration_cat = case_when(
                            as.integer(duration_cat) < 3 ~ "short series",
                            as.integer(duration_cat) < 8 ~ "avg series",
                            as.integer(duration_cat) < 20 ~ "long series",
                            as.integer(duration_cat) < 70 ~ "short movie",
                            as.integer(duration_cat) < 105 ~ "avg movie",
                            as.integer(duration_cat) < 130 ~ "long movie",
                            as.integer(duration_cat) < 320 ~ "very long movie"
                            ))

data <- subset(data, !is.na(duration_cat), select=-`duration`)
data <- data[data$rating!="", ]

ui <- fluidPage(

  theme = shinytheme("darkly"), # paper, darkly, slate
  titlePanel("Data Set Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("Input Fields"),
      
      uiOutput("ui")
      
    ),
    
    mainPanel(
      
      tabsetPanel(id = "tab_selected",
        
        tabPanel("Netflix", value = "tab1", plotlyOutput("histogram")),
        
        tabPanel("Visuals/Aggregation", value = "tab2", plotOutput("visual")),
        
        tabPanel("Models", value = "tab3", plotOutput("model")),
        
        tabPanel("TMDB", value = "tab4", plotlyOutput("map"))
      )
      
    )
    
  )
  
)


server <- function(input, output, session) {
  
  selected_graph <- reactive({
    
    input$graph_type
  })
  
  x_var <- reactive({
    
    tmdb[[input$x]]
  })
  
  y_var <- reactive({
    
    tmdb[[input$y]]
  })
  
  dim3 <- reactive({
    
    tmdb[[input$aggregation]]
  })
  # plot <- reactive({
  #   
  #   
  # })
  
  selected_filter <- reactive({
    
    input$filter1
  })
  
  df <- reactive({
    
    d <- data[[input$filter1]]
    
    if (input$filter1 %in% c("type", "rating", "duration_cat")) {
      
      data[data[[input$filter1]] %in% input$checkbox, ]
    } else {
      
      data %>% mutate(contains = grepl(paste0("\\b", input$search, "\\b"), d, ignore.case = TRUE)) %>% 
        filter(contains)
    }
  })
  
  observe({
    if (input$tab_selected == 'tab1') {
    
      output$ui <- renderUI({
      
        tagList(
          selectInput("filter1", "Filter", choices=colnames(data)[c(2, 4, 5, 9, 12)], selected=selected_filter()),
          
          if (!is.null(input$filter1)) {
            if (isolate(input$filter1) %in% c("cast", "director")) {
              
              textInput("search", "Search a Name")
            } else {
              
              checkboxGroupInput("checkbox", "Select values", choices=sort(unique(data[[isolate(input$filter1)]]), decreasing=T))
            }
          }
        )
        

      })
    } else if (input$tab_selected == "tab4") {
      
      output$ui <- renderUI({
        
        tagList(
          selectInput("graph_type", "Select Graph", choices=c("Time Series", "Scatter Plot", ""), selected=selected_graph()),
          
          if(!is.null(input$graph_type)) {
            
            if(input$graph_type == "Time Series") {
              selectInput("variable", "Variable of Interest", choices=colnames(tmdb)[c(6, 7, 9:12)])
              
            } else if(input$graph_type == "Scatter Plot") {
              
              updateSelectInput(session, "aggregation", label = "Select Third Dim", choices=colnames(tmdb)[c(6, 7, 9:12)])
              
              c(selectInput("x", "X variable", choices=colnames(tmdb)[c(6, 7, 9:12)]),
              selectInput("y", "Y variable", choices=colnames(tmdb)[c(6, 7, 9:12)])
              )
            }
          } else {
            
            
          },
          selectInput("aggregation", "Method of Aggregation", choices=c("min", "mean", "median", "max", "total"))
          
        )
      })
    } else {
      
      output$ui <- renderUI({
        
        helpText("Welcome to Netflix Explorer")
        })
    }
  })
  
  
  output$histogram <- renderPlotly({
    
    ggplot(df(), aes(x=release_year)) +
      geom_histogram(bins = 35)
  })
  
  output$map <- renderPlotly({
    
    if (input$graph_type == "Scatter Plot") {
      
      ggplot(mapping = aes(x=x_var(), y=y_var(), color=dim3())) +
        geom_point(alpha=0.4) +
        labs(x = input$x, y = input$y, color = input$aggregation) +
        scale_color_continuous(type = "viridis")
    } else if (input$graph_type == "Time Series") {
      
      var <- syms(input$variable)
      
      ts <-  case_when(
        
        input$aggregation == "mean" ~ tmdb %>% 
          group_by(release_date) %>% 
          summarize(value = mean(!!!var, na.rm=T)),
        
        input$aggregation == "min" ~ tmdb %>% 
          group_by(release_date) %>% 
          summarize(value = min(!!!var, na.rm=T)),
        
        input$aggregation == "median" ~ tmdb %>% 
          group_by(release_date) %>% 
          summarize(value = median(!!!var, na.rm=T)),
        
        input$aggregation == "max" ~ tmdb %>% 
          group_by(release_date) %>% 
          summarize(value = max(!!!var, na.rm=T)),
        
        input$aggregation == "total" ~ tmdb %>% 
          group_by(release_date) %>% 
          summarize(value = sum(!!!var, na.rm=T)),
      )
      ggplot(ts, aes(x=release_date, y=value)) +
        geom_line() +
        labs(y=input$variable)
    }
  })
  
}


shinyApp(ui, server)