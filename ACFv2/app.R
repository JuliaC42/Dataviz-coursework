## app to display how quickly questions were answered correctly in ACF Regionals 2019

## load packages
library(shiny)
library(tidyverse)
library(viridis)
library(DT)

source("helpers.R")

all_correct <- readRDS("data/all_correct_tossups.rds")
correct_excluding_bounceback <- all_correct %>% 
  filter(is.na(bounceback))
bar_data <- readRDS("data/corr2.rds")

all_subcategories <- sort(unique(all_correct$subcategory))

# Define UI for application 
ui <- fluidPage ( 
  
  # Application title
  titlePanel("ACF Regionals, January 2019"),
  
  
  checkboxInput("checkbox", 
                label = "include bouncebacks (only applies to boxplot and table)",
                value = TRUE),
  
  selectInput("category",
              label = "choose categories",
              choices = list(
                "Science", "RMPSS", "Other", "Literature",
                "History", "Fine Arts"),
              selected =  NULL #c("Science", "RMPSS", "Other", "Literature",
                           #"History", "Fine Arts"),
              ,multiple = TRUE
              , selectize = TRUE),
  
      
  
  
    tabsetPanel(
      tabPanel("Boxplot", plotOutput("boxplot")),
      tabPanel("Table", dataTableOutput("table")),
      tabPanel("Barchart", plotOutput("barchart"))
  
  )
)




  
  

# server logic
    server <- function(input, output, session) {
    output$boxplot <- renderPlot({
      #req(input$categories)
      #data <- all_correct   #dataset %>% 
        #filter(subcategory = input$cats)
      if (input$checkbox == TRUE) {
        all_correct %>% 
          filter(category %in% input$category) %>% 
          mybox2()
      } else {
        correct_excluding_bounceback %>% 
          filter(category %in% input$category) %>% 
          mybox2()  
      }
      
  })
    
    output$table <- renderDataTable({
      if (input$checkbox == TRUE) {
        all_correct %>% 
          filter(category %in% input$category) %>%
          mytab()
      } else {
        correct_excluding_bounceback %>% 
          filter(category %in% input$category) %>% 
          mytab()
      }  
    })
    
    output$barchart <- renderPlot({
      bar_data %>% 
          filter(category %in% input$category) %>%
          mybar()
      
    })
}

# Run app ----
shinyApp(ui, server)