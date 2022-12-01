library(tidyverse)
library(tidycensus)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(janitor)
library(lubridate)
library(leaflet)
library(shinyjs)
library(leaflet.extras)


## putting data here too?
race_table <- readRDS("data/balt_race_percentages_join.rds")
percap_income_table <- readRDS("data/baltcity_percap_income.rds")

redlining <- readRDS("data/d_regions.rds") %>%
  filter(gisjoin != "G2400050400800" &
           gisjoin != "G2400050401505")

race_and_income_table <- readRDS("data/race_and_income_combined.rds") 


## defining vars for dropdowns
# Choices for drop-downs
vars <- c(
    "Percent White" = "Percent White",
    "Percent Black" = "Percent Black",
    "Percent Hispanic/Latino" = "Percent Hispanic/Latino",
    "Percent Asian" = "Percent Asian",
    "Change in Per Capita Income" = "Change in Per Capita Income"
)



ui <- fluidPage(
  
  useShinyjs(),
  
  # App title ----
  #titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  navbarPage("Baltimore Reporting", id="nav",
        
    tabPanel("Interactive map",
                      
  # Main panel for displaying outputs ----

    mainPanel(
      div(class="outer",
          
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),
      
      
      leafletOutput("mymap", height="100%", width="100%" ),
      actionButton("reset_button", "Reset view"),
      
      absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                    draggable = FALSE, top = 20, left = "auto", right = 20, bottom = "auto",
                    width = 350, height = "auto",
                    
                    h4("Explore Income and Demographics"),
                    
                    #selectInput("race", "Race", vars)
                    
                    pickerInput(
                      inputId = "race",
                      choices = vars,
                      options = list(
                        `actions-box` = TRUE,
                        size = 9,
                        `selected-text-format` = "count > 3"
                        
                      )
                    )
      )
      
    ),
        )
  
    ),
  
  tabPanel("Data explorer",
           DT::dataTableOutput("df")
  ),
  )
)