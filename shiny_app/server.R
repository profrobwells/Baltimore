## load libraries
library(tidyverse)
library(tidycensus)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(janitor)
library(sf)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(scales)
#install.packages('shinyjs')
library(shinyjs)
library(htmlwidgets)


race_table <- readRDS("data/balt_race_percentages_join.rds") %>%
  filter(!is.na(percent_white))

redlining <- readRDS("data/d_regions.rds") %>%
  filter(gisjoin != "G2400050400800" &
         gisjoin != "G2400050401505" &
         gisjoin != "G2400050400900" &
         gisjoin != "G2400050401504")

percap_income_table <- readRDS("data/baltcity_percap_income.rds")

race_and_income_table <- readRDS("data/race_and_income_combined.rds") 
  
#redlining_pal <- colorQuantile("red")

## maybe session later
server <- function(input, output, session){
  
  showLog()
  logjs("App started")
  
  
    group_to_map <- reactive({
      input$race
    })
    
    output$mymap <- renderLeaflet({
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng=-76.6122, lat=39.2904, zoom=12) 
      
    })
    
    observeEvent(input$race, {
      
      mymap <- leafletProxy("mymap") %>%
        clearControls() 
        
      
      if (input$race == 'Change in Per Capita Income') {
       # Color and palette are treated specially in the "superzip" case, because the values are categorical instead of continuous.
        
        shinyjs::logjs("hello")
        
        #getElement(group_to_map(), 'Change in Per Capita Income')
      
        pal <- colorQuantile("YlGnBu", domain=race_and_income_table[[group_to_map()]], reverse = TRUE, 10)
        
          leafletProxy("mymap") %>%
          addPolygons(
            data = race_and_income_table,
            color = ~pal(race_and_income_table[[group_to_map()]]),
            weight = 2.5,
            smoothFactor = 0.2,
            fillOpacity = 0.6,
            label = paste0(" Per capita income change is: ",(scales::dollar(race_and_income_table$'Change in Per Capita Income')))
          ) %>%
          clearControls %>%
          addPolylines(
              group = "Neighborhood",
              data = percap_income_table,
              color = "Gray",
              weight = 1,
              label = percap_income_table$community_statistical_area_2020,
              fillOpacity = 0.75,
              #options = layersControlOptions(collapsed = FALSE)
            ) %>%
          addPolylines(
              group = "Redlining Zones",
              data = redlining,
              color = "Red",
              weight = 5,
              smoothFactor = 1
            ) %>%
          addLegend(
            pal = pal,
            position = "bottomleft",
            values = race_and_income_table[[group_to_map()]],
            title =  group_to_map() 
          ) %>%
          addLegend(
            position = "bottomleft", 
            colors= "red", 
            labels=c("Redlined areas"), 
            title="Historic Redlining"
          )
          
        
       } else {

        
      shinyjs::logjs("everything but income") 
      
      race_pal <- colorBin("YlGnBu", domain=race_and_income_table[[group_to_map()]], 5)
       
      
      leafletProxy("mymap") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(#group = "Black Population",
          data = race_and_income_table,
          color = ~race_pal(race_and_income_table[[group_to_map()]]),
          weight = 2.5,
          smoothFactor = 0.2,
          fillOpacity = 0.7,
          ### add percent sign 
          label = paste0(input$race, " is", " ", race_and_income_table[[group_to_map()]],"%", " for", " ",race_and_income_table$Neighborhood, " |", " Per capita income change is: ",(scales::dollar(race_and_income_table$'Change in Per Capita Income')))
        ) %>%
        addPolylines(
          group = "Neighborhood",
          data = percap_income_table,
          color = "Gray",
          weight = 1,
          label = percap_income_table$community_statistical_area_2020,
          fillOpacity = 0.75,
          options = layersControlOptions(collapsed = FALSE)
        ) %>% 
        addPolylines(
          group = "Redlining Zones",
          data = redlining,
          color = "Red",
          weight = 5,
          smoothFactor = 1
        ) %>%
        #clearShapes() %>%
        #clearControls() %>%
      addLegend(
        pal = race_pal,
        position = "bottomleft",
        values = race_and_income_table[[group_to_map()]],
        title =  group_to_map()
        ) %>%
        addLegend(
          position = "bottomleft", 
          colors= "red", 
          labels=c("Redlined areas"), 
          title="Historic Redlining"
          
        ) %>%
      addSearchFeatures(targetGroups = c("Neighborhood"),
                        options = searchFeaturesOptions(zoom = 14)
                          ) 
      
       }
      
                 
    })
    
    ## Data Explorer ###
    
    output$df <- DT::renderDataTable({
      df <- race_and_income_table  %>%
        st_drop_geometry(race_and_income_table) %>%
        ## add 2020 per capita income
        #dplyr::select(-geoid, - name, -state, -census_tract, -object_id) %>%
        dplyr::select("Neighborhood", total_population, "Change in Per Capita Income", "Percent White", "Percent Black", "Percent Hispanic/Latino", "Percent Asian", "Census Tract", -geoid, - name, -state, -census_tract, -object_id) %>%
        rename(
          'Total Population' = total_population,
        )
         # "Tract"= tract, "Neighborhood", "Percent White", "Percent Black", "Percent Hispanic/Latino", "Percent Asian", "Change in Per Capita Income") #%>%
        # rename(
        #   'Census Tract'= tract,
        #   'Neighborhood' = neighborhood,
        #   "Percent White" = percent_white,
        #   "Percent Black" = percent_black,
        #   "ercent White" = percent_hisp_lat,
        #   "Percent Asian" = percent_asian,
        #   "Change in Per Capita Income" = diff_2020_2010
        # )

      
      action <- DT::dataTableAjax(session, df, outputId = "Data explorer")
      DT::datatable(df, escape = FALSE)
    
    }) 
}
   



