library(shiny)
library(dplyr)
library(lubridate)
library(geosphere)
library(leaflet)
library(osrm)
library(sf)
library(tidyr)
library(RColorBrewer)
library(DT)
library(bslib)
library(shinycssloaders)
library(plotly)
library(shinyjs)

custom_popup_style <- "
<style>
  .leaflet-popup-content {
    color: #fff;
    background-color: #333;
    padding: 5px;
    border-radius: 5px;
  }
  .leaflet-popup-content-wrapper{
    background-color: #333;
    width: 150px;
    text-align: center;
  }

</style>
"
theme_color <- "minty"


shapefilePath <- "../dzielnice_Warszawy/dzielnice_Warszawy.shp"
gleb_districts<-read.csv("songs_with_districts_gleb.csv")
zuzia_districts<-read.csv("songs_with_districts_zuzia.csv")
martyna_districts<-read.csv("songs_with_districts_martyna.csv")


ui <- fluidPage(

  
  theme = bs_theme(bootswatch = theme_color),
  navbarPage(
    "My Google Maps and Spotify",

    tabPanel("Districts of Warsaw",
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("personInput", "Select a person:", 
                             choices = c( "Zuzia","Hleb", "Martyna"), multiple = FALSE),
                 dateRangeInput("dateInput", "Select a date:",
                                                   start = max(min(gleb_districts$endTime),min(zuzia_districts$endTime),min(martyna_districts$endTime)),
                                                   end=min(max(gleb_districts$endTime),max(zuzia_districts$endTime),max(martyna_districts$endTime))
                 ),
                 
                 
                 
                 style = "width: 250px;"
               ),
               mainPanel(
                 leafletOutput("map_dzielnice"),
                 plotlyOutput("districtTime")
               )
             )
    ),
    navbarMenu("Links",
               tabPanel("Google Maps", icon = icon("map-marked-alt"),
                        tags$a(href = "https://www.google.com/maps", target = "_blank", "Go to Google Maps")),
               tabPanel("Spotify", icon = icon("spotify"),
                        tags$a(href = "https://www.spotify.com", target = "_blank", "Go to Spotify"))
    )
  )
)



server <- function(input, output) {
  


  districts <- st_read(shapefilePath)
  
  districts_wgs84 <- st_transform(districts, crs = 4326)
  
  
  filteredDataMostPopularSong <- reactive({
    
    
    if(input$personInput == "Hleb"){
      final_final<-gleb_districts
    }
    if(input$personInput == "Martyna"){
      final_final<-martyna_districts
    }
    if(input$personInput == "Zuzia"){
      final_final<-zuzia_districts
    }
    

    

    
    final_final_filtered <- final_final %>%
      
      filter(between(as.Date(endTime), as.Date(input$dateInput[1], format = "%Y-%m-%d"), as.Date(input$dateInput[2], format = "%Y-%m-%d"))) %>%
      select(artistName, trackName, district) %>%
      group_by(district, artistName, trackName) %>%
      summarise(playCount = n(), .groups = 'drop') %>%
      arrange(district, desc(playCount)) %>%
      group_by(district) %>%
      slice(1)
    

    return(final_final_filtered)
  })
  

  output$map_dzielnice <- renderLeaflet({

    songs<-filteredDataMostPopularSong()
    
    map<- merge(districts_wgs84,songs,by.x='nazwa_dzie',by.y='district',
                all.x = TRUE, all.y = FALSE)
    print(map)
    
    leaflet(map) %>% 
      addTiles() %>% 
      addPolygons( fillOpacity = 0.5, smoothFactor = 0.5,
                   
                   popup=ifelse(is.na(map$artistName), "", paste(custom_popup_style,"<b>",map$nazwa_dzie,"</br>",
                                                                 map$artistName,"-</br>",map$trackName,"</b>")),
                   popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
                   col=ifelse(is.na(map$artistName), "grey", "blue")
      )%>% addProviderTiles(providers$Stadia.AlidadeSmoothDark)
  })
  
  filteredDataDistrictsTime <- reactive({
    if(input$personInput == "Hleb"){
      df<-gleb_districts
     
    }
    if(input$personInput == "Martyna"){
      df<-martyna_districts
      
    }
    if(input$personInput == "Zuzia"){
      df<-zuzia_districts
      
    }
    
 
    df$day <- as.Date(df$endTime,format = "%Y-%m-%d")
    

    result_df <- df %>%
      
      filter(between(as.Date(endTime), as.Date(input$dateInput[1], format = "%Y-%m-%d"), as.Date(input$dateInput[2], format = "%Y-%m-%d"))) %>%
      
      group_by(day, district) %>%
      summarise(total_ms_played = sum(msPlayed)) %>%
      mutate(total_seconds_played = total_ms_played / 1000/60) %>%
      select(day, district, total_seconds_played)%>%ungroup()

   return(result_df)
  })
  
  output$districtTime <- renderPlotly({
    start_date <- input$dateInput[1]
    end_date <- input$dateInput[2]

    
    
 
    ggplot(filteredDataDistrictsTime(), aes(x = day, y = total_seconds_played, color = district)) +
      geom_line(size = 1) + 
      geom_point() +        
      labs(title = "Interactive Plot", 
           x = "Date", 
           y = "Second Played in each district") +
      theme_minimal() +     
      theme(plot.background = element_rect(fill = "grey18", color = "grey18"),  
            panel.background = element_rect(fill = "grey18"),
            text = element_text(color = "white")
            ) 
    



  })

}
shinyApp(ui = ui, server = server)