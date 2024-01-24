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
library(jsonlite)
library(stringr)

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

##Zuzia--------------------
df_spotify_maps_zuzia<-readRDS("ZuziaDane/spotify_maps_zuzia.rds")
df_spotify_maps_gleb<-readRDS("ZuziaDane/spotify_maps_gleb.rds")
df_spotify_maps_martyna<-readRDS("ZuziaDane/spotify_maps_martyna.rds")

df<-df_spotify_maps_zuzia
##----------------------------
##Gleb------------
shapefilePath <- "GlebDane/dzielnice_Warszawy/dzielnice_Warszawy.shp"
gleb_districts<-read.csv("GlebDane/songs_with_districts_gleb.csv")
zuzia_districts<-read.csv("GlebDane/songs_with_districts_zuzia.csv")
martyna_districts<-read.csv("GlebDane/songs_with_districts_martyna.csv")
##----------------------

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "cyborg"),
  navbarPage(
    "My Google Maps and Spotify",
    ##Zuzia---------------------
    tabPanel("Map and Data",
             sidebarLayout(
               sidebarPanel(
                 dateInput("dateInput", "Select a date:", value = Sys.Date(), 
                           min = min(df$startTimestamp), max = max(df$startTimestamp)),
                 selectInput("personInput", "Select a person:", 
                             choices = c("Hleb", "Zuzia", "Martyna"), multiple = FALSE),
                 style = "width: 250px;"
               ),
               mainPanel(
                 leafletOutput("map"),
                 DTOutput("spotifyTable"),
                 DTOutput("summaryTable")
               )
             )
    ),
    ##---------------------

    ##Gleb-----------------
    tabPanel("Districts of Warsaw",
             sidebarLayout(
               sidebarPanel(

                 selectInput("personInputGleb", "Select a person:",
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
    ##---------------------

    navbarMenu("Links",
               tabPanel("Google Maps", icon = icon("map-marked-alt"), 
                        tags$a(href = "https://www.google.com/maps", target = "_blank", "Go to Google Maps")),
               tabPanel("Spotify", icon = icon("spotify"), 
                        tags$a(href = "https://www.spotify.com", target = "_blank", "Go to Spotify"))
    )
  )
)

server <- function(input, output) {
  ##Zuzia----------------
  filteredDateSpotifyMaps<-reactive({
    if(input$personInput == "Hleb"){
      dfSpotifyMaps<-df_spotify_maps_gleb
    }
    if(input$personInput == "Martyna"){
      dfSpotifyMaps<-df_spotify_maps_martyna
    }
    if(input$personInput == "Zuzia"){
      dfSpotifyMaps<-df_spotify_maps_zuzia
    }
    
    dfSpotifyMaps<-dfSpotifyMaps %>% filter(!is.na(startTimestamp), as.Date(startTimestamp) == input$dateInput)
    return(dfSpotifyMaps)
  })

  output$map <- renderLeaflet({
    df<-filteredDateSpotifyMaps()
    
    m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark)
    
    num_routes <- length(df$osrm_routes)
    colors <- colorRampPalette(brewer.pal(min(num_routes, 9), "Set1"))(num_routes)
    
    for (i in 1:num_routes) {
      route_geom <- df$osrm_routes[[i]]
      start_time_formatted <- format(as.POSIXct(df$startTimestamp[i]), format = "%H:%M")
      end_time_formatted <- format(as.POSIXct(df$endTimestamp[i]), format = "%H:%M")
      
      if (!is.na(df$latitude[i]) && !is.na(df$longitude[i]) && df$latitude[i] != 0 && df$longitude[i] != 0) {
        m <- m %>% addMarkers(lng = df$longitude[i], lat = df$latitude[i], 
                              popup = df$startName[i])
      }
      
      if (!is.null(route_geom)) {
        popup_content <- paste("Activity Type: ", df$activityType[i], "<br>",
                               "Start: ", start_time_formatted, "<br>",
                               "End: ", end_time_formatted, "<br>",
                               sep = "")
        
        if (i <= length(df$songs_df)) {
          songs_info <- df$songs_df[[i]]
          if (!is.null(songs_info) && nrow(songs_info) > 0) {
            songs_text <- apply(songs_info, 1, function(row) {
              paste(row['artistName'], "-", row['trackName'])
            })
            songs_popup <- paste("Songs:<br>", paste(songs_text, collapse = "<br>"), sep = "")
            popup_content <- paste(popup_content, songs_popup, sep = "<br>")
          }
        }
        m <- m %>% addPolylines(data = route_geom, color = colors[i], popup = popup_content)
      }
    }
    m
  })
  ##-----------------------------
  
  ##Gleb-------------------------
  districts <- st_read(shapefilePath)
  
  districts_wgs84 <- st_transform(districts, crs = 4326)
  
  
  filteredDataMostPopularSong <- reactive({
    
    
    if(input$personInputGleb == "Hleb"){
      final_final<-gleb_districts
    }
    if(input$personInputGleb == "Martyna"){
      final_final<-martyna_districts
    }
    if(input$personInputGleb == "Zuzia"){
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
    if(input$personInputGleb == "Hleb"){
      df<-gleb_districts
      
    }
    if(input$personInputGleb == "Martyna"){
      df<-martyna_districts
      
    }
    if(input$personInputGleb == "Zuzia"){
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
  ##----------------------
}

shinyApp(ui = ui, server = server)
