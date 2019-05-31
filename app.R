
library(shiny)
library(sp)
library(raster)
library(viridisLite)
library(dplyr)
library(rgeos)
library(leaflet)

load("biomes_spdf.RData")

biome_names <- read.csv("biome_names.csv") %>% 
  mutate(biome = factor(biome, levels = biome)) %>% 
  mutate(full = paste0("(", abbrev, ") ", biome))

ui <- fluidPage(
  leafletOutput("mymap", height = 475),
  p(),
  fluidRow(
    column(3, numericInput("lat", "Latitude", 53.28, min = -180, max = 180)),
    column(3, numericInput("lon", "Longitude", -114.41, min = -180, max = 180)),
    column(3, numericInput("rad", "Focal Radius (km)", 200, min = 0, max = 1000))
  ),
  fluidRow(
    column(1, actionButton("update", "Update"))
  )
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$update, {
    cbind(input$lon, input$lat)
  }, ignoreNULL = FALSE)
  
  radius <- eventReactive(input$update, {
    input$rad
  }, ignoreNULL = FALSE)
  
  polys <- eventReactive(input$update, {
    pts <- data.frame(Lon = input$lon, Lat = input$lat)
    
    coordinates(pts) <- c("Lon", "Lat")
    proj4string(pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    biomes_buffer <- buffer(pts, width = radius() * 1000)
    z <- as(extent(biomes_buffer), "SpatialPolygons")
    proj4string(z) <- proj4string(biomes_spdf)
    biome_crop <- gIntersection(biomes_spdf, z, byid = TRUE)
    biome_crop$id <- as.integer(sapply(row.names(biome_crop), function(x) strsplit(x, " ")[[1]][1]))
    biome_crop@data <- left_join(biome_crop@data, biome_names, by = "id")
    biome_crop$fill <- viridis(max(length(biome_crop), 3))[1:length(biome_crop)]
    return(biome_crop)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    biome_poly <- polys()
    
    leaflet() %>%
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = points()) %>% 
      addPolygons(data = biome_poly,
                  label = ~full,
                  fillColor = ~fill,
                  color = "black",
                  weight = 1,
                  opacity = 0.7) %>% 
      addLegend(data = biome_poly,
                "bottomright",
                colors = ~fill,
                labels = ~abbrev,
                title = "Biome",
                opacity = 0.7)
  })
}

shinyApp(ui, server)

