# library(remotes)
# install_version("sf", "1.0-5")
# rsconnect::appDependencies()

library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(shiny)



# Load data ---------------------------------------------------------------

counties <- read_sf("shp/wi-counties-wgs.shp")
nkes <- read_sf("shp/nke-plans-2022-wgs.shp")

baseline <- read_sf("data/wav-station-locations.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
nutrient <- read_sf("data/nutrient-locations.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
thermistor <- read_sf("data/thermistor-locations.csv") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)

basemaps <- list(
  default = "OpenStreetMap",
  positron = "Grey Canvas",
  natgeo = "Nat Geo"
)

layers <- list(
  counties = "Counties/Regions",
  nkes = "Nine Key Elements Plans (<span style='color: blue;'>blue</span>)",
  baseline = "Baseline Stations (<span style='color: green;'>green</span>)",
  nutrient = "Nutrient Stations (<span style='color: orange;'>orange</span>)",
  thermistor = "Temperature Loggers (<span style='color: purple;'>purple</span>)"
)

ui <- fluidPage(
  title = "WAV Stream Monitoring Sites Map",
  
  tags$style(HTML("
    body {
      font-family: 'Lato', sans-serif;
    }
  ")),
  
  div(
    align = "center",
    style = "margin-top: 1em;",
    a(img(src = "wav-logo-color.png", height = "100px"), href = "https://wateractionvolunteers.org", target = "_blank")
  ),
  
  br(),
  
  h2("2021 WAV Monitoring Sites", align = "center"),
  
  br(),
  
  div(
    style = "max-width: 1000px; margin: auto; border: 1px solid grey;",
    leafletOutput("map", width = "100%", height = "800px")
  ),
  
  br(),
  
  div(
    style = "max-width: 1000px; margin: auto;",
    p("The sites on the map above show where in the state Water Action Volunteers made water quality monitoring measurements during the 2021 season."),
    p(strong("DNR Regions:"), "The Department of Natural Resources has grouped Wisconsin's 72 counties into five different regions, which are shown on the map as a light color fill."),
    p(strong("Nine Key Elements Plans:"), "These are long-term plans for specific watersheds that provide a framework for improving water quality in a holistic manner. The nine elements help assess the contributing causes and sources of nonpoint source pollution, involve key stakeholders and prioritize restoration and protection strategies to address water quality problems. Learn more about NKEs at the", HTML("<a href='https://dnr.wisconsin.gov/topic/Nonpoint/9keyElement' target='_blank'>Wisconsin DNR</a>.")),
    p(strong("Baseline monitoring:"), "Volunteers enter the WAV program by training to do baseline stream monitoring. Each year, baseline volunteers journey to their monitoring sites once per month from May to October to collect four baseline parameters: dissolved oxygen, instantaneous temperature, transparency and streamflow. During at least two of these months (May/June and September/October), volunteers also collect macroinvertebrates to calculate a biotic index score. Once per season, some advanced volunteers also conduct a habitat assessment. In 2020, volunteers collected this baseline data at 284 unique monitoring sites. In 2021, these data were collected at 279 unique sites."),
    p(strong("Nutrient monitoring:"), "After at least one season of baseline monitoring, some WAV volunteers will support special projects monitoring. Special projects monitoring is designed to either use the same methods as DNR professionals for data collection or to meet specific data needs. Recently these special projects have included monitoring with meters, aquatic invasive species monitoring, nutrient monitoring, and deploying continuous temperature monitors. Nutrient monitoring is the most widespread of the special projects. Volunteers sample for total phosphorus concentrations in rivers and streams. In some instances, volunteers also collect suspended solids samples and/or nitrogen panels."),
    p(strong("Temperature loggers:"), "Across the state there are a number of automatic, deployed temperature loggers that continuously monitor water temperature in streams. This data can be useful for understanding seasonal stream dynamics, as lower temperatures can indicate higher flow rates, more oxygen-rich water, and overall healther stream systems."),
    p(strong("More information:"), "Visit the Water Action Volunteers web page at", HTML("<a href='https://wateractionvolunteers.org' target='_blank'>wateractionvolunteers.org</a>."))
  ),
  
  br(),
  
  p(
    style = "color: grey; font-size: smaller;",
    align = "center",
    em(paste("Last updated:", format(file.info(".")$mtime, "%Y-%m-%d")))
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = basemaps$default) %>%
      addProviderTiles(providers$CartoDB.Positron, group = basemaps$positron) %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, group = basemaps$natgeo) %>%
      addMapPane("counties", 410) %>%
      addMapPane("nkes", 420) %>%
      addMapPane("points", 430) %>%
      addPolygons(
        data = counties,
        group = layers$counties,
        label = ~ lapply(paste0("<b>", COUNTY_NAM, " County</b><br>", DNR_REGION), HTML),
        fillOpacity = 0.1,
        color = "grey",
        opacity = 0.5,
        fillColor = ~ colorFactor("Dark2", counties$DNR_REGION)(DNR_REGION),
        weight = 1,
        options = pathOptions(pane = "counties")
      ) %>%
      addPolygons(
        data = nkes,
        group = layers$nkes,
        label = ~ lapply(paste0("<b>", PLAN_NAME, "</b><br>Ends: ", END_DATE, "<br>Objective: ", OBJECTIVE_), HTML),
        popup = ~ lapply(paste0("<b>", PLAN_NAME, "</b><br>Ends: ", END_DATE, "<br>Objective: ", OBJECTIVE_), HTML),
        color = "blue",
        weight = 1,
        # fillColor = ~ colorBin("RdYlBu", nkes$END_YEAR)(END_YEAR),
        fillColor = "lightblue",
        options = pathOptions(pane = "nkes"),
        labelOptions = labelOptions(style = list("width" = "300px", "white-space" = "normal"))
      ) %>%
      addCircleMarkers(
        data = baseline,
        group = layers$baseline,
        label = ~ lapply(paste0("<b>Baseline Monitoring Stations</b><br>", StationID, ": ", StationName), HTML),
        popup = ~ lapply(paste0("<b>Baseline Monitoring Stations</b><br>", StationID, ": ", StationName), HTML),
        radius = 2.5,
        color = "black",
        weight = 0.5,
        fillColor = "green",
        fillOpacity = 0.75,
        options = markerOptions(pane = "points", sticky = F)
      ) %>%
      addCircleMarkers(
        data = nutrient,
        group = layers$nutrient,
        label = ~ lapply(paste0("<b>Nutrient Monitoring Station</b><br>", StationID, ": ", StationName), HTML),
        popup = ~ lapply(paste0("<b>Nutrient Monitoring Station</b><br>", StationID, ": ", StationName), HTML),
        radius = 2.5,
        color = "black",
        weight = 0.5,
        fillColor = "orange",
        fillOpacity = 0.75,
        options = markerOptions(pane = "points", sticky = F)
      ) %>%
      addCircleMarkers(
        data = thermistor,
        group = layers$thermistor,
        label = ~ lapply(paste0("<b>Thermistor Station</b><br>", StationID, ":</b> ", StationName), HTML),
        popup = ~ lapply(paste0("<b>Thermistor Station</b><br>", StationID, ":</b> ", StationName), HTML),
        radius = 2.5,
        color = "black",
        weight = 0.5,
        fillColor = "purple",
        fillOpacity = 0.75,
        options = markerOptions(pane = "points")
      ) %>%
      addLayersControl(
        baseGroups = unlist(basemaps, use.names = F),
        overlayGroups = unlist(layers, use.names = F),
        options = layersControlOptions(collapsed = F)
      ) %>%
      htmlwidgets::onRender("
        function() {
          $('.leaflet-control-layers-list').prepend('<b>Basemap:</b>');
          $('.leaflet-control-layers-overlays').prepend('<b>Map Layers:</b>');
        }
      ")
  })
}

shinyApp(ui, server)
