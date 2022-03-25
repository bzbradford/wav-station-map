# library(remotes)
# install_version("sf", "1.0-5")
# rsconnect::appDependencies()

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(shiny)
library(shinyBS)
library(DT)
library(shinyjs)


# Load data ---------------------------------------------------------------

# shapes
counties <- read_sf("shp/wi-counties.shp")
nkes <- read_sf("shp/nke-plans-2022.shp")
huc8 <- read_sf("shp/wi-huc-8.shp")
huc10 <- read_sf("shp/wi-huc-10.shp")
huc12 <- read_sf("shp/wi-huc-12.shp")


# points
baseline <- read_sf("data/baseline-locations.csv")
nutrient <- read_sf("data/nutrient-locations.csv")
thermistor <- read_sf("data/thermistor-locations.csv")
baseline.sf <- st_as_sf(baseline, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
nutrient.sf <- st_as_sf(nutrient, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
thermistor.sf <- st_as_sf(thermistor, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)



# UI ----------------------------------------------------------------------

ui <- fluidPage(
  title = "WAV Stream Monitoring Sites",
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    includeHTML("google-analytics.html"),
    tags$style(HTML("
      body {
        font-family: 'Lato', sans-serif;
      }
      
      .container-fluid {
        max-width: 1000px;
        margin: auto;
      }
      
      .leaflet-control-layers-list::before {
        content: 'Basemap:';
        font-weight: bold;
      }
      
      .leaflet-control-layers-overlays::before {
        content: 'Layers:';
        font-weight: bold;
      }
    "))
  ),
  
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
  
  bsCollapse(
    bsCollapsePanel(
      title = "Additional map options",
      value = "opts",
      radioButtons(
        inputId = "watershed",
        label = HTML("Choose which watershed type to show on the map (<span style='color: blue;'>blue shapes</span>):"),
        choices = list(
          "Nine Key Elements watershed plans" = "nkes",
          "HUC8 (Subbasins)" = "huc8",
          "HUC10 (Watersheds)" = "huc10",
          "HUC12 (Subwatersheds)" = "huc12"
        )
      )
    ),
    open = "opts"
  ),
  
  h3("Monitoring Stations"),
  p("The sites on the map above show where in the state Water Action Volunteers made water quality monitoring measurements during the 2021 season."),
  p(strong("Baseline monitoring:"), "Volunteers enter the WAV program by training to do baseline stream monitoring. Each year, baseline volunteers journey to their monitoring sites once per month from May to October to collect four baseline parameters: dissolved oxygen, instantaneous temperature, transparency and streamflow. During at least two of these months (May/June and September/October), volunteers also collect macroinvertebrates to calculate a biotic index score. Once per season, some advanced volunteers also conduct a habitat assessment. In 2020, volunteers collected this baseline data at 284 unique monitoring sites. In 2021, these data were collected at 279 unique sites."),
  p(strong("Nutrient monitoring:"), "After at least one season of baseline monitoring, some WAV volunteers will support special projects monitoring. Special projects monitoring is designed to either use the same methods as DNR professionals for data collection or to meet specific data needs. Recently these special projects have included monitoring with meters, aquatic invasive species monitoring, nutrient monitoring, and deploying continuous temperature monitors. Nutrient monitoring is the most widespread of the special projects. Volunteers sample for total phosphorus concentrations in rivers and streams. In some instances, volunteers also collect suspended solids samples and/or nitrogen panels."),
  p(strong("Temperature loggers:"), "Across the state there are a number of automatic, deployed temperature loggers that continuously monitor water temperature in streams. This data can be useful for understanding seasonal stream dynamics, as lower temperatures can indicate higher flow rates, more oxygen-rich water, and overall healther stream systems. You can see more detailed stream temperature data from these stations on our ", HTML("<a href='https://data-viz.it.wisc.edu/wav-temp-loggers/' target = '_blank'>temperature logger data dashboard</a>.")),
  
  h3("Map Layers"),
  p(strong("DNR Regions:"), "The Department of Natural Resources has grouped Wisconsin's 72 counties into five different regions, which are shown on the map as a light color fill."),
  p(strong("Nine Key Elements Plans:"), "These are long-term plans for specific watersheds that provide a framework for improving water quality in a holistic manner. The nine elements help assess the contributing causes and sources of nonpoint source pollution, involve key stakeholders and prioritize restoration and protection strategies to address water quality problems. Learn more about NKEs at the", HTML("<a href='https://dnr.wisconsin.gov/topic/Nonpoint/9keyElement' target='_blank'>Wisconsin DNR</a>.")),
  p(strong("HUC8, HUC10, and HUC12 watersheds:"), "HUC stands for Hydrologic Unit Code and is a sequence of numbers or letters that identify a hydrological feature like a river, lake, or drainage basin. For this map, we are including HUC8 boundaries (subbasins), HUC10 boundaries (watersheds), and HUC12 boundaries (subwatersheds) as optional layers so you can better understand the hydrology of Wisconsin. HUC8 is the largest of these classifications, and HUC12 the smallest."),
  
  h3("Station Lists:"),
  bsCollapse(
    bsCollapsePanel(
      title = "Baseline monitoring stations",
      div(style = "overflow: auto;", dataTableOutput("baselineDT")),
      downloadButton("baselineDL")
    ),
    bsCollapsePanel(
      title = "Nutrient monitoring stations",
      div(style = "overflow: auto;", dataTableOutput("nutrientDT")),
      downloadButton("nutrientDL")
    ),
    bsCollapsePanel(
      title = "Temperature logging stations",
      div(style = "overflow: auto;", dataTableOutput("thermistorDT")),
      downloadButton("thermistorDL")
    )
  ),
  
  h3("More Information:"),
  p("Visit the Water Action Volunteers website at", HTML("<a href='https://wateractionvolunteers.org' target='_blank'>wateractionvolunteers.org</a>.")),
  
  br(),
  hr(),
  p(
    style = "color: grey; font-size: smaller; font-style: italic;",
    align = "center",
    "Developed by Ben Bradford, UW-Madison Entomology", br(),
    paste("Last updated:", format(file.info(".")$mtime, "%Y-%m-%d")), br(),
    a("Source code", href = "https://github.com/bzbradford/wav-station-map", target = "_blank")
  )
)



# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Map ----
  
  basemaps <- list(
    one = "OpenStreetMap",
    two = "Grey Canvas",
    three = "ESRI Topo"
  )
  
  layers <- list(
    counties = "Counties/Regions",
    watersheds = "Watersheds/NKE Plans (<span style='color: blue;'>blue</span>)",
    baseline = "Baseline Stations (<span style='color: green;'>green</span>)",
    nutrient = "Nutrient Stations (<span style='color: orange;'>orange</span>)",
    thermistor = "Temperature Loggers (<span style='color: purple;'>purple</span>)"
  )
  
  # create_popup <- function(data) {
  #   with(data,
  #     lapply(paste0(
  #       "<b>Baseline Monitoring Station</b><br>",
  #       "<br><b>Station ID:</b> ", StationID,
  #       "<br><b>Name:</b> ", StationName,
  #       "<br><b>County:</b> ", County, " County",
  #       "<br><b>Region:</b> ", Region,
  #       "<br><b>HUC8:</b> ", HUC8, " Subbasin",
  #       "<br><b>HUC10:</b> ", HUC10, " Watershed",
  #       "<br><b>HUC12:</b> ", HUC12, " Subwatershed",
  #       "<br><b>Latitude:</b>", Latitude,
  #       "<br><b>Longitude:</b>", Longitude
  #     ), HTML))
  # }
  
  create_popup <- function(data, title) {
    data %>% {
      cols <- names(.)
      lapply(1:nrow(.), function(r) {
        row <- .[r,]
        details <- 
          lapply(1:length(cols), function(c) {
            paste0("<br><b>", cols[c], ":</b> ", row[c])
          }) %>%
          paste0(collapse = "")
        paste0(title, details)
      }) %>% paste0()
    }
  }
  
  # baseline %>% {
  #   cols <- names(.)
  #   print(cols)
  #   lapply(1:nrow(.), function(r) {
  #     row <- .[r,]
  #     lapply(1:length(cols), function(c) {
  #       paste0("<br><b>", cols[c], ":</b> ", row[c])
  #     }) %>% paste0()
  #   }) %>% paste0()
  # }

  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      fitBounds(
        lat1 = 42.4,
        lat2 = 47.1,
        lng1 = -92.9,
        lng2 = -86.8
      ) %>%
      addTiles(group = basemaps$one) %>%
      addProviderTiles(providers$CartoDB.Positron, group = basemaps$two) %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = basemaps$three) %>%
      addMapPane("counties", 410) %>%
      addMapPane("watersheds", 420) %>%
      addMapPane("points", 430) %>%
      addPolygons(
        data = counties,
        group = layers$counties,
        label = ~lapply(paste0("<b>", COUNTY_NAM, " County</b><br>", DNR_REGION), HTML),
        fillOpacity = 0.1,
        color = "grey",
        opacity = 0.5,
        fillColor = ~ colorFactor("Dark2", counties$DNR_REGION)(DNR_REGION),
        weight = 1,
        options = pathOptions(pane = "counties")
      ) %>%
      addCircleMarkers(
        data = baseline.sf,
        group = layers$baseline,
        label = ~lapply(paste0("<b>Baseline Monitoring Stations</b><br>Station ID: ", StationID, "<br>Name: ", StationName), HTML),
        popup = ~create_popup(baseline, "<b>Baseline Monitoring Stations</b><br>"),
        radius = 5,
        color = "black",
        weight = 0.5,
        fillColor = "green",
        fillOpacity = 0.75,
        options = markerOptions(pane = "points", sticky = F)
      ) %>%
      addCircleMarkers(
        data = nutrient.sf,
        group = layers$nutrient,
        label = ~lapply(paste0("<b>Nutrient Monitoring Station</b><br>Station ID: ", StationID, "<br>Name: ", StationName), HTML),
        popup = ~create_popup(nutrient, "<b>Nutrient Monitoring Station</b><br>"),
        radius = 5,
        color = "black",
        weight = 0.5,
        fillColor = "orange",
        fillOpacity = 0.75,
        options = markerOptions(pane = "points", sticky = F)
      ) %>%
      addCircleMarkers(
        data = thermistor.sf,
        group = layers$thermistor,
        label = ~lapply(paste0("<b>Thermistor Station</b><br>Station ID: ", StationID, "<br>Name: ", StationName), HTML),
        popup = ~create_popup(thermistor, "<b>Thermistor Station</b><br>"),
        radius = 5,
        color = "black",
        weight = 0.5,
        fillColor = "purple",
        fillOpacity = 0.75,
        options = markerOptions(pane = "points")
      ) %>%
      addLayersControl(
        baseGroups = unlist(basemaps, use.names = FALSE),
        overlayGroups = unlist(layers, use.names = FALSE),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addFullscreenControl(pseudoFullscreen = TRUE) %>%
      addEasyButtonBar(
        easyButton(
          position = "topleft",
          icon = "fa-crosshairs",
          title = "Get my location",
          onClick = JS("
            function(btn, map) {
              map.locate({
                setView: true,
                enableHighAccuracy: false,
                maxZoom: 12
              })
            }
          ")
        ),
        easyButton(
          position = "topleft",
          icon = "fa-globe",
          title = "Reset map view",
          onClick = JS("
            function(btn, map) {
              map.fitBounds([[47.1, -86.8], [42.4, -92.9]])
            }
          ")
        )
      )
  })
  
  
  ## Render counties ----
  
  observeEvent(TRUE, {
    leafletProxy("map") %>%
      addPolygons(
        data = counties,
        group = layers$watersheds,
        label = ~ lapply(paste0("<b>", COUNTY_NAM, " County</b><br>", DNR_REGION), HTML),
        fillOpacity = 0.1,
        color = "grey",
        opacity = 0.5,
        fillColor = ~ colorFactor("Dark2", counties$DNR_REGION)(DNR_REGION),
        weight = 1,
        options = pathOptions(pane = "counties")
      )
  })
  
  ## Watershed/NKE overlays ----
  
  observeEvent(input$watershed, {
    ws <- input$watershed
    map <- leafletProxy("map")
    color <- "blue"
    fill_color <- "lightblue"
    
    map %>% clearGroup(layers$watersheds)
    
    if (ws == "nkes") {
      map %>%
        addPolygons(
          data = nkes,
          group = layers$watersheds,
          label = ~ lapply(paste0("<b>", PLAN_NAME, "</b><br>Ends: ", END_DATE, "<br>Objective: ", OBJECTIVE_), HTML),
          weight = 1,
          color = color,
          fillColor = fill_color,
          options = pathOptions(pane = "watersheds"),
          labelOptions = labelOptions(style = list("width" = "300px", "white-space" = "normal"))
        )
    } else if (ws == "huc8") {
      map %>%
        addPolygons(
          data = huc8,
          group = layers$watersheds,
          label = ~ lapply(
            paste0(
              "<b>", HUC8_NAME, " Subbasin</b>",
              "<br>HUC8 Code: ", HUC8_CODE,
              "<br>Area: ", formatC(SHAPE__Are / 1e6, format = "f", big.mark = ",", digits = 2), " sq km"),
            HTML),
          weight = 1,
          color = color,
          fillColor = fill_color,
          options = pathOptions(pane = "watersheds")
        )
    } else if (ws == "huc10") {
      map %>%
        addPolygons(
          data = huc10,
          group = layers$watersheds,
          label = ~ lapply(
            paste0(
              "<b>", HUC10_NAME, " Watershed</b>",
              "<br>HUC10 Code: ", HUC10_CODE,
              "<br>Area: ", formatC(SHAPE__Are / 1e6, format = "f", big.mark = ",", digits = 2), " sq km"),
            HTML),
          weight = 1,
          color = color,
          fillColor = fill_color,
          options = pathOptions(pane = "watersheds")
        )
    } else if (ws == "huc12") {
      map %>%
        addPolygons(
          data = huc12,
          group = layers$watersheds,
          label = ~ lapply(
            paste0(
              "<b>", HUC12_NAME, " Subwatershed</b>",
              "<br>HUC12 Code: ", HUC12_CODE,
              "<br>Area: ", formatC(SHAPE__Are / 1e6, format = "f", big.mark = ",", digits = 2), " sq km"),
            HTML),
          weight = 1,
          color = color,
          fillColor = fill_color,
          options = pathOptions(pane = "watersheds")
        )
    }
  })
  
  
  
  # Table outputs ----
  
  output$baselineDT <- renderDataTable({
    baseline
  })
  
  output$nutrientDT <- renderDataTable({
    nutrient
  })
  
  output$thermistorDT <- renderDataTable({
    thermistor
  })
  
  
  
  # Download handlers ----
  
  output$baselineDL <- downloadHandler(
    filename = "wav-baseline-stations.csv",
    content = function(file) {write_csv(baseline, file)})
  
  output$nutrientDL <- downloadHandler(
    filename = "wav-nutrient-stations.csv",
    content = function(file) {write_csv(nutrient, file)})
  
  output$thermistorDL <- downloadHandler(
    filename = "wav-temperature-loggers.csv",
    content = function(file) {write_csv(thermistor, file)})
}

shinyApp(ui, server)
