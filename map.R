library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)

# load shapes
# counties <- read_sf("shp/wi-counties.shp") %>%
#   st_transform(crs = st_crs(4326))
# nkes <- read_sf("shp/nke-plans-2022.shp") %>%
#   st_transform(crs = st_crs(4326)) %>%
#   mutate(END_YEAR = lubridate::year(END_DATE)) %>%
#   filter(!is.na(PLAN_NAME)) %>%
#   distinct(PLAN_NAME, .keep_all = T) %>%
#   arrange(Shape_Area)

# save updated shapes
# counties %>% write_sf("shp/wi-counties-wgs.shp")
# nkes %>% write_sf("shp/nke-plans-2022-wgs.shp")

# load shapes
counties <- read_sf("shp/wi-counties-wgs.shp")
nkes <- read_sf("shp/nke-plans-2022-wgs.shp")

# load data
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

# map
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
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  htmlwidgets::onRender("
    function() {
      $('.leaflet-control-layers-list').prepend('<b>Basemap:</b>');
      $('.leaflet-control-layers-overlays').prepend('<b>Map Layers:</b>');
    }
  ")

