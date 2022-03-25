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
baseline <- read_sf("data/wav-station-locations.csv")
nutrient <- read_sf("data/nutrient-locations.csv")
thermistor <- read_sf("data/thermistor-locations.csv")
baseline.sf <- st_as_sf(baseline, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
nutrient.sf <- st_as_sf(nutrient, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)
thermistor.sf <- st_as_sf(thermistor, coords = c("Longitude", "Latitude"), crs = 4326, remove = F)

# add county names to nutrient and thermistor data
# baseline.sf %>%
#   st_join(counties) %>%
#   st_set_geometry(NULL) %>%
#   select(StationID, StationName, County = COUNTY_NAM, Region = DNR_REGION, Latitude, Longitude) %>%
#   mutate(Region = gsub(" Region", "", Region)) %>%
#   write_csv("data/wav-station-locations.csv")
# 
# nutrient.sf %>%
#   st_join(counties) %>%
#   st_set_geometry(NULL) %>%
#   select(StationID, StationName, County = COUNTY_NAM, Region = DNR_REGION, Latitude, Longitude) %>%
#   mutate(Region = gsub(" Region", "", Region)) %>%
#   write_csv("data/nutrient-locations.csv")
# 
# thermistor.sf %>%
#   st_join(counties) %>%
#   st_set_geometry(NULL) %>%
#   select(StationID, StationName, County = COUNTY_NAM, Region = DNR_REGION, Latitude, Longitude) %>%
#   mutate(Region = gsub(" Region", "", Region)) %>%
#   write_csv("data/thermistor-locations.csv")


# add watersheds to station locations
# baseline.sf %>%
#   st_join(huc8[, "HUC8_NAME"]) %>%
#   st_join(huc10[, "HUC10_NAME"]) %>%
#   st_join(huc12[, "HUC12_NAME"]) %>%
#   st_set_geometry(NULL) %>%
#   select(StationID:Region, HUC8 = HUC8_NAME, HUC10 = HUC10_NAME, HUC12 = HUC12_NAME, everything()) %>%
#   write_sf("out/baseline-locations.csv")
# 
# nutrient.sf %>%
#   st_join(counties[, c("COUNTY_NAM", "DNR_REGION")]) %>%
#   st_join(huc8[, "HUC8_NAME"]) %>%
#   st_join(huc10[, "HUC10_NAME"]) %>%
#   st_join(huc12[, "HUC12_NAME"]) %>%
#   st_set_geometry(NULL) %>%
#   select(StationID, StationName, County = COUNTY_NAM, Region = DNR_REGION, HUC8 = HUC8_NAME, HUC10 = HUC10_NAME, HUC12 = HUC12_NAME, everything()) %>%
#   write_sf("out/nutrient-locations.csv")
# 
# thermistor.sf %>%
#   st_join(counties[, c("COUNTY_NAM", "DNR_REGION")]) %>%
#   st_join(huc8[, "HUC8_NAME"]) %>%
#   st_join(huc10[, "HUC10_NAME"]) %>%
#   st_join(huc12[, "HUC12_NAME"]) %>%
#   st_set_geometry(NULL) %>%
#   select(StationID, StationName, County = COUNTY_NAM, Region = DNR_REGION, HUC8 = HUC8_NAME, HUC10 = HUC10_NAME, HUC12 = HUC12_NAME, everything()) %>%
#   write_sf("out/thermistor-locations.csv")

baseline.sf %>%
  select(-c(WBIC, County, Region)) %>%
  st_join(counties[, c("COUNTY_NAM", "DNR_REGION")]) %>%
  select(StationID, StationName, County = COUNTY_NAM, Region = DNR_REGION, everything()) %>%
  st_set_geometry(NULL) %>%
  write_csv("out/baseline-locations.csv")


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
    color = "blue",
    weight = 1,
    # fillColor = ~ colorBin("RdYlBu", nkes$END_YEAR)(END_YEAR),
    fillColor = "lightblue",
    options = pathOptions(pane = "nkes"),
    labelOptions = labelOptions(style = list("width" = "300px", "white-space" = "normal"))
  ) %>%
  addCircleMarkers(
    data = baseline.sf,
    group = layers$baseline,
    label = ~ lapply(paste0("<b>Baseline Monitoring Stations</b><br>", StationID, ": ", StationName), HTML),
    radius = 2.5,
    color = "black",
    weight = 0.5,
    fillColor = "green",
    fillOpacity = 0.75,
    options = markerOptions(pane = "points", sticky = F)
  ) %>%
  addCircleMarkers(
    data = nutrient.sf,
    group = layers$nutrient,
    label = ~ lapply(paste0("<b>Nutrient Monitoring Station</b><br>", StationID, ": ", StationName), HTML),
    radius = 2.5,
    color = "black",
    weight = 0.5,
    fillColor = "orange",
    fillOpacity = 0.75,
    options = markerOptions(pane = "points", sticky = F)
  ) %>%
  addCircleMarkers(
    data = thermistor.sf,
    group = layers$thermistor,
    label = ~ lapply(paste0("<b>Thermistor Station</b><br>", StationID, ":</b> ", StationName), HTML),
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





# simplify watershed maps -------------------------------------------------

library(rmapshaper)

huc6 <- read_sf("edit/wi-huc-6-basins-wgs.shp") %>%
  st_transform(crs = 4326)

huc6_simp <- huc6 %>%
  ms_simplify(keep = 0.1) %>%
  st_make_valid()

huc6_simp %>%
  write_sf("shp/wi-huc-6.shp")

leaflet() %>%
  addTiles() %>%
  # addPolygons(
  #   data = huc8
  # ) %>%
  addPolygons(
    data = huc6_simp,
    weight = 1
  )





huc8 <- read_sf("edit/wi-huc-8-subbasins-wgs.shp") %>%
  st_transform(crs = 4326)

huc8_simp <- huc8 %>%
  ms_simplify(keep = 0.1)

huc8_simp %>%
  write_sf("edit/wi-huc-8-simp.shp")

leaflet() %>%
  addTiles() %>%
  # addPolygons(
  #   data = huc8
  # ) %>%
  addPolygons(
    data = huc8_simp,
    weight = 1
  )


huc10 <- read_sf("edit/wi-huc-10-watersheds-wgs.shp") %>%
  st_transform(crs = 4326)

huc10_simp <- huc10 %>%
  ms_simplify(keep = 0.1)

huc10_simp %>%
  write_sf("edit/wi-huc-10-simp.shp")

leaflet() %>%
  addTiles() %>%
  # addPolygons(
  #   data = huc10,
  #   weight = 1,
  #   color = "blue"
  # ) %>%
  addPolygons(
    data = huc10_simp,
    weight = 1,
    color = "green"
  )


huc12 <- read_sf("edit/wi-huc-12-subwatersheds-wgs.shp") %>%
  st_transform(crs = 4326)

huc12_simp <- huc12 %>%
  ms_simplify(keep = 0.1)

huc12_simp %>%
  write_sf("edit/wi-huc-12-simp.shp")

leaflet() %>%
  addTiles() %>%
  # addPolygons(
  #   data = huc10,
  #   weight = 1,
  #   color = "blue"
  # ) %>%
  addPolygons(
    data = huc12_simp,
    weight = 1,
    color = "green"
  )


counties <- read_sf("edit/wi-counties-wgs.shp") %>%
  ms_simplify(keep = 0.1)

counties %>%
  write_sf("edit/wi-counties-simp.shp")



