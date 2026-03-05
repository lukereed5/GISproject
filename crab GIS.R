rm(list = ls())

# Install if needed:
# install.packages(c("rinat", "dplyr", "sf", "leaflet", "htmlwidgets"))

library(rinat)
library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)

# --------------------------------------------------
# 1. Download BOTH species
# --------------------------------------------------

greencrab <- get_inat_obs(
  taxon_name = "Carcinus maenas",
  bounds = c(-35, -18, -22, 33),
  maxresults = 1000
)

caperock <- get_inat_obs(
  taxon_name = "Guinusia chabrus",
  bounds = c(-35, -18, -22, 33),
  maxresults = 1000
)

# --------------------------------------------------
# 2. Apply identical filtering
# --------------------------------------------------

clean_filter <- function(df) {
  df %>%
    filter(
      positional_accuracy < 100,
      latitude < 0,
      !is.na(latitude),
      captive_cultivated == "false",
      quality_grade == "research"
    )
}

greencrab <- clean_filter(greencrab)
caperock  <- clean_filter(caperock)

# --------------------------------------------------
# 3. Convert to sf
# --------------------------------------------------

greencrab_sf <- st_as_sf(
  greencrab,
  coords = c("longitude", "latitude"),
  crs = 4326
)

caperock_sf <- st_as_sf(
  caperock,
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Add species column for mapping
greencrab_sf$species <- "Carcinus maenas"
caperock_sf$species  <- "Guinusia chabrus"

# Combine datasets
all_crabs <- rbind(greencrab_sf, caperock_sf)

# --------------------------------------------------
# 4. Adjust bounding box (shift east 10 km)
# --------------------------------------------------

bbox <- st_bbox(all_crabs)

mid_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
km_per_deg_lon <- 111.32 * cos(mid_lat * pi / 180)
deg_shift <- 10 / km_per_deg_lon

xmin <- as.numeric(bbox["xmin"])
ymin <- as.numeric(bbox["ymin"])
xmax <- as.numeric(bbox["xmax"] + deg_shift)
ymax <- as.numeric(bbox["ymax"])

# --------------------------------------------------
# 5. Colour palette
# --------------------------------------------------

pal <- colorFactor(
  palette = c("red", "blue"),
  domain = all_crabs$species
)

# --------------------------------------------------
# 6. Build map
# --------------------------------------------------

m <- leaflet(all_crabs) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    radius = 4,
    stroke = FALSE,
    fillOpacity = 0.8,
    color = ~pal(species),
    popup = ~paste(
      "<b>Species:</b>", species,
      "<br><b>Date:</b>", observed_on
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~species,
    title = "Species",
    opacity = 1
  ) %>%
  fitBounds(xmin, ymin, xmax, ymax)

# Force external browser (safer on campus WiFi)
options(viewer = NULL)
m

