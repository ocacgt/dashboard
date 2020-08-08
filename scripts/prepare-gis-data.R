#------------------------------------------------------------------------------*
# Prepare simplified shapes
#------------------------------------------------------------------------------*
  
# Load used packages
library(package = "tidyverse")



#------------------------------------------------------------------------------*
# Main campus shapefiles ----
#------------------------------------------------------------------------------*

usac <- sf::read_sf("data/usac/LIMITES_GENERALES.shp")

# Read in raw shapes
buildings_usac <- sf::read_sf("data/usac.osm", layer = "multipolygons") %>%
  print()

# Define university limits
limits <- buildings_usac %>%
  filter(name == "Universidad de San Carlos de Guatemala") %>%
  print()

# Get the roads
roads_usac <-  sf::read_sf("data/usac.osm", layer = "lines") %>%
  filter(
    name != "Universidad de San Carlos de Guatemala",
    !sapply(sf::st_within(., limits), is_empty)
  ) %>%
  print()

# Filter the buildings
buildings_usac <- buildings_usac %>%
  filter(
    name != "Universidad de San Carlos de Guatemala",
    !sapply(sf::st_within(., limits), is_empty)
  )


# Write reference table
buildings_usac %>%
  as.data.frame() %>%
  select(osm_id, osm_way_id, name) %>% {
    if(!file.exists("output/buildings_ref.xlsx")) {
      writexl::write_xlsx(., path = "output/buildings_ref.xlsx")
    }
  }


# Test buildings
buildings_usac %>%
  leaflet::leaflet() %>%
  # leaflet::addTiles() %>%
  leaflet::addPolygons(data = limits, color = "#dddddd", fillOpacity = 1) %>%
  leaflet::addPolygons(label = ~name, weight = 1) %>%
  leaflet::addPolylines(
    data = roads_usac, color = "red", label = ~name, weight = 1
  )




#------------------------------------------------------------------------------*
# Medical campus shapefiles (CUM) ----
#------------------------------------------------------------------------------*

cum <- sf::read_sf("data/cum.osm", layer = "multipolygons") %>%
  select(osm_way_id, name, amenity, building, place) %>%
  print()


limits_cum <- cum %>%
  filter(name == "Centro Universitario Metropolitano")

buildings_cum <- cum %>%
  sf::st_intersection(limits_cum) %>%
  filter(is.na(name)) %>%
  bind_rows(
    sf::st_difference(limits_cum, sf::st_union(.)) %>%
      mutate(
        name = "CUM"
      )
  ) %>%
  mutate(
    name = recode(
      osm_way_id,
      "40529532" = "CUM A",
      "40529506" = "CUM B",
      "40529464" = "CUM C",
      "40529513" = "Parqueo CUM",
      "40529519" = "CUM"
    )
  ) %>%
  print()


# Test buildings
buildings_cum %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(label = ~name, weight = 1)



# End of script
