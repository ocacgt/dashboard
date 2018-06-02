departments <- sf::read_sf("/data/odeleon/gis/vector/gtm/borders/raw/departments.shp")

municipalities <- sf::read_sf("/data/odeleon/gis/vector/gtm/borders/raw/municipalities.shp")

library(package = "sf")



departments <- departments %>%
  mutate(
    department = NAME_DPTO %>%
      iconv(from = "Latin1", to = "ASCII//TRANSLIT")
  ) %>%
  select(department, dept_id = COD_DPTO)


municipalities <- municipalities %>%
  mutate(
    municipality = MUNICIPIO %>%
      iconv(from = "Latin1", to = "ASCII//TRANSLIT"),
    dept_id = substr(COD_MUNI, start = 1, stop = 2)
  ) %>%
  select(dept_id, municipality)



saveRDS(departments, file = "data/departments.rds")
saveRDS(municipalities, file = "data/municipalities.rds")
