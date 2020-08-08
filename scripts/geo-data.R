library(package = "sf")

departments <- readRDS(file = "data/departments.rds")
municipalities <- readRDS(file = "data/municipalities.rds")
zones <- readRDS(file = "data/zones.rds")

#------------------------------------------------------------------------------*
# Prepare static map with reports by department ----
#------------------------------------------------------------------------------*

# Using the report locations
department_reports <- locations %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  # Join with the department names using the coordinates (report in department)
  st_join(departments) %>%
  # Count departments
  count(department) %>%
  # Rename report points geometry to preserve on joins
  rename(reports = geometry) %>%
  as.data.frame() %>% 
  # Add to departments data
  left_join(departments, y = .)

# Using the report locations
zone_reports <- locations %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  # Join with the department names using the coordinates (report in department)
  st_join(zones) %>%
  filter(!is.na(zona)) %>%
  # Count departments
  count(zona) %>%
  # Rename report points geometry to preserve on joins
  rename(reports = geometry) %>%
  as.data.frame() %>% 
  # Add to departments data
  left_join(zones, y = .)


static_map <- department_reports %>%
  ggplot() +
  # Department boundaries filled given number of reports
  geom_sf(
    aes(fill = n)
  ) +
  # Draw each report as a point
  geom_sf(
    data = department_reports %>%
      # Switch geometry to report points
      st_set_geometry(value = "reports"),
    size = 0.3, fill = "red", color = "red"
  ) +
  labs(
    title = "Figura 1: Reportes por departamento.",
    subtitle = paste(
      "Los puntos rojos representan cada reporte, el color del departamento",
      "representa el conteo total de reportes que tiene.",
      sep = "\n"
    ),
    fill = "Número\nde\nreportes"
  ) +
  scale_fill_viridis_c(
    na.value = "grey90",
    # log transform the fill color, so differences are apparent at lower values
    trans = "log",
    breaks = 10 ^ seq(from = 0, to = 2, by = 1)
  ) +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(-0.05, 1.05),
    plot.subtitle = element_text(face = "plain")
  )




static_zones_map <- zone_reports %>%
  ggplot() +
  # Zone  boundaries filled given number of reports
  geom_sf(
    aes(fill = n)
  ) +
  # Draw each report as a point
  geom_sf(
    data = zone_reports %>%
      # Switch geometry to report points
      st_set_geometry(value = "reports"),
    size = 0.3, fill = "red", color = "red"
  ) +
  labs(
    title = "Figura 1b: Reportes por zona del municipio de Guatemala.",
    subtitle = paste(
      "Los puntos rojos representan cada reporte, el color de la zona",
      "representa el conteo total de reportes que tiene.",
      sep = "\n"
    ),
    fill = "Número\nde\nreportes"
  ) +
  guides(
    fill = guide_colorbar(draw.ulim = TRUE, draw.llim = TRUE)
  ) +
  scale_fill_viridis_c(
    na.value = "grey90",
    # log transform the fill color, so differences are apparent at lower values
    trans = "log",
    breaks = 10 ^ seq(from = 0, to = 2, by = 1),
    labels = 10 ^ seq(from = 0, to = 2, by = 1)
  ) +
  expand_limits(fill = c(1, 100)) +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(-0.05, 1.05),
    plot.subtitle = element_text(face = "plain")
  )

