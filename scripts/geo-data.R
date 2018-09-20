library(package = "sf")

departments <- readRDS(file = "data/departments.rds")
municipalities <- readRDS(file = "data/municipalities.rds")

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
    # log transform the fill color, so diferences are apparent at lower values
    trans = "log",
    breaks = 10 ^ seq(from = 0, to = 2, by = 1)
  ) +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(-0.05, 1.05),
    plot.subtitle = element_text(face = "plain")
  )

