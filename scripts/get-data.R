#------------------------------------------------------------------------------*
# Get data from online questionnaires
#------------------------------------------------------------------------------*



#------------------------------------------------------------------------------*
# Set reference categories ----
#------------------------------------------------------------------------------*

harassment_types <- c(
  "piropos", "\"echadas de mano\"",
  "comentarios sexuales, directos o indirectos al cuerpo", 
  "besos", "bocinazos", "jadeos y otros ruídos", "silbidos",
  "miradas lascivas", "persecución y arrinconamiento (acecho)",
  "gestos obscenos",  "masturbación con o sin eyaculación y exhibicionismo",
  "fotografías y grabaciones del cuerpo no consentidas y con connotación sexual"
)

transport_response_types <- c(
  "grito",
  "aviso a la policía o agente municipal",
  "me alejo o me cambio de lugar",
  "me defiendo sola",
  "no hago nada"
)

harassment_effects <- c(
  "enojo", "indignación", "frustación", "incomodidad", "inseguridad", 
  "miedo", "inconformidad", "pérdida de autonomía", "confusión"
)

harassment_places <- c(
  "avenidas/calles", "transporte público (bus, microbús, transmetro)", 
  "estaciones de buses", "cerca de construcciones", 
  "mercados", "universidad", "parque",
  "restaurante, cafetería, centro comercial, gimnasios, otros"
)

harassment_times <- c(
  "madrugada", "mañana", "medio día", "tarde", "noche", "otro"
)

harassment_frequencies <- c(
  "Varias veces al dia", "Una vez al dia",
  "2 a 3 veces por semana", "1 vez a la semana",
  "1 par de veces al mes", "Pocas veces al año", 
  "Nunca"
)



#------------------------------------------------------------------------------*
# Load data ----
#------------------------------------------------------------------------------*


# Set a snapshot file
form_snapshot <- "data/form-snapshot.RData"

if(!file.exists(form_snapshot)){
  # Load used package
  library(package = googlesheets)
  
  # Formulario acoso callejero
  datos_calle <- gs_title(x = "Mapeo del acoso callejero en Guatemala (respuestas)") %>%
    gs_read()
  
  # Formulario acoso en el transporte público
  datos_transporte <- gs_title(x = "Acoso Callejero en el Transporte Público (respuestas)") %>%
    gs_read()
  
  save(datos_calle, datos_transporte, file = form_snapshot)
} else {
  load(file = form_snapshot)
}




#------------------------------------------------------------------------------*
# Prepare used variables ----
#------------------------------------------------------------------------------*

# Acoso callejero
dashboard_calle <- datos_calle %>%
  select(
    timestamp = `Marca temporal`,
    reporter_gender = Género,
    reporter_age = `Edad (en años)`,
    # reporter_responded = `¿Respondiste de alguna manera al acoso?`,
    # reporter_response = `¿Cuál fue la forma en qué respondiste?`,
    reporter_effect = `¿Qué sentiste?`,
    harasser_sex = `En el incidente de acoso, la persona acosadora era:`,
    harasser_age = `¿Qué edad aproximada dirías que tiene la persona acosadora?`,
    harassment_date = `Fecha del incidente`,
    harassment_time = `Horario del incidente (parte del día)`,
    harassment_location = `¿Dónde pasó?`,
    harassment_place = `Tipo de lugar`,
    harassment_type = `Categorías`,
    harassment_frequency = `¿Cada cuánto tiempo sufres alguna forma de acoso?`
  ) %>%
  # Fix coding
  mutate(
    # Consistent category for sex
    reporter_sex = recode(
      reporter_gender,
      "Femenino" = "Mujer",
      "Masculino" = "Hombre"
    ),
    # Recode additional category
    harasser_sex = recode(
      harasser_sex,
      "Masculino" = "Hombre"
    ),
    # Order harasser ages
    harasser_age = recode_factor(
      harasser_age,
      "Menor de 18 años" = "< 18",
      "19 - 24" = "19 - 24",
      "25 - 29" = "25 - 29",
      "30 - 34" = "30 - 34",
      "35 - 39" = "35 - 39",
      "40 - 44" = "40 - 44",
      "Mayor de 45 años" = "> 45",
      "No sé" = "No sé",
      .ordered = TRUE
    ),
    # Process multiple answers
    harassment_type_rec = tolower(harassment_type) %>%
      # Fix pre specified instances of separator
      gsub(".+(piropos).*", "\\1", .) %>%
      gsub(",( no consentidas| directos)", "\\1", .) %>%
      strsplit(split = ", *") %>%
      map(~ifelse(!.x %in% harassment_types, "otro", .x)),
    reporter_effect_rec = tolower(reporter_effect) %>%
      strsplit(split = ", *") %>%
      map(~ifelse(!.x %in% harassment_effects, "otro", .x)),
    harassment_place_rec = harassment_place %>%
      tolower() %>%
      recode(
        calle = "avenidas/calles"
      ) %>%
      ifelse(
        ! . %in% harassment_places, last(harassment_places), .
      ) %>%
      factor(levels = harassment_places, ordered = TRUE),
    # Fix ages
    reporter_age = reporter_age %>%
      gsub(" ?a[ñn]os?", "", .) %>%
      recode(
        "veinte y cuatro" = "24",
        "veintiuno" = "21"
      ) %>%
      as.integer(),
    # Fix dates
    timestamp = lubridate::dmy_hms(timestamp),
    harassment_date = lubridate::dmy(harassment_date),
    # Fix times
    harassment_time_rec = ifelse(
      test = is.na(hms::as.hms(harassment_time)),
      yes = harassment_time %>%
        tolower() %>%
        ifelse(! . %in% harassment_times, "otro", .),
      # Fixed periods
      no = case_when(
        hms::as.hms(harassment_time) < hms::as.hms("06:00:00") ~ "madrugada",
        hms::as.hms(harassment_time) < hms::as.hms("12:00:00") ~ "mañana",
        hms::as.hms(harassment_time) < hms::as.hms("18:00:00") ~ "tarde",
        hms::as.hms(harassment_time) < hms::as.hms("23:59:59") ~ "noche"
      )
    ) %>%
      factor(., levels = harassment_times, ordered = TRUE),
    # Fix frequencies
    harassment_frequency = harassment_frequency %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      gsub("ano", "año", .) %>%
      recode(
        "Semanalmente" = "1 vez a la semana",
        "Mensualmente" = "1 par de veces al mes",
        "Diariamente" = "Una vez al dia",
        "Un par de veces en el año" = "Pocas veces al año"
      ) %>%
      factor(levels = harassment_frequencies, ordered = TRUE)
  )


# Manual data check
if(interactive()){
  dashboard_calle %>%
    select(
      -harassment_type, -harassment_location, -harassment_place, -harassment_time,
      -reporter_effect
    ) %>%
    walk(
      ~.x %>% unlist %>% table(., useNA = "always") %>% sort(., decreasing = TRUE) %>% print
    )
}



# Acoso en transporte público
dashboard_transporte <- datos_transporte %>%
  select(
    timestamp = `Marca temporal`,
    reporter_age = `Edad (en años)`,
    times_harassed = `¿Cuántas veces ha sido víctima de acoso sexual callejero mientras usaba algún transporte público?`,
    harassment_type = `Cuando sufrió acoso sexual callejero en el trasporte público, a usted (tipos de acoso):`,
    reporter_reaction = `Comúnmente, cuando alguna de esas situaciones le ocurre ¿qué hace en el momento?`,
    transport_type = `¿En qué tipo de transporte ha sido víctima de acoso?`,
    transport_route = `Con base en su respuesta anterior indique la ruta en donde fue víctima de acoso.`,
    knows_report = `¿Usted sabe en dónde o a quién puede denunciar cuando es víctima de acoso sexual callejero en el transporte público?`,
    has_reported = `¿Usted ha avisado a alguna autoridad o denunciado cuando es víctima de acoso sexual en el transporte público?`,
    days_use = `¿Cuántos días a la semana utiliza el transporte público?`
  ) %>%
  # Fix coding
  mutate(
    # Process multiple answers
    harassment_type_rec = tolower(harassment_type) %>%
      strsplit(split = ", *"),
    harassment_type_simp = lapply(
      harassment_type_rec,
      function(.x) {
        data_frame(
          .x = .x,
          type = case_when(
            grepl("ofensivas", .x) ~ "insultos",
            grepl("piropos", .x) ~ "piropos",
            grepl("fotos", .x) ~ "fotografías",
            grepl("ataque", .x) ~ "intimidación",
            grepl("morbosamente", .x) ~ "miradas lascivas",
            grepl("genitales", .x) ~ "exhibicionismo",
            grepl("le tocaron", .x) ~ "tocamientos o manoseos",
            grepl("recargar", .x) ~ "recargarse encima",
            grepl("manosearon", .x) ~ "masturbación"
          )
        ) %>%
          pull(type)
      }
    ),
    reporter_reaction = tolower(reporter_reaction) %>%
      strsplit(split = ", *") %>%
      map(~ifelse(!.x %in% transport_response_types, "otro", .x)) %>%
      map(unique),
    transport_type = transport_type %>%
      strsplit(split = ", *"),
    transport_route = transport_route %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      tolower(),
    # Order categories
    times_harassed = factor(
      times_harassed,
      levels = c(
        "Muchas veces", "Pocas veces", "Una vez", "Ninguna vez"
      ),
      ordered = TRUE
    ),
    # Fix ages
    reporter_age = reporter_age %>%
      gsub(" *a[ñn]os? *", "", .) %>%
      recode(
        "Veintitrés" = "23"
      ) %>%
      as.integer(),
    # Fix dates
    timestamp = lubridate::dmy_hms(timestamp)
  )





#------------------------------------------------------------------------------*
# Process locations ----
#------------------------------------------------------------------------------*

location_cache <- "data/locations.rds"

if(!file.exists(location_cache)){
  # If there is no chache
  locations <- dashboard_calle %>%
    select(timestamp, harassment_location) %>%
    mutate(
      location = map(harassment_location, ggmap::geocode, source = "google")
    ) %>%
    unnest()
  
  # Cache all locations
  saveRDS(locations, file = location_cache)
} else {
  # Load available locations
  locations <- readRDS(location_cache)
  
  new_locations <- dashboard_calle %>%
    select(timestamp, harassment_location) %>%
    anti_join(locations) %>%
    mutate(
      location = map(harassment_location, ggmap::geocode, source = "google")
    ) %>%
    unnest()
  
  # Update available locations
  locations <- bind_rows(locations, new_locations)
  saveRDS(locations, file = location_cache)
}



#------------------------------------------------------------------------------*
# Hard limit inside Guatemala ----
#------------------------------------------------------------------------------*

# Get geo data
source(file = "scripts/geo-data.R")

gt_bbox <- st_bbox(departments)

# Filter on join
locations <- locations %>%
  filter(
    between(lon, gt_bbox["xmin"], gt_bbox["xmax"]),
    between(lat, gt_bbox["ymin"], gt_bbox["ymax"])
  )
  




# End of script