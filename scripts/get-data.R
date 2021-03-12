#------------------------------------------------------------------------------*
# Get data from online questionnaires
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")



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


# Use archive
use_archived <- TRUE

# Set a snapshot files
calle_archive <- "data/archive_calle.rds"
transporte_archive <- "data/archive_transporte.rds"
calle_snapshot <- "data/datos_calle.rds"
transporte_snapshot <- "data/datos_transpote.rds"


if(use_archived & file.exists(calle_archive) & file.exists(transporte_archive)){
  # Read archived
  datos_calle <- readRDS(file = calle_archive)
  datos_transporte <- readRDS(file = transporte_archive)
} else {
  # Load used package
  library(package = googlesheets)
  
  # Formularios anteriores ----
  
  # Formulario acoso callejero
  datos_calle <- gs_title(x = "Mapeo del acoso callejero en Guatemala (respuestas)") %>%
    gs_read()
  
  # Formulario acoso en el transporte público
  datos_transporte <- gs_title(x = "Acoso Callejero en el Transporte Público (respuestas)") %>%
    gs_read()
  
  # Archivar los datos anteriores
  saveRDS(datos_calle, file = calle_archive)
  saveRDS(datos_transporte, file = transporte_archive)
}


if(file.exists(calle_snapshot) & file.exists(transporte_snapshot)){
  # Formularios nuevos ----
  datos_calle_nuevo <- readRDS(file = calle_snapshot)
  datos_transporte_nuevo <- readRDS(file = transporte_snapshot)
} else {
  # Load used package
  library(package = googlesheets)
  
  # Formulario acoso callejero
  datos_calle_nuevo <- gs_title(
    x = "formulario-MapeoAcosoCallejeroGt_2018 (Respuestas)"
  ) %>%
    gs_read()
  
  # Formulario acoso en el transporte público
  datos_transporte_nuevo <- gs_title(
    x = "formulario-AcosoCallejeroTransportePublico-2018 (Respuestas)"
  ) %>%
    gs_read()
  
  # Guardar un snapshot de los datos
  saveRDS(datos_calle_nuevo, file = calle_snapshot)
  saveRDS(datos_transporte_nuevo, file = transporte_snapshot)
}




#------------------------------------------------------------------------------*
# Prepare used variables ----
#------------------------------------------------------------------------------*

# Acoso callejero - cuestionario anterior
dashboard_calle_anterior <- datos_calle %>%
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
      test = is.na(lubridate::ymd_hms(paste(Sys.Date(), harassment_time))),
      yes = harassment_time %>%
        tolower() %>%
        ifelse(! . %in% harassment_times, "otro", .),
      # Fixed periods
      no = case_when(
        lubridate::ymd_hms(paste(Sys.Date(), harassment_time)) < lubridate::ymd_hms(paste(Sys.Date(), "06:00:00")) ~ "madrugada",
        lubridate::ymd_hms(paste(Sys.Date(), harassment_time)) < lubridate::ymd_hms(paste(Sys.Date(), "12:00:00")) ~ "mañana",
        lubridate::ymd_hms(paste(Sys.Date(), harassment_time)) < lubridate::ymd_hms(paste(Sys.Date(), "18:00:00")) ~ "tarde",
        lubridate::ymd_hms(paste(Sys.Date(), harassment_time)) < lubridate::ymd_hms(paste(Sys.Date(), "23:59:59")) ~ "noche"
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


# Datos calle - cuestonario nuevo
dashboard_calle_nuevo <- datos_calle_nuevo %>%
  mutate(
    department = Departamento,
    municipality = `Municipio del Departamento de Guatemala`,
    zone = `Zona de la Ciudad de Guatemala`,
    place = `Nombre del lugar o alguna referencia`,
    harassment_location = paste0(
      ifelse(
        !is.na(Avenida),
        paste0(gsub(" *avenida *", "", Avenida, ignore.case = TRUE), " avenida "),
        ""
      ),
      ifelse(
        !is.na(Calle),
        paste0(gsub(" *calle *", "", Calle, ignore.case = TRUE), " calle "),
        ""
      ),
      ifelse(!is.na(zone), paste0(zone, " "), ""),
      ifelse(!is.na(municipality), paste0(municipality, ", "), ""),
      ifelse(!is.na(department), department, ""),
      ifelse(!is.na(place), paste0(place, " "), "")
    )
  ) %>%
  select(
    timestamp = `Marca temporal`,
    reporter_gender = Sexo,
    reporter_orientation = `Orientación sexual`,
    reporter_age = `Edad (en años)`,
    reporter_effect = `¿Qué sentiste?`,
    harasser_sex = `En el incidente de acoso, la persona acosadora era:`,
    harasser_age = `¿Qué edad aproximada dirías que tiene la persona acosadora?`,
    harassment_location,
    department, municipality, zone,
    harassment_place = `Tipo de lugar`,
    harassment_type = `Tipos de acoso`,
    harassment_frequency = `¿Cada cuánto tiempo sufres alguna forma de acoso?`
  ) %>%
  # Fix coding
  mutate(
    timestamp = lubridate::dmy_hms(timestamp),
    harassment_date = timestamp %>% as.Date(),
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
      "19-24" = "19 - 24",
      "25-29" = "25 - 29",
      "30-34" = "30 - 34",
      "35-39" = "35 - 39",
      "40-44" = "40 - 44",
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


# Join both old and new dataset ----
dashboard_calle <- dashboard_calle_anterior %>%
  bind_rows(dashboard_calle_nuevo)


# Manual data check
if(interactive()){
  # Compare available variables in new and old dataset
  list(
    mutate(dashboard_calle_anterior, .set = "old"),
    mutate(dashboard_calle_nuevo, .set = "new")
  ) %>%
    map(
      ~ .x %>%
        slice(1) %>%
        mutate_at(
          vars(-matches(".set")),
          funs(paste(class(.), collapse = ";"))
        )
    ) %>%
    bind_rows() %>%
    gather(variable, class, -.set) %>%
    spread(.set, class) %>%
    print(n = Inf)
  
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
dashboard_transporte_anterior <- datos_transporte %>%
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
        tibble(
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


dashboard_transporte_nuevo <- datos_transporte_nuevo %>%
  select(
    timestamp = `Marca temporal`,
    reporter_age = `Edad (en años)`,
    times_harassed = `Durante toda tu vida, ¿Cuántas veces has sido víctima de acoso sexual callejero en el transporte público?`,
    harassment_type = `¿Qué sucedió cuando sufriste acoso sexual callejero en el trasporte público?`,
    reporter_reaction = `Comúnmente, cuando alguna de esas situaciones te ocurre ¿qué haces en el momento?`,
    transport_type = `¿En qué tipo de transporte has sido víctima de acoso?`,
    transport_route1 = `Líneas del Transmetro`,
    transport_route2 = `Transurbano (Sur, Norte y otras)`,
    transport_route3 = `Otras rutas o medios de transporte.`,
    days_use = `¿Qué medio(s) de transporte usa con más frecuencia?`
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
      sub("Extraurbanos San Juan", "extraurbanos", .) %>%
      strsplit(split = ", *"),
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
  ) %>%
  print()


dashboard_transporte <- dashboard_transporte_anterior %>%
  bind_rows(
    dashboard_transporte_nuevo
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
    mutate(
      harassment_location = sub(" *$", "", harassment_location),
      timestamp = timestamp %>%
        lubridate::floor_date(unit = "minute") %>%
        as.character()
    ) %>%
    anti_join(
      locations %>%
        mutate(
          timestamp = timestamp %>%
            lubridate::floor_date(unit = "minute") %>%
            as.character()
        )
    ) %>%
    # mutate(
    #   location = map(harassment_location, ggmap::geocode, source = "google")
    # ) %>%
    unnest() %>%
    print()
  
  if(nrow(new_locations) > 0){
    
    new_locations %>%
      write_csv("locations.csv")
    
    stop("Revisar manualmente las ubiaciones en el archivo \"locations.csv\"")
    
    new_locations <- read_csv(file = "locations.csv")
    
    unlink(x = "locations.csv")
    
    # Update available locations
    locations <- bind_rows(locations, new_locations)
  }
  
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
