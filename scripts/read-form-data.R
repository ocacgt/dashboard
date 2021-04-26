#------------------------------------------------------------------------------*
# Initial code to explore the USAC data
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")
library(package = "googlesheets4")

# Authenticate with locally available token
googledrive::drive_auth(
  email = "estudiosocacgt@gmail.com",
  cache = "data/gargle-auth"
)

gs4_auth(
  email = "estudiosocacgt@gmail.com",
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = "data/gargle-auth"
)


# read in the archived data ----
if(!file.exists("data/Mapeo Acoso USAC Original.xls")){
  googledrive::drive_find(
    pattern = "Mapeo Acoso USAC Original"
  ) %>%
    googledrive::drive_download(
      path = "data/Mapeo Acoso USAC Original.xls",
      overwrite = TRUE
    )
}

acoso_usac_original <- "data/Mapeo Acoso USAC Original.xls" %>%
  readxl::read_excel() %>%
  select(-matches("[.]{3}[0-9]")) %>%
  set_names(
    names(.) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      tolower() %>%
      sub("[0-9. ]+", "", .) %>%
      gsub("[^0-9a-z ]", "", .) %>%
      gsub(" +", "_", .)
  ) %>%
  mutate_all(
    list(
      ~ iconv(., from = "UTF-8", to = "ISO-8859-1")
    )
  ) %>%
  print()


each_question <- acoso_usac_original %>%
  select(
    -aquac_sentiste, -tipos_de_acoso,
    -aa_quien_fue_dirigida_la_denuncia,
    -si_la_persona_que_te_acosa3_fue_tu_profesora_escribe_su_nombre_te_recordamos_que_esta_encuesta_es_ana3nima,
    -si_fue_dentro_de_un_edificio_distinto_a_tu_unidad_acadacmica_indica_cual_si_respondiste_esta_pregunta_pasa_a_la_seccia3n_3
  ) %>%
  gather(variable, value, -marcatemporal, na.rm = TRUE) %>%
  count(variable, value)

each_question %>%
  select(-n) %>%
  count(variable, sort = TRUE)






# read in newly collected data ----

restored <- gs4_find(
  pattern = "respuesta_restaurada_version_lidia"
) %>%
  read_sheet() %>%
  set_names(
    names(.) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      tolower() %>%
      sub("[0-9. ]+", "", .) %>%
      gsub("[^0-9a-z ]", "", .) %>%
      gsub(" +", "_", .)
  ) %>%
  print()

acoso_usac_nuevos <- gs4_find(
  pattern = "formularioDenuncia.acosoUSAC.abril21_actual"
) %>%
  read_sheet() %>%
  set_names(
    names(.) %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      tolower() %>%
      sub("[0-9. ]+", "", .) %>%
      gsub("[^0-9a-z ]", "", .) %>%
      gsub(" +", "_", .)
  ) %>%
  bind_rows(
    restored
  ) %>%
  print()

rm(restored)


#------------------------------------------------------------------------------*
#' Variables utilizadas ----
#------------------------------------------------------------------------------*

dashboard_usac <- acoso_usac_original %>%
  rename(
    unit = unidad_acadacmica_en_la_que_estudias,
    in_unit = ael_incidente_ocurria3_dentro_o_fuera_de_tu_unidad_acadacmica,
    where = si_fue_dentro_de_tu_unidad_acadacmica_indica_en_quac_parte_si_respondiste_esta_pregunta_pasa_a_la_seccia3n_3,
    other = si_fue_dentro_de_un_edificio_distinto_a_tu_unidad_acadacmica_indica_cual_si_respondiste_esta_pregunta_pasa_a_la_seccia3n_3,
    location = si_no_fue_dentro_de_tu_unidad_acadacmica_u_otra_unidad_ada3nde_fue,
    harasser_sex = en_el_incidente_de_acoso_la_persona_acosadora_era,
    harasser_group = esta_persona_es,
    harasser_age = aquac_edad_aproximada_diraas_que_tiene_la_persona_acosadora,
    harasser_reincident = ahas_sido_acosadao_por_esa_persona_anteriormente,
    harasser_name = si_la_persona_que_te_acosa3_fue_tu_profesora_escribe_su_nombre_te_recordamos_que_esta_encuesta_es_ana3nima,
    sexual_orientation = orientacia3n_sexual,
    frecuencia_acoso = acada_cuanto_sufres_alguna_forma_de_acoso_en_la_universidad,
    efecto_acoso = aquac_sentiste
  ) %>%
  mutate(
    edad = edad %>%
      as.numeric() %>%
      if_else(
        condition = . > 1,
        true = .,
        false = NA_real_
      ),
    sexo = sexo %>%
      recode(
        Lesbiana = "Mujer",
        queer = "Queer"
      ) %>%
      factor(
        levels = c("Mujer", "Queer", "Hombre")
      ),
    harasser_known = if_else(
      condition = !is.na(harasser_name),
      true = "Identidad de acosador conocida",
      false = "Acosador no es reconocido"
    ),
    marcatemporal = marcatemporal %>%
      sub("([ap]). *(m). *.+", "\\L\\1\\2", ., perl = TRUE) %>%
      lubridate::ymd_hms()
  ) %>%
  select(-harasser_name) %>%
  print()

dashboard_usac %>%
  mutate(
    where = if_else(
      condition = grepl("Dentro", in_unit),
      true = unit,
      false = location
    ) %>%
      if_else(
        condition = is.na(.),
        true = "No indicado",
        false = .
      )
  ) %>%
  group_by(in_unit) %>%
  count(in_unit, where) %>%
  arrange(in_unit, desc(n)) %>%
  slice(1:7) %>%
  print(n = Inf)



dashboard_usac_nuevo <- acoso_usac_nuevos %>%
  rename(
    unit = unidadacademica_en_la_que_estudias,
    #in_unit = ael_incidente_ocurria3_dentro_o_fuera_de_tu_unidad_acadacmica,
    where = sifue_dentro_de_tu_unidad_academica_indica_en_que_parte,
    other = sifue_dentro_de_un_edificio_distinto_a_tu_unidad_academica_indica_cual,
    location = sifue_dentro_de_un_edificio_distinto_a_tu_unidad_academica_indica_cual,
    location_sede = encual_sede_de_la_universidad_ocurrio_el_incidente_de_acoso_que_reportas,
    location_zone = acontinuacion_encontraras_un_mapa_de_las_areas_de_la_universidad_tomalo_de_referencia_para_indicar_en_cual_area_ocurrio_el_incidente_de_acoso,
    location_building = cualera_el_edificio_mas_cercano_del_campus_a_donde_ocurrio_el_incidente_de_acoso,
    location_details = escribecualquier_otra_informacion_que_pienses_que_es_importante_para_saber_en_donde_ocurrio_el_incidente_acoso,
    harasser_sex = enel_incidente_de_acoso_la_persona_acosadora_era,
    harasser_group = estapersona_es,
    harasser_age = queedad_aproximada_dirias_que_tiene_la_persona_acosadora,
    harasser_reincident = hassido_acosadao_por_esa_persona_anteriormente,
    harasser_name = sila_persona_que_te_acoso_fue_tu_profesora_escribe_su_nombre_te_recordamos_que_esta_encuesta_es_anonima,
    sexual_orientation = orientacionsexual,
    frecuencia_acoso = cadacuanto_sufres_alguna_forma_de_acoso_en_la_universidad,
    efecto_acoso = quesentiste
  ) %>%
  mutate(
    edad = edad %>%
      as.numeric() %>%
      if_else(
        condition = . > 1,
        true = .,
        false = NA_real_
      ),
    sexo = sexo %>%
      recode(
        Lesbiana = "Mujer",
        queer = "Queer"
      ) %>%
      factor(
        levels = c("Mujer", "Queer", "Hombre")
      ),
    harasser_known = if_else(
      condition = !is.na(harasser_name),
      true = "Identidad de acosador conocida",
      false = "Acosador no es reconocido"
    )
  ) %>%
  select(-harasser_name) %>%
  print()



dashboard_usac <- dashboard_usac %>%
  bind_rows(dashboard_usac_nuevo)



acoso_locations_original <- dashboard_usac %>%
  select(
    marcatemporal,
    unit,
    where,
    other,
    location
  ) %>%
  arrange(is.na(location), is.na(other), is.na(where)) %>%
  print()

acoso_locations_original %>% {
  if(!file.exists("output/locations_ref.xlsx")) {
    list(
      specific = filter(., !is.na(other) | !is.na(location)),
      units = count(., unit)
    ) %>%
      writexl::write_xlsx(path = "output/locations_ref.xlsx")
  }
}

# End of script
