#------------------------------------------------------------------------------*
# Initial code to explore the USAC data
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")


# read in the data
acoso_usac <- "data/Mapeo Acoso.xls" %>%
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


each_question <- acoso_usac %>%
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

dashboard_usac <- acoso_usac %>%
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
      )
  ) %>%
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


acoso_locations <- dashboard_usac %>%
  select(
    marcatemporal,
    unit,
    where,
    other,
    location
  ) %>%
  arrange(is.na(location), is.na(other), is.na(where)) %>%
  print(n = Inf)

acoso_locations %>% {
  if(!file.exists("output/locations_ref.xlsx")) {
    list(
      specific = filter(., !is.na(other) | !is.na(location)),
      units = count(., unit)
    ) %>%
      writexl::write_xlsx(path = "output/locations_ref.xlsx")
  }
}

# End of script
