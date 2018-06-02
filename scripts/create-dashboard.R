#------------------------------------------------------------------------------*
# Prepare pdf report and dashboard ----
#------------------------------------------------------------------------------*

# Knit and render report to pdf
rmarkdown::render(
  input = "content/indicadores.Rmd",
  output_format = "pdf_document",
  output_file = "reporte.pdf",
  quiet = TRUE,
  clean = TRUE,
  encoding = "UTF8"
)

# Knit and render report to word
rmarkdown::render(
  input = "content/indicadores.Rmd",
  output_format = "word_document",
  output_file = "reporte.docx",
  quiet = TRUE,
  clean = TRUE,
  encoding = "UTF8"
)

# Knit dashboard
rmarkdown::render(input = "content/indicadores.Rmd", encoding = "UTF8")
