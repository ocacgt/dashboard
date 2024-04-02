#------------------------------------------------------------------------------*
# Prepare pdf report and dashboard ----
#------------------------------------------------------------------------------*

# Knit and render report to pdf
rmarkdown::render(
  input = "indicadores.Rmd",
  output_format = "pdf_document",
  output_file = "ocacgt-informeMapeo.pdf",
  quiet = TRUE,
  clean = TRUE,
  encoding = "UTF8"
)

# Knit dashboard
rmarkdown::render(
  input = "indicadores.Rmd",
  output_file = "index.html",
  encoding = "UTF8"
)

# Knit and render report to word
rmarkdown::render(
  input = "indicadores.Rmd",
  output_format = "word_document",
  output_file = "../ocacgt-informeMapeo.docx",
  quiet = TRUE,
  clean = TRUE,
  encoding = "UTF8"
)
