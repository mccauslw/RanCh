library(rmarkdown)
library(here)

render(
  input = here("vignettes", "main_vignette.Rmd"),
  output_format = "pdf_document",
  output_file = "main_vignette.pdf",       # stays in vignettes/
  output_dir = here("vignettes"),
  envir = new.env()                         # clean environment
)
