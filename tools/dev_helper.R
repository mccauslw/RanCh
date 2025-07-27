refresh_pkg <- function() {
  devtools::document()
  devtools::install(clean = TRUE)
}
