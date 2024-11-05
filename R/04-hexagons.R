# description ------------------------------------------------------------

# 2024-11-04

# "Maps using hexagonal grids. Step away from square grids and try mapping with hexagons. A fun way to show density or spatial patterns."

# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite", "arcpullr", "tabulapdf", "rJavaEnv")
pak::pkg_install(packages, upgrade = FALSE, ask = FALSE)
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
rm(packages)



# cache folder -----------------------------------------------------------

cache_dir_path <- "./cache/04-hexagons/"
if (!dir.exists(cache_dir_path)) {
  dir.create(cache_dir_path, recursive = TRUE)
}
