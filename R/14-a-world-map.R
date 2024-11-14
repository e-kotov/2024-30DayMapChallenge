# description ------------------------------------------------------------

# 2024-11-14

# "Map the whole world. Whether it’s continents, ecosystems, or oceans, this is the day to map the entire planet."

# setup packages ---------------------------------------------------------


if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "rostemplate", "fs", "mapgl", "terra", "rnaturalearth", "base64enc", "htmlwidgets")
pak::pkg_install(packages, upgrade = TRUE, ask = FALSE)
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
rm(packages)






# get rivers -------------------------------------------------------------

ocean <- ne_download(
  scale = 50,
  type = "ocean",
  category = "physical")


# malert -----------------------------------------------------------------

library(mosquitoR)
malert_cache_file <- "cache/14-a-world-map/malert.rds"
if (!file_exists(malert_cache_file)) {
  if(!dir_exists(path_dir(malert_cache_file))){
    dir_create(path_dir(malert_cache_file), recurse = TRUE)
  }
  malert_reports = get_malert_data(source = "github")
  
  malert_reports_sf <- malert_reports |>
    select(version_UUID, lat, lon, creation_time, type, movelab_annotation.classification) |> 
    filter(!is.na(lat), !is.na(lon)) |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

  saveRDS(malert_reports_sf, malert_cache_file)
}

tiger_mosquito_reports_sf <- readRDS(malert_cache_file) |>
  st_transform(st_crs(ocean)) |> 
  filter(type == "adult") |> 
  filter(movelab_annotation.classification == "albopictus")

tiger_mosquitoes <- tiger_mosquito_reports_sf[ocean,]

other_mosquitoes_sf <- readRDS(malert_cache_file) |> 
  st_transform(st_crs(ocean)) |>
  filter(type == "adult") |> 
  filter(movelab_annotation.classification != "albopictus")

other_mosquitoes <- other_mosquitoes_sf[ocean,]


# map --------------------------------------------------------------------

tiger_map <- mapboxgl(
  projection = "globe",
  style = carto_style("dark-matter")) |> 
  mapgl::add_circle_layer(
    id = "tiger",
    source = tiger_mosquitoes,
    circle_color = "red",
  ) |> 
    mapgl::add_legend(
      legend_title = "Tiger Mosquito reports<br>over the past 10 years",
      type = "categorical",
      sizes = 20,
      colors = c("red"),
      circular_patches = TRUE,
      values = c(""),
      position = "top-left"
    )

other_map <- mapboxgl(
  projection = "globe",
  style = carto_style("dark-matter")) |> 
  add_circle_layer(
    id = "other",
    source = other_mosquitoes,
    circle_color = "pink",
  ) |> 
    mapgl::add_legend(
      legend_title = "All other Mosquito reports<br>over the past 10 years",
      type = "categorical",
      sizes = 20,
      colors = c("pink"),
      circular_patches = TRUE,
      values = c(""),
      position = "top-right"
    )


map_14_a_world_map <- mapgl::compare(tiger_map, other_map)

# map_14_a_world_map


# mosquito alert logo ----------------------------------------------------

ma_logo_url <- "https://www.mosquitoalert.com/wp-content/uploads/2017/01/logoMA-1.png"
ma_logo_base64 <- dataURI(file = ma_logo_url, mime = "image/png")


html_overlay <- sprintf(
  '<style>
     body {
         background-color: #545658;
     }
     .floating-image1 {
         position: absolute;
         top: 140px;
         left: 20px;
         width: 180px;
         z-index: 1000;
         pointer-events: none;
     }
   </style>
   <img src="%s" class="floating-image1" alt="Overlay Image 1">',
  ma_logo_base64
)

map_with_overlay <- onRender(
  map_14_a_world_map,
  paste0(
    "function(el, x) {",
    "  var overlay = `", html_overlay, "`;",
    "  document.getElementById(el.id).insertAdjacentHTML('beforeend', overlay);",
    "}"
  )
)

# map_with_overlay

saveWidget(map_with_overlay, "maps/14-a-world-map.html", selfcontained = FALSE)
