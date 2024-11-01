# description ------------------------------------------------------------

# 2024-11-02

# "30DayMapChallenge classic: A map with focus on lines. Roads, rivers, routes, or borders—this day is all about mapping connections and divisions. Another traditional way to keep things moving."

# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite", "RcppSimdJson", "mapview", "leafgl")
pak::pkg_install(packages, upgrade = FALSE, ask = FALSE)
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
rm(packages)


# experiment -------------------------------------------------------------

os <- spod_get("overnight_stays", zones = "distr", dates = "2024-04-17") |> collect()

os |> nrow()
os |> group_by(id_overnight_stay, id_residence) |> summarise(n_persons = sum(n_persons), .groups = "drop") |> nrow()

glimpse(os)

od::coords_to_od()


# get zones and add the aggregation results ------------------------------

zones_distr_v2_centroids <- spod_get_zones("distr", ver = 2) |> 
  filter(!grepl("FR.*|PT.*", id)) |> 
  st_point_on_surface() |> 
  st_transform(4326) |>
  select(id)



# get incidents ----------------------------------------------------------

incidents <- RcppSimdJson::fload("cache/BuscarElementos.json")
incidents_sf <- st_as_sf(incidents, coords = c("lng", "lat"), crs = 4326)
mapviewOptions(platform = "leafgl")
mapview(incidents_sf)


# incidents <- httr("https://infocar.dgt.es/etraffic/BuscarElementos?latNS=44&longNS=5&latSW=27&longSW=-19&zoom=4&accion=getElementos&Camaras=true&SensoresTrafico=true&SensoresMeteorologico=true&Paneles=true&Radares=true&IncidenciasRETENCION=true&IncidenciasOBRAS=false&IncidenciasMETEOROLOGICA=true&IncidenciasPUERTOS=true&IncidenciasOTROS=true&IncidenciasEVENTOS=true&IncidenciasRESTRICCIONES=true&niveles=true&caracter=acontecimiento")
