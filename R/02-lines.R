# description ------------------------------------------------------------

# 2024-11-02

# "30DayMapChallenge classic: A map with focus on lines. Roads, rivers, routes, or borders—this day is all about mapping connections and divisions. Another traditional way to keep things moving."


# notes ------------------------------------------------------------------

# Thanks to Virgilio Gómez-Rubio https://x.com/precariobecario
# Thanks to Mireia Camacho https://x.com/mireiacamacho75 https://github.com/DataMirai/

# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite", "arcpullr", "tabulapdf", "rJavaEnv")
pak::pkg_install(packages, upgrade = FALSE, ask = FALSE)
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
rm(packages)



# cache folder -----------------------------------------------------------

cache_dir_path <- "./cache/02-lines/"
if (!dir.exists(cache_dir_path)) {
  dir.create(cache_dir_path, recursive = TRUE)
}

# download pdf with list of affected roads -------------------------------

cached_pdf_path <- "cache/02-lines/CarreterasCortadasInundaciones.pdf"
pdf_url <- "https://www.dgt.es/estaticos/movilidad/CarreterasCortadasInundaciones.pdf" # was relevant as of 2024-11-02
if (!file.exists(cached_pdf_path)) {
  download.file(pdf_url, destfile = cached_pdf_path)  
}

cached_csv_nov_02 <- "cache/02-lines/2024-11-02-affected_roads.csv"
if (!file.exists(cached_csv_nov_02)) {
  # make sure java is available for tabulapdf with rJavaEnv
  use_java(21)
  
  # extract and bind tables
  tables <- extract_tables(cached_pdf_path)
  affected_roads_nov_02 <- do.call(bind_rows, tables) |> 
    filter(!is.na(CARRETERA))
  
  write_csv2(affected_roads_nov_02, cached_csv_nov_02)
}

affected_roads_nov_02 <- read_csv2(cached_csv_nov_02)
affected_road_names_nov_02 <- unique(affected_roads_nov_02$CARRETERA) |>
  str_extract("\\b[A-Z]+-\\d+\\b")

# download affected roads as of Oct 31 -----------------------------------
cached_csv_oct_31 <- "cache/02-lines/2024-10-31-affected_roads.csv"
if (!file.exists(cached_csv_oct_31)) {
  download_oct_31 <- "https://github.com/DataMirai/ShinyCarreterasInundaciones/raw/refs/heads/main/carreteras_cortadas.csv"
  download.file(download_oct_31, destfile = cached_csv_oct_31)
}

affected_roads_oct_31 <- read_csv2(cached_csv_oct_31)
affected_road_names_oct_31 <- unique(affected_roads_oct_31$CARRETERA) |> 
  str_extract("\\b[A-Z]+-\\d+\\b")


# get road segments ------------------------------------------------------

arcgis_server_base_url <- "https://services1.arcgis.com/nCKYwcSONQTkPA4K/arcgis/rest/services/Tramos_de_carreteras/FeatureServer/0/"

affected_roads_sf_oct_31_file <- "cache/02-lines/2024-10-31-affected_roads_sf.rds"
if (!file.exists(affected_roads_sf_oct_31_file)) {
  affected_roads_sf_oct_31 <- get_spatial_layer(
    url = arcgis_server_base_url,
    where = paste0(
      "nombre IN ('",
      paste(affected_road_names_oct_31, collapse = "','"),
      "')"),
    sf_type = "esriGeometryPolyline"
  )
  saveRDS(affected_roads_sf_oct_31, affected_roads_sf_oct_31_file)
}

affected_roads_sf_oct_31 <- readRDS(affected_roads_sf_oct_31_file)


affected_roads_sf_nov_02_file <- "cache/02-lines/2024-11-02-affected_roads_sf.rds"
if (!file.exists(affected_roads_sf_nov_02_file)) {
  affected_roads_sf_nov_02 <- get_spatial_layer(
    url = arcgis_server_base_url,
    where = paste0(
      "nombre IN ('",
      paste(affected_road_names_nov_02, collapse = "','"),
      "')"),
    sf_type = "esriGeometryPolyline"
  )
  saveRDS(affected_roads_sf_nov_02, affected_roads_sf_nov_02_file)
}

affected_roads_sf_nov_02 <- readRDS(affected_roads_sf_nov_02_file)

affected_roads_sf_oct_31_less_nov_02 <- affected_roads_sf_oct_31 |>
  filter(!nombre %in% affected_roads_sf_nov_02$nombre) |> 
  mutate(date = "2024-10-31")

all_affected_roads <- bind_rows(
  affected_roads_sf_oct_31_less_nov_02,
  affected_roads_sf_nov_02 |> mutate(date = "2024-11-02")
)


# get Spanish provinces and move the roads data for Canary Islands -------

spain <- esp_get_country(moveCAN = TRUE, epsg = "4326")
provinces <- esp_get_prov(moveCAN = FALSE, epsg = "4326")
canary_islands <- provinces |> filter(nuts1.name == "CANARIAS")
affected_roads_canary <- all_affected_roads[canary_islands,] |> esp_move_can()
all_affected_roads_except_for_canary <- all_affected_roads |> 
  filter(!OBJECTID %in% affected_roads_canary$OBJECTID)
all_affected_roads_movedCAN <- rbind(
  affected_roads_canary,
  all_affected_roads_except_for_canary
)


# build plot -------------------------------------------------------------


map_02_lines <- ggplot() +
  geom_sf(data = spain, fill = NA, col = "grey", linewidth = 0.07) +
  # this layer is just for the legend
  geom_sf(data = all_affected_roads_movedCAN, aes(col = date), linewidth = 1.5) +
  # to make sure later dates are on top of earlier ones, plot layers by date one by one, otherwise 'natural' order of objects in the dataset may affect the drawing order
  geom_sf(data = all_affected_roads_movedCAN |> filter(date == "2024-10-31"), col = "#0088d9", linewidth = 1.5) +
  geom_sf(data = all_affected_roads_movedCAN |> filter(date == "2024-11-02"), col = "#d8be02", linewidth = 1.5) +
  scale_color_manual(
    values = c(
      "2024-10-31" = "#0088d9",
      "2024-11-02" = "#d8be02"),
    labels = c(
      "2024-10-31" = "<span style='color:#0088d9'>2024-10-31</span>",
      "2024-11-02" = "<span style='color:#d8be02'>2024-11-02</span>"),
    name = "<span style='color:grey70'>Carreteras<br>Cortadas</span></b>"
  ) +
  labs(
    title = "Restablecimiento gradual<br>de la conectividad vial tras el paso de DANA",
    subtitle = "Segmentos de carretera afectados pueden ser más cortos.\n Consulte los intervalos exactos de kilómetros en https://www.dgt.es/",
    caption = "<i>En solidaridad con las víctimas y sus familias</i><br><br>Autor: Egor Kotov | #30DayMapChallenge | Día 2: Líneas<br>Créditos a Virgilio Gómez-Rubio ( @precariobecario ) y Mireia Camacho ( @mireiacamacho75 )<br>por señalarme los datos y capturar las carreteras afectadas al 31 de octubre"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto", size = 18),
    panel.background = element_rect(fill = "grey5", color = NA),
    plot.background = element_rect(fill = "grey5", color = NA),
    legend.text = element_markdown(),
    legend.title = element_markdown(),
    plot.title = element_markdown(hjust = 0.5, color = "grey80"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey80", size = 12, face = "italic"),
    plot.caption = element_markdown(size = 9, hjust = 0.5, color = "grey70"),
    plot.margin = margin(t = 20, r = 5, b = 20, l = 5),
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.6)
  )  


# print(map_02_lines)

# save map to png --------------------------------------------------------

ggsave(
  filename = "maps/02-lines.png",
  map_02_lines,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300,
  create.dir = TRUE
)


# opmimise png with resmush.it -------------------------------------------

resmush_file("maps/02-lines.png", overwrite = TRUE)

# no SVG this time, otherwise it is going to be too large
