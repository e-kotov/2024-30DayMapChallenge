# description ------------------------------------------------------------

# 2024-11-03

# "30DayMapChallenge classic: A map with polygons. Regions, countries, lakes—this day is for defined shapes that fill space."


# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite", "arcpullr", "RcppSimdJson", "fs", "e-kotov/cartogram@cdec44b", "Cairo")
pak::pkg_install(packages, upgrade = FALSE, ask = FALSE)

extract_pkg_name <- function(pkg) {
  if (grepl("/", pkg) && grepl("@", pkg)) {
    # Extract text between '/' and '@'
    pkg_name <- sub("^.*/(.*?)@.*$", "\\1", pkg)
  } else {
    # Take the whole string if '/' and '@' are not present
    pkg_name <- pkg
  }
  return(pkg_name)
}

# Cleaned package names
clean_packages <- sapply(packages, extract_pkg_name)

# Load the packages without startup messages
suppressPackageStartupMessages(invisible(lapply(clean_packages, library, character.only = TRUE)))

rm(packages, clean_packages)


# get the data from IGN --------------------------------------------------
# no time to automate this
# https://centrodedescargas.cnig.es/CentroDescargas/buscar.do?filtro.codFamilia=REDTR#
# extract all archives and link them to cache/spain/


# cache folder -----------------------------------------------------------

cache_dir_path <- "./cache/03-polygons/"
if (!dir.exists(cache_dir_path)) {
  dir.create(cache_dir_path, recursive = TRUE)
}


# download affected roads as of Oct 31 -----------------------------------
cached_csv_oct_31 <- "cache/03-polygons/2024-10-31-affected_roads.csv"
if (!file.exists(cached_csv_oct_31)) {
  download_oct_31 <- "https://github.com/DataMirai/ShinyCarreterasInundaciones/raw/refs/heads/main/carreteras_cortadas.csv"
  download.file(download_oct_31, destfile = cached_csv_oct_31)
}

provinces <- esp_get_prov(moveCAN = FALSE, epsg = "4326") |>
  st_drop_geometry() |> 
  select(ine.prov.name, cpro)

affected_roads_oct_31 <- read_csv2(cached_csv_oct_31) |> 
  mutate(nombre = str_extract(CARRETERA, "\\b[A-Z]+-\\d+\\b")) |> 
  relocate(nombre, .before = CARRETERA) |> 
  rename('PK_INI' = 'PK INI', 'PK_FIN' = 'PK FIN') |> 
  select(-CARRETERA) |> 
  left_join(provinces, by = c("PROVINCIA" = "ine.prov.name")) |>
    mutate(cpro = if_else(grepl("Valencia", PROVINCIA), true = "46", false = cpro)) |>
    mutate(cpro = if_else(grepl("Castelló/Castellón", PROVINCIA), true = "12", false = cpro))
affected_road_names_oct_31 <- unique(affected_roads_oct_31$nombre)



# get road segments ------------------------------------------------------

roads_cache_path <- "cache/03-polygons/roads_sf.rds"
if(!file.exists(roads_cache_path)) {
  # get all roads that are mentioned in the original 31 DGT pdf
  # filter each province gpkg to the required road names
  gpkg_list <- dir_ls("cache/spain/", regexp = "red_viaria\\.gpkg$", recurse = TRUE)
  
  roads_list <- gpkg_list |> 
    map(
      ~ st_read(dsn = path_real(.x),
      query = sql(paste0(
        "SELECT *
          FROM rt_tramo_vial
          WHERE nombre IN ('", paste(affected_road_names_oct_31, collapse = "', '"), "')"
        )),
        quiet = TRUE
      ),
      .progress = TRUE
    )
  
  roads_sf <- do.call(rbind, roads_list)
  provinces <- esp_get_prov(moveCAN = FALSE, epsg = "4258")
  roads_sf <- roads_sf |>
    cbind(roads_sf |> 
      select(id_tramo) |>
      st_centroid() |> 
      st_join(provinces |> select(cpro)) |> 
      st_drop_geometry() |> 
      select(cpro) |>
      as_tibble()
    ) |> 
    remove_rownames() |> 
    filter(!is.na(cpro))
  # roads_sf |> filter(is.na(cpro)) # sanity check
  # roads_sf |> filter(is.na(cpro)) |> st_length() # there are 3 of them and all zero length, so ok to drop them

  saveRDS(roads_sf, roads_cache_path)
}

roads_sf <- readRDS(roads_cache_path)
# all(affected_road_names_oct_31 %in% roads_sf$nombre) # sanity check




# create road-name road-code pairs ---------------------------------------

road_codes_cache_file <- "cache/03-polygons/road_codes_tbl.rds"
if(!file.exists(road_codes_cache_file)) {
  api_road_codes_per_prov <- "https://infocar.dgt.es/etraffic/ShareAjax?accion=getRoad4Provincia&codProv="
  
  required_provinces <- unique(na.omit(roads_sf$cpro))
  
  road_codes_list <- required_provinces |> 
    map(~ fload(paste0(api_road_codes_per_prov, .x), max_simplify_lvl = "data_frame"), .progress = TRUE)
  
  road_codes_tbl <- do.call(bind_rows, road_codes_list) |> as_tibble()
  saveRDS(road_codes_tbl, road_codes_cache_file)
}
road_codes_tbl <- readRDS(road_codes_cache_file)

# match back the provinces from road names from sf object ----------------

road_codes_tbl_for_queries <- road_codes_tbl |> 
  select(-poblacion) |> 
  distinct() |> 
  left_join(
    roads_sf |>
      st_drop_geometry() |> 
      select(nombre, cpro) |> 
      distinct() |>
      as_tibble(),
    by = c("carrNombre" = "nombre"),
    relationship = "many-to-many"
  ) |> 
  na.omit() |> # if not matched, we are not intereseted as these roads were not affected
  select(nombre = carrNombre, codCarretera = carrCodigo, cpro)



# prepare the km intervals for querying ----------------------------------

exact_km_intervals_for_queries <- 
  affected_roads_oct_31 |> 
  select(
    cpro,
    nombre,
    PK_INI,
    PK_FIN
  ) |> 
  left_join(road_codes_tbl_for_queries, by = c("nombre", "cpro"), relationship = "many-to-many")

# create more sample points ----------------------------------------------

exact_km_intervals_for_queries <- exact_km_intervals_for_queries |> 
  mutate(
    segment_id = row_number(),
    distance = PK_FIN - PK_INI
  ) |> 
  # Swap PK_INI and PK_FIN where distance is negative
  mutate(
    temp_PK_INI = ifelse(distance < 0, PK_FIN, PK_INI),
    temp_PK_FIN = ifelse(distance < 0, PK_INI, PK_FIN),
    PK_INI = temp_PK_INI,
    PK_FIN = temp_PK_FIN
  ) |> 
  select(-temp_PK_INI, -temp_PK_FIN) |> 
  mutate(distance = PK_FIN - PK_INI)


# Function to create additional rows at every X km interval, rounding up the last interval to match PK_FIN
generate_intervals <- function(row, interval, min_distance) {
  if (row$distance > min_distance) {
    intervals <- seq(from = row$PK_INI, by = interval, to = row$PK_FIN)
    
    # If the last interval doesn't match PK_FIN, adjust the last segment
    if (tail(intervals, 1) != row$PK_FIN) {
      intervals <- c(intervals, row$PK_FIN)
    }
    
    data.frame(
      cpro = row$cpro,
      nombre = row$nombre,
      PK_INI = intervals[-length(intervals)],
      PK_FIN = intervals[-1],
      codCarretera = row$codCarretera,
      segment_id = row$segment_id,
      distance = c(rep(interval, length(intervals) - 2), row$PK_FIN - intervals[length(intervals) - 1])
    )
  } else {
    # Return the original row if the distance is not greater than Y
    return(data.frame(row))
  }
}

# Apply the function to each row and bind the results
expanded_km_intervals <- exact_km_intervals_for_queries |> 
  rowwise() |> 
  do(generate_intervals(., interval = 0.5, min_distance = 1.1)) |> 
  ungroup()


# query coorindates ------------------------------------------------------

km_coords_tbl_cache_path <- "cache/03-polygons/km_coords_tbl.rds"
if(!file.exists(km_coords_tbl_cache_path)) {
  get_road_coords_from_km <- function(tbl_to_match, sleep = 0.5) {
    # tbl_to_match <- expanded_km_intervals[1,]
    # tbl_to_match <- expanded_km_intervals[115,]
    # tbl_to_match <- expanded_km_intervals[124,]
    api_road_km_coords <- "https://infocar.dgt.es/etraffic/BuscarElementos?accion=centrar&" # provincia=46&codCarretera=59960&PK=371
    
    start_coords <- fload(
      paste0(api_road_km_coords,
        "provincia=", tbl_to_match$cpro,
        "&codCarretera=", tbl_to_match$codCarretera,
        "&PK=", tbl_to_match$PK_INI),
        max_simplify_lvl = "data_frame") |>
      as_tibble()
    
    if(tbl_to_match$PK_INI == tbl_to_match$PK_FIN) {
      end_coords <- start_coords
    } else {
      Sys.sleep(sleep)
      end_coords <- fload(
        paste0(api_road_km_coords,
          "provincia=", tbl_to_match$cpro,
          "&codCarretera=", tbl_to_match$codCarretera,
          "&PK=", tbl_to_match$PK_FIN),
          max_simplify_lvl = "data_frame") |>
        as_tibble()
    }

    if (nrow(start_coords) == 1 & nrow(end_coords) == 1) {
      tbl_matched <- tbl_to_match |> 
        mutate(
          PK_INIT_lng = start_coords$lng,
          PK_INIT_lat = start_coords$lat,
          PK_FIN_lng = end_coords$lng,
          PK_FIN_lat = end_coords$lat
        )
      return(tbl_matched)
    } else {
      return(tbl_to_match)
    }
  }
  
  # testing
  # i <- 124
  # test_1 <- get_road_coords_from_km(
  #   expanded_km_intervals[i,]
  # )
  # test_1
  
  
  # takes about 8-10 minutes to avoid hitting the rate limit
  km_coords_list <- 1:nrow(expanded_km_intervals) |>
    map(~ get_road_coords_from_km(
      expanded_km_intervals[.x,],
      sleep = 0.2
    ), .progress = TRUE)
  
  km_coords_tbl <- do.call(bind_rows, km_coords_list) |> as_tibble()
  km_coords_tbl <- km_coords_tbl[complete.cases(km_coords_tbl),]

  saveRDS(km_coords_tbl, km_coords_tbl_cache_path)
}
km_coords_tbl <- readRDS(km_coords_tbl_cache_path)

# create lines to match with roads ---------------------------------------

km_coords_sf <- km_coords_tbl |> 
  rowwise() |> 
  mutate(
    geometry = st_sfc(st_linestring(matrix(
      c(PK_INIT_lng, PK_INIT_lat, PK_FIN_lng, PK_FIN_lat),
      ncol = 2, byrow = TRUE
    )), crs = 4326)
  ) |> 
  ungroup() |> 
  st_as_sf(sf_column_name = "geometry")
# mapview(km_coords_sf)


# buffer and select from all actual roads --------------------------------

km_coords_sf_buffers <- km_coords_sf |> 
  st_transform(3035) |> 
  st_buffer(40) |> 
  st_transform(4258)

selected_roads <- roads_sf[km_coords_sf_buffers,]
# mapviewOptions(platform = "leafgl")
# mapview(selected_roads) + mapview(km_coords_sf_buffers, col.regions = "red")



# MITMS data -------------------------------------------------------------

districts_sf <- spod_get_zones("distr", ver = 2) |> 
  filter(!grepl("FR.*|PT.*", id))
selected_roads <- st_transform(selected_roads, st_crs(districts_sf))

# mark the zones with affected roads
districts_affected <- districts_sf[selected_roads,] |>
  mutate(affected = TRUE)
districts_classified <- districts_sf |> 
  filter(!id %in% districts_affected$id) |> 
  mutate(affected = FALSE) |>
  rbind(districts_affected)


# move Canary islands ----------------------------------------------------

districts_classified_for_plot <- rbind(
  # all zones except for Canary Islands
  districts_classified |> 
    filter(!grepl("^38|^35", id)),
  # Canary Islands moved closer to mainland Spain
  esp_move_can(
    districts_classified |> filter(grepl("^38|^35", id))
    )
  ) |> 
  st_simplify(preserveTopology = TRUE, dTolerance = 100)



# get number of trips ----------------------------------------------------

districts_with_trips_for_plot_save_path <- "cache/03-polygons/districts_with_trips_for_plot.rds"
if(file.exists(districts_with_trips_for_plot_save_path)) {
  od <- spod_get(
    type = "origin-destination",
    zones = "distr",
    dates = c("2023-10-25") # roughly equivalent to 2024-10-30
  )

  trips_by_origin <- od |> 
    group_by(id_origin) |> 
    summarise(total_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
    collect()

  districts_with_trips_for_plot <- districts_classified_for_plot |> 
    left_join(trips_by_origin, by = c("id" = "id_origin")) |> 
    mutate(total_trips = if_else(is.na(total_trips), 1, total_trips))

  saveRDS(districts_with_trips_for_plot, districts_with_trips_for_plot_save_path)
}
districts_with_trips_for_plot <- readRDS(districts_with_trips_for_plot_save_path)

# cartogram --------------------------------------------------------------

affected_cargotram <- districts_with_trips_for_plot |>
  filter(affected) |>
  cartogram_cont(weight = "total_trips", itermax = 15)
# mapview(affected_cargotram)


# the share of potentially disrupted trips -------------------------------

share_num_disrupted <- districts_with_trips_for_plot |> 
  st_drop_geometry() |> 
  group_by(affected) |>
  summarise(total_trips = sum(total_trips, na.rm = TRUE), .groups = "drop") |>
  mutate(share_disrupted = round(total_trips / sum(total_trips) * 100, 0)) |> 
  filter(affected)


# prepare paths to logos -------------------------------------------------

spod_logo <- system.file("help/figures", "logo.png", package = "spanishoddata")
ropenspain_logo <- system.file("help/figures", "logo.png", package = "rostemplate")

# generate plot ----------------------------------------------------------


map_03_polygons <- ggplot() +
  geom_sf(data = esp_get_country(),
          fill = "grey90", col = "grey20", linewidth = 0.03) +
  geom_sf(data = affected_cargotram,
          # aes(fill = total_trips),
          fill = "grey30",
          col = "grey80",
          linewidth = 0.2) +
  # scale_fill_viridis_c(option = "inferno", name = "Viajes") +
  geom_image(data = tibble(x = 3, y = 37),
    aes(image = spod_logo, x = x, y = y), size = 0.3) +
  geom_image(data = tibble(x = 2.05, y = 35),
    aes(image = ropenspain_logo, x = x, y = y), size = 0.05) +
  annotate("text", x = 2.55, y = 35, label = "rOpenSpain", size = 3, hjust = 0, color = "grey30") +
  labs(
    title = paste0("Hasta el ", share_num_disrupted$share_disrupted, "% de la Movilidad Diaria<br>Nacional Interrumpida por la DANA"),
    subtitle = "Tamaño del municipio escalado según<br>el número de viajes potencialmente afectados",
    caption = "<i>En solidaridad con las víctimas y sus familias</i><br><br>Autor: Egor Kotov | #30DayMapChallenge | Día 3: Polígonos<br>Paquete R para acceder a los datos: ropenspain.github.io/spanishoddata/<br>Fuentes de datos: Ministerio de Transportes y Movilidad Sostenible (MITMS); Nommon;<br>Dirección General de Trafico (DGT); Instituto Geográfico Nacional<br>Basado en los datos de cierres de carreteras de la DGT el 31 de octubre de 2024<br>y en los datos de movilidad típica del MITMS/Nommon<br>en un día comparable el 25 de octubre de 2023."
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto", size = 18),
    panel.background = element_rect(fill = "grey70", color = NA),
    plot.background = element_rect(fill = "grey70", color = NA),
    legend.text = element_markdown(size = 8),
    legend.title = element_markdown(size = 10),
    plot.title = element_markdown(hjust = 0.5, color = "grey30"),
    plot.subtitle = element_markdown(hjust = 0.5, color = "grey40", size = 12, face = "italic"),
    plot.caption = element_markdown(size = 9, hjust = 0.5, color = "grey40"),
    plot.margin = margin(t = 20, r = 5, b = 20, l = 5),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.6),
    legend.key.width  = unit(0.5, "lines"),
    legend.key.size = unit(1.4, "cm"),
    legend.key.height = unit(0.2, "cm")
  )
map_03_polygons


# print(map_03_polygons)

# save map to png --------------------------------------------------------

ggsave(
  filename = "maps/03-polygons.png",
  map_03_polygons,
  width = 7,
  height = 7,
  units = "in",
  dpi = 300,
  create.dir = TRUE
)


# opmimise png with resmush.it -------------------------------------------

resmush_file("maps/03-polygons.png", overwrite = TRUE)


# save to svg ------------------------------------------------------------


ggsave(
  filename = "maps/03-polygons.svg",
  map_03_polygons,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300,
  create.dir = TRUE
)











# below is archived info for when i was working with the roads data on ESRI, but it is outdated ----


# # affected road list -----------------------------------------------------
# # as of 31 Oct - 2 Nov, see code in 02-lines.R
# affected_roads_ids <- c("CA-3113", "CA-5101", "CA-6101", "CA-9101", "A-4200", "GR-4104", 
# "SE-4104", "A-1501", "Z-453", "CM-215", "GU-952", "GU-958", 
# "CV-130", "CV-134", "CV-137", "CV-1486", "CV-200", "A-7", "CV-33", 
# "CV-336", "CV-36", "CV-374", "CV-377", "CV-378", "CV-379", "CV-380", 
# "CV-382", "CV-383", "CV-390", "CV-391", "CV-395", "CV-401", "CV-403", 
# "CV-413", "CV-416", "CV-42", "CV-422", "CV-424", "CV-425", "CV-426", 
# "CV-427", "CV-429", "CV-431", "CV-448", "CV-472", "CV-473", "CV-50", 
# "CV-505", "CV-509", "CV-511", "CV-512", "CV-515", "N-3", "N-322", 
# "V-30", "V-31", "A-314", "A-372", "A-384", "A-393", "A-408", 
# "AP-4", "CA-3101", "CA-3102", "CA-3108", "CA-3205", "CA-4107", 
# "CA-5200", "CA-6105", "CA-6200", "SE-3102", "SE-3105", "SE-3201", 
# "SE-4108", "SE-6300", "TE-28", "A-1105", "A-3", "CV-10", "CV-148", 
# "AP-7", "CV-345", "CV-354", "CV-370", "CV-372", "CV-396", "CV-400", 
# "CV-415", "CV-417", "CV-421", "CV-43", "CV-435", "CV-470", "CV-506", 
# "CV-507", "CV-508", "CV-513", "CV-520", "CV-522", "CV-523", "CV-525", 
# "CV-543", "CV-544", "CV-545", "CV-557", "CV-573", "CV-575", "CV-576", 
# "CV-577", "CV-603", "CV-612", "N-330", "N-332")


# failing roads "CV-413" "CV-426" "CV-448" "CV-544" "CV-577" "N-3" 
# manual checks result in the following referene:
# 'N-3' - 'N-III'
# 'CV-577' - 'CV-5751'
# 'CV-577' - 'CV-5752'
# 'CV-544' - 'CV-5446'
# 'CV-544' - 'CV-5447'
# 'CV-448' - 'CV-4480'
# 'CV-426' - 'CV-4290'
# 'CV-426' - 'CV-4291'
# 'CV-413' - 'CV-4131'

# # recode so that these roads match when querying
# # list of failing roads
# failing_roads <- c("CV-413", "CV-426", "CV-448", "CV-544", "CV-577", "N-3")

# # replace failing roads with correct references
# replacement_roads <- c("N-III", "CV-5751", "CV-5752", "CV-5446", "CV-5447", "CV-4480", "CV-4290", "CV-4291", "CV-4131")

# # remove failing roads
# updated_road_names <- setdiff(affected_road_names_oct_31, failing_roads)

# # add replacements
# updated_road_names <- c(updated_road_names, replacement_roads)
