# description ------------------------------------------------------------

# 2024-11-03

# "30DayMapChallenge classic: A map with polygons. Regions, countries, lakes—this day is for defined shapes that fill space."


# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite", "arcpullr", "RcppSimdJson", "fs")
pak::pkg_install(packages, upgrade = FALSE, ask = FALSE)
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
rm(packages)


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

exact_km_intervals_for_queries
expanded_km_intervals



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
mapview(km_coords_sf)


# buffer and select from all actual roads --------------------------------

km_coords_sf_buffers <- km_coords_sf |> 
  st_transform(3035) |> 
  st_buffer(40) |> 
  st_transform(4258)

selected_roads <- roads_sf[km_coords_sf_buffers,]
# mapview(selected_roads) + mapview(km_coords_sf_buffers, col.regions = "red")

















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
