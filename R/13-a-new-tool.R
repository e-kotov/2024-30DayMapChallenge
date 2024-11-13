# description ------------------------------------------------------------

# 2024-11-13

# "Use a tool you’ve never tried before. The challenge has always been about trying new things. Use a tool, software, or drawing technique you’ve never worked with before."

# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite", "e-kotov/flowmapper@spatial", "mapgl", "ineAtlas", "fs", "arcpullr", "htmlwidgets", "base64enc")
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



# get impact data

impact_area_file <- "cache/13-a-new-tool/2024-11-08-affected_area.rds"
if(!file_exists(impact_area_file)){
  if(!dir_exists(path_dir(impact_area_file))){
    dir_create(path_dir(impact_area_file), recurse = TRUE)
  }
  impact_data_url <- "https://services8.arcgis.com/mApiu3uMEBwNqKz2/arcgis/rest/services/MapDANA_WFL1/FeatureServer/1"
  
  impacted_area <- get_spatial_layer(url = impact_data_url)
  saveRDS(impacted_area, impact_area_file)
}
impacted_area <- readRDS(impact_area_file)


# get data from INE atlas ------------------------------------------------

tract_income <- get_atlas("income", level = "tract")
tracts <- get_tract_geom(year = 2022) |> filter(province == "Valencia/Valéncia")


tracts_income <- tracts |> 
  left_join(tract_income |> filter(year == 2022) |> select(tract_code, net_income_pc),
            by = "tract_code")


map1 <- maplibre(bounds = tracts_income) |> 
  add_fill_layer(
    id = "income",
    source = tracts_income,
    fill_color = interpolate(
      column = "net_income_pc",
      values = c(min(tracts_income$net_income_pc, na.rm = TRUE), max(tract_income$net_income_pc, na.rm = TRUE)),
    stops = c("lightblue", "darkblue")
    )
  )
map1


# get the flows data -----------------------------------------------------

od_flows <- spod_get(
  type = "od",
  zones = "distr",
  dates = c("2024-10-29")
)


# get the zones ----------------------------------------------------------

districts <- spod_get_zones("dist", ver = 2)


# focus data on valencia -------------------------------------------------

zones_barcelona <- districts |>
  filter(grepl("Barcelona", name, ignore.case = TRUE))

zones_barcelona_fua <- districts[
  st_buffer(zones_barcelona, dist = 10000)
  ,
]



# mapview(zones_barcelona_fua)



# prepare data for flowmap -----------------------------------------------

nodes <- zones_barcelona_fua |>
  st_centroid() |>
  st_transform(4326) |> 
  st_coordinates() |>
  as.data.frame() |>
  mutate(name = zones_barcelona_fua$id) |>
  rename(x = X, y = Y) |> 
  as_tibble()


od <- od_flows |>
  filter(id_origin %in% nodes$name & id_destination %in% nodes$name) |> 
  group_by(o = id_origin, d = id_destination, date, activity_origin, activity_destination) |>
  summarise(value = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
  collect() |> 
  as_tibble()

nodes_flows_hw <- flowmap_sf(
  od = od |>
    filter(activity_origin %in% c("home", "work_or_study") & activity_destination %in% c("home", "work_or_study")),
  nodes = nodes,
  crs = 4326,
  k_nodes = 30
)

hw_min_max <- round(range(nodes_flows_hw$flow, na.rm = TRUE),0)

m_hw <- maplibre(bounds = nodes_flows_hw,
  style = carto_style("positron")) |> 
  add_fill_layer(
    id = "29 Oct HW",
    source = nodes_flows_hw,
    fill_color = interpolate(
      column = "flow",
      values = c(hw_min_max[[1]], hw_min_max[[2]]),
      stops = c("lightblue", "darkblue"),
      na_color = "lightgrey"
    ),
    tooltip = "flow"
  ) |> 
    add_legend(
      "Daily\nHome-Work-Home Trips (thoudands)",
      values = c(round(hw_min_max[[1]],0), round(hw_min_max[[2]]/1000,0)),
      colors = c("lightblue", "darkblue")
    )
# m_hw



nodes_flows_other <- flowmap_sf(
  od = od |>
    filter(activity_origin %in% c("frequent_activityuent", "infrequent_activity") & activity_destination %in% c("frequent_activity", "infrequent_activity")),
  nodes = nodes,
  crs = 4326,
  k_nodes = 30
)

# other_min_max <- round(range(nodes_flows_other$flow, na.rm = TRUE),0)

m_other <- maplibre(bounds = nodes_flows_other,
  style = carto_style("positron")) |> 
  add_fill_layer(
    id = "29 Oct other",
    source = nodes_flows_other,
    fill_color = interpolate(
      column = "flow",
      values = c(hw_min_max[[1]], hw_min_max[[2]]),
      stops = c("lightblue", "darkblue"),
      na_color = "lightgrey"
    ),
    tooltip = "flow"
  ) |> 
    add_legend(
      "Daily\nOther Trips (thoudands)",
      values = c(round(hw_min_max[[1]],0), round(hw_min_max[[2]]/1000,0)),
      colors = c("lightblue", "darkblue"),
      position = "top-right"
    )
# m_other


compare_plot <- compare(m_hw, m_other)


# prepare paths to logos -------------------------------------------------

spod_logo <- system.file("help/figures", "logo.png", package = "spanishoddata")
ropenspain_logo <- system.file("help/figures", "logo.png", package = "rostemplate")
base64_spod <- dataURI(file = spod_logo, mime = "image/png")
base64_ropenspain <- dataURI(file = ropenspain_logo, mime = "image/png")

html_overlay <- sprintf(
  '<style>
     .floating-image1 {
         position: absolute;
         top: 320px;
         right: 20px;
         width: 200px;
         z-index: 1000;
         pointer-events: none;
     }
     .floating-image2 {
         position: absolute;
         top: 580px;
         right: 160px;
         width: 60px;
         z-index: 1000;
         pointer-events: none;
     }
      .floating-text {
      position: absolute;
      top: 610px;  
      right: 120px; 
      width: 20px;
      z-index: 1000;
      font-size: 22px;
      color: black; 
      pointer-events: none;
     }
   </style>
   <img src="%s" class="floating-image1" alt="Overlay Image 1">
   <img src="%s" class="floating-image2" alt="Overlay Image 2">
   <div class="floating-text">rOpenSpain</div>', base64_spod, base64_ropenspain)

# Step 4: Inject the HTML overlay into the leaflet map's saved HTML
map_with_overlay <- onRender(
  compare_plot,
  paste0(
    "function(el, x) {",
    "  var overlay = `", html_overlay, "`;",
    "  document.getElementById(el.id).insertAdjacentHTML('beforeend', overlay);",
    "}"
  )
)



saveWidget(map_with_overlay, "maps/13-a-new-tool.html", selfcontained = FALSE)
