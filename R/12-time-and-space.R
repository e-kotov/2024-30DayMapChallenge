# description ------------------------------------------------------------

# 2024-11-11

# "Map the Arctic. Whether it’s ice coverage, wildlife habitats, or the effects of climate change, this day is all about mapping the cold extremes of the Arctic."

# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite", "flowmapblue", "flowmapper", "patchwork", "arcpullr", "fs", "ggnewscale")
pak::pkg_install(packages, upgrade = TRUE, ask = FALSE)
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
rm(packages)

# get impact data

impact_area_file <- "cache/12-time-and-space/2024-11-08-affected_area.rds"
if(!file_exists(impact_area_file)){
  if(!dir_exists(path_dir(impact_area_file))){
    dir_create(path_dir(impact_area_file), recurse = TRUE)
  }
  impact_data_url <- "https://services8.arcgis.com/mApiu3uMEBwNqKz2/arcgis/rest/services/MapDANA_WFL1/FeatureServer/1"
  
  impacted_area <- get_spatial_layer(url = impact_data_url)
  saveRDS(impacted_area, impact_area_file)
}
impacted_area <- readRDS(impact_area_file)


# get the flows data -----------------------------------------------------

od_flows <- spod_get(
  type = "od",
  zones = "distr",
  dates = c(start = "2024-10-28", end = "2024-10-31")
)


# get the zones ----------------------------------------------------------

districts <- spod_get_zones("dist", ver = 2)



# focus data on valencia -------------------------------------------------

zones_valencia <- districts |>
  filter(grepl("València", name, ignore.case = TRUE))

zones_valencia_fua <- districts[
  st_buffer(zones_valencia, dist = 30000)
  ,
]



# mapview(zones_valencia_fua)



# prepare data for flowmap -----------------------------------------------

valencia_fua_coords <- zones_valencia_fua |>
  st_centroid() |>
  st_transform(4326) |> 
  st_coordinates() |>
  as.data.frame() |>
  mutate(id = zones_valencia_fua$id, name = zones_valencia_fua$name) |>
  rename(lon = X, lat = Y) |> 
  as_tibble()


od_flows_valencia <- od_flows |>
  filter(id_origin %in% valencia_fua_coords$id | id_destination %in% valencia_fua_coords$id) |> 
  # filter(date == as.Date("2024-10-29")) |> 
  mutate(time = as.POSIXct(paste0(date, "T", time_slot, ":00:00"))) |>
  group_by(origin = id_origin, dest = id_destination, time, date, time_slot) |>
  summarise(count = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
  collect() |> 
  as_tibble()


# flowmap ----------------------------------------------------------------

# glimpse(valencia_fua_coords)
# head(valencia_fua_coords)
# glimpse(od_flows_valencia)
# head(od_flows_valencia)


# # flowmap <- flowmapblue(locations = valencia_fua_coords, flows = od_flows_valencia, mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"), animation = FALSE, clustering = FALSE)
# # htmlwidgets::saveWidget(flowmap, "maps/12-time-and-space.html")

# flowmap_28 <- flowmapblue(locations = valencia_fua_coords, flows = od_flows_valencia |> filter(date == "2024-10-28"), mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"), animation = FALSE, clustering = FALSE)
# htmlwidgets::saveWidget(flowmap_28, "maps/12-time-and-space-28oct.html")

# flowmap_29 <- flowmapblue(locations = valencia_fua_coords, flows = od_flows_valencia |> filter(date == "2024-10-29"), mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"), animation = FALSE, clustering = FALSE)
# htmlwidgets::saveWidget(flowmap_29, "maps/12-time-and-space-29oct.html")

# flowmap_30 <- flowmapblue(locations = valencia_fua_coords, flows = od_flows_valencia |> filter(date == "2024-10-30"), mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"), animation = FALSE, clustering = FALSE)
# htmlwidgets::saveWidget(flowmap_30, "maps/12-time-and-space-30oct.html")

# flowmap_31 <- flowmapblue(locations = valencia_fua_coords, flows = od_flows_valencia |> filter(date == "2024-10-31"), mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"), animation = FALSE, clustering = FALSE)
# htmlwidgets::saveWidget(flowmap_31, "maps/12-time-and-space-31oct.html")


# prepare paths to logos -------------------------------------------------

spod_logo <- system.file("help/figures", "logo.png", package = "spanishoddata")
ropenspain_logo <- system.file("help/figures", "logo.png", package = "rostemplate")



# 46186 static flowmap ------------------------------------------------------------------

od_46186 <- od_flows_valencia |> 
  filter(origin == "46186" | dest == "46186")
distr_46186 <- districts |>
  filter(id %in% od_46186$origin | id %in% od_46186$dest) |> 
  st_transform(4326)


flowmapper_nodes <- valencia_fua_coords |> 
  select(-name) |> 
    rename(x = lon, y = lat, name = id)
  
flowmapper_od <- od_46186 |>
  filter(time_slot== 13) |> 
    filter(origin %in% flowmapper_nodes$name & dest %in% flowmapper_nodes$name) |>
      rename(o = origin, d = dest, value = count)
      

districts_for_plot <- districts |> st_simplify(dTolerance = 100) |> st_transform(4326)
base_plot <- ggplot() +
  geom_sf(data = districts_for_plot, fill = "grey", color = "white", lwd = 0.1) +
    geom_sf(data = districts_for_plot, fill = NA, color = "white", lwd = 0.1) +
  coord_sf(xlim = c(-0.7, -0.2), ylim = c(39.3, 39.6))

f28 <- base_plot |> add_flowmap(od = flowmapper_od |> filter(date == "2024-10-28"), nodes = flowmapper_nodes, node_radius_factor = 0.5, edge_width_factor = 0.5, arrow_point_angle = 35) + theme_void() + labs(title = "13:00 - 14:00 28 Oct 2024")+ theme(legend.position = c(0.9, 0.5), plot.title = element_text(family = "Roboto")) + scale_fill_viridis_c(limits = c(0,500), name = "Viajes")


impacted_area$area_type <- "Impacted Area"

f30 <- ( base_plot +
  geom_sf(data = impacted_area, aes(fill = area_type), alpha = 0.3) +
  coord_sf(xlim = c(-0.7, -0.2), ylim = c(39.3, 39.6)) +
  scale_fill_manual(values = c("Impacted Area" = "pink"), name = "", labels = "Delimitación\nCartográfica\nde la DANA") + new_scale_fill() )|>
  add_flowmap(od = flowmapper_od |> filter(date == "2024-10-30"), nodes = flowmapper_nodes, node_radius_factor = 0.5, edge_width_factor = 0.5, arrow_point_angle = 35) + theme_void() + labs(title = "13:00 - 14:00 30 Oct 2024") + theme(legend.position = c(0.9, 0.5), plot.title = element_text(family = "Roboto")) + scale_fill_viridis_c(limits = c(0,500), guide = 'none')



h28 <- od_46186 |>
  filter(date == "2024-10-28") |> 
  group_by(time_slot) |> 
  summarise(count = sum(count, na.rm = TRUE)) |> 
  ggplot() +
  geom_bar(aes(x = time_slot, y = count), stat = "identity", fill = "grey70") +
  geom_bar(data = od_46186 |> filter(date == "2024-10-28" & time_slot == 13), aes(x = time_slot, y = count), stat = "identity", fill = "darkblue") +
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 4)) +
  scale_y_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 10000), limits = c(0, 10000)) +
  labs(y = "", x = "Hora") +
  theme_classic(base_size = 14, base_family = "Roboto")


h30 <- od_46186 |>
  filter(date == "2024-10-30") |> 
  group_by(time_slot) |> 
  summarise(count = sum(count, na.rm = TRUE)) |> 
  ggplot() +
  geom_bar(aes(x = time_slot, y = count), stat = "identity", fill = "grey70") +
  geom_bar(data = od_46186 |> filter(date == "2024-10-30" & time_slot == 13), aes(x = time_slot, y = count), stat = "identity", fill = "darkblue") +
  geom_image(data = tibble(x = 6, y = 6400),
    aes(image = spod_logo, x = x, y = y), size = 0.65) +
  geom_image(data = tibble(x = 13, y = 5500),
    aes(image = ropenspain_logo, x = x, y = y), size = 0.2) +
  annotate("text", x = 15, y = 5500, label = "rOpenSpain", size = 6, hjust = 0, color = "grey30") +
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 4)) +
  scale_y_continuous(breaks = c(0, 2000, 4000, 6000, 8000, 10000), limits = c(0, 10000)) +
  labs(y = "", x = "Hora") +
  theme_classic(base_size = 14, base_family = "Roboto")
# h30


combined_plot <- (f28 | f30) / (h28 | h30)

map_12_time_and_space <- combined_plot +
  plot_layout(heights = c(3, 1)) +
  plot_annotation(
  title = "Impacto de la DANA en la movilidad en un barrio de València",
  caption = "<i>En solidaridad con las víctimas y sus familias</i><br><br>Autor: Egor Kotov | #30DayMapChallenge | Día 12: Tiempo y espacio<br>Paquete R para acceder a los datos: ropenspain.github.io/spanishoddata/<br>Fuentes de datos: Ministerio de Transportes y Movilidad Sostenible (MITMS); Nommon;<br>Departamento de Geografía, Universitat de València; Instituto Geográfico Nacional<br>Basado en los datos de movilidad del MITMS/Nommon<br> 28-30 de octubre de 2024",
  theme = theme(
    plot.title = element_text(size = 24, hjust = 0.5, family = "Roboto"),
    plot.subtitle = element_text(size = 18, family = "Roboto"),
    plot.caption = element_markdown(family = "Roboto")
    )
  )


# save plot --------------------------------------------------------------


# save map to png --------------------------------------------------------

ggsave(
  filename = "maps/12-time-and-space.png",
  map_12_time_and_space,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300,
  create.dir = TRUE
)


# opmimise png with resmush.it -------------------------------------------

resmush_file("maps/12-time-and-space.png", overwrite = TRUE)
