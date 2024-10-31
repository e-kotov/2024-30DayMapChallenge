
# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "ggimage", "rostemplate", "ggtext", "resmush", "svglite")
pak::pkg_install(packages, upgrade = TRUE, ask = FALSE)
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
rm(packages)


# get the flows data -----------------------------------------------------

od <- spod_get("od", zones = "distr", dates = "2024-04-17")


# aggregate data ---------------------------------------------------------


incoming_trips_ratio <- od |> 
  filter(!is.na(sex)) |>
  filter(activity_origin == "home" & activity_destination == "work_or_study") |>
  group_by(id_destination, sex) |> 
  summarise(total_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
  collect() |> 
  spread(sex, total_trips, fill = 0) |> 
  mutate(sex_ratio = male / female) |> 
  mutate(sex_ratio_category = if_else(
    condition = sex_ratio < 1, 
    true = "More Female Trips", 
    false = "More Male Trips")
  ) |> 
  filter(id_destination != "external") |>
  select(id_destination, sex_ratio, sex_ratio_category)



# get zones and add the aggregation results ------------------------------

zones_distr_v2_centroids <- spod_get_zones("distr", ver = 2) |> 
  filter(!grepl("FR.*|PT.*", id)) |> 
  st_point_on_surface() |> 
  st_transform(4326) |>
  left_join(incoming_trips_ratio, by = c("id" = "id_destination")) |> 
  select(id, sex_ratio, sex_ratio_category)


# move Canary Islands closer to Spain ------------------------------------

dots_sf <- rbind(
  # all zones except for Canary Islands
  zones_distr_v2_centroids |> 
    filter(!grepl("^38|^35", id)),
  # Canary Islands moved closer to mainland Spain
  esp_move_can(
    zones_distr_v2_centroids |> filter(grepl("^38|^35", id))
    )
  ) |> 
  mutate(sex_ratio_category = if_else(is.na(sex_ratio_category), "No Data Available", sex_ratio_category))


# prepare paths to logos -------------------------------------------------

spod_logo <- system.file("help/figures", "logo.png", package = "spanishoddata")
ropenspain_logo <- system.file("help/figures", "logo.png", package = "rostemplate")



# prepare the map --------------------------------------------------------


map_01_points <- ggplot() +
  geom_sf(
    data = dots_sf,
    aes(col = sex_ratio_category),
    size = 0.5,
    alpha = 0.8,
    shape = 15) +
  scale_color_manual(
    values = c(
      "More Female Trips" = "#f7de38",
      "More Male Trips" = "#440852",
      "No Data Available" = "grey80"),
    labels = c(
      "More Female Trips" = "<span style='color:#f7de38'>More Female Trips</span>",
      "More Male Trips" = "<span style='color:#440852'>More Male Trips</span>",
      "No Data Available" = "<span style='color:grey80'>No Data Available</span>"),
    name = "<span style='color:grey10'>Commuters</span></b>"
  ) +
  geom_image(data = tibble(x = 3, y = 37),
             aes(image = spod_logo, x = x, y = y), size = 0.3) +
  geom_image(data = tibble(x = 2.05, y = 35),
             aes(image = ropenspain_logo, x = x, y = y), size = 0.05) +
  annotate("text", x = 2.55, y = 35, label = "rOpenSpain", size = 3, hjust = 0, color = "grey30") +
  # annotate("text", x = 1.55, y = 32.5, label = "Author: Egor Kotov", size = 3, hjust = 0, color = "grey30", family = "Roboto") +
  labs(
    title = "Dominance of <span style='color:#f7de38'>Female</span> Commuters<br>in Urban Destinations Across Spain",
    caption = "Author: Egor Kotov | #30DayMapChallenge\nR package to get the data: https://ropenspain.github.io/spanishoddata/\nOriginal data source: Ministry of Transport and Sustainable Mobility of Spain; Nommon"
  ) +
  theme_void() +
  theme(
    text = element_text(family = "Roboto", size = 18),
    panel.background = element_rect(fill = "grey65", color = NA),
    plot.background = element_rect(fill = "grey65", color = NA),
    legend.text = element_markdown(),
    legend.title = element_markdown(),
    plot.title = element_markdown(hjust = 0.5, color = "grey10"),
    plot.subtitle = element_markdown(),
    plot.caption = element_text(size = 9, hjust = 0.5, color = "grey30"),
    plot.margin = margin(t = 20, r = 5, b = 20, l = 5),
    legend.position = "inside",
    legend.position.inside = c(0.18, 0.6)
  )

# print(map_01_points)

# save map to png --------------------------------------------------------

ggsave(
  filename = "maps/01-points.png",
  map_01_points,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300,
  create.dir = TRUE
)


# opmimise png with resmush.it -------------------------------------------

resmush_file("maps/01-points.png", overwrite = TRUE)


# save to svg ------------------------------------------------------------


ggsave(
  filename = "maps/01-points.svg",
  map_01_points,
  width = 8,
  height = 7,
  units = "in",
  dpi = 300,
  create.dir = TRUE
)
