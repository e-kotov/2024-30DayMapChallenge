# description ------------------------------------------------------------

# 2024-11-16

# "Classic choropleth map. Use color to show data variation across regions. This simple but effective technique is a staple for showing thematic differences. "


# setup packages ---------------------------------------------------------

if (!require("pak")) install.packages("pak")
options(repos = c(tmap = "https://r-tmap.r-universe.dev", rOpenSpain = "https://ropenspain.r-universe.dev", CRAN = "https://cloud.r-project.org"))
packages <- c("sf", "tidyverse", "spanishoddata", "mapSpain", "rostemplate", "fs", "mapgl", "base64enc", "htmlwidgets")
pak::pkg_install(packages, upgrade = FALSE, ask = FALSE)

# Load the packages without startup messages
suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))

rm(packages)


# get the flows data -----------------------------------------------------

overnight_stays <- spod_get(
  type = "overnight_stays",
  zones = "distr",
  dates = c("2023-10-25", "2024-10-31")
) |> collect()

# sanity checks for location with 100% away
# overnight_stays |> filter(id_residence == "19103_AM", date == "2024-10-31") |> View()
# overnight_stays |> filter(id_residence == id_overnight_stay, date == "2024-10-31")


# find share of persons stayed outside residence -------------------------

outside_residence_share <- overnight_stays |>
    filter(id_residence != id_overnight_stay) |>
    # summarize total people staying outside for each id_residence
    group_by(id_residence, date) |>
    summarize(outside_stays = sum(n_persons, na.rm = TRUE), .groups = "drop") |>
    # join with total n_persons for each id_residence
    left_join(
      overnight_stays |>
        group_by(id_residence, date) |>
        summarize(total_persons = sum(n_persons, na.rm = TRUE), .groups = "drop"),
      by = c("date", "id_residence")
    ) |>
    # calculate share of people staying outside
    mutate(share_outside = round(outside_stays / total_persons * 100, 0))

# outside_residence_share
# hist(outside_residence_share$share_outside)



# get the zones ----------------------------------------------------------

districts <- spod_get_zones("dist", ver = 2) |> 
  # drop France and Portugal
  filter(!grepl("FR.*|PT.*", id))


districts_for_plot <- rbind(
  # all zones except for Canary Islands
  districts |> 
    filter(!grepl("^38|^35", id)),
  # Canary Islands moved closer to mainland Spain
  esp_move_can(
    districts |> filter(grepl("^38|^35", id))
    )
  ) |> 
  st_simplify(preserveTopology = TRUE, dTolerance = 100)


# combine data and map it ------------------------------------------------

districts_share_outside <- districts_for_plot |>
  left_join(outside_residence_share |>
    filter(share_outside != 100) |> 
    select(id_residence, share_outside, date),
    by = c("id" = "id_residence")
  ) |> 
  mutate(tooltip = sprintf(
    "ID: %s<br>Share: %s",
    id, share_outside
  ))

# map_16_choropleth <- districts_share_outside |> 
#   filter(!is.na(date)) |> 
#   ggplot() +
#   geom_sf(aes(fill = share_outside), col = NA) +
#   scale_fill_viridis_c() +
#   facet_wrap(~ date, ncol = 2)

# map_16_choropleth <- tm_shape(districts_share_outside) +
#   tm_fill(
#     fill = "share_outside",
#     fill.scale = tm_scale_intervals(values = "viridis"),
#     fill.legend = tm_legend(title = "Share of people who have not stayed in their residence location"),
#     fill.chart = tm_chart_histogram()
#   ) +
#   tm_facets(by = "date", ncol = 2)

# map_16_choropleth


# mapgl ------------------------------------------------------------------

m2023 <- maplibre(
  bounds = districts_share_outside,
  style = carto_style("dark-matter")) |> 
  add_fill_extrusion_layer(
    id = "2023",
    source = districts_share_outside |>
      filter(date == "2023-10-25"),
    fill_extrusion_color = interpolate(
      column = "share_outside",
      values = c(0, 20, 40, 60, 80, 100),
      stops = viridisLite::viridis(6),
      na_color = "lightgrey"
    ),
    fill_extrusion_height = interpolate(
      column = "share_outside",
      values = c(0, 100),
      stops = c(0, 200000)),
    tooltip = "tooltip",
    popup = "tooltip"
  ) |> 
  add_legend(
    '<span style="font-size: 24px; font-family: Roboto; font-weight: bold;">% de personas que no permanecieron en su lugar de residencia</span>
    <br><br>
    <i style="font-size: 16px; font-family: Roboto;">(valores atípicos con el 100% eliminados)</i>',
    values = c(0, 20, 40, 60, 80, 100),
    # set viridis colours
    colors = viridisLite::viridis(6),
  )
# m2023


m2024 <- maplibre(
  bounds = districts_share_outside,
  style = carto_style("dark-matter")) |> 
  add_fill_extrusion_layer(
    id = "2024",
    source = districts_share_outside |>
      filter(date == "2024-10-31"),
    fill_extrusion_color = interpolate(
      column = "share_outside",
      values = c(0, 20, 40, 60, 80, 100),
      stops = viridisLite::viridis(6),
      na_color = "lightgrey"
    ),
    fill_extrusion_height = interpolate(
      column = "share_outside",
      values = c(0, 100),
      stops = c(0, 200000)),
    tooltip = "tooltip",
    popup = "tooltip"
  )
# m2024

map_23_vs_24 <- compare(m2023, m2024)


# customise map ----------------------------------------------------------

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
      font-family: Roboto;
      color: white; 
      pointer-events: none;
     }
   </style>
   <img src="%s" class="floating-image1" alt="Overlay Image 1">
   <img src="%s" class="floating-image2" alt="Overlay Image 2">
   <div class="floating-text">rOpenSpain</div>', base64_spod, base64_ropenspain)

# Define the Dana title with CSS for a floating top-center element
dana_title <- '
<style>
  .floating-dana-title {
      position: relative;
      top: 20px;
      left: 50%%; /* Escaping %% for sprintf */
      transform: translateX(-50%%); /* Escaping %% for sprintf */
      font-size: 28px;
      font-family: Roboto, sans-serif;
      font-weight: bold;
      color: white;
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.7);
      z-index: 3000; /* Ensure it\'s above all other layers */
      pointer-events: none; /* Prevent interaction */
      text-align: center;
  }
</style>
<div class="floating-dana-title">
  La DANA provoca un<br>desplazamiento significativodesde<br>los lugares de residencia habituales
</div>
'

custom_css_for_legend <- '
<style>
  #legend-927f2.top-left {
      top: 140px;
      left: 10px;
  }
</style>
'

floating_dates <- '
<style>
  /* Floating date on the top-left corner */
  .floating-date-left {
      position: absolute;
      top: 20px;
      left: 20px;
      font-size: 18px;
      font-family: Roboto, sans-serif;
      font-weight: bold;
      text-align: left;
      color: white;
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.7);
      z-index: 3000;
      pointer-events: none; /* Prevent interaction */
  }

  /* Floating date on the top-right corner */
  .floating-date-right {
      position: absolute;
      top: 20px;
      right: 20px;
      font-size: 18px;
      font-family: Roboto, sans-serif;
      font-weight: bold;
      text-align: right;
      color: white;
      text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.7);
      z-index: 3000;
      pointer-events: none; /* Prevent interaction */
  }
</style>
<div class="floating-date-left">
  2023-10-25<br>como de costumbre
</div>
<div class="floating-date-right">
  2024-10-31<br>después de la DANA
</div>
'

footer_text <- '
<style>
  .footer-text {
      position: absolute;
      bottom: 20px;
      left: 20px;
      font-size: 14px;
      font-family: Roboto, sans-serif;
      color: white;
      text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.7);
      z-index: 3000;
      text-align: left;
      pointer-events: none; /* Prevent interaction */
      line-height: 1.5; /* Adjust line spacing */
  }
</style>
<div class="footer-text">
  En solidaridad con las víctimas y sus familias<br>
  Autor: Egor Kotov | #30DayMapChallenge | Día 16: Mapa Coroplético<br>
  Paquete R para acceder a los datos: <a href="https://ropenspain.github.io/spanishoddata/" style="color: white; text-decoration: underline;">ropenspain.github.io/spanishoddata/</a><br>
  Visualización del mapa realizada con mapgl: <a href="https://walker-data.com/mapgl/" style="color: white; text-decoration: underline;">walker-data.com/mapgl/</a><br>
  Fuentes de datos: Ministerio de Transportes y Movilidad Sostenible (MITMS); Nommon
</div>
'

# Combine the Dana title with the existing HTML overlay for images and text
full_html_customization <- paste0(html_overlay, dana_title, custom_css_for_legend, floating_dates, footer_text)

# Inject the combined HTML overlay (including the title) into the leaflet map
styled_map <- onRender(
  map_23_vs_24,
  paste0(
    "function(el, x) {",
    "  var overlay = `", full_html_customization, "`;",
    "  document.getElementById(el.id).insertAdjacentHTML('beforeend', overlay);",
    "}"
  )
)

# Render the map with the title and overlays
styled_map

saveWidget(styled_map, "maps/16-choropleth.html")
