# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure showing the geographical locations of the sediment
# sampling stations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

stations <- read_csv(here(
  "data",
  "raw",
  "DP Nunataryuk tables DRAFT.xlsx - Sediment Map.csv"
),
skip = 1
) %>%
  slice(-7) %>%
  drop_na() %>%
  type_convert() %>%
  mutate(date = lubridate::parse_date_time(sampling_date_utc,
    orders = c("dmY", "Ymd")
  ), .after = name)

stations

## Attach season information to display on the map ----

stations <- stations %>%
  mutate(leg = case_when(
    lubridate::month(date) == 4 ~ "Leg 1",
    lubridate::month(date) %in% c(8, 9) ~ "Leg 4",
    TRUE ~ NA_character_
  ), .after = date)

stations_sf <- stations %>%
  st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326)

crsuggest::suggest_crs(stations_sf)

# bbox --------------------------------------------------------------------

bbox <- st_read(here("data", "raw", "bbox.geojson"))

wm <- ne_countries(returnclass = "sf", country = "canada", scale = "large") %>%
  st_crop(bbox)

# River network -----------------------------------------------------------

# https://open.canada.ca/data/en/dataset/448ec403-6635-456b-8ced-d3ac24143add
river_network <-
  st_read("data/raw/lakes_rivers_shapefiles/ghy_000c11a_e/ghy_000c11a_e.shp") %>%
  st_crop(c(
    xmin = -141,
    xmax = -130,
    ymin = 68,
    ymax = 70
  )) %>%
  st_transform(st_crs(wm))

# Prepare bathymetry data -------------------------------------------------

bathy <- rast(
  here(
    "data",
    "raw",
    "bathymetry",
    "GEBCO_25_Nov_2021_4dbaf056fe94",
    "gebco_2021_n70.5_s68.0_w-140.0_e-132.5.tif"
  )
) %>%
  spatSample(
    size = 1e5,
    as.raster = TRUE,
    method = "regular"
  ) %>%
  as.data.frame(xy = TRUE) %>%
  as_tibble() %>%
  rename(z = 3)

bathy_interpolated <- bathy %>%
  mba.surf(no.X = 600, no.Y = 600, sp = TRUE) %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(xyz.est.z = ifelse(xyz.est.z >= 0, 0, xyz.est.z))

range(bathy_interpolated$xyz.est.z)

# POI ---------------------------------------------------------------------

poi <- tibble(
  longitude = c(
    -133.7218100,
    -135.01071,
    -133.034180,
    -133.5888,
    -135.5,
    -137.1,
    -133.7723,
    -137,
    -137.345
  ),
  latitude = c(
    68.3498600,
    68.21913,
    69.445358,
    69.5499,
    68.78,
    69.1,
    69.4167,
    69.7,
    68.9891
  ),
  longitude_label = c(
    -133.4,
    -135.4,
    -132.9,
    -134,
    -134.9,
    -137.8,
    -134.3,
    -137,
    -137.75
  ),
  latitude_label = c(
    68.3498600,
    68.21913,
    69.38,
    69.8,
    68.78,
    69.25,
    69.46,
    69.75,
    68.85
  ),
  place = c(
    "Inuvik",
    "Aklavik",
    "Tuktoyaktuk",
    "Kugmallit Bay",
    "Shallow Bay",
    "Mackenzie Bay",
    "Kittigazuit Bay",
    "Beaufort Sea",
    "Shingle Point"
  )
)

# Seed for ggrepel
set.seed(3245)

p <- ggplot() +
  ggisoband::geom_isobands(
    data = bathy_interpolated %>% filter(xyz.est.y <= 70),
    aes(xyz.est.x, xyz.est.y, fill = xyz.est.z, z = xyz.est.z),
    bins = 60,
    # breaks = -c(0, 200, by = 5),
    color = NA
  ) +
  paletteer::scale_fill_paletteer_c(
    "ggthemes::Blue",
    direction = -1,
    limits = c(-60, 0),
    breaks = -seq(0, 60, by = 10),
    labels = c("0 m", "10 m", "20 m", "30 m", "40 m", "50 m", "> 60 m"),
    oob = scales::squish,
    guide = guide_legend(
      label.position = "top",
      title = element_blank(),
      title.position = "top",
      title.hjust = 0.5,
      title.theme = element_text(size = 8, family = "Montserrat"),
      label.theme = element_text(size = 6, family = "Montserrat"),
      keyheight = unit(0.25, "cm"),
      keywidth = unit(0.75, "cm"),
      byrow = TRUE,
      nrow = 1
    )
  ) +
  scale_color_manual(
    breaks = c("Leg 1", "Leg 4"),
    values = c("#BD241EFF", "#15274DFF"),
    guide = guide_legend(
      title = element_blank(),
      override.aes = list(size = 3),
      label.position = "top",
      label.theme = element_text(size = 6, family = "Montserrat"),
      order = 1
    )
  ) +
  geom_sf(data = wm, size = 0.15) +
  geom_sf(
    data = river_network %>% st_intersection(wm),
    size = 0.1,
    color = "grey50"
  ) +
  geom_sf(
    data = stations_sf,
    aes(color = leg),
    size = 0.5
  ) +
  ggrepel::geom_label_repel(
    data = stations,
    aes(
      label = station_name,
      x = long_decdeg,
      y = lat_decdeg,
      color = leg
    ),
    label.size = NA,
    label.padding = 0.1,
    box.padding = 0.2,
    min.segment.length = 0.25,
    fill = alpha("white", 0.6),
    size = 2,
    hjust = 1,
    fontface = "bold",
    family = "Montserrat Light",
    segment.size = unit(0.1, "mm"),
    max.iter = Inf,
    max.time = 2,
    show.legend = FALSE
  ) +
  geom_point(
    data = poi,
    aes(x = longitude, y = latitude),
    pch = 18,
    size = 3,
    color = "#007d57"
  ) +
  geom_label(
    data = poi,
    aes(x = longitude_label, y = latitude_label, label = place),
    size = 3,
    family = "Exo",
    color = "gray25",
    label.size = NA,
    fill = alpha("grey95", 0.6),
    label.padding = unit(0.1, "lines")
  ) +
  annotate(
    "curve",
    x = -134,
    xend = -133.65,
    y = 69.75,
    yend = 69.6,
    curvature = -0.3,
    size = 0.25,
    arrow = ggplot2::arrow(length = unit(0.05, "inch")),
    color = "gray50"
  ) +
  annotate(
    "curve",
    x = -137.7,
    xend = -137.2,
    y = 69.21,
    yend = 69.1,
    curvature = 0.3,
    size = 0.25,
    arrow = ggplot2::arrow(length = unit(0.05, "inch")),
    color = "gray50"
  ) +
  annotate(
    "curve",
    x = -137.8,
    xend = -137.45,
    y = 68.9,
    yend = 68.99,
    curvature = -0.3,
    size = 0.25,
    arrow = ggplot2::arrow(length = unit(0.05, "inch")),
    color = "gray50"
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.3,
    height = unit(0.1, "cm"),
    line_width = 0.25
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(1.2, "cm"),
    width = unit(1.2, "cm"),
    pad_x = unit(0.1, "in"),
    pad_y = unit(0.1, "in"),
    style = north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book",
      text_size = 6,
      line_width = 0.5
    )
  ) +
  coord_sf(
    xlim = c(-140, -132.5),
    ylim = c(68, 70),
    expand = FALSE
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#B9DDF1"),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 7),
    legend.key.size = unit(0.25, "cm"),
    legend.margin = margin(t = 0, unit = "cm"),
    legend.direction = "horizontal",
    legend.position = c(0.01, 0.01),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent")
  )

filename <- here("graphs", "fig12.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)

knitr::plot_crop(filename)
