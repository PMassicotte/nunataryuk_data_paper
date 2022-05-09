# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Geographical map of the sampling stations.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Stations ----------------------------------------------------------------

df <- readxl::read_excel(
    here("data", "raw", "Nunataryuk WP4 Mackenzie 2019.xlsx"),
    sheet = "Sub dataset 1",
    .name_repair = janitor::make_clean_names
  )

df

range(df$depth_water_m, na.rm = TRUE)

stations <- df %>%
  distinct(expedition, longitude_dec_deg, latitude_dec_deg)

stations_sf <- stations %>%
  st_as_sf(coords = c("longitude_dec_deg", "latitude_dec_deg"), crs = 4326)

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
    ymin = 67,
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
    -137.345,
    -133.74
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
    68.9891,
    67.45
  ),
  longitude_label = c(
    -133.4,
    -135.4,
    -133.034180,
    -134,
    -134.9,
    -137.8,
    -134.25,
    -137,
    -137.75,
    -133.74
  ),
  latitude_label = c(
    68.3498600,
    68.21913,
    69.38,
    69.8,
    68.78,
    69.25,
    69.4167,
    69.75,
    68.85,
    67.55
  ),
  place = c(
    "Inuvik",
    "Aklavik",
    "Tuktoyaktuk",
    "Kugmallit Bay",
    "Shallow Bay",
    "Mackenzie Bay",
    "Kittigazuit\nBay",
    "Beaufort Sea",
    "Shingle Point",
    "Hydrometric station (10LC014)\nArctic Red River"
  )
) %>%
  mutate(label = "\uf041")

# Zoom in map -------------------------------------------------------------

p1 <- ggplot() +
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
  geom_sf(data = wm, size = 0.15) +
  geom_sf(
    data = river_network %>% st_intersection(wm),
    size = 0.1,
    color = "grey50"
  ) +
  geom_sf(
    data = st_jitter(stations_sf),
    aes(color = factor(expedition), shape = factor(expedition)),
    size = 1.5
  ) +
  paletteer::scale_color_paletteer_d(
    "suffrager::london",
    labels = function(x) {
      paste("Leg", x)
    },
    guide = guide_legend(
      title = element_blank(),
      override.aes = list(size = 3),
      label.position = "top",
      label.theme = element_text(size = 6, family = "Montserrat"),
      order = 1
    )
  ) +
  scale_shape_manual(
    values = c(
      "1" = 0,
      "2" = 1,
      "3" = 2,
      "4" = 3
    ),
    labels = function(x) {
      paste("Leg", x)
    },
    guide = guide_legend(
      title = element_blank(),
      # override.aes = list(size = 3),
      label.position = "top",
      label.theme = element_text(size = 6, family = "Montserrat"),
      order = 1
    )
  ) +
  geom_text(
    data = poi,
    aes(x = longitude, y = latitude, label = label),
    size = 4,
    color = "#007d57",
    family = "fontawesome-webfont"
  ) +
  geom_text(
    data = poi,
    aes(x = longitude_label, y = latitude_label, label = place),
    size = 3,
    family = "Exo"
  ) +
  annotate(
    "curve",
    x = -134,
    xend = -133.65,
    y = 69.75,
    yend = 69.6,
    curvature = -0.3,
    size = 0.25,
    arrow = ggplot2::arrow(length = unit(0.05, "inch"))
  ) +
  annotate(
    "curve",
    x = -137.7,
    xend = -137.2,
    y = 69.21,
    yend = 69.1,
    curvature = 0.3,
    size = 0.25,
    arrow = ggplot2::arrow(length = unit(0.05, "inch"))
  ) +
  annotate(
    "curve",
    x = -137.8,
    xend = -137.45,
    y = 68.9,
    yend = 68.99,
    curvature = -0.3,
    size = 0.25,
    arrow = ggplot2::arrow(length = unit(0.05, "inch"))
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
      text_size = 6,
      line_width = 0.5
    )
  ) +
  # guides(shape = "none") +
  # labs(color = 1, shape = 1) +
  coord_sf(
    xlim = c(-140, -132.5),
    ylim = c(67.25, 70),
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
    legend.position = c(0.01, 0.38),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "transparent", color = "transparent")
  )

# Inset -------------------------------------------------------------------

bbox <- c(xmin = -180, xmax = 0, ymin = 30, ymax = 90)

canada <- rnaturalearth::ne_countries(returnclass = "sf", scale = "large") %>%
  st_crop(bbox) %>%
  filter(name_en != "Canada")

provinces <- rnaturalearth::ne_states(country = "canada", returnclass = "sf") %>%
  st_transform(3979)

unique(provinces$name_en)

nwt_yukon <- provinces %>%
  filter(name_en %in% c("Yukon", "Northwest Territories"))

# crsuggest::suggest_crs(canada)

bbox_nsidc <- canada %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_transform(3979) %>%
  st_bbox()

map_bbox <- st_bbox(c(xmin = -140, xmax = -132, ymin = 67.25, ymax = 70), crs = 4326) %>%
  st_as_sfc() %>%
  st_transform(3979)

locations <- tibble(
  label = c("Canada", "USA", "Greenland", "Arctic Ocean", "Yukon", "NWT"),
  longitude = c(-115, -120, -40, -150, -136.2, -118),
  latitude = c(58, 46, 75, 80, 62, 63.4)
) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3979)

p2 <- canada %>%
  st_difference(st_transform(provinces, st_crs(canada))) %>%
  ggplot() +
  geom_sf(size = 0.1) +
  geom_sf(data = provinces, size = 0.1) +
  geom_sf(data = nwt_yukon, size = 0.2, color = "#3c3c3c", fill = NA) +
  geom_sf(
    data = map_bbox,
    fill = NA,
    color = "red",
    size = 0.5
  ) +
  geom_sf_text(
    data = locations,
    aes(label = label),
    size = 2,
    family = "Montserrat",
    fontface = "bold"
  ) +
  scale_x_continuous(breaks = seq(-180, 180, by = 10)) +
  coord_sf(
    crs = 3979,
    xlim = c(-2500000, 2200000),
    ylim = c(4000000, 10)
  ) +
  theme(
    text = element_text(size = 6),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Combine -----------------------------------------------------------------

p1 <- p1 +
  annotation_custom(ggplotGrob(p2), xmin = -139.9, xmax = -137, ymin = 66.6, ymax = 68.9)

# Save --------------------------------------------------------------------

filename <- here("graphs", "fig01.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)

knitr::plot_crop(filename)

