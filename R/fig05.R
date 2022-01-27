# curl::curl_download("ftp://sidads.colorado.edu/pub/DATASETS/NOAA/G10033/north/weekly/shapefile/nh_20191003.zip", destfile = "~/Desktop/test.zip")

# Stations ----------------------------------------------------------------

df <- readxl::read_excel(
  here("data", "raw", "Nunataryuk WP4 Mackenzie 2019.xlsx"),
  sheet = "Sub dataset 1",
  .name_repair = janitor::make_clean_names
)

df

stations <- df %>%
  distinct(expedition, longitude_dec_deg, latitude_dec_deg)

stations_sf <- stations %>%
  st_as_sf(
    coords = c("longitude_dec_deg", "latitude_dec_deg"),
    crs = 4326
  )

# Based on the sampling date, I choose SIC to represent the middle date of the
# sampling per expedition

df %>%
  mutate(date = lubridate::parse_date_time(utc_date_time_dd_mm_yy_hh_mm, orders = "dmyHM")) %>%
  mutate(date = as.Date(date)) %>%
  distinct(expedition, date) %>%
  drop_na() %>%
  arrange(expedition, date)

# SIC ---------------------------------------------------------------------

files <- fs::dir_ls(
  here("data/raw/ice_cover/sic/"),
  recurse = TRUE,
  glob = "*.shp"
)

# bbox used to crop
bb <- st_bbox(c(
  xmin = -170,
  xmax = -80,
  ymin = 40,
  ymax = 80
), crs = 4326)


sic <- map(files, st_read) %>%
  set_names(files) %>%
  do.call(what = sf:::rbind.sf, .) %>%
  rownames_to_column(var = "date") %>%
  mutate(date = str_match(date, "\\d{8}")) %>%
  mutate(date = as.Date(date, format = "%Y%m%d"))

sic

bb_stereo <- bb %>%
  st_as_sfc() %>%
  st_transform(crs = st_crs(sic))

sic <- sic %>%
  st_make_valid() %>%
  st_crop(st_bbox(bb_stereo)) %>%
  st_transform(crs = 4326)

# WM ----------------------------------------------------------------------

wm <-
  ne_countries(
    returnclass = "sf",
    country = "canada",
    scale = "large"
  ) %>%
  st_crop(bb)

# River network -----------------------------------------------------------

# https://open.canada.ca/data/en/dataset/448ec403-6635-456b-8ced-d3ac24143add
river_network <-
  st_read(
    here(
      "data",
      "raw",
      "lakes_rivers_shapefiles",
      "ghy_000c11a_e",
      "ghy_000c11a_e.shp"
    )
  ) %>%
  st_crop(c(
    xmin = -141,
    xmax = -130,
    ymin = 68,
    ymax = 70
  )) %>%
  st_transform(st_crs(wm))

# Plot --------------------------------------------------------------------

p <- sic %>%
  ggplot() +
  geom_sf(aes(fill = tc_mid / 100), color = NA) +
  geom_sf(data = wm, size = 0.01) +
  geom_sf(data = river_network, size = 0.1, color = "gray50") +
  geom_sf(
    data = st_jitter(stations_sf),
    aes(color = factor(expedition)),
    size = 0.5,
    show.legend = FALSE
  ) +
  paletteer::scale_color_paletteer_d(
    "suffrager::london",
    labels = function(x) {
      paste("Leg", x)
    },
    guide = guide_legend(
      title = element_blank(),
      override.aes = list(size = 3),
      label.theme = element_text(family = "Montserrat"),
      order = 1
    )
  ) +
  paletteer::scale_fill_paletteer_c(
    "pals::kovesi.linear_blue_95_50_c20",
    # direction = -1,
    labels = scales::label_percent(),
    guide = guide_colorbar(
      # label.position = "top",
      title.position = "top",
      title = "Sea ice concentration",
      title.theme = element_text(hjust = 0.5, family = "Montserrat", size = 6),
      label.theme = element_text(family = "Montserrat", size = 5),
      barwidth = unit(3, "cm"),
      barheight = unit(0.1, "cm"),
      override.aes = list(color = "#3c3c3c", size = 0.25),
      nrow = 1,
      direction = "horizontal"
    )
  ) +
  coord_sf(
    xlim = c(-140, -130),
    ylim = c(68, 70.5),
    expand = FALSE
  ) +
  facet_wrap(~date) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0.01, 0.01),
    legend.background = element_blank(),
    strip.text = element_text(hjust = 0, size = 14, face = "bold"),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

filename <- here("graphs", "fig05.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 190,
  height = 190,
  units = "mm"
)

knitr::plot_crop(filename)
