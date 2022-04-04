# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Maps showing the salinity across the four Legs.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_data.csv")) %>%
  distinct(
    event,
    expedition,
    depth_water_m,
    longitude = longitude_dec_deg,
    latitude = latitude_dec_deg,
    sal_ctd
  ) %>%
  group_by(event) %>%
  filter(depth_water_m == min(depth_water_m, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na()

df %>%
  add_count(event, sort = TRUE) %>%
  assertr::verify(n == 1)

df_sf <- df %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

df_sf

# bbox used to crop -------------------------------------------------------

bb <- st_bbox(c(
  xmin = -170,
  xmax = -80,
  ymin = 40,
  ymax = 80
), crs = 4326)

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

p <- ggplot() +
  geom_sf(data = wm, size = 0.01) +
  geom_sf(data = river_network, size = 0.1, color = "gray50") +
  geom_sf(data = df_sf, size = 1.2) +
  geom_sf(data = df_sf, aes(color = sal_ctd), size = 1) +
  scale_color_viridis_c(
    option = "C",
    trans = "log10",
    guide = guide_colorbar(
      title.position = "top",
      barwidth = unit(3, "cm"),
      barheight = unit(0.25, "cm"),
      label.theme = element_text(size = 6, family = "Montserrat"),
      title.theme = element_text(size = 8, family = "Montserrat")
    ),
    breaks = c(0, 0.01, 0.1, 1, 10, 30),
    labels = c(0, 0.01, 0.1, 1, 10, 30),
    limits = c(0.01, 40),
    oob = scales::squish
  ) +
  facet_wrap(~glue("Leg {expedition}")) +
  coord_sf(
    xlim = c(-138, -133),
    ylim = c(68.2, 69.8),
    expand = TRUE
  ) +
  labs(
    color = "Salinity"
  ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.01, 0.62),
    legend.background = element_blank(),
    legend.direction = "horizontal",
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 6)
  )

filename <- here("graphs", "fig05.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 160,
  height = 160,
  units = "mm"
)

knitr::plot_crop(filename)
