# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  EEMs fluorescence collected by Gwen.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

files <- fs::dir_ls(here("data", "raw", "fluorescence_3d_gw_Nuna"), glob = "*f.csv")

ex <- read_lines(files[2], n_max = 1) %>%
  str_split(",") %>%
  unlist() %>%
  .[str_detect(., "^2019")] %>%
  str_match(., "EX_(\\d{3})") %>%
  .[, 2] |>
  parse_number()

eem <- read_csv(files[2], skip = 1)

# Remove duplicated columns
eem <- eem[!duplicated(as.list(eem))] %>%
  janitor::remove_empty("cols") %>%
  set_names(c(names(.)[1], ex)) |>
  pivot_longer(
    matches("\\d{3}"),
    names_to = "ex",
    values_to = "fluorescence",
    names_transform = list(ex = parse_number)
  ) |>
  rename(em = 1)

eem

p1 <- eem %>%
  # mutate(fluorescence = ifelse(fluorescence < 0, 0, fluorescence)) %>%
  ggplot(aes(
    x = ex,
    y = em,
    fill = fluorescence,
    z = fluorescence
  )) +
  ggisoband::geom_isobands(bins = 10, size = 0.1) +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 4)) +
  scale_fill_gradientn(
    colors = MetBrewer::met.brewer("Tam", 6, type = "continuous"),
    # trans = "sqrt"
    guide = guide_colorbar(
      title = "Fluorescence (R.U.)",
      title.position = "top",
      title.theme = element_text(family = "Exo", hjust = 0.5, size = 7),
      label.theme = element_text(family = "Exo", size = 6),
      barwidth = unit(30, "mm"),
      barheight = unit(2, "mm")
    )
  ) +
  labs(
    x = "Excitation (nm)",
    y = "Emission (nm)",
    title = "Station xxx"
  ) +
  # coord_equal(ratio = 0.5) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    panel.grid = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0.52, 0),
    legend.direction = "horizontal"
  )

# Map overview ------------------------------------------------------------

pigments <-
  read_csv(here(
    "data",
    "raw",
    "NUNA2019_HPLC_final_2021_01_23.xlsx - DATA.csv"
  )) %>%
  janitor::clean_names() %>%
  replace(. == -8888, 0)

stations <- c("STN350", "STN360", "STN150alt")

stations_sf <- pigments %>%
  filter(station_name %in% stations) %>%
  select(station_name, long_decdeg, lat_decdeg) %>%
  distinct(station_name, .keep_all = TRUE) %>%
  st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326)

stations_sf
plot(stations_sf)

# crsuggest::suggest_crs(stations_sf)

stations_sf <- stations_sf %>%
  st_transform(6111)

stations_label <- tibble(
  station_name = c("STN360", "STN150alt", "STN350"),
  longitude = c(-136.28, -136.9, -136.85),
  latitude = c(69.08, 68.97, 69.22)
) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(6111)

wm <- rnaturalearth::ne_coastline(scale = "large", returnclass = "sf") %>%
  st_crop(xmin = -138, xmax = -134, ymin = 68, ymax = 70) %>%
  st_transform(6111)

sp <- tibble(name = "Shingle\npoint", longitude = -137.345, latitude = 68.9891) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(6111)

poi <- sp %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(label = "\uf041")

p2 <- ggplot() +
  geom_sf(data = wm, size = 0.1, color = "grey40") +
  geom_sf(data = stations_sf, size = 1) +
  geom_sf_text(
    data = stations_label,
    aes(label = station_name),
    vjust = 2.2,
    hjust = 0.1,
    size = 1.5
  ) +
  geom_sf_text(
    data = sp,
    aes(label = name),
    vjust = -0.3,
    hjust = 1.2,
    size = 2,
    family = "Montserrat",
    fontface = "bold"
  ) +
  geom_text(
    data = poi,
    aes(x = X, y = Y, label = label),
    size = 4,
    color = "#007d57",
    family = "fontawesome-webfont"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme(
    axis.text = element_text(size = 4),
    panel.grid = element_line(size = 0.2)
  )

p <- wrap_plots(list(p1, p1, p1, p1, p1, p2), ncol = 2) +
  plot_layout(widths = 0.5)

ggsave(
  here("graphs", "fig13.pdf"),
  device = cairo_pdf,
  width = 170,
  height = 180,
  units = "mm"
)
