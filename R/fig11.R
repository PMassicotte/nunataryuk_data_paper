# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure showing pigment proportions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Read the raw pigment file -----------------------------------------------

# I am using this file because the limit of detection was identified with the
# value of -8888. I will consider this value as 0 in the calculations.

pigments <-
  read_csv(here(
    "data",
    "raw",
    "NUNA2019_HPLC_final_2021_01_23.xlsx - DATA.csv"
  )) %>%
  janitor::clean_names() %>%
  replace(. == -8888, 0)

# Keep stations that were present in the 4 legs but also with measured pigments
# (i.e. the sum of the selected pigment is not 0).

df_viz <- pigments %>%
  select(
    station_name,
    cruise,
    long_decdeg,
    lat_decdeg,
    tot_chl_a_mg_per_m_3,
    tot_chl_b_mg_per_m_3,
    tot_chl_c_mg_per_m_3,
    psc_mg_per_m_3 = psc,
    allo_mg_per_m_3,
    diadino_mg_per_m_3,
    zea_mg_per_m_3
  ) %>%
  drop_na(cruise) %>%
  rowwise() %>%
  mutate(total_pigment = sum(c_across(
    -c(station_name, cruise, long_decdeg, lat_decdeg)
  ), na.rm = TRUE)) %>%
  ungroup() %>%
  filter(total_pigment != 0) %>%
  add_count(station_name) %>%
  filter(n == 4)

# stations <- c("STN340alt", "STN350", "STN360")

df_viz %>%
  ggplot(aes(long_decdeg, lat_decdeg, color = factor(cruise))) +
  geom_point() +
  geom_text(aes(label = station_name),
    check_overlap = FALSE,
    show.legend = FALSE
  ) +
  facet_wrap(~cruise, scales = "free")

stations <- c("STN350", "STN360", "STN150alt")

df_viz <- df_viz %>%
  filter(station_name %in% stations)

df_viz %>%
  distinct(station_name)

# Select pigments ---------------------------------------------------------

names(df_viz)

df_viz <- df_viz %>%
  select(
    station_name,
    cruise,
    tot_chl_a_mg_per_m_3,
    tot_chl_b_mg_per_m_3,
    tot_chl_c_mg_per_m_3,
    psc_mg_per_m_3,
    allo_mg_per_m_3,
    diadino_mg_per_m_3,
    zea_mg_per_m_3
  )

df_viz <- df_viz %>%
  mutate(
    npc_mg_per_m_3 = allo_mg_per_m_3 + diadino_mg_per_m_3 + zea_mg_per_m_3,
    .keep = "unused"
  ) %>%
  rowwise() %>%
  mutate(total_pigment = sum(c_across(-c(
    station_name, cruise
  )), na.rm = TRUE)) %>%
  mutate(across(-c(station_name, cruise, total_pigment), ~ .x / total_pigment)) %>%
  mutate(total_percentage = sum(c_across(-c(
    station_name, cruise, total_pigment
  )), na.rm = TRUE))

df_viz

# Pigments plot -----------------------------------------------------------

p1 <- df_viz %>%
  mutate(station_name = factor(station_name,
    levels = c("STN150alt", "STN360", "STN350")
  )) %>%
  pivot_longer(contains("mg_per_m_3")) %>%
  ggplot(aes(x = cruise, y = value, fill = name)) +
  geom_col() +
  scale_fill_viridis_d(
    breaks = c(
      "npc_mg_per_m_3",
      "psc_mg_per_m_3",
      "tot_chl_a_mg_per_m_3",
      "tot_chl_b_mg_per_m_3",
      "tot_chl_c_mg_per_m_3"
    ),
    labels = c(
      parse(text = "NPC~(mg~m^{-3})"),
      parse(text = "PSC~(mg~m^{-3})"),
      parse(text = "Total~'Chl-a'~(mg~m^{-3})"),
      parse(text = "Total~'Chl-b'~(mg~m^{-3})"),
      parse(text = "Total~'Chl-c'~(mg~m^{-3})")
    ),
    guide = guide_legend(
      label.position = "top",
      label.theme = element_text(size = 6, family = "Montserrat"),
      keywidth = unit(2, "cm"),
      keyheight = unit(0.25, "cm")
    )
  ) +
  scale_x_continuous(labels = ~ paste("Leg", .), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Pigment proportion"
  ) +
  facet_wrap(~station_name, ncol = 2) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid = element_blank()
  )

# Map overview ------------------------------------------------------------

stations_sf <- pigments %>%
  filter(station_name %in% stations) %>%
  select(station_name, long_decdeg, lat_decdeg) %>%
  distinct(station_name, .keep_all = TRUE) %>%
  st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326)

stations_sf

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

p <- p1 +
  annotation_custom(ggplotGrob(p2), xmin = 4.5, xmax = 8.4, ymin = -0.1, ymax = 1)

ggsave(
  here("graphs", "fig11.pdf"),
  device = cairo_pdf,
  width = 140,
  height = 140,
  units = "mm"
)
