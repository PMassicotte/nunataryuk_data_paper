# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  EEMs fluorescence collected by Gwen. The shapefiles for the Tuk
# area are from https://www.geogratis.gc.ca/.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

eems <- eem_read(here("data/raw/fluorescence_3d_gw_Nuna/eems/"), import_function = "cary")

eems <- eems %>%
  eem_remove_blank() %>%
  eem_raman_normalisation() %>%
  # eem_remove_scattering(type = "raman") %>%
  eem_remove_scattering(type = "rayleigh") %>%
  eem_remove_scattering(type = "rayleigh", 2) %>%
  eem_extract(1, keep = FALSE)

eems <- map_df(eems, function(eem) {

  eem$x %>%
    as_tibble() %>%
    mutate(em = eem$em, .before = 1) %>%
    set_names(c("em", eem$ex)) %>%
    pivot_longer(
      -em,
      names_to = "ex",
      values_to = "fluorescence",
      names_transform = list(ex = parse_number)
    ) %>%
    mutate(sample = eem$sample)
})

eems

range(eems$fluorescence, na.rm = TRUE)

eems <- eems %>%
  mutate(sample = str_match(sample, "(\\d{2})f")[, 2]) %>%
  mutate(sample = parse_number(sample)) %>%
  mutate(sample = glue("Station {sample}"))

# Re-order north -> south
eems <- eems %>%
  mutate(sample = factor(
    sample,
    levels = c("Station 18", "Station 17", "Station 15", "Station 7", "Station 4")
  ))


p1 <- eems %>%
  mutate(fluorescence = ifelse(fluorescence < 0, 0, fluorescence)) %>%
  # filter(sample == "tuk2019_1004f_csv") %>%
  ggplot(aes(
    x = ex,
    y = em,
    fill = fluorescence,
    z = fluorescence
  )) +
  ggisoband::geom_isobands(bins = 10, size = 0.1) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2)), breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 4)) +
  scale_fill_gradientn(
    colors = MetBrewer::met.brewer("Tam", 6, type = "continuous"),
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
    y = "Emission (nm)"
  ) +
  facet_wrap(~sample, ncol = 2) +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    panel.grid = element_blank(),
    legend.justification = c(0, 0),
    legend.position = c(0.17, 0),
    legend.direction = "horizontal"
  )

p1

# Map overview ------------------------------------------------------------

stations <-

  stations <-
  readxl::read_excel(here("data/raw/fluorescence_3d_gw_Nuna/Sample_ID.xlsx"),
                     skip = 1
  ) %>%
  janitor::clean_names() %>%
  filter(
    number_samples %in% c(
      "Tuk2019-1004",
      "Tuk2019-1007",
      "Tuk2019-1015",
      "Tuk2019-1017",
      "Tuk2019-1018"
    )
  ) %>%
  mutate(longitude_w = -longitude_w) %>%
  mutate(sample = str_extract(number_samples, "\\d{2}$")) %>%
  mutate(sample = parse_number(sample))

stations

stations_sf <- stations %>%
  st_as_sf(coords = c("longitude_w", "latitude_n"), crs = 4326)

bbox <- c(xmin = -135, ymin = 75, xmax = -130, ymax = 65)

water <-
  st_read(
    here(
      "data",
      "raw",
      "tuktoyaktuk_shapefiles",
      "bndt_107c07_shp_en",
      "107C07_water_b_a.shp"
    )
  ) %>%
  bind_rows(st_read(
    here(
      "data",
      "raw",
      "tuktoyaktuk_shapefiles",
      "bndt_107c08_shp_en",
      "107C08_water_b_a.shp"
    )
  )) %>%
  st_union() %>%
  st_crop(bbox)

stations_sf <- stations_sf %>%
  st_transform(st_crs(water))

# https://www.latlong.net/place/tuktoyaktuk-nt-canada-4297.html
tuktoyaktuk <- tibble(name = "Tuktoyaktuk", longitude = -133.034180, latitude = 69.445358) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

poi <- tuktoyaktuk %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(label = "\uf041") %>%
  mutate(name = "Tuktoyaktuk")

p2 <- water %>%
  ggplot() +
  geom_sf(fill = "white", size = 0.1) +
  geom_sf(data = stations_sf, size = 0.5) +
  ggrepel::geom_text_repel(
    data = stations,
    aes(x = longitude_w, y = latitude_n, label = sample),
    label.size = NA,
    label.padding = 0.1,
    box.padding = 0.2,
    min.segment.length = 0.25,
    fill = alpha("white", 0.6),
    size = 2,
    hjust = 1,
    fontface = "bold",
    # family = "Montserrat Light",
    segment.size = unit(0.1, "mm"),
    max.iter = Inf,
    max.time = 2,
    show.legend = FALSE
  ) +
  geom_text(
    data = poi,
    aes(x = X, y = Y, label = label),
    size = 4,
    color = "#007d57",
    family = "fontawesome-webfont"
  ) +
  geom_label(
    data = poi,
    aes(x = X, y = Y, label = name),
    size = 3,
    family = "Exo",
    color = "gray25",
    fontface = "bold",
    label.size = NA,
    fill = alpha("grey95", 0.6),
    label.padding = unit(0.1, "lines"),
    vjust = 2
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  coord_sf(xlim = c(-133.06, -132.95), ylim = c(69.44, 69.47)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "grey75", color = NA),
    plot.background = element_rect(color = NA),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_text(size = 5)
  )

# Combine -----------------------------------------------------------------

p <- p1 +
  inset_element(p2, 0.5, -0.08, 0.94, 0.35, align_to = "panel", clip = TRUE)

file <- here("graphs", "fig13.pdf")

ggsave(
  file,
  device = cairo_pdf,
  width = 180,
  height = 180,
  units = "mm"
)

knitr::plot_crop(file)
