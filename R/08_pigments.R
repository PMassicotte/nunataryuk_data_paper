df <- read_csv(here("data", "clean", "merged_data.csv"))

names(df)

pigments <-
  c(
    "allo_mg_m3",
    "a_b_car_mg_m3",
    "but_fuco_mg_m3",
    "diadino_mg_m3",
    "diato_mg_m3",
    "fuco_mg_m3",
    "hex_fuco_mg_m3",
    "perid_mg_m3",
    "chl_a_mg_m3",
    "chl_b_mg_m3",
    "chl_c_mg_m3",
    "zea_mg_m3"
  )

df_viz <- df %>%
  select(expedition, sample_id, any_of(pigments))

df_viz %>%
  summarise(across(-c(expedition, sample_id), ~sum(is.na(.)))) %>%
  pivot_longer(everything()) %>%
  mutate(name = fct_reorder(name, value)) %>%
  ggplot(aes(x = value, y = name)) +
  geom_col()

# Remove pigments that were not detected

pigments <-
  c(
    "allo_mg_m3",
    "a_b_car_mg_m3",
    "diadino_mg_m3",
    "fuco_mg_m3",
    "chl_a_mg_m3",
    "chl_b_mg_m3",
    "chl_c_mg_m3",
    "zea_mg_m3"
  )

df_viz <- df %>%
  select(expedition, sample_id, any_of(pigments))

# test --------------------------------------------------------------------

p <- df_viz %>%
  select(-c(sample_id)) %>%
  mutate(across(-expedition, log10)) %>%
  GGally::ggpairs(
    columns = 2:9,
    aes(
      color = factor(expedition),
      fill = factor(expedition),
      alpha = 0.5
    ),
    upper = list(continuous = wrap("cor", size = 2))
  ) +
  paletteer::scale_color_paletteer_d("suffrager::london",) +
  paletteer::scale_fill_paletteer_d("suffrager::london",) +
  theme(strip.text = element_text(size = 8))

ggsave(
  here("graphs", "08_pigment_01.pdf"),
  p,
  device = cairo_pdf,
  width = 220,
  height = 220,
  units = "mm"
)
