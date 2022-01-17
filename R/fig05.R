absorption <-
  readxl::read_excel(here("data/raw/NUNA2019_acdom_final_2021_01_23.xlsx")) %>%
  pivot_longer(
    starts_with("acdom"),
    names_pattern = c("acdom_(.*)_(\\d{3})_nm"),
    names_to = c("type", "wavelength"),
    values_to = "absorption",
    values_transform = list(absorption = parse_number),
    names_transform = list(wavelength = parse_number)
  ) %>%
  mutate(across(c(
    time_start_utc,
    lat_decdeg,
    long_decdeg
  ), parse_number))

absorption

absorption %>%
  count(station_id, datetime_utc, wavelength)

df_viz <- absorption %>%
  pivot_wider(
    names_from = type,
    values_from = absorption,
    names_prefix = "absorption_"
  ) %>%
  filter(between(wavelength, 350, 700))

df_viz %>%
  filter(wavelength == 350) %>%
  count(absorption_qf)

# Plot --------------------------------------------------------------------

p <- df_viz %>%
  # filter(absorption_qf == 0) %>%
  ggplot(aes(
    x = wavelength,
    y = absorption_dt,
    color = factor(cruise),
    group = interaction(station_id, datetime_utc)
  )) +
  geom_line(size = 0.1) +
  # paletteer::scale_color_paletteer_d("lisa::Jean_MichelBasquiat_1") +
  paletteer::scale_color_paletteer_d("suffrager::london",) +
  labs(
    x = "Wavelength (nm)",
    y = quote(a[CDOM]~(lambda) ~ (m^{-1}))
  ) +
  facet_wrap(~glue("Leg {cruise}")) +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    axis.ticks = element_blank()
  )

ggsave(
  here("graphs", "fig05.pdf"),
  device = cairo_pdf,
  width = 120,
  height = 90,
  units = "mm"
)
