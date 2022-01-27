rm(list = ls())

df <- read_csv(here("data", "clean", "merged_data.csv"))

df %>%
  select(event, expedition, contains("rrs")) %>%
  pivot_longer(-c(expedition, event)) %>%
  drop_na() %>%
  mutate(wavelength = parse_number(str_extract(name, "\\d{3}"))) %>%
  ggplot(aes(x = wavelength, y = value, group = event, color = factor(expedition))) +
  geom_line(size = 0.25) +
  paletteer::scale_color_paletteer_d("suffrager::london") +
  labs(
    x = "Wavelength (nm)",
    y = quote(Rrs~(sr^{-1}))
  ) +
  facet_wrap(~glue("Leg {expedition}"), ncol = 1) +
  theme(
    legend.position = "none"
  )

ggsave(
  here("graphs", "fig10.pdf"),
  device = cairo_pdf,
  width = 80,
  height = 140,
  units = "mm"
)
