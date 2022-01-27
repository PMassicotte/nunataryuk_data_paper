# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Rrs spectra from the C-OPS across the different legs. Note that
# there are no spectra for leg #1.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_data.csv"))

# Using scale_color_manual because no data for leg1, so I manually set the
# correct colors for legs 2-3-4.

p <- df %>%
  select(event, expedition, contains("rrs")) %>%
  pivot_longer(-c(expedition, event)) %>%
  drop_na() %>%
  mutate(wavelength = parse_number(str_extract(name, "\\d{3}"))) %>%
  ggplot(aes(x = wavelength, y = value, group = event, color = factor(expedition))) +
  geom_line(size = 0.25) +
  scale_color_manual(
    breaks = c(2, 3, 4),
    values = c("#E56B1EFF", "#FFCD22FF", "#15274DFF")
  ) +
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
