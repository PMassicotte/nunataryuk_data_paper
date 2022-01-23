# Showing bacteria abundances across the expeditions.

rm(list = ls())

df <- read_csv(here("data/clean/merged_data.csv"))

df

df %>%
  # filter(bacteria_cells_m_l >= 3e5) %>%
  ggplot(aes(x = factor(expedition), y = bacteria_cells_m_l, fill = factor(expedition))) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_x_discrete(labels = function(x) {glue("Leg {x}")}) +
  scale_y_log10(labels = scales::label_number_si()) +
  paletteer::scale_fill_paletteer_d("suffrager::london",) +
  labs(
    x = NULL,
    y = quote("Bacterial abundance" ~ (cells~mL^{-1}))
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

filename <- here("graphs", "fig10.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 90,
  height = 70,
  units = "mm"
)

filename <- here("graphs", "fig10.png")

ggsave(
  filename,
  width = 90,
  height = 70,
  units = "mm",
  dpi = 300
)
