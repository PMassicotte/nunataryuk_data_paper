# Showing bacteria abundances across the expeditions.

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_data.csv"))

df

# Chlorophyll-a -----------------------------------------------------------

p1 <- df %>%
  ggplot(aes(x = factor(expedition), y = chl_a_mg_m3, fill = factor(expedition))) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  # scale_y_log10() +
  # annotation_logticks(sides = "l", size = 0.1) +
  scale_x_discrete(labels = function(x) {
    glue("Leg {x}")
  }) +
  paletteer::scale_fill_paletteer_d("suffrager::london") +
  labs(
    x = NULL,
    y = quote("Chlorophyll-a" ~ (mg ~ m^{
      -3
    }))
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )


# Total bacterial abundance -----------------------------------------------

p2 <- df %>%
  # filter(bacteria_cells_m_l >= 3e5) %>%
  ggplot(aes(x = factor(expedition), y = bacteria_cells_m_l, fill = factor(expedition))) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_x_discrete(labels = function(x) {
    glue("Leg {x}")
  }) +
  scale_y_log10(labels = scales::label_number_si()) +
  paletteer::scale_fill_paletteer_d("suffrager::london", ) +
  labs(
    x = NULL,
    y = quote("Bacterial abundance" ~ (cells ~ mL^{
      -1
    }))
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

# Combine and save --------------------------------------------------------

p <- p1 + p2 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

filename <- here("graphs", "fig10.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 90,
  height = 120,
  units = "mm"
)
