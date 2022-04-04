# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Bio-optical plots
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

stations <- read_csv(here(
  "data",
  "clean",
  "merged_data.csv"
)) %>%
  janitor::clean_names()

stations

# Plot --------------------------------------------------------------------

p1 <- stations %>%
  ggplot(aes(x = a_p443_1_m, y = poc_mug_ml)) +
  geom_point(aes(color = factor(expedition))) +
  paletteer::scale_color_paletteer_d(
    "suffrager::london",
    breaks = c(1, 2, 3, 4),
    labels = function(x) {
      paste("Leg", x)
    }
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(method = "lm", size = 0.5) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 1,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.93,
    aes(label = ..rr.label..),
    size = 2.5,
    family = "Montserrat"
  ) +
  labs(
    x = NULL,
    y = quote(POC ~ (mu*g~mL^{-1}))
  ) +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_blank()
  )

p2 <- stations %>%
  ggplot(aes(x = a_p443_1_m, y = tpc_mug_m_l)) +
  geom_point(aes(color = factor(expedition))) +
  paletteer::scale_color_paletteer_d(
    "suffrager::london",
    breaks = c(1, 2, 3, 4),
    labels = function(x) {
      paste("Leg", x)
    }
  ) +
  scale_x_log10() +
  scale_y_log10() +
  annotation_logticks(sides = "bl", size = 0.25) +
  geom_smooth(method = "lm", size = 0.5) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 1,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.93,
    aes(label = ..rr.label..),
    size = 2.5,
    family = "Montserrat"
  ) +
  labs(
    x = quote(a[p]~(443)~(m^{-1})),
    y = quote(TPC ~ (mu*g~mL^{-1}))
  ) +
  theme(
    panel.border = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.95, 0.05),
    legend.text = element_text(family = "Montserrat"),
    legend.title = element_blank(),
    axis.ticks = element_blank()
  )

p <- p1 / p2 +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs", "fig07.pdf"),
  device = cairo_pdf,
  width = 120,
  height = 160,
  units = "mm"
)
