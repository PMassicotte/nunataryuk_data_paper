# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Boxplots showing the nutrients across the different legs.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

stations <- read_csv(here(
  "data",
  "clean",
  "merged_data.csv"
)) %>%
  janitor::clean_names()

stations

stations %>%
  count(sample_id, date_time_dd_mm_yyyy_hh_mm, depth_water_m, expedition, sort = TRUE) %>%
  assertr::verify(n == 1)

stations %>%
  count(expedition, sample_id)

# Plot --------------------------------------------------------------------

p1 <- stations %>%
  ggplot(aes(
    x = factor(expedition),
    y = no3_mumol_l,
    fill = factor(expedition)
  )) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  labs(
    x = NULL,
    y = quote(NO[3]^{"-"} ~ (mu*Mol~L^{-1}))
  ) +
  paletteer::scale_fill_paletteer_d("suffrager::london") +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank()
  )

p2 <- stations %>%
  ggplot(aes(x = factor(expedition), y = no2_mumol_l, fill = factor(expedition))) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_x_discrete(labels = function(x) {glue("Leg {x}")}) +
  paletteer::scale_fill_paletteer_d("suffrager::london",) +
  labs(
    x = NULL,
    y = quote(NO[2]^{"-"} ~ (mu*Mol~L^{-1}))
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    panel.border = element_blank()
  )

p3 <- stations %>%
  ggplot(aes(x = factor(expedition), y = po4_mumol_l, fill = factor(expedition))) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_x_discrete(labels = function(x) {glue("Leg {x}")}) +
  paletteer::scale_fill_paletteer_d("suffrager::london",) +
  labs(
    x = NULL,
    y = quote(PO[4]^{"3-"} ~ (mu*Mol~L^{-1}))
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

p4 <- stations %>%
  ggplot(aes(x = factor(expedition), y = si_o4_mumol_l, fill = factor(expedition))) +
  geom_boxplot(size = 0.1, outlier.size = 0.1) +
  scale_y_log10() +
  annotation_logticks(sides = "l", size = 0.1) +
  scale_x_discrete(labels = function(x) {glue("Leg {x}")}) +
  paletteer::scale_fill_paletteer_d("suffrager::london",) +
  labs(
    x = NULL,
    y = quote(sio[4]^4["-"] ~ (mu*Mol~L^{-1}))
  ) +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.border = element_blank()
  )

p <- p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))

filename <- here("graphs", "fig09.pdf")

ggsave(
  filename,
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)

## Stats for the paper ----

stations %>%
  select(
    expedition,
    no3_mumol_l,
    no2_mumol_l,
    po4_mumol_l,
    si_o4_mumol_l
  ) %>%
  group_by(expedition) %>%
  summarise(across(
    c(no3_mumol_l, no2_mumol_l, po4_mumol_l, si_o4_mumol_l),
    .fns = list(min = min, max = max, mean = mean), na.rm = TRUE
  ))
