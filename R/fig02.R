# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Overview of the temporal sampling.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_data.csv"))

df

df %>%
  group_by(expedition) %>%
  summarise(
    n = n(),
    timeframe = max(date_time_dd_mm_yyyy_hh_mm) - min(date_time_dd_mm_yyyy_hh_mm)
  ) %>%
  mutate(timeframe = round(timeframe))

p <- df %>%
  count(expedition, date = as.Date(date_time_dd_mm_yyyy_hh_mm)) %>%
  ggplot(aes(
    x = date,
    y = n,
    color = factor(expedition)
  )) +
  geom_segment(aes(
    x = date,
    xend = date,
    y = 0,
    yend = n
  ), show.legend = FALSE, size = 1) +
  geom_point(size = 6, pch = 21, stroke = 1, fill = "white", show.legend = FALSE) +
  geom_text(aes(label = n), color = "#3c3c3c", show.legend = FALSE, size = 3) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.025, 0.1))) +
  paletteer::scale_color_paletteer_d(
    "suffrager::london",
    labels = function(x) {
      paste("Leg", x)
    },
    guide = guide_legend(
      title = element_blank(),
      override.aes = list(size = 1),
      label.position = "top",
      label.theme = element_text(size = 7, family = "Montserrat")
    )
  ) +
  labs(
    x = NULL,
    y = "Number of sampled stations"
  ) +
  facet_wrap(~glue("Leg {expedition}"), scales = "free_x")

ggsave(
  here("graphs", "fig02.pdf"),
  device = cairo_pdf,
  width = 180,
  height = 120,
  units = "mm"
)
