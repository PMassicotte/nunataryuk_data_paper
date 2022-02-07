# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Showing the relationship (linear model) used to calibrate the
# CTD depth sensor from atmospheric pressure downloaded from Environment Canada.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# Data given/extracted by Guislain

df <- read_csv(
  here("data", "raw", "Nunataryuk_depth_tare.csv"),
  name_repair = janitor::make_clean_names
)

df

df %>%
  ggplot(aes(x = pres_dbar_ec, y = ctd_press_dbar_avg)) +
  geom_point(aes(color = df_sector)) +
  paletteer::scale_color_paletteer_d(
    "suffrager::CarolMan",
    guide = guide_legend(
      label.theme = element_text(size = 6, family = "Roboto Light"),
      label.position = "bottom",
      override.aes = list(size = 2),
      ncol = 2
    )
  ) +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  scale_x_continuous(breaks = scales::breaks_pretty()) +
  geom_smooth(method = "lm", size = 0.5, color = "#E25247FF", alpha = 0.25) +
  ggpmisc::stat_poly_eq(
    aes(label = ..eq.label..),
    label.y.npc = 0.95,
    size = 2.5,
    family = "Montserrat"
  ) +
  ggpmisc::stat_poly_eq(
    label.y.npc = 0.90,
    aes(label = ..rr.label..),
    size = 2.5,
    family = "Montserrat"
  ) +
  labs(
    x = "Environment Canada atmospheric pressure (kPa)",
    y = "In-situ CTD based depth measured in air (m)"
  ) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.99, 0.05),
    legend.title = element_blank(),
    legend.direction = "horizontal"
  )

ggsave(
  here("graphs", "appendix03.pdf"),
  device = cairo_pdf,
  width = 120,
  height = 90,
  units = "mm"
)
