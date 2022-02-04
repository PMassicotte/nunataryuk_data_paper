# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Appendix figure showing the SimulO simlations data used for
# shelf-shading correction of the C-OPS data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv2(here("data", "raw", "IceProShading.csv")) %>%
  janitor::clean_names() %>%
  type_convert() %>%
  mutate(ff = case_when(
    ff == 4 ~ "FF 4.00%",
    ff == 183 ~ "FF 1.83%"
  ))

df

# Plot and save -----------------------------------------------------------

formula <- y ~ 1 - exp(a * x)

p <- df %>%
  ggplot(aes(
    x = x_a_bb,
    y = y_shading
  )) +
  geom_point(aes(color = factor(sun_zen_angle), shape = ff)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
  geom_smooth(
    method = "nls",
    formula = formula,
    method.args = list(start = list(a = -0.1)),
    color = "#3c3c3c",
    size = 0.5,
    alpha = 0.25,
    se = FALSE
  ) +
  ggpmisc::stat_fit_tidy(
    method = "nls",
    method.args = list(formula = formula, start = list(a = 0.1)),
    label.x = "left",
    label.y = "top",
    family = "Montserrat",
    size = 2.5,
    aes(
      label = paste("y~`=`~ 1-italic(e)^{", signif(stat(a_estimate), digits = 3), "~x}", sep = "")
    ),
    parse = TRUE
  ) +
  paletteer::scale_color_paletteer_d(
    "ggthemes::excel_Headlines",
    guide = guide_legend(
      title.position = "top",
      title.theme = element_text(size = 8, family = "Roboto Light"),
      label.theme = element_text(size = 6, family = "Roboto Light"),
      label.position = "bottom",
      override.aes = list(size = 2)
    ),
    labels = function(x) paste0(x, "°")
  ) +
  scale_shape(
    guide = guide_legend(
      title.position = "top",
      title.theme = element_text(size = 8, family = "Roboto Light"),
      label.theme = element_text(size = 6, family = "Roboto Light"),
      label.position = "bottom",
      override.aes = list(size = 2)
    )
  ) +
  labs(
    x = quote(italic(a) ~ "+" ~ italic(b[b]) ~ (m^{-1})),
    y = "Shading",
    shape = "Volume scattering function",
    color = "Sun zenith angle"
  ) +
  theme(
    legend.justification = c(1, 0),
    legend.position = c(0.99, 0.05),
    legend.direction = "horizontal"
  )

ggsave(
  here("graphs", "appendix02.pdf"),
  device = cairo_pdf,
  width = 120,
  height = 90,
  units = "mm"
)
