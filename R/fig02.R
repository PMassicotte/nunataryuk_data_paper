# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Mackenzie discharge timeseries.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# https://arcticgreatrivers.org/discharge/

# https://climate.weather.gc.ca/climate_data/daily_data_e.html?hlyRange=1994-08-30%7C2021-11-29&dlyRange=1995-04-21%7C2021-11-29&mlyRange=1999-10-01%7C2006-07-01&StationID=26987&Prov=NT&urlExtension=_e.html&searchType=stnName&optLimit=yearRange&StartYear=1840&EndYear=2021&selRowPerPage=25&Line=1&searchMethod=contains&Month=1&Day=29&txtStationName=Tuktoyaktuk+&timeframe=2&Year=2019

rm(list = ls())

discharge <- readxl::read_excel(here(
  "data",
  "raw",
  "Mackenzie_ArcticRedRiver_Version_20211118.xlsx"
)) %>%
  mutate(date = as.Date(date))

discharge

discharge <- discharge %>%
  filter(lubridate::year(date) == 2019) %>%
  mutate(discharge = parse_number(discharge))

discharge

# Time of the sampling ----------------------------------------------------

stations <- read_csv2(here(
  "data",
  "raw",
  "NUNA2019_WP4_main_results_2021_01_23.csv"
))

stations

legs <- stations %>%
  group_by(cruise) %>%
  filter(
    sampling_date_utc == min(sampling_date_utc) |
      sampling_date_utc == max(sampling_date_utc)
  ) %>%
  distinct(cruise, sampling_date_utc) %>%
  mutate(bound = ifelse(sampling_date_utc == min(sampling_date_utc), "start", "end")) %>%
  pivot_wider(names_from = bound, values_from = sampling_date_utc)

legs

setDT(legs)
setDT(discharge)

discharge[, date_discharge := date]

leg_periods <- discharge[legs, on = .(date >= start, date <= end)] %>%
  as_tibble()

p1 <- discharge %>%
  ggplot(aes(x = date, y = discharge)) +
  geom_line(color = "grey60", size = 0.5) +
  geom_line(data = leg_periods, aes(x = date_discharge, color = factor(cruise)), size = 0.5) +
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
  scale_x_date(expand = expansion(mult = c(0.01, 0.1))) +
  labs(
    x = NULL,
    y = quote("Discharge" ~ (m^3 ~ sec^{-1}))
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0.01, 1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.key.height = unit(0.01, "cm"),
    legend.key.width = unit(1, "cm")
  )

# Air temperature ---------------------------------------------------------

airtemp <- read_csv(here("data", "raw", "air_temperature", "2019_01_01.csv")) %>%
  janitor::clean_names()

airtemp

## Rolling mean ----

# 10 days rolling mean

airtemp <- airtemp %>%
  mutate(roll_mean = slider::slide_dbl(
    mean_temp_c,
    mean,
    .before = 5,
    .after = 5,
    na.rm = TRUE
  ))

## Add leg information to the time series ----

setDT(airtemp)

airtemp[, date_airtemp := date_time]

leg_periods <- airtemp[legs, on = .(date_time >= start, date_time <= end)] %>%
  as_tibble()

## Plot ----

p2 <- airtemp %>%
  ggplot(aes(x = date_time, y = mean_temp_c)) +
  geom_point(size = 0.5, color = "gray80") +
  geom_line(aes(y = roll_mean), color = "gray60") +
  geom_line(
    data = leg_periods,
    aes(
      x = date_airtemp,
      y = roll_mean,
      color = factor(cruise)
    ),
    size = 0.5
  ) +
  paletteer::scale_color_paletteer_d("suffrager::london") +
  geom_hline(
    yintercept = 0,
    lty = 2,
    size = 0.25
  ) +
  scale_x_date(expand = expansion(mult = c(0.01, 0.1))) +
  labs(
    x = NULL,
    y = "Air temperature (°C)"
  ) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )

# Combine plots -----------------------------------------------------------

p <- p1 / p2 +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs", "fig02.pdf"),
  device = cairo_pdf,
  width = 120,
  height = 120,
  units = "mm"
)
