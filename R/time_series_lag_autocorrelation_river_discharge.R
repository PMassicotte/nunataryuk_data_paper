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

stations <- read_csv(here("data", "clean", "merged_data.csv"))

df <- stations %>%
  mutate(date = as.Date(date_time_dd_mm_yyyy_hh_mm)) %>%
  inner_join(discharge, by = "date")

df %>%
  ggplot(aes(x = discharge, y = tpc_mug_m_l, color = factor(expedition))) +
  geom_point()

# Lag correlation ---------------------------------------------------------

d2 <- discharge %>%
  select(date, discharge)

df_discharge <- stations %>%
  mutate(date = as.Date(date_time_dd_mm_yyyy_hh_mm)) %>%
  select(
    sample_id,
    expedition,
    date,
    a_p443_1_m,
    spm_mg_l,
    poc_mug_ml,
  ) %>%
  inner_join(d2, by = character()) %>%
  rename(date = date.x, discharge_date = date.y)

df_discharge

df_discharge %>%
  count(sample_id, expedition, date) %>%
  assertr::verify(n == 365)

df_viz <- df_discharge %>%
  pivot_longer(a_p443_1_m:poc_mug_ml, names_to = "variable", values_to = "value") %>%
  drop_na(value, discharge) %>%
  mutate(date_difference = date - discharge_date) %>%
  mutate(date_difference = as.numeric(date_difference)) %>%
  filter(between(date_difference, 0, 15)) %>%
  group_nest(date_difference, variable) %>%
  mutate(correlation = map_dbl(data, ~cor(log10(.$value), .$discharge)), n = n())

df_viz

lab <- c(
  a_p443_1_m = "aP",
  poc_mug_ml = "POC",
  spm_mg_l = "SPM"
)

p <- df_viz %>%
  filter(n >= 20) %>%
  ggplot(aes(x = date_difference, y = correlation)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  labs(
    x = "Number of days prior to the sampling",
    y = quote("Pearson's correlation" ~(italic(r)))
  ) +
  facet_wrap(~variable, ncol = 1, labeller = labeller(variable = lab)) +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank()
  )

p

# filename <- here("graphs", "fig06.pdf")
#
# ggsave(
#   filename,
#   device = cairo_pdf,
#   width = 100,
#   height = 160,
#   units = "mm"
# )
