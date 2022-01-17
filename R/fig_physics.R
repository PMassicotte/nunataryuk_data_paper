# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Exploring physics parameters (temperature, salinity, etc.).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_data.csv"))

df

df %>%
  count(sample_id, expedition, date_time_dd_mm_yyyy_hh_mm, sort = TRUE) %>%
  assertr::verify(n == 1)

# Most measurements were made near the surface

df %>%
  ggplot(aes(x = depth_water_m)) +
  geom_histogram()

# Temperature -------------------------------------------------------------

p1 <- df %>%
  drop_na(temp_ctd_c) %>%
  group_by(sample_id, expedition) %>%
  summarise(temp_ctd_c = mean(temp_ctd_c, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(expedition), y = temp_ctd_c)) +
  geom_boxplot() +
  scale_y_log10()

p1

# Salinity ----------------------------------------------------------------

df %>%
  drop_na(sal_ctd) %>%
  ggplot(aes(x = sal_ctd)) +
  geom_histogram() +
  scale_x_log10()

df %>%
  drop_na(pon_mug_m_l, bacteria_cells_m_l) %>%
  ggplot(aes(x = d18o_vs_smow, y = d_d_vs_smow)) +
  geom_point()

df %>%
  ggplot(aes(x = cdom_s350_500_1_nm)) +
  geom_histogram()

df %>%
  ggplot(aes(x = a_cdom443_1_m)) +
  geom_histogram()

df %>%
  ggplot(aes(x = a_p443_1_m)) +
  geom_histogram()

df %>%
  ggplot(aes(x = a_cdom350_1_m, y = fdom_m_peak_ru)) +
  geom_point()
