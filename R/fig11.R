# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Figure showing pigment proportions.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(here("data", "clean", "merged_data.csv"))

df

# Select 2-3 stations that were present in all legs -----------------------

df <- df %>%
  # select(expedition, sample_id) %>%
  mutate(station = str_remove(sample_id, "\\d_"), .after = sample_id) %>%
  add_count(station, sort = TRUE) %>%
  filter(n == 4)

df

p1 <- df %>%
  ggplot(aes(x = expedition, y = sal_ctd)) +
  geom_col() +
  facet_wrap(~station)

p2 <- df %>%
  distinct(station, .keep_all = TRUE) %>%
  st_as_sf(coords = c("longitude_dec_deg", "latitude_dec_deg"), crs = 4326) %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = station), check_overlap = FALSE)

p1 / p2

# Following a discussion with Martine, we are going with the 300 line

df_viz <- df %>%
  filter(station %in% c("STN340alt", "STN350", "STN360"))

# Select pigments ---------------------------------------------------------

names(df)

df_viz <- df_viz %>%
  select(
    station,
    expedition,
    chl_a_mg_m3,
    chl_b_mg_m3,
    chl_c_mg_m3,
    psc_mg_m3,
    allo_mg_m3,
    diadino_mg_m3,
    zea_mg_m3
  )

df_viz <- df_viz %>%
  mutate(
    npc_mg_m3 = allo_mg_m3 + diadino_mg_m3 + zea_mg_m3,
    .keep = "unused"
  ) %>%
  rowwise() %>%
  mutate(total_pigment = sum(c_across(-c(station, expedition)), na.rm = TRUE)) %>%
  filter(total_pigment > 0) %>%
  mutate(across(-c(station, expedition, total_pigment), ~.x / total_pigment)) %>%
  replace(is.na(.), 0)

# Plot --------------------------------------------------------------------

df_viz %>%
  pivot_longer(contains("mg_m3")) %>%
  ggplot(aes(x = expedition, y = value, fill = name)) +
  geom_col() +
  facet_wrap(~station)
