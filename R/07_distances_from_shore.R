bbox <- st_read(here("data", "raw", "bbox.geojson")) %>%
  st_transform(crs = 4326)

stations <- read_csv(here("data", "clean", "merged_data.csv")) %>%
  select(
    sample_id,
    expedition,
    longitude = contains("longitude"),
    latitude = contains("latitude"),
    sal_ctd
  ) %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

stations

ne_land <- rnaturalearth::ne_download(
  category = "physical",
  type = "land",
  returnclass = "sf",
  scale = "medium"
) %>%
  st_union() %>%
  st_crop(bbox)

st_nearest_points(stations, ne_land) %>%
  st_length()

st_distance(stations, ne_land)

stations %>%
  mutate(distance = st_distance(., ne_land)) %>%
  ggplot(aes(x = as.numeric(distance), y = sal_ctd)) +
  geom_point()

stations %>%
  mutate(distance = as.numeric(st_distance(., ne_land))) %>%
  ggplot() +
  geom_sf(data = st_nearest_points(stations, ne_land), size = 0.1) +
  geom_sf(data = ne_land, size = 0.1) +
  geom_sf(aes(color = factor(expedition), size = distance)) +
  coord_sf(
    xlim = c(-140, -132.5),
    ylim = c(68, 70),
    expand = FALSE
  )
