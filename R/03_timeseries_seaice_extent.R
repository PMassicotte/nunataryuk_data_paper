bb <- st_bbox(c(xmin =-140, xmax = -132.5, ymin = 68, ymax = 70), crs = 4326)

files <-
  fs::dir_ls(here("data", "raw", "ice_cover", "sic"),
    glob = "*.shp",
    recurse = TRUE
  )

seaice_extent <- function(file, bb) {

  seaice <- st_read(file)

  bb_stereo <- bb %>%
    st_as_sfc() %>%
    st_transform(st_crs(seaice))

  area <- seaice %>%
    st_make_valid() %>%
    st_crop(bb_stereo) %>%
    st_combine() %>%
    st_area() %>%
    units::set_units(km^2)

  df <- tibble(
    date = str_extract(file, "\\d{7}"),
    area = area
  ) %>%
    mutate(date = as.Date(date, "%Y%j"))

}

r <- seaice_extent(files[1], bb)

r

df <- map_dfr(files, seaice_extent, bb = bb)

df %>%
  mutate(area = as.numeric(area)) %>%
  ggplot(aes(x = date, y = area)) +
  geom_line()
