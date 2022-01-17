# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Tidy the data contained in all the sheets of the Excel file.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

file <- here("data", "raw", "Nunataryuk WP4 Mackenzie 2019.xlsx")

sheets <- readxl::excel_sheets(file)
sheets <- sheets[str_detect(sheets, regex("sub dataset", ignore_case = TRUE))]

# TODO: Fix when sheet 1 is ok
df <- map(
  sheets[2:13],
  ~ readxl::read_excel(
    file,
    .,
    na = "NA",
    .name_repair = ~janitor::make_clean_names(., replace = c("µ" = "mu"))
  )
) %>%
  map(. %>% rename_with(~"sample_id", contains("station_id"))) %>%
  reduce(full_join)

df

df %>%
  distinct(sample_id, expedition, longitude_dec_deg, latitude_dec_deg)

write_csv(df, here("data", "clean", "merged_data.csv"))
