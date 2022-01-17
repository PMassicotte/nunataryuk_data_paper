df <- read_csv(here("data", "clean", "merged_data.csv"))

df

df %>%
  ggplot(aes(x = latitude_dec_deg, y = sal_ctd)) +
  geom_point()
