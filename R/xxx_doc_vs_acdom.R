df <- read_csv(here("data", "clean", "merged_data.csv"))

df %>%
  ggplot(aes(x = doc_mg_l, y = a_cdom443_1_m, color = factor(expedition))) +
  geom_point() +
  # scale_x_log10() +
  # scale_y_log10() +
  geom_smooth(method = "lm", show.legend = FALSE, alpha = 0.25) +
  paletteer::scale_color_paletteer_d(
    "lisa::Jean_MichelBasquiat_1",
    labels = function(x) {
      paste("Leg", x)
    },
    guide = guide_legend(
      title = element_blank(),
      override.aes = list(size = 1),
      label.position = "top",
      label.theme = element_text(size = 5, family = "Montserrat")
    )
  )
