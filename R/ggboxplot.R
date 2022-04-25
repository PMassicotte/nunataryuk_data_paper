ggboxlpot <- function(df, expedition, y, strip.text = element_blank(), ylab) {

  # browser()

  p <- ggplot(df, aes(x = paste("Leg", {{ expedition }}), y = {{ y }}, fill = factor({{ expedition }}))) +
    geom_boxplot(size = 0.1, outlier.size = 0.25) +
    paletteer::scale_fill_paletteer_d("suffrager::london") +
    # scale_y_log10(labels = scales::label_number()) +
    # annotation_logticks(sides = "l", size = 0.1) +
    # scale_x_discrete(labels = function(x) {paste("Leg", expedition)}) +
    labs(
      x = NULL,
      y = parse(text = ylab)
    ) +
    theme(
      legend.position = "none",
      strip.text = strip.text
    )

  return(p)
}
