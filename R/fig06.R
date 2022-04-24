# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Boxplots for DOM/DOC related variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source(here("R", "ggboxplot.R"))

df <- read_csv(here("data", "clean", "merged_data.csv"))

df

df_viz <- df %>%
  select(
    event,
    expedition,
    depth_water_m,
    sal_ctd,
    temp_ctd_c,
    d18o_vs_smow,
    a_cdom350_1_m,
    a_cdom443_1_m,
    doc_mg_l,
    spm_mg_l,
    poc_mug_ml,
    a_p443_1_m
  ) %>%
  # Converting absorption to absorbance
  mutate(suva350 = (a_cdom350_1_m / 2.303) / doc_mg_l)

# Boxplots ----------------------------------------------------------------

p1 <- ggboxlpot(df_viz, expedition = expedition, y = sal_ctd, ylab = "Salinity")
p2 <- ggboxlpot(df_viz, expedition = expedition, y = temp_ctd_c, ylab = "Temperature~(C)")
p3 <- ggboxlpot(df_viz, expedition = expedition, y = d18o_vs_smow, ylab = "delta^{18}*O")
p4 <- ggboxlpot(df_viz, expedition = expedition, y = a_cdom443_1_m, ylab = "a[CDOM](443)~(m^{-1})")
p5 <- ggboxlpot(df_viz, expedition = expedition, y = doc_mg_l, ylab = "DOC~(mg~L^{-1})")
p6 <- ggboxlpot(df_viz, expedition = expedition, y = suva350, ylab = "SUVA[350]~(m^2~gC^{-1})")
p7 <- ggboxlpot(df_viz, expedition = expedition, y = spm_mg_l, ylab = "SPM~(mu*g~mL^{-1})")
p8 <- ggboxlpot(df_viz, expedition = expedition, y = poc_mug_ml, ylab = "POC~(mu*g~mL^{-1})")
p9 <- ggboxlpot(df_viz, expedition = expedition, y = a_p443_1_m, ylab = "a[p](443)~(m^{-1})")

# Combine and save --------------------------------------------------------

p <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 +
  plot_layout(ncol = 3, nrow = 3) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(face = "bold"))

ggsave(
  here("graphs", "fig06.pdf"),
  device = cairo_pdf,
  width = 200,
  height = 140,
  units = "mm"
)
