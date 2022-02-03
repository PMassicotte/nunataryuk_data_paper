# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Vertical profiles of temperature and salinity measured by the
# CTD.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- readxl::read_excel(
  here("data/raw/Nunataryuk WP4 Mackenzie 2019.xlsx"),
  sheet = "Sub dataset 1",
  .name_repair = janitor::make_clean_names
)

pdf(here("graphs", "appendix01.pdf"), width = 8, height = 6)

par(mfrow = c(1, 2))

df2 <- df %>%
  filter(str_detect(event, "3_STN125"))

plot(
  df2$temp_ctd_c,
  df2$depth_water_m,
  type = "l",
  ylim = rev(range(df2$depth_water_m)),
  lwd = 2,
  ylab = "Depth (m)",
  col = "#01BAEF",
  tck = -0.02,
  xlab = "",
  xaxt = "none",
  las = 2,
  xlim = c(0, 10)
)

grid(lty = 2, col = "gray", lwd = 0.5)

axis(1,
  tck = -0.02,
  col.axis = "#01BAEF",
  col.ticks = "#01BAEF"
)

mtext("Temperature (C)",
  side = 1,
  line = 3,
  col = "#01BAEF"
)

mtext("A", side = 3, line = -1, adj = 0.95)

par(new = TRUE)

plot(
  df2$sal_ctd,
  df2$depth_water_m,
  type = "l",
  ylim = rev(range(df2$depth_water_m)),
  lwd = 2,
  axes = FALSE,
  xlab = "",
  ylab = "",
  col = "#FE4E00",
  tck = -0.02
)

axis(
  side = 3,
  at = pretty(range(df2$sal_ctd)),
  tck = -0.02,
  col.axis = "#FE4E00",
  col.ticks = "#FE4E00"
)

mtext("Salinity",
  side = 3,
  line = 3,
  col = "#FE4E00"
)

## Stable vertical profile ----

df2 <- df %>%
  filter(str_detect(event, "3_STNR09"))

plot(
  df2$temp_ctd_c,
  df2$depth_water_m,
  type = "l",
  ylim = rev(range(df2$depth_water_m)),
  lwd = 2,
  ylab = "Depth (m)",
  col = "#01BAEF",
  tck = -0.02,
  xlab = "",
  xaxt = "none",
  las = 2,
  xlim = c(18, 20)
)
grid(lty = 2, col = "gray", lwd = 0.5)

axis(1,
  tck = -0.02,
  col.axis = "#01BAEF",
  col.ticks = "#01BAEF"
)

mtext("Temperature (C)",
  side = 1,
  line = 3,
  col = "#01BAEF"
)

mtext("B", side = 3, line = -1, adj = 0.95)

par(new = TRUE)

plot(
  df2$sal_ctd,
  df2$depth_water_m,
  type = "l",
  ylim = rev(range(df2$depth_water_m)),
  lwd = 2,
  axes = FALSE,
  xlab = "",
  ylab = "",
  col = "#FE4E00",
  tck = -0.01,
  xlim = c(0, 1)
)

axis(
  side = 3,
  # at = pretty(range(df2$sal_ctd)),
  tck = -0.02,
  col.axis = "#FE4E00",
  col.ticks = "#FE4E00",
)

mtext("Salinity",
  side = 3,
  line = 3,
  col = "#FE4E00"
)

dev.off()
