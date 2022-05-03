df_table <- tibble(
  Variable = c(
    "Event",
    "Expedition",
    "Latitude",
    "Longitude",
    "Date/Time",
    "Sample ID",
    "Depth water"
  ),
  Description = c(
    "Event identifier (project name_expedition_station)",
    "Expedition identifier ('1', '2', '3', '4') ; also referred to as 'Leg'",
    "Latitude of the sampling site (degree decimals)",
    "Longitude of the sampling site (degree decimals)",
    "Sampling date and time (UTC), following this format : dd.mm.YYYY HH:MM",
    "Sample identifier corresponding to the station code",
    "Depth (m) at which the measurement was made or the sample taken"
  )
)

table <- kable(
  df_table,
  "latex",
  longtable = FALSE,
  booktabs = TRUE,
  escape = TRUE,
  caption = "Descriptions of the minimal variables included in each data set."
) |>
  kable_styling(
    latex_options = c("repeat_header"),
    wraptable_width = "5pt"
  )

write_lines(table, here("tables", "table1.tex"))
