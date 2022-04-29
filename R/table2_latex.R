# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a LaTeX table with all measured variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

df <- read_csv(
  here(
    "data",
    "raw",
    "DP Nunataryuk tables DRAFT.xlsx - Copy of Table 2. Parameters.csv"
  ),
  na = c("not detected", "N/A", "")
) |>
  drop_na(Variable, Group) |>
  select(!starts_with("...")) # Remove

df

df_table <- df |>
  group_by(Group) |>
  arrange(Variable) |>
  nest() |>
  drop_na(Group) |>
  arrange(Group) |>
  unnest(data) |>
  ungroup() |>
  select(-c(Average:Maximum))

df_table

df_table <- df_table |>
  mutate(Origin = factor(
    Origin,
    levels = c(
      "Water",
      "Sediment/Pore water",
      "Coastal permafrost thaw water and surficial seawater at the nearshore"
    )
  )) |>
  relocate(Origin, .before = 1) |>
  arrange(Origin, Group, Variable) |>
  select(-Group)

# Latex -------------------------------------------------------------------

table <- kable(
  df_table,
  "latex",
  longtable = TRUE,
  booktabs = TRUE,
  caption = "Parameters measured during the Nunataryuk surveys. Parameters are ordered by alphabetical order.",
  escape = TRUE
) |>
  kable_styling(
    latex_options = c("repeat_header"),
    font_size = 4,
    wraptable_width = "5pt"
  ) |>
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(df_table), hline_after = TRUE) %>%
  landscape() |>
  column_spec(1, width = "1.5cm") |>
  column_spec(2, width = "1.25cm") |>
  column_spec(3:6, width = "0.05cm") |>
  column_spec(7, width = "0.75cm") |>
  column_spec(8, width = "2cm") |>
  column_spec(9, width = "1.5cm") |>
  column_spec(10, width = "2.5cm") |>
  column_spec(11, width = "1cm") |>
  column_spec(12, width = "1.5cm")

write_lines(table, here("tables", "table2.tex"))
