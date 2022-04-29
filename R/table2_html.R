# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a table with all measured variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# https://malco.io/2020/05/16/replicating-an-nyt-table-of-swedish-covid-deaths-with-gt/

rm(list = ls())

df <- read_csv(
  here(
    "data",
    "raw",
    "DP Nunataryuk tables DRAFT.xlsx - Copy of Table 2. Parameters.csv"
  ),
  na = c("not detected", "N/A", "")
) |>
  drop_na(Variable, Group) |>
  # type_convert() |>
  select(!starts_with("...")) # Remove

df

df_table <- df |>
  group_by(Group) |>
  arrange(Variable) |>
  nest() |>
  drop_na(Group) |>
  arrange(Group) |>
  unnest(data) |>
  ungroup()

df_table

table2 <- df_table |>
  gt(rowname_col = "Group", groupname_col = "Origin") |>
  row_group_order(groups = c(
    "Water",
    "Sediment/Pore water",
    "Coastal permafrost thaw water and surficial seawater at the nearshore"
  )) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) |>
  fmt_scientific(
    columns = where(is.numeric),
    rows = Average >= 1000,
    decimals = 1
  ) |>
  fmt_missing(
    columns = everything(),
    missing_text = "N/A"
  ) |>
  cols_width(
    c(Group, Variable, Method, `Principal investigator`) ~ px(40),
    `Standard Deviation` ~ px(10)
  ) |>
  tab_options(
    row_group.font.weight = "bold",
    row_group.font.size = px(14),
    data_row.padding = px(1),
    table.font.size = px(12),
    column_labels.font.size = px(14),
    column_labels.font.weight = "bold"
  )

table2

# Find the best way to include the table into the final LaTeX file.
gtsave(table2, here("tables", "table2.html"))
gtsave(table2, here("tables", "table2.tex"))
