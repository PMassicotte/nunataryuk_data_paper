# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a table with all measured variables.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv(
  here(
    "data",
    "raw",
    "DP Nunataryuk tables DRAFT.xlsx - Copy of Table 2. Parameters.csv"
  ),
  na = c("not detected", "N/A")
) %>%
  drop_na(Variable, Group) %>%
  type_convert() %>%
  select(!starts_with("..."))

df

table2 <- df %>%
  group_by(Group) %>%
  arrange(Variable) %>%
  nest() %>%
  drop_na(Group) %>%
  arrange(Group) %>%
  unnest(data) %>%
  gt() %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2,
    suffixing = TRUE
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body(c(Method:`Reference DOI`))
  )

table2

gtsave(table2, here("data", "clean", "table2.html"))


# https://malco.io/2020/05/16/replicating-an-nyt-table-of-swedish-covid-deaths-with-gt/
