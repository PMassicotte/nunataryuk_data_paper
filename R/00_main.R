# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# FILE:         main.R
#
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Load libraries and script recipes.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(tidyverse)
library(ggpmthemes)
library(glue)
library(here)
library(sf)
library(rnaturalearth)
library(terra)
library(MBA)
library(ggspatial)
library(data.table)
library(patchwork)
# library(googlesheets4)

# library(furrr)
# plan(multisession(workers = availableCores() - 2))

# renv::install("MilesMcBain/breakerofchains")
# renv::install("mcguinlu/pathformatr")

# renv::install("ropensci/rnaturalearthdata")
# renv::install("ropensci/rnaturalearthhires")

# Set default ggplot2 font size and font family
# devtools::install_github("PMassicotte/ggpmthemes")
theme_set(theme_light_modified(base_family = "Montserrat", base_size = 10))

theme_update(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = 12),
  panel.border = element_blank(),
  axis.ticks = element_blank()
)

sf::sf_use_s2(FALSE)

# Scripts -----------------------------------------------------------------

source(here("R", "fig01.R"))
source(here("R", "fig02.R"))
source(here("R", "fig03.R"))
source(here("R", "fig04.R"))
source(here("R", "fig05.R"))
source(here("R", "fig06.R"))
source(here("R", "fig07.R"))
source(here("R", "fig08.R"))
source(here("R", "fig09.R"))
source(here("R", "fig10.R"))

source(here("R", "appendix01.R"))
source(here("R", "appendix02.R"))
source(here("R", "appendix03.R"))
