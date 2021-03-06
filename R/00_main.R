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
library(ggrepel)
library(MetBrewer) # For color palettes
library(GGally)
library(emojifont)
library(eemR)
library(gt)
library(showtext)
library(knitr)
library(kableExtra)

# renv::install("MilesMcBain/breakerofchains")
# renv::install("mcguinlu/pathformatr")
# renv::install("ropensci/rnaturalearthdata")
# renv::install("ropensci/rnaturalearthhires")
# renv::install("pmassicotte/ggpmthemes")

# Set default ggplot2 font size and font family
font_add_google("Montserrat", "Montserrat")
font_add_google("Montserrat", "Montserrat Light", regular.wt = 200)
font_add_google("Exo", "Exo")
font_add_google("Ubuntu", "Ubuntu")
showtext_auto()

theme_set(theme_minimal(base_family = "Ubuntu", base_size = 10))

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
source(here("R", "fig11.R"))
source(here("R", "fig12.R"))
source(here("R", "fig13.R"))

source(here("R", "appendix_b.R"))
source(here("R", "appendix_c.R"))
source(here("R", "appendix_d.R"))
