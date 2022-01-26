# Setup -------------------------------------------------------------------

library(geobr)
library(GGally)
library(glue)
library(GWmodel)
library(hms)
library(lubridate)
library(osmdata)
library(readxl)
library(rgeoda)
library(sf)
library(spdep)
library(tidyverse)
library(tmap)

input <- "data/input"
output <- "data/output"

# 01 - NDS data arrangement -----------------------------------------------

source("R/01_nds.R")

# 02 - TAZ Data -----------------------------------------------------------

source("R/02_taz.R")

# 03 - Exploratory data analysis on NDS data ------------------------------

source("R/03_edands.R")

# 04 - GWR Model ----------------------------------------------------------

source("R/04_gwr.R")

# 05 - LISA Clustering ---------------------------------------------------

source("R/05_clusters.R")

# 06 - Additional maps and plots -----------------------------------------

source("R/06_misc.R")


