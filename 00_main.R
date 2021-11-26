# Setup -------------------------------------------------------------------

library(geobr)
library(GGally)
library(GWmodel)
library(hms)
library(lubridate)
library(rgeoda)
library(sf)
library(spdep)
library(tidyverse)
library(tmap)

# 01 - NDS data arrangement -----------------------------------------------

source("01_nds.R")

# 02 - TAZ Data -----------------------------------------------------------

source("02_taz.R")

# 03 - Exploratory data analysis on NDS data ------------------------------

source("03_edands.R")

# 04 - GWR Model ----------------------------------------------------------

source("04_gwr.R")

# 05 - LISA Clustering ---------------------------------------------------

source("05_clusters.R")

# 06 - Misc. analysis ----------------------------------------------------

