#Title: Problem Set 10
#Author: Kerri Rose Riley
#Purpose: To draw a series of maps for the 2024 election using the Healy guidelines
#Output: Figures and Quarto Document typeset as a .pdf


# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 10")


# Install Packages
install.packages(c(
  "tidyverse",
  "socviz",
  "sf",
  "statebins",
  "ggrepel",
  "maps"
))

# Import Packages
library(tidyverse)  # ggplot2, dplyr, etc.
library(socviz)   # election24, states_sf, counties_sf, opiates, %nin%, theme_map()
library(sf)     # needed for geom_sf() and the *_sf objects
library(statebins)  # geom_statebins()
library(ggrepel)   # geom_label_repel() in the opiates time-series plot
library(maps) # state outlines

# Load in the data sets
election <- read_csv("~/Desktop/Current Classes/R/Problem Set 10/Data/midterms_2026_preliminary.csv")

# Load 2024 election data
data("election24")

# Load state geographic data
data("states_sf")

# Load county geographic data
data("counties_sf")

# Examine your datasets
View(election24)
View(states_sf)
View(counties_sf)

# See all variable names
names(election24)
names(states_sf)
names(counties_sf)


