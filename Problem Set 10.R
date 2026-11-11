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


# Match election data to state geometries
states_map <- states_sf %>%
  left_join(election24, by = "fips")

# Match county data to election data
# Assuming election24 contains state-level election results
# County election winners require county-level election results.



# Map 1
map1 <- ggplot(states_map) +
  geom_sf(aes(fill = winner),
          color = "white",
          linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Trump" = "#C43C39",
      "Harris" = "#3568A8"
    ),
    name = "Winner"
  ) +
  labs(
    title = "2024 Presidential Election Winner by State",
    subtitle = "United States presidential election"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 15, face = "bold"
    ),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

map1

# Map2




# Map 3
map3 <- ggplot(counties_sf) +
  geom_sf(aes(fill = pop_dens), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "sqrt",
    name = "Population density"
  ) +
  labs(title = "Population Density by County") +
  theme_void()
map3

# Map 4
counties_sf <- counties_sf %>%
  mutate(black_pct = (black / pop) * 100)

map4 <- ggplot(counties_sf) +
  geom_sf(aes(fill = black_pct), color = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name = "% Black"
  ) +
  labs(title = "Percentage Black by County") +
  theme_void()
map4



