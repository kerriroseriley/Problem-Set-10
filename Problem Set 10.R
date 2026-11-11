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

# Load county data 
data("election24_county_df")

# Load opiates data
data("opiates")


elec <- read_csv("~/Desktop/Current Classes/R/Problem Set 10/Data/midterms_2026_preliminary.csv")


# Examine your datasets
View(election24)
View(states_sf)
View(counties_sf)
View(election24_county_df)
View(opiates)


# See all variable names
names(election24)
names(states_sf)
names(counties_sf)
names(opiates)


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

# Map 2
map2 <- ggplot(county_map) +
  geom_sf(
    aes(fill = winner),
    color = "white",
    linewidth = 0.05
  ) +
  scale_fill_manual(
    values = c(
      "Republican" = "#C43C39",
      "Democratic" = "#3568A8"
    ),
    name = "Winner"
  ) +
  labs(
    title = "2024 Presidential Election Winner by County",
    subtitle = "County-level results across the United States"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    plot.subtitle = element_text(size = 11),
    legend.position = "bottom"
  )
map2


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


# Map 5: 2026 Senate Winner

# Filter for top vote-getter per state for Senate races
senate_2026 <- elec %>%
  filter(str_detect(office, "Senate")) %>%
  filter(!is.na(party)) %>%
  group_by(state_fips) %>%
  slice_max(order_by = vote_pct, n = 1, with_ties = FALSE) %>%
  select(fips = state_fips, senate_winner = party) %>%
  mutate(fips = sprintf("%02d", as.numeric(fips))) # Formats FIPS with leading zeros to match states_sf

# Filter for top vote-getter per state for Governor races
gov_2026 <- elec %>%
  filter(office == "Governor") %>%
  filter(!is.na(party)) %>%
  group_by(state_fips) %>%
  slice_max(order_by = vote_pct, n = 1, with_ties = FALSE) %>%
  select(fips = state_fips, gov_winner = party) %>%
  mutate(fips = sprintf("%02d", as.numeric(fips)))

# Join 2026 election results to states_sf geometry
states_map_2026 <- states_sf %>%
  left_join(senate_2026, by = "fips") %>%
  left_join(gov_2026, by = "fips")


map5 <- ggplot(states_map_2026) +
  geom_sf(aes(fill = senate_winner),
          color = "white",
          linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "R" = "#C43C39",
      "D" = "#3568A8"
    ),
    na.value = "gray90", # For states without a 2026 Senate election
    name = "Winner"
  ) +
  labs(
    title = "2026 Senate Election Winner by State",
    subtitle = "United States Senate midterm elections"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

map5

# Map 6: 2026 Governor Winner
map6 <- ggplot(states_map_2026) +
  geom_sf(aes(fill = gov_winner),
          color = "white",
          linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "R" = "#C43C39",
      "D" = "#3568A8"
    ),
    na.value = "gray90", # For states without a 2026 Governor election
    name = "Winner"
  ) +
  labs(
    title = "2026 Governor Election Winner by State",
    subtitle = "United States Gubernatorial midterm elections"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

map6



# Recoding
# 1. 2024 Presidential Margin by state (Harris % minus Trump %)
pres_2024_margin <- election24 %>%
  mutate(
    pres_margin_2024 = pct_harris - pct_trump,
    fips = sprintf("%02d", as.numeric(fips)) # Formats FIPS to 2-digit character string
  ) %>%
  select(fips, pres_margin_2024)

# 2. 2026 Senate Margin by state (D % minus R %)
senate_2026_margin <- elec %>%
  filter(str_detect(office, "Senate")) %>%
  filter(party %in% c("D", "R")) %>%
  group_by(state_fips, party) %>%
  summarize(vote_pct = max(vote_pct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = party, values_from = vote_pct, values_fill = 0) %>%
  mutate(
    senate_margin_2026 = D - R,
    fips = sprintf("%02d", as.numeric(state_fips))
  ) %>%
  select(fips, senate_margin_2026)

# 3. 2026 Governor Margin by state (D % minus R %)
gov_2026_margin <- elec %>%
  filter(office == "Governor") %>%
  filter(party %in% c("D", "R")) %>%
  group_by(state_fips, party) %>%
  summarize(vote_pct = max(vote_pct, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = party, values_from = vote_pct, values_fill = 0) %>%
  mutate(
    gov_margin_2026 = D - R,
    fips = sprintf("%02d", as.numeric(state_fips))
  ) %>%
  select(fips, gov_margin_2026)

# 4. Join to states_sf geometry and calculate Swing
swing_map_data <- states_sf %>%
  left_join(pres_2024_margin, by = "fips") %>%
  left_join(senate_2026_margin, by = "fips") %>%
  left_join(gov_2026_margin, by = "fips") %>%
  mutate(
    senate_swing = senate_margin_2026 - pres_margin_2024,
    gov_swing = gov_margin_2026 - pres_margin_2024
  )

# Map 7: 2026 Senate
map7 <- ggplot(swing_map_data) +
  geom_sf(aes(fill = senate_swing),
          color = "white",
          linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#C43C39",      # Shift toward Republican
    mid = "white",        # No shift
    high = "#3568A8",     # Shift toward Democrat
    midpoint = 0,
    na.value = "gray90",  # States without a 2026 Senate race
    name = "Margin Shift (%)"
  ) +
  labs(
    title = "2026 Senate Election Swing from 2024 Presidential Race",
    subtitle = "Positive values indicate a Democratic shift; negative values indicate a Republican shift"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

map7


# Map 8: 2026 Governor
map8 <- ggplot(swing_map_data) +
  geom_sf(aes(fill = gov_swing),
          color = "white",
          linewidth = 0.2) +
  scale_fill_gradient2(
    low = "#C43C39",      # Shift toward Republican
    mid = "white",        # No shift
    high = "#3568A8",     # Shift toward Democrat
    midpoint = 0,
    na.value = "gray90",  # States without a 2026 Governor race
    name = "Margin Shift (%)"
  ) +
  labs(
    title = "2026 Gubernatorial Election Swing from 2024 Presidential Race",
    subtitle = "Positive values indicate a Democratic shift; negative values indicate a Republican shift"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "bottom"
  )

map8


# Get centroid point of New York State directly from states_sf
ny_centroid <- states_sf %>%
  filter(fips == "36" | st == "NY" | state == "New York") %>%
  st_centroid()

# Draw Map 9
map9 <- ggplot() +
  geom_sf(data = states_sf, 
          fill = "gray95", 
          color = "white", 
          linewidth = 0.2) +
  geom_sf(data = ny_centroid, 
          color = "#D96B27", 
          shape = 8, 
          size = 6, 
          stroke = 1.5) +
  labs(
    title = "Map 9: Location of Syracuse University",
    subtitle = "National United States map indicating Syracuse, NY"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

map9

# https://data.cms.gov/provider-data/dataset/v9e4-nwhh
# Map 10: 
inpatient <- read_csv("~/Desktop/Current Classes/R/Problem Set 10/Data/inpatient.csv")



# Map 11: 
  
  





# Save the figures
ggsave("map1.png", plot = plot1, width = 8, height = 5, dpi = 300)
ggsave("map2.png", plot = plot2, width = 8, height = 5, dpi = 300)
ggsave("map3.png", plot = plot3, width = 8, height = 5, dpi = 300)
ggsave("map4.png", plot = plot4, width = 8, height = 5, dpi = 300)
ggsave("map5.png", plot = plot5, width = 8, height = 5, dpi = 300)
ggsave("map6.png", plot = plot5, width = 8, height = 5, dpi = 300)
ggsave("map7.png", plot = plot5, width = 8, height = 5, dpi = 300)
ggsave("map8.png", plot = plot5, width = 8, height = 5, dpi = 300)
ggsave("map9.png", plot = plot5, width = 8, height = 5, dpi = 300)
ggsave("map10.png", plot = plot5, width = 8, height = 5, dpi = 300)
ggsave("map11.png", plot = plot5, width = 8, height = 5, dpi = 300)
ggsave("map12.png", plot = plot5, width = 8, height = 5, dpi = 300)

