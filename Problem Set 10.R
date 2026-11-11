#Title: Problem Set 10
#Author: Kerri Rose Riley
#Purpose: To draw a series of maps for the 2024 election using the Healy guidelines
#Output: Figures and Quarto Document typeset as a .pdf

# Import Packages
library(tidyverse)

# Set the working directory
setwd("~/Desktop/Current Classes/R/Problem Set 10")

# Load in the data sets
election <- read_csv("~/Desktop/Current Classes/R/Problem Set 10/Data/midterms_2026_preliminary.csv")