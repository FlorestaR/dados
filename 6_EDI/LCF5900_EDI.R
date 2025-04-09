# ---------------------------------------------------------------------
# LCF5900_EDI.R
# Author:  Luiz Carlos Estraviz Rodriguez
# Updated: 09/Abr/2025 (backup)
# https://posit.cloud/content/10144185 (private, Google access enabled)
# ---------------------------------------------------------------------
rm(list=ls(all=TRUE))                                  # Memory cleanup
gc()

# Define working directory
setwd("/cloud/project/LCF5900")

# Load packages
if(!require(tidyverse))
  install.packages("tidyverse")
library(tidyverse)

if(!require(rio))
  install.packages("rio")
library(rio); install_formats()

# Define github URL where climate data from Piracicaba is stored
# OBS: copy the full github URL address and replace "tree" with "blob")
url_1 <- "https://github.com/FlorestaR/dados/blob/main/6_EDI/"
xls_2 <- "CrownGeometryHarvardForest_v2013.xlsx"
prm_3 <- "?raw=true"
gitFile <- paste0(url_1, xls_2,prm_3)

# Imports the Excel spreadsheet from github using the rio package,
# making sure the first 8 columns become "factors" and the rest of
# the columns remain numeric. Then converts the downloaded data
# into a tibble (dataframe)
sheetName <- "CrownGeometryHarvardForest_v201"
my_col_types <- c("date", rep("text", 2), rep("numeric", 16))
df <- import(gitFile, which = sheetName, col_types = my_col_types)
df <- df %>% mutate(across(2:3, factor)) %>% tibble()
# Show column names and structure of the data.daframe
colnames(df)
str(df)
