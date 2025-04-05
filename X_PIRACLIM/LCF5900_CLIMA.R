# ---------------------------------------------------------------------
# LCF5900_CLIMA.R
# Author:  Luiz Carlos Estraviz Rodriguez
# Updated: 04/Abr/2025
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

if(!require(gganimate))
  install.packages("gganimate")
library(gganimate)

if(!require(gifski))
  install.packages("gifski")
library(gifski)

if(!require(av))
  install.packages("av")
library(av)

# Define github URL where climate data from Piracicaba is stored
# OBS: copy the full github URL address and replace "tree" with "blob")
url_1 <- "https://github.com/FlorestaR/dados/blob/main/X_PIRACLIM/"
xls_2 <- "DadosClima_Piracicaba.xlsx"
prm_3 <- "?raw=true"
gitFile <- paste0(url_1, xls_2,prm_3)

# Imports the Excel spreadsheet from github using the rio package,
# making sure the first 8 columns become "factors" and the rest of
# the columns remain numeric. Then converts the downloaded data
# into a tibble (dataframe)
sheetName <- "DadosClima_Piracicaba"
my_col_types <- c(rep("text", 8), rep("numeric", 16))
df <- import(gitFile, which = sheetName, col_types = my_col_types)
df <- df %>% mutate(across(1:8, factor)) %>% tibble()
# Show column names and structure of the data.daframe
colnames(df)
str(df)

# Creation of a simple histogram for a subgroup of years
t_max <- df %>%
  filter(Ano %in% c(2022, 2023, 2024)) %>%
  pull(TMAX)
hist (t_max, 
      main = "Temperaturas 2022-2024 - Piracicaba-SP", 
      xlab = "Temperaturas", ylab = "Freq.", 
      col = "grey",
      border = "black",
      freq =F,
      breaks = c(0,5,10,15,20,25,30,35,40,45), 
      right = T, 
      labels = F)


# Creation of a new dataframe called new_df without NAs
# and filtered by TMED<50
new_df <- df %>%
  select(Ano, Mes, TMED, TMIN, TMAX, Chuva) %>%
  drop_na() %>%
  filter(TMED <50)
str(new_df)

# Summarize a few statistics for new_df
new_df %>% summarise(m_TMED     = mean(TMED),
                     m_TMIN     = mean(TMIN),
                     m_TMAX     = mean(TMAX),
                     m_Chuva    = mean(Chuva))

# Calculate the average TMED per month per year
medMes <- df %>%
  group_by(Ano, Mes) %>%
  summarise(tmedMes = mean(TMED, na.rm = TRUE), .groups = "drop")

# Create one graph per year with monthly average TMAX 
p <- ggplot(medMes, aes(x = Mes, y = tmedMes)) +
  geom_point() +
  labs(title = "Ano: {frame_time}") +
  transition_time(as.numeric(as.character(Ano))) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()

# Create an animated GIF that shows a sequence of annual averages
animate(p, width = 750, height = 450)
anim_save("grafGIF.gif", animation = p)

# Create an MP4 movie that shows the sequence of annual averages
animate(p, renderer = ffmpeg_renderer(), width = 800, height = 450)
anim_save("animGraf.mp4")