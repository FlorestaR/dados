# ---------------------------------------------------------------------
# LCF5900_CLIMA.R
# Author:  Luiz Carlos Estraviz Rodriguez
# Updated: 31/Mai/2026
# ---------------------------------------------------------------------
rm(list=ls(all=TRUE))                                  # Memory cleanup
gc()

# Define working directory
setwd("/cloud/project/LCF5900")

# Load packages
suppressPackageStartupMessages({
  library(tidyverse)
  library(rio)
  library(gganimate)
  library(gifski)
  library(av)
})

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

# Extract values of interest into a vector
t_max <- df %>%
  filter(Ano %in% c(2022, 2023, 2024, 2025)) %>%
  pull(TMAX)

# Compute breaks and counts manually
breaks      <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45)
counts      <- hist(t_max, breaks = breaks, plot = FALSE)$counts
percentages <- counts / sum(counts) * 100            # relative frequencies in %

# Plot using the pre-computed percentages
bp <- barplot(percentages,
              # Create the class names
              names.arg = paste0(breaks[-length(breaks)], "–", breaks[-1]),
              main  = "Temperaturas Mensais Máximas 2022-2025 - Piracicaba-SP",
              xlab  = "Temperatura (°C)",
              ylab  = "Frequência relativa (%)",
              col   = "grey",
              border = "black",
              las   = 1)                      # rotate x labels for readability

# Create a new dataframe, turn TMAX, TMIN and TMED into factors,
# delete NAs and filter  by values < 50
new_df <- df %>%
  select(Ano, Mes, TMED, TMIN, TMAX, Chuva) %>%
  drop_na() %>%
  filter(if_all(c(TMED, TMIN, TMAX), ~ . < 50))
str(new_df)

# Summarize a few statistics for new_df
new_df %>% summarise(m_TMED     = mean(TMED),
                     m_TMIN     = mean(TMIN),
                     m_TMAX     = mean(TMAX),
                     m_Chuva    = mean(Chuva))

# Calculate mean TMED per month per year
medMes <- df %>%
  group_by(Ano, Mes) %>%
  summarise(tmedMes = mean(TMED, na.rm = TRUE), .groups = "drop") %>%
  mutate(Mes = factor(Mes, levels = 1:12,
                      labels = c("Jan","Fev","Mar","Abr","Mai","Jun",
                                 "Jul","Ago","Set","Out","Nov","Dez")))

# Builds a sequence of plots per year
# -------------------------------------/
p <- ggplot(medMes, aes(x = Mes, y = tmedMes, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "Temperatura Média Mensal — Ano: {closest_state}",
    x     = "Mês",
    y     = "Temperatura Média (°C)"
  ) +
  # Longer pause per year
  transition_states(Ano, transition_length = 1, state_length = 3) +
  ease_aes("sine-in-out") +
  enter_fade() +
  exit_fade()

# Render and save an animated GIF plot
# -------------------------------------/
gif <- animate(p, width = 750, height = 450, fps = 3, 
               renderer = gifski_renderer())
anim_save("grafGIF.gif", animation = gif)
browseURL("grafGIF.gif")                                   # Plays GIF in Viewer

# Render and save an animated MP4 movie
# -------------------------------------/
mp4 <- animate(p, width = 800, height = 450, fps = 3,
               renderer = ffmpeg_renderer(), )
anim_save("grafMP4.mp4", animation = mp4)
browseURL("grafMP4.mp4")                                   # Plays MP4 in Viewer