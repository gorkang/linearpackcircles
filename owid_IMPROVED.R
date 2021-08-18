# Crea una visualizacion de packing circles para datos covid. 

# TODO: La posicion de algunas labels no esta del todo OK.

# TODO: La label que se muestra es x * ratio_reduction_x. Es la posicion en la que acaba el circulo, no el dato inicial. En ocasiones habra alguna discrepancia.

# TODO: generalizar para cualquier tipo de datos! 

  # Ahora, por ejemplo, si se usa casos por millon para el area, se va al chancho
    # Habra que normalizar el area tb?

  # mutate(area = (total_cases_per_million)/ratio_reduction_x,
  #        x = (total_deaths_per_million)/ratio_reduction_x,
  #        y = runif(n(), 0, height_y)) %>% 
    

# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(packcircles)
library(purrr)
library(readr)
library(tidyr)

lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)


# Parameters --------------------------------------------------------------

set.seed(12)
separation_factor = 3

ratio_reduction_area = 60000
ratio_reduction_x = 50
height_y = 1
title_str = "COVID cases per million by continent"
x_str = "Deaths per million"
caption_str = "Diameter is cases per million \n Data from https://github.com/owid/covid-19-data"

# ratio_reduction_area = 10000
# ratio_reduction_x = 10000
# height_y = 1
# title_str = "COVID cases per million by continent"
# x_str = "Cases per million"
# caption_str = "Diameter is deaths per million \n Data from https://github.com/owid/covid-19-data"


size_text = 3


# Data preparation --------------------------------------------------------

DF = read_csv("data/owid-covid-data.csv")

ALL_data = 
  DF %>% 
  filter(date == max(date)) %>% 
  select(location, continent, total_cases_per_million, total_deaths_per_million) %>% 
  drop_na(continent) %>% 
  drop_na() %>% 
  
  # ESTO DEBERIA ESTAR EN UNA FUNCION -------------
  # mutate(area = (total_deaths_per_million)/ratio_reduction_area,
  #        x = (total_cases_per_million)/ratio_reduction_x,
  #        y = runif(n(), 0, height_y)) %>%
  
  mutate(area = (total_cases_per_million)/ratio_reduction_area,
         x = (total_deaths_per_million)/ratio_reduction_x,
         y = runif(n(), 0, height_y)) %>%
  select(x, y, area, location, continent, total_cases_per_million)  


# Create polygons ---------------------------------------------------------

DF_CONTINENTS = ALL_data %>% distinct(continent) 

DF1 = 1:nrow(DF_CONTINENTS) %>% 
  map_df(~ create_polygons(ALL_data %>% filter(continent == DF_CONTINENTS$continent[.x])))


# CHECKS -------------------------------------------------------------------

check_diffs(ALL_data, DF1 %>% filter(continent == "Asia"))


# Separate by factor ------------------------------------------------------

DF_factors = DF1 %>% distinct(continent) %>% mutate(ID = (1:n()) * separation_factor)

DFX = DF1 %>% left_join(DF_factors, by = "continent") %>% mutate(y = y + ID)

plot_final = create_plot(DFX, label_circles = TRUE)

final_plot = plot_final +
  labs(title = title_str,
       x = x_str, 
       caption = caption_str)

final_plot

ggsave("outputs/final_plot_improved.png", final_plot, width = 20, height = 10, dpi = 300)
