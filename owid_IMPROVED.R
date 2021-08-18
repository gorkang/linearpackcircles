# Crea una visualizacion de packing circles para datos covid. 

# TODO: generalizar para cualquier tipo de datos! 

  # Ahora, por ejemplo, si se usa casos por millon para el area, se va al chancho
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

ratio_reduction_x = 10000
height_y = 1

# ratio_reduction_x = 0.5
# height_y = 1000

size_text = 3


# Data preparation --------------------------------------------------------

DF = read_csv("data/owid-covid-data.csv")

ALL_data = 
  DF %>% 
  filter(date == max(date)) %>% 
  select(location, continent, total_cases_per_million, total_deaths_per_million) %>% 
  drop_na(continent) %>% 
  drop_na() %>% 
  mutate(area = (total_deaths_per_million)/ratio_reduction_x,
         x = (total_cases_per_million)/ratio_reduction_x,
         y = runif(n(), 0, height_y)) %>%
  
  # mutate(area = (total_cases_per_million)/ratio_reduction_x,
  #        x = (total_deaths_per_million)/ratio_reduction_x,
  #        y = runif(n(), 0, height_y)) %>% 
  select(x, y, area, location, continent, total_cases_per_million)  


# Create polygons ---------------------------------------------------------

DF_CONTINENTS = ALL_data %>% distinct(continent) 

DF1 = 1:nrow(DF_CONTINENTS) %>% 
  map_df(~ create_polygons(ALL_data %>% filter(continent == DF_CONTINENTS$continent[.x])))


# Separate by factor ------------------------------------------------------

DF_factors = DF1 %>% distinct(continent) %>% mutate(ID = (1:n()) * 2)

DFX = DF1 %>% left_join(DF_factors, by = "continent") %>% mutate(y = y + ID)

plot_final = create_plot(DFX, label_circles = TRUE)

final_plot = plot_final +
  labs(title = "COVID cases per million by continent",
       x = "Cases per million", caption = "Diameter is deaths per million \n Data from https://github.com/owid/covid-19-data")

ggsave("outputs/final_plot_improved.png", final_plot, width = 20, height = 10, dpi = 300)
