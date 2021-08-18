# Crea una visualizacion de packing circles para datos covid. 

  # TODO: generalizar para cualquier tipo de datos!

# Libraries ---------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(patchwork)
library(packcircles)
library(ggplot2)

lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source)


# Parameters --------------------------------------------------------------

set.seed(12)

ratio_reduction_x = 10000
height_y = 2
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
  select(x, y, area, location, continent, total_cases_per_million)  


# Create polygons ---------------------------------------------------------

DF1 = create_polygons(ALL_data %>% filter(continent == "Asia"))
DF2 = create_polygons(ALL_data %>% filter(continent == "Europe"))

# Position of text labels
label_positions1 = DF1 %>% group_by(id)  %>% filter(y == max(y)) %>% filter(x == median(x))%>% sample_n(1)
label_positions2 = DF2 %>% group_by(id)  %>% filter(y == max(y)) %>% filter(x == median(x))%>% sample_n(1)



# CHECKS -------------------------------------------------------------------

check_diffs(ALL_data, DF1)
check_diffs(ALL_data, DF2)


# PLOT --------------------------------------------------------------------

plot1 = create_plot(DF1, label_positions1) + labs(title = "COVID cases per million by continent")
plot2 = create_plot(DF2, label_positions2) + labs(x = "Cases per million", caption = "Diameter is deaths per million \n Data from https://github.com/owid/covid-19-data")


# https://stackoverflow.com/questions/60172472/plotting-ggplot2-geom-polygon-on-discrete-x-axis
# See option 2: https://stackoverflow.com/a/60173018/1873521 

final_plot = plot1 / plot2
final_plot

ggsave("outputs/final_plot.png", final_plot, width = 20, height = 10, dpi = 300)
