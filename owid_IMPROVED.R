# Creates a packing circles visualization for COVID OWID data


# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(packcircles)
library(purrr)
library(readr)
library(tidyr)

invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source))


# Parameters --------------------------------------------------------------

set.seed(12)
separation_factor = 10 # Separation between group_var_str levels

ratio_reduction_area = 60000
ratio_reduction_x = 50

height_y = 5

# If using labels, how many max overlaps to tolerate
max_overlaps = 8


ID_var_str = "location"
group_var_str = "continent"
area_var_str = "total_cases_per_million"
x_var_str = "total_deaths_per_million"


title_str = "COVID deaths per million"
subtitle_str = NULL
x_str = "Deaths per million"
caption_str = "Diameter is cases per million \n Data from https://github.com/owid/covid-19-data \nBy @gorkang"
size_text = 3


# ratio_reduction_area = 10000
# ratio_reduction_x = 10000
# height_y = 1
# title_str = "COVID cases per million by continent"
# x_str = "Cases per million"
# caption_str = "Diameter is deaths per million \n Data from https://github.com/owid/covid-19-data"





# Data preparation --------------------------------------------------------

DF = read_csv("data/owid-covid-data.csv", show_col_types = FALSE)

ALL_data = prepare_data(DF %>% filter(date == max(date)),
             ID_var = ID_var_str,
             group_var = group_var_str,
             area_var = area_var_str,
             x_var = x_var_str)



# Create polygons ---------------------------------------------------------

DF_CONTINENTS = ALL_data %>% distinct(continent)# %>% head(1)

DF1 = 
  1:nrow(DF_CONTINENTS) %>% 
  map_df(~ create_polygons(ALL_data %>% filter(continent == DF_CONTINENTS$continent[.x]), group_var = "continent"))



# Separate by factor ------------------------------------------------------

# 
DF_factors = DF1 %>% distinct(continent) %>% mutate(ID = (1:n()) * separation_factor)

DFX = DF1 %>% left_join(DF_factors, by = group_var_str) %>% mutate(y = y + ID)



# Plot --------------------------------------------------------------------

plot_final = create_plot(DFX, label_circles = TRUE, max_overlaps = max_overlaps)

final_plot = plot_final +
  labs(title = title_str,
       subtitle = subtitle_str,
       x = x_str, 
       caption = caption_str)

final_plot

ggsave("outputs/final_plot_improved.png", final_plot, width = 20, height = 11, dpi = 300)


# CHECKS -------------------------------------------------------------------

# check_diffs(ALL_data, DF1 %>% filter(continent == DF_CONTINENTS$continent[4]), check_var = x_var_str)
