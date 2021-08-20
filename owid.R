# Creates a packing circles visualization for COVID OWID data

# TODO: homogenize parameters of functions (e.g. group_var, group_var_str). Eliminate all "_str"?
# TODO: automatic main parameters? Ar least sane defaults?


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

separation_factor = 15 # Separation between group_var_str levels
ratio_reduction_area = 60000
ratio_reduction_x = 50
height_y = 5 # How much space for each group in the y axis to move around the circles

max_overlaps = 8 # If using labels, how many max overlaps to tolerate

# Main variables names
ID_var_str = "location"
group_var_str = "continent"
area_var_str = "total_cases_per_million"
x_var_str = "total_deaths_per_million"

# Plot parameters
title_str = "COVID deaths per million"
subtitle_str = NULL
x_str = "Deaths per million"
caption_str = "Diameter is cases per million \n Data from https://github.com/owid/covid-19-data \nBy @gorkang"
size_text = 3



# Data preparation --------------------------------------------------------

DF = read_csv("data/owid-covid-data.csv", show_col_types = FALSE)

ALL_data = prepare_data(DF %>% filter(date == max(date)), # Use only more recent data
             ID_var = ID_var_str,
             group_var = group_var_str,
             area_var = area_var_str,
             x_var = x_var_str,
             ratio_reduction_area = ratio_reduction_area, 
             ratio_reduction_x = ratio_reduction_x, 
             height_y = height_y)



# Create polygons ---------------------------------------------------------

# We create polygons for each level of group_var_str
DF_groups = ALL_data %>% distinct(group_var = get(group_var_str))# %>% head(1)

DF_polygons = 1:nrow(DF_groups) %>% 
  map_df(~ create_polygons(ALL_data %>% filter(get(group_var_str) == DF_groups$group_var[.x]), group_var = group_var_str))



# Plot --------------------------------------------------------------------

plot_final = create_plot(DF_polygons, label_circles = TRUE, max_overlaps = max_overlaps, ID_var_str = ID_var_str, group_var_str = group_var_str, separation_factor = separation_factor, ratio_reduction_x = ratio_reduction_x)

final_plot = plot_final +
  labs(title = title_str,
       subtitle = subtitle_str,
       x = x_str, 
       caption = caption_str)

final_plot

ggsave("outputs/final_plot.png", final_plot, width = 14, height = 10, dpi = 300)



# CHECKS -------------------------------------------------------------------

# Overlaps
list_overlaps = 1:nrow(DF_groups) %>% map(~ check_overlaps(DF_polygons%>% filter(get(group_var_str) == DF_groups$group_var[.x]), CHECKS_plots = TRUE))
  
  # Extract DF and plots         
  DF_overlaps = 1:length(list_overlaps) %>% map_df(~list_overlaps[[.x]]$DF_overlaps)
  plots_overlaps = 1:length(list_overlaps) %>% map(~list_overlaps[[.x]]$plot_overlaps)


# Differences in initial location and plot locacion
  DF_DIFFS = 1:nrow(DF_groups) %>% 
    map(~ check_diffs(ALL_data, DF_polygons %>% filter(get(group_var_str) == DF_groups$group_var[.x]), check_var = x_var_str, group_var = group_var_str, ID_var_str = ID_var_str, ratio_reduction_x = ratio_reduction_x))
  