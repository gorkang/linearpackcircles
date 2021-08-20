# Libraries and functions
library(readr)
library(dplyr)
invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source))

# Data
DF = read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv") %>% 
  filter(date == max(date)) # Keep only most recent data

# Plot
linearpackcircles(DF, 
                  
                  # Main variables
                  ID_var_str = "location",
                  group_var_str = "continent",
                  area_var_str = "total_cases_per_million",
                  x_var_str = "total_deaths_per_million",
                  
                  # Layout parameters
                  separation_factor = 15,
                  ratio_reduction_area = 60000,
                  ratio_reduction_x = 50,
                  height_y = 5,
                  
                  # Text labels
                  max_overlaps = 8,
                  
                  # Plot parameters
                  title = "COVID deaths per million",
                  x = "Deaths per million",
                  caption = "Diameter is cases per million \n 
                             Data from https://github.com/owid/covid-19-data \n
                             By @gorkang",
                  size_text = 3)


# Check overlaps and position of circles
DF_CHECKS =
  check_linearpackcircles(DF, 
                  
                          # Main variables
                          ID_var_str = "location",
                          group_var_str = "continent",
                          area_var_str = "total_cases_per_million",
                          x_var_str = "total_deaths_per_million",
                          
                          # Layout parameters
                          separation_factor = 15,
                          ratio_reduction_area = 60000,
                          ratio_reduction_x = 50,
                          height_y = 5,
                          
                          # Text labels
                          max_overlaps = 8,
                          
                          # Plot parameters
                          title = "COVID deaths per million",
                          x = "Deaths per million",
                          caption = "Diameter is cases per million \n 
                                     Data from https://github.com/owid/covid-19-data \n
                                     By @gorkang",
                          size_text = 3)
