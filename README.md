
# linearpackcircles

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of linearpackcircles is to create circle packing visualizations along a linear scale. It uses {[packcircles](https://github.com/mbedward/packcircles)} as a backend.  

You can install the development version with `remotes::install_github("gorkang/linearpackcircles")`. Please report any problems you find in the [Issues Github page](https://github.com/gorkang/linearpackcircles/issues).  


## Example plot

With the [OWID dataset](https://github.com/owid/covid-19-data/tree/master/public/data) we can create a plot showing how `total_deaths_per_million` changes by `continent` and `location` (i.e. country), using `total_cases_per_million` for the circle diameter. 


```r 

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

```

---  


![](man/figures/final_plot.png)
