check_linearpackcircles <- function(DF,
                              ID_var_str = "ID",
                              group_var_str = "group",
                              area_var_str = "area",
                              x_var_str = "x",
                              separation_factor = 1, # Separation between group_var_str levels
                              ratio_reduction_area = 600,
                              ratio_reduction_x = 50,
                              height_y = 1,
                              max_overlaps = 8,
                              title_str = "Packing circles visualization",
                              subtitle_str = NULL,
                              x_str = x_var_str,
                              caption_str = "",
                              size_text = 3,
                              CHECKS = TRUE,
                              CHECKS_plots = FALSE,
                              label_circles = TRUE,
                              save_plot = FALSE,
                              ...) {

  
  # Libraries ---------------------------------------------------------------
  
  library(dplyr)
  library(ggplot2)
  library(packcircles)
  library(purrr)
  library(readr)
  library(tidyr)
  
  invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source))
  
  

  # Data preparation --------------------------------------------------------
  
  set.seed(12)
  
  ALL_data = prepare_data(DF,
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
  
  
  # CHECKS -------------------------------------------------------------------
    
  if (CHECKS) {
    
    # Overlaps
    list_overlaps = 1:nrow(DF_groups) %>% map(~ check_overlaps(DF_polygons %>% filter(get(group_var_str) == DF_groups$group_var[.x]), CHECKS_plots = CHECKS_plots))
    
    # Extract DF and plots         
    DF_overlaps = 1:length(list_overlaps) %>% map_df(~list_overlaps[[.x]]$DF_overlaps)
    plots_overlaps = 1:length(list_overlaps) %>% map(~list_overlaps[[.x]]$plot_overlaps)
    
    
    # Differences in initial location and plot locacion
    DF_DIFFS = 1:nrow(DF_groups) %>% 
      map(~ check_diffs(ALL_data, DF_polygons %>% filter(get(group_var_str) == DF_groups$group_var[.x]), check_var = x_var_str, group_var = group_var_str, ID_var_str = ID_var_str, ratio_reduction_x = ratio_reduction_x))
    
  }
  
  output_list = list(DF_overlaps  = DF_overlaps,
                     plots_overlaps = plots_overlaps,
                     DF_DIFFS = DF_DIFFS)
  
  return(output_list)
}