#' check_linearpackcircles
#'
#' @param DF input dataframe
#' @param ID_var ID variable
#' @param group_var group variable
#' @param area_var area variable
#' @param x_var x axis variable
#' @param separation_factor how much separation between groups
#' @param ratio_reduction_area reduce area for plotting by this ratio
#' @param ratio_reduction_x reduce x for plotting by this ratio
#' @param height_y height of y axis for each group
#' @param max_overlaps overlaps in geom_text_repel
#' @param title_str title plot
#' @param subtitle_str subtitle plot
#' @param x_str x axis label plot
#' @param caption_str caption plot
#' @param size_text size text labels
#' @param label_circles Should we draw labels for the circles
#' @param save_plot Should we save the plot
#' @param CHECKS Should we check
#' @param CHECKS_plots Should we show plots
#' @param ... other arguments
#'
#' @return
#' @export
#'
#' @examples
check_linearpackcircles <- function(DF,
                              ID_var = "ID",
                              group_var = "group",
                              area_var = "area",
                              x_var = "x",
                              separation_factor = 1, # Separation between group_var levels
                              ratio_reduction_area = 600,
                              ratio_reduction_x = 50,
                              height_y = 1,
                              max_overlaps = 8,
                              title_str = "Packing circles visualization",
                              subtitle_str = NULL,
                              x_str = x_var,
                              caption_str = "",
                              size_text = 3,
                              label_circles = TRUE,
                              save_plot = FALSE,
                              CHECKS = TRUE,
                              CHECKS_plots = FALSE,
                              ...) {


  # Libraries ---------------------------------------------------------------

  # library(dplyr)
  # library(ggplot2)
  # library(packcircles)
  # library(purrr)
  # library(readr)
  # library(tidyr)

  # invisible(lapply(list.files("./R", full.names = TRUE, pattern = ".R"), source))



  # Data preparation --------------------------------------------------------

  set.seed(12)

  ALL_data = prepare_data(DF,
                          ID_var = ID_var,
                          group_var = group_var,
                          area_var = area_var,
                          x_var = x_var,
                          ratio_reduction_area = ratio_reduction_area,
                          ratio_reduction_x = ratio_reduction_x,
                          height_y = height_y)



  # Create polygons ---------------------------------------------------------

  # We create polygons for each level of group_var
  DF_groups = ALL_data %>% distinct(group_var = get(group_var))# %>% head(1)

  DF_polygons = 1:nrow(DF_groups) %>%
    map_df(~ create_polygons(ALL_data %>% filter(get(group_var) == DF_groups$group_var[.x]), group_var = group_var))


  # CHECKS -------------------------------------------------------------------

  if (CHECKS) {

    # Overlaps
    list_overlaps = 1:nrow(DF_groups) %>% map(~ check_overlaps(DF_polygons %>% filter(get(group_var) == DF_groups$group_var[.x]), CHECKS_plots = CHECKS_plots))

    # Extract DF and plots
    DF_overlaps = 1:length(list_overlaps) %>% map_df(~list_overlaps[[.x]]$DF_overlaps)
    plots_overlaps = 1:length(list_overlaps) %>% map(~list_overlaps[[.x]]$plot_overlaps)


    # Differences in initial location and plot locacion
    DF_DIFFS = 1:nrow(DF_groups) %>%
      map(~ check_diffs(ALL_data, DF_polygons %>% filter(get(group_var) == DF_groups$group_var[.x]), check_var = x_var, group_var = group_var, ID_var = ID_var, ratio_reduction_x = ratio_reduction_x))

  }

  output_list = list(DF_overlaps  = DF_overlaps,
                     plots_overlaps = plots_overlaps,
                     DF_DIFFS = DF_DIFFS)

  return(output_list)
}
