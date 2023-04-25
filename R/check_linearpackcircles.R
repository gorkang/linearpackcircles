#' check_linearpackcircles
#'
#' @param DF input dataframe
#' @param ID_var ID variable
#' @param group_var group variable
#' @param area_var area variable
#' @param x_var x axis variable
#' @param separation_factor how much separation between groups
#' @param width_plot reduce x for plotting by this ratio
#' @param height_group height of y axis for each group
#' @param CHECKS_plots TRUE/FALSE show plot with overlapping areas
#' @param random_seed random seed to use
#'
#' @return list with DF's and a ggplot object
#' @export
#' @importFrom dplyr all_of distinct filter mutate pull select
#' @importFrom ggplot2 labs ggsave
#' @importFrom purrr map map_df
#' @importFrom magrittr %>%
#' @importFrom crayon red
#' @importFrom rlang set_names
#'
#' @examples
check_linearpackcircles <- function(DF,

                              ID_var = "ID",
                              group_var = "group",
                              area_var = "area",
                              x_var = "x",

                              separation_factor = 1, # Separation between group_var levels
                              width_plot = 100,
                              height_group = 10,

                              CHECKS_plots = FALSE,

                              random_seed = 12,
                              area_multiplier = 1) {


  # Data preparation --------------------------------------------------------

  set.seed(random_seed)

  ALL_data = prepare_data(DF,
                          ID_var = ID_var,
                          group_var = group_var,
                          area_var = area_var,
                          x_var = x_var,
                          width_plot = width_plot,
                          height_group = height_group,
                          area_multiplier = area_multiplier)



  # Create polygons ---------------------------------------------------------

  # We create polygons for each level of group_var
  DF_groups = ALL_data %>% distinct(group_var = get(group_var))# %>% head(1)

  DF_polygons = 1:nrow(DF_groups) %>%
    map_df(~ create_polygons(ALL_data %>% filter(get(group_var) == DF_groups$group_var[.x]), group_var = group_var))




  # CHECKS -------------------------------------------------------------------

    # Overlaps
    list_overlaps = 1:nrow(DF_groups) %>% map(~ check_overlaps(DF_polygons %>% filter(get(group_var) == DF_groups$group_var[.x]), CHECKS_plots = CHECKS_plots))

    # Extract DF and plots
    DF_overlaps = 1:length(list_overlaps) %>% map_df(~list_overlaps[[.x]]$DF_overlaps)
    plots_overlaps = 1:length(list_overlaps) %>% map(~list_overlaps[[.x]]$plot_overlaps)


    # Differences in initial location and plot locacion
    DF_DIFFS = 1:nrow(DF_groups) %>%
      map(~ check_diffs(ALL_data, DF_polygons %>% filter(get(group_var) == DF_groups$group_var[.x]),
                        ID_var = ID_var,
                        group_var = group_var,
                        area_var = area_var,
                        x_var = x_var,

                        width_plot = width_plot))


  output_list = list(DF_overlaps  = DF_overlaps,
                     plots_overlaps = plots_overlaps,
                     DF_DIFFS = DF_DIFFS)

  output_list

}
