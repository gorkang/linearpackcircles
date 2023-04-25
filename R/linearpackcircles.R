#' linearpackcircles
#'
#' @param DF input dataframe
#' @param ID_var ID variable
#' @param group_var group variable
#' @param area_var area variable
#' @param x_var x axis variable
#' @param separation_factor how much separation between groups
#' @param width_plot reduce x for plotting by this ratio
#' @param height_group height of y axis for each group
#' @param label_circles Should we draw labels for the circles
#' @param max_overlaps overlaps in geom_text_repel
#' @param size_text size text labels
#' @param highlight_ID Which ID's to highlight
#' @param random_seed random seed to use
#' @param area_multiplier multiply area size by this
#'
#' @return ggplot object
#' @export
#' @importFrom dplyr all_of distinct filter mutate pull select
#' @importFrom ggplot2 labs ggsave
#' @importFrom purrr map map_df
#' @importFrom magrittr %>%
#' @importFrom crayon red
#' @importFrom rlang set_names
#'
#' @examples
linearpackcircles <- function(DF,

                              ID_var = "ID",
                              group_var = "group",
                              area_var = "area",
                              x_var = "x",

                              separation_factor = 1, # Separation between group_var levels
                              width_plot = 100,
                              height_group = 10,

                              label_circles = TRUE,
                              max_overlaps = 8,
                              size_text = 3,

                              highlight_ID = NULL,

                              random_seed = 12,

                              area_multiplier = 1) {


  # CHECK
  # ID_var = "office_character"
  # group_var = "award_type"
  # area_var = "total_awards"
  # x_var = "year"

  # ID_var = "researcher"
  # group_var = "affiliation"
  # area_var = "horas"
  # x_var = "publicaciones"
  # separation_factor = 1
  # width_plot = 10
  # height_group = 1
  # random_seed = 12
  # highlight_ID = NULL
  # label_circles = FALSE

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



  # Plot --------------------------------------------------------------------
  # Automatic parameters

  final_plot = create_plot(DF_prepared = ALL_data, DF = DF_polygons,

                           ID_var = ID_var,
                           group_var = group_var,
                           area_var = area_var,
                           x_var = x_var,

                           separation_factor = separation_factor,
                           width_plot = width_plot,

                           label_circles = label_circles,
                           max_overlaps = max_overlaps,
                           size_text = size_text,

                           highlight_ID = highlight_ID)


  final_plot

}
