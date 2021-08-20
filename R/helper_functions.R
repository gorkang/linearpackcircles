#' prepare_data
#'
#' @param DF input dataframe
#' @param ID_var ID variable
#' @param group_var group variable
#' @param area_var area variable
#' @param x_var x axis variable
#' @param ratio_reduction_area reduce area for plotting by this ratio
#' @param ratio_reduction_x reduce x for plotting by this ratio
#' @param height_y height of y axis for each group
#'
#' @return
#' @importFrom ggplot2 ggplot aes element_text geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave
#' @importFrom dplyr all_of filter mutate pull select
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
#' @importFrom stats median runif
#'
#' @examples
prepare_data <- function(DF, ID_var, group_var, area_var, x_var, ratio_reduction_area, ratio_reduction_x, height_y) {

  # DEBUG
  # DF = DF
  # ID_var = "location"
  # group_var = "continent"
  # area_var = "total_cases_per_million"
  # x_var = "total_deaths_per_million"

  DF_prepared_data =
    DF %>%
    select(all_of(ID_var), all_of(group_var), all_of(area_var), all_of(x_var)) %>%
    drop_na() %>%
    mutate(area = get(area_var)/ratio_reduction_area,
           x = get(x_var)/ratio_reduction_x,
           y = runif(n(), 0, height_y)) %>%
    select(x, y, area, all_of(ID_var), all_of(group_var), all_of(area_var), all_of(x_var))

  return(DF_prepared_data)

}




#' create_polygons
#'
#' @param DF input dataframe
#' @param group_var group variable
#'
#' @return
#' @importFrom dplyr all_of filter left_join mutate pull select
#' @importFrom packcircles circleRepelLayout circleLayoutVertices
#' @importFrom magrittr %>%
#'
#' @examples
create_polygons <- function(DF, group_var) {

  # DEBUG
  # DF = ALL_data %>% filter(continent == DF_CONTINENTS$continent[1])

  # library(packcircles)

  limits_x = c(min(DF$x, na.rm = TRUE), max(DF$x, na.rm = TRUE))
  limits_y = c(min(DF$y, na.rm = TRUE), max(DF$y, na.rm = TRUE))

  res <- circleRepelLayout(DF, xlim = limits_x, ylim = limits_y, xysizecols = 1:3, wrap = FALSE)
  cat("- ", res$niter, "iterations performed\n")

  # Get vertex data for the initial layout where sizes are areas
  dat.gg.initial <- circleLayoutVertices(DF, sizetype = "area")

  # Get vertex data for the layout returned by the function where sizes are radii
  dat.gg.final <- circleLayoutVertices(res$layout)

  # Join original data with generated data
  names_DF = DF %>% select(-x, -y, -area) %>% names(.)

  DF_output = dat.gg.final %>% left_join(DF %>% mutate(id = 1:n()) %>% select(id, all_of(names_DF)), by = "id") %>%
    mutate(id = paste0(id, "_", get(group_var)))

  return(DF_output)

}



#' check_diffs
#'
#' @param ALL_data data1
#' @param DF data2
#' @param check_var x variable
#' @param ID_var ID variable
#' @param group_var group variable
#' @param ratio_reduction_x ratio_reduction_x
#'
#' @return
#' @importFrom dplyr arrange all_of distinct everything filter group_by left_join matches mutate n pull rename sample_n select starts_with summarise ungroup
#'
#' @examples
check_diffs <- function(ALL_data, DF, check_var, ID_var, group_var, ratio_reduction_x) {

  # check_diffs: no visible binding for global variable ‘x’
  # check_diffs: no visible binding for global variable ‘position_check’
  # check_diffs: no visible binding for global variable ‘x_check’
  # check_diffs: no visible binding for global variable ‘total_x_plot’
  # check_diffs: no visible binding for global variable ‘position_x’
  # check_diffs: no visible binding for global variable ‘DIFF’
  # check_diffs: no visible binding for global variable ‘DIFF_n’
  # check_diffs: no visible binding for global variable ‘DIFF_pct’
  # check_diffs: no visible binding for global variable ‘DIFF_abs’

  # DEBUG
  # ALL_data = ALL_data
  # DF = DF_polygons %>% filter(get(group_var) == DF_groups$group_var[4])
  # check_var = x_var
  # group_var = group_var

  # Extract group
  group_str = DF %>% distinct(!!group_var := get(group_var)) %>% pull(group_var)

  DFCHECK =
    DF %>% group_by(eval(ID_var)) %>% sample_n(1) %>% arrange(x) %>% ungroup() %>%
    mutate(position_check = 1:n(), total_x_plot = x * ratio_reduction_x) %>%
    rename(x_check = x) %>%
    select(position_check, x_check, eval(ID_var), total_x_plot)

  DF_output =
    ALL_data %>%

    # TODO: SHOULD make sure we are using circles in the same group / level when checking (?)
    filter(get(group_var) %in% group_str) %>%

    arrange(x) %>% ungroup() %>% mutate(position_x = 1:n()) %>% # arrange() needed to assign position_x
    select(x, eval(ID_var), eval(group_var), position_x, eval(check_var)) %>%
    left_join(DFCHECK, by = eval(ID_var)) %>%
    mutate(DIFF = position_x - position_check,
           DIFF_n = (total_x_plot - get(check_var)),
           DIFF_pct = ((total_x_plot - get(check_var))/get(check_var)) * 100,
           DIFF_abs = ((total_x_plot - get(check_var))/max(get(check_var))) * 100) %>%
    select(DIFF, matches("position_"), starts_with("x"), everything()) %>%
    arrange(DIFF)

  count_output =
    DF_output %>%
    group_by(DIFF) %>%
    summarise(N = n(),
              MEAN = mean(DIFF_n),
              MAX = max(DIFF_n),
              PCT = mean(DIFF_pct),
              PCT_abs = mean(DIFF_abs),
              MAX_PCT_abs = max(DIFF_abs),
              !!group_var := unique(get(group_var))
    )

  list_output = list(DF_output = DF_output, count_output = count_output)

  return(list_output)

}


#' mult_format
#' Return x axis labels to original values (we use ratio_reduction_x above to be able to perform calculations)
#'
#' @param ratio_reduction_x reduce x for plotting by this ratio
#'
#' @return
#' @importFrom scales comma
#'
#' @examples
mult_format <- function(ratio_reduction_x) {
  function(x) format(ratio_reduction_x * x, digits = 2) %>% as.numeric() %>% scales::comma()
}


#' create_plot
#'
#' @param DF input dataframe
#' @param label_circles Should we draw labels for the circles
#' @param max_overlaps overlaps in geom_text_repel
#' @param ID_var ID variable
#' @param group_var group variable
#' @param separation_factor how much separation between groups
#' @param size_text size text labels
#' @param ratio_reduction_x reduce x for plotting by this ratio
#'
#' @return
#' @importFrom dplyr distinct filter group_by left_join mutate
#' @importFrom ggplot2 aes coord_equal element_rect expansion ggplot element_text geom_polygon scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave theme theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang :=

#'
#' @examples
create_plot <- function(DF, label_circles = FALSE, max_overlaps = 5, ID_var, group_var, separation_factor = 5, size_text = 3, ratio_reduction_x) {

  # DEBUG
  # DF = DF_polygons
  # label_circles = TRUE
  # max_overlaps = 5
  # group_var = group_var
  # separation_factor = 1

  # Moves the circles in the y axis to separate by group_var
  DF_factors = DF %>% distinct(!!group_var := get(group_var)) %>% mutate(ID = (1:n()) * separation_factor)

  # Modifies the DF to separate circles
  DF = DF %>% left_join(DF_factors, by = group_var) %>% mutate(y = y + ID)



  # Position for y axis labels (group_var)
  position_y = DF %>% group_by(!!group_var := get(group_var)) %>% summarise(positions = median(y)) %>% arrange(positions)

  # Position of circle labels
  label_positions_temp = DF %>% group_by(id) %>% filter(y == median(y)) %>% filter(x == median(x))
  if (nrow(label_positions_temp) > 0) {
    label_positions = label_positions_temp %>% sample_n(1)
  } else {
    label_positions = DF %>% group_by(id) %>% filter(y == max(y)) %>% filter(x == max(x)) %>% sample_n(1)
  }

  # Main plot
  plot1 =
    ggplot(data = DF, aes(x, y, group = id)) +
    geom_polygon(colour = "#2b695c", fill = "#b8ceca", alpha = 0.3) +
    coord_equal() +
    labs(x = "", y = "") +
    theme_minimal(base_size = 16) +
    scale_x_continuous(labels = mult_format(ratio_reduction_x), n.breaks = 10, expand = expansion(mult = c(.02, .01)))  +
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    scale_y_continuous(breaks = unique(position_y$positions), labels = DF_factors[,1])

  # If including labels for circles
  if (label_circles) {
    plot1 +
      # ggrepel::geom_label_repel(
      ggrepel::geom_text_repel(
        size = size_text,
        max.overlaps = max_overlaps,
        max.time = .5, max.iter = 10000000,
        force_pull = 1,
        force = 1,
        # nudge_x = .1, nudge_y = .5,
        alpha = .5,
        data = label_positions,
        aes(label = paste0(get(ID_var)#, ": ",
                           # round(x * ratio_reduction_x, 0) %>% scales::comma(accuracy = 1), " | ",
                           # round(total_deaths_per_million, 0) %>% scales::comma(accuracy = 1)
                           ))
        )
  } else {
    plot1
  }
}







#' check_overlaps
#'
#' @param DF_polygons input dataframe (should be the output of create polygons)
#' @param CHECKS_plots Should we show plots
#'
#' @return
#' @importFrom utils tail
#' @importFrom dplyr summarise
#' @importFrom ggplot2 aes coord_equal element_rect expansion ggplot geom_sf element_text geom_polygon scale_fill_manual scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave theme theme_minimal
#' @importFrom grDevices colors
#' @importFrom sf st_area st_as_sf st_cast st_drop_geometry st_intersection

#'
#' @examples
check_overlaps <- function(DF_polygons, CHECKS_plots = FALSE) {

  # check_overlaps: no visible binding for global variable ‘id’
  # check_overlaps: no visible binding for global variable ‘geometry’
  # check_overlaps: no visible binding for global variable ‘.’
  # check_overlaps: no visible binding for global variable ‘n.overlaps’
  # check_overlaps: no visible binding for global variable ‘area’
  # check_overlaps: no visible binding for global variable ‘intersect_area’


  # DEBUG
  # DF_polygons = DF_polygons %>% filter(get(group_var) == DF_groups$group_var[.x])
  # CHECKS_plots = CHECKS_plots


  # suppressPackageStartupMessages(library(sf))

  shape_areas <- DF_polygons %>%
    st_as_sf(coords = c("x", "y")) %>%
    group_by(id) %>%
    summarise(do_union = F) %>%
    st_cast("POLYGON") %>%
    # st_cast("MULTIPOLYGON") %>%
    mutate(area = st_area(geometry)) %>%
    mutate(id = as.factor(id))

  intersect_pct <-
    st_intersection(shape_areas) %>%
    mutate(intersect_area = st_area(.))  # create new column with shape area
  # select(id, area, intersect_area, n.overlaps) %>%     # only select columns needed to merge
  # st_drop_geometry()


  DF_overlaps = intersect_pct %>% filter(n.overlaps > 1) %>%
    select(id, area, intersect_area, n.overlaps) %>%  # only select columns needed to merge
    st_drop_geometry()

  if (CHECKS_plots == TRUE & nrow(DF_overlaps) > 0) {

    plot_overlaps = intersect_pct %>%
      mutate(n.overlaps = as.factor(n.overlaps)) %>%
      # filter(n.overlaps > 1) +
      ggplot() +
      geom_sf(aes(fill = n.overlaps), alpha = .8) + #, color = "grey"
      # scale_y_continuous(limits = c(-.5, 2)) +
      # scale_x_continuous(limits = c(-.5, 1)) +
      scale_fill_manual(values = c("grey", "red", colors(distinct = TRUE) %>% tail(20))) +
      theme_minimal()
  } else {
    plot_overlaps = NULL
  }

  list_output = list(DF_overlaps = DF_overlaps, plot_overlaps = plot_overlaps)

  return(list_output)
}
