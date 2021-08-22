#' prepare_data
#'
#' @param DF input dataframe
#' @param ID_var ID variable
#' @param group_var group variable
#' @param area_var area variable
#' @param x_var x axis variable
#' @param width_plot reduce x for plotting by this ratio
#' @param height_group height of y axis for each group
#'
#' @return
#' @importFrom ggplot2 ggplot aes element_text geom_tile scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave
#' @importFrom dplyr all_of filter mutate pull select
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
#' @importFrom stats median runif
#'
#' @examples
prepare_data <- function(DF, ID_var, group_var, area_var, x_var, width_plot = 100, height_group = 10) {

  # Translate input vars to x, group, area. Create y
  DF_prepared_data_raw =
    DF %>%
    select(all_of(ID_var), all_of(group_var), all_of(area_var), all_of(x_var)) %>%
    drop_na() %>%
    mutate(area = get(area_var),
           x = get(x_var),
           y = runif(n(), 0, height_group)) %>%
    select(x, y, area, all_of(ID_var), all_of(group_var), all_of(area_var), all_of(x_var))


  # Automatic parameters
  ratio_reduction_area = DF_prepared_data_raw %>% select(eval(area_var)) %>% max(.)
  ratio_reduction_x = DF_prepared_data_raw %>% select(eval(x_var)) %>% max(.)
  # cat(crayon::yellow("ratio_reduction_area:", round(ratio_reduction_area, 3), " ratio_reduction_x: ", ratio_reduction_x, "\n"))


  # Normalize area and x using width_plot
  DF_prepared_data =
    DF_prepared_data_raw %>%
    mutate(
      area = (area/ratio_reduction_area) * width_plot, # This normalizes from 0 to width_plot
      x = (x/ratio_reduction_x) * width_plot # This normalizes from 0 to width_plot
      ) %>%
    select(x, y, area, all_of(ID_var), all_of(group_var), all_of(area_var), all_of(x_var))

  DF_prepared_data

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
  # DF = ALL_data %>% filter(get(group_var) == DF_groups$group_var[1])

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

  DF_output

}



#' mult_format
#' Return x axis labels to original values (we use ratio_reduction_x above to be able to perform calculations)
#'
#' @param width_plot reduce x for plotting by this ratio
#'
#' @return
#' @importFrom scales comma
#'
#' @examples
mult_format <- function(ratio_reduction_x, width_plot) {
  # function(x) format(ratio_reduction_x * x, digits = 2) %>% as.numeric() %>% scales::comma()

  function(x) format((x/width_plot) * ratio_reduction_x, digits = 2) %>% as.numeric() %>% scales::comma()

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
#' @importFrom dplyr case_when distinct filter group_by left_join mutate
#' @importFrom ggplot2 aes coord_equal element_rect expansion ggplot element_text geom_polygon scale_x_continuous scale_y_continuous scale_fill_gradientn labs margin annotate ggsave theme theme_minimal
#' @importFrom ggrepel geom_text_repel
#' @importFrom rlang :=

#'
#' @examples
create_plot <- function(DF_prepared, DF,

                        ID_var, group_var, area_var, x_var,

                        separation_factor = 5, width_plot = 100,

                        label_circles = FALSE, max_overlaps = 5, size_text = 3,

                        highlight_ID = NULL) {


  # Compute automatic parameters
  ratio_reduction_area = DF_prepared %>% select(eval(area_var)) %>% max(.)
  ratio_reduction_x = DF_prepared %>% select(eval(x_var)) %>% max(.)

  # Moves the circles in the y axis to separate by group_var
  DF_factors = DF %>% distinct(!!group_var := get(group_var)) %>% mutate(ID = (1:n()) * separation_factor)

  # Modifies the DF to separate circles
  DF = DF %>% left_join(DF_factors, by = group_var) %>% mutate(y = y + ID)



  # Position for y axis labels (group_var)
  position_y = DF %>% group_by(!!group_var := get(group_var)) %>% summarise(positions = median(y)) %>% arrange(positions)


  # Position of circle labels.
    # Try with median
  label_positions_temp = DF %>% group_by(id) %>% filter(y == median(y)) %>% filter(x == median(x))

  # If there are no points in the median, use max
  if (nrow(label_positions_temp) > 0) {
    label_positions_temp2 = label_positions_temp %>% sample_n(1)
  } else {
    label_positions_temp2 = DF %>% group_by(id) %>% filter(y == max(y)) %>% filter(x == max(x)) %>% sample_n(1)
  }

  # Final label_positions
  label_positions =
    label_positions_temp2 %>%
    mutate(color_DF = "#333333", fill_DF = "#333333") %>%
    mutate(fill_DF =
             dplyr::case_when(
               get(ID_var) %in% highlight_ID ~ "darkred",
               TRUE ~ fill_DF),
           color_DF =
             dplyr::case_when(
               get(ID_var) %in% highlight_ID ~ "darkred",
               TRUE ~ color_DF))



  # Highlight elements
  DF = DF %>%
    mutate(color_DF = "#2b695c", fill_DF = "#b8ceca") %>% # Here and in label_positions
    mutate(fill_DF =
             dplyr::case_when(
               get(ID_var) %in% highlight_ID ~ "#ffe2e0",
               TRUE ~ fill_DF),
           color_DF =
             dplyr::case_when(
               get(ID_var) %in% highlight_ID ~ "darkred",
               TRUE ~ color_DF))


  # Main plot
  plot1 =
    # ggplot(data = DF, aes(x, y, group = id)) +
    ggplot(data = DF, aes(x, y, group = id, colour = color_DF, fill = fill_DF)) +

    # geom_polygon(colour = "#2b695c", fill = "#b8ceca", alpha = 0.3) +
    geom_polygon(alpha = 0.3) +
    coord_equal() +
    labs(x = "", y = "") +
    theme_minimal(base_size = 16) +
    scale_x_continuous(labels = mult_format(ratio_reduction_x, width_plot), n.breaks = 10, expand = expansion(mult = c(.02, .01)))  +
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    scale_y_continuous(breaks = unique(position_y$positions), labels = DF_factors[,1]) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity()

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
                           # round(x * width_plot, 0) %>% scales::comma(accuracy = 1), " | ",
                           # round(total_deaths_per_million, 0) %>% scales::comma(accuracy = 1)
                           ))
        )
  } else {
    plot1
  }
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
check_diffs <- function(DF_prepared, DF, check_var,

                        ID_var, group_var, area_var, x_var,

                        width_plot) {

  # Compute automatic parameters
  ratio_reduction_area = DF_prepared %>% select(eval(area_var)) %>% max(.)
  ratio_reduction_x = DF_prepared %>% select(eval(x_var)) %>% max(.)

  # Extract group
  group_str = DF %>% distinct(!!group_var := get(group_var)) %>% pull(group_var)

  DFCHECK =
    DF %>% group_by(get(ID_var)) %>% sample_n(1) %>% arrange(x) %>% ungroup() %>%
    # mutate(position_check = 1:n(), total_x_plot = x * ratio_reduction_x) %>%
    mutate(position_check = 1:n(), total_x_plot = (x/width_plot) * ratio_reduction_x) %>%


    rename(x_check = x) %>%
    select(position_check, x_check, eval(ID_var), total_x_plot)

  DF_output =
    DF_prepared %>%

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

  list_output

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

  list_output
}
