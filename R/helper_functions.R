prepare_data <- function(DF, ID_var, group_var, area_var, x_var) {
  
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




create_polygons <- function(DF, group_var) {
  
  # DEBUG
  # DF = ALL_data %>% filter(continent == DF_CONTINENTS$continent[1])
  
  library(packcircles)
  
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



check_diffs <- function(ALL_data, DF, check_var) {
  
  # DEBUG
  # DF = DF1
  # check_var = "total_deaths_per_million"
  
  # Extract continent
  continent_str = DF %>% distinct(continent) %>% pull(continent)
  
  DFCHECK = 
    DF %>% group_by(location) %>% sample_n(1) %>% arrange(x) %>% ungroup() %>% 
    mutate(position_check = 1:n(), total_x_plot = x * ratio_reduction_x) %>% 
    rename(x_check = x) %>% 
    select(position_check, x_check, location, total_x_plot)
  
  DF_output = 
    ALL_data %>% filter(continent == continent_str) %>% 
    arrange(x) %>% ungroup() %>% mutate(position_x = 1:n()) %>% # arrange() needed to assign position_x
    select(x, location, continent, position_x, eval(check_var)) %>% 
    left_join(DFCHECK, by = "location") %>%
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
              continent = unique(continent)
    )
  
  list_output = list(DF_output = DF_output, count_output = count_output)
  
  return(list_output)
  
}


# Return x axis labels to original values (we use ratio_reduction_x above to be able to perform calculations)
mult_format <- function() {
  function(x) format(ratio_reduction_x * x, digits = 2) %>% as.numeric() %>% scales::comma()
}


create_plot <- function(DF, label_circles = FALSE, max_overlaps = 5) {
  
  # DF = DFX
  # label_circles = TRUE
  
  # Position for y axis labels (group_var)
  position_y = DF %>% group_by(continent) %>% summarise(positions = median(y)) %>% arrange(positions)
  
  # Position of circle labels
  label_positions = DF %>% group_by(id)  %>% filter(y == median(y)) %>% filter(x == median(x)) %>% sample_n(1)
  
  # Main plot
  plot1 = 
    ggplot(data = DF, aes(x, y, group = id)) +
    geom_polygon(colour = "#2b695c", fill = "#b8ceca", alpha = 0.3) +
    coord_equal() +
    labs(x = "", y = "") +
    theme_minimal(base_size = 16) +
    scale_x_continuous(labels = mult_format(), n.breaks = 10, expand = expansion(mult = c(.02, .01)))  +
    theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
    scale_y_continuous(breaks = unique(position_y$positions), labels = unique(DF$continent))
  
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
        aes(label = paste0(location#, ": ", 
                           # round(x * ratio_reduction_x, 0) %>% scales::comma(accuracy = 1), " | ",
                           # round(total_deaths_per_million, 0) %>% scales::comma(accuracy = 1)
                           ))
        ) 
  } else {
    plot1
  }
}
