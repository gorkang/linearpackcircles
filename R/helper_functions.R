create_polygons <- function(DF) {
  
  # DF = ALL_data %>% filter(continent == DF_CONTINENTS$continent[1])
  
  library(packcircles)
  
  limits = c(min(DF$x, na.rm = TRUE), max(DF$x, na.rm = TRUE))
  limits_y = c(min(DF$y, na.rm = TRUE), max(DF$y, na.rm = TRUE)) 
  
  res <- circleRepelLayout(DF, xlim = limits, ylim = limits_y, xysizecols = 1:3)
  cat("- ", res$niter, "iterations performed\n")
  
  # Get vertex data for the initial layout where sizes are areas
  dat.gg.initial <- circleLayoutVertices(DF, sizetype = "area")
  
  # Get vertex data for the layout returned by the function where sizes are radii
  dat.gg.final <- circleLayoutVertices(res$layout)
  
  DF_output = dat.gg.final %>% left_join(DF %>% mutate(id = 1:n()) %>% select(id, location, continent, total_cases_per_million), by = "id") %>% 
    mutate(id = paste0(id, "_", continent))
  
  return(DF_output)
  
}


check_diffs <- function(ALL_data, DF) {
  
  # Extract continent
  continent_str = DF %>% distinct(continent) %>% pull(continent)
  
  DFCHECK = 
    DF %>% group_by(location) %>% sample_n(1) %>% arrange(x) %>% ungroup() %>% 
    mutate(position_check = 1:n(), total_cases_plot = x * ratio_reduction_x) %>% 
    rename(x_check = x) %>% 
    select(position_check, x_check, location, total_cases_plot)
  
  DF_output = 
    ALL_data %>% filter(continent == continent_str) %>% 
    arrange(x) %>% ungroup() %>% mutate(position_x = 1:n()) %>% 
    select(x, location, continent, position_x, total_cases_per_million) %>% 
    left_join(DFCHECK, by = "location") %>%
    mutate(DIFF = position_x - position_check,
           DIFF_n = (total_cases_plot - total_cases_per_million),
           DIFF_pct = ((total_cases_plot - total_cases_per_million)/total_cases_per_million) * 100,
           DIFF_abs = ((total_cases_plot - total_cases_per_million)/max(total_cases_per_million)) * 100) %>%
    select(DIFF, matches("position_"), starts_with("x"), everything())
  
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


create_plot <- function(DF, label_circles = FALSE) {
  
  # DF = DFX
  # label_circles = TRUE
  
  mean_Y = DF %>% group_by(continent) %>% summarise(XXX = median(y)) %>% arrange(XXX)
  
  # Position of text labels
  label_positions = DF %>% group_by(id)  %>% filter(y == max(y)) %>% filter(x == median(x)) %>% sample_n(1)
  
  
  plot1 = 
    ggplot(data = DF, aes(x, y, group = id)) +
    geom_polygon(colour = "#2b695c", fill = "#b8ceca", alpha = 0.3) +
    coord_equal() +
    labs(x = "", y = "") +
    theme_minimal() +
    scale_x_continuous(labels = mult_format(), n.breaks = 10)  +
    # theme(axis.text.y=element_blank(),
    #       axis.ticks.y=element_blank()) +
    scale_y_continuous(breaks = unique(mean_Y$XXX), labels = unique(DF$continent))
  
  
  if (label_circles) {
    plot1 + 
      # ggrepel::geom_label_repel(
      ggrepel::geom_text_repel(
        size = 2,
        max.overlaps = 5, 
        # nudge_x = .1, nudge_y = .5,
        alpha = .5,
        data = label_positions, 
        aes(label = paste0(location, ": ", round(x * ratio_reduction_x,0) %>% scales::comma())), size = size_text #total_cases_per_million
      ) 
  } else {
    plot1
  }
}
