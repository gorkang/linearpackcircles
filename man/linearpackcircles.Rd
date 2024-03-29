% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearpackcircles.R
\name{linearpackcircles}
\alias{linearpackcircles}
\title{linearpackcircles}
\usage{
linearpackcircles(
  DF,
  ID_var = "ID",
  group_var = "group",
  area_var = "area",
  x_var = "x",
  separation_factor = 1,
  width_plot = 100,
  height_group = 10,
  label_circles = TRUE,
  max_overlaps = 8,
  size_text = 3,
  highlight_ID = NULL,
  random_seed = 12,
  area_multiplier = 1
)
}
\arguments{
\item{DF}{input dataframe}

\item{ID_var}{ID variable}

\item{group_var}{group variable}

\item{area_var}{area variable}

\item{x_var}{x axis variable}

\item{separation_factor}{how much separation between groups}

\item{width_plot}{reduce x for plotting by this ratio}

\item{height_group}{height of y axis for each group}

\item{label_circles}{Should we draw labels for the circles}

\item{max_overlaps}{overlaps in geom_text_repel}

\item{size_text}{size text labels}

\item{highlight_ID}{Which ID's to highlight}

\item{random_seed}{random seed to use}

\item{area_multiplier}{multiply area size by this}
}
\value{
ggplot object
}
\description{
linearpackcircles
}
