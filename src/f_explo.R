

# Colors used for plots in analysis
cols <-
  c("1" = "#bc4749",
    "2" = "#ffc300",
    "3" = "#a7c957",
    "4" = "#1a535c",
    "5" = "#a9def9",
    "6" = "#f3722c",
    "7" = "#BBBBBB",
    "8" = "#999944",
    "9" = "#332288")



#' create mapview color palette
#'
#' @param data sf-dataset to be plotted
#' @param variable variable used for coloring in map
#' @param colors vector of colors with names as values of the "variable"
#'
#' @return color palette usable for col.regions-argument in mapview object
#'
#' @examples
#' mapv_colors <- mapv_colors(cluster_sf, "cluster", cols)
mapv_colors <- function(data, variable, colors){
  mapview::mapviewColors(x=data,
                         zcol = variable, 
                         colors = colors[1:length(levels(data[[variable]]))],
                         at = names(colors[1:length(levels(data[[variable]]))]))
}



#' grouped boxplots
#' 
#' create boxplots for different subgroupings of one dataset
#'
#' @param data dataset
#' @param y variable to be used for the boxplots
#' @param cluster variable which defines the grouping 
#' @param colors vector of colors depending on groups
#'
#' @return ggplot-boxplot
#'
#' @examples
#' bp(isere, "PC1", "cluster", c("1" = "#1a535c", "2" = "#a9def9", "3" = "#a7c957", "4" = "#ffc300"))
bp <- function(data, y, cluster, colors){
  
  plot <- ggplot(data = data) +
    geom_boxplot(aes(x = !!rlang::sym(cluster), y = !!rlang::sym(y), 
                     fill = !!rlang::sym(cluster)), # color by cluster-variable
                 outlier.colour = "darkgrey", na.rm = TRUE)+
    scale_fill_manual(values = colors, guide = "none") + # coloring by colors-vector, suppress warning
    theme_light() +
    coord_cartesian(ylim = boxplot.stats(data[[y]])$stats[c(1, 5)]) # set y-limits to roughly the 95 %-interval of whole dataset
  
  return(plot)
}



#' Plot PC-series with categorical variable in background
#'
#' @param data input data table
#' @param x longitudinal variable
#' @param y var used for geom_line()-function
#' @param var_cat categorical var to be plotted in background
#' @param colors vector of colors used for printing category
#'
#' @return ggplot()-graph of dataseries plotted as continuous line and categorical variable in background 
#' @export
#'
#' @examples
#' plot_pc(isere, "measure", "PC1", "cluster",
#'                 c("1" = "#bc4749", "2" = "#ffc300", "3" = "#a7c957", "4" = "#1a535c", "5" = "#a9def9"))
plot_pc <- function(data, x, y, cat, colors){
  
  # limit the x-axis to value range
  lims <- c(min(data[[x]]), max(data[[x]]) )
  
  # create plot with limits and y-variable
  plot <- ggplot(data |> dplyr::arrange(!!rlang::sym(x)), # sort acc. to x-variable 
                 mapping = aes(x = !!rlang::sym(x), y = !!rlang::sym(y))) +
    geom_rect(mapping = aes(xmin = !!rlang::sym(x), 
                            xmax = if_else(!is.na(dplyr::lead(!!rlang::sym(x))), 
                                           dplyr::lead(!!rlang::sym(x)),
                                           !!rlang::sym(x) + 200), # set length of last segment to 200 if no next boundary is available
                            ymin = -Inf, ymax = Inf, fill = !!rlang::sym(cat)),
              alpha = 0.6, stat = "identity") + # background-coloring according to cluster
    geom_line() + # actual graph
    scale_fill_manual(values = colors) + # manual coloring
    theme_minimal() +
    labs(x = "length [m]")
  
  return(plot)
}


#' Plot categorical timeseries
#'
#' @param data input data table
#' @param x longitudinal variable
#' @param cat categorical var to be plotted in background
#' @param colors vector of colors used for printing category
#'
#' @return ggplot()-graph of categorical plotted over the network length 
#' @export
#'
#' @examples
#' plot_categ(isere, "measure", "cluster", cols)
#' 
plot_categ <- function(data, x, cat, colors){
  
  plot <- ggplot(data |> dplyr::arrange(!!rlang::sym(x)), # sort acc. to x-variable 
                 mapping = aes(x = !!rlang::sym(x))) +
    geom_rect(mapping = aes(xmin = !!rlang::sym(x), 
                            xmax = if_else(!is.na(dplyr::lead(!!rlang::sym(x))), 
                                           dplyr::lead(!!rlang::sym(x)),
                                           !!rlang::sym(x) + 200), # set length of last segment to 200 if no next boundary is available
                            ymin = -0.5, ymax = 0.5, fill = !!rlang::sym(cat))) +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    labs(x = "length [m]") +
    theme(
      axis.text.y = element_blank(),  # Remove y-axis text
      axis.ticks.y = element_blank(), # Remove y-axis ticks
      axis.title.y = element_blank(), # Remove y-axis title
      panel.grid = element_blank(),
      legend.position = "none")   # Remove grid lines
  
  return(plot)
}

