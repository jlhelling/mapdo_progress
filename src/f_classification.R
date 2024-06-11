library(tidyverse)
library(sf)

#' Classification by most dominant landuse
#' 
#' adds new variable to df representing the name of the variable among `vars` which hast the highest value, 
#' in addition, a corresponding color is added
#'
#' @param df df containing all dgos of a region or axis
#' 
#' @importFrom dplyr rowwise mutate ungroup
#'
#' @return classified df with new variable `metric_max`
#'
#' @examples
#' 
autoclass_dom_landuse <- function(df){
  
  # variables among which to select the one with greatest value
  vars <- c("forest_pc", "grassland_pc", "crops_pc", "built_environment_pc")
  
  colors <- c("#31572c", "#90be6d", "#ffbe0b", "#ae2012") |> setNames(vars) 
  
  # get variable with maximum values and save it as new variable metric_max
  classified_df <- df %>%
    rowwise() %>%
    mutate(
      metric_max = vars[which.max(c_across(vars))],
      color = colors[[metric_max]]
    ) %>%
    ungroup()  # Ungroup after row-wise operation
  
  return(classified_df)
}

autoclass_urban_pressure <- function(df){
  
  # variables among which to select the one with greatest value
  vars <- c("highly urbanised", "urbanised", "moderately urbanised", "no urban land")
  
  colors <- c("#6a040f", "#ba181b", "#ffd97d", "#ffffff") |> setNames(vars) 
  
  classified_df <- df %>%
    rowwise() %>%
    mutate(
      class_urban = 
        case_when(
          built_environment_pc >= 70 ~ vars[[1]],
          built_environment_pc >= 40 ~ vars[[2]],
          built_environment_pc >= 10 ~ vars[[3]],
          built_environment_pc >= 0 ~ vars[[4]],
      ),
      color = colors[[class_urban]]
    ) %>%
    ungroup()
}


autoclass_pressures_pc <- function(df){
  
  # variables among which to select the one with greatest value
  vars <- c("très fort", "fort", "moyen", "faible / absente")
  
  colors_urban <- c("#6a040f", "#ba181b", "#f48c06", "#fff3b0") |> setNames(vars)
  colors_crops <- c("#bc3908", "#ff7b00", "#ffdd00", "#fff3b0") |> setNames(vars)
  colors_natural <- c("#081c15", "#2d6a4f", "#74c69d", "#d8f3dc") |> setNames(vars)
  colors_confinement <- c("#184e77", "#168aad", "#99d98c", "#d9ed92") |> setNames(vars)
  
  classified_df <- df %>%
    rowwise() %>%
    mutate(
      # urban impact
      class_urban = 
        case_when(
          built_environment_pc >= 70 ~ vars[[1]],
          built_environment_pc >= 40 ~ vars[[2]],
          built_environment_pc >= 10 ~ vars[[3]],
          built_environment_pc >= 0 ~ vars[[4]],
        ),
      color_urban = colors_urban[[class_urban]],
      
      # agricultural impact
      class_crops = 
        case_when(
          crops_pc >= 70 ~ vars[[1]],
          crops_pc >= 40 ~ vars[[2]],
          crops_pc >= 10 ~ vars[[3]],
          crops_pc >= 0 ~ vars[[4]],
        ),
      color_crops = colors_crops[[class_crops]],
      
      # natural impact
      natural_pc = natural_open_pc + forest_pc + grassland_pc,
      class_natural = 
        case_when(
          natural_pc >= 70 ~ vars[[1]],
          natural_pc >= 40 ~ vars[[2]],
          natural_pc >= 10 ~ vars[[3]],
          natural_pc >= 0 ~ vars[[4]],
        ),
      color_natural = colors_natural[[class_natural]],
      
      # confinement
      class_confinement = 
        case_when(
          idx_confinement >= 0.75 ~ vars[[4]],
          idx_confinement >= 0.5 ~ vars[[3]],
          idx_confinement >= 0.25 ~ vars[[2]],
          idx_confinement >= 0 ~ vars[[1]],
        ),
      color_confinement = colors_confinement[[class_confinement]],
      
      # active channel
      class_ac = 
        case_when(
          active_channel_pc >= 70 ~ vars[[1]],
          active_channel_pc >= 40 ~ vars[[2]],
          active_channel_pc >= 10 ~ vars[[3]],
          active_channel_pc >= 0 ~ vars[[4]],
        ),
      color_ac = colors_confinement[[class_ac]]
      
    ) %>%
    ungroup()
}
