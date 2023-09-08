#' Calculating group descriptive statistics 
#'
#' Created to provide group descriptive statistics in the tibble format
#' @param data The data frame
#' @param vars The vector of numeric columns/variables of interest
#' @param by The vector of factor columns/variables representing groups
#' @return The tibble with group descriptive statistics of selected columns
#' @examples 
#' data("iris")
#' vars_select <- c("Sepal.Length", "Sepal.Width")
#' group_vars <- c("Species")
#' descby(iris, vars_select, group_vars)
#' @export
descby <- function(data, vars, by){
  require(dplyr)
  require(tidyr)

  desc_tibble <- data %>%
    group_by(across(all_of(by))) %>%
    select(all_of(vars)) %>%
    summarise(across(everything(), 
                     list(n = \(x) number(x), 
                          mean = \(x) mean(x, na.rm = TRUE), 
                          median = \(x) median(x, na.rm = TRUE), 
                          sd = \(x) sd(x, na.rm = TRUE), 
                          min = \(x) min(x, na.rm = TRUE), 
                          max = \(x) max(x, na.rm = TRUE)))) %>% 
    pivot_longer(where(is.numeric), names_to = "name", values_to = "value") %>% 
    separate(name, c("variable", "statistic"), sep = "_") %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    arrange(variable) %>% 
    select(variable, by, n, mean, median, sd, min, max)
  
  return(desc_tibble)
}
