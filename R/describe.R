#' Calculating basic descriptive statistics 
#'
#' Created to provide basic descriptive statistics in the tibble format
#' @param data The data frame
#' @param vars The vector of numeric columns/variables of interest
#' @return The tibble with basic descriptive statistics of selected columns
#' @examples 
#' data("iris")
#' vars_select <- c("Sepal.Length", "Sepal.Width")
#' describe(iris, vars_select) 
#' @export
describe <- function(data, vars){
  require(dplyr)
  require(tidyr)
  
  desc_tibble <- data %>%
    select(all_of(vars)) %>%
    summarise(across(everything(), 
                     list(n = \(x) tidyother::number(x), 
                          mean = \(x) mean(x, na.rm = TRUE), 
                          median = \(x) median(x, na.rm = TRUE), 
                          sd = \(x) sd(x, na.rm = TRUE), 
                          min = \(x) min(x, na.rm = TRUE), 
                          max = \(x) max(x, na.rm = TRUE)))) %>% 
    pivot_longer(everything(), names_to = "name", values_to = "value") %>% 
    separate(name, c("variable", "statistic"), sep = "_") %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    arrange(variable) %>% 
    select(variable, n, mean, median, sd, min, max)
  
  return(desc_tibble)
}
