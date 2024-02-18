#' Calculating group descriptive statistics 
#'
#' Created to provide group descriptive statistics in the tibble format
#' @param data The data frame
#' @param varnames The vector of numeric columns/variables of interest
#' @param groupvar The vector of factor columns/variables representing groups
#' @return The tibble with group descriptive statistics of selected columns
#' @examples 
#' data("iris")
#' describe_by(iris, c(Sepal.Length, Sepal.Width), Species)
#' @export
describe_by <- function(data, varnames, groupvar){
  require(dplyr)
  require(tidyr)
  
  desc_tibble <- data %>%
    group_by({{ groupvar }}) %>%
    select({{ varnames }}) %>%
    summarise(across(everything(), 
                     list(n = \(x) tidyother::number(x), 
                          mean = \(x) mean(x, na.rm = TRUE), 
                          median = \(x) median(x, na.rm = TRUE), 
                          sd = \(x) sd(x, na.rm = TRUE), 
                          min = \(x) min(x, na.rm = TRUE), 
                          max = \(x) max(x, na.rm = TRUE)))) %>% 
    pivot_longer(-{{ groupvar }}, names_to = "name", values_to = "value") %>% 
    extract(name, into = c("variable", "statistic"), "(.*)_([^_]+)$") %>%
    #separate(name, into = c("variable", "statistic"), sep="_(?=[^_]+$)") %>%
    #separate(name, c("variable", "statistic"), sep = "_") %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    arrange(variable)
  
  return(desc_tibble)
}
