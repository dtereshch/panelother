#' Calculating group descriptive statistics 
#'
#' Created to provide group descriptive statistics in the tibble format
#' @param data The data frame
#' @param varnames The vector of numeric columns/variables of interest
#' @param by The vector of factor columns/variables representing groups
#' @return The tibble with group descriptive statistics of selected columns
#' @examples 
#' data("iris")
#' vars_select <- c("Sepal.Length", "Sepal.Width")
#' describe_by(iris, vars_select, "Species")
#' @export
describe_by <- function(data, varnames, by){
  require(dplyr)
  require(tidyr)
  require(stringr)
  
  varnames <- str_replace_all(varnames, "_", ".")
  data <- rename_all(list(~ stringr::str_replace_all(., "_", ".")))

  desc_tibble <- data %>%
    group_by(!!sym(by)) %>%
    select(all_of(varnames)) %>%
    summarise(across(everything(), 
                     list(n = \(x) tidyother::number(x), 
                          mean = \(x) mean(x, na.rm = TRUE), 
                          median = \(x) median(x, na.rm = TRUE), 
                          sd = \(x) sd(x, na.rm = TRUE), 
                          min = \(x) min(x, na.rm = TRUE), 
                          max = \(x) max(x, na.rm = TRUE)))) %>% 
    pivot_longer(-!!sym(by), names_to = "name", values_to = "value") %>% 
    separate(name, c("variable", "statistic"), sep = "_") %>%
    pivot_wider(names_from = statistic, values_from = value) %>%
    arrange(variable) %>% 
    select(variable, !!sym(by), n, mean, median, sd, min, max)
  
  return(desc_tibble)
}
