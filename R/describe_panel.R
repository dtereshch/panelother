#' Calculating descriptive statistics for selected variables in the panel data
#'
#' Inspired by Stata's xtsum command and corresponding discussion at https://stackoverflow.com/questions/49282083/xtsum-command-for-r. 
#' Calculates the mean, the variance, and the standard deviation of selected variables for panel data. 
#' Decomposes the variance and the standard deviation into between and within components.
#' @param data The data frame
#' @param varnames The vector of numeric variables of interest
#' @param entity The object id variable
#' @return The tibble with the mean, the variation, and the standard deviation of selected variables for panel data. 
#' @examples 
#' data("Crime", package = "plm")
#' vars_select <- c("crmrte", "polpc")
#' describe_panel(Crime, vars_select, "county")
#' @export
describe_panel <- function(data, varnames, entity) {
  require(rlang)
  require(dplyr)
  
  desc_table <- tibble()
  
  for (varname in varnames) {
    desc_table <- desc_table %>%
      bind_rows(describe_panel_1(data, !!sym(varname), !!sym(entity)))
  }
  
  return(desc_table)
}
