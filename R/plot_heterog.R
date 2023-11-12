#' Plotting heterogeneity among groups
#'
#' Created to provide insights about heterogeneity among groups using `ggplot2` functions. Typically for panel data, grouping by object identifiers or periods is assumed. 
#' @param data The data frame in long panel format or tidy format
#' @param varname The numeric variable of interest
#' @param by The grouping variable
#' @return The ggplot object
#' @examples 
#' data("Fatalities", package = "AER")
#' Fatalities <- mutate(Fatalities, fatalrate = fatal / pop * 10000)
#' plot_heterog(Fatalities, fatalrate, year)
#' plot_heterog(Fatalities, fatalrate, state)
#' @export
plot_heterog <- function(data, varname, by){
  require(rlang)
  require(dplyr)
  require(ggplot2)
  
  varname <- enquo(varname)
  by <- enquo(by)
  
  data %>% 
    ggplot(aes(!!by, !!varname)) + 
    geom_point(shape = 21, size = 2, alpha = 0.5) + 
    stat_summary(fun = mean, aes(x = !!by, group = 1), geom = "line", color = "royalblue") + 
    stat_summary(fun = mean, aes(x = !!by, group = 1), geom = "point", shape = 18, size = 3, color = "royalblue")
}
