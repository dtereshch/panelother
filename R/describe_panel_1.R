#' Calculating descriptive statistics for one variable in the panel data
#'
#' Inspired by Stata's xtsum command and corresponding discussion at https://stackoverflow.com/questions/49282083/xtsum-command-for-r. 
#' Calculates the mean, the variance, and the standard deviation of selected variable for panel data. 
#' Decomposes the variance and the standard deviation into between and within components.
#' @param data The data frame
#' @param varname The numeric variable of interest
#' @param unit The object id variable
#' @return The tibble with the mean, the variation, and the standard deviation of selected variable for panel data. 
#' @examples 
#' data("Crime", package = "plm")
#' describe_panel_1(Crime, crmrte, county)
#' @export
describe_panel_1 <- function(data, varname, unit) {
  require(rlang)
  require(dplyr)
  require(tidyr)
  
  varname <- enquo(varname)
  unit <- enquo(unit)
  
  overall <- data %>%
    dplyr::select(c(!!unit, !!varname)) %>%
    drop_na() %>%
    ungroup() %>%
    mutate(diff_sq_overall = (!!varname - mean(!!varname, na.rm = TRUE))^2) %>%
    ungroup() %>%
    summarise(mean_overall = mean(!!varname, na.rm = TRUE), 
              variance_overall = sum(diff_sq_overall, na.rm = TRUE)/(n() - 1),
              sd_overall = sd(!!varname, na.rm = TRUE),
              min_overall = min(!!varname, na.rm = TRUE),
              max_overall = max(!!varname, na.rm = TRUE),
              n_overall = n(),
              variable_overall =  quo_name(varname))
  
  between <- data %>% 
    dplyr::select(c(!!unit, !!varname)) %>%
    drop_na() %>%
    mutate(diff_sq_overall = (!!varname - overall$mean_overall)^2) %>%
    dplyr::group_by(!!unit) %>%
    summarise(mean_within = mean(!!varname, na.rm = TRUE),
              diff_sq_berween = (mean_within - overall$mean_overall)^2) %>%
    ungroup() %>%
    summarise(variance_between = sum(diff_sq_berween, na.rm = TRUE)/(n() - 1),
              sd_between = sd(mean_within),
              min_between = min(mean_within, na.rm = TRUE),
              max_between = max(mean_within, na.rm = TRUE),
              n_between = n(),
              variable_between =  quo_name(varname))
  
  within <- data %>% 
    dplyr::select(c(!!unit, !!varname)) %>%
    drop_na() %>%
    dplyr::group_by(!!unit) %>%
    summarise(mean_within = mean(!!varname, na.rm = TRUE),
              n_within = n()) %>%
    ungroup() %>%
    dplyr::full_join(dplyr::select(data, c(!!unit, !!varname)), by = quo_name(unit)) %>%
    dplyr::group_by(!!unit) %>%
    mutate(var_scaled = scale(!!varname, scale = FALSE)) %>%
    ungroup() %>%
    mutate(diff_within = !!varname - mean_within + overall$mean_overall) %>%  # Stata's way
    summarise(variance_within = (sd(var_scaled, na.rm = TRUE))^2,
              sd_within = sd(var_scaled, na.rm = TRUE),
              min_within = min(diff_within, na.rm = TRUE),
              max_within = max(diff_within, na.rm = TRUE),
              n_within = mean(n_within, na.rm = TRUE),
              variable_within =  quo_name(varname))
  
  result <- bind_cols(overall, between, within) %>% 
    gather(key, value) %>%
    separate(key, c("measure", "decomposition")) %>%
    spread(measure, value) %>% 
    mutate_at(vars(-c(variable, decomposition)), as.numeric) %>%
    mutate(frac = variance/overall$variance_overall) %>%
    select(variable, decomposition, mean, variance, frac, sd, min, max, n) %>%
    arrange(match(decomposition, c("overall", "between", "within")))
  
  return(result)
}
