#' Providing a table of entities with incomplete observations (missing values)
#' 
#' Can be helpful to detect if panel dataset is balanced or unbalanced. 
#' @param data The data frame
#' @param idvar Entities' identifier
#' @return The tibble containing entities with number of missings for each variable, as well as total number of missings. The tibble is arranged by total number of missings.
#' @examples
#' # take balanced data and make it unbalanced
#' # by deletion of 2nd row (2nd time period for first individual)
#' data("Grunfeld", package = "plm")
#' Grunfeld_missing_period <- Grunfeld[-2, ]
#' detect_incomplete(Grunfeld_missing_period, firm)
#' @export
explore_incomplete <- function(data, idvar) {
  require(dplyr)
  require(tidyr)
  
  incomplete <- data %>% 
    group_by({{ idvar }}) %>% 
    summarise(across(everything(), ~ sum(is.na(.)))) %>% 
    mutate(total_missings = rowSums(across(where(is.numeric)))) %>%
    filter(total_missings > 0) %>%
    select({{ idvar }}, total_missings, everything()) %>%
    arrange(desc(total_missings))
  
  return(incomplete)
}




