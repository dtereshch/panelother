#' Providing a table of entities with incomplete observations (missing values)
#' 
#' @param data The data frame
#' @param idvar Entities' identifier
#' @return The tibble containing entities with number of missings for each variable, as well as total number of missings. The tibble is arranged by total number of missings.
#' @examples
#' data("USSeatBelts", package = "AER")
#' explore_incomplete(USSeatBelts, state)
#' @export
explore_incomplete <- function(data, idvar) {
  require(dplyr)
  require(tidyr)
  
  incomplete <- data %>% 
    group_by({{ idvar }}) %>% 
    summarise(across(everything(), ~ sum(is.na(.)))) %>% 
    mutate(total_missings = rowSums(across(-{{ idvar }}))) %>%
    filter(total_missings > 0) %>%
    select({{ idvar }}, total_missings, everything()) %>%
    arrange(desc(total_missings))
  
  return(incomplete)
}




