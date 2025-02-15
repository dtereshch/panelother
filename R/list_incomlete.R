#' Providing a table of entities with incomplete observations (missing values)
#' 
#' @param data The data frame
#' @param idvar Entities' identifier
#' @return The vector containing entities with missing values
#' @examples
#' data("USSeatBelts", package = "AER")
#' list_incomplete(USSeatBelts, state)
#' @export
list_incomplete <- function(data, idvar) {
  require(dplyr)
  require(tidyr)
  
  incomplete <- data %>% 
    group_by({{ idvar }}) %>% 
    summarise(across(everything(), ~ sum(is.na(.)))) %>% 
    mutate(total_missings = rowSums(across(-{{ idvar }}))) %>%
    filter(total_missings > 0) %>%
    select({{ idvar }}, total_missings, everything())
  
  incomplete_ids <- incomplete %>% filter(total_missings > 0) %>% select({{ idvar }}) %>% as_vector() %>% unique()
  
  return(incomplete_ids)
}




