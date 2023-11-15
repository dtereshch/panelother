#' Counting the number of non-missing values
#'
#' Created to count the number of non-missing values in a column inside dplyr::summarise()
#' @param varname The column/variable of interest
#' @return The number of non-missing values in x
#' @examples 
#' library(dplyr);
#' data("cars");
#' cars %>% summarise(across(everything(), list(n = number, mean = mean, sd = sd), na.rm = TRUE));
#' cars %>% summarise(across(everything(), \(x) number(x, na.rm = TRUE)));
#' @export
number <- function(varname, na.rm = TRUE){
  return(sum(!is.na(varname)))
  }
