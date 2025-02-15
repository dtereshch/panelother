#' Draws a heatmap showing the structure of the unbalanced panel, i.e., for which periods there are missing observations by entity
#' 
#' @param data The data frame
#' @param idvar Entities' identifier
#' @param timevar Time identifier
#' @return The heatmap with missing time periods coloured red
#' @examples
#' # Load the data
#' data("USSeatBelts", package = "AER")
#' 
#' # Create unbalanced dataset
#' USSeatBelts_unbalanced <- USSeatBelts %>% 
#'   filter(!(year == 1995 & str_detect(state, "^N"))) %>%
#'   filter(!(year == 1985 & str_detect(state, "^A")))
#' 
#' # Draw the heatmap
#' plot_unbalanced(USSeatBelts_unbalanced, state, year)
#' 
#' @export
plot_unbalanced <- function(data, idvar, timevar) {
  require(dplyr)
  
  crosstab <- data %>% select({{ idvar }}, {{ timevar }}) %>% table() %>% as.matrix()
  
  row_order <- hclust(dist(crosstab))$order
  column_order <- hclust(dist(t(crosstab)))$order
  
  return(heatmap(crosstab[row_order, ], Colv = NA, Rowv = NA, scale = "none", col = rainbow(2)))
}



