#' feature selection in Boruta
#'
#' @param mydata the data
#'
#' @return test
#' @export
#'
#' @examples
#' library(dplyr)
#' meta_dat1 <- t(meta_dat) %>%
#'   as.data.frame() %>%
#'   dplyr::mutate(group=group)
#' result_ML_Boruta <- ML_Boruta(meta_dat1)

ML_Boruta <- function(mydata) {

  decision <- NULL
  mydata$group <- as.factor(mydata$group)
  model_boruta <- Boruta::Boruta(group ~ ., data = mydata, doTrace = 2, maxRuns = 500)
  #print(boruta)
  #plot(boruta, las = 2, cex.axis = 0.7)
  #plotImpHistory(boruta)
  #bor <- TentativeRoughFix(boruta)
  #print(bor)
  #attStats(boruta)
  
  filter_result <- Boruta::attStats(model_boruta) %>%
    tibble::rownames_to_column(var="name") %>%
    dplyr::filter(decision=="Confirmed")
  return(filter_result)
}

