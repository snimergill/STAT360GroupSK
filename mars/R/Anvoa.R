#' ANVOA function
#' @description Compute analysis of variance (or deviance) tables for fitted `mars` model objects.
#'
#' @param object an `mars` object from the result from mars() function
#'
#' @return returns an object of class `anova`. These objects represent analysis-of-variance and analysis-of-deviance tables.
#' @export
#'
#' @examples
#' test <- mars(y~.,data=data1[1:100,])
#' anova(test)
anova.mars <- function(object){
  class(object) <- c("lm")
  print(object)
  return(anova(object))
}
