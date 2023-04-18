#' Print mars
#'
#' @description Print some information for a `mars` object, include calls formula and coefficients
#'
#' @param object an `mars` object from the result from mars() function
#'
#' @export
#'
#' @details First calls the mars() function and the regression model formula, then provides the coefficients for each independent variable.
#'
#' @examples
#' test <- mars(y~.,data=data1[1:100,])
#' print(test)
#'
print.mars <- function(object){
  cat("\nCall:\n")
  print(object$call)
  
  model<-"Y = B0"
  for(i in 2:length(object$coefficients)){
    model<-paste(model, names(object$coefficients)[i], sep=" + ")
  }
  cat("Formula: ",model,"\n\n")
  
  for(j in 1:length(object$coefficients)){
    cat(names(object$coefficients)[j]," coefficient: ",object$coefficients[j],"\n")
  }
}