#' Summary Mars
#'
#' @description provides a summary table of the estimated coefficients of all the independent variables with respect to the dependent variable.
#'
#' @param object an `mars` object from the result from mars() function
#'
#' @export
#' @details For each independent variable, a sign (represents where the split direction is left: -1 or right: +1) and a knot for each split are also provided.
#'
#' @examples
#' test <- mars(y~.,data=data1[1:100,])
#'
#' summary(test)
summary.mars <- function(object){
  sumdata <- as.data.frame(object$coefficients)
  rownames(sumdata)[1] < "(Intercept)"
  colnames(sumdata)[1] <- "(Estimated Coefficients)"
  
  print(sumdata)
  summary(object$residuals)
  
  
  for (i in 2:length(object$Bfuncs)){
    cat("Coefficients ",names(object$coefficients)[i],": \n")
    for (j in 1: nrow(object$Bfuncs[[i]])){
      cat("Hinge function:",
          object$x_names[object$Bfuncs[[i]][j,2]],
          "sign",
          object$Bfuncs[[i]][j,1],
          "split at:",
          object$Bfuncs[[i]][j,3],
          "\n")
    }
  }
}


