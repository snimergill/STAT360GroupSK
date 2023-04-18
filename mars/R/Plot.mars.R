#' Plot.mars
#' @description generates four different residual plots
#'
#' @param object an `mars` object from the result from mars() function
#'
#' @export
#' @details
#' A Cumulative Distribution plot shows the empirical cumulative distribution function (CDF) of the
#' data. The empirical CDF is the proportion of values less than or equal to X. It is an increasing step
#' function that has a vertical jump of 1/N at each value of X equal to an observed value.
#'
#' A Residuals vs Fitted plot is a scatter plot of residuals on the y-axis and fitted values (estimated
#' responses) on the x-axis. The plot detects non-linearity, unequal error variances, and outliers.
#'
#' A Normal Q-Q plot is used to evaluate how well the distribution of a dataset matches a standard
#' normal (Gaussian) distribution, where the mean is 0 and the standard deviation is 1.
#'
#' A Scale-Location plot plots a function of residuals that reflect the errors' variability relative to the
#' mean, we will be looking for a random-looking cloud of point, increasing relationship means variance
#' increases as mean increases, and vice versa.
#'
#' @examples
#' test <- mars(y~.,data=data1[1:100,])
#'
#' plot(test)
#' @import graphics
plot.mars <- function(object){
  par(mfrow=c(2,2))
  
  plot(ecdf(abs(object$residuals)),
       xlab="Absolute Residuals",
       ylab="Proportion",
       main="Cumulative Distribution")
  
  plot(object$fitted.values,
       object$residuals,
       xlab="Fitted Values",
       ylab="Residuals",
       main="Residuals vs Fitted")
  abline(h=0, lty=2)
  lines(lowess(abs(object$residuals) ~ object$fitted.values), col = "red")
  
  qqnorm(object$residuals,
         xlab="Theoretical Quantiles",
         ylab="Residuals Quantiles",
         main="Normal Q-Q")
  qqline(object$residuals, lty="dashed")
  
  plot(object$fitted.values,
       sqrt(abs(rstandard(object))),
       xlab = "Fitted Values",
       ylab = "Sqrt of Standardized Residuals",
       main = "Scale-Location")
  lines(lowess(abs(sqrt(abs(rstandard(object)))) ~ object$fitted.values), col = "red")
  
}

