#' Plot.mars
#' @description generates four different residual plots
#'
#' @param object an `mars` object from the result from mars() function
#'
#' @export
#' @details
#' The Cumulative Distribution plot shows the empirical cumulative distribution function (CDF) of the data. The empirical CDF is the ratio of values ​​less than or equal to X. It is an increasing step function that exhibits a vertical jump of 1/N for each value of X equal to the observed value.
#' The Residuals vs. Fit plot is a scatterplot of residuals on the y-axis and fitted values ​​(estimated responses) on the x-axis. The chart detects nonlinearities, uneven error variances, and outliers.
#' Normal Q-Q plots are used to assess how well the distribution of a data set fits a normal (Gaussian) standard distribution with mean  0 and  standard deviation  1.
#' The Scale-Location plot plots a function of the residuals that reflects the  variability of the error about the mean. I'm looking for a point cloud that looks random. An increasing relationship means that as the mean increases the variance increases  and vice versa.
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

