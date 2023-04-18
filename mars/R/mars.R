#' MARS
#'
#' @description The mars() function, which is the main function in the MARS Package, performs a Multivariate Adaptive Regression Splines (MARS) analysis. The MARS algorithm builds a regression model by partitioning the predictor variables into segments and fitting a linear regression model to each segment.
#'
#' @usage mars(formula, data, control)
#' @param formula interface to specify response and explanatory variables, a variable of the model to be fitted.
#' @param data A data frame, list, or environment, containing the variables in the model. If not found in the data, the variables are taken from the environment of formula
#' @param control An object of mars.control with Mmax = 2 and d = 3, by default.
#'
#' @details MARS is a non-parametric regression technique that partitions the predictor space into simple regions defined by piecewise linear functions of the input variables. The MARS algorithm uses a forward stepwise approach to iteratively add basis functions to the model.
#'
#' @return a `mars` object, which contain the model
#' @export
#'
#' @examples
#' test1 <- mars(y~., data1[1:100,], mars.control(Mmax=6))
#' print(test1)
#' summary(test1)
#' plot(test1)
#' anova(test1)
#' predict(test1,data1[101:200,1:10])
#'
#' @import stats
#' @references Jerome H. Friedman. Multivariate Adaptive Regression Splines (with discussion).Annals of Statistics 19/1, 1991. \url{https://statistics.stanford.edu/research/multivariate-adaptive-regression-splines.}
#' @seealso [mars.control] for constructing control objects, The default mars.control has Mmax is 2 and d is 3.
#' @seealso [plot.mars] for generates four different residual plots.
#' @seealso [predict.mars] for generates new dependent values based on the dataset.
#' @seealso [summary.mars] for provides a summary table of the estimated coefficients of all the independent variables with respect to the dependent variable.
#' @seealso [print.mars] for printing the coefficients for each independent variable.
#' @seealso [anova.mars] for compute analysis of variance (or deviance) tables for fitted `mars` model objects.
mars <- function(formula,data,control=mars.control()) {
  cc <- match.call() # save the call
  mf <- model.frame(formula,data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)[,-1,drop=FALSE]
  x_names <- colnames(x)
  control <- validate_mars.control(control)
  fwd <- fwd_stepwise(y,x,control)
  bwd <- bwd_stepwise(fwd,control)
  fit <- lm(y~.-1,data=data.frame(y=y,bwd$B))
  out <- c(list(call=cc,formula=formula,y=y,B=bwd$B,Bfuncs=bwd$Bfuncs,
                x_names=x_names),fit)
  class(out) <- c("mars",class(fit))
  out
}

#' Constructor for 'mars.control' objects
#'
#' This function constructs a 'mars.control' object that specifies
#' parameters used in mars.
#'
#' @param Mmax Maximum number of basis functions. Should be an even integer. Default is 2.
#' @param d The parameter used in calculation of Generalized cross-validation. Default is 3.
#' @param trace logical input to interpret the data basis functions that might reduce GCV in backward selection.
#'
#' @return an object of class 'mars.control', that is a list of parameters
#' @export
#'
#' @examples mc<-mars.control(Mmax=6,d=3,trace=FALSE)
#'
mars.control <- function(Mmax=2,d=3,trace=FALSE) {
  Mmax <- as.integer(Mmax)
  control <- list(Mmax=Mmax,d=d,trace=trace)
  control <- validate_mars.control(control)
  new_mars.control(control)
}

#Constructor for mars.control
new_mars.control <- function(control) {
  structure(control,class="mars.control")
}

#Validator for mars.control
validate_mars.control <- function(control) {
  stopifnot(is.integer(control$Mmax),is.numeric(control$d),
            is.logical(control$trace))
  if(control$Mmax < 2) {
    warning("Mmax must be >= 2; Reset it to 2")
    control$Mmax <- 2}
  if(control$Mmax %% 2 > 0) {
    control$Mmax <- 2*ceiling(control$Mmax/2)
    warning("Mmax should be an even integer. Reset it to ",control$Mmax)}
  control
}

#Forward Stepwise
fwd_stepwise <- function(y,x,control=mars.control()){

  Mmax <- (control$Mmax)/2

  N <- length(y)
  n <- ncol(x)
  B <- init_B(N,control$Mmax)

  Bfuncs <- vector(mode="list",length=control$Mmax+1)

  for(i in 1:Mmax) {
    M <- 2*i-1
    lof_best <- Inf

    for(m in 1:M) {
      V <- setdiff(1:n, Bfuncs[[m]][,"v"])

      for(v in V){
        tt <- split_points(x[,v],B[,m])

        for(t in tt) {
          Bnew <- data.frame(B[,(1:M)],Btem1=B[,m]*h(1,x[,v],t),Btem2=B[,m]*h(-1,x[,v],t))
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.-1,gdat,control)

          if(lof < lof_best) {
            lof_best <- lof
            best_split <- c(m=m,v=v,t=t)
          }
        }
      }
    }

    mstar <- best_split["m"]
    vstar <- best_split["v"]
    tstar <- best_split["t"]

    B[,M+1] <- B[,mstar]*h(-1, x[,vstar],tstar)
    B[,M+2] <- B[,mstar]*h(+1, x[,vstar],tstar)

    Bfuncs[[M+1]] = rbind(Bfuncs[[mstar]], c(s=-1, vstar, tstar))
    Bfuncs[[M+2]] = rbind(Bfuncs[[mstar]], c(s=1, vstar, tstar))
  }
  colnames(B) <- paste0("B", (0:(ncol(B)-1)))
  y <- as.vector(y)
  names(y) <- paste0(1:(length(y)))
  return(list(y=y,B=B,Bfuncs=Bfuncs))
}

#Split Points
split_points <- function(xv,Bm) {
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

#LOF based on GCV
LOF <- function(form, data, control){
  N <- nrow(data)
  ff <- lm(form, data)
  M <- length(ff$coefficients) - 1
  RSS <- sum((ff$res)^2)
  Ctilde <- sum(diag(hatvalues(ff))) + control$d*M
  return(RSS*(N/(N-Ctilde)^2))
}

#Initialized B
init_B <- function(n, Mmax){
  B <- data.frame( matrix(NA,nrow=n,ncol=(Mmax+1)) )
  B[,1] <- 1
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

#Hinge Function
h <- function(s,x,t) {
  return(pmax(0,s*(x-t)))
}

#Backward Stepwise
bwd_stepwise <- function(fwd,control){
  y <- fwd$y
  B <- fwd$B
  Mmax <- ncol(fwd$B)-1
  J <- 2:(Mmax+1)
  Kstar <- J
  data <- data.frame(y=y, B=B)
  lof_best <- LOF(y~.-1,data,control)

  for(M in (Mmax+1):2){
    b <- Inf
    L <- Kstar
    if(control$trace)
      cat("L",L,"\n")
    for (m in L){
      K <- setdiff(L,m)
      gdat <- data.frame(y,B[,K])
      lof <- LOF(y~.,gdat,control)
      if(lof < b){
        b <- lof
        Kstar <- K
      }
      if (lof < lof_best){
        lof_best <- lof
        J <- K
      }
    }
  }
  J <- c(1,J)
  return(list(y=y,B=B[,J],Bfuncs=fwd$Bfuncs[J]))
}

