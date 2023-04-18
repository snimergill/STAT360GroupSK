#' Predict Mars
#' @description Predict with an `mars` model for new data, returns the predicted basis function.
#'
#' @param object 'mars' object to generate the prediction
#' @param newdata dataset that contain independent variable to make the perdition of depend variable using the fitted `mars` model
#'
#' @return dataset which contain the predicted depended variable
#' @export
#'
#' @examples
#' test <- mars(y~.,data=data1[1:100,])
#' predict(test,data1[101:200,])
predict.mars <- function(object,newdata) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}

#Make_B function
make_B <- function(X,Bfuncs){
  output <- init_B(nrow(X), length(Bfuncs)-1)
  for(i in 2:length(Bfuncs)){
    tmp <- 1
    for(j in 1:nrow(Bfuncs[[i]])){
      tmp <- tmp*h( Bfuncs[[i]][j,"s"],X[,Bfuncs[[i]][j,"v"]],Bfuncs[[i]][j,"t"])
    }
    output[,i] <- tmp
  }
  return (as.matrix(output))
}

