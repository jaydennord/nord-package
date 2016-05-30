center <- function(data, vars = colnames(data)){

  d_center <- scale(data[, vars], scale = FALSE)
  colnames(d_center) <- paste0(vars, "c")
  return(d_center)

}




product <- function(data, vars1, vars2){

  d_product <- data[, vars1] * data[, vars2]
  colnames(d_product) <- paste0(vars1, vars2)
  return(d_product)

}




#' @export
#' @title Mean-centered product indicators
#' @description Creates mean-centered and/or double-mean-centered
#' product indicators. Additionally centers all variables in
#' \code{data}
#' @param data A data frame.
#' @param var1,var2 Character vectors naming the variables to be
#' multiplied together.
#' @param bind Logical. If \code{TRUE}, the centered variables and
#' product indicators are appended to the data frame provided by
#' \code{data}. If \code{FALSE}, only the product indicators are
#' returned.
#' @param method Character vector with options "single" and "double".
#' If "single" is included, the matched products of \code{vars1} and
#' \code{vars2} are included. If "double" is included, the product
#' indicators are mean centered.
#' @return A data frame. If \code{bind = TRUE}, all centered variables
#' of \code{data} and all product indicators are appended to the the
#' original data frame. The merged data frame is returned. If
#' \code{bind = FALSE}, only the centered variables and product
#' indicators are returned in a data frame.

product_ind <- function(data, vars1, vars2, bind = TRUE,
                        method = c("single", "double")){

  if(!is.data.frame(data)) stop("'data' must be a data frame")

  d_center <- center(d_center)

  vars1 <- paste0(vars1, "c")
  vars2 <- paste0(vars2, "c")
  d_product <- product(d_center, vars1, vars2)

  if("double" %in% method){
    d_product_center <- center(d_product)
  } else {
    d_product_center <- NULL
  }

  if("single" %in% method){
    d_product <- NULL
  }

  d_out <- cbind(d_center, d_product, d_product_center)

  if(bind) return(cbind(data, d_out)) else return(d_out)

}




#' @export
#' @title Residual-centered product indicators
#' @description Creates residual-centered product indicators.
#' @param data A data frame.
#' @param var1,var2 Character vectors naming the indicators to be
#' multiplied together.
#' @param bind Logical. If \code{TRUE}, the product indicators are
#' appended to the data frame provided by \code{data}. If
#' \code{FALSE}, only the product indicators are returned.
#' @return A data frame. If \code{bind = TRUE}, the product indicators
#' are appended to the input data frame given by \code{data}. If
#' \code{FALSE}, only the product indicators are returned.


ortho <- function(data, vars1, vars2, bind = TRUE){

  if(!is.data.frame(data)) stop("'data' must be a data frame")

  vars <- expand.grid(
    vars1, vars2,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE)

  regress_on <- paste(vars1, vars2, sep = " + ", collapse = " + ")
  d_prod <- d[, vars[, 1]] * d[, vars[, 2]]
  colnames(d_prod) <- paste0(vars[, 1], vars[, 2])

  temp <- cbind(d, d_prod)
  d_prod_o <- sapply(colnames(d_prod), function(i){
    resid(lm(
      as.formula(paste(i, regress_on, sep = " ~ ")), data = temp))
  })
  colnames(d_prod_o) <- paste0(colnames(d_prod), "o")

  if(bind) return(cbind(data, d_prod_o)) else return(d_prod_o)

}
