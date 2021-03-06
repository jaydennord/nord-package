% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/product_indicators.R
\name{product_ind}
\alias{product_ind}
\title{Mean-centered product indicators}
\usage{
product_ind(data, vars1, vars2, bind = TRUE, method = c("single", "double"))
}
\arguments{
\item{data}{A data frame.}

\item{bind}{Logical. If \code{TRUE}, the centered variables and
product indicators are appended to the data frame provided by
\code{data}. If \code{FALSE}, only the product indicators are
returned.}

\item{method}{Character vector with options "single" and "double".
If "single" is included, the matched products of \code{vars1} and
\code{vars2} are included. If "double" is included, the product
indicators are mean centered.}

\item{var1, var2}{Character vectors naming the variables to be
multiplied together.}
}
\value{
A data frame. If \code{bind = TRUE}, all centered variables
of \code{data} and all product indicators are appended to the the
original data frame. The merged data frame is returned. If
\code{bind = FALSE}, only the centered variables and product
indicators are returned in a data frame.
}
\description{
Creates mean-centered and/or double-mean-centered
product indicators. Additionally centers all variables in
\code{data}
}

