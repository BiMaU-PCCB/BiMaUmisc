#' Format \emph{p}-values for display
#'
#' @description
#' Convert \emph{p}-values to a compact textual representation. The function prints decimals up to the first non-zero decimal and the following one, applying standard rounding.
#' 
#' @details
#' The number of decimals shown depends on the magnitude of the \emph{p}-value so that two meaningful digits are displayed after the leading zeros.
#'
#' Specifically:
#' \itemize{
#'  \item For \eqn{p \ge 0.1}, two decimal places are shown.
#'  \item For \eqn{0.01 \le p < 0.1}, three decimal places are shown.
#'  \item For \eqn{0.001 \le p < 0.01}, four decimal places are shown.
#'  \item For \eqn{0.0001 \le p < 0.001}, five decimal places are shown.
#'  \item For \eqn{p < 0.0001}, the string \code{"<0.0001"} is returned.
#' }
#' This formatting ensures that the first non-zero decimal of the \eqn{p}-value and the following digit are displayed, 
#' while avoiding an excessive number of leading zeros.
#' 
#' If \code{add.p = TRUE}, the prefix \code{"*p*="} (or \code{"*p*<"} when applicable) is added to the formatted value. 
#' This will ensure the \emph{p} appears in cursive when calling the function in line on R markdown documents.
#' 
#' @param p a value or vector of the \emph{p}-value(s) to be shown, where \eqn{0<p<1}.
#' @param add.p logical indicating whether the text \emph{p}= (or \emph{p}<, if it applies) should be added before the \emph{p}-value. \cr
#'              The default value is \code{FALSE}.
#' 
#' @returns 
#' A character vector with the formatted \emph{p}-values.
#' 
#' @examples
#' show.p(0.00785)
#' show.p(c(0.03042, 0.1579, 0.0000025))
#' show.p(0.00785, add.p = TRUE)
#' show.p(c(0.03042, 0.1579, 0.0000025), add.p = TRUE)

#' @importFrom exams fmt
#'
#' @export


show.p <- Vectorize(function(p, add.p = FALSE){
  
  # argument type checks
  if (is.na(p)){
    return("")
  }
  if (!is.numeric(p)) {
    stop("'p' must be a numeric value or vector")
  }
  if (!is.logical(add.p)) {
    stop("'add.p' must be logical")
  }
  
  # stop checks
  if (p < 0) {
    stop("p < 0")
  } else if (p > 1.00000001) {
    stop("p > 1")
  } # 1.00000001 -> fisher.test
  
  # function
  if (add.p) {
    if (round(p, 3) < 0.1 & round(p, 3) >= 0.01) {
      return(paste0("*p=*", exams::fmt(p, 3, zeros = TRUE)))
    } else if(round(p, 4) < 0.01 & round(p, 4) >= 0.001) {
      return(paste0("*p=*", exams::fmt(p, 4, zeros = TRUE)))
    } else if (round(p, 5) < 0.001 & round(p, 5) >= 0.0001) {
      return(paste0("*p=*", exams::fmt(p, 5, zeros = TRUE)))
    } else if(p < 0.0001) {
      return("*p<*0.0001")
    } else {
      return(paste0("*p=*", exams::fmt(p, 2, zeros = TRUE)))
    }
  } else {
    if (round(p, 3) < 0.1 & round(p, 3) >= 0.01) {
      return(exams::fmt(p, 3, zeros = TRUE))
    } else if(round(p, 4) < 0.01 & round(p, 4) >= 0.001) {
      return(exams::fmt(p, 4, zeros = TRUE))
    } else if (round(p, 5) < 0.001 & round(p, 5) >= 0.0001) {
      return(exams::fmt(p, 5, zeros = TRUE))
    } else if(p < 0.0001) {
      return("<0.0001")
    } else {
      return(exams::fmt(p, 2, zeros = TRUE))
    }
  }
})
