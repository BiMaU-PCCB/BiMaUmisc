#' Automatically select statistical tests for quantitative variables
#'
#' @description
#' This function evaluates quantitative variables across stratification groups and automatically determines the appropriate parametric or non-parametric hypothesis test. 
#' See \strong{Details} for an exhaustive explanation of the criteria followed.
#' Designed to work seamlessly with \code{\link[gtsummary]{tbl_summary}}, this function generates the input lists for arguments \code{test} and \code{test.args} of the function \code{\link[gtsummary]{add_p}}, specifying the recommended test function and any additional arguments different from the defaults.
#'
#' @details
#' If any level of \code{strata} has less than five non-missing observations, no statistical test is performed for that variable, and a warning is issued.
#' Otherwise, the appropriate test is selected based on the following criteria:
#' 
#' \strong{Two Groups (\code{strata} with 2 levels):}
#' \itemize{
#'   \item \strong{\emph{t}-test} (\code{\link[stats]{t.test}}): selected if sample sizes in both groups are \eqn{\ge 30} and Shapiro-Wilk tests indicate normality in both groups. Variance equality is tested using an \emph{F}-test.
#'   \item \strong{Mann-Whitney test} (\code{\link[stats]{wilcox.test}}): selected if sample sizes are small, this is \eqn{< 30} in some group, or if normality is rejected in either group.
#' }
#' 
#' \strong{More than Two Groups (\code{strata} with > 2 levels):}
#' \itemize{
#'   \item \strong{ANOVA} (\code{\link[stats]{oneway.test}}): selected if ANOVA model residuals adhere to a normal distribution according to the Shapiro-Wilk test. Homoscedasticity is tested via Bartlett's test.
#'   \item \strong{Kruskal-Wallis test} (\code{\link[stats]{kruskal.test}}): selected if ANOVA residuals deviate from normality.
#' }
#' 
#' @usage find.test(data, variables, strata, alpha = 0.05)
#'
#' @param data name of the dataset, of class \code{data.frame}, containing at least two columns (quantitative variable, strata).
#' @param variables column name(s) of the quantitative variable(s) to analyse. Can be specified as a character string (with quotes) or as an unquoted symbol.
#' @param strata strata indicator column name. Can be specified as a character string (with quotes) or as an unquoted symbol. Must be a factor with at least 2 levels.
#' @param alpha significance level threshold used for normality (\code{\link[stats]{shapiro.test}}) and homoscedasticity (\code{\link[stats]{var.test}} or \code{\link[stats]{bartlett.test}}) testing. \cr
#'              The default value is 0.05.
#'
#' @return 
#' A \code{list} of length 2 intended to be passed directly to arguments \code{test} and \code{test.args} of the function \code{\link[gtsummary]{add_p}} when using \code{\link[gtsummary]{tbl_summary}}:
#' \describe{
#'   \item{\code{test}}{a list of formulas matching each variable name to its selected test string (e.g., \code{age ~ "t.test"}).}
#'   \item{\code{test.args}}{a list of formulas specifying additional test parameters (e.g., \code{age ~ list(var.equal = TRUE)}). Variables requiring no additional parameters are omitted from this second list.}
#' }
#'  
#' @examples
#' df <- as.data.frame(gtsummary::trial)
#' df$trt <- factor(df$trt)
#' df$grade <- factor(df$grade)
#' 
#' # Two levels variable (trt: Drug A, Drug B)
#' tests2 <- find.test(df, variables = c(age, marker), strata = trt)
#' tests2
#' 
#' # Use outputs directly with tbl_summary()
#' gtsummary::tbl_summary(df, include = c(age, marker), by = trt) |>
#'   gtsummary::add_p(test = tests2$test, test.args = tests2$test.args)
#' 
#' # Three levels variable (grade: I, II, III)
#' tests3 <- find.test(df, variables = c(age, marker), strata = grade)
#' tests3
#' 
#' # Use outputs directly with tbl_summary()
#' gtsummary::tbl_summary(df, include = c(age, marker), by = grade) |>
#'   gtsummary::add_p(test = tests3$test, test.args = tests3$test.args)
#'   
#' # Using function show.p() from BiMaUmisc package
#' gtsummary::tbl_summary(df, include = c(age, marker), by = grade) |>
#'   gtsummary::add_p(test = tests3$test, test.args = tests3$test.args,
#'                    pvalue_fun = function(x) show.p(x))
#'
#' @importFrom stats as.formula aov residuals shapiro.test var.test bartlett.test na.omit
#' 
#' @export


find.test <- function(data, variables, strata, alpha = 0.05) {
  
  # columns
  strata <- gsub("\\s+", "", gsub('^"|"$', '', deparse(substitute(strata))))
  variables <- paste(deparse(substitute(variables)), collapse = "")
  variables <- gsub("\\s+", "", variables)
  variables <- unlist(strsplit(gsub("\\)", "", gsub("c\\(", "", variables)), ","))
  variables <- gsub('^"|"$', "", variables)  
  
  # argument checks
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (!strata %in% names(data)) {
    stop("strata argument is not a column in 'data'")
  }
  if (!is.factor(data[, strata])) {
    stop("'strata' must be a factor")
  }
  if (length(levels(data[, strata])) < 2) {
    stop("'strata' must have at least 2 levels")
  }
  if (any(table(data[, strata]) == 0)) {
    stop("strata argument can not have a level with 0 elements")
  }
  if (!all(variables %in% names(data))){
    stop("'variables' must only contain column names in 'data'")
  }
  if (any(duplicated(variables))) {
    stop("two of the column identifiers are the same")
  }
  
  # warning checks
  if (any(table(data[, strata]) == 0)) {
    warning("'strata' column has (at least) one empty level. Calculation proceeds ignoring the empty level(s)")
    data[, strata] <- factor(data[, strata])
  }
  
  # stop checks
  if (any(table(data[, strata]) < 4)) {
    aux <- levels(data[, strata])[table(data[, strata]) <= 4]
    if (length(aux) == 1) {
      stop(paste("level", aux, "in 'strata' has less than 5 elements"))
    } else{
      stop(paste("levels", paste(aux, collapse=', '), "in 'strata' have less than 5 elements"))
    }
  }
  
  # extracting necessary columns of data
  df <- data.frame(strata = data[, strata])

  # defining outputs
  result <- list(test = vector(mode = "list", length = length(variables)),
                 test.args = rep(list(NA), length(variables)))
  
  j <- 1
  for (i in variables) {
    df$variable <- data[, i]
    
    if (!is.numeric(df$variable)) {
      stop(paste("variable", i, "is not numeric"))
    }
    
    if (any(table(df$strata[!is.na(df$variable)]) <= 4)) {
      aux <- levels(df$strata[!is.na(df$variable)])[table(df$strata[!is.na(df$variable)]) <= 4]
      if (length(aux) == 1) {
        warning(paste("variable", i, "has less than 5 elements in level", aux, "of 'strata' \n  Do not use this output when calling tbl_summary() \n  Rerun find.test() call without this variable \n"))
        next
      } else{
        warning(paste("variable", i, "has less than 5 elements in levels", paste(aux, collapse=', '), "of 'strata' \n  Do not use this output when calling tbl_summary() \n  Rerun find.test() call without this variable \n"))
        next
      }
    }
    
    # 2 groups
    if(length(levels(df$strata)) == 2) {
      if(any(tapply(df$variable, df$strata, function(g) length(unique(na.omit(g)))) == 1)) {
        result$test[[j]] <- as.formula(paste(i, '~ "wilcox.test"'))
        #attributes(result$test[[j]])$.Environment <- NULL
      } else{
        if (table(df$strata[!is.na(df$variable)])[1] >= 30 & 
            table(df$strata[!is.na(df$variable)])[2] >= 30 & 
            shapiro.test(df$variable[df$strata == levels(df$strata)[1]])$p.value > alpha & 
            shapiro.test(df$variable[df$strata == levels(df$strata)[2]])$p.value > alpha) {
          result$test[[j]] <- as.formula(paste(i, '~ "t.test"'))
          #attributes(result$test[[j]])$.Environment <- NULL
          if (var.test(variable ~ strata, df)$p.value > alpha) {
            result$test.args[[j]] <- as.formula(paste(i, '~ list(var.equal = TRUE)'))
            #attributes(result$test.args[[j]])$.Environment <- NULL
          }
        } else{
          result$test[[j]] <- as.formula(paste(i, '~ "wilcox.test"'))
          #attributes(result$test[[j]])$.Environment <- NULL
        }
      }
    }
    
    # >2 groups
    else if (length(levels(df$strata)) > 2) {
      res <- aov(variable ~ strata, df)$residuals
      if (shapiro.test(res)$p.value > alpha) {
        result$test[[j]] <- as.formula(paste(i, '~ "oneway.test"'))
        #attributes(result$test[[j]])$.Environment <- NULL
        if (bartlett.test(variable ~ strata, df)$p.value > alpha) {
          result$test.args[[j]] <- as.formula(paste(i, '~ list(var.equal = TRUE)'))
          #attributes(result$test.args[[j]])$.Environment <- NULL
        }
      } else{
        result$test[[j]] <- as.formula(paste(i, '~ "kruskal.test"'))
        #attributes(result$test[[j]])$.Environment <- NULL
      }
    }
    j <- j + 1
  }
  
  result$test.args <- result$test.args[!is.na(result$test.args)]
  return(result)
}
