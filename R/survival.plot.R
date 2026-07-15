#' Plot survival curves
#'
#' @description
#' This function produces an elegant survival curve plot. 
#' It can automatically append the number-at-risk table and display the log-tank test \emph{p}-value (obtained from a call to \code{\link[survival]{survdiff}}). 
#' Highly customizable, it offers numerous arguments to easily modify the plot's appearance.
#'
#' @usage survival.plot(data, time, status, strata = 1,
#'               legend = TRUE, legend.pos = "bottomleft", legend.labs = NULL,
#'               legend.title = NULL, legend.cex = 1,
#'               events = FALSE, events.pos = NULL, events.text = NULL,
#'               mark.time = TRUE, lwd = 3, 
#'               risk = TRUE, risk.text = TRUE, risk.labs = NULL, risk.cex = 1, 
#'               p.value = TRUE, p.value.pos = NULL,
#'               units = "days", xlab = NULL, times = NULL,
#'               yscale = 1, ylab = NULL, 
#'               main = "Survival curve", col = NULL, ...)
#'
#' @param data name of the dataset, of class \code{data.frame}, containing at least 2 columns (follow up time, status indicator).
#' @param time follow-up time column name. Can be specified as a character string (with quotes) or as an unquoted symbol.
#' @param status status indicator column name. Can be specified as a character string (with quotes) or as an unquoted symbol.
#' @param strata strata indicator column name. Can be specified as a character string (with quotes) or as an unquoted symbol. Up to four categories. Must be a factor. \cr
#'               The default value is 1, this is, there are no groups.
#' @param legend logical indicating whether a legend should be added to the plot. If \code{strata} is set to 1, this argument is ignored. \cr
#'               The default value is \code{TRUE}.
#' @param legend.pos this argument can be a single keyword from the list in \code{\link[graphics]{legend}}, such as \code{"bottomleft"} or \code{"topright"},
#'                   which places the legend on the inside of the plot frame at the given location. In this case, this argument is passed to \code{\link[base]{match.arg}}. 
#'                   Otherwise, a 2-dimensional numerical vector indicating the x-y coordinates can be indicated. If \code{legend} is set to \code{FALSE}, this argument is ignored. \cr
#'                   The default value is \code{"bottomleft"}.
#' @param legend.labs a character vector of length \eqn{\geq}2 to appear as the legend labels. If \code{legend} is set to \code{FALSE}, this argument is ignored. \cr
#'                    The default value, \code{NULL}, indicates that the levels of \code{strata} are to be used as the legend labels.
#' @param legend.title title for the legend. If \code{legend} is set to \code{FALSE}, this argument is ignored. \cr
#'                     The default value, \code{NULL}, indicates on title should be added.
#' @param legend.cex legend's character expansion factor. A numerical value giving the amount by which the legend text should be magnified relative to the default.
#'                   If \code{legend} is set to \code{FALSE}, this argument is ignored. \cr
#'                   The default value is 1.
#' @param events logical indicating whether the number of events should be added to the text in the legend. 
#'               If \code{strata} is set to 1, the total number of events is added directly to the plot instead.
#'               If \code{legend} is set to \code{FALSE} and \code{strata} is not 1, this argument is ignored. \cr
#'               The default value is \code{FALSE}.
#' @param events.pos 2-dimensional numerical vector indicating the x-y coordinates where the number of events should be added to the plot.
#'                   If \code{strata} is not 1 or \code{events} is set to \code{FALSE}, this argument is ignored. \cr
#'                   The default value, \code{NULL}, indicates that the text should be placed on the bottom-left corner.
#' @param events.text a character indicating the text to display before the number of events in the plot.
#'                    If \code{events} is set to \code{FALSE}, this argument is ignored. \cr
#'                    The default value, \code{NULL}, indicates that the text "Events" should be added when \code{strata} is set to 1 and the text "events" should be added when \code{strata} is not set to 1.
#' @param mark.time logical indicating whether the curves should be marked at each censoring time. \cr
#'                  The default value is \code{TRUE}.
#' @param lwd a number indicating the line width(s). Alternatively, when \code{strata} is not set to 1, 
#'            an \eqn{n}-dimensional numerical vector can be indicated, where \eqn{n} is the number of levels in \code{strata}. \cr
#'            The default value is 3.
#' @param risk logical indicating whether the number-at-risk table should be appended to the plot. \cr
#'             The default value is \code{TRUE}.
#' @param risk.text logical indicating whether the legend for the number-at-risk table should display text labels. When \code{FALSE}, line segments are used instead.
#'                  If \code{risk} is set to \code{FALSE} or \code{strata} is set to 1, this argument is ignored. \cr
#'                  The default value is \code{TRUE}.
#' @param risk.labs \eqn{n}-dimensional character vector, where \eqn{n} is the number of levels in \code{strata}, indicating the text labels in the number-at-risk table.
#'                  If \code{risk.text} is set to \code{FALSE} or \code{strata} is set to 1, this argument is ignored. \cr
#'                  The default value, \code{NULL}, indicates that the levels of \code{strata} should be used.
#' @param risk.cex number-at-risk table's character expansion factor. A numerical value giving the amount by which the number-at-risk table text should be magnified relative to the default.
#'                 If \code{risk} is set to \code{FALSE}, this argument is ignored. \cr
#'                 The default value is 1.
#' @param p.value logical indicating whether the \emph{p}-value obtained from a call to \code{\link[survival]{survdiff}} with \code{rho} equal to 0, this is the log-rank or Mantel-Haenszel test, 
#'                should be displayed in the plot. The \emph{p}-value will be displayed in the format specified by the \code{\link[BiMaUmisc]{show.p}} function.
#'                If \code{strata} is set to 1, this argument is ignored. \cr
#'                The default value is \code{TRUE}.
#' @param p.value.pos 2-dimensional numerical vector indicating the x-y coordinates where the number of events should be added to the plot.
#'                    If \code{strata} is not 1 or \code{events} is set to \code{FALSE}, this argument is ignored. \cr
#'                    The default value, \code{NULL}, indicates that the text should be placed on the upper-right corner.
#' @param units character indicating the time units. Options are: "days", "weeks", "months" or "years".
#'              This argument will be used to create the default label and tick-marks for the x-axis. This argument is not case-sensitive and is passed to \code{\link[base]{match.arg}}. \cr
#'              The default value is \code{"days"}.
#' @param xlab character indicating a label for the x-axis. \cr
#'             The default value, \code{NULL}, indicates that indicates that the text "Time (units)" should be used, 
#'             where "units" is specified by the \code{units} argument.
#' @param times numerical vector with the time points at which tick-marks are to be drawn in the x-axis. \cr
#'              The default value, \code{NULL}, indicates that a time vector will be automatically constructed based on the \code{units} argument:
#'              \itemize{
#'                \item If \code{units} is set to "days": from 0 to the maximum survival time rounded up to the nearest multiple of 7.
#'                \item If \code{units} is set to "weeks": from 0 to the maximum survival time rounded up to the nearest integer.
#'                \item If \code{units} is set to "months": from 0 to the maximum survival time rounded up to the nearest multiple of 12.
#'                \item If \code{units} is set to "years": from 0 to the maximum survival time rounded up to the nearest integer.
#'              }
#' @param yscale a numeric value used to multiply the labels on the y-axis. A value of 100, for instance, would be used to give a percent scale.
#'               Notice only the labels are changed, not the actual plot coordinates. \cr
#'               The default value is 1.
#' @param ylab character indicating a label for the y-axis. \cr
#'             The default value, \code{NULL}, indicates that indicates that the text "Survival" should be used. 
#'             Moreover, if \code{yscale} is set to 1 (or 100) the text "(proportion)" or "(percentage)" will also be added.
#' @param main overall title for the plot. \cr
#'             The default value is \code{"Survival curve"}.
#' @param col colour palette to be used. \cr
#'            The default value, \code{NULL}, indicates that a different colour should be used per level in \code{strata}. 
#'            This is, a vector from 1 to \eqn{n}, where \eqn{n} is the number of levels in \code{strata}, will be used.
#' @param ... other graphical parameters (to be passed to \code{\link[survival]{plot.survfit}}).
#' 
#' @returns 
#' A customized survival curve plot displaying the requested graphical and statistical modifications.
#' 
#' @section Note:
#' A call to \code{\link[graphics]{par}} is used in this function. Notice that the arguments
#' \code{font.axis}, \code{font.lab}, \code{cex.lab}, \code{las} and \code{xpd} are always set to 2, 2, 1.2, 1, and \code{TRUE}, respectively. Moreover,
#' the argument \code{mar} is always modified and depends on the number of levels in strata as well as whether or not the number-at-risk table is appended. \cr
#' For optimal resolution and layout alignment when using the function in an R file, adjust the plot window. 
#' For example, ideal dimensions are approximately 7 x 5'' for 24'' screens and 5.9 x 4.2'' for 13'' screens. \cr
#' For optimal resolution and layout alignment when using the function in an R markdown file or similar, use \code{fig.dim = c(7, 5)} in the corresponding chunk.
#' 
#' @examples
#' lung <- survival::cancer
#' lung$time_y <- lung$time/365.25
#' lung$sex <- factor(lung$sex, levels = 1:2, labels = c("Male", "Female"))
#' 
#' # no strata
#' survival.plot(lung, time_y, status, units = "y")
#' # customizing
#' survival.plot(lung, time_y, status, units = "y",
#'               xlab = "Time since diagnosis (in years)",
#'               yscale = 100,
#'               main = "Overall survival", mark.col = "darkgray")
#'
#' # 2 strata
#' survival.plot(lung, time_y, status, sex, units = "y")
#' # customizing
#' survival.plot(lung, time_y, status, sex, 
#'               legend.pos = "topright", legend.title = "Sex", 
#'               events = TRUE, risk.text = FALSE, p.value.pos = c(2.4, 0.4),
#'               units = "y", xlab = "Time since diagnosis (in years)", 
#'               main = "Overall survival", col = c("darkviolet", "darkgreen"))
#' 
#' @importFrom survival survfit Surv survdiff
#' @importFrom latex2exp TeX
#' @importFrom plotfunctions isColor
#' @importFrom graphics par axis segments text
#' @importFrom utils tail
#' 
#' @export 



survival.plot <- function(data, time, status, strata = 1, 
                          legend = TRUE, legend.pos = "bottomleft", legend.labs = NULL, 
                          legend.title = NULL, legend.cex = 1,
                          events = FALSE, events.pos = NULL, events.text = NULL,
                          mark.time = TRUE, lwd = 3,
                          risk = TRUE, risk.text = TRUE, risk.labs = NULL, risk.cex = 1, 
                          p.value = TRUE, p.value.pos = NULL,
                          units = "days", xlab = NULL, times = NULL,
                          yscale = 1, ylab = NULL,
                          main = "Survival curve", col = NULL, ...){
  
  # reset par at exit
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  # reset options at exit
  op.def <- options()
  options(scipen = 999) # modifies display arguments in TeX()
  
  # columns
  time <- gsub("\\s+", "", gsub('^"|"$', '', deparse(substitute(time))))
  status <- gsub("\\s+", "", gsub('^"|"$', '', deparse(substitute(status))))
  strata <- gsub("\\s+", "", gsub('^"|"$', '', deparse(substitute(strata))))
  
  # argument checks
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame")
  }
  if (strata != "1") {
    if (!all(c(time, status, strata) %in% names(data))) {
      stop("'time', 'status' and 'strata' must be columns in 'data'")
    }
    if (any(duplicated(c(time, status, strata)))) {
      stop("two of the column identifiers are the same")
    }
    if(!is.factor(data[, strata])) {
      stop("'strata' must be a factor")
    }
    if (any(table(data[, strata]) == 0)) {
      warning("'strata' column has (at least) one empty level. Calculation proceeds ignoring the empty level(s)")
      data[, strata] <- factor(data[, strata])
    }
    if (length(levels(strata)) > 4) {
      stop("'strata' must have less than 5 levels")
    }
  } else{
    strata <- 1
    if (!all(c(time, status) %in% names(data))) {
      stop("'time' and 'status' must be columns in 'data'")
    }
    if (any(duplicated(c(time, status)))) {
      stop("two of the column identifiers are the same")
    }
  }
  if (!is.logical(legend)) {
    stop("'legend' must be logical")
  }
  if (legend) {
    if (is.character(legend.pos)) {
      choices.legend.pos <- tolower(c("bottomright", "bottom", "bottomleft", "left", 
                                      "topleft", "top", "topright", "right", "center"))
      legend.pos.pre <- tolower(legend.pos)
      legend.pos <- try(match.arg(legend.pos.pre, choices.legend.pos))
      if (inherits(legend.pos, "try-error")) {
        stop("'legend.pos' must be a 2-dimensional numerical vector indicating the x-y coordinates or one of: 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right' or 'center'")
      }
    } else if (!is.numeric(legend.pos) | length(legend.pos) != 2) {
      stop("'legend.pos' must be a 2-dimensional numerical vector indicating the x-y coordinates or one of: 'bottomright', 'bottom', 'bottomleft', 'left', 'topleft', 'top', 'topright', 'right' or 'center'")
    }
    if (!is.null(legend.labs) & !is.character(legend.labs)) {
      stop("'legend.labs' must be NULL or character vector")
    }
    if (!is.null(legend.title)) {
      if (!is.character(legend.title)) {
        stop("'legend.title' must be NULL or character")
      } else if (length(legend.title) != 1) {
        stop("'legend.title' must be a character, not a character vector")
      }
    }
    if (!is.numeric(legend.cex)) {
      stop("'legend.cex' must be numeric")
    } else if (length(legend.cex) != 1) {
      stop("'legend.cex' must be a number, not a numerical vector")
    }
  }
  if (!is.logical(events)) {
    stop("'events' must be logical")
  } else if (events) {
    if (strata == 1) {
      if (!is.null(events.pos)) {
        if (!is.numeric(events.pos)) {
          stop("'events.pos' must be NULL or numeric")
        } else if (length(events.pos) != 2) {
          stop("'events.pos' must be a 2-dimensional numerical vector")
        }
      }
    }
    if (!is.null(events.text)) {
      if (!is.character(events.text)) {
        stop("'events.text' must be NULL or character")
      } else if (length(events.text) != 1) {
        stop("'events.text' must be a character, not a character vector")
      }
    }
  }
  if (!is.logical(mark.time)) {
    stop("'mark.time' must be logical")
  }
  if (!is.numeric(lwd)) {
    stop("'risk' must be numeric")
  }
  if (!is.logical(risk)) {
    stop("'risk' must be logical")
  } else if (risk) {
    if (!is.logical(risk.text)) {
      stop("'risk.text' must be logical")
    } else if (risk.text) {
      if (!is.null(risk.labs) & !is.character(risk.labs)) {
        stop("'risk.labs' must be NULL or a character vector")
      }
    }
    if (!is.numeric(risk.cex)) {
      stop("'risk.cex' must be numeric")
    } else if (length(risk.cex) != 1) {
      stop("'risk.cex' must be a number, not a numerical vector")
    }
  }
  if (strata != 1) {
    if (!is.logical(p.value)) {
      stop("'p.value' must be logical")
    } else if (p.value) {
      if (!is.null(p.value.pos)) {
        if (!is.numeric(p.value.pos)) {
          stop("'p.value.pos' must be NULL or numeric")
        } else if (length(p.value.pos) != 2) {
          stop("'p.value.pos' must be a 2-dimensional numerical vector")
        }
      }
    }
  }
  choices.units <- c("days", "weeks", "months", "years")
  units.pre <- tolower(units)
  units <- try(match.arg(units.pre, choices.units))
  if (inherits(units, "try-error")) {
    stop("'units' must be one of: 'days', 'weeks', 'months', 'years'")
  }
  if (!is.null(xlab)) {
    if (!is.character(xlab)) {
      stop("'xlab' must be NULL or character")
    } else if (length(xlab) != 1) {
      stop("'xlab' must be a character, not a character vector")
    }
  }
  if (!is.null(times) & !is.numeric(times)) {
    stop("'times' must be NULL or a numerical vector")
  }
  if (!is.numeric(yscale)) {
    stop("'yscale' must be numeric")
  } else if (length(yscale) != 1) {
    stop("'yscale' must be a number, not a numerical vector")
  }
  if (!is.null(ylab)) {
    if (!is.character(ylab)) {
      stop("'ylab' must be NULL or character")
    } else if (length(ylab) != 1) {
      stop("'ylab' must be a character, not a character vector")
    }
  }
  if (!is.character(main) | length(main) != 1) {
    stop("'main' must be a character")
  }
  if (!is.null(col)) {
    if (any(!isColor(col))) {
      stop("'col' must be NULL or only contain valid colours")
    }
  }

  
  # warning checks
  if (strata != 1) {
    if (any(table(data[, strata]) == 0)) {
      warning("'strata' column has (at least) one empty level. Calculation proceeds ignoring the empty level(s)")
      data[, strata] <- factor(data[, strata])
    }
  }

  
  # stop checks
  if (legend) {
    if (!is.null(legend.labs)) {
      if (length(levels(data[, strata])) != length(legend.labs)) {
        stop("'legend.labs' must be the same length as the number of levels in 'strata'")
      }
    }
  }
  if (is.numeric(lwd)) {
    if (strata != 1 & length(lwd) != 1) {
      if(length(levels(data[, strata])) != length(lwd)) {
        stop("'lwd' must be the same length as the number of levels in 'strata'")
      }
    } else if (strata == 1 & length(lwd) != 1) {
      stop("'lwd' must be of length one, since 'strata' is set to 1")
    }
  }
  if (risk) {
    if (!is.null(risk.labs)) {
      if (strata != 1 & length(levels(data[, strata])) != length(risk.labs)) {
        stop("'risk.labs' must be the same length as the number of levels in 'strata'")
      }
    }
  }
  if (!is.null(col)) {
    if (strata != 1 & length(levels(data[, strata])) != length(col)) {
      stop("'col' must be of length one or the same length as the number of levels in 'strata'")
    } else if (strata == 1 & length(col) != 1) {
      stop("'col' must be of length one, since 'strata' is set to 1")
    }
  }

  
  # extracting necessary columns of data
  if (strata != 1) {
    df <- data.frame(time = data[, time], status = data[, status], strata = data[, strata])
  } else{
    df <- data.frame(time = data[, time], status = data[, status])
  }
  
  # defaults
  if (strata != 1 & is.null(legend.labs)){
    legend.labs <- levels(df$strata)
  }
  if (is.numeric(lwd) & strata != 1 & length(lwd) == 1) {
    lwd <- rep(lwd, length(levels(df$strata)))
  }
  if (strata != 1 & is.null(risk.labs)){
    risk.labs <- levels(df$strata)
  }
  if (is.null(events.text)) {
    if (strata != 1){
      events.text <- "events"
    } else if (strata == 1) {
      events.text <- "Events"
    }
  } 
  if (is.null(xlab)) {
    xlab <- paste0("Time (", units,")")
  }
  if (is.null(times)) {
    if (units == "days") {
      times <- seq(0, ceiling(max(df$time, na.rm = TRUE)/7)*7, 7)
    } else if (units == "weeks") {
      if(max(df$time, na.rm = TRUE) == ceiling(max(df$time, na.rm = TRUE))){
        times <- seq(0, ceiling(max(df$time, na.rm = TRUE)) + 1, 1)
      } else{
        times <- seq(0, ceiling(max(df$time, na.rm = TRUE)), 1)
      }
    } else if (units == "months") {
      times <- seq(0, ceiling(max(df$time, na.rm = TRUE)/12)*12, 12)
    } else if (units == "years") {
      if(max(df$time, na.rm = TRUE) == ceiling(max(df$time, na.rm = TRUE))){
        times <- seq(0, ceiling(max(df$time, na.rm = TRUE)) + 1, 1)
      } else{
        times <- seq(0, ceiling(max(df$time, na.rm = TRUE)), 1)
      }
    }  
  }
  if (is.null(ylab)) {
    ylab.extra <- ifelse(yscale == 1, " (proportion)",
                         ifelse(yscale == 100, " (percentage)", ""))
    ylab <- paste0("Survival", ylab.extra)
  }
  if (is.null(col)) {
    if (strata == 1) {
      col <- 1
    } else {
      col <- seq(1, length(levels(df$strata)), 1)
    }
  }

  # PLOTS
  # 2 strata
  if (length(levels(df$strata)) == 2) {
    if (risk) {
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(8, 5, 3, 2))
    } else{
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(5, 5, 3, 2))
    }
    fit <- survfit(Surv(time, status) ~ strata, data = df)
    plot(fit, col = col, xlab = xlab, ylab = ylab, 
         lwd = lwd, main = main, conf.int = FALSE, mark.time = mark.time,
         yaxs = "i", xaxs = "i", xlim = c(0, tail(times, n = 1)), axes = FALSE, ...)
    # axes
    axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)*yscale)
    axis(1, at = times)
    # legend
    if (legend) {
      if (events) {
        table.aux <- with(subset(df, !is.na(time) & !is.na(status)), table(strata, status))
        if (is.character(legend.pos)) {
          legend(legend.pos, paste0(legend.labs, " (", events.text, ": ", table.aux[, 2], ")"), 
                 col = col, lwd = lwd, lty = 1, bty = "n", 
                 title = legend.title, cex = legend.cex)
        } else if (is.numeric(legend.pos)) {
          legend(x = legend.pos[1], y = legend.pos[2], paste0(legend.labs, " (", events.text, ": ", table.aux[, 2], ")"), 
                 col = col, lwd = lwd, lty = 1, bty = "n", 
                 title = legend.title, cex = legend.cex)
        }
      } else{
        if (is.character(legend.pos)) {
          legend(legend.pos, legend.labs, col = col, lwd = lwd, lty = 1, 
                 bty = "n", title = legend.title, cex = legend.cex) 
        } else if (is.numeric(legend.pos)) {
          legend(x = legend.pos[1], y = legend.pos[2], legend.labs, col = col, lwd = lwd, lty = 1, 
                 bty = "n", title = legend.title, cex = legend.cex) 
        }
      }
    }
    # risk table
    if (risk) {
      text(-0.031*tail(times, n = 1), -0.32, "Number at risk")
      nrisk <- summary(fit, times = times, extend = TRUE)
      if (risk.text) {
        text(-0.083*tail(times, n = 1), -0.45, risk.labs[1], col = col[1], cex = risk.cex)
        text(-0.083*tail(times, n = 1), -0.54, risk.labs[2], col = col[2], cex = risk.cex)
      } else{
        segments(-0.109*tail(times, n = 1), -0.45, -0.063*tail(times, n = 1), -0.45, 
                 col = col[1], lwd = lwd[1], xpd = TRUE)
        segments(-0.109*tail(times, n = 1), -0.54, -0.063*tail(times, n = 1), -0.54, 
                 col = col[2], lwd = lwd[2], xpd = TRUE)
      }
      text(times, -0.45, nrisk$n.risk[1:length(times)], cex = risk.cex)
      text(times, -0.54, nrisk$n.risk[(length(times) + 1):(length(times)*2)], cex = risk.cex)
    }
    # p-value
    if(p.value){
      p <- survdiff(Surv(time, status) ~ strata, data = df)[["pvalue"]]
      if (!is.null(p.value.pos)) {
        text(p.value.pos[1], p.value.pos[2], 
             ifelse(p < 0.0001, TeX('$\\textit{p}<"0.0001"$'), 
                    ifelse(show.p(p) == "1.00", TeX('$\\textit{p}="1.00"$'),
                           ifelse(substr(show.p(p), nchar(show.p(p)), nchar(show.p(p))) == "0", 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '"0"$')), 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '$'))))))
      } else{
        text(tail(times, n = 1)*0.85, 0.9, 
             ifelse(p < 0.0001, TeX('$\\textit{p}<"0.0001"$'), 
                    ifelse(show.p(p) == "1.00", TeX('$\\textit{p}="1.00"$'),
                           ifelse(substr(show.p(p), nchar(show.p(p)), nchar(show.p(p))) == "0", 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '"0"$')), 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '$'))))))
      }
    }
  }
  # 3 strata
  else if(length(levels(df$strata)) == 3) {
    if (risk) {
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(9, 5, 3, 2))
    } else{
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(5, 5, 3, 2))
    }
    fit <- survfit(Surv(time, status) ~ strata, data = df)
    plot(fit, col = col, xlab = xlab, ylab = ylab, 
         lwd = lwd, main = main, conf.int = FALSE, mark.time = mark.time,
         yaxs = "i", xaxs = "i", xlim = c(0, tail(times, n = 1)), axes = FALSE, ...)
    # axes
    axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)*yscale)
    axis(1, at = times)
    # legend
    if (legend) {
      if (events) {
        table.aux <- with(subset(df, !is.na(time) & !is.na(status)), table(strata, status))
        if (is.character(legend.pos)) {
          legend(legend.pos, paste0(legend.labs, " (", events.text, ": ", table.aux[, 2], ")"), 
                 col = col, lwd = lwd, lty = 1, bty = "n", 
                 title = legend.title, cex = legend.cex)
        } else if (is.numeric(legend.pos)) {
          legend(x = legend.pos[1], y = legend.pos[2], paste0(legend.labs, " (", events.text, ": ", table.aux[, 2], ")"), 
                 col = col, lwd = lwd, lty = 1, bty = "n", 
                 title = legend.title, cex = legend.cex)
        }
      } else{
        if (is.character(legend.pos)) {
          legend(legend.pos, legend.labs, col = col, lwd = lwd, lty = 1, 
                 bty = "n", title = legend.title, cex = legend.cex) 
        } else if (is.numeric(legend.pos)) {
          legend(x = legend.pos[1], y = legend.pos[2], legend.labs, col = col, lwd = lwd, lty = 1, 
                 bty = "n", title = legend.title, cex = legend.cex) 
        }
      }
    } 
    # risk table
    if (risk) {
      text(-0.031*tail(times, n = 1), -0.34, "Number at risk")
      nrisk <- summary(fit, times = times, extend = TRUE)
      if (risk.text) {
        text(-0.083*tail(times, n = 1), -0.48, risk.labs[1], col = col[1], cex = risk.cex)
        text(-0.083*tail(times, n = 1), -0.57, risk.labs[2], col = col[2], cex = risk.cex) 
        text(-0.083*tail(times, n = 1), -0.66, risk.labs[3], col = col[3], cex = risk.cex)      
      } else {
        segments(-0.109*tail(times, n = 1), -0.48, -0.063*tail(times, n = 1), -0.48, 
                 col = col[1], lwd = lwd[1], xpd = TRUE)
        segments(-0.109*tail(times, n = 1), -0.57, -0.063*tail(times, n = 1), -0.57, 
                 col = col[2], lwd = lwd[2], xpd = TRUE)
        segments(-0.109*tail(times, n = 1), -0.66, -0.063*tail(times, n = 1), -0.66, 
                 col = col[3], lwd = lwd[3], xpd = TRUE)
      }
      text(times, -0.48, nrisk$n.risk[1:length(times)], cex = risk.cex)
      text(times, -0.57, nrisk$n.risk[(length(times) + 1):(length(times)*2)], cex = risk.cex)
      text(times, -0.66, nrisk$n.risk[(length(times)*2 + 1):(length(times)*3)], cex = risk.cex)
    }
    # p-value
    if (p.value) {
      p <- survdiff(Surv(time, status) ~ strata, data = df)[["pvalue"]]
      if (!is.null(p.value.pos)) {
        text(p.value.pos[1], p.value.pos[2], 
             ifelse(p < 0.0001, TeX('$\\textit{p}<"0.0001"$'), 
                    ifelse(show.p(p) == "1.00", TeX('$\\textit{p}="1.00"$'),
                           ifelse(substr(show.p(p), nchar(show.p(p)), nchar(show.p(p))) == "0", 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '"0"$')), 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '$'))))))
      } else{
        text(tail(times, n = 1)*0.85, 0.9, 
             ifelse(p < 0.0001, TeX('$\\textit{p}<"0.0001"$'), 
                    ifelse(show.p(p) == "1.00", TeX('$\\textit{p}="1.00"$'),
                           ifelse(substr(show.p(p), nchar(show.p(p)), nchar(show.p(p))) == "0", 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '"0"$')), 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '$'))))))
      }
    }
  }
  # 4 strata
  else if(length(levels(df$strata)) == 4) {
    if (risk) {
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(10, 5, 3, 2))
    } else{
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(5, 5, 3, 2))
    }
    fit <- survfit(Surv(time, status) ~ strata, data = df)
    plot(fit, col = col, xlab = xlab, ylab = ylab, 
         lwd = lwd, main = main, conf.int = FALSE, mark.time = mark.time,
         yaxs = "i", xaxs = "i", xlim = c(0, tail(times, n = 1)), axes = FALSE, ...)
    # axes
    axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)*yscale)
    axis(1, at = times)
    # legend
    if (legend) {
      if (events) {
        table.aux <- with(subset(df, !is.na(time) & !is.na(status)), table(strata, status))
        if (is.character(legend.pos)) {
          legend(legend.pos, paste0(legend.labs, " (", events.text, ": ", table.aux[, 2], ")"), 
                 col = col, lwd = lwd, lty = 1, bty = "n", 
                 title = legend.title, cex = legend.cex)
        } else if (is.numeric(legend.pos)) {
          legend(x = legend.pos[1], y = legend.pos[2], paste0(legend.labs, " (", events.text, ": ", table.aux[, 2], ")"), 
                 col = col, lwd = lwd, lty = 1, bty = "n", 
                 title = legend.title, cex = legend.cex)
        }
      } else{
        if (is.character(legend.pos)) {
          legend(legend.pos, legend.labs, col = col, lwd = lwd, lty = 1, 
                 bty = "n", title = legend.title, cex = legend.cex) 
        } else if (is.numeric(legend.pos)) {
          legend(x = legend.pos[1], y = legend.pos[2], legend.labs, col = col, lwd = lwd, lty = 1, 
                 bty = "n", title = legend.title, cex = legend.cex) 
        }
      }
    } 
    # risk table
    if (risk) {
      text(-0.031*tail(times, n = 1), -0.38, "Number at risk")
      nrisk <- summary(fit, times = times, extend = TRUE)
      if (risk.text) {
        text(-0.083*tail(times, n = 1), -0.51, risk.labs[1], col = col[1], cex = risk.cex)
        text(-0.083*tail(times, n = 1), -0.60, risk.labs[2], col = col[2], cex = risk.cex) 
        text(-0.083*tail(times, n = 1), -0.69, risk.labs[3], col = col[3], cex = risk.cex) 
        text(-0.083*tail(times, n = 1), -0.78, risk.labs[4], col = col[4], cex = risk.cex)
      } else{
        segments(-0.109*tail(times, n = 1), -0.51, -0.063*tail(times, n = 1), -0.51, 
                 col = col[1], lwd = lwd[1], xpd = TRUE)
        segments(-0.109*tail(times, n = 1), -0.60, -0.063*tail(times, n = 1), -0.60, 
                 col = col[2], lwd = lwd[2], xpd = TRUE)
        segments(-0.109*tail(times, n = 1), -0.69, -0.063*tail(times, n = 1), -0.69, 
                 col = col[3], lwd = lwd[3], xpd = TRUE)
        segments(-0.109*tail(times, n = 1), -0.78, -0.063*tail(times, n = 1), -0.78, 
                 col = col[4], lwd = lwd[4], xpd = TRUE)
      }
      text(times, -0.51, nrisk$n.risk[1:length(times)], cex = risk.cex)
      text(times, -0.60, nrisk$n.risk[(length(times) + 1):(length(times)*2)], cex = risk.cex)
      text(times, -0.69, nrisk$n.risk[(length(times)*2 + 1):(length(times)*3)], cex = risk.cex)
      text(times, -0.78, nrisk$n.risk[(length(times)*3 + 1):(length(times)*4)], cex = risk.cex)
    }
    # p-value
    if (p.value) {
      p <- survdiff(Surv(time, status) ~ strata, data = df)[["pvalue"]]
      if (!is.null(p.value.pos)) {
        text(p.value.pos[1], p.value.pos[2], 
             ifelse(p < 0.0001, TeX('$\\textit{p}<"0.0001"$'), 
                    ifelse(show.p(p) == "1.00", TeX('$\\textit{p}="1.00"$'),
                           ifelse(substr(show.p(p), nchar(show.p(p)), nchar(show.p(p))) == "0", 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '"0"$')), 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '$'))))))
      } else{
        text(tail(times, n = 1)*0.85, 0.9, 
             ifelse(p < 0.0001, TeX('$\\textit{p}<"0.0001"$'), 
                    ifelse(show.p(p) == "1.00", TeX('$\\textit{p}="1.00"$'),
                           ifelse(substr(show.p(p), nchar(show.p(p)), nchar(show.p(p))) == "0", 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '"0"$')), 
                                  TeX(paste0('$\\textit{p}=', show.p(p), '$'))))))
      }
    }
  }
  # no strata
  else{
    if (risk) {
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(7, 5, 3, 2))
    } else{
      par(xpd = TRUE, las = 1, font.axis = 2, font.lab = 2, cex.lab = 1.2, mar = c(5, 5, 3, 2))
    }
    fit <- survfit(Surv(time, status) ~ 1, data = df)
    plot(fit, col = col, xlab = xlab, ylab = ylab, 
         lwd = lwd, main = main, conf.int = FALSE, mark.time = mark.time,
         yaxs = "i", xaxs = "i", xlim = c(0, tail(times, n = 1)), axes = FALSE, ...)
    # axes
    axis(2, at = seq(0, 1, 0.2), labels = seq(0, 1, 0.2)*yscale)
    axis(1, at = times)
    # events
    if (events) {
      table.aux <- with(subset(df, !is.na(time) & !is.na(status)), table(status))
      if (!is.null(events.pos)) {
        text(events.pos[1], events.pos[2], paste0(events.text, ": ", table.aux[2]))
      } else{
        text(tail(times, n = 1)*0.04, 0.15, paste0(events.text, ": ", table.aux[2]), pos = 4)
      }
    }
    # risk table
    if (risk) {
      text(-0.031*tail(times, n = 1), -0.3, "Number at risk")
      nrisk <- summary(fit, times = times, extend = TRUE)
      text(times, -0.43, nrisk$n.risk[1:length(times)], cex = risk.cex)
    }
  }
  
  # reset options
  options(op.def)
}
