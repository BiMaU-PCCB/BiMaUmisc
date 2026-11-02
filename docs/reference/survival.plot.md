# Plot survival curves

This function produces an elegant survival curve plot. It can
automatically append the number-at-risk table and display the log-tank
test *p*-value (obtained from a call to
[`survdiff`](https://rdrr.io/pkg/survival/man/survdiff.html)). Highly
customizable, it offers numerous arguments to easily modify the plot's
appearance.

## Usage

``` r
survival.plot(data, time, status, strata = 1,
              legend = TRUE, legend.pos = "bottomleft", legend.labs = NULL,
              legend.title = NULL, legend.cex = 1,
              events = FALSE, events.pos = NULL, events.text = NULL,
              mark.time = TRUE, lwd = 3, 
              risk = TRUE, risk.text = TRUE, risk.labs = NULL, risk.cex = 1, 
              p.value = TRUE, p.value.pos = NULL,
              units = "days", xlab = NULL, times = NULL,
              yscale = 1, ylab = NULL, 
              main = "Survival curve", col = NULL, ...)
```

## Arguments

- data:

  name of the dataset, of class `data.frame`, containing at least 2
  columns (follow up time, status indicator).

- time:

  follow-up time column name. Can be specified as a character string
  (with quotes) or as an unquoted symbol.

- status:

  status indicator column name. Can be specified as a character string
  (with quotes) or as an unquoted symbol.

- strata:

  strata indicator column name. Can be specified as a character string
  (with quotes) or as an unquoted symbol. Up to four categories. Must be
  a factor.  
  The default value is 1, this is, there are no groups.

- legend:

  logical indicating whether a legend should be added to the plot. If
  `strata` is set to 1, this argument is ignored.  
  The default value is `TRUE`.

- legend.pos:

  this argument can be a single keyword from the list in
  [`legend`](https://rdrr.io/r/graphics/legend.html), such as
  `"bottomleft"` or `"topright"`, which places the legend on the inside
  of the plot frame at the given location. In this case, this argument
  is passed to [`match.arg`](https://rdrr.io/r/base/match.arg.html).
  Otherwise, a 2-dimensional numerical vector indicating the x-y
  coordinates can be indicated. If `legend` is set to `FALSE`, this
  argument is ignored.  
  The default value is `"bottomleft"`.

- legend.labs:

  a character vector of length \\\geq\\2 to appear as the legend labels.
  If `legend` is set to `FALSE`, this argument is ignored.  
  The default value, `NULL`, indicates that the levels of `strata` are
  to be used as the legend labels.

- legend.title:

  title for the legend. If `legend` is set to `FALSE`, this argument is
  ignored.  
  The default value, `NULL`, indicates on title should be added.

- legend.cex:

  legend's character expansion factor. A numerical value giving the
  amount by which the legend text should be magnified relative to the
  default. If `legend` is set to `FALSE`, this argument is ignored.  
  The default value is 1.

- events:

  logical indicating whether the number of events should be added to the
  text in the legend. If `strata` is set to 1, the total number of
  events is added directly to the plot instead. If `legend` is set to
  `FALSE` and `strata` is not 1, this argument is ignored.  
  The default value is `FALSE`.

- events.pos:

  2-dimensional numerical vector indicating the x-y coordinates where
  the number of events should be added to the plot. If `strata` is not 1
  or `events` is set to `FALSE`, this argument is ignored.  
  The default value, `NULL`, indicates that the text should be placed on
  the bottom-left corner.

- events.text:

  a character indicating the text to display before the number of events
  in the plot. If `events` is set to `FALSE`, this argument is
  ignored.  
  The default value, `NULL`, indicates that the text "Events" should be
  added when `strata` is set to 1 and the text "events" should be added
  when `strata` is not set to 1.

- mark.time:

  logical indicating whether the curves should be marked at each
  censoring time.  
  The default value is `TRUE`.

- lwd:

  a number indicating the line width(s). Alternatively, when `strata` is
  not set to 1, an \\n\\-dimensional numerical vector can be indicated,
  where \\n\\ is the number of levels in `strata`.  
  The default value is 3.

- risk:

  logical indicating whether the number-at-risk table should be appended
  to the plot.  
  The default value is `TRUE`.

- risk.text:

  logical indicating whether the legend for the number-at-risk table
  should display text labels. When `FALSE`, line segments are used
  instead. If `risk` is set to `FALSE` or `strata` is set to 1, this
  argument is ignored.  
  The default value is `TRUE`.

- risk.labs:

  \\n\\-dimensional character vector, where \\n\\ is the number of
  levels in `strata`, indicating the text labels in the number-at-risk
  table. If `risk.text` is set to `FALSE` or `strata` is set to 1, this
  argument is ignored.  
  The default value, `NULL`, indicates that the levels of `strata`
  should be used.

- risk.cex:

  number-at-risk table's character expansion factor. A numerical value
  giving the amount by which the number-at-risk table text should be
  magnified relative to the default. If `risk` is set to `FALSE`, this
  argument is ignored.  
  The default value is 1.

- p.value:

  logical indicating whether the *p*-value obtained from a call to
  [`survdiff`](https://rdrr.io/pkg/survival/man/survdiff.html) with
  `rho` equal to 0, this is the log-rank or Mantel-Haenszel test, should
  be displayed in the plot. The *p*-value will be displayed in the
  format specified by the
  [`show.p`](https://bimau-pccb.github.io/BiMaUmisc/reference/show.p.md)
  function. If `strata` is set to 1, this argument is ignored.  
  The default value is `TRUE`.

- p.value.pos:

  2-dimensional numerical vector indicating the x-y coordinates where
  the number of events should be added to the plot. If `strata` is not 1
  or `events` is set to `FALSE`, this argument is ignored.  
  The default value, `NULL`, indicates that the text should be placed on
  the upper-right corner.

- units:

  character indicating the time units. Options are: "days", "weeks",
  "months" or "years". This argument will be used to create the default
  label and tick-marks for the x-axis. This argument is not
  case-sensitive and is passed to
  [`match.arg`](https://rdrr.io/r/base/match.arg.html).  
  The default value is `"days"`.

- xlab:

  character indicating a label for the x-axis.  
  The default value, `NULL`, indicates that indicates that the text
  "Time (units)" should be used, where "units" is specified by the
  `units` argument.

- times:

  numerical vector with the time points at which tick-marks are to be
  drawn in the x-axis.  
  The default value, `NULL`, indicates that a time vector will be
  automatically constructed based on the `units` argument:

  - If `units` is set to "days": from 0 to the maximum survival time
    rounded up to the nearest multiple of 7.

  - If `units` is set to "weeks": from 0 to the maximum survival time
    rounded up to the nearest integer.

  - If `units` is set to "months": from 0 to the maximum survival time
    rounded up to the nearest multiple of 12.

  - If `units` is set to "years": from 0 to the maximum survival time
    rounded up to the nearest integer.

- yscale:

  a numeric value used to multiply the labels on the y-axis. A value of
  100, for instance, would be used to give a percent scale. Notice only
  the labels are changed, not the actual plot coordinates.  
  The default value is 1.

- ylab:

  character indicating a label for the y-axis.  
  The default value, `NULL`, indicates that indicates that the text
  "Survival" should be used. Moreover, if `yscale` is set to 1 (or 100)
  the text "(proportion)" or "(percentage)" will also be added.

- main:

  overall title for the plot.  
  The default value is `"Survival curve"`.

- col:

  colour palette to be used.  
  The default value, `NULL`, indicates that a different colour should be
  used per level in `strata`. This is, a vector from 1 to \\n\\, where
  \\n\\ is the number of levels in `strata`, will be used.

- ...:

  other graphical parameters (to be passed to
  [`plot.survfit`](https://rdrr.io/pkg/survival/man/plot.survfit.html)).

## Value

A customized survival curve plot displaying the requested graphical and
statistical modifications.

## Note

A call to [`par`](https://rdrr.io/r/graphics/par.html) is used in this
function. Notice that the arguments `font.axis`, `font.lab`, `cex.lab`,
`las` and `xpd` are always set to 2, 2, 1.2, 1, and `TRUE`,
respectively. Moreover, the argument `mar` is always modified and
depends on the number of levels in strata as well as whether or not the
number-at-risk table is appended.  
For optimal resolution and layout alignment when using the function in
an R file, adjust the plot window. For example, ideal dimensions are
approximately 7 x 5” for 24” screens and 5.9 x 4.2” for 13” screens.  
For optimal resolution and layout alignment when using the function in
an R markdown file or similar, use `fig.dim = c(7, 5)` in the
corresponding chunk.

## Examples

``` r
# CAUTION: Before running through the Console, read Note to ensure proper 
# visualization (how to modify dimensions of the Plot window).
lung <- survival::cancer
lung$time_y <- lung$time/365.25
lung$sex <- factor(lung$sex, levels = 1:2, labels = c("Male", "Female"))

# no strata
survival.plot(lung, time_y, status, units = "y")
# customizing
survival.plot(lung, time_y, status, units = "y",
              xlab = "Time since diagnosis (in years)",
              yscale = 100,
              main = "Overall survival", mark.col = "darkgray")

# 2 strata
survival.plot(lung, time_y, status, sex, units = "y")
# customizing
survival.plot(lung, time_y, status, sex, 
              legend.pos = "topright", legend.title = "Sex", 
              events = TRUE, risk.text = FALSE, p.value.pos = c(2.4, 0.4),
              units = "y", xlab = "Time since diagnosis (in years)", 
              main = "Overall survival", col = c("darkviolet", "darkgreen"))
```
