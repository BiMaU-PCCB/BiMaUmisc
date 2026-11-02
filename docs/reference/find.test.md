# Automatically select statistical tests for quantitative variables

This function evaluates quantitative variables across stratification
groups and automatically determines the appropriate parametric or
non-parametric hypothesis test. See **Details** for an exhaustive
explanation of the criteria followed. Designed to work seamlessly with
[`tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html),
this function generates the input lists for arguments `test` and
`test.args` of the function
[`add_p`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html),
specifying the recommended test function and any additional arguments
different from the defaults.

## Usage

``` r
find.test(data, variables, strata, alpha = 0.05)
```

## Arguments

- data:

  name of the dataset, of class `data.frame`, containing at least two
  columns (quantitative variable, strata).

- variables:

  column name(s) of the quantitative variable(s) to analyse. Can be
  specified as a character string (with quotes) or as an unquoted
  symbol.

- strata:

  strata indicator column name. Can be specified as a character string
  (with quotes) or as an unquoted symbol. Must be a factor with at least
  2 levels.

- alpha:

  significance level threshold used for normality
  ([`shapiro.test`](https://rdrr.io/r/stats/shapiro.test.html)) and
  homoscedasticity ([`var.test`](https://rdrr.io/r/stats/var.test.html)
  or [`bartlett.test`](https://rdrr.io/r/stats/bartlett.test.html))
  testing.  
  The default value is 0.05.

## Value

A `list` of length 2 intended to be passed directly to arguments `test`
and `test.args` of the function
[`add_p`](https://www.danieldsjoberg.com/gtsummary/reference/add_p.html)
when using
[`tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html):

- `test`:

  a list of formulas matching each variable name to its selected test
  string (e.g., `age ~ "t.test"`).

- `test.args`:

  a list of formulas specifying additional test parameters (e.g.,
  `age ~ list(var.equal = TRUE)`). Variables requiring no additional
  parameters are omitted from this second list.

## Details

If any level of `strata` has less than five non-missing observations, no
statistical test is performed for that variable, and a warning is
issued. Otherwise, the appropriate test is selected based on the
following criteria:

**Two Groups (`strata` with 2 levels):**

- ***t*-test** ([`t.test`](https://rdrr.io/r/stats/t.test.html)):
  selected if sample sizes in both groups are \\\ge 30\\ and
  Shapiro-Wilk tests indicate normality in both groups. Variance
  equality is tested using an *F*-test.

- **Mann-Whitney test**
  ([`wilcox.test`](https://rdrr.io/r/stats/wilcox.test.html)): selected
  if sample sizes are small, this is \\\< 30\\ in some group, or if
  normality is rejected in either group.

**More than Two Groups (`strata` with \> 2 levels):**

- **ANOVA** ([`oneway.test`](https://rdrr.io/r/stats/oneway.test.html)):
  selected if ANOVA model residuals adhere to a normal distribution
  according to the Shapiro-Wilk test. Homoscedasticity is tested via
  Bartlett's test.

- **Kruskal-Wallis test**
  ([`kruskal.test`](https://rdrr.io/r/stats/kruskal.test.html)):
  selected if ANOVA residuals deviate from normality.

## Examples

``` r
df <- as.data.frame(gtsummary::trial)
df$trt <- factor(df$trt)
df$grade <- factor(df$grade)

# Two levels variable (trt: Drug A, Drug B)
tests2 <- find.test(df, variables = c(age, marker), strata = trt)
tests2

# Use outputs directly with tbl_summary()
gtsummary::tbl_summary(df, include = c(age, marker), by = trt) |>
  gtsummary::add_p(test = tests2$test, test.args = tests2$test.args)

# Three levels variable (grade: I, II, III)
tests3 <- find.test(df, variables = c(age, marker), strata = grade)
tests3

# Use outputs directly with tbl_summary()
gtsummary::tbl_summary(df, include = c(age, marker), by = grade) |>
  gtsummary::add_p(test = tests3$test, test.args = tests3$test.args)
  
# Using function show.p() from BiMaUmisc package
gtsummary::tbl_summary(df, include = c(age, marker), by = grade) |>
  gtsummary::add_p(test = tests3$test, test.args = tests3$test.args,
                   pvalue_fun = function(x) show.p(x))
```
