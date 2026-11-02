# Format *p*-values for display

Convert *p*-values to a compact textual representation. The function
prints decimals up to the first non-zero decimal and the following one,
applying standard rounding.

## Usage

``` r
show.p(p, add.p = FALSE)
```

## Arguments

- p:

  a value or vector of the *p*-value(s) to be shown, where \\0\<p\<1\\.

- add.p:

  logical indicating whether the text *p*= (or *p*\<, if it applies)
  should be added before the *p*-value.  
  The default value is `FALSE`.

## Value

A character vector with the formatted *p*-values.

## Details

The number of decimals shown depends on the magnitude of the *p*-value
so that two meaningful digits are displayed after the leading zeros.

Specifically:

- For \\p \ge 0.1\\, two decimal places are shown.

- For \\0.01 \le p \< 0.1\\, three decimal places are shown.

- For \\0.001 \le p \< 0.01\\, four decimal places are shown.

- For \\0.0001 \le p \< 0.001\\, five decimal places are shown.

- For \\p \< 0.0001\\, the string `"<0.0001"` is returned.

This formatting ensures that the first non-zero decimal of the
\\p\\-value and the following digit are displayed, while avoiding an
excessive number of leading zeros.

If `add.p = TRUE`, the prefix `"*p*="` (or `"*p*<"` when applicable) is
added to the formatted value. This will ensure the *p* appears in
cursive when calling the function in line on R markdown documents.

## Examples

``` r
show.p(0.00785)
show.p(c(0.03042, 0.1579, 0.0000025))
show.p(0.00785, add.p = TRUE)
show.p(c(0.03042, 0.1579, 0.0000025), add.p = TRUE)
```
