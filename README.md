<!-- README.md is generated from README.Rmd. Please edit that file -->
Why should I use it?
====================

This package makes using STAN easy!

How do I use it?
================

Create some fake data:
----------------------

``` r
set.seed(9782)
library(dplyr)
N <- 1000
df1 <- data.frame(
  x1 = rnorm(n = N, mean = 10, sd = 3),
  x2 = runif(n = N, min = 0, max = 10),
  c = sample(LETTERS, size = N, replace = T)
  ) %>% mutate(Tr = ifelse(c %in% c("A","E","I","O","U"), yes = 1, no = 0)) %>%
  mutate(y = 0.5*x1 + 0.75*x2 + 0.5*Tr + rnorm(N,0,1))
```

Run stan lm
-----------

``` r
library(easybayesian)
library(rstan)
lm1 <- stanlm(formula = y ~ x1 + x2 + Tr, data = df1)
```

### Regression table

``` r
regtbl(lm1, type = "html", caption = "")
```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b></b>
</th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>Rhat</b>
</th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>n\_eff</b>
</th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>Model 1</b>
</th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
x1
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
2866
</td>
<td style="padding-right: 12px; border: none;">
0.50<sup style="vertical-align: 0px;">\*</sup>
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[0.48; 0.52\]
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
x2
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
2957
</td>
<td style="padding-right: 12px; border: none;">
0.76<sup style="vertical-align: 0px;">\*</sup>
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[0.73; 0.78\]
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
Tr
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
2930
</td>
<td style="padding-right: 12px; border: none;">
0.59<sup style="vertical-align: 0px;">\*</sup>
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[0.43; 0.75\]
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
Constant
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
2914
</td>
<td style="padding-right: 12px; border: none;">
-0.11
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[-0.34; 0.13\]
</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">
N
</td>
<td style="border-top: 1px solid black;">
</td>
<td style="border-top: 1px solid black;">
</td>
<td style="border-top: 1px solid black;">
1000
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
R&\#770 log-posterior
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">
N<sub>eff</sub> log-posterior
</td>
<td style="border-bottom: 2px solid black;">
</td>
<td style="border-bottom: 2px solid black;">
</td>
<td style="border-bottom: 2px solid black;">
1519
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="5">
<span style="font-size:0.8em">\* outside the 95% credible interval.<br>Rhat is the potential scale reduction factor on split chains (at convergence, Rhat=1).<br>n\_{eff} is a crude measure of effective sample size.<br>The log posterior quantifies the combined posterior density of all model parameters.</span>
</td>
</tr>
</table>
### Goodness of fit

``` r
gof.table(lm1, caption = "My caption!", type="html")
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey;">
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Rhat
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
n\_eff
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">
x1
</td>
<td style="text-align: center;">
1.00
</td>
<td style="text-align: center;">
2866.37
</td>
</tr>
<tr>
<td style="text-align: left;">
x2
</td>
<td style="text-align: center;">
1.00
</td>
<td style="text-align: center;">
2957.42
</td>
</tr>
<tr>
<td style="text-align: left;">
Tr
</td>
<td style="text-align: center;">
1.00
</td>
<td style="text-align: center;">
2929.98
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
Constant
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
1.00
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
2914.08
</td>
</tr>
</tbody>
</table>
### Plot

``` r
p <- posteriorplot(model = lm1, parameter = "Tr", cutoff = 0.4, credibleIntervalWidth = .95)
```

![](README-unnamed-chunk-6-1.png)<!-- -->

### Interpret

``` r
interpret(model = lm1, name = "Tr", cutoff = 0)
#> [[1]]
#> [1] "There is a 95% probability that the true impact of the intervention is between 0.43 and 0.75 units."
#> 
#> [[2]]
#> [1] "There is a 100% probability that the intervention increases the outcome by 0 units or more."
```

Clustered
---------

``` r
lm1 <- stanlm(formula = y ~ x1 + x2 + Tr, cluster = "c", data = df1)
```

### Regression table

``` r
regtbl(lm1, type = "html", caption = "")
```

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
<table cellspacing="0" align="center" style="border: none;">
<tr>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b></b>
</th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>Rhat</b>
</th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>n\_eff</b>
</th>
<th style="text-align: left; border-top: 2px solid black; border-bottom: 1px solid black; padding-right: 12px;">
<b>Model 1</b>
</th>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
x1
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
4000
</td>
<td style="padding-right: 12px; border: none;">
0.50<sup style="vertical-align: 0px;">\*</sup>
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[0.48; 0.52\]
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
x2
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
4000
</td>
<td style="padding-right: 12px; border: none;">
0.76<sup style="vertical-align: 0px;">\*</sup>
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[0.73; 0.78\]
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
Tr
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
3159
</td>
<td style="padding-right: 12px; border: none;">
0.59<sup style="vertical-align: 0px;">\*</sup>
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[0.42; 0.76\]
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
Constant
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
<td style="padding-right: 12px; border: none;">
4000
</td>
<td style="padding-right: 12px; border: none;">
-0.11
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
\[-0.35; 0.13\]
</td>
</tr>
<tr>
<td style="border-top: 1px solid black;">
N
</td>
<td style="border-top: 1px solid black;">
</td>
<td style="border-top: 1px solid black;">
</td>
<td style="border-top: 1px solid black;">
1000
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
Clusters
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
26
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;">
R&\#770 log-posterior
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
</td>
<td style="padding-right: 12px; border: none;">
1.00
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid black;">
N<sub>eff</sub> log-posterior
</td>
<td style="border-bottom: 2px solid black;">
</td>
<td style="border-bottom: 2px solid black;">
</td>
<td style="border-bottom: 2px solid black;">
1302
</td>
</tr>
<tr>
<td style="padding-right: 12px; border: none;" colspan="5">
<span style="font-size:0.8em">\* outside the 95% credible interval.<br>Rhat is the potential scale reduction factor on split chains (at convergence, Rhat=1).<br>n\_{eff} is a crude measure of effective sample size.<br>The log posterior quantifies the combined posterior density of all model parameters.</span>
</td>
</tr>
</table>
### Goodness of fit

``` r
gof.table(lm1, caption = "My caption!", type="html")
```

<table class="gmisc_table" style="border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;">
<thead>
<tr>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey;">
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
Rhat
</th>
<th style="border-bottom: 1px solid grey; border-top: 2px solid grey; text-align: center;">
n\_eff
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">
x1
</td>
<td style="text-align: center;">
1.00
</td>
<td style="text-align: center;">
4000.00
</td>
</tr>
<tr>
<td style="text-align: left;">
x2
</td>
<td style="text-align: center;">
1.00
</td>
<td style="text-align: center;">
4000.00
</td>
</tr>
<tr>
<td style="text-align: left;">
Tr
</td>
<td style="text-align: center;">
1.00
</td>
<td style="text-align: center;">
3158.52
</td>
</tr>
<tr>
<td style="border-bottom: 2px solid grey; text-align: left;">
Constant
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
1.00
</td>
<td style="border-bottom: 2px solid grey; text-align: center;">
4000.00
</td>
</tr>
</tbody>
</table>
### Plot

``` r
p <- posteriorplot(model = lm1, parameter = "Tr", cutoff = 0.4, credibleIntervalWidth = .95)
```

![](README-unnamed-chunk-11-1.png)<!-- -->

### Interpret

``` r
interpret(model = lm1, name = "Tr", cutoff = 0)
#> [[1]]
#> [1] "There is a 95% probability that the true impact of the intervention is between 0.42 and 0.76 units."
#> 
#> [[2]]
#> [1] "There is a 100% probability that the intervention increases the outcome by 0 units or more."
```

Dashboard
=========

    bayesiandashboard()

![Bayesian Dashboard](dashboard.png)

How do I get it?
================

For now, this package is not public. But if you are here, I probably want to share it with you.

To install the package you can run `devtools::install_github(repo = 'ignacio82/easybayesian', auth_token = '40748ac7538e4b47244a58cf9b1479d48e7bd531')`
