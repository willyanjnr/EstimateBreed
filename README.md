# EstimateBreed

EstimateBreed is an R package designed to perform analyses and estimate environmental covariates and genetic parameters related to selection strategies and the development of superior genotypes. The package offers two main functionalities: 
- **Prediction models** for environmental covariates and processes.
- **Estimation of genetic parameters** and selection strategies for developing superior genotypes.

# Installation

## GitHub Installation

To install the latest version from GitHub, use the following command:

```r
devtools::install_github("willyanjnr/EstimateBreed")
```

### Example
Obtain the genetic selection index for resilience (ISGR) for selecting genotypes for environmental stressors, as described by Bandeira et al. (2024).

``` r
library("EstimateBreed")
data("desvamb")
#Obtain environmental deviations
with(desvamb,desv_clim(ENV,TMED,PREC))
```
# A tibble: 3 × 5
  ENV   STMED TMEDR SPREC PRECIR
  <chr> <dbl> <dbl> <dbl>  <dbl>
1 E1     2.65  24.8  5.46   339.
2 E2     3.65  23.8  5.27   344.
3 E3     2.81  24.5  5.47   362.

``` r
#Get the ISGR
data("genot")
with(genot, isgr(GEN, ENV, NG, MG, CICLO))
```
    Gen Env      ISGR
26 L454  E1  6.489941
22 L455  E1  7.084315
19 L541  E1  7.653157
18 L367  E1  7.862185
16 L380  E1  8.329434
12 L393  E1  9.638909
10 L439  E1 10.552056
28 L298  E3 12.209433
30 L358  E2 23.347984
29 L346  E2 23.793351
27 L195  E2 24.719927
25 L179  E2 25.747317
24 L359  E2 26.300686
23 L345  E2 26.886419
1  L445  E1 27.255375
21 L185  E2 28.211433
20 L310  E2 28.942165
17 L178  E2 31.418785
15 L261  E2 33.424611
14 L269  E2 34.605133
13 L209  E2 35.959423
11 L263  E2 39.127798
9  L201  E2 43.145922
8  L299  E2 45.686042
7  L152  E2 48.926278
6   L26  E2 52.988109
5  L166  E2 57.596139
4  L155  E2 64.251152
3  L277  E2 74.756384
2  L162  E2 86.543916
