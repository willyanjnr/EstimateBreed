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

#Obtain environmental deviations
data("desvamb")
with(desvamb,desv_clim(ENV,TMED,PREC))

#Get the ISGR
data("genot")
with(genot, isgr(GEN, ENV, NG, MG, CICLO))
```
Predict ∆T to determine the ideal times to apply agricultural pesticides.
``` r
library("EstimateBreed")
deltat(-53.91472,-28.38778,type=1,days=10)
View(forecast)
```

## Documentation
Complete documentation can be found when using the package within R

### Citing
When citing this package, please use,
``` r
library("EstimateBreed")
citation("EstimateBreed")
```

## References
