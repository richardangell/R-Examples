Deviance-Formulas
================

This notebook shows the deviance formula for several different distributions.

Load data
=========

Load the dataCar dataset from the insuranceData package. This will provide a dataset we can use to build both a poisson and gamma glm.

``` r
library(insuranceData)
data(dataCar)
```

``` r
head(dataCar)
```

    ##   veh_value  exposure clm numclaims claimcst0 veh_body veh_age gender area
    ## 1      1.06 0.3039014   0         0         0    HBACK       3      F    C
    ## 2      1.03 0.6488706   0         0         0    HBACK       2      F    A
    ## 3      3.26 0.5694730   0         0         0      UTE       2      F    E
    ## 4      4.14 0.3175907   0         0         0    STNWG       2      F    D
    ## 5      0.72 0.6488706   0         0         0    HBACK       4      F    C
    ## 6      2.01 0.8542094   0         0         0    HDTOP       3      M    C
    ##   agecat            X_OBSTAT_
    ## 1      2 01101    0    0    0
    ## 2      4 01101    0    0    0
    ## 3      2 01101    0    0    0
    ## 4      2 01101    0    0    0
    ## 5      2 01101    0    0    0
    ## 6      4 01101    0    0    0

Poisson deviance
================

Add the log of the exposure (period of cover) to the data, so it can be used as an offset in a poisson glm to model claims frequency (i.e. \# claims per exposure).

``` r
dataCar$log_exposure <- log(dataCar$exposure)
```

Claims frequency model
----------------------

This models number of claims per exposure, where exposure is different for each records.

``` r
frequency_model <- glm(formula = numclaims ~ veh_body + veh_age + gender + area + agecat + offset(log_exposure),
                       family = poisson(link = log),
                       data = dataCar)
```

``` r
frequency_model
```

    ## 
    ## Call:  glm(formula = numclaims ~ veh_body + veh_age + gender + area + 
    ##     agecat + offset(log_exposure), family = poisson(link = log), 
    ##     data = dataCar)
    ## 
    ## Coefficients:
    ##   (Intercept)  veh_bodyCONVT  veh_bodyCOUPE  veh_bodyHBACK  veh_bodyHDTOP  
    ##     -0.439650      -1.524253      -0.501581      -0.983037      -0.817585  
    ## veh_bodyMCARA  veh_bodyMIBUS  veh_bodyPANVN  veh_bodyRDSTR  veh_bodySEDAN  
    ##     -0.324127      -0.975043      -0.847742      -0.517624      -0.921474  
    ## veh_bodySTNWG  veh_bodyTRUCK    veh_bodyUTE        veh_age        genderM  
    ##     -0.886901      -0.932670      -1.104585      -0.066304      -0.023585  
    ##         areaB          areaC          areaD          areaE          areaF  
    ##      0.048340       0.004077      -0.112883      -0.032222       0.064836  
    ##        agecat  
    ##     -0.089366  
    ## 
    ## Degrees of Freedom: 67855 Total (i.e. Null);  67835 Residual
    ## Null Deviance:       25510 
    ## Residual Deviance: 25350     AIC: 34830

Claims models
-------------

This model predict number of claims, note each record has the same exposure.

``` r
claims_model <- glm(formula = numclaims ~ veh_body + veh_age + gender + area + agecat,
                    family = poisson(link = log),
                    data = dataCar)
```

``` r
claims_model
```

    ## 
    ## Call:  glm(formula = numclaims ~ veh_body + veh_age + gender + area + 
    ##     agecat, family = poisson(link = log), data = dataCar)
    ## 
    ## Coefficients:
    ##   (Intercept)  veh_bodyCONVT  veh_bodyCOUPE  veh_bodyHBACK  veh_bodyHDTOP  
    ##     -1.194692      -1.758355      -0.762189      -1.092879      -0.881058  
    ## veh_bodyMCARA  veh_bodyMIBUS  veh_bodyPANVN  veh_bodyRDSTR  veh_bodySEDAN  
    ##     -0.452441      -1.164573      -0.826061      -0.704466      -1.034299  
    ## veh_bodySTNWG  veh_bodyTRUCK    veh_bodyUTE        veh_age        genderM  
    ##     -1.000065      -1.028627      -1.246010      -0.043588      -0.010619  
    ##         areaB          areaC          areaD          areaE          areaF  
    ##      0.059106       0.005371      -0.112508      -0.028193       0.108110  
    ##        agecat  
    ##     -0.078327  
    ## 
    ## Degrees of Freedom: 67855 Total (i.e. Null);  67835 Residual
    ## Null Deviance:       26770 
    ## Residual Deviance: 26640     AIC: 36110

Poisson deviance formula
------------------------

Deviance formulas can be found here; <http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html>. Note that h2o multiplies the deviance by -1.

``` r
poisson_deviance <- function(y, p) {
  
  d <- 2 * ifelse(y == 0, -(y - p), (y * log(y / p)) - (y - p))
  
  dev <- sum(d)
  
  return(dev)
  
}
```

Compare to results from glm()
-----------------------------

### Claims frequecny model

``` r
poisson_deviance(dataCar$numclaims, frequency_model$fitted.values)
```

    ## [1] 25350.45

``` r
frequency_model$deviance
```

    ## [1] 25350.45

``` r
poisson_deviance(dataCar$numclaims, 
                 dataCar$exposure * sum(dataCar$numclaims) / sum(dataCar$exposure))
```

    ## [1] 25506.97

``` r
frequency_model$null.deviance
```

    ## [1] 25506.97

### Claims count model

``` r
poisson_deviance(dataCar$numclaims, claims_model$fitted.values)
```

    ## [1] 26636.71

``` r
claims_model$deviance
```

    ## [1] 26636.71

``` r
poisson_deviance(dataCar$numclaims, 
                 sum(dataCar$numclaims) / length(dataCar$numclaims))
```

    ## [1] 26768.3

``` r
claims_model$null.deviance
```

    ## [1] 26768.3

Gamma deviance
==============

Add the average claims cost that can be used as the response in a claims cost model.

``` r
dataCar$average_claimcst0 <- dataCar$claimcst0 / dataCar$numclaims
```

Claims severity model
---------------------

This models cost (severity) of a claim.

``` r
severity_model <- glm(formula = average_claimcst0 ~ veh_body + veh_age + gender + area + agecat,
                      family = Gamma(link = log),
                      data = dataCar,
                      weights = dataCar$numclaims)
```

``` r
severity_model
```

    ## 
    ## Call:  glm(formula = average_claimcst0 ~ veh_body + veh_age + gender + 
    ##     area + agecat, family = Gamma(link = log), data = dataCar, 
    ##     weights = dataCar$numclaims)
    ## 
    ## Coefficients:
    ##   (Intercept)  veh_bodyCONVT  veh_bodyCOUPE  veh_bodyHBACK  veh_bodyHDTOP  
    ##      6.953871       0.864427       0.762986       0.594349       0.503854  
    ## veh_bodyMCARA  veh_bodyMIBUS  veh_bodyPANVN  veh_bodyRDSTR  veh_bodySEDAN  
    ##     -0.605744       0.784046       0.503019      -0.797992       0.437856  
    ## veh_bodySTNWG  veh_bodyTRUCK    veh_bodyUTE        veh_age        genderM  
    ##      0.441515       0.625090       0.514363       0.048211       0.194434  
    ##         areaB          areaC          areaD          areaE          areaF  
    ##     -0.021885       0.093501       0.002243       0.170740       0.390080  
    ##        agecat  
    ##     -0.060173  
    ## 
    ## Degrees of Freedom: 4623 Total (i.e. Null);  4603 Residual
    ##   (63232 observations deleted due to missingness)
    ## Null Deviance:       7620 
    ## Residual Deviance: 7418  AIC: 84090

Gamma deviance formula
----------------------

Deviance formulas can be found here; <http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html>.

``` r
gamma_deviance <- function(y, p, w) {
  
  d <- 2 * w * (-log(y / p) + ((y - p) / p))

  dev <- sum(d)
  
  return(dev)
  
}
```

Compare to results from glm()
-----------------------------

Note the model excludes records with no claims so these are excluded when calculating the deviance.

``` r
clms <- dataCar$numclaims > 0
```

``` r
gamma_deviance(y = dataCar$average_claimcst0[clms], 
               p = severity_model$fitted.values,
               w = dataCar$numclaims[clms])
```

    ## [1] 7417.677

``` r
severity_model$deviance
```

    ## [1] 7417.677

``` r
gamma_deviance(y = dataCar$average_claimcst0[clms], 
               p = sum(dataCar$average_claimcst0[clms] * dataCar$numclaims[clms]) / sum(dataCar$numclaims[clms]),
               w = dataCar$numclaims[clms])
```

    ## [1] 7619.597

``` r
severity_model$null.deviance
```

    ## [1] 7619.597

Package info
============

``` r
sessionInfo()
```

    ## R version 3.4.4 (2018-03-15)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] insuranceData_1.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.4.4  backports_1.1.2 magrittr_1.5    rprojroot_1.3-2
    ##  [5] tools_3.4.4     htmltools_0.3.6 yaml_2.1.16     Rcpp_0.12.15   
    ##  [9] stringi_1.1.6   rmarkdown_1.8   knitr_1.18      stringr_1.2.0  
    ## [13] digest_0.6.15   evaluate_0.10.1
