Glm-Important-Variables
================

This notebook shows how standardising variables when building regression models results in **standardised coefficients** which are independant of the scales of the predictors. This allows the use of regression coefficients as a general measure of variable importance in the model.

Start h2o
---------

``` r
suppressMessages(library(h2o))
h2o.init()
```

    ##  Connection successful!
    ## 
    ## R is connected to the H2O cluster: 
    ##     H2O cluster uptime:         16 minutes 13 seconds 
    ##     H2O cluster version:        3.16.0.2 
    ##     H2O cluster version age:    2 months and 3 days  
    ##     H2O cluster name:           H2O_started_from_R_richardangell_nus717 
    ##     H2O cluster total nodes:    1 
    ##     H2O cluster total memory:   3.39 GB 
    ##     H2O cluster total cores:    4 
    ##     H2O cluster allowed cores:  4 
    ##     H2O cluster healthy:        TRUE 
    ##     H2O Connection ip:          localhost 
    ##     H2O Connection port:        54321 
    ##     H2O Connection proxy:       NA 
    ##     H2O Internal Security:      FALSE 
    ##     H2O API Extensions:         XGBoost, Algos, AutoML, Core V3, Core V4 
    ##     R Version:                  R version 3.4.3 (2017-11-30)

``` r
h2o.no_progress()
```

Upload data to h2o
------------------

First upload the prostate dataset from the h2o package to the h2o instance.

``` r
prostate.hex = h2o.uploadFile(path = system.file("extdata", "prostate.csv",
                                                 package = "h2o"),
                               destination_frame = "prostate.hex")
```

Add another age column where the units are in days. We will build a model using each age variable.

``` r
prostate.hex["AGE_DAYS"] = prostate.hex["AGE"] * 365
prostate.hex
```

    ##   ID CAPSULE AGE RACE DPROS DCAPS  PSA  VOL GLEASON AGE_DAYS
    ## 1  1       0  65    1     2     1  1.4  0.0       6    23725
    ## 2  2       0  72    1     3     2  6.7  0.0       7    26280
    ## 3  3       0  70    1     1     2  4.9  0.0       6    25550
    ## 4  4       0  76    2     2     1 51.2 20.0       7    27740
    ## 5  5       0  69    1     1     1 12.3 55.9       6    25185
    ## 6  6       1  71    1     3     2  3.3  0.0       8    25915
    ## 
    ## [380 rows x 10 columns]

Build two penalised regression model
------------------------------------

We are specifying the `standardize = TRUE` option to standardise numeric variables to have zero mean and unit variance (note it is set to `TRUE` by default). See [h2o docs](http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/standardize.html) for more info.

Build the first model using age in years.

``` r
explanatory_cols <- c("AGE", "RACE", "DPROS", "DCAPS", "PSA", "VOL", "GLEASON")
h2o_glm <- h2o.glm(x = explanatory_cols,
                   y = "CAPSULE",
                   training_frame = prostate.hex,
                   lambda_search = TRUE,
                   alpha = 1,
                   standardize = TRUE)
```

``` r
h2o_glm
```

    ## Model Details:
    ## ==============
    ## 
    ## H2ORegressionModel: glm
    ## Model ID:  GLM_model_R_1517605758093_11 
    ## GLM Model: summary
    ##     family     link             regularization
    ## 1 gaussian identity Lasso (lambda = 0.002804 )
    ##                                                                  lambda_search
    ## 1 nlambda = 100, lambda.max = 0.2222, lambda.min = 0.002804, lambda.1se = -1.0
    ##   number_of_predictors_total number_of_active_predictors
    ## 1                          7                           7
    ##   number_of_iterations  training_frame
    ## 1                   48 RTMP_sid_b8d1_2
    ## 
    ## Coefficients: glm coefficients
    ##       names coefficients standardized_coefficients
    ## 1 Intercept    -0.684516                  0.402632
    ## 2       AGE    -0.001454                 -0.009493
    ## 3      RACE    -0.072633                 -0.022427
    ## 4     DPROS     0.090846                  0.090856
    ## 5     DCAPS     0.092834                  0.028839
    ## 6       PSA     0.003604                  0.072066
    ## 7       VOL    -0.001897                 -0.034814
    ## 8   GLEASON     0.145272                  0.158630
    ## 
    ## H2ORegressionMetrics: glm
    ## ** Reported on training data. **
    ## 
    ## MSE:  0.1720519
    ## RMSE:  0.4147914
    ## MAE:  0.3622157
    ## RMSLE:  0.2990596
    ## Mean Residual Deviance :  0.1720519
    ## R^2 :  0.284665
    ## Null Deviance :91.39737
    ## Null D.o.F. :379
    ## Residual Deviance :65.37974
    ## Residual D.o.F. :372
    ## AIC :427.6089

Now build the model using age in days.

``` r
explanatory_cols <- c("AGE_DAYS", "RACE", "DPROS", "DCAPS", "PSA", "VOL", "GLEASON")
h2o_glm2 <- h2o.glm(x = explanatory_cols,
                    y = "CAPSULE",
                    training_frame = prostate.hex,
                    lambda_search = TRUE,
                    alpha = 1,
                    standardize = TRUE)
```

``` r
h2o_glm2
```

    ## Model Details:
    ## ==============
    ## 
    ## H2ORegressionModel: glm
    ## Model ID:  GLM_model_R_1517605758093_12 
    ## GLM Model: summary
    ##     family     link             regularization
    ## 1 gaussian identity Lasso (lambda = 0.002804 )
    ##                                                                  lambda_search
    ## 1 nlambda = 100, lambda.max = 0.2222, lambda.min = 0.002804, lambda.1se = -1.0
    ##   number_of_predictors_total number_of_active_predictors
    ## 1                          7                           7
    ##   number_of_iterations  training_frame
    ## 1                   48 RTMP_sid_b8d1_2
    ## 
    ## Coefficients: glm coefficients
    ##       names coefficients standardized_coefficients
    ## 1 Intercept    -0.684602                  0.402632
    ## 2      RACE    -0.072625                 -0.022425
    ## 3     DPROS     0.090850                  0.090860
    ## 4     DCAPS     0.092826                  0.028837
    ## 5       PSA     0.003603                  0.072057
    ## 6       VOL    -0.001898                 -0.034824
    ## 7   GLEASON     0.145279                  0.158638
    ## 8  AGE_DAYS    -0.000004                 -0.009489
    ## 
    ## H2ORegressionMetrics: glm
    ## ** Reported on training data. **
    ## 
    ## MSE:  0.1720519
    ## RMSE:  0.4147914
    ## MAE:  0.3622143
    ## RMSLE:  0.2990618
    ## Mean Residual Deviance :  0.1720519
    ## R^2 :  0.2846651
    ## Null Deviance :91.39737
    ## Null D.o.F. :379
    ## Residual Deviance :65.37973
    ## Residual D.o.F. :372
    ## AIC :427.6089

We see that the standardised coefficients are the same (to several dp) for all variables including age where the scale has changed. For the non-standardised coefficients this is not the case.

Package info
------------

``` r
sessionInfo()
```

    ## R version 3.4.3 (2017-11-30)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Sierra 10.12.6
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
    ## [1] h2o_3.16.0.2
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_0.12.14    digest_0.6.13   rprojroot_1.3-2 bitops_1.0-6   
    ##  [5] jsonlite_1.2    backports_1.1.2 magrittr_1.5    evaluate_0.10.1
    ##  [9] stringi_1.1.6   rmarkdown_1.8   tools_3.4.3     stringr_1.2.0  
    ## [13] RCurl_1.95-4.8  yaml_2.1.16     compiler_3.4.3  htmltools_0.3.6
    ## [17] knitr_1.18
