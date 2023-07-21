# deeptox

<!-- badges: start -->
<!-- badges: end -->

deeptox makes forward predictions using the DEB-TKTD model ‘DEBtox2019’ (for full details of the model theory see Jager, 2020). The software is specifically designed to make forward predictions only, not to perform model calibration and validation. Therefore, validated model parameters for a given species and chemical need to be provided. You may derive and validate your own model parameters using other software (e.g. BYOM) or use a parameter set that has been tested and documented by others.


Specifically, deeptox predicts the EPx multiplier, an endpoint relevant to ecological risk assessment (ERA) of plant protection products (for details see EFSA, 2018). This is the factor by which a given (realistic) exposure profile would need to be multiplied in order to elicit an X% reduction in an endpoint, i.e. either growth or reproduction. The higher the EPx multiplier, the lower the risk that the environmentally relevant exposure would lead to unacceptable impacts. 

The software uses a ‘moving time window’ approach in which long exposure profiles are broken down into multiple overlapping ‘virtual toxicity tests’ (VTTs). Further details on this approach are provided below. The lowest EPx multiplier derived for a compound is then compared to a (user defined) critical value, if that value is exceeded then the test criterion is passed.

## Installation

You can install the most recent version of deeptox from github with the following command
``` r
# install.packages("remotes")
remotes::install_github("RIFCON-GmbH/deeptox")
```
to start the Shiny app use

``` r
library(deeptox)
deeptox::run_app()
```

To run a simple analysis in the console without a GUI run a template as shown in the vignette
``` r
vignette("deeptox", "deeptox")
```

