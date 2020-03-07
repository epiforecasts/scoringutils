<!-- badges: start -->
 [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) [![Travis build status](https://travis-ci.org/epiforecasts/scoringutils.svg?branch=master)](https://travis-ci.org/epiforecasts/scoringutils) [![codecov](https://codecov.io/gh/epiforecasts/scoringutils/branch/master/graphs/badge.svg)](https://codecov.io/gh/epiforecasts/scoringutils/) 
<!-- badges: end -->

 

# scoringutils
Utilities and Functions for Scoring Forecasts


## Components

### Probabilistic Forecasts

  #### binary forecasts
    - Brier Score
  
  #### integer-valued forecasts
    - [x] PIT / calibration
    - [x] bias
    - [x] sharpness
    - [x] DSS
    - [x] CRPS

  #### continuous forecasts
    - [x] CRPS
    - [x] LogS

### Point Forecasts

  #### binary forecasts
    - 

  #### integer-valued forecasts
    - 

  #### continuous forecasts
    - 



### Todo
  - [ ] Vignette
  - [ ] References for CRPS and LogS functions



  
### Things to discuss
  - [ ] do we need an option for working with datetime values
  - [ ] decide on nomenclature of variables
  - [ ] What other functionality do we want to include? Also: what is already there?
  - [ ] Should we / can we wrap functions from other packages? 
  - [ ] Do we want to implement something for probabilistic non-MCMC predictions?
  - [ ] should we make a formal distinction between absolute and comparative metrics?
  - [ ] How do we want to organise error handling? in a separate function? at the lowest level only?




