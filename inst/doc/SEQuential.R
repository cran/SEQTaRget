## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, eval=FALSE--------------------------------------------------------
# library(SEQTaRget)
# 
# options <- SEQopts(km.curves = TRUE, #asks the function to return survival and risk estimates
#                    bootstrap = TRUE, #asks the model to preform bootstrapping
#                    bootstrap.nboot = 10) #asks the model for 10 bootstrap samples

## ----running, eval=FALSE------------------------------------------------------
# data <- SEQdata
# model <- SEQuential(data, id.col = "ID",
#                           time.col = "time",
#                           eligible.col = "eligible",
#                           treatment.col = "tx_init",
#                           outcome.col = "outcome",
#                           time_varying.cols = c("N", "L", "P"),
#                           fixed.cols = "sex",
#                           method = "ITT", options = options)

## ----outcome, eval=FALSE------------------------------------------------------
# outcome(model)     # Returns a list of only the outcome models
# km_curve(model)    # Prints the survival curve

