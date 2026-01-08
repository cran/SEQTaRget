## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SEQTaRget)

## -----------------------------------------------------------------------------
options <- SEQopts(# tells SEQuential to create Kaplan-Meier curves
                   km.curves = TRUE,
                   # tells SEQuential to weight the outcome model
                   weighted = TRUE, 
                   # tells SEQuential to build weights from the pre-expanded data
                   weight.preexpansion = TRUE)

# use some example data in the package
data <- SEQdata                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)

# retrieve risk plot
km_curve(model, plot.type = "risk")
# retrieve survival and risk data
survival_data <- km_data(model)
risk_data(model)
risk_comparison(model)

## -----------------------------------------------------------------------------
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE, 
                   # tells SEQuential to build weights from the post-expanded data
                   weight.preexpansion = FALSE)

data <- SEQdata                                 
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)

km_curve(model, plot.type = "risk")
risk_data(model)
risk_comparison(model)

## -----------------------------------------------------------------------------
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = TRUE,
                   # tells SEQuential to run a dynamic intervention
                   excused = TRUE,                               
                   # tells SEQuential to use columns excusedOne and 
                   # excusedZero as excused conditions for treatment switches
                   excused.cols = c("excusedZero", "excusedOne"), 
                   # tells SEQuential to expect treatment levels 0, 1
                   # (mapping to the same positions as the list in excused.cols)
                   treat.level = c(0, 1))
data <- SEQdata                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)

km_curve(model, plot.type = "risk")
risk_data(model)
risk_comparison(model)

## -----------------------------------------------------------------------------
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = FALSE,
                   excused = TRUE,                               
                   excused.cols = c("excusedZero", "excusedOne"), 
                   treat.level = c(0, 1),
                   weight.p99 = TRUE)
data <- SEQdata                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)

km_curve(model, plot.type = "risk")
risk_data(model)
risk_comparison(model)

## -----------------------------------------------------------------------------
options <- SEQopts(km.curves = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = FALSE,
                   excused = TRUE,                               
                   excused.cols = c("excusedZero", "excusedOne"), 
                   treat.level = c(0, 1),
                   # add a competing event
                   compevent = "LTFU")

data <- SEQdata.LTFU                                
model <- SEQuential(data, 
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)

km_curve(model, plot.type = "risk")
risk_data(model)
risk_comparison(model)

## -----------------------------------------------------------------------------
options <- SEQopts(# tell SEQuential to run hazard ratios
                   hazard = TRUE,
                   weighted = TRUE,
                   weight.preexpansion = FALSE,
                   excused = TRUE,                               
                   excused.cols = c("excusedZero", "excusedOne"),
                   weight.p99 = TRUE)

data <- SEQdata                              
model <- SEQuential(data,
                    id.col = "ID",
                    time.col = "time",
                    eligible.col = "eligible",
                    treatment.col = "tx_init",
                    outcome.col = "outcome",
                    time_varying.cols = c("N", "L", "P"),
                    fixed.cols = "sex",
                    method = "censoring",
                    options = options)
hazard_ratio(model)

