FAST Models for ‘cm’
================
Patrick T. Brandt

July 23, 2025

- [Model Options and Choices](#model-options-and-choices)
- [Model Selection](#model-selection)
- [Appendix](#appendix)

## Model Options and Choices

For the FAST analysis of the `cm` version of the VIEWS data, one faces a
series of choices in constructing a forecast model. These are summarized
by the following:

1.  *Units*: Are the data units of analysis (forecast units) countries,
    grid cells or over months or quarters? In this case the choice is
    country-months, denoted `cm` units.
2.  *Models*: There are a wide variety to consider here and they vary
    according to broad parameters set out prior. These include choices
    of
    1.  *Functional form*: Does one using fixed effects, mixed marginal
        effect, or something else?
    2.  *Distribution of the data*: do we assume a (marginal) Poisson,
        negative binomial, Tweedie, or some zero-inflated likelihood
        form?
3.  *Information Sets*:
    1.  *Covariates*: This is mainly a choice over the covariates of
        interest to the sponsor:

    - Climate
    - Food security
    - Demographics

    2.  *Other conditioning variables or units*: Do only nearest
        neighbors in spadfe and time matter or should all global data be
        leveraged (recall we have the `cm` data for the globe, but are
        scored on data for Africa and the Middle East)
4.  *Scoring*: what criteria do we use to rank or evaluuate winners and
    losers? Options include root mean squared error (RMSE), bias,
    dispersion, continuous (discrete) rank probability scores (CRPS),
    etc.  
5.  *Forecast horizons* How many periods forward are needed in the
    forecasts? This is defined by the sponsor in months 3, 6, and 12.

## Model Selection

A prior set of model selection rounds were conducted. These build on the
datasets and setup described
[here](https://github.com/PTB-OEDA/VIEWS-Startup) from May 2025.

Based on these data for the `cm` VIEWS dataset a series of models were
fit to constrain and select from the larger set of options above. *For a
RMSE criteria* using data from 2010:1–2022:6 and a validation comparison
sample of data from 2022:10-2023:9 the top 20 models are

## Appendix

The initial model runs to do preliminary model selection and
specification searches are all run via a set of batch scripts included
with this repo. These are run in the following sequence via the
`batch.sh` bash shell script to invoke `R` and the code files
designated.

``` default
#! bash

# Training models
R CMD BATCH setup.R
R CMD BATCH modelselect.R &
R CMD BATCH modelselect-globe.R

# Validation models
R CMD BATCH modelselect-valid.R &
R CMD BATCH modelselect-globe-valid.R

R CMD BATCH modelselect-glmm-covar.R

# Scoring across the sets
R CMD BATCH scoring-cm.R
R CMD BATCH scoring-cm-valid.R
```
