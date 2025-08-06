# FAST-cm
FAST Models for VIEWS cm data

*Patrick T. Brandt*

Professor of Political Science 

University of Texas, Dallas

This repo develops the training-test-dev for the `cm` models.  It shows 

1.  The train-dev-test splits and analysis results
2.  The final models chosen and their interpretations
3.  How the forecast results can be summarized by country, quantiles, and time periods.

## Basic starting setup 

1.  Run the startup repo to get the data as documented [here](https://github.com/PTB-OEDA/VIEWS-Startup). This gets you all the data downloaded and setup for analysis.
2.  Run the `setup.R` script.

This will give you the datasets in `cm_subsets.RData`.  This is then the main input to what follows since it defines dataframes for the Globe and Africa and the Middle East over the time periods.

One needs to execute the files in this repo to be able to fully generate the forecasts in the last step.  
Overall, the interim steps generate over 2GB of simulated forecasts and data subsets of for the training and evaluation steps.  

## Training and Validation Steps

Execute the `batch.sh` script to run the additional data prep, training, dev and validation models.  This is all inputs for the final model selection and presentation.  

These are the R scripts (which need to be run in order -- see `batch.sh`):
  - `modelselect.R`
  - `modelselect-globe.R`
  - `modelselect-valid.R`
  - `modelselect-globe-valid.R`
  - `modelselect-glmm-covar.R`
  - `scoring-cm.R` 
  - `scoring-cm-valid.R`

These create all of the training and validation datasets for the model selection.

## Selecting and Fitting the Forecast Production Model

The forecast model is then summarized in the `FAST-cm.Rmd` which selects and fits the final production model.

1.  Model comparison and selection is summarized in `FAST-cm.*` related files, which are here in all the favorite flavors (`Rmd` for code, and outputs in `pdf` / `html` / `md`).
2.  Results and forecast summaries are [here](https://github.com/PTB-OEDA/FAST-cm/blob/main/FAST-cm.md#selected-model-negative-binomial-glmm-with-covariates)
    a.  Spreadsheets summaries by country and month are in the repo as `*.xlsx` files
    b.  Add the samples as a dataframe and output (TBD)
    c.  Add the exceedence / threshold probabilities for `cm` (e.g. Pr(events) > 25)


One needs to execute the files in this repo to be able to fully generate the forecasts in the last step.  
Overall, the interim steps generate over 2GB of simulated forecasts and data subsets of for the training and evaluation steps.  

## Training and Validation Steps

Execute the `batch.sh` script to run the additional data prep, training, dev and validation models.  This is all inputs for the final model selection and presentation.  

These are the R scripts:
  - `modelselect.R`
  - `modelselect-globe.R`
  - `modelselect-valid.R`
  - `modelselect-globe-valid.R`
  - `modelselect-glmm-covar.R`
  - `scoring-cm.R` 
  - `scoring-cm-valid.R`

These create all of the training and validation datasets for the model selection.

## Selecting and Fitting the Forecast Production Model

The forecast model is then summarized in the `FAST-cm.Rmd` which selects and fits the final production model.

1.  Model comparison and selection is summarized in `FAST-cm.*` related files, which are here in all the favorite flavors (`Rmd` for code, and outputs in `pdf` / `html` / `md`).
2.  Results and forecast summaries are [here](https://github.com/PTB-OEDA/FAST-cm/blob/main/FAST-cm.md#selected-model-negative-binomial-glmm-with-covariates)
    a.  Spreadsheets summaries by country and month are in the repo as `*.xlsx` files
    b.  Add the samples
    c.  add the exceedence / threshold probabilities for `cm`.

>>>>>>> fbc13335d0e71ab15e6124f40e9cb4d2f6123762

## Acknowledgments

- Support has been provided by [NSF Award 2311142](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2311142).  Any opinions, findings, and conclusions or recommendations expressed in this material are those of the author(s) and do not necessarily reflect the views of the National Science Foundation.

- This is in support of models for [Fast forward: Forecasting global emerging threats](https://www.canada.ca/en/department-national-defence/programs/defence-ideas/element/contests/challenge/fast-foward-emerging-threats.html)

