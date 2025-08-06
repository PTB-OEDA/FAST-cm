# FinalTest.R
# 
# Tests final models performance before generating predictions
#
# Based on the league table results on the validation and training datasets
#
# Patrick T. Brandt
#
# 20250718 : Initial version

#### Load data ####
# Load data for the various subsets
load("cm_subsets.RData")
rm(list=ls(pattern="train"))
rm(list=ls(pattern="valid"))
rm(africa_me, globe)

#### Set train and test datasets ####
#
# These are going to be in lists so we can do parallel processing or looping 
# as needed at this stage

train <- list(africa_me.test, globe.test)
test <- list(africa_me.test.eval, globe.test.eval)
infosetnames <- c("Africa+ME", "Globe")
names(train) <- names(test) <- infosetnames
rm(africa_me.test, africa_me.test.eval,
   globe.test, globe.test.eval)

#### Define regression models ####
source("formulas.R")
rm(frm, frm1, frm.covar) # only keeps frm.glmm

#### GLMM Estimation and Forecasts ####
library(glmmTMB)
source("predictglmm.R")
source("conflictology.R")

# Set number of samples
N <- 1000

for(i in 1:length(test))
{
  pfix <- names(train)[i]
  
  # Estimate Poisson models
  p.glmm.covar <- glmmTMB(frm.glmm, family = poisson, data=train[[i]])
  p.glmm.nocovar <- update(p.glmm.covar, . ~ ar1(month_factor + 0 | country_id))

  # Generate prediction samples
  assign(paste(pfix, "P.covar.pred", sep="."), predictglmm(p.glmm.covar, newdata = test[[i]], N=N))
  assign(paste(pfix, "P.nocovar.pred", sep="."), predictglmm(p.glmm.nocovar, newdata = test[[i]], N=N))

  cat("Completed Poisson GLMM for", names(train)[i], "\n")
  
  # Estimate NB models
  nb.glmm.covar <- glmmTMB(frm.glmm, family = nbinom1(), data=train[[i]])
  nb.glmm.nocovar <- update(nb.glmm.covar, . ~ ar1(month_factor + 0 | country_id))

  # Generate prediction samples
  assign(paste(pfix, "NB.covar.pred", sep="."), predictglmm(nb.glmm.covar, newdata = test[[i]], N=N))
  assign(paste(pfix, "NB.nocovar.pred", sep="."), predictglmm(nb.glmm.nocovar, newdata = test[[i]], N=N))
  
  cat("Completed Neg Bin GLMM for", names(train)[i], "\n")
  
  # Estimate Tweedie models
  tw.glmm.covar <- glmmTMB(frm.glmm, family = tweedie(), data=train[[i]])
  tw.glmm.nocovar <- update(tw.glmm.covar, . ~ ar1(month_factor + 0 | country_id))

  assign(paste(pfix, "TW.covar.pred", sep="."), predictglmm(tw.glmm.covar, newdata = test[[i]], N=N))
  assign(paste(pfix, "TW.nocovar.pred", sep="."), predictglmm(tw.glmm.nocovar, newdata = test[[i]], N=N))
  
  cat("Completed Tweedie GLMM for", names(train)[i], "\n")
  
  # Get the baselines
  
  assign(paste(names(train)[i],".baselines", sep=""), 
         viewsbaselines(train=train[[i]], test=test[[i]], N = N))

  cat("Completed Baselines for", names(train)[i], "\n")
  
  cat("Saving the results for", names(train)[i], "\n")

  save(list=ls(pattern=".glmm"), 
       file = paste("FinalTest-", names(train)[i], ".RData", sep=""))
  
# print(c(AIC(p.glmm.covar), AIC(p.glmm.nocovar)))
# print(c(AIC(nb.glmm.covar), AIC(nb.glmm.nocovar)))
# print(c(AIC(tw.glmm.covar), AIC(tw.glmm.nocovar)))
}

# Clean up fitted models?
rm(list=ls(pattern=".glmm"))
gc()

#### Forecasts based on Infosets ####

# Add indicators for covariates
`Africa+ME.baselines`$covariates <- "No"
`Africa+ME.NB.covar.pred`$covariates <- "Yes"
`Africa+ME.NB.nocovar.pred`$covariates <- "No" 
`Africa+ME.P.covar.pred`$covariates <- "Yes"
`Africa+ME.P.nocovar.pred`$covariates <- "No"  
`Africa+ME.TW.covar.pred`$covariates <-  "Yes"
`Africa+ME.TW.nocovar.pred`$covariates <- "No"

Globe.baselines$covariates <- "No"
Globe.NB.covar.pred$covariates <- "Yes"
Globe.NB.nocovar.pred$covariates <- "No" 
Globe.P.covar.pred$covariates <- "Yes"
Globe.P.nocovar.pred$covariates <- "No"  
Globe.TW.covar.pred$covariates <-  "Yes"
Globe.TW.nocovar.pred$covariates <- "No"

# Stack the results for the models and datasets
infoset.Africa <- data.table::rbindlist(lapply(ls(pattern="Africa"), "get"))
infoset.Globe <- data.table::rbindlist(lapply(ls(pattern="Globe"), "get"))

# Add labels / factors for the infosets
infoset.Africa$infoset <- "Africa+ME"
infoset.Globe$infoset <- "Globe"

# Combine into a single set of forecasts
forcs <- rbind(infoset.Africa, infoset.Globe)

# Add indicators for whether the cases are
# scored globally or regionally
countries$in_africa_me <- ifelse(countries$in_africa + countries$in_middle_east==1,
                                 "Yes", "No")

names(countries)[1] <- "country_id"
forcs <- merge(forcs, countries[,c(1,9)], by="country_id")

# Then do a clean up
rm(infoset.Africa, infoset.Globe)
rm(`Africa+ME.baselines`,
   `Africa+ME.P.covar.pred`, `Africa+ME.P.nocovar.pred`,
   `Africa+ME.NB.covar.pred`, `Africa+ME.NB.nocovar.pred`,
   `Africa+ME.TW.covar.pred`, `Africa+ME.TW.nocovar.pred`)

rm(Globe.baselines,
   Globe.P.covar.pred, Globe.P.nocovar.pred,
   Globe.NB.covar.pred, Globe.NB.nocovar.pred,
   Globe.TW.covar.pred, Globe.TW.nocovar.pred)

gc()

save(forcs, file = "FinalTest-forcs.RData")
 
#### Scoring ####
library(scoringutils, quietly = TRUE)

scores <- score(as_forecast_sample(forcs,
                                   forecast_unit = c("model", "month_id", "country_id",  "infoset", "in_africa_me", "covariates")))

###### Summarize Scores ######
library(magrittr)
test.scores <- scores %>% summarise_scores(by=c("model", "covariates", "infoset", "in_africa_me"))

###### Final League Tables #####

# scores %>% 
#   summarise_scores(by=c("model", "covariates", "infoset", "in_africa_me")) %>% 
#   filter(in_africa_me =="Yes") %>% arrange(se_mean)
# 
# 
# tmp <- scores %>% filter(in_africa_me=="Yes" & model != "conflictology_country12") %>% 
#   summarise_scores(by=c("model", "covariates", "infoset")) %>% arrange(se_mean)

#### Brier scoring at threshold #####

threshold <- 25
forcs1 <- forcs
forcs1$observed <- ifelse(forcs1$observed>=threshold,1,0)
forcs1$predicted <- ifelse(forcs1$predicted>=threshold,1,0)

binary.scores <- score(as_forecast_sample(forcs1),
                       forecast_unit = c("model", "month_id", "country_id",  "infoset", "in_africa_me", "covariates"))

brier <- binary.scores %>% summarise_scores(by=c("model", "covariates", "infoset", "in_africa_me"))

#### Merged Score League Table ####
names(brier)[7] <- "Brier (>25)"

# test.league.metrics <- test.league[,c(1,2,3,13,6,14)]
# 
# tmp <- test.league.metrics %>% filter(in_africa_me=="Yes") %>% arrange(se_mean)

save.image("FinalTest.RData")

# q(save="no")

