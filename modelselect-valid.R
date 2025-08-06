# modelselect-valid.R
#
# Model selection via fitting a series of models and scoring them 
# for the validation data

#### Load data ####
# Load data for the various subsets
load("cm_subsets.RData")

# Drop all globe data for now
rm(list=ls(pattern="globe"))

#### Load estimation functions ####
# Load the count.cf function 
# (this is not the same one we have used before -- it is new!)
source("count.cf.R")

#### Define regression models ####
source("formulas.R")

# frms <- list(frm, frm1, frm.covar)

#### Define the dataset ####
train <-  africa_me.valid
test <-  africa_me.valid.eval

#### GLMs Estimations and Prediction ####
# These are going to be applied across the choices of 
#
# 1) Validation data (globe versus africa_me = 2)
# 2) Regression or dynamic choices (Reg choices here = 3)
# 3) Distribution choices (P, NB, ZIP, ZINB, TW = 5)

N <- 1000
xi <- seq(1.3,1.75,by=0.05)

# Serial versions for debug and easier use
africa_me.valid.frm.preds <- count.cf(frm=frm, 
                                   train,
                                   test,
                                   xi.vec = xi,
                                   N=N)

africa_me.valid.frm1.preds <- count.cf(frm=frm1, 
                                   train,
                                   test,
                                   xi.vec = xi,
                                   N=N)

africa_me.valid.frm2.preds <- count.cf(frm=frm.covar,
                                       train,
                                       test,
                                       xi.vec = xi,
                                       N=N)

#### GLMM Estimation ####
library(glmmTMB)

# Estimate for each density -- can loop or parallel if needed
p.glmm <- glmmTMB(ged_sb ~ ar1(month_factor + 0|country_id), 
                     family = poisson,
                     data=train, 
                     control=glmmTMBControl(parallel = 4))

nb.glmm <- glmmTMB(ged_sb ~ ar1(month_factor + 0|country_id), 
                      family = nbinom1(),
                      data=train)

tw.glmm <- glmmTMB(ged_sb ~ ar1(month_factor + 0|country_id), 
                      family = tweedie(),
                      data=train)

#### GLMM Predictions ####

eval.p.glmm <- predict(p.glmm, newdata=test, 
                                       type = "response",
                                       allow.new.levels = TRUE)

eval.nb.glmm <- predict(nb.glmm, newdata=test, 
                                       type = "response",
                                       allow.new.levels = TRUE)

eval.tw.glmm <- predict(tw.glmm, newdata=test, 
                                        type = "response",
                                        allow.new.levels = TRUE)


#### Sample GLMM Predictions ####
n <- nrow(test)
set.seed(986)
eval.p.glmm.mcmc <- sapply(1:n, function(i) {rpois(N, eval.p.glmm[i])})          
eval.nb.glmm.mcmc <- sapply(1:n, function(i) {rnbinom(N, size=sigma(nb.glmm), 
                                                      mu=eval.nb.glmm[i])})
eval.tw.glmm.mcmc <- t(replicate(N, mgcv::rTweedie(eval.tw.glmm, 
                                             p=family_params(tw.glmm))))

#### Stack GLMM results ####
test <- test[,c("month_id", "country_id", "ged_sb")]
colnames(test) <- c("month_id", "country_id", "observed")

f.p <- cbind(test, t(eval.p.glmm.mcmc))
P.stacked <- reshape(f.p, 
                     direction = "long",
                     varying = list(names(f.p)[4:(N+3)]),
                     v.names = "predicted",
                     idvar = c("month_id", "country_id"),
                     timevar = "sample_id",
                     times = 1:N)
P.stacked$model <- "Poisson GLMM"
rm(f.p, eval.p.glmm.mcmc, eval.p.glmm)

f.nb <- cbind(test, t(eval.nb.glmm.mcmc))
NB.stacked <- reshape(f.nb, 
                     direction = "long",
                     varying = list(names(f.nb)[4:(N+3)]),
                     v.names = "predicted",
                     idvar = c("month_id", "country_id"),
                     timevar = "sample_id",
                     times = 1:N)
NB.stacked$model <- "Neg Binom GLMM"
rm(f.nb, eval.nb.glmm.mcmc, eval.nb.glmm)

f.tw <- cbind(test, t(eval.tw.glmm.mcmc))
TW.stacked <- reshape(f.tw, 
                     direction = "long",
                     varying = list(names(f.tw)[4:(N+3)]),
                     v.names = "predicted",
                     idvar = c("month_id", "country_id"),
                     timevar = "sample_id",
                     times = 1:N)
TW.stacked$model <- "Tweedie GLMM"
rm(f.tw, eval.tw.glmm.mcmc, eval.tw.glmm)

#### Save fitted glmm models in case we need them later 
save(list=ls(pattern=".glmm"), file = "africa_me.valid.interim-glmm.RData")
rm(list=ls(pattern=".glmm"))
gc()

#### Check all fitted for scoring ####
colnames(africa_me.valid.frm.preds$forecasts)
colnames(africa_me.valid.frm1.preds$forecasts)
colnames(P.stacked)
colnames(NB.stacked)
colnames(TW.stacked)

#### Stacked and saved ####

africa_me.valid.frm.preds$forecasts$model <- paste(africa_me.valid.frm.preds$forecasts$model,
                                                   "+FE", sep="")
africa_me.valid.frm1.preds$forecasts$model <- paste(africa_me.valid.frm1.preds$forecasts$model,
                                                   "+LAGS", sep="")
africa_me.valid.frm2.preds$forecasts$model <- paste(africa_me.valid.frm2.preds$forecasts$model,
                                                     "+LAGS+COVAR", sep="")

forcs <- rbind(africa_me.valid.frm.preds$forecasts,
               africa_me.valid.frm1.preds$forecasts,
               africa_me.valid.frm2.preds$forecasts,
               P.stacked,
               NB.stacked,
               TW.stacked)

save.image("africa_me.valid.forcs.RData")

q(save="no")
