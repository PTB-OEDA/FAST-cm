# modelselect-glmm-covar.R
#
# Model selection via fitting a series of models and scoring them 
# for the training data

#### Load data ####
# Load data for the various subsets
load("cm_subsets.RData")

# Drop all globe data for now
rm(list=ls(pattern="globe"))

#### Load estimation functions ####
# Load the count.cf function 
# (this is not the same one we have used before -- it is new!)
# source("count.cf.R")

#### Define regression models ####
source("formulas.R")
frms <- list(frm, frm1, frm.covar, frm.glmm)

#### GLMs Estimations and Prediction ####

N <- 1000

#### GLMM Training Estimation ####
library(glmmTMB)

train <- africa_me.valid
test <- africa_me.valid.eval

# Estimate for each density -- can loop or parallel if needed
p.glmm <- glmmTMB(frm.glmm,
                  family = poisson,
                  data=train, 
                  control=glmmTMBControl(parallel = 4))

nb.glmm <- glmmTMB(frm.glmm,
                   family = nbinom1(),
                   data=train)

tw.glmm <- glmmTMB(frm.glmm,
                   family = tweedie(),
                   data=train)

AIC(p.glmm)
AIC(nb.glmm)
AIC(tw.glmm)

library(broom.mixed)
library(dotwhisker)
library(dplyr)
p.tmp <- broom.mixed::tidy(p.glmm, effects = "fixed", exponentiate=TRUE) |> mutate(model = "PGLMM + COVAR")
nb.tmp <- broom.mixed::tidy(nb.glmm, effects = "fixed", exponentiate=TRUE) |> mutate(model = "NBGLMM + COVAR")
tw.tmp <- broom.mixed::tidy(tw.glmm, effects = "fixed", exponentiate=TRUE) |> mutate(model = "TWGLMM + COVAR")

pdf("modelselect-glmm-covar-figs.pdf", width=6, height=6)
dw <- dwplot(rbind(p.tmp[2:4,], nb.tmp[2:4,], tw.tmp[2:4,]))
print(dw+geom_vline(xintercept=1,lty=2))

dw2 <- dwplot(rbind(p.tmp[5,], nb.tmp[5,], tw.tmp[5,]))
print(dw2+geom_vline(xintercept=1,lty=2))
dev.off()

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
P.stacked$model <- "Poisson GLMM + COVAR"
rm(f.p, eval.p.glmm.mcmc, eval.p.glmm)

f.nb <- cbind(test, t(eval.nb.glmm.mcmc))
NB.stacked <- reshape(f.nb, 
                     direction = "long",
                     varying = list(names(f.nb)[4:(N+3)]),
                     v.names = "predicted",
                     idvar = c("month_id", "country_id"),
                     timevar = "sample_id",
                     times = 1:N)
NB.stacked$model <- "Neg Binom GLMM + COVAR"
rm(f.nb, eval.nb.glmm.mcmc, eval.nb.glmm)

f.tw <- cbind(test, t(eval.tw.glmm.mcmc))
TW.stacked <- reshape(f.tw, 
                     direction = "long",
                     varying = list(names(f.tw)[4:(N+3)]),
                     v.names = "predicted",
                     idvar = c("month_id", "country_id"),
                     timevar = "sample_id",
                     times = 1:N)
TW.stacked$model <- "Tweedie GLMM + COVAR"
rm(f.tw, eval.tw.glmm.mcmc, eval.tw.glmm)

#### Save fitted glmm models in case we need them later 
save(list=ls(pattern=".glmm"), file = "africa_me.interim-glmm-covar.RData")
rm(list=ls(pattern=".glmm"))
gc()

#### Check all fitted for scoring ####
colnames(P.stacked)
colnames(NB.stacked)
colnames(TW.stacked)

#### Stacked and saved ####

forcs <- rbind(P.stacked,
               NB.stacked,
               TW.stacked)

save.image("africa_me-glmm.valid.forcs.RData")

# # Get the baselines
# source("conflictology.R")
# 
# vb <- viewsbaselines(train=africa_me.valid,
#                      test=africa_me.valid.eval, N = N)
# 
# #### Forecasts based on Africa+ME Infoset ####
# infoset3 <- rbind(forcs,vb)
# infoset3$infoset <- "Africa+ME"
# infoset3$in_africa_me <- "Yes"
# rm(forcs,vb); gc()
# 
# # countries$in_africa_me <- ifelse(countries$in_africa + countries$in_middle_east==1, 
# #                                  "Yes", "No")
# # 
# # names(countries)[1] <- "country_id"
# # infoset3 <- merge(infoset3, countries[,c(1,9)], by="country_id")
# 
# #### Scoring ####
# library(scoringutils, quietly = TRUE)
# 
# scores <- score(as_forecast_sample(infoset3,
#                                    forecast_unit = c("model", "month_id", "country_id")))
# 
# ###### Summarize Scores ######
# library(magrittr)
# train.scores <- scores %>% summarise_scores(by=c("model"))
# 
# ###### League Tables #####
# 
# # Fits sorted by CRPS
# crps.table <- train.scores[order(train.scores$crps),c(1,4,11)]
# print(crps.table, digits=3)
# 
# # Fits sorted by RMSE
# rmse.table <- train.scores[order(train.scores$se_mean),c(1,4,11)]
# print(rmse.table, digits=3)
# 
# # Demo Brier scoring at threshold
# 
# infoset3$observed <- ifelse(infoset3$observed>=25,1,0)
# infoset3$predicted <- ifelse(infoset3$predicted>=25,1,0)
# 
# binary.scores <- score(as_forecast_sample(infoset3,
#                                    forecast_unit = c("model", "month_id", "country_id")))
# 
# brier <- binary.scores %>% summarize_scores(by=c("model"))
# 
# print(brier[,c(1,4)])

# save.image("modelselect-glmm-covar.RData")

q(save="no")
