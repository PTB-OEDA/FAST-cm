# scoring-cm.R
#
# Scores the forecasts produced in the modelselect* scripts
#
# Patrick t. Brandt
#
# 

##### Load packages ####
# library(MASS)
# library(pscl, quietly = TRUE)
# library(statmod)
# library(tweedie)
# library(glmmTMB)
library(scoringutils, quietly = TRUE)

##### Load data for Africa+ME ####

# Load the data from the Africa+ME data models
load("africa_me.train.forcs.RData")

# Drop large objects that are redundant and not needed
rm(NB.stacked, P.stacked, TW.stacked,
   africa_me.train.frm.preds,
   africa_me.train.frm1.preds,
   africa_me.train.frm2.preds)

#### Add "zeros" and "Conflictology-12 month ####

# Get the baselines
source("conflictology.R")

vb <- viewsbaselines(train = africa_me.train,
                     test = africa_me.train.eval, 
                     N = N)

#### Forecasts based on Africa+ME Infoset ####
infoset1 <- rbind(forcs,vb)
infoset1$infoset <- "Africa+ME"
infoset1$in_africa_me <- "Yes"
rm(forcs,vb); gc()

#### Load data for Globe ####
load("globe.train.forcs.RData")

# Drop large objects that are redundant
rm(NB.stacked, P.stacked, TW.stacked,
   globe.train.frm.preds,
   globe.train.frm1.preds,
   globe.train.frm2.preds)

#### Forecasts based on Globe Infoset ####
infoset2 <- forcs; rm(forcs); gc()

# Some data and variable setups
infoset2$infoset <- "Globe"

countries$in_africa_me <- ifelse(countries$in_africa + countries$in_middle_east==1, 
                                 "Yes", "No")

names(countries)[1] <- "country_id"
infoset2 <- merge(infoset2, countries[,c(1,9)], by="country_id")

#### Scoring ####

scores <- score(as_forecast_sample(rbind(infoset1, infoset2),
                forecast_unit = c("model", "month_id", "country_id", "infoset", "in_africa_me")))

###### Summarize Scores ######
library(magrittr)
train.scores <- scores %>% summarise_scores(by=c("model", "infoset", "in_africa_me"))

###### League Tables #####

# Fits sorted by CRPS
crps.table <- train.scores[order(train.scores$crps),c(1,2,3,6,13)]
print(crps.table, digits=3)

# Fits sorted by RMSE
rmse.table <- train.scores[order(train.scores$se_mean),c(1,2,3,6,13)]
print(rmse.table, digits=3)

# Only cases in Africa + ME 
rmse.table.africa_me <- train.scores %>% filter(in_africa_me=="Yes") %>% 
  arrange(se_mean) %>% select(model, infoset, se_mean)

print(rmse.table.africa_me, digits=4)

# Demo Brier scoring at threshold
threshold <- 25
infoset3 <- rbind(infoset1,infoset2)
infoset3$observed <- ifelse(infoset3$observed>=threshold,1,0)
infoset3$predicted <- ifelse(infoset3$predicted>=threshold,1,0)

binary.scores <- score(as_forecast_sample(infoset3),
                                   forecast_unit = c("model", "month_id", 
                                                     "country_id", "infoset", 
                                                     "in_africa_me")) %>% 
  add_relative_skill(by = c("infoset", "in_africa_me"))

brier <- binary.scores %>% summarise_scores(by=c("model", "infoset", "in_africa_me"))

print(brier.out <- brier[order(brier$crps_relative_skill),c(1,2,3,6,14)])

names(brier.out)[4] <- "Threshold Brier Score"

#### Save as a XLSX ####
library(writexl)
write_xlsx(x = list("CRPS Sort, all training sets" = crps.table, 
                    "RMSE Sort, all training sets" = rmse.table, 
                    "RMSE Sort, only Africa + ME" = rmse.table.africa_me,
                    "BS (>25), all training sets" = brier.out),
           path = "TrainingFits.xlsx")




q(save="no")
