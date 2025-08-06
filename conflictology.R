# conflictology.R
#
# Patrick T. Brandt
#
# 2025-06-04 : Initial version
# 2025-07-04 : Updates to make a set of functions and then to use train/dev/test splits

library(tidyverse, warn.conflicts = FALSE, quietly = TRUE)
library(zoo)


#### Hegre et al-alike baselines #####
# 
# Computes the views baseline predictions from
# Hegre et al. (2025) Table 3
#
# exactly_zero
# last_historical
# conflictology_country12

viewsbaselines <- function(train, test, N=10)
{
  # Get only the variables we need
  train <- train[,c("month_id", "country_id", "ged_sb")]
  test <- test[,c("month_id", "country_id", "ged_sb")]
  
  # Get the modal predictions per the paper for each unit
  conflictology.mean <- train %>% group_by(country_id) %>% 
    mutate(rolling12 = zoo::rollmeanr(ged_sb, k=12,  fill = NA_real_)) %>% ungroup()
  c.m <- as.data.frame(conflictology.mean[conflictology.mean$month_id==max(conflictology.mean$month_id),-3])
  colnames(c.m) <- c(colnames(c.m)[1:2], "conflictology12")
  
  last.historical <- train[train$month_id == max(train$month_id),]
  colnames(last.historical) <- c(colnames(last.historical)[1:2], "last_historical")

  # Now carry forward for each time period (assumes test all after train)
  n <- nrow(test)  
  tmp <- range(test$month_id)
  mint <- tmp[1]; maxt <- tmp[2]

  # Merge on the summary stats  
  template <- merge(test, c.m, by.x="country_id", by.y="country_id")  
  template <- merge(template, last.historical, by.x="country_id", by.y="country_id")

  last.tmp <- conflict.tmp <- matrix(NA, nrow = nrow(template), ncol = N)
  for(i in 1:nrow(template))
  {
    if(template$conflictology12[i]==0) {
      conflict.tmp[i,] <- rep(0,N)
    } else {
      conflict.tmp[i,] <- rpois(N, lambda=template$conflictology12[i])
  }

  if(template$last_historical[i]==0) {
    last.tmp[i,] <- rep(0,N)
  } else {
    last.tmp[i,] <- rpois(N, lambda=template$last_historical[i])
  }
  }
  
  # Merge up and names
  colnames(conflict.tmp) <- colnames(last.tmp) <- 1:N
  conflict.tmp <- cbind(test, conflict.tmp)
  last.tmp <- cbind(test, last.tmp)
  zeros.tmp <- last.tmp; zeros.tmp[,4:(3+N)] <- 0
  
  conflict.tmp <- reshape(conflict.tmp, 
                          direction = "long",
                          varying = list(colnames(conflict.tmp)[4:(N+3)]),
                          v.names = "predicted",
                          idvar = c("month_id", "country_id"),
                          timevar = "sample_id",
                          times = 1:N)
  conflict.tmp$model <- "conflictology_country12"

  last.tmp <- reshape(last.tmp, 
                          direction = "long",
                          varying = list(colnames(last.tmp)[4:(N+3)]),
                          v.names = "predicted",
                          idvar = c("month_id", "country_id"),
                          timevar = "sample_id",
                          times = 1:N)
  last.tmp$model <- "last_historical"
  
  zeros.tmp <- reshape(zeros.tmp, 
                          direction = "long",
                          varying = list(colnames(zeros.tmp)[4:(N+3)]),
                          v.names = "predicted",
                          idvar = c("month_id", "country_id"),
                          timevar = "sample_id",
                          times = 1:N)
  zeros.tmp$model <- "exactly_zero"
  
  # Return
  out <- rbind(conflict.tmp, last.tmp, zeros.tmp)
  names(out)[3] <- "observed"
  return(out)
}

# # # Demo
# # # Load the data
# load("cm_subsets.RData")
# # # Drop all globe data for now
# rm(list=ls(pattern="globe"))
# # 
# # 
# # # Do this over train / eval split setup as a function....
# # train <- africa_me.train[,c("month_id", "country_id", "ged_sb")]
# #test <- africa_me.train.eval[,c("month_id", "country_id", "ged_sb")]
# # 
# # 
# # tmp <- viewsbaselines(train, test, N=100)
# tmp <- viewsbaselines(africa_me.train, africa_me.train.eval, N=10)
# # tmp$observed <- tmp$ged_sb
# #  
# library(scoringutils)
# library(magrittr)
# scored.tmp <- score(as_forecast_sample(tmp,
#                                        forecast_unit = c(c("model", "month_id", "country_id"))))
# 
# 
# scored.tmp %>% summarize_scores(by="model")
