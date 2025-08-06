# quicktweedie.R

#### Quick Tweedie evals ####

# # Estimate Tweedie parameters for the training data
# library(statmod)
# library(tweedie)
# param.tw <- tweedie.profile(ged_sb ~ year_factor, 
#                             xi.vec=seq(1.65, 1.78, by=0.015),
#                             data = africa_me.train,
#                             do.plot=TRUE,
#                             control=list(maxit=50),
#                             method="series",
#                             verbose=FALSE)
# 
# print(param.tw$xi.max)
# 
# tw <- glm(ged_sb ~ year_factor, 
#           data = africa_me.train,
#           family=statmod::tweedie(var.power=param.tw$xi.max, link.power=0),
#           control=list(maxit=50))
