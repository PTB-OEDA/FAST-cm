# cf.count: estimates a series of event count models  
# 
# Uses the args
# frm = formula that can be evaluated in the test and train data
# train = training dataframe
# test = testing dataframe
# xi.vec = vector of values to try for the grid for the 
#          Tweedie evals
# N = Number of draws for the posterior for each model
#
# Result is a dataframe of the output that can be scored.
library(MASS)
library(pscl, quietly = TRUE)
library(statmod)
library(tweedie)

count.cf <- function(frm, train, test, xi.vec = seq(1.1, 1.75, by=0.05), 
                     N = 100, oldreturn=FALSE)
{
  # Fit models
  p <- glm(frm, family = poisson, data=train, model=FALSE)

  # NB -- this is pretty boutique, but robust
  train1 <- subset(train, subset = c(ged_sb<100000))
  y <- model.frame(frm, data=train1)[,1]
  X <- model.matrix(frm, data=train1)
  
  # Three ways to get theta start!
  theta1 <-  MASS::theta.md(y, fitted(p), dfr = df.residual(p))
  theta2 <-  MASS::theta.ml(y, fitted(p))
  theta3 <- MASS::theta.mm(y, fitted(p), dfr=df.residual(p))
  # Pick one
  theta <- max(c(theta1, theta2, theta3)) 
  # Robust fit
  nb <- statmod::glmnb.fit(X, y, dispersion = 1/theta, 
                           start.method = "log(y)",
                           maxit=100, trace=TRUE)

#  nb <- glm.nb(frm, data=train, start = coefficients(p), model=FALSE, init.theta = theta)
  
  zip <- zeroinfl(frm, data=train, dist="poisson", model=FALSE, y = FALSE)
  zinb <- zeroinfl(frm, data=train, dist="negbin", model=FALSE, y = FALSE)
  #  zgeom <- zeroinfl(frm, data=train, dist="geometric", model=FALSE, y = FALSE)
  
  cat("Fitting Tweedie model grid for xi:\n")  
  param.tw <- tweedie.profile(frm, xi.vec=xi.vec, 
                              data = train,
                              do.plot=FALSE,
                              control=list(maxit=100),
                              method="series", verbose=FALSE)
  print(param.tw$xi.max)
  tw <- glm(frm, data = train,
            family=statmod::tweedie(var.power=param.tw$xi.max, link.power=0),
            control = list(maxit=100))
  
  # Predict forecasts for test periods
  n <- nrow(test)
  p.p <- predict(p, newdata=test, type="response")
  
  X <- model.matrix(frm, data=test)
  p.nb <- exp(X%*%nb$coefficients)

  p.zip <- predict(zip, newdata=test, type="response")
  p.zinb <- predict(zinb, newdata=test, type="response")
  #  p.zgeom <- predict(zgeom, newdata=test, type="response")
  p.tw <- predict(tw, newdata=test, type = "response")
  
  # Now simulate the forecasts for each test period
  cat("Sampling forecast densities")
  p.sample <- t(sapply(1:n, function(i) {rpois(N, lambda=p.p[i])}))  
  cat(" .")
  nb.sample <- t(sapply(1:n, function(i) {rnbinom(N, size=1/theta, mu=p.nb[i])}))
  cat(" .")
  zip.sample <- t(sapply(1:n, function(i) {rpois(N, lambda=p.zip[i])}))
  cat(" .")
  zinb.sample <- t(sapply(1:n, function(i) {rnbinom(N, size=zinb$theta, mu=p.zinb[i])}))  
  cat(" .\n")
  tw.sample <- t(sapply(1:n, function(i) {rtweedie(N, xi=param.tw$xi.max,
                                                   mu=p.tw[i], phi=1) }))
  
  
  # Save the models
  fits <- list(P=p, 
               NB=nb,
               ZIP = zip, ZINB = zinb,
               TW = tw)
  
  # Some cleanups
  rm(p,nb,zip,zinb,tw,
     p.p,p.nb,p.zip,p.zinb,p.tw)

  # Old return method  
  if(oldreturn==TRUE) { return(list(P = p.sample,
                                    NB = nb.sample,
                                    ZIP = zip.sample,
                                    ZINB = zinb.sample,
                                    TW = tw.sample)) }

  # New return for 2025 analyses
  
  # Do a new setup of the training data...
  test <- test[,c("month_id", "country_id", "ged_sb")]
  colnames(test) <- c("month_id", "country_id", "observed")
  
  # Add the test data back to the series
  # Poisson
  
  f.p <- cbind(test[,], p.sample)
  P.stacked <- reshape(f.p, 
                       direction = "long",
                       varying = list(names(f.p)[4:(N+3)]),
                       v.names = "predicted",
                       idvar = c("month_id", "country_id"),
                       timevar = "sample_id",
                       times = 1:N)
  P.stacked$model <- "Poisson"
  rm(f.p, p.sample)
  
  # NB
  f.nb <- cbind(test[,c("month_id", "country_id", "observed")], nb.sample)
  NB.stacked <- reshape(f.nb, 
                       direction = "long",
                       varying = list(names(f.nb)[4:(N+3)]),
                       v.names = "predicted",
                       idvar = c("month_id", "country_id"),
                       timevar = "sample_id",
                       times = 1:N)
  NB.stacked$model <- "Neg Bin"
  rm(f.nb, nb.sample)

  # ZIP = zip.sample,
  f.zip <- cbind(test[,c("month_id", "country_id", "observed")], zip.sample)
  ZIP.stacked <- reshape(f.zip, 
                        direction = "long",
                        varying = list(names(f.zip)[4:(N+3)]),
                        v.names = "predicted",
                        idvar = c("month_id", "country_id"),
                        timevar = "sample_id",
                        times = 1:N)
  ZIP.stacked$model <- "ZIP"
  rm(f.zip, zip.sample)
  
  # ZINB = zinb.sample,
  f.zinb <- cbind(test[,c("month_id", "country_id", "observed")], zinb.sample)
  ZINB.stacked <- reshape(f.zinb, 
                        direction = "long",
                        varying = list(names(f.zinb)[4:(N+3)]),
                        v.names = "predicted",
                        idvar = c("month_id", "country_id"),
                        timevar = "sample_id",
                        times = 1:N)
  ZINB.stacked$model <- "ZINB"
  rm(f.zinb, zinb.sample)
  
  # TW = tw.sample
  f.tw <- cbind(test[,c("month_id", "country_id", "observed")], tw.sample)
  TW.stacked <- reshape(f.tw, 
                          direction = "long",
                          varying = list(names(f.tw)[4:(N+3)]),
                          v.names = "predicted",
                          idvar = c("month_id", "country_id"),
                          timevar = "sample_id",
                          times = 1:N)
  TW.stacked$model <- "Tweedie"
  rm(f.tw, tw.sample)
  
  # Gets the predictions from the sampled models 
  return(list(fits = fits, 
              forecasts=rbind(P.stacked, NB.stacked, ZIP.stacked,
                         ZINB.stacked,TW.stacked)))
}
