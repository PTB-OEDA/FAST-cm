# predictglmm.R
#
# Patrick T. Brandt
#
# 20250718 : Generated predictions across GLMM models
#
#

predictglmm <- function(fit, newdata, N)
{
  n <- nrow(newdata)
  # Poisson
  if(fit$modelInfo$family$family=="poisson")
  {
      tmp.p <- predict(fit, newdata,
                       type = "response", allow.new.levels=TRUE)
      local.fc <- sapply(1:n, function(i) {rpois(N, tmp.p[i])})
      
      mname <- "Poisson GLMM"
  }
  
  # Tweedie
  if(fit$modelInfo$family$family=="tweedie")
  {
      tmp.p <- predict(fit, newdata,
                       type = "response", allow.new.levels=TRUE)
      local.fc <- t(replicate(N, mgcv::rTweedie(tmp.p, p=family_params(fit))))
      mname <- "Tweedie GLMM"
  }
  
  # Negbin
  if(fit$modelInfo$family$family=="nbinom1")
  {
    theta <- sigma(fit)
    tmp.p <- predict(fit, newdata,
                     type = "response", allow.new.levels=TRUE)
      local.fc <- sapply(1:n, function(i) {rnbinom(N, size=theta, mu=tmp.p[i])})
      mname <- "Neg Binom GLMM"
  }
  
  # Return the correctly simulated object as stack GLMM results for scoring
  test <- newdata[,c("month_id", "country_id", "ged_sb")]
  colnames(test) <- c("month_id", "country_id", "observed")
  
  local.fc <- cbind(test, t(local.fc))
  out.stacked <- reshape(local.fc, 
                       direction = "long",
                       varying = list(names(local.fc)[4:(N+3)]),
                       v.names = "predicted",
                       idvar = c("month_id", "country_id"),
                       timevar = "sample_id",
                       times = 1:N)
  out.stacked$model <- mname
  
  return(out.stacked)
}
