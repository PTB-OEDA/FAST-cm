# final-update.R
#
# Patrick T. Brandt
# 
# 20250915 : Initial version
#            Builds off the 'Brandt-VIEWS2-Demo.Rmd' from the Startup repo
#            and the 'FAST-cm.Rmd' 
#            See those files for details and comments.
#
# This version is just about processing the final forecast training data
# into the competition forecasts for September 2025 delivery


#### Package Loads ####
library(arrow)
library(tidyr)
library(statmod)
library(tweedie)
library(magrittr)
library(glmmTMB)

#### Read Data ####

# Load data since 2009 which is what we decided to use from the earlier analysis
setwd("updated_cm_data/")
cm_2010 <- read_parquet("updated_cm_2010.parquet")
cm_2011 <- read_parquet("updated_cm_2011.parquet")
cm_2012 <- read_parquet("updated_cm_2012.parquet")
cm_2013 <- read_parquet("updated_cm_2013.parquet")
cm_2014 <- read_parquet("updated_cm_2014.parquet")
cm_2015 <- read_parquet("updated_cm_2015.parquet")
cm_2016 <- read_parquet("updated_cm_2016.parquet")
cm_2017 <- read_parquet("updated_cm_2017.parquet")
cm_2018 <- read_parquet("updated_cm_2018.parquet")
cm_2019 <- read_parquet("updated_cm_2019.parquet")
cm_2020 <- read_parquet("updated_cm_2020.parquet")
cm_2021 <- read_parquet("updated_cm_2021.parquet")
cm_2022 <- read_parquet("updated_cm_2022.parquet")
cm_2023 <- read_parquet("updated_cm_2023.parquet")
cm_2024 <- read_parquet("updated_cm_2024.parquet")
cm_2025 <- read_parquet("updated_cm_2025_01-07.parquet")

setwd("../")

# Add ID variables for countries and months
countries <- read.csv("countries.csv", header = TRUE)
month_ids <- read.csv("month_ids.csv", header = TRUE)

# Save the downloaded data for later -- so this is all in one image!
save.image("VIEWS-updated_20250915.RData")

# Put things together
library(data.table)

cm <- rbindlist(lapply(paste("cm_", 2010:2025, sep=""), get))

# Merge on the country label data
df <- merge(cm, countries, 
            by.x = "country_id", by.y="id")

# Merge on the time periods info
df <- merge(df, month_ids[,2:4],
            by.x = "month_id", by.y="month_id")

# Clean up
rm(list=c(ls(pattern="cm")))

### Variable Transforms ####

# Make 'factors' for countries, years, and months.  Defaults are integers
# and later we will want them encoded both ways
df$country_factor <- as.factor(df$isoab)
df$year_factor <- as.factor(df$Year)
df$month_factor <- factor(df$month_id)

# Make a 'real' ISO date variable so you can do time series subsetting
df$date <- ISOdate(year = df$Year, month = df$Month, day=1)

# v is a vector, will grep and return names of all matches
# v should include any of: _sb, _ns, _os, acled_fatalities, acled_fatalities, acled_battles, acled_remote, acled_civvio, acled_protests, acled_riots, acled_stratdev
# a is all other variables to keep, default is month_id and priogrid_gid
getvars <- function(v, df, a=c("month_id", "priogrid_gid")) {
  n <- colnames(df)
  matches <- unique(grep(paste(v,collapse="|"), n, value=TRUE))
  matches <- c(matches, a)
  return(matches)
}

### Selection of the variables ####
#
# Picks the data by columns!
# ADD VARIABLES TO THE ANALYSES BY INCLUDING THEM IN THE LISTS BELOW
#

# Set to sb to get the DV and lags we want via a wildcard
myvars <- "sb"  # Wildcards for the DV and related lags

geotime.ids <- c("name", "gwcode", "isoname", "isoab", "isonum",  # countryids
                 "country_id", "country_factor",
                 "in_africa", "in_middle_east",                   # Region ids
                 "year_factor", "month_factor",                   # Time ids
                 "month_id", "Month","Year", "date")

# demographic covars
demo.covars <- c("wdi_sp_pop_totl",
                 "wdi_sp_dyn_le00_in",                          
                 "wdi_sp_dyn_imrt_in",
                 "wdi_sh_dyn_mort_fe",
                 "wdi_sp_pop_0014_fe_zs",
                 "wdi_sp_pop_1564_fe_zs",
                 "wdi_sp_pop_65up_fe_zs",
                 "wdi_sp_pop_grow",
                 "wdi_sp_urb_totl_in_zs",
                 "splag_wdi_sl_tlf_totl_fe_zs",
                 "splag_wdi_sm_pop_refg_or",
                 "splag_wdi_sm_pop_netm")

# civ-mil covars
civmil.covars <- c("wdi_ms_mil_xpnd_gd_zs",                        
                   "wdi_ms_mil_xpnd_zs",
                   "vdem_v2x_ex_military")

if(myvars=="sb") {
  keeps <- getvars(v="ged_sb", df=df, 
                   a=c(geotime.ids, demo.covars, civmil.covars))
  sel.df <- as.data.frame(df)[,keeps]
}

# clean up
rm(df)

#### Subset by geo ####

# Load the raw data and just dropping everything 
# before January 2010.

sel.df <- sel.df[sel.df$Year>2009,]

# Africa and Middle East alone: this matches the pgm data (?)
africa_me <- sel.df[sel.df$in_africa==1 | sel.df$in_middle_east==1,]

# Note the "full globe" is then in 'sel.df'
globe <- sel.df
rm(sel.df)

#### Fit CM model ####

# From the formulas.R script...
frm.glmm <- as.formula("ged_sb ~ ar1(month_factor + 0|country_id) + 
                        wdi_sp_dyn_imrt_in + 
                        wdi_ms_mil_xpnd_gd_zs +
                        wdi_ms_mil_xpnd_zs +
                        vdem_v2x_ex_military")

FAST.cm <- glmmTMB(frm.glmm,
                   family = nbinom1(),
                   data=globe)

#### Forecast CM for 12 periods ####

# Loads a function for predictions from GLMMs
source("predictglmm.R")
ds <- FAST.cm$frame

# Get last 12 months of data
#dim(ds[as.numeric(ds$month_factor)>(max(as.numeric(ds$month_factor))-12),])
xforcs <- ds[as.numeric(ds$month_factor)>(max(as.numeric(ds$month_factor))-12),]

# Set the month_factor variable and then the covariates by country to match
# the grouping in the GLMM

xnew <- aggregate(xforcs[,4:7], by=list(xforcs$country_id), mean)
names(xnew)[1] <- "country_id"

# Make new country-month ids
idxs <- expand.grid(xnew$country_id, 550:561)
colnames(idxs) <- c("country_id", "month_id")

xout <- merge(idxs, xnew, by="country_id")
xout$month_factor <- as.factor(xout$month_id)
xout$ged_sb <- NA

# Generate the predictions with function loaded earlier
set.seed(324)
forcs <- predictglmm(FAST.cm, newdata = xout, N=1000)

#### Forecast Summaries ####
# Get country labels for any formatting below -- use latest
countrylabels <- globe[globe$month_id==max(globe$month_id),
                       c("country_id", "name", "isoname",
                         "isoab", "isonum", "gwcode")]

# Mean forecast for each country-month
mean.forcs <- forcs %>% group_by(country_id, month_id) %>%
  summarise(total = mean(predicted))

# Add labels to mean forecasts
mean.forcs <- merge(mean.forcs, countrylabels[,c(1,2,4)],
                    by="country_id")
names(mean.forcs)[3] <- "predicted"

# Add dates to mean forecasts
forc.idx <- data.frame(month_id = 550:561,
                       dates=seq(as.Date("2025-10-01"),
                                 by="month", length=12))

# Merge so things have correct labels
mean.forcs <- merge(mean.forcs, forc.idx, by="month_id")

# Generate cumulative mean forecasts for each country
# over the 12 months of performance.
#
# Remember this works since the mean of the sum is the sum
# of the means

cum.mean.forcs <-  mean.forcs %>% group_by(country_id) %>%
  mutate(cumulative_predicted = cumsum(predicted)) %>%
  arrange(country_id, month_id)


#### Cumulative forecast count distributions ####

tmp <- forcs %>% group_by(country_id, month_id, sample_id) %>% summarise(cp = predicted>=25) 

pover25 <- tmp %>% group_by(country_id, month_id) %>% summarise(mean(cp))

# Merge on the labels for the predictions
pover25 <- merge(pover25, countrylabels[,c(1,2,4)],
                 by="country_id")

# Reformat the dates
pover25 <- merge(pover25, forc.idx, by="month_id")

# Name the variable
names(pover25)[3] <- "Pr(>=25)"
rm(tmp)

#### Write out results ####

# Merge the mean, cumulative mean, and Pr(>=25)
out <- merge(as.data.frame(cum.mean.forcs),
             pover25[,1:3], by = c("country_id", "month_id"))

# Write the summary results out into a spreadsheet
library(writexl)
write_xlsx(x = list("Forecasts" = out),
           path = paste("FAST-cm-Forecasts-", Sys.Date(), ".xlsx", sep=""))

# Format for Section 4.1 of the requirements doc
# These are the column names used
# country_id : Country ID
# month_id : time horizon where 1 = January 1980
# outcome_n : predicted fatalities
# outcome_p : predicted probability of > 25 events
# cumulative_outcome_n : predicted cumulative fatalties

names(out)[2] <- "outcome_n"
names(out)[8] <- "outcome_p"
names(out)[7] <- "cumulative_outcome_n"

# Write the parquet file
library(arrow, quietly = TRUE)
write_parquet(out, paste("FAST-Forecast-", Sys.Date(), ".parquet", sep=""))

# Save the forecast sample
save(forcs, file = paste("ForecastSample-", Sys.Date(), ".RData", sep=""))
