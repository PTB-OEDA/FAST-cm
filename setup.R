# setup.R
#
# Patrick T. Brandt
#
# 20250623 : Initial version
# 20250714 : Update for adding training + dev sets

### Load data  and Merges ####

# Load the data -- these are the result of Brandt-VIEWS2-Demo.Rmd
load("VIEWS-alldownloaded.RData")

# Merge on the country label data
df <- merge(cm, countries, 
            by.x = "country_id", by.y="id")

# Merge on the time periods info
df <- merge(df, month_ids[,2:4],
            by.x = "month_id", by.y="month_id")

# Clean up
rm(cm,list=c(ls(pattern="dl"), ls(pattern="dest")))

### Variable Transforms ####

# Make 'factors' for countries, years, and months.  Defaults are integers
# and later we will want them encoded both ways
df$country_factor <- as.factor(df$isoab)
df$year_factor <- as.factor(df$Year)
df$month_factor <- factor(df$month_id)

# Make a 'real' ISO date variable so you can do time series subsetting
df$date <- ISOdate(year = df$Year, month = df$Month, day=1)

### Subsetting Functions ####

# Now load functions from D'Orazio for aligning data for 
# train-validate-test splits

# Can get more of the functions in that script via
# source("https://raw.githubusercontent.com/vjdorazio/forecasting-workshop/refs/heads/main/views_utils.R")

# s is a number 1-12, df must have 3 cols named: ged_sb, month_id, and isoname
# function creates a dataframe with 4 cols: pred_ged_sb, pred_month_id, month_id, isoname
# returns merge with original dataframe.  
# This is via https://github.com/vjdorazio/forecasting-workshop/blob/main/views_utils.R
builddv <- function(s, df) {
  out <- df
  df <- df[,c("ged_sb", "month_id", "country_id")]
  colnames(df)[1] <- "pred_ged_sb"
  df$pred_month_id <- df$month_id
  df$month_id <- df$month_id - s
  out <- merge(out, df, by=c("country_id", "month_id"), all.x=TRUE)
  out <- na.omit(out) 
  return(out)
}

# v is a vector, will grep and return names of all matches
# v should include any of: _sb, _ns, _os, acled_fatalities, acled_fatalities, acled_battles, acled_remote, acled_civvio, acled_protests, acled_riots, acled_stratdev
# a is all other variables to keep, default is month_id and priogrid_gid
getvars <- function(v, df, a=c("month_id", "priogrid_gid")) {
  n <- colnames(df)
  matches <- unique (grep(paste(v,collapse="|"), n, value=TRUE))
  matches <- c(matches, a)
  return(matches)
}

# function that returns matrix of prediction cutoffs
# assumes that df has a column 'predict'
# assumes cuts is an ordinal array with cuts greaters than 0 and less than 1
# if cuts is NA everything less than 1 is 0 and returns 1 column for preds
# could be adjusted to allow cuts greater than 1 cut down to 1 or 0
buildpred <- function(cuts, df) {
  
  if(any(is.na(cuts))) {
    pred <- matrix(df$predict, nrow=nrow(df), ncol=1)
    pred[which(df$predict < 1),1] <- 0
    pred[,1] <- round(pred[,1], digits=0)
    return(pred)
  }
  
  if(cuts==-1) {
    pred <- matrix(df$predict, nrow=nrow(df), ncol=1)
    pred[which(df$predict < 0),1] <- 0
    return(pred)
  }
  
  # pred is the matrix that holds different values that correspond to different
  # thresholds for predicting 0 and 1 (everything else is simply)
  nc <- length(cuts) + 1
  pred <- matrix(df$predict, nrow=nrow(df), ncol=nc)
  
  for(ci in 1:nc) {
    if(ci==nc) {
      pred[which(df$predict < 1),ci] <- 0
      pred[,ci] <- round(pred[,ci], digits=0)
    } else {
      pred[which(df$predict <= cuts[ci]),ci] <- 0
      pred[which(df$predict > cuts[ci] & df$predict < 1),ci] <- 1
      pred[,ci] <- round(pred[,ci], digits=0)
    }
  }
  return(pred)
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
demo.covars <- c("wdi_sp_dyn_le00_in",                          
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
  sel.df <- df[,keeps]
}

# clean up
rm(df)

### Subset train-validate-test ####

# Load the raw data and just dropping everything 
# before January 2010.

sel.df <- sel.df[sel.df$Year>2009,]

# Africa and Middle East alone: this matches the pgm data (?)
africa_me <- sel.df[sel.df$in_africa==1 | sel.df$in_middle_east==1,]

# Note the "full globe" is then in 'sel.df'
globe <- sel.df
rm(sel.df)

#### Time Assumptions ####
# Expect data for June 2025 and will be making 
# forecasts for October 2025 (s=1) through September 2026 (s=12). 
# Since we don’t have Sept 2025 yet, move back to end our test 
# data with Sept 2024. 

#### Train subset ####

# Train, predictors: Jan 2010 – June 2020
# Train, DV: Oct 2020 when s=1, Sep 2021 when s=12
globe.train <- globe[globe$date<ISOdate(year=2020, month=7, day=1),]
globe.train.eval <- globe[globe$date>ISOdate(year=2020, month=10, day=1) &
                          globe$date<ISOdate(year=2021, month=9, day=1),]

africa_me.train <- africa_me[africa_me$date<ISOdate(year=2020, month=7, day=1),]
africa_me.train.eval <- africa_me[africa_me$date>ISOdate(year=2020, month=10, day=1) &
                            africa_me$date<ISOdate(year=2021, month=9, day=1),]

#### Validate subset ####

# Valid, predictors: July 2020 – June 2022
# Valid, DV: Oct 2022 when s=1, Sep 2023 when s=12
globe.valid <- globe[globe$date<ISOdate(year=2022, month=7,day=1),]
globe.valid.eval <- globe[globe$date>ISOdate(year=2022, month=9, day=1) &
                          globe$date<ISOdate(year=2023, month=10, day=1),]

africa_me.valid <- africa_me[africa_me$date<ISOdate(year=2022, month=7,day=1),]
africa_me.valid.eval <- africa_me[africa_me$date>ISOdate(year=2022, month=9, day=1) &
                            africa_me$date<ISOdate(year=2023, month=10, day=1),]

#### Test subset ####
# Test, predictors: input predictors for July 2022 - June 2023
# Test, DV: Oct 2023 (s=1), Sep 2024 (s=12)
globe.test <- globe[globe$date<ISOdate(year=2023, month=7,day=1),]
globe.test.eval <- globe[globe$date>ISOdate(year=2023, month=9,day=1) & 
                           globe$date<ISOdate(year=2024, month=10, day=1),]

africa_me.test <- africa_me[africa_me$date<ISOdate(year=2023, month=7,day=1),]
africa_me.test.eval <- africa_me[africa_me$date>ISOdate(2023, month=9, day=1) & 
                                   africa_me$date<ISOdate(year=2024, month=10, day=1),]

### Save Image of Datasets ###
save.image("cm_subsets.RData")

q(save="no")