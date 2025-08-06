# formulas.R
#
# Model formulas so they can be called once and edited once!

# Set up regression models for the glm-alikes
# Base effects for country and month
frm <- as.formula("ged_sb ~ country_id + month_id")

# Dynamics or lags only model
frm1 <- as.formula("ged_sb ~ ged_sb_tlag_1 + ged_sb_tlag_2 +
                             ged_sb_tlag_3 + ged_sb_tlag_4 +
                             ged_sb_tlag_5 + ged_sb_tlag_6")

# Covariate blocks model + lags
frm.covar <- as.formula("ged_sb ~ 
                         ged_sb_tlag_1 + ged_sb_tlag_2 +
                         ged_sb_tlag_3 + ged_sb_tlag_4 +
                         ged_sb_tlag_5 + ged_sb_tlag_6 +
                         wdi_sp_dyn_le00_in + 
                         wdi_sp_dyn_imrt_in + 
                         splag_wdi_sm_pop_refg_or + 
                         splag_wdi_sm_pop_netm + 
                         wdi_ms_mil_xpnd_gd_zs +
                         wdi_ms_mil_xpnd_zs +
                         vdem_v2x_ex_military")

frm.glmm <- as.formula("ged_sb ~ ar1(month_factor + 0|country_id) + 
                        wdi_sp_dyn_imrt_in + 
                        wdi_ms_mil_xpnd_gd_zs +
                        wdi_ms_mil_xpnd_zs +
                        vdem_v2x_ex_military")
