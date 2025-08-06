#! bash

# Training models
R CMD BATCH setup.R
R CMD BATCH modelselect.R &
R CMD BATCH modelselect-globe.R

# Validation models
R CMD BATCH modelselect-valid.R &
R CMD BATCH modelselect-globe-valid.R

R CMD BATCH modelselect-glmm-covar.R

# Scoring across the sets
R CMD BATCH scoring-cm.R
R CMD BATCH scoring-cm-valid.R

