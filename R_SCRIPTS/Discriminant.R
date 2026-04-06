# PART 4: DISCRIMINANT ANALYSIS
source(file = "./R_SCRIPTS/Functions.R")

# LIBRARIES
library(tidyverse)
library(MASS)

# USING PC SCORES AS VARIABLES, GET A LINEAR DISCRIMINANT ANALYSIS RULE TO 
# DISCRIMINATE BETWEEN CELLS AT 12 HOURS (see file)

# Get embeddings and data
D <- load_data()
embeddings <- get_embeddings(npcs, data = D$data)
embeddings$times <- as.factor(D$times)

LDA <- MASS::lda(times ~ PC1 + PC2 + PC3 + PC4 + PC5, data = (embeddings))

predicted <- predict(LDA)$class

table(D$times, predicted)
chisq.test(table(D$times, predicted))

# This is our confusion matrix. We see perfect performance from our discriminant
# rule. 

embeddings$predicted <- predicted

# HOW MANY PARAMETERS NEED TO BE ESTIMATED TO DERIVE THIS RULE? COMPARE IT TO 
# THE NUMBER OF SAMPLES

# From the notes, the number of estimated parameters is p(p+1)/2 + pg, with our
# number of observations being np. In our case, p = 5, n = 60, g = 2. 
# so n_params = 25; n_obs = 300. So we're alright.

# RATIO OF SAMPLES TO PARAMETERS NUMBER SHOULD BE >1 - VERIFY


#DISCUSS CLASSIFICATION ERRORS

# There are no classification errors. This suggests very well-separated data,
# and that we have selected enough PCs to remove any overlaps completely.

