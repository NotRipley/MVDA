# LIBRARIES
library(tidyverse)

# DATA
load(file = "Assignment3_Data.RData")
hdata <- t(data_hypertension)

# PROPORTION OF VARIANCE EXPLAINED BY PC
# Note: centering of PCs is not optional; scaling of PCs is.
# Here we use: UNSCALED PCs
# Why: The data is log normalised already. We want to preserve the level of
# normalisation that we have at the minute. 

# Run prcomp
PC_hdata <- prcomp(hdata, center = TRUE, scale. = FALSE)

# Check summary table
summ <- summary(PC_hdata_unscaled)
summ

# Compute and plot cumulative variation explained by PC
var_pc <- PC_hdata$sdev^2
tot_var_pc <- sum(PC_hdata$sdev^2)
cumulative_var_pc <- cumsum(var_pc / tot_var_pc)

ggplot() +
  geom_point(mapping = aes(x = 1:60, y = cumulative_var_pc)) +
  theme_bw() +
  labs(x = "PC Index",
       y = "Prop. Variation Explained",
       title = "Proportion of Variation Explained by PC")

# CHOICE OF PC NUMBER FOR DOWNSTREAM: 5 PCS

# IDENTIFY WHETHER SAMPLES CAN BE GROUPED BY ANY OF THE FACTORS AND IN WHICH PC 
# DIRECTIONS


