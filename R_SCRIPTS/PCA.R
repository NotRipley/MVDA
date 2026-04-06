# PART 2: PCA
source(file = "./R_SCRIPTS/Functions.R")

# LIBRARIES
library(tidyverse)

# DATA
D <- load_data()

# PROPORTION OF VARIANCE EXPLAINED BY PC
# Note: centering of PCs is not optional; scaling of PCs is.
# Here we use: UNSCALED PCs
# Why: The data is log normalised already. We want to preserve the level of
# normalisation that we have at the minute. 

# Run prcomp
PC_hdata <- prcomp(D$data, center = TRUE, scale. = FALSE)

# Check summary table
summ <- summary(PC_hdata)
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

# IDENTIFY WHETHER SAMPLES CAN BE GROUPED BY ANY OF THE FACTORS (treatment,
# time, replicate) AND IN WHICH PC DIRECTIONS
PC_hdata_scores <- as.data.frame(PC_hdata$x) |>
  rownames_to_column("sample") |>
  add_column("times" = D$times,
             "treatments" = D$treatments,
             "replicates" = D$replicates)

ggplot(PC_hdata_scores, aes(x = PC1, y = PC2, colour = treatments, shape = factor(times))) +
  geom_point(size = 3) +
  scale_colour_manual(values = treatment_colours) +
  theme_bw()



  


