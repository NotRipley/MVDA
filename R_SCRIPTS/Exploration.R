# PART 1: DATA and EXPLORATION
source("Functions.R")

# LIBRARIES
library(tidyverse)

# DATA (under "data_hypertension")
# At point of loading is in Genes x Samples format. 
load(file = "Assignment3_Data.RData")

hyperdata <- t(data_hypertension)


# WHAT ARE THE MIN, MAX, MEAN, SD EXPRESSION OF EACH GENE? (Detect columns
# with no expression)
summary_table <- hyperdata |> 
  as.data.frame() |> 
  summarise(across(everything(), list(
    min = min,
    max = max,
    mean = mean,
    sd = sd
  ))) |>
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_sep = "_(?=[^_]+$)",
    values_to = "value"
  ) |>
  pivot_wider(
    names_from = variable,
    values_from = value
  ) |> 
  t()

colnames(summary_table) <- summary_table[1, ]
summary_table <- summary_table[-1, ] |> 
  as.data.frame() |> 
  mutate(across(everything(), as.numeric))

# PROPORTION OF VARIANCE EXPLAINED BY PC
# Note: centering of PCs is not optional; scaling of PCs is.
# Here we use:
# Why:



