# PART 1: DATA and EXPLORATION
source(file = "./R_SCRIPTS/Functions.R")

# LIBRARIES
library(tidyverse)

# DATA
D <- load_data()

# HISTOGRAM OF LOG GENE EXPRESSION IN A SAMPLE / FOR A PARTICULAR GENE
ggplot() +
  geom_histogram(mapping = aes(x = D$data[1,]), bins = 40, fill = "steelblue", colour = "white") +
  theme_bw() +
  labs(x = "log Gene Expression",
       y = "Frequency",
       title = "Sa6C1: log Gene Expression Histogram")

# BOXPLOT OF LOG GENE EXPRESSION IN A SAMPLE / FOR A PARTICULAR GENE
data_temp <- D$data
data_temp$sample <- rownames(D$data)

ggplot() +
  geom_boxplot(mapping = aes())

# PLOT A TIME COURSE FOR A GENE IN DIFFERENT TREATMENTS

# COMMENT ON WHETHER THERE ARE ANY OBVIOUS OUTLIERS AMONG SAMPLES












