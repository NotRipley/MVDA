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
selection = c("sample", "times", "treatments")

long_data <- as.data.frame(D$data) |> 
  add_column("times" = D$times, 
             "treatments" = D$treatments) |> 
  rownames_to_column(var = "sample") |> 
  pivot_longer(cols = !any_of((selection)),
               names_to = "gene",
               values_to = "expression")

ggplot(data = long_data) +
  facet_wrap(~times, scales = "free_x", ncol = 2) +
  geom_boxplot(mapping = aes(x = sample, y = expression, fill = treatments,
                             group = sample)) +
  scale_fill_manual(values = treatment_colours, name = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


# PLOT A TIME COURSE FOR A GENE IN DIFFERENT TREATMENTS

time_data <- as.data.frame(D$data[, 1]) |> 
  rename(expression = 1) |> 
  add_column("times" = D$times,
             "treatments" = D$treatments)

ggplot(data = time_data) +
  geom_jitter(mapping = aes(x = times, y = expression, colour = treatments), width = 0.1) +
  geom_line(mapping = aes(x = times, y = expression, colour = treatments,
                          group = treatments), linewidth = 0.8) +
  scale_colour_manual(values = treatment_colours) +
  theme_bw()
  


# COMMENT ON WHETHER THERE ARE ANY OBVIOUS OUTLIERS AMONG SAMPLES












