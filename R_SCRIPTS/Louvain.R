# PART 5: LOUVAIN CLUSTERING
source(file = "./R_SCRIPTS/Functions.R")

# DATA
D <- load_data()

# LIBRARIES
library(tidyverse)
library(igraph)
library(ggraph)

sim_matrix <- 1 / (1 + as.matrix(dist(D$data)))

N <- 5 # set centrally in a second, but a lot of this needs moved over. 

knn_matrix <- apply(sim_matrix, 1, function(row) {
  threshold <- sort(row, decreasing = TRUE)[N]
  row[row < threshold] <- 0
  return(row)
})

G <- graph_from_adjacency_matrix(knn_matrix, mode = "undirected",
                                 weighted = TRUE)
communities <- cluster_louvain(G, weights = E(G)$weight)

V(G)$community <- membership(communities)

V(G)$name <- rownames(D$data)  

ggraph(G, layout = "fr") +
  geom_edge_link(aes(alpha = weight), show.legend = FALSE) +
  geom_node_point(aes(colour = factor(community)), size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_colour_discrete(name = "Community") +
  theme_graph()
