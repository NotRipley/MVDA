# PART 3: CLUSTERING
source(file = "./R_SCRIPTS/Functions.R")

# LIBRARIES
library(tidyverse) # misc
library(cluster) # kmeans, hierarchical, silhouette
library(factoextra) # hierarchical
library(mclust) # ari
library(fpc) # calinhara

# DATA
D <- load_data()

# CLUSTER SAMPLES WITH AN APPROPRIATE DISTANCE METRIC 
# We will use squared euclidean distance, the one used as a default in a lot
# of algorithms

# =====================================================
# IDENTIFY THE MOST APPROPRIATE NUMBER OF CLUSTERS (ELBOW OR GAP)
# =====================================================

# N.B. we are carrying forward only five PCs from our PC section.

P <- prcomp(D$data)
embeddings <- get_embeddings(npcs = npcs, data = D$data)

# =====================================================
# COMPARE CLUSTERING RESULTS ON TWO DIFFERENT METHODS
# =====================================================

# =====================================================
# KMEANS
# =====================================================

K_kmeans <- estimate_k(embeddings, FUN = kmeans, b = 100)

K_clustering <- run_kmeans(embeddings, K = K_kmeans)

ggplot(data = embeddings) +
  geom_point(mapping = aes(x = PC1, y = PC2, colour = as.factor(K_clustering$cluster)))

# =====================================================
# HIERARCHICAL
# =====================================================

# Check which method yields the highest "agglomerative coefficient", this 
# being "ward".
(sapply(m, function(m_val) hierarchical_check_methods(m_val, df = embeddings)))

# Choose an optimal K for use with hierarchical clustering, this also being 6. 
K_hier <- estimate_k(embeddings, hcut)

# Choice of method based on agglomerative coefficient from cluster::agnes()
method_choice = "ward"

# Run the clustering.
H_clustering <- run_hierarchical(matrix = embeddings, method = method_choice, 
                                 K = K_hier)

# =====================================================
# DISCUSS WHETHER WE CAN DESCRIBE OUR CLUSTERS IN TERMS OF THE COVARIATES: TIME.
# TREATMENT, REPLICATES
# =====================================================

# This is adding the partitions onto the df, clustering and then colouring by 
# each of our covariates. 

# KMeans first
table(K_clustering$cluster, D$treatments)
table(K_clustering$cluster, D$times)

chisq.test(table(K_clustering$cluster, D$treatments))
chisq.test(table(K_clustering$cluster, D$times))

# Hierarchical next - need to cuttree first.
table(H_clustering$cluster, D$treatments)
table(H_clustering$cluster, D$times)

chisq.test(table(H_clustering$cluster, D$treatments))
chisq.test(table(H_clustering$cluster, D$times))

# Wow, these are really similar! Are they the same partition?
table(H_clustering$cluster, K_clustering$cluster)
# Yes, the table is decoupled. To really force the point:
HK_ari <- mclust::adjustedRandIndex(H_clustering$cluster, K_clustering$cluster)

# =====================================================
# EVALUATE CLUSTERING PERFORMANCE
# =====================================================

H_eval <- evaluate_partition(embeddings, H_clustering$cluster, FUN = hcut)

K_eval <- evaluate_partition(embeddings, K_clustering$cluster, FUN = kmeans)



