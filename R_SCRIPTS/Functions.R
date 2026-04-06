seed <- 1618

##########
# PART 1 #
#########


load_data <- function(){
  # Loads data from named file and returns as a list.
  # -------------------------------------------------
  
  e <- new.env()
  load(file = "./Assignment3_Data.RData", envir = e)
  
  return(list(
    data = t(e$data_hypertension),
    replicates = e$replicate_names,
    times = e$time_names,
    treatments = e$treatment_names
  ))
  
}

treatment_colours <- c(
  "Control" <- "grey",
  "Lisinopril" <- "steelblue",
  "Amlodipine" <- "tomato"
)

##########
# PART 2 #
##########

# Set centrally the number of principal components to carry forward, define
# function for quickly getting embeddings

npcs <- 5

get_embeddings <- function(npcs, data) {
  # Loads data using load_data() and returns embeddings in PCs of up to 
  # a specified number, set just above here. 
  # --------------------------------------------------------------------
  # npcs - the integer number of PCs up to which to store in output. 
  # data - the data to be used
  
  P <- prcomp(D$data)
  embeddings <- P$x[, 1:npcs]
  
  return(data.frame(embeddings))
  
  
}

##########
# PART 3 #
##########


estimate_k <- function(matrix, FUN, scale = FALSE, b = 100, kmax = 20, itermax = 20, 
                       nstart = 25) {
  # Takes a value/counts matrix and estimates the best number of clusters
  # via a Gap statistic method, using the cluster::clusGap function. Returns
  # list comprised of the K recommendation and a visualisation, via 
  # fviz_gap_stat().
  # ----------------------------------------------------------------------
  # matrix - matrix of values on which to perform KMeans clustering. 
  # FUN - function to perform. kmeans or hcut for example. 
  # scale - logical on whether to scale as part of this step. FALSE by default.
  # other parameters - passed to clusGap function. 
  
  if (!is.logical(scale) | scale == FALSE) {
    cat("Not scaling matrix... \n")
  } else if (scale == TRUE) {
    matrix <- scale(matrix)
    cat("Scaling matrix... \n")
  }
  
  clusGapcall <- clusGap(matrix,
                         FUNcluster = FUN,
                         nstart = 25,
                         iter.max = 20,
                         K.max = kmax,
                         B = b)
  
  k_val <- maxSE(clusGapcall$Tab[, "gap"], clusGapcall$Tab[, "SE.sim"], 
                 method = "globalmax")
  
  cat("clusGap gives ", k_val, " as our K estimate. \n")
  
  viz <- fviz_gap_stat(clusGapcall)
  
  return(list("k_val" = k_val,
              "viz" = viz))
  
}

# Set centrally the number of clusters we want to use, after running the above
# with each method.
K_kmeans <- 6
K_hier <- 6



run_kmeans <- function(matrix, K, itermax = 20, nstart = 25){
  # Runs KMeans clustering on a matrix with given K.
  # -------------------------------------------------
  # matrix - matrix to cluster on.
  # K - number of centroids.
  # other params - to pass to stats::kmeans()
  
  kmobject <- kmeans(matrix,
                     centers = K,
                     iter.max = itermax,
                     nstart = nstart)
  
  return(kmobject)
  
}

# Linkage methods for hierarchical
m <- c("average", "single", "complete", "ward")
names(m) <- m


hierarchical_check_methods <- function(x, df) {
  # Takes a method and a dataframe, then returns the agglomerative coefficient
  # resulting from that method performed on that dataframe. 
  # --------------------------------------------------------------------------
  # x - the method. "average", "single", etc.
  # df - the dataframe. Should be like an expression matrix, and suitably
  # scaled. 
  
  return(agnes(df, method = x)$ac)
  
}

method_choice <- "ward"

run_hierarchical <- function(matrix, method, K){
  # Takes a matrix and a method, performs hierarchical clustering on it. 
  # Returns the clustering call along with the resultant dendrogram, in a list.
  # ---------------------------------------------------------------------------
  # matrix - the expression matrix or similar. Should already be scaled etc. 
  # method - method to pass to cluster::agnes. "average", "single", etc.
  # K - number of centres to stats::cutree at, and return this partition.
  
  hobject <- agnes(matrix, method = method)
  
  dendrogram <- pltree(hobject,
                       main = "Dendrogram",
                       cex = 0.5,
                       hang = -1)
  
  clustering <- cutree(as.hclust(hobject), k = K)
  
  return(list(
    "hobject" = hobject,
    "cluster" = clustering,
    "dendrogram" = dendrogram
  ))
  
  
}

evaluate_partition <- function(matrix, labels, FUN, kmax = 20, b = 100) {
  # Takes a partition of a dataset and computes several cluster evaluation 
  # metrics: Silhouette score, Gap statistic, Variance Ratio Criterion. 
  # -----------------------------------------------------------------------
  # matrix - original value matrix that was clustered
  # labels - partition
  # FUN - cluster used to calc Gap, as it must be refit. 
  
  # Silhouette
  whole_silhouette <- cluster::silhouette(labels, dist = dist(matrix))
  avg_silhouette_width <- mean(whole_silhouette[, 3])
  
  # Calinski-Harabasz / VRC
  calinhara <- fpc::calinhara(matrix, labels)
  
  # Gap - must refit to evaluate but seed is specified so this is valid. 
  gap_stat <- cluster::clusGap(matrix,
                               FUNcluster = FUN,
                               K.max = kmax,
                               B = b)$Tab[length(unique(labels)), "gap"]
  
  return(list("Silhouette" = avg_silhouette_width,
              "VRC" = calinhara,
              "Gap" = gap_stat))
}


