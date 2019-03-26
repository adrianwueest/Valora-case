#clustering methods codes template - datanovia

rm(list = ls())

#packages
#########
install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")
install.packages("factoextra")
install.packages("NbClust")
library("NbClust")
library("cluster")
library("factoextra")
library("ggplot2")
library("magrittr")
library("pylr")

#---------------------------------------------------------------------------

#load and prepare the data
##########################
data("USArrests")

my_data <- USArrests %>%
  na.omit() %>%          # Remove missing values (NA)
  scale()                # Scale variables

head(my_data, n = 3)     # View the firt 3 rows

res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson") #distance measures

fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
#--------------------------------------------------------------------------
#other way to handle missing data - mice package
library("mice")
tempData <- mice(my_data,m=5, method = "pmm", seed = 1)
noNAData.set <- complete(tempData,1)
summary(noNAData.set)

#-----------------------------------------------

#partitioning clustering
########################
fviz_nbclust(my_data, kmeans, method = "gap_stat") #determining the optimal number of clusters

set.seed(123)
km.res <- kmeans(my_data, 3, nstart = 25) #k-means clustering method

fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal()) # Visualize

pam.res <- pam(my_data, 3) #PAM clustering method

fviz_cluster(pam.res) # Visualize
#--------------------------------------------------------------------------

#hierarchical clustering
########################

res.hc <- USArrests %>%          # Compute hierarchical clustering
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


set.seed(123)

res.nbclust <- USArrests %>% #optimal number of clusters
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 10, 
          method = "complete", index ="all")

fviz_nbclust(res.nbclust, ggtheme = theme_minimal()) #visualize


#----------------------------------------------------------------------------


#clustering validation statistics by silhouette plot
###################################################
set.seed(123)

res.hc <- iris[, -5] %>% # Enhanced hierarchical clustering, cut in 3 groups
  scale() %>%
  eclust("hclust", k = 3, graph = FALSE)

# Visualize with factoextra
fviz_dend(res.hc, palette = "jco",
          rect = TRUE, show_labels = FALSE)

fviz_silhouette(res.hc) #inspect the silhouette plot

# Silhouette width of observations
sil <- res.hc$silinfo$widths[, 1:3]

# Objects with negative silhouette
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]

