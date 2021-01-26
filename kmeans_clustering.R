
# Kmeans clustering modelling

# Authors: Ikra Mohamed and Deajra Tulloch

performKmeans<-function(){
  listings_pp<-processListings()
  
  #Make clustering dataframe including price, availability30, hostid, hostlistingscount, hostsince
  clust_dataset <- listings_pp[c(1,10,19,4,2)]
  
  clustering_dataset <- clust_dataset %>% 
    group_by(hostid) %>%                            # multiple group columns
    summarise(max_daysashost = max(hostsince),max_hostlistingscount = max(hostlistingscount), mean_price = mean(price), median_availability30 = median(availability30))  # multiple summary columns
  
  #Check for nulls incase
  colSums(is.na(clustering_dataset))
  
  #Make hostid rowname
  rownames(clustering_dataset) <- clustering_dataset$hostid
  
  #Preview
  head(clustering_dataset)
  
  # Data is scaled as we don't want the clustering algorithm to depend to an arbitrary variable unit
  
  head(clustering_dataset)
  
  #Kmeans cluster- start with 5 clusters
  k5 <- kmeans(clustering_dataset, centers = 5, nstart = 20)
  str(k5)
  k5
  
  #Metrics
  k5$withinss
  k5$tot.withinss
  k5$betweenss
  
  #Plot Cluster
  fviz_cluster(k5, geom = "point", data = clustering_dataset) + ggtitle("k = 5")
  
  #Plot cluster of all columns 
  p1<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = k5$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(median_availability30, max_daysashost, color = factor(cluster), label = hostid)) +
    geom_text()
  
  
  p2<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = k5$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(median_availability30, mean_price, color = factor(cluster), label = hostid)) +
    geom_text()
  
  p3<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = k5$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(median_availability30, max_hostlistingscount, color = factor(cluster), label = hostid)) +
    geom_text()
  
  p4<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = k5$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(max_hostlistingscount, max_daysashost, color = factor(cluster), label = hostid)) +
    geom_text()
  
  
  p5<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = k5$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(max_hostlistingscount, mean_price, color = factor(cluster), label = hostid)) +
    geom_text()
  
  p6<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = k5$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(mean_price, max_daysashost, color = factor(cluster), label = hostid)) +
    geom_text()
  
  grid.arrange(p1, p2,p3,p4,p5,p6, nrow = 3)
  
  
  #Try other cluster sizes
  k2<- kmeans(clustering_dataset, centers = 2, nstart = 20)
  k3<- kmeans(clustering_dataset, centers = 3, nstart = 20)
  k4 <- kmeans(clustering_dataset, centers = 4, nstart = 20)
  
  
  # Plot to compare
  k2_plot <- fviz_cluster(k2, geom = "point", data = clustering_dataset) + ggtitle("k = 2")
  k3_plot <- fviz_cluster(k3, geom = "point",  data = clustering_dataset) + ggtitle("k = 3")
  k4_plot <- fviz_cluster(k4, geom = "point",  data = clustering_dataset) + ggtitle("k = 4")
  k5_plot <- fviz_cluster(k5, geom = "point",  data = clustering_dataset) + ggtitle("k = 5")
  
  
  grid.arrange(k2_plot, k3_plot, k4_plot, k5_plot, nrow = 2)
  
  
  #Elbow method
  #Determine best k
  set.seed(123)
  fviz_nbclust(clustering_dataset, kmeans, method = "wss", k.max = 10, nboot = 100,)
  
  #Silhoutte value
  #Determine best k
  set.seed(123)
  fviz_nbclust(clustering_dataset, kmeans, method = "silhouette", k.max = 10, nboot = 100,)
  
  # Compute k-means clustering with optimal number of clusters k = 2
  set.seed(123)
  finalkmeans2 <- eclust(clustering_dataset, "kmeans", hc_metric="euclidean", k=2, nstart= 20)
  
  fviz_cluster(finalkmeans2, ellipse.type = "convex", geom = "point", ggtheme=theme_classic())
  
  #Internal validation: assess the fit of the cluster using silhouette width
  plot(fviz_silhouette(finalkmeans2))
  
  #Metrics
  finalkmeans2$withinss
  finalkmeans2$tot.withinss
  finalkmeans2$betweenss
  finalkmeans2$size
  
  #Evaluate cluster
  eval_cluster<-cclust(clustering_dataset, 2, dist="euclidean")
  stripes(eval_cluster)
  
  table(finalkmeans2$cluster)
  summary(finalkmeans2)
  
  #Add grouping column to clustering dataset
  clustering_dataset$grouping <-finalkmeans2$cluster
  summary(clustering_dataset)
  
  #Plot cluster of all columns 
  q1<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = finalkmeans2$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(median_availability30, max_daysashost, color = factor(cluster), label = hostid)) +
    geom_text()
  
  
  q2<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = finalkmeans2$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(median_availability30, mean_price, color = factor(cluster), label = hostid)) +
    geom_text()
  
  q3<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = finalkmeans2$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(median_availability30, max_hostlistingscount, color = factor(cluster), label = hostid)) +
    geom_text()
  
  q4<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = finalkmeans2$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(max_hostlistingscount, max_daysashost, color = factor(cluster), label = hostid)) +
    geom_text()
  
  
  q5<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = finalkmeans2$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(max_hostlistingscount, mean_price, color = factor(cluster), label = hostid)) +
    geom_text()
  
  q6<-clustering_dataset %>%
    as_tibble() %>%
    mutate(cluster = finalkmeans2$cluster,
           hostid = row.names(clustering_dataset)) %>%
    ggplot(aes(mean_price, max_daysashost, color = factor(cluster), label = hostid)) +
    geom_text()
  
  grid.arrange(q1,q2,q3,q4,q5,q6, nrow = 3)
  
  #Boxplots for each variable
  BP1<-boxplot(clustering_dataset$max_daysashost ~ clustering_dataset$grouping, main = "Days as host distribution in cluster groups",
               xlab = "Cluster",
               ylab = "daysashost",
               col = "orange")
  BP2<-boxplot(clustering_dataset$max_hostlistingscount ~ clustering_dataset$grouping, main = "Host listing count distribution in cluster groups",
               xlab = "Cluster",
               ylab = "hostlistingcount",
               col = "orange")
  BP3<-boxplot(clustering_dataset$mean_price ~ clustering_dataset$grouping, main = "Listing price distribution in cluster groups",
               xlab = "Cluster",
               ylab = "price",
               col = "orange")
  BP4<-boxplot(clustering_dataset$median_availability30 ~ clustering_dataset$grouping, main = "Median host listing availability distribution in cluster groups",
               xlab = "Cluster",
               ylab = "availability30",
               col = "orange")
  
  
  #Hiearchical clustering
  set.seed(123)
  hcluster <- hclust(dist(clustering_dataset, method = "euclidean"), method = "ward.D2")
  hcluster
  plot(hcluster, 
       main = paste('Dendrogram'),
       xlab = 'Hosts',
       ylab = 'Euclidean distances')
  
  #Cut dendrogram tree into two clusters
  hclust2 <- cutree(hcluster, k=2)
  hclust2 
  table(hclust2) 
  rect.hclust(tree=hcluster, k=2, border=2:4)
  
  #Optimal clusters
  #Elbow method
  set.seed(123)
  fviz_nbclust(clustering_dataset, hcut, method = "wss", k.max = 10, nboot = 100,)
  
  #Silhoutte value
  #Determine best k
  fviz_nbclust(clustering_dataset, hcut, method = "silhouette", k.max = 10, nboot = 100,)
  
  #Using optimal cluster
  #Cut dendrogram tree into two clusters
  set.seed(123)
  finalhcluster2 <- hcut(clustering_dataset, k = 2, hc_method = "ward.D2")
  
  # Visualize dendrogram
  plot(finalhcluster2, 
       main = paste('Dendrogram'),
       xlab = 'Hosts',
       ylab = 'Euclidean distances')
  rect.hclust(tree=finalhcluster2, k=2, border=2:4)
  
  
  #Internal validation: assess the fit of the cluster using silhouette width
  fviz_silhouette(finalhcluster2)
}

# ************************************************

# Load other R script files
source("clean_preprocess.R")
