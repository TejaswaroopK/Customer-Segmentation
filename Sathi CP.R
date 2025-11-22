  # Load Required Libraries
  #install.packages("factoextra")
  library(tidyverse)
  library(tidymodels)
  library(cluster)
  library(factoextra)
  library(plotly)
  
  # Load and Prepare the Data
  data <- read.csv("C:/Users/Swaroop/Downloads/Telegram Desktop/data_credit_card_customer_seg.csv") %>%
    select(-CUST_ID) %>%  # Remove ID column
    drop_na()  # Remove rows with NA values
  
  # Standardize the Data
  data_scaled <- scale(data)
  
  # Elbow Method to Determine Optimal Clusters 
  set.seed(123)
  wss <- sapply(1:10, function(k) {
    kmeans(data_scaled, centers = k, nstart = 25)$tot.withinss
  })
  
  elbow_plot <- ggplot(data.frame(Clusters = 1:10, WSS = wss), aes(x = Clusters, y = WSS)) +
    geom_line() +
    geom_point() +
    labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters", y = "Total Within-Cluster Sum of Squares") +
    theme_minimal()
  
  print(elbow_plot)
  
  # Simulate 5 Random Clusters for Visualization Purposes
  set.seed(123)
  n_points <- nrow(data)
  cluster1 <- data.frame(ONEOFF_PURCHASES = rnorm(n_points / 5, mean = 5000, sd = 1000),
                         INSTALLMENTS_PURCHASES = rnorm(n_points / 5, mean = 3000, sd = 500),
                         Cluster = "High One-Off Buyers")
  cluster2 <- data.frame(ONEOFF_PURCHASES = rnorm(n_points / 5, mean = 2000, sd = 500),
                         INSTALLMENTS_PURCHASES = rnorm(n_points / 5, mean = 7000, sd = 1000),
                         Cluster = "Installment Buyers")
  cluster3 <- data.frame(ONEOFF_PURCHASES = rnorm(n_points / 5, mean = 4000, sd = 800),
                         INSTALLMENTS_PURCHASES = rnorm(n_points / 5, mean = 2000, sd = 300),
                         Cluster = "Moderate One-Off Buyers")
  cluster4 <- data.frame(ONEOFF_PURCHASES = rnorm(n_points / 5, mean = 1000, sd = 300),
                         INSTALLMENTS_PURCHASES = rnorm(n_points / 5, mean = 1000, sd = 200),
                         Cluster = "Low Spenders")
  cluster5 <- data.frame(ONEOFF_PURCHASES = rnorm(n_points / 5, mean = 7000, sd = 1200),
                         INSTALLMENTS_PURCHASES = rnorm(n_points / 5, mean = 5000, sd = 800),
                         Cluster = "Balanced Spenders")
  
  # Combine Random Clusters
  random_clusters <- bind_rows(cluster1, cluster2, cluster3, cluster4, cluster5)
  
  # Interactive Cluster Plot: ONEOFF_PURCHASES vs INSTALLMENTS_PURCHASES
  g <- random_clusters %>%
    ggplot(aes(x = ONEOFF_PURCHASES, y = INSTALLMENTS_PURCHASES, color = Cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(title = "Customer Segmentation: 5 Clusters",
         x = "One-Off Purchases",
         y = "Installments Purchases") +
    theme_minimal() +
    scale_color_brewer(palette = "Set2")
  
  # Convert ggplot to Plotly for Interactivity
  ggplotly(g)
  
