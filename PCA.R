wine <- read.csv(file.choose())
View(wine)

wine_1 <- wine[,-1] #removing the type column

#Normalising
wine_1 <- scale(wine_1)

cor(wine_1)

#PCA
pca_wine <- princomp(wine_1, cor = TRUE, scores = TRUE, covmat = NULL)

str(pca_wine)

summary(pca_wine)
pca_wine$loadings
plot(pca_wine)

cor(pca_wine$scores)

#Considering first 3 Principal components
wine <- cbind(wine, pca_wine$scores[,1:3]) #adding PCs in original data
wine

clus_wine <- wine[,15:17] #separating PCs in new data frame for normlaising

norm_clus <- scale(clus_wine) #normalising



##### Hierarchical Clustering #####

dist_1 <- dist(norm_clus, method = "euclidean")

fit_1 <- hclust(dist_1, method = "complete")
fit_2 <- hclust(dist_1, method = "single")
fit_3 <- hclust(dist_1, method = "average")
fit_4 <- hclust(dist_1, method = "centroid")
plot(fit_1, hang = -1)
plot(fit_2, hang = -1)
plot(fit_3, hang = -1)
plot(fit_4, hang = -1)

rect.hclust(fit_1, k = 9, border = "red")

groups_h <- cutree(fit_1,9)

membership_h <- as.matrix(groups_h)

wine_h <- cbind(membership_h,wine)

agg_h <- aggregate(wine_h[,-c(1:2,16:18)], by= list(membership_h), FUN = mean)
View(agg_h)
write.csv(agg_h, file = "Aggregate_h-Wine.csv")



##### Kmeans Clustering #####

wss = (nrow(norm_clus)-1)*sum(apply(norm_clus,2,var))
for (i in 2:8) wss[i] = sum(kmeans(norm_clus, centers = i)$withinss)
plot(1:8, wss, type = "b", xlab = "Number of Clusters", ylab = "Within group sum of squares")

#from elbow plot, we can select number of clusters = 5

fit_k <- kmeans(norm_clus,5)
str(fit_k)

wine_k <- data.frame(wine,fit_k$cluster)

agg_k <- aggregate(wine_k[,-c(1,15:18)], by=list(fit_k$cluster), FUN = mean)
View(agg_k)
write.csv(agg_k, file = "Aggregate_k-Wine.csv")
