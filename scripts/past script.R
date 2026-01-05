# Lista dei pacchetti
packages <- c(
  "e1071",
  "cluster",
  "factoextra",
  "stats",
  "clustertend",
  "NbClust",
  "fpc",
  "gridExtra",
  "grid",
  "gamlss",
  "mclust",
  "datasets",
  "tidyverse",
  "gridExtra",
  "gclus",
  "KernSmooth"
)

# Installa i pacchetti
install.packages(packages, dependencies = TRUE)

# Carica i pacchetti installati
library(e1071)
library(cluster)
library(factoextra)
library(stats)
library(clustertend)
library(NbClust)
library(fpc)
library(gridExtra)
library(grid)
library(gamlss)
library(mclust)
library(datasets)   # contains the data
library(tidyverse)  # data manipulation and visualization
library(gridExtra)  # plot arrangement
library(gclus) # for the wine data
library("KernSmooth")
#Set seed if needed
set.seed(123)

attach(diabetes)
str(diabetes)

head(diabetes)

table(diabetes$class)

round(table(diabetes$class)/length(diabetes$class)*100, digits=2)
Chemical Normal Overt
24.83 52.41 22.76
pie(table(diabetes$class)/length(diabetes$class)*100, col = c("green","blue
", "red"))


#CODE TO ANALIZE A VARIABLE


summary(diabetes$glucose)

sd(diabetes$glucose)

var(diabetes$glucose)

(quantile(diabetes$glucose,0.75)) - (quantile(diabetes$glucose,0.25))

hist(diabetes$glucose, freq=FALSE, main = "Histogram of Glucose", xlab = "
mg/100ml·h")
lines(density(diabetes$glucose), col="red")

fit.gamma <- histDist(diabetes$glucose, family=GA, nbins=21, main="Gamma d
istribution")
fit.t = histDist(diabetes$glucose, family=TF, nbins=21, main="t-Student di
stribution")
fit.sep2 = histDist(diabetes$glucose, family=SEP2, nbins=21, main="Skew Po
wer Exponential type 2 distribution")
fit.st5 = histDist(diabetes$glucose, family=ST5, nbins=21, main="Skew t ty
pe 5")

data.frame(row.names=c("Gamma", "t-Student", "SEP2", "ST5"),
           AIC=c(AIC(fit.gamma), AIC(fit.t), AIC(fit.sep2),AIC(fit.st5)),
           BIC=c(fit.gamma$sbc, fit.t$sbc, fit.sep2$sbc, fit.st5$sbc))

boxplot(diabetes$glucose, main = "Boxplot of Glucose", ylab = "mg/100ml·h")

boxplot.stats(diabetes$glucose)$out

#CODE TO ANALYZE THE VARIABLE

summary(diabetes$insulin)

sd(diabetes$insulin)

var(diabetes$insulin)

(quantile(diabetes$insulin,0.75)) - (quantile(diabetes$insulin,0.25))

hist(diabetes$insulin, freq=FALSE, main = "Histogram of Insulin", xlab = "
μU/ml·h")
lines(density(diabetes$insulin), col="red")

fit.gamma <- histDist(diabetes$insulin, family=GA, nbins=21, main="Gamma d
istribution")
fit.t = histDist(diabetes$insulin, family=TF, nbins=21, main="t-Student di
stribution")
fit.sep2 = histDist(diabetes$insulin, family=SEP2, nbins=21, main="Skew Po
wer Exponential type 2 distribution")
fit.st5 = histDist(diabetes$insulin, family=ST5, nbins=21, main="Skew t ty
pe 5")


#do this for all variables 

#Univariate analysis 

normal_data = diabetes[diabetes$class == "Normal", ]
chemical_data = diabetes[diabetes$class == "Chemical", ]
overt_data = diabetes[diabetes$class == "Overt", ]

skewness(normal_data$glucose)
kurtosis(normal_data$glucose, type = 1)


#Multivariate analysis 

(cor(diabetes[2:4]))
pr.out = prcomp(diabetes[2:4], scale=TRUE)
pr.out

pr.out$sdev
pr.out$center
pr.out$scale

pr.out$rotation = -pr.out$rotation
pr.out$rotation

pr.out$x = -pr.out$x
head(pr.out$x)

fviz_pca_ind(pr.out, title = "PCA - Diabetes Data", legend = "top",
             habillage = diabetes$class, palette = c("green","blue","red"),
             geom = "point", ggtheme = theme_classic())

#Biplot, Kaiser’s rule and Scree plot

biplot(pr.out, scale=0, cex = 0.5)

pr.var = pr.out$sdev^2
pr.var

par(mfrow=c(1,2)) #Set the plotting area into a 1*2 array
plot(pve, main = "Scree plot", xlab="Principal Component", ylab="Proportion
of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), main ="Cumulative Scree plot", xlab="Principal Component"
     , ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

#Cluster Analysis 

scaled.df = scale(diabetes[2:4]) # Standardize the dataset
pairs(scaled.df, pch = 21)

fviz_pca_ind(pr.out, title = "PCA - Diabetes Data", legend = "bottom",
             habillage = diabetes$class, palette = c("green", "blue","red"),
             geom = "point", ggtheme = theme_classic())


fviz_pca_ind(prcomp(scaled.random_df), title = "PCA - Random data",
             geom = "point", ggtheme = theme_classic())

hopkins(scaled.df, n = nrow(scaled.df)-1)
$H

hopkins(scaled.random_df, n = nrow(random_df)-1)

#To calculate the distance we used the euclidean method (the same used in the paper):
dist.eucl = dist(scaled.df, method = "euclidean")
round(as.matrix(dist.eucl)[1:6, 1:6], 2)

fviz_dist(dist.eucl, show_labels = FALSE

#And we can compare it with the distance matrix of the randomnly generated dataset:
fviz_dist(dist(scaled.random_df, method = "euclidean"), show_labels =FALSE)
fviz_nbclust(scaled.df, hcut, method = "wss") +
geom_vline(xintercept = 3, linetype = 2) +
labs(subtitle = "Hierarchical: Elbow method")
          
fviz_nbclust(scaled.df, hcut, method = "silhouette") +
labs(subtitle = "Hierarchical: Silhouette method")
          
grid.arrange(arrangeGrob(fviz_nbclust(NbClust(scaled.df, distance="euclide
an", method="single")), top=textGrob("Single linkage method with euclidean
distance",gp=gpar(fontsize=17))),
             arrangeGrob(fviz_nbclust(NbClust(scaled.df, distance="euclide
an", method="average")), top=textGrob("Average linkage method with euclide
an distance",gp=gpar(fontsize=17))),
             arrangeGrob(fviz_nbclust(NbClust(scaled.df, distance="euclide
an", method="complete")), top=textGrob("Complete linkage method with eucli
dean distance",gp=gpar(fontsize=17))),
             arrangeGrob(fviz_nbclust(NbClust(scaled.df, distance="euclide
an", method="ward.D")), top=textGrob("Ward.D linkage method with euclidean
distance",gp=gpar(fontsize=17))),
             arrangeGrob(fviz_nbclust(NbClust(scaled.df, distance="euclide
an", method="ward.D2")), top=textGrob("Ward.D2 linkage method with euclide
an distance",gp=gpar(fontsize=17))),
             arrangeGrob(fviz_nbclust(NbClust(scaled.df, distance="euclide
an", method="centroid")), top=textGrob("Centroid linkage method with eucli
dean distance",gp=gpar(fontsize=17))), ncol = 2, nrow = 3)

4.3.2.2 For K-means
fviz_nbclust(scaled.df, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "K-Means: Elbow method")

fviz_nbclust(scaled.df, kmeans, method = "silhouette") +
  labs(subtitle = "K-Means: Silhouette method")

fviz_nbclust(scaled.df, kmeans, nstart = 25, method = "gap_stat", nboot = 5
             00) + labs(subtitle = "K-Means: Gap statistic method")

#As we can see also using NbClust library the optimal number of clusters is 3:
  fviz_nbclust(NbClust(scaled.df, distance = "euclidean", min.nc = 2, max.nc
                       = 10, method = "kmeans"))
#4.3.2.3 For K-Medoids
fviz_nbclust(scaled.df, cluster::pam, method = "wss") +
geom_vline(xintercept = 3, linetype = 2) +
labs(subtitle = "K-Medoids: Elbow method")
  
fviz_nbclust(scaled.df, cluster::pam, method = "silhouette") +
  labs(subtitle = "K-Medoids: Silhouette method")  

fviz_nbclust(scaled.df, cluster::pam, method = "gap_stat", nboot = 500) +
  labs(subtitle = "K-Medoids: Gap statistic method")

#Hierarchical clustering

# Single linkage method 

hc.single = hclust(d = dist.eucl, method = "single")
fviz_dend(hc.single, cex = 0.5, main="Single linkage method")

cor(dist.eucl, cophenetic(hc.single))

hc.average = hclust(d = dist.eucl, method = "average")
fviz_dend(hc.average, cex = 0.5, main="Average linkage method")

cor(dist.eucl, cophenetic(hc.average))

#Complete linkage method
hc.complete = hclust(d = dist.eucl, method = "complete")
fviz_dend(hc.complete, cex = 0.5, main="Complete linkage method")

cor(dist.eucl, cophenetic(hc.complete))

#Ward.D linkage method
hc.wardd = hclust(d = dist.eucl, method = "ward.D")
fviz_dend(hc.wardd, cex = 0.5, main="Ward.D linkage method")

#Ward.D2 linkage method
hc.wardd2 = hclust(d = dist.eucl, method = "ward.D2")
fviz_dend(hc.wardd2, cex = 0.5, main="Ward.D2 linkage method")

cor(dist.eucl, cophenetic(hc.wardd2))

#Centroid linkage method
hc.centroid = hclust(d = dist.eucl, method = "centroid")
fviz_dend(hc.centroid, cex = 0.5, main="Centroid linkage method")

cor(dist.eucl, cophenetic(hc.centroid))

#Average linkage method
grp.average = cutree(hc.average, k = 3)
fviz_dend(hc.average, k = 3, cex = 0.5, k_colors = c("green", "blue", "red
"), color_labels_by_k = TRUE, rect = TRUE, main="Average linkage method")

#Complete linkage method
grp.complete = cutree(hc.complete, k = 3)
fviz_dend(hc.complete, k = 3, cex = 0.5, k_colors = c("green", "blue", "re
d"),color_labels_by_k = TRUE, rect = TRUE, main="Complete linkage method")

#Ward.D2 linkage method
grp.wardd2 = cutree(hc.wardd2, k = 3)
fviz_dend(hc.wardd2, k = 3, cex = 0.5, k_colors = c("green", "blue", "red"
), color_labels_by_k = TRUE, rect = TRUE, main="Ward.D2 linkage method")

#Visualize Clusters

#Scatterplot matrix on the original variables using the Class Variable
pairs(scaled.df, main = "Glucose, Insulin and Sspg for Classes", pch = 21,
      bg = c("blue", "green", "red")[unclass(diabetes$class)])

#Scatterplot matrix on the original variables with Average
pairs(scaled.df, main = "Glucose, Insulin and Sspg with Average", pch = 19,
      col=c("green", "blue", "red")[grp.average])

#Scatterplot matrix on the original variables with Complete
pairs(scaled.df, main = "Glucose, Insulin and Sspg with Complete", pch = 1
      9, col=c("green", "blue", "red")[grp.complete])

#Scatterplot matrix on the original variables with Ward.D2
pairs(scaled.df, main = "Glucose, Insulin and Sspg with Ward.D2", pch = 19,
      col=c("green", "blue", "red")[grp.wardd2])

#Cluster plot of the first 2 PCs with Average linkage method
fviz_cluster(list(data = scaled.df, cluster = grp.average),
             palette = c("green", "blue", "red"), ellipse.type = "convex",
             repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_classic(),
             main = "Cluster plot the first 2 PCs with Average method")

#Cluster plot of the first 2 PCs with Complete linkage method
fviz_cluster(list(data = scaled.df, cluster = grp.complete),
             palette = c("green", "blue", "red"), ellipse.type = "convex",
             repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_classic(),
             main = "Cluster plot the first 2 PCs with Complete method")

#Cluster plot of the first 2 PCs with Ward.D2 linkage method
fviz_cluster(list(data = scaled.df, cluster = grp.wardd2),
             palette = c("green", "blue", "red"),ellipse.type = "convex",
             repel = TRUE, show.clust.cent = FALSE, ggtheme = theme_classic(),
             main = "Cluster plot the first 2 PCs with Ward.D2 method")

#Paritioning clustering

km.res = kmeans(scaled.df, 3, nstart = 50)

print(km.res)
K-means clustering with 3 clusters of sizes 106, 17, 22

cl <- km.res$cluster
pairs(scaled.df, main = "Cluster Plot K-Means", pch = 19, col=c("green", "
blue", "red")[cl])

fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = c("green", "blue", "red"), ggtheme = theme_classic(),
             main = "Cluster plot on the first 2 PCs with K-Means")

# K-Medoids

pam.eucl = pam(scaled.df, 3, metric = "euclidean")
print(pam.eucl)

cl = pam.eucl$clustering
pairs(scaled.df, main = "Cluster Plot K-Medoids", pch = 19, col=c("green",
                                                                  "blue")[cl])
fviz_cluster(pam.eucl, ellipse.type = "norm",
             palette = c("green","blue","red"), ggtheme = theme_classic(),
             main = "Cluster plot on the first 2 PCs with K-Medoids")

#For Hierarchical Clustering


hc.res = eclust(scaled.df, "hclust", k = 3, hc_metric = "euclidean", hc_me
                thod = "ward.D2", graph = FALSE)

fviz_silhouette(hc.res, palette = c("green","blue","red"), ggtheme = theme
                _classic())

#The Average Silhouette Widths of the clusters (close to 1 is well clustered):
hc.res$silinfo$clus.avg.widths

hc.res$silinfo$avg.width

neg_sil_index = which(hc.res$silinfo$widths[, "sil_width"] < 0)
hc.res$silinfo$widths[neg_sil_index, , drop = FALSE]

hc_stats = cluster.stats(dist(scaled.df), hc.res$cluster)
hc_stats$dunn

#confusion matrix 

table(diabetes$class, hc.res$cluster)

class = as.numeric(diabetes$class)
hc.clust_stats = cluster.stats(d = dist(scaled.df), class, hc.res$cluster)

#rand index

hc.clust_stats$corrected.rand

#melia's VI index

hc.clust_stats$vi

#repeat the analysis 

#Choosing the best Clustering Algorithm

intern = clValid(scaled.df, nClust = 2:10, clMethods = c("hierarchical","k
means","pam"), validation = "internal")
summary(intern)

stab = clValid(scaled.df, nClust = 2:10, clMethods = c("hierarchical","kme
ans","pam"), validation = "stability")
summary(stab)

class = factor(diabetes$class)
pairs(scaled.df, gap=0, pch = 16, col = as.numeric(class))

mod = Mclust(scaled.df, G = 1:9, modelNames = NULL)

summary(mod$BIC)

fviz_mclust(mod, "BIC", palette = "jco")

summary(mod)

head(round(mod$z, 6))

pairs(scaled.df, gap=0, pch = 16, col = mod$classification)

table(class, mod$classification)

adjustedRandIndex(class, mod$classification)

fviz_mclust(mod, "classification", geom = "point", pointsize = 1.5, palett
            e = c("green","blue","red"))

fviz_mclust(mod, "uncertainty", palette = c("green","blue","red"))

fviz_mclust(mod, "uncertainty", palette = c("green","blue","red"), choose.
            vars = c("glucose", "insulin"))

fviz_mclust(mod, "uncertainty", palette = c("green","blue","red"), choose.
            vars = c("glucose", "sspg"))

fviz_mclust(mod, "uncertainty", palette = c("green","blue","red"), choose.
            vars = c("sspg", "insulin"))


