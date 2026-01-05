###Package and data import
lib <- c(
  "moments", "gamlss", "psych", "corrplot", "ggplot2", "factoextra", 
  "hopkins", "NbClust", "stats", "fpc", "cluster", "clValid", "mclust",
  "nnet", "here", "dplyr", "GGally", "ggcorrplot", "car", "smotefamily",
  "skimr", "Metrics", "patchwork", "stringr", "gamlss.dist","factoextra","caret", "seriation","fclust"
) #defining the list of the package we are using
#install.packages(lib, dependencies = TRUE) #installing all the packages
lapply(lib, library, character.only = TRUE) #loading all the lybraries 
insurance <- read.csv(here("data/processed", "insurance_data.csv")) #importing the dataset insurance using the here package which use the directory of the project as a starting point
dataset<- insurance #we will use dataset as the data frame to work with 

#set.seed(123)
#sdata_index <- createDataPartition(dataset$claim, p = 0.2, list = FALSE)
#dataset  <- dataset[sdata_index, ]

###Data Cleaning
rownames ( dataset )= dataset [ ,2] #selecting PatientID as rowname
dataset<-dataset[,-1] #removing PatientID
dataset<-dataset[,-1] #Removing INdex
dataset <- na.omit(dataset) # removing the 5 row with missing value IMPORTANT 
dataset <- dataset[dataset$region != "", ] # removing 3 row with region = "" 
#defining a function to calculate automatically the number of bins
num_bins <- function(dataset, variable) {
  # Scott's rule to estimate the number of bins for histograms based on standard deviation and sample size.
  # The formula aims to find an optimal bin width that represents the distribution of the data without oversimplifying or creating excessive detail.
  
  n <- length(dataset[[variable]])
  bin_width <- 3.5 * sd(dataset[[variable]]) / (n ^ (1/3))
  num_bins <- (max(dataset[[variable]]) - min(dataset[[variable]])) / bin_width
  return(as.integer(num_bins))
}



#I could use winsorizing to manage outliers but since we have an health dataset i prefer to mantain the original data

name<-names(dataset) #this is needed to select the name of all the variables
#("age"  "gender"  "bmi"   "bloodpressure" "diabetic"  "children"  "smoker"  "region" "claim")

### Univariate Analysis 
skim(dataset) # producing a quick analysis of the dataset 


#Univariate Analysis foe every variable 

#age 
variable<-name[1]
print(variable)
summary(select(dataset,variable))
mean(dataset[,variable])
var(dataset[,variable])
sd(dataset[,variable])
skewness(dataset[,variable])
kurtosis(dataset[,variable])
plot_title <- str_to_title(variable)
boxplot(dataset[,1], main = paste("Boxplot",plot_title))
nb<-num_bins(dataset,variable)
histogram<-hist(dataset[,variable], main = paste("Histogram of", plot_title), xlab = plot_title, ylab = "Frequency", prob = TRUE, breaks = nb, col = "grey")
lines(density(dataset[,variable]), col = "red")

# Calculate the histogram to get the maximum density or frequency
hist_data <- hist(dataset[, variable], plot = FALSE, breaks = nb, prob = TRUE)

# Find the maximum density or frequency
max_f <- max(hist_data$density)

# Fit various distributions to the data and plot the histograms with fit lines
fit_gaussian <- histDist(dataset[, variable], family = NO, nbins = nb, 
                         main = "Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.40))

fit_logno <- histDist(dataset[, variable], family = LOGNO, nbins = nb, 
                      main = "Log-Normal Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))

fit_gamma <- histDist(dataset[, variable], family = GA, nbins = nb, 
                      main = "Gamma Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))

fit_exponential <- histDist(dataset[, variable], family = EXP, nbins = nb, 
                            main = "Exponential Distribution", xlab = plot_title, 
                            ylab = "Frequency", ylim = c(0, max_f * 1.40))

fit_invgauss <- histDist(dataset[, variable], family = IG, nbins = nb, 
                         main = "Inverse-Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.40))

# Calculate the AIC for each fitted distribution
AIC_values <- c(AIC(fit_gaussian), AIC(fit_logno), AIC(fit_gamma), AIC(fit_exponential), AIC(fit_invgauss))
BIC_values <- c(fit_gaussian$sbc,fit_logno$sbc,fit_gamma$sbc,fit_exponential$sbc,fit_invgauss$sbc)
# Create a data frame to display the AIC results
goodness_of_fit <- data.frame(
  Distribution = c("Gaussian", "Log-Normal", "Gamma", "Exponential", 
                   "Inverse-Gaussian"),
  AIC = AIC_values,
  BIC = BIC_values
)

# Display the results
print(goodness_of_fit)

#gender 
variable<-name[2]
print(variable)
table(select(dataset,variable))  
plot_title <- str_to_title(variable)
boxplot(dataset$clai ~ dataset$gender, main = paste("Boxplot",plot_title), ylab = "Claim")
relfreq <- table(dataset[,variable])/length(dataset[,variable])
relfreq
barplot(relfreq, main = paste("Barplot of", plot_title))


#bmi
variable<-name[3]
print(variable) 
summary(select(dataset,variable))
mean(dataset[,variable])
var(dataset[,variable])
sd(dataset[,variable])
skewness(dataset[,variable])
kurtosis(dataset[,variable])
plot_title <- str_to_title(variable)
boxplot(dataset[,variable], main = paste("Boxplot",plot_title))
nb<-num_bins(dataset,variable)
hist(dataset[,variable], main = paste("Histogram of", plot_title), xlab = plot_title, ylab = "Frequency", prob = TRUE, breaks = nb, col = "grey")
lines(density(dataset[,variable]), col = "red")
# Fit various distributions to the data and plot the histograms with fit lines
hist_data <- hist(dataset[, variable], plot = FALSE, breaks = nb, prob = TRUE)
max_f <- max(hist_data$density)
fit_gaussian <- histDist(dataset[, variable], family = NO, nbins = nb, 
                         main = "Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_logno <- histDist(dataset[, variable], family = LOGNO, nbins = nb, 
                      main = "Log-Normal Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_gamma <- histDist(dataset[, variable], family = GA, nbins = nb, 
                      main = "Gamma Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_exponential <- histDist(dataset[, variable], family = EXP, nbins = nb, 
                            main = "Exponential Distribution", xlab = plot_title, 
                            ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_invgauss <- histDist(dataset[, variable], family = IG, nbins = nb, 
                         main = "Inverse-Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.40))
AIC_values <- c(AIC(fit_gaussian), AIC(fit_logno), AIC(fit_gamma), AIC(fit_exponential), AIC(fit_invgauss))
BIC_values <- c(fit_gaussian$sbc,fit_logno$sbc,fit_gamma$sbc,fit_exponential$sbc,fit_invgauss$sbc)

goodness_of_fit <- data.frame(
  Distribution = c("Gaussian", "Log-Normal", "Gamma", "Exponential", 
                   "Inverse-Gaussian"),
  AIC = AIC_values,
  BIC = BIC_values
)
print(goodness_of_fit)

#bloodpressure
variable<-name[4]
print(variable)
summary(select(dataset,variable))   
summary(select(dataset,variable))
mean(dataset[,variable])
var(dataset[,variable])
sd(dataset[,variable])
skewness(dataset[,variable])
kurtosis(dataset[,variable])
plot_title <- str_to_title(variable)
boxplot(dataset[,4], main = paste("Boxplot",plot_title))
nb<-num_bins(dataset,variable)
hist(dataset[,variable], main = paste("Histogram of", plot_title), xlab = plot_title, ylab = "Frequency", prob = TRUE, breaks = nb, col = "grey")
lines(density(dataset[,variable]), col = "red")
# Fit various distributions to the data and plot the histograms with fit lines
hist_data <- hist(dataset[, variable], plot = FALSE, breaks = nb, prob = TRUE)
max_f <- max(hist_data$density)
fit_gaussian <- histDist(dataset[, variable], family = NO, nbins = nb, 
                         main = "Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_logno <- histDist(dataset[, variable], family = LOGNO, nbins = nb, 
                      main = "Log-Normal Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_gamma <- histDist(dataset[, variable], family = GA, nbins = nb, 
                      main = "Gamma Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_exponential <- histDist(dataset[, variable], family = EXP, nbins = nb, 
                            main = "Exponential Distribution", xlab = plot_title, 
                            ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_invgauss <- histDist(dataset[, variable], family = IG, nbins = nb, 
                         main = "Inverse-Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.40))
AIC_values <- c(AIC(fit_gaussian), AIC(fit_logno), AIC(fit_gamma), AIC(fit_exponential), AIC(fit_invgauss))
BIC_values <- c(fit_gaussian$sbc,fit_logno$sbc,fit_gamma$sbc,fit_exponential$sbc,fit_invgauss$sbc)

goodness_of_fit <- data.frame(
  Distribution = c("Gaussian", "Log-Normal", "Gamma", "Exponential", 
                   "Inverse-Gaussian"),
  AIC = AIC_values,
  BIC = BIC_values
)
print(goodness_of_fit)

#diabetic
variable<-name[5]
print(variable)
table(select(dataset,variable))   
plot_title <- str_to_title(variable)
boxplot(dataset$claim ~ dataset[,5], main = paste("Boxplot",plot_title), xlab=plot_title, ylab="Claim")
relfreq <- table(dataset[,variable])/length(dataset[,variable])
relfreq
barplot(relfreq, main = paste("Barplot of", plot_title))

#children
variable<-name[6]
print(variable)
table(select(dataset,variable))
summary(select(dataset,variable))
mean(dataset[,variable])
var(dataset[,variable])
sd(dataset[,variable])
skewness(dataset[,variable])
kurtosis(dataset[,variable])
plot_title <- str_to_title(variable)
boxplot(dataset[,6], main = paste("Boxplot",plot_title))
nb<-num_bins(dataset,variable)
hist(dataset[,variable], main = paste("Histogram of", plot_title), xlab = plot_title, ylab = "Frequency", prob = TRUE, breaks = nb, col = "grey")
lines(density(dataset[,variable]), col = "red")
relfreq <- table(dataset[,variable])/length(dataset[,variable])
relfreq
barplot(relfreq, main = paste("Barplot of", plot_title))

#smoker
variable<-name[7]
print(variable)
table(select(dataset,variable))  #this is to create a vertical output 
plot_title <- str_to_title(variable)
boxplot(dataset$claim ~ dataset[,7], main = paste("Boxplot",plot_title), xlab=plot_title, ylab="Claim")
relfreq <- table(dataset[,variable])/length(dataset[,variable])
relfreq
barplot(relfreq, main = paste("Barplot of", plot_title))

#region
variable<-name[8]
print(variable)
table(select(dataset,variable))  #this is to create a vertical output 
plot_title <- str_to_title(variable)
boxplot(dataset$claim ~ dataset[,8], main = paste("Boxplot",plot_title), xlab=plot_title, ylab="Claim")
relfreq <- table(dataset[,variable])/length(dataset[,variable])
relfreq
barplot(relfreq, main = paste("Barplot of", plot_title))

#claim 
variable<-name[9]
print(variable)
summary(select(dataset,variable))  #this is to create a vertical output 
plot_title <- str_to_title(variable)
boxplot(dataset[,9], main = paste("Boxplot",plot_title))
nb<-num_bins(dataset,variable)
hist_data <- hist(dataset[, variable], plot = FALSE, breaks = nb, prob = TRUE)
max_f <- max(hist_data$density)
hist(dataset[,variable], main = paste("Histogram of", plot_title), xlab = plot_title, ylab = "Frequency", prob = TRUE, breaks = nb, col = "grey",ylim = c(0, max_f * 1.40))
lines(density(dataset[,variable]), col = "red")
# Fit various distributions to the data and plot the histograms with fit lines
fit_gaussian <- histDist(dataset[, variable], family = NO, nbins = nb, 
                         main = "Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_logno <- histDist(dataset[, variable], family = LOGNO, nbins = nb, 
                      main = "Log-Normal Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_gamma <- histDist(dataset[, variable], family = GA, nbins = nb, 
                      main = "Gamma Distribution", xlab = plot_title, 
                      ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_exponential <- histDist(dataset[, variable], family = EXP, nbins = nb, 
                            main = "Exponential Distribution", xlab = plot_title, 
                            ylab = "Frequency", ylim = c(0, max_f * 1.40))
fit_invgauss <- histDist(dataset[, variable], family = IG, nbins = nb, 
                         main = "Inverse-Gaussian Distribution", xlab = plot_title, 
                         ylab = "Frequency", ylim = c(0, max_f * 1.60))
AIC_values <- c(AIC(fit_gaussian), AIC(fit_logno), AIC(fit_gamma), AIC(fit_exponential), AIC(fit_invgauss))
BIC_values <- c(fit_gaussian$sbc,fit_logno$sbc,fit_gamma$sbc,fit_exponential$sbc,fit_invgauss$sbc)

goodness_of_fit <- data.frame(
  Distribution = c("Gaussian", "Log-Normal", "Gamma", "Exponential", 
                   "Inverse-Gaussian"),
  AIC = AIC_values,
  BIC = BIC_values
)
print(goodness_of_fit)


#Creating the numeric dataset 
n_data <- dataset[, sapply(dataset, is.numeric)]
head(n_data)
cov_matrix<-cov(n_data) #creating the covariance amtrix
cov_matrix
corr_matrix <- cor(n_data) #creating the correlation matrix
corr_matrix
pairs.panels(n_data, hist.col="skyblue", density = TRUE, ellipses = FALSE)


#PCA first method
scaled_data <- scale(n_data, center = TRUE, scale = TRUE) # scale data
eigen_R <- eigen(corr_matrix) # eigen decomposition of scaled data
eigen_R
phi <- -eigen_R$vectors # loadings multiplied by -1 for graphical interpretation ATTENZIONE PERCHé NON LO USI NEI PLOT ADESSO
rownames(phi) <- c(names(n_data))
colnames(phi) <- c("PC1", "PC2", "PC3", "PC4", "PC5")
phi
var_pca <- eigen_R$values # variability explained by PCs
var_pca



#PCA second method

pca_data <- prcomp(n_data, scale=TRUE)
pca_data


# Choosing the number of principal component

PVE <- var_pca/sum(var_pca)
PVE <- round(PVE, 3)
PVE
CPVE <- cumsum(PVE) #cumulative variability explained.
CPVE

barplot(CPVE)
abline(h = 0.8, col = "red", lwd = 1.5)

benchmark <- sum(var_pca) / length(var_pca)

which(var_pca > 1)

CPVE_plot <- qplot(c(1:5), CPVE) +
  geom_line() +
  xlab("Principal Components") +
  ylab("CPVE") +
  ggtitle("Cumulative Scree Plot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0.8, col = "red", linetype = "dashed")
CPVE_plot # CPVE in relation to PCs number

PVE_plot <- qplot(c(1:5), PVE) +
  geom_line() +
  xlab("Principal Components") +
  ylab("PVE") +
  ggtitle("Scree Plot") +
  ylim(0, 1) +
  theme(plot.title = element_text(hjust = 0.5))
PVE_plot # PVE in relation to PCs number


#Interpretation

final_phi <- phi[,c(1:3)]
final_phi


set.seed(123)
biplot(pca_data, scale=0)
sdata_index <- createDataPartition(dataset$gender, p = 0.2, list = FALSE)
sdata  <- n_data[sdata_index, ]
pca_sdata <- prcomp(sdata, scale=TRUE)
biplot(pca_sdata, scale=0)


pca_data$x <- pca_data$x
final_phi <- as.data.frame(pca_data$x[, 1:3])
fviz_pca_ind(pca_data, geom.ind = "point", habillage = dataset$smoker, palette = c("green","blue","red","yellow"))

pc_data <- data.frame(PC1 = pca_data$x[, 1], PC2 = pca_data$x[, 2], smoker = dataset$smoker)
ggplot(pc_data, aes(x = PC1, y = PC2, color = smoker)) + 
  geom_point() + 
  scale_color_manual(values = c("green","blue","red","yellow")) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlab("First Principal Component") + 
  ylab("Second Principal Component") + 
  ggtitle("First Two Principal Components")


#Cluster ANALYSIS

##Assessment of cluster tendency

random_data <- apply(scaled_data,2, function(x){runif(length(x),min = min(x),max = max(x))})
random_data <- as.data.frame(random_data)
random_data <- apply(random_data,2,scale)
pairs.panels(scaled_data, hist.col="skyblue", density = TRUE, ellipses = FALSE) #observed data
pairs.panels(random_data, hist.col="violetred2", density = TRUE, ellipses = FALSE) #randomly-distributed data

hopkins(scaled_data) #we have clustered structure since the value is near 1
hopkins(random_data) #there are no cluster in random data 

eucldist_scaled <- dist(scaled_data, method = "euclidean") # Creation of the distance matrix according to the Euclidian distance
fviz_dist(eucldist_scaled, show_labels = FALSE) + labs(title = "Original data")
head(eucldist_scaled)

#library(seriation) to create an IVAT map, a better version of the previous plot 
ivat_order <- seriate(eucldist_scaled, method = "VAT")
order <- get_order(ivat_order)
eucldist_scaled_ivat <- as.dist(as.matrix(eucldist_scaled)[order, order])
fviz_dist(eucldist_scaled_ivat, show_labels = FALSE) + labs(title = "IVAT Map")

fviz_dist(dist(random_data), show_labels = FALSE) + labs(title = "Random data")


#HC: Manhattan distance and single linkage criterion

#Computing the distance matrix
manhdist_scaled <- dist(scaled_data, method = "manhattan")


nb_single <- NbClust(scaled_data, distance = "manhattan",min.nc = 2, max.nc = 10, method = "single") # Choose K
counts <- table(nb_single$Best.nc[1,])
barplot(counts,
        main = "Number of Clusters (Single Linkage)",
        xlab = "Number of Clusters",
        ylab = "Frequency",
        col = "lightblue")

manhdata_hc1 <- hclust(d = manhdist_scaled, method = "single")
manhdata1_tree <- cutree(manhdata_hc1, k = 2) # Identify clusters
table(manhdata1_tree)
# NON FUNGE fviz_dend(manhdata_hc1, k = 2, cex = 0.3,main = "Manhattan distance + Single Linkage Criterion",k_colors = c("#2E9FDF", "#FC4E07"),rect = TRUE) # Dendrogram

pairs(scaled_data, gap = 0, pch = manhdata1_tree,col = c("#2E9FDF", "#FC4E07")[manhdata1_tree])

cor(manhdist_scaled, cophenetic(manhdata_hc1)) # Cophenetic distance


#HC: Manhattan distance and average linkage criterion

nb_average <- NbClust(scaled_data, distance = "manhattan",min.nc = 2, max.nc = 10, method = "average") # Choose K
counts <- table(nb_average$Best.nc[1,])

barplot(counts,main = "Number of Clusters (Average Linkage)",xlab = "Number of Clusters",ylab = "Frequency",col = "lightblue") # Barplot

manhdata_hc2 <- hclust(d = manhdist_scaled, method = "average")
manhdata2_tree <- cutree(manhdata_hc2, k = 3) # Identify clusters
table(manhdata2_tree)
fviz_dend(manhdata_hc2, k = 3, cex = 0.3,
          main = "Manhattan distance + Average Linkage Criterion",
          k_colors = c("#2E9FDF", "#FC4E07","green"),
          rect = TRUE) # Dendrogram
pairs(scaled_data, gap = 0, pch = manhdata2_tree,
      col = c("#2E9FDF", "#FC4E07","green")[manhdata2_tree])

cor(manhdist_scaled, cophenetic(manhdata_hc2)) # Cophenetic distance

#HC: Manhattan distance and Complete linkage criterion

nb_complete <- NbClust(scaled_data, distance = "manhattan",min.nc = 2, max.nc = 10, method = "complete") # Choose K
counts <- table(nb_complete$Best.nc[1,])

barplot(counts,main = "Number of Clusters (Complete Linkage)",xlab = "Number of Clusters",ylab = "Frequency",col = "lightblue") # Barplot

manhdata_hc3 <- hclust(d = manhdist_scaled, method = "complete")
manhdata3_tree <- cutree(manhdata_hc3, k = 2) # Identify clusters
table(manhdata3_tree)
fviz_dend(manhdata_hc3, k = 2, cex = 0.3,
          main = "Manhattan distance + Complete Linkage Criterion",
          k_colors = c("#2E9FDF", "#FC4E07"),
          rect = TRUE) # Dendrogram
pairs(scaled_data, gap = 0, pch = manhdata3_tree,
      col = c("#2E9FDF", "#FC4E07")[manhdata3_tree])

cor(manhdist_scaled, cophenetic(manhdata_hc3)) # Cophenetic distance




#HC: Manhattan distance and centroid criterion

nb_centroid <- NbClust(scaled_data, distance = "manhattan",min.nc = 2, max.nc = 10, method = "centroid") # Choose K
counts <- table(nb_centroid$Best.nc[1,])

barplot(counts,main = "Number of Clusters (Centroid Linkage)",xlab = "Number of Clusters",ylab = "Frequency",col = "lightblue") # Barplot

manhdata_hc4 <- hclust(d = manhdist_scaled, method = "centroid")
manhdata4_tree <- cutree(manhdata_hc4, k = 2) # Identify clusters
table(manhdata4_tree)
fviz_dend(manhdata_hc4, k = 2, cex = 0.3,
          main = "Manhattan distance + Centroid Linkage Criterion",
          k_colors = c("#2E9FDF", "#FC4E07"),
          rect = TRUE) # Dendrogram
pairs(scaled_data, gap = 0, pch = manhdata4_tree,
      col = c("#2E9FDF", "#FC4E07")[manhdata4_tree])

cor(manhdist_scaled, cophenetic(manhdata_hc4)) # Cophenetic distance


#HC: Manhattan distance and Ward’s distance method

nb_ward <- NbClust(scaled_data, distance = "manhattan",min.nc = 2, max.nc = 10, method = "ward.D2") # Choose K
counts <- table(nb_ward$Best.nc[1,])

barplot(counts,main = "Number of Clusters (Ward's distance)",xlab = "Number of Clusters",ylab = "Frequency",col = "lightblue") # Barplot

manhdata_hc5 <- hclust(d = manhdist_scaled, method = "ward.D2")
manhdata5_tree <- cutree(manhdata_hc5, k = 2) # Identify clusters
table(manhdata5_tree)
fviz_dend(manhdata_hc5, k = 2, cex = 0.3,
          main = "Manhattan distance + Ward's distance Criterion",
          k_colors = c("#2E9FDF", "#FC4E07"),
          rect = TRUE) # Dendrogram
pairs(scaled_data, gap = 0, pch = manhdata5_tree,
      col = c("#2E9FDF", "#FC4E07")[manhdata5_tree])

cor(manhdist_scaled, cophenetic(manhdata_hc5)) # Cophenetic distance

#Partitional clustering: K-means

nb_kmeans <- NbClust(scaled_data, distance = "manhattan",min.nc = 2, max.nc = 10, method = "kmeans") # Choose K
counts <- table(nb_kmeans$Best.nc[1,])
barplot(counts,main = "Number of Clusters (K-means)",xlab = "Number of Clusters",ylab = "Frequency",col = "lightblue") # Barplot
set.seed(12345)
kmeans_r <- kmeans(scaled_data, centers = 2, nstart = 25)
kmeans_r$size
pairs(scaled_data, gap = 0, pch = kmeans_r$cluster, col = c("#2E9FDF", "#FC4E07")[kmeans_r$cluster])

#Partitional clustering: K-medoids (PAM)

# Use the elbow method to determine the number of clusters using PAM
fviz_nbclust(scaled_data, FUNcluster = pam, method = "wss") +
  labs(subtitle = "PAM elbow method") +
  geom_vline(xintercept = 5, linetype = 2)

# Use the silhouette method to determine the number of clusters using PAM
fviz_nbclust(scaled_data, FUNcluster = pam, method = "silhouette") +
  labs(subtitle = "PAM silhouette method")

# Use the gap statistic method to determine the number of clusters using PAM
fviz_nbclust(scaled_data, FUNcluster = pam, method = "gap_stat") +
  labs(subtitle = "PAM gap statistic method")

# Perform K-medoids clustering with k = 2 and Manhattan metric
kmedoids <- pam(scaled_data, k = 2, metric = "manhattan")

# Display a table of cluster assignments
table(kmedoids$clustering)

# Visualize the K-medoids cluster plot
fviz_cluster(kmedoids, data = scaled_data,
             palette = c("#2E9FDF", "#FC4E07"),
             ellipse.type = "euclid", star.plot = TRUE,
             repel = TRUE, ggtheme = theme_minimal(),
             main = "K-medoids cluster plot")

#Cluster Validation

set.seed(123)
kmeans.data <- eclust(scaled_data, "kmeans", k = 2, nstart = 25, graph = FALSE)
table(kmeans.data$cluster)
fviz_silhouette(kmeans.data, palette = "jco", ggtheme = theme_classic())

sil_kmeans <- silhouette(kmeans.data$cluster, manhdist_scaled)
neg_sil_kmeans <- which(sil_kmeans[, "sil_width"] < 0)
neg_sil_kmeans # Units characterized by negative silhouette widths

set.seed(123)
kmedoids.data <- eclust(scaled_data, "pam", k = 2, nstart = 25, graph = FALSE)
table(kmedoids.data$clustering)
fviz_silhouette(kmedoids.data, palette = "jco", ggtheme = theme_classic())

sil_kmedoids <- silhouette(kmedoids.data$clustering, manhdist_scaled)
neg_sil_kmedoids <- which(sil_kmedoids[, "sil_width"] < 0)
neg_sil_kmedoids # Units characterized by negative silhouette widths

kmeans_stats <- cluster.stats(manhdist_scaled, kmeans.data$cluster)
kmeans_stats$dunn

kmedoids_stats <- cluster.stats(manhdist_scaled, kmedoids.data$clustering)
kmedoids_stats$dunn

cluster_methods <- c("hierarchical", "kmeans", "pam")
internal <- clValid(scaled_data, nClust = 2, clMethods = cluster_methods,validation = "internal")

summary(internal)

cluster_methods <- c("hierarchical","kmeans","pam")
stability <- clValid(scaled_data, nClust = 2, clMethods = cluster_methods,validation = "stability")

summary(stability)


#MODEL BASED CLUSTERING 

model_based <- Mclust(scaled_data)
fviz_mclust(model_based, "BIC", palette = "jco")
summary(model_based$BIC)
summary(model_based)

head(model_based$z, 30) # Soft assignment
head(model_based$classification, 200) # Hard assignment

table(model_based$classification)
pairs(scaled_data, gap = 0, pch = 16, col = model_based$classification)


#Fuzzy clustering 

# Use the dataset `n_data` with all numerical variables

# Perform fuzzy clustering with all numerical columns
fkm_result <- FKM(X = n_data, stand = 1, RS = 10, seed = 264)

# Display the number of clusters chosen and the criterion
fkm_result$k
fkm_result$criterion

# Perform fuzzy clustering with the XB index
fkm_result_XB <- FKM(X = n_data, stand = 1, RS = 10, seed = 264, index = "XB")
fkm_result_XB$criterion

# Perform fuzzy clustering with the PC index
fkm_result_PC <- FKM(X = n_data, stand = 1, RS = 10, seed = 264, index = "PC")
fkm_result_PC$criterion

# Perform fuzzy clustering with K=2
fkm_result_2 <- FKM(X = n_data, k = 2, stand = 1, RS = 10, seed = 264)

# Display the first results of the membership degree for each cluster
head(fkm_result_2[["clus"]])

# Display the size of the clusters
cl.size(fkm_result_2$U)

# Calculate the raw entropy
Hraw(fkm_result_2$X, fkm_result_2$H)

# Determine the max membership cluster for each observation
max_membership_cluster <- apply(fkm_result_2$clus, 1, which.max)

# Plot the membership degrees for the first two clusters
clus1 <- fkm_result_2$clus[, 1]
clus2 <- fkm_result_2$clus[, 2]

# Create a data frame for plotting
plot_data <- data.frame(n_data, clus1, clus2)

# Plot using ggplot2
ggplot(plot_data, aes(x = clus1, y = clus2)) +
  geom_point(aes(color = as.factor(max_membership_cluster)), alpha = 0.6) +
  labs(title = "Fuzzy Clustering Results", x = "Cluster 1 Membership Degree", y = "Cluster 2 Membership Degree") +
  scale_color_discrete(name = "Max Membership Cluster") +
  theme_minimal()

# Pairwise plot of the original data colored by the max membership cluster
pairs.panels(n_data, col = max_membership_cluster, main = "Pairwise Plot of Original Data")


#BABUSKA

# Use the dataset `n_data` with all numerical variables

# Perform fuzzy clustering with the babushka method
fkm_result <- FKM(X = n_data, stand = 1, RS = 10, seed = 264, m = 1.5)

# Display the number of clusters chosen and the criterion
fkm_result$k
fkm_result$criterion

# Perform fuzzy clustering with the XB index and babushka method
fkm_result_XB <- FKM(X = n_data, stand = 1, RS = 10, seed = 264, index = "XB", m = 1.5)
fkm_result_XB$criterion

# Perform fuzzy clustering with the PC index and babushka method
fkm_result_PC <- FKM(X = n_data, stand = 1, RS = 10, seed = 264, index = "PC", m = 1.5)
fkm_result_PC$criterion

# Perform fuzzy clustering with K=2 and babushka method
fkm_result_2 <- FKM(X = n_data, k = 2, stand = 1, RS = 10, seed = 264, m = 1.5)

# Display the first results of the membership degree for each cluster
head(fkm_result_2[["clus"]])

# Display the size of the clusters
cl.size(fkm_result_2$U)

# Calculate the raw entropy
Hraw(fkm_result_2$X, fkm_result_2$H)

# Determine the max membership cluster for each observation
max_membership_cluster <- apply(fkm_result_2$clus, 1, which.max)

# Plot the membership degrees for the first two clusters
clus1 <- fkm_result_2$clus[, 1]
clus2 <- fkm_result_2$clus[, 2]

# Create a data frame for plotting
plot_data <- data.frame(n_data, clus1, clus2)

# Plot using ggplot2
ggplot(plot_data, aes(x = clus1, y = clus2)) +
  geom_point(aes(color = as.factor(max_membership_cluster)), alpha = 0.6) +
  labs(title = "Fuzzy Clustering Results", x = "Cluster 1 Membership Degree", y = "Cluster 2 Membership Degree") +
  scale_color_discrete(name = "Max Membership Cluster") +
  theme_minimal()

# Pairwise plot of the original data colored by the max membership cluster
pairs.panels(n_data, col = max_membership_cluster, main = "Pairwise Plot of Original Data")







#ESPERIMENTI FALLIMENTARI 
#Gower's index

prova <- dataset
prova$gender <- ifelse(prova$gender == "male", 1, 0)
prova$diabetic <- ifelse(prova$diabetic == "Yes", 1, 0)
prova$smoker <- ifelse(prova$smoker == "Yes", 1, 0)
region_means <- aggregate(claim ~ region, data = prova, FUN = mean)
prova <- merge(prova, region_means, by = "region", suffixes = c("", "_mean"))
prova$region <- prova$claim_mean
prova$claim_mean <- NULL

pca_prova <- prcomp(prova, scale=TRUE)
summary(pca_prova)

# Plot the proportion of variance explained by each component
screeplot(pca_prova, type = "lines", main = "Scree Plot")

# Calculate the cumulative proportion of variance explained
explained_variance <- cumsum(pca_prova$sdev^2 / sum(pca_prova$sdev^2))

# Plot cumulative explained variance
plot(explained_variance, type = "b", xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance Explained",
     main = "Cumulative Variance Explained")
abline(h = 0.9, col = "red", lty = 2) # for example, to see where 90% of variance is explained

pca_data <- data.frame(pca_prova$x[, 1:4])

# Perform hierarchical clustering on PCA data using Manhattan distance and centroid linkage
pca_manhdist_scaled <- dist(pca_data, method = "manhattan")

# Determine the optimal number of clusters
nb_centroid <- NbClust(pca_data, distance = "manhattan",min.nc = 2, max.nc = 10, method = "ward.D2")
counts <- table(nb_centroid$Best.nc[1,])
barplot(counts, main = "Number of Clusters (Centroid Linkage)", xlab = "Number of Clusters", ylab = "Frequency", col = "lightblue")

# Perform hierarchical clustering on PCA data
pca_manhdata_hc4 <- hclust(d = pca_manhdist_scaled, method = "centroid")

# Identify clusters
pca_manhdata4_tree <- cutree(pca_manhdata_hc4, k = 2)
table(pca_manhdata4_tree)

# Visualize the dendrogram
fviz_dend(pca_manhdata_hc4, k = 2, cex = 0.3, main = "Manhattan distance + Centroid Linkage Criterion",
          k_colors = c("#2E9FDF", "#FC4E07"), rect = TRUE)

# Pairs plot to visualize the clusters
pairs(pca_data, gap = 0, pch = pca_manhdata4_tree, col = c("#2E9FDF", "#FC4E07")[pca_manhdata4_tree])

# Calculate the cophenetic correlation coefficient
cor(pca_manhdist_scaled, cophenetic(pca_manhdata_hc4))

#codice paolo
{
playlist_name <- data_playlist$playlist_name #useful for comparison later
play_FKM <- FKM(X = data_playlist[2:7], stand = 1, RS = 10, seed = 264) #we use the default k (2:6)
play_FKM$k
play_FKM$criterion
play_FKM_XB <- FKM(X = data_playlist[2:7], stand = 1, RS = 10, seed = 264, index = "XB")
play_FKM_XB$criterion
play_FKM_PC <- FKM(X = data_playlist[2:7], stand = 1, RS = 10, seed = 264, index = "PC") #the higher
play_FKM_PC$criterion # K=2 is bes
play_FKM_2 <- FKM(X = data_playlist[2:7] , k = 2, stand = 1, RS = 10, seed = 264)
head(play_FKM_2[["clus"]]) #we have the membership degree for each cluster
cl.size(play_FKM_2$U)
Hraw(play_FKM_2$X, play_FKM_2$H) 
table(playlist_name, play_FKM_2$clus[,1])
playlist_name2 <- c(playlist_name[1],playlist_name[102],
                    playlist_name[202],playlist_name[302],
                    playlist_name[402]) #to select playlist names
misc_rock <- which(playlist_name == playlist_name2[2] & play_FKM_2$clus[, 1] == 1)
#to see the membership degree of misclassified groups
misc_jazz <- which(playlist_name == playlist_name2[4] & play_FKM_2$clus[, 1] == 2)
misc_hip <- which(playlist_name == playlist_name2[5] & play_FKM_2$clus[, 1] == 1)
misc_metal <- which(playlist_name == playlist_name2[2] & play_FKM_2$clus[, 1] == 1)
}


#EXTRA CODE 

#Winsorizing

boxplot(dataset[,variable], main = paste("Boxplot",plot_title))

treshold <- quantile(dataset[,variable], 0.95)
extra <- pmin(dataset[,variable], treshold)
dataset[,variable] <- na.omit(extra)

boxplot(dataset[,variable], main = paste("Boxplot",plot_title))




```{r}
#Fuzzy clustering 

# Use the dataset `n_data` with all numerical variables

# Perform fuzzy clustering with all numerical columns
fkm_result <- FKM(X = n_data, stand = 1, RS = 10, seed = 264)

# Display the number of clusters chosen and the criterion
fkm_result$k
fkm_result$criterion

# Perform fuzzy clustering with the XB index
fkm_result_XB <- FKM(X = n_data, stand = 1, RS = 10, seed = 264, index = "XB")
fkm_result_XB$criterion

# Perform fuzzy clustering with the PC index
fkm_result_PC <- FKM(X = n_data, stand = 1, RS = 10, seed = 264, index = "PC")
fkm_result_PC$criterion

# Perform fuzzy clustering with K=2
fkm_result_2 <- FKM(X = n_data, k = 2, stand = 1, RS = 10, seed = 264)

# Display the first results of the membership degree for each cluster
head(fkm_result_2[["clus"]])

# Display the size of the clusters
cl.size(fkm_result_2$U)

# Calculate the raw entropy
Hraw(fkm_result_2$X, fkm_result_2$H)

# Determine the max membership cluster for each observation
max_membership_cluster <- apply(fkm_result_2$clus, 1, which.max)

# Plot the membership degrees for the first two clusters
clus1 <- fkm_result_2$clus[, 1]
clus2 <- fkm_result_2$clus[, 2]

# Create a data frame for plotting
plot_data <- data.frame(n_data, clus1, clus2)

# Plot using ggplot2
ggplot(plot_data, aes(x = clus1, y = clus2)) +
  geom_point(aes(color = as.factor(max_membership_cluster)), alpha = 0.6) +
  labs(title = "Fuzzy Clustering Results", x = "Cluster 1 Membership Degree", y = "Cluster 2 Membership Degree") +
  scale_color_discrete(name = "Max Membership Cluster") +
  theme_minimal()

# Pairwise plot of the original data colored by the max membership cluster
pairs.panels(n_data, col = max_membership_cluster, main = "Pairwise Plot of Original Data")

```