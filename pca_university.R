install.packages("princomp")
install.packages("gdata")
install.packages("xlsx")
library(gdata)
library(xlsx)
library(csv)
mydata<-read.csv("D:/assignments/pca/Universities.csv") ## use read.csv for csv files
View(mydata)

help(princomp) ## to understand the api for princomp

## the first column in mydata has university names
View(mydata[-1]) 
# mydata[-1] -> Considering only numerical values for applying PCA
data <- mydata[,-1]
attach(data)
cor(data)
# cor = TRUE use correlation matrix for getting PCA scores
?princomp
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
## princomp(mydata, cor = TRUE) not_same_as prcomp(mydata, scale=TRUE); similar, but different
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)

plot(pcaObj) # graph showing importance of principal components 
# Comp.1 having highest importance (highest variance)

biplot(pcaObj)

# Showing the increase of variance with considering principal components
# Which helps in choosing number of principal components
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcaObj$loadings

pcaObj$scores[,1:3] # Top 3 PCA Scores which represents the whole data

# cbind used to bind the data in column wise
# Considering top 3 principal component scores and binding them with mydata
mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,8:10]

# Normalizing the data 
norm_clus<-scale(clus_data) # Scale function is used to normalize data
##dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance
# here I am considering Euclidean distance
dist1 <- dist(norm_clus,method = "manhattan")
dist2 <- dist(norm_clus,method= "maximum")
# Clustering the data using hclust function --> Hierarchical

fit1<-hclust(dist1,method="complete")# method here is complete linkage
plot(fit1) # Displaying Dendrogram
fit2<-hclust(dist1,method="average") 
plot(fit2)
fit3 <- hclust(dist,method = "complete")
plot(fit3)
groups<-cutree(fit1,5) # Cutting the dendrogram for 5 clusters

membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)

final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean)) # Inferences can be
# drawn from the aggregate of the universities data on membership_1

write.csv(final1,file="universities_clustered.csv",row.names = F,col.names = F)
getwd()

#k means clustering
km <- kmeans(norm_clus,5)
str(km)

install.packages("animation") 
library(animation)
km1 <- kmeans.ani(norm_clus,5)

km$centers
km$cluster
wss <- (nrow(norm_clus)-1)*sum(apply(norm_clus,2,var))
for(i in 2:5) wss[i] <- sum(kmeans(norm_clus,centers = i)$withinss)
plot(1:55,wss,type ="b", xlab = "no of cluster",ylab = " within group sum of squares")

#other methods for k selection
install.packages("kselection")
library(kselection)
k <- kselection(norm_clus[,],fun_cluster = stats:: kmeans, k_threshold=0.85,max_centers = 5)
k

