# PCA with function PCA
library(FactoMineR)

#Checking the Structure
str(epi_r)

# Imputing the values
epi_r$title<-as.numeric(as.factor(epi_r$title))
epi_r$protein[is.na(epi_r$protein)]<-mean(epi_r$protein,na.rm = T)
epi_r$fat[is.na(epi_r$fat)]<-mean(epi_r$fat,na.rm = T)
epi_r$sodium[is.na(epi_r$sodium)]<-mean(epi_r$sodium,na.rm = T)
epi_r$calories[is.na(epi_r$calories)]<-mean(epi_r$calories,na.rm = T)



# apply PCA
pca_1<-PCA(epi_r,graph=FALSE)

# matrix with eigenvalues
pca_1$eig

# correlations between variables and PCs
pca_1$var$coord


# PCs (aka scores)
head(pca_1$ind$coord)

# Running PCA using SVD
svd_1<-svd(epi_r)

svd_1

#Plotting PCA Components
names(pca_1)

#[1] "eig"  "var"  "ind"  "svd"  "call"

plot(pca_1$eig[, 1], pca_1$eig[, 2],  main = "PCA", xlab = "PC1", ylab = "PC2")
