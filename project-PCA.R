getwd()
setwd("D:\\Rprojects\\Project")
library(ggplot2)
library("FactoMineR")
library("factoextra")
library("pls")
sink("./outputPCA2.txt", append = F)

df= read.table(file = "dataHousing.txt", header = FALSE)
n = nrow(df)
p = ncol(df)
# rename columns
colnames(df)= c('date','age','MRT','store','lat','long','Y')

# comment the bottom two when checking for model without transformation 
df$Y <- log(df$Y)
df$MRT <- log(df$MRT)

# bifurcate into training and test set 
dt = sort(sample(n, 0.8*n))
train <- df[dt,]
test <- df[-dt,]

#%% Model 5 PCA - full model with transformation
cat("-------------------------------------------------------------------------")
cat("\nPCA on Full Model with log transformations of Y and MRT\n")

train.norm <- as.data.frame(scale(train))
test.norm <- as.data.frame(scale(test))
df.norm <- as.data.frame(scale(df))

# full data pcs
X = df[,1:6]
Y = df[,7]
X.norm <- df.norm[,1:6]
data.pca <- prcomp(X.norm, center = TRUE, scale. = TRUE)
pcs <- as.data.frame(data.pca$x)
data.ols <- cbind(pcs, df[,7])
colnames(data.ols)= c('PC1','PC2','PC3','PC4','PC5','PC6','Y')

jpeg(file="PCA-M2-YvsPCS.jpeg", width=10, height=10, unit="in", res=300)
par(mfrow=c(2,1))
plot(pcs$PC1, Y, pch=1, ylab = "Cost", xlab = "PC1")
plot(pcs$PC2, Y, pch=1, ylab = "Cost", xlab = "PC1")
dev.off()

# pcs fitting 
fit.train <- pcr(Y ~., data = train.norm)
fit.full <- pcr(Y ~., data = df.norm)
print(summary(fit.full))

y.pred.test <- predict(fit.train, newdata=test.norm)
jpeg(file="PCA-M2-YfitvsY.jpeg", width=6, height=6, unit="in", res=300)
plot(test.norm$Y, y.pred.test[,1,4], pch=1, xlab = "Cost", ylab = "predicted cost", main = "Using 4 PCS")
abline(a = 0, b= 1, lty= 4)
dev.off()

jpeg(file="PCA-M2-R2vsncomp.jpeg", width=6, height=6, unit="in", res=300)
validationplot(fit.full, val.type="R2", cex.axis=0.7)
abline(v = 4, col = "blue", lty = 3)
dev.off()

res.pca <-PCA(X, scale.unit = TRUE, ncp = 6, graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
var <- get_pca_var(res.pca)
ind <- get_pca_ind(res.pca)

# PCA dimension 
jpeg(file="PCA-M1-dim_dir.jpeg", width=6, height=6, unit="in", res=600)
fviz_pca_var(res.pca, col.var = "black")
dev.off

jpeg(file="PCA-M2-dim_contribution.jpeg", width=6, height=6, unit="in", res=600)
plt1 <- fviz_eig(data.pca, addlabels = TRUE)
print(plt1)
dev.off()
# 
# jpeg(file="PCA_plot_12.jpeg", width=6, height=6, unit="in", res=600)
# plt2 <- fviz_pca_ind(res.pca, geom = c("point"), axes= c(1,2), col.ind = "cos2", 
#                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
# print(plt2)
# dev.off()
# jpeg(file="PCA_plot_23.jpeg", width=6, height=6, unit="in", res=600)
# plt2 <- fviz_pca_ind(res.pca, geom = c("point"), axes= c(2,3), col.ind = "cos2", 
#                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
# print(plt2)
# dev.off()
# jpeg(file="PCA_plot_13.jpeg", width=6, height=6, unit="in", res=600)
# plt2 <- fviz_pca_ind(res.pca, geom = c("point"), axes= c(1,3), col.ind = "cos2", 
#                      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
# print(plt2)
# dev.off()

sink()