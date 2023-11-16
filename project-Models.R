getwd()
setwd("D:\\Rprojects\\Project")
library(ggplot2)
sink("./output.txt", append = F)

df= read.table(file = "dataHousing.txt", header = FALSE)
n = nrow(df)
p = ncol(df)
# rename columns
colnames(df)= c('date','age','MRT','store','lat','long','Y')

#%% scatter plot matrix
jpeg(file="scatter_plot_matrix.jpeg", width=15, height=15, unit="in", res=600)
pairs(df, main = "Scatterplot Matrix", pch=19, lowe.panel=NULL)
dev.off()

#%% correlation Matrix
cat("-------------------------------------------------------------------------")
cat("\nCorrelation Matrix")
cat("\n\n")
print(cor(df))

#%% various case of MRT and Y
jpeg(file="YvsMRT.jpeg", width=10, height=10, unit="in", res=600)
par(mfrow=c(2,2))
plot(df$MRT, df$Y, pch=19, xlab = "MRT", ylab = "Y")
plot(log(df$MRT), df$Y, pch=19, xlab = "log MRT", ylab = "Y")
plot(df$MRT, log(df$Y), pch=19, xlab = "MRT", ylab = "log Y")
plot(log(df$MRT), log(df$Y), pch=19, xlab = "log MRT", ylab = "log Y")
dev.off()

# 3D map plot 
# jpeg(file="MRT_Location.jpeg", width=6, height=6, unit="in", res=600)
# ggplot() + geom_point(data = df, aes(x = long, y = lat, color = MRT), alpha = 1.0) + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))
# dev.off()
# jpeg(file="date_Location.jpeg", width=6, height=6, unit="in", res=600)
# ggplot() + geom_point(data = df, aes(x = long, y = lat, color = date), alpha = 1.0) + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))
# dev.off()
# jpeg(file="age_Location.jpeg", width=6, height=6, unit="in", res=600)
# ggplot() + geom_point(data = df, aes(x = long, y = lat, color = age), alpha = 1.0) + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))
# dev.off()
jpeg(file="store_Location.jpeg", width=6, height=6, unit="in", res=600)
ggplot() + geom_point(data = df, aes(x = long, y = lat, color = store), alpha = 1.0) + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))
dev.off()
jpeg(file="store_MRT_Location.jpeg", width=6, height=6, unit="in", res=600)
ggplot() + geom_point(data = df, aes(x = long, y = lat, size = store, color = MRT), alpha = 0.3) + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))
dev.off()

#%% Model 1 - full model without transformation
cat("-------------------------------------------------------------------------")
cat("\nFull Model without any transformations\n")

model <- lm(Y ~ ., data = df)
print(summary(model))
sse <- sum((fitted(model)-df$Y)^2)
ssr <- sum((fitted(model)-mean(df$Y))^2)
sst <- sse + ssr
mse <- sse/(n-p)
se <- sqrt(diag(vcov(model)))
errors = data.frame(SSE=c(sse), SSR=c(ssr), SSTO=c(sst))
print(errors)
cat("\nMSE = ", mse)
varXX = vcov(model)
cat("\nvcov of estimators : \n")
print(varXX)

cat("\nANOVA table\n")
print(aov(model))

# # checking the assumption of Gaussian Linear Model

#1 : The distribution of model residuals should be approximately normal.
jpeg(file="M1-residual_histogram.jpeg", width=6, height=6, unit="in", res=300)
hist(residuals(model), col = "steelblue", xlab = "Residual")
dev.off()

#2: QQ plot
jpeg(file="M1-residual_qqplot.jpeg", width=6, height=6, unit="in", res=300)
qqnorm(residuals(model), pch= 19)
abline(a = 0, b= 1, lty= 4)
dev.off()

#3 residual vs. Y value 
jpeg(file="M1-residualvsY.jpeg", width=10, height=6, unit="in", res=300)
par(mfrow=c(1,2))
plot(df$Y, residuals(model), pch=1, main='vs. given Y')
plot(fitted(model), residuals(model), pch=1, main='vs. fitted Y')
dev.off()

#4 scatter plot of residual
jpeg(file="M1-residual_scatter.jpeg", width=10, height=10, unit="in", res=600)
par(mfrow=c(3,2))

plot(df$date, residuals(model), pch=19, 
     xlab = "date", ylab = "Residuals", main = "residual vs date")
abline(h = 0, lty = 4)

plot(df$age, residuals(model), pch=19, 
     xlab = "age", ylab = "Residuals", main = "residual vs age")
abline(h = 0, lty = 4)

plot(df$MRT, residuals(model), pch=19, 
     xlab = "MRT", ylab = "Residuals", main = "residual vs MRT")
abline(h = 0, lty = 4)

plot(df$store, residuals(model), pch=19, 
     xlab = "store", ylab = "Residuals", main = "residual vs store")
abline(h = 0, lty = 4)

plot(df$lat, residuals(model), pch=19, 
     xlab = "latitude", ylab = "Residuals", main = "residual vs latitude")
abline(h = 0, lty = 4)

plot(df$long, residuals(model), pch=19, 
     xlab = "longitude", ylab = "Residuals", main = "residual vs longitude")
abline(h = 0, lty = 4)
dev.off()

#%% Model 2 - full model with transformation of MRT
cat("-------------------------------------------------------------------------")
cat("\nModels with log transformation of MRT\n")
model2 <- lm(Y ~ date + age + log(MRT) + store + lat + long, data = df)
print(summary(model2))
cat("\nANOVA table\n")
print(aov(model2))

#%% Model 3 - Sub model with transformation of MRT (remove long)
cat("\n")
cat("-------------------------------------------------------------------------")
cat("\nModels with log transformation of MRT and removing long data\n")
model3 <- lm(Y ~ date + age + log(MRT) + store + lat, data = df)
print(summary(model3))
cat("\nANOVA table\n")
print(aov(model3))

#%% Model 4 - Sub model with transformation of Y (remove long)
cat("\n")
cat("-------------------------------------------------------------------------")
cat("\nModels with log transformation of Y and removing long data\n")
model4 <- lm(log(Y) ~ date + age + MRT + store + lat, data = df)
print(summary(model4))
sse4 <- sum((fitted(model4)-log(df$Y))^2)
ssr4 <- sum((fitted(model4)-mean(log(df$Y)))^2)
sst4 <- sse4 + ssr4
mse4 <- sse4/(n-6)
errors = data.frame(SSE=c(sse4), SSR=c(ssr4), SSTO=c(sst4))
print(errors)
cat("\nMSE = ", mse4)
varXX4 = vcov(model4)
cat("\nvcov of estimators : \n")
print(varXX4)
cat("\nANOVA table\n")
print(aov(model4))

#3 residual vs. Y value 
jpeg(file="M4-residualvsY.jpeg", width=10, height=6, unit="in", res=300)
par(mfrow=c(1,2))
plot(log(df$Y), residuals(model4), pch=1, main='vs. given log(Y)')
plot(fitted(model4), residuals(model4), pch=1, main='vs. fitted log(Y)')
dev.off()

#%% Model 5 - Sub model with transformation of MRT and Y (remove long)
cat("\n")
cat("-------------------------------------------------------------------------")
cat("\nModels with log transformation of Y and MRT and removing long data\n")
model5 <- lm(log(Y) ~ date + age + log(MRT) + store + lat, data = df)
print(summary(model5))
sse5 <- sum((fitted(model5)-log(df$Y))^2)
ssr5 <- sum((fitted(model5)-mean(log(df$Y)))^2)
sst5 <- sse5 + ssr5
mse5 <- sse5/(n-6)
errors = data.frame(SSE=c(sse5), SSR=c(ssr5), SSTO=c(sst5))
print(errors)
cat("\nMSE = ", mse5)
varXX5 = vcov(model5)
cat("\nvcov of estimators : \n")
print(varXX5)

cat("\nANOVA table\n")
print(aov(model5))

#2 : The distribution of model residuals should be approximately normal.
jpeg(file="M5-residual_histogram.jpeg", width=6, height=6, unit="in", res=300)
hist(residuals(model5), col = "steelblue", xlab = "Residual")
dev.off()

#3: QQ plot
jpeg(file="M5-residual_qqplot.jpeg", width=6, height=6, unit="in", res=300)
qqnorm(residuals(model5), pch= 19)
qqline(residuals(model5), lty= 4)
dev.off()

#3 residual vs. Y value 
jpeg(file="M5-residualvsY.jpeg", width=10, height=6, unit="in", res=300)
par(mfrow=c(1,2))
plot(log(df$Y), residuals(model5), pch=1, main='vs. given log(Y)')
plot(fitted(model5), residuals(model5), pch=1, main='vs. fitted log(Y)')
dev.off()

#4 scatter plot of residual
jpeg(file="M5-residual_scatter.jpeg", width=10, height=10, unit="in", res=600)
par(mfrow=c(3,2))

plot(df$date, residuals(model5), pch=19, 
     xlab = "date", ylab = "Residuals", main = "residual vs date")
abline(h = 0, lty = 4)

plot(df$age, residuals(model5), pch=19, 
     xlab = "age", ylab = "Residuals", main = "residual vs age")
abline(h = 0, lty = 4)

plot(log(df$MRT), residuals(model5), pch=19, 
     xlab = "log(MRT)", ylab = "Residuals", main = "residual vs log(MRT)")
abline(h = 0, lty = 4)

plot(df$store, residuals(model5), pch=19, 
     xlab = "store", ylab = "Residuals", main = "residual vs store")
abline(h = 0, lty = 4)

plot(df$lat, residuals(model5), pch=19, 
     xlab = "latitude", ylab = "Residuals", main = "residual vs latitude")
abline(h = 0, lty = 4)

plot(df$long, residuals(model5), pch=19, 
     xlab = "longitude", ylab = "Residuals", main = "residual vs longitude")
abline(h = 0, lty = 4)
dev.off()

sink()