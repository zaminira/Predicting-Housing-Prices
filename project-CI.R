getwd()
setwd("D:\\Rprojects\\Project")
library(ggplot2)
library(lmreg)
sink("./outputCI.txt", append = F)

df= read.table(file = "dataHousing.txt", header = FALSE)
n = nrow(df)
p = ncol(df)
# rename columns
colnames(df)= c('date','age','MRT','store','lat','long','Y')

cat("\nModels with log transformation of Y and MRT and removing long data\n")
model <- lm(log(Y) ~ date + age + log(MRT) + store + lat, data = df)
p = 6
print(summary(model))
sse <- sum((fitted(model)-log(df$Y))^2)
ssr <- sum((fitted(model)-mean(log(df$Y)))^2)
sst <- sse + ssr
mse <- sse/(n-p)
varXX = vcov(model)
se <- sqrt(diag(varXX))
errors = data.frame(SSE=c(sse), SSR=c(ssr), SSTO=c(sst))
print(errors)
cat("\nMSE = ", mse, "\n")
cat("\nsigma = ", sqrt(mse), "\n")

#%% Confidence interval of hypothesis
# Making the matrix
Lval.data1 <- c(0,1,0,0,0,0)
Lval.data2 <- c(0,0,1,0,0,0)
Lval.data3 <- c(0,0,0,1,0,0)
Lval.data4 <- c(0,0,0,0,1,0)
Lval.data5 <- c(0,0,0,0,0,1)
L <- cbind(Lval.data1, Lval.data2, Lval.data3, Lval.data4, Lval.data5)

X = as.matrix(cbind(1, df[,1:2], log(df[,3]), df[,4:5]))
Y = as.matrix(cbind(log(df[,1])))
beta_hat = model$coefficients
var.beta = vcov(model)
se.beta = sqrt(diag(var.beta))

theta_hat = t(L) %*% beta_hat
var.theta = t(L) %*% var.beta %*% L
se.theta = sqrt(diag(var.theta))

r = 6       # no of parameters
# m = 5       # no of rows in L' for bonferroni
m = 5
alpha = 0.05
T_CI = qtukey(1-alpha, r, n-r)/sqrt(2)
B_CI = qt(1- alpha/2/m, n-r)
S_CI = sqrt((m)*qf(1-alpha,m,n-r))      # scheffe should touch the ellipse boundary

upperBoundBF = theta_hat + B_CI * se.theta
lowerBoundBF = theta_hat - B_CI * se.theta
upperBoundSF = theta_hat + S_CI * se.theta
lowerBoundSF = theta_hat - S_CI * se.theta
CI = data.frame(estimate = theta_hat, LB_BF = lowerBoundBF, UB_BF = upperBoundBF,
                  LB_SF = lowerBoundSF, UB_SF = upperBoundSF)
cat("\nSimultaneous confidence interval (intercept term not included): \n")
print(CI)


F_val = qf(1-alpha,m,n-p)
jpeg(file="CI_MRT_store.jpeg", width=6, height=6, unit="in", res=300)
i = 3 
j = 4
car::ellipse(as.vector(theta_hat[i:j,1]), var.theta[i:j, i:j], sqrt(m*F_val), 
             add=FALSE, xlab="beta_log(MRT)", ylab="beta_store")
rect(lowerBoundBF[i], lowerBoundBF[j], upperBoundBF[i], upperBoundBF[j], lwd = 2, border = "red")
rect(lowerBoundSF[i], lowerBoundSF[j], upperBoundSF[i], upperBoundSF[j], lwd = 2, border = "black")
dev.off()


sink()
