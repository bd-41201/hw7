### Homework 7 Write Up Script

# Before starting in on the questions load the data from sp500.csv and FXmonthly.csv
sp500 <- read.csv("sp500.csv")
fx_monthly <- read.csv("FXmonthly.csv")

# Convert to rate
fx_monthly <- (fx_monthly[2:120,]-fx_monthly[1:119,])/(fx_monthly[1:119,])

## Q1
# Explore the correlation between the different columns of the FX data
# Find the correlation matrix and then square it to get the R2 matrix
fx_corr <- cor(fx_monthly)

library(corrplot)
corrplot(fx_corr)

## Q2
# Run prcomp to determine the principle components

pca.fx.monthly <- prcomp(fx_monthly, scale=TRUE)
plot(pca.fx.monthly, main="")
mtext(side=1, "Foreign Exchange Principle Components",  line=1, font=2)

z.pca.fx.monthly <- predict(pca.fx.monthly)

fx_monthly$year <- sapply(1:119, function(x) as.integer(unlist(strsplit(rownames(fx_monthly)[x],"2"))[2]))
fx_monthly$year <- sapply(1:119, function(x) ifelse(fx_monthly$year[x]==0,2,fx_monthly$year[x]))
plot(z.pca.fx.monthly[,1:2],type="n")
text(x=z.pca.fx.monthly[,1], y=z.pca.fx.monthly[,2], labels=rownames(z.pca.fx.monthly),col=rainbow(10)[fx_monthly$year])

## Q3
# glm on first k

library(gamlr)
## Get glm fits on 1:20 factors
z.pca.fx.frame <- as.data.frame(z.pca.fx.monthly)
kfits <- lapply(1:20, # do the below for K=1:20
  function(K) glm(sp500[,2]~., data=z.pca.fx.frame[,1:K,drop=FALSE]))
aicc <- sapply(kfits, AICc) # apply AICc to each fit
which.min(aicc)
# it likes 3 factors best
## you could also use BIC
bic <- sapply(kfits, BIC)
which.min(bic)
# also likes 3 factors


## now the lasso
lassoPCR <- cv.gamlr(x=z.pca.fx.monthly, y=sp500[,2])
## lasso.1se agrees with IC on first 3, then grabs a couple extra
coef(lassoPCR, s="min")

## Q4
# Look at a CV lasso regression at an un-factorized model

lasso <- cv.gamlr(x=as.matrix(fx_monthly[,1:23]), y=sp500[,2], nfolds=23)
plot(lasso)
coef(lasso, s="1se")
