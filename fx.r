fx <- read.csv("FXmonthly.csv")
sp <- read.csv("sp500.csv") 
fx <- fx[-1,]
fx

fxsp  <- cbind(sp, fx)
fxsp

help(cbind)

#1 Discuss correlation amongst dimensions of fx. How does this relate to the applicability of factor modelling? 

# There should be correlation among dimensions of fx because currencies are correlated with both positive and negative correlations.
# The factor model rotations assume that there is no correlation.  Simply stated, the factors of the first PC should be orthoganal to the subsequent PC's but if there is in fact correlation this will throw off this assumption.


#2 Fit, plot, and interpret principal components. 
pcfx <- prcomp(fx, scale=TRUE)
plot(pcfx)
predict(pcfx)[,1:2]
zpcfx <- predict(pcfx)
t( round(pcfx$rotation[,1:2],2) )
# PC1  <- strong us dollar?
# PC2  <- week US dollar?

#3 Regress SP500 returns on to currency movement factors, using both ‘glm on ﬁrst K’ and lasso techniques. Use the results to add to your factor interpretation. 

#4 Fit lasso to the original covariates and describe how it differs from PCR here. 

#5 Fit marginal regression and PLS and compare to PCA.
