library("MASS")
library("kernlab")

colors = c("magenta", "cyan")
n <- 100
m <- 100

sigma1 <- matrix(c(1, 0, 0, 1), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)

mu1 <- c(5, 10)
mu2 <- c(12, 10)

xc1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xc2 <- mvrnorm(n=m, mu = mu2, Sigma = sigma2)

dat <- rbind(xc1, xc2)
dat <- cbind(dat, c(rep(-1, n), rep(1, m)))

plotxmin <- min(dat[,1], dat[,1]) - 0.3
plotxmax <- max(dat[,1], dat[,1]) + 0.3
plotymin <- min(dat[,2], dat[,2]) - 0.5
plotymax <- max(dat[,2], dat[,2]) + 0.5
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="SVM")

points(dat, pch=21, col=colors[ifelse(dat[,3] == -1, 1, 2)], bg=colors[ifelse(dat[,3] == -1, 1, 2)])

svp <- ksvm(dat[,1:2], dat[,3], type = "C-svc", kernel = "vanilladot", C=100, scaled=c())
#plot(svp, data = dat[,1:2])

#alpha	-- The resulting support vectors, (alpha vector) (possibly scaled).
#alphaindex	-- The index of the resulting support vectors in the data matrix. Note that this index refers to the pre-processed data (after the possible effect of na.omit and subset)
#coef	-- The corresponding coefficients times the training labels.
#b -- The negative intercept.
#nSV -- The number of Support Vectors
#obj -- The value of the objective function. In case of one-against-one classification this is a vector of values
#error -- Training error
#cross -- Cross validation error, (when cross > 0)
#prob.model -- Contains the width of the Laplacian fitted on the residuals in case of regression, or the parameters of the sigmoid fitted on the decision values in case of classification.

