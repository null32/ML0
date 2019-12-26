library("MASS")
library("kernlab")
library("ROCR")

colors = c("magenta", "cyan")
n <- 100
m <- 100

sigma1 <- matrix(c(1, 0, 0, 1), 2, 2)
sigma2 <- matrix(c(1, 0, 0, 1), 2, 2)

mu1 <- c(8, 10)
mu2 <- c(10, 10)

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

w<-colSums(svp@coef[[1]] * dat[svp@SVindex,][,1:2])
b<-svp@b

abline(b/w[2], -w[1]/w[2], lwd=2)
abline((b-1)/w[2], -w[1]/w[2])
abline((b+1)/w[2], -w[1]/w[2])

ypredscore <- predict(svp, dat[,1:2], type = "decision")
pred <- prediction(ypredscore, dat[,3])
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)
lines(c(0, 1), c(0, 1), col="red")