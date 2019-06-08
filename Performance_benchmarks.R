#########################################################################################################################
### Project  : FlexBoost
### Script   : Performance_benchmarks.R
### Contents : A flexible boosting algorithm with adaptive loss functions
#########################################################################################################################

#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
pkgs <- c("rpart", "ada", "caTools", "crayon", "ggplot2")
sapply(pkgs, require, character.only = T)

# Load data
res.all <- read.csv(url("http://bit.ly/Flex_Boost"), header = T)


#########################################################################################################################
### Analysis
#########################################################################################################################

# Figure 1.
x   <- seq(-1, 1, 0.0001)
s   <- 1.7
l   <- 2
y.1 <- exp(-x)
y.2 <- exp(-s*x)
y.3 <- exp(-(1/s)*x)
m   <- min(y.1, y.2, y.3)
M   <- max(y.1, y.2, y.3)
plot(x, y.1, type = "l", col = "black", ylim = c(m, M), xlab = "y·f", ylab = "LOSS", lwd = l)
grid()
par(new = TRUE)
plot(x, y.2, type = "l", col = "red",   ylim = c(m, M), xlab = "", ylab = "", lwd = l)
par(new = TRUE)
plot(x, y.3, type = "l", col = "blue",  ylim = c(m, M), xlab = "", ylab = "", lwd = l)
segments(-1, 1, 0, 1, lty = l, col = "orange", lwd = l)
segments( 0, 1, 0, 0, lty = l, col = "orange", lwd = l)
segments(-0, 0, 1, 0, lty = l, col = "orange", lwd = l)
legend(0, 5.2, legend = c("Exponential K > 1", "Exponential K = 1", "Exponential K < 1", "Classification\n(Zero-One)"),
       col = c("red", "black", "blue", "orange"), lty = c(1, 1, 1, 2), cex = 0.74, lwd = l)

# Table 2. Performance Benchmarks (round 1)
res.r1 <- subset(res.all, Round == 1)
acc.r1 <- rbind(res.r1[,3:6], colMeans(res.r1[,c(3:6)]))
data.frame(Data = c(as.character(res.r1$Data), "Avg.Acc"),
           round(acc.r1, 6),
           Optimal.K = c(res.r1$Optimal.K, NA))



