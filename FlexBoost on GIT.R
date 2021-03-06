#########################################################################################################################
### Project  : FlexBoost
### Script   : FlexBoost on GIT.R
### Contents : A flexible boosting algorithm with adaptive loss functions
#########################################################################################################################

#########################################################################################################################
### Setting up environment
#########################################################################################################################

# Load library
pkgs <- c("rpart", "ada", "caTools", "crayon", "ggplot2", "gplots", "coin", "reshape")
sapply(pkgs, require, character.only = T)

# Load data
load("Datasets.RData")

# Load function
load("Functions.RData")

# Load result
load("Results.RData")


#########################################################################################################################
### Analysis
#########################################################################################################################

## Figure 1. Loss functions
x   <- seq(-1, 1, 0.0001)
s   <- 1.7
l   <- 2
y.1 <- exp(-x)
y.2 <- exp(-s*x)
y.3 <- exp(-(1/s)*x)
m   <- min(y.1, y.2, y.3)
M   <- max(y.1, y.2, y.3)
plot(x, y.1, type = "l", col = "black", ylim = c(m, M), xlab = "y??f", ylab = "LOSS", lwd = l)
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


## Table 1. Description of the datasets
table.1 <- matrix(NA, length(df.all), 2, dimnames = list(names(df.all), c("No.Instances", "No.Attributes")))
for(i in 1:nrow(table.1)){table.1[i,] <- dim(df.all[[i]]) - c(0, sum(grepl("class", names(df.all[[i]]))) - 1)}
print(table.1[-c(17:18, 20:21, 23:24, 26:27, 29:30),])


## Table 2. Performance benchmarks (accuracy)
res.r1  <- subset(res.acc.all, Round == 9)
acc.r1  <- rbind(res.r1[,3:6], colMeans(res.r1[,c(3:6)]))
table.2 <- data.frame(Data = c(as.character(res.r1$Data), "Avg.Acc"),
                      round(acc.r1, 6), Optimal.K = c(res.r1$Optimal.K, NA))
print(table.2)


## Figure 3. Performance benchmarks on 30 UCI and Kaggle datasets
plot.result(1) # Plot 1-29 data accuracies(Except Iris_1 due to acc 1)


## Table 3. Friedman post hoc test results (p-value) on rankings and average accuracies
res.ranks <- as.matrix(res.acc.all[, 3:6])
for(i in 1:nrow(res.ranks)){res.ranks[i,] <- rank(-res.acc.all[i, 3:6], ties.method = "min")}
res.fm.ph <- friedman.post.hoc(value ~ X2 | X1, melt(res.ranks))
res.p.val <- c(rep(NA, 4), res.fm.ph$PostHoc.Test[3], rep(NA, 3), 
               res.fm.ph$PostHoc.Test[c(2, 6)], rep(NA, 2),
               res.fm.ph$PostHoc.Test[c(1, 5, 4)], NA)
res.mrank <- colMeans(res.ranks)
res.macc  <- as.character(round(colMeans(res.acc.all[,3:6]),4))
table.3   <- as.data.frame(matrix(rbind(matrix(round(res.p.val, 4), 4, 4), as.character(round(colMeans(res.ranks), 2)), res.macc), 6, 4, 
                                  dimnames = list(c(names(res.mrank), "Mean Rank", "Mean Accuracy"), names(res.mrank))))
print(table.3)


#########################################################################################################################
### Appendix. Experiment
#########################################################################################################################

# Parameter
data        <- Haberman
independent <- 1:3
dependent   <- 4
s           <- 9 #Seed
X           <- data[, independent]
y           <- data[, dependent]


## Kfold AdaBoost
kfold.ada(30)


## Kfold LogitBoost
kfold.logit(30)


## Kfold GentleBoost
kfold.gentle(30)

## Kfold FlexBoost 
kfold.flex(30,0.2,1) #iteration, par.k, type(1-3 for tie evaluation)
