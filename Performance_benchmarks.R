#########################################################################################################################
### Project  : FlexBoost
### Script   : FlexBoost on GIT.R
### Contents : A flexible boosting algorithm with adaptive loss functions
#########################################################################################################################

#########################################################################################################################
### Setting up Environment
#########################################################################################################################

# Load library
pkgs <- c("rpart", "ada", "caTools", "crayon", "ggplot2", "gplots", "PairedData", "car", "tidyr", "reshape")
sapply(pkgs, require, character.only = T)

# Load functions and data
load("Functions.RData")
load("Datasets.RData")
res.acc.all <- read.csv(url("http://bit.ly/Flex_Boost"), header = T)

#########################################################################################################################
### Analysis
#########################################################################################################################

# Figure 1. Loss functions (k < 1, k = 1, k > 1)
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

# Table 1. Description of the datasets
table.1 <- matrix(NA, length(df.all), 2, dimnames = list(names(df.all), c("No.Instances", "No.Attributes")))
for(i in 1:nrow(table.1)){table.1[i,] <- dim(df.all[[i]]) - c(0, sum(grepl("class", names(df.all[[i]]))) - 1)}
print(table.1[-c(17:18, 20:21, 23:24, 26:27, 29:30),])

# Table 2. Performance Benchmarks (round 1)
res.r1  <- subset(res.acc.all, Round == 1)
acc.r1  <- rbind(res.r1[,3:6], colMeans(res.r1[,c(3:6)]))
table.2 <- data.frame(Data = c(as.character(res.r1$Data), "Avg.Acc"),
                      round(acc.r1, 6), Optimal.K = c(res.r1$Optimal.K, NA))

# Figure 3.Performance benchmarks
data_names = "Iris3" 

result_b   <- res.itr.all.ada[ , colnames(res.itr.all.ada) == data_names]
result_l   <- res.itr.all.logit[ , colnames(res.itr.all.logit) == data_names]
result_g   <- res.itr.all.gentle[ , colnames(res.itr.all.gentle) == data_names]
result_h   <- res.itr.all.flex[ , colnames(res.itr.all.flex) == data_names]
iterations <- 100

plot(1:iterations, result_h, col = "red",  xlab = "Iterations", ylab = "Accuracy", main = data_names, 
     cex.lab = 2, cex.main = 2,
     ylim = c(min(min(unlist(result_b)), 
                  min(unlist(result_h)),
                  min(unlist(result_g)),
                  min(unlist(result_l))), 
              max(max(unlist(result_b)), 
                  max(unlist(result_h)),
                  max(unlist(result_g)),
                  max(unlist(result_l)))))

lines(1:iterations,result_h,col = "red")
par(new = TRUE)

# plot the graph of basic adaboost
par(new = TRUE)
plot(1:iterations, result_b, col = "green3", xlab = "", ylab = "", 
     ylim = c(min(min(unlist(result_b)), 
                  min(unlist(result_h)),
                  min(unlist(result_g)),
                  min(unlist(result_l))), 
              max(max(unlist(result_b)), 
                  max(unlist(result_h)),
                  max(unlist(result_g)),
                  max(unlist(result_l)))))
lines(1:iterations, result_b, col = "green3", lty = 2)
par(new = TRUE)
plot(1:iterations, result_l, col = "blue", xlab = "", ylab = "", 
     ylim = c(min(min(unlist(result_b)), 
                  min(unlist(result_h)),
                  min(unlist(result_g)),
                  min(unlist(result_l))), 
              max(max(unlist(result_b)), 
                  max(unlist(result_h)),
                  max(unlist(result_g)),
                  max(unlist(result_l)))))
lines(1:iterations, result_l, col = "blue", lty = 3)
par(new = TRUE)
plot(1:iterations, result_g, col = "black", xlab = "", ylab = "", 
     ylim = c(min(min(unlist(result_b)), 
                  min(unlist(result_h)),
                  min(unlist(result_g)),
                  min(unlist(result_l))), 
              max(max(unlist(result_b)), 
                  max(unlist(result_h)),
                  max(unlist(result_g)),
                  max(unlist(result_l)))))
lines(1:iterations, result_g, col = "black", lty = 6)

# Table 3. Friedman post hoc test
res.ranks <- as.matrix(res.acc.all[, 3:6])
for(i in 1:nrow(res.ranks)){res.ranks[i,] <- rank(-res.acc.all[i, 3:6], ties.method = "min")}
res.fm.ph <- friedman.post.hoc(value ~ X2 | X1, melt(res.ranks))
res.p.val <- c(rep(NA, 4), res.fm.ph$PostHoc.Test[3], rep(NA, 3), 
               res.fm.ph$PostHoc.Test[c(2, 6)], rep(NA, 2),
               res.fm.ph$PostHoc.Test[c(1, 5, 4)], NA)
res.mrank <- colMeans(res.ranks)
table.3   <- matrix(cbind(matrix(res.p.val, 4, 4), round(res.mrank, 2)), 4, 5, 
                    dimnames = list(names(res.mrank), c(names(res.mrank), "Mean rank")))


# Figure 4. Ratio of algorithms to be in top-n ranks
res.ranks <- cbind(Data = res.acc.all$Data, 
                   as.data.frame(t(apply(res.acc.all[, 3:6], 1, function(temp){rank(-temp, ties.method = "min")}))), 
                   Round = res.acc.all$Round)

res.ratio <- as.numeric(t(apply(res.ranks[, 2:5], 2, 
                                function(temp){c(length(which(temp <= 1)), length(which(temp <= 2)), 
                                                 length(which(temp <= 3)))}) / nrow(res.ranks)))

rank.ratio <- data.frame(Ranking   = c("Top1","Top1","Top1","Top1",
                                       "Top2","Top2","Top2","Top2",
                                       "Top3","Top3","Top3","Top3"),
                         Algorithm = c(1:length(res.ranks[, 2:5])),
                         Ratio     = round(res.ratio, 2))

ggplot(data = rank.ratio, aes(x = Ranking, y = Ratio, fill = factor(Algorithm))) +
  geom_bar (stat="identity", position=position_dodge (), width = 0.5) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0.0,1.05)) +
  theme_bw() +
  theme(legend.position = c(0.07,0.85), legend.title = element_text(size = 15)) +
  labs(x = "Rank", y = "Ratio", fill = "Algorithm") +
  theme(axis.title.x = element_text(family ='sans' , face = 2, color = 'black', size=12)) +
  theme(axis.title.y = element_text(family ='sans' , face = 2, color = 'black', size=12)) +
  theme(axis.text.x = element_text(family ='sans' , face = 2, color = 'black', size=12)) +
  theme(axis.text.y = element_text(family ='sans' , face = 2, color = 'black', size=12)) +
  theme(plot.title = element_text(family ='sans' , face = 2, color = 'black', size=20)) +
  theme(legend.text=element_text(size=12)) +
  geom_text(aes(label=Ratio), color="black", vjust=-0.5, position = position_dodge(0.5), size=4) +
  scale_fill_manual(values=c("green3", "blue2", "black", "red2"),
                    labels = c("AdaBoost", "LogitBoost", "GentleBoost", "FlexBoost")) +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.2,
    default.unit="inch")
  )


#########################################################################################################################
### Experiment
#########################################################################################################################

data <- df.all$Iris3

independent   <- c(1:4)
dependent     <- c(5)

X             <- data[, independent]
y             <- data[, dependent]

kfold.ada(seed = 1,iter = 100)       # (seed, iterations)
kfold.gentle(seed = 1,iter = 100)    # (seed, iterations)
kfold.logit(seed = 1,iter = 100)     # (seed, iterations)
kfold.flex(k.value = 0.34, seed = 1,iter = 100)  # (parameter_k , seed, iterations)

sprintf("adaboost    mean acc : %.6f", mean(unlist(result_b)))
sprintf("gentleboost mean acc : %.6f", mean(unlist(result_g)))
sprintf("logitboost  mean acc : %.6f", mean(unlist(result_l)))
sprintf("flexboost   mean acc : %.6f", mean(unlist(result_h)))