load("A.RData") 
# Create a Data Frame
nominal_data <- data.frame(nominal)
nominal_data

# Question 1 
# Calculate the mean for each assets
mean_stocks <- mean(nominal_data$Stocks)
mean_bonds <- mean(nominal_data$Bonds)
mean_gold <- mean(nominal_data$Gold)
mean_commodities <- mean(nominal_data$Commodities)
mean_tbill <- mean(nominal_data$Tbill)
mean_inflation <- mean(nominal_data$Inflation)
# Calculate the STDEV for each assets
sd_stocks <- sd(nominal_data$Stocks)
sd_bonds <- sd(nominal_data$Bonds)
sd_gold <- sd(nominal_data$Gold)
sd_commodities <- sd(nominal_data$Commodities)
sd_tbill <- sd(nominal_data$Tbill)
sd_inflation <- sd(nominal_data$Inflation)
colnames(nominal_data)

# Question 1&2
(cnames = colnames(nominal_data))
assets = nominal[, cnames[2:6]]
assets.r = assets[, -ncol(assets)]
assets.rf = assets[, ncol(assets)]
assets.mean = colMeans(assets)
assets.r.sd = apply(X = assets.r, MARGIN = 2, FUN = sd)
assets.r.cor = cor(assets.r)

# Question 3
vcv = assets.r.sd %*% t(assets.r.sd) * assets.r.cor

# Question 4
assets.r.mean = assets.mean[-length(assets.mean)]
assets.rf.mean = assets.mean[length(assets.mean)]
vec1 = rep(1, length = length(assets.r.mean))
# allocation of MSR
alloc.r.MSR = t(solve(vcv) %% (assets.r.mean - assets.rf.mean)) / as.numeric(t(vec1) %% solve(vcv) %*% (assets.r.mean - assets.rf.mean))
(alloc.MSR = transform(alloc.r.MSR, rf = 1 - sum(alloc.r.MSR)))
# allocation of GMV
alloc.r.GMV = t(solve(vcv) %% vec1) / as.numeric(t(vec1) %% solve(vcv) %*% vec1)
(alloc.GMV = transform(alloc.r.GMV, rf = 1 - sum(alloc.r.GMV)))
# For each risk
ra = c(1.3, 2.8, 6.5, 10.5, 16.9)
alloc.r.ra = t(sapply(ra, FUN = function(x) {t(solve(vcv) %*% (assets.r.mean - assets.rf.mean)) / x}))
(alloc.ra = transform(alloc.r.ra, rf = 1 - rowSums(alloc.r.ra)))
colnames(alloc.ra) = colnames(alloc.MSR)
## Combine all of them 
alloc = as.matrix(rbind(alloc.MSR, alloc.GMV, alloc.ra))
rownames(alloc) = c('MSR', 'GMV', as.character(ra))
alloc

# Question 5 
# Sharpo Ratio
ER = alloc %*% assets.mean
alloc.r = alloc[, -ncol(alloc)]
std = sapply(
  X = 1:nrow(alloc.r),
  FUN = function(x) {(alloc.r[x, ] %% vcv%% (alloc.r[x, ])) ^0.5}
)
sharpe.Ratio = (ER - assets.rf.mean) / std
ER
std
sharpe.Ratio
# Plot
M1 = as.numeric(t(vec1) %% solve(vcv) %% assets.r.mean)
M2 = as.numeric(t(assets.r.mean) %% solve(vcv) %% assets.r.mean)
M3 = as.numeric(t(vec1) %% solve(vcv) %% vec1)
M4 = M2 * M3 - M1 ^2
M5 = M2 - 2*M1 * assets.rf.mean + M3 * assets.rf.mean ^2
N1 = 1/M4 * (M2 solve(vcv) %% vec1 - M1 * solve(vcv) %*% assets.r.mean)
N2 = 1/M4 * (M3 solve(vcv) %% assets.r.mean - M1 * solve(vcv) %*% vec1)
# with rf
TargetER = seq(0, 0.024, by = 0.0001)
alloc1.r = t((solve(vcv) %% (assets.r.mean - assets.rf.mean)) %% t((TargetER - assets.rf.mean) / M5))
alloc1 = as.matrix(transform(alloc1.r, rf =1 -rowSums(alloc1.r)))
ER1 = alloc1 %*% assets.mean
Std1 = sapply(X = 1:nrow(alloc1.r), FUN = function(x) {(alloc1.r[x,] %% vcv %% (alloc1.r[x,])) ^0.5}
)
# without rf
alloc2 = t(sapply(TargetER, FUN = function(x) {N1 + N2 * x}))
ER2 = alloc2 %*% assets.r.mean
Std2 = sapply(X = 1:nrow(alloc2), FUN = function(x){(alloc2[x,] %% vcv %% (alloc2[x,])) ^0.5})

## draw the froniter
plot(Std1, ER1, type = 'l', col = 'green', lwd = 2
     , main = 'Portfolio Froniter with RF, choice with risk aversion'
     , xlab = 'STDEV', ylab = 'Expected Return'
     , xlim = c(-0.01, 0.15), ylim = c(0,0.025)
     , xaxt = 'n', yaxt = 'n')
par(new = T)
lines(Std2, ER2, col = 'red', lwd =2)
par(new = T)
points(std, ER, pch = 19)
text(std[1:2], ER[1:2], labels = rownames(alloc)[1:2], pos=4)
text(std[3:7], ER[3:7], labels = rownames(alloc)[3:7], pos=2)
axis(1, at=seq(-0.01,0.15,by=0.02), labels = paste0(seq(-1,15,by =2), '.0%'))
axis(2, at=seq(0,0.025,by=0.005), labels = paste0(seq(0,2.5,by =0.5), '%'), las = 2)

# Create a data frame from the ER matrix if it's not already a data frame
ER_df <- as.data.frame(ER)

# Add the standard deviations as a new column to the data frame
ER_df$std <- std

# Now ER_df is a data frame with two columns, the expected return and the standard deviation
print(ER_df)