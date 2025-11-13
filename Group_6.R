
# ==============================================================================
# SMM047 Probability and Mathematical Statistics (Subject CS1) 
# Group 6 :     Mohamoud, Ardi Wara, Bongsu
# Professor:    Dr Russell Gerrard
# Date:         24 November 2025
# ==============================================================================

# ==============================================================================
# Preparation Step(0) eliminating circumstance and varaibles. 
# ==============================================================================

rm(list = ls())       # Remove all objects
graphics.off()        # Close all graphical devices
setwd("C:/cs1")

# ==============================================================================
# Preparation Step (1) Extracting data 
# ==============================================================================

# --------------------------------------------------------------
# Load necessary package
# install.packages("quantmod")
# quantmod = Quantitative Modelling Framework
# The quantmod package is installed to easily download, model, and analyze
# financial market data
# The e1071 package is installed to perform statistical and machine learning 
# tasks, such as SVM, clustering, and skewness/kurtosis calculations.
# --------------------------------------------------------------

if(!require(quantmod)) install.packages("quantmod", dependencies=TRUE)
library(quantmod)  
if(!require(e1071)) install.packages("e1071", dependencies=TRUE)
library(e1071)

# Download All Ordinaries Index (^AORD) data from Yahoo Finance
# Period = 2018-01-01 to 2024-12-31 (Yahoo Finance)
getSymbols("^AORD", src = "yahoo", from = "2018-01-01", to = "2024-12-31")

# ==============================================================================
# Preparation Step (2) extract closing prices and transform
# ==============================================================================

# Extract the closing prices
# Check if the data has been loaded correctly and NA
X_close <- Cl(AORD)
stopifnot(!is.null(X_close))
head(X_close)
tail(X_close)

# Perform a logarithmic transformation using natural log
# Inspect first few transformed values
Na_Y_log <- log(X_close)
head(Na_Y_log)
tail(Na_Y_log)

# Check if there are any missing(NA) values in the dataset
anyNA(Na_Y_log)

# answer is true, so we should remove the missing data
Y_log <- na.omit(Na_Y_log)
anyNA(Y_log)


# ==============================================================================
# Element 1 : data cleaning and standard test of normality
# ==============================================================================

# 1-1  Y : Calculate the sample mean, sample variance, sample median and quartiles.
library(summarytools)
Y_mean <- mean(Y_log)
Y_variance <- var(Y_log)
Y_median <- median(Y_log)
Y_quantiles <- quantile(Y_log)

print(list(
  mean = Y_mean,
  variance = Y_variance,
  median = Y_median,
  quantiles = Y_quantiles
))
summary(Y_log)

# 1-2 Define exclude data relating to market shutdows or Covid-19 pandemic
# looking at boxplot definite outlier from Y_log data
Q1 <- quantile(Y_log, 0.25, na.rm=TRUE)
Q3 <- quantile(Y_log, 0.75, na.rm=TRUE)
IQR_Y <- Q3 - Q1

Lower_Y <- Q1 - 3 * IQR_Y
Upper_Y <- Q3 + 3 * IQR_Y

outlier_index <- which(Y_log < Lower_Y | Y_log > Upper_Y)
outlier_dates <- index(Y_log)[outlier_index]
outlier_values <- Y_log[outlier_index]
outlier_values

# we can't find the outlier from definite outlier
# However, looking at boxplot definite outlier from Z data (Yn - Yn-1)
Na_Z_log <-diff(Y_log)
anyNA(Na_Z_log)
Z_log <- na.omit(Na_Z_log)
anyNA(Z_log)

Z_mean <- mean(Z_log)
Z_variance <- var(Z_log)
Z_median <- median(Z_log)
Z_quantiles <- quantile(Z_log)

print(list(
  mean = Z_mean,
  variance = Z_variance,
  median = Z_median,
  quantiles = Z_quantiles
))
summary(Z_log)

Q1_Z <- quantile(Z_log, 0.25, na.rm=TRUE)
Q3_Z <- quantile(Z_log, 0.75, na.rm=TRUE)
IQR_Z <- Q3_Z - Q1_Z

Lower_Z <- Q1_Z - 3 * IQR_Z
Upper_Z <- Q3_Z + 3 * IQR_Z

outlier_Z_index <- which(Z_log < Lower_Z | Z_log > Upper_Z)
outlier_Z_dates <- index(Z_log)[outlier_Z_index]
outlier_Z_values <- Z_log[outlier_Z_index]
outlier_Z_values

plot(Y_log)
plot(Y_log["2020-02-27/2020-04-09"])

# 1-3 Exclude data from 2020-02-27 to 2020-04-09
Z_log_Clean <- Z_log[!(index(Z_log)>=as.Date("2020-02-27") & index(Z_log) <= as.Date("2020-04-09"))]
Z_log_Clean

# 1-4 calculate cleaned Z data

Z_Clean_mean <- mean(Z_log_Clean)
Z_Clean_variance <- var(Z_log_Clean)
Z_Clean_median <- median(Z_log_Clean)
Z_Clean_quantiles <- quantile(Z_log_Clean)
Z_Clean_skewness <-skewness(Z_log_Clean, na.rm = FALSE, type = 3)
Z_Clean_kurtosis <- kurtosis(Z_log_Clean, type=3)
print(list(
  mean = Z_Clean_mean,
  variance = Z_Clean_variance,
  median = Z_Clean_median,
  quantiles = Z_Clean_quantiles,
  skewness = Z_Clean_skewness,
  kurtosis = Z_Clean_kurtosis
))

# 1-5 summary of cleaned Z data

summary(Z_log_Clean)

summary_table_z <- descr(Z_log_Clean,
                       stats     = c("mean", "sd", "skewness", "kurtosis"),
                       transpose = TRUE,
                       headings  = FALSE,
                       round.digits = 6)
print(summary_table_z)
windows()
op <- par(
  mfrow = c(2, 1),
  mar = c(4, 3, 3, 1),
  mgp = c(2, 0.7, 0),
  cex=0.9,
  lwd=1.5,
  lty=2,
  pch=16,
  col="steelblue"
)


plot(Z_log, main = "Z log data(original)")
plot(
  Z_log_Clean,
  main = "Z log data(clean)",
  ylim = range(Z_log),
  col = "lightgreen",
  outline = TRUE
)

windows()
par(mfrow = c(1, 2)) 
boxplot(Z_log, main = "Z log data(original)")
boxplot(
  Z_log_Clean,
  main = "Z log data(clean)",
  ylim = range(Z_log),
  col = "lightgreen",
  outline = TRUE
)


# 1.6 normal test
## Anderson–Darling (good, does not require user to supply mean/sd)
if(!require(nortest)) install.packages("nortest", dependencies=TRUE)
library(nortest)
ad_test <- ad.test(as.numeric(Z_log_Clean))
print(ad_test)

# ==============================================================================
# element 2 : investigation of normality by resampling
# ==============================================================================

# Install the 'moments' package to calculate statistical methods. 
if(!require(moments)) install.packages("moments", dependencies=TRUE)
library(moments)  

# set a fixed random variable
set.seed(2025)

# initial sample size and number of simulation
sample_N <- 12
sample_simulation <- 50000

Z_Clean_mean 
Z_Clean_variance 
Z_Clean_sigma <- sqrt(Z_Clean_variance)

# 2-a simulate form a Normal distribution, and calculate the sample excess kurtosis

excess_kurtosis <- function(x) {
  m2 <- mean((x - mean(x))^2)
  m4 <- mean((x - mean(x))^4)
  return(m4 / (m2^2) - 3)
}

gamma2_normal <- replicate(
  sample_simulation,
  excess_kurtosis(
    rnorm(sample_N, mean = Z_Clean_mean, sd =
            Z_Clean_sigma)))

mean(gamma2_normal)
sd(gamma2_normal)
quantile(gamma2_normal, c(0.025, 0.5, 0.975))

windows()
hist(gamma2_normal, main = "Excess kurtosis from normal simualtion")
qqnorm(gamma2_normal, main = "Excess kurtosis from normal simualtion")

# 2-b resamping to obtain data from using bootstrap sample

gamma2_boot <- replicate(sample_simulation, excess_kurtosis(sample(
  Z_log_Clean, size = sample_N, replace = TRUE
)))

mean(gamma2_boot)
sd(gamma2_boot)
quantile(gamma2_boot, c(0.025, 0.5, 0.975))

windows()
hist(gamma2_boot, main = "Excess kurtosis from bootrap")
qqnorm(gamma2_boot, main = "Excess kurtosis from bootrap")

# 2-c Use suitable illustrations to investigate the di erences between the two sets of values.

result_quantile <- rbind(
  Normal_Simulation = quantile(gamma2_normal, c(0.025, 0.5, 0.975)),
  Bootstrap_zdata   = quantile(gamma2_boot,   c(0.025, 0.5, 0.975))
)

print(result_quantile)

min(gamma2_boot)

windows()
par(mfrow=c(1,2))
hist(gamma2_normal, main = "Excess kurtosis from normal simualtion", ylim = c(0,17000) , xlim = c(-2,7))
hist(gamma2_boot, main = "Excess kurtosis from bootrap", ylim = c(0,17000), xlim = c(-2,7))

windows()
par(mfrow=c(1,2))
qqnorm(gamma2_normal, main = "Excess kurtosis from normal simualtion", ylim = c(-2,7))
qqline(gamma2_normal)
qqnorm(gamma2_boot, main = "Excess kurtosis from bootrap", ylim = c(-2,7))
qqline(gamma2_boot)

# ==============================================================================
# element 3 : investigation of constant mean
# ==============================================================================


# 3-0 Divide your z data into 14 subsamples, each representing six months. 

Z_date <- as.Date(index(Z_log_Clean))
Z_group <- character(length(Z_date))

for (i in 1:length(Z_date)) {
  year <- format(Z_date[i], "%y")
  month <- as.numeric(format(Z_date[i], "%m"))
  if (month <= 6) {
    Z_group[i] <- paste0(year, "_1")
  } else {
    Z_group[i] <- paste0(year, "_2")
  }
}

Z_log_Clean_group <- data.frame(z = as.numeric(Z_log_Clean), group = Z_group)
Z_log_Clean_group

# 3-1 Visuals: Boxplot by 6-month group
windows()
par(mfrow = c(1, 1))
boxplot(Z_log_Clean ~ Z_group,
        data = Z_log_Clean_group,
        main = "14 subsamples (2018–2024)",
        xlab = "Half-Year Group (YY_1 = firsr half of year , YY_2 = second half of year)",
        ylab = "Z Value",
        las = 2,
        col = "lightgreen",
        outline = TRUE)

# 3-2 normal test : method 1 Shapiro-Wilk, method 2 Kolmogorov-Smirnov, method 3 Anderson-Darling
## setting the variable 
z_val <- as.numeric(Z_log_Clean_group$z)
Z_group_label <- as.character(Z_log_Clean_group$group)
head(z_val)
head(Z_group_label)
Z_groups <- unique(Z_group)
size_group <- length(unique(Z_group))

### define the p-value variable for testing multiple normal test

Sw_group <- Sw_p <- numeric(size_group)
Ks_group <- Ks_p <- numeric(size_group)
Ad_group <- Ad_p <- numeric(size_group) 

#### multiple normal test

for (i in 1:size_group) {
  g  <- Z_groups[i]
  xg <- z_val[Z_group_label == g]   # data in group g
  
  ###### Shapiro–Wilk test (Method 1) 
  ## H0: data in this group come from a Normal distribution
  Sw_res <- shapiro.test(xg)
  Sw_group[i] <- Sw_res$statistic
  Sw_p[i] <- Sw_res$p.value
  
  ###### Kolmogorov–Smirnov test (Method 2) 
  ## Compare empirical CDF with Normal(mean, sd) fitted to this group
  mu_g <- mean(xg)
  sd_g <- sd(xg)
  Ks_res <- ks.test(xg, "pnorm", mean = mu_g, sd = sd_g)
  Ks_group[i] <- Ks_res$statistic
  Ks_p[i] <- Ks_res$p.value
  
  ##### Anderson–Darling test (Method 3) 
  ## H0: data in this group come from a Normal distribution
  Ad_res <- ad.test(xg)
  Ad_group[i] <- Ad_res$statistic
  Ad_p[i] <- Ad_res$p.value
}

# Collect all results in one table
normality_results <- data.frame(
  Z_groups = Z_groups,
  Sw_group  = Sw_group,
  Sw_p  = Sw_p,
  Ks_group  = Ks_group,
  Ks_p  = Ks_p,
  Ad_group  = Ad_group,
  Ad_p  = Ad_p
)
normality_results2 <- normality_results
normality_results2[, -1] <- round(normality_results2[, -1], 6) 
print(normality_results2)

# Create flag columns: O if p-value > 0.05, X otherwise
normality_flag <- normality_results2
normality_flag$Sw_flag <- ifelse(normality_flag$Sw_p > 0.05, "O", "X")
normality_flag$Ks_flag <- ifelse(normality_flag$Ks_p > 0.05, "O", "X")
normality_flag$Ad_flag <- ifelse(normality_flag$Ad_p > 0.05, "O", "X")

# Print with flags
print(normality_flag[, c("Z_groups", "Sw_flag", "Ks_flag", "Ad_flag")])
colSums(normality_flag == "O")

### CONCLUSION : KS METHOD FOLLOWS THE NORMAL DISTRIBUTION
###              BUT OTHER METHODS DON'T FOLLOW THE NORMAL DISTRIBUTION
###              WE SHOULD LOOK AT THE DATA BY Kruskal-Wallis's method 


kw_result <- kruskal.test(z ~ group, data = Z_log_Clean_group)
kw_result
