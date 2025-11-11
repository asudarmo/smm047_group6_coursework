
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
  mfrow = c(1, 1),
  mar = c(4, 3, 3, 1),
  mgp = c(2, 0.7, 0),
  cex=0.9,
  lwd=1.5,
  lty=2,
  pch=16,
  col="steelblue"
)

plot(Z_log_Clean)
boxplot(Z_log_Clean)


# 1.6 normal test
## Anderson–Darling (good, does not require user to supply mean/sd)
if(!require(nortest)) install.packages("nortest", dependencies=TRUE)
library(nortest)
ad_test <- ad.test(as.numeric(Z_log_Clean))
print(ad_test)
