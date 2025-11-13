

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

if (!require(quantmod))
  install.packages("quantmod", dependencies = TRUE)
library(quantmod)
if (!require(e1071))
  install.packages("e1071", dependencies = TRUE)
library(e1071)

# Download All Ordinaries Index (^AORD) data from Yahoo Finance
# Period = 2018-01-01 to 2024-12-31 (Yahoo Finance)
getSymbols("^AORD",
           src = "yahoo",
           from = "2018-01-01",
           to = "2024-12-31")

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
Q1 <- quantile(Y_log, 0.25, na.rm = TRUE)
Q3 <- quantile(Y_log, 0.75, na.rm = TRUE)
IQR_Y <- Q3 - Q1

Lower_Y <- Q1 - 3 * IQR_Y
Upper_Y <- Q3 + 3 * IQR_Y

outlier_index <- which(Y_log < Lower_Y | Y_log > Upper_Y)
outlier_dates <- index(Y_log)[outlier_index]
outlier_values <- Y_log[outlier_index]
outlier_values

# we can't find the outlier from definite outlier
# However, looking at boxplot definite outlier from Z data (Yn - Yn-1)
Na_Z_log <- diff(Y_log)
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

Q1_Z <- quantile(Z_log, 0.25, na.rm = TRUE)
Q3_Z <- quantile(Z_log, 0.75, na.rm = TRUE)
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
Z_log_Clean <- Z_log[!(index(Z_log) >= as.Date("2020-02-27") &
                         index(Z_log) <= as.Date("2020-04-09"))]
Z_log_Clean

# 1-4 calculate cleaned Z data

Z_Clean_mean <- mean(Z_log_Clean)
Z_Clean_variance <- var(Z_log_Clean)
Z_Clean_median <- median(Z_log_Clean)
Z_Clean_quantiles <- quantile(Z_log_Clean)
Z_Clean_skewness <- e1071::skewness(Z_log_Clean, na.rm = FALSE, type = 3)
Z_Clean_kurtosis <- e1071::kurtosis(Z_log_Clean, type = 3)
print(
  list(
    mean = Z_Clean_mean,
    variance = Z_Clean_variance,
    median = Z_Clean_median,
    quantiles = Z_Clean_quantiles,
    skewness = Z_Clean_skewness,
    kurtosis = Z_Clean_kurtosis
  )
)

# 1-5 summary of cleaned Z data

summary(Z_log_Clean)

summary_table_z <- descr(
  Z_log_Clean,
  stats     = c("mean", "sd", "skewness", "kurtosis"),
  transpose = TRUE,
  headings  = FALSE,
  round.digits = 6
)
print(summary_table_z)
windows()
op <- par(
  mfrow = c(2, 1),
  mar = c(4, 3, 3, 1),
  mgp = c(2, 0.7, 0),
  cex = 0.9,
  lwd = 1.5,
  lty = 2,
  pch = 16,
  col = "steelblue"
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
if (!require(nortest))
  install.packages("nortest", dependencies = TRUE)
library(nortest)
ad_test <- ad.test(as.numeric(Z_log_Clean))
print(ad_test)

# ==============================================================================
# element 2 : investigation of normality by resampling
# ==============================================================================

# Install the 'moments' package to calculate statistical methods.
if (!require(moments))
  install.packages("moments", dependencies = TRUE)
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
  m2 <- sum((x - mean(x))^2) / (sample_N - 1)
  m4 <- sum((x - mean(x))^4) / (sample_N - 1)
  return(m4 / (m2^2) - 3)
}

gamma2_normal <- replicate(sample_simulation, excess_kurtosis(rnorm(sample_N, mean = Z_Clean_mean, sd =
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
  Bootstrap_zdata   = quantile(gamma2_boot, c(0.025, 0.5, 0.975))
)

print(result_quantile)

min(gamma2_boot)

windows()
par(mfrow = c(1, 2))
hist(
  gamma2_normal,
  main = "Excess kurtosis from normal simualtion",
  ylim = c(0, 17000) ,
  xlim = c(-2, 7)
)
hist(
  gamma2_boot,
  main = "Excess kurtosis from bootrap",
  ylim = c(0, 17000),
  xlim = c(-2, 7)
)

windows()
par(mfrow = c(1, 2))
qqnorm(gamma2_normal, main = "Excess kurtosis from normal simualtion", ylim = c(-2, 7))
qqline(gamma2_normal)
qqnorm(gamma2_boot, main = "Excess kurtosis from bootrap", ylim = c(-2, 7))
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
boxplot(
  Z_log_Clean ~ Z_group,
  data = Z_log_Clean_group,
  main = "14 subsamples (2018–2024)",
  xlab = "Half-Year Group (YY_1 = firsr half of year , YY_2 = second half of year)",
  ylab = "Z Value",
  las = 2,
  col = "lightgreen",
  outline = TRUE
)

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

library(dplyr)
library(ggplot2)

group_means_el3 <- Z_log_Clean_group %>%
  group_by(group) %>%
  summarise(mean = mean(z, na.rm=TRUE),
            sd = sd(z, na.rm=TRUE),
            se = sd(z, na.rm=TRUE) / sqrt(n())
            )

# Create bar plot with standard error bars
group_means_el3 %>% ggplot(aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(
    title = "Bar Plot with Standard Error",
    x = "Group",
    y = "Mean Value"
  ) +
  theme_minimal()

length((Z_log_Clean_group %>% dplyr::filter(group == '19_1'))$z)
length((Z_log_Clean_group %>% dplyr::filter(group == '22_1'))$z)

wilcox.test((Z_log_Clean_group %>% dplyr::filter(group == '19_1'))$z,
            (Z_log_Clean_group %>% dplyr::filter(group == '22_1'))$z,
            paired = FALSE)

# ==============================================================================
# element 4 : investigation of constant variance
# ==============================================================================

## 4-0 calcuate the variance each group

var_by_group <- aggregate(z ~ group, data = Z_log_Clean_group, FUN =  var)
names(var_by_group) <- c("group", "sample_var")
print(var_by_group)

### 4-1-1 Visuals - looking at the barplot

windows()
par(mfrow = c(1, 1))

barplot(
  var_by_group$sample_var,
  names.arg = var_by_group$group,
  las = 2,
  main = "Sample variance of z by 6-month group",
  xlab = "Half-year group",
  ylab = "Sample variance of z",
  col = "lightblue",
)

### 4-1-1  Visuals - looking at 95% confidence interval

#### bootstrap method
var_group_max <- var_by_group$group[which.max(var_by_group$sample_var)]
var_group_min <- var_by_group$group[which.min(var_by_group$sample_var)]
z_max <- Z_log_Clean_group$z[Z_log_Clean_group$group == var_group_max]
z_min <- Z_log_Clean_group$z[Z_log_Clean_group$group == var_group_min]

simple_boot <- function(x, B = sample_simulation) {
  boot_vals <- replicate(B, var(sample(x, replace = TRUE)))
  quantile(boot_vals, c(0.025, 0.975))
}
ci_max <- simple_boot(z_max)
ci_min <- simple_boot(z_min)
print(ci_max)
print(ci_min)

#### Jackknife method

simple_jackknife <- function(x) {
  n <- length(x)
  jack_vals <- sapply(1:n, function(i) var(x[-i]))
  quantile(jack_vals, c(0.025, 0.975))
}

ci_max_jack <- simple_jackknife(z_max)
ci_min_jack <- simple_jackknife(z_min)

ci_max_jack
ci_min_jack

#### visuals - two methods
ci_lower <- c(ci_max[1],      # max - bootstrap
              ci_min[1],      # min - bootstrap
              ci_max_jack[1], # max - jackknife
              ci_min_jack[1]) # min - jackknife

ci_upper <- c(ci_max[2],
              ci_min[2],
              ci_max_jack[2],
              ci_min_jack[2])

x_min <- min(ci_max[1], ci_min[1], ci_max_jack[1], ci_min_jack[1])
x_max <- max(ci_max[2], ci_min[2], ci_max_jack[2], ci_min_jack[2])

y_pos <- 1:4
y_labels <- c(
  paste0(var_group_max, " (bootstrap)"),
  paste0(var_group_min, " (bootstrap)"),
  paste0(var_group_max, " (jackknife)"),
  paste0(var_group_min, " (jackknife)")
)

plot(0,0,
     xlim = c(x_min, x_max),
     ylim = c(0.5, 4.5),
     yaxt = "n",
     xlab = "Variance (95% CI)",
     ylab = "",
     main = "95% CI: Max vs Min Variance Groups",
     type = "n")

axis(2, at = y_pos, labels = y_labels)
segments(ci_lower, y_pos, ci_upper, y_pos, lwd = 3)


points(var(z_max), 1, pch = 19)
points(var(z_min), 2, pch = 19)
points(var(z_max), 3, pch = 19)
points(var(z_min), 4, pch = 19)

### H₀ : The variances of all groups are equal.
### H₁: At least one group’s variance differs
### Looking at different 4 methods(Bartlett, Levene, Brown–Forsythe, Fligner–Killeen)

#### 4-2 test of variances

#### 4-2-1 Bartlett test
##### Used when the data are assumed to be normally distributed.
##### Since the grouped returns do not follow a normal distribution, the Bartlett test is not reliable here

bartlett.test(z ~ group, data = Z_log_Clean_group)

#### 4-2-2 Levene test
#### Used to assess homogeneity of variances when normality is doubtful

if (!require(car))
  install.packages("car", dependencies = TRUE)
library(car)
leveneTest(z ~ group, data = Z_log_Clean_group, center = mean)

#### 4-2-3 Brown–Forsythe test
#### Based on deviations from the median, it is less affected by skewness and outliers.
#### Used when the data show heavy tails or strong asymmetry.
leveneTest(z ~ group, data = Z_log_Clean_group, center = median)

#### 4-2-4 Fligner–Killeen Test
#### Used when the distribution is unknown or highly non-normal.
#### It is the most robust option for heteroscedasticity under arbitrary distributions.
fligner.test(z ~ group, data = Z_log_Clean_group)

#### comparing four methods

res <- list()
res$Bartlett <- bartlett.test(z ~ group, data = Z_log_Clean_group)$p.value
res$Levene_mean <- leveneTest(z ~ group, data = Z_log_Clean_group, center = mean)$`Pr(>F)`[1]
res$Brown_Forsythe <- leveneTest(z ~ group, data = Z_log_Clean_group, center = median)$`Pr(>F)`[1]
res$Fligner <- fligner.test(z ~ group, data = Z_log_Clean_group)$p.value

simple_table <- data.frame(Test = names(res), P_value = unlist(res))

print(simple_table)

# ==============================================================================
# element 5 : independence of increments
# ==============================================================================

z_quartiles <- quantile(Z_log_Clean, probs = c(0.25, 0.5, 0.75))
q1_Z_clean <- z_quartiles[1]
q2_Z_clean <- z_quartiles[2]
q3_Z_clean <- z_quartiles[3]

print(z_quartiles)

