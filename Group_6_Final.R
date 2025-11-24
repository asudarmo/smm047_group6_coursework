

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
#setwd("C:/cs1")
setwd("C:/Users/ardih/Study/City_St_George/CS1_SMM047/smm047_group6_coursework")

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
# However, looking at boxlot definite outlier from Z data (Yn - Yn-1)
Na_Z_log <- diff(Y_log)
anyNA(Na_Z_log)
Z_log <- na.omit(Na_Z_log)
anyNA(Z_log)

Q1_Z <- quantile(Z_log, 0.25, na.rm = TRUE)
Q3_Z <- quantile(Z_log, 0.75, na.rm = TRUE)
IQR_Z <- Q3_Z - Q1_Z

Lower_Z <- Q1_Z - 3 * IQR_Z
Upper_Z <- Q3_Z + 3 * IQR_Z

outlier_Z_index <- which(Z_log < Lower_Z | Z_log > Upper_Z)
outlier_Z_dates <- index(Z_log)[outlier_Z_index]
outlier_Z_values <- Z_log[outlier_Z_index]
outlier_Z_values

# 1-3 Exclude data from 2020-02-27 to 2020-04-09
Z_log_Clean <- Z_log[!(index(Z_log) >= as.Date("2020-02-27") &
                         index(Z_log) <= as.Date("2020-04-09"))]
Z_log_Clean

# 1-4 calculate Original data and cleaned Z data

Z_mean <- mean(Z_log)
Z_variance <- var(Z_log)
Z_median <- median(Z_log)
Z_quantiles <- quantile(Z_log) 
Z_skewness <- e1071::skewness(Z_log, na.rm = FALSE, type = 3)
Z_kurtosis <- e1071::kurtosis(Z_log, type = 3)

Z_Clean_mean <- mean(Z_log_Clean)
Z_Clean_variance <- var(Z_log_Clean)
Z_Clean_median <- median(Z_log_Clean)
Z_Clean_quantiles <- quantile(Z_log_Clean)
Z_Clean_skewness <- e1071::skewness(Z_log_Clean, na.rm = FALSE, type = 3)
Z_Clean_kurtosis <- e1071::kurtosis(Z_log_Clean, type = 3)

summary_table <- data.frame(
  Statistic = c(
    "Mean", "Variance", "Median",
    "Quantile_0%", "Quantile_25%",
    "Quantile_50%", "Quantile_75%",
    "Quantile_100%", "Skewness", "Kurtosis"
  ),
  
  Z_original = c(
    Z_mean,
    Z_variance,
    Z_median,
    Z_quantiles[1],
    Z_quantiles[2],
    Z_quantiles[3],
    Z_quantiles[4],
    Z_quantiles[5],
    Z_skewness,
    Z_kurtosis
  ),
  
  Z_cleaning = c(
    Z_Clean_mean,
    Z_Clean_variance,
    Z_Clean_median,
    Z_Clean_quantiles[1],
    Z_Clean_quantiles[2],
    Z_Clean_quantiles[3],
    Z_Clean_quantiles[4],
    Z_Clean_quantiles[5],
    Z_Clean_skewness,
    Z_Clean_kurtosis
  )
)
summary_table$Z_original <- round(as.numeric(summary_table$Z_original), 6)
summary_table$Z_cleaning <- round(as.numeric(summary_table$Z_cleaning), 6)
summary_table$Change_percent <- 
  round(((summary_table$Z_cleaning - summary_table$Z_original) / abs(summary_table$Z_original)
  ) * 100, 2)
print(summary_table)

# 1-5 Visualise : time plot, boxplot


windows()
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(Z_log, main = "Z log data(original)")
plot(
  Z_log_Clean,
  main = "Z log data(clean)",
  ylim = range(Z_log),
  col = "lightgreen",
  outline = TRUE
)

windows()
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
boxplot(Z_log,
        main = "Z log data(original)",
        horizontal = TRUE,
        outline = TRUE)
boxplot(
  Z_log_Clean,
  main = "Z log data(clean)",
  horizontal = TRUE,
  ylim = range(Z_log),
  col = "lightgreen",
  outline = TRUE
)

# 1.6 normal test
## to apply to AD test
if (!require(nortest))
  install.packages("nortest", dependencies = TRUE)
library(nortest)

## to apply to jarque.bera test
if (!require(tseries)) {
  install.packages("tseries", dependencies = TRUE)
  library(tseries)
}
library(tseries)

## 1. Kolmogorov–Smirnov Test (KS Test)(bad, require user to supply mean/sd)

 Z_Clean_mean
 Z_Clean_sd <- sd(Z_log_Clean)
 ks_test <- ks.test(Z_log_Clean, "pnorm", mean = Z_Clean_mean, sd = Z_Clean_sd)
 print(ks_test)
 
## 2. Anderson–Darling (good, does not require user to supply mean/sd)

ad_test <- ad.test(as.numeric(Z_log_Clean))
print(ad_test)

## 3. Jarque–Bera Test 
jarque.bera.test(as.numeric(Z_log_Clean))

# ==============================================================================
# element 2 : investigation of normality by resampling
# ==============================================================================

# Install the 'moments' package to calculate statistical methods.
if (!require(moments))
  install.packages("moments", dependencies = TRUE)
library(moments)

# initial sample size and number of simulation
N <- 12 # Sample size
B <- 50000 # Bootstrap and Simulation number

Z_Clean_mean
Z_Clean_variance
Z_Clean_sigma <- sqrt(Z_Clean_variance)

# 2-a simulate form a Normal distribution, and calculate the sample excess kurtosis

sample_excess_kurtosis <- function(x) {
  n <- length(x)
  m4 <- sum((x-mean(x))^4) / (n-1)
  m2 <- sum((x-mean(x))^2) / (n-1)
  return( m4 / m2^2 - 3 )
}

set.seed(2025) # set a fixed random variable
gamma2_normal <- replicate(B, sample_excess_kurtosis(rnorm(N, mean = Z_Clean_mean, sd = Z_Clean_sigma)))

mean(gamma2_normal)
sd(gamma2_normal)
quantile(gamma2_normal, c(0.025, 0.5, 0.975))

windows()
hist(gamma2_normal, main = "Excess kurtosis from normal simulation")
qqnorm(gamma2_normal, main = "Excess kurtosis from normal simulation")

# 2-b resamping to obtain data from using bootstrap sample

set.seed(2025) # set a fixed random variable
gamma2_boot <- replicate(B, sample_excess_kurtosis(sample(Z_log_Clean, size = N, replace = TRUE)))

mean(gamma2_boot)
sd(gamma2_boot)
quantile(gamma2_boot, c(0.025, 0.5, 0.975))

windows()
hist(gamma2_boot, main = "Excess kurtosis from bootrap")
qqnorm(gamma2_boot, main = "Excess kurtosis from bootrap")

# 2-c Use suitable illustrations to investigate the differences between the two sets of values.

result_quantile <- rbind(
  Normal_Simulation = quantile(gamma2_normal, c(0.025, 0.5, 0.975)),
  Bootstrap_zdata   = quantile(gamma2_boot, c(0.025, 0.5, 0.975))
)

print(result_quantile)

if (!require(ggplot2))
  install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

windows()
# Plot density histogram of both datasets (for N=12) in the same axis
data.frame(
  value = c(gamma2_normal, gamma2_boot),
  group = factor(rep(c("Normal", "Bootstrap"), each = B))
) |>  # Plot density histogram
  ggplot(aes(x = value, fill = group, colour = group)) +
  geom_histogram( aes(y=..density..), bins=20, alpha=0.4, position = "identity") +
  geom_density(linewidth = 1, alpha = 0.2) +
  guides(colour = "none") +
  labs(
    title = "Density Histograms of Sample Excess Kurtosis for N = 12",
    x = "Sample Excess Kurtosis",
    y = "Density",
    color = "Group"
  ) +
  theme_minimal()

windows()
# Plot empirical cumulative density function (for N=12)
data.frame(
  value = c(gamma2_normal, gamma2_boot),
  group = factor(rep(c("Normal", "Bootstrap"), each = B))
) |>  # Plot cumulative density
  ggplot(aes(x = value, color = group)) +
  stat_ecdf(linewidth = 1) +
  labs(
    title = "ECDF plot of Sample Excess Kurtosis for N = 12",
    x = "Sample Excess Kurtosis",
    y = "Emprical Cumulative Density",
    color = "Group"
  ) +
  theme_minimal()

# Welch's t-test to check if mean excess kurtosis are the same (for N=12)
t.test(gamma2_normal, gamma2_boot, var.equal = FALSE)

# Kolmogorov-Smirnov test to check if sample excess kurtosis of bootstrapped and simulated normal (for N=12)
# come from the same distribution
ks.test(gamma2_normal, gamma2_boot)


#Investigate when sample size is increased, N=100

set.seed(2025) # set a fixed random variable
gamma2_normal_100 <- replicate(B, sample_excess_kurtosis(rnorm(100, mean = Z_Clean_mean, sd = Z_Clean_sigma)))

set.seed(2025) # set a fixed random variable
gamma2_boot_100 <- replicate(B, sample_excess_kurtosis(sample(Z_log_Clean, size = 100, replace = TRUE)))

mean(gamma2_normal_100)
sd(gamma2_normal_100)

mean(gamma2_boot_100)
sd(gamma2_boot_100)

# Put the mean and sd for both datasets for N=12 and N=100 in dataframe for better comparison and presentation
tbl2.df <- data.frame(
  distribution = c('Normal', 'Normal', 'Bootstrapped', 'Bootstrapped'),
  N = c(12, 100, 12, 100),
  mean = c(mean(gamma2_normal), mean(gamma2_normal_100), mean(gamma2_boot), mean(gamma2_boot_100)),
  sd = c(sd(gamma2_normal), sd(gamma2_normal_100), sd(gamma2_boot), sd(gamma2_boot_100))
)

if (!require(tidyr))
  install.packages("tidyr", dependencies = TRUE)
library(tidyr)

if (!require(kableExtra))
  install.packages("kableExtra", dependencies = TRUE)
library(kableExtra)

# Draw a table for presentation
tbl2.df |> pivot_wider(
  names_from = N,
  values_from = c(mean, sd)
) |>
  kbl(col.names = c('Dataset', 'N=12', 'N=100', 'N=12', 'N=100')) |>
  kable_paper(full_width = F) |>
  add_header_above(c(" " = 1, "Mean" = 2, "Std Dev" = 2))



# ==============================================================================
# element 3 : investigation of constant mean
# ==============================================================================


# 3-0 Divide your z data into 14 subsamples, each representing six months.

if (!require(dplyr))
  install.packages("dplyr", dependencies = TRUE)
library(dplyr)

if (!require(tibble))
  install.packages("tibble", dependencies = TRUE)
library(tibble)

if (!require(lubridate))
  install.packages("lubridate", dependencies = TRUE)
library(lubridate)

Z_grp <- data.frame(Z_log_Clean) |>
  rownames_to_column(var='Date') |>
  rename(z = AORD.Close) |> 
  mutate(group = paste0(substr(year(Date),3,4), "_" , ifelse(month(Date) <= 6, 1, 2)))


# 3-1 Visuals: Barplot with standard errors

# For each group, compute mean, sd, and standard error (se) of mean
group_means_el3 <- Z_grp %>%
  group_by(group) %>%
  summarise(mean = mean(z, na.rm=TRUE),
            sd = sd(z, na.rm=TRUE),
            n = n(),
            se = sd(z, na.rm=TRUE) / sqrt(n())
  )

group_means_el3

# Put group means, sd and se in a table for presentation
group_means_el3_kbl <- group_means_el3 |>
  kbl() |>
  kable_classic(full_width = F)

group_means_el3_kbl

# Draw bar plot with standard error whiskers
windows()
par(mfrow = c(1, 1))
group_means_el3 %>% ggplot(aes(x = group, y = mean, fill = group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  labs(
    title = "Bar Plot with Standard Error",
    x = "Group",
    y = "Mean Value"
  ) +
  theme_minimal()

# 3-2 normal test : method 1 Shapiro-Wilk, method 2 Kolmogorov-Smirnov, method 3 Anderson-Darling

# Utility function to apply Shapiro-Wilk test on group, and get the p-value
get_SW_pval <- function(g){
  return((Z_grp |> filter(group == g) |> pull(z) |> shapiro.test())$p.value)
}

# Utility function to apply Kolmogorov-Smirnov test on group, and get the p-value
get_KS_pval <- function(g){
  z <- Z_grp |> filter(group == g) |> pull(z)
  mu_g <- mean(z)
  sd_g <- sd(z)
  return((ks.test(z, "pnorm", mean = mu_g, sd = sd_g))$p.value)
}

# Utility function to apply Anderson-Darling test on group, and get the p-value
get_AD_pval <- function(g){
  return((Z_grp |> filter(group == g) |> pull(z) |> ad.test())$p.value)
}

# Perform the above 3 tests on each group and save to dataframe
group_normal_tests <- group_means_el3 |> 
  select(c(group)) |>
  rowwise() |>
  mutate(SW_pval = round(get_SW_pval(group), 5),
         KS_pval = round(get_KS_pval(group), 5),
         AD_pval = round(get_AD_pval(group), 5)
  )

group_normal_tests

# For presentation, highlight tests where null hypothesis is rejected at 5% level
# i.e. the particular group is not normally distributed according to that test
group_normal_tests_kbl <- group_normal_tests |>
  mutate_all(~cell_spec(
    .x, 
    background = ifelse(.x < 0.05, "red", "white"))) |> 
  kbl(escape=F) |>
  kable_classic(full_width = F)

group_normal_tests_kbl

### CONCLUSION : KS METHOD FOLLOWS THE NORMAL DISTRIBUTION
###              BUT OTHER METHODS DON'T FOLLOW THE NORMAL DISTRIBUTION
###              WE SHOULD LOOK AT THE DATA BY Kruskal-Wallis's method

kw_result <- kruskal.test(z ~ as.factor(group), data = Z_grp)
kw_result

# Two-sample Wilcoxon test between '19_1' and '22_1', the two seemingly furthest groups in the barplot
wilcox.test((Z_grp %>% dplyr::filter(group == '19_1'))$z,
            (Z_grp %>% dplyr::filter(group == '22_1'))$z,
            paired = FALSE)

# Pairwise Wilcoxon test with Benjamini-Hochberg correction to control FDR due to multiple hyp. testing
pairwise.wilcox.test(Z_grp$z, as.factor(Z_grp$group), paired=FALSE, p.adjust.method = "BH")



# ==============================================================================
# element 4 : investigation of constant variance
# ==============================================================================

## 4-0 calculate the variance each group

Z_log_Clean_group <- Z_grp

group_var_el4 <- Z_log_Clean_group %>%
  group_by(group) %>%
  summarise(mean = mean(z, na.rm=TRUE),
            var = var(z, na.rm=TRUE),
            sd = sd(z, na.rm=TRUE)
  )
group_var_el4 
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
# z_max <- Z_log_Clean_group$z[Z_log_Clean_group$group == '22_1']
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

## 5-0 define the z(n-1), z(n) variables
z_n1 <- Z_log_Clean[-length(Z_log_Clean)] # z1 to z(n-1)
z_n <- Z_log_Clean[-1] # z2 to z(n)

## 5-0-1 define the quartiles(q1,q2,q3)
z_quartiles <- quantile(Z_log_Clean, probs = c(0.25, 0.5, 0.75))
q1_Z_clean <- z_quartiles[1]
q2_Z_clean <- z_quartiles[2]
q3_Z_clean <- z_quartiles[3]

print(z_quartiles)

## 5-1  Classify each of the zn observations according to
## whether it is less than q1, between q1 and q2, between q2 and q3,
## or greater than q3. 

Z_quartile_group_n1 <- cut(
  z_n1,
  breaks = c(-Inf, q1_Z_clean, q2_Z_clean, q3_Z_clean, Inf),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE, right = TRUE
)

Z_quartile_group_n <- cut(
  z_n,
  breaks = c(-Inf, q1_Z_clean, q2_Z_clean, q3_Z_clean, Inf),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE, right = TRUE
)
## 5-2 contingency table
contingency_tbl <- table(Z_quartile_group_n1, Z_quartile_group_n )
contingency_tbl

## 5-3 visual : contingency table
### 5-3-1 : hitmap of contingency table
library(ggplot2)

df_tbl <- as.data.frame(contingency_tbl)
colnames(df_tbl) <- c("Prev", "Next", "Freq")
windows()
ggplot(df_tbl, aes(x = Next, y = Prev, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Contingency Table Heatmap",
       x = "z_n (Next state)", 
       y = "z_{n-1} (Previous state)") +
  theme_minimal()

### 5-3-2 : probability
transition_prob <- prop.table(contingency_tbl, 1)
df_tbl2 <- as.data.frame(transition_prob)
windows()
colnames(df_tbl2) <- c("Prev", "Next", "Prob")

ggplot(df_tbl2, aes(x = Next, y = Prev, fill = Prob)) +
  geom_tile() +
  geom_text(aes(label = round(Prob, 3)), color = "white", size = 4) +
  scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Transition Probability Heatmap",
       x = "z_n (Next state)", 
       y = "z_{n-1} (Previous state)") +
  theme_minimal()

### 5-4 : chisq. test

 chisq.test(Z_quartile_group_n1, Z_quartile_group_n)
 
#### 