##############################################################
#                                                            #
#                    STATE OF THE UNIONS                     #
#                                                            #
##############################################################

### PLOTS ###

# Import data into R (install the package if you dont already have it on your system)
install.packages('readxl')
library(readxl)
su_data <- read_excel("# insert path to data here")
View(su_data) 

# Create subsets of the data
# subset below nss26 benchmark 
su_data_nss_below <- subset(su_data, nss_deviation_from_benchmark < 0, select = c(university, total_students, income, expenditure, block_grant, no_sabbs, block_grant_per_student, red_dummy, amber_dummy, nss_deviation_from_benchmark))
# subset above nss26 benchmark
su_data_nss_above <- subset(su_data, nss_deviation_from_benchmark > 0, select = c(university, total_students, income, expenditure, block_grant, no_sabbs, block_grant_per_student, red_dummy, amber_dummy, nss_deviation_from_benchmark))
# subset significantly below benchmark
su_data_nss_significantly_below <- subset(su_data, significantly_below_benchmark == 1, select = c(university, total_students, income, expenditure, block_grant, no_sabbs, block_grant_per_student, red_dummy, amber_dummy, nss_deviation_from_benchmark))

# load ggplot (install the package if you need to)
install.packages('ggplot2')
library(ggplot2)

# block grant income bar plots
ggplot(su_data_nss_above, aes(x = university, y = block_grant/1000)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", fill = "black") +
  coord_flip() +
  labs (title = "Block grant by Students' Union: Chart 1", x = "Students' Union", y = "Block grant (£000)") +
  theme_minimal()

ggplot(su_data_nss_below, aes(x = university, y = block_grant/1000)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", fill = "black") +
  coord_flip() +
  labs (title = "Block grant by Students' Union: Chart 2", x = "Students' Union", y = "Block grant (£000)") +
  theme_minimal()

# block grant density plot
ggplot(su_data, aes(block_grant/1000)) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(block_grant/1000)), color = "black", linetype = "dashed", size = 0.3) +
  labs(title = "Students' Union block grant", x = "Block grant (£000)") + 
  theme_minimal()

# block grant per student density plot
ggplot(su_data, aes(block_grant_per_student)) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(block_grant_per_student)), color = "black", linetype = "dashed", size = 0.3) +
  labs(title = "Students' Union block grant per student",x = "Block grant per student (£)") + 
  theme_minimal()

# block grant to income ratio density plot
ggplot(su_data, aes(block_grant_to_income_ratio)) + 
  geom_density() +
  geom_vline(aes(xintercept = mean(block_grant_to_income_ratio)), color = "black", linetype = "dashed", size = 0.3) +
  labs(title = "Students' Union block grant to income ratio", x = "Block grant to income ratio") + 
  theme_minimal()

# log block grant by log total students
ggplot(su_data, aes(x = log(total_students), y = log(block_grant))) +
  geom_point(size = 0.5) +
  geom_smooth(method = lm, size = 0.3, se = FALSE, color = "black") + 
  labs(title = "SU block grant by total students", x = "log (Total students)", y = "log (Block grant)") + 
  theme_minimal()

# block grant to income ratio by total students
ggplot(su_data, aes(x = log(su_data$income), y = block_grant_to_income_ratio)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = lm, size = 0.3, se = FALSE, color = "black") + 
  labs(title = "SU block grant to income ratio by total income" ,x = "log (Total income)", y = "Block grant to income ratio") + 
  theme_minimal()

# SUs performing significantly below benchmark bar plot
ggplot(su_data_nss_significantly_below, aes(x = university, y = nss_deviation_from_benchmark)) +
  geom_bar(stat = "identity", width = 0.5, color = "black", fill = "black") +
  coord_flip() +
  labs(title = "SUs performing significantly below NSS Q26 benchmark", x = "Students' Union", y = "NSS Q26 deviation from benchmark (%)") +
  theme_minimal()

# NSS deviation from bechmark by log block grant per student
ggplot(su_data, aes(x = log(su_data$block_grant_per_student), y = nss_deviation_from_benchmark)) + 
  geom_point(size = 0.5) + 
  labs(title = "NSS deviation from benchmark by block grant per student" ,x = "log (block grant per student)", y = "NSS Q26 deviation from benchmark") + 
  theme_minimal()

# Regressing NSS nss deviation from benchmark on log block grant
nss_bgincome_model <- lm(su_data$nss_deviation_from_benchmark ~ log(su_data$block_grant_per_student))
summary(nss_bgincome_model)

## Generating the density plot comparing block grant to income ratio of SUs above,
## below, and significantly below NSS Q26 benchmark

# create column in dataset
su_data_nss_above$`SU NSS Q26 performance` <- "Above benchmark"
su_data_nss_below$`SU NSS Q26 performance` <- "Below benchmark"
su_data_nss_significantly_below$`SU NSS Q26 performance` <- "Significantly below benchmark"

# Merge datasets with new column
su_data_bgi_above_and_below <- rbind(su_data_nss_above, su_data_nss_below)
su_data_bgi_comparison <- rbind(su_data_bgi_above_and_below, su_data_nss_significantly_below)

# Calculate the means of above, below, and significantly below benchmark
install.packages('plyr')
library(plyr)
mu <- ddply(su_data_bgi_comparison, "`SU NSS Q26 performance`", summarise, grp.mean = mean(block_grant/income))
head(mu)

# Density plot 
ggplot(su_data_bgi_comparison, aes(block_grant/income, color = `SU NSS Q26 performance`)) + 
  geom_density() + 
  geom_vline(data = mu, aes(xintercept = grp.mean, color = `SU NSS Q26 performance`), linetype = "dashed") +
  scale_color_grey() +
  theme_minimal() + 
  labs(title = "Comparison of NSS Q26 performance by block grant to income ratio", x = "Block grant to income ratio")

### ANALYSIS ###

# Average SU election turnout estimate
turn_out <- c(8, 3.4, 5.5, 12.5, 14.1, 11, 6.8, 17, 12.4, 13.6, 6.3, 5.5, 3.06, 11.3, 11.79, 11.2, 7.56, 23.8, 11.1, 5.95, 9, 13.9, 6.7, 11.3, 8.3, 8, 15.3)
t.test(turn_out)
10.16148-8.366459

# Average nus delegate election turnout
turn_out_nus <- c(2.8, 0.5, 5, 3.1, 0.9, 4.1, 1.8, 6.8, 2.06, 4.6, 2.75, 4.4)
t.test(turn_out_nus)
3.234167-2.075073

# Block grant quantiles
quantile(su_data$block_grant, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 1))

# Mean block grant per student
t.test(su_data$block_grant_per_student)

# Block grant per student percentiles
quantile(su_data$block_grant_per_student, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1))

# Mean estimate of block grant
t.test(su_data$block_grant)

# Mean block grant to income ratio
t.test(block_grant_by_income)

# Percentiles of block grant to income ratio
quantile(block_grant_by_income, probs = c(0, 0.5, 0.9, 1))

## Regressing block grant to income ratio on log total income
bg_income_model <- lm(su_data$block_grant_to_income_ratio ~ log(su_data$income))
summary(bg_income_model)
# Test for heteroskedasticity
library(olsrr)
ols_test_f(bg_income_model)
# Generate residual plots
library(ggfortify)
autoplot(bg_income_model)

## Regressing log block grant on log total students
bg_model <- lm(log(su_data$block_grant) ~ log(su_data$total_students))
summary(bg_model)
# Descriptives of regression variables
descr(su_data$block_grant)
descr(su_data$total_students)
# Residual plots
autoplot(bg_model, which = 1:6)
# Test for heteroskedasticity
library(olsrr)
ols_test_f(bg_model)

# Mean estimates of block grant to income ratio of NSS above and below benchmark
t.test(su_data_nss_above$block_grant/su_data_nss_above$income)
mean(su_data_nss_below$block_grant/su_data_nss_below$income)

# Block grant per student of nss q26 significantly below benchmark
t.test(su_data_nss_significantly_below$block_grant_per_student)
t.test(su_data$block_grant_per_student)

