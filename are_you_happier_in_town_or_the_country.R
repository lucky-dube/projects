##############################################################
#                                                            #
#              ARE YOU HAPPIER IN TOWN OR COUNTRY            #
#                                                            #
##############################################################

# Import data into R (install the package if you dont already have it on your system)
library(readxl)
ASE_data <- read_excel("# insert path to data here")

## Create subsets of the data

# Subset such that 0<PRP<50 where PRP is the local authority population resident in rural areas
ZeroToFiftyPRP <- subset(ASE_data, PRP < 50, select = c(LAD, MHE, PRP, SCOTLAND))

# Subset such that 50<PRP<100
FiftyToHundredPRP <- subset(ASE_data, PRP > 50, select = c(LAD, MHE, PRP, SCOTLAND))

# Subset for Scotland
ASE_Scotland <- subset(ASE_data, SCOTLAND == 1, select = c(LAD, MHE, PRP))

# Subset for England
EnglandData <- subset(ASE_data, SCOTLAND == 0, select = c(LAD, MHE, PRP))

# Subset for England such that 0<PRP<50
EnglandZeroToFifty <- subset(EnglandData, PRP < 50, select = c(LAD, MHE, PRP))

# Subset for England such that 50<PRP<100
EnglandFiftyToHundred <- subset(EnglandData, PRP > 50, select = c(LAD, MHE, PRP))

# Subset of 0% and 100% PRP
allrural <- subset(ASE_data, PRP == 100, select = c(LAD, MHE, PRP, SCOTLAND))
allurban <- subset(ASE_data, PRP == 0, select = c(LAD, MHE, PRP, SCOTLAND))
allrural$key <- '100% Rural'
allurban$key <- '0% Rural'
ruralandurban <- rbind(allrural, allurban)

## Load variables and regression model 

# OLS regression of local authority mean estimates for happiness (MHE) on PRP
model <- lm(MHE ~ PRP, data = ASE_data)

# Load residuals into u
u <- (residuals(model))

# Load Mean Estimates for Happiness into MHE
MHE <- (ASE_data$MHE)

# Load Percentage Population resident in rural areas into PRP
PRP <- (ASE_data$PRP)

# Load predicted values
MHEhat <- predict.lm(model)

### PLOTS ###

# MHE by PRP
plot(PRP, MHE,
     main = "LAD Mean estimates for happiness by % Population resident in rural areas",
     xlab = "PRP (%)",
     ylab = "MHE (out of 10)",
     pch = 20,
     col = "black")

# Add OLS regression line
abline(lm(MHE ~ PRP), col="black")

# Scotland MHE by PRP
plot(ASE_Scotland$PRP, ASE_Scotland$MHE,
     main = "Scotland",
     xlab = "PRP (%)",
     ylab = "MHE (out of 10)",
     pch = 20,
     col = "black")

# Add OLS regression line
abline(lm(ASE_Scotland$MHE ~ ASE_Scotland$PRP), col = "black")

# England MHE by PRP
plot(EnglandData$PRP, EnglandData$MHE,
     main = "England",
     xlab = "PRP (%)",
     ylab = "MHE (out of 10)",
     pch = 20,
     col = "black")

# Add OLS regression line
abline(lm(EnglandData$MHE ~ EnglandData$PRP), col = "black")

# MHE by PRP for 0<PRP<50
plot(ZeroToFiftyPRP$PRP, ZeroToFiftyPRP$MHE,
     main = "0 < PRP < 50",
     xlab = "PRP (%)",
     ylab = "MHE (out of 10)",
     pch = 20,
     col = "black")

# Add OLS regression line
abline(lm(ZeroToFiftyPRP$MHE ~ ZeroToFiftyPRP$PRP), col = 'black')

# MHE by PRP for 50<PRP<100
plot(FiftyToHundredPRP$PRP, FiftyToHundredPRP$MHE,
     main = "50 < PRP < 100",
     xlab = "PRP (%)",
     ylab = "MHE (out of 10)",
     pch = 20,
     col = "black")

# Add OLS regression line
abline(lm(FiftyToHundredPRP$MHE ~ FiftyToHundredPRP$PRP), col = 'black')

# MHE by PRP for England 0<PRP<50
plot(EnglandZeroToFifty$PRP, EnglandZeroToFifty$MHE,
     main = "England 0<PRP<50",
     xlab = "PRP (%)",
     ylab = "MHE (out of 10)",
     pch = 20,
     col = "black")

# Add OLS regression line
abline(lm(EnglandZeroToFifty$MHE ~ EnglandZeroToFifty$PRP), col = 'black')

# MHE for PRP for England 50<PRP<100
plot(EnglandFiftyToHundred$PRP, EnglandFiftyToHundred$MHE,
     main = "England 50<PRP<100",
     xlab = "PRP (%)",
     ylab = "MHE (out of 10)",
     pch = 20,
     col = "black")

# Add OLS regression line
abline(lm(EnglandFiftyToHundred$MHE ~ EnglandFiftyToHundred$PRP), col = 'black')

# Density plot comparing MHE for 0% and 100% MHE
library(ggplot2)
ggplot(ruralandurban, aes(MHE, fill = key)) + geom_density(alpha = 0.3)

### ANALYSIS ###

# Variable descriptive statistics
library(summarytools)
descr(MHE)
descr(PRP)

# Descriptive statistics for MHE for 0% and 100% Rural
descr(allrural$MHE)
descr(allurban$MHE)

## Regressing MHE on PRP
summary(model)

# Residual plots
library(ggfortify)
autoplot(model, which = 1:4, nrow = 2, ncol = 2)

# RESET test for non-linearity
library(lmtest)
reset(model)

# Test for Heteroscedasticity
library(olsrr)
ols_test_f(model)

## Regressing MHE for PRP for 0<PRP<50
zero_fifty_model <- lm(ZeroToFiftyPRP$MHE ~ ZeroToFiftyPRP$PRP)
summary(zero_fifty_model)

# Residual plots
autoplot(zero_fifty_model, which = 1:1, ncol = 1, nrow = 1)

# RESET test
reset(zero_fifty_model)

# Test for Heteroscedasticity
ols_test_f(zero_fifty_model)

## Regressing MHE on PRP for 50<PRP<100
fifty_hundred_model <- lm(FiftyToHundredPRP$MHE ~ FiftyToHundredPRP$PRP)
summary(fifty_hundred_model)

# Residual plot
autoplot(fifty_hundred_model, which = 1:1, ncol = 1, nrow = 1)

## Regressing MHE on PRP for Scotland
Scotland_model <- lm(ASE_Scotland$MHE ~ ASE_Scotland$PRP)
summary(Scotland_model)

# Residual plots
autoplot(Scotland_model, which = 1:1, ncol = 1, nrow = 1)

# RESET test
reset(Scotland_model)

# Test for Heteroscedasticity
ols_test_f(Scotland_model)

## Regressing MHE on PRP for England 0<PRP<50
England_zero_fifty_model <- lm(EnglandZeroToFifty$MHE ~ EnglandZeroToFifty$PRP)
summary(England_zero_fifty_model)

# Residual plot
autoplot(England_zero_fifty_model, which = 1:1, ncol = 1, nrow = 1)

# RESET test
reset(England_zero_fifty_model)

# Test for Heteroscedasticity
ols_test_f(England_zero_fifty_model)

## Regressing MHE on PRP for England 50<PRP<100
England_fifty_hundred_model <- lm(EnglandFiftyToHundred$MHE ~ EnglandFiftyToHundred$PRP)
summary(England_fifty_hundred_model)

# Residual plot
autoplot(England_fifty_hundred_model, which = 1:1, ncol = 1, nrow = 1)