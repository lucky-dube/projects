##############################################################
#                                                            #
#         DIVIDENDS AND EARNINGS IN THE S&P 500              #
#                                                            #
##############################################################

# load data
library(readxl)
Shiller16 <- read_excel("#insert path to data here")
View(dataset)

# create ts objects
ND <- ts(Shiller16$ND, start = 1871)
NE <- ts(Shiller16$NE, start = 1871)
ld <- ts(log(Shiller16$ND), start = 1871)
le <- ts(log(Shiller16$NE), start = 1871)
po <- ts((Shiller16$ND/Shiller16$NE), start = 1871)

# plot of nominal dividends and nominal earnings
install.packages("astsa")
library(astsa)
tsplot(NE, ylab = "", xlab = "Year", col = "#000000")
lines(ND, col = "#939393")
legend('topleft', col = c("#000000", "#939393"), lwd =2,
       legend = c("Nominal earnings", "Nominal dividends"), bg = "white")

# plot of log dividends and log earnigns
tsplot(le, ylab = "", xlab = "Year", col = "#000000")
lines(ld, col = "#939393")
legend('topleft', col = c("#000000", "#939393"), lwd = 2,
       legend = c("log(Earnings)", "log(Dividends)"), bg = "white")

# plot of payout ratio
tsplot(po, ylab = "Pay-out ratio", xlab = "Year", col = "#000000")

# histogram of pay out ratio
hist(po, xlim = c(0,2), ylim = c(0,50), breaks = 20, main = " ", ylab = "count", xlab = "Pay-out ratio")

# descriptive statistics of payout ratio
library(summarytools)
descr(po)

# percentiles of payout ratios
quantile(na.omit(po), c(0.05, 0.95))

# regress log dividends on log earnings (static model)
static <- dynlm(ld ~ le)
summary(static)

# plot of log dividends and fitted values of static model
tsplot(ld, ylab = " ", xlab = "Year", col = "#000000")
lines(fitted.values(static), col = "#939393")
legend('topleft', col = c("#000000", "#939393"), lwd = 2,
       legend = c("log(dividends)", "Static model fitted values"))

# residual plot for static model
static_residuals <- ts(residuals(static), start = 1871)
tsplot(static_residuals, xlab = "Year", ylab = "residuals")

# durbin watson test of static model
library(lmtest)
dwtest(static)

# f test for heteroskedasticity
library(olsrr)
ols_test_f(static)

# estmating log dividends using an ARDL(1,1) with a time trend
install.packages("dynlm") # install dynmaic liner model package is you need to
library(dynlm)

ardl <- dynlm(ld ~ L(ld) + le + L(le) + Shiller16$YEAR)
summary(ardl)

# plot of log dividends and ardl fitted values
tsplot(ld, ylab = "", xlab = "Year", col = "#000000")
lines(fitted.values(ardl), col = "#939393")
legend('topleft',col = c("#000000", "#939393"), lwd = 2,
       legend = c("log(dividends)", "ARDL model residuals"))

# residuals of ardl model
ardl_residuals <- ts(residuals(ardl), start = 1871)
tsplot(ardl_residuals, xlab = "Year", ylab = "residuals")

# durbin watston statistic for ardl model
dwtest(ardl)

# test hypothesis that coefficents for time and lagged earnings are
# jointly equal to zero
library(car)
linearHypothesis(ardl, test = c("F"), c("Shiller16$YEAR = 0", "L(le) = 0"))

# wald test for linear restrictions: time trend = 0 and all other slope coefficients -1 = 0
linearHypothesis(ardl, test = c("Chisq"), c("Shiller16$YEAR = 0", "L(ld) + le + L(le) = 1"))

# reparametrisied model to test H0: slope coefficents - 1 = 0
repa_ardl <- dynlm(ld - L(ld) ~ L(ld) + (le - L(ld)) + (L(le) - L(ld)))
summary(repa_ardl)         

# estimating a restricted reparameterisation of ardl model where: slope coefficients - 1 = 0;
# time trend = 0
rr_ardl <- dynlm(ld - L(ld) ~ (le - L(ld)) + (L(le) - L(ld)))
summary(rr_ardl)

# likelihood ratio test testing above model against ardl
lrtest(rr_ardl, ardl)
