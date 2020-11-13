##############################################################
#                                                            #
#            LIFE EXPECTANCY AND PER CAPITA GDP              #
#                                                            #
##############################################################

# Load data
library(readxl)
LEPCGDP17 <- read_excel(# insert path to data)
View(LEPCGDP17)

# descriptives of regression variables
library(summarytools)
# life expectancy
descr(LEPCGDP17$LE)
# income per capita
descr(LEPCGDP17$PCGDP)

# OLS model # LE on log per capita GDP
le_model <- lm(LEPCGDP17$LE ~ log(LEPCGDP17$PCGDP))
summary(le_model)

### Diagnostics for LE on log per capita GDP

# test for heteroscedasticity 
library(olsrr)
ols_test_f(le_model)

# reset test
library(lmtest)
resettest(le_model)

# Jarque-Bera test for normally distributed residuals
library(moments)
jarque.test(residuals(le_model))

# OLS model LE on per capita GDP
le_model_two <- lm(LEPCGDP17$LE ~ LEPCGDP17$PCGDP)
summary(le_model_two)

### Diagnostics for LO on per capita GDP
# F test for heteroskedaticity
ols_test_f(le_model_two)

# reset test
reset(le_model_two)

# Jarque-Bera test
jarque.test(residuals(le_model_two))

### Plots
library(ggplot2)

# density plots of variables
# life expectancy
le_density <- ggplot(LEPCGDP17, aes(LE)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(LE)), color = "black", linetype = "dashed", size = 0.3) +
  labs(x = "Life expectancy (years)") +
  theme_minimal()

le_density

# per capita GDP
pcgdp_density <- ggplot(LEPCGDP17, aes(PCGDP)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(PCGDP)), color = "black", linetype = "dashed", size = 0.3) +
  labs(x = "per capita GDP (2011 PPP$)") +
  theme_minimal()

pcgdp_density

# log GDP per capita
lpcgdp_density <- ggplot(LEPCGDP17, aes(log(PCGDP))) +
  geom_density() +
  geom_vline(aes(xintercept = mean(log(PCGDP))), color = "black", linetype = "dashed", size = 0.3) +
  labs(x = "log per capita GDP") +
  theme_minimal()

lpcgdp_density

# life expectancy by log per capita GDP
install.packages("ggrepel")
library(ggrepel)
plot <- ggplot(LEPCGDP17, aes(x = log(PCGDP), y = LE, label = name)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = lm, size = 0.3, se = FALSE, color = "black") + 
  labs(x = "ln (per capita GDP)", y = "Life expectancy (years)") + 
  theme_minimal()

# add labels
plot +
  geom_text_repel(aes(label = name), 
                   size = 2.5,
                   box.padding = 0, 
                   point.padding = 0.1,
                   segment.color = 'grey50')

# residual plot # LE on log GDP per capita
residual_plot <- ggplot(LEPCGDP17, aes(x = fitted.values(le_model), y = residuals(le_model), label = name)) +
  geom_point(size = 0.5) +
  labs(x = "Fitted values", y = "residuals") +
  theme_minimal()

# add labels
residual_plot +
  geom_text_repel(aes(label = ifelse(residuals(le_model) > 5, as.character(name), '')),
                  size = 2.5,
                  segment.color = 'grey50') +
  geom_text_repel(aes(label = ifelse(residuals(le_model) < -5, as.character(name), '')),
                  size = 2.5,
                  segment.color = 'grey50')

# scatter plot for second model
plot_two <- ggplot(LEPCGDP17, aes(x = PCGDP, y = LE, label = name)) + 
  geom_point(size = 0.5) + 
  geom_smooth(method = lm, size = 0.3, se = FALSE, color = "black") + 
  labs(x = "per capita GDP (2011 PPP$)", y = "Life expectancy (years)") + 
  theme_minimal()

# add labels
plot_two 

# residual plot for second model
residual_plot_two <- ggplot(LEPCGDP17, aes(x = fitted.values(le_model_two), y = residuals(le_model_two), label = name)) +
  geom_point(size = 0.5) +
  labs(x = "Fitted values", y = "residuals") +
  theme_minimal()

# add labels
residual_plot_two +
  geom_text_repel(aes(label = ifelse(residuals(le_model_two) > 8, as.character(name), '')),
                  size = 2.5,
                  segment.color = 'grey50') +
  geom_text_repel(aes(label = ifelse(residuals(le_model_two) < -10, as.character(name), '')),
                  size = 2.5,
                  segment.color = 'grey50')