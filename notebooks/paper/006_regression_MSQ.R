
# Linear regression

# If MSR is correlated with PCR status, then we can use MSR as appr. for disease....

# linear regression, MSQ vs selected variables
# using 007 data. So only Wood's
lm.woods <- lm(log(MSQ) ~ BES_ID + PARITY, data = animal_data) #FIX: add PCR and parity in data.. Or use PCR data...


cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7", "#F0E442", "#0072B2", "#D55E00")

# Residuals vs. Fitted plot
residuals_vs_fitted <- ggplot(data.frame(fitted = lm.woods$fitted.values, residuals = lm.woods$residuals),
                              aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals") +
  ggtitle("Residuals vs. Fitted")

# Normal Q-Q plot
qq_plot <- ggplot(data.frame(residuals = lm.woods$residuals),
                  aes(sample = residuals)) +
  geom_qq(colour= "#0072B2") +
  geom_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")  +
  theme(text = element_text(size = 12))
qq_plot
ggsave("C:/Users/zjt234/PhD/PaperII_woodcurve/FigureS1_QQ.tiff", dpi=300)


# Residuals vs. Leverage plot
leverage_plot <- ggplot(data.frame(hatvalues = hatvalues(lm.woods),
                                   residuals = lm.woods$residuals),
                        aes(x = hatvalues, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Leverage", y = "Residuals") +
  ggtitle("Residuals vs. Leverage")

# Cook's Distance plot
cooks_plot <- ggplot(data.frame(cooks = cooks.distance(lm.woods)),
                     aes(x = seq_along(cooks), y = cooks)) +
  geom_point() +
  labs(x = "Observation", y = "Cook's Distance") +
  ggtitle("Cook's Distance")

# Combine all plots using grid.arrange from the gridExtra package
library(gridExtra)
grid.arrange(residuals_vs_fitted, qq_plot, leverage_plot, cooks_plot, ncol = 2)


# Calculate partial residuals for the 'herd' variable
partial_res_herd <- residuals(lm.woods) + predict(lm.woods, newdata = data.frame(BES_ID = animal_data$BES_ID, PARITY = mean(animal_data$PARITY)))

# Calculate partial residuals for the 'parity' variable
partial_res_parity <- residuals(lm.woods) + predict(lm.woods, newdata = data.frame(BES_ID = mean(animal_data$BES_ID), PARITY = animal_data$PARITY))


anova(lm.woods)
# from this we found that both BES_ID and PARITY has an impact on the model
