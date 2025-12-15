# final-stat-proj

# Hi! I am not super familiar with GitHub, so I have made this public and available for you to edit in order to view the code more clearly. Just read the READ.ME in orderr to see things more clearly!

library(faraway)
library(ggplot2)
library(tidyverse)
library(car)
library(modelsummary)

# Load data (loaded using the Faraway library used in a previous course for simplicity. I had issues converting the txt file provided in the assignment over to R)
data(fat, package = "faraway")
head(data)
names(fat)

# Edit dataset down to remove variables that aren't needed for the analysis
data <- fat[, c("brozek","age","weight","height","neck","chest","abdom",
              "hip","thigh","knee","ankle","biceps","forearm","wrist")]


# Plot of distribution of body fat
ggplot(data, aes(x = brozek)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Body Fat (%)", x = "Body Fat (%)", y = "Count")

# Scatterplot of abdomen vs body fat
ggplot(data, aes(x = abdom, y = brozek)) +
  geom_point(color = "darkred", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Abdomen Circumference vs Body Fat", x = "Abdomen (cm)", y = "Body Fat (%)")

# Full model
full_mod <- lm(brozek ~ ., data = data)

# Backward stepwise selection
step_mod <- step(full_mod, direction = "backward")

# Final model summary
summary(step_mod)
modelsummary(step_mod, output = "gt", title = "Model Summary")

# Diagnostics
par(mfrow=c(2,2))
plot(step_mod)   # Residuals, QQ, Scale-Location, Cook’s distance

# Multicollinearity
vif(step_mod)

# Observed vs Predicted
data$pred <- predict(step_mod)
ggplot(data, aes(x = brozek, y = pred)) +
  geom_point(color = "forestgreen", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Observed vs Predicted Body Fat", x = "Observed (%)", y = "Predicted (%)")
