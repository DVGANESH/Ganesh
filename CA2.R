# Load necessary libraries
library(readxl)
library(dplyr)

# Load the dataset
data <- read_excel("Dataset_2024.xlsx")

# Inspect the data
str(data)
summary(data)

# Data preprocessing: check for missing values
data <- na.omit(data)

colnames(data)[colnames(data) == "Body fat (%)"] <- "Body_fat"
colnames(data)[colnames(data) == "Age (years)"] <- "Age"
colnames(data)[colnames(data) == "Knee circumference (cm)"] <- "Knee_circumference"
colnames(data)[colnames(data) == "Chest circumference (cm)"] <- "Chest_circumference"
colnames(data)[colnames(data) == "Density (g/cm³)"] <- "Density"
colnames(data)[colnames(data) == "Weight (lbs)"] <- "Weight"

# Pairwise scatter plot to check for linearity and relationships
pairs(data, smooth=FALSE, scale=FALSE, density=TRUE, ellipses=FALSE, method="spearman",
      pch=21, lm=FALSE, cor=TRUE, jiggle=FALSE, factor=2, hist.col=4, stars=TRUE, ci=TRUE)

# Individual scatter plots with smoothing line
par(mfrow= c(3,2)) # Setting up the layout for multiple plots

scatter.smooth(x=data$Age, y=data$Body_fat, xlab="Age", ylab="Body fat", main="Body Fat vs Age")
scatter.smooth(x=data$Chest_circumference, y=data$Body_fat, xlab="Chest Circumference", ylab="Body fat", main="Body Fat vs Chest Circumference")
scatter.smooth(x=data$Density, y=data$Body_fat, xlab="Density", ylab="Body fat", main="Body Fat vs Density")
scatter.smooth(x=data$Knee_circumference, y=data$Body_fat, xlab="Knee Circumference", ylab="Body fat", main="Body Fat vs Knee Circumference")
scatter.smooth(x=data$Weight, y=data$Body_fat, xlab="Weight", ylab="Body fat", main="Body Fat vs Weight")

# Correlation matrix
cor(data)

# Boxplot to visually inspect outliers
par(mfrow=c(3,2)) # Layout for multiple boxplots

boxplot(data$Age, main="Age")
boxplot(data$Body_fat, main="Body_fat")
boxplot(data$Knee_circumference, main="Knee_circumference")
boxplot(data$Chest_circumference, main="Chest_circumference")
boxplot(data$Density, main="Density")
boxplot(data$Weight, main="Weight")

# Detect and remove outliers for each variable
data <- data %>%
  filter(Body_fat < 47.5) %>%
  filter(Chest_circumference < 128.3 & Chest_circumference < 136.2) %>%
  filter(Density < 0.995) %>%
  filter(Knee_circumference < 49.1 & Knee_circumference < 45 & Knee_circumference < 46) %>%
  filter(Weight < 363.15 & Weight < 262.75)
# Check skewness for each variable
par(mfrow = c(3,2)) # Layout for density plots

plot(density(data$Age), main="Density plot: Age", ylab="Frequency", xlab="Age", sub=paste("Skewness:", round(skewness(data$Age), 2)))
polygon(density(data$Age), col="red")

plot(density(data$Body_fat), main="Density plot: Body_fat", ylab="Frequency", xlab="Body_fat", sub=paste("Skewness:", round(skewness(data$Body_fat), 2)))
polygon(density(data$Body_fat), col="red")

plot(density(data$Knee_circumference), main="Density plot: Knee Circumference", ylab="Frequency", xlab="Knee Circumference", sub=paste("Skewness:", round(skewness(data$Knee_circumference), 2)))
polygon(density(data$Knee_circumference), col="red")

plot(density(data$Chest_circumference), main="Density plot: Chest Circumference", ylab="Frequency", xlab="Chest Circumference", sub=paste("Skewness:", round(skewness(data$Chest_circumference), 2)))
polygon(density(data$Chest_circumference), col="red")

plot(density(data$Density), main="Density plot: Density", ylab="Frequency", xlab="Density", sub=paste("Skewness:", round(skewness(data$Density), 2)))
polygon(density(data$Density), col="red")

plot(density(data$Weight), main="Density plot: Weight", ylab="Frequency", xlab="Weight", sub=paste("Skewness:", round(skewness(data$Weight), 2)))
polygon(density(data$Weight), col="red")

# Perform Shapiro-Wilk normality test for each variable
shapiro.test(data$Age)
shapiro.test(data$Body_fat)
shapiro.test(data$Knee_circumference)
shapiro.test(data$Chest_circumference)
shapiro.test(data$Density)
shapiro.test(data$Weight)

# Apply Box-Cox transformation for normalizing the skewed variables
boxcox_transform_age <- boxcox(lm(Body_fat ~ Age, data=data))
lambda_age <- boxcox_transform_age$x[which.max(boxcox_transform_age$y)]
data$Age_transformed <- (data$Age^lambda_age - 1) / lambda_age

# Recheck normality
shapiro.test(data$Age_transformed)

# Apply Box-Cox transformation for Chest Circumference
boxcox_transform_chest <- boxcox(lm(Body_fat ~ Chest_circumference, data=data))
lambda_chest <- boxcox_transform_chest$x[which.max(boxcox_transform_chest$y)]
data$Chest_transformed <- (data$Chest_circumference^lambda_chest - 1) / lambda_chest

# Recheck normality
shapiro.test(data$Chest_transformed)
# Fit the linear model using transformed data where necessary
model <- lm(Body_fat ~ Age_transformed + Chest_transformed + Density + Knee_circumference + Weight, data=data)

# Summary of the model
summary(model)

# Check for multicollinearity
vif(model)

# Residual diagnostics
par(mfrow=c(2,2))
plot(model)

# Shapiro-Wilk test for residuals
shapiro.test(residuals(model))

# Model selection criteria
AIC(model)
BIC(model)


