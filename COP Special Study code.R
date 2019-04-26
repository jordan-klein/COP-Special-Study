### COP Special Study Analysis ###

## Import data

library(tidyverse)
Special <- read.csv("COP 2016 Special Study Data.csv", header = TRUE)
View(Special)

## Data management

# Rename variables

"Group" -> names(Special)[names(Special)=="Group.no.and.Batch"]
"Household no." -> names(Special)[names(Special)=="Map.sl.no."]
"Height (cm)" -> names(Special)[names(Special)=="Height.in.cms"]
"Socioeconomic status score" -> names(Special)[names(Special)=="SES.value"]
"Socioeconomic status class" -> names(Special)[names(Special)=="SES.class"]

# Clean data

Special$`Socioeconomic status class` <- sub("N", "M", Special$`Socioeconomic status class`)
Special[52, 6] = "L"

# Generate age group variable

Special <- mutate(Special, 'Age group' = ifelse(Special$Age >= 50, "Older", "Younger"))

# Establish age group and Socioeconomic status class as factor variables

Special$`Age group` <- as.factor(Special$`Age group`)
Special$`Socioeconomic status class` <- as.factor(Special$`Socioeconomic status class`)

# Subset out Socioeconomic status classes

Low <- subset(Special, `Socioeconomic status class` == "L")
Middle <- subset(Special, `Socioeconomic status class` == "M")
High <- subset(Special, `Socioeconomic status class` == "H")

# Attach dataframe

attach(Special)

## Descriptive statistics

# Write descriptive statistics functions

contstats <- function(x) {
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x, na.rm = TRUE)
  median <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  'Continuous Statistics' <- list("Mean" = mean, 
                                  "Standard Deviation" = sd, 
                                  "Median" = median, "Inter-quartile range" = iqr)
  return(`Continuous Statistics`)
}

catstats <- function(x) {
  table <- table(x)
  proptable <- prop.table(table)
  'Categorical Statistics' <- list("Frequency Table" = table, 
                                   "Table of Proportion" =  proptable)
  return(`Categorical Statistics`)
}

# Descriptive statistics

contstats(Age)
contstats(`Height (cm)`)
contstats(`Socioeconomic status score`)
catstats(`Socioeconomic status class`)
catstats(`Age group`)

### Regression analysis

# Write function & establish reference categories

regout <- function(x) {
  summary <- summary(x)
  confint <- signif(confint(x), digits = 4)
  'Regression' <- list("Regression Output" = summary, 
                       "95 % Confidence Interval" = confint)
  return(`Regression`)
}

Special <- within(Special, `Socioeconomic status class` <- relevel(`Socioeconomic status class`, ref = "L"))
Special <- within(Special, `Age group` <- relevel(`Age group`, ref = "Older"))
attach(Special)

# Height as a function of age group

'Height~Age' <- lm(`Height (cm)` ~ `Age group`)
regout(`Height~Age`)

# Height as a funciton of Socioeconomic status class

'Height~ES' <- lm(`Height (cm)` ~ `Socioeconomic status class`)
regout(`Height~ES`)

# Socioeconomic status class as confounder

'Height~Age+ES' <- lm(`Height (cm)` ~ `Age group` + `Socioeconomic status class`)
regout(`Height~Age+ES`)
anova(`Height~Age`, `Height~Age+ES`)

# Socioeconomic status class as effect modifier

'Height~Age+Age:ES' <- lm(`Height (cm)` ~ `Age group` + `Age group`:`Socioeconomic status class`)
regout(`Height~Age+Age:ES`)
anova(`Height~Age`, `Height~Age+Age:ES`)

# Socioeconomic status class as confounder & effect modifier

'Height~Age+ES+Age:ES' <- lm(`Height (cm)` ~ `Age group` + `Socioeconomic status class` + `Age group`:`Socioeconomic status class`)
regout(`Height~Age+ES+Age:ES`)
anova(`Height~Age+ES`, `Height~Age+ES+Age:ES`)

# Stratified analysis

'Height~Age(L)' <- lm(`Height (cm)` ~ `Age group`, data = Low)
'Height~Age(M)' <- lm(`Height (cm)` ~ `Age group`, data = Middle)
'Height~Age(H)' <- lm(`Height (cm)` ~ `Age group`, data = High)
regout(`Height~Age(L)`)
regout(`Height~Age(M)`)
regout(`Height~Age(H)`)

### Model diagnostics

## Simple linear regression (Height~Age)- Model 1

# Model descriptor variables

"Model 1 residual" <- `Height~Age`$residual
"Model 1 residual^2" <- `Model 1 residual`**2
"Model 1 std residual" <- (`Model 1 residual`- mean(`Model 1 residual`))/sd(`Model 1 residual`)
"Model 1 fitted" <- `Height~Age`$fitted
"Model 1 leverage" <- hatvalues(`Height~Age`)
"Model 1 cookd" <- cooks.distance(`Height~Age`)
"Model 1 cookdlvl" <- vector()
`Model 1 cookdlvl`[`Model 1 cookd` < 0.6] <- 1
`Model 1 cookdlvl`[`Model 1 cookd` >= 0.6 & `Model 1 cookd` < 1] <- 2
`Model 1 cookdlvl`[`Model 1 cookd` >= 1.0] <- 3

# Check for linearity (Scatterplot)
Special$`Age group` <- as.numeric(Special$`Age group`)
Special$`Socioeconomic status class` <- as.numeric(Special$`Socioeconomic status class`)

plot(`Age group`, `Height (cm)`, col = "blue", xlab = "Age group", ylab = "Height (cm)")
lines(loess.smooth(`Age group`, `Height (cm)`, span = 0.5), col = "red", lwd = 2)
abline(`Height~Age`)

# Check normality (Q-Q plot)

qqnorm(`Model 1 residual`)
qqline(`Model 1 residual`, col = 2, lwd = 2)

# Check for homoscedasticity (squares of residuals plot)

plot(`Model 1 fitted`, `Model 1 residual^2`, col = "blue", 
     xlab = "Fitted values", ylab = "Square of residuals")

# Check for outliers and case-influential statistics 
# (standardized residuals, leverage, and cook's distance plot)

plot(`Model 1 std residual`, `Model 1 leverage`, col = `Model 1 cookdlvl`, 
     xlab = "Standardized residual", ylab = "Leverage")
abline(v = c(-4, 4), col = "red")
abline(h = (2*(1 + 1))/304, col = "red")
legend("topright", pch = 1, col = c(1, 2, 3), c("<0.6", "0.6 to <1.0", ">=1.0"), 
       cex = 0.7, bg = "white", box.col = "grey", title = "Cook's Distance")

## Linear regression with ES as confounder (Height~Age+ES)- Model 2

# Model descriptor variables

"Model 2 residual" <- `Height~Age+ES`$residual
"Model 2 residual^2" <- `Model 2 residual`**2
"Model 2 std residual" <- (`Model 2 residual`- mean(`Model 2 residual`))/sd(`Model 2 residual`)
"Model 2 fitted" <- `Height~Age+ES`$fitted
"Model 2 CPR" <- `Height~Age+ES`$coefficient[3]*`Socioeconomic status score` + `Model 2 residual`
"Model 2 leverage" <- hatvalues(`Height~Age+ES`)
"Model 2 cookd" <- cooks.distance(`Height~Age+ES`)
"Model 2 cookdlvl" <- vector()
`Model 2 cookdlvl`[`Model 2 cookd` < 0.6] <- 1
`Model 2 cookdlvl`[`Model 2 cookd` >= 0.6 & `Model 2 cookd` < 1] <- 2
`Model 2 cookdlvl`[`Model 2 cookd` >= 1.0] <- 3

# Check for linearity (components-plus residuals plot)

plot(`Socioeconomic status score`, `Model 2 CPR`, col = "blue", xlab = "Economic Score")
lines(loess.smooth(`Socioeconomic status score`, `Model 2 CPR`, span = 0.5), col = "red", lwd = 2)
abline(lm(`Model 2 CPR` ~ `Socioeconomic status score`))

# Check normality (Q-Q plot)

qqnorm(`Model 2 residual`)
qqline(`Model 2 residual`, col = 2, lwd = 2)

# Check for homoscedasticity (squares of residuals plot)

plot(`Model 2 fitted`, `Model 2 residual^2`, col = "blue")

# Check for outliers and case-influential statistics 
# (standardized residuals, leverage, and cook's distance plot)

plot(`Model 2 std residual`, `Model 2 leverage`, col = `Model 2 cookdlvl`, 
     xlab = "Standardized residual", ylab = "Leverage")
abline(v = c(-4, 4), col = "red")
abline(h = (2*(2 + 1))/304, col = "red")
legend("topright", pch = 1, col = c(1, 2, 3), c("<0.6", "0.6 to <1.0", ">=1.0"), 
       cex = 0.7, bg = "white", box.col = "grey", title = "Cook's Distance")

## Stratified simple linear regression (Height~Age, Low)- Model 3a

# Model descriptor variables

"Model 3a residual" <- `Height~Age(L)`$residual
"Model 3a residual^2" <- `Model 3a residual`**2
"Model 3a std residual" <- (`Model 3a residual`- mean(`Model 3a residual`))/sd(`Model 3a residual`)
"Model 3a fitted" <- `Height~Age(L)`$fitted
"Model 3a leverage" <- hatvalues(`Height~Age(L)`)
"Model 3a cookd" <- cooks.distance(`Height~Age(L)`)
"Model 3a cookdlvl" <- vector()
`Model 3a cookdlvl`[`Model 3a cookd` < 0.6] <- 1
`Model 3a cookdlvl`[`Model 3a cookd` >= 0.6 & `Model 3a cookd` < 1] <- 2
`Model 3a cookdlvl`[`Model 3a cookd` >= 1.0] <- 3

# Check for linearity (Scatterplot)

plot(Low$`Age group`, Low$`Height (cm)`, col = "blue", xlab = "Age group", ylab = "Height (cm)")
lines(loess.smooth(Low$`Age group`, Low$`Height (cm)`, span = 0.5), col = "red", lwd = 2)
abline(`Height~Age(L)`)

# Check normality (Q-Q plot)

qqnorm(`Model 3a residual`)
qqline(`Model 3a residual`, col = 2, lwd = 2)

# Check for homoscedasticity (squares of residuals plot)

plot(`Model 3a fitted`, `Model 3a residual^2`, col = "blue", 
     xlab = "Fitted values", ylab = "Square of residuals")

# Check for outliers and case-influential statistics 
# (standardized residuals, leverage, and cook's distance plot)

plot(`Model 3a std residual`, `Model 3a leverage`, col = `Model 3a cookdlvl`, 
     xlab = "Standardized residual", ylab = "Leverage")
abline(v = c(-4, 4), col = "red")
abline(h = (2*(1 + 1))/304, col = "red")
legend("topright", pch = 1, col = c(1, 2, 3), c("<0.6", "0.6 to <1.0", ">=1.0"), 
       cex = 0.7, bg = "white", box.col = "grey", title = "Cook's Distance")

## Stratified simple linear regression (Height~Age, Middle)- Model 3b

# Model descriptor variables

"Model 3b residual" <- `Height~Age(M)`$residual
"Model 3b residual^2" <- `Model 3b residual`**2
"Model 3b std residual" <- (`Model 3b residual`- mean(`Model 3b residual`))/sd(`Model 3b residual`)
"Model 3b fitted" <- `Height~Age(M)`$fitted
"Model 3b leverage" <- hatvalues(`Height~Age(M)`)
"Model 3b cookd" <- cooks.distance(`Height~Age(M)`)
"Model 3b cookdlvl" <- vector()
`Model 3b cookdlvl`[`Model 3b cookd` < 0.6] <- 1
`Model 3b cookdlvl`[`Model 3b cookd` >= 0.6 & `Model 3b cookd` < 1] <- 2
`Model 3b cookdlvl`[`Model 3b cookd` >= 1.0] <- 3

# Check for linearity (Scatterplot)

plot(Middle$`Age group`, Middle$`Height (cm)`, col = "blue", xlab = "Age group", ylab = "Height (cm)")
lines(loess.smooth(Middle$`Age group`, Middle$`Height (cm)`, span = 0.5), col = "red", lwd = 2)
abline(`Height~Age(M)`)

# Check normality (Q-Q plot)

qqnorm(`Model 3b residual`)
qqline(`Model 3b residual`, col = 2, lwd = 2)

# Check for homoscedasticity (squares of residuals plot)

plot(`Model 3b fitted`, `Model 3b residual^2`, col = "blue", 
     xlab = "Fitted values", ylab = "Square of residuals")

# Check for outliers and case-influential statistics 
# (standardized residuals, leverage, and cook's distance plot)

plot(`Model 3b std residual`, `Model 3b leverage`, col = `Model 3b cookdlvl`, 
     xlab = "Standardized residual", ylab = "Leverage")
abline(v = c(-4, 4), col = "red")
abline(h = (2*(1 + 1))/304, col = "red")
legend("topright", pch = 1, col = c(1, 2, 3), c("<0.6", "0.6 to <1.0", ">=1.0"), 
       cex = 0.7, bg = "white", box.col = "grey", title = "Cook's Distance")

## Stratified simple linear regression (Height~Age, High)- Model 3c

# Model descriptor variables

"Model 3c residual" <- `Height~Age(H)`$residual
"Model 3c residual^2" <- `Model 3c residual`**2
"Model 3c std residual" <- (`Model 3c residual`- mean(`Model 3c residual`))/sd(`Model 3c residual`)
"Model 3c fitted" <- `Height~Age(H)`$fitted
"Model 3c leverage" <- hatvalues(`Height~Age(H)`)
"Model 3c cookd" <- cooks.distance(`Height~Age(H)`)
"Model 3c cookdlvl" <- vector()
`Model 3c cookdlvl`[`Model 3c cookd` < 0.6] <- 1
`Model 3c cookdlvl`[`Model 3c cookd` >= 0.6 & `Model 3c cookd` < 1] <- 2
`Model 3c cookdlvl`[`Model 3c cookd` >= 1.0] <- 3

# Check for linearity (Scatterplot)

plot(High$`Age group`, High$`Height (cm)`, col = "blue", xlab = "Age group", ylab = "Height (cm)")
lines(loess.smooth(High$`Age group`, High$`Height (cm)`, span = 0.5), col = "red", lwd = 2)
abline(`Height~Age(H)`)

# Check normality (Q-Q plot)

qqnorm(`Model 3c residual`)
qqline(`Model 3c residual`, col = 2, lwd = 2)

# Check for homoscedasticity (squares of residuals plot)

plot(`Model 3c fitted`, `Model 3c residual^2`, col = "blue", 
     xlab = "Fitted values", ylab = "Square of residuals")

# Check for outliers and case-influential statistics 
# (standardized residuals, leverage, and cook's distance plot)

plot(`Model 3c std residual`, `Model 3c leverage`, col = `Model 3c cookdlvl`, 
     xlab = "Standardized residual", ylab = "Leverage")
abline(v = c(-4, 4), col = "red")
abline(h = (2*(1 + 1))/304, col = "red")
legend("topright", pch = 1, col = c(1, 2, 3), c("<0.6", "0.6 to <1.0", ">=1.0"), 
       cex = 0.7, bg = "white", box.col = "grey", title = "Cook's Distance")

# Percent growth function

pctgrowth <- function(x, y) {
  'Percent growth' <- ((y - x)/x)*100
  return(`Percent growth`)
}
