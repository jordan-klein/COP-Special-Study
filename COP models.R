library(reporttools)
library(stargazer)

names(Special)[names(Special) == "Age"] <- "Age (years)"

Special$`Age group` <- factor(Special$`Age group`, levels = c("Older", "Younger"), 
                              labels = c("Older (50-60)", "Younger (20-30)"))
Special$`Socioeconomic status class` <- factor(Special$`Socioeconomic status class`, 
                                               levels = c("L", "M", "H"), 
                              labels = c("Low (0-17)", "Middle (18-39)", "High (40-107)"))

contvars <- Special[, c("Age (years)", "Height (cm)", "Socioeconomic status score")]

tableContinuous(vars = contvars, cap = "Descriptive statistics: continuous variables.", 
                lab = "tab: cont1", longtable = FALSE)

catvars <- Special[, c("Age group", "Socioeconomic status class")]

tableNominal(vars = catvars, cap = "Descriptive statistics: categorical variables.", vertical = FALSE, 
             lab = "tab: nominal1", longtable = FALSE)


model <- lm(`Height (cm)` ~ `Age group`, data = Special)

stargazer(model, title = "Height as a function of age group", style = "demography", 
          notes = "Age group reference = Older (50 -60)")



'Height~Age+ES' <- lm(`Height (cm)` ~ `Age group` + `Socioeconomic status class`, data = Special)
stargazer(`Height~Age+ES`, title = "Height as a function of age group, adjusted for socioeconomic status", 
          style = "demography", notes = "Age group reference = Older (50-60); Socioeconomic status class reference = Low (0-17)")

'Height~Age(L)' <- lm(`Height (cm)` ~ `Age group`, data = Low)
stargazer(`Height~Age(L)`, title = "Low socioeconomic status class stratum: Height as a function of age group", 
          style = "demography", notes = "Age group reference = Older (50 -60)")
'Height~Age(M)' <- lm(`Height (cm)` ~ `Age group`, data = Middle)
stargazer(`Height~Age(M)`, title = "Middle socioeconomic status class stratum: Height as a function of age group", 
          style = "demography", notes = "Age group reference = Older (50 -60)")
'Height~Age(H)' <- lm(`Height (cm)` ~ `Age group`, data = High)
stargazer(`Height~Age(H)`, title = "High socioeconomic status class stratum: Height as a function of age group", 
          style = "demography", notes = "Age group reference = Older (50 -60)")

'Height~ES' <- lm(`Height (cm)` ~ `Socioeconomic status class`, data = Special)
stargazer(`Height~ES`, title = "Height as a function of socioeconomic status class", 
          style = "demography", notes = "Socioeconomic status class reference = Low (0-17)")

chi <- chisq.test(Special$`Socioeconomic status class`, Special$`Age group`)

xtable(tidy(chisq.test(Special$`Socioeconomic status class`, Special$`Age group`)), 
       label = "Chi-squared test of goodness-of-fit of socioeconomic status class and age group")

xtable(tidy(anova(model, `Height~Age+ES`)), label = "Extra sum of squares F-test comparing model of height as a function to age group (model 1) to model of height as a function of age group, adjusted by economic score group (model 2)")

