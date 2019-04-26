Special <- read.csv("COP 2016 Special Study Data.csv", header = TRUE)

library(reporttools)
library(stargazer)
library(tidyverse)
library(broom)

"Group" -> names(Special)[names(Special)=="Group.no.and.Batch"]
"Household no." -> names(Special)[names(Special)=="Map.sl.no."]
"Height (cm)" -> names(Special)[names(Special)=="Height.in.cms"]
"Socioeconomic status score" -> names(Special)[names(Special)=="SES.value"]
"Socioeconomic status class" -> names(Special)[names(Special)=="SES.class"]
names(Special)[names(Special) == "Age"] <- "Age (years)"

Special <- mutate(Special, 'Age group' = ifelse(Special$`Age (years)` >= 50, "Older", "Younger"))

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

