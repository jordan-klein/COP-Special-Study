library(ggplot2)

ggplot(Special, aes(`Age group`, `Height (cm)`)) + 
  geom_boxplot(color="dark blue", fill="white") + labs(title = "Figure 1: Box plot of height by age group")

ggplot(Special, aes(`Age group`, `Height (cm)`)) + facet_grid(. ~ `Socioeconomic status class`) + 
         geom_boxplot(aes(colour = `Socioeconomic status class`), fill="white") + labs(title = "Figure 2: Box plot of height by age group, stratified by socioeconomic status")
