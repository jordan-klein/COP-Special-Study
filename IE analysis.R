IE <- read.csv("~/Google Drive/GIS/Important Projects/COP IE raw data.csv", header = TRUE)

Low <- filter(COP, SES == "Low")
High <- filter(COP, SES == "High")

(sum(Low[, grepl("income", names(Low), ignore.case = TRUE)])/sum(Low$Household.members))/67

(sum(High[, grepl("income", names(High), ignore.case = TRUE)])/sum(High$Household.members))/67

(sum(Low$`Aggregate income`)/sum(Low$`Household members`))/17.152

(sum(High$`Aggregate income`)/sum(High$`Household members`))/17.152

(log(842.98)+16.717)/.143
(log(2856.56)+16.717)/.143

1/(.143*842.98)
1/(.143*2856.56)
