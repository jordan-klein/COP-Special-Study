# Read file

COP <- read.csv("~/Documents/COP IE raw data.csv", header = TRUE)
View(COP)

# Rename variables

"Household members" -> names(COP)[names(COP)=="Household.members"]
"Earning members" -> names(COP)[names(COP)=="Earning.members"]
"Agriculture income per cap" -> names(COP)[names(COP)=="Agriculture.income"]
"Occupational income per cap" -> names(COP)[names(COP)=="Occupational.income"]
"Livestock income per cap" -> names(COP)[names(COP)=="Livestock.income"]
"Other income per cap" -> names(COP)[names(COP)=="Other.income"]
"Food expenditures per cap" -> names(COP)[names(COP)=="Food.expenditures"]
"Healthcare expenditures per cap" -> names(COP)[names(COP)=="Healthcare.expenditures"]
"Education expenditures per cap" -> names(COP)[names(COP)=="Education.expenditures"]
"Mobile phone expenditures per cap" -> names(COP)[names(COP)=="Mobile.phone.expenditures"]
"Alcohol expenditures per cap" -> names(COP)[names(COP)=="Alcohol.expenditures"]
"Fuel, kerosene expenditures per cap" -> names(COP)[names(COP)=="Fuel..kerosene.expenditures"]
"Transport expenditures per cap" -> names(COP)[names(COP)=="Transport.expenditures"]
"Other expenditures per cap" -> names(COP)[names(COP)=="Other.expenditures"]

# Calculate aggregate variables

COP$'Agriculture income' <- COP$`Agriculture income per cap`*COP$`Household members`
COP$'Occupational income' <- COP$`Occupational income per cap`*COP$`Household members`
COP$'Livestock income' <- COP$`Livestock income per cap`*COP$`Household members`
COP$'Other income' <- COP$`Other income per cap`*COP$`Household members`
COP$'Aggregate income' <- COP$'Agriculture income' + COP$'Occupational income' + 
  COP$'Livestock income' + COP$'Other income'
COP$'Aggregate income per cap' <- COP$`Aggregate income`/COP$`Household members`

COP$'Food expenditures' <- COP$`Food expenditures per cap`*COP$`Household members`
COP$'Healthcare expenditures' <- COP$`Healthcare expenditures per cap`*COP$`Household members`
COP$'Education expenditures' <- COP$`Education expenditures per cap`*COP$`Household members`
COP$'Mobile phone expenditures' <- COP$`Mobile phone expenditures per cap`*COP$`Household members`
COP$'Alcohol expenditures' <- COP$`Alcohol expenditures per cap`*COP$`Household members`
COP$'Fuel, kerosene expenditures' <- COP$`Fuel, kerosene expenditures per cap`*COP$`Household members`
COP$'Transport expenditures' <- COP$`Transport expenditures per cap`*COP$`Household members`
COP$'Other expenditures' <- COP$`Other expenditures per cap`*COP$`Household members`
COP$'Aggregate expenditures' <- COP$'Food expenditures' + COP$'Healthcare expenditures' + 
  COP$'Education expenditures' + COP$'Mobile phone expenditures' + COP$'Alcohol expenditures' + 
  COP$'Fuel, kerosene expenditures' + COP$'Transport expenditures' + COP$'Other expenditures'
COP$'Aggregate expenditures per cap' <- COP$`Aggregate expenditures`/COP$`Household members`

COP$'E/I ratio' <- COP$'Aggregate expenditures'/COP$'Aggregate income'

COP$'Household members/household' <- COP$`Household members`/COP$Households
COP$'Earning members/household' <- COP$`Earning members`/COP$Households
COP$'Non-earning members' <- COP$`Household members`- COP$`Earning members`
COP$'Non-earning members/household' <- COP$`Non-earning members`/COP$Households
COP$'Earning/non-earning ratio' <- COP$`Earning members`/COP$`Non-earning members`

# Summary statistics

# Per capita annual income 

# Agriculture income
'Low agriculture income per cap' <- sum(COP$`Agriculture income`[COP$SES=="Low"])/
  sum(COP$`Household members`[COP$SES=="Low"])
'High agriculture income per cap' <- sum(COP$`Agriculture income`[COP$SES=="High"])/
  sum(COP$`Household members`[COP$SES=="High"])
'Cum agriculture income per cap' <- sum(COP$`Agriculture income`)/sum(COP$`Household members`)
mean(COP$`Agriculture income per cap`[COP$SES=="Low"])
mean(COP$`Agriculture income per cap`[COP$SES=="High"])
t.test(COP$`Agriculture income per cap`[COP$SES=="Low"], 
       COP$`Agriculture income per cap`[COP$SES=="High"], paired = TRUE)

# Occupational  income
'Low occupational income per cap' <- sum(COP$`Occupational income`[COP$SES=="Low"])/
  sum(COP$`Household members`[COP$SES=="Low"])
'High occupational income per cap' <- sum(COP$`Occupational income`[COP$SES=="High"])/
  sum(COP$`Household members`[COP$SES=="High"])
'Cum occupational income per cap' <- sum(COP$`Occupational income`)/
  sum(COP$`Household members`)
mean(COP$`Occupational income per cap`[COP$SES=="Low"])
mean(COP$`Occupational income per cap`[COP$SES=="High"])
t.test(COP$`Occupational income per cap`[COP$SES=="Low"], 
       COP$`Occupational income per cap`[COP$SES=="High"], paired = TRUE)

# Livestock  income
'Low livestock income per cap' <- sum(COP$`Livestock income`[COP$SES=="Low"])/
  sum(COP$`Household members`[COP$SES=="Low"])
'High livestock income per cap' <- sum(COP$`Livestock income`[COP$SES=="High"])/
  sum(COP$`Household members`[COP$SES=="High"])
'Cum livestock income per cap' <- sum(COP$`Livestock income`)/sum(COP$`Household members`)
mean(COP$`Livestock income per cap`[COP$SES=="Low"])
mean(COP$`Livestock income per cap`[COP$SES=="High"])
t.test(COP$`Livestock income per cap`[COP$SES=="Low"], 
       COP$`Livestock income per cap`[COP$SES=="High"], paired = TRUE)

# Other  income
'Low other income per cap' <- sum(COP$`Other income`[COP$SES=="Low"])/
  sum(COP$`Household members`[COP$SES=="Low"])
'High other income per cap' <- sum(COP$`Other income`[COP$SES=="High"])/
  sum(COP$`Household members`[COP$SES=="High"])
'Cum other income per cap' <- sum(COP$`Other income`)/sum(COP$`Household members`)
mean(COP$`Other income per cap`[COP$SES=="Low"])
mean(COP$`Other income per cap`[COP$SES=="High"])
t.test(COP$`Other income per cap`[COP$SES=="Low"], 
       COP$`Other income per cap`[COP$SES=="High"], paired = TRUE)

# Aggregate  income
'Low aggregate income per cap' <- sum(COP$`Aggregate income`[COP$SES=="Low"])/
  sum(COP$`Household members`[COP$SES=="Low"])
'High aggregate income per cap' <- sum(COP$`Aggregate income`[COP$SES=="High"])/
  sum(COP$`Household members`[COP$SES=="High"])
'Cum aggregate income per cap' <- sum(COP$`Aggregate income`)/sum(COP$`Household members`)
mean(COP$`Aggregate income per cap`[COP$SES=="Low"])
mean(COP$`Aggregate income per cap`[COP$SES=="High"])
t.test(COP$`Aggregate income per cap`[COP$SES=="Low"], 
       COP$`Aggregate income per cap`[COP$SES=="High"], paired = TRUE)

# Per capita annual expenditures

# Food expenditures
'Low food expenditures per cap' <- 
  sum(COP$`Food expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High food expenditures per cap' <- 
  sum(COP$`Food expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum food expenditures per cap' <- sum(COP$`Food expenditures`)/sum(COP$`Household members`)
mean(COP$`Food expenditures per cap`[COP$SES=="Low"])
mean(COP$`Food expenditures per cap`[COP$SES=="High"])
t.test(COP$`Food expenditures per cap`[COP$SES=="Low"], 
       COP$`Food expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Healthcare expenditures
'Low healthcare expenditures per cap' <- 
  sum(COP$`Healthcare expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High healthcare expenditures per cap' <- 
  sum(COP$`Healthcare expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum healthcare expenditures per cap' <- 
  sum(COP$`Healthcare expenditures`)/sum(COP$`Household members`)
mean(COP$`Healthcare expenditures per cap`[COP$SES=="Low"])
mean(COP$`Healthcare expenditures per cap`[COP$SES=="High"])
t.test(COP$`Healthcare expenditures per cap`[COP$SES=="Low"], 
       COP$`Healthcare expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Education expenditures
'Low education expenditures per cap' <- 
  sum(COP$`Education expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High education expenditures per cap' <- 
  sum(COP$`Education expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum education expenditures per cap' <- 
  sum(COP$`Education expenditures`)/sum(COP$`Household members`)
mean(COP$`Education expenditures per cap`[COP$SES=="Low"])
mean(COP$`Education expenditures per cap`[COP$SES=="High"])
t.test(COP$`Education expenditures per cap`[COP$SES=="Low"], 
       COP$`Education expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Mobile phone expenditures
'Low mobile phone expenditures per cap' <- 
  sum(COP$`Mobile phone expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High mobile phone expenditures per cap' <- 
  sum(COP$`Mobile phone expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum mobile phone expenditures per cap' <- 
  sum(COP$`Mobile phone expenditures`)/sum(COP$`Household members`)
mean(COP$`Mobile phone expenditures per cap`[COP$SES=="Low"])
mean(COP$`Mobile phone expenditures per cap`[COP$SES=="High"])
t.test(COP$`Mobile phone expenditures per cap`[COP$SES=="Low"], 
       COP$`Mobile phone expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Alcohol expenditures
'Low alcohol expenditures per cap' <- 
  sum(COP$`Alcohol expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High alcohol expenditures per cap' <- 
  sum(COP$`Alcohol expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum alcohol expenditures per cap' <- sum(COP$`Alcohol expenditures`)/sum(COP$`Household members`)
mean(COP$`Alcohol expenditures per cap`[COP$SES=="Low"])
mean(COP$`Alcohol expenditures per cap`[COP$SES=="High"])
t.test(COP$`Alcohol expenditures per cap`[COP$SES=="Low"], 
       COP$`Alcohol expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Fuel, kerosene expenditures
'Low fuel, kerosene expenditures per cap' <- 
  sum(COP$`Fuel, kerosene expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High fuel, kerosene expenditures per cap' <- 
  sum(COP$`Fuel, kerosene expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum fuel, kerosene expenditures per cap' <- 
  sum(COP$`Fuel, kerosene expenditures`)/sum(COP$`Household members`)
mean(COP$`Fuel, kerosene expenditures per cap`[COP$SES=="Low"])
mean(COP$`Fuel, kerosene expenditures per cap`[COP$SES=="High"])
t.test(COP$`Fuel, kerosene expenditures per cap`[COP$SES=="Low"], 
       COP$`Fuel, kerosene expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Transport expenditures
'Low transport expenditures per cap' <- 
  sum(COP$`Transport expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High transport expenditures per cap' <- 
  sum(COP$`Transport expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum transport expenditures per cap' <- sum(COP$`Transport expenditures`)/sum(COP$`Household members`)
mean(COP$`Transport expenditures per cap`[COP$SES=="Low"])
mean(COP$`Transport expenditures per cap`[COP$SES=="High"])
t.test(COP$`Transport expenditures per cap`[COP$SES=="Low"], 
       COP$`Transport expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Other expenditures
'Low other expenditures per cap' <- 
  sum(COP$`Other expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High other expenditures per cap' <- 
  sum(COP$`Other expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum other expenditures per cap' <- sum(COP$`Other expenditures`)/sum(COP$`Household members`)
mean(COP$`Other expenditures per cap`[COP$SES=="Low"])
mean(COP$`Other expenditures per cap`[COP$SES=="High"])
t.test(COP$`Other expenditures per cap`[COP$SES=="Low"], 
       COP$`Other expenditures per cap`[COP$SES=="High"], paired = TRUE)

# Aggregate expenditures
'Low aggregate expenditures per cap' <- 
  sum(COP$`Aggregate expenditures`[COP$SES=="Low"])/sum(COP$`Household members`[COP$SES=="Low"])
'High aggregate expenditures per cap' <- 
  sum(COP$`Aggregate expenditures`[COP$SES=="High"])/sum(COP$`Household members`[COP$SES=="High"])
'Cum aggregate expenditures per cap' <- sum(COP$`Aggregate expenditures`)/sum(COP$`Household members`)
mean(COP$`Aggregate expenditures per cap`[COP$SES=="Low"])
mean(COP$`Aggregate expenditures per cap`[COP$SES=="High"])
t.test(COP$`Aggregate expenditures per cap`[COP$SES=="Low"], 
       COP$`Aggregate expenditures per cap`[COP$SES=="High"], paired = TRUE)

# E/I ratios
'Low E/I' <- 
  sum(COP$`Aggregate expenditures`[COP$SES=="Low"])/sum(COP$`Aggregate income`[COP$SES=="Low"])
'High E/I' <- 
  sum(COP$`Aggregate expenditures`[COP$SES=="High"])/sum(COP$`Aggregate income`[COP$SES=="High"])
'Cum E/I' <- sum(COP$`Aggregate expenditures`)/sum(COP$`Aggregate income`)
mean(COP$`E/I ratio`[COP$SES=="Low"])
mean(COP$`E/I ratio`[COP$SES=="High"])
t.test(COP$`E/I ratio`[COP$SES=="Low"], COP$`E/I ratio`[COP$SES=="High"], paired = TRUE)

# Household members per household
mean(COP$`Household members/household`[COP$SES=="Low"])
mean(COP$`Household members/household`[COP$SES=="High"])
t.test(COP$`Household members/household`[COP$SES=="Low"],
       COP$`Household members/household`[COP$SES=="High"], paired = TRUE)

# Earning members per household
mean(COP$`Earning members/household`[COP$SES=="Low"])
mean(COP$`Earning members/household`[COP$SES=="High"])
t.test(COP$`Earning members/household`[COP$SES=="Low"],
       COP$`Earning members/household`[COP$SES=="High"], paired = TRUE)

# Non-earning members per household
mean(COP$`Non-earning members/household`[COP$SES=="Low"])
mean(COP$`Non-earning members/household`[COP$SES=="High"])
t.test(COP$`Non-earning members/household`[COP$SES=="Low"], 
       COP$`Non-earning members/household`[COP$SES=="High"], paired = TRUE)

# Ratio of earning to non-earning members per household
mean(COP$`Earning/non-earning ratio`[COP$SES=="Low"])
mean(COP$`Earning/non-earning ratio`[COP$SES=="High"])
t.test(COP$`Earning/non-earning ratio`[COP$SES=="Low"], 
       COP$`Earning/non-earning ratio`[COP$SES=="High"], paired = TRUE)

# Create aggregated dataframe

'COP Aggregate' <- data.frame(c("Agriculture income","Occupational income","Livestock income",
                                "Other income","Total income","Food expenditures",
                                "Healthcare expenditures","Education expenditures",
                                "Mobile phone expenditures","Alcohol expenditures",
                                "Fuel expenditures","Transport expenditures","Other expenditures",
                                "Total expenditures","E/I ratio"), 
                              c(`Low agriculture income per cap`,`Low occupational income per cap`,
                                `Low livestock income per cap`,`Low other income per cap`,
                                `Low aggregate income per cap`,`Low food expenditures per cap`,
                                `Low healthcare expenditures per cap`,
                                `Low education expenditures per cap`,
                                `Low mobile phone expenditures per cap`,
                                `Low alcohol expenditures per cap`,
                                `Low fuel, kerosene expenditures per cap`,
                                `Low transport expenditures per cap`,`Low other expenditures per cap`,
                                `Low aggregate expenditures per cap`,`Low E/I`), 
                              c(`High agriculture income per cap`,`High occupational income per cap`,
                                `High livestock income per cap`,`High other income per cap`,
                                `High aggregate income per cap`,`High food expenditures per cap`,
                                `High healthcare expenditures per cap`,
                                `High education expenditures per cap`,
                                `High mobile phone expenditures per cap`,
                                `High alcohol expenditures per cap`,
                                `High fuel, kerosene expenditures per cap`,
                                `High transport expenditures per cap`,
                                `High other expenditures per cap`,
                                `High aggregate expenditures per cap`,`High E/I`))                       
View(`COP Aggregate`)
colnames(`COP Aggregate`) <- c("Cashflow","Low SES","High SES")

write.csv(COP, file = "COP revised.csv")

write.csv(`COP Aggregate`, file = "COP Aggregate.csv")