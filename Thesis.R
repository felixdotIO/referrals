##installing relevant packages - some will follow

options(scipen = 999)

install.packages('ggplot2')
library(ggplot2)

install.packages('DescTools')
library(DescTools)

install.packages('dplyr')
library(dplyr)

install.packages('tidyr')
library(tidyr)

install.packages('tidyverse')
library(tidyverse)

install.packages('data.table')
library(data.table)

install.packages('survival')
library(survival)

install.packages('MASS')
library(MASS)

install.packages("sandwich")
library(sandwich)

install.packages("modelsummary")
library(modelsummary)

##subsetting according to sub-chapter data-preprocessing

SD = subset(Dataset2019, most_frequent_community!= 'AT-o') #removing communities with < 500 users -> communities are anonymized
SD = subset(SD, most_frequent_community!= 'A')
SD = subset(SD, most_frequent_community!= 'B')
SD = subset(SD, most_frequent_community!= 'C')
SD = subset(SD, most_frequent_community!= 'D')
SD = subset(SD, most_frequent_community!= 'E')
SD = subset(SD, most_frequent_community!= 'F')
SD = subset(SD, most_frequent_community!= 'G')
SD = subset(SD, most_frequent_community!= 'H')
SD = subset(SD, most_frequent_community!= 'I')
SD = subset(SD, most_frequent_community!= 'J')
SD = subset(SD, most_frequent_community!= 'K')
SD = subset(SD, most_frequent_community!= 'L')
SD = subset(SD, most_frequent_community!= 'M')
SD = subset(SD, most_frequent_community!= 'N')
SD = subset(SD, birth_year < 2014) #removing unrealistic birth years
SD = subset(SD, birth_year > 1920)

colSums(is.na(SD))
SD <- SD[SD$num_active_days >0,] 
SD <- SD[SD$total_spendings >0,]
SD <-na.omit(SD)

# formatting

SD$country = SD$most_frequent_community #create country codes
SD$country[SD$country == 'AT-l'] <- 'AT' #CA1 in the paper
SD$country[SD$country == 'AT-v'] <- 'AT' #CA2 in the paper
SD$country[SD$country == 'CH-f'] <- 'CH' 
SD$country[SD$country == 'DE-g'] <- 'DE' #CG1 in the paper
SD$country[SD$country == 'DE-h'] <- 'DE' #CG2 in the paper

SD$Nummer <- seq.int(nrow(SD)) #create IDs
SD = SD[,c(31,1:30)]
SD = SD[,c(1:9, 11:21, 24:27, 31)]

SD$most_frequent_community[SD$most_frequent_community == "AT-v"] <- "CA2"
SD$most_frequent_community[SD$most_frequent_community == "AT-l"] <- "CA1"
SD$most_frequent_community[SD$most_frequent_community == "CH-f"] <- "CH"
SD$most_frequent_community[SD$most_frequent_community == "DE-g"] <- "CG1"
SD$most_frequent_community[SD$most_frequent_community == "DE-h"] <- "CG2"

#calculating the days existing
SD$days_existing = as.Date(1, origin = '2021-12-31') - as.Date(as.character(SD$created_at))
SD$days_existing_2019 = as.Date(1, origin = '2019-12-31') - as.Date(as.character(SD$created_at))
SD$days_existing_2020 = as.Date(1, origin = '2020-12-31') - as.Date(as.character(SD$created_at))
SD$days_existing_2021 = as.Date(1, origin = '2021-12-31') - as.Date(as.character(SD$created_at))

SD$days_existing_2019 = as.numeric(SD$days_existing_2019)
SD$days_existing_2020 = as.numeric(SD$days_existing_2020)
SD$days_existing_2021 = as.numeric(SD$days_existing_2021)

SD$months_existing_2019 = as.numeric(SD$days_existing_2019)/31
SD$months_existing_2020 = as.numeric(SD$days_existing_2020)/31
SD$months_existing_2021 = as.numeric(SD$days_existing_2021)/31

#calculating the months existing in each year of the observation period by subsetting
sub2019 <- subset(SD, last_active_day <'2020-01-01')
sub2019$days2019 = as.Date(as.character(sub2019$last_active_day)) + 1 - as.Date(as.character(sub2019$created_at))
sub2019$days2020 = '0'
sub2019$days2021 = '0'

sub2020 <- subset(SD, last_active_day > '2020-01-01')
sub2020 <- subset(sub2020, last_active_day < '2021-01-01')
sub2020$days2019 = as.Date(1, origin = '2019-12-31') - as.Date(as.character(sub2020$created_at))
sub2020$days2020 = as.Date(as.character(sub2020$last_active_day)) - as.Date(0, origin = '2020-01-01')+1
sub2020$days2021 = '0'

sub2021 <- subset(SD, last_active_day > '2021-01-01')
sub2021$days2019 = as.Date(1, origin = '2019-12-31') - as.Date(as.character(sub2021$created_at))
sub2021$days2020 = '366'
sub2021$days2021 = as.Date(as.character(sub2021$last_active_day)) - as.Date(0, origin = '2021-01-01')+1
sub2021$days2021[sub2021$days2021>365] <- 365

SDB <- rbind(sub2019, sub2020) #bind the subsets to a whole dataset again
SDB <- rbind(SDB, sub2021)
SDB$days2019 = as.numeric(SDB$days2019)
SDB$days2020 = as.numeric(SDB$days2020)
SDB$days2021 = as.numeric(SDB$days2021)

SDB$reg_month <- substr(SDB$created_at, 6, 7)

SDB$months2019 = SDB$days2019 / 31
SDB$months2020 = SDB$days2020 / 31
SDB$months2021 = SDB$days2021 / 31

SDB$months2019 = as.numeric(SDB$months2019)
SDB$months2020 = as.numeric(SDB$months2020)
SDB$months2021 = as.numeric(SDB$months2021)

SDB$months2019 = ceiling(SDB$months2019)
SDB$months2020 = ceiling(SDB$months2020)
SDB$months2021 = ceiling(SDB$months2021)
SDB$months_total = rowSums(SDB[,c(37, 38, 39)])

# further data cleaning, preparation, and variable creation (focus on spendings/revenue)

SDB$X2019_totalspendings[SDB$X2019_totalspendings<0] <- 0
SDB$X2020_totalspendings[SDB$X2020_totalspendings<0] <- 0
SDB$X2021_totalspendings[SDB$X2021_totalspendings<0] <- 0

SDB$avg_MS_2019 = SDB$X2019_totalspendings / SDB$months2019
SDB$avg_MS_2020 = SDB$X2020_totalspendings / SDB$months2020
SDB$avg_MS_2021 = SDB$X2021_totalspendings / SDB$months2021

SDB$avg_MS_2019[SDB$avg_MS_2019 == 'NaN'] <- '0'
SDB$avg_MS_2020[SDB$avg_MS_2020 == 'NaN'] <- '0'
SDB$avg_MS_2021[SDB$avg_MS_2021 == 'NaN'] <- '0'

SDB$avg_MS_2019 = as.numeric(SDB$avg_MS_2019)
SDB$avg_MS_2020 = as.numeric(SDB$avg_MS_2020)
SDB$avg_MS_2021 = as.numeric(SDB$avg_MS_2021)

SDB$avg_MS_2019[SDB$avg_MS_2019<0] <- 0
SDB$avg_MS_2020[SDB$avg_MS_2020<0] <- 0
SDB$avg_MS_2021[SDB$avg_MS_2021<0] <- 0

SDB$avg_MS_2019[SDB$avg_MS_2019 == 0] <- NA
SDB$avg_MS_2020[SDB$avg_MS_2020 == 0] <- NA
SDB$avg_MS_2021[SDB$avg_MS_2021 == 0] <- NA

SDB$avgS_total = SDB$total_spendings / SDB$months_total

SDB = SDB[,c(1:16, 21, 24:44)]

SDB$winzAVGS <- Winsorize(SDB$avgS_total, probs = c(0.01, 0.99)) #apply winsorizing on overall average spending and on the single years
SDB$winzMS2019 <- Winsorize(SDB$avg_MS_2019, probs = c(0.01, 0.99), na.rm = TRUE)
SDB$winzMS2020 <- Winsorize(SDB$avg_MS_2020, probs = c(0.01, 0.99), na.rm = TRUE)
SDB$winzMS2021 <- Winsorize(SDB$avg_MS_2021, probs = c(0.01, 0.99), na.rm = TRUE)

SDB$zeit = as.Date(as.character(SDB$last_active_day)) + 1 - as.Date(as.character(SDB$created_at)) #calculate survival time
SDB$zeit = as.numeric(SDB$zeit)
hist(SDB$zeit)

SDB$winzREF = SDB$num_mgm_referrals #calculate non-fraudulent referral amount (above 10 is considered fraudulent)
SDB$winzREF[SDB$num_mgm_referrals >= 10] <- 10

SDBall <- na.omit(SDB) #removing users without a spending in any year for long format
SDB2 = SDB

## Creating the Long Format for H1b (panel format for fixed-effects regression)

library(data.table)
SDBLong <- melt(setDT(SDBall), id.vars = c(1:39, 43:44), variable.name = "year") #depending on the order of commands, the number in the brackets may vary (always omit the number of the columns with winzMS2019/20/21)

SDBLong$daysexisting2[SDBLong$year == 'winzMS2019'] <- SDBLong$days_existing_2019
SDBLong$daysexisting2[SDBLong$year == 'winzMS2020'] <- SDBLong$days_existing_2020
SDBLong$daysexisting2[SDBLong$year == 'winzMS2021'] <- SDBLong$days_existing_2021

SDBLong$monthsexisting2[SDBLong$year == 'winzMS2019'] <- SDBLong$months_existing_2019
SDBLong$monthsexisting2[SDBLong$year == 'winzMS2020'] <- SDBLong$months_existing_2020
SDBLong$monthsexisting2[SDBLong$year == 'winzMS2021'] <- SDBLong$months_existing_2021

SDBLong$value = as.numeric(SDBLong$value)

###creating dummy variables

SDB$logMS <- log(SDB$winzAVGS+1) 
SDB$AMD = SDB$num_active_days / SDB$months_total #average days per month (used within robustness checks later)

n <- 10 #assess whale dynamics
XY <- subset(RG, total_spendings > quantile(total_spendings, prob = 1 - n/100)) #top 10 percent with overall spending
X <- subset(RG, winzAVGS <= 5) # users with an AMR of max. 5
min(XY$winzAVGS) #minimum AMR of the top 10 percent
sum(X$total_spendings) / sum(RG$total_spendings) *100 #percentage of the top spender's revenue of the total revenue

SDB$cluster[SDB$winzAVGS < 25] <- as.numeric(0) #create whale cluster
SDB$cluster[SDB$winzAVGS >= 25.00] <- as.numeric(1)

RG = SDB

CA2 = ifelse(RG$most_frequent_community == 'CA2', 1, 0)
CH = ifelse(RG$most_frequent_community == 'CH', 1, 0)
CG1 = ifelse(RG$most_frequent_community == 'CG1', 1, 0)
CG2 = ifelse(RG$most_frequent_community == 'CG2', 1, 0)
AGEtwenty = ifelse(RG$age_group == '0 - 20', 1, 0)
AGEthirty = ifelse(RG$age_group == '20 - 30', 1, 0)
AGEfourty = ifelse(RG$age_group == '30 - 40', 1, 0)
AGEfifty = ifelse(RG$age_group == '40 - 50', 1, 0)
AGEsixty = ifelse(RG$age_group == '50 - 60', 1, 0)
AGEseventy = ifelse(RG$age_group == '60 - 70', 1, 0)
AGEplus = ifelse(RG$age_group == '70+', 1, 0)
Acq_Feb_2019 = ifelse(RG$reg_month == '02', 1, 0)
Acq_Mar_2019 = ifelse(RG$reg_month == '03', 1, 0)
Acq_Apr_2019 = ifelse(RG$reg_month == '04', 1, 0)
Acq_May_2019 = ifelse(RG$reg_month == '05', 1, 0)
Acq_Jun_2019 = ifelse(RG$reg_month == '06', 1, 0)
Acq_Jul_2019 = ifelse(RG$reg_month == '07', 1, 0)
Acq_Aug_2019 = ifelse(RG$reg_month == '08', 1, 0)
Acq_Sep_2019 = ifelse(RG$reg_month == '09', 1, 0)
Acq_Oct_2019 = ifelse(RG$reg_month == '10', 1, 0)
Acq_Nov_2019 = ifelse(RG$reg_month == '11', 1, 0)
Acq_Dez_2019 = ifelse(RG$reg_month == '12', 1, 0)

RG$CA2 = CA2
RG$CH = CH
RG$CG1 = CG1
RG$CG2 = CG2
RG$twenty = AGEtwenty
RG$thirty = AGEthirty
RG$fourty = AGEfourty
RG$fifty = AGEfifty
RG$sixty = AGEsixty
RG$seventy = AGEseventy
RG$seventyplus = AGEplus
RG$Feb = Acq_Feb_2019
RG$Mar = Acq_Mar_2019
RG$Apr = Acq_Apr_2019
RG$May = Acq_May_2019
RG$Jun = Acq_Jun_2019
RG$Jul = Acq_Jul_2019
RG$Aug = Acq_Aug_2019
RG$Sep = Acq_Sep_2019
RG$Oct = Acq_Oct_2019
RG$Nov = Acq_Nov_2019
RG$Dez = Acq_Dez_2019

rm(CA2)
rm(CH)
rm(CG1)
rm(CG2)
rm(AGEtwenty)
rm(AGEthirty)
rm(AGEfourty)
rm(AGEfifty)
rm(AGEsixty)
rm(AGEseventy)
rm(AGEplus)
rm(Acq_Feb_2019)
rm(Acq_Mar_2019)
rm(Acq_Apr_2019)
rm(Acq_May_2019)
rm(Acq_Jun_2019)
rm(Acq_Jul_2019)
rm(Acq_Aug_2019)
rm(Acq_Sep_2019)
rm(Acq_Oct_2019)
rm(Acq_Nov_2019)
rm(Acq_Dez_2019)

#correlation table 

APA = RG
APA <- APA[, -c(1:3, 5, 7:38, 40:42,45:46,52:58)]
APA$age = 2021-APA$birth_year-1
APA$age = as.numeric(APA$age)
APA = APA[,c(2:5,22,6:21)]
APA$cluster <- as.numeric(APA$cluster)
data.cor = cor(APA, method = c("pearson")) #pearson correlation - other options would be spearman or kendall

## H1a model

h1a1model = lm(logMS ~ mgm_acquired, data = RG) #OLS
summary(h1a1model, robust = T)
wilcox.test(logMS ~ mgm_acquired, data = RG) #Mann Whitney Test
plot(lm(logMS~mgm_acquired,data=RG)) # residuals vs. fitted values

h1a2model = lm(logMS ~ CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
               + Feb + Mar + Apr + May + Jun + Jul +Aug + Sep + Oct + Nov + Dez , RG) #model without main effect
summary(h1a2model, robust = T)

h1a3model = lm(logMS ~ mgm_acquired + CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
              + Feb + Mar + Apr + May + Jun + Jul +Aug + Sep + Oct + Nov + Dez , RG) #model with main effect RRP (considered mgm in the dataset)
summary(h1a3model, robust = T)

## H1b model

install.packages("plm")
library(plm) #panel format fixed-effects regression package

subLong1 <- SDBLong
subLong1 = subLong1[,c(1, 6, 42:45)] #removing irrelevant columns
subLong1$logvalue <- log(subLong1$value +1)

h1b1model = plm(value ~ mgm_acquired:monthsexisting2 + factor(year), # random oneway panel model (fixed effects)
                index=c("Nummer"), 
                model = "random", data = subLong1)
summary(h1b1model)

h1b2model = plm(logvalue ~ mgm_acquired:monthsexisting2 + factor(year), # within oneway panel model (fixed effects)
                 index=c("Nummer"), 
                 model = "within", data = subLong1)
summary(h1b2model)

phtest(h1b1model, h1b2model) #Hausman test

# Identification of overall model r-squared 
library(modelsummary)

install.packages("fixest")
library(fixest)
h1b3model = feols(logvalue ~ mgm_acquired:monthsexisting2 | Nummer + year, data = subLong1)
modelsummary(h1b3model)

#survival
library(survival)
library(survminer)
install.packages("rms")
library(rms)

RG$status = RG$activity_status

RG$status[RG$status == 'churned'] <- '1' #activity status based on churn window of 84 days
RG$status[RG$status == 'paused'] <- '0'
RG$status[RG$status == 'active'] <- '0'
RG$status = as.numeric(RG$status)

# Kaplan Meier plots

sfit = survfit(Surv(zeit, status) ~ mgm_acquired, data = RG)
yfit = survfit(Surv(zeit, status) ~ cluster + mgm_acquired, data = RG)
ggsurvplot(sfit,
           pval = TRUE,
           risk.table = "percentage", # Add risk table
           risk.table.col = "strata", 
           linetype = c(1, 8, 1, 8), # Change line type by groups
           break.time.by = 150,
           ggtheme = theme_classic(), # Change ggplot2 theme
           palette = c("#D9DDDC", "#BEBDB8"),
           xlim = c(0, 1150))

ggsurvplot(yfit,
           pval = TRUE,
           risk.table = "percentage", # Add risk table
           risk.table.col = "strata", 
           linetype = c(1, 8, 1, 8), # Change line type by groups
           break.time.by = 150,
           ggtheme = theme_classic(), # Change ggplot2 theme
           palette = c("#D9DDDC", "#BEBDB8", "#808080", "#000000"),
           xlim = c(0, 1150))

#H2a model

myfit = survfit(Surv(log(1+zeit), status) ~ cluster, data = RG) #estimate Log-Log-Curves; insert every covariate; the formula for Schoenfeld test is: test.ph <- cox.zph(Model)
ggsurvplot(myfit, fun = "cloglog")

res.cox.a <- coxph(Surv(zeit, status) ~ mgm_acquired, data =  RG)
summary(res.cox.a)

# Adding covariates and stratified variables
res.cox.b <- coxph(Surv(zeit, status) ~ CA2 + CH + strata(CG1) + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
                   + Feb + strata(Mar) + strata(Apr) + strata(May) + Jun + Jul +Aug + Sep + Oct + strata(Nov) + strata(Dez) + strata(cluster), data =  RG)
summary(res.cox.b)

res.cox.c <- coxph(Surv(zeit, status) ~ mgm_acquired + CA2 + CH + strata(CG1) + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
                   + Feb + strata(Mar) + strata(Apr) + strata(May) + Jun + Jul +Aug + Sep + Oct + strata(Nov) + strata(Dez) + strata(cluster), data =  RG)
summary(res.cox.c)

#H2b Model -> time varying-component through tt()
res.cox2 <- coxph(Surv(zeit, status) ~ mgm_acquired + tt(mgm_acquired) 
                  + strata(most_frequent_community) + strata(age_group) + strata(reg_month) + strata(cluster), 
                  tt = function(x, t, ...) x * log(t), data =  RG)
modelsummary(res.cox2)
summary(res.cox2)

#Pseudo Log-Likelihood
logLik(res.cox.b)
logLik(res.cox.c)
logLik(res.cox2)

## H3 Model
h3model <- glm.nb(winzREF ~ CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
               + factor(reg_month) + factor(cluster), data = RG, link = log)
summary(h3model)

h3model2 <- glm.nb(winzREF ~ mgm_acquired + CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
                + factor(reg_month) + factor(cluster), link = log, data = RG)
summary(h3model2)

# Overdispersion Test
install.packages("pscl")
library(pscl)
odTest(h3model2)

# Split-Sampling based on Age
Young <- subset(RG,RG$birth_year > 1980) #for every model except H1b
Exp <- subset(RG,RG$birth_year <= 1980)
YoungP <- subset(subLong1,subLong1$birth_year > 1980) #for H1b
ExpP <- subset(subLong1,subLong1$birth_year <= 1980)

#H1a age-based
h1aexp = lm(logMS ~ mgm_acquired + CA2 + CH + CG1 + CG2
            + Feb + Mar + Apr + May + Jun + Jul +Aug + Sep + Oct + Nov + Dez, Exp)
summary(h1aexp)
h1ayoung = lm(logMS ~ mgm_acquired + CA2 + CH + CG1 + CG2
              + Feb + Mar + Apr + May + Jun + Jul +Aug + Sep + Oct + Nov + Dez, Young)
summary(h1ayoung)

#H1b age-based
h1b2exp = plm(logvalue ~ mgm_acquired:monthsexisting2 + factor(year), 
              index=c("Nummer"), 
              model = "within", data = ExpP)
summary(h1b2exp)

h1b2young = plm(logvalue ~ mgm_acquired:monthsexisting2 + factor(year), 
                index=c("Nummer"), 
                model = "within", data = YoungP)
summary(h1b2young)

table(RG$X2021_totalspendings == 0)

#H2a age-based (unstratified) 
res.cox.exp <- coxph(Surv(zeit, status) ~ mgm_acquired + CA2 + CH + CG1 + CG2
                     + Feb + Mar + Apr + May + Jun + Jul +Aug + Sep + Oct + Nov + Dez+ cluster, data =  Exp)
summary(res.cox.exp)

res.cox.young <- coxph(Surv(zeit, status) ~ mgm_acquired + CA2 + CH + CG1 + CG2
                       + Feb + Mar + Apr + May + Jun + Jul +Aug + Sep + Oct + Nov + Dez + cluster, data =  Young)
summary(res.cox.young)

#H2b age-based
res.cox.texp <- coxph(Surv(zeit, status) ~ mgm_acquired + tt(mgm_acquired) 
                      + strata(most_frequent_community) + strata(reg_month) + strata(cluster), 
                      tt = function(x, t, ...) x * log(t), data =  Exp)
summary(res.cox.texp)

res.cox.tyoung <- coxph(Surv(zeit, status) ~ mgm_acquired + tt(mgm_acquired) 
                        + strata(most_frequent_community) + strata(reg_month) + strata(cluster), 
                        tt = function(x, t, ...) x * log(t), data =  Young)
summary(res.cox.tyoung)

logLik(res.cox.exp)
logLik(res.cox.young)
logLik(res.cox.tyoung)
logLik(res.cox.texp)

#H3 age-based

h3modelexp <- glm.nb(winzREF ~ mgm_acquired + CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
                   + factor(reg_month) + factor(cluster), link = log, data = Exp)
summary(h3modelexp)

h3modelyoung <- glm.nb(winzREF ~ mgm_acquired + CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
                     + factor(reg_month) + factor(cluster), link = log, data = Young)
summary(h3modelyoung)

#robustness checks
#H1a robustness check
RG$winzAVGD <- Winsorize(RG$AMD, probs = c(0.01, 0.99))
RG$logAMD <- log(RG$winzAVGD +1)

h1amodelrobust = lm(logAMD ~ mgm_acquired + CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus
              + Feb + Mar + Apr + May + Jun + Jul +Aug + Sep + Oct + Nov + Dez , RG)
summary(h1amodelrobust, robust = T)

# H1b robustness check
RG$avg_MD_2019 = RG$X2019_numberofactivedays / RG$months2019
RG$avg_MD_2020 = RG$X2020_numberofactivedays / RG$months2020
RG$avg_MD_2021 = RG$X2021_numberofactivedays / RG$months2021

table(is.na(RG$avg_MD_2021))
table(RG$avg_MD_2021 == 0)

RG$avg_MD_2019[RG$avg_MD_2019 == 'NaN'] <- '0'
RG$avg_MD_2020[RG$avg_MD_2020 == 'NaN'] <- '0'
RG$avg_MD_2021[RG$avg_MD_2021 == 'NaN'] <- '0'

RG$avg_MD_2019 = as.numeric(RG$avg_MD_2019)
RG$avg_MD_2020 = as.numeric(RG$avg_MD_2020)
RG$avg_MD_2021 = as.numeric(RG$avg_MD_2021)

RG$avg_MD_2019[RG$avg_MD_2019<0] <- 0
RG$avg_MD_2020[RG$avg_MD_2020<0] <- 0
RG$avg_MD_2021[RG$avg_MD_2021<0] <- 0

RG$winzMD2019 <- Winsorize(RG$avg_MD_2019, probs = c(0.01, 0.99), na.rm = TRUE)
RG$winzMD2020 <- Winsorize(RG$avg_MD_2020, probs = c(0.01, 0.99), na.rm = TRUE)
RG$winzMD2021 <- Winsorize(RG$avg_MD_2021, probs = c(0.01, 0.99), na.rm = TRUE)

RGrob = subset(RG, RG$winzMD2019 > 0)
RGrob = subset(RGrob, RGrob$winzMD2020 > 0)
RGrob = subset(RGrob, RGrob$winzMD2021 > 0)

## Creating the Long Format for H1b robustness check
library(data.table)
RGLong <- melt(setDT(RGrob), id.vars = c(1:75), variable.name = "year") #depending on the order of commands, the number in the brackets may vary (always omit the number of the columns with winzMD2019/20/21)
summary(RGLong$logvalue)

RGLong$daysexisting2[RGLong$year == 'winzMD2019'] <- RGLong$days_existing_2019
RGLong$daysexisting2[RGLong$year == 'winzMD2020'] <- RGLong$days_existing_2020
RGLong$daysexisting2[RGLong$year == 'winzMD2021'] <- RGLong$days_existing_2021

RGLong$monthsexisting2[RGLong$year == 'winzMD2019'] <- RGLong$months_existing_2019
RGLong$monthsexisting2[RGLong$year == 'winzMD2020'] <- RGLong$months_existing_2020
RGLong$monthsexisting2[RGLong$year == 'winzMD2021'] <- RGLong$months_existing_2021

RGLong$value = as.numeric(RGLong$value)
RGLong$logvalue = log(RGLong$value+1)

h1b2rob = plm(logvalue ~ mgm_acquired:monthsexisting2 + factor(year), 
              index=c("Nummer"), 
              model = "within", data = RGLong)

summary(h1b2rob)
h1b3rob = feols(logvalue ~ mgm_acquired:monthsexisting2 | Nummer + year, data = RGLong)
modelsummary(h1b3rob)

#H3 robustness check with poisson model

h3modelrob <- glm(winzREF ~ mgm_acquired + CA2 + CH + CG1 + CG2 + twenty + thirty + fourty + sixty +seventy + seventyplus 
                   + factor(reg_month) + factor(cluster), family = poisson, data = RG)
summary(h3modelrob)
