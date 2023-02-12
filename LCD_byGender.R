
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#######
####### Life cycle deficit/surplus by age and gender in 39 countries
####### by Vargha, Lili; Binder-Hammer, Bernhard & Donehower, Gretchen & Istenic, Tanja
#######
####### Contact: Lili Vargha (lili.vargha@hu-berlin.de or vargha@demografia.hu)
#######

####### Original data source:
####### 1. European AGENTA Project (Istenic et al 2019): http://dataexplorer.wittgensteincentre.org/nta/ for 25 EU countries 2010
####### 2. Counting Women's Work (2022): https://www.countingwomenswork.org/data for countries US, ZA, SN, GH, UY, CO, VN, IN, MX, MU, TG, NE, ML, CI


####### Data downloaded from the AGENTA and CWW website: October 2022

####### Last update: 9 February 2023

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



####### Loading packages

library(tidyverse)
library(readxl)
library(ggplot2)
library(plyr)
library(ggplot2)
library(cowplot)
library(data.table)
library(reshape)
library(patchwork)
library(devtools)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#####
##### reading the European AGENTA data (Istenic et al. 2019): values are already normalized
#####

setwd ("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/NTA/VIZ")

##### long database downloaded from http://dataexplorer.wittgensteincentre.org/nta/

agenta <- read.csv(file = 'nta2010_long_Norm.csv')


str(agenta)

agenta$sex <- as.factor(agenta$sex)

agenta2 <- agenta[agenta$sex=="total",]

age<- c("0", "10", "20","30", "40", "50", "60", "70", "80")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### calculate LCD

agenta$LCD <- agenta$CG + agenta$CF - agenta$YL

#### calculate net transfers

agenta$T <- agenta$TG + agenta$TF

#### calculate asset based reallocation

agenta$AR <- agenta$YA - agenta$S

#### calculate consumption

agenta$C <- agenta$CF + agenta$CG

#### plot LCD and net transfers, asset income (YA), AR

agenta2 <- agenta[agenta$sex=="total",]

#### basic line plots for all countries

ggplot(data=agenta2, aes(x=age, y=LCD, group=country))+
  geom_line()

ggplot(data=agenta2, aes(x=age, y=YA, group=country))+
  geom_line()

ggplot(data=agenta2, aes(x=age, y=T, group=country))+
  geom_line()

ggplot(data=agenta2, aes(x=age, y=AR, group=country))+
  geom_line()

ggplot(data=agenta2, aes(x=age, y=TF, group=country))+
  geom_line()

ggplot(data=agenta2, aes(x=age, y=TG, group=country))+
  geom_line()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###### Reshaping in order to merge with Counting Women's Work Data
######
###### Even though the long format is good for GGPlot, the wide dataset is used for
###### finalizing the order of countries, for calculating differences from AVG age profile and later for clustering

###### We will thus have a merged AGENTA + CWW in both wide and long format

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#### selecting only LCD

agenta3 <- select (agenta, c(country, sex, age, LCD))

agentamen <- agenta3 %>% filter( sex == "male")
agentawomen <- agenta3 %>% filter( sex == "female")


#### creating wide database of age profiles for men (LCD1)

agentamen <- select (agentamen,-c(sex))

agentamen <- reshape(agentamen, idvar = "country", timevar = "age", direction = "wide")

agentamen$country <- revalue(agentamen$country,     c("Austria" = "LCD1_AT", "Belgium"="LCD1_BE", "Bulgaria"="LCD1_BG", "Cyprus"="LCD1_CY", "Czech Republic"="LCD1_CZ",
                                                      "Estonia"="LCD1_EE",   "Finland"="LCD1_FI", "France"="LCD1_FR", "Germany"="LCD1_DE", "Greece"="LCD1_GR",
                                                      "Hungary"="LCD1_HU", "Ireland"="LCD1_IE", "Italy"="LCD1_IT", "Latvia"="LCD1_LV", "Lithuania"="LCD1_LT",
                                                      "Luxembourg"="LCD1_LU", "Poland"="LCD1_PL", "Portugal"="LCD1_PT", "Romania"="LCD1_RO", "Slovakia"="LCD1_SK",
                                                      "Slovenia"="LCD1_SI", "Sweden"="LCD1_SE", "United Kingdom"="LCD1_UK", "Denmark"="LCD1_DK", "Spain"="LCD1_ES",
                                                      "EU25 Country Avg."="avg1","EU25 Population Avg."="avg2" ))


#### transpose age profiles for men and make sure they stay numeric values

rownames(agentamen) <- agentamen[,1 ]  
agentamen <- select (agentamen,-c(country))

agentamen <- as.data.frame(t(agentamen), stringsAsFactors = FALSE)
#agentamen <- select (agentamen,-c(avg1,avg2))

#### creating wide database of age profiles for women (LCD2)

agentawomen <- select(agentawomen,-c(sex))

agentawomen <- reshape(agentawomen, idvar = "country", timevar = "age", direction = "wide")

agentawomen$country <- revalue(agentawomen$country, c("Austria" = "LCD2_AT", "Belgium"="LCD2_BE", "Bulgaria"="LCD2_BG", "Cyprus"="LCD2_CY", "Czech Republic"="LCD2_CZ",
                                                      "Estonia"="LCD2_EE",   "Finland"="LCD2_FI", "France"="LCD2_FR", "Germany"="LCD2_DE", "Greece"="LCD2_GR",
                                                      "Hungary"="LCD2_HU", "Ireland"="LCD2_IE", "Italy"="LCD2_IT", "Latvia"="LCD2_LV", "Lithuania"="LCD2_LT",
                                                      "Luxembourg"="LCD2_LU", "Poland"="LCD2_PL", "Portugal"="LCD2_PT", "Romania"="LCD2_RO", "Slovakia"="LCD2_SK",
                                                      "Slovenia"="LCD2_SI", "Sweden"="LCD2_SE", "United Kingdom"="LCD2_UK", "Denmark"="LCD2_DK", "Spain"="LCD2_ES",
                                                      "EU25 Country Avg."="avg1","EU25 Population Avg."="avg2"))

#### transpose age profiles for women (LCD2) and make sure they stay numeric values

rownames(agentawomen) <- agentawomen[,1 ]  
agentawomen <- select (agentawomen,-c(country))
agentawomen <- as.data.frame(t(agentawomen), stringsAsFactors = FALSE)
#agentawomen <- select (agentawomen,-c(avg1,avg2))

#### merge data for men and women age 0-80

agentafinal <- cbind(agentamen, agentawomen)

#### use data point 80+ for ages 81-90+

agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])
agentafinal <- rbind(agentafinal, agentafinal[81,])

#### renaming row names
vec <- 0:90
row.names(agentafinal) <- vec

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#####
##### reading the Counting Women's Work Data (Counting Women's Work 2022)
#####

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


setwd("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/2020")

cww <- as.data.frame(read_excel("cww_database_PUBLIC_RELEASE_V3.0.xlsx", sheet = "data", range = "B8:JY99"))

str(cww)

#variables my02m: Market labor income, female, LCU per year
#variables my01m: Market labor income, male, LCU per year
#variables mc00m: Market consumption, both sexes, LCU per year
#variables my02t: Time spent with market work, female
#variables my01t: Time spent with market work, male


# for NTA normalization labour income for both sexes combined is needed

# for labour income for both sexes population by age for men and women is needed
# we use UN population Projections for this and its R package: https://github.com/PPgp/wpp2022

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###
### Population by age and gender: United Nations, DESA, Population Division. World Population Prospects 2022.
###

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#options(timeout = 600)
#install_github("PPgp/wpp2022")
library(wpp2022)

data(pop1dt)
pop1dt
data(popF1)
data(popAge1dt)
str(popAge1dt)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###Calculating country specific results

###1. For normalization purposes: calculating Labour income age 30-49 (simple average) using CWW and UN data
###2. Calculating normalized labour income for women
###3. Calculating normalized labour income for men
###4. Calculating normalized consumption for men and women
###5. Calculating normalized life cycle deficit/consumption
###6. Plot LCD/LCS & YL age profile by gender for individual countries for check-up

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


###
### US 2009
###

pop_US <- popAge1dt[name=="United States of America" & year==2009]
pop_US2009 <- pop_US[1:91,]

YLavgUS <- (cww$my02mUS * pop_US2009$popF + cww$my01mUS * pop_US2009$popM)/pop_US2009$pop
YLavgUS3049 <- mean(YLavgUS[31:50])
cww$YL1_US <- cww$my01mUS / YLavgUS3049
cww$YL2_US <- cww$my02mUS / YLavgUS3049
cww$C1_US <- cww$mc00mUS /  YLavgUS3049
cww$C2_US <- cww$mc00mUS /  YLavgUS3049
cww$LCD1_US <- cww$C1_US - cww$YL1_US
cww$LCD2_US <- cww$C2_US - cww$YL2_US

plot(cww$age, cww$LCD1_US, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_US, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the US")

plot(cww$age,cww$YL1_US, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_US, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the US")

###
### ZA 2010
###

pop_ZA <- popAge1dt[name=="South Africa" & year==2010]
pop_ZA2010 <- pop_ZA[1:91,]

YLavgZA <- (cww$my02mZA * pop_ZA2010$popF + cww$my01mZA * pop_ZA2010$popM)/pop_ZA2010$pop
YLavgZA3049 <- mean(YLavgZA[31:50])
cww$YL1_ZA <- cww$my01mZA / YLavgZA3049
cww$YL2_ZA <- cww$my02mZA / YLavgZA3049
cww$C1_ZA <- cww$mc00mZA /  YLavgZA3049
cww$C2_ZA <- cww$mc00mZA /  YLavgZA3049
cww$LCD1_ZA <- cww$C1_ZA - cww$YL1_ZA
cww$LCD2_ZA <- cww$C2_ZA - cww$YL2_ZA

plot(cww$age, cww$LCD1_ZA, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_ZA, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the ZA")

plot(cww$age, cww$YL1_ZA, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_ZA, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the ZA")

###
### SN 2011
###

pop_SN <- popAge1dt[name=="Senegal" & year==2011]
pop_SN2011 <- pop_SN[1:91,]

YLavgSN <- (cww$my02mSN * pop_SN2011$popF + cww$my01mSN * pop_SN2011$popM)/pop_SN2011$pop
YLavgSN3049 <- mean(YLavgSN[31:50])
cww$YL1_SN <- cww$my01mSN / YLavgSN3049
cww$YL2_SN <- cww$my02mSN / YLavgSN3049
cww$C1_SN <- cww$mc00mSN /  YLavgSN3049
cww$C2_SN <- cww$mc00mSN /  YLavgSN3049
cww$LCD1_SN <- cww$C1_SN - cww$YL1_SN
cww$LCD2_SN <- cww$C2_SN - cww$YL2_SN

plot(cww$age, cww$LCD1_SN, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_SN, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the SN")

plot(cww$age,cww$YL1_SN, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_SN, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the SN")

###
### SN 2018
###

pop_SN2 <- popAge1dt[name=="Senegal" & year==2018]
pop_SN22018 <- pop_SN2[1:91,]

YLavgSN2 <- (cww$my02mSN2 * pop_SN22018$popF + cww$my01mSN2 * pop_SN22018$popM)/pop_SN22018$pop
YLavgSN23049 <- mean(YLavgSN2[31:50])
cww$YL1_SN2 <- cww$my01mSN2 / YLavgSN23049
cww$YL2_SN2 <- cww$my02mSN2 / YLavgSN23049
cww$C1_SN2 <- cww$mc00mSN2 /  YLavgSN23049
cww$C2_SN2 <- cww$mc00mSN2 /  YLavgSN23049
cww$LCD1_SN2 <- cww$C1_SN2 - cww$YL1_SN2
cww$LCD2_SN2 <- cww$C2_SN2 - cww$YL2_SN2

plot(cww$age, cww$LCD1_SN2, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_SN2, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the SN2")

plot(cww$age,cww$YL1_SN2, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_SN2, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the SN2")

###
### GH 2009
###

pop_GH <- popAge1dt[name=="Ghana" & year==2009]
pop_GH2009 <- pop_GH[1:91,]

YLavgGH <- (cww$my02mGH * pop_GH2009$popF + cww$my01mGH * pop_GH2009$popM)/pop_GH2009$pop
YLavgGH3049 <- mean(YLavgGH[31:50])
cww$YL1_GH <- cww$my01mGH / YLavgGH3049
cww$YL2_GH <- cww$my02mGH / YLavgGH3049
cww$C1_GH <- cww$mc00mGH /  YLavgGH3049
cww$C2_GH <- cww$mc00mGH /  YLavgGH3049
cww$LCD1_GH <- cww$C1_GH - cww$YL1_GH
cww$LCD2_GH <- cww$C2_GH - cww$YL2_GH

plot(cww$age, cww$LCD1_GH, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_GH, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the GH")

plot(cww$age,cww$YL1_GH, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_GH, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the GH")

###
### UY 2013
###

pop_UY <- popAge1dt[name=="Uruguay" & year==2013]
pop_UY2013 <- pop_UY[1:91,]

YLavgUY <- (cww$my02mUY * pop_UY2013$popF + cww$my01mUY * pop_UY2013$popM)/pop_UY2013$pop
YLavgUY3049 <- mean(YLavgUY[31:50])
cww$YL1_UY <- cww$my01mUY / YLavgUY3049
cww$YL2_UY <- cww$my02mUY / YLavgUY3049
cww$C1_UY <- cww$mc00mUY /  YLavgUY3049
cww$C2_UY <- cww$mc00mUY /  YLavgUY3049
cww$LCD1_UY <- cww$C1_UY - cww$YL1_UY
cww$LCD2_UY <- cww$C2_UY - cww$YL2_UY

plot(cww$age, cww$LCD1_UY, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_UY, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the UY")

plot(cww$age,cww$YL1_UY, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_UY, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the UY")


###
### CO 2009
###

pop_CO <- popAge1dt[name=="Colombia" & year==2012]
pop_CO2012 <- pop_CO[1:91,]

YLavgCO <- (cww$my02mCO * pop_CO2012$popF + cww$my01mCO * pop_CO2012$popM)/pop_CO2012$pop
YLavgCO3049 <- mean(YLavgCO[31:50])
cww$YL1_CO <- cww$my01mCO / YLavgCO3049
cww$YL2_CO <- cww$my02mCO / YLavgCO3049
cww$C1_CO <- cww$mc00mCO /  YLavgCO3049
cww$C2_CO <- cww$mc00mCO /  YLavgCO3049
cww$LCD1_CO <- cww$C1_CO - cww$YL1_CO
cww$LCD2_CO <- cww$C2_CO - cww$YL2_CO

plot(cww$age, cww$LCD1_CO, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_CO, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the CO")

plot(cww$age,cww$YL1_CO, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_CO, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the CO")

###
### VN 2009
###

pop_VN <- popAge1dt[name=="Viet Nam" & year==2015]
pop_VN2015 <- pop_VN[1:91,]

YLavgVN <- (cww$my02mVN * pop_VN2015$popF + cww$my01mVN * pop_VN2015$popM)/pop_VN2015$pop
YLavgVN3049 <- mean(YLavgVN[31:50])
cww$YL1_VN <- cww$my01mVN / YLavgVN3049
cww$YL2_VN <- cww$my02mVN / YLavgVN3049
cww$C1_VN <- cww$mc00mVN /  YLavgVN3049
cww$C2_VN <- cww$mc00mVN /  YLavgVN3049
cww$LCD1_VN <- cww$C1_VN - cww$YL1_VN
cww$LCD2_VN <- cww$C2_VN - cww$YL2_VN

cww$LCD1_VN

plot(cww$age, cww$LCD1_VN, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value", )
lines(cww$age, cww$LCD2_VN, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the VN")

plot(cww$age,cww$YL1_VN, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_VN, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the VN")


###
### IN 2009
###

pop_IN <- popAge1dt[name=="India" & year==1999]
pop_IN1999 <- pop_IN[1:91,]

YLavgIN <- (cww$my02mIN * pop_IN1999$popF + cww$my01mIN * pop_IN1999$popM)/pop_IN1999$pop
YLavgIN3049 <- mean(YLavgIN[31:50])
cww$YL1_IN <- cww$my01mIN / YLavgIN3049
cww$YL2_IN <- cww$my02mIN / YLavgIN3049
cww$C1_IN <- cww$mc00mIN /  YLavgIN3049
cww$C2_IN <- cww$mc00mIN /  YLavgIN3049
cww$LCD1_IN <- cww$C1_IN - cww$YL1_IN
cww$LCD2_IN <- cww$C2_IN - cww$YL2_IN

plot(cww$age, cww$LCD1_IN, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_IN, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the IN")

plot(cww$age,cww$YL1_IN, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_IN, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the IN")

###
### MX 2014
###

pop_MX <- popAge1dt[name=="Mexico" & year==2014]
pop_MX2014 <- pop_MX[1:91,]

YLavgMX <- (cww$my02mMX * pop_MX2014$popF + cww$my01mMX * pop_MX2014$popM)/pop_MX2014$pop
YLavgMX3049 <- mean(YLavgMX[31:50])
cww$YL1_MX <- cww$my01mMX / YLavgMX3049
cww$YL2_MX <- cww$my02mMX / YLavgMX3049
cww$C1_MX <- cww$mc00mMX /  YLavgMX3049
cww$C2_MX <- cww$mc00mMX /  YLavgMX3049
cww$LCD1_MX <- cww$C1_MX - cww$YL1_MX
cww$LCD2_MX <- cww$C2_MX - cww$YL2_MX

plot(cww$age, cww$LCD1_MX, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_MX, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the MX")

plot(cww$age,cww$YL1_MX, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_MX, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the MX")

###
### MU 2003
###

pop_MU <- popAge1dt[name=="Mauritius" & year==2003]
pop_MU2003 <- pop_MU[1:91,]

YLavgMU <- (cww$my02mMU * pop_MU2003$popF + cww$my01mMU * pop_MU2003$popM)/pop_MU2003$pop
YLavgMU3049 <- mean(YLavgMU[31:50])
cww$YL1_MU <- cww$my01mMU / YLavgMU3049
cww$YL2_MU <- cww$my02mMU / YLavgMU3049
cww$C1_MU <- cww$mc00mMU /  YLavgMU3049
cww$C2_MU <- cww$mc00mMU /  YLavgMU3049
cww$LCD1_MU <- cww$C1_MU - cww$YL1_MU
cww$LCD2_MU <- cww$C2_MU - cww$YL2_MU

plot(cww$age, cww$LCD1_MU, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_MU, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the MU")

plot(cww$age,cww$YL1_MU, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_MU, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the MU")

###
### TG 2018
###

pop_TG <- popAge1dt[name=="Togo" & year==2018]
pop_TG2018 <- pop_TG[1:91,]

YLavgTG <- (cww$my02mTG * pop_TG2018$popF + cww$my01mTG * pop_TG2018$popM)/pop_TG2018$pop
YLavgTG3049 <- mean(YLavgTG[31:50])
cww$YL1_TG <- cww$my01mTG / YLavgTG3049
cww$YL2_TG <- cww$my02mTG / YLavgTG3049
cww$C1_TG <- cww$mc00mTG /  YLavgTG3049
cww$C2_TG <- cww$mc00mTG /  YLavgTG3049
cww$LCD1_TG <- cww$C1_TG - cww$YL1_TG
cww$LCD2_TG <- cww$C2_TG - cww$YL2_TG


plot(cww$age, cww$LCD1_TG, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_TG, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the TG")

plot(cww$age,cww$YL1_TG, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_TG, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the TG")


###
### NE 2018
###

pop_NE <- popAge1dt[name=="Niger" & year==2018]
pop_NE2018 <- pop_NE[1:91,]

YLavgNE <- (cww$my02mNE * pop_NE2018$popF + cww$my01mNE * pop_NE2018$popM)/pop_NE2018$pop
YLavgNE3049 <- mean(YLavgNE[31:50])
cww$YL1_NE <- cww$my01mNE / YLavgNE3049
cww$YL2_NE <- cww$my02mNE / YLavgNE3049
cww$C1_NE <- cww$mc00mNE /  YLavgNE3049
cww$C2_NE <- cww$mc00mNE /  YLavgNE3049
cww$LCD1_NE <- cww$C1_NE - cww$YL1_NE
cww$LCD2_NE <- cww$C2_NE - cww$YL2_NE

plot(cww$age, cww$LCD1_NE, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_NE, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the NE")

plot(cww$age,cww$YL1_NE, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_NE, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the NE")

###
### ML 2018
###

pop_ML <- popAge1dt[name=="Mali" & year==2018]
pop_ML2018 <- pop_ML[1:91,]

YLavgML <- (cww$my02mML * pop_ML2018$popF + cww$my01mML * pop_ML2018$popM)/pop_ML2018$pop
YLavgML3049 <- mean(YLavgML[31:50])
cww$YL1_ML <- cww$my01mML / YLavgML3049
cww$YL2_ML <- cww$my02mML / YLavgML3049
cww$C1_ML <- cww$mc00mML /  YLavgML3049
cww$C2_ML <- cww$mc00mML /  YLavgML3049
cww$LCD1_ML <- cww$C1_ML - cww$YL1_ML
cww$LCD2_ML <- cww$C2_ML - cww$YL2_ML

plot(cww$age, cww$LCD1_ML, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_ML, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the ML")

plot(cww$age,cww$YL1_ML, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_ML, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the ML")

###
### CI 2018
###

pop_CI <- popAge1dt[name=="Cote d'Ivoire" & year==2018]
pop_CI2018 <- pop_CI[1:91,]

YLavgCI <- (cww$my02mCI * pop_CI2018$popF + cww$my01mCI * pop_CI2018$popM)/pop_CI2018$pop
YLavgCI3049 <- mean(YLavgCI[31:50])
cww$YL1_CI <- cww$my01mCI / YLavgCI3049
cww$YL2_CI <- cww$my02mCI / YLavgCI3049
cww$C1_CI <- cww$mc00mCI /  YLavgCI3049
cww$C2_CI <- cww$mc00mCI /  YLavgCI3049
cww$LCD1_CI <- cww$C1_CI - cww$YL1_CI
cww$LCD2_CI <- cww$C2_CI - cww$YL2_CI

plot(cww$age, cww$LCD1_CI, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$LCD2_CI, col="red", lwd=3)
abline(h = 0, col = "black")
title("LCD/LCS in the CI")

plot(cww$age,cww$YL1_CI, type="l", col="blue", lwd=3, xlab="age", ylab="normalized value")
lines(cww$age, cww$YL2_CI, col="red", lwd=3)
abline(h = 0, col = "black")
title("YL in the CI")


####### merge with agentafinal dataframe

nta <- cbind(agentafinal, cww)
str(nta)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

####### creating seperate database for time transfers by women and men

LCD1var <- c("LCD1_US", "LCD1_ZA", "LCD1_SN", "LCD1_GH", "LCD1_UY", "LCD1_CO", "LCD1_VN",
             "LCD1_IN", "LCD1_MX", "LCD1_MU", "LCD1_TG", "LCD1_NE", "LCD1_ML", "LCD1_CI", 
             "LCD1_HU", "LCD1_AT", "LCD1_BE", "LCD1_BG", "LCD1_CY", "LCD1_CZ", "LCD1_GR",
             "LCD1_DE", "LCD1_DK", "LCD1_EE", "LCD1_ES", "LCD1_FI", "LCD1_FR",  "LCD1_IE",
             "LCD1_IT", "LCD1_LU", "LCD1_PT", "LCD1_RO", "LCD1_SK",  "LCD1_LT", "LCD1_LV",
             "LCD1_PL", "LCD1_SE", "LCD1_SI", "LCD1_UK", "age")

LCD2var <- c("LCD2_US", "LCD2_ZA", "LCD2_SN", "LCD2_GH", "LCD2_UY", "LCD2_CO", "LCD2_VN",
             "LCD2_IN", "LCD2_MX", "LCD2_MU", "LCD2_TG", "LCD2_NE", "LCD2_ML", "LCD2_CI", 
             "LCD2_HU", "LCD2_AT", "LCD2_BE", "LCD2_BG", "LCD2_CY", "LCD2_CZ", "LCD2_GR",
             "LCD2_DE", "LCD2_DK", "LCD2_EE", "LCD2_ES", "LCD2_FI", "LCD2_FR",  "LCD2_IE",
             "LCD2_IT", "LCD2_LU", "LCD2_PT", "LCD2_RO", "LCD2_SK",  "LCD2_LT", "LCD2_LV",
             "LCD2_PL", "LCD2_SE", "LCD2_SI", "LCD2_UK", "age")


ntaLCD1 <- nta[LCD1var]

ntaLCD2 <- nta[LCD2var]

colnames(ntaLCD1)

names(ntaLCD1)

####### using the name of the country and year for the visualization


ntaLCD1c <- rename(ntaLCD1, c("LCD1_US"="United States 2009", "LCD1_ZA"="South Africa 2010", "LCD1_SN"="Senegal 2011", "LCD1_GH"="Ghana 2009",
                              "LCD1_UY"="Uruguay 2013",       "LCD1_CO"="Colombia 2012",     "LCD1_VN"="Vietnam 2015", "LCD1_IN"="India 1999",
                              "LCD1_MX"="Mexico 2014",        "LCD1_MU"="Mauritius 2003",    "LCD1_TG"="Togo 2018",    "LCD1_NE"="Niger 2018",
                              "LCD1_ML"="Mali 2018",          "LCD1_CI"="Cote d'Ivoire 2018","LCD1_HU"="Hungary 2010", "LCD1_AT"="Austria 2010",
                              "LCD1_BE"="Belgium 2010",       "LCD1_BG"="Bulgaria 2010",     "LCD1_CY"="Cyprus 2010",  "LCD1_CZ"="Czech Republic 2010",
                              "LCD1_DE"="Germany 2010",       "LCD1_GR"="Greece 2010",       "LCD1_IE"="Ireland 2010", "LCD1_LU"="Luxembourg 2010",
                              "LCD1_DK"="Denmark 2010",       "LCD1_EE"="Estonia 2010",      "LCD1_ES"="Spain 2010",   "LCD1_PT"="Portugal 2010",
                              "LCD1_FI"="Finland 2010",       "LCD1_FR"="France 2010",       "LCD1_IT"="Italy 2010",   "LCD1_RO"="Romania 2010",
                              "LCD1_LT"="Lithuania 2010",     "LCD1_LV"="Latvia 2010",       "LCD1_PL"="Poland 2010",  "LCD1_SK"="Slovakia 2010",
                              "LCD1_SE"="Sweden 2010",        "LCD1_SI"="Slovenia 2010",     "LCD1_UK"="United Kingdom 2010" ))

ntaLCD2c <- rename(ntaLCD2, c("LCD2_US"="United States 2009", "LCD2_ZA"="South Africa 2010", "LCD2_SN"="Senegal 2011", "LCD2_GH"="Ghana 2009",
                              "LCD2_UY"="Uruguay 2013",       "LCD2_CO"="Colombia 2012",     "LCD2_VN"="Vietnam 2015", "LCD2_IN"="India 1999",
                              "LCD2_MX"="Mexico 2014",        "LCD2_MU"="Mauritius 2003",    "LCD2_TG"="Togo 2018",    "LCD2_NE"="Niger 2018",
                              "LCD2_ML"="Mali 2018",          "LCD2_CI"="Cote d'Ivoire 2018","LCD2_HU"="Hungary 2010", "LCD2_AT"="Austria 2010",
                              "LCD2_BE"="Belgium 2010",       "LCD2_BG"="Bulgaria 2010",     "LCD2_CY"="Cyprus 2010",  "LCD2_CZ"="Czech Republic 2010",
                              "LCD2_DE"="Germany 2010",       "LCD2_GR"="Greece 2010",       "LCD2_IE"="Ireland 2010",      "LCD2_LU"="Luxembourg 2010",
                              "LCD2_DK"="Denmark 2010",       "LCD2_EE"="Estonia 2010",      "LCD2_ES"="Spain 2010",   "LCD2_PT"="Portugal 2010",
                              "LCD2_FI"="Finland 2010",       "LCD2_FR"="France 2010",       "LCD2_IT"="Italy 2010",   "LCD2_RO"="Romania 2010",
                              "LCD2_LT"="Lithuania 2010",     "LCD2_LV"="Latvia 2010",       "LCD2_PL"="Poland 2010",  "LCD2_SK"="Slovakia 2010",
                              "LCD2_SE"="Sweden 2010",        "LCD2_SI"="Slovenia 2010",     "LCD2_UK"="United Kingdom 2010" ))


rownames(ntaLCD1c)
rownames(ntaLCD1c) <- ntaLCD1c$age
rownames(ntaLCD2c) <- ntaLCD2c$age


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############
############ Preparing LCD of women for the final GGPlot
############


ntaLCD2h <- t(as.matrix(ntaLCD2c))

colnames(ntaLCD2h)

LCD2heatmap <- heatmap(ntaLCD2h, Rowv=NA, Colv=NA)

####### deleting row age 
ntaLCD2h <- ntaLCD2h[-c(40),]

####### maximum and minimum values in the matrix
maxLCD2 <- max(ntaLCD2h)
minLCD2 <- min(ntaLCD2h)

maxLCD2
minLCD2

########
########
######## In what order the countries appear on the figure: sorting the countries
########


#1. Sort the data by the maximum place
#max <- as.numeric(max.col(ntaLCD2h))
#ntaLCD2hs <- cbind(ntaLCD2h, max)
#ntaLCD2hso <- ntaLCD2hs[order(-ntaLCD2hs[,87]),]


#2. sort the data according to when values turn negative
neg2 <- as.numeric(max.col(ntaLCD2h < 0,ties.method = "first"))
neg2

#replace value of no negative values
#neg <- replace(neg, neg==1, 80)
#neg

#3. sort the data according to how many ages have negative values
#neg <- as.numeric(rowMeans(ntaLCD2h < 0))
#neg

ntaLCD2hs <- cbind(ntaLCD2h, neg2)

ntaLCD2hso <- ntaLCD2hs[order(-ntaLCD2hs[,92]),]

#4. Other sorting is also possible

######## Making a list of countries in order

ntaLCD2hsor <- as.data.frame(ntaLCD2hso)
Tborder <- setDT(ntaLCD2hsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder

######## deleting the sorting coloumn
ntaLCD2hso <- ntaLCD2hso[,-c(92)]

LCD2heatmap <- heatmap(ntaLCD2hso, Rowv=NA, Colv=NA)

########
######## GGPLOT is used for the final visualization, so the data has to be restructured
########

LCD2df <- as.data.frame(ntaLCD2hso)


LCD2d <- setDT(LCD2df, keep.rownames = TRUE)[]
names(LCD2d)[1] <- "country"

######## Reorder Data for ggplot2 plot
LCD2melt <- melt(LCD2d)

LCD2melt
str(LCD2melt)

######## Save the restructured data (women) for the plot

save(LCD2melt, file="LCD2melt.Rdata")

setwd("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/2020")
load("LCD2melt.Rdata")

#reorder the country names 

LCD2melt$country <- factor(x = LCD2melt$country,
                           levels = corder, 
                           ordered = TRUE)

LCD2tiles <- ggplot(LCD2melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_discrete(name="Age", expand=c(0,0))+
  labs(title="LCD/LCS by age (women)",
       caption="")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
LCD2tiles


############
############ Preparing LCD1 of men for the final GGPlot
############


ntaLCD1h <- t(as.matrix(ntaLCD1c))

####### deleting row age 
ntaLCD1h <- ntaLCD1h[-c(40),]

###### general heatmap


colnames(ntaLCD1h)

LCD1heatmap <- heatmap(ntaLCD1h, Rowv=NA, Colv=NA)

####### maximum and minimum values in the matrix
maxLCD1 <- max(ntaLCD1h)
minLCD1 <- min(ntaLCD1h)

maxLCD1
minLCD1

########
########
######## In what order the countries appear on the figure: sorting the countries
########

#1. Sort the data by the maximum place
#max <- as.numeric(max.col(ntaLCD1h))
#ntaLCD1hs <- cbind(ntaLCD1h, max)
#ntaLCD1hso <- ntaLCD1hs[order(-ntaLCD1hs[,87]),]

#2. sort the data according to when values turn negative (this method is used in the paper)
neg1 <- as.numeric(max.col(ntaLCD1h < 0,ties.method = "first"))
neg1


#3. sort the data according to how many ages have negative values
#neg <- as.numeric(rowMeans(ntaLCD1h < 0))
#neg


#ntaLCD1hs <- cbind(ntaLCD1h, neg1)
ntaLCD1hs <- cbind(ntaLCD1h, neg2)

ntaLCD1hso <- ntaLCD1hs[order(-ntaLCD1hs[,92]),]

#4. other sorting is also possible, like alphabetical, etc.

####### making a list of countries in order

ntaLCD1hsor <- as.data.frame(ntaLCD1hso)
Tborder <- setDT(ntaLCD1hsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder


####### deleting the sorting coloumn
ntaLCD1hso <- ntaLCD1hso[,-c(92)]

LCD1heatmap <- heatmap(ntaLCD1hso, Rowv=NA, Colv=NA)

########
######## GGPLOT is used for the final visualization, so the data has to be restructured
########

LCD1df <- as.data.frame(ntaLCD1hso)

LCD1d <- setDT(LCD1df, keep.rownames = TRUE)[]
names(LCD1d)[1] <- "country"

######## Reorder Data for ggplot2 plot
LCD1melt <- melt(LCD1d)

LCD1melt
str(LCD1melt)

######## Save the restructured data (men) for the plot

save(LCD1melt, file="LCD1melt.Rdata")

setwd("C:/Users/varghali/seadrive_root/Lili Var/My Libraries/My Library/2020")
load("LCD1melt.Rdata")

######## Looking at basic heatmap in ggplot

#reorder the country names

LCD1melt$country <- factor(x = LCD1melt$country,
                          levels = corder, 
                          ordered = TRUE)

LCD1tiles <- ggplot(LCD1melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_discrete(name="Age", expand=c(0,0))+
  labs(title="LCD/LCS by age (men)",
       caption="")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))                                                          
LCD1tiles



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############
############ Final figure for men and women using shades of blue and red
############
############

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

########## Defining the colours of blue and red

bl <- colorRampPalette(c("darkblue", "royalblue","lightskyblue1"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)

########## Age appearing on the plot

age<- c("0", "10", "20","30", "40", "50", "60", "70", "80", "90+")

############ Final figure for men 

LCD1tiles <- ggplot(LCD1melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", linewidth=0.45)+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.4, 1.4)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 91, by=10), labels=(age))+
  labs(title="Men") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Normalized Value") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCD1tiles 

############ Final figure for women 

LCD2tiles <- ggplot(LCD2melt, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White", linewidth=0.45)+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.4, 1.4)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 91, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Normalized value") +
  labs(title="Women") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCD2tiles 

############ Patchwork: putting the two plots together


fig <- LCD2tiles + LCD1tiles

figLCD <- fig +
  plot_layout(guides = 'collect')

figLCD <- figLCD + plot_annotation(
  title = "Life cycle deficit/surplus by gender in 39 countries",
  theme = theme(plot.title = element_text(size = 18)),
  
  caption = "Data: Istenic et al. 2019, Counting Womens Work 2022  
  Replication files & details: https://github.com/LiliVargha/Life-Cycle-Deficit_LCD")

figLCD

tiff("Outputs/LCDbygenderViz2.tiff", units="in", width=20, height=6, res=500)
plot(figLCD, align="h", rel_widths=c(1,0.2))
dev.off()

jpeg("Outputs/GitHub/LCDbygenderViz2.jpg", units="in", width=20, height=6, res=500)
plot(figLCD, align="h", rel_widths=c(1,0.2))
dev.off()

jpeg("Outputs/LCDbygenderViz3.jpg", units="in", width=20, height=6, res=500)
plot(figLCD, align="h", rel_widths=c(1,0.2))
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

############
############ Visualizing differences from the country avg age profiles
############ 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NTALCD1h <- ntaLCD1h

colMeans(NTALCD1h)
AVG <- colMeans(NTALCD1h)
NTALCD1dif <- sweep(NTALCD1h,2,AVG)

LCD1difdf <- as.data.frame(NTALCD1dif)

LCD1difdf2 <- setDT(LCD1difdf, keep.rownames = TRUE)[]
names(LCD1difdf2)[1] <- "country"

##maximum and minimum values in the matrix
maxLCD1dif <- max(NTALCD1dif)
minLCD1dif <- min(NTALCD1dif)

maxLCD1dif
minLCD1dif

NTALCD1difs <- cbind(NTALCD1dif, neg1)

NTALCD1difso <- NTALCD1difs[order(-NTALCD1difs[,92]),]

#making a list of countries in order

NTALCD1difsor <- as.data.frame(NTALCD1difso)
Tborder <- setDT(NTALCD1difsor, keep.rownames = TRUE)[]
names(Tborder)[1] <- "country"

corder <- c(Tborder$country)
corder

#deleting the sorting coloumn
NTALCD1difso <- NTALCD1difso[,-c(92)]

LCD1difeatmap <- heatmap(NTALCD1difso, Rowv=NA, Colv=NA)

####GGPLOT

#restructuring the data
LCD1df <- as.data.frame(NTALCD1difso)

LCD1d <- setDT(LCD1df, keep.rownames = TRUE)[]
names(LCD1d)[1] <- "country"


#Reshape Data for ggplot2 plot
LCD1difmelt_o <- melt(LCD1d)

#reorder the country names 

LCD1difmelt_o$country <- factor(x = LCD1difmelt_o$country,
                               levels = corder, 
                               ordered = TRUE)

LCD1diftiles_o <- ggplot(LCD1difmelt_o, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 91, by=10), labels=(age))+
  labs(title="Difference to the 39 country average LCD/LCS",
       caption="Data: Istenic et al. 2019, Counting Womens Work 2022")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"))  
LCD1diftiles_o

jpeg("Outputs/dif/LCD1diftiles_o.jpg", units="in", width=14, height=16, res=500)
plot_grid(LCD1diftiles_o, align="h", rel_widths=c(1,0.2))
dev.off()  


fig2 <- LCD1tiles + LCD1diftiles_o

figLCD2 <- fig2 +
  plot_layout(guides = 'collect')

figLCD2 <- figLCD + plot_annotation(
  title = "Life cycle deficit/surplus by gender in 39 countries",
  theme = theme(plot.title = element_text(size = 18)),
  
  caption = "Data: Istenic et al. 2019, Counting Womens Work 2022")

figLCD2

tiff("Outputs/LCD1DIF.tiff", units="in", width=20, height=6, res=500)
plot(figLCD2, align="h", rel_widths=c(1,0.2))
dev.off()

jpeg("Outputs/DR/LCD1DIF.jpg", units="in", width=20, height=6, res=500)
plot(figLCD2, align="h", rel_widths=c(1,0.2))
dev.off()

bl <- colorRampPalette(c("navy","royalblue","lightskyblue"))(200)                      
re <- colorRampPalette(c("mistyrose", "red2","darkred"))(200)
age<- c("0", "10", "20","30", "40", "50", "60", "70", "80", "90+")


LCD1diftiles2_o <- ggplot(LCD1difmelt_o, aes(x=as.numeric(variable), y=country))+
  geom_tile(aes(fill=value), colour="White")+
  theme_classic()+
  scale_fill_gradientn(colours=c(bl, re), na.value = "grey98",
                       limits = c(-1.0, 1.0)) +
  scale_y_discrete(name="", expand=c(0,0))+
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(1, 91, by=10), labels=(age))+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  labs(fill = "Dif to avg") +
  theme(axis.text.y = element_text(size = 10)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=20)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

LCD1diftiles2_o 
jpeg("Outputs/dif/LCD1diftiles2_o.jpg", units="in", width=14, height=16, res=500)
plot_grid(LCD1diftiles2_o, align="h", rel_widths=c(1,0.2))
dev.off() 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#######
####### Looking for clusters of countries
#######


####### First calculating dissimilarity matrix

####### Using different cluster methods


library(cluster)
library(WeightedCluster)
library(TraMineR)

library(factoextra)
library(ggplot2)
library(heatmaply)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Merging database of men and women

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

nta1 <- as.matrix(ntaLCD1hso)
nta2 <- as.matrix(ntaLCD2hso)
ntamatrix <- cbind(nta1,nta2)
str(ntamatrix)

####### Standardise the age specific variables

ntamatrix <- scale(ntamatrix)

####### Euclidean (default) and the sum of absolute distances (manhattan)

LCDdismatrix <- as.matrix(daisy(ntamatrix))
LCDdismatrix2 <- as.matrix(daisy(ntamatrix, metric = "manhattan"))

str(LCDdismatrix)

LCDdismatrix[1:5,1:5]

####### Hierarchical clustering 

cluster1 <- hclust(as.dist(LCDdismatrix), 
                   method = "ward.D")

cluster2 <- hclust(as.dist(LCDdismatrix2), 
                   method = "ward.D")


#plot the dendrogram

plot(cluster1, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")

plot(cluster2, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")


wardtest1 <- as.clustrange(cluster1,
                           diss=LCDdismatrix,
                           ncluster=9)

wardtest2 <- as.clustrange(cluster2,
                           diss=LCDdismatrix,
                           ncluster=10)
wardtest1
wardtest2

plot(wardtest1, norm="zscore", lwd=4)


#elbow method

#fviz_nbclust(LCDdismatrix, FUN = hcut, method = "wss",
#barfill = "black",
#barcolor = "black",
#linecolor = "black")

#fviz_nbclust(LCDdismatrix, FUN = hcut, method = "silhouette",
#barfill = "black",
#barcolor = "black",
#linecolor = "black")

#fviz_nbclust(LCDdismatrix2, FUN = hcut, method = "silhouette",
#barfill = "black",
#barcolor = "black",
#linecolor = "black")

#choosing 3 clusters

cluster1.3 <- cutree(cluster1, 
                     k = 3)

cluster1.3

summary(silh.ward <- silhouette(cluster1.3, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 2 clusters

cluster1.2 <- cutree(cluster1, 
                     k = 2)

cluster1.2

summary(silh.ward <- silhouette(cluster1.2, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 4 clusters

cluster1.4 <- cutree(cluster1, 
                     k = 4)

cluster1.4

summary(silh.ward <- silhouette(cluster1.4, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 5 clusters

cluster1.5 <- cutree(cluster1, 
                     k = 5)

cluster1.5

summary(silh.ward <- silhouette(cluster1.5, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 6 clusters

cluster1.6 <- cutree(cluster1, 
                     k = 6)

cluster1.6

summary(silh.ward <- silhouette(cluster1.6, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)



#test different cluster solutions

pam <- wcKMedRange(LCDdismatrix, 
                   kvals = 2:15)

#print the quality test for different cluster solutions

pam 

#apply the PAM-clustering algorithm

pam2 <- wcKMedoids(LCDdismatrix, 
                   k = 2)

pam2 <- pam2$clustering 

table(pam2)

pam2


# checking different clustering techniques: ward method seems the best
proxmat <- dist(LCDdismatrix, method = 'euclidean')

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

ac <- function(x) {
  agnes(LCDdismatrix, method = x)$ac
}

map_dbl(m, ac)

# choosing 4 cluster solution for plotting

LCD1d$cluster4 <-  as.factor(cluster1.4)

LCDmelt <- melt(LCD1d)
LCDmelt

ggplot(data=LCDmelt, aes(x=variable, y=value, group=country))+
  geom_line()


# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                   
# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", )
                   
age<- c("0", "10", "20","30", "40", "50", "60", "70", "80", "90+")


LCDmelt$variable <- as.numeric(LCDmelt$variable)
fig1 <- ggplot(data=LCDmelt, aes(x=variable, y=value, group=country, color=cluster4)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("red", "#00AFBB", "#E69F00", "navy"), name  ="4 Clusters",
                     breaks=c("1", "2", "3", "4"),
                     labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(0, 90, by=10), labels=(age)) +
  labs(title="Men") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(name="Normalized Value", limits=c(-1.4,1.1) )

fig1 

LCD2d$cluster4 <-  as.factor(cluster1.4)
cluster1.4

LCDmelt <- melt(LCD2d)
LCDmelt

ggplot(data=LCDmelt, aes(x=variable, y=value, group=country))+
  geom_line()


age<- c("0", "10", "20","30", "40", "50", "60", "70", "80", "90+")


LCDmelt$variable <- as.numeric(LCDmelt$variable)
fig2 <- ggplot(data=LCDmelt, aes(x=variable, y=value, group=country, color=cluster4)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("red", "#00AFBB", "#E69F00", "navy"), name  ="4 Clusters",
                     breaks=c("1", "2", "3", "4"),
                     labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(0, 90, by=10), labels=(age)) +
  labs(title="Women") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(name="Normalized Value", limits=c(-1.4,1.1) )

fig2 


figc <- fig2 + fig1

figLCD <- figc +
  plot_layout(guides = 'collect')


figLCD

tiff("Outputs/LCDbygenderCluster.tiff", units="in", width=20, height=6, res=500)
plot(figLCD, align="h", rel_widths=c(1,0.2))
dev.off()

jpeg("Outputs/LCDbygenderCluster.jpg", units="in", width=20, height=6, res=500)
plot(figLCD, align="h", rel_widths=c(1,0.2))
dev.off()








#######
#######
####### Dissmilarity/distance matrix, NTALCDhso for men
#######
#######
ntamatrix <- as.matrix(ntaLCD1hso)

str(ntamatrix)

####### Standardise the age specific variables

ntamatrix <- scale(ntamatrix)

####### Euclidean (default) and the sum of absolute distances (manhattan)

LCDdismatrix <- as.matrix(daisy(ntamatrix))
LCDdismatrix2 <- as.matrix(daisy(ntamatrix, metric = "manhattan"))

str(LCDdismatrix)

LCDdismatrix[1:5,1:5]

####### Hierarchical clustering 

cluster1 <- hclust(as.dist(LCDdismatrix), 
                   method = "ward.D")

cluster2 <- hclust(as.dist(LCDdismatrix2), 
                   method = "ward.D")
?hclust

#plot the dendrogram

plot(cluster1, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")

plot(cluster2, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")


wardtest1 <- as.clustrange(cluster1,
                           diss=LCDdismatrix,
                           ncluster=9)

wardtest2 <- as.clustrange(cluster2,
                           diss=LCDdismatrix,
                           ncluster=10)
wardtest1
wardtest2

#choosing 2 clusters

cluster1.2 <- cutree(cluster1, 
                     k = 2)

cluster1.2

summary(silh.ward <- silhouette(cluster1.2, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

cluster1.2
str(cluster1.2)

LCD1df <- as.data.frame(ntaLCD1hso)

LCD1d <- setDT(LCD1df, keep.rownames = TRUE)[]
names(LCD1d)[1] <- "country"

LCD1d$cluster2 <-  as.factor(cluster1.2)

LCDmelt <- melt(LCD1d)
LCDmelt

ggplot(data=LCDmelt, aes(x=variable, y=value, group=country))+
  geom_line()


# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                   
# The palette with black:
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                   
age<- c("0", "10", "20","30", "40", "50", "60", "70", "80", "90+")


LCDmelt$variable <- as.numeric(LCDmelt$variable)
fig <- ggplot(data=LCDmelt, aes(x=variable, y=value, group=country, color=cluster2)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("red", "#00AFBB"), name  ="2 Clusters",
                     breaks=c("1", "2"),
                     labels=c("Cluster 1", "Cluster 2")) +
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(0, 90, by=10), labels=(age)) +
  labs(title="LCD/LCS Profile Clusters, Men (N=39)") +
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(name="Normalized Value", limits=c(-1.5,1.1) )

fig 

jpeg("Outputs/ClusterLCD1.jpg", units="in", width=10, height=6, res=500)
plot(fig, align="h", rel_widths=c(1,0.2))
dev.off()

####### Dissmilarity/distance matrix, NTALCDhso for women

ntamatrix <- as.matrix(ntaLCD2hso)

str(ntamatrix)

####### Standardise the age specific variables

ntamatrix <- scale(ntamatrix)

####### Euclidean (default) and the sum of absolute distances (manhattan)

LCDdismatrix <- as.matrix(daisy(ntamatrix))
LCDdismatrix2 <- as.matrix(daisy(ntamatrix, metric = "manhattan"))

str(LCDdismatrix)

LCDdismatrix[1:5,1:5]

####### Hierarchical clustering 

cluster1 <- hclust(as.dist(LCDdismatrix), 
                   method = "ward.D")

cluster2 <- hclust(as.dist(LCDdismatrix2), 
                   method = "ward.D")
?hclust

#plot the dendrogram

plot(cluster1, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")

plot(cluster2, 
     labels = FALSE, 
     ylab="Dissimilarity threshold")


wardtest1 <- as.clustrange(cluster1,
                           diss=LCDdismatrix,
                           ncluster=9)

wardtest2 <- as.clustrange(cluster2,
                           diss=LCDdismatrix,
                           ncluster=10)
wardtest1
wardtest2

#choosing 3 clusters

cluster1.3 <- cutree(cluster1, 
                     k = 3)

cluster1.3

summary(silh.ward <- silhouette(cluster1.3, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 4 clusters

cluster1.4 <- cutree(cluster1, 
                     k = 4)

cluster1.4

summary(silh.ward <- silhouette(cluster1.4, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)

#choosing 2 clusters

cluster1.2 <- cutree(cluster1, 
                     k = 2)

cluster1.2

summary(silh.ward <- silhouette(cluster1.2, dmatrix = LCDdismatrix))

plot(silh.ward, 
     main = "Silhouette WARD solution", 
     col = "blue",
     border = NA)


cluster1.3
str(cluster1.3)

LCD2df <- as.data.frame(ntaLCD2hso)

LCD2d <- setDT(LCD2df, keep.rownames = TRUE)[]
names(LCD2d)[1] <- "country"

LCD2d$cluster4 <-  as.factor(cluster1.4)

LCDmelt <- melt(LCD2d)
LCDmelt

ggplot(data=LCDmelt, aes(x=variable, y=value, group=country))+
  geom_line()


age<- c("0", "10", "20","30", "40", "50", "60", "70", "80", "90+")


LCDmelt$variable <- as.numeric(LCDmelt$variable)
fig <- ggplot(data=LCDmelt, aes(x=variable, y=value, group=country, color=cluster4)) +
  geom_line() +
  theme_minimal() +
  scale_color_manual(values = c("red", "#00AFBB", "navy", "#E69F00"), name  ="4 Clusters",
                     breaks=c("1", "2", "3", "4"),
                     labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4")) +
  scale_x_continuous(name="Age", expand=c(0,0), breaks=seq(0, 90, by=10), labels=(age)) +
  labs(title="Labour Income Age Profile Clusters, Women (N=39)") +
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot",
        axis.text.y=element_text(colour="Black"),
        axis.text.x=element_text(size=12)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.subtitle = element_text(size=12)) +
  theme(plot.title = element_text(size=16)) +
  theme(axis.title.x = element_text(size=14)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(plot.caption = element_text(size=12)) +
  theme(legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(name="Normalized Value", limits=c(-1,1.2) )

fig 

jpeg("Outputs/ClusterLCD2.jpg", units="in", width=10, height=6, res=500)
plot(fig, align="h", rel_widths=c(1,0.2))
dev.off()




