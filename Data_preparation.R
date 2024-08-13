
rm(list=ls(all=TRUE))
library("Hmisc")
library("readr")
library("dplyr")

options(scipen=999)
setwd("/Users/Ang/Desktop/Research/Counterfactual slopes/Data")

load("data.RData")
colnames(data)

table(data$AGEATINT_1979)
data <- data[data$AGEATINT_1979<=17,]   # 11286 to 5650
# We restrict the sample to age 14-17 in 1979.

# We downloaded the PCEPI data from https://fred.stlouisfed.org/series/PCEPI
pce <- read.csv("PCEPI.csv")

pce$year <- substr(pce$DATE, 1, 4)
table(pce$year)
baseline <- mean(pce$PCEPI[pce$year==2019])  # I'll transform the family income to constant dollars in 2019

data$income79 <- data$TNFI_TRUNC_1979/mean(pce$PCEPI[pce$year==1979])*baseline
data$income80 <- data$TNFI_TRUNC_1980/mean(pce$PCEPI[pce$year==1980])*baseline
data$income81 <- data$TNFI_TRUNC_1981/mean(pce$PCEPI[pce$year==1981])*baseline
data$income82 <- data$TNFI_TRUNC_1982/mean(pce$PCEPI[pce$year==1982])*baseline
data$income83 <- data$TNFI_TRUNC_1983/mean(pce$PCEPI[pce$year==1983])*baseline
data$income84 <- data$TNFI_TRUNC_1984/mean(pce$PCEPI[pce$year==1984])*baseline

data$income06 <- data$TNFI_TRUNC_2006/mean(pce$PCEPI[pce$year==2006])*baseline
data$income08 <- data$TNFI_TRUNC_2008/mean(pce$PCEPI[pce$year==2008])*baseline
data$income10 <- data$TNFI_TRUNC_2010/mean(pce$PCEPI[pce$year==2010])*baseline
data$income12 <- data$TNFI_TRUNC_2012/mean(pce$PCEPI[pce$year==2012])*baseline
data$income14 <- data$TNFI_TRUNC_2014/mean(pce$PCEPI[pce$year==2014])*baseline
data$income16 <- data$TNFI_TRUNC_2016/mean(pce$PCEPI[pce$year==2016])*baseline
data$income18 <- data$TNFI_TRUNC_2018/mean(pce$PCEPI[pce$year==2018])*baseline

# divide family income by the square root of family size (following Zhou)
data$income79_per <- data$income79/sqrt(data$FAMSIZE_1979)
data$income80_per <- data$income80/sqrt(data$FAMSIZE_1980)
data$income81_per <- data$income81/sqrt(data$FAMSIZE_1981)
data$income82_per <- data$income82/sqrt(data$FAMSIZE_1982)
data$income83_per <- data$income83/sqrt(data$FAMSIZE_1983)
data$income84_per <- data$income84/sqrt(data$FAMSIZE_1984)

data$income06_per <- data$income06/sqrt(data$FAMSIZE_2006)
data$income08_per <- data$income08/sqrt(data$FAMSIZE_2008)
data$income10_per <- data$income10/sqrt(data$FAMSIZE_2010)
data$income12_per <- data$income12/sqrt(data$FAMSIZE_2012)
data$income14_per <- data$income14/sqrt(data$FAMSIZE_2014)
data$income16_per <- data$income16/sqrt(data$FAMSIZE_2016)
data$income18_per <- data$income18/sqrt(data$FAMSIZE_2018)

# take log
data$income79_per_log <- log(data$income79_per+1000)
data$income80_per_log <- log(data$income80_per+1000)
data$income81_per_log <- log(data$income81_per+1000)
data$income82_per_log <- log(data$income82_per+1000)
data$income83_per_log <- log(data$income83_per+1000)
data$income84_per_log <- log(data$income84_per+1000)

data$income06_per_log <- log(data$income06_per+1000)
data$income08_per_log <- log(data$income08_per+1000)
data$income10_per_log <- log(data$income10_per+1000)
data$income12_per_log <- log(data$income12_per+1000)
data$income14_per_log <- log(data$income14_per+1000)
data$income16_per_log <- log(data$income16_per+1000)
data$income18_per_log <- log(data$income18_per+1000)

# get percentile rank
data$income79_per_rank <- percent_rank(data$income79_per)
data$income80_per_rank <- percent_rank(data$income80_per)
data$income81_per_rank <- percent_rank(data$income81_per)
data$income82_per_rank <- percent_rank(data$income82_per)
data$income83_per_rank <- percent_rank(data$income83_per)
data$income84_per_rank <- percent_rank(data$income84_per)

data$income06_per_rank <- percent_rank(data$income06_per)
data$income08_per_rank <- percent_rank(data$income08_per)
data$income10_per_rank <- percent_rank(data$income10_per)
data$income12_per_rank <- percent_rank(data$income12_per)
data$income14_per_rank <- percent_rank(data$income14_per)
data$income16_per_rank <- percent_rank(data$income16_per)
data$income18_per_rank <- percent_rank(data$income18_per)

## parental income measures
# For 17 year olds in 1979, we use data from 1979, 1980, 1981 to capture family income at age 16, 17, 18
# For 16 year olds in 1979, we use data from 1980, 1981, 1982 to capture family income at age 16, 17, 18
# For 15 year olds in 1979, we use data from 1981, 1982, 1983 to capture family income at age 16, 17, 18
# For 14 year olds in 1979, we use data from 1982, 1983, 1984 to capture family income at age 16, 17, 18
data$parental_income_log <- NA
data$parental_income_log[data$AGEATINT_1979==17] <- rowMeans(data[data$AGEATINT_1979==17,c("income79_per_log","income80_per_log","income81_per_log")], na.rm = T)
data$parental_income_log[data$AGEATINT_1979==16] <- rowMeans(data[data$AGEATINT_1979==16,c("income80_per_log","income81_per_log","income82_per_log")], na.rm = T)
data$parental_income_log[data$AGEATINT_1979==15] <- rowMeans(data[data$AGEATINT_1979==15,c("income81_per_log","income82_per_log","income83_per_log")], na.rm = T)
data$parental_income_log[data$AGEATINT_1979==14] <- rowMeans(data[data$AGEATINT_1979==14,c("income82_per_log","income83_per_log","income84_per_log")], na.rm = T)
data$parental_income_log[is.nan(data$parental_income_log)] <- NA # recode NaN to NA. Nothing worrisome. 
summary(data$parental_income_log)

data$parental_income_rank <- NA
data$parental_income_rank[data$AGEATINT_1979==17] <- rowMeans(data[data$AGEATINT_1979==17,c("income79_per_rank","income80_per_rank","income81_per_rank")], na.rm = T)
data$parental_income_rank[data$AGEATINT_1979==16] <- rowMeans(data[data$AGEATINT_1979==16,c("income80_per_rank","income81_per_rank","income82_per_rank")], na.rm = T)
data$parental_income_rank[data$AGEATINT_1979==15] <- rowMeans(data[data$AGEATINT_1979==15,c("income81_per_rank","income82_per_rank","income83_per_rank")], na.rm = T)
data$parental_income_rank[data$AGEATINT_1979==14] <- rowMeans(data[data$AGEATINT_1979==14,c("income82_per_rank","income83_per_rank","income84_per_rank")], na.rm = T)
data$parental_income_rank[is.nan(data$parental_income_rank)] <- NA # recode NaN to NA. Nothing worrisome. 
summary(data$parental_income_rank)

## income destination measures
# We focus on family income from age 44 to age 53. Note that survey year reflects income in the previous year
# For 17 year olds in 1979, we use data from 2008, 2010, 2012, 2014, 2016 to capture family income at age 45, 47, 49, 51, 53.
# For 16 year olds in 1979, we use data from 2008, 2010, 2012, 2014, 2016 to capture family income at age 44, 46, 48, 50, 52.
# For 15 year olds in 1979, we use data from 2010, 2012, 2014, 2016, 2018 to capture family income at age 45, 47, 49, 51, 53.
# For 14 year olds in 1979, we use data from 2010, 2012, 2014, 2016, 2018 to capture family income at age 44, 46, 48, 50, 52.
data$adult_income_log <- NA
data$adult_income_log[data$AGEATINT_1979==17] <- rowMeans(data[data$AGEATINT_1979==17,c("income06_per_log","income08_per_log","income10_per_log","income12_per_log","income14_per_log")], na.rm = T)
data$adult_income_log[data$AGEATINT_1979==16] <- rowMeans(data[data$AGEATINT_1979==16,c("income06_per_log","income08_per_log","income10_per_log","income12_per_log","income14_per_log")], na.rm = T)
data$adult_income_log[data$AGEATINT_1979==15] <- rowMeans(data[data$AGEATINT_1979==15,c("income08_per_log","income10_per_log","income12_per_log","income14_per_log","income16_per_log")], na.rm = T)
data$adult_income_log[data$AGEATINT_1979==14] <- rowMeans(data[data$AGEATINT_1979==14,c("income08_per_log","income10_per_log","income12_per_log","income14_per_log","income16_per_log")], na.rm = T)
data$adult_income_log[is.nan(data$adult_income_log)] <- NA # recode NaN to NA. Nothing worrisome. 
summary(data$adult_income_log)
cor.test(data$parental_income_log, data$adult_income_log)

data$adult_income_rank <- NA
data$adult_income_rank[data$AGEATINT_1979==17] <- rowMeans(data[data$AGEATINT_1979==17,c("income06_per_rank","income08_per_rank","income10_per_rank","income12_per_rank","income14_per_rank")], na.rm = T)
data$adult_income_rank[data$AGEATINT_1979==16] <- rowMeans(data[data$AGEATINT_1979==16,c("income06_per_rank","income08_per_rank","income10_per_rank","income12_per_rank","income14_per_rank")], na.rm = T)
data$adult_income_rank[data$AGEATINT_1979==15] <- rowMeans(data[data$AGEATINT_1979==15,c("income08_per_rank","income10_per_rank","income12_per_rank","income14_per_rank","income16_per_rank")], na.rm = T)
data$adult_income_rank[data$AGEATINT_1979==14] <- rowMeans(data[data$AGEATINT_1979==14,c("income08_per_rank","income10_per_rank","income12_per_rank","income14_per_rank","income16_per_rank")], na.rm = T)
data$adult_income_rank[is.nan(data$adult_income_rank)] <- NA # recode NaN to NA. Nothing worrisome. 
summary(data$adult_income_rank)
cor.test(data$parental_income_rank, data$adult_income_rank)


# College attendance
# Any attendance by age 23. 
# Note that the enrollment variables refer to enrollment status on May 1st. So enrollment when 19 years old captures enrollment in the academic year spanning age 18 and age 19.
# For the 14 year olds in 1979, we use data from 1984, 1985, 1986, 1987, 1988.
# For the 15 year olds in 1979, we use data from 1983, 1984, 1985, 1986, 1987.
# For the 16 year olds in 1979, we use data from 1982, 1983, 1984, 1985, 1986.
# For the 17 year olds in 1979, we use data from 1981, 1982, 1983, 1984, 1985.
data$attendance23 <- NA
data$attendance23[data$AGEATINT_1979==17&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3)] <- 1
data$attendance23[data$AGEATINT_1979==17&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3)] <- 0
data$attendance23[data$AGEATINT_1979==16&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3|data$ENROLLMTREV86_1986==3)] <- 1
data$attendance23[data$AGEATINT_1979==16&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3&data$ENROLLMTREV86_1986!=3)] <- 0
data$attendance23[data$AGEATINT_1979==15&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3|data$ENROLLMTREV86_1986==3|data$ENROLLMTREV87_1987==3)] <- 1
data$attendance23[data$AGEATINT_1979==15&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3&data$ENROLLMTREV86_1986!=3&data$ENROLLMTREV87_1987!=3)] <- 0
data$attendance23[data$AGEATINT_1979==14&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3|data$ENROLLMTREV86_1986==3|data$ENROLLMTREV87_1987==3|data$ENROLLMTREV88_1988==3)] <- 1
data$attendance23[data$AGEATINT_1979==14&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3&data$ENROLLMTREV86_1986!=3&data$ENROLLMTREV87_1987!=3&data$ENROLLMTREV88_1988!=3)] <- 0
# If in any year enrollment=3, but in some other years enrollment=NA, attendance23 will still be 1. 
data$attendance23 <- factor(data$attendance23, levels = c(0,1), labels = c("no","yes"))
table(data$attendance23, useNA="always")

# Any attendance by age 29. 
# Note that the enrollment variables refer to enrollment status on May 1st. So enrollment when 19 years old captures enrollment in the academic year spanning age 18 and age 19.
# For the 14 year olds in 1979, we use data from 1984, 1985, 1986, 1987, 1988.  1994
# For the 15 year olds in 1979, we use data from 1983, 1984, 1985, 1986, 1987.  1993
# For the 16 year olds in 1979, we use data from 1982, 1983, 1984, 1985, 1986.  1992
# For the 17 year olds in 1979, we use data from 1981, 1982, 1983, 1984, 1985.  1991
data$attendance29 <- NA
data$attendance29[data$AGEATINT_1979==17&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3|data$ENROLLMTREV86_1986==3|data$ENROLLMTREV87_1987==3|data$ENROLLMTREV88_1988==3|data$ENROLLMTREV89_1989==3|data$ENROLLMTREV91_1991==3)] <- 1
data$attendance29[data$AGEATINT_1979==17&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3&data$ENROLLMTREV86_1986!=3&data$ENROLLMTREV87_1987!=3&data$ENROLLMTREV88_1988!=3&data$ENROLLMTREV89_1989!=3&data$ENROLLMTREV91_1991!=3)] <- 0
data$attendance29[data$AGEATINT_1979==16&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3|data$ENROLLMTREV86_1986==3|data$ENROLLMTREV87_1987==3|data$ENROLLMTREV88_1988==3|data$ENROLLMTREV89_1989==3|data$ENROLLMTREV91_1991==3|data$ENROLLMTREV92_1992==3)] <- 1
data$attendance29[data$AGEATINT_1979==16&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3&data$ENROLLMTREV86_1986!=3&data$ENROLLMTREV87_1987!=3&data$ENROLLMTREV88_1988!=3&data$ENROLLMTREV89_1989!=3&data$ENROLLMTREV91_1991!=3&data$ENROLLMTREV92_1992!=3)] <- 0
data$attendance29[data$AGEATINT_1979==15&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3|data$ENROLLMTREV86_1986==3|data$ENROLLMTREV87_1987==3|data$ENROLLMTREV88_1988==3|data$ENROLLMTREV89_1989==3|data$ENROLLMTREV91_1991==3|data$ENROLLMTREV92_1992==3|data$ENROLLMTREV93_1993==3)] <- 1
data$attendance29[data$AGEATINT_1979==15&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3&data$ENROLLMTREV86_1986!=3&data$ENROLLMTREV87_1987!=3&data$ENROLLMTREV88_1988!=3&data$ENROLLMTREV89_1989!=3&data$ENROLLMTREV91_1991!=3&data$ENROLLMTREV92_1992!=3&data$ENROLLMTREV93_1993!=3)] <- 0
data$attendance29[data$AGEATINT_1979==14&(data$ENROLLMTREV79_1979==3|data$ENROLLMTREV80_1980==3|data$ENROLLMTREV81_1981==3|data$ENROLLMTREV82_1982==3|data$ENROLLMTREV83_1983==3|data$ENROLLMTREV84_1984==3|data$ENROLLMTREV85_1985==3|data$ENROLLMTREV86_1986==3|data$ENROLLMTREV87_1987==3|data$ENROLLMTREV88_1988==3|data$ENROLLMTREV89_1989==3|data$ENROLLMTREV91_1991==3|data$ENROLLMTREV92_1992==3|data$ENROLLMTREV93_1993==3|data$ENROLLMTREV94_1994==3)] <- 1
data$attendance29[data$AGEATINT_1979==14&(data$ENROLLMTREV79_1979!=3&data$ENROLLMTREV80_1980!=3&data$ENROLLMTREV81_1981!=3&data$ENROLLMTREV82_1982!=3&data$ENROLLMTREV83_1983!=3&data$ENROLLMTREV84_1984!=3&data$ENROLLMTREV85_1985!=3&data$ENROLLMTREV86_1986!=3&data$ENROLLMTREV87_1987!=3&data$ENROLLMTREV88_1988!=3&data$ENROLLMTREV89_1989!=3&data$ENROLLMTREV91_1991!=3&data$ENROLLMTREV92_1992!=3&data$ENROLLMTREV93_1993!=3&data$ENROLLMTREV94_1994!=3)] <- 0
# If in any year enrollment=3, but in some other years enrollment=NA, attendance29 will still be 1. 
data$attendance29 <- factor(data$attendance29, levels = c(0,1), labels = c("no","yes"))
table(data$attendance29, useNA="always")

# high school graduation status at age 20
data$HS20 <- NA  
data$HS20[data$AGEATINT_1979==17&(data$ENROLLMTREV82_1982>=3)] <- 1  # enrolled in college | not enrolled, high school graduate
data$HS20[data$AGEATINT_1979==17&(data$ENROLLMTREV82_1982<=2)] <- 0  # not enrolled, completed less than 12th grade | enrolled in high school
data$HS20[data$AGEATINT_1979==16&(data$ENROLLMTREV83_1983>=3)] <- 1 
data$HS20[data$AGEATINT_1979==16&(data$ENROLLMTREV83_1983<=2)] <- 0  
data$HS20[data$AGEATINT_1979==15&(data$ENROLLMTREV84_1984>=3)] <- 1 
data$HS20[data$AGEATINT_1979==15&(data$ENROLLMTREV84_1984<=2)] <- 0  
data$HS20[data$AGEATINT_1979==14&(data$ENROLLMTREV85_1985>=3)] <- 1 
data$HS20[data$AGEATINT_1979==14&(data$ENROLLMTREV85_1985<=2)] <- 0  
data$HS20 <- factor(data$HS20, levels = c(0,1), labels = c("no","yes"))
table(data$HS20, useNA="always")

# high school graduation status at age 23
data$HS23 <- NA  
data$HS23[data$AGEATINT_1979==17&(data$ENROLLMTREV85_1985==3|data$ENROLLMTREV85_1985==4)] <- 1  # enrolled in college | not enrolled, high school graduate
data$HS23[data$AGEATINT_1979==17&(data$ENROLLMTREV85_1985==1|data$ENROLLMTREV85_1985==2)] <- 0  # not enrolled, completed less than 12th grade | enrolled in high school
data$HS23[data$AGEATINT_1979==16&(data$ENROLLMTREV86_1986==3|data$ENROLLMTREV86_1986==4)] <- 1 
data$HS23[data$AGEATINT_1979==16&(data$ENROLLMTREV86_1986==1|data$ENROLLMTREV86_1986==2)] <- 0  
data$HS23[data$AGEATINT_1979==15&(data$ENROLLMTREV87_1987==3|data$ENROLLMTREV87_1987==4)] <- 1 
data$HS23[data$AGEATINT_1979==15&(data$ENROLLMTREV87_1987==1|data$ENROLLMTREV87_1987==2)] <- 0  
data$HS23[data$AGEATINT_1979==14&(data$ENROLLMTREV88_1988==3|data$ENROLLMTREV88_1988==4)] <- 1 
data$HS23[data$AGEATINT_1979==14&(data$ENROLLMTREV88_1988==1|data$ENROLLMTREV88_1988==2)] <- 0  
table(data$HS23, useNA="always")
table(data$HS23,data$attendance23, useNA="always")
data$HS23[data$attendance23=="yes"] <- 1

# College completion 
# We use age 29 as the cut-off. The treatment is essentially getting bachelor degree in or before age 28.
# 1988 is the first year educational attainment is measured. 
# For the 14 year olds in 1979, we use data from 1988, 1989, 1990, 1991, 1992, 1993, 1994.
# For the 15 year olds in 1979, we use data from 1988, 1989, 1990, 1991, 1992, 1993.
# For the 16 year olds in 1979, we use data from 1988, 1989, 1990, 1991, 1992.
# For the 17 year olds in 1979, we use data from 1988, 1989, 1990, 1991.
# Q3-10B variables >=3 & <=7: bachelor degree or above; Q3-10B <=2 | ==8: HS, associate/junior college, or "other"
data$completion29 <- NA
data$completion29[data$`Q3-10B_1988`>=3 & data$`Q3-10B_1988`<=7] <- 1 
data$completion29[data$`Q3-10B_1988`<=2 | data$`Q3-10B_1988`==8] <- 0 
data$completion29[data$`Q3-10B_1989`>=3 & data$`Q3-10B_1989`<=7] <- 1 
data$completion29[data$`Q3-10B_1989`<=2 | data$`Q3-10B_1989`==8] <- 0 
data$completion29[data$`Q3-10B_1990`>=3 & data$`Q3-10B_1990`<=7] <- 1 
data$completion29[data$`Q3-10B_1990`<=2 | data$`Q3-10B_1990`==8] <- 0 
data$completion29[data$`Q3-10B_1991`>=3 & data$`Q3-10B_1991`<=7] <- 1 
data$completion29[data$`Q3-10B_1991`<=2 | data$`Q3-10B_1991`==8] <- 0 
data$completion29[data$`Q3-10B_1992`>=3 & data$`Q3-10B_1992`<=7 & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15 | data$AGEATINT_1979==16)] <- 1 
data$completion29[(data$`Q3-10B_1992`<=2 | data$`Q3-10B_1992`==8) & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15 | data$AGEATINT_1979==16)] <- 0 
data$completion29[data$`Q3-10B_1993`>=3 & data$`Q3-10B_1993`<=7 & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15)] <- 1 
data$completion29[(data$`Q3-10B_1993`<=2 | data$`Q3-10B_1993`==8) & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15)] <- 0 
data$completion29[data$`Q3-10B_1994`>=3 & data$`Q3-10B_1994`<=7 & (data$AGEATINT_1979==14)] <- 1 
data$completion29[(data$`Q3-10B_1994`<=2 | data$`Q3-10B_1994`==8) & (data$AGEATINT_1979==14)] <- 0 
data$completion29 <- factor(data$completion29, levels = c(0,1), labels = c("no","yes"))
table(data$completion29, useNA = "always")
## Note that the completion variable is NA if the respondent hasn't graduated from high shcool

# graduate degree attainment at age 34
data$grad34 <- NA
data$grad34[data$`Q3-10B_1988`>=5 & data$`Q3-10B_1988`<=7] <- 1 
data$grad34[data$`Q3-10B_1988`<=4 | data$`Q3-10B_1988`==8] <- 0 
data$grad34[data$`Q3-10B_1989`>=5 & data$`Q3-10B_1989`<=7] <- 1 
data$grad34[data$`Q3-10B_1989`<=4 | data$`Q3-10B_1989`==8] <- 0 
data$grad34[data$`Q3-10B_1990`>=5 & data$`Q3-10B_1990`<=7] <- 1 
data$grad34[data$`Q3-10B_1990`<=4 | data$`Q3-10B_1990`==8] <- 0 
data$grad34[data$`Q3-10B_1991`>=5 & data$`Q3-10B_1991`<=7] <- 1 
data$grad34[data$`Q3-10B_1991`<=4 | data$`Q3-10B_1991`==8] <- 0 
data$grad34[data$`Q3-10B_1992`>=5 & data$`Q3-10B_1992`<=7] <- 1 
data$grad34[data$`Q3-10B_1992`<=4 | data$`Q3-10B_1992`==8] <- 0 
data$grad34[data$`Q3-10B_1993`>=5 & data$`Q3-10B_1993`<=7] <- 1 
data$grad34[data$`Q3-10B_1993`<=4 | data$`Q3-10B_1993`==8] <- 0 
data$grad34[data$`Q3-10B_1994`>=5 & data$`Q3-10B_1994`<=7] <- 1 
data$grad34[data$`Q3-10B_1994`<=4 | data$`Q3-10B_1994`==8] <- 0 
data$grad34[data$`Q3-10B_1996`>=5 & data$`Q3-10B_1996`<=7] <- 1 
data$grad34[data$`Q3-10B_1996`<=4 | data$`Q3-10B_1996`==8] <- 0 
data$grad34[data$`Q3-10B_1998`>=5 & data$`Q3-10B_1998`<=7 & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15 | data$AGEATINT_1979==16)] <- 1 
data$grad34[(data$`Q3-10B_1998`<=4 | data$`Q3-10B_1998`==8) & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15 | data$AGEATINT_1979==16)] <- 0 
data$grad34[data$`Q3-10B_2000`>=5 & data$`Q3-10B_2000`<=7 & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15)] <- 1 
data$grad34[(data$`Q3-10B_2000`<=4 | data$`Q3-10B_2000`==8) & (data$AGEATINT_1979==14 | data$AGEATINT_1979==15)] <- 0 
data$grad34 <- factor(data$grad34, levels = c(0,1), labels = c("no","yes"))
table(data$grad34, useNA = "always")


# high school graduation status at age 29
# It is sufficient to focus on the final year in the period instead of all years from 1979. I have checked and the two approach do not differ at all.
data$HS29 <- NA  
data$HS29[data$AGEATINT_1979==17&(data$ENROLLMTREV91_1991==3|data$ENROLLMTREV91_1991==4)] <- 1  # enrolled in college | not enrolled, high school graduate
data$HS29[data$AGEATINT_1979==17&(data$ENROLLMTREV91_1991==1|data$ENROLLMTREV91_1991==2)] <- 0  # not enrolled, completed less than 12th grade | enrolled in high school
data$HS29[data$AGEATINT_1979==16&(data$ENROLLMTREV92_1992==3|data$ENROLLMTREV92_1992==4)] <- 1 
data$HS29[data$AGEATINT_1979==16&(data$ENROLLMTREV92_1992==1|data$ENROLLMTREV92_1992==2)] <- 0  
data$HS29[data$AGEATINT_1979==15&(data$ENROLLMTREV93_1993==3|data$ENROLLMTREV93_1993==4)] <- 1 
data$HS29[data$AGEATINT_1979==15&(data$ENROLLMTREV93_1993==1|data$ENROLLMTREV93_1993==2)] <- 0  
data$HS29[data$AGEATINT_1979==14&(data$ENROLLMTREV94_1994==3|data$ENROLLMTREV94_1994==4)] <- 1 
data$HS29[data$AGEATINT_1979==14&(data$ENROLLMTREV94_1994==1|data$ENROLLMTREV94_1994==2)] <- 0  
table(data$HS29, useNA="always")
table(data$HS29, data$completion29, useNA="always")
data$HS29[!is.na(data$completion29)] <- 1  # if completion is not NA, then high school graduation is established with data from some previous year.

data$completion29[data$HS29==0] <- "no"  # if HS29=0, college completion must be 0


# gender
table(data$SAMPLE_SEX_1979)
data$gender <- NA
data$gender[data$SAMPLE_SEX_1979==1] <- 0
data$gender[data$SAMPLE_SEX_1979==2] <- 1
data$gender <- factor(data$gender, labels = c("Male","Female"))
table(data$gender, useNA = "always")

# race
table(data$SAMPLE_RACE_78SCRN)
data$race <- NA
data$race[data$SAMPLE_RACE_78SCRN==3] <- 0
data$race[data$SAMPLE_RACE_78SCRN==2] <- 1
data$race[data$SAMPLE_RACE_78SCRN==1] <- 2
data$race <- factor(data$race, labels = c("Other","Black","Hispanic"))
table(data$race)

# mother's years of schooling
table(data$`HGC-MOTHER_1979`, useNA = "always")
data$medu <- data$`HGC-MOTHER_1979`

# father's presence
# the closest variable I found is: 
# LIVED WITH BOTH BIOLOGICAL PARENTS FROM BIRTH TO AGE 18?
table(data$`CRES-1_1988`, useNA = "always")
data$parental_presence <- data$`CRES-1_1988`
data$parental_presence <- factor(data$parental_presence, labels = c("No","Yes"))

# number of siblings
table(data$`FAM-28A_1979`, useNA = "always")
data$n_sib <- data$`FAM-28A_1979`
data$n_sib[data$n_sib >=6] <- 6
data$n_sib <- factor(data$n_sib, labels = c("0","1","2","3","4","5","6_or_more"))
table(data$n_sib, useNA = "always")

# urban residence
table(data$`FAM-6_1979`, useNA = "always")
data$urban <- NA
data$urban[data$`FAM-6_1979`==2 | data$`FAM-6_1979`==3] <- 0
data$urban[data$`FAM-6_1979`==1] <- 1
data$urban <- factor(data$urban, labels = c("No","Yes"))
table(data$urban, useNA = "always")

# educational expectation
table(data$`SCHOOL-31_1979`, useNA = "always")
#hist(data$`SCHOOL-31_1979`)   # a three-category coarsening won't loss much information
data$edu_exp <- NA
data$edu_exp[data$`SCHOOL-31_1979`<=11] <- 0 # no HS degree
data$edu_exp[data$`SCHOOL-31_1979`>=12 & data$`SCHOOL-31_1979`<=15] <- 1  # HS degree but no bachelor
data$edu_exp[data$`SCHOOL-31_1979`>=16] <- 2  # bachelor degree or higher
data$edu_exp <- factor(data$edu_exp, labels = c("no_HS_degree","HS_degree_no_bachelor","bachelor_degree_or_higher"))
table(data$edu_exp, useNA = "always")

# AFQT score
# note that the test for this is administered in 1980 when the respondents are 15-18. so there's a small risk of having college reversely affects AFQT.
summary(data$`AFQT-3_1981`)
# hist(data$`AFQT-3_1981`)
data$AFQT <- data$`AFQT-3_1981`

# age in 1979
table(data$AGEATINT_1979, useNA = "always")
data$age <- as.factor(data$AGEATINT_1979)

# HIGHEST GRADE COMPLETED R'S CLOSEST FRIEND WOULD LIKE
table(data$`SCHOOL-32_1979`, useNA = "always")
#hist(data$`SCHOOL-32_1979`)   # a three-category coarsening won't loss much information
data$friend_edu_exp <- NA
data$friend_edu_exp[data$`SCHOOL-32_1979`<=11] <- 0 # no HS degree
data$friend_edu_exp[data$`SCHOOL-32_1979`>=12 & data$`SCHOOL-32_1979`<=15] <- 1  # HS degree but no bachelor
data$friend_edu_exp[data$`SCHOOL-32_1979`>=16] <- 2  # bachelor degree or higher
data$friend_edu_exp <- factor(data$friend_edu_exp, labels = c("no_HS_degree","HS_degree_no_bachelor","bachelor_degree_or_higher"))
table(data$friend_edu_exp, useNA = "always")

# Rotter - Locus of Control Scale Score
summary(data$ROTTER_SCORE_1979)
#hist(data$ROTTER_SCORE_1979)
data$rotter_score <- data$ROTTER_SCORE_1979

# ROSENBERG ESTEEM ITEM RESPONSE SCORE
summary(data$ROSENBERG_IRT_SCORE_1980)
data$rosenberg_irt_score <- data$ROSENBERG_IRT_SCORE_1980

# ATTITUDE OF INFLUENTIAL PERSON TO R'S DECISION TO NOT ATTEND COLLEGE
table(data$`OTHER-3E`, useNA = 'always')
# 1 STRONGLY DISAPPROVE; 2 SOMEWHAT DISAPPROVE; 3 SOMEWHAT APPROVE; 4 STRONGLY APPROVE
data$sig_other_expec <- as.factor(data$`OTHER-3E`)

# WAS FOREIGN LANGUAGE SPOKEN AT HOME DURING R'S CHILDHOOD?
table(data$`FAM-3`, useNA = 'always')
data$foreign_lang <- as.factor(data$`FAM-3`)
table(data$foreign_lang)

# IS R'S CURRENT RESIDENCE IN SMSA?
table(data$`SMSARES_1979`, useNA = 'always')
# 0 NOT IN SMSA； 1 SMSA, NOT CENTRAL CITY； 2 SMSA, CENTRAL CITY NOT KNOWN； 3 SMSA, IN CENTRAL CITY
data$SMSA <- as.factor(data$`SMSARES_1979`)
table(data$SMSA, data$urban)

# INT CHECK 79 - DO R AND MOTHER/STEPMOTHER LIVE SEPARATELY?
table(data$`FAM-20_1979`, useNA = 'always')
data$mother_seperate <- as.factor(data$`FAM-20_1979`)

# ATTITUDE TOWARD CURRENT SCHOOL
table(data$`SCHOOL-3B_1979`, useNA = 'always')
data$school_satisfaction <- as.factor(data$`SCHOOL-3B_1979`)

# BIRTHPLACE OF R'S FATHER - COUNTRY
table(data$`FAM-21_1979`, useNA = 'always')
data$f_foreign_born <- data$`FAM-21_1979`
data$f_foreign_born[data$f_foreign_born==3] <- NA # never knew father
data$f_foreign_born <- as.factor(data$f_foreign_born)
table(data$f_foreign_born, useNA = 'always')

#	BIRTHPLACE OF R'S MOTHER - COUNTRY
table(data$`FAM-15_1979`, useNA = 'always')
data$m_foreign_born <- data$`FAM-15_1979`
data$m_foreign_born[data$m_foreign_born==3] <- NA # never knew mother
data$m_foreign_born <- as.factor(data$m_foreign_born)
table(data$m_foreign_born, useNA = 'always')

data$fm_foreign_born <- NA
data$fm_foreign_born[(data$f_foreign_born==2 | data$m_foreign_born==2) & !is.na(data$f_foreign_born) & !is.na(data$m_foreign_born)] <- 1
data$fm_foreign_born[(data$f_foreign_born==1 | data$m_foreign_born==1) & !is.na(data$f_foreign_born) & !is.na(data$m_foreign_born)] <- 0
data$fm_foreign_born <- as.factor(data$fm_foreign_born)

#	REGION OF CURRENT RESIDENCE
table(data$REGION_1979, useNA = 'always')
data$region <- as.factor(data$REGION_1979)

# DID ADULT FEMALE PRESENT IN HOUSEHOLD AT AGE 14 WORK FOR PAY?
table(data$`FAM-9_1979`, useNA = "always")
data$m_work <- as.factor(data$`FAM-9_1979`)


# data for the great equalizer thesis
#data_cleaned_ge <- data[!is.na(data$HS29),]
#data_cleaned_ge <- data_cleaned_ge[data_cleaned_ge$HS29==1,]
data_cleaned_ge <- data
data_cleaned_ge <- data_cleaned_ge[,c("adult_income_log","adult_income_rank","completion29","parental_income_log","parental_income_rank","race","gender",
                        "medu","parental_presence","n_sib","urban","edu_exp","AFQT","age","friend_edu_exp",
                        "rotter_score","rosenberg_irt_score","sig_other_expec","school_satisfaction","foreign_lang",
                        "SMSA","mother_seperate","fm_foreign_born","region","m_work")]

colMeans(is.na(data_cleaned_ge))

saveRDS(data_cleaned_ge, file="data_cleaned_ge.rds")

# data for the Mare thesis (high school graduation)
data_cleaned_mare_hs <- data[,c("HS20","attendance23","parental_income_log","parental_income_rank","race","gender",
                                                                "medu","parental_presence","n_sib","urban","edu_exp","AFQT","age","friend_edu_exp",
                                                                "rotter_score","rosenberg_irt_score","sig_other_expec","school_satisfaction","foreign_lang",
                                                                "SMSA","mother_seperate","fm_foreign_born","region","m_work")]

saveRDS(data_cleaned_mare_hs, file="data_cleaned_mare_hs.rds")

# data for the Mare thesis (attendance)
#data_cleaned_mare_attendance <- data[!is.na(data$HS29),]
#data_cleaned_mare_attendance <- data_cleaned_mare_attendance[data_cleaned_mare_attendance$HS29==1,]
data_cleaned_mare_attendance <- data
data_cleaned_mare_attendance <- data_cleaned_mare_attendance[,c("attendance23","completion29","parental_income_log","parental_income_rank","race","gender",
                                      "medu","parental_presence","n_sib","urban","edu_exp","AFQT","age","friend_edu_exp",
                                      "rotter_score","rosenberg_irt_score","sig_other_expec","school_satisfaction","foreign_lang",
                                      "SMSA","mother_seperate","fm_foreign_born","region","m_work")]

saveRDS(data_cleaned_mare_attendance, file="data_cleaned_mare_attendance.rds")

data_cleaned_mare_attendance_conditional <- data
data_cleaned_mare_attendance_conditional <- data_cleaned_mare_attendance_conditional[,c("HS20","attendance23","completion29","parental_income_log","parental_income_rank","race","gender",
                                                                "medu","parental_presence","n_sib","urban","edu_exp","AFQT","age","friend_edu_exp",
                                                                "rotter_score","rosenberg_irt_score","sig_other_expec","school_satisfaction","foreign_lang",
                                                                "SMSA","mother_seperate","fm_foreign_born","region","m_work")]

saveRDS(data_cleaned_mare_attendance_conditional, file="data_cleaned_mare_attendance_conditional.rds")

# data for the Mare thesis (graduate degree)
#data_cleaned_mare_gradaute <- data[!is.na(data$HS29),]
#data_cleaned_mare_gradaute <- data_cleaned_mare_gradaute[data_cleaned_mare_gradaute$HS29==1,]
data_cleaned_mare_gradaute <- data
data_cleaned_mare_gradaute <- data_cleaned_mare_gradaute[,c("completion29","grad34","parental_income_log","parental_income_rank","race","gender",
                                                                "medu","parental_presence","n_sib","urban","edu_exp","AFQT","age","friend_edu_exp",
                                                                "rotter_score","rosenberg_irt_score","sig_other_expec","school_satisfaction","foreign_lang",
                                                                "SMSA","mother_seperate","fm_foreign_born","region","m_work")]

saveRDS(data_cleaned_mare_gradaute, file="data_cleaned_mare_gradaute.rds")

data_cleaned_mare_gradaute_conditional <- data
data_cleaned_mare_gradaute_conditional <- data_cleaned_mare_gradaute_conditional[,c("attendance23","completion29","grad34","parental_income_log","parental_income_rank","race","gender",
                                                            "medu","parental_presence","n_sib","urban","edu_exp","AFQT","age","friend_edu_exp",
                                                            "rotter_score","rosenberg_irt_score","sig_other_expec","school_satisfaction","foreign_lang",
                                                            "SMSA","mother_seperate","fm_foreign_born","region","m_work")]

saveRDS(data_cleaned_mare_gradaute_conditional, file="data_cleaned_mare_gradaute_conditional.rds")


data_cleaned_mare_all <- data[,c("HS20","attendance23","completion29","grad34","parental_income_log",
                                                  "gender","medu","parental_presence",
                                                  "n_sib","urban","edu_exp","AFQT","age","friend_edu_exp","rotter_score","rosenberg_irt_score",
                                                  "sig_other_expec","foreign_lang",
                                                  "SMSA","mother_seperate","fm_foreign_born",
                                                  "region","m_work","race")]

saveRDS(data_cleaned_mare_all, file="data_cleaned_mare_all.rds")

