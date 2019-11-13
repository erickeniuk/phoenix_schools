library(data.table)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(plyr)
library(leaps)
library(rio)
library(foreign)

### TESTING TESTING TESTING TESTING TESTING TESTING
fullscores_enroll <- readRDS("fullscores_enroll.RDS")
# merge full scores with shares of investors by school ownership
length(which(is.na(fullscores_enroll$sch_id))==TRUE)
fullscores_enroll <- fullscores_enroll[!is.na(fullscores_enroll$sch_id),]
fullscores_enroll <- fullscores_enroll[dt_table_own, on = c("year"="int_year","school"="school"), ]  ## adding shares of investors
fullscores_enroll <- fullscores_enroll[!is.na(fullscores_enroll$sch_id),]
fullscores_enroll <- fullscores_enroll[dt_table, on = c("year"="int_year","school"="school"), ]  ## adding shares of investors
fullscores_enroll <- fullscores_enroll[!is.na(fullscores_enroll$sch_id),]
names(fullscores_enroll)[14] <- "share_investor_purchased"
final <- fullscores_enroll
#impute the average for shares = to 0?
saveRDS(final, file="phoenix_panel_final.RDS")









E <- pdata.frame(final, index=c("sch_id","year"), drop.index=TRUE, row.names=TRUE)
head(diff(E$share_investor), 10)
E$share_investor <- diff(E$share_investor)
summary(plm(science_pass~share_investor, data = E, model = "within"))
summary(plm(science_pass~share_investor, data = E, model = "random"))

summary(lm(log(math_pass) ~ charter + log(share_investor+1) + factor(year), data=final))
summary(lm(log(science_pass) ~ charter + log(share_investor+1) + factor(year), data=final))
summary(lm(log(reading_pass) ~ charter + log(share_investor+1) + factor(year), data=final))


summary(lm(log(math_pass) ~ charter + log(share_investor+1) + factor(school), data=final))
summary(lm(log(science_pass) ~ charter + log(share_investor+1) + factor(school), data=final))
summary(lm(log(reading_pass) ~ charter + log(share_investor+1) + factor(school), data=final))

summary(lm(final$science_pass ~ final$`Teachers absent more than 10 days of the school year`+ final$`Classroom Teachers` + final$`High-School Counselors`
           + final$`Total enrollment`+final$`Non-personnel expenditures at school level`+final$`Personnel salaries at school level - total`+log(final$share_investor+1) + factor(school), data=final))

fixed <- plm(science_pass ~ final$`Teachers absent more than 10 days of the school year`+ final$`Classroom Teachers` + final$`High-School Counselors`
             + final$`Total enrollment`+final$`Non-personnel expenditures at school level`+final$`Personnel salaries at school level - total`+log(share_investor+1), data=final, index=c('school','year'), model="within")
summary(fixed)
fixed <- plm(math_pass ~ log(share_investor+1), data=final, index=c('sch_id','year'), model="within")
summary(fixed)
fixed <- plm(reading_pass ~ log(share_investor+1), data=final, index=c('sch_id','year'), model="within")
summary(fixed)

random <- plm(science_pass ~ log(share_investor+1), data=final, index=c('sch_id','year'), model="random")
summary(random)
random <- plm(math_pass ~ log(share_investor+1), data=final, index=c('sch_id','year'), model="random")
summary(random)
random <- plm(reading_pass ~ log(share_investor+1), data=final, index=c('sch_id','year'), model="random")
summary(random)
random <- plm(science_pass ~ log(share_investor+1), data=final, index=c('sch_id','year'), model="random")
summary(random)

final$`Classroom Teachers` <- as.integer(final$`Classroom Teachers`)
final$`Total enrollment` <- as.integer(final$`Total enrollment`)
final$`High-School Counselors` <- as.integer(final$`High-School Counselors`)
final$`Non-personnel expenditures at school level` <- as.integer(final$`Non-personnel expenditures at school level`)
final$`Personnel salaries at school level - total` <- as.integer(final$`Personnel salaries at school level - total`)
final$`Teachers absent more than 10 days of the school year` <- as.integer(final$`Teachers absent more than 10 days of the school year`)
names(final)
## first difference
summary(plm(science_pass ~ final$`Teachers absent more than 10 days of the school year`+ final$`Classroom Teachers` + final$`High-School Counselors`
            + final$`Total enrollment`+final$`Non-personnel expenditures at school level`+final$`Personnel salaries at school level - total`+log(share_investor+1) - 1, data = final, model = "fd"))

###

head(lag(final$share_investor, 2), 10)

phtest(fixed, random)


head(lag(E$share_investor_owned, 1))
# merge full scores with shares of investors by school OWNERSHIP
final <- fullscores[dt_table, on = c("year"="int_year","school"="school"), ]  ## adding shares of investors
final <- final[!is.na(final$sch_id),]
#saveRDS(final,  file="final_no_extra.RDS")
final <- final[final$year==c(2010,2015),]
table(final$school) != 2
names(which(table(final$school) != 2))
final <- final[!final$school %in% names(which(table(final$school) != 2)), ]
final <- final[final$`0` != 0,]

summary(lm(log(math_pass) ~ log(share_investor_owned+1), data=final))
summary(lm(science_pass ~ charter + log(share_investor_owned+1), data=final))
summary(lm(reading_pass ~ charter + log(share_investor_owned+1), data=final))
### add control variables
length(unique(final$school))
length(unique(AZteach$school))
length(unique(final$school) %in% unique(AZteach$school))
final <- final[AZteach, on = c("year"="year","school"="school")] ## merge supporting statistics
final <- final[!is.na(final$sch_id),]
length(unique(final$school))
final <- final[,-c("Personnel salaries at school level - instructional staff only","Total enrollment", "Total enrollment.1")]

names(final)
str(final)

final$`Classroom Teachers` <- as.numeric(final$`Classroom Teachers`)
final$`Classroom teachers in their second year of teaching` <- as.numeric(final$`Classroom teachers in their second year of teaching`)
final$`Classroom teachers in their first year of teaching` <- as.numeric(final$`Classroom teachers in their first year of teaching`)

final$`Non-personnel expenditures at school level` <- as.numeric(final$`Non-personnel expenditures at school level`)
final$`Teachers absent more than 10 days of the school year` <- as.numeric(final$`Teachers absent more than 10 days of the school year`)
final$`High-School Counselors` <- as.numeric(final$`High-School Counselors`)
final$`Personnel salaries at school level - teachers only` <- as.numeric(final$`Personnel salaries at school level - teachers only`)
final$`Personnel salaries at school level - total` <- as.numeric(final$`Personnel salaries at school level - total`)
final <- na.omit(final)
finaltest <- final[,-c("sch_id","math_pass","reading_pass", "mean_pass","charter")]
finaltest <- pdata.frame(finaltest, index=c("school","year"), drop.index=TRUE, row.names=TRUE)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(finaltest), replace = T, prob = c(0.6,0.4))
train <- finaltest[sample, ]
test <- finaltest[!sample, ]
names(finaltest)

backward <- regsubsets(share_investor ~  . , train, nvmax = 10, method = "backward")
which.min(summary(backward)$cp)
coef(backward, 5)

summary(lm(share_investor ~ `Classroom teachers in their first year of teaching` + `American.Indian.or.Alaska.Native`
           + `White` + `total.students` + `Personnel salaries at school level - teachers only`, data=finaltest))


### FINAL MERGE
fullscores_enroll$school <- tolower(fullscores_enroll$school)
dt_table$school <- tolower(dt_table$school)
dt_table_own$school <- tolower(dt_table_own$school)

final_dataset <- fullscores_enroll[dt_table,on = c("school"="school","year"="int_year"),]
final_dataset <- final_dataset[dt_table_own,on = c("school"="school","year"="int_year"),]

final_dataset <- final_dataset[!is.na(final_dataset$sch_id),]        


