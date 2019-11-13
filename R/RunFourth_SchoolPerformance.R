library(data.table)
library(plm)

AZ10 <- read_csv("AZ2010.csv")
AZ11 <- read_csv("AZ2011.csv")
AZ12 <- read_csv("AZ2012.csv")
AZ13 <- read_csv("AZ2013.csv")
AZ14 <- read_csv("AZ2014.csv")

AZSci15 <- read_csv("AZScience2015.csv")
AZMat_Read15 <- read_csv("AZMathReading2015.csv")

AZSci16 <- read_csv("AZScience2016.csv")
AZMat_Read16 <- read_csv("AZMathReading2016.csv")

AZSci17 <- read_csv("AZScience2017.csv")
AZMat_Read17 <- read_csv("AZMathReading2017.csv")

AZSci18 <- read_csv("AZScience2018.csv")
AZMat_Read18 <- read_csv("AZMathReading2018.csv")

AZSci19 <- read_csv("AZScience2019.csv")
AZMat_Read19 <- read_csv("AZMathReading2019.csv")

### SPLIT MATH AND READING SCORES
AZMat15 <- AZMat_Read15[AZMat_Read15$`Content Area` == 'Math',]
AZMat15 <- AZMat15[AZMat15$`Subgroup/Ethnicity` == 'All Students',]
AZMat15 <- AZMat15[AZMat15$`Test Level` == 'All',]

AZMat16 <- AZMat_Read16[AZMat_Read16$`Content Area` == 'Math',]
AZMat16 <- AZMat16[AZMat16$`Subgroup/Ethnicity` == 'All Students',]
AZMat16 <- AZMat16[AZMat16$`Test Level` == 'All',]

AZMat17 <- AZMat_Read17[AZMat_Read17$`Content Area` == 'Math',]
AZMat17 <- AZMat17[AZMat17$`Subgroup/Ethnicity` == 'All Students',]
AZMat17 <- AZMat17[AZMat17$`Test Level` == 'All',]

AZMat18 <- AZMat_Read18[AZMat_Read18$`Content Area` == 'Math',]
AZMat18 <- AZMat18[AZMat18$`Subgroup/Ethnicity` == 'All Students',]
AZMat18 <- AZMat18[AZMat18$`Test Level` == 'All',]

AZMat19 <- AZMat_Read19[AZMat_Read19$Subject == 'Mathematics',]
AZMat19 <- AZMat19[AZMat19$Subgroup == 'All Students',]
AZMat19 <- AZMat19[AZMat19$`Test Level` == 'All Assessments',]
### reading
AZRead15 <- AZMat_Read15[AZMat_Read15$`Content Area` == 'English Language Arts',]
AZRead15 <- AZRead15[AZRead15$`Subgroup/Ethnicity` == 'All Students',]
AZRead15 <- AZRead15[AZRead15$`Test Level` == 'All',]

AZRead16 <- AZMat_Read16[AZMat_Read16$`Content Area` == 'English Language Arts',]
AZRead16 <- AZRead16[AZRead16$`Subgroup/Ethnicity` == 'All Students',]
AZRead16 <- AZRead16[AZRead16$`Test Level` == 'All',]

AZRead17 <- AZMat_Read17[AZMat_Read17$`Content Area` == 'English Language Arts',]
AZRead17 <- AZRead17[AZRead17$`Subgroup/Ethnicity` == 'All Students',]
AZRead17 <- AZRead17[AZRead17$`Test Level` == 'All',]

AZRead18 <- AZMat_Read18[AZMat_Read18$`Content Area` == 'English Language Arts',]
AZRead18 <- AZRead18[AZRead18$`Subgroup/Ethnicity` == 'All Students',]
AZRead18 <- AZRead18[AZRead18$`Test Level` == 'All',]

AZRead19 <- AZMat_Read19[AZMat_Read19$Subject == 'English Language Arts',]
AZRead19 <- AZRead19[AZRead19$Subgroup == 'All Students',]
AZRead19 <- AZRead19[AZRead19$`Test Level` == 'All Assessments',] 
### science 2019
AZSci19 <- AZSci19[AZSci19$Subgroup == 'All Students',]
AZSci19 <- AZSci19[AZSci19$`Grade/Cohort (High School defined by Cohort Year)`== 'All Grades',] 

####building final dataset 2010 to start
fullscores <- data.table(year=AZ10$`Fiscal Year`,sch_id=AZ10$`School CTDS Number`,school=AZ10$`School Name`,
                         science_pass=AZ10$`Science Percent Passing`, science_exceed=
                         math_pass=AZ10$`Math Percent Passing`,
                         reading_pass=AZ10$`Reading Percent Passing`
                         ,charter=AZ10$`Charter School`)
# 2011
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ11$`Fiscal Year`, sch_id=AZ11$`School CTDS Number`,school=AZ11$`School Name`,
                                                    science_pass=AZ11$`Science Percent Passing`,
                                                    math_pass=AZ11$`Math Percent Passing`,
                                                    reading_pass=AZ11$`Reading Percent Passing`
                                                    ,charter=AZ11$`Charter School`)))
# 2012
names(AZ12)[c(22,34)] <- c(names(AZ11)[c(22,34)])
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ12$`Fiscal Year`, sch_id=AZ12$`School CTDS Number`,school=AZ12$`School Name`,
                                                    science_pass=AZ12$`Science Percent Passing`,
                                                    math_pass=AZ12$`Math Percent Passing`,
                                                    reading_pass=AZ12$`Reading Percent Passing`
                                                    ,charter=AZ12$`Charter School`)))
# 2013
names(AZ13)[c(22,34)] <- c(names(AZ11)[c(22,34)])
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ13$`Fiscal Year`, sch_id=AZ13$`School CTDS Number`,school=AZ13$`School Name`,
                                                    science_pass=AZ13$`Science Percent Passing`,
                                                    math_pass=AZ13$`Math Percent Passing`,
                                                    reading_pass=AZ13$`Reading Percent Passing`
                                                    ,charter=AZ13$`Charter School`)))
# 2014
names(AZ14)[c(22,34)] <- c(names(AZ11)[c(22,34)])
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ14$`Fiscal Year`, sch_id=AZ14$`School CTDS Number`,school=AZ14$`School Name`,
                                                    science_pass=AZ14$`Science Percent Passing`,
                                                    math_pass=AZ14$`Math Percent Passing`,
                                                    reading_pass=AZ14$`Reading Percent Passing`
                                                    ,charter=AZ14$`Charter School`)))
# 2015 
# Merging 2015
names(AZSci15)
names(AZRead15)
names(AZMat15)
AZSci15 <- AZSci15[,c(1,8,9,10,16)]
AZRead15 <- AZRead15[,c(1,2,4,5,13)]
AZMat15 <- AZMat15[,c(1,2,4,5,13)]
AZSci15 <- as.data.table(AZSci15)
AZMat15 <- as.data.table(AZMat15)
AZRead15 <- as.data.table(AZRead15)
setkey(AZSci15, 'School CTDS Number')
setkey(AZRead15, 'School CTDS Number')
AZ15 <- merge(AZSci15, AZRead15, all.x=TRUE)
names(AZ15)[9] <- "Reading Percent Passing"
names(AZ15)
setkey(AZMat15, 'School CTDS Number')
AZ15 <- merge(AZ15, AZMat15, all.x=TRUE)
names(AZ15)[13] <- "Math Percent Passing"
names(AZ15)
AZ15 <- AZ15[,c(1,2,3,4,5,9,13)]
### Adding 2015 to the full dataset
names(AZ11) 
names(AZ15)[c(2,3,4,5)] <- c(names(AZ11)[c(1,9,10,34)])
names(AZ15)
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ15$`Fiscal Year`, sch_id=AZ15$`School CTDS Number`,school=AZ15$`School Name`,
                                                    science_pass=AZ15$`Science Percent Passing`,
                                                    math_pass=AZ15$`Math Percent Passing`,
                                                    reading_pass=AZ15$`Reading Percent Passing`
                                                    ,charter=AZ15$`Charter School`)))
# 2016
names(AZSci16)
names(AZRead16)
names(AZMat16)
AZSci16 <- as.data.table(AZSci16)
AZMat16 <- as.data.table(AZMat16)
AZRead16 <- as.data.table(AZRead16)
AZSci16[,'Fiscal Year':=2016] # addding column for year
AZSci16 <- AZSci16[,c(16,2,4,5,15)]
AZRead16 <- AZRead16[,c(1,2,4,5,13)]
AZMat16 <- AZMat16[,c(1,2,4,5,13)]
setkey(AZSci16, 'School CTDS Number')
setkey(AZRead16, 'School CTDS Number')
AZ16 <- merge(AZSci16, AZRead16, all.x=TRUE)
names(AZ16)[9] <- "Reading Percent Passing"
names(AZ16)
setkey(AZMat16, 'School CTDS Number')
AZ16 <- merge(AZ16, AZMat16, all.x=TRUE)
names(AZ16)[13] <- "Math Percent Passing"
names(AZ16)
AZ16 <- AZ16[,c(1,2,3,4,5,9,13)]
### Adding 2015 to the full dataset
names(AZ11) 
names(AZ16)[c(2,3,4,5)] <- c(names(AZ11)[c(1,9,10,34)])
names(AZ16)
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ16$`Fiscal Year`, sch_id=AZ16$`School CTDS Number`,school=AZ16$`School Name`,
                                                    science_pass=AZ16$`Science Percent Passing`,
                                                    math_pass=AZ16$`Math Percent Passing`,
                                                    reading_pass=AZ16$`Reading Percent Passing`
                                                    ,charter=AZ16$`Charter School`)))
saveRDS(fullscores, file="Fulldataset_2016")
#2017
names(AZSci17)
names(AZRead17)
names(AZMat17)
AZSci17 <- as.data.table(AZSci17)
AZMat17 <- as.data.table(AZMat17)
AZRead17 <- as.data.table(AZRead17)
AZSci17 <- AZSci17[,c(1,7,8,9,16)]
AZRead17 <- AZRead17[,c(1,2,4,5,14)]
AZMat17 <- AZMat17[,c(1,2,4,5,14)]
setkey(AZSci17, 'School CTDS Number')
names(AZRead17)[3] <- 'School CTDS Number'
setkey(AZRead17, 'School CTDS Number')
AZ17 <- merge(AZSci17, AZRead17, all.x=TRUE)
names(AZ17)[9] <- "Reading Percent Passing"
names(AZ17)
names(AZMat17)[3] <- 'School CTDS Number'
setkey(AZMat17, 'School CTDS Number')
AZ17 <- merge(AZ17, AZMat17, all.x=TRUE)
names(AZ17)[13] <- "Math Percent Passing"
names(AZ17)
AZ17 <- AZ17[,c(1,2,3,4,5,9,13)]
### Adding 2015 to the full dataset
names(AZ11) 
names(AZ17)[c(2,3,4,5)] <- c(names(AZ11)[c(1,9,10,34)])
names(AZ17)
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ17$`Fiscal Year`, sch_id=AZ17$`School CTDS Number`,school=AZ17$`School Name`,
                                                    science_pass=AZ17$`Science Percent Passing`,
                                                    math_pass=AZ17$`Math Percent Passing`,
                                                    reading_pass=AZ17$`Reading Percent Passing`
                                                    ,charter=AZ17$`Charter School`)))

saveRDS(fullscores, file="Fulldataset_2017")
#2018
names(AZSci18)
names(AZRead18)
names(AZMat18)
AZSci18 <- as.data.table(AZSci18)
AZMat18 <- as.data.table(AZMat18)
AZRead18 <- as.data.table(AZRead18)
AZSci18 <- AZSci18[,c(1,7,8,9,16)]
AZRead18 <- AZRead18[,c(1,2,4,5,14)]
AZMat18 <- AZMat18[,c(1,2,4,5,14)]
setkey(AZSci18, 'School CTDS Number')
names(AZRead18)[3] <- 'School CTDS Number'
setkey(AZRead18, 'School CTDS Number')
AZ18 <- merge(AZSci18, AZRead18, all.x=TRUE)
names(AZ18)[9] <- "Reading Percent Passing"
names(AZ18)
names(AZMat18)[3] <- 'School CTDS Number'
setkey(AZMat18, 'School CTDS Number')
AZ18 <- merge(AZ18, AZMat18, all.x=TRUE)
names(AZ18)[13] <- "Math Percent Passing"
names(AZ18)
AZ18 <- AZ18[,c(1,2,3,4,5,9,13)]
### Adding 2015 to the full dataset
names(AZ11) 
names(AZ18)[c(2,3,4,5)] <- c(names(AZ11)[c(1,9,10,34)])
names(AZ18)
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ18$`Fiscal Year`, sch_id=AZ18$`School CTDS Number`,school=AZ18$`School Name`,
                                                    science_pass=AZ18$`Science Percent Passing`,
                                                    math_pass=AZ18$`Math Percent Passing`,
                                                    reading_pass=AZ18$`Reading Percent Passing`
                                                    ,charter=AZ18$`Charter School`)))
saveRDS(fullscores, file="Fulldataset_2018")
#2019
names(AZSci19)
names(AZRead19)
names(AZMat19)
AZSci19 <- AZSci19[,c(1,6,7,8,19)]
AZRead19 <- AZRead19[,c(1,3,4,5,14)]
AZMat19<- AZMat19[,c(1,3,4,5,14)]
names(AZSci19)[2] <- 'School CTDS Number'
names(AZRead19)[3] <- 'School CTDS Number'
names(AZMat19)[3] <- 'School CTDS Number'
AZSci19 <- as.data.table(AZSci19)
AZMat19 <- as.data.table(AZMat19)
AZRead19 <- as.data.table(AZRead19)
setkey(AZSci19, 'School CTDS Number')
setkey(AZRead19, 'School CTDS Number')
AZ19 <- merge(AZSci19, AZRead19, all.x=TRUE)
names(AZ19)[9] <- "Reading Percent Passing"
setkey(AZMat19, 'School CTDS Number')
AZ19 <- merge(AZ19, AZMat19, all.x=TRUE)
names(AZ19)[13] <- "Math Percent Passing"
names(AZ19)[5] <- "Science Percent Passing"
names(AZ19)
AZ19 <- AZ19[,c(1,2,3,4,5,9,13)]
### Adding 2015 to the full dataset
names(AZ11) 
names(AZ19)[c(2,3,4,5)] <- c(names(AZ11)[c(1,9,10,34)])
names(AZ19)
fullscores<- rbindlist(list(fullscores,  data.table(year=AZ19$`Fiscal Year`, sch_id=AZ19$`School CTDS Number`,school=AZ19$`School Name`,
                                                    science_pass=AZ19$`Science Percent Passing`,
                                                    math_pass=AZ19$`Math Percent Passing`,
                                                    reading_pass=AZ19$`Reading Percent Passing`
                                                    ,charter=AZ19$`Charter School`)))
# converting charters to dummy 1 and 0 ; yes = 1
fullscores$charter[fullscores$charter=='Yes'] = 1
fullscores$charter[fullscores$charter=='Y'] = 1
fullscores$charter[fullscores$charter=='No'] = 0
fullscores$charter[fullscores$charter=='N'] = 0

fullscores <- fullscores[,science_pass_int:=as.integer(fullscores$science_pass)]
fullscores <- fullscores[,math_pass_int:=as.integer(fullscores$math_pass)]
fullscores <- fullscores[,reading_pass_int:=as.integer(fullscores$reading_pass)]
fullscores <- fullscores[,charter_int:= as.integer(fullscores$charter)]
str(fullscores)
names(fullscores)
fullscores <- fullscores[,-c(4,5,6,7)]
names(fullscores)[c(4,5,6,7)] <- c("science_pass","math_pass","reading_pass","charter")

#(fullscores, file="Fulldataset_2019")
fullscores[,mean_pass:=rowMeans(fullscores[,c("science_pass","math_pass","reading_pass")])]
#saveRDS(fullscores, file="Fulldataset_2019")
fullscores <- readRDS("Fulldataset_2019")

###### school data
AZteach <- read.csv("AZteachers.csv")
AZteach <- AZteach[,c(4,6,7,8,9,18)]
AZteach <- data.table(school=as.character(AZteach$School), 
                      year=as.integer(AZteach$Year),
                      category=as.character(AZteach$Category),
                      totalnums=as.integer(AZteach$Total),
                      totalstudents=as.integer(AZteach$Total.Students))

AZteach$totalnums[AZteach$category=="Total enrollment"] = AZteach$totalstudents[AZteach$category=="Total enrollment"]
AZteach <- AZteach[,-5]
AZteach <- dcast(AZteach, year + school ~ category, toString, value.var= "totalnums")
AZteach <- AZteach[,-c(8,9)]
AZenroll <- read.csv("AZenrollment.csv")
AZenroll <- AZenroll[,c(4,6,8,10,11,12,13,14,15,16)]
AZteach <- AZteach[AZenroll, on = c("year"="Year","school"="School")]  ### merging datasets together
#financials
AZfinance <- read.csv("AZfinancials_20092015.csv")
AZfinance <- data.table(school=AZfinance$School,year=AZfinance$Year,
                        category=as.character(AZfinance$Category),amount=as.integer(AZfinance$Amount),
                        total=AZfinance$Total)
str(AZfinance)
AZfinance$amount[AZfinance$category=="Total enrollment"] = AZfinance$total[AZfinance$category=="Total enrollment"]
AZfinance <- AZfinance[,-5]
AZteach <- AZteach[AZfinance, on = c("year"="year","school"="school")] ## merging together teach, enroll, and finance
names(AZteach)
AZteach <- AZteach[,-c("Asian","Hawaiian..Pacific.Islander","Two.or.more.races")]
names(AZteach)[13] <- "total.students"
names(AZteach)
AZteach <- dcast(AZteach, ... ~ category, toString,value.var= "amount")
AZteach <- AZteach[,-c("Instructional Aides" ,"School Administration Staff","Support Services Staff (for Pupils and Instructional Staff)")]


length(which(final$school %in% AZteach$school)==TRUE)
length(which(final$school %in% AZfinance$school)==TRUE)
length(which(final$school %in% AZenroll$School)==TRUE)

AZenrolled <- read.csv("enrolled20112015.csv")

AZenrolled <- AZenrolled[AZenrolled$School %in% names(which(table(AZenrolled$School)==2)),]


######
#2010
enrollment10 <-read.csv("2010enroll.csv")
enrollment10 <- enrollment10[enrollment10$SchoolName != '',]
enrollment10 <- data.table(school=as.character(enrollment10$SchoolName), type=as.character(enrollment10$Type), total=as.integer(enrollment10$Total))

enrollment10all <- enrollment10[enrollment10$type=="All Students",]
enrollment10all <- enrollment10all[,allstudents:=max(total), by=school]
enrollment10all <- enrollment10all[,-c("total", "type")]
enrollment10all <- unique(enrollment10all)

enrollment10ell <- enrollment10[enrollment10$type=="ELL",]
enrollment10ell <- enrollment10ell[,ell:=max(total), by=school]
enrollment10ell <- enrollment10ell[,-c("total", "type")]
enrollment10ell <- unique(enrollment10ell)

enrollment10freelunch <- enrollment10[enrollment10$type=="Free Lunch",]
enrollment10freelunch <- enrollment10freelunch[,freelunch:=max(total), by=school]
enrollment10freelunch <- enrollment10freelunch[,-c("total", "type")]
enrollment10freelunch <- unique(enrollment10freelunch)

enrollment10sped <- enrollment10[enrollment10$type=="Sped",]
enrollment10sped <- enrollment10sped[,sped:=max(total), by=school]
enrollment10sped <- enrollment10sped[,-c("total", "type")]
enrollment10sped <- unique(enrollment10sped)

enrollment10 <- enrollment10all[enrollment10ell, on = "school",]
enrollment10 <- enrollment10[enrollment10sped, on = "school",]
enrollment10 <- enrollment10[enrollment10freelunch, on = "school",]
enrollment10 <- enrollment10[,year:=2010,]
#saveRDS(enrollment10,file="enroll10.RDS")

# 2011
enrollment11 <-read.csv("2011enroll.csv")
enrollment11 <- enrollment11[enrollment11$School.Name != '',]
enrollment11 <- data.table(school=as.character(enrollment11$School.Name), 
                           type=as.character(enrollment11$Sub.Group), total=as.integer(enrollment11$Total))

enrollment11all <- enrollment11[enrollment11$type=="All Students",]
enrollment11all <- enrollment11all[,allstudents:=max(total), by=school]
enrollment11all <- enrollment11all[,-c("total", "type")]
enrollment11all <- unique(enrollment11all)

enrollment11ell <- enrollment11[enrollment11$type=="ELL",]
enrollment11ell <- enrollment11ell[,ell:=max(total), by=school]
enrollment11ell <- enrollment11ell[,-c("total", "type")]
enrollment11ell <- unique(enrollment11ell)

enrollment11freelunch <- enrollment11[enrollment11$type=="Free Lunch",]
enrollment11freelunch <- enrollment11freelunch[,freelunch:=max(total), by=school]
enrollment11freelunch <- enrollment11freelunch[,-c("total", "type")]
enrollment11freelunch <- unique(enrollment11freelunch)

enrollment11sped <- enrollment11[enrollment11$type=="SPED",]
enrollment11sped <- enrollment11sped[,sped:=max(total), by=school]
enrollment11sped <- enrollment11sped[,-c("total", "type")]
enrollment11sped <- unique(enrollment11sped)

enrollment11 <- enrollment11all[enrollment11ell, on = "school",]
enrollment11 <- enrollment11[enrollment11sped, on = "school",]
enrollment11 <- enrollment11[enrollment11freelunch, on = "school",]
enrollment11 <- enrollment11[,year:=2011,]
#saveRDS(enrollment11,file="enroll11.RDS")

# 2012
enrollment12 <-read.csv("2012enroll.csv")
enrollment12 <- enrollment12[enrollment12$School.Name != '',]
enrollment12 <- data.table(school=as.character(enrollment12$School.Name), 
                           type=as.character(enrollment12$Sub.Group), 
                           total=as.integer(enrollment12$Total))

enrollment12all <- enrollment12[enrollment12$type=="total",]
enrollment12all <- enrollment12all[,allstudents:=max(total), by=school]
enrollment12all <- enrollment12all[,-c("total", "type")]
enrollment12all <- unique(enrollment12all)

enrollment12ell <- enrollment12[enrollment12$type=="El",]
enrollment12ell <- enrollment12ell[,ell:=max(total), by=school]
enrollment12ell <- enrollment12ell[,-c("total", "type")]
enrollment12ell <- unique(enrollment12ell)

enrollment12freelunch <- enrollment12[enrollment12$type=="Frl",]
enrollment12freelunch <- enrollment12freelunch[,freelunch:=max(total), by=school]
enrollment12freelunch <- enrollment12freelunch[,-c("total", "type")]
enrollment12freelunch <- unique(enrollment12freelunch)

enrollment12sped <- enrollment12[enrollment12$type=="spedstds",]
enrollment12sped <- enrollment12sped[,sped:=max(total), by=school]
enrollment12sped <- enrollment12sped[,-c("total", "type")]
enrollment12sped <- unique(enrollment12sped)

enrollment12 <- enrollment12all[enrollment12ell, on = "school",]
enrollment12 <- enrollment12[enrollment12sped, on = "school",]
enrollment12 <- enrollment12[enrollment12freelunch, on = "school",]
enrollment12 <- enrollment12[,year:=2012,]

#saveRDS(enrollment12,file="enroll12.RDS")

# 2013
enrollment13 <-read.csv("2013enroll.csv")
enrollment13 <- enrollment13[enrollment13$SchoolEntityName != '',]
enrollment13 <- data.table(school=as.character(enrollment13$SchoolEntityName), 
                           type=as.character(enrollment13$SubGroup), 
                           total=as.integer(enrollment13$Total))

enrollment13all <- enrollment13[enrollment13$type=="All Students",]
enrollment13all <- enrollment13all[,allstudents:=max(total), by=school]
enrollment13all <- enrollment13all[,-c("total", "type")]
enrollment13all <- unique(enrollment13all)

enrollment13ell <- enrollment13[enrollment13$type=="English Language Learners",]
enrollment13ell <- enrollment13ell[,ell:=max(total), by=school]
enrollment13ell <- enrollment13ell[,-c("total", "type")]
enrollment13ell <- unique(enrollment13ell)

enrollment13freelunch <- enrollment13[enrollment13$type=="Free and Reduced Lunch",]
enrollment13freelunch <- enrollment13freelunch[,freelunch:=max(total), by=school]
enrollment13freelunch <- enrollment13freelunch[,-c("total", "type")]
enrollment13freelunch <- unique(enrollment13freelunch)

enrollment13sped <- enrollment13[enrollment13$type=="Special Education",]
enrollment13sped <- enrollment13sped[,sped:=max(total), by=school]
enrollment13sped <- enrollment13sped[,-c("total", "type")]
enrollment13sped <- unique(enrollment13sped)

enrollment13 <- enrollment13all[enrollment13ell, on = "school",]
enrollment13 <- enrollment13[enrollment13sped, on = "school",]
enrollment13 <- enrollment13[enrollment13freelunch, on = "school",]
enrollment13 <- enrollment13[,year:=2013,]

#saveRDS(enrollment13,file="enroll13.RDS")

# 2014
enrollment14 <-read.csv("2014enroll.csv")
enrollment14 <- enrollment14[enrollment14$SchoolEntityName != '',]
enrollment14 <- data.table(school=as.character(enrollment14$SchoolEntityName), 
                           type=as.character(enrollment14$SubGroup), 
                           total=as.integer(enrollment14$Total))

enrollment14all <- enrollment14[enrollment14$type=="All Students",]
enrollment14all <- enrollment14all[,allstudents:=max(total), by=school]
enrollment14all <- enrollment14all[,-c("total", "type")]
enrollment14all <- unique(enrollment14all)

enrollment14ell <- enrollment14[enrollment14$type=="ELL Students",]
enrollment14ell <- enrollment14ell[,ell:=max(total), by=school]
enrollment14ell <- enrollment14ell[,-c("total", "type")]
enrollment14ell <- unique(enrollment14ell)

enrollment14freelunch <- enrollment14[enrollment14$type=="Free Lunch Students",]
enrollment14freelunch <- enrollment14freelunch[,freelunch:=max(total), by=school]
enrollment14freelunch <- enrollment14freelunch[,-c("total", "type")]
enrollment14freelunch <- unique(enrollment14freelunch)

enrollment14sped <- enrollment14[enrollment14$type=="Sped Students",]
enrollment14sped <- enrollment14sped[,sped:=max(total), by=school]
enrollment14sped <- enrollment14sped[,-c("total", "type")]
enrollment14sped <- unique(enrollment14sped)

enrollment14 <- enrollment14all[enrollment14ell, on = "school",]
enrollment14 <- enrollment14[enrollment14sped, on = "school",]
enrollment14 <- enrollment14[enrollment14freelunch, on = "school",]
enrollment14 <- enrollment14[,year:=2014,]

#saveRDS(enrollment14,file="enroll14.RDS")

# 2015
enrollment15 <-read.csv("2015enroll.csv")
enrollment15 <- enrollment15[enrollment15$SchoolEntityName != '',]
enrollment15 <- data.table(school=as.character(enrollment15$SchoolEntityName), 
                           type=as.character(enrollment15$SubGroup), 
                           total=as.integer(enrollment15$Total))

enrollment15all <- enrollment15[enrollment15$type=="All Students",]
enrollment15all <- enrollment15all[,allstudents:=max(total), by=school]
enrollment15all <- enrollment15all[,-c("total", "type")]
enrollment15all <- unique(enrollment15all)

enrollment15ell <- enrollment15[enrollment15$type=="English Language Learners",]
enrollment15ell <- enrollment15ell[,ell:=max(total), by=school]
enrollment15ell <- enrollment15ell[,-c("total", "type")]
enrollment15ell <- unique(enrollment15ell)

enrollment15freelunch <- enrollment15[enrollment15$type=="Free and Reduced Lunch",]
enrollment15freelunch <- enrollment15freelunch[,freelunch:=max(total), by=school]
enrollment15freelunch <- enrollment15freelunch[,-c("total", "type")]
enrollment15freelunch <- unique(enrollment15freelunch)

enrollment15sped <- enrollment15[enrollment15$type=="Students With Disabilities",]
enrollment15sped <- enrollment15sped[,sped:=max(total), by=school]
enrollment15sped <- enrollment15sped[,-c("total", "type")]
enrollment15sped <- unique(enrollment15sped)

enrollment15 <- enrollment15all[enrollment15ell, on = "school",]
enrollment15 <- enrollment15[enrollment15sped, on = "school",]
enrollment15 <- enrollment15[enrollment15freelunch, on = "school",]
enrollment15 <- enrollment15[,year:=2015,]

#saveRDS(enrollment15,file="enroll15.RDS")

### combine all years
enrollment10 <- readRDS("enroll10.RDS")
enrollment11 <- readRDS("enroll11.RDS")
enrollment12 <- readRDS("enroll12.RDS")
enrollment13 <- readRDS("enroll13.RDS")
enrollment14 <- readRDS("enroll14.RDS")
enrollment15 <- readRDS("enroll15.RDS")
fullscores <- readRDS("Fulldataset_2019")

enrollment <- rbind(enrollment10,enrollment11,enrollment12,enrollment13,enrollment14,enrollment15)

fullscores_enroll <- fullscores[enrollment, on = c("school"="school", "year"="year"),]
#saveRDS(fullscores_enroll,file="fullscores_enroll.RDS")
fullscores_enroll <- readRDS("fullscores_enroll.RDS")
