#####################
##script for data cleaning and preparation
####################


rm(list=ls())

# set directory
#setwd("..")

#read in data from the csv file
data<- read.csv(file = 'database_rel_impact.csv', header=TRUE, sep=";" )
# correct first column name
colnames(data)[1]<-c("first_authors")


##recodes variables 
##adds variables such as time between data collection and intervention, dot size for the plots, converts incidence rates and prevalence rates
##into actual case numbers/number of people infected
##codes 'shorter_time' which is a data frame that shows all studies with two interventions within 6 months
##adds mixed cases to vivax and falciparum


#converting dates from characters to dates
data$date_start_incidence<- as.Date(data$date_start_incidence, "%d.%m.%Y")
data$date_end_incidence<- as.Date(data$date_end_incidence, "%d.%m.%Y")
data$date_start_prevalence<- as.Date(data$date_start_prevalence, "%d.%m.%Y")
data$date_end_prevalence<- as.Date(data$date_end_prevalence, "%d.%m.%Y")
data$implementation1_start<- as.Date(data$implementation1_start, "%d.%m.%Y")
data$implementation1_end<- as.Date(data$implementation1_end, "%d.%m.%Y")
data$implementation2_start<- as.Date(data$implementation2_start, "%d.%m.%Y")
data$implementation2_end<- as.Date(data$implementation2_end, "%d.%m.%Y")
data$implementation3_start<- as.Date(data$implementation3_start, "%d.%m.%Y")
data$implementation3_end<- as.Date(data$implementation3_end, "%d.%m.%Y")
data$implementation4_start<- as.Date(data$implementation4_start, "%d.%m.%Y")
data$implementation4_end<- as.Date(data$implementation4_end, "%d.%m.%Y")
data$implementation5_start<- as.Date(data$implementation5_start, "%d.%m.%Y")
data$implementation5_end<- as.Date(data$implementation5_end, "%d.%m.%Y")
data$implementation6_start<- as.Date(data$implementation6_start, "%d.%m.%Y")
data$implementation6_end<- as.Date(data$implementation6_end, "%d.%m.%Y")
data$implementation7_start<- as.Date(data$implementation7_start, "%d.%m.%Y")
data$implementation7_end<- as.Date(data$implementation7_end, "%d.%m.%Y")
data$implementation8_start<- as.Date(data$implementation8_start, "%d.%m.%Y")
data$implementation8_end<- as.Date(data$implementation8_end, "%d.%m.%Y")
data$implementation9_start<- as.Date(data$implementation9_start, "%d.%m.%Y")
data$implementation9_end<- as.Date(data$implementation9_end, "%d.%m.%Y")


#get mid point of implementation time span
data$implementation1<-(data$implementation1_start + ((data$implementation1_end - data$implementation1_start) / 2))
data$implementation2<-(data$implementation2_start + ((data$implementation2_end - data$implementation2_start) / 2))
data$implementation3<-(data$implementation3_start + ((data$implementation3_end - data$implementation3_start) / 2))
data$implementation4<-(data$implementation4_start + ((data$implementation4_end - data$implementation4_start) / 2))
data$implementation5<-(data$implementation5_start + ((data$implementation5_end - data$implementation5_start) / 2))
data$implementation6<-(data$implementation6_start + ((data$implementation6_end - data$implementation6_start) / 2))
data$implementation7<-(data$implementation7_start + ((data$implementation7_end - data$implementation7_start) / 2))
data$implementation8<-(data$implementation8_start + ((data$implementation8_end - data$implementation8_start) / 2))
data$implementation9<-(data$implementation9_start + ((data$implementation9_end - data$implementation9_start) / 2))


#check if any implementation end points are missing
data$study_number[is.na(data$implementation1)&!is.na(data$implementation1_start)]

#mid-point of prevalence and incidence data collection time span
data$date_incidence<-(data$date_start_incidence + ((data$date_end_incidence - data$date_start_incidence) / 2))
data$date_prevalence<-(data$date_start_prevalence + ((data$date_end_prevalence - data$date_start_prevalence) / 2))


#converting characters to factors for: incidence_diagnostics, mode_control and interventions
data$Intervention1<- as.factor(data$Intervention1)
data$Intervention2<- as.factor(data$Intervention2)
data$Intervention3<- as.factor(data$Intervention3)
data$Intervention4<- as.factor(data$Intervention4)
data$Intervention5<- as.factor(data$Intervention5)
data$Intervention6<- as.factor(data$Intervention6)
data$Intervention7<- as.factor(data$Intervention7)
data$Intervention8<- as.factor(data$Intervention8)
data$Intervention9<- as.factor(data$Intervention9)
data$mode_control<- as.factor(data$mode_control)
data$incidence_diagnostics<- as.factor(data$incidence_diagnostics)


#recoding mode_control
data$mode_control[data$mode_control== 'vector and drug'] <- 'drug and vector'
data$mode_control<-droplevels(data$mode_control)
data$mode_control<- as.character(data$mode_control)
data$mode_control<- as.factor(data$mode_control)


#recoding of seasonality with a cut off at 0.6
data$seasonality[data$seasonality== 0.2] <- 'low'
data$seasonality[data$seasonality== 0.4] <- 'low'
data$seasonality[data$seasonality== 0.6] <- 'low'
data$seasonality[data$seasonality== 0.8] <- 'high'
data$seasonality[data$seasonality== 1.0] <- 'high'
data$seasonality[data$seasonality== 1.2] <- 'high'
data$seasonality[data$seasonality== 1.4] <- 'high'
data$seasonality[data$seasonality== 1.6] <- 'high'
data$seasonality[data$seasonality== 1.8] <- 'high'
data$seasonality[data$seasonality== 2.0] <- 'high'
data$seasonality[data$seasonality== ''] <- 'missing'
data$seasonality[is.na(data$seasonality)] <- 'missing'
data$seasonality<- as.factor(data$seasonality)
data$seasonality <- factor(data$seasonality, levels = c("low", "high", "missing"))


#recoding season of survey_incidence
data$season_survey_incidence<-as.character(data$season_survey_incidence)
data$season_survey_incidence[data$season_survey_incidence=='']<-'missing'
data$season_survey_incidence[is.na(data$season_survey_incidence)]<-'missing'
data$season_survey_incidence[data$season_survey_incidence=='non_monsoon']<-'dry'
data$season_survey_incidence[data$season_survey_incidence=='non-monsoon']<-'dry'
data$season_survey_incidence[data$season_survey_incidence=='monsoon']<-'wet'
data$season_survey_incidence[data$season_survey_incidence=='moderate wet']<-'wet'
data$season_survey_incidence[data$season_survey_incidence=='both monson and non-monsoon']<-'both'
data$season_survey_incidence[data$season_survey_incidence=='dry (non-monsoon)']<-'dry'
data$season_survey_incidence[data$season_survey_incidence=='dry and following monsoon']<-'dry'
data$season_survey_incidence[data$season_survey_incidence=='following monsoon']<-'dry'
data$season_survey_incidence<-as.factor(data$season_survey_incidence)
data$season_survey_incidence<-as.character(data$season_survey_incidence)
data$season_survey_incidence<-as.factor(data$season_survey_incidence)

data$season_survey_incidence <- factor(data$season_survey_incidence, levels = c("dry", "wet", "both", "missing"))


#recoding season of survey_prevalence
data$season_survey_prevalence<-as.character(data$season_survey_prevalence)
data$season_survey_prevalence[data$season_survey_prevalence=='']<-'missing'
data$season_survey_prevalence[is.na(data$season_survey_prevalence)]<-'missing'
data$season_survey_prevalence[data$season_survey_prevalence=='dry/cool']<-'dry'
data$season_survey_prevalence[data$season_survey_prevalence=='post-monsoon']<-'dry'
data$season_survey_prevalence[data$season_survey_prevalence=='both monson and non-monsoon']<-'both'
data$season_survey_prevalence[data$season_survey_prevalence=='both monsoon and non-monsoon']<-'both'
data$season_survey_prevalence[data$season_survey_prevalence=='cool season (=dry?)']<-'dry'
data$season_survey_prevalence[data$season_survey_prevalence=='dry (non-monsoon)']<-'dry'
data$season_survey_prevalence[data$season_survey_prevalence=='dry and following monsoon']<-'dry'
data$season_survey_prevalence[data$season_survey_prevalence=='following monsoon']<-'dry'
data$season_survey_prevalence[data$season_survey_prevalence=='non-monsoon']<-'dry'
data$season_survey_prevalence<-as.factor(data$season_survey_prevalence)

data$season_survey_prevalence<-as.character(data$season_survey_prevalence)
data$season_survey_prevalence<-as.factor(data$season_survey_prevalence)

data$season_survey_prevalence <- factor(data$season_survey_prevalence, levels = c("dry", "wet", "both", "missing"))


#recoding diagnostic tool
data$incidence_diagnostics<-as.character(data$incidence_diagnostics)

data$incidence_diagnostics[data$incidence_diagnostics=='']<-'missing'
data$incidence_diagnostics[data$incidence_diagnostics=="doesn't say"]<-'missing'
data$incidence_diagnostics[data$incidence_diagnostics=='microscopy or RDT']<-'RDT or microscopy'
data$incidence_diagnostics[data$incidence_diagnostics=='LM']<-'microscopy'
data$incidence_diagnostics[data$incidence_diagnostics=='missing (assume microscopy)']<-'microscopy'
data$incidence_diagnostics[data$incidence_diagnostics=='RDT positive and then species determination with LM']<-'RDT then microscopy'
data$incidence_diagnostics<-as.factor(data$incidence_diagnostics)

data$incidence_diagnostics<-as.character(data$incidence_diagnostics)
data$incidence_diagnostics<-as.factor(data$incidence_diagnostics)


#recoding of age groups for age_incidence and age_prevalence
#incidence
data$age_incidence[data$age_incidence== 'all ages above 6 months'] <- 'age_all'
data$age_incidence[data$age_incidence== 'all ages'] <- 'age_all'
data$age_incidence[data$age_incidence== '1-3 yo'] <- 'age_5'
data$age_incidence[data$age_incidence== '<5'] <- 'age_5'
data$age_incidence[data$age_incidence== 'below 10'] <- 'age_10'
data$age_incidence[data$age_incidence== 'above 10'] <- 'age_10+'
data$age_incidence[data$age_incidence== ''] <- 'missing'
data$age_incidence<- as.factor(data$age_incidence)

data$age_incidence <- factor(data$age_incidence, levels = c("age_5", "age_10", "age_10+", "age_all", "missing"))


#recoding age groups prevalence
data$age_prevalence<- as.character(data$age_prevalence)
data$age_prevalence[data$age_prevalence== 'all ages above 6 months'] <- 'age_all'
data$age_prevalence[data$age_prevalence== 'all ages above 5 months'] <- 'age_all'
data$age_prevalence[data$age_prevalence== 'all ages'] <- 'age_all'
data$age_prevalence[data$age_prevalence== '4-15 yo'] <- 'age_15'
data$age_prevalence[data$age_prevalence== 'pre-school children'] <- 'age_15'
data$age_prevalence[data$age_prevalence== '1-3 yo'] <- 'age_15'
data$age_prevalence[data$age_prevalence== '1-3yo'] <- 'age_15'
data$age_prevalence[data$age_prevalence== 'under 10'] <- 'age_15'
data$age_prevalence[data$age_prevalence== '0-14'] <- 'age_15'
data$age_prevalence[data$age_prevalence== '1-9 year olds'] <- 'age_15'
data$age_prevalence[data$age_prevalence== '5-10 year olds'] <- 'age_15'
data$age_prevalence[data$age_prevalence== 'below 10'] <- 'age_15'
data$age_prevalence[data$age_prevalence== '5-15 year old school children'] <- 'age_15'
data$age_prevalence[data$age_prevalence== '5-15 years, age adjusted'] <- 'age_15'
data$age_prevalence[data$age_prevalence== 'below 10'] <- 'age_15'
data$age_prevalence[data$age_prevalence== 'age below 10'] <- 'age_15'
data$age_prevalence[data$age_prevalence== 'schoolchildren'] <- 'age_15'
data$age_prevalence[data$age_prevalence== 'above 15'] <- 'age_15+'
data$age_prevalence[data$age_prevalence== 'above 2 year olds'] <- 'age_2+'
data$age_prevalence[data$age_prevalence== 'all ages above 5 year olds'] <- 'age_5+'
data$age_prevalence<- as.factor(data$age_prevalence)


data$age_prevalence <- factor(data$age_prevalence, levels = c("age_2+", "age_5+", "age_15", "age_15+", "age_all"))


#coding time differences to first intervention in months
data$time_incidence_int1 <- data$date_incidence - data$implementation1
data$time_prevalence_int1 <- data$date_prevalence - data$implementation1

data$time_prevalence_int1 <- data$time_prevalence_int1/(365.25/12)
data$time_incidence_int1 <- data$time_incidence_int1/(365.25/12)

#coding time differences to second intervention in months
data$time_incidence_int2 <- data$date_incidence - data$implementation2
data$time_prevalence_int2 <- data$date_prevalence - data$implementation2

data$time_prevalence_int2 <- data$time_prevalence_int2/(365.25/12)
data$time_incidence_int2 <- data$time_incidence_int2/(365.25/12)

#coding time differences to third intervention in months
data$time_incidence_int3 <- data$date_incidence - data$implementation3
data$time_prevalence_int3 <- data$date_prevalence - data$implementation3

data$time_prevalence_int3 <- data$time_prevalence_int3/(365.25/12)
data$time_incidence_int3 <- data$time_incidence_int3/(365.25/12)

#coding time differences to fourth intervention in months
data$time_incidence_int4 <- data$date_incidence - data$implementation4
data$time_prevalence_int4 <- data$date_prevalence - data$implementation4

data$time_prevalence_int4 <- data$time_prevalence_int4/(365.25/12)
data$time_incidence_int4 <- data$time_incidence_int4/(365.25/12)

#coding time differences to fifth intervention in months
data$time_incidence_int5 <- data$date_incidence - data$implementation5
data$time_prevalence_int5 <- data$date_prevalence - data$implementation5

data$time_prevalence_int5 <- data$time_prevalence_int5/(365.25/12)
data$time_incidence_int5 <- data$time_incidence_int5/(365.25/12)

#coding time differences to sixth intervention in months
data$time_incidence_int6 <- data$date_incidence - data$implementation6
data$time_prevalence_int6 <- data$date_prevalence - data$implementation6

data$time_prevalence_int6 <- data$time_prevalence_int6/(365.25/12)
data$time_incidence_int6 <- data$time_incidence_int6/(365.25/12)

#coding time differences to seventh intervention in months
data$time_incidence_int7 <- data$date_incidence - data$implementation7
data$time_prevalence_int7 <- data$date_prevalence - data$implementation7

data$time_prevalence_int7 <- data$time_prevalence_int7/(365.25/12)
data$time_incidence_int7 <- data$time_incidence_int7/(365.25/12)

#coding time differences to eighth intervention in months
data$time_incidence_int8 <- data$date_incidence - data$implementation8
data$time_prevalence_int8 <- data$date_prevalence - data$implementation8

data$time_prevalence_int8 <- data$time_prevalence_int8/(365.25/12)
data$time_incidence_int8 <- data$time_incidence_int8/(365.25/12)

#coding time differences to ninth intervention in months
data$time_incidence_int9 <- data$date_incidence - data$implementation9
data$time_prevalence_int9 <- data$date_prevalence - data$implementation9

data$time_prevalence_int9 <- data$time_prevalence_int9/(365.25/12)
data$time_incidence_int9 <- data$time_incidence_int9/(365.25/12)



#new variable which categorizes the Interventions
data$Sum_Intervention1<- data$Intervention1
data$Sum_Intervention2<- data$Intervention2
data$Sum_Intervention3<- data$Intervention3
data$Sum_Intervention4<- data$Intervention4
data$Sum_Intervention5<- data$Intervention5
data$Sum_Intervention6<- data$Intervention6
data$Sum_Intervention7<- data$Intervention7
data$Sum_Intervention8<- data$Intervention8
data$Sum_Intervention9<- data$Intervention9

#summarize the new variables Intervention 1
data$Sum_Intervention1<- as.character(data$Sum_Intervention1)
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN 1st round'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN 2nd round'] <- 'LLIN later dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN 3rd round'] <- 'LLIN later dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN (5-yearly)'] <- 'LLIN'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN (repeated probably cause 65% had at least one per hosuehold already)'] <- 'LLIN later dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN (not first distribution, 1 net per individual)'] <- 'LLIN later dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'ITN (permethrin) (80% used nets before, there were distributions before, probably untreated)'] <- 'LLIN later dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'ITN (permethrin treated, first time, or not mentioned if happened before)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'ITNs (first time deltamethrin)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'treated nets (alphacypermethrin) (no mention of distribution before)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== "introduction benets/Permethrin-net re impregnation (first time or don't mentioned it happened before)"] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'untreated nets (76% used nets before, there were dirtsibutions before)'] <- 'untreated nets'
data$Sum_Intervention1[data$Sum_Intervention1== 'untreated nets (no mention of any distribution before)'] <- 'untreated nets'
data$Sum_Intervention1[data$Sum_Intervention1== 'untreated nets, probably first dist'] <- 'untreated nets'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN (used nets before (11%), not sure which round)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN (had ITNs before but first state wide distribution, before could get ITN distributed from PHC)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN 2nd round(one was done 6 years previous)'] <- 'LLIN later dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN distribution (first)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'ITN (does not speak of a round of distribution before, but not sure if they had nets before)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'ITN (permethrin or lambdacyhalothrin), offering of reatreatment in following years, probably first dist'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'ITN (permethrin or lambdacyhalothrin), offering of reatreatment in following years, probably first distribution'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'ITN (some already distributde before but only low coverage, so first time?)'] <- 'LLIN 1st dist'   #this is the chaves one in vanuatu
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN distribution (not sure which round, had nets before, no mention of previous distributipon but might have happened)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN (seemed to hae nets before and distributed them, not sure how extensivelxy)'] <- 'LLIN'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN distribution (probably first round)'] <- 'LLIN 1st dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'later time LLIN'] <- 'LLIN later dist'
data$Sum_Intervention1[data$Sum_Intervention1== 'LLIN (none before mentioned)'] <- 'LLIN 1st dist'

data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (restarted with deltamethrin)'] <- 'IRS 1st time'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS biannualy with DDT (continued IRS) until 78 then irregular'] <- 'IRS 1st time'
data$Sum_Intervention1[data$Sum_Intervention1== 'first time IRS (twice a year DDT)'] <- 'IRS 1st time'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (some areas received before otherwise first, 2.3 spraying per year avergae, DDT)'] <- 'IRS 1st time'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (some areas received before otherwise first, 2.0 spraying per year avergae, DDT)'] <- 'IRS 1st time'
data$Sum_Intervention1[data$Sum_Intervention1== 'spraying (alphacypermethrin suspeincion concentrate), spraying happened in pakistan before but doesnt say anything about this area'] <- 'IRS'
data$Sum_Intervention1[data$Sum_Intervention1== 'spraying (alphacypermethrin wettable powder),  spraying happened in pakistan before but doesnt say anything about this area'] <- 'IRS'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (lambdacyhalothrin, malathion was used before, so repeated)'] <- 'IRS repeated time'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (malathion, repeated spraying)'] <- 'IRS repeated time'
data$Sum_Intervention1[data$Sum_Intervention1== 'spraying with lambda-cyhalotrin 10% WP (first time with lambda-cyhalotrin but DDT before)'] <- 'IRS repeated time'
data$Sum_Intervention1[data$Sum_Intervention1== 'spraying (malathion or lambdacyhalothrin) (probably first time, but very unsure)'] <- 'IRS'
data$Sum_Intervention1[data$Sum_Intervention1== 'DDT spraying (no spraying in previous year, one spraiyng in feb and one in sept 87, had spraying at some point before)'] <- 'IRS 1st time'
data$Sum_Intervention1[data$Sum_Intervention1== "DDT spraying two rounds one aug/sep the second nov/dec, dont know if any spraying happened the year before, if it did, then not mentioned, but 'control' villages were also sprayed by state health authorities, so could have happened before)"] <- 'IRS'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (as spraying done by government could be that not first time, one in july 13 and sept 13)'] <- 'IRS'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (none in year before so first?, sep 14, july 15, july 16)'] <- 'IRS 1st time'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS (nov 13, apr 14, april 15, august 15) not clear if there was sprayin before or not)'] <- 'IRS'
data$Sum_Intervention1[data$Sum_Intervention1== 'IRS with DDT (yearly, june 09 with DDT and july 10 with deltamethrin, there was some spraying nationally but doesnt say if in this area or not. So we dont know if first time)'] <- 'IRS'
data$Sum_Intervention1[data$Sum_Intervention1== 'in 87 DDT two rounds, probably not first time and previous spraying the year before?, then in 88 one spraying with ICON)'] <- 'IRS'

data$Sum_Intervention1[data$Sum_Intervention1== 'AL(over 3 days) and CQ (over 3 days 25mg/kg total dose) and PQ 20 days (no mention of it being done before)'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'AL(over 3 days) and CQ (over 3 days 25mg/kg total dose) and placebo (no mention of it being done before)'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA (chloroquine and primaquine, three day course (subtherapeutic dose of primaquine for exo-erythorcyte stages of Pv), adults 600 mg,450,450 of chloroquine, and 15 mg of primaquine each day), did MDA in 1967 before but not since 1969'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA(chloroquine, primaquine, pyrimethamine, sulfadoxine,) once a week for nine weeks, week 1,5,9: 45mg primaquine, 600mg chloroquine, 1500mg sulfadoxine and 3 tablets of 75mg pyrimethamine, other weeks only45 mg primaquine and 300 mg chloroquine, no mention of any MDA before this'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'treatment (chloroquine and sulfadoxine/pyrimethamine) (first time)'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA (dihydroartemisinin (7mg/kg) and piperaquine tetraphosphate (55mg/kg) every month three days for three months, probably first time)'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== "MDA (might be first time, doesn't really say anything, doesn't say what drug used)"] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA (three rounds,daily dosage adults for 5 days: plasmochine (30mg) and quinine sulphate (900mg), twice daily, 3x every three weeks), did a study the year before not sure if at same scale. Then would be repeated MDA)'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA every four weeks, elevn treatment rounds, chloroquine, if above 45 kg then a dose of 450mg (also diethylcarbamazine for filariasis), measured after 10 rounds (nothing like this before mentioned\342\200\246 but kinda over a long time? So repeated or not?)'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'three monthly rounds MDA (3 days of dihydroartemisinin-piperaquine 7.5/60mg/kg) only people above 14 years'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA (3 times a month apart, dihydroartemisnin (7mg per kg) and piperaquine (55mg per kg), 3 days once per day, probably first time apart from maybe pilot'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA 600 mg chloroquine (and 45mg primaquine where Pf is predominant), probably first time'] <- 'MDA 1st'
data$Sum_Intervention1[data$Sum_Intervention1== 'MDA (low dose primaquine, dihydroartemisinin-piperaquine, three rounds, 1 monthly, no mention of any MDA done before'] <- 'MDA 1st'


data$Sum_Intervention1[data$Sum_Intervention1== 'more screening (weekly screening)'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'test and treat'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'community health workers that diagnosed and treated and also could give LLINs'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'village malaria workers scale up'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'first mass screening and treating (DHP and primaquine for Pf and primaquine over 14 days for Pv)'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'early diagnosis and access to treatment via malaria posts whcih also provided LLIN to all'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'increased case management (community agent that detects and treats, passive case detection though, but before needed to go 25km to next village)'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'aggressive active case detection: monthly testing and treating of around 60-90% of population, by 2000 15 trained in 2001 39)'] <- 'increased case management'
data$Sum_Intervention1[data$Sum_Intervention1== 'increased case management: health posts with health extension workers, village level'] <- 'increased case management'

data$Sum_Intervention1[data$Sum_Intervention1== 'control to ITN (permethrin treated)'] <- 'control'
data$Sum_Intervention1[data$Sum_Intervention1== 'control to ITNs (first time deltamethrin)'] <- 'control'
data$Sum_Intervention1[data$Sum_Intervention1== 'control to nets'] <- 'control'
data$Sum_Intervention1[data$Sum_Intervention1== 'control to spraying'] <- 'control'
data$Sum_Intervention1[data$Sum_Intervention1== 'none'] <- 'control'

data$Sum_Intervention1<- as.factor(data$Sum_Intervention1)
data$Sum_Intervention1<- as.character(data$Sum_Intervention1)
data$Sum_Intervention1<- as.factor(data$Sum_Intervention1)

#summarize intervention2
data$Sum_Intervention2<- as.character(data$Sum_Intervention2)
data$Sum_Intervention2[data$Sum_Intervention2== 'LLIN 2nd round'] <- 'LLIN later dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'LLIN 3rd round'] <- 'LLIN later dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'LLIN (replacing older ones)'] <- 'LLIN later dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'ITN (retreated yearly) no mention of any ITN provdied before this time'] <- 'LLIN 1st dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'LLIN distribution (first)'] <- 'LLIN 1st dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'LLIN 2nd distribution'] <- 'LLIN later dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'LLIN 2nd dist'] <- 'LLIN later dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'second ITN distribution'] <- 'LLIN later dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'ITN distribution (not first time)'] <- 'LLIN later dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'ITN scale up (no big scale up before, more gradual)'] <- 'LLIN 1st dist'
data$Sum_Intervention2[data$Sum_Intervention2== 'permethrin-impregnated nets (many owned nets before, not sure when and how they got them)'] <- 'LLIN'
data$Sum_Intervention2[data$Sum_Intervention2== 'LLIN (not known which round)'] <- 'LLIN'
data$Sum_Intervention2[data$Sum_Intervention2== 'later time LLIN'] <- 'LLIN later dist'


data$Sum_Intervention2[data$Sum_Intervention2== 'retreatment'] <- 'net retreatment'
data$Sum_Intervention2[data$Sum_Intervention2== 'Permethrin-net re impregnation'] <- 'net retreatment'
data$Sum_Intervention2[data$Sum_Intervention2== 'first retreatment'] <- 'net retreatment'



data$Sum_Intervention2[data$Sum_Intervention2== 'IRS (on a larger scale)'] <- 'IRS repeated time'
data$Sum_Intervention2[data$Sum_Intervention2== 'IRS (itensification, also doesnt specify what was before and what this intensification exactly was, spraying with cypermethrin)'] <- 'IRS repeated time'
data$Sum_Intervention2[data$Sum_Intervention2== 'IRS (none in year before so first?, sep 14, july 15, july 16)'] <- 'IRS 1st time'
data$Sum_Intervention2[data$Sum_Intervention2== 'IRS (pyrtehroid, repeated, before they used DDT, so there was spraying the year before)'] <- 'IRS repeated time'

data$Sum_Intervention2[data$Sum_Intervention2== 'test and treat'] <- 'increased case management'
data$Sum_Intervention2[data$Sum_Intervention2== 'increased case management: health posts with health extension workers, village level'] <- 'increased case management'
data$Sum_Intervention2[data$Sum_Intervention2== 'second mass screening and treating (DHP and primaquine for Pf and primaquine over 14 days for Pv)'] <- 'increased case management'

data$Sum_Intervention2[data$Sum_Intervention2== 'MDA (low dose primaquine, dihydroartemisinin-piperaquine, three rounds, 1 monthly, no mention of any MDA done before'] <- 'MDA 1st'
data$Sum_Intervention2[data$Sum_Intervention2== 'MDA chloeoquine and pyrimethamine, whenever spraying happened, for three years (68-70), pyrimethamine and chloroquine, first time)'] <- 'MDA 1st'
data$Sum_Intervention2[data$Sum_Intervention2== 'second of three monthly MDA (3 days of dihydroartemisinin-piperaquine 7.5/60mg/kg) only people above 14 years'] <- 'MDA repeated'
data$Sum_Intervention2[data$Sum_Intervention2== 'MDA (450 mgchloroquine,  50 mg pyrimethamine, 4 doses in 1958)'] <- 'MDA repeated'    #considered repeatd because measurement after last and dont know how much space there was inbetween
data$Sum_Intervention2[data$Sum_Intervention2== 'MDA 600 mg chloroquine (and 45mg primaquine where Pf is predominant), repeated time'] <- 'MDA repeated'

data$Sum_Intervention2[data$Sum_Intervention2== 'control to starting of anual retreating'] <- 'control'
data$Sum_Intervention2[data$Sum_Intervention2== 'none'] <- 'control'
data$Sum_Intervention2[data$Sum_Intervention2== ''] <- 'none'

data$Sum_Intervention2<- as.character(data$Sum_Intervention2)
data$Sum_Intervention2<- as.factor(data$Sum_Intervention2)


#summarize intervention3
data$Sum_Intervention3<- as.character(data$Sum_Intervention3)
data$Sum_Intervention3[data$Sum_Intervention3== 'LLIN 3rd round'] <- 'LLIN later dist'
data$Sum_Intervention3[data$Sum_Intervention3== 'LLIN 2nd round(replacing of old nets)'] <- 'LLIN later dist'
data$Sum_Intervention3[data$Sum_Intervention3== 'second retreatment'] <- 'net retreatment'

data$Sum_Intervention3[data$Sum_Intervention3== 'third of three monthly MDA (3 days of dihydroartemisinin-piperaquine 7.5/60mg/kg) only people above 14 years'] <- 'MDA repeated'
data$Sum_Intervention3[data$Sum_Intervention3== 'MDA (adults: 450mg chloroquine,50 mg pyrimethamine, 2 doses in 1959)'] <- 'MDA repeated'

data$Sum_Intervention3[data$Sum_Intervention3== 'none'] <- 'control'
data$Sum_Intervention3[data$Sum_Intervention3== 'control to retreat'] <- 'control'
data$Sum_Intervention3[data$Sum_Intervention3== ''] <- 'none'
data$Sum_Intervention3[data$Sum_Intervention3== 'window screening'] <- 'none'

data$Sum_Intervention3<- as.character(data$Sum_Intervention3)
data$Sum_Intervention3<- as.factor(data$Sum_Intervention3)

#summarize intervention4
data$Sum_Intervention4<- as.character(data$Sum_Intervention4)

data$Sum_Intervention4[data$Sum_Intervention4== 'control to retreatment'] <- 'control'
data$Sum_Intervention4[data$Sum_Intervention4== 'third retreatment'] <- 'net retreatment'
data$Sum_Intervention4[data$Sum_Intervention4== ''] <- 'none'

data$Sum_Intervention4<- as.character(data$Sum_Intervention4)
data$Sum_Intervention4<- as.factor(data$Sum_Intervention4)


#baseline intervention, put into category yes/no (everything yes unless if empty field or none/nothing)
data$baseline_intervention_YN<- 'yes'
data$baseline_intervention_YN[data$baselin_intervention== '']<- 'unknown'
data$baseline_intervention_YN[data$baselin_intervention== 'nothing']<- 'no'
data$baseline_intervention_YN[data$baselin_intervention== 'none']<- 'no'

#making time numeric
data$time_incidence_int1<-as.numeric(data$time_incidence_int1)
data$time_incidence_int2<-as.numeric(data$time_incidence_int2)
data$time_incidence_int3<-as.numeric(data$time_incidence_int3)
data$time_incidence_int4<-as.numeric(data$time_incidence_int4)
data$time_incidence_int5<-as.numeric(data$time_incidence_int5)
data$time_incidence_int6<-as.numeric(data$time_incidence_int6)
data$time_incidence_int7<-as.numeric(data$time_incidence_int7)
data$time_incidence_int8<-as.numeric(data$time_incidence_int8)
data$time_incidence_int9<-as.numeric(data$time_incidence_int9)

data$time_prevalence_int1<-as.numeric(data$time_prevalence_int1)
data$time_prevalence_int2<-as.numeric(data$time_prevalence_int2)
data$time_prevalence_int3<-as.numeric(data$time_prevalence_int3)
data$time_prevalence_int4<-as.numeric(data$time_prevalence_int4)
data$time_prevalence_int5<-as.numeric(data$time_prevalence_int5)
data$time_prevalence_int6<-as.numeric(data$time_prevalence_int6)
data$time_prevalence_int7<-as.numeric(data$time_prevalence_int7)
data$time_prevalence_int8<-as.numeric(data$time_prevalence_int8)
data$time_prevalence_int9<-as.numeric(data$time_prevalence_int9)


#codes study type (trial or observational)
data$study_type<-as.factor(data$study_type)
data$study_type<-as.character(data$study_type)
data$study_type[data$study_type== 'RCT (pilot)'] <- 'trial'
data$study_type[data$study_type== 'RCT'] <- 'trial'
data$study_type[data$study_type== 'trial but not randomized'] <- 'trial'
data$study_type[data$study_type== 'RCT in 1996, then follow up more observational'] <- 'trial'

data$study_type[data$study_type== 'RCT but couldnt use control because doesnt give prevalence, so before and after'] <- 'observational'
data$study_type[data$study_type== 'observational (before after)'] <- 'observational'
data$study_type[data$study_type== 'observational, before and after'] <- 'observational'
data$study_type[data$study_type== 'bservational, cross sectional'] <- 'observational'
data$study_type[data$study_type== 'observational: longitudinal'] <- 'observational'
data$study_type[data$study_type== 'RCT, but here used before and after as control is untreated nets and not no nets'] <- 'observational'
data$study_type[data$study_type== 'before-after'] <- 'observational'
data$study_type[data$study_type== 'longitudinal'] <- 'observational'
data$study_type[data$study_type== 'longitudinal??'] <- 'observational'
data$study_type[data$study_type== 'observational before after'] <- 'observational'
data$study_type[data$study_type== 'observational, cross sectional'] <- 'observational'
data$study_type[data$study_type== 'uncontrolled before and after study'] <- 'observational'
data$study_type[data$study_type== 'uncotrolled before and after'] <- 'observational'
data$study_type[data$study_type== 'RCT (but here only control used because compare LLIN and LLIN and repellent)'] <- 'observational'
data$study_type[data$study_type== 'trial but we look at control'] <- 'observational'
data$study_type[data$study_type== 'trial: no control though because compared one regimen with primaquine and one without'] <- 'observational'

data$study_type[data$study_type== 'RCT, but this is control group that received intervention later'] <- 'observational'
data$study_type[data$study_type== 'RCT (pilot), but took before time points'] <- 'observational'
data$study_type[data$study_type== 'trial but control group with untreated nets'] <- 'observational'



data$study_type<-as.factor(data$study_type)

#codes relapse zones from battle et al. 
data$relapse_pattern_zone<-as.character(data$relapse_pattern_zone)
data$relapse_pattern_zone[data$relapse_pattern_zone=='3']<-'South America'
data$relapse_pattern_zone[data$relapse_pattern_zone=='2']<-'Central America'
data$relapse_pattern_zone[data$relapse_pattern_zone=='12']<-'Melanesia'
data$relapse_pattern_zone[data$relapse_pattern_zone=='7']<-'Sub Saharan Africa'
data$relapse_pattern_zone[data$relapse_pattern_zone=='10']<-'South East Asia'
data$relapse_pattern_zone[data$relapse_pattern_zone=='8']<-'Monsoon Asia'
data$relapse_pattern_zone[data$relapse_pattern_zone=='11']<-'Northern Europe and Asia'
data$relapse_pattern_zone[data$relapse_pattern_zone=='5']<-'Mediterranean and North Africa'
data$relapse_pattern_zone<-as.factor(data$relapse_pattern_zone)



#relapse pattern from white
data$relapse_pattern_white<-as.factor(data$relapse_pattern_white)


#recoding sample size incidence: is the number of people that could get ill:
data$sample_size_incidence_number_people<-as.factor(data$sample_size_incidence_number_people)
data$sample_size_incidence_number_people<-as.character(data$sample_size_incidence_number_people)
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '103799 people at risk'] <- '103799'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '106882 people at risk'] <- '106882'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '109827 people at risk'] <- '109827'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '112755 people at risk'] <- '112755'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '120457 pop mimika around apr 2004-mar 2006'] <- '120457'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '143723 pop mimika around apr2006-mar2008'] <- '143723'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '148124 pop mimika around apr 2008-dec 2009'] <- '148124'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '189447pop mimika around  jan 2010-dec 2013'] <- '189447'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '31292 (population in district in 2009)'] <- '31292'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== '96496 people at risk'] <- '96496'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'ariound 150000 people at risk'] <- '150000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'a it below 130000 people at risk'] <- ''
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 130000 people at risk'] <- '130000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 135000 people at risk'] <- '135000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 140000 people at risk'] <- '140000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 145000 people at risk'] <- '145000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 155000 people at risk'] <- '155000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 160000 people at risk'] <- '160000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 165000 people at risk'] <- '165000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 170000 people at risk'] <- '170000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 175000 people at risk'] <- '175000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 180000 people at risk'] <- '180000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'around 50000 residents'] <- '50000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'because national: whople population: 2.7 million people'] <- '2700000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'between 120000 and 130000 people at risk'] <- '125000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'between 297 and 366 people'] <- '332'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'between 710 and 928 people'] <- '819'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'missing, say something abot how many people in subdiustrict but dont know what that means'] <- ''
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'no exact numbers: in 86/87 around 3600 people, a lot of migration, porbably had less people before that'] <- ''
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'none given, but at least 5000 catchement population'] <- '5000'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'not clear, this is a mixture of all the hotspots that got MDA, there are different follow up times. So the closer to time point zero the bigger the population size and at the end tehre will be less because not all have such a long follow up. In total 12465 people in hotspot village'] <- '12465'
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'not know for the individual PHCs in total would be 51325'] <- ''
data$sample_size_incidence_number_people[data$sample_size_incidence_number_people== 'missing'] <- ''

data$sample_size_incidence_number_people<-as.factor(data$sample_size_incidence_number_people)
is.na(data$sample_size_incidence_number_people) <- data$sample_size_incidence_number_people == ''
data$sample_size_incidence_number_people<-strtoi(data$sample_size_incidence_number_people)



#######
##add mixed cases to both vivax as well as falciparum
#######

#for prevalence
data$mixed_what_done<-as.factor(data$mixed_what_done)
#levels(data$mixed_what_done)

x<-c(1:length(data$master_study_number))
for (val in x)
{if (!is.na(data$pr_numb_vivax_LM[val])) {(data$pr_numb_vivax_LM[val] <- rowSums(data[val,c("pr_numb_vivax_LM", "pr_numb_mixed_LM")], na.rm=TRUE))}}
for (val in x)
{if (!is.na(data$pr_numb_falciparum_LM[val])) {(data$pr_numb_falciparum_LM[val] <- rowSums(data[val,c("pr_numb_falciparum_LM", "pr_numb_mixed_LM")], na.rm=TRUE))}}

for (val in x)
{if (!is.na(data$pr_numb_vivax_PCR[val])) {(data$pr_numb_vivax_PCR[val] <- rowSums(data[val,c("pr_numb_vivax_PCR", "pr_numb_mixed_PCR")], na.rm=TRUE))}}
for (val in x)
{if (!is.na(data$pr_numb_falciparum_PCR[val])) {(data$pr_numb_falciparum_PCR[val] <- rowSums(data[val,c("pr_numb_falciparum_PCR", "pr_numb_mixed_PCR")], na.rm=TRUE))}}


for (val in x)
{if (!is.na(data$prevalence_vivax_LM[val])) {(data$prevalence_vivax_LM[val] <- rowSums(data[val,c("prevalence_vivax_LM", "prevalence_mixed_LM")], na.rm=TRUE))}}
for (val in x)
{if (!is.na(data$prevalence_falciparum_LM[val])) {(data$prevalence_falciparum_LM[val] <- rowSums(data[val,c("prevalence_falciparum_LM", "prevalence_mixed_LM")], na.rm=TRUE))}}

for (val in x)
{if (!is.na(data$prevalence_vivax_PCR[val])) {(data$prevalence_vivax_PCR[val] <- rowSums(data[val,c("prevalence_vivax_PCR", "prevalence_mixed_PCR")], na.rm=TRUE))}}
for (val in x)
{if (!is.na(data$prevalence_falciparum_PCR[val])) {(data$prevalence_falciparum_PCR[val] <- rowSums(data[val,c("prevalence_falciparum_PCR", "prevalence_mixed_PCR")], na.rm=TRUE))}}

for (val in x)
{if (!is.na(data$prevalence_vivax_RDT[val])) {(data$prevalence_vivax_RDT[val] <- rowSums(data[val,c("prevalence_vivax_RDT", "prevalence_mixed_RDT")], na.rm=TRUE))}}
for (val in x)
{if (!is.na(data$prevalence_falciparum_RDT[val])) {(data$prevalence_falciparum_RDT[val] <- rowSums(data[val,c("prevalence_falciparum_RDT", "prevalence_mixed_RDT")], na.rm=TRUE))}}


#for incidence
data$pr_what_done_mixed<-as.factor(data$pr_what_done_mixed)
#levels(data$pr_what_done_mixed)

x<-c(1:length(data$master_study_number))
for (val in x)
{if (!is.na(data$incidence_rate_vivax[val])) {(data$incidence_rate_vivax[val] <- rowSums(data[val,c("incidence_rate_vivax", "incidence_rate_mixed")], na.rm=TRUE))}}
for (val in x)
{if (!is.na(data$incidence_rate_falciparum[val])) {(data$incidence_rate_falciparum[val] <- rowSums(data[val,c("incidence_rate_falciparum", "incidence_rate_mixed")], na.rm=TRUE))}}

for (val in x)
{if (!is.na(data$case_numbers_vivax[val])) {(data$case_numbers_vivax[val] <- rowSums(data[val,c("case_numbers_vivax", "case_numbers_mixed")], na.rm=TRUE))}}
for (val in x)
{if (!is.na(data$case_numbers_falciparum[val])) {(data$case_numbers_falciparum[val] <- rowSums(data[val,c("case_numbers_falciparum", "case_numbers_mixed")], na.rm=TRUE))}}



#remove the kessler because no vivax cases found in that time
#find which row numbers
data$row_number<-seq.int(nrow(data))
b<-data$row_number[data$study_number==48]
data<-data[-c(b),]
data$row_number<-seq.int(nrow(data))



#convert incidence rates in actual case numbers
data$unit_incidence_rate<-as.character(data$unit_incidence_rate)
data$inc_converted_vivax<-data$incidence_rate_vivax
data$inc_converted_vivax<-NA



x<-c(1:length(data$master_study_number))
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 people per month'){data$inc_converted_vivax[val]<-data$incidence_rate_vivax[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 child-months'){data$inc_converted_vivax[val]<-data$incidence_rate_vivax[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 person months'){data$inc_converted_vivax[val]<-data$incidence_rate_vivax[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 person years!'){data$inc_converted_vivax[val]<-data$incidence_rate_vivax[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/365/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per person per year'){data$inc_converted_vivax[val]<-data$incidence_rate_vivax[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/365)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='episodes/child/year-at-risk'){data$inc_converted_vivax[val]<-data$incidence_rate_vivax[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/365)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 per month'){data$inc_converted_vivax[val]<-data$incidence_rate_vivax[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}


#for falciparum
data$unit_incidence_rate<-as.character(data$unit_incidence_rate)
data$inc_converted_falc<-data$incidence_rate_falciparum
data$inc_converted_falc<-NA


x<-c(1:length(data$master_study_number))
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 people per month'){data$inc_converted_falc[val]<-data$incidence_rate_falciparum[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 child-months'){data$inc_converted_falc[val]<-data$incidence_rate_falciparum[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 person months'){data$inc_converted_falc[val]<-data$incidence_rate_falciparum[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 person years!'){data$inc_converted_falc[val]<-data$incidence_rate_falciparum[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/365/1000)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per person per year'){data$inc_converted_falc[val]<-data$incidence_rate_falciparum[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/365)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='episodes/child/year-at-risk'){data$inc_converted_falc[val]<-data$incidence_rate_falciparum[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/365)}}
for (val in x){
  if(data$unit_incidence_rate[val]=='cases per 1000 per month'){data$inc_converted_falc[val]<-data$incidence_rate_falciparum[val]*data$sample_size_incidence_number_people[val]*(as.numeric(data$date_end_incidence[val]-data$date_start_incidence[val])/30/1000)}}





#do the same for prevalence: convert prevalence rates in actual number of infections found 
#for falciparum
data$prev_converted_falc<-data$prevalence_falciparum_PCR
data$prev_converted_falc<-NA

x<-c(1:length(data$master_study_number))
for (val in x){
  data$prev_converted_falc[val]<-data$prevalence_falciparum_LM[val]/100*data$sample_size_prevalence[val]}
for (val in x){
  if(is.na(data$prev_converted_falc[val])){data$prev_converted_falc[val]<-data$prevalence_falciparum_PCR[val]/100*data$sample_size_prevalence[val]}}
for (val in x){
  if(is.na(data$prev_converted_falc[val])){data$prev_converted_falc[val]<-data$prevalence_falciparum_RDT[val]/100*data$sample_size_prevalence[val]}}


#also for prevalence make number of infection found
#for vivax
data$prev_converted_viv<-data$prevalence_vivax_PCR
data$prev_converted_viv<-NA

x<-c(1:length(data$master_study_number))
for (val in x){
  if(is.na(data$prev_converted_viv[val])){data$prev_converted_viv[val]<-data$prevalence_vivax_RDT[val]/100*data$sample_size_prevalence[val]}}
for (val in x){
  if(is.na(data$prev_converted_viv[val])){data$prev_converted_viv[val]<-data$prevalence_vivax_PCR[val]/100*data$sample_size_prevalence[val]}}
for (val in x){
  if(is.na(data$prev_converted_viv[val])){data$prev_converted_viv[val]<-data$prevalence_vivax_LM[val]/100*data$sample_size_prevalence[val]}}




#code categories for case numbers--> which then gives the size of the dots for the plots

data$row_number<-seq.int(nrow(data))

#vivax_new_inc and falciparum_new_inc are variables for the actual case numbers found in incidence
data$vivax_new_inc<-data$case_numbers_vivax
data$falciparum_new_inc<-data$case_numbers_falciparum
x<-c(1:length(data$first_authors))

for (val in x){
  if(is.na(data$vivax_new_inc[val])){data$vivax_new_inc[val]<-data$inc_converted_vivax[val]}}
x<-c(1:length(data$first_authors))
for (val in x){
  if(is.na(data$falciparum_new_inc[val])){data$falciparum_new_inc[val]<-data$inc_converted_falc[val]}}


data$case_numbers_total_inc<-data$vivax_new_inc+data$falciparum_new_inc
data$case_numbers_total_cat_inc<-data$case_numbers_total_inc
data$case_numbers_total_cat_inc<-NA

x<-c(1:length(data$first_authors))
for (val in x){
  if(!is.na(data$case_numbers_total_inc[val])&data$case_numbers_total_inc[val]<200){data$case_numbers_total_cat_inc[val]<-0.75}
  if(!is.na(data$case_numbers_total_inc[val])&data$case_numbers_total_inc[val]>=200){data$case_numbers_total_cat_inc[val]<-1.0}
  if(!is.na(data$case_numbers_total_inc[val])&data$case_numbers_total_inc[val]>=500){data$case_numbers_total_cat_inc[val]<-1.25}
  if(!is.na(data$case_numbers_total_inc[val])&data$case_numbers_total_inc[val]>=1000){data$case_numbers_total_cat_inc[val]<-1.5}
  if(!is.na(data$case_numbers_total_inc[val])&data$case_numbers_total_inc[val]>=2000){data$case_numbers_total_cat_inc[val]<-1.75}
 }




#code for getting the dot size for prevalence data

data$row_number<-seq.int(nrow(data))

data$vivax_new_prev<-data$pr_numb_vivax_LM
data$falciparum_new_prev<-data$pr_numb_falciparum_LM
x<-c(1:length(data$Intervention1))
for (val in x){
  if(is.na(data$vivax_new_prev[val])){data$vivax_new_prev[val]<-data$pr_numb_vivax_PCR[val]}}
for (val in x){
  if(is.na(data$falciparum_new_prev[val])){data$falciparum_new_prev[val]<-data$pr_numb_falciparum_PCR[val]}}
x<-c(1:length(data$Intervention1))
for (val in x){
  if(is.na(data$vivax_new_prev[val])){data$vivax_new_prev[val]<-data$prev_converted_viv[val]}}
for (val in x){
  if(is.na(data$falciparum_new_prev[val])){data$falciparum_new_prev[val]<-data$prev_converted_falc[val]}}


data$case_numbers_total_prev<-data$vivax_new_prev+data$falciparum_new_prev
data$case_numbers_total_cat_prev<-data$case_numbers_total_prev
data$case_numbers_total_cat_prev<-NA

x<-c(1:length(data$first_authors))
for (val in x){
  if(!is.na(data$case_numbers_total_prev[val])&data$case_numbers_total_prev[val]<100){data$case_numbers_total_cat_prev[val]<-0.75}
  if(!is.na(data$case_numbers_total_prev[val])&data$case_numbers_total_prev[val]>=100){data$case_numbers_total_cat_prev[val]<-1.0}
  if(!is.na(data$case_numbers_total_prev[val])&data$case_numbers_total_prev[val]>=200){data$case_numbers_total_cat_prev[val]<-1.25}
  if(!is.na(data$case_numbers_total_prev[val])&data$case_numbers_total_prev[val]>=500){data$case_numbers_total_cat_prev[val]<-1.5}
  if(!is.na(data$case_numbers_total_prev[val])&data$case_numbers_total_prev[val]>=1000){data$case_numbers_total_cat_prev[val]<-1.75}
}


#what was done with mixed cases (incidence), recoding
data$mixed_what_done<-as.character(data$mixed_what_done)
data$mixed_what_done[data$mixed_what_done=='counted separetly']<-'given separately'
data$mixed_what_done[data$mixed_what_done=='counted seperately']<-'given separately'
data$mixed_what_done[data$mixed_what_done=='reported seperately']<-'given separately'
data$mixed_what_done[data$mixed_what_done=="doesn't clearly say"]<-'not reported'
data$mixed_what_done[data$mixed_what_done=="doesn't say"]<-'not reported'
data$mixed_what_done[data$mixed_what_done=="doesn't say, give mixed but could also be with Pm"]<-'mixed given but could be Pm too, hence not added'
data$mixed_what_done[data$mixed_what_done=="gives mixed but could also be Pm although unlikely because only few Pm cases"]<-'mixed given, could be with Pm but because few cases counted towards both Pv and Pf'
data$mixed_what_done[data$mixed_what_done=="mixed are counted towards falciparum"]<-'counted towards falciparum'
data$mixed_what_done[data$mixed_what_done=="not sure, not seperate, probably counted towards falciparum, but only few mixed cases"]<-'probably counted towards falciparum'
data$mixed_what_done<-as.factor(data$mixed_what_done)


#what was done with mixed cases (prevalence), recoding
data$pr_what_done_mixed<-as.character(data$pr_what_done_mixed)
data$pr_what_done_mixed[data$pr_what_done_mixed=='counted separately']<-'given separately'
data$pr_what_done_mixed[data$pr_what_done_mixed=='counted seperately']<-'given separately'
data$pr_what_done_mixed[data$pr_what_done_mixed=='counted towards both']<-'counted towards both'
data$pr_what_done_mixed[data$pr_what_done_mixed=='counted towards both Pv and Pf infections']<-'counted towards both'
data$pr_what_done_mixed[data$pr_what_done_mixed=='counted towards both: vivax as well as falciparum']<-'counted towards both'
data$pr_what_done_mixed[data$pr_what_done_mixed=='counted towards falciparum']<-'counted towards falciparum'
data$pr_what_done_mixed[data$pr_what_done_mixed=="doesn't say"]<-'not reported'
data$pr_what_done_mixed[data$pr_what_done_mixed=="doesn't say (assume counted towards both Pv and Pf)"]<-'not reported'
data$pr_what_done_mixed[data$pr_what_done_mixed=="doesn't say, probably counted towards both, in table 2 P spp is less than if you add up Pv and Pf"]<-'not reported'
data$pr_what_done_mixed[data$pr_what_done_mixed=='mixed cases counted towards falciparum']<-'counted towards falciparum'
data$pr_what_done_mixed[data$pr_what_done_mixed=='mixed counted towards both species']<-'counted towards both'
data$pr_what_done_mixed[data$pr_what_done_mixed=='mixed counted towards falciparum and vivax cases as well (probably), otherwise would be supe rhigh numbers!']<-'counted towards both'
data$pr_what_done_mixed[data$pr_what_done_mixed=='most probably counted towards both']<-'counted towards both'
data$pr_what_done_mixed[data$pr_what_done_mixed=='reported seperately']<-'given separately'
data$pr_what_done_mixed[data$pr_what_done_mixed=="there are some (not too many) but doesn't say if counted towards both or not"]<-'not reported'
data$pr_what_done_mixed[data$pr_what_done_mixed=="they have a seperate category but could also contain Pm, but could also be already counted towards the different groups and then mentioned again in the mixed ones"]<-'not reported'
data$pr_what_done_mixed<-as.factor(data$pr_what_done_mixed)



#code diagnostic tool for prevalence
data$prevalence_diagnostics<-NA
x<-c(1:length(data$first_authors))
for (val in x)
{if (!is.na(data$prevalence_falciparum_LM[val])){data$prevalence_diagnostics[val]<-'LM'}
  if (!is.na(data$prevalence_falciparum_PCR[val])){data$prevalence_diagnostics[val]<-'PCR'}
  if (!is.na(data$pr_numb_falciparum_LM[val])){data$prevalence_diagnostics[val]<-'LM'}
  if (!is.na(data$pr_numb_falciparum_PCR[val])){data$prevalence_diagnostics[val]<-'PCR'}
  if (!is.na(data$pr_numb_falciparum_PCR[val])&!is.na(data$pr_numb_falciparum_LM[val])){data$prevalence_diagnostics[val]<-'PCR and LM'}
  if (!is.na(data$prevalence_falciparum_PCR[val])&!is.na(data$prevalence_falciparum_LM[val])){data$prevalence_diagnostics[val]<-'PCR and LM'}
  
}

data$prevalence_diagnostics<-as.factor(data$prevalence_diagnostics)  


