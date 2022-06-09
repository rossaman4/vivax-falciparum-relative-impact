#######################
##statistical analysis
#######################

source("preparation_analysis.R")


library(lme4)
library(lmtest)



#this function makes a table from model with fixed effects and CI with log(OR) exponentiated to get the OR 

fab_function<-function(my_model)
{cc <- confint(my_model,parm="beta_", method='Wald')  
ctab <- cbind(est=fixef(my_model),cc)
ctab<-exp(ctab)
ctab<-data.frame(ctab)
ctab$names <- rownames(ctab)
colnames(ctab)[1] <- "Odds ratio"
colnames(ctab)[2] <- "95%CI-lower bound"
colnames(ctab)[3] <- "95%CI-upper bound"
ctab$names[ctab$names=='time_incidence_cat(0,6]']<-'0-6 months'
ctab$names[ctab$names=='time_incidence_cat(6,12]']<-'6-12 months'
ctab$names[ctab$names=='time_incidence_cat(12,18]']<-'12-18 months'
ctab$names[ctab$names=='time_incidence_cat(18,24]']<-'18-24 months'
ctab$names[ctab$names=='time_incidence_cat(24,72]']<-'24+ months'
ctab$names[ctab$names=='relevel(time_incidence_cat, "(-36,0]")(0,1]']<-'0-1 months'
ctab$names[ctab$names=='relevel(time_incidence_cat, "(-36,0]")(1,2]']<-'1-2 months'
ctab$names[ctab$names=='relevel(time_incidence_cat, "(-36,0]")(2,3]']<-'2-3 months'
ctab$names[ctab$names=='relevel(time_incidence_cat, "(-36,0]")(3,4]']<-'3-4 months'
ctab$names[ctab$names=='relevel(time_incidence_cat, "(-36,0]")(4,5]']<-'4-5 months'
ctab$names[ctab$names=='relevel(time_incidence_cat, "(-36,0]")(5,27]']<-'5+ months'

ctab$names[ctab$names=='(Intercept)']<-'Intercept'
ctab$names[ctab$names=='relevel(relapse_pattern_white, "frequent")both']<-'Frequent and long latency'
ctab$names[ctab$names=='relevel(relapse_pattern_white, "frequent")long']<-'Long latency'
ctab$names[ctab$names=='relevel(seasonality, "low")high']<-'High seasonality'
ctab$names[ctab$names=='relevel(season_survey_incidence, "dry")both']<-'Both'
ctab$names[ctab$names=='relevel(season_survey_incidence, "dry")wet']<-'Wet'
ctab$names[ctab$names=='time_incidence']<-'Time [months]'
ctab$names[ctab$names=='time_zero']<-'Time [months]'
ctab$names[ctab$names=='time_zero_new']<-'Time [months]'
ctab$names[ctab$names=='time_zero:relevel(season_survey_incidence, "dry")wet']<-'Time [months]:wet'
ctab$names[ctab$names=='time_zero:relevel(season_survey_incidence, "dry")both']<-'Time [months]:dry and wet season'
ctab$names[ctab$names=='time_zero:relevel(season_survey_incidence, "dry")missing']<-'Time [months]:season missing'
ctab$names[ctab$names=='relevel(season_survey_prevalence, "dry")missing']<-'Missing season'

ctab$names[ctab$names=='relevel(season_survey_incidence, "dry")missing']<-'Season of survey missing'
ctab$names[ctab$names=='time_zero:relevel(relapse_pattern_white, "frequent")both']<-'Time [months]:long and frequent'
ctab$names[ctab$names=='time_zero:relevel(relapse_pattern_white, "frequent")long']<-'Time [months]:long latency'
ctab$names[ctab$names=='time_zero:relevel(seasonality, "low")high']<-'Time [months]:high seasonality'
ctab$names[ctab$names=='time_zero_new:relevel(relapse_pattern_white, "frequent")both']<-'Time [months]:long and frequent'
ctab$names[ctab$names=='time_zero_new:relevel(relapse_pattern_white, "frequent")long']<-'Time [months]:long latency'

ctab$names[ctab$names=='relevel(transmission_falc_viv, "low low")high low']<-'High Pf low Pv'
ctab$names[ctab$names=='time_zero:relevel(transmission_falc_viv, "low low")high low']<-'Time [months]:high Pf low Pv'
ctab$names[ctab$names=='time_incidence_cat(0,3]:relevel(relapse_pattern_white, "frequent")both']<-'0-3 months:frequent and long'
ctab$names[ctab$names=='time_incidence_cat(3,6]:relevel(relapse_pattern_white, "frequent")both']<-'3-6 months:frequent and long'
ctab$names[ctab$names=='time_incidence_cat(6,9]:relevel(relapse_pattern_white, "frequent")both']<-'6-9 months:frequent and long'
ctab$names[ctab$names=='time_incidence_cat(0,3]:relevel(relapse_pattern_white, "frequent")long']<-'0-3 months:long latency'
ctab$names[ctab$names=='time_incidence_cat(3,6]:relevel(relapse_pattern_white, "frequent")long']<-'3-6 months:long latency'
ctab$names[ctab$names=='time_incidence_cat(6,9]:relevel(relapse_pattern_white, "frequent")long']<-'6-9 months:long latency'

ctab$names[ctab$names=='time_incidence_cat(0,3]:relevel(seasonality, "low")high']<-'0-3 months:high seasonality'
ctab$names[ctab$names=='time_incidence_cat(3,6]:relevel(seasonality, "low")high']<-'3-6 months:high seasonality'
ctab$names[ctab$names=='time_incidence_cat(6,9]:relevel(seasonality, "low")high']<-'6-9 months:high seasonality'
ctab$names[ctab$names=='time_incidence_cat(0,3]']<-'0-3 months'
ctab$names[ctab$names=='time_incidence_cat(3,6]']<-'3-6 months'
ctab$names[ctab$names=='time_incidence_cat(6,9]']<-'6-9 months'
ctab$names[ctab$names=='time_incidence_cat(9,12]']<-'9-12 months'
ctab$names[ctab$names=='time_incidence_cat(12,27]']<-'12+ months'

ctab$names[ctab$names=='relevel(transmission_study, "low low")high high']<-'High Pf, high Pv'
ctab$names[ctab$names=='relevel(transmission_study, "low low")high low']<-'High Pf, low Pv'
ctab$names[ctab$names=='relevel(transmission_study, "low low")low high']<-'Low Pf, high Pv'
ctab$names[ctab$names=='time_zero:relevel(transmission_study, "low low")high high']<-'Time [months]:High Pf, high Pv'
ctab$names[ctab$names=='time_zero:relevel(transmission_study, "low low")high low']<-'Time [months]:High Pf, low Pv'
ctab$names[ctab$names=='time_zero:relevel(transmission_study, "low low")low high']<-'Time [months]:Low Pf, high Pv'
ctab$names[ctab$names=='relevel(season_survey_prevalence, "dry")both']<-'Both'
ctab$names[ctab$names=='relevel(season_survey_prevalence, "dry")wet']<-'Wet'
ctab$names[ctab$names=='time_zero:relevel(coverage, "low")high']<-'Time [months]:High coverage'
ctab$names[ctab$names=='time_zero:relevel(coverage, "low")missing']<-'Time [months]:Missing coverage'
ctab$names[ctab$names=='relevel(coverage, "low")high']<-'High coverage'
ctab$names[ctab$names=='relevel(coverage, "low")missing']<-'Missing coverage'
ctab$names[ctab$names=='relevel(initial_proportion_cat, "low")high']<-'High initial proportion'
ctab$names[ctab$names=='time_zero:relevel(initial_proportion_cat, "low")high']<-'Time [months]:High initial proportion'
ctab$names[ctab$names=='time_zero_new:relevel(coverage, "low")high']<-'Time [months]:High coverage'
ctab$names[ctab$names=='time_zero_new:relevel(coverage, "low")missing']<-'Time [months]:Missing coverage'
ctab$names[ctab$names=='time_zero_new:relevel(transmission_study, "low low")high high']<-'Time [months]:High Pf, high Pv'
ctab$names[ctab$names=='time_zero_new:relevel(transmission_study, "low low")high low']<-'Time [months]:High Pf, low Pv'
ctab$names[ctab$names=='time_zero_new:relevel(transmission_study, "low low")low high']<-'Time [months]:Low Pf, high Pv'

ctab$names[ctab$names=='time_zero:InterventionLLIN later dist']<-'Time [months]:Repeated distribution'
ctab$names[ctab$names=='InterventionLLIN later dist']<-'Repeated distribution'
ctab$names[ctab$names=='relevel(coverage, "low")high:InterventionLLIN later dist']<-'Repeated distribution: high coverage'
ctab$names[ctab$names=='relevel(coverage, "low")missing:InterventionLLIN later dist']<-'Repeated distribution: missing coverage'
ctab$names[ctab$names=='time_zero:relevel(coverage, "low")high:InterventionLLIN later dist']<-'Time [months]:Repeated distribution: high coverage'
ctab$names[ctab$names=='time_zero:relevel(coverage, "low")missing:InterventionLLIN later dist']<-'Time [months]:Repeated distribution: missing coverage'

ctab$names[ctab$names=='InterventionLLIN later dist:relevel(coverage, "low")high']<-'Repeated distribution: high coverage'
ctab$names[ctab$names=='time_zero:InterventionLLIN later dist:relevel(coverage, "low")high']<-'Time [months]:Repeated distribution: high coverage'
ctab$names[ctab$names=='InterventionLLIN later dist:relevel(seasonality, "low")high']<-'Repeated distribution: high seasonality'
ctab$names[ctab$names=='time_zero:InterventionLLIN later dist:relevel(seasonality, "low")high']<-'Time [months]:Repeated distribution: high seasonality'

ctab$names[ctab$names=='InterventionLLIN later dist:relevel(relapse_pattern_white, "frequent")long']<-'Repeated distribution: long latency'
ctab$names[ctab$names=='time_zero:InterventionLLIN later dist:relevel(relapse_pattern_white, "frequent")long']<-'Time [months]:Repeated distribution: long latency'
ctab$names[ctab$names=='relevel(transmission_study, "low low")high low:InterventionLLIN later dist']<-'Repeated distribution: high Pf low Pv'
ctab$names[ctab$names=='time_zero:relevel(transmission_study, "low low")high low:InterventionLLIN later dist']<-'Time [months]:Repeated distribution: high Pf low Pv'
ctab$names[ctab$names=='relevel(relapse_pattern_white, "frequent")long:InterventionLLIN later dist']<-'Repeated distribution: long latency'
ctab$names[ctab$names=='time_zero:relevel(relapse_pattern_white, "frequent")long:InterventionLLIN later dist']<-'Time [months]:Repeated distribution: long latency'

ctab$names[ctab$names=='InterventionLLIN later dist:relevel(initial_proportion_cat, "low")high']<-'Repeated distribution: high initial proportion'
ctab$names[ctab$names=='time_zero:InterventionLLIN later dist:relevel(initial_proportion_cat, "low")high']<-'Time [months]:Repeated distribution: high initial proportion'
ctab$names[ctab$names=='relevel(round_int, "once")three times']<-'Three rounds of MDA'
ctab$names[ctab$names=='time_zero:relevel(round_int, "once")three times']<-'Time [months]:three rounds of MDA'
ctab$names[ctab$names=='time_zero_new:relevel(round_int, "once")three times']<-'Time [months]:three rounds of MDA'


ctab<-ctab[,c(4,1,2,3)]
}



## --- TABLE 4 - Clinical cases ---

###########
#first LLIN 
###########

#make value for time 0 for all before time points! also for control
data_inc_LLIN_first$time_zero<-data_inc_LLIN_first$time_incidence
data_inc_LLIN_first$time_zero[data_inc_LLIN_first$time_zero<0]<-0
data_inc_LLIN_first$time_zero[data_inc_LLIN_first$Intervention=='control']<-0

#find control ones, they are kept with the assumption that before would be same as control group that did not receive intervention
data_inc_LLIN_first$study_number_new[data_inc_LLIN_first$Intervention=='control']

#for sahu (study_number_new ==112) there are also before time points --> keep those and exclude control time points
data_inc_LLIN_first$ID[data_inc_LLIN_first$study_number_new==112&data_inc_LLIN_first$Intervention=='control']
subset_data<-subset(data_inc_LLIN_first, ID!=1941)
subset_data<-subset(subset_data, ID!=1943)

#only use data up to 24 months after intervention
subset_data<-subset(subset_data, time_zero<=24)

# rename long factor level names
levels(subset_data$coverage)<-c("decreasing","high","low","missing")
# decreasing = all hosueholds got them depending on household size, however LLIN use lower half a year around 50%, then 26% and in second yar only around 6-8% all hosueholds


# Continous time, Base model: a random intercept and slope, adjusted for season at timepoint of survey
model_LLIN_first<-glmer(cbind(vivax_new, falciparum_new)~time_zero+relevel(season_survey_incidence,'dry')+ (1 + time_zero| study_number_new), subset_data, family=binomial)
summary(model_LLIN_first)
ctab1<-fab_function(model_LLIN_first)
ctab1




#############
#repeated LLIN 
#############
#prepare time where it is 0 for all that where before
data_inc_LLIN_rep$time_zero<-data_inc_LLIN_rep$time_incidence
data_inc_LLIN_rep$time_zero[data_inc_LLIN_rep$time_zero<0]<-0
data_inc_LLIN_rep$time_zero[data_inc_LLIN_rep$Intervention=='control']<-0


#only include 24 months after the intervention
subset_data<-subset(data_inc_LLIN_rep, time_zero<=24)

#exclude ome-kaius that only has an after time point more than 2 years after
subset_data<-subset(subset_data, study_number_new!=167)

#continous time, base model, adjusted for season at time point of survey
model_LLIN_rep<-glmer(cbind(vivax_new, falciparum_new)~time_zero+relevel(season_survey_incidence,'dry')+ (1 + time_zero| study_number_new), subset_data, family=binomial)
summary(model_LLIN_rep)
ctab1<-fab_function(model_LLIN_rep)
ctab1



######
#IRS all 
#########

#prepare time where it is 0 for all that where before
data_inc_IRS_all$time_zero<-data_inc_IRS_all$time_incidence
data_inc_IRS_all$time_zero[data_inc_IRS_all$time_zero<0]<-0
data_inc_IRS_all$time_zero[data_inc_IRS_all$Intervention=='control']<-0

#find control ones, are included 
data_inc_IRS_all$study_number_new[data_inc_IRS_all$Intervention=='control']

subset_data<-subset(data_inc_IRS_all, time_zero<24)

#time as continuous
model_IRS<-glmer(cbind(vivax_new, falciparum_new)~time_zero+relevel(season_survey_incidence, 'dry') +(1 + time_zero | study_number_new), subset_data, family=binomial)
summary(model_IRS)

ctab1<-fab_function(model_IRS)
ctab1




############
##MDA 0-3 months
###########

#code the rounds of MDA that was done
data_inc_MDA_all$round_int<-as.character(data_inc_MDA_all$master_study_number)
data_inc_MDA_all$round_int[data_inc_MDA_all$round_int=='23']<-'once'
data_inc_MDA_all$round_int[data_inc_MDA_all$round_int=='58']<-'once'
data_inc_MDA_all$round_int[data_inc_MDA_all$round_int!='once']<-'three times'
data_inc_MDA_all$round_int<-as.factor(data_inc_MDA_all$round_int)

#code before time points as time point 0
data_inc_MDA_all$time_zero<-data_inc_MDA_all$time_incidence
data_inc_MDA_all$time_zero[data_inc_MDA_all$time_zero<0]<-0
data_inc_MDA_all$time_zero[data_inc_MDA_all$Intervention=='control']<-0


#only include the first three months
subset_data<-subset(data_inc_MDA_all, time_zero<3)

#check which dont contribute towards 
#subset_data[,c('study_number_new','time_zero',"first_authors","vivax_new","falciparum_new")]



#adjust for season of survey
model_MDA_all<-glmer(cbind(vivax_new, falciparum_new)~time_zero+ relevel(season_survey_incidence,'dry')+(1+time_zero | study_number_new), subset_data, family=binomial)
summary(model_MDA_all)
ctab1<-fab_function(model_MDA_all)
ctab1





###
##MDA 3-6 months 
###
data_inc_MDA_all$round_int<-as.character(data_inc_MDA_all$master_study_number)
data_inc_MDA_all$round_int[data_inc_MDA_all$round_int=='23']<-'once'
data_inc_MDA_all$round_int[data_inc_MDA_all$round_int=='58']<-'once'
data_inc_MDA_all$round_int[data_inc_MDA_all$round_int!='once']<-'three times'
data_inc_MDA_all$round_int<-as.factor(data_inc_MDA_all$round_int)

#code before time points as time point 0
data_inc_MDA_all$time_zero<-data_inc_MDA_all$time_incidence
data_inc_MDA_all$time_zero[data_inc_MDA_all$time_zero<0]<-0
data_inc_MDA_all$time_zero[data_inc_MDA_all$Intervention=='control']<-0


#exclude control points from Tripura (study_number_new==27) because we also have before time points
subset_data<-data_inc_MDA_all


#only include the first three months
subset_data<-subset(subset_data, time_zero>=3)
subset_data<-subset(subset_data, time_zero<6)


#check which dont contribute towards 
#subset_data[,c('study_number_new','time_zero',"X...first_authors","vivax_new","falciparum_new")]



#check what needs excluding because only one data point
subset_data[,c('study_number_new')]
subset_data<-subset(subset_data, study_number_new!=104)
subset_data<-subset(subset_data, study_number_new!=269)
subset_data<-subset(subset_data, study_number_new!=268)


subset_data$time_zero_new<-subset_data$time_zero-3

# time and season survey
model_MDA_all<-glmer(cbind(vivax_new, falciparum_new)~time_zero_new+relevel(season_survey_incidence,'dry') +(1 +time_zero_new| study_number_new), subset_data, family=binomial, glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
ctab1<-fab_function(model_MDA_all)
ctab1




### --- TABLE 5 - PATENT INFECTIONS ----

########
##ITN first and repeated combined 
##########

#code all before data as time point 0
data_prev_LLIN_all$time_zero<-data_prev_LLIN_all$time_prevalence
data_prev_LLIN_all$time_zero[data_prev_LLIN_all$time_zero<0]<-0
data_prev_LLIN_all$time_zero[data_prev_LLIN_all$Intervention=='control']<-0

#find control studies and exclude control for 183 and 136 because there is also before data
data_prev_LLIN_all$study_number_new[data_prev_LLIN_all$Intervention=='control']
data_prev_LLIN_all$ID[data_prev_LLIN_all$study_number_new==183&data_prev_LLIN_all$Intervention=='control']
data_prev_LLIN_all$ID[data_prev_LLIN_all$study_number_new==136&data_prev_LLIN_all$Intervention=='control']
subset_data<-data_prev_LLIN_all
x<-c(1:length(subset_data$Intervention))
for (val in x){
  if(subset_data$Intervention[val]=='control'){subset_data$Intervention[val]<-subset_data$Intervention[val+1]}}
subset_data<-subset(subset_data, ID!=1951)
subset_data<-subset(subset_data, ID!=1953)
subset_data<-subset(subset_data, ID!=1955)
subset_data<-subset(subset_data, ID!=1946)
subset_data<-subset(subset_data, ID!=1948)


#exclude all that is longer than 24 months after
subset_data<-subset(subset_data, time_zero<=24)
#exclude the studies that only have a data point more than 24 months after intervention
subset_data<-subset(subset_data, study_number_new!=39)
subset_data<-subset(subset_data, study_number_new!=173)
subset_data<-subset(subset_data, study_number_new!=184)
subset_data<-subset(subset_data, study_number_new!=189)
#exclude the studies that have not found any species at the before time point
subset_data<-subset(subset_data, study_number_new!=38)
subset_data<-subset(subset_data, study_number_new!=167)
subset_data<-subset(subset_data, study_number_new!=168)
subset_data<-subset(subset_data, study_number_new!=171)
subset_data<-subset(subset_data, study_number_new!=176)
subset_data<-subset(subset_data, study_number_new!=204)
subset_data<-subset(subset_data, study_number_new!=364)


#time only, adjust for season of survey
model_LLIN_all<-glmer(cbind(vivax_new, falciparum_new)~time_zero+relevel(season_survey_prevalence, 'dry')+ (1 + time_zero| study_number_new), subset_data, family=binomial)
ctab1<-fab_function(model_LLIN_all)
ctab1




############
##IRS all 
############
#code time so before time points are all time point 0
data_prev_IRS_all$time_zero<-data_prev_IRS_all$time_prevalence
data_prev_IRS_all$time_zero[data_prev_IRS_all$time_zero<0]<-0
data_prev_IRS_all$time_zero[data_prev_IRS_all$Intervention=='control']<-0

#only include the first 24 months after the intervention
subset_data<-subset(data_prev_IRS_all, time_zero<24)

# base model with season of collection only
model_IRS_all<-glmer(cbind(vivax_new, falciparum_new)~time_zero+relevel(season_survey_prevalence,'dry') +(1+time_zero| study_number_new), subset_data, family=binomial)
summary(model_IRS_all)
ctab1<-fab_function(model_IRS_all)
ctab1



############
##MDA 0-3mo 
###########
#recoding before data points so they are time point 0
data_prev_MDA_first$time_zero<-data_prev_MDA_first$time_prevalence
data_prev_MDA_first$time_zero[data_prev_MDA_first$time_zero<0]<-0
data_prev_MDA_first$time_zero[data_prev_MDA_first$Intervention=='control']<-0

#time but continous but only first 3 months
subset_data<-data_prev_MDA_first
subset_data<-subset(data_prev_MDA_first, time_prevalence<=3)

#exclude the data points that dont have any measuremnts after the intervention in the first three months after
subset_data<-subset(subset_data,study_number_new!=97)

#time, adjusted for season at time point of survey
model_MDA_first<-glmer(cbind(vivax_new, falciparum_new)~time_zero+ relevel(season_survey_prevalence,'dry')+(1+time_zero | study_number_new), subset_data, family=binomial)
summary(model_MDA_first)
ctab1<-fab_function(model_MDA_first)
ctab1





