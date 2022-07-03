
#########################
# preparation_analysis.r
#########################



######
##incidence
#######


data_new_inc$row_number<-seq.int(nrow(data_new_inc))

#make variable vivax_new in incidence data frame with actual case numbers
data_new_inc$vivax_new<-data_new_inc$case_numbers_vivax
data_new_inc$falciparum_new<-data_new_inc$case_numbers_falciparum
x<-c(1:length(data_new_inc$Intervention))
for (val in x){
  if(is.na(data_new_inc$vivax_new[val])){data_new_inc$vivax_new[val]<-data_new_inc$inc_converted_vivax[val]}}
x<-c(1:length(data_new_inc$Intervention))
for (val in x){
  if(is.na(data_new_inc$falciparum_new[val])){data_new_inc$falciparum_new[val]<-data_new_inc$inc_converted_falc[val]}}



#get the time bins by steps of 6 months for categorical time
data_new_inc$time_incidence_cat <- cut(data_new_inc$time_incidence, breaks=c(-36, 0 , 6, 12, 18, 24, 72))


######
##code for intial proportion of Pv (high if above or equal to 50%)
#######
#add a before data point for comparison and take the average where there are more than one
data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$Intervention=='control']<-0
s<-data_new_inc[data_new_inc$time_zero<=0,c('vivax_new',"falciparum_new",'study_number_new')]
s$initial_proportion<-s$vivax_new/(s$vivax_new+s$falciparum_new)
data_new_inc$initial_proportion<-data_new_inc$vivax_new
data_new_inc$initial_proportion<-NA
x<-unique(s$study_number_new)
for (val in x){
   s$initial_proportion_cat[s$study_number_new==val]<-mean(s$initial_proportion[s$study_number_new==val])
}
for (val in x){
   numrec<-length(data_new_inc$initial_proportion[data_new_inc$study_number_new==val])
   data_new_inc$initial_proportion[data_new_inc$study_number_new==val]<-rep(s$initial_proportion_cat[s$study_number_new==val][1], numrec)
}

data_new_inc$initial_proportion_cat[data_new_inc$initial_proportion<0.5]<-'low'
data_new_inc$initial_proportion_cat[data_new_inc$initial_proportion>=0.5]<-'high'
data_new_inc$initial_proportion_cat<-as.factor(data_new_inc$initial_proportion_cat)



#classify controls as time category before interventions happened
data_new_inc$time_incidence_cat[data_new_inc$Intervention=='control']<-'(-36,0]'
  

#drop levels that are not used
data_new_inc$seasonality<-as.character(data_new_inc$seasonality)
data_new_inc$seasonality<-as.factor(data_new_inc$seasonality)
data_new_inc$age_incidence<-as.character(data_new_inc$age_incidence)
data_new_inc$age_incidence<-as.factor(data_new_inc$age_incidence)
data_new_inc$season_survey_incidence<-as.character(data_new_inc$season_survey_incidence)
data_new_inc$season_survey_incidence<-as.factor(data_new_inc$season_survey_incidence)
data_new_inc$incidence_diagnostics<-as.character(data_new_inc$incidence_diagnostics)
data_new_inc$incidence_diagnostics<-as.factor(data_new_inc$incidence_diagnostics)
data_new_inc$relapse_pattern_white<-as.character(data_new_inc$relapse_pattern_white)
data_new_inc$relapse_pattern_white<-as.factor(data_new_inc$relapse_pattern_white)




##########
##data frames for the different interventions
#########

#LLIN first time
data_inc_LLIN_first<-data_new_inc[(data_new_inc$Intervention=='LLIN 1st dist')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN 1st dist'),]

data_inc_LLIN_first$seasonality<-as.character(data_inc_LLIN_first$seasonality)
data_inc_LLIN_first$seasonality<-as.factor(data_inc_LLIN_first$seasonality)
data_inc_LLIN_first$age_incidence<-as.character(data_inc_LLIN_first$age_incidence)
data_inc_LLIN_first$age_incidence<-as.factor(data_inc_LLIN_first$age_incidence)
data_inc_LLIN_first$season_survey_incidence<-as.character(data_inc_LLIN_first$season_survey_incidence)
data_inc_LLIN_first$season_survey_incidence<-as.factor(data_inc_LLIN_first$season_survey_incidence)
data_inc_LLIN_first$incidence_diagnostics<-as.character(data_inc_LLIN_first$incidence_diagnostics)
data_inc_LLIN_first$incidence_diagnostics<-as.factor(data_inc_LLIN_first$incidence_diagnostics)
data_inc_LLIN_first$time_incidence_cat<-as.character(data_inc_LLIN_first$time_incidence_cat)
data_inc_LLIN_first$time_incidence_cat<-as.factor(data_inc_LLIN_first$time_incidence_cat)


#LLIN repeated time
data_inc_LLIN_rep<-data_new_inc[(data_new_inc$Intervention=='LLIN later dist')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN later dist'),]

data_inc_LLIN_rep$seasonality<-as.character(data_inc_LLIN_rep$seasonality)
data_inc_LLIN_rep$seasonality<-as.factor(data_inc_LLIN_rep$seasonality)
data_inc_LLIN_rep$age_incidence<-as.character(data_inc_LLIN_rep$age_incidence)
data_inc_LLIN_rep$age_incidence<-as.factor(data_inc_LLIN_rep$age_incidence)
data_inc_LLIN_rep$season_survey_incidence<-as.character(data_inc_LLIN_rep$season_survey_incidence)
data_inc_LLIN_rep$season_survey_incidence<-as.factor(data_inc_LLIN_rep$season_survey_incidence)
data_inc_LLIN_rep$incidence_diagnostics<-as.character(data_inc_LLIN_rep$incidence_diagnostics)
data_inc_LLIN_rep$incidence_diagnostics<-as.factor(data_inc_LLIN_rep$incidence_diagnostics)
data_inc_LLIN_rep$time_incidence_cat<-as.character(data_inc_LLIN_rep$time_incidence_cat)
data_inc_LLIN_rep$time_incidence_cat<-as.factor(data_inc_LLIN_rep$time_incidence_cat)


#one dataset that combines repeated and first time distribution of nets
data_inc_LLIN_all<-rbind(data_inc_LLIN_first,data_inc_LLIN_rep)

#LLIN rest
data_inc_LLIN<-data_new_inc[(data_new_inc$Intervention=='LLIN')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN'),]

data_inc_LLIN$seasonality<-as.character(data_inc_LLIN$seasonality)
data_inc_LLIN$seasonality<-as.factor(data_inc_LLIN$seasonality)
data_inc_LLIN$age_incidence<-as.character(data_inc_LLIN$age_incidence)
data_inc_LLIN$age_incidence<-as.factor(data_inc_LLIN$age_incidence)
data_inc_LLIN$season_survey_incidence<-as.character(data_inc_LLIN$season_survey_incidence)
data_inc_LLIN$season_survey_incidence<-as.factor(data_inc_LLIN$season_survey_incidence)
data_inc_LLIN$incidence_diagnostics<-as.character(data_inc_LLIN$incidence_diagnostics)
data_inc_LLIN$incidence_diagnostics<-as.factor(data_inc_LLIN$incidence_diagnostics)
data_inc_LLIN$time_incidence_cat<-as.character(data_inc_LLIN$time_incidence_cat)
data_inc_LLIN$time_incidence_cat<-as.factor(data_inc_LLIN$time_incidence_cat)

#untreated
data_inc_untreated<-data_new_inc[(data_new_inc$Intervention=='untreated nets')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='untreated nets'),]

data_inc_untreated$seasonality<-as.character(data_inc_untreated$seasonality)
data_inc_untreated$seasonality<-as.factor(data_inc_untreated$seasonality)
data_inc_untreated$age_incidence<-as.character(data_inc_untreated$age_incidence)
data_inc_untreated$age_incidence<-as.factor(data_inc_untreated$age_incidence)
data_inc_untreated$season_survey_incidence<-as.character(data_inc_untreated$season_survey_incidence)
data_inc_untreated$season_survey_incidence<-as.factor(data_inc_untreated$season_survey_incidence)
data_inc_untreated$incidence_diagnostics<-as.character(data_inc_untreated$incidence_diagnostics)
data_inc_untreated$incidence_diagnostics<-as.factor(data_inc_untreated$incidence_diagnostics)
data_inc_untreated$time_incidence_cat<-as.character(data_inc_untreated$time_incidence_cat)
data_inc_untreated$time_incidence_cat<-as.factor(data_inc_untreated$time_incidence_cat)

#net retreatment
data_inc_retreat<-data_new_inc[(data_new_inc$Intervention=='net retreatment')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='net retreatment'),]

data_inc_retreat$seasonality<-as.character(data_inc_retreat$seasonality)
data_inc_retreat$seasonality<-as.factor(data_inc_retreat$seasonality)
data_inc_retreat$age_incidence<-as.character(data_inc_retreat$age_incidence)
data_inc_retreat$age_incidence<-as.factor(data_inc_retreat$age_incidence)
data_inc_retreat$season_survey_incidence<-as.character(data_inc_retreat$season_survey_incidence)
data_inc_retreat$season_survey_incidence<-as.factor(data_inc_retreat$season_survey_incidence)
data_inc_retreat$incidence_diagnostics<-as.character(data_inc_retreat$incidence_diagnostics)
data_inc_retreat$incidence_diagnostics<-as.factor(data_inc_retreat$incidence_diagnostics)
data_inc_retreat$time_incidence_cat<-as.character(data_inc_retreat$time_incidence_cat)
data_inc_retreat$time_incidence_cat<-as.factor(data_inc_retreat$time_incidence_cat)


#MDA first time
data_inc_MDA_first<-data_new_inc[(data_new_inc$Intervention=='MDA 1st')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA 1st'),]

data_inc_MDA_first$seasonality<-as.character(data_inc_MDA_first$seasonality)
data_inc_MDA_first$seasonality<-as.factor(data_inc_MDA_first$seasonality)
data_inc_MDA_first$age_incidence<-as.character(data_inc_MDA_first$age_incidence)
data_inc_MDA_first$age_incidence<-as.factor(data_inc_MDA_first$age_incidence)
data_inc_MDA_first$season_survey_incidence<-as.character(data_inc_MDA_first$season_survey_incidence)
data_inc_MDA_first$season_survey_incidence<-as.factor(data_inc_MDA_first$season_survey_incidence)
data_inc_MDA_first$incidence_diagnostics<-as.character(data_inc_MDA_first$incidence_diagnostics)
data_inc_MDA_first$incidence_diagnostics<-as.factor(data_inc_MDA_first$incidence_diagnostics)
data_inc_MDA_first$time_incidence_cat<-as.character(data_inc_MDA_first$time_incidence_cat)
data_inc_MDA_first$time_incidence_cat<-as.factor(data_inc_MDA_first$time_incidence_cat)


#change the time categories for MDA to shorter 
data_inc_MDA_first$time_incidence_cat <- cut(data_inc_MDA_first$time_incidence, breaks=c(-36, 0,2,4,28))

#MDA repeated
data_inc_MDA_rep<-data_new_inc[(data_new_inc$Intervention=='MDA repeated')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA repeated'),]

data_inc_MDA_rep$seasonality<-as.character(data_inc_MDA_rep$seasonality)
data_inc_MDA_rep$seasonality<-as.factor(data_inc_MDA_rep$seasonality)
data_inc_MDA_rep$age_incidence<-as.character(data_inc_MDA_rep$age_incidence)
data_inc_MDA_rep$age_incidence<-as.factor(data_inc_MDA_rep$age_incidence)
data_inc_MDA_rep$season_survey_incidence<-as.character(data_inc_MDA_rep$season_survey_incidence)
data_inc_MDA_rep$season_survey_incidence<-as.factor(data_inc_MDA_rep$season_survey_incidence)
data_inc_MDA_rep$incidence_diagnostics<-as.character(data_inc_MDA_rep$incidence_diagnostics)
data_inc_MDA_rep$incidence_diagnostics<-as.factor(data_inc_MDA_rep$incidence_diagnostics)
data_inc_MDA_rep$time_incidence_cat<-as.character(data_inc_MDA_rep$time_incidence_cat)
data_inc_MDA_rep$time_incidence_cat<-as.factor(data_inc_MDA_rep$time_incidence_cat)

data_inc_MDA_rep$time_incidence<-as.numeric(data_inc_MDA_rep$time_incidence)
#hist(data_inc_MDA_rep$time_incidence, breaks=seq(-36,28,by=1))
data_inc_MDA_rep$time_incidence_cat <- cut(data_inc_MDA_rep$time_incidence, breaks=c(-36, 0,2,4,28))

#combination of repeated and first time
data_inc_MDA_all<-rbind(data_inc_MDA_first,data_inc_MDA_rep)


#MDA rest
data_inc_MDA<-data_new_inc[(data_new_inc$Intervention=='MDA')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA'),]

data_inc_MDA$seasonality<-as.character(data_inc_MDA$seasonality)
data_inc_MDA$seasonality<-as.factor(data_inc_MDA$seasonality)
data_inc_MDA$age_incidence<-as.character(data_inc_MDA$age_incidence)
data_inc_MDA$age_incidence<-as.factor(data_inc_MDA$age_incidence)
data_inc_MDA$season_survey_incidence<-as.character(data_inc_MDA$season_survey_incidence)
data_inc_MDA$season_survey_incidence<-as.factor(data_inc_MDA$season_survey_incidence)
data_inc_MDA$incidence_diagnostics<-as.character(data_inc_MDA$incidence_diagnostics)
data_inc_MDA$incidence_diagnostics<-as.factor(data_inc_MDA$incidence_diagnostics)
data_inc_MDA$time_incidence_cat<-as.character(data_inc_MDA$time_incidence_cat)
data_inc_MDA$time_incidence_cat<-as.factor(data_inc_MDA$time_incidence_cat)


#IRS first
data_inc_IRS_first<-data_new_inc[(data_new_inc$Intervention=='IRS 1st time')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS 1st time'),]

data_inc_IRS_first$seasonality<-as.character(data_inc_IRS_first$seasonality)
data_inc_IRS_first$seasonality<-as.factor(data_inc_IRS_first$seasonality)
data_inc_IRS_first$age_incidence<-as.character(data_inc_IRS_first$age_incidence)
data_inc_IRS_first$age_incidence<-as.factor(data_inc_IRS_first$age_incidence)
data_inc_IRS_first$season_survey_incidence<-as.character(data_inc_IRS_first$season_survey_incidence)
data_inc_IRS_first$season_survey_incidence<-as.factor(data_inc_IRS_first$season_survey_incidence)
data_inc_IRS_first$incidence_diagnostics<-as.character(data_inc_IRS_first$incidence_diagnostics)
data_inc_IRS_first$incidence_diagnostics<-as.factor(data_inc_IRS_first$incidence_diagnostics)
data_inc_IRS_first$time_incidence_cat<-as.character(data_inc_IRS_first$time_incidence_cat)
data_inc_IRS_first$time_incidence_cat<-as.factor(data_inc_IRS_first$time_incidence_cat)

#IRS repeated
data_inc_IRS_rep<-data_new_inc[(data_new_inc$Intervention=='IRS repeated time')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS repeated time'),]

data_inc_IRS_rep$seasonality<-as.character(data_inc_IRS_rep$seasonality)
data_inc_IRS_rep$seasonality<-as.factor(data_inc_IRS_rep$seasonality)
data_inc_IRS_rep$age_incidence<-as.character(data_inc_IRS_rep$age_incidence)
data_inc_IRS_rep$age_incidence<-as.factor(data_inc_IRS_rep$age_incidence)
data_inc_IRS_rep$season_survey_incidence<-as.character(data_inc_IRS_rep$season_survey_incidence)
data_inc_IRS_rep$season_survey_incidence<-as.factor(data_inc_IRS_rep$season_survey_incidence)
data_inc_IRS_rep$incidence_diagnostics<-as.character(data_inc_IRS_rep$incidence_diagnostics)
data_inc_IRS_rep$incidence_diagnostics<-as.factor(data_inc_IRS_rep$incidence_diagnostics)
data_inc_IRS_rep$time_incidence_cat<-as.character(data_inc_IRS_rep$time_incidence_cat)
data_inc_IRS_rep$time_incidence_cat<-as.factor(data_inc_IRS_rep$time_incidence_cat)


#IRS rest
data_inc_IRS<-data_new_inc[(data_new_inc$Intervention=='IRS')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS'),]

data_inc_IRS$seasonality<-as.character(data_inc_IRS$seasonality)
data_inc_IRS$seasonality<-as.factor(data_inc_IRS$seasonality)
data_inc_IRS$age_incidence<-as.character(data_inc_IRS$age_incidence)
data_inc_IRS$age_incidence<-as.factor(data_inc_IRS$age_incidence)
data_inc_IRS$season_survey_incidence<-as.character(data_inc_IRS$season_survey_incidence)
data_inc_IRS$season_survey_incidence<-as.factor(data_inc_IRS$season_survey_incidence)
data_inc_IRS$incidence_diagnostics<-as.character(data_inc_IRS$incidence_diagnostics)
data_inc_IRS$incidence_diagnostics<-as.factor(data_inc_IRS$incidence_diagnostics)
data_inc_IRS$time_incidence_cat<-as.character(data_inc_IRS$time_incidence_cat)
data_inc_IRS$time_incidence_cat<-as.factor(data_inc_IRS$time_incidence_cat)

#all IRS
data_inc_IRS_all<-rbind(data_inc_IRS_first,data_inc_IRS_rep, data_inc_IRS)


#increased case managment
data_inc_case<-data_new_inc[(data_new_inc$Intervention=='increased case management')| (data_new_inc$Intervention=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='increased case management'),]

data_inc_case$seasonality<-as.character(data_inc_case$seasonality)
data_inc_case$seasonality<-as.factor(data_inc_case$seasonality)
data_inc_case$age_incidence<-as.character(data_inc_case$age_incidence)
data_inc_case$age_incidence<-as.factor(data_inc_case$age_incidence)
data_inc_case$season_survey_incidence<-as.character(data_inc_case$season_survey_incidence)
data_inc_case$season_survey_incidence<-as.factor(data_inc_case$season_survey_incidence)
data_inc_case$incidence_diagnostics<-as.character(data_inc_case$incidence_diagnostics)
data_inc_case$incidence_diagnostics<-as.factor(data_inc_case$incidence_diagnostics)
data_inc_case$time_incidence_cat<-as.character(data_inc_case$time_incidence_cat)
data_inc_case$time_incidence_cat<-as.factor(data_inc_case$time_incidence_cat)




######
##prevalence
#######

data_new_prev$row_number<-seq.int(nrow(data_new_prev))


#variable vivax_new and falciparum_new with actual number of people infected
data_new_prev$vivax_new<-data_new_prev$pr_numb_vivax_LM
data_new_prev$falciparum_new<-data_new_prev$pr_numb_falciparum_LM
x<-c(1:length(data_new_prev$Intervention))
for (val in x){
  if(is.na(data_new_prev$vivax_new[val])){data_new_prev$vivax_new[val]<-data_new_prev$pr_numb_vivax_PCR[val]}}
for (val in x){
  if(is.na(data_new_prev$falciparum_new[val])){data_new_prev$falciparum_new[val]<-data_new_prev$pr_numb_falciparum_PCR[val]}}
x<-c(1:length(data_new_prev$Intervention))
for (val in x){
  if(is.na(data_new_prev$vivax_new[val])){data_new_prev$vivax_new[val]<-data_new_prev$prev_converted_viv[val]}}
for (val in x){
  if(is.na(data_new_prev$falciparum_new[val])){data_new_prev$falciparum_new[val]<-data_new_prev$prev_converted_falc[val]}}




######
##code for intial proportion of Pv
#######
data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$Intervention=='control']<-0
s<-data_new_prev[data_new_prev$time_zero<=0,c('vivax_new',"falciparum_new",'study_number_new')]
s$initial_proportion<-s$vivax_new/(s$vivax_new+s$falciparum_new)
data_new_prev$initial_proportion<-data_new_prev$vivax_new
data_new_prev$initial_proportion<-NA
x<-unique(s$study_number_new)
for (val in x)
{s$initial_proportion_cat[s$study_number_new==val]<-mean(s$initial_proportion[s$study_number_new==val])
}
for (val in x) {
   numrec<-length(data_new_prev$initial_proportion[data_new_prev$study_number_new==val])
   data_new_prev$initial_proportion[data_new_prev$study_number_new==val]<-rep( s$initial_proportion_cat[s$study_number_new==val][1], numrec)
 }


data_new_prev$initial_proportion_cat[data_new_prev$initial_proportion<0.5]<-'low'
data_new_prev$initial_proportion_cat[data_new_prev$initial_proportion>=0.5]<-'high'
data_new_prev$initial_proportion_cat<-as.factor(data_new_prev$initial_proportion_cat)

#get the time bins
#hist(data_new_prev$time_prevalence, breaks=seq(-90,402,by=6))
data_new_prev$time_prevalence_cat <- cut(data_new_prev$time_prevalence, breaks=c(-90, 0 , 6, 12, 18, 24, 402))


#classify control into before intervention time category
data_new_prev$time_prevalence_cat[data_new_prev$Intervention=='control']<-'(-90,0]'

#LLIN first time
data_prev_LLIN_first<-data_new_prev[(data_new_prev$Intervention=='LLIN 1st dist')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist'),]

data_prev_LLIN_first$seasonality<-as.character(data_prev_LLIN_first$seasonality)
data_prev_LLIN_first$seasonality<-as.factor(data_prev_LLIN_first$seasonality)
data_prev_LLIN_first$age_prevalence<-as.character(data_prev_LLIN_first$age_prevalence)
data_prev_LLIN_first$age_prevalence<-as.factor(data_prev_LLIN_first$age_prevalence)
data_prev_LLIN_first$season_survey_prevalence<-as.character(data_prev_LLIN_first$season_survey_prevalence)
data_prev_LLIN_first$season_survey_prevalence<-as.factor(data_prev_LLIN_first$season_survey_prevalence)
data_prev_LLIN_first$time_prevalence_cat<-as.character(data_prev_LLIN_first$time_prevalence_cat)
data_prev_LLIN_first$time_prevalence_cat<-as.factor(data_prev_LLIN_first$time_prevalence_cat)


#LLIN repeated time
data_prev_LLIN_rep<-data_new_prev[(data_new_prev$Intervention=='LLIN later dist')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist'),]

data_prev_LLIN_rep$seasonality<-as.character(data_prev_LLIN_rep$seasonality)
data_prev_LLIN_rep$seasonality<-as.factor(data_prev_LLIN_rep$seasonality)
data_prev_LLIN_rep$age_prevalence<-as.character(data_prev_LLIN_rep$age_prevalence)
data_prev_LLIN_rep$age_prevalence<-as.factor(data_prev_LLIN_rep$age_prevalence)
data_prev_LLIN_rep$season_survey_prevalence<-as.character(data_prev_LLIN_rep$season_survey_prevalence)
data_prev_LLIN_rep$season_survey_prevalence<-as.factor(data_prev_LLIN_rep$season_survey_prevalence)
data_prev_LLIN_rep$time_prevalence_cat<-as.character(data_prev_LLIN_rep$time_prevalence_cat)
data_prev_LLIN_rep$time_prevalence_cat<-as.factor(data_prev_LLIN_rep$time_prevalence_cat)


#exclude the ones that only have 24 months after the intervention or find no cases before interventon or after
data_prev_LLIN_rep$row_number<-seq.int(nrow(data_prev_LLIN_rep))
b<-data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==39]
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==173])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==184])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==189])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==38])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==167])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==168])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==171])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==176])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==204])
b<-c(b,data_prev_LLIN_rep$row_number[data_prev_LLIN_rep$study_number_new==364])
data_prev_LLIN_rep<-data_prev_LLIN_rep[-c(b),]
data_prev_LLIN_rep$row_number<-seq.int(nrow(data_prev_LLIN_rep))


#one data frame for first time distribution and repeated
data_prev_LLIN_all<-rbind(data_prev_LLIN_first,data_prev_LLIN_rep)


#LLIN rest
data_prev_LLIN<-data_new_prev[(data_new_prev$Intervention=='LLIN')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN'),]

data_prev_LLIN$seasonality<-as.character(data_prev_LLIN$seasonality)
data_prev_LLIN$seasonality<-as.factor(data_prev_LLIN$seasonality)
data_prev_LLIN$age_prevalence<-as.character(data_prev_LLIN$age_prevalence)
data_prev_LLIN$age_prevalence<-as.factor(data_prev_LLIN$age_prevalence)
data_prev_LLIN$season_survey_prevalence<-as.character(data_prev_LLIN$season_survey_prevalence)
data_prev_LLIN$season_survey_prevalence<-as.factor(data_prev_LLIN$season_survey_prevalence)
data_prev_LLIN$time_prevalence_cat<-as.character(data_prev_LLIN$time_prevalence_cat)
data_prev_LLIN$time_prevalence_cat<-as.factor(data_prev_LLIN$time_prevalence_cat)



#untreated
data_prev_untreated<-data_new_prev[(data_new_prev$Intervention=='untreated nets')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='untreated nets'),]

data_prev_untreated$seasonality<-as.character(data_prev_untreated$seasonality)
data_prev_untreated$seasonality<-as.factor(data_prev_untreated$seasonality)
data_prev_untreated$age_prevalence<-as.character(data_prev_untreated$age_prevalence)
data_prev_untreated$age_prevalence<-as.factor(data_prev_untreated$age_prevalence)
data_prev_untreated$season_survey_prevalence<-as.character(data_prev_untreated$season_survey_prevalence)
data_prev_untreated$season_survey_prevalence<-as.factor(data_prev_untreated$season_survey_prevalence)
data_prev_untreated$time_prevalence_cat<-as.character(data_prev_untreated$time_prevalence_cat)
data_prev_untreated$time_prevalence_cat<-as.factor(data_prev_untreated$time_prevalence_cat)

#net retreatment
data_prev_retreat<-data_new_prev[(data_new_prev$Intervention=='net retreatment')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='net retreatment'),]

data_prev_retreat$seasonality<-as.character(data_prev_retreat$seasonality)
data_prev_retreat$seasonality<-as.factor(data_prev_retreat$seasonality)
data_prev_retreat$age_prevalence<-as.character(data_prev_retreat$age_prevalence)
data_prev_retreat$age_prevalence<-as.factor(data_prev_retreat$age_prevalence)
data_prev_retreat$season_survey_prevalence<-as.character(data_prev_retreat$season_survey_prevalence)
data_prev_retreat$season_survey_prevalence<-as.factor(data_prev_retreat$season_survey_prevalence)
data_prev_retreat$time_prevalence_cat<-as.character(data_prev_retreat$time_prevalence_cat)
data_prev_retreat$time_prevalence_cat<-as.factor(data_prev_retreat$time_prevalence_cat)


#MDA first time
data_prev_MDA_first<-data_new_prev[(data_new_prev$Intervention=='MDA 1st')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st'),]

data_prev_MDA_first$seasonality<-as.character(data_prev_MDA_first$seasonality)
data_prev_MDA_first$seasonality<-as.factor(data_prev_MDA_first$seasonality)
data_prev_MDA_first$age_prevalence<-as.character(data_prev_MDA_first$age_prevalence)
data_prev_MDA_first$age_prevalence<-as.factor(data_prev_MDA_first$age_prevalence)
data_prev_MDA_first$season_survey_prevalence<-as.character(data_prev_MDA_first$season_survey_prevalence)
data_prev_MDA_first$season_survey_prevalence<-as.factor(data_prev_MDA_first$season_survey_prevalence)
data_prev_MDA_first$time_prevalence_cat<-as.character(data_prev_MDA_first$time_prevalence_cat)
data_prev_MDA_first$time_prevalence_cat<-as.factor(data_prev_MDA_first$time_prevalence_cat)

#change it into three monthly categories
#hist(data_prev_MDA_first$time_prevalence, breaks=seq(-68,402,by=1))
data_prev_MDA_first$time_prevalence_cat <- cut(data_prev_MDA_first$time_prevalence, breaks=c(-68, 0,3,7,402))


#MDA repeated
data_prev_MDA_rep<-data_new_prev[(data_new_prev$Intervention=='MDA repeated')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA repeated'),]

data_prev_MDA_rep$seasonality<-as.character(data_prev_MDA_rep$seasonality)
data_prev_MDA_rep$seasonality<-as.factor(data_prev_MDA_rep$seasonality)
data_prev_MDA_rep$age_prevalence<-as.character(data_prev_MDA_rep$age_prevalence)
data_prev_MDA_rep$age_prevalence<-as.factor(data_prev_MDA_rep$age_prevalence)
data_prev_MDA_rep$season_survey_prevalence<-as.character(data_prev_MDA_rep$season_survey_prevalence)
data_prev_MDA_rep$season_survey_prevalence<-as.factor(data_prev_MDA_rep$season_survey_prevalence)
data_prev_MDA_rep$time_prevalence_cat<-as.character(data_prev_MDA_rep$time_prevalence_cat)
data_prev_MDA_rep$time_prevalence_cat<-as.factor(data_prev_MDA_rep$time_prevalence_cat)
#hist(data_prev_MDA_rep$time_prevalence, breaks=seq(-68,402,by=1))
data_prev_MDA_rep$time_prevalence_cat <- cut(data_prev_MDA_rep$time_prevalence, breaks=c(-68, 0,3,7,402))


#MDA rest
data_prev_MDA<-data_new_prev[(data_new_prev$Intervention=='MDA')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA'),]

data_prev_MDA$seasonality<-as.character(data_prev_MDA$seasonality)
data_prev_MDA$seasonality<-as.factor(data_prev_MDA$seasonality)
data_prev_MDA$age_prevalence<-as.character(data_prev_MDA$age_prevalence)
data_prev_MDA$age_prevalence<-as.factor(data_prev_MDA$age_prevalence)
data_prev_MDA$season_survey_prevalence<-as.character(data_prev_MDA$season_survey_prevalence)
data_prev_MDA$season_survey_prevalence<-as.factor(data_prev_MDA$season_survey_prevalence)
data_prev_MDA$time_prevalence_cat<-as.character(data_prev_MDA$time_prevalence_cat)
data_prev_MDA$time_prevalence_cat<-as.factor(data_prev_MDA$time_prevalence_cat)


#IRS first
data_prev_IRS_first<-data_new_prev[(data_new_prev$Intervention=='IRS 1st time')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS 1st time'),]

data_prev_IRS_first$seasonality<-as.character(data_prev_IRS_first$seasonality)
data_prev_IRS_first$seasonality<-as.factor(data_prev_IRS_first$seasonality)
data_prev_IRS_first$age_prevalence<-as.character(data_prev_IRS_first$age_prevalence)
data_prev_IRS_first$age_prevalence<-as.factor(data_prev_IRS_first$age_prevalence)
data_prev_IRS_first$season_survey_prevalence<-as.character(data_prev_IRS_first$season_survey_prevalence)
data_prev_IRS_first$season_survey_prevalence<-as.factor(data_prev_IRS_first$season_survey_prevalence)
data_prev_IRS_first$time_prevalence_cat<-as.character(data_prev_IRS_first$time_prevalence_cat)
data_prev_IRS_first$time_prevalence_cat<-as.factor(data_prev_IRS_first$time_prevalence_cat)

#IRS repeated
data_prev_IRS_rep<-data_new_prev[(data_new_prev$Intervention=='IRS repeated time')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS repeated time'),]

data_prev_IRS_rep$seasonality<-as.character(data_prev_IRS_rep$seasonality)
data_prev_IRS_rep$seasonality<-as.factor(data_prev_IRS_rep$seasonality)
data_prev_IRS_rep$age_prevalence<-as.character(data_prev_IRS_rep$age_prevalence)
data_prev_IRS_rep$age_prevalence<-as.factor(data_prev_IRS_rep$age_prevalence)
data_prev_IRS_rep$season_survey_prevalence<-as.character(data_prev_IRS_rep$season_survey_prevalence)
data_prev_IRS_rep$season_survey_prevalence<-as.factor(data_prev_IRS_rep$season_survey_prevalence)
data_prev_IRS_rep$time_prevalence_cat<-as.character(data_prev_IRS_rep$time_prevalence_cat)
data_prev_IRS_rep$time_prevalence_cat<-as.factor(data_prev_IRS_rep$time_prevalence_cat)


#IRS rest
data_prev_IRS<-data_new_prev[(data_new_prev$Intervention=='IRS')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS'),]

data_prev_IRS$seasonality<-as.character(data_prev_IRS$seasonality)
data_prev_IRS$seasonality<-as.factor(data_prev_IRS$seasonality)
data_prev_IRS$age_prevalence<-as.character(data_prev_IRS$age_prevalence)
data_prev_IRS$age_prevalence<-as.factor(data_prev_IRS$age_prevalence)
data_prev_IRS$season_survey_prevalence<-as.character(data_prev_IRS$season_survey_prevalence)
data_prev_IRS$season_survey_prevalence<-as.factor(data_prev_IRS$season_survey_prevalence)
data_prev_IRS$time_prevalence_cat<-as.character(data_prev_IRS$time_prevalence_cat)
data_prev_IRS$time_prevalence_cat<-as.factor(data_prev_IRS$time_prevalence_cat)


#data frame for all IRS no matter which round
data_prev_IRS_all<-rbind(data_prev_IRS_first,data_prev_IRS_rep, data_prev_IRS)

#increased case managment
data_prev_case<-data_new_prev[(data_new_prev$Intervention=='increased case management')| (data_new_prev$Intervention=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='increased case management'),]

data_prev_case$seasonality<-as.character(data_prev_case$seasonality)
data_prev_case$seasonality<-as.factor(data_prev_case$seasonality)
data_prev_case$age_prevalence<-as.character(data_prev_case$age_prevalence)
data_prev_case$age_prevalence<-as.factor(data_prev_case$age_prevalence)
data_prev_case$season_survey_prevalence<-as.character(data_prev_case$season_survey_prevalence)
data_prev_case$season_survey_prevalence<-as.factor(data_prev_case$season_survey_prevalence)
data_prev_case$time_prevalence_cat<-as.character(data_prev_case$time_prevalence_cat)
data_prev_case$time_prevalence_cat<-as.factor(data_prev_case$time_prevalence_cat)









