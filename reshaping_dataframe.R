


#######
##restacking data
#########

##restacks data frame so each intervention that leads to a series of time points has its own study number (study_number_new)
##also recodes coverage: cut off 70% for LLIN, 90% for IRS and MDA


data$study_area[data$study_area=='Papua New Guinea, Ilahita area of Maprik district, East Sepik Province']<-'Papua New Guinea, Ilahita area'


#adding row numbers
data$row_number<-seq.int(nrow(data))
data$row_number_smaller<-data$row_number-1
data$row_number_smaller[data$row_number_smaller==0]<-1



#############
##incidence data
#############

#make subsets but only with data points that follow one intervention, and also add the last data point that happened before the intervention
data_int1_inc<-subset(data, ((data$time_incidence_int1[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1]) & data$time_incidence_int1[data$row_number]>0)) & ((data$time_incidence_int1<=0 & (data$time_incidence_int2[data$row_number+1]<=0 | is.na(data$time_incidence_int2[data$row_number+1]))) | ((data$time_incidence_int2<=0 | is.na(data$time_incidence_int2)) & data$time_incidence_int1>0)) |((data$Sum_Intervention1=='control' | data$Sum_Intervention1[data$row_number_smaller]=='control') & data$incidence_YN=='WAHR'& data$time_incidence_int1>=-6)))
data_int2_inc<-subset(data, ((data$time_incidence_int2[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1]) & data$time_incidence_int2[data$row_number]>0)) & ((data$time_incidence_int2<=0 & (data$time_incidence_int3[data$row_number+1]<=0 | is.na(data$time_incidence_int3[data$row_number+1]))) | ((data$time_incidence_int3<=0 | is.na(data$time_incidence_int3)) & data$time_incidence_int2>0)) | ((data$Sum_Intervention2[row_number]=='control' | data$Sum_Intervention2[data$row_number_smaller]=='control') & data$incidence_YN[row_number]=='WAHR'& data$time_incidence_int2>=-6)))
data_int3_inc<-subset(data, ((data$time_incidence_int3[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1]) & data$time_incidence_int3[data$row_number]>0)) & ((data$time_incidence_int3<=0 & (data$time_incidence_int4[data$row_number+1]<=0 | is.na(data$time_incidence_int4[data$row_number+1]))) | ((data$time_incidence_int4<=0 | is.na(data$time_incidence_int4)) & data$time_incidence_int3>0)) | ((data$Sum_Intervention3[row_number]=='control' | data$Sum_Intervention3[data$row_number_smaller]=='control') & data$incidence_YN[row_number]=='WAHR'& data$time_incidence_int3>=-6)))
data_int4_inc<-subset(data, ((data$time_incidence_int4[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1]) & data$time_incidence_int4[data$row_number]>0)) & ((data$time_incidence_int4<=0 & (data$time_incidence_int5[data$row_number+1]<=0 | is.na(data$time_incidence_int5[data$row_number+1]))) | ((data$time_incidence_int5<=0 | is.na(data$time_incidence_int5)) & data$time_incidence_int4>0)) | ((data$Sum_Intervention4[row_number]=='control' | data$Sum_Intervention4[data$row_number_smaller]=='control') & data$incidence_YN[row_number]=='WAHR'& data$time_incidence_int4>=-6)))


#######
#first intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int1_inc$study_number)
w<-c()
for (val in s)
  {w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_incidence_int1<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_incidence_int1>-6])}
#for (val in k) {print(data$time_incidence_int1[data$ID==val])}
data_int1_inc_extra<-data_int1_inc
for (val in k)
{data_int1_inc_extra<-rbind(data_int1_inc_extra,data[data$ID==val,])}
data_int1_inc_extra<-unique(data_int1_inc_extra)
data_int1_inc<-data_int1_inc_extra

data_int1_inc$Intervention<-data_int1_inc$Sum_Intervention1
data_int1_inc$Intervention<- as.character(data_int1_inc$Intervention)
data_int1_inc$Intervention<- as.factor(data_int1_inc$Intervention)
data_int1_inc$coverage<-data_int1_inc$coverage1
data_int1_inc$implementation_start<-data_int1_inc$implementation1_start
data_int1_inc$implementation_end<-data_int1_inc$implementation1_end
data_int1_inc$study_number_new<-data_int1_inc$study_number
data_int1_inc$time_incidence<-data_int1_inc$time_incidence_int1
data_int1_inc$transmission_falc<-data_int1_inc$trans_falc_int1
data_int1_inc$transmission_viv<-data_int1_inc$trans_viv_int1


#########
#second intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int2_inc$study_number)
w<-c()
for (val in s)
{w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_incidence_int2<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_incidence_int2>-6])}
#for (val in k)
#{print(data$time_incidence_int2[data$ID==val])}
data_int2_inc_extra<-data_int2_inc
for (val in k)
{data_int2_inc_extra<-rbind(data_int2_inc_extra,data[data$ID==val,])}
data_int2_inc_extra<-unique(data_int2_inc_extra)
data_int2_inc<-data_int2_inc_extra

data_int2_inc$Intervention<-data_int2_inc$Sum_Intervention2
data_int2_inc$Intervention<- as.character(data_int2_inc$Intervention)
data_int2_inc$Intervention<- as.factor(data_int2_inc$Intervention)
data_int2_inc$coverage<-data_int2_inc$coverage2
data_int2_inc$implementation_start<-data_int2_inc$implementation2_start
data_int2_inc$implementation_end<-data_int2_inc$implementation2_end
data_int2_inc$study_number_new<-data_int2_inc$study_number + 1*166
data_int2_inc$time_incidence<-data_int2_inc$time_incidence_int2
data_int2_inc$transmission_falc<-data_int2_inc$trans_falc_int2
data_int2_inc$transmission_viv<-data_int2_inc$trans_viv_int2


##########
#third intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int3_inc$study_number)
w<-c()
for (val in s)
{w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_incidence_int3<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_incidence_int3>-6])}
#for (val in k)
#{print(data$time_incidence_int3[data$ID==val])}
data_int3_inc_extra<-data_int3_inc
for (val in k)
{data_int3_inc_extra<-rbind(data_int3_inc_extra,data[data$ID==val,])}
data_int3_inc_extra<-unique(data_int3_inc_extra)
data_int3_inc<-data_int3_inc_extra


data_int3_inc$Intervention<-data_int3_inc$Sum_Intervention3
data_int3_inc$Intervention<- as.character(data_int3_inc$Intervention)
data_int3_inc$Intervention<- as.factor(data_int3_inc$Intervention)
data_int3_inc$coverage<-data_int3_inc$coverage3
data_int3_inc$implementation_start<-data_int3_inc$implementation3_start
data_int3_inc$implementation_end<-data_int3_inc$implementation3_end
data_int3_inc$study_number_new<-data_int3_inc$study_number + 2*166
data_int3_inc$time_incidence<-data_int3_inc$time_incidence_int3
data_int3_inc$transmission_falc<-data_int3_inc$trans_falc_int3
data_int3_inc$transmission_viv<-data_int3_inc$trans_viv_int3


##########
#fourth intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int4_inc$study_number)
w<-c()
for (val in s)
{w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_incidence_int4<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_incidence_int4>-6])}
#for (val in k)
#{print(data$time_incidence_int4[data$ID==val])}
data_int4_inc_extra<-data_int4_inc
for (val in k)
{data_int4_inc_extra<-rbind(data_int4_inc_extra,data[data$ID==val,])}
data_int4_inc_extra<-unique(data_int4_inc_extra)
data_int4_inc<-data_int4_inc_extra


data_int4_inc$Intervention<-data_int4_inc$Sum_Intervention4
data_int4_inc$Intervention<- as.character(data_int4_inc$Intervention)
data_int4_inc$Intervention<- as.factor(data_int4_inc$Intervention)
data_int4_inc$coverage<-data_int4_inc$coverage4
data_int4_inc$implementation_start<-data_int4_inc$implementation4_start
data_int4_inc$implementation_end<-data_int4_inc$implementation4_end
data_int4_inc$study_number_new<-data_int4_inc$study_number + 3*166
data_int4_inc$time_incidence<-data_int4_inc$time_incidence_int4
data_int4_inc$transmission_falc<-data_int4_inc$trans_falc_int4
data_int4_inc$transmission_viv<-data_int4_inc$trans_viv_int4




#rbind the datasets
data_new_inc<-rbind(data_int1_inc, data_int2_inc, data_int3_inc, data_int4_inc)


#only keep one time point before, for kondrashin repeated MDA before because had first MDA 3-6 months before
data_new_inc<-subset(data_new_inc, subset=!(study_number_new==268&time_incidence<(-1)))
data_new_inc<-subset(data_new_inc, subset=!(study_number_new==269&time_incidence<(-1)))
data_new_inc<-subset(data_new_inc, subset=!(study_number_new==270&time_incidence<(-1)))
data_new_inc<-subset(data_new_inc, subset=!(study_number_new==271&time_incidence<(-1)))



#get the coverage coded:
#LLIN
data_new_inc$coverage[data_new_inc$coverage=="'extensive' distribution"]<-'high'
data_new_inc$coverage[data_new_inc$coverage=='1 net per 2.5 for the whole village']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='10% of commuity got nets and then looked at these (so no community effect) compared to other people. So in people who were in bed net group all of them had nets']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='2.3 ITN per household']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='all households got them depending on household size, however LLIN use lower half a year around 50%, then 26% and in second yar only around 6-8%']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='all villagers according to family size etc, usage depended on season, above 80% in rainy,58.6 in cool and around 50 in hot']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='all villagers according to family size etc, usage: in rainy season 93.7%, cool season 46%, hot season 32.1%']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='around 25% covered afterwards']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='around 50% of population covered afterwards']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='between 96% and 715 ownership and usage between 67% and 27%']<-'low'
data_new_inc$coverage[data_new_inc$coverage=="high ('nearly universal')"]<-'high'
data_new_inc$coverage[data_new_inc$coverage=='in Lihir villages 36-89% of households with one net']<-'high'
data_new_inc$coverage[data_new_inc$coverage=="in all of meghalaya state: 900'000 nets for 3.5 mio people. According to number and ages of individuals in household)"]<-'low'
data_new_inc$coverage[data_new_inc$coverage=='increase from 40% of households at risk to 100% in 2011']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='missing but in one commune had high coverage with only 2.5% of households no nets']<-'high'
data_new_inc$coverage[data_new_inc$coverage=="missing, MP at villlage and villages not too big, so covers all?, says 'LLINs were distributed to all hosueholds at M0'"]<-'high'
data_new_inc$coverage[data_new_inc$coverage=='self repordet: 77%, 89%, 86% in 2012,13,14']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='self reported use after distribution:69% in 2013 and 48% in 2014']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='self reported use: 69%']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='self reported: 47% and 48% in 2013 and 2014']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='self reported: 73%,53%,61% in 2011,12,13']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='selfrepored use99%']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='selfreported use: always above 98% in 2012 until 2014']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='LLINs to all households']<-'high'

#net retreatment
data_new_inc$coverage[data_new_inc$coverage=='64% (for incidenc eonly looked at families who retreated in 94 so would be 100% there)']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='87%']<-'high'

#increased case management
data_new_inc$coverage[data_new_inc$coverage=='60-90% of population tested, and treated']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='assume high as not many people living at farm around 150 --< could easly cover all?']<-'high'


#MDA with cut of at 90% covered at least once
data_new_inc$coverage[data_new_inc$coverage=='29% three rounds, 18% two, 23% one']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='57% three rounds, 18% two, 17% one']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='80% of censued people. Estimated 70% of population']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='90.9% at least one round, 69.5% two and 52.5% three rounds']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='estimated 85%']<-'low'
data_new_inc$coverage[data_new_inc$coverage=='one round 91% three rounds 64%']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='86.8% at least one round, 74% two and 60.3% three rounds']<-'low'




#IRS
data_new_inc$coverage[data_new_inc$coverage=='91% of houses sprayed and 97.5% the following year']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='96%']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='97%']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='around 80% for DDT, almost 100% in 88']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='around 80%. Around 96% for 88']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='high (says all the people with 92% of rooms coverede)']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='over 90%']<-'high'
data_new_inc$coverage[data_new_inc$coverage=='always mor ethan 90%']<-'high'
data_new_inc$coverage<-as.factor(data_new_inc$coverage)



data_new_inc$coverage<-as.character(data_new_inc$coverage)
x<-c(1:length(data_new_inc$Intervention))
for (val in x){
  if(data_new_inc$Intervention[val]=='control'){data_new_inc$coverage[val]<-data_new_inc$coverage[val+1]}}

data_new_inc$coverage<-as.factor(data_new_inc$coverage)




#see if the the season of survey span different seasons: for incidence
data_new_inc$season_span<-data_new_inc$season_survey_incidence
data_new_inc$season_span<-as.character(data_new_inc$season_span)
data_new_inc$season_span<-'NA'


x<-c(1:max(data_new_inc$study_number_new))
for (val in x)
{n<-c()
n<-unique(data_new_inc$season_survey_incidence[data_new_inc$study_number_new==val])
n<-paste(n,collapse=" ")
data_new_inc$season_span[data_new_inc$study_number_new==val]<-as.character(n)}

data_new_inc$season_span[data_new_inc$season_span=='wet dry']<- 'yes'
data_new_inc$season_span[data_new_inc$season_span=='dry wet']<- 'yes'
data_new_inc$season_span[data_new_inc$season_span=='both dry']<- 'yes'
data_new_inc$season_span[data_new_inc$season_span=='dry both']<- 'yes'
data_new_inc$season_span[data_new_inc$season_span=='wet']<- 'no'
data_new_inc$season_span[data_new_inc$season_span=='dry']<- 'no'
data_new_inc$season_span[data_new_inc$season_span=='wet both']<- 'yes'
data_new_inc$season_span[data_new_inc$season_span=='both']<- 'no'
data_new_inc$season_span[data_new_inc$season_span=='missing both']<- 'unknown'


data_new_inc$season_span<-as.factor(data_new_inc$season_span)


#quantify transmission
data_new_inc$transmission_study<-paste(data_new_inc$transmission_falc,data_new_inc$transmission_viv)
data_new_inc$transmission_study<-as.factor(data_new_inc$transmission_study)




#############
##prevalence data
#############

#make subsets but only with data points that follow on one intervention, also last data point before intervention happened
data_int1_prev<-subset(data, ((data$time_prevalence_int1[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1])& data$time_prevalence_int1[data$row_number]>0)) & ((data$time_prevalence_int1<=0 & (data$time_prevalence_int2[data$row_number+1]<=0 | is.na(data$time_prevalence_int2[data$row_number+1]))) | ((data$time_prevalence_int2<=0 | is.na(data$time_prevalence_int2)) & data$time_prevalence_int1>0))| ((data$Sum_Intervention1[row_number]=='control' | data$Sum_Intervention1[data$row_number_smaller]=='control') & data$prevalence_YN[row_number]=='WAHR')))
data_int2_prev<-subset(data, ((data$time_prevalence_int2[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1])& data$time_prevalence_int2[data$row_number]>0)) & ((data$time_prevalence_int2<=0 & (data$time_prevalence_int3[data$row_number+1]<=0 | is.na(data$time_prevalence_int3[data$row_number+1]))) | ((data$time_prevalence_int3<=0 | is.na(data$time_prevalence_int3)) & data$time_prevalence_int2>0))| ((data$Sum_Intervention2[row_number]=='control' | data$Sum_Intervention2[data$row_number_smaller]=='control') & data$prevalence_YN[row_number]=='WAHR')))
data_int3_prev<-subset(data, ((data$time_prevalence_int3[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1])& data$time_prevalence_int3[data$row_number]>0)) & ((data$time_prevalence_int3<=0 & (data$time_prevalence_int4[data$row_number+1]<=0 | is.na(data$time_prevalence_int4[data$row_number+1]))) | ((data$time_prevalence_int4<=0 | is.na(data$time_prevalence_int4)) & data$time_prevalence_int3>0))| ((data$Sum_Intervention3[row_number]=='control' | data$Sum_Intervention3[data$row_number_smaller]=='control') & data$prevalence_YN[row_number]=='WAHR')))
data_int4_prev<-subset(data, ((data$time_prevalence_int4[data$row_number+1]>0 | ((data$study_number[data$row_number] != data$study_number[data$row_number + 1])& data$time_prevalence_int4[data$row_number]>0)) & ((data$time_prevalence_int4<=0 & (data$time_prevalence_int5[data$row_number+1]<=0 | is.na(data$time_prevalence_int5[data$row_number+1]))) | ((data$time_prevalence_int5<=0 | is.na(data$time_prevalence_int5)) & data$time_prevalence_int4>0))| ((data$Sum_Intervention4[row_number]=='control' | data$Sum_Intervention4[data$row_number_smaller]=='control') & data$prevalence_YN[row_number]=='WAHR')))


#######
#first intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int1_prev$study_number)
w<-c()
for (val in s)
{w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_prevalence_int1<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_prevalence_int1>-6])}
#for (val in k)
#{print(data$time_prevalence_int1[data$ID==val])}

data_int1_prev_extra<-data_int1_prev
for (val in k)
{data_int1_prev_extra<-rbind(data_int1_prev_extra,data[data$ID==val,])}
data_int1_prev_extra<-unique(data_int1_prev_extra)
data_int1_prev<-data_int1_prev_extra


data_int1_prev$Intervention<-data_int1_prev$Sum_Intervention1
data_int1_prev$Intervention<- as.character(data_int1_prev$Intervention)
data_int1_prev$Intervention<- as.factor(data_int1_prev$Intervention)
data_int1_prev$implementation_start<-data_int1_prev$implementation1_start
data_int1_prev$implementation_end<-data_int1_prev$implementation1_end
data_int1_prev$coverage<-data_int1_prev$coverage1
data_int1_prev$study_number_new<-data_int1_prev$study_number
data_int1_prev$time_prevalence<-data_int1_prev$time_prevalence_int1
data_int1_prev$transmission_falc<-data_int1_prev$trans_falc_int1
data_int1_prev$transmission_viv<-data_int1_prev$trans_viv_int1

#######
#second intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int2_prev$study_number)
w<-c()
for (val in s)
{w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_prevalence_int2<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_prevalence_int2>-6])}
#for (val in k)
#{print(data$time_prevalence_int2[data$ID==val])}

data_int2_prev_extra<-data_int2_prev
for (val in k)
{data_int2_prev_extra<-rbind(data_int2_prev_extra,data[data$ID==val,])}
data_int2_prev_extra<-unique(data_int2_prev_extra)
data_int2_prev<-data_int2_prev_extra

data_int2_prev$Intervention<-data_int2_prev$Sum_Intervention2
data_int2_prev$Intervention<- as.character(data_int2_prev$Intervention)
data_int2_prev$Intervention<- as.factor(data_int2_prev$Intervention)
data_int2_prev$coverage<-data_int2_prev$coverage2
data_int2_prev$implementation_start<-data_int2_prev$implementation2_start
data_int2_prev$implementation_end<-data_int2_prev$implementation2_end
data_int2_prev$study_number_new<-data_int2_prev$study_number + 1*182
data_int2_prev$time_prevalence<-data_int2_prev$time_prevalence_int2
data_int2_prev$transmission_falc<-data_int2_prev$trans_falc_int2
data_int2_prev$transmission_viv<-data_int2_prev$trans_viv_int2


#######
#third intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int3_prev$study_number)
w<-c()
for (val in s)
{w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_prevalence_int3<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_prevalence_int3>-6])}
#for (val in k)
#{print(data$time_prevalence_int3[data$ID==val])}

data_int3_prev_extra<-data_int3_prev
for (val in k)
{data_int3_prev_extra<-rbind(data_int3_prev_extra,data[data$ID==val,])}
data_int3_prev_extra<-unique(data_int3_prev_extra)
data_int3_prev<-data_int3_prev_extra


data_int3_prev$Intervention<-data_int3_prev$Sum_Intervention3
data_int3_prev$Intervention<- as.character(data_int3_prev$Intervention)
data_int3_prev$Intervention<- as.factor(data_int3_prev$Intervention)
data_int3_prev$coverage<-data_int3_prev$coverage3
data_int3_prev$implementation_start<-data_int3_prev$implementation3_start
data_int3_prev$implementation_end<-data_int3_prev$implementation3_end
data_int3_prev$study_number_new<-data_int3_prev$study_number + 2*182
data_int3_prev$time_prevalence<-data_int3_prev$time_prevalence_int3
data_int3_prev$transmission_falc<-data_int3_prev$trans_falc_int3
data_int3_prev$transmission_viv<-data_int3_prev$trans_viv_int3



#######
#fourth intervention: this code is to find more baseline data points but only up until 6 months before intervention
s<-unique(data_int4_prev$study_number)
w<-c()
for (val in s)
{w<-c(w,data$ID[data$study_number==val])}
g<-c()
for (val in w)
{g<-c(g,data$ID[data$ID==val&data$time_prevalence_int4<=0])}
k<-c() 
for (val in g)
{k<-c(k,data$ID[data$ID==val&data$time_prevalence_int4>-6])}
#for (val in k)
#{print(data$time_prevalence_int4[data$ID==val])}

data_int4_prev_extra<-data_int4_prev
for (val in k)
{data_int4_prev_extra<-rbind(data_int4_prev_extra,data[data$ID==val,])}
data_int4_prev_extra<-unique(data_int4_prev_extra)
data_int4_prev<-data_int4_prev_extra


data_int4_prev$Intervention<-data_int4_prev$Sum_Intervention4
data_int4_prev$Intervention<- as.character(data_int4_prev$Intervention)
data_int4_prev$Intervention<- as.factor(data_int4_prev$Intervention)
data_int4_prev$coverage<-data_int4_prev$coverage4
data_int4_prev$implementation_start<-data_int4_prev$implementation4_start
data_int4_prev$implementation_end<-data_int4_prev$implementation4_end
data_int4_prev$study_number_new<-data_int4_prev$study_number + 3*182
data_int4_prev$time_prevalence<-data_int4_prev$time_prevalence_int4
data_int4_prev$transmission_falc<-data_int4_prev$trans_falc_int4
data_int4_prev$transmission_viv<-data_int4_prev$trans_viv_int4



#rbind the datasets together
data_new_prev<-rbind(data_int1_prev, data_int2_prev, data_int3_prev, data_int4_prev)

#excluding metseelar for MDA because timing cant be pinpointed well enough
data_new_prev<-subset(data_new_prev, study_number_new!=225)
data_new_prev<-subset(data_new_prev, study_number_new!=407)

#exclude the ones that either have no cases before or no cases after the implementation of interventions
data_new_prev<-subset(data_new_prev, study_number_new!=38)
data_new_prev<-subset(data_new_prev, study_number_new!=22)
data_new_prev<-subset(data_new_prev, study_number_new!=170)
data_new_prev<-subset(data_new_prev, study_number_new!=182)
data_new_prev<-subset(data_new_prev, study_number_new!=167)
data_new_prev<-subset(data_new_prev, study_number_new!=168)
data_new_prev<-subset(data_new_prev, study_number_new!=171)
data_new_prev<-subset(data_new_prev, study_number_new!=176)
data_new_prev<-subset(data_new_prev, study_number_new!=204)
data_new_prev<-subset(data_new_prev, study_number_new!=364)



#recoding of coverage
#LLIN
data_new_prev$coverage[data_new_prev$coverage=="'high'"]<-'high'
data_new_prev$coverage[data_new_prev$coverage=='0.94 nets per villager']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='10% of commuity got nets and then looked at these (so no community effect) compared to other people. So in people who were in bed net group all of them had nets']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='375 nets for 580 people']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='52% bought net']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='56% used nets (before only 33%), but 80% reported to having rceived a net, but only 60% of remporary workers']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='59% bought net']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='68% bought net']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='71% covered with nets afterwards but only 27% use']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='91% covered afterwards but only 60% use']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='91% covered but only 61% use']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='92% covered but only 49% use']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='92% covered but only 72% use']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='94.9% reported use of nets']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='at least one LLIN per household but tried 1 per 2 people']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='each child received one net, bigger if slept with more than 2 people, only covered and also only tested children']<-'high'
data_new_prev$coverage[data_new_prev$coverage=="high ('nearly universal')"]<-'high'
data_new_prev$coverage[data_new_prev$coverage=='high, 98.6 reported using net in final survey']<-'high'
data_new_prev$coverage[data_new_prev$coverage=="missing, MP at villlage and villages not too big, so covers all?, says 'LLINs were distributed to all hosueholds at M0'"]<-'high'
data_new_prev$coverage[data_new_prev$coverage=='selfreported use above 90%']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='39% bought net']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='LLINs to all households']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='104']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='105']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='117']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='118']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='12']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='122']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='18']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='20']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='37']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='38']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='42']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='55']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='7']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='74']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='80']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='92']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='96']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='98']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='36.70%']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='38.70%']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='43.30%']<-'low'

#MDA, cut of 90% at least one round
data_new_prev$coverage[data_new_prev$coverage=='29% three rounds, 18% two, 23% one']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='57% three rounds, 18% two, 17% one']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='90.9% at least one round, 69.5% two and 52.5% three rounds']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='97.2% (93.1-100)']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='adults 72%,76% and 78%fully treated, children 85%, 84% 85% fully treated, also some partially treated']<-'low'
data_new_prev$coverage[data_new_prev$coverage=='all of the children that were followed completed treatment (but other people in village were not covered)']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='average attendance 90%']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='38.6% at least one dose']<-'low'

#increased case management
data_new_prev$coverage[data_new_prev$coverage=='mean screening 89%']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='mean screening87%']<-'high'


#net retreatment
data_new_prev$coverage[data_new_prev$coverage=='64% (for incidenc eonly looked at families who retreated in 94 so would be 100% there)']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='87%']<-'high'

#IRS
data_new_prev$coverage[data_new_prev$coverage=='95% (85%-100%)']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='95.3% of houses sprayed and second round 87%']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='96%']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='97%']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='97% (92-100%)']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='all houses']<-'high'
data_new_prev$coverage[data_new_prev$coverage=='always mor ethan 90%']<-'high'





data_new_prev$coverage<-as.factor(data_new_prev$coverage)

data_new_prev$coverage<-as.character(data_new_prev$coverage)
x<-c(1:length(data_new_prev$Intervention))
for (val in x){
  if(data_new_prev$Intervention[val]=='control'){data_new_prev$coverage[val]<-data_new_prev$coverage[val+1]}}

data_new_prev$coverage<-as.factor(data_new_prev$coverage)



#see if the the season of survey span different seasons: for prevalence
data_new_prev$season_span<-data_new_prev$season_survey_prevalence
data_new_prev$season_span<-as.character(data_new_prev$season_span)
data_new_prev$season_span<-'NA'

x<-c(1:max(data_new_prev$study_number_new))
for (val in x)
{n<-c()
n<-unique(data_new_prev$season_survey_prevalence[data_new_prev$study_number_new==val])
n<-paste(n,collapse=" ")
data_new_prev$season_span[data_new_prev$study_number_new==val]<-as.character(n)}

data_new_prev$season_span[data_new_prev$season_span=='wet dry']<- 'yes'
data_new_prev$season_span[data_new_prev$season_span=='dry wet']<- 'yes'
data_new_prev$season_span[data_new_prev$season_span=='dry both']<- 'yes'
data_new_prev$season_span[data_new_prev$season_span=='dry wet both']<- 'yes'
data_new_prev$season_span[data_new_prev$season_span=='both dry']<- 'yes'
data_new_prev$season_span[data_new_prev$season_span=='wet']<- 'no'
data_new_prev$season_span[data_new_prev$season_span=='dry']<- 'no'
data_new_prev$season_span[data_new_prev$season_span=='wet both']<- 'yes'
data_new_prev$season_span[data_new_prev$season_span=='both']<- 'no'
data_new_prev$season_span[data_new_prev$season_span=='missing both']<- 'unknown'
data_new_prev$season_span[data_new_prev$season_span=='missing']<- 'unknown'
data_new_prev$season_span[data_new_prev$season_span=='missing wet']<- 'unknown'


data_new_prev$season_span<-as.factor(data_new_prev$season_span)


#quantify transmission: 4 categories
data_new_prev$transmission_study<-paste(data_new_prev$transmission_falc,data_new_prev$transmission_viv)
data_new_prev$transmission_study<-as.factor(data_new_prev$transmission_study)


#exclude studies that are not included
#LOHA master study number 51: the one where there is LLIN and IRS pretty close together
data_new_inc$ID[data_new_inc$study_number_new==149]
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
b<-data_new_inc$row_number[data_new_inc$study_number_new==149]
data_new_inc<-data_new_inc[-c(b),]
data_new_inc$row_number<-seq.int(nrow(data_new_inc))

#exclude this LOHA one with master study number 51 for IRS as well...
data_new_inc$ID[data_new_inc$study_number_new==315]
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
b<-data_new_inc$row_number[data_new_inc$study_number_new==315]
data_new_inc<-data_new_inc[-c(b),]
data_new_inc$row_number<-seq.int(nrow(data_new_inc))


#because there are two 183 make ome-kaius one to 184!
v<-unique(data_new_prev[,c('first_authors',"study_number_new")])
v$study_number_new[duplicated(v$study_number_new)]
data_new_prev$study_number_new[data_new_prev$study_number_new==183&data_new_prev$first_authors=='Ome-Kaius, Maria']<-184

v<-unique(data_new_inc[,c('first_authors',"study_number_new")])
v$study_number_new[duplicated(v$study_number_new)]
remove(v)

