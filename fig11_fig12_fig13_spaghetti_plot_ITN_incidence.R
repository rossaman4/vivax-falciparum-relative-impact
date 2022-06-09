#########
# ITN, incidence, spaghetti by factor
########


source("preparation_analysis.R")

#adding row numbers
data_new_inc<-data_new_inc[order(data_new_inc$ID),]
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1

first_row_study<-data_new_inc[!duplicated(data_new_inc$study_number_new),]
first_row_study$row_number<-seq.int(nrow(first_row_study))
first_row_study$study_area<-gsub(",.*","", x=first_row_study$study_area)
first_row_study$study_area[first_row_study$study_area=='Papua New Guinea']<-'PNG'
first_row_study$first_authors<-gsub(",.*","", x=first_row_study$first_authors)
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors)

data_new_inc$study_number_new[data_new_inc$ID==1565]<-13800
data_new_inc$study_number_new[data_new_inc$ID==1566]<-13800


data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0
data_new_inc_store<-data_new_inc



########
# ITN first time
######

#series of time points that need plotting
unique(data_inc_LLIN_first$study_number_new)

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32",
                                      "#627bc8",
                                      "#90ac50",
                                      "#cf479a",
                                      "#53bd9b",
                                      "#cd4733",
                                      "#4eacd7",
                                      "#c67439",
                                      "#ca87cb",
                                      "#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32",
                                      "#627bc8",
                                      "#90ac50",
                                      "#cf479a",
                                      "#53bd9b",
                                      "#cd4733",
                                      "#4eacd7",
                                      "#c67439",
                                      "#ca87cb",
                                      "#9c496c",
                                      "#5ab74d",
                                      "#9c496c",
                                      "#8e5bc8"), number =c(47,49,50,51,52,113, 114, 115,112,138, 244, 245, 150,   230, 231, 232, 233, 234, 235, 236, 237, 238,   1,  23,57,145,146,111), letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)','n)','o)','p)','q)','r)','s)','t)','u)','v)','w)','x)','y)','z)', 'aa)', 'untreated \n nets'))



#function
x<-c(47,49,50,51,52,113,114,115,112,138,244, 245, 150,230, 231, 232, 233, 234, 235, 236, 237, 238,   1,  23,57, 145)

plot_spaghetti<-function(y)
{data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1
plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, type='n', data=data_new_inc, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,60), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
        data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN 1st dist'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN 1st dist'& is.na(data_new_inc$case_numbers_vivax)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)  & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN 1st dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN 1st dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b')
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)& is.na(data_new_inc$case_numbers_vivax) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN 1st dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN 1st dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b')
}}




# -- Figure 11 ---


##########
#relapse pattern
##########
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(4,4))

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$relapse_pattern_white=='frequent',]
y<-'frequent relapses'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$relapse_pattern_white=='long',]
y<-'long latency relapses'
plot_spaghetti(y)

plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')


##########
#coverage
##########

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$coverage=='high',]
y<-'high coverage'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$coverage=='low',]
y<-'low coverage'
plot_spaghetti(y)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$coverage=='missing',]
y<-'missing coverage'
plot_spaghetti(y)

plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('black'), pch=c(16), pt.cex=c(0.75, 1, 1.25, 1.5, 1.75), cex=1.25)


##########
#seasonality
##########
data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$seasonality=='high',]
y<-'high seasonality'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$seasonality=='low',]
y<-'low seasonality'
plot_spaghetti(y)


########
##initial proportion
########
data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$initial_proportion_cat=='high',]
y<-'high initial proportion'
plot_spaghetti(y)
axis(2,labels=NA, las=1)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$initial_proportion_cat=='low',]
y<-'low initial proportion'
plot_spaghetti(y)



###########
#transmission
###########

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$transmission_study=='low low',]
y<-'low Pf, low Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$transmission_study=='low high',]
y<-'low Pf, high Pv'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$transmission_study=='high low',]
y<-'high Pf, low Pv'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$transmission_study=='high high',]
y<-'high Pf, high Pv'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)





# -- Figure 12 ---



########
#LLIN repeated time
######

#series of time points that need plotting
#not plotting 167 becazse time point is more than 24 months after intervention
unique(data_inc_LLIN_rep$study_number_new)

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#8b354d",
                                      "#00cc5c",
                                      "#7c31b7",
                                      "#66cb3e",
                                      "#4b56dc",
                                      "#a3d649",
                                      "#e31c99",
                                      "#008842",
                                      "#5f8aff",
                                      "#fabc3a",
                                      "#4447a7",
                                      "#8c8b00",
                                      "#0261bc"), number =c(10,12,13,15,16,55,179,180,219,223,410,411,167))


x<-c(10,12,13,15,16,55,179,180,219,223,410,411)

#adding the incidence rate data


plot_spaghetti<-function(y)
{data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1
plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, type='n', data=data_new_inc, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,60), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
        data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN later dist'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN later dist'& is.na(data_new_inc$case_numbers_vivax)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)  & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN later dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN later dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b')
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)& is.na(data_new_inc$case_numbers_vivax) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN later dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN later dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b')
  }}


##########
##relapse pattern
#######

quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(3,3))
data_new_inc<-data_new_inc_store
data_new_inc<-data_new_inc[data_new_inc$relapse_pattern_white=='frequent',]
y<-'frequent relapses'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)


data_new_inc<-data_new_inc_store[data_new_inc_store$relapse_pattern_white=='long',]
y<-'long latency relapses'
plot_spaghetti(y)

plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')

mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)



##########
##transmission
#######


data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='low low',]
y<-'low Pf, low Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
#axis(1,labels=TRUE, las=1)


#data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='low high',]
#y<-'low Pf, high Pv'
#plot_spaghetti(y)
#axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='high low',]
y<-'high Pf low Pv'
plot_spaghetti(y)
#axis(1,labels=TRUE, las=1)

#data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='high high',]
#y<-'high Pf high Pv'
#plot_spaghetti(y)
#axis(1,labels=TRUE, las=1)

plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('black'), pch=c(16), pt.cex=c(0.75, 1, 1.25, 1.5, 1.75), cex=1.5)



##########
##coverage
#######


data_new_inc<-data_new_inc_store[data_new_inc_store$coverage=='low',]
y<-'low coverage'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$coverage=='high',]
y<-'high coverage'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$coverage=='missing',]
y<-'missing coverage'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)





# -- Figure 13 ---


#######
###plots for first time vs second time LLIN
######

data_new_inc<-data_new_inc_store
#####first time

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#8b354d",
                                      "#00cc5c",
                                      "#7c31b7",
                                      "#66cb3e",
                                      "#4b56dc",
                                      "#a3d649",
                                      "#e31c99",
                                      "#008842",
                                      "#5f8aff",
                                      "#fabc3a",
                                      "#4447a7",
                                      "#8c8b00",
                                      "#0261bc",
                                      "#b62100",
                                      "#50d9da",
                                      "#ff7d54",
                                      "#01a4d3",
                                      "#7b5700",
                                      "#a39bff",
                                      "#ffb1bf",
                                      "#7bbeff",
                                      "#892f70",
                                      "#c99cff",
                                      "#774069",
                                      "#ff84b7",
                                      "#6d5c8d",
                                      "#4447a7"), number =c(47,49,50,51,52,57,149, 113, 114, 115, 138, 150, 112, 230, 231, 232, 233, 234, 235, 236, 237, 238,   1,  23, 244, 245, 13800), letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)','n)','o)','p)','q)','r)','s)','t)','u)','v)','w)','x)','y)','z)','k)'))


x<-c(47,49,50,51,52,113,114,115,112,138,244, 245, 150,230, 231, 232, 233, 234, 235, 236, 237, 238,   1,  23,57, 145)

data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_incidence[data_new_inc$time_incidence<0]<-0

data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1



#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(1,2))
plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_incidence,type='n', data=data_new_inc, main=expression('First time LLIN') , xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', las=1)
for (val in x)
{points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN 1st dist' & !is.na(data_new_inc$incidence_rate_vivax)&data_new_inc$Intervention[data_new_inc$row_number_smaller]!='control'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN 1st dist'& !is.na(data_new_inc$case_numbers_vivax)&data_new_inc$Intervention[data_new_inc$row_number_smaller]!='control'),],col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN 1st dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN 1st dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN 1st dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN 1st dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number]) 
}




##second time ITN


fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#8b354d",
                                      "#00cc5c",
                                      "#7c31b7",
                                      "#66cb3e",
                                      "#4b56dc",
                                      "#a3d649",
                                      "#e31c99",
                                      "#008842",
                                      "#5f8aff",
                                      "#fabc3a",
                                      "#4447a7",
                                      "#8c8b00",
                                      "#0261bc"), number =c(10,12,13,15,16,55,179,180,219,223,410,411,167))


x<-c(10,12,13,15,16,55,179,180,219,223,410,411)

plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_incidence,type='n', data=data_new_inc, main=expression('Repeated time ITN') , xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', las=1)
for (val in x)
{points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN later dist' & !is.na(data_new_inc$incidence_rate_vivax)&data_new_inc$Intervention[data_new_inc$row_number_smaller]!='control'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN later dist'& !is.na(data_new_inc$case_numbers_vivax)&data_new_inc$Intervention[data_new_inc$row_number_smaller]!='control'),],col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN later dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN later dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_incidence, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN later dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN later dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number]) 
}

mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)





