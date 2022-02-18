#####
#MDA, prevalence, not_individual
######


source("preparation_analysis.R")


#adding row numbers
data_new_prev<-data_new_prev[order(data_new_prev$ID),]
data_new_prev$row_number<-seq.int(nrow(data_new_prev))
first_row_study<-data_new_prev[!duplicated(data_new_prev$study_number_new),]
first_row_study$row_number<-seq.int(nrow(first_row_study))
first_row_study$study_area<-gsub(",.*","", x=first_row_study$study_area)
first_row_study$first_authors<-gsub(",.*","", x=first_row_study$first_authors)
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors)
data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0

#to make the connections nice for control studies
data_new_prev$ID[data_new_prev$study_number_new==166]
data_new_prev$study_number_new[data_new_prev$ID==1858]<-1660
data_new_prev$study_number_new[data_new_prev$ID==1859]<-1660
data_new_prev$study_number_new[data_new_prev$ID==1860]<-1661
data_new_prev$study_number_new[data_new_prev$ID==1861]<-1661
data_new_prev$study_number_new[data_new_prev$ID==1862]<-1662
data_new_prev$study_number_new[data_new_prev$ID==1863]<-1662
data_new_prev$study_number_new[data_new_prev$ID==1864]<-1663
data_new_prev$study_number_new[data_new_prev$ID==1865]<-1663
data_new_prev_store<-data_new_prev

########
##MDA first dist (only first ones were found)
#####

#the series that need plotting, but 97 not plotted because no cases found within the time analyzed
unique(data_prev_MDA_first$study_number_new)


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
                                      "#ca87cb"), number =c(76,89,90,97,225,144, 191, 329,166,94,95,96),letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)'))



x<-c(76,89,90,144, 191, 329,94,95,96)


plot_spaghetti<-function(y)
{data_new_prev$row_number<-seq.int(nrow(data_new_prev))
data_new_prev$row_number_smaller<-data_new_prev$row_number-1
data_new_prev$row_number_smaller[data_new_prev$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,15), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
        data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<=3,yes=16,no=1), type='b', cex=case_numbers_total_cat_prev[data_new_prev$row_number], lwd=2)
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<=3,yes=16,no=1), type='b',cex=case_numbers_total_cat_prev[data_new_prev$row_number], lwd=2)
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<=3,yes=16,no=1), type='b',cex=case_numbers_total_cat_prev[data_new_prev$row_number], lwd=2)
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<=3,yes=16,no=1), type='b',cex=case_numbers_total_cat_prev[data_new_prev$row_number], lwd=2)
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  if(val==166)
  {t<-c(1660:1663)
  for(value in t)
    {points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
           data=data_new_prev[c(data_new_prev$study_number_new==value) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==value) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==value) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==value) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b', lwd=2)
  }}}}



##########
##relapse pattern
#######

#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(4,4))
data_new_prev<-data_new_prev_store
data_new_prev<-data_new_prev[data_new_prev$relapse_pattern_white=='frequent',]
y<-'frequent relapses'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$relapse_pattern_white=='long',]
y<-'long latency relapses'
plot_spaghetti(y)

data_new_prev<-data_new_prev_store[data_new_prev_store$relapse_pattern_white=='both',]
y<-'both relapse patterns'
plot_spaghetti(y)

plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')


##########
##coverage
#########

data_new_prev<-data_new_prev_store[data_new_prev_store$coverage=='low',]
y<-'low coverage'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$coverage=='high',]
y<-'high coverage'
plot_spaghetti(y)

data_new_prev<-data_new_prev_store[data_new_prev_store$coverage=='missing',]
y<-'missing coverage'
plot_spaghetti(y)

plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')


##########
##transmission
#########
#data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='low low',]
#y<-'low Pf, low Pv'
#plot_spaghetti(y)
#axis(2,labels=TRUE, las=1)
#axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='low high',]
y<-'low Pf, high Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=NA, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high low',]
y<-'high Pf, low Pv'
plot_spaghetti(y)
axis(1,labels=NA, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high high',]
y<-'high Pf, high Pv'
plot_spaghetti(y)
axis(1,labels=NA, las=1)

plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')


#######
##initial proportion
#######
data_new_prev<-data_new_prev_store[data_new_prev_store$initial_proportion_cat=='low',]
y<-'low initial proportion'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$initial_proportion_cat=='high',]
y<-'high initital proportion'
plot_spaghetti(y)
#axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

##########
##seasonality
##########

data_new_prev<-data_new_prev_store[data_new_prev_store$seasonality=='low',]
y<-'low seasonality'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$seasonality=='high',]
y<-'high seasonality'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

mtext(text='time since MDA in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of patent infections that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)





