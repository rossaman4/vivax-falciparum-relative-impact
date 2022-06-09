#########
#IRS, incidence, individual
########


source("preparation_analysis.R")
library(stringr)


#adding row numbers
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1

#generating individual plot titles
first_row_study<-data_new_inc[!duplicated(data_new_inc$study_number_new),]
first_row_study$row_number<-seq.int(nrow(first_row_study))
first_row_study$study_area_add<-gsub(".*,","", x=first_row_study$study_area)
first_row_study$study_area_add<-str_trim(first_row_study$study_area_add)
first_row_study$study_area_add<-str_replace(first_row_study$study_area_add, "^\\w{1}", toupper)
first_row_study$study_area<-gsub(",.*","", x=first_row_study$study_area)
first_row_study$first_authors<-gsub(",.*","", x=first_row_study$first_authors)
first_row_study$study_area[first_row_study$study_area=='Papua New Guinea']<-'PNG'
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors)
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors, '\n', first_row_study$study_area_add)


########
#IRS first time
######

#this gives the series of time points that need to be plotted
unique(data_inc_IRS_first$study_number_new)

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32"), number =c(60,61,92,141),letter=c('a)','b)','c)','d)'))



#adding the incidence rate data

data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0
x<-c(60,61,92,141)

tiff("individual_spag_IRS_Inc.tiff", width = 7, height = 5, units = 'in', res = 700, pointsize=8)
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(4,4))
for (val in x)
{plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n',xlim= c(0,40), cex.main=0.9) 
   ifelse((val==60), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
    axis(1,labels=NA, las=1)
    points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS 1st time' & !is.na(data_new_inc$incidence_rate_vivax)),], col=ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS 1st time'& !is.na(data_new_inc$case_numbers_vivax)),],col=ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
           data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS 1st time') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS 1st time' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS 1st time',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
           data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS 1st time') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS 1st time' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS 1st time',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    text(35,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
    abline(h = 0, col = "gray80")
    }
mtext(text=expression('proportion of clinical cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
mtext(text='time since IRS in months',side=1, line=2, cex=1, outer=TRUE)


########
#IRS repeated time - on the same plot
######

#this gives the series of time points that need to be plotted
unique(data_inc_IRS_rep$study_number_new)


fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c","#9c496c","#9c496c"), number =c(93, 305, 306),letter=c('e)','f)','g)'))


#adding the incidence rate data
data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0
x<-c(93, 305, 306)

for (val in x)
{plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1),xlim=c(0,40), bty='n', yaxt='n', xaxt='n', cex.main=0.9) 
  ifelse((val==93), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
  axis(1,labels=NA)
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS repeated time' & !is.na(data_new_inc$incidence_rate_vivax)),], col= ifelse(relapse_pattern_white=='frequent',yes='darkred',no=ifelse(relapse_pattern_white=='both',yes='darkred',no='darkred')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS repeated time'& !is.na(data_new_inc$case_numbers_vivax)),],col= ifelse(relapse_pattern_white=='frequent',yes='darkred',no=ifelse(relapse_pattern_white=='both',yes='darkred',no='darkred')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS repeated time') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS repeated time' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS repeated time',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS repeated time') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS repeated time' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS repeated time',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  text(35,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80")}


########
#all other IRS - on the same plot
######

#this gives the series of time points that need to be plotted
unique(data_inc_IRS$study_number_new)

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32",
                                      "#627bc8",
                                      "#90ac50",
                                      "#cf479a",
                                      "#9c496c",
                                      "#5ab74d"
                                      ), number =c(53,62,73,74,116,122,123,134,152),letter=c('h)','i)','j)','k)','l)', 'm)','n)','o)','p)'))



data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0
x<-c(53,62,73,74,116,122,123,134,152)

for (val in x)
{plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1),xlim=c(0,40), bty='n', yaxt='n', xaxt='n', cex.main=0.9) 
  ifelse((val==62|val==122), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
  ifelse((val==123|val==152|val==134|val==122), axis(1,labels=TRUE, las=1), axis(1,labels=NA))
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS' & !is.na(data_new_inc$incidence_rate_vivax)),], col= ifelse(relapse_pattern_white=='frequent',yes='darkgreen',no=ifelse(relapse_pattern_white=='both',yes='darkgreen',no='darkgreen')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS'& !is.na(data_new_inc$case_numbers_vivax)),],col= ifelse(relapse_pattern_white=='frequent',yes='darkgreen',no=ifelse(relapse_pattern_white=='both',yes='darkgreen',no='darkgreen')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS',yes='darkgreen',no='darkgreen')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS',yes='darkgreen',no='darkgreen')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  text(35,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80")
  }

legend('bottomright',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('navy'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=1)

dev.off()

