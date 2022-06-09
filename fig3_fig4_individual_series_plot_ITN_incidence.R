#########
##ITN, incidence, individual plots
########


source('preparation_analysis.r')
library(stringr)

#adding row numbers
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1
#name for plots
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
#LLIN first time
######

#this gives the series of time points that need to be plotted
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
                                      "#8e5bc8"), number =c(47,49,50,51,52,113, 114, 115,112,138, 244, 245, 150,   230, 231, 232, 233, 234, 235, 236, 237, 238,   1,  23,57, 145,146,111), letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)','n)','o)','p)','q)','r)','s)','t)','u)','v)','w)','x)','y)','z)', 'aa)', 'untreated \n nets'))




x<-c(47,49,50,51,52,113,114,115,112,138,244, 245, 150,230, 231, 232, 233, 234, 235, 236, 237, 238,   1,  23,57, 145)

#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(4,7))

data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0

for (val in x)
{plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_incidence,type='n', data=data_new_inc, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n', xlim=c(0,60), cex.main=0.8) 
  ifelse((val==231|val==115|val==47|val==238), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
  ifelse((val==1|val==23|val==57|val==238|val==237|val==145|val==236), axis(1,labels= c('0','','20','','40','','60'), las=1, at=c(0,10,20,30,40,50,60)), axis(1,labels=NA,at=c(0,10,20,30,40,50,60)))
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN 1st dist' & !is.na(data_new_inc$incidence_rate_vivax)),], col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_incidence<=24,yes=16,no=1), cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN 1st dist'& !is.na(data_new_inc$case_numbers_vivax)),],col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_incidence<=24,yes=16,no=1), cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN 1st dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN 1st dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no='navy'), pch= ifelse(Intervention=='control',yes=ifelse(time_incidence<=24,yes=18,no=5),no=ifelse(time_incidence<=24,yes=16,no=1)),cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN 1st dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN 1st dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no='navy'), pch= ifelse(Intervention=='control',yes=ifelse(time_incidence<=24,yes=18,no=5),no=ifelse(time_incidence<=24,yes=16,no=1)),cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  text(55,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80") 
  }
  mtext(text=expression('proportion of cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
  mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)

plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('navy'), pch=c(16), pt.cex=c(0.75, 1, 1.25, 1.5, 1.75), cex=0.9)
  
  
  
 

########
#LLIN repeated time
######
  
#this gives the series of time points that need to be plotted
unique(data_inc_LLIN_rep$study_number_new)
  

#making data frame for
#for 13 colours

#data_store<-data_new_inc
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
                                      "#9c496c"), number =c(219,410,411,10,12,13,15,16,55,179,180,167,223),letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)'))


data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0

x<-c(219,410,411,10,12,13,15,16,55,179,180,167,223)


par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(3,5))
for (val in x)
{plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n', xlim=c(0,40), cex.main=0.8) 
data_new_inc$time_incidence[data_new_inc$time_incidence<0]<-0
ifelse((val==219|val==13|val==180), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
  ifelse((val==223|val==167|val==180|val==179|val==55), axis(1,labels=TRUE, las=1), axis(1,labels=NA))
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN later dist' & !is.na(data_new_inc$incidence_rate_vivax)),], col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_incidence<=24,yes=16,no=1), cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='LLIN later dist'& !is.na(data_new_inc$case_numbers_vivax)),],col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_incidence<=24,yes=16,no=1), cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN later dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN later dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=18,no=16),cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='LLIN later dist') | (data_new_inc$Intervention[data_new_inc$row_number]=='LLIN later dist' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=18,no=16),cex=case_numbers_total_cat_inc[data_new_inc$row_number])
  text(35,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80") 
   }
mtext(text=expression('proportion of cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)
plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('navy'), pch=c(16), pt.cex=c(0.75, 1, 1.25, 1.5, 1.75), cex=0.9)







