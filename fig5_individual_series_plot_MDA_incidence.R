########
##MDA, incidence, individual
########

source('data_preparation_and_cleaning.r')
source('reshaping_dataframe.r')
source('preparation_analysis.r')
library(stringr)


#adding row numbers
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1
#title for plots
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
first_row_study$plot_title[first_row_study$plot_title=="Brazil , McGreevy \n Costa Margue + Forte principe de beira + settlement along BR429"]<-"Brazil , McGreevy \n  Costa Margue + Forte + settlement"


########
#MDA first time
######

#this gives the series of time points that need to be plotted
unique(data_inc_MDA_first$study_number_new)

#making data frame for
#for 13 colours

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32",
                                      "#627bc8",
                                      "#90ac50",
                                      "#cf479a",
                                      "#9c496c",
                                      "#5ab74d",
                                      "#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32"
                                      ), number =c(30,75,102,103,104,105,106,107,108,109,137,311,146), letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)'))


#change time_zero
data_new_inc$time_incidence <- data_new_inc$date_start_incidence - data_new_inc$implementation_start
data_new_inc$time_incidence <- data_new_inc$time_incidence/(365.25/12)
data_new_inc$time_incidence<-as.numeric(data_new_inc$time_incidence)
data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0

x<-c(30,75,102,103,104,105,106,107,108,109,137,311,146)


tiff("fig5_individual_series_MDA_inc.tiff", width = 7.5, height = 3.5, units = 'in', res = 300, pointsize=7)
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(3,7))
for (val in x)
{plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n',xlim=c(0,30), cex.main=0.9) 
    ifelse((val==30| val==107), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
    ifelse((val==311 | val==146|val==137), axis(1,labels=TRUE, las=1), axis(1,labels=NA))
    points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='MDA 1st' & !is.na(data_new_inc$incidence_rate_vivax)),], col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_incidence<3,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='MDA 1st'& !is.na(data_new_inc$case_numbers_vivax)),],col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_incidence<3,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
           data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA 1st') | (data_new_inc$Intervention[data_new_inc$row_number]=='MDA 1st' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='orange',no=ifelse(Intervention=='MDA 1st',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_incidence<3,yes=18,no=5),no=ifelse(time_incidence<3,yes=16,no=1)), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
           data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA 1st') | (data_new_inc$Intervention[data_new_inc$row_number]=='MDA 1st' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='orange',no=ifelse(Intervention=='MDA 1st',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_incidence<3,yes=18,no=5),no=ifelse(time_incidence<3,yes=16,no=1)),cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
    text(27,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
    abline(h = 0, col = "gray80")}
mtext(text=expression('proportion of cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
mtext(text='time since MDA in months',side=1, line=2, cex=1, outer=TRUE)



########
#MDA repeated time (on same plot)
######

#this gives the series of time points that need to be plotted
unique(data_inc_MDA_rep$study_number_new)

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32"), number =c(268,269,270,271), letter=c('n)','o)','p)','q)'))


x<-c(268,269,270,271)

for (val in x)
{plot((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero,type='n', data=data_new_inc, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n', xlim=c(0,30), cex.main=0.9) 
 # ifelse((val==270), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
 # ifelse((val==270|val==271|val==268|val==269), axis(1,labels=TRUE, las=1), axis(1,labels=NA))
   ifelse((val==269), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
   ifelse((val==270|val==271|val==268|val==269), axis(1,labels=TRUE, las=1), axis(1,labels=NA))
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='MDA repeated' & !is.na(data_new_inc$incidence_rate_vivax)),], col= ifelse(relapse_pattern_white=='frequent',yes='darkred',no=ifelse(relapse_pattern_white=='both',yes='darkred',no='darkred')), pch= ifelse(time_incidence<3,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='MDA repeated'& !is.na(data_new_inc$case_numbers_vivax)),],col= ifelse(relapse_pattern_white=='frequent',yes='darkred',no=ifelse(relapse_pattern_white=='both',yes='darkred',no='darkred')), pch= ifelse(time_incidence<3,yes=16,no=1), cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA repeated') | (data_new_inc$Intervention[data_new_inc$row_number]=='MDA repeated' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(relapse_pattern_white=='frequent',yes='darkred',no=ifelse(relapse_pattern_white=='both',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_incidence<3,yes=18,no=5),no=ifelse(time_incidence<3,yes=16,no=1)),cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA repeated') | (data_new_inc$Intervention[data_new_inc$row_number]=='MDA repeated' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col=ifelse(relapse_pattern_white=='frequent',yes='darkred',no=ifelse(relapse_pattern_white=='both',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_incidence<3,yes=18,no=5),no=ifelse(time_incidence<3,yes=16,no=1)),cex=1.5*case_numbers_total_cat_inc[data_new_inc$row_number])
  
  text(27,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80")}

plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('left',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('navy'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=0.9)

dev.off()


