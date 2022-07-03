#####
#MDA, prevalence, individual
####


source('data_preparation_and_cleaning.r')
source('reshaping_dataframe.r')
source('preparation_analysis.r')
library(stringr)



#adding row numbers
data_new_prev$row_number<-seq.int(nrow(data_new_prev))
#name the plots
first_row_study<-data_new_prev[!duplicated(data_new_prev$study_number_new),]
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
##MDA first dist
#####

#this gives the series of time points that need to be plotted
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
                                      "#4eacd7"), number =c(76,89,90,97,144, 191, 329,94,95,96),letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)'))





x<-c(76,89,90,97,144, 191, 329,94,95,96)

data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0

tiff("fig6_individual_series_MDA_prev.tiff", width = 7.5, height = 2.5, units = 'in', res = 300, pointsize=8)
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(2,7))
for (val in x)
{plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n', xlim=c(0,15), cex.main=0.9) 
    ifelse((val==76|val==94), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
    ifelse((val==96|val==95|val==94|val==329|val==144|val==191), axis(1,labels=TRUE, las=1), axis(1,labels=NA))

  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st'),], col= 'navy', pch= ifelse(time_zero<=3,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= 'navy', pch= ifelse(time_zero<=3,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= 'navy', pch= ifelse(time_zero<=3,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='MDA 1st' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= 'navy', pch= ifelse(time_zero<=3,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='orange',no=ifelse(Intervention=='MDA 1st',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='orange',no=ifelse(Intervention=='MDA 1st',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='orange',no=ifelse(Intervention=='MDA 1st',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='MDA 1st') | (data_new_prev$Intervention[data_new_prev$row_number]=='MDA 1st' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='orange',no=ifelse(Intervention=='MDA 1st',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<=3,yes=18,no=5),no=ifelse(time_zero<=3,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
    text(12.5,0.7,fave_cols$letter[fave_cols$number==val],cex=1.2)
   abline(h = 0, col = "gray80")}
mtext(text=expression('proportion of patent infections that are  ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
mtext(text='time since MDA in months',side=1, line=2, cex=1, outer=TRUE)

plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<100 cases', '100-199 cases', '200-499 cases', '500-999 cases', '>999 cases'), col=c('navy'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=1.3)

dev.off()

