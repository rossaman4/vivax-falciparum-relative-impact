######
#IRS, prevalence, individual plots
#########


source('data_preparation_and_cleaning.r')
source('reshaping_dataframe.r')
source('preparation_analysis.r')
library(stringr)



#adding row numbers
data_new_prev$row_number<-seq.int(nrow(data_new_prev))
#plot titles
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
##IRS first dist
#####
#this gives the series of time points that need to be plotted
unique(data_prev_IRS_first$study_number_new)

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c", 
                                      "#5ab74d"), number =c(43,98), letter=c('a)','b)'))


x<-c(43,98)
data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0

tiff("fig8_individual_series_IRS_prev.tiff", width = 7.5, height = 3.5, units = 'in', res = 300, pointsize=8)
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(3,7))
for (val in x)
{plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0,1), bty='n', yaxt='n', xaxt='n', xlim=c(0,50), cex.main=0.9) 
   ifelse((val==43), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
   axis(1,labels=NA)
   points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS 1st time'),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS 1st time' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS 1st time' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS 1st time' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS 1st time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS 1st time' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS 1st time',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS 1st time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS 1st time' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS 1st time',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS 1st time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS 1st time' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS 1st time',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS 1st time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS 1st time' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS 1st time',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
   text(45,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
   abline(h = 0, col = "gray80")}

mtext(text=expression('proportion of patent infections that are  ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
mtext(text='time since IRS in months',side=1, line=2, cex=1, outer=TRUE)




########
##IRS repeated distribution - on the same plot
#####
#this gives the series of time points that need to be plotted
unique(data_prev_IRS_rep$study_number_new)


fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8",
                                      "#c2af32",
                                      "#627bc8"
                                      ), number =c(117,118,119,120,121),letter=c('c)','d)','e)','f)','g)'))


x<-c(117,118,119,120,121)

data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0

for (val in x)
{plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n', xlim=c(0,50), cex.main=0.9) 
  ifelse((val==120), axis(2,labels=NA, las=1), axis(2,labels=NA))
         axis(1,labels=NA)
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS repeated time'),], col= 'darkred', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS repeated time' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= 'darkred', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS repeated time' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= 'darkred', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS repeated time' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= 'darkred', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS repeated time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS repeated time' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS repeated time',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS repeated time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS repeated time' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS repeated time',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS repeated time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS repeated time' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS repeated time',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS repeated time') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS repeated timet' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS repeated time',yes='darkred',no='darkred')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  text(45,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80")}


########
#IRS rest - on the same plot
#####

#this gives the series of time points that need to be plotted
unique(data_prev_IRS$study_number_new)


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
                                      "#c67439"), number =c(56,124,125,126,127,130,131,132,133,134,152),letter=c('h)','i)','j)','k)','l)','m)','n)','o)','p)','q)','r)'))


x<-c(56,124,125,126,127,130,131,132,133,134,152)
data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0


for (val in x)
{plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n', xlim=c(0,50), cex.main=0.9) 
  ifelse((val==56|val==132), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
  ifelse((val==152|val==134|val==133|val==132|val==131|val==130), axis(1,labels=TRUE, las=1), axis(1,labels=NA))

  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS'),], col= 'darkgreen', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= 'darkgreen', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= 'darkgreen', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= 'darkgreen', pch= ifelse(time_zero<24,yes=16,no=1), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS',yes='darkgreen',no='darkgreen')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS',yes='darkgreen',no='darkgreen')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS',yes='darkgreen',no='darkgreen')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='IRS',yes='darkgreen',no='darkgreen')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=2*case_numbers_total_cat_prev[data_new_prev$row_number])
  text(45,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80")}

plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<100 cases', '100-199 cases', '200-499 cases', '500-999 cases', '>999 cases'), col=c('navy'), pch=c(16), pt.cex=c(2*0.75, 2*1, 2*1.25, 2*1.5, 2*1.75), cex=1)

dev.off()

