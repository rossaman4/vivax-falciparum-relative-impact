#####
# ITN, prevalence, individual plots
# fig4_individual_series_plot_prevalence.r
#####


source('preparation_analysis.r')
library(stringr)

#adding row numbers
data_new_prev$row_number<-seq.int(nrow(data_new_prev))
#naming plots
first_row_study<-data_new_prev[!duplicated(data_new_prev$study_number_new),]
first_row_study$row_number<-seq.int(nrow(first_row_study))
first_row_study$study_area_add<-gsub(".*,","", x=first_row_study$study_area)
first_row_study$study_area<-gsub(",.*","", x=first_row_study$study_area)
first_row_study$first_authors<-gsub(",.*","", x=first_row_study$first_authors)
first_row_study$study_area[first_row_study$study_area=='Papua New Guinea']<-'PNG'
first_row_study$study_area_add<-str_trim(first_row_study$study_area_add)
first_row_study$study_area_add<-str_replace(first_row_study$study_area_add, "^\\w{1}", toupper)
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors)
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors, '\n', first_row_study$study_area_add)



########
##LLIN first dist
#####
#this gives the series of time points that need to be plotted
unique(data_prev_LLIN_first$study_number_new)


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
                                      "#8e5bc8"), number =c(1,7,33,34,35,36,37,91,9,147,136,151,99,154,183), letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)','n)','o)'))


x<-c(1,7,33,34,35,36,37,91,9,147,136,151,99,154,183)

data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0

#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(6,7))
for (val in x)
{plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1), bty='n', yaxt='n', xaxt='n', xlim=c(0,60), cex.main=0.75) 
  ifelse((val==1|val==91|val==183), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
  axis(1,labels=NA)
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist'),], col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= ifelse(relapse_pattern_white=='frequent',yes='navy',no=ifelse(relapse_pattern_white=='both',yes='navy',no='navy')), pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN 1st dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN 1st dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN 1st dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN 1st dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  text(55,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80")}
mtext(text=expression('proportion of patent infections that are  ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)





########
## ITN repeated dist
#####

#this gives the series of time points that need to be plotted
unique(data_prev_LLIN_rep$study_number_new)


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
                                      "#9c496c",
                                      "#5ab74d",
                                      "#8e5bc8"), number =c(39,  40,173, 174, 177, 178, 179, 180,169, 356, 184, 189, 185, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347,  41, 135,21),letter=c('p)','q)','r)','s)','t)','u)','v)','w)','x)','y)','z)','aa)','ab)','ac)','ad)','ae)','af)','ag)','ah)','ai)','aj)','ak)','al)','am)','an)','ao)'))



#take out the ones that have no cases before or in all the after cases
x<-c(39,  40,173, 174, 177, 178, 179, 180,169, 356, 184, 189, 185, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347,  41, 135,21)


data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0


for (val in x)
{plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=first_row_study$plot_title[first_row_study$study_number_new==val] , xlab='Time [months]', ylab= 'proportion vivax', ylim=c(0:1),xlim=c(0,60), bty='n', yaxt='n', xaxt='n', cex.main=0.75) 
  rect(par("usr")[1], par("usr")[3],par("usr")[2], par("usr")[4],col = "grey", border=NA)
  ifelse((val==179|val==338|val==345), axis(2,labels=TRUE, las=1), axis(2,labels=NA))
  ifelse((val==21|val==135|val==41|val==347|val==346|val==345|val==344), axis(1,labels=c('0','','20','','40','','60'), las=1, at=c(0,10,20,30,40,50,60)), axis(1,labels=NA))
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist'),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= 'navy', pch= ifelse(time_zero<24,yes=16,no=1), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN later dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN later dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN later dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col=ifelse(Intervention=='control',yes='darkorange2',no=ifelse(Intervention=='LLIN later dist',yes='navy',no='navy')), pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  text(55,0.8,fave_cols$letter[fave_cols$number==val],cex=1.2)
  abline(h = 0, col = "gray80")}
plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<100 cases', '100-199 cases', '200-499 cases', '500-999 cases', '>999 cases'), col=c('navy'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=0.9)




