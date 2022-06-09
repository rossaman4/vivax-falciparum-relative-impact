#####
#IRS, prevalence, not_individual
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
data_new_prev$Intervention[data_new_prev$Intervention=='IRS 1st time']<-'IRS'
data_new_prev$Intervention[data_new_prev$Intervention=='IRS repeated time']<-'IRS'
data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0
data_new_prev_store<-data_new_prev


######
##function
#######
unique(data_prev_IRS_all$study_number_new)

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
                                      "#53bd9b"), number =c(43,  56,  98, 117, 118, 119, 120, 121, 124, 125, 126, 127, 130, 131, 132, 133, 134, 152, 321, 322))



x<-c(43,  56,  98, 117, 118, 119, 120, 121, 124, 125, 126, 127, 130, 131, 132, 133, 134, 152)
plot_spaghetti<-function(y)
{data_new_prev$row_number<-seq.int(nrow(data_new_prev))
data_new_prev$row_number_smaller<-data_new_prev$row_number-1
data_new_prev$row_number_smaller[data_new_prev$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,60), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
        data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='IRS' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='IRS') | (data_new_prev$Intervention[data_new_prev$row_number]=='IRS' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  }}



##########
##transmission
#######
tiff("spag_IRS_prev.tiff", width = 7, height = 3, units = 'in', res = 700, pointsize=8)
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(2,4))

data_new_prev<-data_new_prev_store
data_new_prev<-data_new_prev[data_new_prev$transmission_study=='low low',]
y<-'low Pf, low Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)


##no data in this category
#data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high low',]
#y<-'high Pf, low Pv'
#plot_spaghetti(y)
#axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='low high',]
y<-'low Pf high Pv'
plot_spaghetti(y)


data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high high',]
y<-'high Pf high Pv'
plot_spaghetti(y)

plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<100 cases', '100-199 cases', '200-499 cases', '500-999 cases', '>999 cases'), col=c('black'), pch=c(16), pt.cex=c(2*0.75, 2*1, 2*1.25, 2*1.5, 2*1.75), cex=1.5)





##########
##seasonality
#######


data_new_prev<-data_new_prev_store
data_new_prev<-data_new_prev[data_new_prev$seasonality=='low',]
y<-'low seasonality'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$seasonality=='high',]
y<-'high seasonality'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

mtext(text='time since IRS in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of patent infections that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)


##########
##initial proportion
#######


data_new_prev<-data_new_prev_store
data_new_prev<-data_new_prev[data_new_prev$initial_proportion_cat=='low',]
y<-'low initial proportion'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$initial_proportion_cat=='high',]
y<-'high initial proportion'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

dev.off()


