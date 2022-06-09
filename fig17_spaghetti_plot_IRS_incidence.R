########
#IRS, incidence, not_individual
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
first_row_study$first_authors<-gsub(",.*","", x=first_row_study$first_authors)
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors)
data_new_inc$Intervention[data_new_inc$Intervention=='IRS 1st time']<-'IRS'
data_new_inc$Intervention[data_new_inc$Intervention=='IRS repeated time']<-'IRS'

data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0
data_new_inc_store<-data_new_inc


########
#IRS all
######
#the series of time points that need plotting
unique(data_inc_IRS_all$study_number_new)

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
                                      "#c2af32"), number =c(53,  60,  61,  62,  73,  74,  92,  93, 116, 122, 123, 134, 141, 152,305,306))




x<-c(53,  60,  61,  62,  73,  74,  92,  93, 116, 122, 123, 134, 141, 152,305,306)
plot_spaghetti<-function(y)
{data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_inc, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,50), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
        data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=case_numbers_total_cat_inc[data_new_inc$row_number],lwd=2)
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='IRS'& is.na(data_new_inc$case_numbers_vivax)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number],lwd=2)
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)  & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b', lwd=2)
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)& is.na(data_new_inc$case_numbers_vivax) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b',lwd=2)
  if(val==27)
  {t<-c(2700:2721)
  for(value in t)
    points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
           data=data_new_inc[c(data_new_inc$study_number_new==value) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='IRS') | (data_new_inc$Intervention[data_new_inc$row_number]=='IRS' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b',lwd=2)
  
  }}}


##########
##seasonality
#######
#quartz()
tiff("spag_IRS_Inc.tiff", width = 7, height = 5, units = 'in', res = 700, pointsize=8)
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(3,3))
data_new_inc<-data_new_inc_store[data_new_inc_store$seasonality=='low',]
y<-'low seasonality'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$seasonality=='high',]
y<-'high seasonality'
plot_spaghetti(y)

plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')

##########
##intial proportion
#######
data_new_inc<-data_new_inc_store[data_new_inc_store$initial_proportion_cat=='low',]
y<-'low initial proportion'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$initial_proportion_cat=='high',]
y<-'high initial proportion'
plot_spaghetti(y)


plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('black'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=1.25)


##########
##relapse pattern
#######


data_new_inc<-data_new_inc_store[data_new_inc_store$relapse_pattern_white=='frequent',]
y<-'frequent relapses'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$relapse_pattern_white=='long',]
y<-'long latency relapses'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$relapse_pattern_white=='both',]
y<-'both relapse patterns'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

mtext(text='time since IRS in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of clinical cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)

dev.off()
