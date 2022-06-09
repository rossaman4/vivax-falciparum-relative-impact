#####
#ITN, prevalence, not_individual
######


source("preparation_analysis.R")



#adding row numbers
data_new_prev$row_number<-seq.int(nrow(data_new_prev))

first_row_study<-data_new_prev[!duplicated(data_new_prev$study_number_new),]
first_row_study$row_number<-seq.int(nrow(first_row_study))
first_row_study$study_area<-gsub(",.*","", x=first_row_study$study_area)
first_row_study$first_authors<-gsub(",.*","", x=first_row_study$first_authors)
first_row_study$plot_title<-paste(first_row_study$study_area, ',',first_row_study$first_authors)

data_new_prev$row_number<-seq.int(nrow(data_new_prev))
data_new_prev$row_number_smaller<-data_new_prev$row_number-1
data_new_prev$row_number_smaller[data_new_prev$row_number_smaller==0]<-1

#make sure the connection between dots are neat for control group ones
data_prev_LLIN_first$study_number_new[data_prev_LLIN_first$Intervention=='control']
data_prev_LLIN_first$ID[data_prev_LLIN_first$study_number_new==136]
data_new_prev$study_number_new[data_new_prev$ID==1948]<-13600
data_new_prev$study_number_new[data_new_prev$ID==1949]<-13600
data_prev_LLIN_first$ID[data_prev_LLIN_first$study_number_new==183]
data_new_prev$study_number_new[data_new_prev$ID==1953]<-1830
data_new_prev$study_number_new[data_new_prev$ID==1954]<-1830
data_new_prev$study_number_new[data_new_prev$ID==1955]<-18300
data_new_prev$study_number_new[data_new_prev$ID==1956]<-18300

data_new_prev$time_zero<-data_new_prev$time_prevalence
data_new_prev$time_zero[data_new_prev$time_zero<0]<-0
data_new_prev_store<-data_new_prev

########
## ITN first distribution
#####

#find the series of time points that need plotting
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
                                      "#8e5bc8"), number =c(1,7,33,34,35,36,37,91,9,147,136,151,99,154,183))




x<-c(1,7,33,34,35,36,37,91,9,147,136,151,99,154,183)

plot_spaghetti<-function(y)
{data_new_prev$row_number<-seq.int(nrow(data_new_prev))
data_new_prev$row_number_smaller<-data_new_prev$row_number-1
data_new_prev$row_number_smaller[data_new_prev$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,40), xaxt='n', font.main=1, las=1, yaxt='n')
  axis(1,labels=NA)
  axis(2,labels=NA)
  for (val in x)
 {points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
   points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
   points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
   points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
   points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  if(val==136)
    {points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
           data=data_new_prev[c(data_new_prev$study_number_new==13600) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  }
    if(val==183)
    {points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
            data=data_new_prev[c(data_new_prev$study_number_new==1830) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
      points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
             data=data_new_prev[c(data_new_prev$study_number_new==18300) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
      
   }}}


# -- Figure 12 ---

##########
##relapse pattern
#######

tiff("spag_ITN_first_prev.tiff", width = 7, height = 5, units = 'in', res = 700, pointsize=8)
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

plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
plot(pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM)~time_zero,type='n', data=data_new_prev, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')

##########
##coverage
#######


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
legend('topleft',legend=c('<100 cases', '100-199 cases', '200-499 cases', '500-999 cases', '>999 cases'), col=c('black'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=1.25)


##########
##transmission
#######


data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='low low',]
y<-'low Pf, low Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)


data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='low high',]
y<-'low Pf, high Pv'
plot_spaghetti(y)

data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high low',]
y<-'high Pf low Pv'
plot_spaghetti(y)


data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high high',]
y<-'high Pf high Pv'
plot_spaghetti(y)


##########
##seasonality
#######


data_new_prev<-data_new_prev_store[data_new_prev_store$seasonality=='low',]
y<-'low seasonality'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$seasonality=='high',]
y<-'high seasonality'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

##########
##initial proportion
#######


data_new_prev<-data_new_prev_store[data_new_prev_store$initial_proportion_cat=='low',]
y<-'low initial proportion'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$initial_proportion_cat=='high',]
y<-'high initial proportion'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of patent infections that are  ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)

dev.off()



# -- Figure 13 ----


########
##LLIn repeated dist
#####

#get rid of the ones with no cases at all so that the connection works
data_new_prev<-data_new_prev_store
data_new_prev$row_number<-seq.int(nrow(data_new_prev))
b<-c()
b<-data_new_prev$row_number[data_new_prev$study_number_new==39&data_new_prev$vivax_new_prev==0]
data_new_prev<-data_new_prev[-c(b),]
data_new_prev$row_number<-seq.int(nrow(data_new_prev))

data_new_prev$row_number<-seq.int(nrow(data_new_prev))
b<-c()
b<-data_new_prev$row_number[data_new_prev$study_number_new==173&data_new_prev$vivax_new_prev==0]
data_new_prev<-data_new_prev[-c(b),]
data_new_prev$row_number<-seq.int(nrow(data_new_prev))


data_new_prev_store<-data_new_prev

#find which series of time points need plotting
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
                                      "#8e5bc8",
                                      "#c2af32"), number =c(39,  40,173, 174, 177, 178, 179, 180,169, 354, 356, 184, 189, 185, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347,  41, 135,21),letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)','n)','o)','p)','q)','r)','s)','t)','u)','v)','w)','x)','y)','z)','aa)'))




x<-c(40, 174, 177, 178, 179, 180,169, 356, 185, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347,  41, 135,21)



plot_spaghetti<-function(y)
{data_new_prev$row_number<-seq.int(nrow(data_new_prev))
data_new_prev$row_number_smaller<-data_new_prev$row_number-1
data_new_prev$row_number_smaller[data_new_prev$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,40), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
        data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number])
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b')
}}

##########
##initial proportion
#######
tiff("spag_ITN_rep_prev.tiff", width = 5, height = 5, units = 'in', res = 700, pointsize=8)
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(3,2))

data_new_prev<-data_new_prev_store[data_new_prev_store$initial_proportion_cat=='low',]
y<-'low initial proportion'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$initial_proportion_cat=='high',]
y<-'high initial proportion'
plot_spaghetti(y)


##########
##coverage
#######

data_new_prev<-data_new_prev_store[data_new_prev_store$coverage=='low',]
y<-'low coverage'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_prev<-data_new_prev_store[data_new_prev_store$coverage=='high',]
y<-'high coverage'
plot_spaghetti(y)

#data_new_prev<-data_new_prev_store[data_new_prev_store$coverage=='missing',]
#y<-'missing coverage'
#plot_spaghetti(y)



##########
##transmission
#######


data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='low low',]
y<-'low Pf, low Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

#data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='low high',]
#y<-'low Pf, high Pv'
#plot_spaghetti(y)


data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high low',]
y<-'high Pf low Pv'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)

#data_new_prev<-data_new_prev_store[data_new_prev_store$transmission_study=='high high',]
#y<-'high Pf high Pv'
#plot_spaghetti(y)
#axis(1,labels=TRUE, las=1)

legend('bottomright',legend=c('<100 cases', '100-199 cases', '200-499 cases', '500-999 cases', '>999 cases'), col=c('black'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=1.25)
mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of patent infections that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)



dev.off()



# --- Figure 14 ---

########
##LLIn first and repeated
#####

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
                                      "#8e5bc8"), number =c(1,7,33,34,35,36,37,91,9,147,136,151,99,154,183))






x<-c(1,7,33,34,35,36,37,91,9,147,136,151,99,154,183)

plot_spaghetti<-function(y)
{data_new_prev$row_number<-seq.int(nrow(data_new_prev))
data_new_prev$row_number_smaller<-data_new_prev$row_number-1
data_new_prev$row_number_smaller[data_new_prev$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,50), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
        data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN 1st dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  if(val==136)
  {points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==13600) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  }
  if(val==183)
  {points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
          data=data_new_prev[c(data_new_prev$study_number_new==1830) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
    points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
           data=data_new_prev[c(data_new_prev$study_number_new==18300) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN 1st dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN 1st dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
    
  }}}



#quartz()
tiff("spag_ITN_first_rep_prev.tiff", width = 7, height = 4, units = 'in', res = 700, pointsize=8)
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(1,2))
data_new_prev<-data_new_prev_store
y<-'First distribution'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

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
                                      "#8e5bc8",
                                      "#c2af32"), number =c(39,  40,173, 174, 177, 178, 179, 180,169, 354, 356, 184, 189, 185, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347,  41, 135,21),letter=c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)','l)','m)','n)','o)','p)','q)','r)','s)','t)','u)','v)','w)','x)','y)','z)','aa)'))



x<-c(40, 174, 177, 178, 179, 180,169, 356, 185, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347,  41, 135,21)

plot_spaghetti<-function(y)
{data_new_prev$row_number<-seq.int(nrow(data_new_prev))
data_new_prev$row_number_smaller<-data_new_prev$row_number-1
data_new_prev$row_number_smaller[data_new_prev$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_prev, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,50), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
        data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b', cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val & data_new_prev$Intervention=='LLIN later dist' & is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM)& is.na(data_new_prev$pr_numb_vivax_PCR)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<24,yes=16,no=1), type='b',cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number],lwd=2)
  points((pr_numb_vivax_LM/(pr_numb_vivax_LM+pr_numb_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  points((prevalence_vivax_LM/(prevalence_vivax_LM+prevalence_falciparum_LM))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) & is.na(data_new_prev$pr_numb_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  points((pr_numb_vivax_PCR/(pr_numb_vivax_PCR+pr_numb_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
  points((prevalence_vivax_PCR/(prevalence_vivax_PCR+prevalence_falciparum_PCR))~time_zero, 
         data=data_new_prev[c(data_new_prev$study_number_new==val) &  is.na(data_new_prev$pr_numb_vivax_LM) & is.na(data_new_prev$prevalence_vivax_LM) & is.na(data_new_prev$pr_numb_vivax_PCR) & ((data_new_prev$Intervention[data_new_prev$row_number]=='control' & data_new_prev$Intervention[data_new_prev$row_number+1]=='LLIN later dist') | (data_new_prev$Intervention[data_new_prev$row_number]=='LLIN later dist' & data_new_prev$Intervention[data_new_prev$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<24,yes=18,no=5),no=ifelse(time_zero<24,yes=16,no=1)), cex=1.5*case_numbers_total_cat_prev[data_new_prev$row_number], type='b',lwd=2)
}}


data_new_prev<-data_new_prev_store
y<-'Repeated distribution'
plot_spaghetti(y)

axis(1,labels=TRUE, las=1)

mtext(text='time since ITN distribution in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('proportion of patent infections that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)
legend('bottomright',legend=c('<100 cases', '100-199 cases', '200-499 cases', '500-999 cases', '>999 cases'), col=c('black'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=0.9)

dev.off()



