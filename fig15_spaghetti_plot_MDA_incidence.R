##########
#MDA, incidence, not_individual
##########


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

#make connnections of points nice for control studies
data_new_inc$ID[data_new_inc$study_number_new==27]
data_new_inc$study_number_new[data_new_inc$ID==1972&data_new_inc$study_number_new==27]<-2700
data_new_inc$study_number_new[data_new_inc$ID==1973&data_new_inc$study_number_new==27]<-2700
data_new_inc$study_number_new[data_new_inc$ID==1974&data_new_inc$study_number_new==27]<-2701
data_new_inc$study_number_new[data_new_inc$ID==1975&data_new_inc$study_number_new==27]<-2701
data_new_inc$study_number_new[data_new_inc$ID==1976&data_new_inc$study_number_new==27]<-2702
data_new_inc$study_number_new[data_new_inc$ID==1977&data_new_inc$study_number_new==27]<-2702
data_new_inc$study_number_new[data_new_inc$ID==1978&data_new_inc$study_number_new==27]<-2703
data_new_inc$study_number_new[data_new_inc$ID==1979&data_new_inc$study_number_new==27]<-2703
data_new_inc$study_number_new[data_new_inc$ID==1980&data_new_inc$study_number_new==27]<-2704
data_new_inc$study_number_new[data_new_inc$ID==1981&data_new_inc$study_number_new==27]<-2704
data_new_inc$study_number_new[data_new_inc$ID==1982&data_new_inc$study_number_new==27]<-2705
data_new_inc$study_number_new[data_new_inc$ID==1983&data_new_inc$study_number_new==27]<-2705
data_new_inc$study_number_new[data_new_inc$ID==1984&data_new_inc$study_number_new==27]<-2706
data_new_inc$study_number_new[data_new_inc$ID==1985&data_new_inc$study_number_new==27]<-2706
data_new_inc$study_number_new[data_new_inc$ID==1986&data_new_inc$study_number_new==27]<-2707
data_new_inc$study_number_new[data_new_inc$ID==1987&data_new_inc$study_number_new==27]<-2707
data_new_inc$study_number_new[data_new_inc$ID==1988&data_new_inc$study_number_new==27]<-2708
data_new_inc$study_number_new[data_new_inc$ID==1989&data_new_inc$study_number_new==27]<-2708
data_new_inc$study_number_new[data_new_inc$ID==1990&data_new_inc$study_number_new==27]<-2709
data_new_inc$study_number_new[data_new_inc$ID==1991&data_new_inc$study_number_new==27]<-2709
data_new_inc$study_number_new[data_new_inc$ID==1992&data_new_inc$study_number_new==27]<-2710
data_new_inc$study_number_new[data_new_inc$ID==1993&data_new_inc$study_number_new==27]<-2710
data_new_inc$study_number_new[data_new_inc$ID==1994&data_new_inc$study_number_new==27]<-2711
data_new_inc$study_number_new[data_new_inc$ID==1995&data_new_inc$study_number_new==27]<-2711
data_new_inc$study_number_new[data_new_inc$ID==1996&data_new_inc$study_number_new==27]<-2712
data_new_inc$study_number_new[data_new_inc$ID==1997&data_new_inc$study_number_new==27]<-2712
data_new_inc$study_number_new[data_new_inc$ID==1998&data_new_inc$study_number_new==27]<-2713
data_new_inc$study_number_new[data_new_inc$ID==1999&data_new_inc$study_number_new==27]<-2713
data_new_inc$study_number_new[data_new_inc$ID==2000&data_new_inc$study_number_new==27]<-2714
data_new_inc$study_number_new[data_new_inc$ID==2001&data_new_inc$study_number_new==27]<-2714
data_new_inc$study_number_new[data_new_inc$ID==2002&data_new_inc$study_number_new==27]<-2715
data_new_inc$study_number_new[data_new_inc$ID==2003&data_new_inc$study_number_new==27]<-2715
data_new_inc$study_number_new[data_new_inc$ID==2004&data_new_inc$study_number_new==27]<-2716
data_new_inc$study_number_new[data_new_inc$ID==2005&data_new_inc$study_number_new==27]<-2716

########
#MDA all
######

data_new_inc$time_zero<-data_new_inc$time_incidence
data_new_inc$time_zero[data_new_inc$time_zero<0]<-0
data_new_inc$Intervention<-as.character(data_new_inc$Intervention)
data_new_inc$Intervention[data_new_inc$Intervention=='MDA 1st']<- 'MDA'
data_new_inc$Intervention[data_new_inc$Intervention=='MDA repeated']<- 'MDA'
data_new_inc$Intervention<-as.factor(data_new_inc$Intervention)


data_new_inc_store<-data_new_inc


#exclude points with no cases found so connections are done correctly
data_new_inc<-data_new_inc_store
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
b<-c()
b<-data_new_inc$row_number[data_new_inc$study_number_new==107&data_new_inc$vivax_new==0&data_new_inc$falciparum_new==0]
data_new_inc<-data_new_inc[-c(b),]
data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc_store<-data_new_inc



####
##function
#####

#the studies that need plotting
unique(data_inc_MDA_all$study_number_new)

fave_cols<-data.frame()
fave_cols<-cbind.data.frame(colour= c("#64e397",
                                      "#82d54c",
                                      "#7e359f",
                                      "#3b7832",
                                      "#c7373a",
                                      "#469087",
                                      "#6d6cd8",
                                      "#c6d747",
                                      "#51508b",
                                      "#cbba4a",
                                      "#c794d4",
                                      "#e29e3f",
                                      "#d56640",
                                      "#86bce1",
                                      "#c6d8a3",
                                      "#813845",
                                      "#dd9fa2",
                                      "#7f5b28",
                                      "#d643a9"), number =c(27,184,30,75,102,103,104,105,106,107,108,109,137,311,146,268,269,270,271))





##take out the ones where no cases found after intervention
x<-c(30,75,102,103,104,105,106,107,108,109,137,311,146,268,269,270,271)
plot_spaghetti<-function(y)
{data_new_inc$row_number<-seq.int(nrow(data_new_inc))
data_new_inc$row_number_smaller<-data_new_inc$row_number-1
data_new_inc$row_number_smaller[data_new_inc$row_number_smaller==0]<-1
plot((pr_numb_vivax_LM/(pr_numb_falciparum_LM+pr_numb_vivax_LM))~time_zero, type='n', data=data_new_inc, main=y, xlab='Time [months]', ylab=expression('Proportion of cases attributed to ' *italic(P.vivax)* ''), ylim=c(0:1), bty='n', xlim=c(0,40), xaxt='n', font.main=1, las=1, yaxt='n')
axis(1,labels=NA)
axis(2,labels=NA)
for (val in x)
{points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
        data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='MDA'),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<3,yes=16,no=1), type='b',cex=case_numbers_total_cat_inc[data_new_inc$row_number], lwd=2)
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
        data=data_new_inc[c(data_new_inc$study_number_new==val & data_new_inc$Intervention=='MDA'& is.na(data_new_inc$case_numbers_vivax)),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(time_zero<3,yes=16,no=1), type='b', cex=case_numbers_total_cat_inc[data_new_inc$row_number],lwd=2)
  points((case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)  & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA') | (data_new_inc$Intervention[data_new_inc$row_number]=='MDA' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<3,yes=18,no=5),no=ifelse(time_zero<3,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b',lwd=2)
  points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
         data=data_new_inc[c(data_new_inc$study_number_new==val)& is.na(data_new_inc$case_numbers_vivax) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA') | (data_new_inc$Intervention[data_new_inc$row_number]=='MDA' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<3,yes=18,no=5),no=ifelse(time_zero<3,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b',lwd=2)
  if(val==27)
  {t<-c(2700:2716)
  for(value in t)
    {points((incidence_rate_vivax/(incidence_rate_falciparum+incidence_rate_vivax))~time_zero, 
          data=data_new_inc[c(data_new_inc$study_number_new==value) & ((data_new_inc$Intervention[data_new_inc$row_number]=='control' & data_new_inc$Intervention[data_new_inc$row_number+1]=='MDA') | (data_new_inc$Intervention[data_new_inc$row_number]=='MDA' & data_new_inc$Intervention[data_new_inc$row_number_smaller]=='control')),], col= fave_cols$colour[fave_cols$number==val], pch= ifelse(Intervention=='control',yes=ifelse(time_zero<3,yes=18,no=5),no=ifelse(time_zero<3,yes=16,no=1)), cex=case_numbers_total_cat_inc[data_new_inc$row_number], type='b',lwd=2)
    
  }}}}



# --- Figure 15 ---


##########
##coverage
#######
tiff("spag_MDA_inc.tiff", width = 7, height = 5, units = 'in', res = 700, pointsize=8)
#quartz()
par(oma=c(3,3.5,3,0.5))
par(mar=c(1,1,1.5,1))
par(mfrow=c(4,4))
x<-c(30,75,102,103,104,105,106,107,108,109,137,311,146,268,269,270,271)
data_new_inc<-data_new_inc_store[data_new_inc_store$coverage=='low',]
y<-'low coverage'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$coverage=='high',]
y<-'high coverage'
plot_spaghetti(y)

data_new_inc<-data_new_inc_store[data_new_inc_store$coverage=='missing',]
y<-'missing coverage'
plot_spaghetti(y)

plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')




##########
##relapse pattern
#######

x<-c(30,75,102,103,104,105,106,107,108,109,137,311,146,268,269,270,271)
data_new_inc<-data_new_inc_store[data_new_inc_store$relapse_pattern_white=='frequent',]
y<-'frequent relapses'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$relapse_pattern_white=='long',]
y<-'long latency relapses'
plot_spaghetti(y)

data_new_inc<-data_new_inc_store[data_new_inc_store$relapse_pattern_white=='both',]
y<-'both relapse patterns'
plot_spaghetti(y)

plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')



##########
##transmission
#######


data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='low low',]
y<-'low Pf, low Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=NA, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='low high',]
y<-'low Pf, high Pv'
plot_spaghetti(y)
axis(2,labels=NA, las=1)
axis(1,labels=NA, las=1)

#data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='high low',]
#y<-'high Pf, low Pv'
#plot_spaghetti(y)
#axis(2,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$transmission_study=='high high',]
y<-'high Pf, high Pv'
plot_spaghetti(y)
axis(2,labels=NA, las=1)
axis(1,labels=NA, las=1)

plot(case_numbers_vivax/(case_numbers_falciparum+case_numbers_vivax)~time_zero,type='n', data=data_new_inc, bty='n', xaxt='n', yaxt='n', ylab='', xlab='')
legend('topleft',legend=c('<200 cases', '200-499 cases', '500-999 cases', '1000-1999 cases', '>1999 cases'), col=c('black'), pch=c(16), pt.cex=c(1.5*0.75, 1.5*1, 1.5*1.25, 1.5*1.5, 1.5*1.75), cex=1.25)

#######
##initial proportion
######

data_new_inc<-data_new_inc_store[data_new_inc_store$initial_proportion_cat=='low',]
y<-'low initial proportion of Pv'
plot_spaghetti(y)
axis(2,labels=TRUE, las=1)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store[data_new_inc_store$initial_proportion_cat=='high',]
y<-'high initial proportion of Pv'
plot_spaghetti(y)
axis(1,labels=TRUE, las=1)



#######
##one time MDA vs several
######

data_new_inc<-data_new_inc_store
x<-c(102,103,104,105,106,107,108,109,30,268,269,270,271)
y<-'One time MDA'
plot_spaghetti(y)
axis(2,labels=NA, las=1)
axis(1,labels=TRUE, las=1)

data_new_inc<-data_new_inc_store
x<-c(75,137,311,146)
data_new_inc<-data_new_inc_store[data_new_inc_store$master_study_number==44|68|20|8,]
y<-'3 rounds of MDA'
plot_spaghetti(y)
axis(2,labels=NA, las=1)
axis(1,labels=TRUE, las=1)


mtext(text='time since MDA in months',side=1, line=2, cex=1, outer=TRUE)
mtext(text=expression('Proportion of clinical cases that are ' *italic(P.vivax)* ''),side=2, line=2, cex=1, outer=TRUE)


dev.off()

