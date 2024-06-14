#importing final dataset we will be working with for analysis and summary stats for dataset we are working with
#original code by A. R. Martinig
#last edited on June 14, 2024 by A. R. Martinig 

#dataset we are using
gest<-read.csv("final_dataset.csv", header=T)      

################################
###### Summary statistics ######
################################

#year range and sample sizes
table(gest $year)

#masting and sample sizes
table(gest $mast)

#gestation length in days and sample size
table(gest $gestation_days)

#gestation age and sample size
table(gest $gestation_age)

#gestation length stats
summary(gest $gestation_days)

test <- gest %>% 
		group_by(gestation_age) %>%
		mutate(avg=mean(gestation_days), 
			min=min(gestation_days), 
			max=max(gestation_days), 
			med=median(gestation_days)) %>%
		filter(row_number()==1) %>%
		arrange(gestation_age)

gest %>% select(gestation_age, avg, min, max, med) #stats for gestation length at each age (yearlings through 7 years old)        