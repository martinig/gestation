#importing final dataset we will be working with for analysis and summary stats for dataset we are working with
#original code by A. R. Martinig
#last edited on May 15, 2024 by A. R. Martinig 

#dataset we are using
gest<-read.csv("final_dataset.csv", header=T)      

################################
###### Summary statistics ######
################################

#year range and sample sizes
table(mating$year)

#masting and sample sizes
table(mating$mast)

#gestation length in days and sample size
table(mating$gestation_days)

#gestation age and sample size
table(mating$gestation_age)

#gestation length stats
summary(mating$gestation_days)

test <- mating %>% 
		group_by(gestation_age) %>%
		mutate(avg=mean(gestation_days), 
			min=min(gestation_days), 
			max=max(gestation_days), 
			med=median(gestation_days)) %>%
		filter(row_number()==1) %>%
		arrange(gestation_age)

test %>% select(gestation_age, avg, min, max, med) #stats for gestation length at each age (yearlings through 7 years old)        