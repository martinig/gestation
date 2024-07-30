#importing final dataset we will be working with for analysis and summary stats for dataset we are working with
#original code by A. R. Martinig
#last edited on July 23, 2024 by N. Brunner

#dataset we are using
gest<-read.csv("final_dataset.csv", header=T)      

################################
###### Summary statistics ######
################################


####################################
#summary stats - sample sizes
####################################

nrow(gest) #174 records
gest %>% as_tibble() %>% count(squirrel_id) %>% nrow() #160 females	

#year range and sample sizes
table(gest $year)

#grid locations ##should also pull out grid year breakdown for coauthors
table(gest$grid)

##grid/year breakdown
table(gest$year, gest$grid)

#masting and sample sizes
table(gest $mast)

#gestation age and sample size
table(gest $gestation_age) 

#intermast interval
#original mast year data: (1993, 1998, 2005, 2010, 2014, 2019, 2022) 
m_data <- c(19930101, 19980101, 20050101, 20100101, 20140101, 20190101, 20220101)
m_data <- ymd(m_data)

library(pastecs)
days_btwn_masts <- diff(m_data)
days_btwn_masts
years_btwn_masts <- daystoyears(days_btwn_masts)
years_btwn_masts

mean(m_data)
#you'll need to figure out how to convert this to years (hint: package lubridate)
#then you will need to calculate the average time between masts and the SD


####################################
#gestation length stats
####################################

#gestation length in days and sample size
table(gest $gestation_days)

#gestation length stats
summary(gest $gestation_days)

hist(gest$gestation_days)    

#stats for gestation length at each age (yearlings through 7 years old)   
mom_age <- gest %>% 
		group_by(gestation_age) %>%
		mutate(avg=mean(gestation_days), 
			min=min(gestation_days), 
			max=max(gestation_days), 
			med=median(gestation_days)) %>%
		filter(row_number()==1) %>%
		arrange(gestation_age)

mom_age %>% select(gestation_age, avg, min, max, med) 
