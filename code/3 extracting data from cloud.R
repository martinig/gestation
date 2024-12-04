#all the data extraction and cleaning is here 
#original code by A. R. Martinig
#last edited on December 4, 2024 by A. R. Martinig 


##############################
#  importing  data and formatting it  #
##############################

#import Data
con <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                  dbname ="krsp",
           user="amartinig",
           password = keyring::key_get("krsp")
)

all <- tbl(con, "flastall2") %>% 
  	collect() %>%
  	select(squirrel_id, byear)

litter <- tbl(con, "litter") %>% 
  	collect() %>%
  	# remove squirrels with no id
  	filter(!is.na(squirrel_id)) %>%
  	mutate(fieldBDate=ymd(fieldBDate)) %>%
  	left_join(all, by=c("squirrel_id")) %>%
  	select(id, squirrel_id, year=yr, date1, fieldBDate, mom_byear=byear)
  	
summary(litter)
head(litter)

juv <- tbl(con, "juvenile") %>% 
  	collect() %>%
  	group_by(litter_id) %>%
  	mutate(n_pups=n(),
  		total_males=sum(sex=="M"),
  		litter_ratio=total_males/n_pups) %>% 
  	select(litter_id, n_pups, litter_ratio)

head(juv)

rep<-left_join(litter, juv, by=c("id"="litter_id")) %>%
	group_by(squirrel_id, date1) %>%
	filter(row_number()==1) %>%
	ungroup()

new_beh <- tbl(con, "behaviour") %>% 
  	collect() %>%
  	group_by(squirrel_id, date) %>%
  	filter(
  		row_number()==1, 
  		!is.na(squirrel_id), 
  		behaviour==22 | 
  		#checking for mating chases that were recorded as interactions instead
  		#pulled up all beh = 7 observations and then check the comments for those that are ACTUALLY MATING chases (did this manually by reading the comments)
  	id %in% c(3600, 13747, 23017, 50810, 50812, 50817, 96480, 107568, 131391, 182890, 382716, 389989, 1059,  3133, 3144, 3268, 3282, 3305, 3364, 3414, 3460, 3532, 3600, 4814, 4957, 5760, 6011, 30563, 39330, 39455, 39457, 49626, 49704, 49749, 49807, 49908, 49910, 50564, 50731, 50798, 50799, 50800, 51019, 51516, 52355, 52388, 53410, 53867, 54997, 78003, 78033, 78239, 78253, 78335, 78752, 78936, 79142, 79477, 79478, 79480, 79610, 80334, 80924, 81965, 87939, 95969, 96167, 96376, 96425, 97249, 97442, 97632, 104553, 104563, 104572, 104943, 104965, 104999, 105722, 106137, 106138, 106206, 106236, 106864, 107274, 107567, 107569, 107746, 107748, 108034, 108258, 108713, 109073, 109110, 109657, 120601, 120703, 121142, 125998, 126008, 126048, 126157, 127504, 128497, 131378, 181921, 183025, 183041, 184170, 184454, 184636, 184637, 375671, 382802, 383817, 383822, 383991, 384671, 384673, 389507, 389588, 389904, 410883, 414662)) %>% 
  	mutate(date=ymd(date)) %>% 
  	mutate(year=year(date)) %>%
  	select(squirrel_id, year, date, grid, time) %>%
  	ungroup()
  
old_beh <- tbl(con, "dbabehaviour") %>% 
  	collect() %>%
  	group_by(squirrel_id, date) %>%
  	filter(
  		row_number()==1, 
  		!is.na(squirrel_id), 
  		beh==22 |   		
  		#checking for mating chases that were recorded as interactions instead
  		#pulled up all beh = 7 observations and then check the comments for those that are ACTUALLY MATING chases (did this manually by reading the comments)
  		 id %in% c(23017, 49869, 50731, 78003, 78033, 78133, 78239, 78253, 78335, 78752, 78808, 78936, 79142, 79477, 80334, 80924, 95969, 96167, 96376, 96425, 96480, 97249, 97442, 97632, 104553, 104563, 104572, 104965, 104999, 105722, 106137, 106138, 106206, 106864, 107274, 107567, 107746, 107748, 108713, 109073, 109110, 109657, 120601, 120703, 121142, 13747, 30563, 39330, 39455, 39457, 41515, 49626, 49704, 49749, 49807, 49908, 49910, 50564, 50798, 50799, 50800, 51019, 51516, 52355, 52388, 53410, 53867, 54997)) %>% 
  	mutate(date=ymd(date)) %>% 
  	mutate(year=year(date)) %>%
  	select(squirrel_id, year, date, grid, time) %>%
  	ungroup()     
  
#importing Ryan Taylor's data
ryan<-read.csv("Chase data 2008.csv", header=T) %>%
	mutate(date=mdy(Date)) %>%
	mutate(year=year(date)) %>%
	group_by(dam_taglft, date) %>%
	filter(row_number()==1) %>%
	ungroup() %>%
	left_join((tbl(con, "litter") %>% collect()), by=c("dam_taglft"="mTagLft", "year"="yr")) %>% 
	select(squirrel_id, year, date, grid, time) 

#importing Jeff Lane's data
jeff<-read.csv("chase data JEL.csv", header=T) %>%
	mutate(date=dmy(date)) %>%
	mutate(year=year(date), 
			month=month(date)) %>%
	left_join((tbl(con, "litter") %>% collect()), by=c("left_tag"="mTagLft", "year"="yr")) %>% 
	mutate(squirrel_id=ifelse(mTagRt=="C6644", 6342, squirrel_id)) %>% #because the left tag is a rip, need to make sure it gets the right squirrel_id based on the right tag
	group_by(left_tag, month) %>% #squirrel 7278 (tags D5285/D5286) has two mating chases in same month, so I picked the later one
	filter(row_number()==1) %>%
	ungroup() %>%
	select(squirrel_id, year, date, grid=grid.x, time)

##################################
# Means calculated per Grid Year #
##################################
 		
cone_counts<-tbl(con, "cones") %>%
	collect() %>%
  	mutate(Year=as.numeric(Year),
		NumNew = as.numeric(NumNew),
		cone_index = log(NumNew + 1),
		total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01))) # according to Krebs et al. 2012

cones_grids_years <- group_by(cone_counts, Year, Grid) %>% 
  summarise(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(cone_index, na.rm = TRUE)) %>% 
  mutate(Year_tp1 = Year+1,
         cone_index_t = ifelse(is.finite(cone_index), cone_index, NA))

#link in cones from the previous year

cone_temp<-cones_grids_years %>% 
  select(Grid, Year, Year_tp1, cone_index_tm1=cone_index_t)

cones<-left_join(cones_grids_years, cone_temp, by=c("Grid", "Year" = "Year_tp1")) %>% 
  select(year=Year, grid=Grid, cone_index_t, cone_index_tm1)

summary(cones)
tail(cones)     
     
     
##################################
#creating final dataset
##################################
     
mating<-bind_rows(new_beh, old_beh, ryan, jeff) %>%  
  	left_join(rep, by=c("squirrel_id", "year")) %>%
  	mutate(grid=ifelse(grid=="SX", "SU", grid)) %>%
	left_join(cones, by=c("year"="year", "grid"="grid")) %>% #add cone stuff
  	mutate(gestation_days=fieldBDate-date, 
  		gestation_age=year-mom_byear,
  		gestation_age2= gestation_age^2) %>%
  	mutate(gestation_days=as.numeric(gestation_days),
  		#only have values for BT starting in 2018, replaced with values from JO
  		cone_index_t = ifelse(grid=="BT"& year==2017, 1.75, cone_index_t),
  		cone_index_tm1 = ifelse(grid=="BT" & year==2017, 0.108, cone_index_tm1)) %>%
  	filter(
  		!is.na(n_pups), #NA for n_pups is due to lost litters (no usable records sadly)
  		gestation_days>28, 
  		gestation_days<40) %>% #remove errors due to: 1) mating chases coming after litter is born (i.e., possible lost litter); 2) multiple mating chases and multiple litters
	mutate(mast=ifelse(year %in% c(1993, 1998, 2005, 2010, 2014, 2019, 2022), 1, 0)) %>%
	group_by(squirrel_id, year) %>% #use year here instead of date so that it keeps the first mating chase record for that year
	arrange(date) %>%
	filter(row_number()==n()) %>% #when there are duplicates (more than one mating chase, I have kept the last record)
	ungroup() %>%
	mutate(treatment = case_when( 
	grid == "AG" & year >= 2004 & year <= 2017 ~ "food", 
	grid == "LL" & year >= 2005 & year <= 2011 ~ "food", 
	grid == "JO" & year >= 2006 & year <= 2012 ~ "food", 
	grid == "LL" & year >= 2012 & year <= 2022 ~ "GC", 
	(grid == "JO" | grid == "BT") & year >= 2015 & year <= 2022 ~ "GC", 
		TRUE ~ "control" ))
	
summary(mating) 
head(mating)
str(mating)

#homework
#investigate outliers for gestation_days 
#make sure there are no sample sizes duplicates in final dataset after all the merging

summary(mating$gestation_days) #there are some possibly "impossible" values here
hist(mating$gestation_days) #need to investigate the outliers
mating%>%filter(gestation_days>37| gestation_days<30)

new_beh%>%filter(squirrel_id %in% c(13688, 19865, 20415, 22031, 20529, 21350, 24409, 24002, 24356)) 
old_beh%>%filter(squirrel_id %in% c(13688, 19865, 20415, 22031, 20529, 21350, 24409, 24002, 24356)) 
ryan%>%filter(squirrel_id %in% c(8330)) #8330
jeff%>%filter(squirrel_id %in% c(6495)) #6495

#consider keeping outliers (<30 and >37) for the first round of the manuscript and let coauthors comment on it 
#make sure to report the breakdown of where the records come from in the manuscript


####################################
#summary stats - sample sizes
####################################

nrow(mating) #174 records
mating %>% as_tibble() %>% count(squirrel_id) %>% nrow() #160 females			
mating %>% as_tibble() %>% count(id) %>% nrow() #174 ids (this number should be the same as the number of records, if it isn't, this means that a female has multiple mating chases that year on different days recorded for her, when this happens, I have opted to keep the last record (i.e., latest date))		

table(mating$grid)
table(mating$year)


#to see if there are duplicate mating chase records
distinct(mating) |>
  filter(.by = id, n() > 1) #if this has anything except 0 as an output this means there are replicates that need to be dealt with 

write.csv(mating, "/Users/april-martinig/Documents/Files/Post-docs/UofC/Naomi (PURE)/gestation/data/final_dataset.csv") 	



############################################################
############################################################
#manually checking the LTD for records that could be mating chases
############################################################
############################################################

#how I did this:

#I pulled up all beh = 7 observations that could ACTUALLY be MATING CHASES because the mating chases could have been recorded as interactions instead
#went through all the comments manually and looked for "MC", "mating chase", "copulation", or some version of these words
#after I went through the records, I added the ids to the filter functions above for the relevant datasets


### IMPORTANT ###
#before ~ 2005/2006 males did not have pipe cleaners, so their colours look EXACTLY THE SAME as female colours (hence, I merged the behaviour records with the flastall records to filter out males)


new_beh_checks <- tbl(con, "behaviour") %>% 
  	collect() %>%
  	left_join((tbl(con, "flastall2") %>% 
  	collect() %>%
  	select(squirrel_id, sex)), by="squirrel_id") %>%
  	mutate(date=ymd(date)) %>% 
  	mutate(year=year(date)) %>%
  	filter(
  		sex=="F",
  		comments!="",
  		!is.na(squirrel_id), 
  		behaviour==7,
  		!(color_left %in% c("B*", "R*", "Y*", "W*","O*", "G*", "R!", "W!", "Y!", "G!", "B!", "BK!", "Bk!", "O!", "P!")),
  		!(color_right %in% c("B*", "R*", "Y*", "W*","O*", "G*", "R!", "W!", "Y!", "G!", "B!", "BK!", "Bk!", "O!", "P!"))) %>% 
  	mutate(date=ymd(date)) %>% 
  	select(squirrel_id, sex, color_left, color_right, comments, id) %>%
  	arrange(id)  
  	
new_beh_checks  
  
  
old_beh_checks <- tbl(con, "dbabehaviour") %>% 
  	collect() %>%
  	left_join((tbl(con, "flastall2") %>% 
  	collect() %>%
  	select(squirrel_id, sex)), by="squirrel_id") %>%
  	mutate(date=ymd(date)) %>% 
  	mutate(year=year(date)) %>%
  	filter(
  		year<2006,
  		sex=="F",
  		comments!="",
  		!is.na(squirrel_id), 
  		beh==7, !(color_left %in% c("B*", "R*", "Y*", "W*","O*", "G*", "R!", "W!", "Y!", "G!", "B!", "BK!", "Bk!", "O!", "P!")),
  		!(color_right %in% c("B*", "R*", "Y*", "W*","O*", "G*", "R!", "W!", "Y!", "G!", "B!", "BK!", "Bk!", "O!", "P!"))) %>% 
  	mutate(date=ymd(date)) %>% 
  	#select(squirrel_id, sex, color_left, color_right, comments, id) %>%
  	arrange(id) 
  	
old_beh_checks