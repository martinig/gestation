#all the data extraction and cleaning is here 
#original code by A. R. Martinig
#last edited on May 15, 2024 by A. R. Martinig 



#homework
#make sure the sample sizes are not duplicated in final dataset after all the merging






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

juv <- tbl(con, "juvenile") %>% 
  	collect() %>%
  	group_by(litter_id) %>%
  	mutate(n_pups=n()) %>% 
  	select(litter_id, n_pups)

rep<-left_join(litter, juv, by=c("id"="litter_id"))

new_beh <- tbl(con, "behaviour") %>% 
  	collect() %>%
  	group_by(squirrel_id, date) %>%
  	filter(
  		row_number()==1, 
  		!is.na(squirrel_id), 
  		behaviour==22 | id %in% c(1059, 3133, 3282, 3600, 13747, 23017, 30563, 39330, 39455, 39457, 49626, 49704, 49749, 49807, 49869, 49908, 49910, 50564, 50798, 50799, 50800, 50810, 50812, 50817, 51019, 51516, 52355, 52388, 53410, 78033, 78239, 78253, 78335, 78936, 79142, 79477, 80334, 80924, 95969, 96167, 96376, 96480, 97442, 97632, 104553, 104563, 104572, 104943, 104965, 104999, 105722, 106137, 106138, 106206, 106864, 107274, 107567, 107568, 107746, 107748, 108034, 108258, 109073, 109110, 109657, 120601, 120703, 121142, 125998, 126008, 126048, 126157, 127504, 131378, 131391, 182890, 183025, 183041, 184170, 184454, 184485, 184636, 184637, 375671, 382716, 384671, 384673, 389904, 389989, 410883)) %>% 
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
  		beh==22 | id %in% c(13747, 23017, 30563, 39330, 39455, 39457, 49626, 49704, 49749, 49807, 49869, 49908, 49910, 50564, 50731, 50798, 50799, 50800, 51019, 52355, 52388, 53410, 78003, 78033, 78133, 78239, 78253, 78335, 78752, 78808, 78936, 79142, 79477, 80334, 80924, 95969, 96167, 96376, 96425, 96480, 97249, 97442, 97632, 104553, 104563, 104572, 104965, 104999, 105722, 106137, 106138, 106206, 106864, 107274, 107567, 107746, 107748, 108713, 109073, 109110, 109657, 120601, 120703, 121142)) %>% 
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
     
     
     
     
mating<-bind_rows(new_beh, old_beh, ryan, jeff) %>%  
  	left_join(rep, by=c("squirrel_id", "year")) %>%
	left_join(cones, by=c("year"="year", "grid"="grid")) %>% #add cone stuff
  	mutate(gestation_days=fieldBDate-date, 
  		gestation_age=year-mom_byear) %>%
  	mutate(gestation_days=as.numeric(gestation_days)) %>%
  	filter(
  		!is.na(n_pups), #NA for n_pups is due to lost litters
  		gestation_days>28, 
  		gestation_days<40) %>% #remove errors due to: 1) mating chases coming after litter is born (i.e., possible lost litter); 2) multiple mating chases and multiple litters
	mutate(mast=ifelse(year %in% c(1993, 1998, 2005, 2010, 2014, 2019), 1, 0)) %>%
	group_by(squirrel_id, date) %>%
	filter(row_number()==1) %>%
	ungroup() 

summary(mating) 
head(mating)
nrow(mating) #166
str(mating)




######
#checking for mating chases that were recorded as interactions instead

#pull up all beh = 7 observations that are ACTUALLY MATING chases (will need to decide on a case by case basis)

### IMPORTANT ###
#before ~ 2005/2006 males did not have pipe cleaners, so their colours look EXACTLY THE SAME as female colours (hence, I merged the behaviour records with the flastall records to filter out males)

new_beh <- tbl(con, "behaviour") %>% 
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
  
old_beh <- tbl(con, "dbabehaviour") %>% 
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
  	select(squirrel_id, sex, color_left, color_right, comments, id) %>%
  	arrange(id) 	
  	
  	
  	
write.csv(final_gestation_df, "/Users/april-martinig/Documents/Files/Post-docs/UofC/Naomi (PURE)/gestation/data/final_dataset.csv") 	
        