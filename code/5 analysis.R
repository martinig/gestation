#analysis investigating factors that may affect gestation length
#original code by A. R. Martinig
#last edited on May 15, 2024 by A. R. Martinig 



##################################
###### Statistical analysis ######
##################################

#mean center and standardize numerical variables before running model

mating2<-mating %>% 
	ungroup() %>%
	group_by(grid, year) %>%
	mutate(
		gestation_age=((gestation_age-mean(gestation_age))/(1*(sd(gestation_age)))),
		gestation_age = replace(gestation_age, is.na(gestation_age), 0),
		n_pups=((n_pups-mean(n_pups, na.rm=T))/(1*(sd(n_pups, na.rm=T)))),
		n_pups = replace(n_pups, is.na(n_pups), 0),
		#can't standardize cone_index_tm1
		year=(year-2000)) %>%
	ungroup()

summary(mating2)

#mean > var, #good to use logistic regression
#if var > mean, then the data would be significantly overdispersed (variance > mean)
mean(mating2$gestation_days)
var(mating2$gestation_days)

plot(mating2$gestation_days)

model<-lmer(gestation_days ~ gestation_age + mast + n_pups + cone_index_tm1 + (1|year), data = mating2)
summary(model)

plot(model) 
hist(resid(model))

confint(model,method='Wald')












