#analysis investigating factors that may affect gestation length
#original code by A. R. Martinig
#last edited on December 19, 2024 by N. Brunner

##################################
###### Statistical analysis ######
##################################

#mean center and standardize numerical variables before running model

final_df<-gest %>% 
	ungroup() %>%
	group_by(grid) %>%
	mutate(
		mast=as.factor(mast),
		treatment=as.factor(treatment),
		gestation_age_sd=((gestation_age-mean(gestation_age))/(1*(sd(gestation_age)))),
		gestation_age_sd = replace(gestation_age_sd, is.na(gestation_age_sd), 0),
		gestation_age2_sd=((gestation_age2-mean(gestation_age2))/(1*(sd(gestation_age2)))),
		gestation_age2_sd = replace(gestation_age2_sd, is.na(gestation_age2_sd), 0),
		n_pups_sd=((n_pups-mean(n_pups, na.rm=T))/(1*(sd(n_pups, na.rm=T)))),
		n_pups_sd = replace(n_pups_sd, is.na(n_pups_sd), 0),
		litter_ratio_sd =((litter_ratio-mean(litter_ratio, na.rm=T))/(1*(sd(litter_ratio, na.rm=T)))),
		litter_ratio_sd = replace(litter_ratio_sd, is.na(litter_ratio_sd), 0),
		#can't standardize cone_index_tm1
		year_sd=(year-1995)) %>%
	ungroup() 

summary(final_df)

#correlations 
attach(final_df);tt=cbind(gestation_age, gestation_age2, litter_ratio, n_pups,  mast, cone_index_tm1)
cor(tt) 
#gestation_age & gestation_age2 are highly correlated (0.98) #should not keep both in the analysis
#highest value is 0.37

#mean > var, #good to use logistic regression
#if var > mean, then the data would be significantly overdispersed (variance > mean)
mean(final_df $gestation_days)
var(final_df $gestation_days)

plot(final_df $gestation_days)

#standardized model

model_sd<-lmer(gestation_days ~ gestation_age2_sd + litter_ratio_sd + n_pups_sd + mast + cone_index_tm1 + treatment + (1|year_sd), data= transform(final_df, treatment =relevel(treatment, "control"))) #relevel is to make the control the reference category
summary(model_sd)

plot(model_sd) 
hist(resid(model_sd))

confint(model_sd, method='Wald')

#variance inflation factor for standardized model
vif(model_sd)

#model with variables not standardized
model<-lmer(gestation_days ~ gestation_age2 + litter_ratio + n_pups + mast + cone_index_tm1 + treatment + (1|year), data= transform(final_df, treatment =relevel(treatment, "control")))
summary(model)

plot(model) 
hist(resid(model))

confint(model,method='Wald')