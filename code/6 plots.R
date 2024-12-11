#code to generate plots
#original code by A. R. Martinig
#last edited on December 11, 2024 by A. R. Martinig


#dot and whisker plots of model coefficients

a<-plot_model(model_sd, 
	terms = c("gestation_age2_sd", "litter_ratio_sd", "n_pups_sd", "cone_index_tm1", "mast1", "treatmentfood", "treatmentGC"),
	axis.labels=c("gestation_age2_sd"=expression(Gestation~age^2),  "litter_ratio_sd"="Litter sex ratio",  "n_pups_sd"="Litter size", "cone_index_tm1"="Previous year\ncone abundance", "mast1"="Spruce mast\nyear", "treatmentfood"="Food add", "treatmentGC"="Experiemental"),
	order.terms = c(1,2,3,4,5,6,7),
	colors=c("#000000"),
	transform=NULL, #plots the untransformed output/estimates
	sort.est=TRUE, 
	show.values=TRUE, 
	value.offset=0.3, 
	title = "",
	axis.title="Estimates ± 95% CIs",
	vline.color="lightgrey") + 
	label_angle(base.theme = theme_squirrel) +
	theme(axis.text.y = element_text(lineheight = 0.7))

b<-ggplot(gest, aes(x= as.factor(gestation_age), y= gestation_days))+
	geom_boxplot(outlier.shape=NA, varwidth=TRUE, na.rm=TRUE)+
	geom_jitter(colour="darkgrey", position = position_jitter(width = .2))+
  coord_cartesian(ylim = c(28, 40)) +
  scale_y_continuous(breaks = seq(28, 40, by = 2)) +
	labs(x="Maternal age (years)", y="Gestation length (days)") +
  theme_squirrel
		
cowplot::plot_grid(a, b, labels=c("(a)", "(b)"), ncol = 2, label_x=0.9, label_y=0.95)



#ggplot(gest, aes(x= as.factor(mast), y= gestation_days)) + 
	geom_boxplot(outlier.shape=NA, varwidth=TRUE, na.rm=TRUE)+
	geom_jitter(colour="darkgrey", position = position_jitter(width = .2))+
	scale_y_continuous(breaks = pretty_breaks(5))+ 
	labs(x="Spruce mast year", y="Gestation length (days)") +
	scale_x_discrete(labels=c("Non-mast year", "Mast year")) +
	theme_squirrel

#plot(ggpredict(model_sd, terms = "mast"))+ 
	labs(x="Spruce mast year", y="Gestation length (days)", title="") +
  coord_cartesian(ylim = c(28, 40)) +
  scale_y_continuous(breaks = seq(28, 40, by = 2)) + 
	scale_x_continuous(labels=c("Non-mast year", "Mast year"), breaks=c(0, 1)) +
	theme_squirrel 
