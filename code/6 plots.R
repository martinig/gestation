#code to generate plots
#original code by A. R. Martinig
#last edited on June 18, 2024 by A. R. Martinig 


#dot and whisker plots of model coefficients

plot_model(model_sd, 
	terms = c("gestation_age_sd", "litter_ratio_sd", "n_pups_sd", "cone_index_tm1", "mast1"),
	axis.labels=c("gestation_age_sd"="Gestation age",  "litter_ratio_sd"="Litter sex ratio",  "n_pups_sd"="Litter size", "cone_index_tm1"="Previous year\ncone abundance", "mast1"="Spruce mast\nyear"),
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
	scale_y_continuous(breaks = pretty_breaks(5))+ 
	theme_squirrel +
	labs(x="Maternal age (years)", y="Gestation length (days)")

c<-ggplot(gest, aes(x= as.factor(mast), y= gestation_days)) + 
	geom_boxplot(outlier.shape=NA, varwidth=TRUE, na.rm=TRUE)+
	geom_jitter(colour="darkgrey", position = position_jitter(width = .2))+
	scale_y_continuous(breaks = pretty_breaks(5))+ 
	labs(x="Spruce mast year", y="Gestation length (days)")+
	scale_x_discrete(labels=c("Non-mast year", "Mast year"))+
	theme_squirrel

d<-plot(ggpredict(model_sd, terms = "mast"))+ 
	labs(x="Spruce mast year", y="Gestation length (days)", title="") +
	scale_y_continuous(breaks = c(34, 35, 36))+ 
	scale_x_continuous(labels=c("Non-mast year", "Mast year"), breaks=c(0, 1))+
	theme_squirrel 
		
cowplot::plot_grid(b, d, labels=c("(a)", "(b)"), ncol = 2, nrow =1, align = "hv", label_x=0.9, label_y=0.95)