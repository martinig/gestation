#code to generate plots
#original code by A. R. Martinig
#last edited on May 15, 2024 by A. R. Martinig 



#dot and whisker plots of model coefficients

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(scales)

plot_model(model, 
	terms = c("gestation_age", "n_pups", "cone_index_tm1", "mast"),
	axis.labels=c("gestation_age"="Gestation age",  "n_pups"="Litter size", "cone_index_tm1"="Previous cone index", "mast"="Mast"),
	colors=c("#000000", "#999999"),
    #axis.lim=c(-30, 40),
	sort.est=TRUE, 
	show.values=TRUE, 
	value.offset=0.3, 
	title = "",
	axis.title="Estimates ± 95% CIs",
	vline.color="lightgrey") + 
	label_angle(base.theme = theme_squirrel)

a<-ggplot(mating, aes(x= as.factor(gestation_age), y= gestation_days))+
	geom_boxplot(outlier.size=3, varwidth=TRUE, na.rm=TRUE)+
	scale_y_continuous(breaks = pretty_breaks(5))+ 
	theme_squirrel +
	labs(x="Gestation age (years)", y="Gestation length (days)")

b<-ggplot(mating, aes(x= as.factor(n_pups), y= gestation_days))+
	geom_boxplot(outlier.size=3, varwidth=TRUE, na.rm=TRUE)+
	scale_y_continuous(breaks = pretty_breaks(5))+ 
	theme_squirrel +
	labs(x="Litter size", y="Gestation length (days)")

c<-ggplot(mating, aes(x= cone_index_tm1, y= gestation_days)) + 
	geom_point(shape=16,cex=3) + 
	labs(y="Gestation length (days)", x="Previous year cone crop (index)") +
	scale_y_continuous(breaks = pretty_breaks(5))+ 
	scale_x_continuous(breaks = pretty_breaks(5))  +
	theme_squirrel +
    geom_smooth(aes(y = gestation_days, x= cone_index_tm1), method=lm, se=TRUE, fullrange=TRUE, color="grey")

d<-ggplot(mating, aes(x= as.factor(mast), y= gestation_days)) + 
	geom_boxplot(outlier.size=3, varwidth=TRUE, na.rm=TRUE)+
	scale_y_continuous(breaks = pretty_breaks(5))+ 
	labs(x="Mast year", y="Gestation length (days)")+
	scale_x_discrete(labels=c("Non-mast year", "Mast year"))+
	theme_squirrel
	
cowplot::plot_grid(a, b, c, d, labels=c("(a)", "(b)", "(c)", "(d)"), ncol = 2, nrow =2, align = "hv", label_x=0.8, label_y=1)














