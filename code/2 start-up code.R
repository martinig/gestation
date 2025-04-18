#script to download packages and set formatting for plots
#original code by A. R. Martinig
#last edited on June 18, 2024 by A. R. Martinig 

options(scipen=999, dplyr.width = Inf, tibble.print_min = 50, repos='http://cran.rstudio.com/') #scipen forces outputs to not be in scientific notation #dplyr.width will show all columns for head() function and tibble.print_min sets how many rows are printed and repos sets the cran mirror

#load libraries
pacman::p_load(
				dplyr,
				lubridate,
				tidyverse,
				data.table,
				lme4,
               	krsp,
               	sjPlot,
               	sjlabelled, 
               	sjmisc,
               	scales,
               	ggplot2,
               	ggeffects,
				car
)

select<-dplyr::select
filter<-dplyr::filter



#conserved theme across plots for plots
#general theme
theme_squirrel <-
	theme_bw() +
	theme(axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 15)) 
 
#dot-whisker plot theme       
theme_squirrel_dot <- 
	theme_bw() +
 	theme(plot.margin = margin(0, 0.5, 0, 0, "cm"),
		axis.line=element_line(),
		axis.line.y=element_blank(),
		axis.ticks.length=unit(0.4, "cm"),
		axis.ticks.y=element_blank(),
    	axis.text=element_text(size=10), #changes size of axes #s
        axis.title=element_text(size=15), #changes size of axes labels
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 10))      
        