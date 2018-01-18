setwd("~/Desktop")
raw_dat<-read.csv("Holmon_incubations.csv")

##### Resp (averaged over 4 replicates) over time

# create respiration data frame
library(dplyr)
resp_dat<-filter(raw_dat,resp_rate_ug_CO2.g_soil.h.!="NA")
grazing<-resp_dat$grazing_treatment
litter<-resp_dat$litter_treatment
resp<-resp_dat$resp_rate_ug_CO2.g_soil.h.
day<-resp_dat$day

# Create figure: Resp over incbuation
library(ggplot2)

# Graph with points (messy)
ggplot(data=resp_dat,aes(x=day,y=resp,color=litter,linetype=grazing,shape=grazing)) +
  geom_smooth(fill=NA) +
  geom_point() +
  theme(legend.position="top",
        #scale_linetype_discrete()
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
        )

# Graph with points (messy)

  
# Graph without points (better)
ggplot(data=resp_dat,aes(x=day,y=resp,color=litter,linetype=grazing)) +
  geom_smooth(fill=NA) +
  scale_colour_hue(labels=c("Aspen","Empetrum","Lichen","Moss"),name="Litter Added") +
  scale_linetype_discrete(labels=c("Grazed","Ungrazed","Moss-Dominated"),name="Grazing Cond.") +
  labs(x= "Incubation Day", y= "Respiration Rate (ug CO2/g soil/hr)") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
        )

