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
  scale_colour_hue(labels=c("Aspen","Empetrum","Lichen","Moss"),name="Litter Added") +
  scale_linetype_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  scale_shape_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  labs(x= "Incubation Day", y= "Respiration Rate (ug CO2/g soil/hr)") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
  )
  
# Graph without points (better)
ggplot(data=resp_dat,aes(x=day,y=resp,color=litter,linetype=grazing)) +
  geom_smooth(fill=NA) +
  scale_colour_hue(labels=c("Aspen","Empetrum","Lichen","Moss"),name="Litter Added") +
  scale_linetype_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  labs(x= "Incubation Day", y= "Respiration Rate (ug CO2/g soil/hr)") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
        )

##### Bacterial growth (averaged over 4 replicates) over time

# Create bacterial growth data frame
library(dplyr)
bact_dat<-filter(raw_dat,bacterial_growth!="NA")
grazingBact<-bact_dat$grazing_treatment
litterBact<-bact_dat$litter_treatment
bact_growth<-bact_dat$bacterial_growth
dayBact<-bact_dat$day

# Create figure, bacterial growth over incubation (no points, clean)
ggplot(data=bact_dat,aes(x=dayBact,y=bact_growth,color=litterBact,linetype=grazingBact)) +
  geom_smooth(fill=NA) +
  scale_colour_hue(labels=c("Aspen","Empetrum","Lichen","Moss"),name="Litter Added") +
  scale_linetype_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  labs(x= "Incubation Day", y= "Bacterial Growth Rate (picomoles leucine/g soil/hr) ") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
        )

# Create figure, bacterial growth over incubation (points, messy)
ggplot(data=bact_dat,aes(x=dayBact,y=bact_growth,color=litterBact,linetype=grazingBact,shape=grazingBact)) +
  geom_smooth(fill=NA) +
  geom_point() +
  scale_colour_hue(labels=c("Aspen","Empetrum","Lichen","Moss"),name="Litter Added") +
  scale_linetype_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  scale_shape_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  labs(x= "Incubation Day", y= "Bacterial Growth Rate (picomoles leucine/g soil/hr) ") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
        )
