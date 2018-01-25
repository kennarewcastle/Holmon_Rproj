setwd("~/Desktop")
raw_dat<-read.csv("Holmon_incubations.csv") ##### IN ORDER FOR FUNCTION TO WORK, dat is ordered by:
# rep number -> grazing treatment (U,G,M) -> litter treatment (A,E,L,M) --> day


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
  labs(x= "Incubation Day", y= "Respiration Rate (ug CO2/g dry soil/hr)") +
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
  labs(x= "Incubation Day", y= "Bacterial Growth Rate (pmole leucine/g dry soil/hr) ") +
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
  labs(x= "Incubation Day", y= "Bacterial Growth Rate (pmole leucine/g soil/hr) ") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
        )

##### Fungal growth (averaged over 4 replicates) over time

# Create fungal growth data frame
library(dplyr)
fung_dat<-filter(raw_dat,fungal_growth_pmol_ergosterol_h.1_g_dry_soil..1!="NA")
grazingFung<-fung_dat$grazing_treatment
litterFung<-fung_dat$litter_treatment
fung_growth<-fung_dat$fungal_growth_pmol_ergosterol_h.1_g_dry_soil..1
dayFung<-fung_dat$day

# Create figure, fungal growth over incubation (no points, clean)
ggplot(data=fung_dat,aes(x=dayFung,y=fung_growth,color=litterFung,linetype=grazingFung)) +
  geom_smooth(fill=NA) +
  scale_colour_hue(labels=c("Aspen","Empetrum","Lichen","Moss"),name="Litter Added") +
  scale_linetype_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  labs(x= "Incubation Day", y= "Fungal Growth Rate (pmole ergosterol/g dry soil/hr) ") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
      )

# Create figure, fungal growth over incubation (points, messy)
ggplot(data=fung_dat,aes(x=dayFung,y=fung_growth,color=litterFung,linetype=grazingFung,shape=grazingFung)) +
  geom_smooth(fill=NA) +
  geom_point() +
  scale_colour_hue(labels=c("Aspen","Empetrum","Lichen","Moss"),name="Litter Added") +
  scale_linetype_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  scale_shape_discrete(labels=c("Grazed","Moss-Dominated","Ungrazed"),name="Grazing Cond.") +
  labs(x= "Incubation Day", y= "Fungal Growth Rate (pmole ergosterol/g dry soil/hr) ") +
  theme(legend.position="top",
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        axis.text=element_text(colour="black",size=10),
        axis.title=element_text(size=14,face="bold"),
        panel.border=element_rect(fill=NA,colour="black",size=1.5),
        panel.background=element_rect(fill=NA)
        )

##### Create function to calculate cumulative resp, bacterial growth, and fungal growth over incubation

cumulative_response<-function(dat){
  library(dplyr)
  response<-dat$resp6 # Will need to change this for each replicate/response variable
  N<-length(response)
  response_vect<-c()
  Day<-dat$day6
  treat<-dat$treat6
  
  for (i in 1:N) {
    if (Day[i] > 0) { 
    inc_growth<-((response[i]+response[i-1]/2)*((Day[i]-Day[(i-1)])*24))
    response_vect[i]<-inc_growth
    }
    else {
    response_vect[i]<-0
    }
  }
  treat_vect <- as.factor(treat)
  results<-cbind(treat_vect,response_vect)
  results<- as.data.frame(results)
  results$treat_vect <- as.factor(results$treat_vect)

  names(results) <- c("treat_vect", "response_vect")
  UA<-results[treat_vect=="UA",]
  UE<-results[treat_vect=="UE",]
  UL<-results[treat_vect=="UL",]
  UM<-results[treat_vect=="UM",]
  GA<-results[treat_vect=="GA",]
  GE<-results[treat_vect=="GE",]
  GL<-results[treat_vect=="GL",]
  GM<-results[treat_vect=="GM",]
  MA<-results[treat_vect=="MA",]
  ME<-results[treat_vect=="ME",]
  ML<-results[treat_vect=="ML",]
  MM<-results[treat_vect=="MM",]
  
  treatNames<-c("UA","UE","UL","UM","GA","GE","GL","GM","MA","ME","ML","MM")
  res <- cbind(UA$response_vect,UE$response_vect,UL$response_vect,UM$response_vect,GA$response_vect,GE$respnse_vect,GL$response_vect,GM$response_vect,MA$response_vect,ME$response_vect,ML$response_vect,MM$response_vect)
  colnames(res) <- c("UA","UE","UL","UM","GA","GE","GL","GM","MA","ME","ML","MM")

  totals <- apply(res, 2, sum)
 
  print(totals)
  print(head(res, 20))

}

##### Test function for resp, bact growth, and fungal growth

# Respiration
resp_dat<-filter(raw_dat,resp_rate_ug_CO2.g_soil.h.!="NA")
respdat6<-filter(resp_dat,replicate_numb==6)
resp6<-respdat6$resp_rate_ug_CO2.g_soil.h.
day6<-as.numeric(respdat6$day)
treat6<-respdat6$treatment_ID
dat6<-data.frame(resp6,day6,treat6)

cumulative_response(dat=dat6)


