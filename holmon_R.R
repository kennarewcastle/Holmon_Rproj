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
fung_growth<-fung_dat$bacterial_growth
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

cumulative_response<-function(dat,response_name){
  library(dplyr)
  
  response<-dat$response_name
  N<-length(response)
  response_vect<-c()
  day<-dat$day
  
  for (i in 1:N) {
    inc_growth<-response[i]*(day[i]-day[(i-1)])
    response_vect[i]<-inc_growth
  }

  
  cumulative_growth<-sum(response_vect)
  
  return(cumulative_growth)

}

##### Test function for resp, bact growth, and fungal growth

# Respiration
resp_dat<-filter(raw_dat,resp_rate_ug_CO2.g_soil.h.!="NA") # "dat" data frame that function will work with
real_respdat<-filter(resp_dat,day>0)
real_respdat6<-filter(real_respdat,replicate_numb==6)

cumulative_response(dat=real_respdat6, response_name=resp_rate_ug_CO2.g_soil.h.)

bact_df<-data.frame(rep6=c(), rep1=c(), rep8=c(), rep7=c())
fung_df<-data.frame(rep6=c(), rep1=c(), rep8=c(), rep7=c())


#### JUNK
cumulative_response<-function(dat,response_name){
  library(dplyr)
  
  rep6<-filter(dat,replicate_numb==6)
  rep1<-filter(dat,replicate_numb==1)
  rep8<-filter(dat,replicate_numb==8)
  rep7<-filter(dat,replicate_numb==7)
  
  response6<-rep6$response_name
  response1<-rep1$response_name
  response8<-rep8$response_name
  response7<-rep7$response_name
  
  day6<-rep6$day
  day1<-rep1$day
  day8<-rep8$day
  day7<-rep7$day
  
  N6<-length(response6)
  N1<-length(response1)
  N8<-length(response8)
  N7<-length(response7)
  
  response_rep6<-c()
  response_rep8<-c()
  response_rep7<-c()
  response_rep1<-c()
  
  for (i in 1:N6) {
    inc_growth6<-response6[i]*(day6[i]-day6[(i-1)])
    response_rep6[i]<-inc_growth6
  }
  
  for (j in 1:N1){
    inc_growth1<-response1[j]*(day1[j]-day1[(j-1)])
    response_rep1[j]<-inc_growth1
  }
  
  for (k in 1:N8) {
    inc_growth8<-response8[k]*(day8[k]-day8[(k-1)])
    response_rep8[k]<-inc_growth8
  }
  
  for (l in 1:N7) {
    inc_growth7<-response7[l]*(day7[l]-day7[(l-1)])
    response_rep7[l]<-inc_growth7
  }
  
  rep6_total<-sum(response_rep6)
  rep1_total<-sum(response_rep1)
  rep8_total<-sum(response_rep8)
  rep7_total<-sum(response_rep7)
  
  summary_vect<-c(rep6_total,rep1_total,rep8_total,rep7_total)
  
  return(summary_vect)
  
}
