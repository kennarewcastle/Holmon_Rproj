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

cumulative_response<-function(dat,response_name){
  library(dplyr)
  response<-dat$response_name
  N<-length(response)
  response_vect<-c()
  treat_vect<-c()
  Day<-as.numeric(dat$day)
  treat<-dat$treatment_ID
  
  for (i in 1:N) {
    #if (Day[i] > 0) { 
    inc_growth<-((response[i]+response[i-1]/2)*((Day[i]-Day[(i-1)])*24))
    response_vect[i]<-inc_growth
    treat_vect[i]<-treat[i]
    #}
    #else {
    #response_vect[i]<-0
    #treat_vect[i]<-treat[i]
    #}
  }

  results<-cbind(treat_vect,response_vect)
  #UA<-filter(results,treat_vect=="UA")
  #UE<-filter(results,treat_vect=="UE")
  #UL<-filter(results,treat_vect=="UL")
  #UM<-filter(results,treat_vect=="UM")
  #GA<-filter(results,treat_vect=="GA")
  #GE<-filter(results,treat_vect=="GE")
  #GL<-filter(results,treat_vect=="GL")
  #GM<-filter(results,treat_vect=="GM")
  #MA<-filter(results,treat_vect=="MA")
  #ME<-filter(results,treat_vect=="ME")
  #ML<-filter(results,treat_vect=="ML")
  #MM<-filter(results,treat_vect=="MM")
  
  #treatNames<-c("UA","UE","UL","UM","GA","GE","GL","GM","MA","ME","ML","MM")
  #totals<-c(sum(UA$response_vect),sum(UE$response_vect),sum(UL$response_vect),sum(UM$response_vect),sum(GA$response_vect),sum(GE$response_vect),sum(GL$response_vect),sum(GM$response_vect),sum(MA$response_vect),sum(ME$response_vect),sum(ML$response_vect),sum(MM$response_vect))
  
  #ENDresults<-cbind(treatNames,totals)
  #return(ENDresults)
  return(results)
}

##### Test function for resp, bact growth, and fungal growth

# Respiration
resp_dat<-filter(raw_dat,resp_rate_ug_CO2.g_soil.h.!="NA") # "dat" data frame that function will work with
respdat6<-filter(resp_dat,replicate_numb==6)
resp6<-respdat6$resp_rate_ug_CO2.g_soil.h.
day6<-as.numeric(respdat6$day)
treat6<-respdat6$treatment_ID
dat6<-data.frame(resp6,day6,treat6)

cumulative_response(dat=dat6,response_name=resp_rate_ug_CO2.g_soil.h.)

bact_df<-data.frame(rep6=c(), rep1=c(), rep8=c(), rep7=c())
fung_df<-data.frame(rep6=c(), rep1=c(), rep8=c(), rep7=c())


#### JUNK 
### Problem is that data should be sorted by replicate, ID (UA, UE, etc), day so that day[i-1]
### corresponds to previous sampling day.
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

#### More junk

UAdat6<-filter(dat6,treat6=="UA")


cumulative_response<-function(dat){
  library(dplyr)
  response<-dat$resp6
  N<-length(response)
  response_vect<-c()
  #treat_vect<-c()
  Day<-dat$day6
  treat<-dat$treat6
  
  for (i in 1:N) {
    if (Day[i] > 0) { 
    inc_growth<-((response[i]+response[i-1]/2)*((Day[i]-Day[(i-1)])*24))
    response_vect[i]<-inc_growth
    #print(treat[i])
    #treat_vect[i]<-treat[i ]
    }
    else {
    response_vect[i]<-0
    #treat_vect[i]<-treat[i]
    }
  }
  treat_vect <- as.factor(treat)
  results<-cbind(treat_vect,response_vect)
  results<- as.data.frame(results)
  results$treat_vect <- as.factor(results$treat_vect)
  #print(head(results))
  #print(summary(results))
  #results$treat_vect <- as.factor(results$treat_vect)
  #ltr <- levels(results$treat_vect)
  #res.test <- sum(results[, results$response_vect])
  #print(res.test)
  #res.test <- dplyr(results, .(treat_vect), summarise())
  #return(res.test)
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
  res <- cbind(UA$response_vect,UE$response_vect,UL$response_vect,UM$response_vect,GA$response_vect,GE$response_vect,GL$response_vect,GM$response_vect,MA$response_vect,ME$response_vect,ML$response_vect,MM$response_vect)
  colnames(res) <- c("UA","UE","UL","UM","GA","GE","GL","GM","MA","ME","ML","MM")

  #totals<-c(sum(UA$response_vect),sum(UE$response_vect),sum(UL$response_vect),sum(UM$response_vect),sum(GA$response_vect),sum(GE$response_vect),sum(GL$response_vect),sum(GM$response_vect),sum(MA$response_vect),sum(ME$response_vect),sum(ML$response_vect),sum(MM$response_vect))
  
  totals <- apply(res, 2, sum)
  print(totals)
#  res <- as.data.frame(res)
  print(head(res, 20))
  #ENDresults<-cbind(treatNames,totals)
  #return(ENDresults)

}

cumulative_response(dat=dat6)

##### Test function for resp, bact growth, and fungal growth

# Respiration
resp_dat<-filter(raw_dat,resp_rate_ug_CO2.g_soil.h.!="NA") # "dat" data frame that function will work with
respdat6<-filter(resp_dat,replicate_numb==6)
resp6<-respdat6$resp_rate_ug_CO2.g_soil.h.
day6<-as.numeric(respdat6$day)
treat6<-respdat6$treatment_ID
dat6<-data.frame(resp6,day6,treat6)

cumulative_response(dat=dat6)

bact_df<-data.frame(rep6=c(), rep1=c(), rep8=c(), rep7=c())
fung_df<-data.frame(rep6=c(), rep1=c(), rep8=c(), rep7=c())

