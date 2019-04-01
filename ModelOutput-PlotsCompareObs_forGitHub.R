### Visualize MyLake modelled temperature output,
### including detailed comparisons with observed temperature data
### RMP (last modified 2019-MAR-27)

## load necessary packages:

library(tidyverse)
library(lubridate)
library(Metrics)
library(scales)
library(akima)
library(ggpubr)
library(rLakeAnalyzer)


## set folder directory for files:

path="G:/My Drive/rachel-PC/Miami-OH/Sentinel North - Laval/MyLake_public/v12/Giles_application_GitHub"
setwd(path)


## names for the temperature output and ice/snow output file
## (default names listed here)

temp.file="ModelledTemp-Giles_modelOutput.csv"
ice.file="ModelledHis-Giles_modelOutput.csv"


## WATER TEMPERATURE COMPARISONS, DIAGNOSTICS, & EVALUATIONS through line 282

## read in modelled water temperature data, clean up,
## and average layers to 1 m increment readings

mod.temp=as.data.frame(t(read.csv(temp.file,header=F)))
colnames(mod.temp)=seq(0.5,24.5,by=1)
mod.temp.all=mod.temp %>%
  mutate(Date=seq.Date(as.Date("2016-05-17"),as.Date("2018-12-31"),by=1)) %>%
  gather(key="Depth",value="Mod_Temp",`0.5`:`24.5`) %>%
  mutate(Depth=as.numeric(Depth))
mod.temp.interp=interp(x=mod.temp.all$Date,y=mod.temp.all$Depth,z=mod.temp.all$Mod_Temp,
                       xo=seq.Date(min(mod.temp.all$Date),max(mod.temp.all$Date),by=1),
                       yo=seq(0.5,25,by=0.5),
                       extrap=F,linear=T)
mod.temp.clean=as.data.frame(interp2xyz(mod.temp.interp)) %>%
  mutate(x=as.Date(x,origin="1970-01-01")) %>%
  rename(Date=x,
         Depth=y,
         Mod_Temp=z)


## read in obesrved temperature data, and clean up data,
## including subsetting for only high-frequency temperature readings
## following consistent deployment after 2017-Aug-11

setwd(paste0(path,"/Observations"))
obs.temp=read.csv("GILEStemp_all.csv",header=T) %>%
  rename(Date=V1,
         Time=V2,
         Depth=V3,
         Obs_Temp=V4,
         Instrument=V5) %>%
  select(Date,Depth,Obs_Temp,Instrument) %>%
  mutate(Date=ymd(Date),
         Depth=Depth/-100) %>%
  filter(Date>=ymd("2017-08-11"),
         Instrument=="miniDOT")


## join modelled vs. observed water temperature into same data frame
## for simple diagnostics, comparisons, and plotting

all.temp=mod.temp.clean %>%
  full_join(obs.temp) %>%
  arrange(Date,Depth)


## diagnostics of RMSE, R2, and bias across all mod vs. observed temperature data
## (need to remove "NA" values, or calculations will not work)

## NOTE:  model diagnostics (overall or grouped by depth, below) are calculated
## when there are both model and observed temperature data at the same date and depth

diag.overall=all.temp %>%
  filter(!is.na(Mod_Temp),
         !is.na(Obs_Temp)) %>%
  summarize(RMSE=rmse(actual=Obs_Temp,predicted=Mod_Temp),
            R2=summary(lm(Mod_Temp~Obs_Temp))$r.square,
            Bias=bias(actual=Obs_Temp,predicted=Mod_Temp),
            ObsN=NROW(.))
diag.overall


## similar model diagnostics, but grouped here by depth
## (too see which depths perform well or not)

diag.bydepth=all.temp %>%
  filter(!is.na(Mod_Temp),
         !is.na(Obs_Temp),
         !Depth==0.5) %>%
  group_by(Depth) %>%
  summarize(RMSE=rmse(actual=Obs_Temp,predicted=Mod_Temp),
            R2=summary(lm(Mod_Temp~Obs_Temp))$r.square,
            Bias=bias(actual=Obs_Temp,predicted=Mod_Temp),
            ObsN=NROW(Mod_Temp))
diag.bydepth


## create three plots displaying the model diagnostic values over a depths

rmse.depth=ggplot() +
  geom_point(data=diag.bydepth,aes(x=Depth,y=RMSE),size=3) +
  geom_line(data=diag.bydepth,aes(x=Depth,y=RMSE)) +
  coord_flip() +
  scale_x_reverse() +
  labs(x="Depth  (m)",y="RMSE  (°C)",title="RMSE by Depth") +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),axis.title=element_text(size=15,color="black"),
        legend.text=element_text(size=15,color="black"),legend.title=element_text(size=15,color="black"),
        panel.grid=element_blank())

r2.depth=ggplot() +
  geom_point(data=diag.bydepth,aes(x=Depth,y=R2),size=3) +
  geom_line(data=diag.bydepth,aes(x=Depth,y=R2)) +
  coord_flip() +
  scale_x_reverse() +
  labs(x="Depth  (m)",y="R2",title="R2 by Depth") +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),axis.title=element_text(size=15,color="black"),
        legend.text=element_text(size=15,color="black"),legend.title=element_text(size=15,color="black"),
        panel.grid=element_blank())

bias.depth=ggplot() +
  geom_hline(yintercept=0,lty=2) +
  geom_point(data=diag.bydepth,aes(x=Depth,y=Bias),size=3) +
  geom_line(data=diag.bydepth,aes(x=Depth,y=Bias)) +
  coord_flip() +
  scale_x_reverse() +
  labs(x="Depth  (m)",y="Bias  (°C)",title="Bias by Depth") +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),axis.title=element_text(size=15,color="black"),
        legend.text=element_text(size=15,color="black"),legend.title=element_text(size=15,color="black"),
        panel.grid=element_blank())

ggarrange(rmse.depth,bias.depth,r2.depth,nrow=1,ncol=3)


## visualize modelled vs. observed data of selected depths to compare timing of model performance

select.depths=c(0.5, 4, 6, 12, 16, 22)

compare.timing=all.temp %>%
  filter(Depth %in% select.depths) %>%
  mutate(DepthPrint=paste0(Depth," m"),
         DepthPrint=factor(DepthPrint,levels=paste(sort(select.depths),"m"))) %>%
  arrange(Date,Depth)

ggplot() +
  geom_line(data=compare.timing,aes(x=Date,y=Mod_Temp,color="Modelled"),size=1,alpha=0.5) +
  geom_point(data=compare.timing,aes(x=Date,y=Obs_Temp,color="Observed"),alpha=0.5,size=0.75) +
  facet_grid(DepthPrint~.) +
  scale_x_date(date_breaks="2 month",date_labels="%b\n'%y",date_minor_breaks="1 month",expand=c(0,0)) +
  scale_color_manual(values=c("Modelled"="blue2","Observed"="grey25"),name=NULL) +
  labs(x=NULL,y="Temperature (°C)") +
  theme_bw() +
  theme(axis.title=element_text(size=13,color="black"),axis.text=element_text(size=13,color="black"),
        strip.text=element_text(size=13,color="black",hjust=0),legend.position="top",
        legend.background=element_blank(),legend.key=element_blank(),
        legend.title=element_text(size=13,color="black"),legend.text=element_text(size=13,color="black"))


## visualize and compare full vertical profiles of modelled vs. observed temperature from select sampling dates

select.dates=c(ymd("2017-08-11"),ymd("2017-09-01"),ymd("2017-10-01"),ymd("2017-11-01"),ymd("2017-12-01"),
               ymd("2018-01-01"),ymd("2018-02-01"),ymd("2018-03-01"),ymd("2018-04-01"),ymd("2018-05-01"),ymd("2018-05-23"))

temp.profiles=all.temp %>%
  filter(Date %in% select.dates) %>%
  mutate(DatePrint=factor(format(Date,"%Y-%b-%d"),
                          levels=format(sort(unique(Date)),"%Y-%b-%d"))) %>%
  arrange(Date,Depth)

ggplot() +
  geom_point(data=temp.profiles,aes(x=Depth,y=Obs_Temp,color="Observed"),size=2,alpha=0.5) +
  geom_line(data=temp.profiles,aes(x=Depth,y=Mod_Temp,color="Modelled"),size=0.75) +
  coord_flip() +
  scale_x_reverse() +
  facet_wrap(~DatePrint,nrow=2) +
  scale_color_manual(values=c("Modelled"="blue2","Observed"="grey25"),name=NULL) +
  labs(x="Depth  (m)",y="Temperature  (°C)") +
  theme_bw() +
  theme(axis.title=element_text(size=13,color="black"),axis.text=element_text(size=13,color="black"),
        strip.text=element_text(size=13,color="black",hjust=0),legend.position="top",
        legend.background=element_blank(),legend.key=element_blank(),
        legend.title=element_text(size=13,color="black"),legend.text=element_text(size=13,color="black"))


## evaluate model performance by calculating temperature difference and thermocline depth,
## and comparing with observed data for these evaluations

temp.diffs=all.temp %>%
  group_by(Date) %>%
  summarize(Mod_TempDiff=Mod_Temp[Depth==0.5]-Mod_Temp[Depth==18],
            Obs_TempDiff=Obs_Temp[Depth==0.5]-Obs_Temp[Depth==18])

temp.diffs.summary=temp.diffs %>%
  filter(!is.na(Mod_TempDiff),
         !is.na(Obs_TempDiff)) %>%
  summarize(RMSE_TempDiff=rmse(actual=Obs_TempDiff,predicted=Mod_TempDiff),
            R2_TempDiff=summary(lm(Mod_TempDiff~Obs_TempDiff))$r.square,
            Bias_TempDiff=bias(actual=Obs_TempDiff,predicted=Mod_TempDiff),
            ObsN=NROW(Obs_TempDiff))

mod.thermo.depths=all.temp %>%
  group_by(Date) %>%
  filter(!is.na(Mod_Temp)) %>%
  summarize(Mod_ThermoDepth=thermo.depth(wtr=Mod_Temp,depths=Depth))

obs.thermo.depths=all.temp %>%
  group_by(Date) %>%
  filter(!is.na(Obs_Temp)) %>%
  summarize(Obs_ThermoDepth=thermo.depth(wtr=Obs_Temp,depths=Depth))

thermo.depths=mod.thermo.depths %>%
  full_join(obs.thermo.depths)

thermo.depths.summary=thermo.depths %>%
  filter(!is.na(Mod_ThermoDepth),
         !is.na(Obs_ThermoDepth)) %>%
  summarize(RMSE_ThermoDepth=rmse(actual=Obs_ThermoDepth,predicted=Mod_ThermoDepth),
            R2_ThermoDepth=summary(lm(Mod_ThermoDepth~Obs_ThermoDepth))$r.square,
            Bias_ThermoDepth=bias(actual=Obs_ThermoDepth,predicted=Mod_ThermoDepth),
            ObsN=NROW(Obs_ThermoDepth))


## create 3 plots for model evaluation, which include R2, RMSE, and bias values in legend:
## 1) all temperature readings, mod vs. obs
## 2) temperature difference calculations, mod vs. obs
## 3) thermocline depth calculations, mod vs. obs

all.temp.plot=ggplot() +
  geom_abline(intercept=0,slope=1,lty=2,size=0.65) +
  geom_point(data=all.temp,aes(x=Obs_Temp,y=Mod_Temp),alpha=0.25,size=2) +
  geom_smooth(data=all.temp,aes(x=Obs_Temp,y=Mod_Temp),method="lm",se=F,size=1.25,color="red2") +
  annotate(geom="text",x=2,y=c(29,27,25),hjust=0,vjust=1,size=4.25,
           label=c(paste0("R2 = ",format(diag.overall$R2,digits=3,nsmall=2)),
                   paste0("RMSE = ",format(diag.overall$RMSE,digits=3,nsmall=2),"°C"),
                   paste0("Bias = ",format(diag.overall$Bias,digits=3,nsmall=2),"°C"))) +
  labs(x="Observed Temperature  (°C)",y="Modelled Temperature  (°C)",
       title="Water Temperature") +
  theme_bw() +
  theme(axis.title=element_text(size=13,color="black"),axis.text=element_text(size=13,color="black"),
        plot.title=element_text(size=13,color="black"))

temp.diff.plot=ggplot() +
  geom_abline(intercept=0,slope=1,lty=2,size=0.65) +
  geom_point(data=temp.diffs,aes(x=Obs_TempDiff,y=Mod_TempDiff),alpha=0.25,size=2) +
  geom_smooth(data=temp.diffs,aes(x=Obs_TempDiff,y=Mod_TempDiff),method="lm",se=F,size=1.25,color="red2") +
  annotate(geom="text",x=-0.5,y=c(21.5,19.75,18),hjust=0,vjust=1,size=4.25,
           label=c(paste0("R2 = ",format(temp.diffs.summary$R2_TempDiff,digits=3,nsmall=2)),
                   paste0("RMSE = ",format(temp.diffs.summary$RMSE_TempDiff,digits=3,nsmall=2),"°C"),
                   paste0("Bias = ",format(temp.diffs.summary$Bias_TempDiff,digits=3,nsmall=2),"°C"))) +
  labs(x="Observed Temperature Difference  (°C)",y="Modelled Temperature Difference  (°C)",
       title="Temperature Difference") +
  theme_bw() +
  theme(axis.title=element_text(size=13,color="black"),axis.text=element_text(size=13,color="black"),
        plot.title=element_text(size=13,color="black"))

thermo.depth.plot=ggplot() +
  geom_abline(intercept=0,slope=1,lty=2,size=0.65) +
  geom_point(data=thermo.depths,aes(x=Obs_ThermoDepth,y=Mod_ThermoDepth),alpha=0.25,size=2) +
  geom_smooth(data=thermo.depths,aes(x=Obs_ThermoDepth,y=Mod_ThermoDepth),method="lm",se=F,size=1.25,color="red2") +
  annotate(geom="text",x=1,y=c(21,19.5,18),hjust=0,vjust=1,size=4.25,
           label=c(paste0("R2 = ",format(thermo.depths.summary$R2_ThermoDepth,digits=3,nsmall=2)),
                   paste0("RMSE = ",format(thermo.depths.summary$RMSE_ThermoDepth,digits=3,nsmall=2)," m"),
                   paste0("Bias = ",format(thermo.depths.summary$Bias_ThermoDepth,digits=3,nsmall=2)," m"))) +
  labs(x="Observed Thermocline Depth  (m)",y="Modelled Thermocline Depth  (m)",
       title="Thermocline Depth") +
  theme_bw() +
  theme(axis.title=element_text(size=13,color="black"),axis.text=element_text(size=13,color="black"),
        plot.title=element_text(size=13,color="black"))

ggarrange(all.temp.plot,temp.diff.plot,thermo.depth.plot,nrow=1,ncol=3)


#################################################################################################
#################################################################################################
#################################################################################################


## ICE COVER COMPARISONS, DIAGNOSTICS, & EVALUATIONS here


## set file directory and load in/clean up model output of ice data
setwd(path)

mod.his=as.data.frame(t(read.csv(ice.file,header=F))) %>%
  mutate(Mod_Date=seq.Date(as.Date("2018-12-31")-nrow(.)+1,as.Date("2018-12-31"),by=1)) %>%
  rename(h.ice_m=V1,
         h.snow_m=V2,
         h.both_m=V3,
         temp.ice_degC=V4,
         temp.air_degC=V5,
         rho.snow_kgm3=V6,
         ice.cover=V7)
mod.ice=mod.his %>%
  mutate(Year=year(Mod_Date)) %>%
  group_by(Year) %>%
  mutate(IceYear=ifelse(month(Mod_Date)<9,paste0(Year-1,"-",Year),paste0(Year,"-",Year+1))) %>%
  ungroup() %>%
  mutate(diffs=c(NA,diff(ice.cover))) %>%
  filter(diffs==1 | diffs==-1) %>%
  mutate(IceCover=ifelse(diffs==1,"Ice-on","Ice-off")) %>%
  select(IceYear,IceCover,Mod_Date)


## load observed ice cover data:

setwd(paste0(path,"/Observations"))

obs.ice=read.table("ice1991-2018.txt") %>%
  rename(IceCover=1,
         Obs_Date=V2) %>%
  mutate(Obs_Date=dmy(Obs_Date),
         Year=year(Obs_Date)) %>%
  group_by(Year) %>%
  mutate(IceYear=ifelse(month(Obs_Date)<9,paste0(Year-1,"-",Year),paste0(Year,"-",Year+1))) %>%
  ungroup() %>%
  select(IceYear,IceCover,Obs_Date)


## join both data frames for easy comparison:

all.ice=mod.ice %>%
  full_join(obs.ice) %>%
  gather(key="Source",value="Date",Mod_Date,Obs_Date) %>%
  arrange(IceYear)


## find two measures of modelled ice cover to compare with observations:
## 1) longest continuous ice cover period (elimiates "flickering" of ice)
## 2) earliest date of ice on and latest date of ice off per year (includes "flickering")

obs.ice.spread=obs.ice %>%
  spread(key=IceCover,value=Obs_Date) %>%
  rename(IceOn_Obs=`Ice-on`,
         IceOff_Obs=`Ice-off`)

longest.ice.dates=mod.ice %>%
  filter((IceCover=="Ice-on" & month(Mod_Date)>=7) |
           (IceCover=="Ice-off" & month(Mod_Date)<=6)) %>%
  group_by(IceYear) %>%
  summarize(IceOn_ModEarliest=min(Mod_Date[IceCover=="Ice-on"]),
            IceOff_ModLatest=max(Mod_Date[IceCover=="Ice-off"])) %>%
  full_join(obs.ice.spread) %>%
  na.omit()

longest.ice.diag=longest.ice.dates %>%
  summarize(Method="earliest ice on & latest ice off dates",
            RMSE_IceOn=rmse(actual=yday(IceOn_Obs),predicted=yday(IceOn_ModEarliest)),
            Bias_IceOn=bias(actual=yday(IceOn_Obs),predicted=yday(IceOn_ModEarliest)),
            ObsN_IceOn=NROW(IceOn_Obs),
            RMSE_IceOff=rmse(actual=yday(IceOff_Obs),predicted=yday(IceOff_ModLatest)),
            Bias_IceOff=bias(actual=yday(IceOff_Obs),predicted=yday(IceOff_ModLatest)),
            ObsN_IceOff=NROW(IceOff_Obs))

cont.ice.dates=mod.ice %>%
  filter((IceCover=="Ice-on" & month(Mod_Date)>=7) |
           (IceCover=="Ice-off" & month(Mod_Date)<=6)) %>%
  group_by(IceYear) %>%
  mutate(CoverDur=c(NA,diff(Mod_Date))) %>%
  filter(IceCover=="Ice-off") %>%
  summarize(IceOff_ModLongest=Mod_Date[which.max(CoverDur)],
            IceCoverDur=max(CoverDur,na.rm=T)) %>%
  mutate(IceOn_ModLongest=IceOff_ModLongest-IceCoverDur) %>%
  select(IceYear,IceOn_ModLongest,IceOff_ModLongest) %>%
  full_join(obs.ice.spread) %>%
  na.omit()

cont.ice.diag=cont.ice.dates %>%
  summarize(Method="longest continuous ice cover",
            RMSE_IceOn=rmse(actual=yday(IceOn_Obs),predicted=yday(IceOn_ModLongest)),
            Bias_IceOn=bias(actual=yday(IceOn_Obs),predicted=yday(IceOn_ModLongest)),
            ObsN_IceOn=NROW(IceOn_Obs),
            RMSE_IceOff=rmse(actual=yday(IceOff_Obs),predicted=yday(IceOff_ModLongest)),
            Bias_IceOff=bias(actual=yday(IceOff_Obs),predicted=yday(IceOff_ModLongest)),
            ObsN_IceOff=NROW(IceOff_Obs))

all.ice.diag=full_join(longest.ice.diag,cont.ice.diag)
all.ice.diag  


## plot of modelled vs. obs ice cover, including snow/ice thickness trends:

mod.thickness=mod.his %>%
  select(Mod_Date,h.ice_m,h.snow_m) %>%
  gather(key="Variable",value="Thickness_m",h.ice_m,h.snow_m) %>%
  mutate(Variable=factor(Variable,levels=c("h.snow_m","h.ice_m")))

ggplot() +
  geom_hline(yintercept=0) +
  geom_area(data=mod.thickness,aes(x=Mod_Date,y=Thickness_m,fill=Variable),alpha=0.25) +
  geom_vline(data=all.ice,aes(xintercept=Date,color=IceCover,linetype=Source,size=Source,alpha=Source)) +
  geom_text(data=all.ice.diag[all.ice.diag$Method=="longest continuous ice cover",],hjust=0,size=4.75,
            aes(x=min(mod.thickness$Mod_Date),y=0.425,label=paste0("Ice On RMSE = ",format(RMSE_IceOn,digits=1,nsmall=1)," d"))) +
  geom_text(data=all.ice.diag[all.ice.diag$Method=="longest continuous ice cover",],hjust=0,size=4.75,
            aes(x=min(mod.thickness$Mod_Date),y=0.4,label=paste0("Ice Off RMSE = ",format(RMSE_IceOff,digits=1,nsmall=1)," d"))) +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_date(limits=c(ymd("2016-05-17"),ymd("2018-05-31")),minor_breaks="1 month",date_labels="%b\n'%y",date_breaks="1 months") +
  scale_fill_manual(values=c("grey50","blue2"),labels=c("Snow Thickness","Ice Thickness"),guide=F) +
  scale_color_manual(values=c("red2","black")) +
  scale_linetype_manual(values=c(2,1)) +
  scale_size_manual(values=c(0.75,1.5)) +
  scale_alpha_manual(values=c(1,0.3),guide=F) +
  labs(x="Date",y="Thickness  (m)") +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),axis.title=element_text(size=15,color="black"),
        legend.text=element_text(size=15,color="black"),legend.title=element_text(size=15,color="black"),
        legend.position="right")





