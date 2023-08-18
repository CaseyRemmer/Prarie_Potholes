##prarie pothold wetlands

setwd("C:/Users/Casey/Dropbox/Prarie Potholes")
data<-read.csv("Isotopic Framework 2015_CR_for R.csv")
data<-read.csv("Isotopic Framework 2014_CR_for R.csv")

data$Date <- as.Date(data$Date, format = "%d-%b-%y") ##2015
data$Date <- as.Date(data$Date, format = "%Y-%m-%d") #2014


#framework calculations
## temp (ie K) and rh have to be inputed

met1<-read.csv("C:/Users/Casey/Dropbox/Prarie Potholes/met data/ACISDailyData-BRhead_BigV_BretP_Camr_Delb_Ferin_Fleet.csv")
met2<-read.csv("C:/Users/Casey/Dropbox/Prarie Potholes/met data/ACISDailyData-Brooks_cabin lake_Craigmyle_Drum_finni_handhills_Lathom.csv")
met3<-read.csv("C:/Users/Casey/Dropbox/Prarie Potholes/met data/ACISDailyData-Frstbrg_Kess_Lac_Nwsar_Prntss_Rslnd_stttlr.csv")
met4<-read.csv("C:/Users/Casey/Dropbox/Prarie Potholes/met data/ACISDailyData-Pllck_RllHills_Sheer_Spon_Tidlk_Yngtwn_Allia.csv")
met5<-read.csv("C:/Users/Casey/Dropbox/Prarie Potholes/met data/ACISDailyData-Thrsby_Wtskiwn.csv")

met_data<-rbind(met1, met2, met3,met4,met5)
met_data$Date<-as.Date(met_data$Date, format = "%d-%b-%y")

#assign met stations to data

data['met_station']<-c("NA")
data$met_station <- ifelse(data$Site_ID %in% c(30, 321), "Alliance", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c("BATL"), "Battle River Headwaters", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c("RUM_4","Rum4", 301, 395, 344, "Kerbe","Kerbe-02"), "Big Valley", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(396), "Breton Plots", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(202, "KIN_1"), "Brooks", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(101, 336, 388), "Cabin Lake", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(56, "Ozmen","Ozmen-05", "Pearl","Pearl-06", "Camrose City Airport"), "Camrose", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(379), "Craigmyle", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(194, "TOL_3", "Colli", "Kinvi-03", "Kinvi-06","Colli-02"), "Delburne", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(184), "Drumheller East", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(187, 377, "Holt","Holt-04", "Hwy53","Hwy53-02"), "Ferintosh", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(345), "Finnegan", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(25), "Fleet", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(18), "Forestburg", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(124, 131, 133, 135, 142, 145, 149, 308, 312, 346, 366), "Hand Hills", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(368), "Kessler", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(32, "JJCOL"), "Lacombe", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(158, 203), "Lathom", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c("MIQ_2", 317, 333, 351, "Bergq","Bergq-07", "Busen", "Busen-01","Forbs","Forbs-10", "Labyr-02", "Labyr-56", "Retta","Retta-09"),
                           "New Sarepta", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(109, 117, 338, 375, 384), "Pollockville", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c("GAD_1", "Parlb","Parlb-01"), "Prentiss", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(165, 173), "Rolling Hills", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(182), "Rosalind", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(110), "Sheerness", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(98, 186, 360), "Spondin", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(67, 89, 90, 195, 10, 13, 200, "Baron","Baron-01","Beltz", "Beltz-03","Caine","Caine-01", "Gilbe","Gilbe-02",
 "Grand","Grand-06", "Green", "Green-03","Heber","Heber-03", "Hille","Hille-03","Mika","Mika-10"), "Stettler", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(398), "Thorsby", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(152, 153), "Tide Lake", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(31, 35, 190, 365), "Wetaskiwin", data$met_station)
data$met_station <- ifelse(data$Site_ID %in% c(115, 188), "Youngstown", data$met_station)


library(lubridate)
# add column for rh, add column for temp
data['rh']<-c("NA")
data['temp']<-c("NA")


##tsibble will mask inerval!!
for (i in 1:nrow(data)){
  past <- data$Date[i] - days(30) #define the 30 day interval for each site ## adjust to try different date widows
  subset_data <- subset(met_data, Date %within% interval(past, data$Date[i]) #pull out the met_data for that interval at the met station of the site
                        & grepl(data$met_station[i], met_data$Station.Name)) 
      subset_data <- na.omit(subset_data)
         data$rh[i]<- mean(subset_data$rh)/100 #assign the average rh of the above data to the site
         data$temp[i]<- mean(subset_data$Temp)+273.15
}
head(data)
data[66:67,]
data$rh[66:67]<-data$rh[68]
data$temp[66:67]<-data$temp[68]


## next step is to calculate the E/I's using the new rh values
EI<-matrix(ncol=11, nrow=nrow(data))
colnames(EI)<-c("EI", "starO", "starH", "sslO", "sslH", "EO", "EH", "slope", "input", "IO", "IH")

for (i in 1:nrow(data)){
  K<-as.numeric(data$temp[i])
  rh<-as.numeric(data$rh[i])
  # α*
  a18O<-exp((-7.685+6.7123*((10^3)/K)-1.664*((10^6)/(K^2))+0.35041*((10^9)/(K^3)))/1000)
  
  a2H<- exp((1158.8*(K^3/10^9)-1620.1*(K^2/10^6)+794.84*(K/10^3)-161.04+2.9992*(10^9/K^3))/1000)
  
  ##ε*
  e18O<-a18O-1
  e2H<-a2H-1
  
  #εk
  ek18O<-0.0142*(1-rh)
  ek2H<-0.0125*(1-rh)
  
  #δAs , PAD version (SSL known)------------
  #dAs18O<-((dssl18O-e18O)/a18O-ek18O-dp18O*(1-rh+ek18O))/rh
  #dAs2H<-((dssl2H-ek2H)/a2H-ek2H-dp2H*(1-rh+ek2H))/rh
  
  ##alternate version dAS for Prarie pothole (sSSL calculated)
  #needs dPs
  dPs18O<-data$Ps.18O.[i]
  dPs2H<-data$Ps.2H.[i]
  dAs18O<-(dPs18O-e18O)/a18O
  dAs2H<-(dPs2H-e2H)/a2H
  #δp
  dp18O<-data$P.18O.[i]
  dp2H<-data$P.2H.[i]
  
  #δ*
  dstar18O<-((rh*dAs18O)+ek18O+(e18O/a18O))/(rh-ek18O-(e18O/a18O))
  dstar2H<-(rh*dAs2H+ek2H+e2H/a2H)/(rh-ek2H-e2H/a2H)
  EI[i,2]<-dstar18O
  EI[i,3]<-dstar2H

  
  #δSSL ##for Prarie potholes, in the PAD this comes from PAD18
  dssl18O<-a18O*dp18O*(1-rh+ek18O)+a18O*rh*dAs18O+a18O*ek18O+e18O
  dssl2H<-a2H*dp2H*(1-rh+ek2H)+a2H*rh*dAs2H+a2H*ek2H+e2H
  EI[i,4]<- dssl18O
  EI[i,5]<- dssl2H
  
  #δE
  dE18O<-((data$Opm[i]-e18O)/a18O-rh*dAs18O-ek18O)/(1-rh+ek18O)
  dE2H<-((data$Hpm[i]-e2H)/a2H-rh*dAs2H-ek2H)/(1-rh+ek2H)
  EI[i,6]<- dE18O
  EI[i,7]<-dE2H
  
  ##slope and intercept
  #slope<-(dstar2H-dp2H)/(dstar18O-dp18O) ##PAD
  slope<-(dE2H-data$Hpm[i])/(dE18O-data$Opm[i])
  int<-(data$Hpm[i]*1000)-(slope*(data$Opm[i]*1000))
  EI[i,8]<- slope
  EI[i,9]<- int
  
  #input water isotopic composition
  #dI18O<-((-19.2/1000)-(int))/(slope-6.7) ##PAD
  ##from Prarie potholes 
  dI18O<-(int-10)/(8-slope)
  dI2H<-(slope*dI18O)+int
  EI[i,10]<- dI18O
  EI[i,11]<- dI2H
  
  ##E/I
  ei<-((dI18O/1000)-data$Opm[i])/(dE18O-data$Opm[i])
  
  EI[i,1]<-ei
  
}
head(EI)
tail(EI)

write.csv()

##2014------------

data_2014<-cbind(data,EI)
write.csv(data_2014, file="2014 result with new rh.csv")
## okay so bringing in the original values
#EI_comp_2014<-read.csv("C:/Users/Casey/Dropbox/Prarie Potholes/EI_values_compare_2014.csv")
#colnames(EI)<-c("EI_30days")
#colnames(EI)<-c("EI_14days")
#colnames(EI)<-c("EI_7days")
EI_comp_2014<-cbind(EI_comp_2014, EI)
write.csv(EI_comp_2014, file="EIs_for_compare_0805423.csv")
head(EI_comp_2014)


#now i am going to see how many sites are still not working for each time
nrow(subset(EI_comp_2014,EI_orig<0 | EI_orig>1.5 ))
nrow(subset(EI_comp_2014,EI_30days<0 | EI_30days>1.5 ))
nrow(subset(EI_comp_2014,EI_14days<0 | EI_14days>1.5 ))
nrow(subset(EI_comp_2014,EI_7days<0 | EI_7days>1.5 ))

data14_cor<-cbind(data_2014[,1:7], data_2014$EI)
data14_cor<-subset(data14_cor,data_2014$EI>0 & data_2014$EI<1.5 )
names(data14_cor)[names(data14_cor) == 'data_2014$EI'] <- 'EI'
head(data14_cor)

TEST<-data14_cor %>% select(Site_ID,Site_Visit, O) %>% pivot_wider(names_from = Site_ID, values_from= O)
head(TEST)
na.counts<-colSums(is.na(TEST))

TEST2 <- TEST[, na.counts < 3]
TEST2<-column_to_rownames(TEST2,var = "Site_Visit") 

test3<-as.numeric(unlist(TEST2[[1]]))

for (i in 2:length(TEST2)){
  test3<- cbind(test3,as.numeric(unlist(TEST2[[i]])))
  
}
colnames(test3)<-colnames(TEST2)
test3
#library(synchrony)
meancorr(test3, nrands=999) ##this works (and it mirrors the values Nicole got), but doesnt have confidence intervals


##for 2015 data---------

nrow(subset(EI,EI[,1]<0 | EI[,1]>1.5 ))
data_2015<-cbind(data,EI)
write.csv( data_2015, file="2015_results_new_rh.csv")

trbl_2015<-subset(data_2015,EI<0 | EI>1.5 )
write.csv(trbl_2015, file="trbl_sites_2015.csv")

#synchrony---------------------
#δ18O, δ2H, and E/I
#2014 and 2015, Grassland and Parkland, and natural and restored Parkland sites

# Load the data

# Install and load the synchrony package
install.packages("synchrony")
library(synchrony)


##this is working
#install.packages("tidyverse")
#install.packages("tsibble")
library(tidyverse)
library(tsibble)

data_2015 <- data_2015[-c(181), ] ##date missing
data15_cor<-cbind(data_2015[,1:7], data_2015$EI)
data15_cor<-subset(data15_cor,data_2015$EI>0 & data_2015$EI<1.5 )
names(data15_cor)[names(data15_cor) == 'data_2015$EI'] <- 'EI'
head(data15_cor)
#data15_cor_ts<-data15_cor %>% as_tsibble(key=(c(Region,SbWtrshd, Site_ID)))

#data15_cortb <- data15_cor %>% as_tibble()

#data15_cortb %>% pivot_wider(names_from = Site_Visit, values_from= O)
#data15_cortb_O <- data15_cortb %>% pivot_wider(names_from = Date, values_from= O)
#write.csv(data15_cortb_O, file="test_O_pivot.csv")

#data15_cortb %>% group_by(Site_ID)

TEST<-data15_cor %>% select(Site_ID,Site_Visit, O) %>% pivot_wider(names_from = Site_ID, values_from= O)
na.counts<-colSums(is.na(TEST))

TEST2 <- TEST[, na.counts < 3]
TEST2<-column_to_rownames(TEST2,var = "Site_Visit") 

test3<-as.numeric(unlist(TEST2[[1]]))

for (i in 2:length(TEST2)){
test3<- cbind(test3,as.numeric(unlist(TEST2[[i]])))

}
colnames(test3)<-colnames(TEST2)
test3
library(synchrony)
meancorr(test3, nrands=999) ##this works (and it mirrors the values Nicole got), but doesnt have confidence intervals

## okay now bring in the 2014 data--------


####going to turn this into a little loopy loop
#δ18O, δ2H, and E/I
#2014 and 2015, Grassland and Parkland, and natural and restored Parkland sites
data15_cor<-subset(data15_cor,EI>0 & EI<1.5 )
view(data15_cor)
raw_dt<-list(data14_cor, data15_cor)
var<-c("O", "H", "EI")
results<-matrix(ncol=2, nrow=3)
rownames(results)<-c(var)
colnames(results)<-c("2014_cor", "2015_cor")
results2<-matrix(ncol=2, nrow=3)
rownames(results2)<-c(var)
colnames(results2)<-c("2014_p", "2015_p")
for (i in 1:2)
  {
  dtfrm<-raw_dt[[i]]
    for (n in 1:3)
      {
      TEST<-dtfrm %>% select(Site_ID,Site_Visit, var[n]) %>% pivot_wider(names_from = Site_ID, values_from= var[n])
      na.counts<-colSums(is.na(TEST))
      TEST2 <- TEST[, na.counts < 3]
      TEST2<-column_to_rownames(TEST2,var = "Site_Visit")
      test3<-as.numeric(unlist(TEST2[[1]]))
        for (p in 2:length(TEST2))
          {
         test3<- cbind(test3,as.numeric(unlist(TEST2[[p]])))
          }
      colnames(test3)<-colnames(TEST2)
      res<-meancorr(test3, nrands=999)
      results[n,i]<-res$obs
      results2[n,i]<-res$pval
       }
   }
results
results2

full_results<-cbind(results, results2)

##okay so now within each year, group by the type (grassland, parkland, restored) -----------------------
##where does this go int he loop? after n or before n?
#new loop?
data15_cor<-read.csv("2015_results_new_rh.csv")
data15_cor$EI <- replace(data15_cor$EI,data15_cor$EI >1.5, NA)
data15_cor$EI <- replace(data15_cor$EI,data15_cor$EI <0, NA)


raw_dt<-list(data14_cor, data15_cor)
reg_obs<-matrix(ncol=3, nrow=3)
#rownames(reg_obs)<-c(var)
colnames(reg_obs)<-c("Grass_cor", "Park_cor", "Rest_cor")
reg_ci<-matrix(ncol=6, nrow=3)
colnames(reg_ci)<-c("Grass_dwn_ci","Park_dwn_ci", "Rest_dwn_ci", "Grass_up_ci", "Park_up_ci", "Rest_up_ci")
reg_p<-matrix(ncol=3, nrow=3)
#rownames(reg_p)<-c(var)
#colnames(reg_p)<-c("Grass_p", "Park_p")
reg_year<-list()
ci_year<-list()
library(metan)


corr.fun <- function(data,i)
{
  df <- data[i,]
  res<-meancorr(df, nrands=0, use = "pairwise.complete.obs")
  res$obs
}

for (i in 1:2){
  
  dtfrm<-raw_dt[[i]]
  rgn<-c(unique(dtfrm$Region))
    for (y in 1:length(rgn)){
      dtfrm2<-dtfrm %>% subset(Region == rgn[y]) #dtfrm2<-data15_cor %>% subset(Region == rgn[1])
  for (n in 1:3)
  {
    TEST<-dtfrm2 %>%
    select(Site_ID,Site_Visit, var[n]) %>% pivot_wider(names_from = Site_ID, values_from= var[n])
    na.counts<-colSums(is.na(TEST))
    TEST2 <- TEST[, na.counts < 3]
    TEST2<-column_to_rownames(TEST2,var = "Site_Visit")
    print(ncol(TEST2))
      test3<-as.numeric(unlist(TEST2[[1]]))
      for (p in 2:length(TEST2))
      {
        test3<- cbind(test3,as.numeric(unlist(TEST2[[p]])))
      }
      colnames(test3)<-colnames(TEST2)
      
      res<-meancorr(test3, nrands=999, use = "pairwise.complete.obs")
      reg_obs[n,y]<-res$obs
      rownames(reg_obs)<-c(var)
      
      reg_p[n,y]<-res$pval
      rownames(reg_p)<-c(var)
      #reg_ci[n,y]<-quantile(res$rands, probs = 0.05)
      #reg_ci[n,y+3]<-), probs = 0.95)
      #corr_ci(r=res$obs, n=ncol(test3))
      bootstrap <- boot(test3, corr.fun, R = 1000)
      bci<-boot.ci(boot.out = bootstrap, conf=0.9,
                   type =  "perc")
      reg_ci[n,y]<-bci$percent[4]
      reg_ci[n,y+3]<-bci$percent[5]
    }
      reg_year[[i]]<-cbind(reg_obs, reg_p)
      ci_year[[i]]<-reg_ci
  }
}



bootstrap <- boot(test3, corr.fun, R = 1000)
bci<-boot.ci(boot.out = bootstrap, conf=0.9,
        type =  "perc")

reg_year
write.csv(reg_year[[1]], file="2014_synchrony_results.csv")
write.csv(reg_year[[2]], file="2015_synchrony_results.csv")
write.csv(ci_year[[1]], file="2014_ci_bootstraps.csv")
write.csv(ci_year[[2]], file="2015_ci_bootstraps.csv")



##recaluclating synchrony with d* used for the missing values and 1.5 for missing EI-----------
data14_cor<-read.csv("2014 result with new rh.csv")
data14_cor$EI <- replace(data14_cor$EI,data14_cor$EI >1.5, 1.5)
data14_cor$EI <- replace(data14_cor$EI,data14_cor$EI <0, 1.5)

view(data14_cor)
view(data14_cor$O)


raw_dt<-list(data14_cor, data15_cor)
star_obs<-matrix(ncol=3, nrow=3)
#rownames(reg_obs)<-c(var)
colnames(star_obs)<-c("Grass_cor", "Park_cor", "Rest_cor")
star_p<-matrix(ncol=3, nrow=3)
#rownames(star_p)<-c(var)
#colnames(star_p)<-c("Grass_p", "Park_p")
star_year<-list()
#library(metan)

for (i in 1:2){
  
  dtfrm<-raw_dt[[i]]
  rgn<-c(unique(dtfrm$Region))
  for (y in 1:length(rgn)){
    dtfrm2<-dtfrm %>% subset(Region == rgn[y]) #dtfrm2<-data15_cor %>% subset(Region == rgn[1])
    for (n in 1:3)
    {
      TEST<-dtfrm2 %>%
        select(Site_ID,Site_Visit, var[n]) %>% pivot_wider(names_from = Site_ID, values_from= var[n])
      na.counts<-colSums(is.na(TEST))
      TEST<-column_to_rownames(TEST,var = "Site_Visit")
##
      for(c in 1:length(TEST)){
       if(n ==1){
      star<-dtfrm2 %>%
        select(Site_ID,Site_Visit, starO) %>% pivot_wider(names_from = Site_ID, values_from=  starO)
      star<-column_to_rownames(star,var = "Site_Visit")
      TEST[c]<- replace(TEST[c], is.na(TEST[c]), max(star[c], na.rm = TRUE)*1000) 
      }else if (n==2){
        star<-dtfrm2 %>%
          select(Site_ID,Site_Visit, starH) %>% pivot_wider(names_from = Site_ID, values_from=  starH)
        star<-column_to_rownames(star,var = "Site_Visit")
        TEST[c]<- replace(TEST[c], is.na(TEST[c]), max(star[c], na.rm = TRUE)*1000) 
      }else if (n==3){
        TEST[c]<- replace(TEST[c], is.na(TEST[c]),  1.5)
      }
      }
##      
      test3<-as.numeric(unlist(TEST[[1]]))
      for (p in 2:length(TEST))
      {
        test3<- cbind(test3,as.numeric(unlist(TEST[[p]])))
      }
      colnames(test3)<-colnames(TEST)
      file_name <- paste0("df_", i, rgn[y],var[n], ".csv")
      
      # Save the dataframe as a CSV file
      write.csv(test3, file = file_name)
    }
      #print(test3)
      res<-meancorr(test3, nrands=999, use = "pairwise.complete.obs")
      star_obs[n,y]<-res$obs
      rownames(star_obs)<-c(var)
      
      star_p[n,y]<-res$pval
      rownames(star_p)<-c(var)
      corr_ci(r=res$obs, n=ncol(test3))
    }
    star_year[[i]]<-cbind(star_obs, reg_p)   
  }
}

##comparing 2014, 2015

raw_dt<-list(data14_cor, data15_cor)
star_obs<-matrix(ncol=2, nrow=3)
#rownames(reg_obs)<-c(var)
colnames(star_obs)<-c("2014", "2015")
star_p<-matrix(ncol=2, nrow=3)
#rownames(star_p)<-c(var)
#colnames(star_p)<-c("Grass_p", "Park_p")
star_year<-list()
#library(metan)
var<-c("O", "H", "EI")

for (i in 1:2){
  
  dtfrm<-raw_dt[[i]]
  #rgn<-c(unique(dtfrm$Region))
    for (n in 1:3)
    {
      TEST<-dtfrm %>%
        select(Site_ID,Site_Visit,var[n]) %>% pivot_wider(names_from = Site_ID, values_from= var[n])
      na.counts<-colSums(is.na(TEST))
      TEST<-column_to_rownames(TEST,var = "Site_Visit")
      ##
      for(c in 1:length(TEST)){
        if(n ==1){
          star<-dtfrm %>%
            select(Site_ID,Site_Visit, starO) %>% pivot_wider(names_from = Site_ID, values_from=  starO)
          star<-column_to_rownames(star,var = "Site_Visit")
          TEST[c]<- replace(TEST[c], is.na(TEST[c]), max(star[c], na.rm = TRUE)*1000) 
        }else if (n==2){
          star<-dtfrm %>%
            select(Site_ID,Site_Visit, starH) %>% pivot_wider(names_from = Site_ID, values_from=  starH)
          star<-column_to_rownames(star,var = "Site_Visit")
          TEST[c]<- replace(TEST[c], is.na(TEST[c]), max(star[c], na.rm = TRUE)*1000) 
        }else if (n==3){
          TEST[c]<- replace(TEST[c], is.na(TEST[c]),  1.5)
        }
      }
      ##      
      test3<-as.numeric(unlist(TEST[[1]]))
      for (p in 2:length(TEST))
      {
        test3<- cbind(test3,as.numeric(unlist(TEST[[p]])))
      }
      colnames(test3)<-colnames(TEST)
      file_name <- paste0("df_", i,var[n], ".csv")
      
      # Save the dataframe as a CSV file
      write.csv(test3, file = file_name)
    
    #print(test3)
    res<-meancorr(test3, nrands=999, use = "pairwise.complete.obs")
    star_obs[n,i]<-res$obs
    rownames(star_obs)<-c(var)
    
    star_p[n,i]<-res$pval
    rownames(star_p)<-c(var)
    corr_ci(r=res$obs, n=ncol(test3))
}}



yrbyyr<-star_obs
write.csv(yrbyyr, file="outputs/data results/2014vs2015_synchrony_results.csv")

star_year
write.csv(star_year[[1]], file="2014_synchrony_results_star.csv")
write.csv(star_year[[2]], file="2015_synchrony_results_star.csv")

## man-whitney u tests-----------------------------------
#δ18O, δ2H, δI, E/I
## input comp?
data14_cor<-read.csv("2014 result with new rh.csv")
data14_cor$EI <- replace(data14_cor$EI,data14_cor$EI >1.5, 1.5)
data14_cor$EI <- replace(data14_cor$EI,data14_cor$EI <0, 1.5)
view(data14_cor$EI)
data15_cor<-read.csv("2015_results_new_rh.csv")
data15_cor$EI <- replace(data15_cor$EI,data15_cor$EI >1.5, 1.5)
data15_cor$EI <- replace(data15_cor$EI,data15_cor$EI <0, 1.5)
view(data15_cor$EI)


variables<-c("O", "H", "EI", "IO", "IH")
#rgn<-c(unique(data$Region)


manu_res<-data.frame(ncol=)

mwu_by.yr <- function(data1, data2) {
  variables<-c("O", "H", "EI", "IO", "IH")
  manu_res<-data.frame(matrix(ncol = 5, nrow = 5))
  manu_stat<-data.frame(matrix(ncol = 5, nrow = 5))
  colnames(manu_res) <- c(variables)
  for(i in 1:5) {
    for(y in 1:length(variables)){
      if (y<3){
    x1<-data1%>% filter(Site_Visit == i) %>% select (variables[y])%>%unlist()%>%as.numeric()
    x2<-data2%>%filter(Site_Visit == i) %>% select (variables[y])%>%unlist()%>%as.numeric()
    }else{
        x1<-data1%>% filter(Site_Visit == i & EI <1.5) %>% select (variables[y])%>%unlist()%>%as.numeric()
        x2<-data2%>%filter(Site_Visit == i & EI <1.5) %>% select (variables[y])%>%unlist()%>%as.numeric()
      }
    wc<-wilcox.test(x1,x2,exact=FALSE)
    ##save results
    manu_res[i,y]<-wc$p.value
    manu_stat[i,y]<-wc$statistic
    }
  }
  print(manu_res)
  print(manu_stat)
  write.csv(manu_res, file="2014vs2015_manWu.csv")
  write.csv(manu_stat, file="2014vs2015_manWu_stat.csv")
}	


mwu_by.yr(data14_cor, data15_cor)

#### many-whitney u tests by region & year

mwu_by.rgn <- function(data1, region1, region2) {
  variables<-c("O", "H", "EI","IO", "IH")
  manu_res<-data.frame(matrix(ncol = 5, nrow = 5))
  manu_stat<-data.frame(matrix(ncol = 5, nrow = 5))
  colnames(manu_res) <- c(variables)
  for(i in 1:5) {
    for(y in 1:length(variables)){
      if (y<3){
        x1<-data1%>% filter(Site_Visit == i & Region == region1) %>% select (variables[y])%>%unlist()%>%as.numeric()
        x2<-data1%>%filter(Site_Visit == i & Region == region2) %>% select (variables[y])%>%unlist()%>%as.numeric()
      }else{
        x1<-data1%>% filter(Site_Visit == i& Region == region1 & EI <1.5) %>% select (variables[y])%>%unlist()%>%as.numeric()
        x2<-data1%>%filter(Site_Visit == i & Region == region2 & EI <1.5) %>% select (variables[y])%>%unlist()%>%as.numeric()
      }
      if (length(x1) == 0 | length(x2) == 0) {
        # Handle the case when the subset is empty
        # Return or do something appropriate
        manu_res[i,y]<-NA
      } else {
      wc<-wilcox.test(x1,x2,exact=FALSE)
      ##save results
      manu_res[i,y]<-wc$p.value
      manu_stat[i,y]<-wc$statistic}
    }
  }
  print(manu_res)
  print(manu_stat)
  file_name <- paste0(substitute(data1), region1, "vs" , region2, "MWU", ".csv")
  write.csv(manu_res, file= file_name)
}	


mwu_by.rgn(data14_cor, "Grassland", "Parkland")
mwu_by.rgn(data15_cor, "Grassland", "Parkland")
mwu_by.rgn(data15_cor, "Parkland", "Restored")
-----------------------------------------------------------------------

### calulating the proportion of input water atributable to rain/snow--------------------------------------------

#% rain= (IO -x18O_snow) / (X18O_rain - X18O_snow)

O1 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O01c.asc") #Jan
O2 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O02c.asc") #Feb
O12 <- raster("C:/Users/Casey/Downloads/IsotopeMaps/IsotopeMaps/O12c.asc") #Dec
#crs(O1)<-"+proj=longlat +datum=WGS84"
coords<-read.csv("Prarie_potholes_lat.long.csv")
head(coords)
e<-c(xmin=min(coords$X_Long)-1, xmax=max(coords$X_Long)+1, ymin= min(coords$Y_Lat)-1, ymax=max(coords$Y_Lat)+1)

winter_O<-list(O1,O2,O12)

  for (i in 1:3){
  x<-winter_O[[i]]
  x<-crop(x,e)
  winter_O[[i]]<-x
  }

winter_O[[1]][[1]]

Ow<-data.frame(matrix(ncol=3, nrow=nrow(coords)))
Ow[,1]=coords$Site_ID
names(Ow)[1]<-"site id"
coords_sf<-st_as_sf(coords, coords = c("X_Long", "Y_Lat"), crs=4326)

for (i in 1:3) {
  Ow[,i+1]=raster::extract(winter_O[[i]], coords_sf)
}


mean_Ow<-data.frame(matrix(ncol=2, nrow=nrow(coords)))
mean_Ow[,1]=coords$Site_ID
names(mean_Ow)[1]<-"Site_ID"

for (s in 1:nrow(coords)){
  mean_Ow[s,2]<- (Ow[s,2]+Ow[s,3]+Ow[s,4])/3
}



mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "BatL", "BATL")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "JJColl", "JJCOL")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "MIQ2", "MIQ_2")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "Rum4", "RUM_4")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "Tol3", "TOL_3")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "KIN1", "KIN_1")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "Gad1", "GAD_1")

mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "BARON-01", "Baron-01")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "BELTZ-03", "Beltz-03")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "BERGQ-07", "Bergq-07")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "CAINE-01", "Caine-01")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "COLLI-02", "Colli-02")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "FORBS-10", "Forbs-10")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "GILBE-02", "Gilbe-02")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "HEBER-03", "Heber-03")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "HILLE-03", "Hille-03")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "HOLT-04","Holt-04")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "HWY53-02", "Hwy53-02")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "KERBE-02", "Kerbe-02")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "KINVI-03", "Kinvi-03")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "KINVI-06", "Kinvi-06")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "LABYR-02", "Labyr-02")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "MIKA-10", "Mika-10")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "OZMEN-05", "Ozmen-05")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "PARLB-01", "Parlb-01")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "PEARL-06", "Pearl-06")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "RETTA-09", "Retta-09")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "RUM_4", "Rum4")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "LABYR-56", "Labyr-56")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "BUSEN-01", "Busen-01")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "GREEN-03", "Green-03")
mean_Ow$Site_ID <- replace(mean_Ow$Site_ID,mean_Ow$Site_ID == "GRAND-06", "Grand-06")

data14_wOIPC<-merge(data14_cor, mean_Ow, by= "Site_ID",all.x=TRUE)
data15_wOIPC<-merge(data15_cor, mean_Ow, by= "Site_ID", all.x=TRUE)

#% rain= (IO -x18O_snow) / (X18O_rain - X18O_snow)
prop_rain14<-data.frame(matrix(ncol=3, nrow=nrow(data14_wOIPC)))
prop_rain14[,1]<-data14_wOIPC$Site_ID
prop_rain14[,2]<-data14_wOIPC$Site_Visit
prop_rain15<-data.frame(matrix(ncol=3, nrow=nrow(data15_wOIPC)))
prop_rain15[,1]<-data15_wOIPC$Site_ID
prop_rain15[,2]<-data15_wOIPC$Site_Visit

for (s in 1:nrow(data14_wOIPC)){
  prop_rain14[s,3]<- (data14_wOIPC$IO[s] -data14_wOIPC$X2[s]) / (data14_wOIPC$X18O_rain[s] -data14_wOIPC$X2[s])*100
}
colnames(prop_rain14)<-c("Site_ID","Site_Visit", "per.rain")

data14_wOIPC<-data14_wOIPC%>% merge ( prop_rain14, by =c( "Site_ID" ,"Site_Visit"))
view(data14_wOIPC)

for (s in 1:nrow(data15_wOIPC)){
  prop_rain15[s,3]<- (data15_wOIPC$IO[s] -data15_wOIPC$X2[s]) / ((data15_wOIPC$Ps.18O.[s]*1000) -data15_wOIPC$X2[s])*100
}
colnames(prop_rain15)<-c("Site_ID","Site_Visit", "per.rain")

data15_wOIPC<-data15_wOIPC%>% merge ( prop_rain15, by =c( "Site_ID" ,"Site_Visit"))
view(data15_wOIPC)


write.csv( data14_wOIPC, file="data2014_w_per.rain.csv")
write.csv( data15_wOIPC, file="data2015_w_per.rain.csv")
