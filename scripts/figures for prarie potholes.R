#### mapping the EI's and input waters of the 2014-2015 prairie potholes
setwd("C:/Users/Casey/Dropbox/Prarie Potholes")

##synchrony plots-------------------------------------
sync15<-read.csv("2015_synchrony_results_star.csv")
sync15<-sync15[,-c(5,6,7)]
names(sync15)<-c("X", "Grass", "Park", "Rest")

sync14<-read.csv("2014_synchrony_results_star.csv")
sync14<-sync14[,-c(4,5,6,7)]
names(sync14)<-c("X", "Grass", "Park")
yrbyyr<-as.data.frame(yrbyyr)
yrbyyr<-cbind(yrbyyr, row.names(yrbyyr))
names(yrbyyr)<-c("2014", "2015", "X")

cis<-read.csv("ci_s.csv")
cisB<-ci_year




sync15_plot<-
  ggplot(data=sync15 %>% pivot_longer(names_to= "Region", cols=c(Grass, Park, Rest)) %>% left_join(cis%>%filter(Year== 2015), join_by ( X==X, Region==Region)),
         aes(y=value, x=X, fill= Region, group=Region))+
  geom_col(width = 0.5, position = position_dodge(0.7),colour="#AA4499", lwd=0.75)+
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2,
                position=position_dodge(.7),colour="#AA4499", lwd=0.75)+ ##from the package
  geom_errorbar(aes(ymin=ci_year, ymax=value+ci), width=.2,
                position=position_dodge(.7),colour="#AA4499", lwd=0.75)+ ##from the boostrap method
  ylab("synchrony")+
  xlab("")+
  theme_minimal()+
  scale_fill_manual(values=c( "#DDCC77", "#44AA99","#6699CC"))+
  ylim(c(0,1.15))


##bootstrap plot 

cisB15<-as.data.frame(ci_year[[2]])
cisB15$X<-c("O","H","EI")
Region<-rep(c("Grass", "Park", "Rest"), times=3)
dir<-rep(c("d","d","d","p","p","p"), times=3)
cisB15<-cisB15%>%pivot_longer(names_to="ci", cols=c(1:6)) %>% cbind(Region, dir)

sync15_plotB<-
  ggplot(data=sync15 %>% pivot_longer(names_to= "Region", cols=c(Grass, Park, Rest)) %>% full_join(cisB15, join_by ( X==X, Region==Region)),
         aes(y=value, x=X, fill= Region, group=Region))+
  geom_col(width = 0.5, position = position_dodge(0.7),colour="#AA4499", lwd=0.75)+
  geom_errorbar(aes(ymin=ci_year, ymax=value+ci), width=.2,
                position=position_dodge(.7),colour="#AA4499", lwd=0.75)+ ##from the boostrap method
  ylab("synchrony")+
  xlab("")+
  theme_minimal()+
  scale_fill_manual(values=c( "#DDCC77", "#44AA99","#6699CC"))+
  ylim(c(0,1.15))

sync14_plot<-
  ggplot(sync14 %>% pivot_longer(names_to= "Region", cols=c(Grass, Park)) %>% left_join(cis%>%filter(Year== 2014), join_by ( X==X, Region==Region)),
         aes(y=value, x=X, fill= Region, group=Region))+
  geom_col(width = 0.5, position = position_dodge(0.7), colour="#332288", lwd=0.75)+
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2,
                position=position_dodge(.7),colour="#332288", lwd=0.75)+
  ylab("synchrony")+
  xlab("")+
  theme_minimal()+
  scale_fill_manual(values=c( "#DDCC77", "#44AA99","#6699CC"))+
  ylim(c(0,1.15))

yr_plot<-
  ggplot(yrbyyr %>% pivot_longer(names_to= "Year", cols=c("2014", "2015")) %>% left_join(cis%>% filter(is.na(Region)), join_by ( X==X, Year==Year)),
         aes(y=value, x=X, fill= Year, group=Year))+
  geom_col(width = 0.5, position = position_dodge(0.7), lwd=0.75)+
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci), width=.2,
                position=position_dodge(.7), lwd=0.75)+
  ylab("synchrony")+
  xlab("")+
  theme_minimal()+
  scale_fill_manual(values=c( "#332288", "#AA4499"))+
  ylim(c(0,1.15))

leg1<-get_legend(yr_plot, position="bottom")
leg2<-get_legend(sync15_plot,position="bottom")

sync_full<- ggarrange(ggarrange(yr_plot, 
                      sync14_plot+ theme(axis.title.y=element_blank()), 
                      sync15_plot+ theme(axis.title.y=element_blank()), 
                      legend=FALSE, nrow=1, labels=c("a)", "b) 2014", "c) 2015"),
                      font.label = list(size = 11),
                      hjust=c(-4, -0.75, -0.75)),
                      ggarrange(leg1,leg2, nrow=1),
                      nrow=2, heights=c(0.9,0.1))
                     
ggsave(sync_full, file= "sync_full.png", width=230, height=190, units = "mm")
ggsave(sync_full, file= "sync_full.pdf", width=230, height=190, units = "mm")

-----------------------------------------------
library(cowplot)
library(ggrepel)
library(ggspatial)
library(rgeos)
library(ggmap)
library(rgdal)

library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2)

data14_cor<-read.csv("outputs/data results/2014 result with new rh.csv")
data14_cor$EI <- replace(data14_cor$EI,data14_cor$EI >1.5, 1.5)
data14_cor$EI <- replace(data14_cor$EI,data14_cor$EI <0, 1.5)
view(data14_cor$EI)
data15_cor<-read.csv("outputs/data results/2015_results_new_rh.csv")
data15_cor$EI <- replace(data15_cor$EI,data15_cor$EI >1.5, 1.5)
data15_cor$EI <- replace(data15_cor$EI,data15_cor$EI <0, 1.5)
view(data15_cor)

##starting with E/I because we might calc. % of input
coords<-read.csv("Prarie_potholes_lat.long.csv")
head(coords)
head(data14_cor)
coords$Site_ID <- mean_Ow$Site_ID


rain_rplce<-
  function(x){
    for (i in 1:nrow(x)){
    if (x$per.rain[i] < 0){
    x$per.rain[i] <- 0}
    else if(x$per.rain[i] >100){
    x$per.rain[i] <- 100
  }else { NA}
}
    return(x)
    } 



data14_rain<-read.csv("outputs/data results/data2014_w_per.rain.csv")
rain_rplce_NA<-
  function(x){
    for (i in 1:nrow(x)){
      if (x$per.rain[i] < 0){
        x$per.rain[i] <- NA}
      else if(x$per.rain[i] >100){
        x$per.rain[i] <- NA
      }else if (is.na(x$per.rain[i])){
        x$per.rain[i] <- NA}
    }
    return(x)
  } 

rain_rplce_NA<-
  function(x){
    for (i in 1:nrow(x)){
      if (x$per.rain[i] < 0 |x$per.rain[i] >100 |is.na(x$per.rain[i])){
        x$per.rain[i] <- NA}
    }
    return(x)
  } 

data14_rain_NA<-rain_rplce_NA(data14_rain)
view(data14_rain_NA)
data15_rain<-read.csv("outputs/data results/data2015_w_per.rain.csv")
data15_rain_NA<-rain_rplce_NA(data15_rain)

######--Input maps-------
data15_wOIPC <- data15_wOIPC[-176,]
map15_2<-rain_rplce(data15_wOIPC)

map_data14<-merge(map14_2, coords, by= c("Region", "Site_ID"))
view(map_data14)
map_data15<-merge(map15_2, coords, by= c("Region", "Site_ID"))
view(map_data15)

write.csv(map_data14, file="map_data14.csv")
write.csv(map_data15, file="map_data15.csv")

map_data14_sf<-st_as_sf(map_data14, coords = c("X_Long","Y_Lat"), crs="EPSG:4326")
map_data15_sf<-st_as_sf(map_data15, coords = c("X_Long","Y_Lat"), crs="EPSG:4326")

view(map_data15_sf)

map_plot_14<-
  ggplot() + geom_sf(data=map_data14_sf, aes(colour=per.rain, shape=Region) ,size=4)+
  scale_colour_viridis(option="plasma")+
  xlab("Longitude") + ylab("Latitude")+
  theme_minimal()

ggplot()+
  geom_sf(data=nrs, aes(geometry=geometry,fill=NRNAME))

map_plot_14<-map_plot_14 + facet_wrap(.~Site_Visit, nrow=1)

map_plot_15<-
  ggplot() + geom_sf(data=map_data15_sf2, aes(colour=per.rain, shape=Region) ,size=4)+
  scale_colour_viridis(option="plasma")+
  xlab("Longitude") + ylab("Latitude")+
  theme_minimal()
map_plot_15<-map_plot_15 + facet_wrap(.~Site_Visit, nrow=1)

input_maps<-ggarrange(map_plot_14,map_plot_15, labels = c("2014", "2015"), 
                   nrow=2, common.legend = TRUE)
input_maps

ggplot(data = map_data15_sf2) + geom_sf()+
  scale_x_continuous(breaks = c(-114, -112, -110)) +
  scale_y_continuous(breaks = 50:53)



install.packages("akima")
library(akima)

#map_data15_2 <- map_data15[-c(74, 147, 178),]

in_plts2<-list(1,2,3,4,5)

x2 <- x %>% subset (Site_Visit == i)

map_data15_2

  for (i in 1:5){
  x2 <- map_data15_2 %>% filter( Site_Visit == i)
  di <- interp(x2$X_Long, x2$Y_Lat, x2$per.rain,
               xo=seq(min(x2$X_Long), max(x2$X_Long), length=200),
               yo=seq(min(x2$Y_Lat), max(x2$Y_Lat), length=200))
  
  di_expnd <- data.frame(expand.grid(x=di$x, y=di$y), z=c(di$z))
  pp<- 
    ggplot(di_expnd, aes(x=x, y=y, z=z))+
    #geom_contour_fill()+
    geom_tile(aes(fill = z), alpha=0.5) +
    #stat_contour(bins = 5,colour="white")+
    xlab("Longitude") + ylab("Latitude")+
    guides(fill = guide_colorbar(title = "% rain"))+
    theme_minimal()+
    #metR::geom_contour2(aes(z = z, label = stat(level)),colour="white", breaks=seq(0, 100, 25))+
    scale_fill_gradientn(colours = viridis(10), na.value="white", limits=c(0,100))+
    geom_point(data=map_data15%>% filter(Site_Visit==i), aes(x=X_Long, y=Y_Lat, z=per.rain, fill= per.rain, shape= Region), na.rm=TRUE, size=3)+
    scale_colour_viridis()+
    scale_shape_manual(values = c(21:23))

    in_plts[[i]]<-pp##not saving to the list??
  }


library(stars) 
library(gstat)
x.range <- c(min(map_data15_2$X_Long), max(map_data15_2$X_Long))
y.range <- c(min(map_data15_2$Y_Lat), max(map_data15_2$Y_Lat))
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.001), 
                   y = seq(from = y.range[1], to = y.range[2], by = 0.001))
#convert to SpatialPixels 
grd1<-st_as_sf(x = grd, 
               coords = c("x", "y"),
               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84")

grd2<-raster(grd1)
star_grd<-st_as_stars(grd2, dx = 10000, dy = 10000) 

vis1_2015<- map_data15_sf %>% filter (Site_Visit == 1)

i2 <- idw(per.rain ~ 1, vis1_2015, star_grd)

ggplot() + geom_stars(data = i2, 
                      aes(fill = var1.pred)) + 
  scale_fill_viridis(na.value="white", limits=c(0,100))+
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = vis1_2015) ## this is working, need to increase the resolution and maybe crop it to a polygon 





g=gstat(formula=per.rain ~ 1, data= vis1_2015)
z = predict(g, star_grd)

input_percent_plots<-
  ggarrange(in_plts2[[1]]+theme(axis.text.x = element_blank(),
                               axis.ticks.x = element_blank(),
                               axis.title.x = element_blank()),
            in_plts2[[2]]+theme(axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.y = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank()),
            in_plts2[[3]]+theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.x = element_blank(),
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank()),
            in_plts2[[4]]+theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.x = element_blank(),
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank()),
            in_plts2[[5]]+theme(axis.text.y = element_blank(),
                               axis.ticks.y = element_blank(),
                               axis.title.y = element_blank(),
                               axis.ticks.x = element_blank(),
                               axis.text.x = element_blank(),
                               axis.title.x = element_blank()),
            in_plts[[1]]+ theme(axis.title.x = element_blank()),
            in_plts[[2]]+theme (axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()),
            in_plts[[3]]+theme (axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()),
            in_plts[[4]]+theme(axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()),
            in_plts[[5]]+theme(axis.text.y = element_blank(),
                                axis.ticks.y = element_blank(),
                                axis.title.y = element_blank(),
                                axis.title.x = element_blank()), common.legend = TRUE,
            nrow=2, ncol=5,align = "hv",labels=c("1","2","3","4","5", "1", "2", "3", "4", "5"), label.x = 0.2, label.y= 0.95)
          

ggsave(input_percent_plots, file= "input_percent_plots.pdf", width=230, height=190, units = "mm")
ggsave(input_percent_plots, file= "input_percent_plots.png", width=230, height=190, units = "mm")



### results as boxplots-------------------

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
safe_colorblind_palette2 <- c("#88CCEE", "#CC6677", "#DDCC77", "#AA4499", "#44AA99")



box_plot_14<-ggplot(data14_rain_NA, aes(y= (100-per.rain), x=as.factor(Site_Visit), 
                          group=as.factor(Site_Visit), fill=as.factor(Site_Visit), colour=as.factor(Site_Visit)))+
  geom_boxplot(alpha=0.25)+
  geom_jitter()+
  facet_wrap(~Region)+
  theme_classic()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())+
  ylab("% snow")+
  scale_y_continuous(name = "% Snow", sec.axis = sec_axis(~(.-100)*-1, name="% Rain"))+
  scale_colour_manual(values=safe_colorblind_palette2, name= "Site Visit")+
  scale_fill_manual(values=safe_colorblind_palette2,name= "Site Visit")

box_plot_15<-ggplot(data15_rain_NA, aes(y= (100-per.rain), x=as.factor(Site_Visit), 
                                        group=as.factor(Site_Visit), fill=as.factor(Site_Visit), colour=as.factor(Site_Visit)))+
  geom_boxplot(alpha=0.25)+
  geom_jitter()+
  facet_wrap(~Region)+
  theme_classic()+
  theme(axis.title.x = element_blank(),axis.text.x = element_blank())+
  ylab("% snow")+
  scale_y_continuous(name = "% Snow", sec.axis = sec_axis(~(.-100)*-1, name="% Rain"))+
  scale_colour_manual(values=safe_colorblind_palette2, name= "Site Visit")+
  scale_fill_manual(values=safe_colorblind_palette2,name= "Site Visit")

leg_box<-get_legend(box_plot_15)
                        
boxplot_bothyrs<-ggarrange(ggarrange(box_plot_14+theme (axis.title.x = element_blank()), 
                           as_ggplot(leg_box),legend="none", widths=c(2.6,1)),
                           box_plot_15,
                           labels = c("2014", "2015"), label.x =-0.025  , label.y =1 , nrow=2, heights=c(1,1),
                           common.legend = TRUE, legend="none")



ggsave(boxplot_bothyrs, file= "boxplot_bothyrs.pdf", width=230, height=190, units = "mm")
ggsave(boxplot_bothyrs, file= "boxplot_bothyrs.png", width=230, height=190, units = "mm")

--------------------------------------

### EI plots------------------------------------

install.packages("colorspace", repos = "http://R-Forge.R-project.org")
install.packages("colorblindr")
remotes::install_github("clauswilke/colorblindr")
library(cowplot)
library(colorspace)
library(colorblindr)
library(viridis)
remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

#wide<-data14_cor%>%select(Site_ID,Site_Visit, EI)%>% pivot_wider(names_from = Site_ID, values_from = EI)
#wide2<-wide%>% pivot_longer(!Site_Visit, names_to = "Site_ID", values_to = "EI", values_drop_na=FALSE)


pre_tile<-
function (data){
  df = data %>% complete(Site_ID,Site_Visit)
  for (i in 1:nrow(df)){
    if (is.na(df$Region[i])==TRUE){
      df$Region[i] <- df$Region[i-1]
    }
  } 
  return(df)
}


fil_15<-data15_cor %>% subset(Region == "Grassland" | Region =="Parkland")
r15<-data15_cor %>% subset(Region == "Restored")

df14<-pre_tile(data14_cor)
df15<-pre_tile(fil_15)
dfr15<-pre_tile(r15)


p14<-
  ggplot(df14,aes(x=Site_Visit, y=Site_ID, fill = EI))+
  geom_tile(color= "white",linewidth=0.1) + 
  scale_fill_viridis(option="plasma", na.value="#FEFFD5")+
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.margin = unit(c(0,0,0,0.3), 'lines'),
        axis.text.y = element_text(size = 8))+
  ylab ("")+xlab("")
  
                            

p14<-p14 + facet_wrap(.~Region, scales = "free_y")


p15<-ggplot(df15,aes(y=Site_ID, x=Site_Visit,fill=EI))+
  geom_tile(color= "white",linewidth=0.1) + 
  scale_fill_viridis(option="plasma",na.value= "#FEFFD5")+
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.margin = unit(c(0,0,0,0.3), 'lines'),
        axis.text.y = element_text(size = 8))+
  xlab("Site Visit")
p15<-p15 + facet_wrap(.~Region, scales = "free_y")


rp15<-ggplot(dfr15,aes(y=Site_ID, x=Site_Visit,fill=EI,color= EI))+
  geom_tile(color= "white",linewidth=0.1) + 
  scale_fill_viridis(option="plasma",na.value="#FEFFD5")+
  theme_minimal()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),plot.margin = unit(c(0,0.3,0,-0.2), 'lines'),
        axis.text.y = element_text(size = 8))+
  ylab("")+xlab("")
rp15<-rp15 + facet_wrap(.~Region, scales = "free")

leg_EI <- get_legend(p15)


EI_plot<-ggarrange(p14,as_ggplot(leg_EI), p15,rp15, widths = c(2,1), labels = c("2014","", "2015"), 
                   font.label = list(size = 12), nrow=2, ncol=2, common.legend = TRUE, legend= "none")
EI_plot
ggsave(EI_plot, file= "outputs/figures/EI_plot.pdf", width=230, height=190, units = "mm", bg="white")
ggsave(EI_plot, file= "outputs/figures/EI_plot.png", width=230, height=190, units = "mm", bg="white")

