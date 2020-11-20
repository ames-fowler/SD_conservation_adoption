############### Figures for SD man 1
############### FROM T WATESHEDS FOR MANUSCIPRT
#### Packages and libraries --------------------------
list.of.packages <- c("data.table","tidyverse","lubridate","grid.arrange")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# ####INSTALL LIBRARYS  ---------------------------------------------------
library(tidyverse)
library(data.table)
library(stringr)
library (gtools)
library(ggplot2)
library(gridExtra)
library(knitr)
library(rgdal)
library(raster)
library(maps)
library(ggmaps)
library(cowplot)
library(rasterVis)


#### functions -----------------------
#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

### site map ==========----

landtypes_man<-raster("./Data/processed_data/landtypes/landtype_2.tif")
WA <- map_data("state", c("washington"))
WA_co <- map_data("county", c("washington"))
huc12 <- readOGR(dsn = file.path("./Data/extent/HUC12.shp"), stringsAsFactors = F)
xy <- WA[,c(1,2)]
# xy <- WA_co[,c(1,2)]

i=huc12$OBJECTID
j=28

# kam is 19
# thorn is 28 
thorn_mask <- subset(huc12, OBJECTID == i[j])
t <- extent(thorn_mask)

r <- raster(xmn=457961, xmx=481471, ymx=5227151, ymn=5211695, res=30)
thorn_man<- crop(landtypes_man, thorn_mask) %>% mask(thorn_mask)

spdf <- SpatialPointsDataFrame(coords = xy, data = WA,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

thorn_mask_ll <- spTransform(thorn_mask, crs(spdf))
thorn_man_ll <- projectRaster(thorn_man, crs = crs(spdf), method = 'ngb')
thorn_man_ll <- crop(thorn_man_ll, thorn_mask_ll)

# unique(thorn_man_ll)

reclass_df<-c(#NA, 1,
              1,1, #other
              2,2, #Range
              3,3, #Forest
              4,4, #Water
              5,4, #Water
              6,5, #Ag land
              7,5, #Ag land
              9,5) #Ag land 

reclass_m <- matrix(reclass_df,
                    ncol = 2, 
                    byrow = T)       

man <- reclassify(thorn_man_ll,reclass_m)
# man[is.na(man[])] <- 1 

# plot(man)
# unique(man)
wa_theme <- theme(line  = element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
                  rect  = element_blank(),
                  text  = element_blank())
my_col_man <-c("#F2F2F2FF" ,"#EFC2B3FF", "burlywood4",
               "steelblue", "#00A600FF")

my_col_man_labs <- c("Other", "Range","Forest","Water",
                     "Ag land")
plot.theme.map <- theme(
  axis.ticks = element_line(color = 'black'),
  plot.title= element_text(size = 11,face="bold",hjust = 0.5), # plot title
  legend.key = element_rect(fill = NA),
  legend.key.size = unit(2, "lines"),
  legend.key.width = unit(0.5, "in"), ## here to control the legend to be smaller 
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 11),
  # axis.text.x = element_text(angle = 0, vjust = 0.5),
  panel.grid.major  = element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
  legend.background = element_rect(fill = NA),
  panel.border = element_blank(),
  panel.background  = element_blank(),
  axis.line.x = element_line(color = "grey"),
  axis.line.y = element_line(color = "grey"),
  panel.grid.minor= element_blank(),#element_line(colour = "grey10", size = 0.5, linetype = 'dashed'),
  # panel.grid.minor.y = element_blank(),
  panel.ontop = F,
  strip.text=element_blank(),
  axis.text.x = element_text(size=11), 
  axis.text.y = element_text(size=11), 
  axis.title=element_text(size=11)
  )

m <- gplot(man,maxpixels=30000000) + 
  geom_raster(aes(fill = factor(value)), alpha = .5) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=my_col_man, labels= my_col_man_labs, name = "Land use") + 
  theme_minimal()+coord_equal()+
  geom_path(data=thorn_mask_ll,
            aes(x=long, y=lat, group=group))+
  labs(x="\nLongitude", y="Latitude\n")+ theme(strip.text=element_blank())


# m
thorn_plot <- ggplot()+
  geom_path(data=thorn_mask_ll, 
                           aes(x=long, y=lat, group=group))+
  wa_theme

wa_plot <- ggplot()+
  geom_polygon(data=WA_co, aes(x=long, y=lat, group=group), fill ="white",color = "gray")+
  geom_path(data=WA, aes(x=long, y=lat, group=group))+
  geom_polygon(data=thorn_mask_ll, aes(x=long, y=lat, group=group), 
               fill="black")+wa_theme



site_map <- ggdraw() + draw_plot(m)+
  draw_plot(plot = wa_plot, -.23, -.075, scale=.33)
        
site_map
    
ggsave(filename="./figures/site_map_10.28.20.png",
       plot=site_map,
       width=6.5,
       height=3.5)


# draw into the top-right corner of a larger plot area
ggdraw() + draw_plot(p, .6, .6, .4, .4)

#### Fread in tables -------------------------------

baseline <-  fread("F:/Dropbox/active/SD_Conservation_adoption/20.07.15/baseline.csv",na.strings=NULL)
T1  <-  fread("F:/Dropbox/active/SD_Conservation_adoption/20.07.15/targeting_1.csv", na.strings=NULL)
T2  <-  fread("F:/Dropbox/active/SD_Conservation_adoption/20.07.15/targeting_2.csv", na.strings=NULL)
T3 <- fread("F:/Dropbox/active/SD_Conservation_adoption/20.07.15/targeting_3.csv", na.strings=NULL)


colnames(baseline) <- make.unique(names(baseline))
colnames(T1) <- make.unique(names(T1))
colnames(T2) <- make.unique(names(T2))
colnames(T3) <- make.unique(names(T3))

data_list <- list(baseline, T1, T2, T3)
scenarios <- c("Baseline", "Targeting 1", "Targeting 2", "Targeting 3")
names(data_list) <- scenarios



### split out NA files 
const_names <- (names(which(colSums(is.na(T1))>0)))
b_const <- baseline[,..const_names]
T1_const <- T1[,..const_names]
T2_const <- T2[,..const_names]
T3_const <- T3[,..const_names]

baseline$label<-factor(baseline$label,levels=c("CRP pre","CRP data percent"))

### plot data fit 
colors <- c("Model" = "Blue", "Data" = "Black")
y_lim1=c(0,1.0)
# Create row and column titles
col.titles = c("CRP", "Conv. Till", "Mulch Till", "No Till")
row.titles = scenarios

################### plot function 

CRP_fit <- ggplot(data = baseline,aes(x =Time))+
  geom_line(aes(y=`CRP pre`, col = 'Model'), size=1.05)+
  geom_point(aes(y = `CRP data percent`, col = 'Data'))+
  theme_minimal()+
  labs(x="Year", y="Percent of landscape", color = "Legend") + ylim(y_lim1)+
  theme(axis.text.x = element_text(angle=45))+
  scale_color_manual(values = colors)
  

MT_fit <- ggplot(data = baseline,aes(x =Time))+
  geom_line(aes(y=`mt pre`, col = 'Model'), size=1.05)+
  geom_point(aes(y = `mt data percent`, col = 'Data'))+
  theme_minimal()+ theme(legend.position="none",
                         axis.text.x = element_text(angle=45),
                         axis.title.y = element_blank())+
  labs(x="Year", y="Percent of landscape", color = "Legend")  + ylim(y_lim1)+
  scale_color_manual(values = colors)
  
CT_fit <- ggplot(data = baseline,aes(x =Time))+
  geom_line(aes(y=`ct pre`, col = 'Model'), size=1.05)+
  geom_point(aes(y = `ct data percent`, col = 'Data'))+
  theme_minimal()+ theme(legend.position="none",
                         axis.text.x = element_text(angle=45),
                         axis.title.y = element_blank())+
  labs(x="Year", y="Percent of landscape", color = "Legend")  + ylim(y_lim1)+
  scale_color_manual(values = colors)

NT_fit <- ggplot(data = baseline,aes(x =Time))+
  geom_line(aes(y=`nt pre`, col = 'Model'), size=1.05)+
  geom_point(aes(y = `nt percent data`, col = 'Data'))+
  theme_minimal()+ theme(legend.position="none",
                         axis.text.x = element_text(angle=45),
                         axis.title.y = element_blank())+
  labs(x="Year", y="Percent of landscape", color = "Legend")  + ylim(y_lim1)+
  scale_color_manual(values = colors)

### plot function

mylegend<-g_legend(CRP_fit)

p3 <- grid.arrange(arrangeGrob(CRP_fit + theme(legend.position="none"), top = col.titles[1]),
                  arrangeGrob(CT_fit + theme(legend.position="none"), top = col.titles[2]),
                  arrangeGrob(MT_fit + theme(legend.position="none"), top = col.titles[3]),
                  arrangeGrob(NT_fit + theme(legend.position="none"), top = col.titles[4]),
                               mylegend,
                               nrow=1, ncol=5, widths = c(2.3, 2.3,2.3, 2.3, 0.8))

ggsave(filename="./figures/Baseline_fit_07.15.20.png",plot=p3,width=7.5,height=2.5)

#print figure


#### by characterisitics 
#### rev(viridis(5)) ="#FDE725FF" "#5DC863FF" "#21908CFF" "#3B528BFF" "#440154FF"

colors2 <- c("CLT1" = "#FDE725FF", 
            "CLT2" = "#5DC863FF", 
            "CLT3" = "#21908CFF", 
            "CLT4" = "#3B528BFF", 
            "CLT5" = "#440154FF")

y_lim2=c(0,4000)

scenarios = c("Baseline", "Targeting 1", "Targeting 2", "Targeting 3")


#get sim legend -----------------
CT_sim <- ggplot(data = baseline,aes(x =Time))+
  geom_line(aes(y=`CT farmed area[hc1]`, col = 'CLT1'), size=1.)+
  geom_line(aes(y=`CT farmed area[hc2]`, col = 'CLT2'), size=1.)+
  geom_line(aes(y=`CT farmed area[hc3]`, col = 'CLT3'), size=1.)+
  geom_line(aes(y=`CT farmed area[hc4]`, col = 'CLT4'), size=1)+
  geom_line(aes(y=`CT farmed area[hc5]`, col = 'CLT5'), size=1.)+
  theme_minimal()+ theme(axis.text.x = element_text(angle=45),
                         axis.title.y = element_blank())+
  labs(x="Year", y="Area (ha)", color = "Legend")  + ylim(y_lim2)+
  scale_color_manual(values = colors2,guide = guide_legend(
    direction = "horizontal"))
mylegend_2<-g_legend(CT_sim)

#sim figure -----------
pl = lapply(1:length(data_list), function(i) {
  CRP_sim <- ggplot(data = data_list[[i]],aes(x =Time))+
    geom_line(aes(y=`CRP area[hc1]`, col = 'CLT1'), size=1.)+
    geom_line(aes(y=`CRP area[hc2]`, col = 'CLT2'), size=1.)+
    geom_line(aes(y=`CRP area[hc3]`, col = 'CLT3'), size=1.)+
    geom_line(aes(y=`CRP area[hc4]`, col = 'CLT4'), size=1)+
    geom_line(aes(y=`CRP area[hc5]`, col = 'CLT5'), size=1.)+
    theme_minimal()+ theme(axis.text.x = element_text(angle=45))+
    labs(main= col.titles[1],x="Year", y="Area (ha)", color = "Legend") + ylim(y_lim2)+
    scale_color_manual(values = colors2,guide = guide_legend("Land type",
                                                             direction = "horizontal"))
  CT_sim <- ggplot(data = data_list[[i]],aes(x =Time))+
    geom_line(aes(y=`CT farmed area[hc1]`, col = 'CLT1'), size=1.)+
    geom_line(aes(y=`CT farmed area[hc2]`, col = 'CLT2'), size=1.)+
    geom_line(aes(y=`CT farmed area[hc3]`, col = 'CLT3'), size=1.)+
    geom_line(aes(y=`CT farmed area[hc4]`, col = 'CLT4'), size=1)+
    geom_line(aes(y=`CT farmed area[hc5]`, col = 'CLT5'), size=1.)+
    theme_minimal()+ theme(legend.position="none",
                           axis.text.x = element_text(angle=45),
                           axis.title.y = element_blank())+
    labs(x="Year", y="Area (ha)", color = "Legend")  + ylim(y_lim2)+
    scale_color_manual(values = colors2,guide = guide_legend(
      direction = "horizontal"))
  
  MT_sim <- ggplot(data = data_list[[i]],aes(x =Time))+
    geom_line(aes(y=`MT farmed area[hc1]`, col = 'CLT1'), size=1.)+
    geom_line(aes(y=`MT farmed area[hc2]`, col = 'CLT2'), size=1.)+
    geom_line(aes(y=`MT farmed area[hc3]`, col = 'CLT3'), size=1.)+
    geom_line(aes(y=`MT farmed area[hc4]`, col = 'CLT4'), size=1)+
    geom_line(aes(y=`MT farmed area[hc5]`, col = 'CLT5'), size=1.)+
    theme_minimal()+  theme(legend.position="none",
                            axis.text.x = element_text(angle=45),
                            axis.title.y = element_blank())+
    labs(x="Year", y="Area (ha)", color = "Legend")  + ylim(y_lim2)+
    scale_color_manual(values = colors2,guide = guide_legend(
      direction = "horizontal"))
  
  NT_sim <- ggplot(data = data_list[[i]],aes(x =Time))+
    geom_line(aes(y=`NT farmed area[hc1]`, col = 'CLT1'), size=1.)+
    geom_line(aes(y=`NT farmed area[hc2]`, col = 'CLT2'), size=1.)+
    geom_line(aes(y=`NT farmed area[hc3]`, col = 'CLT3'), size=1.)+
    geom_line(aes(y=`NT farmed area[hc4]`, col = 'CLT4'), size=1)+
    geom_line(aes(y=`NT farmed area[hc5]`, col = 'CLT5'), size=1.)+
    theme_minimal()+ theme(axis.text.x = element_text(angle=45),
                           axis.title.y = element_blank())+ 
    labs(x="Year", y="Area (ha)", color = "Legend") + ylim(y_lim2)+
    scale_color_manual(values = colors2,guide = guide_legend(
      direction = "horizontal"))  
  
return(arrangeGrob(arrangeGrob(CRP_sim + theme(legend.position="none")), #, top = col.titles[1]
                   CT_sim + theme(legend.position="none"), 
                   MT_sim + theme(legend.position="none"),
                   NT_sim + theme(legend.position="none"), 
                   nrow=1,left=names(data_list)[i]))
})

figure2 <- grid.arrange(pl[[1]], pl[[2]], pl[[3]],pl[[4]], nrow=4, bottom = mylegend_2)
ggsave(filename="./figures/scenarios_9.21.20.png",plot=figure2,width=6.5,height=7.5)


pl = lapply(1:9, function(i) {
  p = ggplot(data.frame(x=1:10,y=rnorm(10)),aes(x, y)) + 
    geom_line()
})

# Create row and column titles
col.titles = paste("C_Title", 1:3)
row.titles = paste("R_Title", 4:6)

# Add row titles
pl[1:3] = lapply(1:3, function(i) arrangeGrob(pl[[i]], left=row.titles[i]))

#Add column titles and lay out plots
grid.arrange(grobs=lapply(c(1,4,7), function(i) {
  arrangeGrob(grobs=pl[i:(i+2)], top=col.titles[i/3 + 1], ncol=1)
}), ncol=3)

# grid.arrange(grobs=lapply(c(1,5,7,9), function(i) {
#   arrangeGrob(grobs=pl[[i:(i+3)]], top=col.titles[i/4 + 1], ncol=1)
# }), ncol=3)

# Plot erosion fig -------------------------------

colors3 <- c("Baseline" = "#FDE725FF", 
             "Targeting 1" = "#5DC863FF", 
             "Targeting 2" = "#21908CFF", 
             "Targeting 3" = "#3B528BFF")

Erosion <- ggplot()+
  geom_line(data = data_list$Baseline, 
            aes(x = Time, y = `summed total erosion rate`,
                linetype = names(data_list)[[1]] ))+
  geom_line(data = data_list$`Targeting 1`, 
            aes(x = Time, y = `summed total erosion rate`,
                linetype = names(data_list)[[2]]  ))+
  geom_line(data = data_list$`Targeting 2`, 
            aes(x = Time, y = `summed total erosion rate` ,
                linetype = names(data_list)[[3]] ))+
  geom_line(data = data_list$`Targeting 3`, 
            aes(x = Time, y = `summed total erosion rate` ,
                linetype = names(data_list)[[4]] ))+
  theme_minimal()+labs(x="Year", y="Erosion Rate (t/ha)", linetype = "Legend") + 
  scale_color_manual(values = colors3)+ 
  theme(legend.position = c(0.8, 0.75),
        legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))
          

Erosion

ggsave(filename="./figures/erosion_rate_7.15.20.png",plot=Erosion,width=7.5,height=3.5)


###fund fig --------------------
funds <- ggplot()+
  geom_line(data = data_list$Baseline, 
            aes(x = Time, y =`erosion reduction per conservation funds`,
                linetype = names(data_list)[[1]] ))+
  geom_line(data = data_list$`Targeting 1`, 
            aes(x = Time, y =`erosion reduction per conservation funds`,
                linetype = names(data_list)[[2]]  ))+
  geom_line(data = data_list$`Targeting 2`, 
            aes(x = Time, y =`erosion reduction per conservation funds`,
                linetype = names(data_list)[[3]] ))+
  geom_line(data = data_list$`Targeting 3`, 
            aes(x = Time, y =`erosion reduction per conservation funds`,
                linetype = names(data_list)[[4]] ))+
  theme_minimal()+labs(x="Year", y="Erosion reducion per dollar (t/$)", linetype = "Legend") + 
  scale_color_manual(values = colors3)+ 
  theme(legend.position = c(0.8, 0.75),
        legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))


funds

ggsave(filename="./figures/fund_effects_7.15.20.png",plot=funds,width=7.5,height=3.5)



# grid.arrange(figure2$grobs[[1]]$grobs[[1:4]], top = col.titles[1:4])
# View(figure2)
# # Add column titles and lay out plots
# 
# grid.arrange(grobs=lapply(c(1,5,9,13), function(i) {
#   arrangeGrob(grobs=pl[i:(i+3)], top=col.titles[i/4 + 1], ncol=1)
# }), ncol=4)
# 
# grid.arrange(grobs=lapply(c(1,4,7), function(i) {
#   arrangeGrob(grobs=pl[[i:(i+2, top=col.titles[i/3 + 1], ncol=1)
# }), ncol=3)
# CRP_sim <- ggplot(data = baseline,aes(x =Time))+
#   geom_line(aes(y=`CRP area[hc1]`, col = 'CLT1'), size=1.)+
#   geom_line(aes(y=`CRP area[hc2]`, col = 'CLT2'), size=1.)+
#   geom_line(aes(y=`CRP area[hc3]`, col = 'CLT3'), size=1.)+
#   geom_line(aes(y=`CRP area[hc4]`, col = 'CLT4'), size=1)+
#   geom_line(aes(y=`CRP area[hc5]`, col = 'CLT5'), size=1.)+
#   theme_minimal()+ theme(axis.text.x = element_text(angle=45))+
#   labs(x="Year", y="Area (ha)", color = "Legend") + ylim(y_lim2)+
#   scale_color_manual(values = colors2,guide = guide_legend("Land type",
#     direction = "horizontal"))
# 
# CT_sim <- ggplot(data = baseline,aes(x =Time))+
#   geom_line(aes(y=`CT farmed area[hc1]`, col = 'CLT1'), size=1.)+
#   geom_line(aes(y=`CT farmed area[hc2]`, col = 'CLT2'), size=1.)+
#   geom_line(aes(y=`CT farmed area[hc3]`, col = 'CLT3'), size=1.)+
#   geom_line(aes(y=`CT farmed area[hc4]`, col = 'CLT4'), size=1)+
#   geom_line(aes(y=`CT farmed area[hc5]`, col = 'CLT5'), size=1.)+
#   theme_minimal()+ theme(legend.position="none",
#                          axis.text.x = element_text(angle=45),
#                          axis.title.y = element_blank())+
#   labs(x="Year", y="Area (ha)", color = "Legend")  + ylim(y_lim2)+
#   scale_color_manual(values = colors2,guide = guide_legend(
#     direction = "horizontal"))
# mylegend_2<-g_legend(CRP_sim)
# 
# MT_sim <- ggplot(data = baseline,aes(x =Time))+
#   geom_line(aes(y=`MT farmed area[hc1]`, col = 'CLT1'), size=1.)+
#   geom_line(aes(y=`MT farmed area[hc2]`, col = 'CLT2'), size=1.)+
#   geom_line(aes(y=`MT farmed area[hc3]`, col = 'CLT3'), size=1.)+
#   geom_line(aes(y=`MT farmed area[hc4]`, col = 'CLT4'), size=1)+
#   geom_line(aes(y=`MT farmed area[hc5]`, col = 'CLT5'), size=1.)+
#   theme_minimal()+  theme(legend.position="none",
#                             axis.text.x = element_text(angle=45),
#                             axis.title.y = element_blank())+
#   labs(x="Year", y="Area (ha)", color = "Legend")  + ylim(y_lim2)+
#   scale_color_manual(values = colors2,guide = guide_legend(
#     direction = "horizontal"))
# 
# NT_sim <- ggplot(data = baseline,aes(x =Time))+
#   geom_line(aes(y=`NT farmed area[hc1]`, col = 'CLT1'), size=1.)+
#   geom_line(aes(y=`NT farmed area[hc2]`, col = 'CLT2'), size=1.)+
#   geom_line(aes(y=`NT farmed area[hc3]`, col = 'CLT3'), size=1.)+
#   geom_line(aes(y=`NT farmed area[hc4]`, col = 'CLT4'), size=1)+
#   geom_line(aes(y=`NT farmed area[hc5]`, col = 'CLT5'), size=1.)+
#   theme_minimal()+ theme(axis.text.x = element_text(angle=45),
#                          axis.title.y = element_blank())+ 
#   labs(x="Year", y="Area (ha)", color = "Legend") + ylim(y_lim2)+
#   scale_color_manual(values = colors2,guide = guide_legend(
#     direction = "horizontal"))
# #legend.position="none",
# #, direction = "horizontal"
# 
# 
# 
# 
# 
# p4 <- grid.arrange(arrangeGrob(CRP_sim + theme(legend.position="none"), 
#                                CT_sim + theme(legend.position="none"), 
#                                MT_sim + theme(legend.position="none"),
#                                NT_sim + theme(legend.position="none"), 
#                                nrow=1,left="Baseline"),
#                    arrangeGrob(CRP_sim + theme(legend.position="none"), 
#                                CT_sim + theme(legend.position="none"), 
#                                MT_sim + theme(legend.position="none"),
#                                NT_sim + theme(legend.position="none"), 
#                                nrow=1,left="Targeting 1"),
#                   mylegend_2, nrow = 3, heights = c(10,10,1))
# 
# grid_arrange_shared_legend(CRP_sim,CT_sim, MT_sim,NT_sim, ncol = 2, nrow = 2, position='top')
# # 
# #### turn this to a function then run for each Simulation make all one figure? 
# #### make the y scale all the same. Find a way to move the ledgend to the first row 
# #### or flatten... 

#testing ###_-------
nt_data <- baseline %>% select(c(`nt percent data`, `nt pre`))%>% 
  na.omit()%>%
  mutate(SE = (`nt percent data`-`nt pre`)^2) 

nt_RME <- nt_data$SE %>% mean()%>% sqrt()
test<- nt_RME%>% as.data.frame()
names(test)<- "nt_RMSE"
mt_data <- baseline %>% select(c(`mt data percent`, `mt pre`))%>% 
  na.omit()%>%
  mutate(SE = (`mt data percent`-`mt pre`)^2) 

test$mt_RMSE<- mt_data$SE %>% mean()%>% sqrt()

ct_data <- baseline %>% select(c(`ct data percent`, `ct pre`))%>% 
  na.omit()%>%
  mutate(SE = (`ct data percent`-`ct pre`)^2) 

test$ct_RMSE<- ct_data$SE %>% mean()%>% sqrt()

crp_data <- baseline %>% select(c(`CRP data percent`, `CRP pre`))%>% 
  na.omit()%>%
  mutate(SE = (`CRP data percent`-`CRP pre`)^2) 

test$CRP_RMSE<- crp_data$SE %>% mean()%>% sqrt()

str(t)
nt_r <- cor.test(x = nt_data$`nt percent data`, y= nt_data$`nt pre`, method=c("pearson"))
mt_r <- cor.test(x = mt_data$`mt data percent`, y= mt_data$`mt pre`, method="pearson") 
ct_r <- cor.test(x = ct_data$`ct data percent`, y= ct_data$`ct pre`, method="pearson") 
crp_r <- cor.test(x = crp_data$`CRP data percent`, y= crp_data$`CRP pre`, method="pearson") 

#### parameter table ------------

Parameter <- c("CRP CONTRACT LENGTH",
"MT CONTRACT LENGTH",
"NT CONTRACT LENGTH",
"CT TO NT WORD OF MOUTH EFFECTIVENESS",
"MT TO NT WORD OF MOUTH EFFECTIVENESS",
'MT WORD OF MOUTH EFFECTIVENESS',
"CRP WORD OF MOUTH EFFECTIVENESS ",
"NT OUT COEFFICIENT ",
"MT OUT COEFFICIENT")
  
Value <- c("10 [years]","3 [years]","3 [years]",
           "0.04 [-]","0.19 [-]","0.125 [-]",
           "0.005 [-]","0.005 [-]", "0.015 [-]")

  
Citation <- c("Bell 2017", "Johnson 2018", "Johnson 2018", 
"Calibrated","Calibrated","Calibrated","Calibrated",
"Calibrated","Calibrated")

par_df <- as.data.table(cbind(Parameter, Value, Citation))

#png("test.png", height=1000, width=200)
p<-tableGrob(par_df)
grid.arrange(p)
#dev.off()
save_kable(g,"figures/table_test.png")
####sensativity ----- 
sens <- fread("Data/sense.tab")
# sens_test <- sens[,c(1,9:12)]
# 
# # sens_test[grepl("[[:punct:]]", names(sens_test))]
# # 
# # strsplit(names(sens_test)[1], " ")[[1]][2:3]
# # 
# # DT.m1 = melt(sens_test, id.vars = c("Simulation", "age_mother"),
# #              measure.vars = c("dob_child1", "dob_child2", "dob_child3"))
# 
#test <- sens[names(sens)[grep("T[0-9*$]", names(sens))]]
sens_long <- melt(sens, id.vars = c("Simulation"))
test <- sens_long[grepl("T[0-9*$]", sens_long$variable),]

test2 <- head(test)

test2$variable %>% lapply(.,strsplit(as.character(.), split = " "))
lapply(test2$variable, function(x) as.numeric(strsplit(x, '[ ]')[[1]][-1]))
sens_long[1,] 
# 
# sens_2 <- cbind(sens_long$Simulation,
#       colsplit(sens_long$variable, 
#                pattern=c("","[","]"), 
#                names = c("chaffa", "step", "chaffa_2", "type")),
#       sens_long$value) 
# 
# sens_2 <- sens_2[,c(1,3,5,6)]
# 
#   reshape(sens_test, direction = "long", 
#         idvar = c("Simulation"), 
#         timevar = "Channel")
# ?reshape
# sens_test[[3:ncol(sens_test)]]
# 
# melt(setDT(sens_test), measure = patterns(paste0("T", "[0-9]*$")), 
#      value.name=letters[1:6])
# 
# g <- strsplit(names(sens_test)[2:5]," ")

test <- names(sens)[grep("T[0-9*$]", names(sens))]
g <- c(1:8,as.double(grep("T399",names(sens))))
sens_last <- sens[,c(1,2,3, 4, 5, 6, 7,8,407, 2008, 3609, 5210, # fix with select contains
                  6811, 8412, 10013, 11614, 13215, 14816, 16417, 
                  18018, 19619, 21220, 22821, 24422, 26023, 27624,
                  29225, 30826, 32427, 34028, 35629, 37230, 38831,
                  40432)]

sens_last <- sens_last %>%
  mutate(`CRP area` = sens_last %>% select(contains("CRP area"))%>% rowSums(),
         `CT area` = sens_last %>% select(contains("CT f"))%>% rowSums(),
         `MT area` = sens_last %>% select(contains("MT f"))%>% rowSums(),
         `NT area` = sens_last %>% select(contains("NT f"))%>% rowSums(),
         `Total Erosion` = sens_last %>% select(contains("Total E"))%>% rowSums())

sens_last <- sens_last %>%
  select(names(sens_last[c(2:8,35:39)]))

sens_cor<- cor(sens_last)

View(sens_cor)

#### pull loss files ---------------------------------
rm(file_paths)
file_paths <- map(.x = list_path,
                  .f = ~ dir(.x, pattern = "*loss.txt", 
                             full.names = TRUE)) %>% 
  unlist() %>%
  as.data.frame()
  