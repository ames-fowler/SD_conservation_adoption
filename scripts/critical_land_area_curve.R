############### STAND ANLONE SCRIPT TO GATHER EROSION RATE (30YR) AND HILLSLOPE AREA
############### FROM T WATESHEDS FOR MANUSCIPRT
#### Packages and libraries --------------------------
list.of.packages <- c("data.table","tidyverse","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# ####INSTALL LIBRARYS  ---------------------------------------------------
library(tidyverse)
library(data.table)
library(stringr)
library (gtools)
library(viridis)
library(rasterVis)
#### define loss paths -------------------------------

path_ct  <-  "F:/F drive/TK_WEPP/Twshed/WWBF_CT_OUT_CLI/"
path_mt  <-  "F:/F drive/TK_WEPP/Twshed/WWBF_MT_OUT_CLI/"
path_nt  <-  "F:/F drive/TK_WEPP/Twshed/WWBF_NT_OUT_CLI/"
path_grass <- "F:/F drive/TK_WEPP/Twshed/Grass"
list_path <- c(path_ct, path_mt, path_nt)
#### pull loss files ---------------------------------
rm(file_paths)
file_paths <- map(.x = list_path,
                  .f = ~ dir(.x, pattern = "*loss.txt", 
full.names = TRUE)) %>% 
  unlist() %>%
  as.data.frame()

grass_paths <- map(path_grass,
                   ~ dir(.x, pattern = "*loss.txt",
                                     full.names = T)) %>%
  unlist() %>% 
  as.data.frame()

file_df<- file_paths %>%
  mutate(management = file_paths$.%>% 
           map(str_split, pattern = "/") %>% 
           map_chr(c(1,5)) %>% map(str_split, pattern = "_") %>% 
          map_chr(c(1,2)))%>%
  mutate(hillslope = file_paths$.%>% 
           map(str_split, pattern = "/") %>% 
           map_chr(c(1,6)) %>% map(str_split, pattern = "_") %>% 
           map_chr(c(1,1)) %>%
           as.integer())

grass_df <- grass_paths %>%
  mutate(management = grass_paths$.%>% 
           map(str_split, pattern = "/") %>%
           map_chr(c(1,5)) %>% map(str_split, pattern = "_") %>% 
           map_chr(c(1,1))) %>%
  mutate(hillslope = grass_paths$.%>% 
           map(str_split, pattern = "/") %>% 
           map_chr(c(1,6)) %>% map(str_split, pattern = "_") %>% 
           map_chr(c(1,1)) %>%
           as.integer())
file_df <- rbind(file_df, grass_df)


#### scrub Area and erosion --------------------------

wepp_loss_year_avg <- function(x){
  lines <- readLines(as.character(x))
  spot <- which(lines == "     A.  AVERAGE ANNUAL SEDIMENT LEAVING PROFILE")
  line <- lines[c(spot+3)]
  numbers <- as.numeric(unlist(regmatches(line, gregexpr("[[:digit:]]+\\.*[[:digit:]]*",line))))
  return(numbers)
}


wepp_dat<- file_df$. %>% map(wepp_loss_year_avg) %>% transpose()

file_df <- file_df %>%   mutate(Erosion_T_ha1 = unlist(wepp_dat[[1]])) %>%
  mutate(area_ha = wepp_dat[[2]])


drops <- c(".")
sub_df <- file_df[ , !(names(file_df) %in% drops)]


wide_df <- sub_df %>% pivot_wider(id_cols= c(area_ha, hillslope), names_from = c(management), 
                       values_from = c(Erosion_T_ha1)) 
wide_df <- wide_df %>%
  mutate(area_ha = unlist(area_ha))         
names(wide_df) <- c("area_ha", "hillslope","CT_t_ha1","MT_t_ha1","NT_t_ha1", "grass_t_ha1")

## add topaz to wepp 
topaz_to_wepp <- fread(file = "Data/processed_data/topaz2wepp_t.txt.txt", skip = 1)[,1:2]
names(topaz_to_wepp) <- c("Topaz", "hillslope")

wide_df <- merge(wide_df, topaz_to_wepp)



wide_df <- wide_df %>% arrange(desc(CT_t_ha1)) %>%
  mutate( area_cum_ha = cumsum(area_ha)) %>%
  mutate(int = cut_interval(area_cum_ha,5))%>%
  mutate(log_area_cum = log(area_cum_ha))%>%
  mutate(log_int = cut_interval(log_area_cum,5)) %>%
  mutate(erosion_ct_ton = area_ha * CT_t_ha1) %>%
  mutate(sum_ct_erosion_ton = cumsum(erosion_ct_ton)) %>%
  mutate(erosion_int = cut_interval(sum_ct_erosion_ton,5))

fwrite(wide_df, "Data/processed_data/T_erosion.csv")

summary(wide_df)
int_df <- wide_df %>%
  group_by(int) %>%
  summarise_at(c("area_cum_ha","CT_t_ha1", "MT_t_ha1", "NT_t_ha1", "grass_t_ha1"),mean)

log_int_df <- list(c("area_ha"), c("CT_t_ha1", "MT_t_ha1", "NT_t_ha1","grass_t_ha1")) %>% 
  map2(lst(sum = sum, mean = mean), ~ wide_df %>% group_by(log_int) %>% summarise_at(.x, .y)) %>% 
  reduce(inner_join) 

erosion_int_df <- list(c("area_ha"), c("CT_t_ha1", "MT_t_ha1", "NT_t_ha1","grass_t_ha1")) %>% 
  map2(lst(sum = sum, mean = mean), ~ wide_df %>% group_by(erosion_int) %>% summarise_at(.x, .y)) %>% 
  reduce(inner_join)  

fwrite(log_int_df, "F:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/processed_data/log_bin_erosion.csv")
fwrite(erosion_int_df, "F:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/processed_data/erosion_bin_erosion.csv")

plot(wide_df$area_cum_ha,wide_df$CT_t_ha1*wide_df$area_ha, "l")

################ figure Marignal erosion returns --------------------------------------------------------

Marginal_errosion_return_fig <- ggplot(wide_df)+
  geom_area(aes(y= sum_ct_erosion_ton/max(sum_ct_erosion_ton)*100,x = area_cum_ha/max(area_cum_ha)* 100 , 
                  fill = erosion_int))+
  scale_fill_manual(values=(rev(viridis(5, alpha =.9))),labels=c("CLT1", "CLT2", "CLT3", "CLT4","CLT5"))+
  geom_line(aes(x = area_cum_ha/max(area_cum_ha)* 100 , y = sum_ct_erosion_ton/max(sum_ct_erosion_ton)*100))+
  xlab("Percent of area [%]")+ ylab("Percent of erosion [%]")+ 
  scale_y_continuous(breaks=seq(0,100,20))+scale_x_continuous(breaks=seq(0,100,20))+ theme_bw()+
  guides(fill=guide_legend(title="Land Types"))+ 
  theme(legend.position = c(0.7, 0.5),
        legend.background = element_rect(fill="white",
                                         size=0.25, linetype="solid", 
                                         colour ="black"))
  
ggsave("F:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/figures/Marg_erro_area.png", 
       plot = Marginal_errosion_return_fig,width=3.5,height=3.5)


######### raster figure --------------------------------------------------------------------
t_hills <- raster("Data/processed_data/T_hills_w.tif")
huc12 <-  shapefile("Data/extent/HUC12")
i=huc12$OBJECTID
j=28 #thorn
thorn_mask <- subset(huc12, OBJECTID == i[j])
t <- extent(thorn_mask)

t_hills <- t_hills %>% crop(thorn_mask)
plot(t_hills)

t_hills_df <- rasterToPoints(t_hills)
t_hills_df <- merge(t_hills_df, wide_df, by.x = "T_hills_w", by.y = "Topaz" )

all_topaz <-  unique(t_hills) %>% as.data.frame()
names(all_topaz)<-  "T_hills_w"
r1 <- t_hills_df[,c("T_hills_w","erosion_ct_ton")]
r2 <- t_hills_df[,c("T_hills_w","erosion_int")]

test <- merge (all_topaz, r1, all.x=T)%>% na.replace(0)                    

test2 <- merge (all_topaz, r2, all.x=T) %>% na.replace(0)                    

reclass_df1<- matrix(rbind(test2$T_hills_w,test2$erosion_int %>% as.factor()),
                     ncol = 2, 
                     byrow = T)
reclass_df2<- matrix(rbind(test$T_hills_w, test$erosion_ct_ton),
                     ncol = 2, 
                     byrow = T)
erosion_rast <- reclassify(t_hills, reclass_df2)
clt_raster <- reclassify(t_hills, reclass_df1)
plot(clt_raster)
raste_df <- rasterFromXYZ(t_hills_df, ) 







my_col_man <-rev(c(viridis(5)))

my_col_man_labs <- c("CLT 1", "CLT 2","CLT 3","CLT 4",
                     "CLT 5")
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

spatial_CLT <- gplot(clt_raster,maxpixels=30000000) + 
  geom_raster(aes(fill = factor(value)),alpha = .9) +#
  facet_wrap(~ variable) +
  scale_fill_manual(values=my_col_man, labels= my_col_man_labs, name = "Land type") + 
   theme_minimal()+coord_equal()+
  # geom_path(data=thorn_mask,
  #           aes(x=long, y=lat, group=group))+
  labs(x="\nNorthing", y="Easting\n")+ theme(strip.text=element_blank())

ggsave("./figures/spatial_CLT.png", 
       plot = spatial_CLT,
       width=6.5,
       height=3.5)
