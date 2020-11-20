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

#### define loss paths -------------------------------

path_ct  <-  "F:/F drive/TK_WEPP/Twshed/WWBF_CT_OUT_CLI/"
path_mt  <-  "F:/F drive/TK_WEPP/Twshed/WWBF_MT_OUT_CLI/"
path_nt  <-  "F:/F drive/TK_WEPP/Twshed/WWBF_NT_OUT_CLI/"
path_grass <- "F:/F drive/TK_WEPP/Twshed/Grass"
list_path <- c(path_ct, path_mt, path_nt)
#### pull loss files ---------------------------------
#dir() list all file sin directory 
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

#columns for management and hillslope

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

#for each file pass through the wepp_loss_year_avg funct
wepp_dat<- file_df$. %>% map(wepp_loss_year_avg) %>% transpose()

file_df <- file_df %>%   mutate(Erosion_T_ha1 = unlist(wepp_dat[[1]])) %>%
  mutate(area_ha = wepp_dat[[2]])


drops <- c(".")
sub_df <- file_df[ , !(names(file_df) %in% drops)]


wide_df <- sub_df %>% pivot_wider(id_cols= c(area_ha, hillslope), names_from = c(management), 
                                  values_from = c(Erosion_T_ha1)) 

names(wide_df) <- c("area_ha", "hillslope","CT_t_ha1","MT_t_ha1","NT_t_ha1", "grass_t_ha1")

wide_df <- wide_df %>% arrange(desc(CT_t_ha1)) %>%
  mutate( area_cum_ha = cumsum(area_ha)) %>%
  mutate(int = cut_interval(area_cum_ha,5))%>%
  mutate(log_area_cum = log(area_cum_ha))%>%
  mutate(log_int = cut_interval(log_area_cum,5)) %>%
  mutate(erosion_ct_ton = area_ha * CT_t_ha1) %>%
  mutate(sum_ct_erosion_ton = cumsum(erosion_ct_ton)) %>%
  mutate(erosion_int = cut_interval(sum_ct_erosion_ton,5))

fwrite(wide_df, "D:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/processed_data/20.10.27_wide_erosion.csv")


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
# log_int_df<- fread("D:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/processed_data/log_bin_erosion.csv")
# erosion_int_df <- fread("D:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/processed_data/erosion_bin_erosion.csv")



# plot(wide_df$area_cum_ha,wide_df$CT_t_ha1*wide_df$area_ha, "l")

################ figure Marginal erosion returns --------------------------------------------------------

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

summary(wide$area_cum_ha)

plot(wide_df$log_area_cum,wide_df$CT_t_ha1, "l")
plot(wide_df$area_cum_ha,wide_df$MT_t_ha1, "l")
plot(wide_df$area_cum_ha,wide_df$NT_t_ha1, "l")

?cut_interval
points(log_int_df$log_int, log_int_df$CT_t_ha1)
points(log(log_int_df$, )
       names(wide_df)
       #### combine ----------------------------------------
       
       quantcut(wide_df$area_cum_ha,10)
       
       ##### order ------------------------------------------ 