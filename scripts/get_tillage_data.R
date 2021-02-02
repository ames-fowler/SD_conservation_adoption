#
# Author Ames Fowler
# email ames.Fowler@wsu.edu  
# Date: 7/15/20
#####################

#load-----
install.packages("rnassqs")
library(rnassqs)
library(data.table)
library(tidyverse)
library(stringr)
library(zoo)

#get data ----
path <- getwd()
whitman_files <- list.files("Data", pattern ="whit*")
wa_files <- list.files("Data/Raw_data", pattern ="wa_*")

list.files("Data/Raw_data")
CRP <- fread("Data/Raw_data/CRP Enrollment County 1986-2018.csv")
till <- fread("Data/Raw_data/CROP RESIDUE MANAGEMENT SURVEY.csv")
whitman_census <- fread()

list_of_whitman[[1]] %>%
  as.data.frame()
CRP <- CRP %>% mutate(cost = Rent_dol_acre*Area_acre)

# pull all NASS data -----
setwd("Data/")
getwd()
list_of_wa <- lapply(wa_files, fread)
list_of_whitman <- lapply(whitman_files, fread)
whitman_census <- list_of_whitman[[1]] %>%
  as.data.frame()

whitman_survey <- list_of_whitman[[2]]

## NASS census NT 2017 for whitman mannuallt added to the tillage data sheet  -----
whitman_MT <-whitman_census %>% filter(`Data Item` ==	"PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES") %>% filter(Domain == "TOTAL")
whitman_NT <-whitman_census %>% filter(`Data Item` ==	"PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES") %>% filter(Domain == "TOTAL")
whitman_CT <-whitman_census %>% filter(`Data Item` ==	"PRACTICES, LAND USE, CROPLAND, CONVENTIONAL TILLAGE - ACRES") %>% filter(Domain == "TOTAL")
whitman_easment <- whitman_census %>% filter(`Data Item` ==	"PRACTICES, LAND USE, CONSERVATION EASEMENT - ACRES") %>% filter(Domain == "TOTAL")
whitman_fallow  <- whitman_census %>% filter(`Data Item` ==	"AG LAND, CROPLAND, (EXCL HARVESTED & PASTURED), CULTIVATED SUMMER FALLOW - ACRES") %>% filter(Domain == "TOTAL")

NT_17_per <- as.numeric(gsub(",", "",whitman_NT$Value[whitman_NT$Year==2017]))/
  (as.numeric(gsub(",", "",whitman_CT$Value[whitman_CT$Year==2017]))+
     as.numeric(gsub(",", "",whitman_MT$Value[whitman_MT$Year==2017]))+
     as.numeric(gsub(",", "",whitman_NT$Value[whitman_NT$Year==2017]))+
     as.numeric(gsub(",", "",whitman_easment$Value[whitman_easment$Year==2017])))

### merge CRP Enrollment County 1986-2018.csv and ROP RESIDUE MANAGEMENT SURVEY.csv data, rename -------------------------------- 
till_CRP<- till %>% 
  mutate(`Total Planted (Acres)` = as.numeric(gsub(",","",`Total Planted (Acres)`)),
         `No-Till` = as.numeric(gsub(",","",`No-Till`)),
         `Mulch-Till` = as.numeric(gsub(",","",`Mulch-Till`)),
         `Reduced-Till` = as.numeric(gsub(",","",`Reduced-Till`)),
         `Conventional-Till` = as.numeric(gsub(",","",`Conventional-Till`)),
         `CPR` = as.numeric(gsub(",","",`CPR`))) %>% 
  merge(CRP, all=T) %>%
  mutate(`Total Planted (Acres) plus` = `Total Planted (Acres)`+ Area_acre,
         `No-Till percent` = `No-Till`/`Total Planted (Acres) plus`,
         `Mulch-Till percent` = `Mulch-Till`/`Total Planted (Acres) plus`,
         `Reduced-Till percent` = `Reduced-Till`/`Total Planted (Acres) plus`,
         `Conventional-Till percent` = `Conventional-Till`/`Total Planted (Acres) plus`,
         `CRP percent` = Area_acre/`Total Planted (Acres) plus`) 

fwrite(till_CRP, "/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/processed_data/till.dat")
