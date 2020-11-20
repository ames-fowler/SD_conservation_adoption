#load shit-----
install.packages("rnassqs")
library(rnassqs)
library(data.table)
library(tidyverse)
library(stringr)
library(zoo)

#get data ----
path <- getwd()
whitman_files <- list.files("F:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data", pattern ="whit*")
wa_files <- list.files("F:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data", pattern ="wa_*")

CRP <- fread("F:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/CRP Enrollment County 1986-2018.csv")
till <- fread("F:/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/CROP RESIDUE MANAGEMENT SURVEY.csv")

CRP <- CRP %>% mutate(cost = Rent_dol_acre*Area_acre)

# hooper data and modification ----
hooper <- fread("./Data/hooper_data.txt",skip=30) %>%
  tail(-1)%>%
  rename(Discharge_cfs = `150249_00060_00003`,
         SSC_mgL = `150251_80154_00003`,
         SS_dis_tonday =`150252_80155_00003`)%>%
  select(agency_cd,  site_no,   datetime, Discharge_cfs, SSC_mgL, SS_dis_tonday)%>%
  mutate(datetime = as.Date(datetime))%>%
  mutate(Discharge_cfs = Discharge_cfs%>%
           as.character()%>%
           as.numeric())%>%
  mutate(SSC_mgL = SSC_mgL%>%
          as.character()%>%
          as.numeric())%>%
  mutate(SS_dis_tonday = SS_dis_tonday%>%
          as.character()%>%
          as.numeric())%>%
  mutate(ss_dis_avg = rollapply(SS_dis_tonday,300,mean,align='left',fill=NA))



str(hooper)
hooper_yr <- hooper %>% 
  mutate(year = year(datetime)) %>%
  group_by(year) %>%
  summarise(sum_SS = sum(SS_dis_tonday), sum_dis = sum(Discharge_cfs)*60^2*24)
##hooper plots ----
ggplot(hooper)+
  geom_point(aes(datetime,log(SS_dis_tonday)))+
  geom_line(aes(datetime,log(ss_dis_avg)))

ggplot(hooper_yr)+
  geom_point(aes(year,log(sum_SS)))+
  xlim(1956,2002)+xlab("Year")+ylab("Sediment loading (tons/year)")+
  theme_bw()
  
# explore till -----
plot(till$Year, till$`No-Till_per`, xlab=("Year"), ylab=("No-till precent"))
plot(till$Year, till$`Mulch-Till_per`, xlab=("Year"), ylab=("Mulch-till precent"))
plot(till$Year, till$`Reduced-Till_per`+till$`Conventional-Till_per`, xlab=("Year"), ylab=("Conventional till precent"))
plot(till$Year, till$CPR_per, xlab=("Year"), ylab=("CRP precent"))
plot(CRP$Year, CRP$Area_acre)
# pull all data -----
setwd("Data/")
getwd()
list_of_wa <- lapply(wa_files, fread)
list_of_whitman <- lapply(whitman_files, fread)
wa_data <- mapply(c, list_of_wa[[1]], list_of_wa[[2]],list_of_wa[[3]],list_of_wa[[4]], SIMPLIFY=FALSE)%>% 
  as.data.table()
names(list_of_whitman) <- whitman_files
whitman_census <- list_of_whitman[[1]] %>%
  as.data.frame()

whitman_survey <- list_of_whitman[[2]]
###################################################
#1
whitman_wheat_yeild<- whitman_survey %>% filter(`Data Item` == "WHEAT - YIELD, MEASURED IN BU / ACRE")
#2
Whitman_cashrent <- whitman_survey %>% filter(`Data Item` ==	"RENT, CASH, CROPLAND, NON-IRRIGATED - EXPENSE, MEASURED IN $ / ACRE")
plot(whitman_wheat_yeild$Year,whitman_wheat_yeild$Value)
plot(Whitman_cashrent$Year,Whitman_cashrent$Value)  ### cash rent record too short at county level 

#### Wash survey data for modeling whitman ----------------------------------------------- 
wa_wheat_price <- wa_wheat%>%
  filter( `Data Item` == "WHEAT - PRICE RECEIVED, MEASURED IN $ / BU") %>%
  subset(Period=="MARKETING YEAR") %>%
  select(Year, Value) %>% 
  mutate(model = exp((Year)*.019-36.499))%>%
  mutate(residuals = as.numeric(Value) - model)

ggplot(data = wa_wheat_price)+
  geom_point(aes(x=Year, y = as.numeric(Value)))+
  geom_line(aes(x=Year, y = model), col = "blue")+theme_bw()+
  ylab("Wheat Price")

plot(wa_wheat_price$Year,wa_wheat_price$Value)
line(wa_wheat_price$Year, wa_wheat_price$model, col = "blue")
model_price_inflation <- lm(formula = log(as.numeric(wa_wheat_price$Value)) ~ wa_wheat_price$Year)


str(model_price_inflation)
plot(model_price_inflation)


?lm
wa_wheat_dol <- wa_wheat%>%
  filter(`Data Item` == "WHEAT - PRODUCTION, MEASURED IN $") %>%  
  mutate(wheat_sales = as.numeric(gsub(",","",Value))) %>%
  select(Year, wheat_sales)

wa_wheat_yields <- wa_wheat%>%
  filter(`Data Item` == "WHEAT - YIELD, MEASURED IN BU / ACRE") %>%  
  mutate(wheat_yield = as.numeric(gsub(",","",Value))) %>%
  select(Year, wheat_yield)

wa_wheat_BU <- wa_wheat%>%
  filter(`Data Item` == "WHEAT - PRODUCTION, MEASURED IN BU") %>%  
  mutate(wheat_bu = as.numeric(gsub(",","",Value))) %>%
  select(Year, wheat_bu)

wa_ag_expense <- wa_data%>%
  filter(`Data Item` == "EXPENSE TOTALS, PRODUCTION - EXPENSE, MEASURED IN $") %>%  
  mutate(tot_expense = as.numeric(gsub(",","",Value))) %>%
  select(Year, tot_expense)

wa_ag_chem <- wa_data%>%
  filter(`Data Item` == "CHEMICAL TOTALS - EXPENSE, MEASURED IN $") %>%  
  mutate(chem_expense = as.numeric(gsub(",","",Value))) %>%
  select(Year, chem_expense)

wa_ag_fert <- wa_data%>%
  filter(`Data Item` == "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $") %>%  
  mutate(fert_expense = as.numeric(gsub(",","",Value))) %>%
  select(Year, fert_expense)

wa_ag_fuel <- wa_data%>%
  filter(`Data Item` == "FUELS - EXPENSE, MEASURED IN $") %>%  
  mutate(fuel_expense = as.numeric(gsub(",","",Value))) %>%
  select(Year, fuel_expense)

wa_ag_rent <- wa_data%>%
  filter(`Data Item` == "RENT, LAND & BUILDINGS - EXPENSE, MEASURED IN $") %>%  
  mutate(rent_expense = as.numeric(gsub(",","",Value))) %>%
  select(Year, rent_expense)

wa_survey <- wa_data%>%
  filter(Domain == "TOTAL") %>%  
  filter(`Domain Category` =="NOT SPECIFIED")%>%
  mutate(Value = as.numeric(gsub(",","",Value))) %>%
  select(Year, `Data Item`,Value)%>%
  pivot_wider(
    names_from = `Data Item`,
    values_from = Value) 
View(wa_survey)
  

wht_data <- whitman_tot_acres %>%
  merge(whitman_tot_expenese, all = T) %>%
  merge(whitman_tot_grain_dol,all = T) %>%
  merge(whitman_tot_sales,all = T)

#whuitman totals ----
#5 dollars
whitman_tot_sales <-whitman_census %>% filter(`Data Item` ==	"COMMODITY TOTALS - SALES, MEASURED IN $") %>% 
  filter(Domain == "TOTAL") %>%
  mutate(sales = as.numeric(gsub(",","",Value))) %>%
  select(Year, sales)

whitman_tot_expenese <-whitman_census %>% filter(`Data Item` ==	"EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $") %>% 
  filter(Domain == "TOTAL") %>%
  mutate(expenese = as.numeric(gsub(",","",Value))) %>%
  select(Year, expenese)

whitman_tot_acres <-whitman_census %>% filter(`Data Item` ==	"FARM OPERATIONS - ACRES OPERATED") %>% 
  filter(Domain == "TOTAL") %>% 
  mutate(acres = as.numeric(gsub(",","",Value))) %>%
  select(Year, acres)
  
whitman_tot_grain_dol <-whitman_census %>% filter(`Data Item` ==	"GRAIN - SALES, MEASURED IN $") %>% 
  filter(Domain == "TOTAL") %>%
  mutate(grain_sales = as.numeric(gsub(",","",Value))) %>%
  select(Year, grain_sales)

whitman_tot_sales <-whitman_census %>% filter(`Data Item` ==	"COMMODITY TOTALS - SALES, MEASURED IN $") %>% filter(Domain == "TOTAL")
##one year only## whitman_net_income <-whitman_census %>% filter(`Data Item` ==	"INCOME, NET CASH FARM, OF PRODUCERS - NET INCOME, MEASURED IN $") %>% filter(Domain == "TOTAL")

wht_data <- whitman_tot_acres %>%
  merge(whitman_tot_expenese, all = T) %>%
  merge(whitman_tot_grain_dol,all = T) %>%
  merge(whitman_tot_sales,all = T)
  



yeild_test <- whitman_tot_grain_dol %>% merge(wa_wheat_production, all.X=T)
plot(yeild_test$grain_sales, yeild_test$wheat_sales)
  
ggplot(data = wht_data, aes(x = sales, y= grain_sales))+
  geom_point()+
  geom_smooth(method = lm)

ggplot(data = wht_data, aes(x = acres, y= (sales - expenese)))+
  geom_point()+
  geom_smooth(method = lm)


pairs(wht_data)

plot(whitman_tot_sales)

#tillage
whitman_cen <- whitman_census %>%
  filter(Domain == "TOTAL")%>%
  filter(`Data Item`%in% 
           c("PRACTICES, LAND USE, CONSERVATION EASEMENT - ACRES",
             "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES",
             "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES",
             "PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES",
             "PRACTICES, LAND USE, CROPLAND, CONVENTIONAL TILLAGE - ACRES",
             "AG LAND, CROPLAND, (EXCL HARVESTED & PASTURED), CULTIVATED SUMMER FALLOW - ACRES",
             "WHEAT - ACRES HARVESTED",
             "WHEAT - SALES, MEASURED IN $",
             "WHEAT - PRODUCTION, MEASURED IN BU",
             "LENTILS - ACRES HARVESTED",
             "PEAS, DRY EDIBLE - ACRES HARVESTED",
             "PEAS, DRY EDIBLE - ACRES HARVESTED",
             "CHICKPEAS - ACRES HARVESTED",
             "CANOLA - ACRES HARVESTED",
             "BARLEY - SALES, MEASURED IN $",
             "EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $",
             "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $",
             "INCOME, FARM-RELATED - RECEIPTS, MEASURED IN $",
             "INCOME, NET CASH FARM, OF OPERATORS - LOSS, MEASURED IN $ / OPERATION",
             "INCOME, NET CASH FARM, OF OPERATORS - GAIN, MEASURED IN $ / OPERATION",
             "FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $",
             "FUELS, INCL LUBRICANTS - EXPENSE, MEASURED IN $",
             "LABOR, CONTRACT - EXPENSE, MEASURED IN $",
             "AG LAND, CROPLAND, HARVESTED - ACRES",
             "AG LAND, CROPLAND - NUMBER OF OPERATIONS",
             "RENT, CASH, LAND & BUILDINGS - EXPENSE, MEASURED IN $"))%>%
  mutate(Value = as.numeric(gsub(",","",Value))) %>%
  select(Year, Value, `Data Item`)%>%
  pivot_wider(
     names_from = `Data Item`,
     values_from = Value)%>%
  mutate(`Income` = `EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $`+
           `INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $` )%>%
  pivot_longer(names(whitman_cen)[2:length(names(whitman_cen))])


test <- whitman_cen%>%
  mutate(wheat_price = `WHEAT - SALES, MEASURED IN $`/`WHEAT - PRODUCTION, MEASURED IN BU`)%>%
  select(Year, wheat_price)

test2 <- merge(test, wa_wheat_price)

plot(test2$Year, test2$wheat_price)

ggplot(whitman_cen%>%
         filter(name %in% c("EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $",
                            'Income',
                            "INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $") ))+
  geom_line(aes(x=Year,y=value, col=name),size=1)+
  theme_bw()+ylab("Dollar Value")  +
  scale_colour_discrete(name  ="", 
                        labels=c("Expense", "Income", "Net Income"),
                      )+

  ggplot(whitman_cen%>%
           filter(name %in% c("AG LAND, CROPLAND - NUMBER OF OPERATIONS")))+
  geom_point(aes(x=Year,y=value, col=name),size=1)+
  theme_bw()+ylab("Dollar Value")  +
  scale_colour_discrete(name  ="", 
                        labels=c("Expense", "Income", "Net Income"),
  )+
  

ggplot(whitman_cen)+
  geom_line(aes(x=Year, y=whitman_cen$`WHEAT - PRODUCTION, MEASURED IN BU`/whitman_cen$`WHEAT - ACRES HARVESTED`))

ggplot(whitman_cen)+
  geom_line(aes(x=Year, y=whitman_cen$`AG LAND, CROPLAND, HARVESTED - ACRES`))+
  geom_point(aes(x=Year, y=whitman_cen$`PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES`), col = "blue")+
  geom_point(aes(x=Year, y=whitman_cen$`PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES`), col = "green")+
  geom_point(aes(x=Year, y=whitman_cen$`PRACTICES, LAND USE, CROPLAND, CONVENTIONAL TILLAGE - ACRES`), col = "brown")+
  geom_point(aes(x=Year, y=whitman_cen$`AG LAND, CROPLAND, (EXCL HARVESTED & PASTURED), CULTIVATED SUMMER FALLOW - ACRES`), col = "red")

ggplot(whitman_cen)+
  geom_point(aes(x=Year, y=whitman_cen$`PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES`/whitman_cen$`AG LAND, CROPLAND, HARVESTED - ACRES`), col = "blue")+
  geom_point(aes(x=Year, y=whitman_cen$`PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES`/whitman_cen$`AG LAND, CROPLAND, HARVESTED - ACRES`), col = "green")+
  geom_point(aes(x=Year, y=whitman_cen$`PRACTICES, LAND USE, CROPLAND, CONVENTIONAL TILLAGE - ACRES`/whitman_cen$`AG LAND, CROPLAND, HARVESTED - ACRES`), col = "brown")+
  geom_point(aes(x=Year, y=whitman_cen$`AG LAND, CROPLAND, (EXCL HARVESTED & PASTURED), CULTIVATED SUMMER FALLOW - ACRES`/whitman_cen$`AG LAND, CROPLAND, HARVESTED - ACRES`), col = "red")

ggplot(whitman_cen)+
  geom_point(aes(x=whitman_cen$`WHEAT - SALES, MEASURED IN $`/whitman_cen$`WHEAT - PRODUCTION, MEASURED IN BU`, y=whitman_cen$`AG LAND, CROPLAND, HARVESTED - ACRES`))+



plot(test$Value, test$`INCOME, NET CASH FARM, OF OPERATIONS - NET INCOME, MEASURED IN $`+test$`EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $`)
plot(test$`WHEAT - PRODUCTION, MEASURED IN BU`/test$`WHEAT - ACRES HARVESTED`,test$`EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $`)

plot(test$year, test$`WHEAT - PRODUCTION, MEASURED IN BU`)

- whitman_cen$`EXPENSE TOTALS, OPERATING - EXPENSE, MEASURED IN $`
test <- merge(wa_wheat_price, whitmane_wheat_price)

whitman_sur<-whitman_survey %>%
  filter(Domain == "TOTAL")%>%
  # filter(`Data Item`%in% 
  #          c("WHEAT - ACRES HARVESTED",
  #            "WHEAT - PRODUCTION, MEASURED IN BU",
  #            "WHEAT - YIELD, MEASURED IN BU / ACRE",
  #            "WHEAT, WINTER - ACRES HARVESTED",
  #            "WHEAT, WINTER - PRODUCTION, MEASURED IN BU",
  #            "WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE",
  #            "WHEAT, SPRING, (EXCL DURUM) - ACRES HARVESTED",
  #            "WHEAT, SPRING, (EXCL DURUM) - PRODUCTION, MEASURED IN BU",
  #            "WHEAT, SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE"))%>%
  mutate(Value = as.numeric(gsub(",","",Value))) %>%
  select(Year, Value, `Data Item`) %>%
  pivot_wider(
    names_from = `Data Item`,
    values_from = Value) 

ggplot(whitman_sur)+
  geom_point(aes(x=Year, y= `WHEAT - YIELD, MEASURED IN BU / ACRE` ))+
  geom_point(aes(x=Year, y= `WHEAT, WINTER - YIELD, MEASURED IN BU / ACRE` ), col="blue")+
  geom_point(aes(x=Year, y= `SPRING, (EXCL DURUM) - YIELD, MEASURED IN BU / ACRE` ), col="red")
## man area -----
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
     
"PRACTICES, LAND USE, CONSERVATION EASEMENT - ACRES",
"PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES",
"PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, (EXCL NO-TILL) - ACRES",
"PRACTICES, LAND USE, CROPLAND, CONSERVATION TILLAGE, NO-TILL - ACRES",
"PRACTICES, LAND USE, CROPLAND, CONVENTIONAL TILLAGE - ACRES",
"AG LAND, CROPLAND, (EXCL HARVESTED & PASTURED), CULTIVATED SUMMER FALLOW - ACRES",

## one year for each - need to add to STEEP data 

#sales
whitman_wheat_acres<- whitman_census %>% filter(`Data Item` ==	"WHEAT - ACRES HARVESTED") %>% filter(Domain == "TOTAL")
whitman_wheat_sales <- whitman_census %>% filter(`Data Item` ==	"WHEAT - SALES, MEASURED IN $") %>% filter(Domain == "TOTAL")
whitman_lentil_acres <- whitman_census %>% filter(`Data Item` ==	"LENTILS - ACRES HARVESTED") %>% filter(Domain == "TOTAL")
whitman_peas_acres <- whitman_census %>% filter(`Data Item` ==	"PEAS, DRY EDIBLE - ACRES HARVESTED") %>% filter(Domain == "TOTAL")
whitman_barley_acres <- whitman_census %>% filter(`Data Item` ==	"BARLEY - ACRES HARVESTED") %>% filter(Domain == "TOTAL")
whitman_chpeas_acres <- whitman_census %>% filter(`Data Item` ==	"CHICKPEAS - ACRES HARVESTED") %>% filter(Domain == "TOTAL")
whitman_canola_acres <- whitman_census %>% filter(`Data Item` ==	"CANOLA - ACRES HARVESTED") %>% filter(Domain == "TOTAL")
#whitman_lentil_saless <- whitman_census %>% filter(`Data Item` ==	"LENTILS - SALES, MEASURED IN $") %>% filter(Domain == "TOTAL")
#whitman_peas_sales <- whitman_census %>% filter(`Data Item` ==	"PEAS, DRY EDIBLE - SALES, MEASURED IN $") %>% filter(Domain == "TOTAL")
whitman_barley_sales <- whitman_census %>% filter(`Data Item` ==	"BARLEY - SALES, MEASURED IN $") %>% filter(Domain == "TOTAL")



whitman_fert_cost <- whitman_census %>% filter(`Data Item` ==	"FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $") %>% filter(Domain == "TOTAL")
whitman_fuel_cost <- whitman_census %>% filter(`Data Item` ==	"FUELS, INCL LUBRICANTS - EXPENSE, MEASURED IN $") %>% filter(Domain == "TOTAL")
whitman_labor_cost <- whitman_census %>% filter(`Data Item` ==	"LABOR, CONTRACT - EXPENSE, MEASURED IN $") %>% filter(Domain == "TOTAL")
whitman_labor_hired_cost <- whitman_census %>% filter(`Data Item` ==	"LABOR, HIRED - EXPENSE, MEASURED IN $") %>% filter(Domain == "TOTAL")
whitman_land_expense <- whitman_census %>% filter(`Data Item` ==	"RENT, CASH, LAND & BUILDINGS - EXPENSE, MEASURED IN $") %>% filter(Domain == "TOTAL")


## one year for each - need to add to STEEP data 
plot(whitman_tot_sales$Year,whitman_tot_sales$Value)
View(whitman_tot_expenese)

View(list_of_whitman[[2]])
wa_wheat <- filter(wa_data,Commodity == "WHEAT")

wa_barlet <- subset(wa_data, Commodity == "BARLEY")
wa_lentils <- subset(wa_data, Commodity == "LENTILS")
wa_peas <- subset(wa_data, Commodity == "PEAS")

wa_data$Commodity
View(wa_wheat_price)


library(ggplot2)

ggplot(wa_wheat_price)+
  geom_point(aes(x=Year, y= Value))

params <- list(sector_desc = "crops",
               commodity_desc = "Wheat",
               state_alpha = "WA",
               county_name  = "Whitman",
               agg_level_desc = "County",
               domaincat_desc = "not specified" )

#Check number of records
n_records <- as.integer(nassqs_record_count(params)[[1]])

v <- nassqs(params)%>%as.data.table()

v2 <- v %>%
  pivot_wider(names_from = c(short_desc,unit_desc), values_from = Value)

View(t1)

t1 <- v[v$short_desc == "WHEAT - ACRES HARVESTED"]
t2 <- v[v$short_desc == "WHEAT - ACRES PLANTED, NET"]
t3 <- v[v$short_desc == "WHEAT - ACRES PLANTED"]
t4 <- v[v$short_desc == "WHEAT - PRODUCTION, MEASURED IN BU" ]
t5 <- v[v$short_desc == "WHEAT - YIELD, MEASURED IN BU / ACRE"]
t6 <- v[v$short_desc == "WHEAT - SALES, MEASURED IN $"]

plot(t5$year,as.numeric(gsub(",","",(t5$Value))))
plot(t1$year,as.numeric(gsub(",","",(t1$Value))), type="l")

yield_model <- lm(as.numeric(gsub(",","",(t5$Value)))~t5$year)
plot(yield_model$model)
plot(as.numeric(gsub(",","",(t1$Value))),as.numeric(gsub(",","",(t5$Value))), type="l")

plot(t1$year,as.numeric(gsub(",","",(t1$Value))), type="l")


y <- c("1,200","20,000","100","12,111")
as.numeric(gsub(",", "", y))

View(v)
names(v)
unique(v$short_desc)
wheat_yeild_total <- (v$short_desc = "WHEAT - ACRES HARVESTED")

v$short_desc
str(v)

############################out of order FIX THIS SHIT ------------------------------------
test3 <- CRP %>% 
  mutate(CRP_diff =Area_acre-lag(Area_acre))%>%
  merge(wa_wheat_price, all.y=T) %>%
  mutate(Value_diff =as.numeric(Value)-lag(as.numeric(Value))) 

plot(test3$Value, test3$Area_acre, xlab="Price", ylab="CRP Area (ac)")
plot(test3$residuals, test3$Area_acre, xlab="Price residual", ylab="CRP Area (ac)")
plot(test3$Year, test3$Area_acre)
plot(test3$Year, test3$Value)
till_CRP<- till %>% 
  mutate(`Total Planted (Acres)` = as.numeric(gsub(",","",`Total Planted (Acres)`))) %>%
  mutate(`No-Till` = as.numeric(gsub(",","",`No-Till`))) %>%
  mutate(`Mulch-Till` = as.numeric(gsub(",","",`Mulch-Till`))) %>%
  mutate(`Reduced-Till` = as.numeric(gsub(",","",`Reduced-Till`))) %>%
  mutate(`Conventional-Till` = as.numeric(gsub(",","",`Conventional-Till`))) %>%
  mutate(`CPR` = as.numeric(gsub(",","",`CPR`))) %>% 
  merge(CRP, all=T) %>%
  mutate(`Total Planted (Acres) plus` = `Total Planted (Acres)`+ Area_acre) %>%
  mutate(`No-Till percent` = `No-Till`/`Total Planted (Acres) plus`) %>%
  mutate(`Mulch-Till percent` = `Mulch-Till`/`Total Planted (Acres) plus`) %>%
  mutate(`Reduced-Till percent` = `Reduced-Till`/`Total Planted (Acres) plus`) %>%
  mutate(`Conventional-Till percent` = `Conventional-Till`/`Total Planted (Acres) plus`)%>%
  mutate(`CRP percent` = Area_acre/`Total Planted (Acres) plus`)
fwrite(till_CRP, "/Dropbox/active/SD_Conservation_adoption/nass_concs_adopt/Data/processed_data/till.dat")
plot(CRP$Year, CRP$Area_acre)
ggplot(till_CRP)+
  geom_point(aes(x=`No-Till percent`, y=(`No-Till_per`)), na.rm=F)+
  geom_smooth(aes(x=`No-Till percent`, y=(`No-Till_per`)))

ggplot(till_CRP)+
  geom_point(aes(x=`Mulch-Till percent`+`Reduced-Till percent`, y=(`Mulch-Till_per`)), na.rm=F)

ggplot(till_CRP)+
  geom_point(aes(x=`Conventional-Till percent`+`Reduced-Till percent`, y=(`Mulch-Till_per`+`Reduced-Till_per`)), na.rm=F)


plot(till$Year, till$`Reduced-Till_per`+till$`Mulch-Till_per`)


ggplot(data = as.data.frame(test3))+
  geom_point(aes(x = Year, y = Value))
