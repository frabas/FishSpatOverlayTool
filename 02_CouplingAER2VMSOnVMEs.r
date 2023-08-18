
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# MERGE STECF AER WITH VMS 
# AND DISPACH AER LANDINGS, EFFORT AND COSTS 
# ON VMS C-SQUARE CELLS PER YEAR, COUNTRY AND METIER LEVEL6
# Author: Francois Bastardie (May 2023)
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 setwd(file.path("D:","FBA", "FishSpatOverlayTool"))

 RinputPath  <- file.path(getwd(), "INPUT_DATASETS") 
 ROutputPathToDatasets <- file.path(getwd(), "OUTCOME_DATASETS")
 RoutputPath4LargeFiles <- file.path("E:", "SeaWise")
  
 library(data.table)
 library(plyr)
 library(dplyr)
 library(vmstools)


#-----------------------------------
#-----------------------------------
#-----------------------------------
# READ STECF AER DATA

#filename         <- file.path(RinputPath, "STECF_DATA", "STECF 22-06 AER 2022 - data",
#                              "2022_STECF_22-06_EU_Fleet_Economic_and_Transversal data_fleet segment_landings_2017_2021.csv")
filename         <- file.path(RinputPath, "STECF_DATA", "2023_AER_data",
                              "2018_2021_landings_per_fs.csv")
stecf_fleetdata  <- read.csv(file=filename, sep = ";", dec = ",")
stecf_fleetdata$gear_type <- NA
stecf_fleetdata  <- stecf_fleetdata[,c("country_code","year","supra_reg","fishing_tech","vessel_length","cluster_name","fs_name","variable_group","variable_name","variable_code",
                                       "value","species_name","species_code",
                                       "sub_reg","gear_type")]

# check numbers
 dd  <- stecf_fleetdata[stecf_fleetdata$supra_reg=="NAO",]
 ddd <- dd[dd$country_code=="ESP" & dd$vessel_length=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018" & dd$variable_code=="totwghtlandg",]    
 sum(an(ddd$value))  # weight kg

str(stecf_fleetdata)
stecf_fleetdata$year <- as.character(stecf_fleetdata$year)
stecf_fleetdata$year <- as.factor(stecf_fleetdata$year)
# setting -1 values to 0
stecf_fleetdata$value[stecf_fleetdata$value < 0] <- 0

# filtering for NAO, calculating value of landings ####
stecf_fleetdata_value <- filter(stecf_fleetdata, variable_name=="Value of landings")
stecf_fleetdata_value <- stecf_fleetdata_value[, c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name","value","species_name","species_code","sub_reg","gear_type")]

# filtering for NAO, calculating kilo of landings ####
stecf_fleetdata_kilo            <- filter(stecf_fleetdata, variable_name=="Live weight of landings")
colnames(stecf_fleetdata_kilo)[names(stecf_fleetdata_kilo)=="value"] <-"weight"
stecf_fleetdata_kilo            <- stecf_fleetdata_kilo[,c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name","weight","species_name","species_code","sub_reg","gear_type")]

# joining AER value and weight data
stecf_fleetdata_with_kilo <- left_join(stecf_fleetdata_value, stecf_fleetdata_kilo, by=c("year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg","gear_type","country_code","species_name","species_code"))
# removing rows containing weight NAs
stecf_fleetdata_with_kilo <- stecf_fleetdata_with_kilo[!is.na(stecf_fleetdata_with_kilo$weight),]

# Refine dataframe to all variables necessary
stecf_fleetdata_with_kilo <- stecf_fleetdata_with_kilo[,c("country_code", "year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg","gear_type","species_code","value","weight")]


#-----------------------------------
#-----------------------------------
#-----------------------------------
# AGGREGATE STECF AER to REMOVE THE AER SPECIES INFO

stecf_fleetdata_with_kilo <- data.table(stecf_fleetdata_with_kilo)

# filter out non-deep sea species for adding an extra effort column
deepseaspp <- c("CFB","CYO","CYP","SCK","ETR","APQ","HXC","DCA","SHO","GAM","SBL","ETX","OXN","SYR","GSK",
                "PZC","ALC","PHO","BSF","ARU","ALF","KEF","CMO","CYH","RCT","RNG","EPI","BRF","ORY","RHG",
                "BLI","RIB","ANT","SBR","WRF","GHL","TVY","HPR","RTX","NEN","NNN","RIW","RJG","JAD","TJX","SFS","LXK","SFV") #  with Annex I of Reg https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:32016R2336?
stecf_fleetdata_with_kilo$value_deepsea <- stecf_fleetdata_with_kilo$value # init
stecf_fleetdata_with_kilo$weight_deepsea <- stecf_fleetdata_with_kilo$weight # init
stecf_fleetdata_with_kilo[!stecf_fleetdata_with_kilo$species_code %in% deepseaspp, "weight_deepsea"] <- 0
stecf_fleetdata_with_kilo[!stecf_fleetdata_with_kilo$species_code %in% deepseaspp, "value_deepsea"] <- 0

stecf_fleetdata_with_kilo <- stecf_fleetdata_with_kilo[, .(
                     value = sum(an(value), na.rm=T),
                     weight = sum(an(weight), na.rm=T),
                     value_deepsea = sum(an(value_deepsea), na.rm=T),
                     weight_deepsea = sum(an(weight_deepsea), na.rm=T)),
                    by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg","gear_type")]

# check numbers
 dd  <- stecf_fleetdata_with_kilo[stecf_fleetdata_with_kilo$supra_reg=="NAO",]
 ddd <- dd[dd$country_code=="ESP" & dd$vessel_length=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018",]    
 sum(ddd$weight) 


##-------
##-------
#### ### Reading and exploring STECF effort Data ####
#filename         <- file.path(RinputPath, "STECF_DATA", "STECF 22-06 AER 2022 - data",
#                              "2022_STECF_22-06_EU_Fleet_Economic_and_Transversal data_fleet segment_effort.csv")
filename         <- file.path(RinputPath, "STECF_DATA", "2023_AER_data",
                              "2018_2021_effort_per_fs.csv")
stecf_fleetdata_effort <- read.csv(file=file.path(filename), sep = ";", dec = ",")
stecf_fleetdata_effort_aerdaysatsea                <- filter(stecf_fleetdata_effort, variable_name=="Days at sea")
stecf_fleetdata_effort_aerfishingdays              <- filter(stecf_fleetdata_effort, variable_name=="Fishing days")
stecf_fleetdata_effort_aerkwFishingdays            <- filter(stecf_fleetdata_effort, variable_name=="kW fishing days")
stecf_fleetdata_effort_daysatsea                <- stecf_fleetdata_effort_aerdaysatsea[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value","sub_reg")]
stecf_fleetdata_effort_fishingdays              <- stecf_fleetdata_effort_aerfishingdays[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value","sub_reg")]
stecf_fleetdata_effort_kwFishingdays            <- stecf_fleetdata_effort_aerkwFishingdays[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value","sub_reg")]
names(stecf_fleetdata_effort_daysatsea)[7]      <- "aerdaysatsea"
names(stecf_fleetdata_effort_fishingdays)[7]    <- "aerfishingdays"
names(stecf_fleetdata_effort_kwFishingdays)[7]  <- "aerkwFishingdays"

# Joining...
stecf_fleetdata_effort_kwFishingdays  <- left_join(stecf_fleetdata_effort_kwFishingdays, stecf_fleetdata_effort_fishingdays,
                                                      by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg"))
stecf_fleetdata_effort_kwFishingdays  <- left_join(stecf_fleetdata_effort_kwFishingdays, stecf_fleetdata_effort_daysatsea,
                                                      by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name","sub_reg"))

table(is.na(stecf_fleetdata_effort_kwFishingdays$kwFishingdays))

stecf_fleetdata_effort_kwFishingdays$year <- as.factor(stecf_fleetdata_effort_kwFishingdays$year)

# joining dataframe with value and weight with the effort-dataframe
stecf_fleetdata_with_kiloandeffort <- left_join(stecf_fleetdata_with_kilo, stecf_fleetdata_effort_kwFishingdays,
                                                 by=c("country_code","year","fishing_tech","vessel_length","sub_reg", "fs_name", "supra_reg"))

stecf_fleetdata_with_kiloandeffort$aerkwFishingdays <- an(stecf_fleetdata_with_kiloandeffort$aerkwFishingdays)
stecf_fleetdata_with_kiloandeffort$aerfishingdays  <-  an(stecf_fleetdata_with_kiloandeffort$aerfishingdays)
stecf_fleetdata_with_kiloandeffort$aerdaysatsea    <-  an(stecf_fleetdata_with_kiloandeffort$aerdaysatsea)


# check 
dd <- stecf_fleetdata_with_kiloandeffort 
ddd <- dd[dd$country_code=="BEL" & dd$fishing_tech=="TBB" & dd$vessel_length=="VL2440" & dd$year=="2019",]                              
sum(ddd$aerkwFishingdays) 

dd <- stecf_fleetdata_with_kiloandeffort 
ddd <- dd[dd$country_code=="ESP" & dd$fishing_tech=="DTS" & dd$vessel_length=="VL2440" & dd$year=="2021",]                              
sum(ddd$aerkwFishingdays, na.rm=TRUE) 


#-----------------------------------
#-----------------------------------
# AGGREGATE STECF AER to REMOVE THE AER FS_NAME DIMENSION
#caution: some combination of coutry-fishtech-vesselsize has several fs_name....  
# input
dat <- stecf_fleetdata_with_kiloandeffort

# check
dat[dat$year=="2020" & dat$country_code=="ESP" & dat$fishing_tech=="HOK" & dat$vessel_length=="VL2440" ,]

# so aggregate
dat <- as.data.frame(dat)
dat <- dat[!duplicated(dat [,c("country_code", "year", "fishing_tech", "vessel_length", "fs_name", "sub_reg")]),]
dat <- data.table(dat)
dat <- dat[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("value", "weight", "aerkwFishingdays", "aerfishingdays", "aerdaysatsea"),
                                   by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length", "sub_reg")
                              ]
#caution: some combination of coutry-fishtech-vesselsize has several fs_name....  
dat[dat$year=="2020" & dat$country_code=="ESP" & dat$fishing_tech=="HOK" & dat$vessel_length=="VL2440" ,]

# output
agg_stecf_fleetdata_with_kiloeffort_nofs  <- dat


  
# check numbers
 dd  <- agg_stecf_fleetdata_with_kiloeffort_nofs[agg_stecf_fleetdata_with_kiloeffort_nofs$supra_reg=="NAO",]
 ddd <- dd[dd$country_code=="ESP" & dd$vessel_length=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018",]    
 sum(ddd$weight) 


#-----------------------------------
#-----------------------------------
#-----------------------------------
##-------
####### formatting a AER costratios ####  
#filename         <- file.path(RinputPath, "STECF_DATA", "STECF 22-06 AER 2022 - data",
#                              "2022_STECF_22-06_EU_Fleet_Economic_and_Transversal data_fleet segment_economic_variables.csv")
filename         <- file.path(RinputPath, "STECF_DATA", "STECF 22-06 AER 2022 - data",
                              "2022_STECF_22-06_EU_Fleet_Economic_and_Transversal data_fleet segment_economic_variables.csv")
AER2022_eco                <- read.csv(file=file.path(filename),sep = ";",dec = ",")
filename         <- file.path(RinputPath, "STECF_DATA", "2023_AER_data",
                              "2018_2021_economic_variables_per_fs.csv")
AER_eco                <- read.csv(file=file.path(filename),sep = ";",dec = ",") ##MISSING 2018!!

cols <- colnames(AER_eco)[colnames(AER_eco) %in% colnames(AER2022_eco)]
AER_eco   <- rbind.data.frame(AER2022_eco[AER2022_eco$year==2018,cols], AER_eco[cols])


stecf_fleetdata_energy_costs          <- filter(AER_eco, variable_name=="Energy costs") # EUR
stecf_fleetdata_personnel_costs       <- filter(AER_eco, variable_name=="Personnel costs") # EUR
stecf_fleetdata_repair_costs          <- filter(AER_eco, variable_name=="Repair & maintenance costs") # EUR
stecf_fleetdata_oth_variable_costs    <- filter(AER_eco, variable_name=="Other variable costs") # EUR
stecf_fleetdata_other_income          <- filter(AER_eco, variable_name=="Other income")  # EUR
stecf_fleetdata_unpaid_labour         <- filter(AER_eco, variable_name=="Unpaid labour")  # EUR
stecf_fleetdata_oth_non_var_costs     <- filter(AER_eco, variable_name=="Other non-variable costs")  # EUR
stecf_fleetdata_kwFishingdays         <- filter(AER_eco, variable_name=="kW fishing days")  # kWday
stecf_fleetdata_fishingdays           <- filter(AER_eco, variable_name=="Fishing days")  # day
stecf_fleetdata_kwDaysAtSea           <- filter(AER_eco, variable_name=="kW days at sea")  # kWday
stecf_fleetdata_engagedCrew           <- filter(AER_eco, variable_name=="Engaged crew")  # number 
stecf_fleetdata_cons_of_fixed_capital  <- filter(AER_eco, variable_name=="Consumption of fixed capital")  # EUR 
stecf_fleetdata_value_of_physical_capital  <- filter(AER_eco, variable_name=="Value of physical capital")  # EUR 


stecf_fleetdata_energy_costs                  <- stecf_fleetdata_energy_costs[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_personnel_costs               <- stecf_fleetdata_personnel_costs[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_repair_costs                  <- stecf_fleetdata_repair_costs[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_oth_variable_costs            <- stecf_fleetdata_oth_variable_costs[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_other_income                  <- stecf_fleetdata_other_income[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_unpaid_labour                 <- stecf_fleetdata_unpaid_labour[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_oth_non_var_costs             <- stecf_fleetdata_oth_non_var_costs[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_aerECOkwFishingdays           <- stecf_fleetdata_kwFishingdays[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_fishingdays                   <- stecf_fleetdata_fishingdays[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_kwDaysAtSea                   <- stecf_fleetdata_kwDaysAtSea [,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_engagedCrew                   <- stecf_fleetdata_engagedCrew [,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_cons_of_fixed_capital         <- stecf_fleetdata_cons_of_fixed_capital[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]
stecf_fleetdata_value_of_physical_capital     <- stecf_fleetdata_value_of_physical_capital[,c("country_code",  "year","supra_reg","fishing_tech","vessel_length","fs_name","value")]


names(stecf_fleetdata_energy_costs)[7]        <- "energycosts"
names(stecf_fleetdata_personnel_costs)[7]     <- "personnelcosts"
names(stecf_fleetdata_repair_costs)[7]        <- "repaircosts"
names(stecf_fleetdata_oth_variable_costs)[7]  <- "othvarcosts"

names(stecf_fleetdata_aerECOkwFishingdays)[7] <- "aerECOkwFishingdays"
names(stecf_fleetdata_fishingdays)[7]         <- "fishingdays"
names(stecf_fleetdata_kwDaysAtSea)[7]         <- "kwDaysAtSea"

names(stecf_fleetdata_other_income)[7]        <- "other_income"
names(stecf_fleetdata_unpaid_labour)[7]       <- "unpaid_labour"
names(stecf_fleetdata_oth_non_var_costs)[7]   <- "oth_non_var_costs"
names(stecf_fleetdata_engagedCrew)[7]         <- "engagedCrew"
names(stecf_fleetdata_cons_of_fixed_capital)[7]  <- "cons_of_fixed_capital"     
names(stecf_fleetdata_value_of_physical_capital)[7]   <- "value_of_physical_capital" 



# the equations for the Economic Evaluation we target are:
# GVA <- (landings_kg * average_price_EUR_per_kg) + other_income - unpaid_labour -  energycosts - othvarcosts -  oth_non_var_costs -  repaircosts
# GrossProfit <- GVA - personnelcosts    
# OperatingProfit <-  GrossProfit - cons_of_fixed_capital
# CapitalOpportunityCosts <- value_of_physical_capital * opportunity_interest_rate/100.0 
# NetProfit <-  OperatingProfit - CapitalOpportunityCosts  - value_of_physical_capital * ((100.0-annual_depreciation_rate)/100.0)
#=> to be computed on the final merged dataset, then to be recomputed after applying displacement scenarios (e.g. based on LPUEs-costs...) changing the income from landings and the costs
#=> would need to compute a price from landing_eur/landing_kg...

# Joining...
AERcosts  <- left_join(stecf_fleetdata_energy_costs, stecf_fleetdata_personnel_costs, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERcosts  <- left_join(AERcosts, stecf_fleetdata_repair_costs, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERcosts  <- left_join(AERcosts, stecf_fleetdata_oth_variable_costs, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERcosts  <- left_join(AERcosts, stecf_fleetdata_aerECOkwFishingdays, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERcosts  <- left_join(AERcosts, stecf_fleetdata_fishingdays, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERcosts  <- left_join(AERcosts, stecf_fleetdata_kwDaysAtSea, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))

dd <- AERcosts
dd[dd$country_code=="ESP" & dd$year=="2021" & dd$vessel_length=="VL2440" & dd$supra_reg=="NAO" & dd$fishing_tech=="DTS",]


dd <- AERcosts[AERcosts$country_code=="BEL" & AERcosts$fishing_tech=="TBB" & AERcosts$vessel_length=="VL2440" & AERcosts$year=="2019",]                              
(dd$aerECOkwFishingdays) #=>  6864051   # really different than the AER fs effort one...


# aggregate to remove the fs_name dimension
AERcosts <- as.data.frame(AERcosts)
AERcosts <- AERcosts[!duplicated(AERcosts [,c("country_code", "year", "fishing_tech", "vessel_length", "fs_name", "supra_reg")]),]
AERcosts <- data.table(AERcosts)
AERcosts$energycosts <- as.numeric(AERcosts$energycosts) 
AERcosts$personnelcosts <- as.numeric(AERcosts$personnelcosts) 
AERcosts$repaircosts <- as.numeric(AERcosts$repaircosts) 
AERcosts$othvarcosts <- as.numeric(AERcosts$othvarcosts) 
AERcosts$aerECOkwFishingdays <- as.numeric(AERcosts$aerECOkwFishingdays) 
AERcosts$fishingdays <- as.numeric(AERcosts$fishingdays) 
AERcosts$kwDaysAtSea <- as.numeric(AERcosts$kwDaysAtSea) 
AERcosts <- AERcosts[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("energycosts", "personnelcosts", "repaircosts", "othvarcosts", "aerECOkwFishingdays", "fishingdays", "kwDaysAtSea"),
                                   by=c("year", "country_code", "fishing_tech", "vessel_length", "supra_reg")
                              ]

AERcosts[AERcosts$year=="2020" & AERcosts$country_code=="ESP" & AERcosts$fishing_tech=="HOK" & AERcosts$vessel_length=="VL2440" ,]





AERothvars  <- left_join(stecf_fleetdata_engagedCrew, stecf_fleetdata_cons_of_fixed_capital, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERothvars  <- left_join(AERothvars, stecf_fleetdata_value_of_physical_capital, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERothvars  <- left_join(AERothvars, stecf_fleetdata_other_income, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERothvars  <- left_join(AERothvars, stecf_fleetdata_unpaid_labour, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))
AERothvars  <- left_join(AERothvars, stecf_fleetdata_oth_non_var_costs, by=c("country_code","year","supra_reg","fishing_tech","vessel_length","fs_name"))

# aggregate to remove the fs_name dimension
AERothvars <- as.data.frame(AERothvars)
AERothvars <- AERothvars[!duplicated(AERothvars [,c("country_code", "supra_reg", "year", "fishing_tech", "vessel_length", "fs_name")]),]
AERothvars <- data.table(AERothvars)
AERothvars$engagedCrew <- as.numeric(AERothvars$engagedCrew) 
AERothvars$other_income <- as.numeric(AERothvars$other_income) 
AERothvars$unpaid_labour <- as.numeric(AERothvars$unpaid_labour) 
AERothvars$oth_non_var_costs <- as.numeric(AERothvars$oth_non_var_costs) 
AERothvars$cons_of_fixed_capital <- as.numeric(AERothvars$cons_of_fixed_capital) 
AERothvars$value_of_physical_capital <- as.numeric(AERothvars$value_of_physical_capital) 
AERothvars <- AERothvars[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("engagedCrew", "other_income", "unpaid_labour", "oth_non_var_costs", "cons_of_fixed_capital", "value_of_physical_capital"),
                                   by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length")
                              ]

AERothvars[AERothvars$year=="2021" & AERothvars$country_code=="ESP" & AERothvars$fishing_tech=="DTS" & AERothvars$vessel_length=="VL2440" ,]


# save for later use
#filename         <- file.path(RinputPath, "STECF_DATA", "STECF 22-06 AER 2022 - data",
#                              "AERothvars_fromAER2022.RData")
filename         <- file.path(RinputPath, "STECF_DATA", "2023_AER_Data",
                              "AERothvars_fromAER2023.RData")

save(AERothvars, file=file.path(filename))


# side note: a change in nb of jobs could be expressed as: nb job change = (variation in revenue before-after) / (revenue/crew)
# or: In terms of the short term and long-term effect my suggestion, and given that there is not a dynamic model, 
# would be to calculate gross profits as short term indicator and net profit as a long term indicator.
#  When assessing the likely risk on unemployment you can refer to these two indicators, by saying that the quantity x of employees are at a risk
# in the short and/or long term if any of these indicators shifts from positive to negative. 



uan <- function(x) unlist(c(x)) # caution with data.table
AER_costratios    <-         cbind.data.frame(
                                              AERcosts[, c("country_code","year","supra_reg","fishing_tech","vessel_length")],
                                              aerECOkwFishingdays=uan(AERcosts[, "aerECOkwFishingdays"]),
                                              enerbykwfishdy= uan(AERcosts[, "energycosts"]) / uan(AERcosts[, "aerECOkwFishingdays"]),  # because variable
                                              wagebyinc= uan(AERcosts[, "personnelcosts"]) / uan(AERcosts[, "aerECOkwFishingdays"]),   # because variable
                                              repbykwfishday= uan(AERcosts[, "repaircosts"]) / uan(AERcosts[, "aerECOkwFishingdays"]),  # because variable
                                              varbykwfishday= uan(AERcosts[, "othvarcosts"]) / uan(AERcosts[, "aerECOkwFishingdays"])  # because variable
                                              )
AER_costratios$year <- as.character(AER_costratios$year)
AER_costratios <- AER_costratios[,c("country_code", "year", "supra_reg", "fishing_tech",   "vessel_length",  "aerECOkwFishingdays", "enerbykwfishdy", "wagebyinc",      "repbykwfishday", "varbykwfishday")]

AER_nonvariablevars <-       cbind.data.frame( 
                                              AERothvars[, c("country_code","year","supra_reg","fishing_tech","vessel_length")],
                                              engagedCrew= AERothvars[, "engagedCrew"],  # not variable
                                              other_income= AERothvars[, "other_income"],  # not variable
                                              unpaid_labour= AERothvars[, "unpaid_labour"],  # not variable
                                              oth_non_var_costs= AERothvars[, "oth_non_var_costs"], # not variable
                                              cons_of_fixed_capital= AERothvars[, "cons_of_fixed_capital"], # not variable
                                              value_of_physical_capital= AERothvars[, "value_of_physical_capital"] # not variable
                                              )
AER_nonvariablevars$year <- as.character(AER_nonvariablevars$year)
AER_nonvariablevars <- AER_nonvariablevars[,c("country_code", "year",  "supra_reg", "fishing_tech",   "vessel_length",  "engagedCrew",
                                        "other_income", "unpaid_labour", "oth_non_var_costs", "cons_of_fixed_capital", "value_of_physical_capital")]

AER_nonvariablevars[AER_nonvariablevars$year=="2021" & AER_nonvariablevars$country_code=="ESP" & AER_nonvariablevars$fishing_tech=="DTS" & AER_nonvariablevars$vessel_length=="VL2440" ,]






### CAUTION: NO COST INFORMED IN 2019 FROM DATABASE "STECF 20-06 - AER 2020 - data" 
### CAUTION: LIKELY NO COST INFORMED IN 2021 FROM DATABASE "STECF xx-xx - AER 2022 - data" 


aa<- AER_costratios[AER_costratios$year=="2019", "fs_name"]
bb<- AER_costratios[AER_costratios$year=="2020", "fs_name"]
unique(bb[!bb %in% aa])



##-------
# formatting a AER costratios   
#joining AER dataframe with value, weight and effort with costratio-dataframe
stecf_fleetdata_with_kiloandeffort_andcostratios <- left_join(agg_stecf_fleetdata_with_kiloeffort_nofs, AER_costratios,
                                                                           by= c("country_code", "year","supra_reg", "fishing_tech","vessel_length"))

dd <- stecf_fleetdata_with_kiloandeffort_andcostratios
dd[dd$year=="2021" & dd$country_code=="ESP" & dd$fishing_tech=="DTS" & dd$vessel_length=="VL2440" ,]


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""


# input
dat <- stecf_fleetdata_with_kiloandeffort_andcostratios


# dispatch the aerECOkwFishingdays variable over region. This var (as all AER_eco variables) is not regionalised 
tot <-  dat[,.(tot_aerECOkwFishingdays=sum(an(aerECOkwFishingdays)/1e6)), by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length")] 
dd  <- left_join(dat, tot) 
dd$share_aerECOkwFishingdays     <- (an(dd$aerECOkwFishingdays)/1e6) / an(dd$tot_aerECOkwFishingdays) # caution: a 1e6 rescaling used to avoid the too large numbers overflow
dd$aerECOkwFishingdays_perregion <- dd$share_aerECOkwFishingdays * dd$aerECOkwFishingdays  


AER_nonvariablevars[AER_nonvariablevars$year=="2020" & AER_nonvariablevars$country_code=="ESP" & AER_nonvariablevars$fishing_tech=="HOK" & AER_nonvariablevars$vessel_length=="VL2440" ,]


dd          <- left_join (dd, AER_nonvariablevars, by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length")) 
dd$varcosts <- dd$share_aerECOkwFishingdays * dd$aerECOkwFishingdays * (dd$enerbykwfishdy + dd$repbykwfishday + dd$varbykwfishday)  
#dd$fs_name <- dd$fs_name.x # a fix


# check numbers...
ddd <- dd[dd$year=="2019" & dd$country_code=="BEL" & dd$fishing_tech=="TBB" & dd$vessel_length=="VL2440",]
ddd$GVA <- (an(ddd$weight) *  # landing kg  * price
                      (an(ddd$value)/an(ddd$weight))) + 
                      an(ddd$other_income) - # plus other income
                      an(ddd$unpaid_labour) - an(ddd$varcosts)  # minus var costs



dd[dd$year=="2019" & dd$country_code=="ESP" & dd$fishing_tech=="HOK" & dd$vessel_length=="VL2440" ,]


 
# output 
stecf_fleetdata_with_kiloandeffort_andcostratios_kwdispatched  <- dd [, .SD, .SDcols=c(colnames(stecf_fleetdata_with_kiloandeffort_andcostratios), "aerECOkwFishingdays_perregion")]

  
  
# check numbers
 dd  <- stecf_fleetdata_with_kiloandeffort_andcostratios_kwdispatched[stecf_fleetdata_with_kiloandeffort_andcostratios_kwdispatched$supra_reg=="NAO",]
 ddd <- dd[dd$country_code=="ESP" & dd$vessel_length=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018",]    
 sum(ddd$weight) 
     

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""



#-----------------------------------
#-----------------------------------
#-----------------------------------
# READ SOME VMS DATA

 #vms <- read.csv(file=file.path(RinputPath, "VMS_DATA", "ICES_adhoc_VMEs", "vms_2018_21.csv"),  sep = ",", dec = ".")  # ICES
 #save(vms, file=file.path(RinputPath, "VMS_DATA", "ICES_adhoc_VMEs", "vms_2018_21.RData"))
 load(file=file.path(RinputPath, "VMS_DATA", "ICES_adhoc_VMEs", "vms_2018_21.RData"))
 head(vms)
 
 # get rid of interesting but useless info for now
 vms <- vms[, c("country", "year", "month", "c_square", "vessel_length_category", "LE_MET_level6", "fishingHours", "kwFishinghours", "totweight", "totvalue", "stat_rec", "lat", "lon", "ices_area")]


 colnames(vms)[colnames(vms)=="vessel_length_category"] <- "vessel_length" 

 # merge vessel_length cat to be consistent with AER STECF data
 vms$vessel_length         <- factor(vms$vessel_length)
 levels(vms$vessel_length) <- c("VL1218",  "VL1218", "VL1218", "VL1824", "VL2440", "VL40XX")
 vms$vessel_length         <- as.character(vms$vessel_length)
 
 # rename a bit for consistency
 colnames(vms)[colnames(vms) %in% "year"] <- "Year"
 colnames(vms)[colnames(vms) %in% "month"] <- "Month"
 colnames(vms)[colnames(vms) %in% "c_square"] <- "Csquare"
 colnames(vms)[colnames(vms) %in% "lat"] <- "SI_LATI"
 colnames(vms)[colnames(vms) %in% "lon"] <- "SI_LONG"
 colnames(vms)[colnames(vms) %in% "fishingHours"] <- "FishingHour"
 colnames(vms)[colnames(vms) %in% "kwFishinghours"] <- "kWFishingHour"
 colnames(vms)[colnames(vms) %in% "stat_rec"] <- "icesname"
 colnames(vms)[colnames(vms) %in% "LE_MET_level6"] <- "MetierL6"
 colnames(vms)[colnames(vms) %in% "ices_area"] <- "subreg"
 
 vms$country <- factor(vms$country)
 levels(vms$country)
 #[1] "BE" "ES" "FR" "GB" "IE" "NL" "PT"
 levels(vms$country) <- c("BEL", "ESP", "FRA", "GBR", "IRL", "NLD", "PRT")

  

 library(vmstools)







 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 
 # make a compatible fleet seg names with STECF AER coding
 
 
 # splitting the metier data using a temp dataframe
 library(stringr)
 temp <- as.data.frame(str_split_fixed(vms$MetierL6,"_",3))  
 vms$gear       <- temp[,1]
 vms$assemblage <- temp[,2]
 vms$meshsize   <- temp[,3]

 # checking gear table (VMS Data - DCF Data gears) to retrieve the AER compatible fishing_tech coding
 dcf_gears <- read.csv(file=file.path(getwd(),"INPUT_DATASETS", "DCF_DATA", "codetransl_fish_tech08xx.csv"), sep = ";")
 names(dcf_gears)[1]<- "gear"
 names(dcf_gears)[2]<- "fishing_tech"

 # joining gear table with VMS rawdata
 vms <- left_join(vms, dcf_gears, by="gear")


 # Retrieve the sub_reg? 
  data(ICESareas) # in vmstools
  sq         <- cbind.data.frame(unique(vms$Csquare), vmstools::CSquare2LonLat(unique(vms$Csquare), 0.05))
  sq$idx     <- an(ICESarea(sq, ICESareas, fast=TRUE))
  sq         <- sq[!is.na(sq$idx),]
  dd         <- as.data.frame(ICESareas)
  sq$sub_reg <- dd[match(sq$idx+1,dd$OBJECTID),"Area_Full"]  # caution: +1
  colnames(sq) <- c("Csquare", "lat_csq", "long_csq", "idx", "sub_reg")
  sq <- sq[,c("Csquare", "sub_reg")]
  
 # Faster (but only for NS and BS): retrieve the sub_reg corresponding to the c-squares 
 #csquare_region_data <- read.csv(file=file.path(getwd(),"SEAWise","WP5","T 5.5 Fishable areas", "Software",  "C_SQS.csv"))
 #csquare_region_data <- csquare_region_data[!is.na(csquare_region_data$EcoRegion),]
 #names(csquare_region_data)[1] <- "Csquare"
 #names(csquare_region_data)[2] <- "ecoregion"
 #names(csquare_region_data)[4] <- "sub_reg"

 # joining geo information to vms data
 #prodT53all_agg <- left_join(prodT53all_agg, csquare_region_data, by=c("Csquare"))
 vms <- left_join(vms, sq, by=c("Csquare"))




 vms                  <- data.table(vms)

 

 # aggregate VMS at Level5 (caution: all y here)
 vms$fs_name     <- paste(vms$fishing_tech, vms$vessel_length, vms$sub_reg, sep="_") # vms

 # a tentative to fix
 unique(vms[is.na(vms$sub_reg), "subreg"])
 # [1] "2a2"  "4c"   "1b"   "7j2"  "2b2"  "9a"   "7d"   "8b"   "8a"   "7b"   "12b"  "7e"   "6a"   "4a"   "7a"   "4b"   "6b1"  "7g"   "6b2"  "8c"   "12c"  "12a1" "3a"   "7f"   "12a4" "1a"   "5b1b" "5b2"  "14b2" "2a1"  "8d2"  "7k2" 
 # [33] "7c2"  "5a2"  "7h" 
 vms[is.na(vms$sub_reg) & vms$"subreg"=="2a2", "sub_reg"] <- "27.2.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="4c", "sub_reg"] <- "27.4.c"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="1b", "sub_reg"] <- "27.1.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7j2", "sub_reg"] <- "27.7.j"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="2b2", "sub_reg"] <- "27.2.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="9a", "sub_reg"] <- "27.9.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7d", "sub_reg"] <- "27.7.d"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="8b", "sub_reg"] <- "27.8.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="8a", "sub_reg"] <- "27.8.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7b", "sub_reg"] <- "27.7.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="12b", "sub_reg"] <- "27.12.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7e", "sub_reg"] <- "27.7.e"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="6a", "sub_reg"] <- "27.6.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="4a", "sub_reg"] <- "27.4.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7a", "sub_reg"] <- "27.7.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="4b", "sub_reg"] <- "27.4.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="6b1", "sub_reg"] <- "27.6.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7g", "sub_reg"] <- "27.7.g"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="6b2", "sub_reg"] <- "27.6.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="8c", "sub_reg"] <- "27.8.c"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="12c", "sub_reg"] <- "27.12.c"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="12a1", "sub_reg"] <- "27.12.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="3a", "sub_reg"] <- "27.3.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7f", "sub_reg"] <- "27.7.f"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="12a4", "sub_reg"] <- "27.12.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="1a", "sub_reg"] <- "27.1.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="5b1b", "sub_reg"] <- "27.5.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="5b2", "sub_reg"] <- "27.5.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="14b2", "sub_reg"] <- "27.14.b"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="2a1", "sub_reg"] <- "27.2.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7c2", "sub_reg"] <- "27.7.c"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="5a2", "sub_reg"] <- "27.5.a"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7h", "sub_reg"] <- "27.7.h"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="8d2", "sub_reg"] <- "27.8.d"
 vms[is.na(vms$sub_reg) & vms$"subreg"=="7k2", "sub_reg"] <- "27.7.k"
 

 vms <- vms[, .(FishingHour = sum(FishingHour, na.rm=T),
                                                 kWFishingHour = sum(kWFishingHour, na.rm=T)),
                                             by=c("country", "Year", "Csquare", "icesname", "SI_LATI", "SI_LONG",  "MetierL6", "vessel_length", "fishing_tech", "sub_reg")]  # here the month dim is lost
 vms$year <- vms$Year
 vms <- vms[, colnames(vms)[!colnames(vms) %in% "Year"], with=FALSE]

 gc(full=TRUE)



 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 
 # forsee the merging consistency and fix
    
 dd                                                          <- stecf_fleetdata_with_kiloandeffort_andcostratios_kwdispatched  # 2017-2021
 dd2                                                         <- vms[vms$year %in% c("2018", "2019", "2020", "2021"),]
 
 # inconsistent sub_reg definition found with 
 unique(dd$sub_reg[!dd$sub_reg %in% dd2$sub_reg])
 unique(dd2$sub_reg[!dd2$sub_reg %in% dd$sub_reg])
 # and a close match searched by hand with e.g.:
 # unique(dd$sub_reg)[grep("27.3", unique(dd$sub_reg))]
 # unique(dd2$sub_reg)[grep("34.1", unique(dd2$sub_reg))]
 

 
  # fix on the AER side
  # 1. fix for GSA coding in AER not compatible with vms
  idx <- grepl("sa ", dd$sub_reg) 
  dd[idx,"sub_reg"] <- paste0("g", as.character(unlist(dd[idx,"sub_reg"])))
  idx <- grepl("gsa ", dd$sub_reg) 
  library(stringr)
  temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(dd[idx,"sub_reg"]))," ",2))  
  dd[idx,"sub_reg"] <-paste0(temp[,1], temp[,2])
  # 2. fix for too refined coding:
   dd[dd$sub_reg %in% c("34.1.1.1"), "sub_reg"] <- "34.1.1"
   dd[dd$sub_reg %in% c("34.1.3.1"), "sub_reg"] <- "34.1.3"
   dd[dd$sub_reg %in% c("41.1.1"), "sub_reg"] <- "41.1"
   dd[dd$sub_reg %in% c("41.2.4"), "sub_reg"] <- "41.2"
   dd[dd$sub_reg %in% c("47.1.3"), "sub_reg"] <- "47.1.1"
   dd[dd$sub_reg %in% c("34.3.1.2"), "sub_reg"] <- "34.3.1"
   dd[dd$sub_reg %in% c("27.7.j.2"), "sub_reg"] <- "27.7.j"
   dd[dd$sub_reg %in% c("34.3.1.3"), "sub_reg"] <- "34.3.1"
   dd[dd$sub_reg %in% c("41.2.2"), "sub_reg"] <- "41.2.1"
   dd[dd$sub_reg %in% c("27.12.c"), "sub_reg"] <- "27.12"
   dd[dd$sub_reg %in% c("27.7.c.2"), "sub_reg"] <- "27.7.c"
   dd[dd$sub_reg %in% c("34.1.1.2"), "sub_reg"] <- "34.1.1"
   dd[dd$sub_reg %in% c("27.10.a.2"), "sub_reg"] <- "27.10.a"
   dd[dd$sub_reg %in% c("34.2"), "sub_reg"] <- "34.2.0"
   dd[dd$sub_reg %in% c("27.6.b.2"), "sub_reg"] <- "27.6.b"
   dd[dd$sub_reg %in% c("41.1.4"), "sub_reg"] <- "41.1"
   dd[dd$sub_reg %in% c("27.9.b.1"), "sub_reg"] <- "27.9.b"
   dd[dd$sub_reg %in% c("34.1.3.2"), "sub_reg"] <- "34.1.3"
   dd[dd$sub_reg %in% c("34.3.1.1"), "sub_reg"] <- "34.3.1"
   dd[dd$sub_reg %in% c("27.2.b.2"), "sub_reg"] <- "27.2.b"
   dd[dd$sub_reg %in% c("41.3.2"), "sub_reg"] <- "41.3.1"
   dd[dd$sub_reg %in% c("47.1.2"), "sub_reg"] <- "47.1.1"
   dd[dd$sub_reg %in% c("47.1.5"), "sub_reg"] <- "47.1.1"
   dd[dd$sub_reg %in% c("21.3.m"), "sub_reg"] <- "41.3.n"
   dd[dd$sub_reg %in% c("27.8.e.1"), "sub_reg"] <- "27.8.e"
   dd[dd$sub_reg %in% c("27.7.k.2"), "sub_reg"] <- "27.7.k"
   dd[dd$sub_reg %in% c("27.8"), "sub_reg"] <- "27.8.a"
   dd[dd$sub_reg %in% c("27.10.a.1"), "sub_reg"] <- "27.10.a"
   dd[dd$sub_reg %in% c("27.6.b.1"), "sub_reg"] <- "27.6.b"
   dd[dd$sub_reg %in% c("47.b.1"), "sub_reg"] <- "47.b.0"
   dd[dd$sub_reg %in% c("27.3.d.28"), "sub_reg"] <- "27.3.d.28.1"
   dd[dd$sub_reg %in% c("27.12.b"), "sub_reg"] <- "27.12"
   dd[dd$sub_reg %in% c("47.1.4"), "sub_reg"] <- "47.1.1"
   dd[dd$sub_reg %in% c("47.c.1"), "sub_reg"] <- "47.c.0"
   dd[dd$sub_reg %in% c("27.8.e.2"), "sub_reg"] <- "27.8.e"
   dd[dd$sub_reg %in% c("27.2.a.2"), "sub_reg"] <- "27.2.a"
   dd[dd$sub_reg %in% c("27.12.a.1"), "sub_reg"] <- "27.12.a"
   dd[dd$sub_reg %in% c("27.14.b.1"), "sub_reg"] <- "27.14.b"
   dd[dd$sub_reg %in% c("27.1"), "sub_reg"] <- "27.1.a"
   dd[dd$sub_reg %in% c("21.4.v.s"), "sub_reg"] <- "21.4.v"
   dd[dd$sub_reg %in% c( "27.9.b.2"), "sub_reg"] <- "27.9.b"
   dd[dd$sub_reg %in% c("27.12.a"), "sub_reg"] <- "27.12"
   dd[dd$sub_reg %in% c("34.1.1.3"), "sub_reg"] <- "34.1.1"
   dd[dd$sub_reg %in% c("41.2.3"), "sub_reg"] <- "41.2.1"
   dd[dd$sub_reg %in% c("27.7.j.1"), "sub_reg"] <- "27.7.j"
   dd[dd$sub_reg %in% c("21.1.f"), "sub_reg"] <- "21.1.d"
   dd[dd$sub_reg %in% c("41.3.3"), "sub_reg"] <- "41.3.1"
   dd[dd$sub_reg %in% c("27.7.k.1"), "sub_reg"] <- "27.7.k"
   dd[dd$sub_reg %in% c("27.3.b"), "sub_reg"] <- "27.3.b.23"
   dd[dd$sub_reg %in% c("27.5.b.2"), "sub_reg"] <- "27.5.b"
   dd[dd$sub_reg %in% c("27.8.d.1"), "sub_reg"] <- "27.8.d"  
   dd[dd$sub_reg %in% c( "27.7.c.1"), "sub_reg"] <- "27.7.c"
   dd[dd$sub_reg %in% c("58"), "sub_reg"] <- "58.4.1"
   dd[dd$sub_reg %in% c("87.3.3"), "sub_reg"] <- "87"
   dd[dd$sub_reg %in% c("37.1.3"), "sub_reg"] <- "gsa6"
   dd[dd$sub_reg %in% c("27.9"), "sub_reg"] <- "27.9.a"
   dd[dd$sub_reg %in% c("27.12.a.4"), "sub_reg"] <- "27.12"
   dd[dd$sub_reg %in% c("27.5.b.1.b"), "sub_reg"] <- "27.5.b"
   dd[dd$sub_reg %in% c("37.2.2"), "sub_reg"] <- "gsa19"
   dd[dd$sub_reg %in% c("27.2.a.1"), "sub_reg"] <- "27.2.a"
   dd[dd$sub_reg %in% c("37.3.1"), "sub_reg"] <- "gsa24"
   dd[dd$sub_reg %in% c("37.1.1"), "sub_reg"] <- "gsa1"
   dd[dd$sub_reg %in% c("27.5.b.1"), "sub_reg"] <- "27.5.b"
 
  # fix on the VMS side
   dd2[dd2$sub_reg %in% c("27.3.a.20", "27.3.a.21"), "sub_reg"] <- "27.3.a"
  
  
  aer_key_met                                                 <- paste(dd$year, dd$fishing_tech, dd$vessel_length, dd$sub_reg, sep="_") # aer
  vms_key_met                                                 <- paste(dd2$year, dd2$fishing_tech, dd2$vessel_length, dd2$sub_reg, sep="_") # vms
  not_in_aer_keys <- vms_key_met[!vms_key_met %in% aer_key_met]
  not_in_aer <- unique(not_in_aer_keys)
  not_in_vms_keys <- aer_key_met[!aer_key_met %in% vms_key_met]
  not_in_vms <- unique(not_in_vms_keys)

  # one example of unmatched seg?
  dd[dd$year=="2019" & dd$fishing_tech=="DFN" & dd$vessel_length=="VL0612" & dd$sub_reg=="gsa5",]  # aer
  dd2[dd2$year=="2019" & dd2$fishing_tech=="DFN" & dd2$vessel_length=="VL0612" & dd2$sub_reg=="gsa5",]  # vms
  dd2[dd2$year=="2019" & dd2$fishing_tech=="DFN" & dd2$vessel_length=="VL0612",]
  
  
  # output
  stecf_fleetdata_with_kiloandeffort_andcostratios  <- data.table(dd) # 2017-2021
  vms_effort                              <-  data.table(dd2)   # 2018-2021


  # check 
 dd <- stecf_fleetdata_with_kiloandeffort_andcostratios
  #"2019_VL0010_DFN_27.10.a" 
 head(dd[year=="2019" & vessel_length=="VL0010" & fishing_tech=="DFN" &   sub_reg=="27.10.a",])



 

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# subset for the region of interest....
# HERE THE NAO:



  stecf_fleetdata_with_kiloandeffort_andcostratios_nao  <- 
   stecf_fleetdata_with_kiloandeffort_andcostratios[stecf_fleetdata_with_kiloandeffort_andcostratios$supra_reg=="NAO",]
  
  vms_effort_nao   <-  vms_effort#[supra_region=="NAO",]
  rm(vms_effort)
  gc()



# an overall check
 dd <- stecf_fleetdata_with_kiloandeffort_andcostratios_nao[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("value","weight","aerkwFishingdays", "aerfishingdays", "aerdaysatsea"),
          keyby=c("year")]
 dd <- knitr::kable(as.data.frame(dd), format = "html")
 library(readr)
 readr::write_file(dd, file.path(ROutputPathToDatasets, "an_overall_check0.html")) 



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

years <- as.character(2018:2021) 
# caution: merging for 2021 is leaking, likely because incomplete AER data (if AER 2022 is used)


 # add primary keys before merging - adapt the key on the fly to avoid loosing effort or landings....
 
   # ROBUST MERGING: catch for leaks and adapt the key on the fly 
   aer_all_y <- NULL
   vms_all_y <- NULL
   aer_y_leftover <- NULL
   aer_y_leftover2 <- NULL
   aer_y_leftover3 <- NULL
   vms_y_leftover <- NULL
   vms_y_leftover2 <- NULL
   vms_y_leftover3 <- NULL
   tracked_leaks <- NULL
   for(y in years)
   {
     # Add a default key (i.e. year-country-vessellength-Level5-icesrect)
      vms_y                   <- vms_effort_nao[year==y,]
      vms_y$key               <- paste(vms_y$year, vms_y$country, vms_y$vessel_length, vms_y$fishing_tech, vms_y$sub_reg, sep="_")  # full key on vms
      aer_y                   <- stecf_fleetdata_with_kiloandeffort_andcostratios_nao[year==y,]
      aer_y$key               <- paste(aer_y$year, aer_y$country_code, aer_y$vessel_length, aer_y$fishing_tech, aer_y$sub_reg, sep="_") # full key on aer
      aer_y$chunk <- 0
      vms_y$chunk <- 0
      m1                      <- unique(vms_y$key)
      m2                      <- unique(aer_y$key)
     vms_y_met_not_in_aer     <- m1[!m1 %in% m2]
     vms_y_met_in_aer         <- m1[m1 %in% m2]
     aer_y_leftover           <- aer_y[!aer_y$key %in% vms_y_met_in_aer,]
     vms_y_leftover           <- vms_y[!vms_y$key %in% vms_y_met_in_aer,]
     aer_y_main               <- aer_y[aer_y$key %in% vms_y_met_in_aer,]
     vms_y_main               <- vms_y[vms_y$key %in% vms_y_met_in_aer,]
     aer_y_main$chunk       <- 1 # coding
     vms_y_main$chunk       <- 1  # coding
     # repeat with a less constraining key  to catch the left-over records:
     if(nrow(aer_y_leftover)!=0 && nrow(vms_y_leftover)!=0)
     {
      aer_y_leftover$key       <- paste(aer_y_leftover$year,  aer_y_leftover$country_code, aer_y_leftover$vessel_length, aer_y_leftover$fishing_tech,  sep="_") # aer
      vms_y_leftover$key       <- paste(vms_y_leftover$year, vms_y_leftover$country, vms_y_leftover$vessel_length, vms_y_leftover$fishing_tech, sep="_")  # vms
      m1                       <- unique(vms_y_leftover$key)
      m2                       <- unique(aer_y_leftover$key)
      vms_y_met_not_in_aer     <- m1[!m1 %in% m2]
      vms_y_met_in_aer         <- m1[m1 %in% m2]
      aer_y_leftover2          <- aer_y_leftover[!aer_y_leftover$key %in% vms_y_met_in_aer,]
      vms_y_leftover2          <- vms_y_leftover[!vms_y_leftover$key %in% vms_y_met_in_aer,]
      aer_y_leftover           <- aer_y_leftover[aer_y_leftover$key %in% vms_y_met_in_aer,]
      vms_y_leftover           <- vms_y_leftover[vms_y_leftover$key %in% vms_y_met_in_aer,]
      #destroy the sub_reg dim in aer
      aer_y_leftover_agg1 <- aer_y_leftover[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("value", "weight", "aerkwFishingdays", "aerfishingdays", "aerdaysatsea", "aerECOkwFishingdays", "aerECOkwFishingdays_perregion"),
                                   by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length", "key")]
      aer_y_leftover_agg2 <- aer_y_leftover[,lapply(.SD, mean, na.rm=TRUE), 
                                   .SDcols=c("enerbykwfishdy","wagebyinc","repbykwfishday","varbykwfishday"),
                                   by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length", "key")]
      aer_y_leftover      <- cbind(aer_y_leftover_agg1, aer_y_leftover_agg2[,-c(1:6)])
      aer_y_leftover$chunk       <- 2 # coding
      #destroy the sub_reg dim in vms
      vms_y_leftover      <- vms_y_leftover[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("FishingHour", "kWFishingHour"),
                                   by=c("Csquare", "icesname", "SI_LATI", "SI_LONG", "MetierL6", "vessel_length", "fishing_tech", "year", "country", "key")]    
      vms_y_leftover$chunk <- 2  # coding      
      if(nrow(aer_y_leftover2))  warning("First attempt: There aer records left here...")
      if(nrow(vms_y_leftover2)) warning("First attempt: There vms records left here...")
    
       # repeat with a less constraining key  to catch the left-over records:
      if(nrow(aer_y_leftover2)!=0 && nrow(vms_y_leftover2)!=0){
        aer_y_leftover2$key       <- paste(aer_y_leftover2$year, aer_y_leftover2$vessel_length,  sep="_") # aer
        vms_y_leftover2$key       <- paste(vms_y_leftover2$year, vms_y_leftover2$vessel_length,  sep="_")  # vms
        m1                       <- unique(vms_y_leftover2$key)
        m2                       <- unique(aer_y_leftover2$key)
        vms_y_met_not_in_aer     <- m1[!m1 %in% m2]
        vms_y_met_in_aer         <- m1[m1 %in% m2]
        aer_y_leftover3          <- aer_y_leftover2[!aer_y_leftover2$key %in% vms_y_met_in_aer,]
        vms_y_leftover3          <- vms_y_leftover2[!vms_y_leftover2$key %in% vms_y_met_in_aer,]
        aer_y_leftover2           <- aer_y_leftover2[aer_y_leftover2$key %in% vms_y_met_in_aer,]
        vms_y_leftover2           <- vms_y_leftover2[vms_y_leftover2$key %in% vms_y_met_in_aer,]
        #destroy the sub_reg and fishing_tech dims in aer
        aer_y_leftover2_agg1 <- aer_y_leftover2[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("value", "weight", "aerkwFishingdays", "aerfishingdays", "aerdaysatsea", "aerECOkwFishingdays", "aerECOkwFishingdays_perregion"),
                                   by=c("year", "supra_reg", "country_code", "vessel_length", "key")]
        aer_y_leftover2_agg2 <- aer_y_leftover2[,lapply(.SD, mean, na.rm=TRUE), 
                                   .SDcols=c("enerbykwfishdy","wagebyinc","repbykwfishday","varbykwfishday"),
                                   by=c("year", "supra_reg", "country_code", "vessel_length", "key")]
        aer_y_leftover2      <- cbind(aer_y_leftover2_agg1, aer_y_leftover2_agg2[,-c(1:6)])
        aer_y_leftover2$chunk       <- 3 # coding
        #destroy the sub_reg and fishing_tech dims in vms
        vms_y_leftover2      <- vms_y_leftover2[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("FishingHour", "kWFishingHour"),
                                   by=c("Csquare", "icesname", "SI_LATI", "SI_LONG", "MetierL6", "vessel_length", "year", "key")]    
        vms_y_leftover2$chunk <- 3  # coding      
        if(nrow(aer_y_leftover3))  warning("Second attempt: There aer records left here...")  # lost
        if(nrow(vms_y_leftover3)) warning("Second attempt: There vms records left here...")   # lost
      }
     } 
    
    
    # bind # fill=TRUE to add missing columns and fill out with NAs 
    aer_left <- rbind(aer_y_main, aer_y_leftover, aer_y_leftover2, fill=TRUE)   
    vms_left <- rbind(vms_y_main, vms_y_leftover, vms_y_leftover2, fill=TRUE)
   
   # document effort leak this y  
   d <- function(x) as.data.frame(x)
   dd1 <-rbind.data.frame(
   "init"=d(vms_y[  ,.(vmstotfishhours=sum(FishingHour )),]), # initial tot eff vms met in vms
   "year, country, vessel_length, fishing_tech, sub_reg"=d(vms_y_main[  ,.(vmstotfishhours =sum(FishingHour )),]),        #  year, vessel_length, fishing_tech, sub_reg  # full key 
   "year, country, vessel_length, fishing_tech"=d(vms_y_leftover[  ,.(vmstotfishhours =sum(FishingHour )),]),    #  year, vessel_length, fishing_tech
   "year,  vessel_length"=d(vms_y_leftover2[  ,.(vmstotfishhours =sum(FishingHour )),]),   #  year, vessel_length
   "unfortunate lost"=d(vms_y_leftover3[  ,.(vmstotfishhours =sum(FishingHour )),]),   #  the remaining: not matched...
   "finally left"=d(vms_left[  ,.(vmstotfishhours =sum(FishingHour )),]) # tot eff vms left
   )

   # document landings leak this y
   d <- function(x) as.data.frame(x)
   dd2 <-rbind.data.frame(
   "init"=d(vms_y[  ,.(vmstotKwfishhours=sum(an(kWFishingHour))),]), # initial tot in vms
   "year, country, vessel_length, fishing_tech, sub_reg"=d(vms_y_main[  ,.(vmstotKwfishhours=sum(an(kWFishingHour))),]),        #  year, vessel_length, fishing_tech, sub_reg  # full key  
   "year, country, vessel_length, fishing_tech"=d(vms_y_leftover[  ,.(vmstotKwfishhours=sum(an(kWFishingHour))),]),    #  year, vessel_length, fishing_tech 
   "year, vessel_length"=d(vms_y_leftover2[  ,.(vmstotKwfishhours=sum(an(kWFishingHour))),]),   #  year, vessel_length 
   "unfortunate lost"=d(vms_y_leftover3[  ,.(vmstotKwfishhours=sum(an(kWFishingHour))),]),   #  the remaining: not matched... 
   "finally left"=d(vms_left[  ,.(vmstotKwfishhours=sum(an(kWFishingHour))),]) # tot in vms left
   )
   
   tracked_leaks <- rbind.data.frame (tracked_leaks, cbind.data.frame(y, dd1, dd2))
   
   
   aer_all_y <- rbind(aer_all_y, aer_left)
   vms_all_y <- rbind(vms_all_y, vms_left)
   
   } # end y
   
   
# check what is left, what is lost...   
 print(tracked_leaks)
 dd <- knitr::kable(as.data.frame(tracked_leaks), format = "html")
 library(readr)
 readr::write_file(dd, file.path(ROutputPathToDatasets, "tracked_leaks_AER_to_VMS.html")) 
 
 
# check 
vms_all_y[, .(vmstotKwfishhours = sum(an(kWFishingHour), na.rm=T),
                                                 vmstotfishhours = sum(an(FishingHour), na.rm=T)),
                                             by=c("year", "chunk")]  
    
aer_all_y[, .(weight = sum(an(weight), na.rm=T),
                                                 value = sum(an(value), na.rm=T)),
                                             by=c("year", "chunk")]  
 
 #-----------------------------------
 #-----------------------------------
 #-----------------------------------


 # calculate share of countries on effort
# aa <- data.table(aer_all_y)
# agg_kwfdays <- aa[,.(tot_kwFishingdays=sum(aerECOkwFishingdays_perregion)),by=c("year", "key")]
# aer_all_y <- merge(aer_all_y, agg_kwfdays,
#                  by= c("year", "key"))

# aer_all_y$country_share_effort <- 
#       aer_all_y$aerECOkwFishingdays_perregion/aer_all_y$tot_kwFishingdays



 

 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 # MERGE vms WITH AER, AND DISPACH vms EFFORT AND AER kwfishingdays and COSTS ON vms C-SQUARE CELLS PER YEAR, COUNTRY AND METIER LEVEL6

 save(vms_effort_nao, file=file.path(ROutputPathToDatasets, "vms_2018_2021_in_NAO_before_merging.RData"))
 head(vms_effort_nao)

 
 dispatched_aer <- list()

 for (y in as.character(years))
 {
   cat(paste("y", y, "\n"))
  
   # landings
   aer_y                                      <-   aer_all_y[aer_all_y$year==y,]
   aer_y[,.(tot_kg=sum(an(weight))),] # check 
   # effort
   vms_y                                      <-  vms_all_y[vms_all_y$year==y,]
   vms_y[,.(tot_totfishhours=sum(FishingHour)),] # check 
   
   # get a share_effort for dispatching data (caution, vms data are species explicit data)
   sum_effort_y_per_key                     <-  vms_y[, .(tot_totfishhours = sum(FishingHour, na.rm=T)), by=c("key")]  # removing the c-square dimension here...
   sum_effort_y_per_key_level6              <-  vms_y[, .(tot_totfishhours_met = sum(FishingHour, na.rm=T)), by=c("MetierL6", "key")]  # removing the c-square dimension here...
   #sum_effort_y_per_key_level6_inzone[,.(tot_effort=sum(tot_totfishhours_met_csquare)),] # check 
   vms_y_e                                 <-  merge(vms_y, sum_effort_y_per_key, by=c("key"))
   vms_y_e                                 <-  merge(vms_y_e, sum_effort_y_per_key_level6, by=c("MetierL6", "key"))
   vms_y_e$share_effort_level6             <-  vms_y_e$tot_totfishhours_met / vms_y_e$tot_totfishhours # for dispatching depending on the contribution of that metier to the total effort in this key
   vms_y_e$share_effort_level6_csquare     <-  vms_y_e$FishingHour / vms_y_e$tot_totfishhours_met # for dispatching depending on the contribution of that cell to the total effort in that key-metier-zone
   vms_y_e$year <- as.character(vms_y_e$year)
   
   # a check column: "md5" should be equal to the sum at the key level i.e. "vmstotfishhours"
   vms_y_e$md5 <- vms_y_e$tot_totfishhours * vms_y_e$share_effort_level6 * vms_y_e$share_effort_level6_csquare  
   vms_y_e[,.(tot_totfishhours=sum(FishingHour)),] # check 
   
   # clean a bit the data.table to save memory!
   rm_col <- c("md5", "tot_totfishhours", "tot_totfishhours_met", "SI_LATI", "SI_LONG")
   vms_y_e <- vms_y_e[, (rm_col):=NULL]
   rm_col <- c("fishing_tech", "supra_reg", "vessel_length")
   aer_y <- aer_y[, (rm_col):=NULL]
  
  
   # check for unexpected duplicates?
   #a_df <- as.data.frame(vms_y_e)
   #a_tab <- table(paste0(a_df$key, a_df$Csquare, a_df$MetierL6))
   #a_tab[a_tab>1]

  
  
  # a check before the merging as such
  if(FALSE){
    nonvariablevars <- AER_nonvariablevars[AER_nonvariablevars$supra_reg=="NAO",]
    temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(aer_y  [,"key"])),"_",5))  
    aer_y[, "vessel_length"] <- temp[,3]
    aer_y[, "fishing_tech"] <- temp[,4]
    dd          <- left_join (aer_y, nonvariablevars, by=c("year", "country_code", "fishing_tech", "vessel_length")) 
    dd$varcosts <- dd$aerECOkwFishingdays_perregion * (dd$enerbykwfishdy + dd$repbykwfishday + dd$varbykwfishday)  
    # check numbers...
    ddd <- dd[dd$year=="2019" & dd$country_code=="BEL" & dd$fishing_tech=="TBB" & dd$vessel_length=="VL2440",]
    ddd$GVA <- (an(ddd$weight) *  # landing kg  * price
                      (an(ddd$value)/an(ddd$weight))) + 
                      an(ddd$other_income) - # plus other income
                      an(ddd$unpaid_labour) - an(ddd$varcosts)  # minus var costs
    ddd[ddd$sub_reg=="27.4.b","weight"] #=> 4204869
    sum(ddd[,"weight"]) #=> 13386476
    } # end FALSE
  
  
   
   # merge (by chunk, to avoid memory issue)
   gc(full=TRUE); rm(merged)
   keys <- unlist(c(unique(aer_y[aer_y$chunk==1, "key"])))  # needed for the trick to chunk...
   idx  <- c(floor(seq(1,length(keys),by=length(keys)/10)),length(keys))
   chunk1_1                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[1:idx[2]] ,], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_2                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[2]+1):idx[3]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_3                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[3]+1):idx[4]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_4                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[4]+1):idx[5]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_5                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[5]+1):idx[6]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_6                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[6]+1):idx[7]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_7                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[7]+1):idx[8]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_8                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[8]+1):idx[9]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_9                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[9]+1):idx[10]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_10                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[10]+1):idx[11]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   keys <- unlist(c(unique(aer_y[aer_y$chunk==2, "key"])))  # needed for the trick to chunk...
   if(length(keys)>0) idx  <- c(floor(seq(1,length(keys),by=length(keys)/5)),length(keys))
   chunk2_1                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[1:idx[2]],], vms_y_e[vms_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_2                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[2]+1):idx[3]],], vms_y_e[vms_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_3                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[3]+1):idx[4]],], vms_y_e[vms_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_4                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[4]+1):idx[5]],], vms_y_e[vms_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_5                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[5]+1):idx[6]],], vms_y_e[vms_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   keys <- unlist(c(unique(aer_y[aer_y$chunk==3, "key"])))  # needed for the trick to chunk...
   if(length(keys)>0) idx  <- c(floor(seq(1,length(keys),by=length(keys)/2)),length(keys))
   chunk3_1                                       <- merge(aer_y[aer_y$chunk==3 & aer_y$key %in% keys[1:idx[2]],], vms_y_e[vms_y_e$chunk==3,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk3_2                                       <- merge(aer_y[aer_y$chunk==3 & aer_y$key %in% keys[(idx[2]+1):idx[3]],], vms_y_e[vms_y_e$chunk==3,], by.x="key", by.y="key", allow.cartesian=TRUE)
 
 
   gc(full=TRUE); rm(merged)
   cols <- c("key", "country_code","year.x","value", "weight","aerkwFishingdays", "aerdaysatsea", "aerECOkwFishingdays_perregion", 
             "FishingHour", "kWFishingHour","enerbykwfishdy","wagebyinc","repbykwfishday","varbykwfishday", #"country_share_effort",
             "MetierL6","Csquare","icesname","share_effort_level6", "share_effort_level6_csquare")
   merged <- rbind.data.frame(chunk1_1[,..cols], chunk1_2[,..cols], chunk1_3[,..cols],  chunk1_4[,..cols],  chunk1_5[,..cols],  chunk1_6[,..cols], chunk1_7[,..cols], chunk1_8[,..cols], chunk1_9[,..cols], chunk1_10[,..cols],
                  chunk2_1[,..cols], chunk2_2[,..cols], chunk2_3[,..cols], chunk2_4[,..cols], chunk2_5[,..cols],
                  chunk3_1[,..cols], chunk3_2[,..cols])
   gc(full=TRUE)
         
   
   # dispatch AER vars over ctry-level6-csquare combinaisons depending on share alloc keys. Caution: no species info in AER
   #(caution:  merged$country_share_effort should only be used if dispatching the vms data....)
   merged$FishingHour                                     <-  merged$FishingHour  #* merged$country_share_effort                   
   a_dispatcher_key                                       <-  merged$share_effort_level6 * merged$share_effort_level6_csquare # * merged$country_share_effort     # country_share when VMS are only metier-based data on Csquare  
   merged$landings_aer_in_ctry_level6_csquare             <-  merged$weight                             * a_dispatcher_key  # dispatch on cell 
   merged$value_aer_in_ctry_level6_csquare                <-  merged$value                              * a_dispatcher_key  # dispatch on cell 
   merged$KwFishingdays_aer_in_ctry_level6_csquare        <-  an(merged$aerECOkwFishingdays_perregion)  * a_dispatcher_key  # dispatch on cell 
   merged$daysatsea_aer_in_ctry_level6_csquare            <-  an(merged$aerdaysatsea)                   * a_dispatcher_key  # dispatch on cell 
  
   # check the overall conservation of FishingHour
   merged[,.(tot_totfishhours=sum(FishingHour, na.rm=TRUE)),] # check 
   vms_y[,.(tot_totfishhours=sum(FishingHour, na.rm=TRUE)),] # check
  
  
   
   # check for conservation of effort per fs
   if(FALSE){
    dd <- vms_y_e
    for(k in unique(dd$key)){
      aa <- dd[dd$year=="2020"  & dd$key==k,]
      ddd <- aer_y
      temp <- as.data.frame(str_split_fixed(ddd$key,"_",5))  
      ddd$vessel_length <- temp[,3]
      ddd$fishing_tech <- temp[,4]
      bb <- ddd[ddd$year=="2020"  & ddd$key==k,]
      cc                                     <- merge(bb, aa, by.x="key", by.y="key", allow.cartesian=TRUE)
    if (round(sum(aa$FishingHour)) != round(sum(cc$FishingHour))) {
    #if (round(sum(aa$FishingHour)) != round(sum(cc$FishingHour*cc$country_share_effort))) {
      cat(paste("no conservation for", k, "...break\n"))
      break
      }
    }
   } # end FALSE
  
  
  
   # check
   library(stringr)
   temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(merged[,"key"])),"_",5))  
   dd <- merged[merged$country_code=="BEL" & temp[,4]=="TBB" & temp[,3]=="VL2440" & merged$y=="2019",]                              
   ddd <- unique(dd, by = c("key", "country_code", "Csquare"))# remove spp dim
   ddd[,sum( KwFishingdays_aer_in_ctry_level6_csquare, na.rm=TRUE),] #=> 14230352
   
   temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(aer_y[,"key"])),"_",5))  
   dd <- aer_y[aer_y$country_code=="BEL" & temp[,4]=="TBB" & temp[,3]=="VL2440" & aer_y$y=="2019",]
   dd[,sum(aerECOkwFishingdays, na.rm=TRUE),] 
   dd[,sum(aerECOkwFishingdays, na.rm=TRUE),]*1.727336   #=> 130421746
  
  dd <- AERcosts[AERcosts$country_code=="BEL" & AERcosts$fishing_tech=="TBB" & AERcosts$vessel_length=="VL2440" & AERcosts$y=="2019",]                              
  an(dd$aerECOkwFishingdays) 
  an(dd$energycosts) # 11856524
  an(dd$energycosts)/an(dd$aerECOkwFishingdays)
  an(dd$energycosts)+an(dd$repaircosts)+an(dd$othvarcosts)


 
   # clean a bit 
   colnames(merged) <- gsub(".x", "", colnames(merged), fixed=TRUE) 
   merged             <- merged  [, c("key","country_code","year","MetierL6","Csquare", "icesname",  "FishingHour", "kWFishingHour",
                                                        "daysatsea_aer_in_ctry_level6_csquare","landings_aer_in_ctry_level6_csquare",
                                                        "value_aer_in_ctry_level6_csquare","KwFishingdays_aer_in_ctry_level6_csquare",
                                                         "enerbykwfishdy", "wagebyinc", "repbykwfishday", "varbykwfishday")]
  

  # a check after the merging and dispatching
  if(FALSE){
    nonvariablevars <- AER_nonvariablevars[AER_nonvariablevars$supra_reg=="NAO",]
    temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(merged  [,"key"])),"_",5))  
    merged[, "vessel_length"] <- temp[,3]
    merged[, "fishing_tech"] <- temp[,4]
    merged[, "sub_reg"] <- temp[,5]
    dd          <- left_join (merged, nonvariablevars, by=c("year", "country_code", "fishing_tech", "vessel_length")) 
    dd$varcosts <- dd$KwFishingdays_aer_in_ctry_level6_csquare * (dd$enerbykwfishdy + dd$repbykwfishday + dd$varbykwfishday)  
    # check numbers...
    ddd <- dd[dd$year=="2019" & dd$country_code=="BEL" & dd$fishing_tech=="TBB" & dd$vessel_length=="VL2440",]
    ddd$GVAish <- (an(ddd$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(ddd$value_aer_in_ctry_level6_csquare)/an(ddd$landings_aer_in_ctry_level6_csquare))) - an(ddd$varcosts)  # landing value minus var costs  (-ish because here we lack oth income and unpaid labour and fixed costs...)
    sum(ddd[ddd$sub_reg=="27.4.b","landings_aer_in_ctry_level6_csquare"]) #=>  4204869
    sum(ddd[,"landings_aer_in_ctry_level6_csquare", ],na.rm=TRUE) #=> 13386476
    } # end FALSE

                                                                                                                                 
   #compute LPUEs and disagregate costs 
   merged$lpue_aer_this_species_in_ctry_level6_csquare       <- merged$landings_aer_in_ctry_level6_csquare/merged$FishingHour  # aer kg per day in this cell, for all species pooled
   merged$energycosts_in_ctry_level6_csquare                 <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$enerbykwfishdy  # energy costs
   merged$personnelcosts_in_ctry_level6_csquare              <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$wagebyinc  # personel costs
   merged$repaircosts_in_ctry_level6_csquare                 <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$repbykwfishday  # repair costs
   merged$othvarcosts_in_ctry_level6_csquare                 <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$varbykwfishday  # other variable costs
 
  
   # clean a bit 
   merged             <- merged  [, c("key","country_code","year","MetierL6","Csquare", "icesname",  "FishingHour", "kWFishingHour",
                                                        "landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare", 
                                                        "daysatsea_aer_in_ctry_level6_csquare", "KwFishingdays_aer_in_ctry_level6_csquare",
                                                        "lpue_aer_this_species_in_ctry_level6_csquare", 
                                                        "energycosts_in_ctry_level6_csquare", "personnelcosts_in_ctry_level6_csquare", "repaircosts_in_ctry_level6_csquare", 
                                                        "othvarcosts_in_ctry_level6_csquare")]
   # save
   save(merged, file=file.path(ROutputPathToDatasets, paste0("dispatched_aer_vars_using_vms_effort_csquares_",y,".RData"))) 
   
   dispatched_aer[[y]]  <- merged 
   }

 # check 
 #head(dispatched_aer[["2019"]])
 


 # save 
 dispatched_aer_ally <- do.call("rbind", dispatched_aer)
 
 

 # check what is left, what is lost...   
 # an overall check
 dd <- dispatched_aer_ally[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("FishingHour","kWFishingHour","landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare","daysatsea_aer_in_ctry_level6_csquare",
                  "KwFishingdays_aer_in_ctry_level6_csquare","energycosts_in_ctry_level6_csquare","personnelcosts_in_ctry_level6_csquare","repaircosts_in_ctry_level6_csquare","othvarcosts_in_ctry_level6_csquare"),
          keyby=c("year")]
 library(readr)
 dd <- knitr::kable(as.data.frame(dd), format = "html")
 readr::write_file(dd, file.path(ROutputPathToDatasets, "an_overall_check_ICES_vms_AER_2018_2021.html")) 
 #=> leaks in landings due to missing countries in the VMS dataset (SeaWise misses Sweden, Spain and Portugal)...so not all the initial AER landings can be matched


 save(dispatched_aer_ally, file=file.path(ROutputPathToDatasets, "dispatched_aer_vars_on_ICES_vms_2018_2021_effort_csquares_ally.RData")) 



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# Compute the economic indicators!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!(standalone code)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#


# the equations for the Economic Evaluation we target are:

# 1. MERGE with AERothvars

# 2. FOR LANDINGS AND VARIABLE COSTS, DO A RASTER CALCULATION FOR GETTING THE GVA:
# GVA <- (landings_kg * average_price_EUR_per_kg) + other_income - unpaid_labour -  energycosts - othvarcosts -  oth_non_var_costs -  repaircosts

# 3. FOR NON-VARIABLE AER VARS, DO A MERGE WITH AER PER FS, AND THEN COMPUTE THE FOLLOWING:
# GrossProfit <- GVA - personnelcosts    
# OperatingProfit <-  GrossProfit - cons_of_fixed_capital
# CapitalOpportunityCosts <- value_of_physical_capital * opportunity_interest_rate/100.0 
# NetProfit <-  OperatingProfit - CapitalOpportunityCosts  - value_of_physical_capital * ((100.0-annual_depreciation_rate)/100.0)
#=> to be computed on the final merged dataset, then to be recomputed after applying displacement scenarios (e.g. based on LPUEs-costs...) changing the income from landings and the costs

# TABULATE per year and deduce the change before/after closure
 

library(data.table)
 
 # load (likely needed to reload here because we might be short in memory...)
 load(file=file.path(file.path(ROutputPathToDatasets, "dispatched_aer_vars_on_ICES_vms_2018_2021_effort_csquares_ally.RData")))  # get "dispatched_aer_ally"

  # check
  library(stringr)
  temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(dispatched_aer_ally[,"key"])),"_",5))  
  dispatched_aer_ally[,"fs"] <- paste0(unlist(c(dispatched_aer_ally[, "country_code"])), "_", temp[,4], "_", temp[,3])
  dispatched_aer_ally[,"fs2"] <- paste0(unlist(c(dispatched_aer_ally[, "country_code"])), "_", temp[,2])
  

 # an overall check
 dd <- dispatched_aer_ally[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("FishingHour","kWFishingHour","landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare","daysatsea_aer_in_ctry_level6_csquare",
                  "KwFishingdays_aer_in_ctry_level6_csquare","energycosts_in_ctry_level6_csquare","personnelcosts_in_ctry_level6_csquare","repaircosts_in_ctry_level6_csquare","othvarcosts_in_ctry_level6_csquare"),
          keyby=c("year", "fs")]
 dd[fs=="ESP_DTS_VL2440",]  
 dd <- knitr::kable(as.data.frame(dd), format = "html")
 library(readr)
 readr::write_file(dd, file.path(ROutputPathToDatasets, "an_overall_check.html")) 
    
 
 
      
 # retrieve the coord
 library(vmstools)
 library(dplyr)
 sq         <- cbind.data.frame(Csquare=unique(dispatched_aer_ally$Csquare), vmstools::CSquare2LonLat(unique(dispatched_aer_ally$Csquare), 0.05))
 distr         <- dplyr::left_join(dispatched_aer_ally, sq, by="Csquare")
 

  ## !!!export!!! ##
 save(distr, file=file.path(ROutputPathToDatasets, "distr_AER2022_on_ICES_VMS_2018_2021.RData"))



## aggregate per Csquare, year and key-country
  # all dims with the explicit species dim can be summed (i.e. FDI totwghtlandg totvallandg totfishhours, but also all the AER dispatched ones)
  distr_allsp <- 
     distr[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("FishingHour","kWFishingHour","landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare","daysatsea_aer_in_ctry_level6_csquare",
                  "KwFishingdays_aer_in_ctry_level6_csquare","energycosts_in_ctry_level6_csquare","personnelcosts_in_ctry_level6_csquare","repaircosts_in_ctry_level6_csquare","othvarcosts_in_ctry_level6_csquare"),
          keyby=c("year","key", "country_code", "Csquare", "icesname", "SI_LATI", "SI_LONG")]
                    
 
  # recompute a LPUE        
  distr_allsp$lpue_csquare_aer_kgperdayatsea  <- distr_allsp$landings_aer_in_ctry_level6_csquare/distr_allsp$daysatsea_aer_in_ctry_level6_csquare  # kg per daysatsea
  distr_allsp$lpue_csquare_vms_kgperfhour     <- distr_allsp$landings_aer_in_ctry_level6_csquare/distr_allsp$FishingHour  # kg per fhour
 
 
  # load to later merge with AERothvars
  #filename         <- file.path(RinputPath, "STECF_DATA", "STECF 22-06 AER 2022 - data",
  #                            "AERothvars_fromAER2022.RData")
  filename         <- file.path(RinputPath, "STECF_DATA", "2023_AER_Data",
                              "AERothvars_fromAER2023.RData")
 load(file=file.path(filename))  #  get AERothvars
  
  # check
  AERothvars[AERothvars$year=="2020" & AERothvars$country_code=="ESP" & AERothvars$fishing_tech=="HOK" & AERothvars$vessel_length=="VL2440" ,]

  # add fs
  AERothvars     <- as.data.frame(AERothvars)
  AERothvars$fs  <- paste0(AERothvars[, "country_code"], "_", AERothvars[, "fishing_tech"], "_", AERothvars[, "vessel_length"])
  AERothvars$fs2 <- paste0(AERothvars[, "country_code"], "_", AERothvars[, "vessel_length"])
  AERothvars     <- data.table(AERothvars)
  
  # check
  AERothvars[AERothvars$year=="2020" & AERothvars$fs=="ESP_HOK_VL2440" ,]
  AERothvars[AERothvars$year=="2021" & AERothvars$fs=="ESP_DTS_VL1824" ,]
  AERothvars[AERothvars$year=="2018" & AERothvars$fs=="ESP_DTS_VL1824" ,]


  # retrieve fleet segments
  library(stringr)
  temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(distr_allsp[,"key"])),"_",5))  
  distr_allsp[,"fs"] <- paste0(unlist(c(distr_allsp[, "country_code"])), "_", temp[,4], "_", temp[,3])
  distr_allsp[,"fs2"] <- paste0(unlist(c(distr_allsp[, "country_code"])), "_", temp[,2])

  # caution: we are only looking at the NAO region for now
  AERothvars <- AERothvars[supra_reg=="NAO",]

  # compute the GVA from spatial landings and variable costs and non-spatial fixed costs
  # GVA <- (landings_kg * average_price_EUR_per_kg) + other_income - unpaid_labour -  energycosts - othvarcosts -  oth_non_var_costs -  repaircosts
  # but then, first merge to dispatch other_income and unpaid_labour on Csquare per fs per year...
  # count csquares...
  
  # but first split into 2 given the fs vs fs2 segmentation:
  distr_allsp_fs <- distr_allsp[!grep("__", distr_allsp$fs),]
  distr_allsp_fs2 <- distr_allsp[grep("__", distr_allsp$fs),] 
  
  # merge for fs
  an <- function(x) as.numeric(x) 
  csquare_per_fs_per_y    <-  distr_allsp_fs[,.(nb_csquare_per_fs_per_y=length(unique(Csquare))), by=c("year","fs")]
  distr_allsp_fs          <-  dplyr::left_join(distr_allsp_fs, csquare_per_fs_per_y)
  AERothvars$year         <-  as.character(AERothvars$year)
  # then merge...
  distr_allsp_fs <- dplyr::left_join(distr_allsp_fs, AERothvars[,c("other_income", "unpaid_labour", "oth_non_var_costs", "year", "fs")])
  # then dispatch...
  distr_allsp_fs$other_income_in_csquare    <- an(distr_allsp_fs$other_income)/an(distr_allsp_fs$nb_csquare_per_fs_per_y)
  distr_allsp_fs$unpaid_labour_in_csquare   <- an(distr_allsp_fs$unpaid_labour)/an(distr_allsp_fs$nb_csquare_per_fs_per_y)
  distr_allsp_fs$oth_non_var_costs_in_csquare   <- an(distr_allsp_fs$oth_non_var_costs)/an(distr_allsp_fs$nb_csquare_per_fs_per_y)
  distr_allsp_fs$other_income_in_csquare [is.na(distr_allsp_fs$other_income_in_csquare)] <- 0
  distr_allsp_fs$unpaid_labour_in_csquare [is.na(distr_allsp_fs$unpaid_labour_in_csquare)] <- 0
  distr_allsp_fs$oth_non_var_costs_in_csquare [is.na(distr_allsp_fs$oth_non_var_costs_in_csquare)] <- 0
 
  # merge for fs2
  an <- function(x) as.numeric(x) 
  csquare_per_fs_per_y <-  distr_allsp_fs2[,.(nb_csquare_per_fs_per_y=length(unique(Csquare))), by=c("year","fs2")]
  distr_allsp_fs2      <-  dplyr::left_join(distr_allsp_fs2, csquare_per_fs_per_y)
  AERothvars$year      <-  as.character(AERothvars$year)
  AERothvars2          <-   AERothvars[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("engagedCrew","other_income","unpaid_labour","oth_non_var_costs","cons_of_fixed_capital","value_of_physical_capital"),
          keyby=c("year", "fs2")]  
  # then merge...
  distr_allsp_fs2 <- dplyr::left_join(distr_allsp_fs2, AERothvars2[,c("other_income", "unpaid_labour", "oth_non_var_costs","year", "fs2")])
  # then dispatch...
  distr_allsp_fs2$other_income_in_csquare    <- an(distr_allsp_fs2$other_income)/an(distr_allsp_fs2$nb_csquare_per_fs_per_y)
  distr_allsp_fs2$unpaid_labour_in_csquare   <- an(distr_allsp_fs2$unpaid_labour)/an(distr_allsp_fs2$nb_csquare_per_fs_per_y)
  distr_allsp_fs2$oth_non_var_costs_in_csquare   <- an(distr_allsp_fs2$oth_non_var_costs)/an(distr_allsp_fs2$nb_csquare_per_fs_per_y)
  distr_allsp_fs2$other_income_in_csquare [is.na(distr_allsp_fs2$other_income_in_csquare)] <- 0
  distr_allsp_fs2$unpaid_labour_in_csquare [is.na(distr_allsp_fs2$unpaid_labour_in_csquare)] <- 0
  distr_allsp_fs2$oth_non_var_costs_in_csquare [is.na(distr_allsp_fs2$oth_non_var_costs_in_csquare)] <- 0
  distr_allsp_fs2$fs <- distr_allsp_fs2$fs2
  
  # then rebuild
  distr_allsp <- rbind(distr_allsp_fs, distr_allsp_fs2)
  
 # compute an overall var costs (those come from energycosts per fday...)
 distr_allsp$varcosts_in_ctry_level6_csquare    <- distr_allsp$energycosts_in_ctry_level6_csquare + 
                    distr_allsp$repaircosts_in_ctry_level6_csquare + 
                    distr_allsp$othvarcosts_in_ctry_level6_csquare
 
 # then compute GVA (remember that in FDI totwghtlandg is in tonnes)
  distr_allsp$GVA <- (an(distr_allsp$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(distr_allsp$value_aer_in_ctry_level6_csquare)/an(distr_allsp$landings_aer_in_ctry_level6_csquare))) + 
                      an(distr_allsp$other_income_in_csquare) - # plus other income
                      an(distr_allsp$unpaid_labour_in_csquare) - an(distr_allsp$varcosts_in_ctry_level6_csquare) - an(distr_allsp$oth_non_var_costs_in_csquare)  # minus var and fixed costs

 
 
  # check 
  distr_allsp[distr_allsp$fs=="BEL_TBB_VL2440",]
  dd <- knitr::kable(as.data.frame(distr_allsp[distr_allsp$fs=="BEL_TBB_VL2440",]), format = "html")
  library(readr)
  readr::write_file(dd, file.path("D:", "FBA", "Temp", "distr_allsp_2.html")) 
  # check further
  dd <- distr_allsp[distr_allsp$year=="2019" & distr_allsp$fs=="BEL_TBB_VL2440",]
  dd[,.( sumkwfday=sum(KwFishingdays_aer_in_ctry_level6_csquare),
          sumkg=sum(landings_aer_in_ctry_level6_csquare), 
          value=sum(value_aer_in_ctry_level6_csquare), 
          varcosts=sum(varcosts_in_ctry_level6_csquare, na.rm=TRUE),
          fixedcosts=sum(oth_non_var_costs_in_csquare, na.rm=TRUE),
          GVA=sum(GVA, na.rm=TRUE)
          ),]
  #AER_costratios[AER_costratios$country_code=="BEL" & AER_costratios$fishing_tech=="TBB" & AER_costratios$vessel_length=="VL2440",]                              
  #aer_2019 <-   aer_all_y[aer_all_y$year=="2019",]
  #dd <- aer_2019[aer_2019$country_code=="BEL" & aer_2019$fishing_tech=="TBB" & aer_2019$vessel_length=="VL2440",]                              
  #dd <- AERcosts[AERcosts$country_code=="BEL" & AERcosts$fishing_tech=="TBB" & AERcosts$vessel_length=="VL2440" & AERcosts$y=="2019",]                              
  #an(dd$energycosts)/an(dd$aerECOkwFishingdays)
  #an(dd$energycosts)+an(dd$repaircosts)+an(dd$othvarcosts)
 
 

# 3. FOR NON-VARIABLE AER VARS, DO A MERGE WITH AER PER FS, AND THEN COMPUTE THE FOLLOWING:
# GrossProfit <- GVA - personnelcosts    
# OperatingProfit <-  GrossProfit - cons_of_fixed_capital
# CapitalOpportunityCosts <- value_of_physical_capital * opportunity_interest_rate/100.0 
# NetProfit <-  OperatingProfit - CapitalOpportunityCosts  - value_of_physical_capital * ((100.0-annual_depreciation_rate)/100.0)
#=> to be computed on the final merged dataset, then to be recomputed after applying displacement scenarios (e.g. based on LPUEs-costs...) changing the income from landings and the costs

 distr_allsp$GrossProfit <- distr_allsp$GVA - an(distr_allsp$personnelcosts_in_ctry_level6_csquare)


  ## !!!export!!! ##
 save(distr_allsp, file=file.path(ROutputPathToDatasets, "distr_allsp_from_AER_coupled_to_ICES_VMS_2018_2021.RData"))

                                                        

 # remove the spatial dim to get final eco estimates
 agg_eco_fs <- distr_allsp[,.( sumkwfday=sum(KwFishingdays_aer_in_ctry_level6_csquare),
          sumkg=sum(landings_aer_in_ctry_level6_csquare), 
          value=sum(value_aer_in_ctry_level6_csquare), 
          varcosts=sum(varcosts_in_ctry_level6_csquare, na.rm=TRUE),
          fixedcosts=sum(oth_non_var_costs_in_csquare, na.rm=TRUE),
          GVA=sum(GVA, na.rm=TRUE),
          GrossProfit=sum(GrossProfit, na.rm=TRUE)
          ), by=c("year","fs")]


 agg_eco_fs <- dplyr::left_join(agg_eco_fs, AERothvars[,c("value_of_physical_capital", "cons_of_fixed_capital", "engagedCrew", "year", "fs")])  
 opportunity_interest_rate  <- 4.0 # ??
 annual_depreciation_rate   <- 2.0 # ??
 agg_eco_fs$CapitalOpportunityCosts    <- an(agg_eco_fs$value_of_physical_capital) * opportunity_interest_rate/100.0 

 agg_eco_fs$OperatingProfit <-  agg_eco_fs$GrossProfit - an(agg_eco_fs$cons_of_fixed_capital)
 agg_eco_fs$NetProfit       <-  agg_eco_fs$OperatingProfit - agg_eco_fs$CapitalOpportunityCosts  - (an(agg_eco_fs$value_of_physical_capital) * (1-((100.0-annual_depreciation_rate)/100.0)))


 # check
 agg_eco_fs[agg_eco_fs$fs=="BEL_TBB_VL2440",]
 agg_eco_fs[agg_eco_fs$fs=="ESP_DTS_VL2440",]
 
 
 ## !!!export!!! ##
 save(agg_eco_fs, file=file.path(ROutputPathToDatasets, "agg_ices_vms_2018_2021_aer_eco_fs.RData"))











##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# Compute rasters!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!(STANDALONE)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#




 RinputPath  <- file.path(getwd(), "INPUT_DATASETS") 
 ROutputPathToDatasets <- file.path(getwd(), "OUTCOME_DATASETS")
 RoutputPath4LargeFiles <- file.path("E:", "SeaWise")
 
 #----------------------
 # load
 load(file=file.path(ROutputPathToDatasets, "distr_allsp_from_AER_coupled_to_ICES_VMS_2018_2021.RData"))  # get distr_allsp i.e. all species aggregated (if AER)
 library(data.table)
  

 
 #----------------------
 # a quick and dirty plot
if(FALSE){ 
 an <- function (x) as.numeric(x)
 #a_var <- distr_allsp[, .(a_var_to_plot=sum(varcosts_in_ctry_level6_csquare)), by=c("Csquare", "SI_LATI", "SI_LONG")]
 a_var <- distr_allsp_fs2[distr_allsp_fs2$country_code=="PRT" , ] # in North Sea only
 a_var <- a_var[, .(a_var_to_plot=sum(lpue_csquare_vms_kgperfhour, na.rm=TRUE)), by=c("Csquare", "SI_LATI", "SI_LONG")]
 a_var <- a_var[ SI_LATI>30 &  SI_LATI<62 &  SI_LONG>-15  &  SI_LONG<10 , ] # in North Sea only

 a_var      <- as.data.frame(a_var)
 a_var$a_var_to_plot <- log(an(a_var$a_var_to_plot)) 
 a_var$a_var_to_plot <- (an(a_var$a_var_to_plot)) 

 cutbreakval            <- seq(1, 20, by=1)  # log(kilos)
 colyellowred           <- terrain.colors(length(cutbreakval))

 a_species              <- "SKJ"
 a_species              <- "HKE"
 if("species" %in% colnames(a_var)){
   cols                   <- c("white", colyellowred)[cut(unlist(an(a_var[a_var$species==a_species, "a_var_to_plot"])), breaks=cutbreakval)]
   } else{
    cols                   <- c("white", colyellowred)[cut(unlist(an(a_var[, "a_var_to_plot"])), breaks=cutbreakval)]
   }
   coord                  <- a_var[, c("SI_LONG", "SI_LATI")]
 plot(coord[,c("SI_LONG", "SI_LATI")], pch="")


# resy <-  diff(unique(a_var$SI_LATI)[order(unique(a_var$SI_LATI))])  [2]
# resx <-  diff(unique(a_var$SI_LONG)[order(unique(a_var$SI_LONG))])  [2]

 for (i in 1: nrow(coord)){
   rect(coord[i, "SI_LONG"]-0.05/2, coord[i,"SI_LATI"]-0.05/2, coord[i,"SI_LONG"]+0.05/2, coord[i,"SI_LATI"]+0.05/2, col=cols[i], border=FALSE)
 }
 library(maps)
 map(add=T)
 #=> looks good....
 # then proceed with an more advanced plotting:
}


# check NAs
unique(distr_allsp[is.na(lpue_csquare_vms_kgperfhour) & SI_LATI>50 &  SI_LATI<62 &  SI_LONG>-2  &  SI_LONG<10 , "key"]) # in North Sea only
distr_allsp[key=="2019_VL40XX_TM_27.4.a",]

                                           

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# get the data.table linked to a master raster grid
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

      
   

my_raster_export <- function(master_dt, slave_dt, effort_only=FALSE){     
  # create a grdtable
  # first create sf of a df
  library(sf)
  library(raster)
  library(terra)
  a_distr                       <- st_as_sf(x = slave_dt,  
                                          coords = c("SI_LONG", "SI_LATI"),
                                          crs = 4326) # EPSG:4326 is  WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS, see https://epsg.io/4326
  #a_distr                       <- st_transform(a_distr, 3035) # to lambert....but then adapt the rounding trick for grid alignement...

  xrange                        <- range(master_dt$SI_LONG, na.rm=TRUE)
  yrange                        <- range(master_dt$SI_LATI, na.rm=TRUE)
  resx                          <-  diff(unique(master_dt$SI_LONG)[order(unique(master_dt$SI_LONG))])  [3]  # master distr giving the bbox
  grd                           <- raster(xmn=plyr::round_any(xrange[1], resx, floor), xmx=plyr::round_any(xrange[2], resx, ceiling), ymn=plyr::round_any(yrange[1], resx, floor), ymx=plyr::round_any(yrange[2], resx, ceiling), res=resx, crs=CRS("+proj=longlat +datum=WGS84"))
  #grd                           <- rstr_totfishdays_2019
     
  values(grd)                   <- c(1:ncell(grd)) # overwrite with an index
  grdtable                      <- data.table(idx  = coordinates(grd)[,1],
                                            idy  = coordinates(grd)[,2],
                                            grID = values(grd))
  setkeyv(grdtable, c("idx", "idy")) # sort and provide a key
  
  grdtable$idx <- round(grdtable$idx, 3) # for Csquare at 0.05 degree res
  grdtable$idy <- round(grdtable$idy, 3)
  
  # align coord of the object of interest to coord of the grid
  # determine the raster cell-centroids of each distr record
  a                              <- data.table(st_coordinates(a_distr))
  a_distr$idx                    <- round(a$X,3)
  a_distr$idy                    <- round(a$Y,3)
 
  # make the object of interest a dt again, add grID
  a_distr                        <- data.table(st_drop_geometry(a_distr))
  setkeyv(a_distr, c("idx", "idy"))
  distr_with_grid                <- merge(grdtable, a_distr, by= c("idx", "idy")) # merge by c("idx", "idy")

  # a check
  all(unique(a_distr$idx) %in% unique(grdtable$idx)) # => should be TRUE
  all(unique(a_distr$idy) %in% unique(grdtable$idy)) # => should be TRUE
  unique(distr_with_grid[,grID]) # should return other than NA, otherwise correct the above rounding trick...
 
  # caution: do the average on the data with informed costs only
  if("varcosts_in_ctry_level6_csquare" %in% colnames(distr_with_grid)) distr_with_grid <- distr_with_grid[varcosts_in_ctry_level6_csquare!=0 & oth_non_var_costs_in_csquare!=0 ,]
  
  if(nrow(distr_with_grid)!=0){ # do nothing if no data left here...
  
  ## aggregate per grID
  some_cols_to_sweep <- c("FishingHour", "daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
             "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare",
          "other_income_in_csquare" ,"unpaid_labour_in_csquare", 
          "personnelcosts_in_ctry_level6_csquare",
           "KwFishingdays_aer_in_ctry_level6_csquare", "lpue_csquare_vms_kgperfhour")
  if(effort_only) some_cols_to_sweep <- c("FishingHour") 
  some_cols_to_sweep <- some_cols_to_sweep [some_cols_to_sweep %in% colnames(master_dt)]         

  nby_per_fs <- distr_with_grid[,.(nby=length(unique(year))),by=fs]
  distr_with_grid <- merge(distr_with_grid, nby_per_fs, by="fs")
  
  # do a sweep() in advance of the sum to come to result into an average
  distr_with_grid <- data.table(cbind.data.frame(
                                distr_with_grid[,c("idx", "idy",  "grID")], 
                                 sweep(distr_with_grid[, ..some_cols_to_sweep], 1, distr_with_grid[, nby], FUN="/")))  
  
  some_cols_to_sum <- c("FishingHour", "daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
             "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare",
          "other_income_in_csquare" ,"unpaid_labour_in_csquare", 
          "personnelcosts_in_ctry_level6_csquare",
           "KwFishingdays_aer_in_ctry_level6_csquare")
  if(effort_only) some_cols_to_sum <- c("FishingHour") 
  some_cols_to_sum <- some_cols_to_sum [some_cols_to_sum %in% colnames(master_dt)]         
distr_with_grid_1 <- NULL 
  if(length(some_cols_to_sum)>0) 
     {
     distr_with_grid_1 <- 
       distr_with_grid[,lapply(.SD, sum, na.rm=TRUE),
         .SDcols=some_cols_to_sum,
          keyby=c("idx", "idy",  "grID")]
     }
  some_cols_to_average <- c("lpue_csquare_vms_kgperfhour")
  some_cols_to_average <- some_cols_to_average [some_cols_to_average %in% colnames(master_dt)]         
  distr_with_grid_2 <- NULL
  if(length(some_cols_to_average)>0) 
     {
     distr_with_grid_2 <- 
        distr_with_grid[,lapply(.SD, mean, na.rm=TRUE),
         .SDcols=some_cols_to_average,
          keyby=c("idx", "idy",  "grID")]
     }
  distr_with_grid <- cbind(distr_with_grid_1, distr_with_grid_2[, -c(1:3)])


  
  # assign some values to the grid
  # and divide by nby if several years in input...
  for(a_var in colnames(distr_with_grid[,-(1:3)])){
     grdtable[[a_var]]                        <- unlist(c(distr_with_grid[,a_var, with=FALSE])) [match(grdtable$grID, distr_with_grid$grID)]  
  }
  
  
  
  setkeyv(grdtable, "grID")  # re-order


  ## create and export rasters if needed
  #for(i in c(4:ncol(grdtable))){
  #   clnm                        <- colnames(grdtable)[i]
  #   values(grd)                 <- grdtable[[i]]
  #   filename                    <- file.path(filepath, paste0(clnm, ".tif"))
  #   writeRaster(grd, filename=filename, overwrite=TRUE)
  #}
  ## export the grdtable
  #save(grdtable, file=file.path(filepath, "grdtable.RData"))
  

 # re-open all to get a terra::spatRaster for later use
 all_rast <- NULL
  for(i in c(4:ncol(grdtable))){
    library(terra)
    clnm                         <- colnames(grdtable)[i]
    values(grd)                  <- grdtable[[i]]
    names(grd)                   <- clnm
    if(!is.null(all_rast)){
       add(all_rast)              <- rast(grd) 
    } else {
      all_rast                    <- rast(grd) 
    }
  }
  writeRaster(all_rast, filename=file.path(filepath, "spatRaster.tif"), overwrite=TRUE)
  
  } else{
  cat(paste0(" No costs informed in the data to properly compute a GVA" ,"\n"))
  }
  # end checking if any years in the dataset with some costs informed
  
  
return()
}

     
#my_raster_export <- function(master_dt, slave_dt, nby, effort_only=FALSE){     
#  # create a grdtable
#  # first create sf of a df
#  library(sf)
#  library(raster)
#  library(terra)
#  a_distr                       <- st_as_sf(x = slave_dt,  
#                                          coords = c("SI_LONG", "SI_LATI"),
#                                          crs = 4326) # EPSG:4326 is  WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS, see https://epsg.io/4326
#  #a_distr                       <- st_transform(a_distr, 3035) # to lambert....but then adapt the rounding trick for grid alignement...
#
#  xrange                        <- range(master_dt$SI_LONG, na.rm=TRUE)
#  yrange                        <- range(master_dt$SI_LATI, na.rm=TRUE)
#  resx                          <-  diff(unique(master_dt$SI_LONG)[order(unique(master_dt$SI_LONG))])  [10]  # master distr giving the bbox
#  grd                           <- raster(xmn=plyr::round_any(xrange[1], resx, floor), xmx=plyr::round_any(xrange[2], resx, ceiling), ymn=plyr::round_any(yrange[1], resx, floor), ymx=plyr::round_any(yrange[2], resx, ceiling), res=resx, crs=CRS("+proj=longlat +datum=WGS84"))
#  #grd                           <- rstr_totfishdays_2019
#     
#  values(grd)                   <- c(1:ncell(grd)) # overwrite with an index
#  grdtable                      <- data.table(idx  = coordinates(grd)[,1],
#                                            idy  = coordinates(grd)[,2],
#                                            grID = values(grd))
#  setkeyv(grdtable, c("idx", "idy")) # sort and provide a key
#  
#  grdtable$idx <- round(grdtable$idx, 3) # for Csquare at 0.05 degree res
#  grdtable$idy <- round(grdtable$idy, 3)
#  
#  # align coord of the object of interest to coord of the grid
#  # determine the raster cell-centroids of each distr record
#  a                              <- data.table(st_coordinates(a_distr))
#  a_distr$idx                    <- round(a$X,3)
#  a_distr$idy                    <- round(a$Y,3)
# 
#  # make the object of interest a dt again, add grID
#  a_distr                        <- data.table(st_drop_geometry(a_distr))
#  setkeyv(a_distr, c("idx", "idy"))
#  distr_with_grid                <- merge(grdtable, a_distr, by= c("idx", "idy")) # merge by c("idx", "idy")
#
#  # a check
#  all(unique(a_distr$idx) %in% unique(grdtable$idx)) # => should be TRUE
#  all(unique(a_distr$idy) %in% unique(grdtable$idy)) # => should be TRUE
#  unique(distr_with_grid[,grID]) # should return other than NA, otherwise correct the above rounding trick...
# 
#  ## aggregate per grID
#  some_cols <- c("FishingHour", "daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
#        "varcosts_in_ctry_level6_csquare",  "oth_non_var_costs_in_csquare",
#          "other_income_in_csquare" ,"unpaid_labour_in_csquare", 
#          "personnelcosts_in_ctry_level6_csquare",
#           "KwFishingdays_aer_in_ctry_level6_csquare", "lpue_csquare_vms_kgperfhour")
# if(effort_only) some_cols <- c("FishingHour", "kWFishingHour") 
#  some_cols <- some_cols [some_cols %in% colnames(master_dt)]         
#  distr_with_grid_1 <- NULL
#  if(length(some_cols)>0) distr_with_grid_1 <- 
#     distr_with_grid[,lapply(.SD, sum, na.rm=TRUE),
#      .SDcols=some_cols,
#          keyby=c("idx", "idy",  "grID")]
#  some_cols <- c("lpue_csquare_aer_kgperdayatsea")
#  some_cols <- some_cols [some_cols %in% colnames(master_dt)]         
#  distr_with_grid_2 <- NULL
#  if(length(some_cols)>0) distr_with_grid_2 <- 
#     distr_with_grid[,lapply(.SD, mean, na.rm=TRUE),
#      .SDcols=some_cols,
#          keyby=c("idx", "idy",  "grID")]
#  distr_with_grid <- cbind(distr_with_grid_1, distr_with_grid_2[, -c(1:3)])
#
#  # assign some values to the grid
#  # and divide by nby if several years in input...
#  for(a_var in colnames(distr_with_grid[,-(1:3)])){
#     grdtable[[a_var]]                        <- unlist(c(distr_with_grid[,a_var, with=FALSE])) [match(grdtable$grID, distr_with_grid$grID)]  /nby
#  }
#  
#  
#  
#  setkeyv(grdtable, "grID")  # re-order
#
#
#  ## create and export rasters if needed
#  #for(i in c(4:ncol(grdtable))){
#  #   clnm                        <- colnames(grdtable)[i]
#  #   values(grd)                 <- grdtable[[i]]
#  #   filename                    <- file.path(filepath, paste0(clnm, ".tif"))
#  #   writeRaster(grd, filename=filename, overwrite=TRUE)
#  #}
#  ## export the grdtable
#  #save(grdtable, file=file.path(filepath, "grdtable.RData"))
#  
#
# # re-open all to get a terra::spatRaster for later use
# all_rast <- NULL
#  for(i in c(4:ncol(grdtable))){
#    library(terra)
#    clnm                         <- colnames(grdtable)[i]
#    values(grd)                  <- grdtable[[i]]
#    names(grd)                   <- clnm
#    if(!is.null(all_rast)){
#       add(all_rast)              <- rast(grd) 
#    } else {
#      all_rast                    <- rast(grd) 
#    }
#  }
#  writeRaster(all_rast, filename=file.path(filepath, "spatRaster.tif"), overwrite=TRUE)
#  
#  
#return()
#}

##------------------------------------
## CALLS------------------------------
  years <- c(2018:2021)
  library(terra)
  
 
  # first, plot VMS data
  load(file=file.path(ROutputPathToDatasets, "vms_2018_2021_in_NAO_before_merging.RData")) # vms_effort_nao
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_VMEs", "all_metiers", "2018_2021")  

  dir.create(filepath, recursive=TRUE)
  vms_effort_nao$fs <- c(vms_effort_nao$MetierL6)
  my_raster_export(master_dt=vms_effort_nao, slave_dt=vms_effort_nao, effort_only=TRUE)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$FishingHour[], na.rm=TRUE)
  
  a_width <- 3000 ;  a_height <- 4000
   tiff(filename=file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_VMEs", paste0("ICES_VMS_VMEs_log_of_FishingHour_mapped_for_2018_2021.tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
    par(mar=c(1,1,1,1))
    e<- ext(-16, 10, 30, 62)
    plot(log(crop(dd$FishingHour, e)))
  dev.off()

  
  
  # all metiers, all years
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs", "all_metiers", "2018_2021")  

  dir.create(filepath, recursive=TRUE)
  my_raster_export(master_dt=distr_allsp, slave_dt=distr_allsp)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$FishingHour[], na.rm=TRUE)
 
   
  # or all metiers, per y
  for (y in years)
  {
  cat(paste0(y, "\n"))
  distr_this_y <- distr_allsp[distr_allsp$year %in% y, ] # test on this year
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs", "all_metiers", y)  
  dir.create(filepath, recursive=TRUE)
  my_raster_export(master_dt=distr_allsp, slave_dt=distr_this_y)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(dd)
  } # end y

  # or per metier, all years
  a_df                 <- as.data.frame(distr_allsp)
  for (fs in unlist(unique(distr_allsp[,"fs"])))
  {
   cat(paste0(fs, "\n"))
     distr_this_fs        <- data.table(a_df[a_df$fs %in% fs,])
     #distr_this_fs_this_y <- distr_this_fs[distr_this_fs$year=="2019",]
     filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs", fs, "2018_2021")

     dir.create(filepath, recursive=TRUE)
     my_raster_export(master_dt=distr_allsp, slave_dt=distr_this_fs)
     # check: 
     dd <- rast(file.path(filepath, "spatRaster.tif"))
     plot(dd)
  }
     
 

  # or per metier, per y
  a_df                 <- as.data.frame(distr_allsp)
  for (y in years)
  {
  cat(paste0(y, "\n"))
  for (fs in unlist(unique(distr_allsp[,"fs"])))
  {
   cat(paste0(fs, "\n"))
     distr_this_fs        <- data.table(a_df[a_df$fs %in% fs,])
     distr_this_fs_this_y <- distr_this_fs[distr_this_fs$year==y,]
     filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs", fs, y)
     dir.create(filepath, recursive=TRUE)
     if(nrow(distr_this_fs_this_y)!=0){
        my_raster_export(master_dt=distr_allsp, slave_dt=distr_this_fs_this_y)
        # check: 
        #dd <- rast(file.path(filepath, "spatRaster.tif"))
       #plot(dd)
       }
  } # end fs
  } # end y
 
 # check    
 # =>    
 # the idea from there is to use these raster layers to displace effort per metier (based on VMS effort, LPUE and costs)
 #  and recompute the GVA (based on AER variables). Using AER var is required to make the estimates consistent with the AER report,
 # but is not a good idea to displace effort because VMS effort is better spatially disaggregated.... 
  # ...and then recompute the GVA
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs", "all_metiers", "2018_2021")
  dd <- rast(file.path(filepath, "spatRaster.tif")) # an average over years...
  dd$GVA <-  (an(dd$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(dd$value_aer_in_ctry_level6_csquare)/an(dd$landings_aer_in_ctry_level6_csquare))) + 
                      an(dd$other_income_in_csquare) - # plus other income
                      an(dd$unpaid_labour_in_csquare) - an(dd$varcosts_in_ctry_level6_csquare) - an(dd$oth_non_var_costs_in_csquare)  # minus var and fixed costs
 
  plot(dd, xlim=c(-10, 10), ylim=c(40,65))
  plot(log(dd$GVA), xlim=c(-10, 10), ylim=c(40,65))
















