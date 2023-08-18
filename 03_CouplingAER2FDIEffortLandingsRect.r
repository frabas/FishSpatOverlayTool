
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# MERGE STECF AER WITH STECF FDI Csquare (i.e. ICES Rectangle)
# AND DISPACH AER LANDINGS, EFFORT AND COSTS 
# ON  C-SQUARE (ICES RECTANGLE) CELLS PER YEAR, COUNTRY AND METIER LEVEL6
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
                                   .SDcols=c("value", "weight", "aerkwFishingdays", "aerkwFishingdaysDeepSea", "aerfishingdays", "aerdaysatsea"),
                                   by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length", "sub_reg")
                              ]
#caution: some combination of coutry-fishtech-vesselsize has several fs_name....  
dat[dat$year=="2020" & dat$country_code=="ESP" & dat$fishing_tech=="HOK" & dat$vessel_length=="VL2440" ,]

# output
agg_stecf_fleetdata_with_kiloeffort_nofs  <- dat



#-----------------------------------
#-----------------------------------
#-----------------------------------
##-------
####### formatting a AER costratios ####  
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

AERothvars[AERothvars$year=="2020" & AERothvars$country_code=="ESP" & AERothvars$fishing_tech=="HOK" & AERothvars$vessel_length=="VL2440" ,]


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

AER_nonvariablevars[AER_nonvariablevars$year=="2020" & AER_nonvariablevars$country_code=="ESP" & AER_nonvariablevars$fishing_tech=="HOK" & AER_nonvariablevars$vessel_length=="VL2440" ,]






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


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!""


# input
dat <- stecf_fleetdata_with_kiloandeffort_andcostratios


# dispatch the aerECOkwFishingdays variable over region. This var (as all AER_eco variables) is not regionalised 
tot <-  dat[,.(tot_aerECOkwFishingdays=sum(an(aerECOkwFishingdays)/1e9)), by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length")] 
dd  <- left_join(dat, tot) 
dd$share_aerECOkwFishingdays     <- (an(dd$aerECOkwFishingdays)/1e9) / an(dd$tot_aerECOkwFishingdays) # caution: a 1e9 rescaling used to avoid the too large numbers overflow
dd$aerECOkwFishingdays_perregion <- an(dd$share_aerECOkwFishingdays) * an(dd$aerECOkwFishingdays)  


AER_nonvariablevars[AER_nonvariablevars$year=="2020" & AER_nonvariablevars$country_code=="ESP" & AER_nonvariablevars$fishing_tech=="HOK" & AER_nonvariablevars$vessel_length=="VL2440" ,]


dd          <- left_join (dd, AER_nonvariablevars, by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length")) 
dd$varcosts <- dd$share_aerECOkwFishingdays * dd$aerECOkwFishingdays * (dd$enerbykwfishdy + dd$repbykwfishday + dd$varbykwfishday)  
#dd$fs_name <- dd$fs_name.x # a fix


# check numbers...
ddd <- dd[dd$year=="2019" & dd$country_code=="BEL" & dd$fishing_tech=="TBB" & dd$vessel_length=="VL2440",]
ddd <- dd[dd$country_code=="ESP" & dd$vessel_length=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018",]    
 ddd$GVA <- (an(ddd$weight) *  # landing kg  * price
                      (an(ddd$value)/an(ddd$weight))) + 
                      an(ddd$other_income) - # plus other income
                      an(ddd$unpaid_labour) - an(ddd$varcosts)  # minus var costs



dd[dd$year=="2019" & dd$country_code=="ESP" & dd$fishing_tech=="HOK" & dd$vessel_length=="VL2440" ,]


 
# output 
stecf_fleetdata_with_kiloandeffort_andcostratios_kwdispatched  <- dd [, .SD, .SDcols=c(colnames(stecf_fleetdata_with_kiloandeffort_andcostratios), "aerECOkwFishingdays_perregion")]

# check
dd <- stecf_fleetdata_with_kiloandeffort_andcostratios_kwdispatched
head(dd[year=="2019" & vessel_length=="VL0010" & fishing_tech=="DFN" &   sub_reg=="27.10.a",])
     
     
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
# READ STECF FDI DATA

# Disclaimer STECF FDI "Fisheries landings & effort: data by c-square" (https://data.jrc.ec.europa.eu/dataset/00ae6659-ddde-4314-a9da-717bb2e82582):
#The spatial dataset (years 2013-2021) published on this page is a subset of the data provided by EU Member States in the context of the DCF (Data Collection Framework) data call
#collecting Fisheries Dependent Information (FDI). The data provided by Member States during the 2022 FDI data call were analyzed by the Scientific, Technical and Economic Committee for Fisheries (STECF) Expert Working Group 22-10. Before accessing the data we strongly encourage to read the STECF report 22-10 (https://stecf.jrc.ec.europa.eu/reports). Disclaimer: Although the data published here have been assessed by STECF, the quality and completeness of the data are under the responsibility of the EU Member States.

# Note: no country info....(hence, the previous historical dataset set up during the “STECF fishing effort regime working groups” (not anymore updated since 2017) is still downloaded because contains the country information.
# Note: some record kept confidential in FDI, therefore there are some datagaps.....

#------------
# Read the FDI effort per rectangle
filename <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_effort_EU28.csv")
library(data.table)
fdi_effort_rect22 <- fread(filename, quote="\"")


# 2023 FDI data with confidential data and also adding the country info
filename <- file.path(RinputPath, "STECF_DATA","FDI_2023_data_adhoc","table_i.csv")
library(data.table)
fdi_effort_rect23 <- fread(filename, quote="\"")


# fdi_effort_rect23[ ,sum(totfishdays), by=c( "vessel_length", "supra_region", "confidential")]

 # check
 #fdi_effort_rect23[ year=="2019"   &       supra_region=="NAO" & fishing_tech =="DTS" &    vessel_length=="VL1824"  & sub_region =="27.4.A" & metier== "OTB_DEF_100-119_0_0" & icesname=="44E7" ,]


 # Caution: the Table I 2023 confidential does not come up as the the pulbic 2022 spatial_effort_EU28.csv which is already split per csquare...
 # Therfore, assign a cscode but caution to overwrite for ICES rectangle by splitting them into two c-squares
 dd <- as.data.frame(fdi_effort_rect23)
 dd$cscode          <- vmstools::CSquare(an(dd[,"rectangle_lon"]), an(dd[,"rectangle_lat"]), degrees=0.5)
 idx                       <- dd$c_square!="NA" & dd$rectangle_type=="NA" # if c_square already informed then use it!
 dd[idx, "cscode"]         <- dd[idx, "c_square"] # if c_square already informed then use it!
 dd[idx, "rectangle_type"] <- "05*05" # if c_square already informed then use it!
 dd$icesname        <- vmstools::ICESrectangle(data.frame(SI_LONG=an(dd$rectangle_lon), SI_LATI=an(dd$rectangle_lat)))
 ddd1               <- dd[dd$rectangle_type=="05*1",]
 ddd2               <- dd[dd$rectangle_type=="05*1",]
 ddd1$rectangle_lon2 <- an(ddd1$rectangle_lon) -0.25
 ddd2$rectangle_lon2 <- an(ddd2$rectangle_lon) +0.25
 ddd1$totfishdays   <- ddd1$totfishdays/2
 ddd2$totfishdays   <- ddd2$totfishdays/2
 ddd1$cscode        <- vmstools::CSquare(an(ddd1[,"rectangle_lon2"]), an(ddd1[,"rectangle_lat"]), degrees=0.5)
 ddd2$cscode        <- vmstools::CSquare(an(ddd2[,"rectangle_lon2"]), an(ddd2[,"rectangle_lat"]), degrees=0.5)
 ddd1               <- ddd1[,!colnames(ddd1) %in% "rectangle_lon2"]
 ddd2               <- ddd2[,!colnames(ddd2) %in% "rectangle_lon2"]
 res                <- rbind.data.frame(ddd1, ddd2)
 fdi_effort_rect    <- rbind.data.frame(dd[dd$rectangle_type!="05*1",], res)
 fdi_effort_rect    <- data.table (fdi_effort_rect)


 # check
 #fdi_effort_rect[ year=="2019"   &       supra_region=="NAO" & fishing_tech =="DTS" &    vessel_length=="VL1824"  & sub_region =="27.4.A" & metier== "OTB_DEF_100-119_0_0" & icesname=="44E7" ,]


# make the code conistent in case the FDI data are not having the country dimension informed
if(!"country_code"  %in% colnames(fdi_effort_rect)) fdi_effort_rect$country_code <- "ALL"


# a small fix and renaming
fdi_effort_rect$fditotfishdays <- as.numeric(gsub(",", ".", fdi_effort_rect$totfishdays))


# AER and FDI are compatible given fleet seg names 
 
# Retrieve the sub_reg? 
#library(vmstools)
#data(ICESareas) # in vmstools
#sq         <- cbind.data.frame(unique(fdi_effort_rect$cscode), vmstools::CSquare2LonLat(unique(fdi_effort_rect$cscode), 0.5))
#sq$idx     <- an(ICESarea(sq, ICESareas, fast=TRUE))
#sq         <- sq[!is.na(sq$idx),]
#dd         <- as.data.frame(ICESareas)
#sq$sub_reg <- dd[match(sq$idx+1,dd$OBJECTID),"Area_Full"]  # caution: +1
#colnames(sq) <- c("cscode", "lat_csq", "long_csq", "idx", "sub_reg")
#sq <- sq[,c("cscode", "sub_reg")]
## joining geo information to fdi data
#fdi_effort_rect <- dplyr::left_join(fdi_effort_rect, sq, by=c("cscode"))
# replaced by:
 fdi_effort_rect$sub_reg <- tolower(fdi_effort_rect$sub_region)
 
 
 # renaming
 colnames(fdi_effort_rect)[colnames(fdi_effort_rect)=="fshng_t"] <- "fishing_tech" 
 colnames(fdi_effort_rect)[colnames(fdi_effort_rect)=="vssl_ln"] <- "vessel_length" 
 colnames(fdi_effort_rect)[colnames(fdi_effort_rect)=="ger_typ"] <- "gear_type" 

 # a fix in naming countries
 dd <- as.data.frame(fdi_effort_rect)
 dd$country_code <- factor(dd$country_code)
 levels(dd$country_code)[levels(dd$country_code) %in% "ENG"] <- "GBR"
 levels(dd$country_code)[levels(dd$country_code) %in% "SCO"] <- "GBR"
 fdi_effort_rect <- data.table(fdi_effort_rect)


#------------
# Read the FDI landings per rectangle per year (tonnes, and euros) 
#(identical values for a given key is likely the outcome of an even dispatching over c-square done by EU MS before submitting data to the JRC...)
#(...here we don´t care as we aggregate back to the ICES rectangle level)
#filename <- file.path(getwd(), "ADVICES","STECF","newFDI","2021_FDI_data","spatial_effort_attribute_table.csv")

filename <- file.path(RinputPath, "STECF_DATA","FDI_2023_data_adhoc","table_h2018.csv")   # 2023 FDI Confidential years: 2013-2021
library(data.table)
fdi_land_rect_2018 <- fread(filename, quote="\"")

#filename <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_landings_2019_EU28.csv")   # years: 2013-2021
filename <- file.path(RinputPath, "STECF_DATA","FDI_2023_data_adhoc","table_h2019.csv")   # 2023 FDI Confidential years: 2013-2021
library(data.table)
fdi_land_rect_2019 <- fread(filename, quote="\"")

#filename <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_landings_2020_EU28.csv")
filename <- file.path(RinputPath, "STECF_DATA","FDI_2023_data_adhoc","table_h2020.csv")   # 2023 FDI Confidential years: 2013-2021
library(data.table)
fdi_land_rect_2020 <- fread(filename, quote="\"")

#filename <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_landings_2021_EU28.csv")
filename <- file.path(RinputPath, "STECF_DATA","FDI_2023_data_adhoc","table_h2021.csv")   # 2023 FDI Confidential years: 2013-2021
library(data.table)
fdi_land_rect_2021 <- fread(filename, quote="\"")



fdi_land_rect <- rbind(fdi_land_rect_2018,
                       fdi_land_rect_2019,
                       fdi_land_rect_2020,
                       fdi_land_rect_2021)

 # a fix in naming countries
 dd <- as.data.frame(fdi_land_rect)
 dd$country_code <- factor(dd$country_code)
 levels(dd$country_code)[levels(dd$country_code) %in% "ENG"] <- "GBR"
 levels(dd$country_code)[levels(dd$country_code) %in% "SCO"] <- "GBR"
 fdi_land_rect <- data.table(fdi_land_rect)
 
 

 # Caution: the Table H 2023 confidential does not come up as the public 2022 which is already split per csquare...
 # Therfore, assign a cscode but caution to overwrite for ICES rectangle by splitting them into two c-squares
 dd <- as.data.frame(fdi_land_rect)
 dd$cscode           <- vmstools::CSquare(an(dd[,"rectangle_lon"]), an(dd[,"rectangle_lat"]), degrees=0.5)
 idx                       <- dd$c_square!="NA" & dd$rectangle_type=="NA" # if c_square already informed then use it!
 dd[idx, "cscode"]         <- dd[idx, "c_square"] # if c_square already informed then use it!
 dd[idx, "rectangle_type"] <- "05*05" # if c_square already informed then use it!
 dd$icesname         <- vmstools::ICESrectangle(data.frame(SI_LONG=an(dd$rectangle_lon), SI_LATI=an(dd$rectangle_lat)))
 ddd1                <- dd[dd$rectangle_type=="05*1",]
 ddd2                <- dd[dd$rectangle_type=="05*1",]
 ddd1$rectangle_lon2 <- an(ddd1$rectangle_lon) -0.25
 ddd2$rectangle_lon2 <- an(ddd2$rectangle_lon) +0.25
 ddd1$totwghtlandg   <- ddd1$totwghtlandg/2
 ddd2$totwghtlandg   <- ddd2$totwghtlandg/2
 ddd1$totvallandg    <- ddd1$totvallandg/2
 ddd2$totvallandg    <- ddd2$totvallandg/2
 ddd1$cscode         <- vmstools::CSquare(an(ddd1[,"rectangle_lon2"]), an(ddd1[,"rectangle_lat"]), degrees=0.5)
 ddd2$cscode         <- vmstools::CSquare(an(ddd2[,"rectangle_lon2"]), an(ddd2[,"rectangle_lat"]), degrees=0.5)
 ddd1                <- ddd1[,!colnames(ddd1) %in% "rectangle_lon2"]
 ddd2                <- ddd2[,!colnames(ddd2) %in% "rectangle_lon2"]
 res                 <- rbind.data.frame(ddd1, ddd2)
 fdi_land_rect       <- rbind.data.frame(dd[dd$rectangle_type!="05*1",], res)
 fdi_land_rect       <- data.table (fdi_land_rect)





dim(fdi_land_rect[deep=="DEEP",])

##-------
##-------

# aggregate FDI for keeping main species only (with a percent threshold)
# AND get rid of the "year-quarter" dimension
an <- function (x) as.numeric(as.character(x))
main_species <- fdi_land_rect[, .(fditotwghtlandg = sum(an(totwghtlandg), na.rm=T),
                                                 fditotvallandg = sum(an(totvallandg), na.rm=T)),
                                             by=c("species")]   [order(-fditotvallandg)] 
main_species$percent <- cumsum(an(main_species$fditotwghtlandg)/sum(an(main_species$fditotwghtlandg)))*100                                             
#oth_species <- unique(unlist(c(main_species[main_species$percent>98, "species"])))
oth_species <- unique(unlist(c(main_species[main_species$percent>50, "species"])))

fdi_land_rect[fdi_land_rect$species %in% oth_species, "species"] <- "OTH" 


fdi_land_rect_mainsp <- fdi_land_rect[, .(fditotvallandg = sum(an(totvallandg), na.rm=T),
                     fditotwghtlandg = sum(an(totwghtlandg), na.rm=T)),
                    by=c("year", "country_code", "supra_region","fishing_tech","vessel_length","sub_region", "metier","species", "rectangle_type", "icesname","cscode", "rectangle_lon", "rectangle_lat")]
                   

fdi_land_rect_mainsp$sub_reg <- tolower(fdi_land_rect_mainsp$sub_reg)





 # interlude to show that STECF FDI landings Rect is at the ICES rectangle resolution
 if(FALSE){
  # retrieve the coord
 library(vmstools)

 a_var <- fdi_land_rect_mainsp[, .(landings_in_cell=sum(fditotwghtlandg)), by=c("cscode", "rectangle_lat", "rectangle_lon", "species")]
 a_var      <- as.data.frame(a_var)
 

 resy <- 0.5
 resx <- 1
 cutbreakval            <- c(-1,0,20,40,80,160,320,3000000)  # kilos
 colyellowred           <- terrain.colors(length(cutbreakval))

 a_species              <- "HER"
 cols                   <- c("white", colyellowred)[cut(unlist(an(a_var[a_var$species==a_species, "landings_in_cell"])), breaks=cutbreakval)]
 coord                  <- a_var[a_var$species==a_species, c("rectangle_lon", "rectangle_lat")]
 plot(coord, pch="")
 an <- function(x) as.numeric(as.character(x))
 for (i in 1: nrow(coord)) rect(an(coord[i, "rectangle_lon"])-resx/2, an(coord[i,"rectangle_lat"])-resy/2, an(coord[i,"rectangle_lon"])+resx/2, an(coord[i,"rectangle_lat"])+resy/2, col=cols[i], border=FALSE)
 #=> looks quite a coarse resolution (i.e. ICES rect)
}
 



 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 #------------
 # merge FDI effort rect with FDI Landings rect
 # but first remove the "quarter" dimension:
 fdi_effort_rect_annual <- fdi_effort_rect[, .(fditotfishdays = sum(an(fditotfishdays), na.rm=T)),
                    by=c("year","country_code","supra_region","fishing_tech","vessel_length","sub_region", "metier","rectangle_type", "icesname", "cscode", "rectangle_lon", "rectangle_lat")]
                   
 fdi_land_effort_rect_mainsp <- left_join(fdi_land_rect_mainsp, fdi_effort_rect_annual, 
                 by=c("year", "country_code","supra_region", "fishing_tech", "vessel_length",  "metier", "icesname", "cscode", "rectangle_type", "rectangle_lon", "rectangle_lat"))
                
 
 # caution however: the effort is repeated alongside the species dimension....so the sum of effort from now onwards are imflated because of this....
   dd <- fdi_effort_rect_annual[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_region=="27.8.B",]
   dd[,sum(fditotfishdays, na.rm=TRUE),] 
   ddd <- fdi_land_effort_rect_mainsp[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_region.x=="27.8.B",]
   ddd[,sum(fditotfishdays, na.rm=TRUE),] 
  
 
 #=> Effort, Landings per SPecies in C-square....with such a dataset it is possible to compute percentage of landing value & volume in any kind of polygons and LPUEs for reallocation....
 # However we need to couple with AER economic info:

 
 
 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 
 # forsee the merging consistency
    
 dd                                                          <- stecf_fleetdata_with_kiloandeffort_andcostratios_kwdispatched  # AER 2017-2021
 dd2                                                         <- fdi_land_effort_rect_mainsp[fdi_land_effort_rect_mainsp$year %in% c("2018", "2019", "2020", "2021"),]
 
 # inconsistent sub_reg definition found with 
 unique(dd$sub_reg[!dd$sub_reg %in% dd2$sub_reg]) # aer reg not in fdi reg
 unique(dd2$sub_reg[!dd2$sub_reg %in% dd$sub_reg])
 # and a close match searched by hand with e.g.:
 # unique(dd$sub_reg)[grep("27.3", unique(dd$sub_reg))]
 # unique(dd2$sub_reg)[grep("34.1", unique(dd2$sub_reg))]
 

 
  # fix on the AER side
  # 1. fix for GSA coding in AER not compatible with FDI
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
   
   dd[dd$sub_reg %in% c("41.3.n"), "sub_reg"] <- "41.3"
   dd[dd$sub_reg %in% c("41.2"), "sub_reg"] <- "41.2.1"
   dd[dd$sub_reg %in% c("47.d.1"), "sub_reg"] <- "47.d"
   dd[dd$sub_reg %in% c("27.4"), "sub_reg"] <- "27.4.a"
   dd[dd$sub_reg %in% c("21"), "sub_reg"] <- "21.1.b"
   dd[dd$sub_reg %in% c("18"), "sub_reg"] <- "gsa18"
   dd[dd$sub_reg %in% c("27.8.d.2"), "sub_reg"] <- "27.8.d"
   dd[dd$sub_reg %in% c("41.3"), "sub_reg"] <- "41.3.1"
   dd[dd$sub_reg %in% c("34"), "sub_reg"] <- "34.1.3"
   dd[dd$sub_reg %in% c("47"), "sub_reg"] <- "47.a.0"
   dd[dd$sub_reg %in% c("27.7"), "sub_reg"] <- "27.7.e"
   dd[dd$sub_reg %in% c("27.7"), "sub_reg"] <- "27.7.e"
   dd[dd$sub_reg %in% c("57.3"), "sub_reg"] <- "57"
   dd[dd$sub_reg %in% c("27.14.b.2"), "sub_reg"] <- "27.14.b"
   dd[dd$sub_reg %in% c("21.4"), "sub_reg"] <- "21.4.v"
   dd[dd$sub_reg %in% c("21.4.x"), "sub_reg"] <- "21.4.v"
   dd[dd$sub_reg %in% c("58.4.1"), "sub_reg"] <- "21.4.v"
 
 
  # fix on the FDI side
   dd2[dd2$sub_reg %in% c("27.3.a.20", "27.3.a.21"), "sub_reg"] <- "27.3.a"
   dd2[dd2$sub_reg %in% c("21.3m"),"sub_reg"] <- "27.3.m"
   dd2[dd2$sub_reg %in% c("21.6h"),"sub_reg"] <- "21.6.h" 
   dd2[dd2$sub_reg %in% c("21.1c"),"sub_reg"] <- "21.1.c" 
   dd2[dd2$sub_reg %in% c("21.3k"),"sub_reg"] <- "21.3.k" 
   dd2[dd2$sub_reg %in% c("21.1d"),"sub_reg"] <- "21.1.d" 
   dd2[dd2$sub_reg %in% c("57.1"),"sub_reg"] <- "57" 
   dd2[dd2$sub_reg %in% c("47.a"),"sub_reg"] <- "47.a.1" 
   dd2[dd2$sub_reg %in% c("47.1"),"sub_reg"] <- "47.1.1" 
   dd2[dd2$sub_reg %in% c("47"),"sub_reg"] <- "47.a.0" 
   dd2[dd2$sub_reg %in% c("47.b"),"sub_reg"] <- "47.b.0" 
   dd2[dd2$sub_reg %in% c("21.3n"),"sub_reg"] <- "21.3.n" 
   dd2[dd2$sub_reg %in% c("21.6g"),"sub_reg"] <- "21.6.g" 
   dd2[dd2$sub_reg %in% c("21.4v"),"sub_reg"] <- "21.4.v" 
   dd2[dd2$sub_reg %in% c("21.3l"),"sub_reg"] <- "21.3.l" 
   dd2[dd2$sub_reg %in% c("21.3o"),"sub_reg"] <- "21.3.o" 
   dd2[dd2$sub_reg %in% c("21.1b"),"sub_reg"] <- "21.1.b" 
   dd2[dd2$sub_reg %in% c("41.2"),"sub_reg"] <- "41.2.1" 
   dd2[dd2$sub_reg %in% c("47.c"),"sub_reg"] <- "47.c.0" 
   dd2[dd2$sub_reg %in% c("21.6f"),"sub_reg"] <- "21.6.f"                                   
   dd2[dd2$sub_reg %in% c("87.1"),"sub_reg"] <- "87.1.4" 
   dd2[dd2$sub_reg %in% c("87.2"),"sub_reg"] <- "87.2.6" 
   dd2[dd2$sub_reg %in% c("41.3"),"sub_reg"] <- "41.3.1" 
   dd2[dd2$sub_reg %in% c("58.4"),"sub_reg"] <- "58.4.1" 
   dd2[dd2$sub_reg %in% c("21.1a"),"sub_reg"] <- "21.1.a" 
   dd2[dd2$sub_reg %in% c("gsa28"),"sub_reg"] <- "gsa29" 
   dd2[dd2$sub_reg %in% c("21.4w"),"sub_reg"] <- "21.4.w" 
   dd2[dd2$sub_reg %in% c("21.4x"),"sub_reg"] <- "21.4.x" 
   dd2[dd2$sub_reg %in% c("61"),"sub_reg"] <- "27.8.d.2" 
   dd2[dd2$sub_reg %in% c("47.d"),"sub_reg"] <- "47.d.1" 
   dd2[dd2$sub_reg %in% c("87.3"),"sub_reg"] <- "87" 
 
 
 
 
  aer_key_met                                                 <- paste(dd$year, dd$fishing_tech, dd$vessel_length, dd$sub_reg, sep="_") # aer
  fdi_key_met                                                 <- paste(dd2$year, dd2$fishing_tech, dd2$vessel_length, dd2$sub_reg, sep="_") # fdi
  not_in_aer_keys <- fdi_key_met[!fdi_key_met %in% aer_key_met]
  not_in_aer <- unique(not_in_aer_keys)
  not_in_fdi_keys <- aer_key_met[!aer_key_met %in% fdi_key_met]
  not_in_fdi <- unique(not_in_fdi_keys)

  # one example of unmatched seg:
  dd[dd$year=="2018" & dd$fishing_tech=="DFN" & dd$vessel_length=="VL0612" & dd$sub_reg=="gsa5",]  # aer
  dd2[dd2$year=="2018" & dd2$fishing_tech=="DFN" & dd2$vessel_length=="VL0612" & dd2$sub_reg=="gsa5",]  # fdi
  dd2[dd2$year=="2018" & dd2$fishing_tech=="DFN" & dd2$vessel_length=="VL0612",]
  
  
  # output
  stecf_fleetdata_with_kiloandeffort_andcostratios  <- data.table(dd) # 2017-2021
  fdi_land_effort_rect_mainsp                              <-  data.table(dd2)   # 2019-2021


  # check 
 dd <- stecf_fleetdata_with_kiloandeffort_andcostratios
  #"2019_VL0010_DFN_27.10.a" 
 head(dd[year=="2019" & vessel_length=="VL0010" & fishing_tech=="DFN" &   sub_reg=="27.10.a",])

  # check
  dd <- fdi_land_effort_rect_mainsp[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
  dd[,sum(fditotfishdays, na.rm=TRUE),] 
  

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# subset for the region of interest....
# HERE THE NAO:
  stecf_fleetdata_with_kiloandeffort_andcostratios_nao  <- 
   stecf_fleetdata_with_kiloandeffort_andcostratios[stecf_fleetdata_with_kiloandeffort_andcostratios$supra_reg=="NAO",]
 
  # check numbers
  dd  <- stecf_fleetdata_with_kiloandeffort_andcostratios_nao[stecf_fleetdata_with_kiloandeffort_andcostratios_nao$supra_reg=="NAO",]
  ddd <- dd[dd$country_code=="ESP" & dd$vessel_length=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018",]    
  sum(ddd$weight) 

 
 
  fdi_land_effort_rect_mainsp_nao   <-  
   fdi_land_effort_rect_mainsp[supra_region=="NAO",]



  # check
  dd <- fdi_land_effort_rect_mainsp_nao[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
  dd[,sum(fditotfishdays, na.rm=TRUE),] 
  
  dd <- fdi_land_effort_rect_mainsp_nao[country_code=="PRT" & fishing_tech=="HOK" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.9.a",]
  dd[,sum(fditotfishdays, na.rm=TRUE),] 
 
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# aggregate a bit to remove the quarter dim and to keep the essential columns only...
 fdi_land_effort_rect_mainsp_nao_annual <- fdi_land_effort_rect_mainsp_nao[, .(
                                                               fditotwghtlandg = sum(an(fditotwghtlandg), na.rm=T),
                                                                 fditotvallandg = sum(an(fditotvallandg), na.rm=T),
                                                                  fditotfishdays = sum(an(fditotfishdays), na.rm=T)),
                                             by=c("year", "country_code", "vessel_length", "fishing_tech", "sub_reg", "metier", "species", "rectangle_type", "icesname", "cscode")]  

 
 
 # check
 dd <- fdi_land_effort_rect_mainsp_nao_annual[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
 dd[,sum(fditotfishdays, na.rm=TRUE),] 

 
 head(fdi_land_effort_rect_mainsp_nao_annual)
 head(fdi_effort_rect[fdi_effort_rect$year=="2019" & fdi_effort_rect$vessel_length=="VL0010" & fdi_effort_rect$fishing_tech=="MGO" &   fdi_effort_rect$metier=="GND_DEF_80-99_0_0",])
 head(fdi_land_rect[fdi_land_rect$year=="2019" & fdi_land_rect$vessel_length=="VL0010" & fdi_land_rect$fishing_tech=="MGO" &   fdi_land_rect$metier=="GND_DEF_80-99_0_0",])

 head(fdi_effort_rect[fdi_effort_rect$year=="2019" & fdi_effort_rect$vessel_length=="VL1824" & fdi_effort_rect$fishing_tech=="DTS" &   fdi_effort_rect$metier=="OTT_CRU_100-119_0_0",])
 head(fdi_land_rect[fdi_land_rect$year=="2019" & fdi_land_rect$vessel_length=="VL1824" & fdi_land_rect$fishing_tech=="DTS" &   fdi_land_rect$metier=="OTT_CRU_100-119_0_0",])
 #=> we can already see that there was an even split over c-square within ices rectangle that has been done to produce these data..... 
 # so, in the NAO,  those data are really resolved at the ices rectangle data.


 dd <- fdi_land_effort_rect_mainsp_nao_annual
  "2019_VL0010_DFN_27.10.a" 
 head(dd[year=="2019" & vessel_length=="VL0010" & fishing_tech=="DFN" &   sub_reg=="27.10.a",])
 head(dd[year=="2019" & vessel_length=="VL2440" & fishing_tech=="HOK" &   sub_reg=="27.4.b",])



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# adding a coding to FDI csquare for a depth zone

# According to the FDI data call specification, spatial data on landings and effort (Tables H and I) must be submitted using one of the following notations:
# C-square code at 0.5x0.5 degree resolution, or:
# Latitude and longitude of the center of the rectangle together and its dimensions in decimal degrees:
# 0.5*0.5, corresponding to a c-square,
# 0.5*1, corresponding to an ICES rectangle,
# 1*1 for ICCAT squares,
# 5*5 for IOTC squares.

# retrieve the coord
 library(vmstools)
 library(dplyr)
 dd         <- fdi_land_effort_rect_mainsp_nao_annual[fdi_land_effort_rect_mainsp_nao_annual$rectangle_type=="05*1",] # ices rectangle
 sq         <- cbind.data.frame(cscode=unique(dd$cscode), vmstools::CSquare2LonLat(unique(dd$cscode), 0.5))
 dd         <- dplyr::left_join(dd, sq, by="cscode")
 dd1         <- fdi_land_effort_rect_mainsp_nao_annual[fdi_land_effort_rect_mainsp_nao_annual$rectangle_type=="05*05",]
 sq         <- cbind.data.frame(cscode=unique(dd1$cscode), vmstools::CSquare2LonLat(unique(dd1$cscode), 0.5))
 dd1         <- dplyr::left_join(dd1, sq, by="cscode")
 dd2         <- fdi_land_effort_rect_mainsp_nao_annual[fdi_land_effort_rect_mainsp_nao_annual$rectangle_type=="1*1",]
 if(nrow(dd2)!=0){
   sq         <- cbind.data.frame(cscode=unique(dd2$cscode), vmstools::CSquare2LonLat(unique(dd2$cscode), 1))
   dd2         <- dplyr::left_join(dd2, sq, by="cscode")
 }
 dd3         <- fdi_land_effort_rect_mainsp_nao_annual[fdi_land_effort_rect_mainsp_nao_annual$rectangle_type=="5*5",]
 if(nrow(dd3)!=0){
    sq         <- cbind.data.frame(cscode=unique(dd3$cscode), vmstools::CSquare2LonLat(unique(dd3$cscode), 5))
    dd3         <- dplyr::left_join(dd3, sq, by="cscode")
 }
fdi_land_effort_rect_mainsp_nao_annual_and_depths      <- rbind(dd, dd1)


# do an extract
library(raster)
coords      <- fdi_land_effort_rect_mainsp_nao_annual_and_depths[, c("SI_LONG","SI_LATI")]
bathy       <- raster(file.path(getwd(), "INPUT_SPATIAL_LAYERS", "GEBCO_May_2023","gebco.tif"))
id.cells    <- extract(bathy, SpatialPoints(coords), cellnumbers=TRUE)

fdi_land_effort_rect_mainsp_nao_annual_and_depths$in_400_800_zone <- id.cells[,2] # NA if not in the 400-800m

# CAUTION: OVERWRITE FOR NOW: ASSUMING NO EFFECT OF BATHYMETRY:
#fdi_land_effort_rect_mainsp_nao_annual_and_depths$in_400_800_zone <- 1
  

 
 # check
 dd <- fdi_land_effort_rect_mainsp_nao_annual_and_depths[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
 dd[,sum(fditotfishdays, na.rm=TRUE),] 




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##



years <- as.character(2018:2021) 
# caution: merging for 2021 is leaking, likely because incomplete AER data (if AER 2022 is used)




 # add primary keys before merging - adapt the key on the fly to avoid loosing effort or landings....
 
   # ROBUST MERGING: catch for leaks and adapt the key on the fly 
   aer_all_y <- NULL
   fdi_all_y <- NULL
   aer_y_leftover <- NULL
   aer_y_leftover2 <- NULL
   aer_y_leftover3 <- NULL
   fdi_y_leftover <- NULL
   fdi_y_leftover2 <- NULL
   fdi_y_leftover3 <- NULL
   tracked_leaks <- NULL
   for(y in years)
   {
     # Add a default key (i.e. year-quarter-Level5-icesrect)
      fdi_y                   <- fdi_land_effort_rect_mainsp_nao_annual_and_depths[year==y,]
      fdi_y$key               <- paste(fdi_y$year, fdi_y$country_code, fdi_y$vessel_length, fdi_y$fishing_tech, fdi_y$sub_reg, sep="_")  # full key on fdi
      aer_y                   <- stecf_fleetdata_with_kiloandeffort_andcostratios_nao[year==y,]
      aer_y$key               <- paste(aer_y$year, aer_y$country_code, aer_y$vessel_length, aer_y$fishing_tech, aer_y$sub_reg, sep="_") # full key on aer
      aer_y$chunk <- 0
      fdi_y$chunk <- 0
      m1                      <- unique(fdi_y$key)
      m2                      <- unique(aer_y$key)
     fdi_y_met_not_in_aer     <- m1[!m1 %in% m2]
     fdi_y_met_in_aer         <- m1[m1 %in% m2]
     aer_y_leftover           <- aer_y[!aer_y$key %in% fdi_y_met_in_aer,]
     fdi_y_leftover           <- fdi_y[!fdi_y$key %in% fdi_y_met_in_aer,]
     aer_y_main               <- aer_y[aer_y$key %in% fdi_y_met_in_aer,]
     fdi_y_main               <- fdi_y[fdi_y$key %in% fdi_y_met_in_aer,]
     aer_y_main$chunk       <- 1 # coding
     fdi_y_main$chunk       <- 1  # coding
     # repeat with a less constraining key  to catch the left-over records:
     if(nrow(aer_y_leftover)!=0 && nrow(fdi_y_leftover)!=0)
     {
      aer_y_leftover$key       <- paste(aer_y_leftover$year, aer_y_leftover$country_code, aer_y_leftover$vessel_length, aer_y_leftover$fishing_tech,  sep="_") # aer
      fdi_y_leftover$key       <- paste(fdi_y_leftover$year, fdi_y_leftover$country_code, fdi_y_leftover$vessel_length, fdi_y_leftover$fishing_tech, sep="_")  # fdi
      m1                       <- unique(fdi_y_leftover$key)
      m2                       <- unique(aer_y_leftover$key)
      fdi_y_met_not_in_aer     <- m1[!m1 %in% m2]
      fdi_y_met_in_aer         <- m1[m1 %in% m2]
      aer_y_leftover2          <- aer_y_leftover[!aer_y_leftover$key %in% fdi_y_met_in_aer,]
      fdi_y_leftover2          <- fdi_y_leftover[!fdi_y_leftover$key %in% fdi_y_met_in_aer,]
      aer_y_leftover           <- aer_y_leftover[aer_y_leftover$key %in% fdi_y_met_in_aer,]
      fdi_y_leftover           <- fdi_y_leftover[fdi_y_leftover$key %in% fdi_y_met_in_aer,]
      # destroy the sub_reg dim in aer
      aer_y_leftover_agg1 <- aer_y_leftover[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("value", "weight", "aerkwFishingdays", "aerfishingdays", "aerdaysatsea", "aerECOkwFishingdays", "aerECOkwFishingdays_perregion"),
                                   by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length", "key")]
      aer_y_leftover_agg2 <- aer_y_leftover[,lapply(.SD, mean, na.rm=TRUE), 
                                   .SDcols=c("enerbykwfishdy","wagebyinc","repbykwfishday","varbykwfishday"),
                                   by=c("year", "supra_reg", "country_code", "fishing_tech", "vessel_length", "key")]
      aer_y_leftover      <- cbind(aer_y_leftover_agg1, aer_y_leftover_agg2[,-c(1:6)])
      aer_y_leftover$chunk       <- 2 # coding
      #destroy the sub_reg dim in FDI
      fdi_y_leftover      <- fdi_y_leftover[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("fditotfishdays", "fditotwghtlandg"),
                                   by=c("cscode", "icesname", "SI_LATI", "SI_LONG", "metier", "vessel_length", "fishing_tech", "year", "country_code", "key")]    
      fdi_y_leftover$chunk <- 2  # coding      
      if(nrow(aer_y_leftover2))  warning("First attempt: There aer records left here...")
      if(nrow(fdi_y_leftover2)) warning("First attempt: There vms records left here...")
    
       # repeat with a less constraining key  to catch the left-over records:
      if(nrow(aer_y_leftover2)!=0 && nrow(fdi_y_leftover2)!=0){
        aer_y_leftover2$key       <- paste(aer_y_leftover2$year, aer_y_leftover2$vessel_length,  sep="_") # aer
        fdi_y_leftover2$key       <- paste(fdi_y_leftover2$year, fdi_y_leftover2$vessel_length,  sep="_")  # fdi
        aer_y_leftover2$chunk       <- 3 # coding
        fdi_y_leftover2$chunk       <- 3  # coding
        m1                       <- unique(fdi_y_leftover2$key)
        m2                       <- unique(aer_y_leftover2$key)
        fdi_y_met_not_in_aer     <- m1[!m1 %in% m2]
        fdi_y_met_in_aer         <- m1[m1 %in% m2]
        aer_y_leftover3          <- aer_y_leftover2[!aer_y_leftover2$key %in% fdi_y_met_in_aer,]
        fdi_y_leftover3          <- fdi_y_leftover2[!fdi_y_leftover2$key %in% fdi_y_met_in_aer,]
        aer_y_leftover2           <- aer_y_leftover2[aer_y_leftover2$key %in% fdi_y_met_in_aer,]
        fdi_y_leftover2           <- fdi_y_leftover2[fdi_y_leftover2$key %in% fdi_y_met_in_aer,]
        # destroy the sub_reg and fishing_tech and the country code dims in aer
        aer_y_leftover2_agg1 <- aer_y_leftover2[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("value", "weight", "aerkwFishingdays", "aerfishingdays", "aerdaysatsea", "aerECOkwFishingdays", "aerECOkwFishingdays_perregion"),
                                   by=c("year", "supra_reg", "vessel_length", "key")]
        aer_y_leftover2_agg2 <- aer_y_leftover2[,lapply(.SD, mean, na.rm=TRUE), 
                                   .SDcols=c("enerbykwfishdy","wagebyinc","repbykwfishday","varbykwfishday"),
                                   by=c("year", "supra_reg",  "vessel_length", "key")]
        aer_y_leftover2      <- cbind(aer_y_leftover2_agg1, aer_y_leftover2_agg2[,-c(1:6)])
        aer_y_leftover2$chunk       <- 3 # coding
        # destroy the sub_reg and fishing_tech and the country_code dims in FDI
        fdi_y_leftover2      <- fdi_y_leftover2[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("fditotfishdays", "fditotwghtlandg"),
                                   by=c("cscode", "icesname", "SI_LATI", "SI_LONG", "metier", "vessel_length", "year", "key")]    
        fdi_y_leftover2$chunk <- 3  # coding      
        if(nrow(aer_y_leftover3))  warning("Second attempt: There aer records left here...")  # lost
        if(nrow(fdi_y_leftover3)) warning("Second attempt: There fdi records left here...")   # lost
      }
     } 
    
    
    # bind # fill=TRUE to add missing columns and fill out with NAs 
    aer_left <- rbind(aer_y_main, aer_y_leftover, aer_y_leftover2, fill=TRUE)   
    fdi_left <- rbind(fdi_y_main, fdi_y_leftover, fdi_y_leftover2, fill=TRUE)
   
   # document effort leak this y  
   d <- function(x) as.data.frame(x)
   dd1 <-rbind.data.frame(
   "init"=d(fdi_y[  ,.(fditotfishdays=sum(fditotfishdays )),]), # initial tot eff fdi met in fdi
   "year, country, vessel_length, fishing_tech, sub_reg"=d(fdi_y_main[  ,.(fditotfishdays =sum(fditotfishdays )),]),        #  year, vessel_length, fishing_tech, sub_reg  # full key 
   "year, country, vessel_length, fishing_tech"=d(fdi_y_leftover[  ,.(fditotfishdays =sum(fditotfishdays )),]),    #  year, vessel_length, fishing_tech
   "year, vessel_length"=d(fdi_y_leftover2[  ,.(fditotfishdays =sum(fditotfishdays )),]),   #  year, vessel_length
   "unfortunate lost"=d(fdi_y_leftover3[  ,.(fditotfishdays =sum(fditotfishdays )),]),   #  the remaining: not matched...
   "finally left"=d(fdi_left[  ,.(fditotfishdays =sum(fditotfishdays )),]) # tot eff fdi left
   )

   # document landings leak this y
   d <- function(x) as.data.frame(x)
   dd2 <-rbind.data.frame(
   "init"=d(fdi_y[  ,.(fditotwghtlandg=sum(an(fditotwghtlandg))),]), # initial tot in fdi
   "year, vessel_length, fishing_tech, sub_reg"=d(fdi_y_main[  ,.(fditotwghtlandg=sum(an(fditotwghtlandg))),]),        #  year, vessel_length, fishing_tech, sub_reg  # full key  
   "year, vessel_length, fishing_tech"=d(fdi_y_leftover[  ,.(fditotwghtlandg=sum(an(fditotwghtlandg))),]),    #  year, vessel_length, fishing_tech 
   "year, vessel_length"=d(fdi_y_leftover2[  ,.(fditotwghtlandg=sum(an(fditotwghtlandg))),]),   #  year, vessel_length 
   "unfortunate lost"=d(fdi_y_leftover3[  ,.(fditotwghtlandg=sum(an(fditotwghtlandg))),]),   #  the remaining: not matched... 
   "finally left"=d(fdi_left[  ,.(fditotwghtlandg=sum(an(fditotwghtlandg))),]) # tot in fdi left
   )
   
   tracked_leaks <- rbind.data.frame (tracked_leaks, cbind.data.frame(y, dd1, dd2))
   
   
   aer_all_y <- rbind(aer_all_y, aer_left)
   fdi_all_y <- rbind(fdi_all_y, fdi_left)
   
   } # end y
   
   
# check what is left, what is lost...   
print(tracked_leaks)
 
 # check
 dd <- fdi_all_y[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
 dd[,sum(fditotfishdays, na.rm=TRUE),] 

 dd <- fdi_all_y[country_code=="PRT" & fishing_tech=="HOK" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.9.a",]
 dd[,sum(fditotfishdays, na.rm=TRUE),] 

 
# check what is left, what is lost...   
 print(tracked_leaks)
 dd <- knitr::kable(as.data.frame(tracked_leaks), format = "html")
 library(readr)
 readr::write_file(dd, file.path(ROutputPathToDatasets, "tracked_leaks_AER_to_FDI.html")) 

 
# check 
fdi_all_y[, .(fishingdays = sum(an(fditotfishdays), na.rm=T),
                                                 fdifishdays = sum(an(fditotfishdays), na.rm=T)),
                                             by=c("year")]  
fdi_all_y[, .(totvallandg = sum(an(fditotvallandg), na.rm=T),
                                                 fditotwghtlandg = sum(an(fditotwghtlandg), na.rm=T)),  # in tonnes
                                             by=c("year")]   
fdi_all_y[, .(totvallandg = sum(an(fditotvallandg), na.rm=T),
                                                 fditotwghtlandg = sum(an(fditotwghtlandg), na.rm=T)),
                                             by=c("year", "chunk")]    
    
 # check numbers
 dd  <- aer_all_y[aer_all_y$supra_reg=="NAO",]
 ddd <- dd[dd$country_code=="ESP" & dd$vessel_length=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018",]    
 sum(ddd$weight) 

 
 #-----------------------------------
 #-----------------------------------
 #-----------------------------------


 # calculate share of countries on effort (NOT USED WHEN FDI CONFIDENTIAL DATA ARE USED BECAUSE THE COUNTRY IS INFORMED)
 #aa <- data.table(aer_all_y)
 #agg_kwfdays <- aa[,.(tot_kwFishingdays=sum(aerECOkwFishingdays_perregion)),by=c("year", "key")]
 #aer_all_y <- merge(aer_all_y, agg_kwfdays,
 #                 by= c("year", "key"))
 #
 #aer_all_y$country_share_effort <- 
 #      aer_all_y$aerECOkwFishingdays_perregion/aer_all_y$tot_kwFishingdays




 # alternatively, calculate share of countries on effort from the "Deepsea" effort
 #aa <- data.table(aer_all_y)
 #agg_kwfdays <- aa[,.(tot_kwFishingdays=sum(aerECOkwFishingdaysDeepSea_perregion)),by=c("year", "key")]
 #aer_all_y <- merge(aer_all_y, agg_kwfdays,
 #                 by= c("year", "key"))
 #
 #aer_all_y$country_share_effort_deepsea <- 
 #      aer_all_y$aerECOkwFishingdaysDeepSea_perregion/aer_all_y$tot_kwFishingdays


 
 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 # MERGE FDI WITH AER, AND DISPACH FDI LANDINGS, FDI EFFORT AND AER kwfishingdays and COSTS ON FDI C-SQUARE CELLS PER YEAR, COUNTRY AND METIER LEVEL6
 # CAUTION: THE SPECIES DIMENSION MAKES EVERYTHING MUCH MORE COMPLICATED....(CONSIDER REMOVING SPECIES BEFOREHAND)

 #save(fdi_all_y, file=file.path(ROutputPathToDatasets, "FDI_2018_2021_in_NAO_before_merging.RData"))
 #head(fdi_all_y)
 #save(aer_all_y, file=file.path(ROutputPathToDatasets, "AER_2018_2021_in_NAO_before_merging.RData"))
 #head(aer_all_y)

 #load(file=file.path(ROutputPathToDatasets, "FDI_2018_2021_in_NAO_before_merging.RData"))
 #head(fdi_all_y)
 #load(file=file.path(ROutputPathToDatasets, "AER_2018_2021_in_NAO_before_merging.RData"))
 #head(aer_all_y)

  # check
 dd <- fdi_all_y[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
 dd[,.(sum(fditotwghtlandg, na.rm=TRUE),sum(fditotfishdays, na.rm=TRUE)),] 

 dd <- fdi_all_y[country_code=="ESP" & fishing_tech=="DTS" & vessel_length=="VL2440" & year=="2018",]
 dd[,.(sum(fditotwghtlandg, na.rm=TRUE),sum(fditotfishdays, na.rm=TRUE)),] 

 unique(dd[dd$in_400_800_zone==1, "metier"])


 dispatched_aer <- list()

 for (y in as.character(years))
 {
   cat(paste("y", y, "\n"))
  
   # landings
   aer_y                                      <-   aer_all_y[aer_all_y$year==y,]
   aer_y[,.(tot_kg=sum(an(weight))),] # check 
   # effort
   fdi_y                                      <-  fdi_all_y[fdi_all_y$year==y,]
   fdi_y[,.(tot_totfishdays=sum(fditotfishdays)),] # check 
   
   # check with a toy and see the first chunk1_1...
   #temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(aer_y[,"key"])),"_",5))  
   #aer_y <- aer_y[aer_y$country_code=="BEL" & temp[,4]=="TBB" & temp[,3]=="VL2440" & aer_y$y=="2018"  & temp[,5]=="27.8.b" ,]
   #temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(fdi_y[,"key"])),"_",5))  
   #fdi_y <- fdi_y[fdi_y$country_code=="BEL" & temp[,4]=="TBB" & temp[,3]=="VL2440" & fdi_y$y=="2018" & temp[,5]=="27.8.b" ,]


   # get a share_effort for dispatching data (caution, fdi data are species explicit data)
   sum_effort_y_per_key                     <-  fdi_y[, .(tot_totfishdays = sum(fditotfishdays, na.rm=T)), by=c("key")]  # removing the c-square dimension here...
   sum_effort_y_per_key_level6              <-  fdi_y[, .(tot_totfishdays_met = sum(fditotfishdays, na.rm=T)), by=c("metier", "key")]  # removing the c-square dimension here...
   sum_effort_y_per_key_level6_inzone       <-  fdi_y[, .(tot_totfishdays_met_inzone = sum(fditotfishdays, na.rm=T)), by=c("in_400_800_zone","metier", "key")]  # removing the c-square dimension here...
   sum_effort_y_per_key_level6_inzone_spp   <-  fdi_y[, .(tot_totfishdays_met_inzone_spp = sum(fditotfishdays, na.rm=T)), by=c("species","in_400_800_zone","metier", "key")]  # removing the c-square dimension here...
   #sum_effort_y_per_key_level6_inzone[,.(tot_effort=sum(tot_totfishdays_met_csquare)),] # check 
   fdi_y_e                                 <-  merge(fdi_y, sum_effort_y_per_key, by=c("key"))
   fdi_y_e                                 <-  merge(fdi_y_e, sum_effort_y_per_key_level6, by=c("metier", "key"))
   fdi_y_e                                 <-  merge(fdi_y_e, sum_effort_y_per_key_level6_inzone, by=c("in_400_800_zone","metier", "key"))
   fdi_y_e                                 <-  merge(fdi_y_e, sum_effort_y_per_key_level6_inzone_spp, by=c("species", "in_400_800_zone","metier", "key"))
   fdi_y_e$share_effort_level6                    <-  fdi_y_e$tot_totfishdays_met / fdi_y_e$tot_totfishdays # for dispatching depending on the contribution of that metier to the total effort in this key
   fdi_y_e$share_effort_level6_inzone             <-  fdi_y_e$tot_totfishdays_met_inzone / fdi_y_e$tot_totfishdays_met # for dispatching depending on the contribution of that metier-zone to the total effort in this key-metier
   fdi_y_e$share_effort_level6_inzone_species     <-  fdi_y_e$tot_totfishdays_met_inzone_spp / fdi_y_e$tot_totfishdays_met_inzone # for dispatching depending on the contribution of that cell to the total effort in that key-metier-zone
   fdi_y_e$share_effort_level6_inzone_species_csquare     <-  fdi_y_e$fditotfishdays / fdi_y_e$tot_totfishdays_met_inzone_spp # for dispatching depending on the contribution of that cell to the total effort in that key-metier-zone
   #=>  for now, those share allocation keys are a bit overkill here as not used later (it is equivalent and decompose a dispatching of total effort per fs-key over the c-squares).
   
   # a check column: "md5" should be equal to the sum at the key level i.e. "fditotfishdays"
   fdi_y_e$md5 <- fdi_y_e$tot_totfishdays * fdi_y_e$share_effort_level6 * fdi_y_e$share_effort_level6_inzone *  fdi_y_e$share_effort_level6_inzone_species  * fdi_y_e$share_effort_level6_inzone_species_csquare  
  
   # check
   #dd <- fdi_y_e[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
   #dd[,.(sum(fditotwghtlandg, na.rm=TRUE),sum(fditotfishdays, na.rm=TRUE)),] 
   #dd <- aer_y[country_code=="BEL" & fishing_tech=="TBB" & vessel_length=="VL2440" & year=="2018"  & sub_reg=="27.8.b",]
   #dd[,.(sum(weight, na.rm=TRUE),sum(value, na.rm=TRUE)),] 

   
   # clean a bit the data.table to save memory!
   rm_col <- c("md5", "tot_totfishdays", "tot_totfishdays_met", "tot_totfishdays_met_inzone", "tot_totfishdays_met_inzone_spp", "SI_LATI", "SI_LONG")
   fdi_y_e <- fdi_y_e[, (rm_col):=NULL]   #=> 571 tons, 2479 days (caution: counted several times depending on sp)
   rm_col <- c("fishing_tech", "supra_reg", "vessel_length")
   aer_y <- aer_y[, (rm_col):=NULL] #=> 570550 kg, 3841711 euro 
  
  
  
  # a check before the merging as such
  if(FALSE){
    nonvariablevars <- AER_nonvariablevars[AER_nonvariablevars$supra_reg=="NAO",]
    temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(aer_y  [,"key"])),"_",5))  
    aer_y[, "vessel_length"] <- temp[,3]
    aer_y[, "fishing_tech"] <- temp[,4]
    dd          <- left_join (aer_y, nonvariablevars, by=c("year", "country_code", "fishing_tech", "vessel_length")) 
    dd$varcosts <- dd$aerECOkwFishingdays_perregion * (dd$enerbykwfishdy + dd$repbykwfishday + dd$varbykwfishday)  
    # check numbers...
    ddd <- dd[dd$year=="2018" & dd$country_code=="BEL" & dd$fishing_tech=="TBB" & dd$vessel_length=="VL2440",]
    ddd$GVA <- (an(ddd$weight) *  # landing kg  * price
                      (an(ddd$value)/an(ddd$weight))) + 
                      an(ddd$other_income) - # plus other income
                      an(ddd$unpaid_labour) - an(ddd$varcosts) # minus var costs (caution: other non var costs missing here)
    ddd[ddd$sub_reg=="27.4.b","weight"] #=> 4204869
    sum(ddd[,"weight"]) #=> 13386476
    } # end FALSE
  
  
   
   # merge (by chunk, to avoid memory issue)
   gc(full=TRUE); rm(merged)
   keys <- unlist(c(unique(aer_y[aer_y$chunk==1, "key"])))  # needed for the trick to chunk...
   idx  <- c(floor(seq(1,length(keys),by=length(keys)/10)),length(keys))
   chunk1_1                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[1:idx[2]] ,], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_2                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[2]+1):idx[3]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_3                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[3]+1):idx[4]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_4                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[4]+1):idx[5]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_5                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[5]+1):idx[6]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_6                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[6]+1):idx[7]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_7                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[7]+1):idx[8]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_8                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[8]+1):idx[9]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_9                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[9]+1):idx[10]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_10                                     <- merge(aer_y[aer_y$chunk==1 & aer_y$key %in% keys[(idx[10]+1):idx[11]], ], fdi_y_e[fdi_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   keys <- unlist(c(unique(aer_y[aer_y$chunk==2, "key"])))  # needed for the trick to chunk...
   if(length(keys)>0) idx  <- c(floor(seq(1,length(keys),by=length(keys)/5)),length(keys))
   chunk2_1                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[1:idx[2]],], fdi_y_e[fdi_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_2                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[2]+1):idx[3]],], fdi_y_e[fdi_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_3                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[3]+1):idx[4]],], fdi_y_e[fdi_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_4                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[4]+1):idx[5]],], fdi_y_e[fdi_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2_5                                       <- merge(aer_y[aer_y$chunk==2 & aer_y$key %in% keys[(idx[5]+1):idx[6]],], fdi_y_e[fdi_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   keys <- unlist(c(unique(aer_y[aer_y$chunk==3, "key"])))  # needed for the trick to chunk...
   if(length(keys)>0) idx  <- c(floor(seq(1,length(keys),by=length(keys)/2)),length(keys))
   chunk3_1                                       <- merge(aer_y[aer_y$chunk==3 & aer_y$key %in% keys[1:idx[2]],], fdi_y_e[fdi_y_e$chunk==3,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk3_2                                       <- merge(aer_y[aer_y$chunk==3 & aer_y$key %in% keys[(idx[2]+1):idx[3]],], fdi_y_e[fdi_y_e$chunk==3,], by.x="key", by.y="key", allow.cartesian=TRUE)
 
 
   gc(full=TRUE); rm(merged)
   cols <- c("key", "country_code.x","year.x","species","value", "weight","value_deepsea", "weight_deepsea", "fditotvallandg","fditotwghtlandg","aerkwFishingdays", "aerdaysatsea", "aerECOkwFishingdays_perregion", 
             "fditotfishdays","enerbykwfishdy","wagebyinc","repbykwfishday","varbykwfishday", #"country_share_effort",
             "metier","cscode","rectangle_type","share_effort_level6","share_effort_level6_inzone", "share_effort_level6_inzone_species", "share_effort_level6_inzone_species_csquare")
   merged <- rbind.data.frame(chunk1_1[,..cols], chunk1_2[,..cols], chunk1_3[,..cols],  chunk1_4[,..cols],  chunk1_5[,..cols],  chunk1_6[,..cols], chunk1_7[,..cols], chunk1_8[,..cols], chunk1_9[,..cols], chunk1_10[,..cols],
                  chunk2_1[,..cols], chunk2_2[,..cols], chunk2_3[,..cols], chunk2_4[,..cols], chunk2_5[,..cols],
                  chunk3_1[,..cols], chunk3_2[,..cols])
   gc(full=TRUE)
         
   
   # dispatch AER vars over ctry-level6-csquare combinaisons depending on share alloc keys. Caution: all species are pooled in AER
   #(caution:  merged$country_share_effort should only be used if dispatching the non-confidential FDI data....)
   merged$fditotfishdays                                  <-  merged$fditotfishdays* merged$share_effort_level6 * merged$share_effort_level6_inzone * merged$share_effort_level6_inzone_species # * merged$country_share_effort                   
   #a_dispatcher_key_for_aer_var                            <-  merged$share_effort_level6 * merged$share_effort_level6_inzone  * merged$share_effort_level6_inzone_species_csquare  # * merged$country_share_effort   
   a_dispatcher_key_for_aer_var                            <-  merged$share_effort_level6 * merged$share_effort_level6_inzone   * merged$share_effort_level6_inzone_species * merged$share_effort_level6_inzone_species_csquare  # * merged$country_share_effort   
   a_dispatcher_key_for_effort                             <-  merged$share_effort_level6 * merged$share_effort_level6_inzone * merged$share_effort_level6_inzone_species * merged$share_effort_level6_inzone_species_csquare  # * merged$country_share_effort   
   merged$landings_aer_in_ctry_level6_csquare             <-  merged$weight                             * a_dispatcher_key_for_aer_var  # dispatch on cell per species
   merged$value_aer_in_ctry_level6_csquare                <-  merged$value                              * a_dispatcher_key_for_aer_var  # dispatch on cell per species
   merged$landingsdeepsea_aer_in_ctry_level6_csquare      <-  merged$weight_deepsea                             * a_dispatcher_key_for_aer_var  # dispatch on cell per species
   merged$valuedeepsea_aer_in_ctry_level6_csquare         <-  merged$value_deepsea                              * a_dispatcher_key_for_aer_var  # dispatch on cell per species
   merged$KwFishingdays_aer_in_ctry_level6_csquare        <-  an(merged$aerECOkwFishingdays_perregion)  * a_dispatcher_key_for_aer_var  # dispatch on cell per species
   merged$daysatsea_aer_in_ctry_level6_csquare            <-  an(merged$aerdaysatsea)                   * a_dispatcher_key_for_aer_var  # dispatch on cell per species
  
  
    # cautionfditotfishdays:  now will be conserved when aggregating over ALL species
    #merged[,.(tot_totfishdays=sum(fditotfishdays, na.rm=TRUE)),] # check 
    #fdi_y[,.(tot_totfishhdays=sum(fditotfishdays, na.rm=TRUE)),] # check
   
   
   # check for conservation of effort per fs
   # [DEPRECATED: EFFORT IS SPLIT BETWEEN SPECIES NOW]
   if(FALSE){
    dd <- fdi_y_e
    for(k in unique(dd$key)){
      aa <- dd[dd$year=="2018"  & dd$key==k,]
      ddd <- aer_y
      temp <- as.data.frame(str_split_fixed(ddd$key,"_",4))  
      ddd$vessel_length <- temp[,2]
      ddd$fishing_tech <- temp[,3]
      bb <- ddd[ddd$year=="2018"  & ddd$key==k,]
      cc                                     <- merge(bb, aa, by.x="key", by.y="key", allow.cartesian=TRUE)
    #if (round(sum(aa$fditotfishdays)) != round(sum(cc$fditotfishdays*cc$country_share_effort))) {
    if (round(sum(aa$fditotfishdays)) != round(sum(cc$fditotfishdays))) {
      cat(paste("no conservation for", k, "...break\n"))
      break
      }
    }
   } # end FALSE

  
  
   # check conservation of effort and landings
   library(stringr)
   temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(merged[,"key"])),"_",5))  
   dd <- merged[merged$country_code.x=="BEL" & temp[,4]=="TBB" & temp[,3]=="VL2440" & merged$y=="2018" & temp[,5]=="27.8.b" ,]                              
   ddd <- unique(dd, by = c("key", "country_code.x", "cscode"))# remove spp dim
   ddd[,sum( KwFishingdays_aer_in_ctry_level6_csquare, na.rm=TRUE),]  #  666695.5 # CAUTION: split over species for now, so should be multiplied by the nb of species to be comparable...will be later aggregated anyway
   ddd[,sum( landings_aer_in_ctry_level6_csquare, na.rm=TRUE),] #  570550.5
   
   temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(aer_y[,"key"])),"_",5))  
   dd <- aer_y[aer_y$country_code=="BEL" & temp[,4]=="TBB" & temp[,3]=="VL2440" & aer_y$y=="2018"  & temp[,5]=="27.8.b",]
   dd[,sum(aerECOkwFishingdays_perregion, na.rm=TRUE),] # 666695.5
   dd[,sum(aerECOkwFishingdays_perregion, na.rm=TRUE),]*1.836975   
   dd[,sum(weight, na.rm=TRUE),]    #  570550.5
  
  dd <- AERcosts[AERcosts$country_code=="BEL" & AERcosts$fishing_tech=="TBB" & AERcosts$vessel_length=="VL2440" & AERcosts$y=="2018",]                              
  an(dd$aerECOkwFishingdays) # 7333651 all sub_region
  an(dd$energycosts) 
  an(dd$energycosts)/an(dd$aerECOkwFishingdays) # 1.836975
  an(dd$energycosts)+an(dd$repaircosts)+an(dd$othvarcosts)


 
   # clean a bit 
   colnames(merged) <- gsub(".x", "", colnames(merged), fixed=TRUE) 
   merged             <- merged  [, c("key","country_code","year","species","metier","cscode", "fditotwghtlandg", "fditotvallandg", "fditotfishdays",
                                                        "daysatsea_aer_in_ctry_level6_csquare",
                                                        "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
                                                        "landingsdeepsea_aer_in_ctry_level6_csquare", "valuedeepsea_aer_in_ctry_level6_csquare",
                                                        "KwFishingdays_aer_in_ctry_level6_csquare", "rectangle_type",
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
                      (an(ddd$value_aer_in_ctry_level6_csquare)/an(ddd$landings_aer_in_ctry_level6_csquare))) - an(ddd$varcosts)  # landing value minus var costs (-ish because here we lack oth income and unpaid labour and other non var costs ...)
    sum(ddd[ddd$sub_reg=="27.4.b","landings_aer_in_ctry_level6_csquare"]) #=>  4204869
    sum(ddd[,"landings_aer_in_ctry_level6_csquare", ],na.rm=TRUE) #=> 13386476
    } # end FALSE

                                                                                                                                 
   #compute LPUEs and disagregate costs 
   merged$lpue_aer_this_species_in_ctry_level6_csquare       <- merged$landings_aer_in_level6_csquare/merged$fditotfishdays  # aer kg per day in this cell, for all species pooled
   merged$lpue_fdi_this_species_in_ctry_level6_csquare       <- (merged$fditotwghtlandg*1000)/merged$fditotfishdays  # fdi kg per day, for this species in this cell
   merged$energycosts_in_ctry_level6_csquare                 <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$enerbykwfishdy  # energy costs
   merged$personnelcosts_in_ctry_level6_csquare              <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$wagebyinc  # personel costs
   merged$repaircosts_in_ctry_level6_csquare                 <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$repbykwfishday  # repair costs
   merged$othvarcosts_in_ctry_level6_csquare                 <- merged$KwFishingdays_aer_in_ctry_level6_csquare * merged$varbykwfishday  # other variable costs
 
  
   # clean a bit 
   merged             <- merged  [, c("key","country_code","year","species","metier","cscode","rectangle_type",  "fditotwghtlandg", "fditotvallandg", "fditotfishdays",
                                                        "landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare", 
                                                        "landingsdeepsea_aer_in_ctry_level6_csquare","valuedeepsea_aer_in_ctry_level6_csquare", 
                                                        "daysatsea_aer_in_ctry_level6_csquare", "KwFishingdays_aer_in_ctry_level6_csquare",
                                                        "lpue_aer_this_species_in_ctry_level6_csquare", "lpue_fdi_this_species_in_ctry_level6_csquare",
                                                        "energycosts_in_ctry_level6_csquare", "personnelcosts_in_ctry_level6_csquare", "repaircosts_in_ctry_level6_csquare", 
                                                        "othvarcosts_in_ctry_level6_csquare")]
   # save
   save(merged, file=file.path(ROutputPathToDatasets, paste0("dispatched_aer_vars_and_fdi_land_using_2018_2021_fdi_effort_csquares_in_",y,".RData"))) 
   
   dispatched_aer[[y]]  <- merged 
   }

 # check 
 #head(dispatched_aer[["2019"]])

  # check numbers
 dd  <- dispatched_aer[["2018"]]
 temp <- as.data.frame(str_split_fixed(dd$key,"_",5))  
 dd$country       <- temp[,2]
 dd$fishing_tech  <- temp[,4]
 dd$vessel_size   <- temp[,3]
 ddd <- dd[dd$country_code=="ESP" & dd$vessel_size=="VL2440" & dd$fishing_tech=="DTS" & dd$year=="2018",]    
 sum(ddd$landings_aer_in_ctry_level6_csquare) 


 # save 
 dispatched_aer_ally <- do.call("rbind", dispatched_aer)
 save(dispatched_aer_ally, file=file.path(ROutputPathToDatasets, "dispatched_aer_vars_and_fdi_land_using_2018_2021_fdi_effort_csquares_in_ally.RData")) 

 # CAUTION: THE SPECIES DIM IS THERE, ----SO DO NOT AGGREGATE EFFORT-RELATED AER VARIABLES UNLESS REMOVING FIRST THE DUPLICATES OVER SPECIES...
 # however, FDI totwghtlandg totvallandg totfishdays can be aggregated over c-squares...


   # check conservation of land and effort
  temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(dispatched_aer_ally[,"key"])),"_",5))  
  dd <- dispatched_aer_ally[temp[,2]=="BEL" & temp[,4]=="TBB" & temp[,3]=="VL2440" & temp[,1]=="2018" & temp[,5]=="27.8.b" ,]                              
  dd[,.(sum(fditotwghtlandg, na.rm=TRUE),sum(fditotfishdays, na.rm=TRUE)),] 



  ## AT THIS POINT THE METIER DIMENSION IS STILL THERE
  ## MAYBE SOME FILTERING ON METIER COULD BE DONE IF SOME OF THEM OF NOT RELEVANT TO ANSWER THE ISSUE?
  ## PROBLEM IS THAT IF WE FILTER OUT SOME METIERS NOW THEN THE AERothvars will not be not consistent because it applies to the key


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
 load(file=file.path(file.path(ROutputPathToDatasets, "dispatched_aer_vars_and_fdi_land_using_2018_2021_fdi_effort_csquares_in_ally.RData")))  # get "dispatched_aer_ally"

 # in FDI, remember totwghtlandg is in tonnes
 
 # compute an overall var costs (those come from energycosts per fday...)
 dispatched_aer_ally$varcosts_in_ctry_level6_csquare    <- dispatched_aer_ally$energycosts_in_ctry_level6_csquare + 
                    dispatched_aer_ally$repaircosts_in_ctry_level6_csquare + 
                    dispatched_aer_ally$othvarcosts_in_ctry_level6_csquare
 

 # retrieve the coord
 library(vmstools)
 library(dplyr)
 dd         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="05*1",] # ices rectangle
 sq         <- cbind.data.frame(cscode=unique(dd$cscode), vmstools::CSquare2LonLat(unique(dd$cscode), 0.5))
 dd         <- dplyr::left_join(dd, sq, by="cscode")
 dd1         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="05*05",]
 sq         <- cbind.data.frame(cscode=unique(dd1$cscode), vmstools::CSquare2LonLat(unique(dd1$cscode), 0.5))
 dd1         <- dplyr::left_join(dd1, sq, by="cscode")
 dd2         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="1*1",]
 if(nrow(dd2)!=0){
   sq         <- cbind.data.frame(cscode=unique(dd2$cscode), vmstools::CSquare2LonLat(unique(dd2$cscode), 1))
   dd2         <- dplyr::left_join(dd2, sq, by="cscode")
 }
 dd3         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="5*5",]
 if(nrow(dd3)!=0){
    sq         <- cbind.data.frame(cscode=unique(dd3$cscode), vmstools::CSquare2LonLat(unique(dd3$cscode), 5))
    dd3         <- dplyr::left_join(dd3, sq, by="cscode")
 }
 distr      <- rbind(dd, dd1)
 #distr      <- rbind(dd, dd1,dd2,dd3)
 

  ## !!!export!!! ##
 save(distr, file=file.path(ROutputPathToDatasets, "distr_per_species_from_2023AER_coupled_to_2018_2021_FDI.RData"))


 # a check
 head(fdi_land_effort_rect_mainsp)
 dd         <- fdi_land_effort_rect_mainsp[fdi_land_effort_rect_mainsp$rectangle_type=="05*1",] # ices rectangle
 sq         <- cbind.data.frame(cscode=unique(dd$cscode), vmstools::CSquare2LonLat(unique(dd$cscode), 0.5))
 dd         <- dplyr::left_join(dd, sq, by="cscode")
 distr_hke <- dd[dd$species=="HKE",]

 head(fdi_all_y)
 dd         <- fdi_all_y[fdi_all_y$rectangle_type=="05*1",] # ices rectangle
 distr_hke1 <- dd[dd$species=="HKE",]

 # a check
 head(merged)
 dd         <- merged[merged$rectangle_type=="05*1",] # ices rectangle
 sq         <- cbind.data.frame(cscode=unique(dd$cscode), vmstools::CSquare2LonLat(unique(dd$cscode), 0.5))
 dd         <- dplyr::left_join(dd, sq, by="cscode")
 dd1         <- merged[merged$rectangle_type=="05*05",]
 sq         <- cbind.data.frame(cscode=unique(dd1$cscode), vmstools::CSquare2LonLat(unique(dd1$cscode), 0.5))
 dd1         <- dplyr::left_join(dd1, sq, by="cscode")
 dd2         <- merged[merged$rectangle_type=="1*1",]
 if(nrow(dd2)!=0){
   sq         <- cbind.data.frame(cscode=unique(dd2$cscode), vmstools::CSquare2LonLat(unique(dd2$cscode), 1))
   dd2         <- dplyr::left_join(dd2, sq, by="cscode")
 }
 dd3         <- merged[merged$rectangle_type=="5*5",]
 if(nrow(dd3)!=0){
    sq         <- cbind.data.frame(cscode=unique(dd3$cscode), vmstools::CSquare2LonLat(unique(dd3$cscode), 5))
    dd3         <- dplyr::left_join(dd3, sq, by="cscode")
 }
 #distr      <- rbind(dd, dd1,dd2,dd3)
 a_distr      <- rbind(dd, dd1)
 distr_hke2 <- a_distr[a_distr$species=="HKE",]


  # a check
 head(dispatched_aer_ally)
 dd         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="05*1",] # ices rectangle
 sq         <- cbind.data.frame(cscode=unique(dd$cscode), vmstools::CSquare2LonLat(unique(dd$cscode), 0.5))
 dd         <- dplyr::left_join(dd, sq, by="cscode")
 dd1         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="05*05",]
 sq         <- cbind.data.frame(cscode=unique(dd1$cscode), vmstools::CSquare2LonLat(unique(dd1$cscode), 0.5))
 dd1         <- dplyr::left_join(dd1, sq, by="cscode")
 dd2         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="1*1",]
 if(nrow(dd2)!=0){
   sq         <- cbind.data.frame(cscode=unique(dd2$cscode), vmstools::CSquare2LonLat(unique(dd2$cscode), 1))
   dd2         <- dplyr::left_join(dd2, sq, by="cscode")
 }
 dd3         <- dispatched_aer_ally[dispatched_aer_ally$rectangle_type=="5*5",]
 if(nrow(dd3)!=0){
    sq         <- cbind.data.frame(cscode=unique(dd3$cscode), vmstools::CSquare2LonLat(unique(dd3$cscode), 5))
    dd3         <- dplyr::left_join(dd3, sq, by="cscode")
 }
 #distr      <- rbind(dd, dd1,dd2,dd3)
 a_distr      <- rbind(dd, dd1)
 distr_hke3 <- a_distr[a_distr$species=="HKE",]

## aggregate per Csquare, year and key-country
  # all dims with the explicit species dim can be summed (i.e. FDI totwghtlandg totvallandg totfishdays, but also all the AER dispatched ones)
  distr_allsp <- 
     distr[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("fditotfishdays", "fditotwghtlandg", "fditotvallandg", "KwFishingdays_aer_in_ctry_level6_csquare", "varcosts_in_ctry_level6_csquare", "personnelcosts_in_ctry_level6_csquare",
         "daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare", "landingsdeepsea_aer_in_ctry_level6_csquare", "valuedeepsea_aer_in_ctry_level6_csquare"),
          keyby=c("year","key", "country_code", "cscode", "rectangle_type", "SI_LATI", "SI_LONG")]
                    
 ## the metier dim could have been kept here but would require some further assumption to disaggregate the AERothvars given per fs over metiers...
 
 
  # recompute a LPUE        
  distr_allsp$lpue_csquare_aer_kgperdayatsea <- distr_allsp$landings_aer_in_ctry_level6_csquare/distr_allsp$daysatsea_aer_in_ctry_level6_csquare  # kg per daysatsea
  distr_allsp$lpue_csquare_fdi_kgperfday <- distr_allsp$fditotwghtlandg*1000/distr_allsp$fditotfishdays  # kg per fday
  # check
  #plot(distr_allsp$lpue_csquare_aer_kgperdayatsea, distr_allsp$lpue_csquare_fdi_kgperfday, pch="+")
  #lines(seq(1,1e6, by=1e2), seq(1,1e6, by=1e2)) # 1:1
 
 

  # load to later merge with AERothvars
  #filename         <- file.path(RinputPath, "STECF_DATA", "STECF 22-06 AER 2022 - data",
  #                            "AERothvars_fromAER2022.RData")
  filename         <- file.path(RinputPath, "STECF_DATA", "2023_AER_Data",
                              "AERothvars_fromAER2023.RData")
  load(file=file.path(filename))  #  get AERothvars
  
  # check
  AERothvars[AERothvars$year=="2020" & AERothvars$country_code=="ESP" & AERothvars$fishing_tech=="HOK" & AERothvars$vessel_length=="VL2440" ,]

  # add fs
  AERothvars    <-  as.data.frame(AERothvars)
  AERothvars$fs <-  paste0(AERothvars[, "country_code"], "_", AERothvars[, "fishing_tech"], "_", AERothvars[, "vessel_length"])
  AERothvars    <-  data.table(AERothvars)
  
  # check
  AERothvars[AERothvars$year=="2020" & AERothvars$fs=="ESP_HOK_VL2440" ,]


  # retrieve fleet segments
  library(stringr)
  temp <- as.data.frame(stringr::str_split_fixed(as.character(unlist(distr_allsp[,"key"])),"_",5))  
  distr_allsp[,"fs"] <- paste0(unlist(c(distr_allsp[, "country_code"])), "_", temp[,4], "_", temp[,3])

  # caution: we are only looking at the NAO region for now
  AERothvars <- AERothvars[supra_reg=="NAO",]

  # compute the GVA from spatial landings and variable costs and non-spatial fixed costs
  # GVA <- (landings_kg * average_price_EUR_per_kg) + other_income - unpaid_labour -  energycosts - othvarcosts -  oth_non_var_costs -  repaircosts
  # but then, first merge to dispatch other_income and unpaid_labour on Csquare per fs per year...
  # count csquares...
  an <- function(x) as.numeric(x) 
  csquare_per_fs_per_y <-  distr_allsp[,.(nb_csquare_per_fs_per_y=length(unique(cscode))), by=c("year","fs")]
  distr_allsp          <-  dplyr::left_join(distr_allsp, csquare_per_fs_per_y)
  AERothvars$year      <-  as.character(AERothvars$year)
  # then merge...
  distr_allsp          <- dplyr::left_join(distr_allsp, AERothvars[,c("other_income", "unpaid_labour", "oth_non_var_costs", "year", "fs")])
  # then dispatch...
  distr_allsp$other_income_in_csquare    <- an(distr_allsp$other_income)/an(distr_allsp$nb_csquare_per_fs_per_y)
  distr_allsp$unpaid_labour_in_csquare   <- an(distr_allsp$unpaid_labour)/an(distr_allsp$nb_csquare_per_fs_per_y)
  distr_allsp$oth_non_var_costs_in_csquare   <- an(distr_allsp$oth_non_var_costs)/an(distr_allsp$nb_csquare_per_fs_per_y)
  distr_allsp$other_income_in_csquare [is.na(distr_allsp$other_income_in_csquare)] <- 0
  distr_allsp$unpaid_labour_in_csquare [is.na(distr_allsp$unpaid_labour_in_csquare)] <- 0
  distr_allsp$oth_non_var_costs_in_csquare [is.na(distr_allsp$oth_non_var_costs_in_csquare)] <- 0
  # then compute GVA (remember that in FDI totwghtlandg is in tonnes)
  distr_allsp$GVA <- (an(distr_allsp$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(distr_allsp$value_aer_in_ctry_level6_csquare)/an(distr_allsp$landings_aer_in_ctry_level6_csquare))) + 
                      an(distr_allsp$other_income_in_csquare) - # plus other income
                      an(distr_allsp$unpaid_labour_in_csquare) - an(distr_allsp$varcosts_in_ctry_level6_csquare) - an(distr_allsp$oth_non_var_costs_in_csquare) # minus var and fixed costs

 
 
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
  #dd[,.(kwfdays=sum(aerECOkwFishingdays_perregion)),]
  #dd <- AERcosts[AERcosts$country_code=="BEL" & AERcosts$fishing_tech=="TBB" & AERcosts$vessel_length=="VL2440" & AERcosts$y=="2019",]                              
  #an(dd$energycosts)/an(dd$aerECOkwFishingdays_perregion)
  #an(dd$energycosts)+an(dd$repaircosts)+an(dd$othvarcosts)
 
 

# 3. FOR NON-VARIABLE AER VARS, DO A MERGE WITH AER PER FS, AND THEN COMPUTE THE FOLLOWING:
# GrossProfit <- GVA - personnelcosts    
# OperatingProfit <-  GrossProfit - cons_of_fixed_capital
# CapitalOpportunityCosts <- value_of_physical_capital * opportunity_interest_rate/100.0 
# NetProfit <-  OperatingProfit - CapitalOpportunityCosts  - value_of_physical_capital * ((100.0-annual_depreciation_rate)/100.0)
#=> to be computed on the final merged dataset, then to be recomputed after applying displacement scenarios (e.g. based on LPUEs-costs...) changing the income from landings and the costs

 distr_allsp$GrossProfit <- distr_allsp$GVA - an(distr_allsp$personnelcosts_in_ctry_level6_csquare)


  ## !!!export!!! ##
 save(distr_allsp, file=file.path(ROutputPathToDatasets, "distr_allsp_from_2023AER_coupled_to_2018_2021_FDI.RData"))



 # remove the spatial dim to get final eco estimates
 agg_eco_fs <- distr_allsp[,.( 
          fditotfishdays=sum(fditotfishdays),
          sumkwfday=sum(KwFishingdays_aer_in_ctry_level6_csquare),
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
 
 
 ## !!!export!!! ##
 save(agg_eco_fs, file=file.path(ROutputPathToDatasets, "agg_2018_2021_fdi_2023aer_eco_fs.RData"))











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


 #----------------------
 # load
 load(file=file.path(ROutputPathToDatasets, "distr_per_species_from_2023AER_coupled_to_2018_2021_FDI.RData"))  # species explicit
 load(file=file.path(ROutputPathToDatasets, "distr_allsp_from_2023AER_coupled_to_2018_2021_FDI.RData"))  # all species aggregated


 
 #----------------------
 # a quick and dirty plot
if(FALSE){ 
 #a_var <- distr[, .(a_var_to_plot=sum(landingsdeepsea_aer_in_ctry_level6_csquare)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr[, .(a_var_to_plot=sum(varcosts_in_ctry_level6_csquare, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr_allsp[, .(a_var_to_plot=sum(lpue_csquare_aer_kgperdayatsea, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type")]
  a_var <- distr[, .(a_var_to_plot=sum(fditotfishdays), na.rm=TRUE), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr_hke[, .(a_var_to_plot=sum(fditotwghtlandg, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr_hke1[, .(a_var_to_plot=sum(fditotwghtlandg, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr_hke2[, .(a_var_to_plot=sum(landings_aer_in_ctry_level6_csquare, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr_hke3[, .(a_var_to_plot=sum(landings_aer_in_ctry_level6_csquare, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr[, .(a_var_to_plot=sum(landings_aer_in_ctry_level6_csquare, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr[species=="HKE", .(a_var_to_plot=sum(landings_aer_in_ctry_level6_csquare, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type", "species")]
 a_var <- distr_allsp[, .(a_var_to_plot=sum(landings_aer_in_ctry_level6_csquare, na.rm=TRUE)), by=c("cscode", "SI_LATI", "SI_LONG", "rectangle_type")]

 an <- function(x) as.numeric(x)

 a_var      <- as.data.frame(a_var)
 a_var$a_var_to_plot <- log(a_var$a_var_to_plot) 

 cutbreakval            <- seq(1, 20, by=1)  # log(kilos)
 colyellowred           <- terrain.colors(length(cutbreakval))

 a_species              <- "SKJ" # in OTH?
 a_species              <- "COD"
 a_species              <- "HKE"
 #a_species              <- "OTH"
 if("species" %in% colnames(a_var)){
   cols                   <- c("yellow", colyellowred)[cut(unlist(an(a_var[a_var$species==a_species, "a_var_to_plot"])), breaks=cutbreakval)]
   } else{
    cols                   <- c("yellow", colyellowred)[cut(unlist(an(a_var[, "a_var_to_plot"])), breaks=cutbreakval)]
   }
   coord                  <- a_var[, c("SI_LONG", "SI_LATI", "rectangle_type")]
 plot(coord[,c("SI_LONG", "SI_LATI")], pch="")

# resy <-  diff(unique(a_var$SI_LATI)[order(unique(a_var$SI_LATI))])  [2]
# resx <-  diff(unique(a_var$SI_LONG)[order(unique(a_var$SI_LONG))])  [2]

 for (i in 1: nrow(coord)){
  if(coord[i,"rectangle_type"]=="05*05") rect(coord[i, "SI_LONG"]-0.5/2, coord[i,"SI_LATI"]-0.5/2, coord[i,"SI_LONG"]+0.5/2, coord[i,"SI_LATI"]+0.5/2, col=cols[i], border=FALSE)
  if(coord[i,"rectangle_type"]=="05*1") rect(coord[i, "SI_LONG"]-1/2, coord[i,"SI_LATI"]-0.5/2, coord[i,"SI_LONG"]+1/2, coord[i,"SI_LATI"]+0.5/2, col=cols[i], border=FALSE)
  if(coord[i,"rectangle_type"]=="1*1") rect(coord[i, "SI_LONG"]-1/2, coord[i,"SI_LATI"]-1/2, coord[i,"SI_LONG"]+1/2, coord[i,"SI_LATI"]+1/2, col=cols[i], border=FALSE)
  if(coord[i,"rectangle_type"]=="5*5") rect(coord[i, "SI_LONG"]-5/2, coord[i,"SI_LATI"]-5/2, coord[i,"SI_LONG"]+5/2, coord[i,"SI_LATI"]+5/2, col=cols[i], border=FALSE)
 }
 library(maps)
 map(add=T)
 #=> looks good....
 # then proceed with an more advanced plotting:
}




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# get a the data.table linked to a master raster grid
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

      

my_raster_export <- function(master_dt, slave_dt){     
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
  #resx                          <-  diff(unique(master_dt$SI_LONG)[order(unique(master_dt$SI_LONG))])  [3]  # master distr giving the bbox
  resx                          <-  0.5  # master distr giving the bbox
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
  some_cols_to_sweep <- c("fditotfishdays", "daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
             "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare",
          "other_income_in_csquare" ,"unpaid_labour_in_csquare", 
          "personnelcosts_in_ctry_level6_csquare",
           "KwFishingdays_aer_in_ctry_level6_csquare", "lpue_csquare_fdi_kgperfday", "lpue_csquare_aer_kgperdayatsea")
  some_cols_to_sweep <- some_cols_to_sweep [some_cols_to_sweep %in% colnames(master_dt)]         

  nby_per_fs <- distr_with_grid[,.(nby=length(unique(year))),by=fs]
  distr_with_grid <- merge(distr_with_grid, nby_per_fs, by="fs")
  
  # do a sweep() in advance of the sum to come to result into an average
  distr_with_grid <- data.table(cbind.data.frame(
                                distr_with_grid[,c("idx", "idy",  "grID")], 
                                 sweep(distr_with_grid[, ..some_cols_to_sweep], 1, distr_with_grid[, nby], FUN="/")))  
  
  some_cols_to_sum <- c("fditotfishdays", "daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
             "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare",
          "other_income_in_csquare" ,"unpaid_labour_in_csquare", 
          "personnelcosts_in_ctry_level6_csquare",
           "KwFishingdays_aer_in_ctry_level6_csquare")
  distr_with_grid_1 <- NULL 
  if(length(some_cols_to_sum)>0) 
     {
     distr_with_grid_1 <- 
       distr_with_grid[,lapply(.SD, sum, na.rm=TRUE),
         .SDcols=some_cols_to_sum,
          keyby=c("idx", "idy",  "grID")]
     }
  some_cols_to_average <- c("lpue_csquare_fdi_kgperfday", "lpue_csquare_aer_kgperdayatsea")
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

     

##------------------------------------
## CALLS------------------------------
  years <- c(2018:2021)
  
  # note that if costs infos are missing for some years and fs, an average is done anyway from the informed years
  
  # first, plot the FDI
  load(file=file.path(ROutputPathToDatasets, "FDI_2018_2021_in_NAO_before_merging.RData"))  # fdi_all_y 
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI", "all_metiers", "2018_2021")  
  nby <- length(unique(fdi_all_y$year))
  dir.create(filepath, recursive=TRUE)
  fdi_all_y$fs <- fdi_all_y$metier
  my_raster_export(master_dt=fdi_all_y, slave_dt=fdi_all_y)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$fditotfishdays[], na.rm=TRUE)

  
  # all metiers, all years
  nby <- length(unique(distr_allsp$year))
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", "all_metiers", "2018_2021")  
  dir.create(filepath, recursive=TRUE)
  my_raster_export(master_dt=distr_allsp, slave_dt=distr_allsp)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$fditotfishdays[], na.rm=TRUE)
  
  # all metiers, all years 2018-2021
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", "all_metiers", "2018_2021")  
  dir.create(filepath, recursive=TRUE)
  some_yrs <- c("2018", "2019", "2020", "2021")
  distr_this_y <- distr_allsp[distr_allsp$year %in% some_yrs, ] # test on this year
  my_raster_export(master_dt=distr_allsp, slave_dt=distr_this_y)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$fditotfishdays[], na.rm=TRUE)

  # all BOTTOM metiers, all years
  a_df                  <- as.data.frame(distr_allsp)
  nby                   <- length(unique(distr_allsp$year))
  filepath              <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", "all_bottom_metiers", "2018_2021")  
  dir.create(filepath, recursive=TRUE)
  all_fs                <- unique(a_df$fs)
  #fss                   <- all_fs[c(grep("DTS", all_fs), grep("TBB", all_fs), grep("DRB", all_fs), grep("HOK", all_fs), grep("DFN", all_fs))] # bottom contacting gears only
  fss                   <- all_fs[c(grep("DTS", all_fs), grep("TBB", all_fs), grep("DRB", all_fs))] # bottom contacting gears only
  distr_these_fs        <- data.table(a_df[a_df$fs %in% fss,])
  my_raster_export(master_dt=distr_allsp, slave_dt=distr_these_fs)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$fditotfishdays[], na.rm=TRUE)

  # or all BOTTOM metiers, per y
  for (y in years)
  {
  distr_this_fs_this_y <- distr_allsp[distr_allsp$year %in% y, ] # test on this year
  # all BOTTOM metiers, all years
  a_df                  <- as.data.frame(distr_this_fs_this_y)
  nby                   <- length(unique(distr_this_fs_this_y$year))
  filepath              <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", "all_bottom_metiers", y)  
  dir.create(filepath, recursive=TRUE)
  all_fs                <- unique(a_df$fs)
  #fss                   <- all_fs[c(grep("DTS", all_fs), grep("TBB", all_fs), grep("DRB", all_fs), grep("HOK", all_fs), grep("DFN", all_fs))] # bottom contacting gears only
  fss                   <- all_fs[c(grep("DTS", all_fs), grep("TBB", all_fs), grep("DRB", all_fs))] # bottom contacting gears only
  distr_these_fs        <- data.table(a_df[a_df$fs %in% fss,])
  my_raster_export(master_dt=distr_allsp, slave_dt=distr_these_fs)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$fditotfishdays[], na.rm=TRUE)
  }

  
  # or all metiers, per y
  for (y in years)
  {
  distr_this_fs_this_y <- distr_allsp[distr_allsp$year %in% y, ] # test on this year
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", "all_metiers", y)  
  dir.create(filepath, recursive=TRUE)
  my_raster_export(master_dt=distr_allsp, slave_dt=distr_this_fs_this_y)
  # check: 
  er <- try(   {
     dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
     plot(dd)
   }, silent=TRUE)
  if(class(er)=="try-error"){                                       
     cat(paste0("no rast for this one...","\n")) 
   }
  } # end y

  # or per metier, all years
  a_df                 <- as.data.frame(distr_allsp)
  for (fs in unlist(unique(distr_allsp[,"fs"])))
  {
     cat(paste0(fs,"\n")) 
     distr_this_fs        <- data.table(a_df[a_df$fs %in% fs,])
     #distr_this_fs_this_y <- distr_this_fs[distr_this_fs$year=="2019",]
     nby                  <- length(unique(distr_allsp$year))
     filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", fs, "2018_2021")
     dir.create(filepath, recursive=TRUE)
     my_raster_export(master_dt=distr_allsp, slave_dt=distr_this_fs)
     # check: 
    er <- try(   {
     dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
     plot(dd)
     }, silent=TRUE)
    if(class(er)=="try-error"){                                       
     cat(paste0("no rast for this one...","\n")) 
   }
  }
     
  # or per metier, per y
  a_df                 <- as.data.frame(distr_allsp)
  for (y in years)
  {
  cat(paste0(y, "\n"))
  for (fs in unlist(unique(distr_allsp[,"fs"])))
  {
     cat(paste0(fs,"\n")) 
     distr_this_fs        <- data.table(a_df[a_df$fs %in% fs,])
     distr_this_fs_this_y <- distr_this_fs[distr_this_fs$year==y,]
     filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", fs, y)
     dir.create(filepath, recursive=TRUE)
     if(nrow(distr_this_fs_this_y)!=0){
        my_raster_export(master_dt=distr_allsp, slave_dt=distr_this_fs_this_y)
        # check: 
         er <- try(   {
           dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
          plot(dd)
         }, silent=TRUE)
       if(class(er)=="try-error"){                                       
          cat(paste0("no rast for this one...","\n")) 
       }
    }
  } # end fs
  } # end y
 
 # check    
 # =>    
 # the idea from there is to use these raster layers to displace effort per metier (based on FDI effort, LPUE and costs)
 #  and recompute the GVA (based on AER variables). Using AER var is required to make the estimates consistent with the AER report,
 # but is not a good idea to displace effort because FDI effort is better spatially disaggregated.... 
  # ...and then recompute the GVA
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_FDI_AER", "all_metiers", "2018_2021")
  dd <- rast(file.path(filepath, "spatRaster.tif")) # an average over years...
  dd$GVA <-  (an(dd$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(dd$value_aer_in_ctry_level6_csquare)/an(dd$landings_aer_in_ctry_level6_csquare))) + 
                      an(dd$other_income_in_csquare) - # plus other income
                      an(dd$unpaid_labour_in_csquare) - an(dd$varcosts_in_ctry_level6_csquare) -an(dd$oth_non_var_costs_in_csquare) # minus var and fixed costs
 
  plot(dd, xlim=c(-10, 10), ylim=c(40,65))
  plot(dd$GVA, xlim=c(-10, 10), ylim=c(40,65))















































