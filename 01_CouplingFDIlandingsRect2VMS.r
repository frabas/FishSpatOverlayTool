
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# MERGE STECF FDI WITH VMS 
# AND DISPACH FDI LANDINGS 
# ON VMS C-SQUARE CELLS PER YEAR, COUNTRY AND METIER LEVEL6
# Author: Francois Bastardie (May 2023)
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 setwd(file.path("D:","FBA","DISPLACE_PAPERS","DraftsProjects","SEAWise","WP5","T 5.5 Fishable areas", "Software", "FishSpatOverlayTool"))

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
# READ STECF FDI DATA

# Disclaimer STECF FDI "Fisheries landings & effort: data by c-square" (https://data.jrc.ec.europa.eu/dataset/00ae6659-ddde-4314-a9da-717bb2e82582):
#The spatial dataset (years 2013-2021) published on this page is a subset of the data provided by EU Member States in the context of the DCF (Data Collection Framework) data call
#collecting Fisheries Dependent Information (FDI). The data provided by Member States during the 2022 FDI data call were analyzed by the Scientific, Technical and Economic Committee for Fisheries (STECF) Expert Working Group 22-10. Before accessing the data we strongly encourage to read the STECF report 22-10 (https://stecf.jrc.ec.europa.eu/reports). Disclaimer: Although the data published here have been assessed by STECF, the quality and completeness of the data are under the responsibility of the EU Member States.

# Note: no country info....(hence, the previous historical dataset set up during the “STECF fishing effort regime working groups” (not anymore updated since 2017) is still downloaded because contains the country information.
# Note: some record kept confidential in FDI, therefore there are some datagaps.....

# by c-square at 0.5 x 0.5 degrees of resolution??

#------------
# Read the FDI effort per rectangle
library(data.table)
filename          <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_effort_EU28.csv")
fdi_effort_rect   <- fread(filename, quote="\"")
#fdi_effort_rect  <- as.data.frame(fdi_effort_rect)

# a small fix and renaming
fdi_effort_rect$fditotfishdays <- as.numeric(gsub(",", ".", fdi_effort_rect$totfishdays))


#------------
# Read the FDI landings per rectangle per year (tonnes, and euros?) 
#(identical values for a given key is likely the outcome of an even dispatching ver c-square done by EU MS before submitting data to the JRC...)
#(...here we don´t care as we aggregate back to the ICES rectangle level)
filename <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_landings_2019_EU28.csv")
library(data.table)
fdi_land_rect_2019 <- fread(filename, quote="\"")

filename <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_landings_2020_EU28.csv")
library(data.table)
fdi_land_rect_2020 <- fread(filename, quote="\"")

filename <- file.path(RinputPath, "STECF_DATA","FDI_2022_data","spatial_landings_2021_EU28.csv")
library(data.table)
fdi_land_rect_2021 <- fread(filename, quote="\"")



#-----------------------------------
 #-----------------------------------
 #-----------------------------------
# HARMONISE FDI DATA METIER NAMES WITH VMS DATA 
 
translateMetiers <- function(x){
  x <- within(x,{  MetierL6[MetierL6 %in%   c("DRB_MOL_>=0_0_0","DRB_MOL_>0_0_0", "DRB_MOL_0_0_0")]            <- "DRB_MOL_0"
  MetierL6[MetierL6 %in%   c("FPO_CRU_>0_0_0","FPO_CRU_ALL_0_0")]                                              <- "FPO_CRU_0_0_0"
  MetierL6[MetierL6 %in%   c("GNS_CRU_50_70_0","GNS_CRU_60_79_0","GNS_CRU_90_99_0")]                           <- "GNS_CRU_50-99"
  MetierL6[MetierL6 %in%   c("GNS_CRU_120-219_0_0","GNS_CRU_120_219_0")]                                       <- "GNS_CRU_120-219"
  MetierL6[MetierL6 %in%   c("GNS_CRU_>=220_0","GNS_CRU_>=220_0_0")]                                           <- "GNS_CRU_>=220"
  MetierL6[MetierL6 %in%   c("GNS_DEF_0_40_0","GNS_DEF_10_30_0")]                                              <- "GNS_DEF_0-40"
  MetierL6[MetierL6 %in%   c("GNS_DEF_50_59_0","GNS_DEF_50_70_0","GNS_DEF_60_79_0")]                           <- "GNS_DEF_50-79"
  MetierL6[MetierL6 %in%   c("GNS_DEF_90-99","GNS_DEF_90-99_0_0","GNS_DEF_90-109_0_0")]                        <- "GNS_DEF_90-109"
  MetierL6[MetierL6 %in%   c("GNS_DEF_100-119_0_0","GNS_DEF_100_119_0")]                                       <- "GNS_DEF_100-119"
  MetierL6[MetierL6 %in%   c("GNS_DEF_120-219_0_0","GNS_DEF_120_219_0")]                                       <- "GNS_DEF_120-219"
  MetierL6[MetierL6 %in%   c("GNS_DEF_>=220_0","GNS_DEF_>=220_0_0")]                                           <- "GNS_DEF_>=220"
  MetierL6[MetierL6 %in%   c("GTR_DEF_0","GTR_DEF_0_0_0")]                                                     <- "GTR_DEF_0_0_0"
  MetierL6[MetierL6 %in%   c("GTR_DEF_50_59_0","GTR_DEF_50_70_0","GTR_DEF_60_79_0")]                           <- "GTR_DEF_50-79"
  MetierL6[MetierL6 %in%   c("GTR_DEF_90-99","GTR_DEF_90-99_0_0","GTR_DEF_90_99_0")]                           <- "GTR_DEF_90-99"
  MetierL6[MetierL6 %in%   c("GTR_DEF_120-219","GTR_DEF_120_219_0")]                                           <- "GTR_DEF_120-219"
  MetierL6[MetierL6 %in%   c("OTB_CEP_32_54_0","OTB_CEP_32_69_0","OTB_CEP_55_69_0")]                           <- "OTB_CEP_32-69" 
  MetierL6[MetierL6 %in%   c("OTB_CRU_>0_0_0","OTB_CRU_0_0_0","OTB_CRU_0")]                                    <- "OTB_CRU_0_0_0"
  MetierL6[MetierL6 %in%   c("OTB_CRU_16-31_0_0","OTB_CRU_16-31_0","OTB_CRU_16_31_0")]                         <- "OTB_CRU_16-31" 
  MetierL6[MetierL6 %in%   c("OTB_CRU_32-69_0_0","OTB_CRU_32-69_2_22", "OTB_CRU_32_54_0","OTB_CRU_32_69_0")]   <- "OTB_CRU_32-69" 
  MetierL6[MetierL6 %in%   c("OTB_CRU_70-89_2_35","OTB_CRU_70-99_0_0", "OTB_CRU_70-99_0",  "OTB_CRU_70_99_0")] <- "OTB_CRU_70-99" 
  MetierL6[MetierL6 %in%   c("OTB_CRU_100-119_0_0","OTB_CRU_100-119_0","OTB_CRU_100_119_0")]                   <- "OTB_CRU_100-119" 
  MetierL6[MetierL6 %in%   c("OTB_DEF_16-31_0_0","OTB_DEF_16_31_0")]                                           <- "OTB_DEF_16-31"
  MetierL6[MetierL6 %in%   c("OTB_DEF_32-69_0_0","OTB_DEF_32_54_0", "OTB_DEF_32_69_0","OTB_DEF_55_69_0")]      <- "OTB_DEF_32-69"
  MetierL6[MetierL6 %in%   c("OTB_DEF_>=70_0","OTB_DEF_>=70_0_0")]                                             <- "OTB_DEF_>=70"
  MetierL6[MetierL6 %in%   c("OTB_DEF_70-99_0_0","OTB_DEF_70-99_0", "OTB_DEF_70-89_0_0","OTB_DEF_70_99_0")]    <- "OTB_DEF_70-99"
  MetierL6[MetierL6 %in%   c("OTB_DEF_100-119_0_0","OTB_DEF_100_119_0")]                                       <- "OTB_DEF_100-119"
  MetierL6[MetierL6 %in%   c("OTB_DEF_>=105_0_0","OTB_DEF_>=105_1_110","OTB_DEF_>=105_1_120")]                 <- "OTB_DEF_>=105"
  MetierL6[MetierL6 %in%   c("OTB_DEF_130-219_0_0","OTB_DEF_130-279_0_0")]                                     <- "OTB_DEF_130-279"
  MetierL6[MetierL6 %in%   c("OTB_DEF_>=120_0_0","OTB_DEF_>=120_0", "OTB_DEF_>=130_0_0","OTB_DEF_130-279")]    <- "OTB_DEF_>=120"
  MetierL6[MetierL6 %in%   c("OTB_DWS_70-99_0_0","OTB_DWS_70_99_0")]                                           <- "OTB_DWS_70-99"  
  MetierL6[MetierL6 %in%   c("OTB_DWS_100-119_0_0","OTB_DWS_100_119_0")]                                       <- "OTB_DWS_100-119" 
  MetierL6[MetierL6 %in%   c("OTB_DWS_>=120_0","OTB_DWS_>=120_0_0")]                                           <- "OTB_DWS_>=120" 
  MetierL6[MetierL6 %in%   c("OTB_MCD_70-99","OTB_MCD_70-99_0_0")]                                             <- "OTB_MCD_70-99" 
  MetierL6[MetierL6 %in%   c("OTB_MOL_32-69_0_0","OTB_MOL_32_69_0")]                                           <- "OTB_MOL_32-69" 
  MetierL6[MetierL6 %in%   c("OTB_MOL_70-99_0_0","OTB_MOL_70_99_0")]                                           <- "OTB_MOL_70-99" 
  MetierL6[MetierL6 %in%   c("OTB_MOL_>=120_0_0","OTB_MOL_100-119_0_0")]                                       <- "OTB_MOL_>=100"
  MetierL6[MetierL6 %in%   c("OTB_SPF_16-31_0_0","OTB_SPF_16_31_0")]                                           <- "OTB_SPF_16-31"
  MetierL6[MetierL6 %in%   c("OTB_SPF_32-69_0_0","OTB_SPF_32_54_0","OTB_SPF_32_69_0")]                         <- "OTB_SPF_32-69"
  MetierL6[MetierL6 %in%   c("OTB_SPF_70-99_0_0","OTB_SPF_70_99_0")]                                           <- "OTB_SPF_70-99" 
  MetierL6[MetierL6 %in%   c("OTB_SPF_>=120_0","OTB_SPF_>=120_0_0")]                                           <- "OTB_SPF_>=120" 
  MetierL6[MetierL6 %in%   c("OTM_DEF_16-31_0_0","OTM_DEF_16_31_0" )]                                          <- "OTM_DEF_16-31"
  MetierL6[MetierL6 %in%   c("OTM_DEF_>=105_1_120","OTM_DEF_>=120_0_0","OTM_DEF_100-119_0_0","OTM_DEF_100-129_0_0", "OTM_DEF_90-119_0_0" )] <- "OTM_DEF_>=90"
  MetierL6[MetierL6 %in%   c("OTM_SPF_16-31_0_0","OTM_SPF_16_31_0" )]                                          <- "OTM_SPF_16-31"
  MetierL6[MetierL6 %in%   c("OTT_CRU_70-99_0_0","OTT_CRU_70_99_0")]                                           <- "OTT_CRU_70-99"
  MetierL6[MetierL6 %in%   c("OTT_CRU_100-119_0_0","OTT_CRU_100_119_0")]                                       <- "OTT_CRU_100-119"
  MetierL6[MetierL6 %in%   c("OTT_DEF_32-69_0_0","OTT_DEF_32_54_0", "OTT_DEF_32_69_0","OTT_DEF_55_69_0")]      <- "OTT_DEF_32-69"
  MetierL6[MetierL6 %in%   c("OTT_DEF_70-99_0_0"," OTT_DEF_70_99_0")]                                          <- "OTT_DEF_70-999"
  MetierL6[MetierL6 %in%   c("OTT_DEF_100-119_0_0","OTT_DEF_100_119_0")]                                       <- "OTT_DEF_100-119"
  MetierL6[MetierL6 %in%   c("OTT_DEF_>=120_0","OTT_DEF_>=120_0_0")]                                           <- "OTT_DEF_>=120"
  MetierL6[MetierL6 %in%   c("OTM_SPF_32-104_0_0","OTM_SPF_32-54_0_0","OTM_SPF_32-69_0_0" )]                   <- "OTM_SPF_32-104"
  MetierL6[MetierL6 %in%   c("PTB_DEF_70-99_0_0","PTB_DEF_70_99_0" )]                                          <- "PTB_DEF_70-99"
  MetierL6[MetierL6 %in%   c("PTB_DEF_>=105_1_120","PTB_DEF_>=120_0_0","PTB_DEF_100-119_0_0","PTB_DEF_100-129_0_0" )] <- "PTB_DEF_>=100"
  MetierL6[MetierL6 %in%   c("PTB_SPF_32-104_0_0","PTB_SPF_32-69_0_0","PTB_SPF_32-89_0_0" )]                   <- "PTB_SPF_32-104"
  MetierL6[MetierL6 %in%   c("PTM_DEF_16-31_0_0","PTM_DEF_16-31_0_0" )]                                        <- "PTM_DEF_16-31"
  MetierL6[MetierL6 %in%   c("PTM_LPF_100-119_0_0","PTM_LPF_100_119_0" )]                                      <- "PTM_LPF_100-119"
  MetierL6[MetierL6 %in%   c("PTM_SPF_16-31_0_0","PTM_SPF_16_31_0")]                                           <- "PTM_SPF_16-31"
  MetierL6[MetierL6 %in%   c("PTM_SPF_32-104_0_0","PTM_SPF_32-69_0_0","PTM_SPF_32-89_0_0" )]                   <- "PTM_SPF_32-104"
  MetierL6[MetierL6 %in%   c("SDN_DEF_70-99_0_0","SDN_DEF_70_99_0")]                                           <- "SDN_DEF_70-99"
  MetierL6[MetierL6 %in%   c("SDN_DEF_100-119_0_0","SDN_DEF_100_119_0", "SDN_DEF_90-119_0_0" )]                <- "SDN_DEF_90-119"
  MetierL6[MetierL6 %in%   c("SDN_DEF_>=120_0","SDN_DEF_>=120_0_0")]                                           <- "SDN_DEF_>=120"
  MetierL6[MetierL6 %in%   c("SSC_DEF_32-69","SSC_DEF_32-69_0_0")]                                             <- "SSC_DEF_32-69"
  MetierL6[MetierL6 %in%   c("SSC_DEF_70-99","SSC_DEF_70-99_0_0")]                                             <- "SSC_DEF_70-99"
  MetierL6[MetierL6 %in%   c("SSC_DEF_100-119","SSC_DEF_100-119_0_0")]                                         <- "SSC_DEF_100-119"
  MetierL6[MetierL6 %in%   c("TBB_CRU_16-31","TBB_CRU_16-31_0_0" )]                                            <- "TBB_CRU_16-31"
  MetierL6[MetierL6 %in%   c("TBB_CRU_70-99_0_0","TBB_CRU_70_99_0" )]                                          <- "TBB_CRU_70-99"
  MetierL6[MetierL6 %in%   c("TBB_DEF_16-31_0_0","TBB_DEF_16_31_0" )]                                          <- "TBB_DEF_16-31"
  MetierL6[MetierL6 %in%   c("TBB_DEF_32-69_0_0","TBB_DEF_32_69_0" )]                                          <- "TBB_DEF_32-69"
  MetierL6[MetierL6 %in%   c("TBB_DEF_70-99_0_0","TBB_DEF_70-99_0_0", "TBB_DEF_70_99_0" )]                     <- "TBB_DEF_70-99"
  MetierL6[MetierL6 %in%   c("TBB_DEF_100-119","TBB_DEF_100-119_0_0", "TBB_DEF_100_119_0" )]                   <- "TBB_DEF_100-119"
  MetierL6[MetierL6 %in%   c("TBB_DEF_>=120","TBB_DEF_>=120_0_0" )]                                            <- "TBB_DEF_>=120"
  #remainder with very small usage of gears and MIS categories", either because low swpt areas, low n obs, or low fish hours
  MetierL6[MetierL6 %in%   c("MIS","LLD_ANA_0_0_0","LLS_DEF_0_0_0","LHP_FIF_0_0_0", "No_Matrix6",
                             "MIS_MIS_0_0_0", "PTB_DWS_0_0_0", "OTB_ANA_0_0_0", "DRB_CRU_0_0_0",
                             "FPO_MCD_0","OTB_CAT_>=70_0","PS_SPF_16-31_0_0", "PS_SPF_32-69_0_0",
                             "FPO_MOL_>0_0_0","TBB_CEP_0_0_0",
                             "TBB_MOL_0_0_0", "TBB_MOL_70_99_0",
                             "PTB_CRU_>=120_0_0", "PTB_CRU_70-99_0_0","PTB_CRU_90-119_0_0","GNS_CRU_>0_0_0",
                             "FPN_SPF_32-54_0_0","OTB_FWS_>0_0_0","OTM_DWS_0_0_0", "LTL_SPF_0_0_0",
                             "GNS_SPF_>=100_0", "LTL_DEF_0_0_0", "LHP_CEP_0_0_0","GNS_LPF_>=100_0",
                             "FPO_FIF_0_0_0", 
                             "GND_SPF_0_0_0", "GND_SPF_120_219_0", "GND_SPF_50_70_0", 
                             "LHP_SPF_0_0_0",
                             "GNS_CEP_0_0_0", "MIS_MDD_0_0_0", "SPR_DEF_0_0_0", "LHP_DEF_0_0_0",
                             "GND_DEF_>=100_0", "GND_DEF_120_219_0",
                             "LTL_LPF_0_0_0",
                             "LLD_DEF_0_0_0", 
                             "PTM_CEP_>=70_0", "PTM_CEP_100_119_0", "PTM_CEP_70_99_0",                 
                             "GTN_DEF_>=100_0", "GTN_DEF_>=220_0", "GTN_DEF_0_0_0", "GTN_DEF_120_219_0",
                             "LHP_LPF_0_0_0",
                             "TBB_CRU_<16_0_0","TBB_CRU_0_0_0", "TBB_CRU_70-99",
                             "OTM_LPF_>=120_0", "OTM_LPF_>=70_0", "OTM_LPF_0_0_0", "OTM_LPF_100_119_0",
                             "OTM_LPF_16-31_0_0", "OTM_LPF_70_99_0",     
                             "OTT_DWS_>=70_0","OTT_DWS_100_119_0"                                             )] <-  "MIS_MIS"
  
  })
  return(x)
}

#make sure all metiers are translated combined when obvious overlap and low levels of obs
fdi_land_rect_2019$MetierL6 <- fdi_land_rect_2019$metier
fdi_land_rect_2019 <- translateMetiers(fdi_land_rect_2019)

fdi_land_rect_2020$MetierL6 <- fdi_land_rect_2020$metier
fdi_land_rect_2020 <- translateMetiers(fdi_land_rect_2020)

fdi_land_rect_2021$MetierL6 <- fdi_land_rect_2021$metier
fdi_land_rect_2021 <- translateMetiers(fdi_land_rect_2021)



#-----------------------------------
#-----------------------------------
#-----------------------------------
# READ VMS SEAWISE DATA

 load(file=file.path(RinputPath,"VMS_DATA", "prodT53all_agg_FB.Rdata"))
 head(prodT53all_agg)
 
 #unique(prodT53all_agg$Year)
 #[1] 2011 2010 2009 2013 2015 2012 2016 2018 2014 2019 2017 2020 2021



 library(vmstools)
 prodT53all_agg$icesname         <- vmstools::ICESrectangle(prodT53all_agg)

 prodT53all_agg$quarter          <- factor(prodT53all_agg$Month) # init
 levels(prodT53all_agg$quarter)  <- c(1,1,1,2,2,2,3,3,3,4,4,4)
 prodT53all_agg$quarter          <- as.numeric(prodT53all_agg$quarter)

 prodT53all_agg                  <- data.table(prodT53all_agg)
 

 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 
 
 # Restrict to the set of ICES rectangles covered in the VMS data 
 # (not really useful, but just to be sure to understand there is no leaks before the merging)
 vmsrectangles <- unique(prodT53all_agg$icesname)
       
 fdi_land_rect_2019_this_region <- fdi_land_rect_2019[fdi_land_rect_2019$icesname %in% vmsrectangles,]
 fdi_land_rect_2020_this_region <- fdi_land_rect_2020[fdi_land_rect_2020$icesname %in% vmsrectangles,]
 fdi_land_rect_2021_this_region <- fdi_land_rect_2021[fdi_land_rect_2021$icesname %in% vmsrectangles,]

 # interlude to show that STECF FDI Effort Rect is at the ICES rectangle resolution
 if(FALSE){
  # retrieve the coord
 library(vmstools)

 a_var <- fdi_land_rect_2019_this_region[, .(landings_in_cell=sum(totwghtlandg)), by=c("cscode", "rectangle_lat", "rectangle_lon", "species")]
 a_var      <- as.data.frame(a_var)
 

 resy <- 0.5
 resx <- 1
 cutbreakval            <- c(-1,0,20,40,80,160,320,3000000)  # kilos
 colyellowred           <- terrain.colors(length(cutbreakval))

 a_species              <- "HER"
 cols                   <- c("white", colyellowred)[cut(unlist(an(a_var[a_var$species==a_species, "landings_in_cell"])), breaks=cutbreakval)]
 coord                  <- a_var[a_var$species==a_species, c("rectangle_lon", "rectangle_lat")]
 plot(coord, pch="")
 for (i in 1: nrow(coord)) rect(coord[i, "rectangle_lon"]-resx/2, coord[i,"rectangle_lat"]-resy/2, coord[i,"rectangle_lon"]+resx/2, coord[i,"rectangle_lat"]+resy/2, col=cols[i], border=FALSE)
 #=> looks quite a coarse resolution (i.e. ICES rect) compared to the coming final product, as expected...
}
 
         
 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 
 # forsee the merging at the metier Level5 because Level6 is likely inconsistent between FDI and VMS (especially mesh size definitions....)
    
 # check Level6
 dd                                                          <- fdi_land_rect_2021_this_region
 dd2                                                         <- prodT53all_agg[prodT53all_agg$Year=="2021",]
 fdi_key_met                                                 <- paste(dd$year, dd$quarter, dd$MetierL6, sep="_") # fdi
 vms_key_met                                                 <- paste(dd2$Year, dd2$quarter, dd2$MetierL6, sep="_") # vms
 not_in_fdi_keys <- vms_key_met[!vms_key_met %in% fdi_key_met]
 not_in_fdi <- unique(not_in_fdi_keys)
 not_in_vms_keys <- fdi_key_met[!fdi_key_met %in% vms_key_met]
 not_in_vms <- unique(not_in_vms_keys)
 #=> inconsistent.....
 
 # ...then, retrieve a level 5
 fdi_land_rect_2019_this_region$target <- sapply(c(fdi_land_rect_2019_this_region$MetierL6), function (x)  {strsplit(x, split="_")[[1]][2]})
 fdi_land_rect_2019_this_region$Level5 <- paste(fdi_land_rect_2019_this_region$gear_type, fdi_land_rect_2019_this_region$target, sep="_") 

 fdi_land_rect_2020_this_region$target <- sapply(c(fdi_land_rect_2020_this_region$MetierL6), function (x)  {strsplit(x, split="_")[[1]][2]})
 fdi_land_rect_2020_this_region$Level5 <- paste(fdi_land_rect_2020_this_region$gear_type, fdi_land_rect_2020_this_region$target, sep="_") 

 fdi_land_rect_2021_this_region$target <- sapply(c(fdi_land_rect_2021_this_region$MetierL6), function (x)  {strsplit(x, split="_")[[1]][2]})
 fdi_land_rect_2021_this_region$Level5 <- paste(fdi_land_rect_2021_this_region$gear_type, fdi_land_rect_2021_this_region$target, sep="_") 

 prodT53all_agg$gear_type                  <- substr(c(prodT53all_agg$MetierL6), 1, 3)
 prodT53all_agg$target                     <- substr(c(prodT53all_agg$MetierL6), 5, 7) # sapply(c(prodT53all_agg$MetierL6), function (x)  {strsplit(x, split="_")[[1]][2]})
 prodT53all_agg$Level5                     <- paste(prodT53all_agg$gear_type, prodT53all_agg$target, sep="_") 


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


# aggregate FDI at Level5
fdi_land_rect_2019_agg_this_region <- fdi_land_rect_2019_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "icesname",  "Level5", "species", "vessel_length")]

fdi_land_rect_2020_agg_this_region <- fdi_land_rect_2020_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "icesname",  "Level5", "species", "vessel_length")]

fdi_land_rect_2021_agg_this_region <- fdi_land_rect_2021_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "icesname",  "Level5", "species", "vessel_length")]


# aggregate FDI for keeping main species only (with a percent threshold)
main_species_2019 <- fdi_land_rect_2019_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("species")]   [order(-totvallandg)] 
main_species_2020 <- fdi_land_rect_2020_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("species")]   [order(-totvallandg)] 
main_species_2021 <- fdi_land_rect_2021_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("species")]   [order(-totvallandg)] 
main_species <- rbind(main_species_2019, main_species_2020, main_species_2021)
main_species <- main_species[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("species")]   [order(-totvallandg)] 
main_species$percent <- cumsum(main_species$totwghtlandg/sum(main_species$totwghtlandg))*100                                             
oth_species <- unique(unlist(c(main_species[main_species$percent>90, "species"])))

fdi_land_rect_2019_agg_this_region[fdi_land_rect_2019_agg_this_region$species %in% oth_species, "species"] <- "OTH" 
fdi_land_rect_2020_agg_this_region[fdi_land_rect_2020_agg_this_region$species %in% oth_species, "species"] <- "OTH" 
fdi_land_rect_2021_agg_this_region[fdi_land_rect_2021_agg_this_region$species %in% oth_species, "species"] <- "OTH" 




fdi_land_rect_2019_agg_this_region <- fdi_land_rect_2019_agg_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "icesname",  "Level5", "species", "vessel_length")]

fdi_land_rect_2020_agg_this_region <- fdi_land_rect_2020_agg_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "icesname",  "Level5", "species", "vessel_length")]

fdi_land_rect_2021_agg_this_region <- fdi_land_rect_2021_agg_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "icesname",  "Level5", "species", "vessel_length")]




# check tot
fdi_land_rect_2019_agg_this_region[, .(fdi_land_rect_2019_agg_this_region = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year")]
fdi_land_rect_2020_agg_this_region[, .(fdi_land_rect_2020_agg_this_region = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year")]
fdi_land_rect_2021_agg_this_region[, .(fdi_land_rect_2021_agg_this_region = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year")]

# concatenate
fdi_land_rect_agg_this_region <- rbind(fdi_land_rect_2019_agg_this_region, fdi_land_rect_2020_agg_this_region, fdi_land_rect_2021_agg_this_region)

# interlude
#save(fdi_land_rect_agg_this_region,  file=file.path(file.path(getwd(), "OUTCOME_DATASETS", "fdi_land_rect_agg_this_region.RData")))
if(FALSE){ 
load(file=file.path(file.path(getwd(), "OUTCOME_DATASETS", "fdi_land_rect_agg_this_region.RData")))   # fdi_land_rect_agg_this_region
 
 # retrieve the coord
 library(vmstools)
 fdi_land_rect_agg_this_region         <- cbind(fdi_land_rect_agg_this_region, vmstools::ICESrectangle2LonLat(fdi_land_rect_agg_this_region$icesname,  midpoint = TRUE) )

 a_var <- fdi_land_rect_agg_this_region[, .(landings_in_cell=sum(totwghtlandg)), by=c("icesname", "SI_LATI", "SI_LONG", "species")]
 a_var      <- as.data.frame(a_var)
 

 resy <-  diff(unique(a_var$SI_LATI)[order(unique(a_var$SI_LATI))])  [2]
 resx <-  diff(unique(a_var$SI_LONG)[order(unique(a_var$SI_LONG))])  [2]
 cutbreakval            <- c(-1,0,20,40,80,160,320,3000000)  # kilos
 colyellowred           <- terrain.colors(length(cutbreakval))

 a_species              <- "HER"
 cols                   <- c("white", colyellowred)[cut(unlist(an(a_var[a_var$species==a_species, "landings_in_cell"])), breaks=cutbreakval)]
 coord                  <- a_var[a_var$species==a_species, c("SI_LONG", "SI_LATI")]
 plot(coord, pch="")
 for (i in 1: nrow(coord)) rect(coord[i, "SI_LONG"]-resx/2, coord[i,"SI_LATI"]-resy/2, coord[i,"SI_LONG"]+resx/2, coord[i,"SI_LATI"]+resy/2, col=cols[i], border=FALSE)
 #=> looks quite a coarse resolution (i.e. ICES rect) compared to the coming final product, as expected...
}



# aggregate VMS at Level5 (caution: all y here)
vms <- prodT53all_agg[, .(FishingHour = sum(FishingHour, na.rm=T),
                                                 kWFishingHour = sum(kWFishingHour, na.rm=T)),
                                             by=c("Year", "quarter", "icesname",  "Level5", "Csquare", "SI_LATI",   "SI_LONG", "VesselLengthRange")]
rm(prodT53all_agg)
gc(full=TRUE)


 #-----------------------------------
 #-----------------------------------
 #-----------------------------------

 # add primary keys before merging - adapt the key on the fly to avoid loosing effort or landings....
 
   # ROBUST MERGING: catch for leaks and adapt the key on the fly 
   fdi_all_y <- NULL
   vms_all_y <- NULL
   tracked_leaks <- NULL
   for(y in as.character(2019:2021))
   {
     # Add a default key (i.e. year-quarter-Level5-vessellength-icesrect)
      vms_y                   <- vms[Year==y,]
      vms_y$key               <- paste(vms_y$Year, vms_y$quarter, vms_y$Level5, vms_y$VesselLengthRange,  vms_y$icesname, sep="_")  # full key on vms
      fdi_y                   <- fdi_land_rect_agg_this_region[year==y,]
      fdi_y$key               <- paste(fdi_y$year, fdi_y$quarter, fdi_y$Level5, fdi_y$vessel_length,  fdi_y$icesname, sep="_") # full key on fdi
      fdi_y$chunk <- 0
      vms_y$chunk <- 0
      m1                      <- unique(vms_y$key)
      m2                      <- unique(fdi_y$key)
     vms_y_met_not_in_fdi     <- m1[!m1 %in% m2]
     vms_y_met_in_fdi         <- m1[m1 %in% m2]
     fdi_y_leftover           <- fdi_y[!fdi_y$key %in% vms_y_met_in_fdi,]
     vms_y_leftover           <- vms_y[!vms_y$key %in% vms_y_met_in_fdi,]
     fdi_y_main               <- fdi_y[fdi_y$key %in% vms_y_met_in_fdi,]
     vms_y_main               <- vms_y[vms_y$key %in% vms_y_met_in_fdi,]
     fdi_y_main$chunk       <- 1 # coding
     vms_y_main$chunk       <- 1  # coding
     # repeat with a less constraining key (i.e. year-quarter-Level5) to catch the left-over records:
     if(nrow(fdi_y_leftover)!=0 && nrow(vms_y_leftover)!=0)
     {
      fdi_y_leftover$key       <- paste(fdi_y_leftover$year, fdi_y_leftover$quarter, fdi_y_leftover$Level5,  sep="_") # fdi
      vms_y_leftover$key       <- paste(vms_y_leftover$Year, vms_y_leftover$quarter, vms_y_leftover$Level5, sep="_")  # vms
      m1                       <- unique(vms_y_leftover$key)
      m2                       <- unique(fdi_y_leftover$key)
      vms_y_met_not_in_fdi     <- m1[!m1 %in% m2]
      vms_y_met_in_fdi         <- m1[m1 %in% m2]
      fdi_y_leftover2          <- fdi_y_leftover[!fdi_y_leftover$key %in% vms_y_met_in_fdi,]
      vms_y_leftover2          <- vms_y_leftover[!vms_y_leftover$key %in% vms_y_met_in_fdi,]
      fdi_y_leftover           <- fdi_y_leftover[fdi_y_leftover$key %in% vms_y_met_in_fdi,]
      vms_y_leftover           <- vms_y_leftover[vms_y_leftover$key %in% vms_y_met_in_fdi,]
      #destroy the sub_reg dim in fdi
      fdi_y_leftover <- fdi_y_leftover[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("totwghtlandg","totvallandg"),
                                   by=c("year", "quarter", "species", "Level5", "key")]
      fdi_y_leftover$chunk       <- 2 # coding
      #destroy the sub_reg dim in vms
      vms_y_leftover      <- vms_y_leftover[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("FishingHour", "kWFishingHour"),
                                   by=c("Year", "quarter", "Csquare", "Level5", "key")]    
      vms_y_leftover$chunk <- 2  # coding      
      if(nrow(fdi_y_leftover2))  warning("First attempt: There FDI records left here...")
      if(nrow(vms_y_leftover2)) warning("First attempt: There VMS records left here...")
    
       # repeat with a less constraining key (i.e. year-quarter-species) to catch the left-over records:
      if(nrow(fdi_y_leftover2)!=0 && nrow(vms_y_leftover2)!=0){
        fdi_y_leftover2$key       <- paste(fdi_y_leftover2$year, fdi_y_leftover2$quarter, fdi_y_leftover2$vessel_length,  sep="_") # fdi
        vms_y_leftover2$key       <- paste(vms_y_leftover2$Year, vms_y_leftover2$quarter, vms_y_leftover2$VesselLengthRange,  sep="_")  # vms
        m1                       <- unique(vms_y_leftover2$key)
        m2                       <- unique(fdi_y_leftover2$key)
        vms_y_met_not_in_fdi     <- m1[!m1 %in% m2]
        vms_y_met_in_fdi         <- m1[m1 %in% m2]
        fdi_y_leftover3          <- fdi_y_leftover2[!fdi_y_leftover2$key %in% vms_y_met_in_fdi,]
        vms_y_leftover3          <- vms_y_leftover2[!vms_y_leftover2$key %in% vms_y_met_in_fdi,]
        fdi_y_leftover2           <- fdi_y_leftover2[fdi_y_leftover2$key %in% vms_y_met_in_fdi,]
        vms_y_leftover2           <- vms_y_leftover2[vms_y_leftover2$key %in% vms_y_met_in_fdi,]
        #destroy the sub_reg and fishing_tech dims in aer
        fdi_y_leftover2 <- fdi_y_leftover2[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("totwghtlandg","totvallandg"),
                                   by=c("year", "quarter", "species", "vessel_length", "key")]
        fdi_y_leftover2$chunk       <- 3 # coding
        #destroy the sub_reg dim in vms
        vms_y_leftover2      <- vms_y_leftover2[,lapply(.SD, sum, na.rm=TRUE), 
                                   .SDcols=c("FishingHour", "kWFishingHour"),
                                   by=c("Year", "quarter", "Csquare","VesselLengthRange", "key")]    
        vms_y_leftover2$chunk <- 3  # coding      
        if(nrow(fdi_y_leftover3))  warning("Second attempt: There FDI records left here...")  # lost
        if(nrow(vms_y_leftover3)) warning("Second attempt: There VMS records left here...")   # lost
      }
     } 
    
     # aggregate and reconstruct from parts to comply with fdi_y format before rbind
    fdi_y_main <- fdi_y_main[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "icesname", "quarter", "species", "vessel_length", "Level5", "key", "chunk")]
    fdi_y_leftover <- fdi_y_leftover[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "species", "Level5", "key", "chunk")]
    fdi_y_leftover2 <- fdi_y_leftover2[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year", "quarter", "vessel_length", "species", "key", "chunk")]  
    
    
    # bind # fill=TRUE to add missing columns and fill out with NAs 
    fdi_left <- rbind(fdi_y_main, fdi_y_leftover, fdi_y_leftover2, fill=TRUE)   
    vms_left <- rbind(vms_y_main, vms_y_leftover, vms_y_leftover2, fill=TRUE)
   
   
     # document effort leak this y  
    d <- function(x) as.data.frame(x)
    dd1 <-rbind.data.frame(
    "init"=d(vms_y[  ,.(vmstotfishhours=sum(FishingHour )),]), # initial tot eff vms met in vms
    "year, quarter, vessel_length, Level5"=d(vms_y_main[  ,.(vmstotfishhours =sum(FishingHour )),]),        #  year, vessel_length, fishing_tech, sub_reg  # full key 
    "year, quarter, Level5"=d(vms_y_leftover[  ,.(vmstotfishhours =sum(FishingHour )),]),    #  year, vessel_length, fishing_tech
    "year, quarter, vessel_length"=d(vms_y_leftover2[  ,.(vmstotfishhours =sum(FishingHour )),]),   #  year, vessel_length
    "unfortunate lost"=d(vms_y_leftover3[  ,.(vmstotfishhours =sum(FishingHour )),]),   #  the remaining: not matched...
    "finally left"=d(vms_left[  ,.(vmstotfishhours =sum(FishingHour )),]) # tot eff vms left
    )

    # document landings leak this y
    d <- function(x) as.data.frame(x)
    dd2 <-rbind.data.frame(
    "init"=d(fdi_y[  ,.(totwghtlandg=sum(an(totwghtlandg))),]), # initial tot in vms
    "year, quarter, vessel_length, Level5"=d(fdi_y_main[  ,.(totwghtlandg=sum(an(totwghtlandg))),]),        #  year, vessel_length, fishing_tech, sub_reg  # full key  
    "year, quarter, Level5"=d(fdi_y_leftover[  ,.(totwghtlandg=sum(an(totwghtlandg))),]),    #  year, vessel_length, fishing_tech 
    "year, quarter, vessel_length"=d(fdi_y_leftover2[  ,.(totwghtlandg=sum(an(totwghtlandg))),]),   #  year, vessel_length 
    "unfortunate lost"=d(fdi_y_leftover3[  ,.(totwghtlandg=sum(an(totwghtlandg))),]),   #  the remaining: not matched... 
    "finally left"=d(fdi_left[  ,.(totwghtlandg=sum(an(totwghtlandg))),]) # tot in vms left
    )
   
    tracked_leaks <- rbind.data.frame (tracked_leaks, cbind.data.frame(y, dd1, dd2))

   
   fdi_all_y <- rbind(fdi_all_y, fdi_left)
   vms_all_y <- rbind(vms_all_y, vms_left)
   
   } # end y
   
 
 
# check 
fdi_land_rect_agg_this_region[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year")]  
fdi_all_y[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year")]  
fdi_all_y[, .(totwghtlandg = sum(totwghtlandg, na.rm=T),
                                                 totvallandg = sum(totvallandg, na.rm=T)),
                                             by=c("year",  "chunk")]  
    
 
 
 #-----------------------------------
 #-----------------------------------
 #-----------------------------------
 # MERGE AND DISPACH LANDINGS ON VMS CELLS PER YEAR, QUARTER, AND METIER

 
 #...and then do the merging
 dispatched_landings <- list()

 for (y in as.character(c(2019:2021)))
 {
   cat(paste("y", y, "\n"))
  
   # landings
   fdi_y                                      <-   fdi_all_y[fdi_all_y$year==y,]
   fdi_y[,.(tot_tons=sum(totwghtlandg)),] # check 
   # effort
   vms_y                                      <-  vms_all_y[vms_all_y$Year==y,]
   vms_y[,.(toteff=sum(FishingHour)),] # check 
   
   # get a share_effort
   sum_effort_y                            <-  vms_y[, .(totFishingHour = sum(FishingHour, na.rm=T)), by=c("Year", "quarter", "key")]  # removing the c-square dimension here...
   sum_effort_y[,.(tot_effort=sum(totFishingHour)),] # check 
   vms_y_e                      <-  merge(vms_y, sum_effort_y, by=c("Year", "quarter", "key"))
   vms_y_e$share_effort         <-  vms_y_e$FishingHour / vms_y_e$totFishingHour # for dispatching depending on the contribution of that cell to the total effort in that cell
   # check
   vms_y_e[,(sum(share_effort)), by=key]
   c(vms_y_e[,(sum(share_effort)), by=key][,2]) # should return only 1s
   vms_y_e[,.(toteff=sum(FishingHour)),] # check 
   vms_y_e[vms_y_e$chunk==1,][,.(toteff=sum(FishingHour)),] # check 
   
   # a check column: "md5" should be equal to the sum at the key level i.e. "FishingHour"
   vms_y_e$md5 <- vms_y_e$totFishingHour * vms_y_e$share_effort  
   vms_y_e[,.(tot_totfishhours=sum(FishingHour)),] # check 
 
   
   # merge (by chunk, to avoid memory issue)
   keys <- unlist(c(unique(fdi_y[fdi_y$chunk==1, "key"])))  # needed for the trick to chunk...
   idx  <- c(floor(seq(1,length(keys),by=length(keys)/3)),length(keys))
   chunk1_1                                     <- merge(fdi_y[fdi_y$chunk==1 & fdi_y$key %in% keys[1:idx[2]] ,], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_2                                     <- merge(fdi_y[fdi_y$chunk==1 & fdi_y$key %in% keys[idx[2]:idx[3]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk1_3                                     <- merge(fdi_y[fdi_y$chunk==1 & fdi_y$key %in% keys[idx[3]:idx[4]], ], vms_y_e[vms_y_e$chunk==1,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk2                                       <- merge(fdi_y[fdi_y$chunk==2,], vms_y_e[vms_y_e$chunk==2,], by.x="key", by.y="key", allow.cartesian=TRUE)
   chunk3                                       <- merge(fdi_y[fdi_y$chunk==3,], vms_y_e[vms_y_e$chunk==3,], by.x="key", by.y="key", allow.cartesian=TRUE)
   merged <- rbind(chunk1_1, chunk1_2, chunk1_3, chunk2, chunk3)
   gc(full=TRUE)
   
   # check after
   cc <- unique(merged, by=c("key","species")) # check removing duplicated lines 
   cc [,.(tot_tons=sum(totwghtlandg)),] # check 
   cc2 <- unique(merged, by=c("key", "Csquare")) # check removing duplicated lines 
   cc2[,.(toteff=sum(FishingHour)),] # check 
  
   # a bit a cleaning
   merged <- merged[, colnames(merged) [grepl(".y", colnames(merged), fixed=TRUE)]:=NULL ]
   colnames(merged) <- gsub(".x", "", colnames(merged), fixed=TRUE) 
 
  
   # dispatch
   merged$landings_in_cell <- merged$totwghtlandg * merged$share_effort  # dispatch on cell per species
   merged[,.(tot_tons=sum(landings_in_cell)),] # check 
   cc3 <- unique(merged, by=c("key", "Csquare")) # check removing duplicated lines 
   cc3[,.(eff_hours=sum(FishingHour)),] # check 
   
   #compute LPUEs 
   merged$lpue <- merged$landings_in_cell*1000/merged$FishingHour  # kg per h
  
   dispatched_landings[[y]]                <- merged
   }

 # check 
 head(dispatched_landings[["2019"]])

 dispatched_landings_ally <- do.call("rbind", dispatched_landings)

 # save
 rm(merged); gc(reset=TRUE)
 save(dispatched_landings_ally, file=file.path(file.path(ROutputPathToDatasets, "dispatched_fdi_landings_using_VMS_ally.RData"))) 




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# Make rasters !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!standalone!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#


  setwd(file.path("D:","FBA","DISPLACE_PAPERS","DraftsProjects","SEAWise","WP5","T 5.5 Fishable areas", "Software", "FishSpatOverlayTool"))

  RinputPath  <- file.path(getwd(), "INPUT_DATASETS") 
  ROutputPathToDatasets <- file.path(getwd(), "OUTCOME_DATASETS")
  RoutputPath4LargeFiles <- file.path("E:", "SeaWise")

 # re-load 
  load(file=file.path(file.path(ROutputPathToDatasets, "dispatched_fdi_landings_using_VMS_ally.RData")))  # get dispatched_landings_ally 

  library(sf)
  library(raster)
  library(data.table)
  library(dplyr)
  library(vmstools)

  # retrieve some coords from the Csquare coding
   dd                               <- dispatched_landings_ally 
   dd                               <- dd[,c("SI_LATI","SI_LONG"):=NULL]
   sq                               <- cbind.data.frame(Csquare=unique(dd$Csquare), vmstools::CSquare2LonLat(unique(dd$Csquare), 0.05))
   dispatched_landings_ally         <- dplyr::left_join(dd, sq, by="Csquare")
   rm(dd) ; gc(reset=TRUE)

  # add a fs coding 
   dispatched_landings_ally$fs         <- paste0(dispatched_landings_ally$Level5, "_", dispatched_landings_ally$vessel_length)
   

if(FALSE){ 
 a_var <- dispatched_landings_ally[, .(landings_in_cell=sum(landings_in_cell)), by=c("Csquare", "SI_LATI", "SI_LONG", "species")]
 a_var      <- as.data.frame(a_var)
 

 resy <-  diff(unique(a_var$SI_LATI)[order(unique(a_var$SI_LATI))])  [2]
 resx <-  diff(unique(a_var$SI_LONG)[order(unique(a_var$SI_LONG))])  [2]
 cutbreakval            <- c(-1,0,20,40,80,160,320,3000000)  # kilos
 colyellowred           <- terrain.colors(length(cutbreakval))

 a_species              <- "HER"
 cols                   <- c("white", colyellowred)[cut(unlist(an(a_var[a_var$species==a_species, "landings_in_cell"])), breaks=cutbreakval)]
 coord                  <- a_var[a_var$species==a_species, c("SI_LONG", "SI_LATI")]
 plot(coord, pch="")
 for (i in 1: nrow(coord)) rect(coord[i, "SI_LONG"]-resx/2, coord[i,"SI_LATI"]-resy/2, coord[i,"SI_LONG"]+resx/2, coord[i,"SI_LATI"]+resy/2, col=cols[i], border=FALSE)
 #=> looks good....
 # then proceed with an more advanced plotting:
}

 
 
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# get the data.table linked to a master raster grid
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

      
     
my_raster_export <- function(master_dt, slave_dt, nby){     
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
  resx                          <-  diff(unique(master_dt$SI_LONG)[order(unique(master_dt$SI_LONG))])  [2]  # master distr giving the bbox
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
 
  ## aggregate per grID
  some_cols <- c("FishingHour", "landings_in_cell")
  some_cols <- some_cols [some_cols %in% colnames(master_dt)]         
  distr_with_grid_1 <- NULL
  if(length(some_cols)>0) distr_with_grid_1 <- 
     distr_with_grid[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=some_cols,
          keyby=c("idx", "idy",  "grID")]
  some_cols <- c("lpue")
  some_cols <- some_cols [some_cols %in% colnames(master_dt)]         
  distr_with_grid_2 <- NULL
  if(length(some_cols)>0) distr_with_grid_2 <- 
     distr_with_grid[,lapply(.SD, mean, na.rm=TRUE),
      .SDcols=some_cols,
          keyby=c("idx", "idy",  "grID")]
  distr_with_grid <- cbind(distr_with_grid_1, distr_with_grid_2[, -c(1:3)])

  # assign some values to the grid
  # and divide by nby if several years in input...
  for(a_var in colnames(distr_with_grid[,-(1:3)])){
     grdtable[[a_var]]                        <- unlist(c(distr_with_grid[,a_var, with=FALSE])) [match(grdtable$grID, distr_with_grid$grID)]  /nby
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
  
  
return()
}

##------------------------------------
## CALLS------------------------------
  years <- c(2019:2021)
  
 
  # first, plot VMS data
  load(file=file.path(ROutputPathToDatasets, "vms_2019_2021_in_NAO_before_merging.RData")) # vms_effort_nao
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS", "all_metiers", "2019_2021")  
  nby <- length(unique(vms_effort_nao$year))
  dir.create(filepath, recursive=TRUE)
  my_raster_export(master_dt=vms_effort_nao, slave_dt=vms_effort_nao, nby)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$FishingHour[], na.rm=TRUE) # ....but remember that it does not make sense to sum up effort because the species dim is present.

  
  # all metiers, all years
  nby <- length(unique(dispatched_landings_ally$year))
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_FDI", "all_metiers", "2019_2021")  
  dir.create(filepath, recursive=TRUE)
  my_raster_export(master_dt=dispatched_landings_ally, slave_dt=dispatched_landings_ally, nby)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(log(dd))
  sum(dd$FishingHour[], na.rm=TRUE) # ....but remember that it does not make sense to sum up effort because the species dim is present.
 
  
  # or all metiers, per y
  for (y in years)
  {
  distr_this_y <- dispatched_landings_ally[dispatched_landings_ally$year %in% y, ] # test on this year
  filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_FDI", "all_metiers", y)  
  dir.create(filepath, recursive=TRUE)
  my_raster_export(master_dt=dispatched_landings_ally, slave_dt=distr_this_y, nby=1)
  # check: 
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
  plot(dd)
  } # end y

  # or per metier, all years
  a_df                 <- as.data.frame(dispatched_landings_ally)
  for (fs in unlist(unique(dispatched_landings_ally[,"fs"])))
  {
     distr_this_fs        <- data.table(a_df[a_df$fs %in% fs,])
     #distr_this_fs_this_y <- distr_this_fs[distr_this_fs$year=="2019",]
     nby                  <- length(unique(dispatched_landings_ally$year))
     filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_FDI", fs, "2019_2021")
     dir.create(filepath, recursive=TRUE)
     my_raster_export(master_dt=dispatched_landings_ally, slave_dt=distr_this_fs, nby)
     # check: 
     dd <- rast(file.path(filepath, "spatRaster.tif"))
     plot(dd)
  }
     
  # or per metier, per y
  a_df                 <- as.data.frame(dispatched_landings_ally)
  for (y in years)
  {
  cat(paste0(y, "\n"))
  for (fs in unlist(unique(dispatched_landings_ally[,"fs"])))
  {
     distr_this_fs        <- data.table(a_df[a_df$fs %in% fs,])
     distr_this_fs_this_y <- distr_this_fs[distr_this_fs$year==y,]
     filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_FDI", fs, y)
     dir.create(filepath, recursive=TRUE)
     if(nrow(distr_this_fs_this_y)!=0){
        my_raster_export(master_dt=dispatched_landings_ally, slave_dt=distr_this_fs_this_y, nby=1)
        # check: 
        dd <- rast(file.path(filepath, "spatRaster.tif"))
       plot(dd)
       }
  } # end fs
  } # end y

 
 
 