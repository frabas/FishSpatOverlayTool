# Author: Francois Bastardie (DTU-Aqua), June 2023

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# SPATIAL OVERLAY ANALYSIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!(STANDALONE)!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#



 setwd(file.path("..","FishSpatOverlayTool"))

 RinputPath  <- file.path(getwd(), "INPUT_DATASETS")
 ROutputPathToDatasets <- file.path(getwd(), "OUTCOME_DATASETS")

 library(sf)
 library(raster)
 library(terra)
 
 years_span <- "2018_2021"
 
 a_folder   <- "OUTCOME_FISHERIES_DISTR_VMS_AER"

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!THE FISHABLE AREAS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!(MAXIMUM EXTENT i.e. IGNORING OTHER MARINE USES)!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

 # the fishable area is restricted by law depending on bathymetry:
 # MED&BS >1000m
 # Other EU regions >800m

 library(raster)
 coded_bathy_longlat <- raster(file.path(getwd(), "INPUT_SPATIAL_LAYERS", "GEBCO_May_2023", "GEBCO_bathy_coding_for_o800_and_o1000m_on_msfd.tif"))
 library(terra)
 coded_bathy_longlat <- terra::rast(coded_bathy_longlat)
  # see make_a_fishable_bathy_layer_from_GEBCO.r
 # -800< : 1
 # -1000<: 2

 #align with whatever spatRast file that will be later used....
 library(terra)
 filepath      <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER", "all_metiers", "2018_2021")
 aer_layers    <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
 cat(paste0("Desired resolution:",res(aer_layers),"\n"))
 cat(paste0("Actual resolution:",res(coded_bathy_longlat),"\n"))
 coded_bathy_longlat_resampled <- resample(coded_bathy_longlat, aer_layers, method = 'bilinear') # resample output
 res(coded_bathy_longlat_resampled)
 plot(coded_bathy_longlat_resampled)  # code: 1:<800m; >1: 800-1000m


 # INPUT 0/1 REGION SPECIFIC RASTERS (see Utils/make_regional_raster_0_1_coding.R. caution: grid resolution and extend dependent on a master spatRast e.g. aer_layers)
  ns_raster_005    <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","North_Sea_raster_based_on_FAO_reg.tiff"))   # FAO 27.4
  bs_raster_005    <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","Baltic_Sea_raster_based_on_FAO_reg.tiff"))  # FAO 27.3
  cs_raster_005    <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","Celtic_Seas_raster_based_on_FAO_reg.tiff")) # FAO 27.7
  bob_raster_005   <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","BoB_raster_based_on_FAO_reg.tiff"))         # FAO 27.8
  port_raster_005  <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","Portugal_raster_based_on_FAO_reg.tiff"))    # FAO 27.9
  mac_raster_005   <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","Macronesie_raster_based_on_FAO_reg.tiff"))  # FAO 27.10
  wmed_raster_005  <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","WMed_raster_based_on_FAO_reg.tiff")) # FAO 37.1
  cmed_raster_005  <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","CentralMed_raster_based_on_FAO_reg.tiff")) # FAO 37.2
  emed_raster_005  <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","EastMed_raster_based_on_FAO_reg.tiff")) # FAO 37.3
  black_raster_005  <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","BlackSea_raster_based_on_FAO_reg.tiff")) # FAO 37.4

  # refine the fishable area using a sediments.shp to associate certain fishing practices to certain bottom types? and where suitable conditions?
  # NO. We can instead roughtly assume that all surface areas that have been recorded fished in a past year period would define the fishable area
  # TODO?
  #....


  # fishable area per FAO region
  fishable_ns   <- ns_raster_005 + coded_bathy_longlat_resampled
  fishable_bs   <- bs_raster_005 + coded_bathy_longlat_resampled
  fishable_cs   <- cs_raster_005 + coded_bathy_longlat_resampled
  fishable_bob  <- bob_raster_005 + coded_bathy_longlat_resampled
  fishable_port <- port_raster_005 + coded_bathy_longlat_resampled
  fishable_mac  <- mac_raster_005 + coded_bathy_longlat_resampled

  # check
  plot(fishable_cs)
  plot(cs_raster_005, col=rgb(0.2,0,0,0.1), add=TRUE)
  library(rnaturalearth)
  sf_world <- ne_countries(returnclass='sf')
  plot(sf_world, add=TRUE, col="grey", border=FALSE)

  # transform in Lambert and compute fishable areas for later use
  # ns
  fishable_ns_eea <- project(fishable_ns, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  area_cell                               <- prod(res(fishable_ns_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
  fishable_ns_eea[!is.na(fishable_ns_eea) & fishable_ns_eea>2] <- NA  # no fishing allowed if < -800m in the NEA
  fishable_ns_eea[!is.na(fishable_ns_eea)] <- 1
  sum_fishable_area_km2_ns <- sum(sum(fishable_ns_eea * area_cell)[], na.rm=TRUE)

  # bs
  fishable_bs_eea <- project(fishable_bs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  area_cell                               <- prod(res(fishable_bs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
  fishable_bs_eea[!is.na(fishable_bs_eea) & fishable_bs_eea>2] <- NA  # no fishing allowed if < -800m in the NEA
  fishable_bs_eea[!is.na(fishable_bs_eea)] <- 1
  sum_fishable_area_km2_bs <- sum(sum(fishable_bs_eea * area_cell)[], na.rm=TRUE)

  # cs
  fishable_cs_eea <- project(fishable_cs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  area_cell                               <- prod(res(fishable_cs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
  fishable_cs_eea[!is.na(fishable_cs_eea) & fishable_cs_eea>2] <- NA  # no fishing allowed if < -800m in the NEA
  fishable_cs_eea[!is.na(fishable_cs_eea)] <- 1
  sum_fishable_area_km2_cs <- sum(sum(fishable_cs_eea * area_cell)[], na.rm=TRUE)

  # bob
  fishable_bob_eea <- project(fishable_bob, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  area_cell                               <- prod(res(fishable_bob_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
  fishable_bob_eea[!is.na(fishable_bob_eea) & fishable_bob_eea>2] <- NA  # no fishing allowed if < -800m in the NEA
  fishable_bob_eea[!is.na(fishable_bob_eea)] <- 1
  sum_fishable_area_km2_bob <- sum(sum(fishable_bob_eea * area_cell)[], na.rm=TRUE)



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!FIND ACTUALLY FISHED AREAS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#


 # read-in a AER layer
 library(terra)
 filepath      <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER", "all_metiers", "2018_2021")
 aer_layers    <- terra::rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
 plot(log(aer_layers))
 sum(aer_layers$FishingHour[], na.rm=TRUE)
 sum(aer_layers$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)


 # fished area per FAO region
 # ns
 fished_ns                    <- ns_raster_005 +  aer_layers$FishingHour
 fished_ns_eea                <- project(fished_ns, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell                    <- prod(res(fished_ns_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fished_ns_eea[!is.na(fished_ns_eea) & fished_ns_eea<10] <- NA # use a threshold to avoid noise
 fished_ns_eea[!is.na(fished_ns_eea)] <- 1 # all grid cells with effort > 10h
 sum_fished_area_km2_ns <- sum(sum(fished_ns_eea * area_cell)[], na.rm=TRUE)
 sum_fished_area_km2_ns/sum_fishable_area_km2_ns  # proportion of surface area fished in this region for the time period examined

 # bs
 fished_bs                    <- bs_raster_005 +  aer_layers$FishingHour
 fished_bs_eea                <- project(fished_bs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell                    <- prod(res(fished_bs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fished_bs_eea[!is.na(fished_bs_eea) & fished_bs_eea<10] <- NA # use a threshold to avoid noise
 fished_bs_eea[!is.na(fished_bs_eea)] <- 1 # all grid cells with effort > 10h
 sum_fished_area_km2_bs <- sum(sum(fished_bs_eea * area_cell)[], na.rm=TRUE)
 sum_fished_area_km2_bs/sum_fishable_area_km2_bs  # proportion of surface area fished in this region for the time period examined

 # cs
 fished_cs                    <- cs_raster_005 +  aer_layers$FishingHour
 fished_cs_eea                <- project(fished_cs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell                    <- prod(res(fished_cs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fished_cs_eea[!is.na(fished_cs_eea) & fished_cs_eea<10] <- NA # use a threshold to avoid noise
 fished_cs_eea[!is.na(fished_cs_eea)] <- 1 # all grid cells with effort > 10h
 sum_fished_area_km2_cs <- sum(sum(fished_cs_eea * area_cell)[], na.rm=TRUE)
 sum_fished_area_km2_cs/sum_fishable_area_km2_cs  # proportion of surface area fished in this region for the time period examined

 # bob
 fished_bob                    <- bob_raster_005 +  aer_layers$FishingHour
 fished_bob_eea                <- project(fished_bob, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell                    <- prod(res(fished_bob_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fished_bob_eea[!is.na(fished_bob_eea) & fished_bob_eea<10] <- NA # use a threshold to avoid noise
 fished_bob_eea[!is.na(fished_bob_eea)] <- 1 # all grid cells with effort > 10h
 sum_fished_area_km2_bob <- sum(sum(fished_bob_eea * area_cell)[], na.rm=TRUE)
 sum_fished_area_km2_bob/sum_fishable_area_km2_bob  # proportion of surface area fished in this region for the time period examined

 library(readr)
 output <- rbind(
              cbind(sum_fished_area_km2_ns, round(sum_fished_area_km2_ns/sum_fishable_area_km2_ns,3)),
              cbind(sum_fished_area_km2_bs, round(sum_fished_area_km2_bs/sum_fishable_area_km2_bs,3)),
              cbind(sum_fished_area_km2_cs, round(sum_fished_area_km2_cs/sum_fishable_area_km2_cs,3)),
              cbind(sum_fished_area_km2_bob, round(sum_fished_area_km2_bob/sum_fishable_area_km2_bob,3))
        )
 rownames(output) <- c("North Sea", "Baltic Sea", "Celtic Seas", "BoB")
 colnames(output) <- c("Fished km^2", "Proportion of the fishable area")
 print(output)
 dd <- knitr::kable(as.data.frame(output), format = "html")
 readr::write_file(dd, file.path(getwd(), "OUTCOME_OVERLAY", paste0("prop_fished_over_fishable_output_from_",years_span,".html")))

  



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!OVERLAY OTHER USES, EXTRACT & TABULATE!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

 # 1. THE FISHABLE AREA MIGHT BE AN OVERESTIMATION OF THE SPACE AVAILABLE TO FISHING (e.g.
 # in the Baltic Sea, it is not expected all area is fishable given the spaecies distribution,
 # the anoxic areas etc.),
 # THEN IT WILL BE BETTER TO COMPARE THE CHANGE IN FISH SPACE AGAINST
 # THE ACTUAL FISHED AREA. THIS IS WHAT IS BEING USE BELOW.


 # 2. The FISHABLE AREA MIGHT CHANGE IN FUTURE GIVEN SPECIES (RE-)DISTRIBUTION
 # AFFECTED BY CLIMATE CHANGE etc.



#--------------------
# utils
aggregate_from_raster_overlay <- function (a_sce="OWF",
                                           a_reg_name="NorthSea",
                                           a_reg_layer=ns_raster,
                                           save_a_plot=FALSE,
                                           some_colnames=c("effort", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare", "GVA"),
                                           a_data_layers=aer_layers,
                                           a_closed_area_layer=mpas_3035_msfd_rast_terra,
                                           name_closure="OWF",
                                           name_fs="all_fs")
{
  # raster layers projected in EEA are required in input
  a_reg_layer <- trim(a_reg_layer)
  a_data_layers <- crop(a_data_layers, a_reg_layer)
  
  # do a RASTER OVERLAY with closed areas per fished area per FAO region
  data_layers_this_reg                <- a_data_layers * a_reg_layer # filter out if not inside region
  # the closed areas layer
  a_closed_area_layer_c               <- crop(a_closed_area_layer, data_layers_this_reg) # align
  # the overlay
  overlay                             <- data_layers_this_reg * a_closed_area_layer_c
  # the complementary non-closed areas layer
  a_non_closed_area_area_c            <- a_closed_area_layer_c # init
  a_non_closed_area_area_c [] <- 0
  #values(a_non_closed_area_area_c) <- NA
  values(a_non_closed_area_area_c) [is.na(values(a_closed_area_layer_c))] <- 1
  non_overlay                         <- data_layers_this_reg * a_non_closed_area_area_c    * a_reg_layer
  # trim to the region for better visualisation
  if(!all(is.na(overlay$effort[]))){ 
     overlay_t                           <- terra::crop(overlay, a_reg_layer) # trim to stick to the region
     a_closed_area_layer_c               <- terra::crop(a_closed_area_layer, a_reg_layer)
     a_closed_area_layer_c               <- a_closed_area_layer_c * a_reg_layer
     non_overlay_c                       <- terra::crop(non_overlay, a_reg_layer)

     # visual check
     if(save_a_plot){
        a_width <- 4000 ; a_height <- 4000
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY", paste0(name_closure,"-", fs, ".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
       plot(log(overlay_t$FishingHour))
       plot(log(non_overlay_c$FishingHour), add=TRUE, legend=FALSE) # caution: leg breaks can differ. But here we plot only for a quick visual check
       plot(a_closed_area_layer_c, col=rgb(0.2,0.2,0.2,0.2), add=TRUE, legend=FALSE)
       bi <- boundaries(a_closed_area_layer_c)
       #plot(bi, add=TRUE, col=rgb(0.1,0.1,0.1,0.1))
      dev.off()
      }

     # aggregate over the entire area and format
     library(data.table)
     sum_all_inside <- data.frame(data.table(as.data.frame(overlay_t))[,lapply(.SD, sum, na.rm=TRUE),
                                   .SDcols=c(some_colnames)])
   }else{  # capture the edge case of no impact of the closure...
     sum_all_inside <- as.data.frame(matrix(0, ncol=length(some_colnames)))
     colnames(sum_all_inside) <- some_colnames
   }  
     sum_all_outside <- data.frame(data.table(as.data.frame(non_overlay))[,lapply(.SD, sum, na.rm=TRUE),
                                   .SDcols=c(some_colnames)])
     sum_all <- rbind.data.frame(
                     cbind.data.frame(Sce=a_sce, Region=a_reg_name, variable=colnames(sum_all_outside), value= as.numeric(sum_all_outside[1,]), CLOSED=FALSE, name_closure=name_closure, name_fs=name_fs),
                     cbind.data.frame(Sce=a_sce, Region=a_reg_name, variable=colnames(sum_all_inside), value= as.numeric(sum_all_inside[1,]), CLOSED=TRUE, name_closure=name_closure, name_fs=name_fs)
                          )
 return(sum_all)
}




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!CDDA + NATURA 2000 + OTHER FROM PARTNERS!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

  library(sf)
  mpas    <- st_read(file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","wetransfer_eu_uk_final_2023-08-10_1644","EU_UK_final","EU__plus_UK_future_restrictions.shp"))
  # =>  produced based on the CINEA MPA database
  a_df              <- st_drop_geometry(mpas) # get the df

   plot(mpas["Origin"])

  library(terra)
  mpas_vect_terra         <- vect(mpas)
  #=> NOT in EEA Lambert proj because we do NOT do the overlay in Lambert proj (because would induce a resampling)


  idx                                 <- which(a_df["rmv_lns_S0"]==1)
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_lns <- mpas_vect_terra[idx]
  #writeVector(mpas_rmv_lns, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_lns.shp"), filetype="ESRI Shapefile")
  
  idx                                 <- which(a_df["rmv_lns_S0"]==1 & a_df$reason_lns %in% c("current", "current_habitat","current_spp", "current_habitat_spp"))
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_lns_current <- mpas_vect_terra[idx]
  #writeVector(mpas_rmv_lns_current, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_lns_current.shp"), filetype="ESRI Shapefile")
  
  idx                                 <- which(a_df["rmv_nts_S0"]==1)
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_nts <- mpas_vect_terra[idx]
  #writeVector(mpas_rmv_nts, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_nts.shp"), filetype="ESRI Shapefile")
 
  idx                                 <- which(a_df["rmv_nts_S0"]==1 & a_df$reason_nts %in% c("current", "current_habitat","current_spp", "current_habitat_spp"))
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_nts_current <- mpas_vect_terra[idx]
  #writeVector(mpas_rmv_nts_current, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_nts_current.shp"), filetype="ESRI Shapefile")
 
  idx                                 <- which(a_df["rmv_bt__S0"]==1)
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_bt <- mpas_vect_terra[idx]
  #writeVector(mpas_rmv_bt, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_bt.shp"), filetype="ESRI Shapefile")

  idx                                 <- which(a_df["rmv_bt__S0"]==1 & a_df$reason_bt %in% c("current", "current_habitat","current_spp", "current_habitat_spp"))
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_bt_current <- mpas_vect_terra[idx]
  #writeVector(mpas_rmv_bt_current, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_bt_current.shp"), filetype="ESRI Shapefile")

#  * *current* = current restrictions in place
#* *current_habitat* = current restrictions in place plus hypothetical habitat restriction
#* *current_spp* = current restrictions in place plus hypothetical directive species restriction
#* *Notrescurrent* = No current restrictions in place or in hypothetical scenario
#* *Notrescurrent_habitat* = No current restrictions in place but hypothetical habitat restriction
#* *Notrescurrent_habitat_spp* = No current restrictions in place but hypothetical habitat and directive species restriction
#* *Notrescurrent_habitat* = No current restrictions in place but hypothetical directive species restriction


     
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!OWF!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

  library(sf)
  owf_msfd                 <- st_read(file.path(getwd(), "INPUT_SPATIAL_LAYERS", "OWF","EMODnet_HA_WindFarms_20221219", "EMODnet_HA_WindFarms_pg_20221219.shp"))
  a_df                     <- st_drop_geometry(owf_msfd) # get the df

  owf_msfd_missing                 <- st_read(file.path(getwd(), "INPUT_SPATIAL_LAYERS", "OWF","missing_polygons.shp"))
  a_df_missing                     <- st_drop_geometry(owf_msfd_missing) # get the df


  # sf vect to terra::vect to do some extract with it
  library(terra)
  owf_msfd_vect_terra         <- vect(owf_msfd)

  #owf_msfd_vect_terra       <- project(owf_msfd_vect_terra, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  #=> cause we do the overlay in Lambert proj
  owf_miss_msfd_vect_terra         <- vect(owf_msfd_missing) # NO PROJ!

  # check
  graphics.off()
  plot(owf_miss_msfd_vect_terra)
  plot(owf_msfd_vect_terra, add=TRUE, col="red")

  #plot(mpas_vect_terra, add=TRUE, col="green")





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## CREATE RASTERS FOR RESTRICTED AREAS
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


#align with whatever spatRast file that will be later used....
 library(terra)
 filepath      <- file.path(getwd(),a_folder, "all_metiers", years_span)
 aer_layers    <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content

   # NATURA2000+CDDA-------------
   # rasterize the closed areas
   #dd                          <- mpas_3035_msfd_vect_terra
   #dd$value                    <- 1
   ##aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   #aer_layers_eea_terra        <- aer_layers # NO PROJ!
   #n                           <- 10
   #aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   #mpas_3035_msfd_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   #mpas_3035_msfd_rast_terra   <- aggregate(mpas_3035_msfd_rast_terra, n, "modal")
   ## visual check
   ##plot(aer_layers_eea_terra$effort)
   #plot(trim(mpas_3035_msfd_rast_terra), col=rgb(0.2,0.2,0.2,0.3))
   ##plot(mpas_3035_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)

   # NATURA2000+CDDA partners RESTRICT TO LONGLINERS-------------
   # rasterize the closed areas
   dd                          <- mpas_rmv_lns
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   mpas_rmv_lns_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   mpas_rmv_lns_rast_terra   <- aggregate(mpas_rmv_lns_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(trim(mpas_rmv_lns_rast_terra), col=rgb(0.2,0.2,0.2,0.3))
   #plot(mpas_3035_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)

   # NATURA2000+CDDA partners RESTRICT TO LONGLINERS-------------
   # rasterize the closed areas
   dd                          <- mpas_rmv_lns_current
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   mpas_rmv_lns_current_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   mpas_rmv_lns_current_rast_terra   <- aggregate(mpas_rmv_lns_current_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(trim(mpas_rmv_lns_current_rast_terra), col=rgb(0.2,0.2,0.2,0.3))
   #plot(mpas_3035_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)

   # NATURA2000+CDDA partners RESTRICT TO NETTERS-------------
   # rasterize the closed areas
   dd                          <- mpas_rmv_nts
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   mpas_rmv_nts_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   mpas_rmv_nts_rast_terra   <- aggregate(mpas_rmv_nts_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(trim(mpas_rmv_nts_rast_terra), col=rgb(0.2,0.2,0.2,0.3))
   #plot(mpas_3035_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)

    # NATURA2000+CDDA partners RESTRICT TO NETTERS-------------
   # rasterize the closed areas
   dd                          <- mpas_rmv_nts_current
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   mpas_rmv_nts_current_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   mpas_rmv_nts_current_rast_terra   <- aggregate(mpas_rmv_nts_current_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(trim(mpas_rmv_nts_current_rast_terra), col=rgb(0.2,0.2,0.2,0.3))
   #plot(mpas_3035_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)

   # NATURA2000+CDDA partners RESTRICT TO BOTTOM TRAWLERS-------------
   # rasterize the closed areas
   dd                          <- mpas_rmv_bt
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   mpas_rmv_bt_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   mpas_rmv_bt_rast_terra   <- aggregate(mpas_rmv_bt_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(trim(mpas_rmv_bt_rast_terra), col=rgb(0.2,0.2,0.2,0.3))
   #plot(mpas_3035_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)
   writeRaster(mpas_rmv_bt_rast_terra, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_bt_rast_terra.tif"))

    # NATURA2000+CDDA partners RESTRICT TO BOTTOM TRAWLERS-------------
   # rasterize the closed areas
   dd                          <- mpas_rmv_bt_current
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   mpas_rmv_bt_current_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   mpas_rmv_bt_current_rast_terra   <- aggregate(mpas_rmv_bt_current_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(trim(mpas_rmv_bt_current_rast_terra), col=rgb(0.2,0.2,0.2,0.3))
   #plot(mpas_3035_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)
  

   # OWF------------------
   # rasterize the closed areas
   dd                         <- owf_msfd_vect_terra
   dd$value                   <- 1
   #aer_layers_eea_terra       <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   owf_msfd_rast_terra        <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   owf_msfd_rast_terra        <- aggregate(owf_msfd_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(owf_msfd_rast_terra, col=rgb(0.2,0.2,0.2,0.3), add=FALSE)
   #plot(owf_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)


    # OWF MISSING POLYGONS------------------
   # rasterize the closed areas
   dd                         <- owf_miss_msfd_vect_terra
   dd$value                   <- 1
   #aer_layers_eea_terra       <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra        <- aer_layers # NO PROJ!
   n                           <- 10
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   owf_miss_msfd_rast_terra        <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   owf_miss_msfd_rast_terra        <- aggregate(owf_miss_msfd_rast_terra, n, "modal")
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(owf_miss_msfd_rast_terra, col=rgb(0.2,0.2,0.2,0.3), add=FALSE)
   #plot(owf_msfd_vect_terra, col=rgb(0.2,0.2,0.2,0.3), add=TRUE)


   # OWF------------------------------------
   restricted_area_owf  <- sum(owf_msfd_rast_terra, owf_miss_msfd_rast_terra, na.rm=TRUE)


   # CURRENTCDDA+NATURA2000------------------
   mpas_msfd_current_rast_terra        <- sum(mpas_rmv_lns_current_rast_terra, mpas_rmv_nts_current_rast_terra, mpas_rmv_bt_current_rast_terra, na.rm=TRUE)
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(mpas_msfd_current_rast_terra, col=rgb(0.2,0.2,0.2,0.3), add=FALSE)

    # CDDA+NATURA2000------------------
   mpas_msfd_rast_terra        <- sum(mpas_rmv_lns_rast_terra, mpas_rmv_nts_rast_terra, mpas_rmv_bt_rast_terra, na.rm=TRUE)
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(mpas_msfd_rast_terra, col=rgb(0.2,0.2,0.2,0.3), add=FALSE)
   writeRaster(mpas_msfd_rast_terra, filename=file.path(getwd(), "INPUT_SPATIAL_LAYERS","CLOSURES_PARTNERS","EU__plus_UK_future_restrictions_rast_terra.tif"))



   # CURRENTCDDA+NATURA2000+OWF ------------------
   mpas_owf_msfd_current_rast_terra        <- sum(owf_msfd_rast_terra, owf_miss_msfd_rast_terra, mpas_rmv_lns_current_rast_terra, mpas_rmv_nts_current_rast_terra, mpas_rmv_bt_current_rast_terra, na.rm=TRUE)
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(mpas_owf_msfd_current_rast_terra, col=rgb(0.2,0.2,0.2,0.3), add=FALSE)

    # CDDA+NATURA2000+OWF------------------
   mpas_owf_msfd_rast_terra        <- sum(owf_msfd_rast_terra, owf_miss_msfd_rast_terra, mpas_rmv_lns_rast_terra, mpas_rmv_nts_rast_terra, mpas_rmv_bt_rast_terra, na.rm=TRUE)
   # visual check
   #plot(aer_layers_eea_terra$effort)
   plot(mpas_owf_msfd_rast_terra, col=rgb(0.2,0.2,0.2,0.3), add=FALSE)



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## ESTIMATE THE FISHABLE AREA LEFT PER RESTRICTION SCENARIO
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  filepath       <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS", "all_metiers", "2018_2021")
  aer_layers                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"


 # loop over layers per scenario
 output <- NULL
 for(lyri in c("restricted_area_owf", "mpas_msfd_current_rast_terra", "mpas_msfd_rast_terra", "mpas_owf_msfd_current_rast_terra", "mpas_owf_msfd_rast_terra" ))
 {
  lyr_restriction <- get(lyri)

  # getting a mask layer
  lyr_restriction_mask <- lyr_restriction
  values(lyr_restriction_mask)[is.na(values(lyr_restriction_mask))] <- 100
  values(lyr_restriction_mask)[values(lyr_restriction_mask)<100] <- NA


 # ns
 restricted_ns                  <- ns_raster_005  + lyr_restriction  + fishable_ns
 fishable_left_ns               <- ns_raster_005  + lyr_restriction_mask  + fishable_ns
 restricted_ns_eea              <- project(restricted_ns, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 fishable_left_ns_eea           <- project(fishable_left_ns, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell_restricted           <- prod(res(restricted_ns_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 area_cell_fishable_left        <- prod(res(fishable_left_ns_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fishable_left_ns_eea[!is.na(fishable_left_ns_eea)] <- 1
 restricted_ns_eea[!is.na(restricted_ns_eea)] <- 1
 sum_restricted_area_km2_ns     <- sum(sum(restricted_ns_eea * area_cell)[], na.rm=TRUE)
 sum_fishable_left_area_km2_ns  <- sum(sum(fishable_left_ns_eea * area_cell)[], na.rm=TRUE)

 # bs
 restricted_bs                  <- bs_raster_005  + lyr_restriction   + fishable_bs
 fishable_left_bs               <- bs_raster_005  + lyr_restriction_mask  + fishable_bs
 restricted_bs_eea              <- project(restricted_bs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 fishable_left_bs_eea           <- project(fishable_left_bs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell_restricted           <- prod(res(restricted_bs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 area_cell_fishable_left        <- prod(res(fishable_left_bs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fishable_left_bs_eea[!is.na(fishable_bs_eea)] <- 1
 restricted_bs_eea[!is.na(restricted_bs_eea)] <- 1
 sum_restricted_area_km2_bs     <- sum(sum(restricted_bs_eea * area_cell)[], na.rm=TRUE)
 sum_fishable_left_area_km2_bs  <- sum(sum(fishable_left_bs_eea * area_cell)[], na.rm=TRUE)

 # cs
 restricted_cs                  <- cs_raster_005  + lyr_restriction       + fishable_cs
 fishable_left_cs               <- cs_raster_005  + lyr_restriction_mask   + fishable_cs
 restricted_cs_eea              <- project(restricted_cs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 fishable_left_cs_eea           <- project(fishable_left_cs, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell_restricted           <- prod(res(restricted_cs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 area_cell_fishable_left        <- prod(res(fishable_left_cs_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fishable_left_cs_eea[!is.na(fishable_left_cs_eea)] <- 1
 restricted_cs_eea[!is.na(restricted_cs_eea)] <- 1
 sum_restricted_area_km2_cs     <- sum(sum(restricted_cs_eea * area_cell)[], na.rm=TRUE)
 sum_fishable_left_area_km2_cs  <- sum(sum(fishable_left_cs_eea * area_cell)[], na.rm=TRUE)

 # bob
 restricted_bob                  <- bob_raster_005  + lyr_restriction        + fishable_bob
 fishable_left_bob               <- bob_raster_005  + lyr_restriction_mask  + fishable_bob
 restricted_bob_eea              <- project(restricted_bob, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 fishable_left_bob_eea           <- project(fishable_left_bob, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
 area_cell_restricted            <- prod(res(restricted_bob_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 area_cell_fishable_left         <- prod(res(fishable_left_bob_eea))/1e6 # converted in km^2 as the resolution of the raster is in meter
 fishable_left_bob_eea[!is.na(fishable_left_bob_eea)] <- 1
 restricted_bob_eea[!is.na(restricted_bob_eea)] <- 1
 sum_restricted_area_km2_bob     <- sum(sum(restricted_bob_eea * area_cell)[], na.rm=TRUE)
 sum_fishable_left_area_km2_bob  <- sum(sum(fishable_left_bob_eea * area_cell)[], na.rm=TRUE)

 library(readr)
 output <- rbind.data.frame( output,
              cbind.data.frame(Region="North_Sea", Scenario=lyri, restricted=round(sum_restricted_area_km2_ns),
                                              fishable_left=round(sum_fishable_left_area_km2_ns),
                                              prop=round(sum_restricted_area_km2_ns/(sum_restricted_area_km2_ns+sum_fishable_left_area_km2_ns), 3)),
              cbind.data.frame(Region="Baltic_Sea", Scenario=lyri, restricted=round(sum_restricted_area_km2_bs),
                                              fishable_left=round(sum_fishable_left_area_km2_bs),
                                              prop=round(sum_restricted_area_km2_bs/(sum_restricted_area_km2_bs+sum_fishable_left_area_km2_bs), 3)),
              cbind.data.frame(Region="Celtic_Seas", Scenario=lyri, restricted=round(sum_restricted_area_km2_cs),
                                              fishable_left=round(sum_fishable_left_area_km2_cs),
                                              prop=round(sum_restricted_area_km2_cs/(sum_restricted_area_km2_cs+sum_fishable_left_area_km2_cs), 3)),
              cbind.data.frame(Region="BoB", Scenario=lyri, restricted=round(sum_restricted_area_km2_bob),
                                              fishable_left=round(sum_fishable_left_area_km2_bob),
                                              prop=round(sum_restricted_area_km2_bob/(sum_restricted_area_km2_bob+sum_fishable_left_area_km2_bob), 3))
        )
} # end lyri
 colnames(output) <- c("Region", "Scenario", "Restricted km^2", "left km^2 to fishing", "Proportion of the fishable area")
 print(output)
 dd <- knitr::kable(as.data.frame(output), format = "html")
 readr::write_file(dd, file.path(getwd(), "OUTCOME_OVERLAY", paste0("prop_fishable_area_left_from_restriction_sces.html")))



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## CREATE LOOKUP FOR RESTRICTION SPECS
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # read-in a layer and proceed to effort displacement in a systematic way
   library(terra)
   a_folder  <- "OUTCOME_FISHERIES_DISTR_VMS_AER"   # a VMS-AER layer
   a_folder  <- "OUTCOME_FISHERIES_DISTR_VMS"   # a VMS layer
   #a_folder  <- "OUTCOME_FISHERIES_DISTR_FDI_AER"  # a FDI-AER layer
   dir.create(file.path(getwd(),"OUTCOME_OVERLAY", a_folder), recursive=TRUE)
  lst_files <- list.files(file.path(getwd(), a_folder))

  # example of fishing-technique-specific specs
  restriction_per_fs_per_sce <- list(NULL)
  sces <- c("OWF", "currentMPAs", "MPAs", "OWF+currentMPAs", "OWF+MPAs")
  count <- 0
  for(a_sce in sces)
  {
  count <- count+1
  restriction_per_fs <- NULL
  if(a_sce=="OWF") for(fs in lst_files)
     {
     areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra")
     restriction_per_fs <- rbind.data.frame(
                                restriction_per_fs,
                                expand.grid(fs=fs, restricted_area=areas, scenario=a_sce)
                                )
     }  # end fs
  if(a_sce=="currentMPAs") for(fs in lst_files)
     {
     areas <- NULL
    if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0 |
        length(grep("OTB", fs))>0 |
        length(grep("PTB", fs))>0 |
        length(grep("PS", fs))>0 |
        length(grep("SDN", fs))>0 |
        length(grep("SSC", fs))>0 |
        length(grep("OTT", fs))>0)         areas <- c("mpas_rmv_bt_current_rast_terra")
     if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0 |
        length(grep("HMD", fs))>0 |
        length(grep("DRB", fs))>0) areas <- c("mpas_rmv_bt_current_rast_terra")
     if(length(grep("DFN", fs))>0 |
        length(grep("FPO", fs))>0 |
        length(grep("GTR", fs))>0 |
        length(grep("GNS", fs))>0)         areas <-  c("mpas_rmv_nts_current_rast_terra")
     if(length(grep("HOK", fs))>0 |
        length(grep("LLD", fs))>0)        areas <-  c("mpas_rmv_lns_current_rast_terra")
     if(length(areas)==0)                  areas <- "mpas_msfd_current_rast_terra" # default
     if(length(grep("PTM", fs))>0 |
        length(grep("OTM", fs))>0)                  areas <-  c("") # default
        restriction_per_fs <- rbind.data.frame(
                                restriction_per_fs,
                                expand.grid(fs=fs, restricted_area=areas, scenario=a_sce)
                                )
     }  # end fs
  if(a_sce=="MPAs") for(fs in lst_files)
     {
    areas <- NULL
    if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0)         areas <- c("mpas_rmv_bt_rast_terra")
     if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0 |
        length(grep("DRB", fs))>0) areas <- c("mpas_rmv_bt_rast_terra")
     if(length(grep("DFN", fs))>0 |
        length(grep("FPO", fs))>0)         areas <-  c("mpas_rmv_nts_rast_terra")
     if(length(grep("HOK", fs))>0 )        areas <-  c("mpas_rmv_lns_rast_terra")
     if(length(areas)==0)                  areas <- "mpas_msfd_rast_terra" # default
    if(length(grep("PTM", fs))>0 |
        length(grep("OTM", fs))>0)                  areas <-  c("") # default
       restriction_per_fs <- rbind.data.frame(
                                restriction_per_fs,
                                expand.grid(fs=fs, restricted_area=areas, scenario=a_sce)
                                )
     }  # end fs
  if(a_sce=="OWF+currentMPAs") for(fs in lst_files)
     {
     areas <- NULL
     if(length(grep("TM", fs))>0)          areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra")
     if(length(grep("PS", fs))>0)          areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra")
     if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0)         areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_bt_current_rast_terra")
     if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0 |
        length(grep("DRB", fs))>0) areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_bt_current_rast_terra")
     if(length(grep("DFN", fs))>0 |
        length(grep("FPO", fs))>0)         areas <-  c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_nts_current_rast_terra")
     if(length(grep("HOK", fs))>0 )        areas <-  c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_lns_current_rast_terra")
    if(length(grep("PTM", fs))>0 |
        length(grep("OTM", fs))>0)                  areas <-  c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra") # default
      if(length(areas)==0)                  areas <- "mpas_owf_msfd_current_rast_terra" # default
     restriction_per_fs <- rbind.data.frame(
                                restriction_per_fs,
                                expand.grid(fs=fs, restricted_area=areas, scenario=a_sce)
                                )
     }  # end fs
  if(a_sce=="OWF+MPAs") for(fs in lst_files)
     {
     areas <- NULL
    if(length(grep("TM", fs))>0)          areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra")
     if(length(grep("PS", fs))>0)          areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra")
     if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0)         areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_bt_rast_terra")
     if(length(grep("DTS", fs))>0 |
        length(grep("TBB", fs))>0 |
        length(grep("DRB", fs))>0) areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_bt_rast_terra")
     if(length(grep("DFN", fs))>0 |
        length(grep("FPO", fs))>0)         areas <-  c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_nts_rast_terra")
     if(length(grep("HOK", fs))>0 )        areas <-  c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_lns_rast_terra")
     if(length(grep("PTM", fs))>0 |
        length(grep("OTM", fs))>0)                  areas <-  c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra") # default
     if(length(areas)==0)                  areas <- "mpas_owf_msfd_rast_terra" # default
     restriction_per_fs <- rbind.data.frame(
                                restriction_per_fs,
                                expand.grid(fs=fs, restricted_area=areas, scenario=a_sce)
                                )
     }  # end fs


    restriction_per_fs_per_sce[[  count ]] <- restriction_per_fs
    }  # end a_sce
    names(restriction_per_fs_per_sce) <- sces





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!OVERLAY AND EXTRACT PER FISHING ACTIVITIES!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#



   # read-in a layer and proceed to overlay in a systematic way
   library(terra)
   a_folder  <- "OUTCOME_FISHERIES_DISTR_VMS_AER"   # a VMS-AER layer
   #a_folder  <- "OUTCOME_FISHERIES_DISTR_FDI_AER"  # a FDI-AER layer 
   a_folder  <- "OUTCOME_FISHERIES_DISTR_VMS"   # a VMS-AER layer
   dir.create(file.path(getwd(),"OUTCOME_OVERLAY", a_folder), recursive=TRUE)
   lst_files <- list.files(file.path(getwd(), a_folder))



 output_per_fs <- NULL # collector
 for(a_sce in sces){
   cat(paste0(a_sce,"\n"))
   count <- 0
 for(fs in lst_files){  # caution: this loop takes a while....
   count <- count+1
   cat(paste0(fs, "...", count, " out of ", length(lst_files)," files\n"))
   filepath       <- file.path(getwd(), a_folder, fs, "2018_2021")
     er <- try(   {
     aer_layers                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
   }, silent=TRUE)
   if(class(er)!="try-error"){
    if (a_folder %in% c("OUTCOME_FISHERIES_DISTR_VMS_AER"))
    {
          GVA <- aer_layers$landings_aer_in_ctry_level6_csquare *
                      (aer_layers$value_aer_in_ctry_level6_csquare / aer_layers$landings_aer_in_ctry_level6_csquare) +
                      aer_layers$other_income_in_csquare -
                      aer_layers$unpaid_labour_in_csquare - aer_layers$varcosts_in_ctry_level6_csquare  -  aer_layers$oth_non_var_costs_in_csquare
         names(GVA) <- "GVA"
         add(aer_layers) <- GVA
         #dd <- as.data.frame(aer_layers)
         #head(dd[!is.na(dd$GVA),])
         #plot(log(aer_layers))
         #sum(aer_layers$FishingHour[], na.rm=TRUE)
         #sum(aer_layers$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)
    }


    # stack fs specific restrictions to get one fs specific layer for closure
    dd <- restriction_per_fs_per_sce[[a_sce]]
    ddd <- dd[dd$fs==fs,]
    this_closed_area_layer <- NULL
    count2 <- 0
    for (lyr in ddd$restricted_area)
    {
       if(lyr!="")
       {
         count2 <- count2+1
         if (count2==1)
         {
             this_closed_area_layer <-  get(lyr)
         } else{
              this_closed_area_layer <- sum(this_closed_area_layer, get(lyr), na.rm=TRUE)
         }
       values(this_closed_area_layer) [values(this_closed_area_layer)>1] <- 1 # avoid double counting in case of overlapping restrictions...
       }
    }

      is_vms <- TRUE
      if(is_vms) aer_layers$effort <- aer_layers$FishingHour # generic name


   if (a_folder %in% c("OUTCOME_FISHERIES_DISTR_VMS") ) some_colnames <- c("effort")
   if (a_folder %in% c("OUTCOME_FISHERIES_DISTR_VMS_AER") ) some_colnames <- c("effort", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare", "GVA")

   if (a_folder %in% c("OUTCOME_FISHERIES_DISTR_VMS", "OUTCOME_FISHERIES_DISTR_VMS_AER") && !is.null(this_closed_area_layer))
     {

     ns_raster   <- resample(ns_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
     bs_raster   <- resample(bs_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
     cs_raster   <- resample(cs_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
     bob_raster  <- resample(bob_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
     #port_raster <- resample(port_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
     #mac_raster <- resample(mac_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
     # etc.
     
 #    # if the overlay is done in Lambert projection:
#     ns_raster_eea_terra     <- project(ns_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     bs_raster_eea_terra     <- project(bs_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     cs_raster_eea_terra     <- project(cs_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     bob_raster_eea_terra    <- project(bob_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     #port_raster_eea_terra   <- project(port_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     #mac_raster_eea_terra   <- project(mac_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
#     # etc.
#
#     aer_layers_eea_terra    <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

    obj_names <- c("sum_all_ns_closure", "sum_all_bs_closure", "sum_all_cs_closure", "sum_all_bob_closure")
    rm(list=obj_names)
   
     # do a RASTER OVERLAY with closed areas per fished area per FAO region
     sum_all_ns_closure   <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="NorthSea",
                                                            a_reg_layer=ns_raster, some_colnames=some_colnames, a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_fs=fs)
     sum_all_bs_closure   <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="BalticSea",
                                                            a_reg_layer=bs_raster,  some_colnames=some_colnames,  a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_fs=fs)
     sum_all_cs_closure   <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="CelticSea",
                                                           a_reg_layer=cs_raster, some_colnames=some_colnames,  a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_fs=fs)
     sum_all_bob_closure  <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="BoB",
                                                           a_reg_layer=bob_raster, some_colnames=some_colnames,  a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_fs=fs)
     #sum_all_port_closure <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="Portugal", a_reg_layer=port_raster, a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_fs=fs)
     #sum_all_mac_closure <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="Macronesie", a_reg_layer=port_raster, a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_fs=fs)
     # etc. (i.e. for FDI, but not for VMS)


     obj_names <- c("sum_all_ns_closure", "sum_all_bs_closure", "sum_all_cs_closure", "sum_all_bob_closure")

     for (objn in obj_names) if(exists(objn)) if(length(get(objn))>0) output_per_fs <- rbind.data.frame(output_per_fs, get(objn) )

     cat(paste0(fs, "...OK\n"))
     } else{
     cat(paste0("no effort info for this seg ", fs, ", or no closed area specified...\n"))
     }
     
     } else{
     cat(paste0("not such file for ", fs, "...\n"))
     }

} # end fs
} # end a_sce
  
# export--------
library(readr)
 print(output_per_fs)
 dd <- knitr::kable(as.data.frame(output_per_fs), format = "html")
 readr::write_file(dd, file.path(getwd(), "OUTCOME_OVERLAY", a_folder, paste0("aggregate_inside_outside_closed_areas_from_",years_span,".html")))

 save(output_per_fs, file=file.path(getwd(), "OUTCOME_OVERLAY", a_folder, paste0("aggregate_inside_outside_closed_areas_from_",years_span,".RData")))
  





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 # do a ggplot barplot out of this..........

 # re-load
 load(file.path(getwd(), "OUTCOME_OVERLAY", a_folder, paste0("aggregate_inside_outside_closed_areas_from_",years_span,".RData")))  # output_per_fs


 library(ggplot2)
 library(doBy)

 a_var <- "effort"
 a_var <- "landings_aer_in_ctry_level6_csquare"
 a_var <- "GVA"

 output_per_fs[output_per_fs[,"value"]<0, "value"] <- 0

 a_dt <- data.table(output_per_fs)

 an <- function(x) as.numeric(x)
 agg <- a_dt[variable==a_var,.(sum(an(value))),by=c("name_fs", "Region", "name_closure")]
 prop <- merge(agg, a_dt[variable==a_var,], by=c("name_fs","Region", "name_closure"))
 prop$prop <- prop$value / prop$V1   


 # all-metiers plot
 dd <- as.data.frame(prop)
 p1 <- ggplot(dd[dd$name_fs=="all_metiers",], aes(x = prop, y=name_fs, fill=CLOSED))  + geom_bar(stat = "summary", fun = "mean", position = "fill")  + facet_wrap(~name_closure+Region, ncol=4)   +
                xlab("Proportion" )    + ylab("Fleet-segment")

 a_width <- 5000 ; a_height <- 4000
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY", a_folder, paste0("Proportion_closed_all_metiers_from_",years_span,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p1)
 dev.off()
   
 # filter out depending on overall estimate
 a_df_true <- as.data.frame(prop[CLOSED==TRUE,])
 if(a_var=="effort") a_df_true <- a_df_true[a_df_true$V1>5000, ]
 a_df_true <- a_df_true[a_df_true$name_fs!="all_metiers",]
 a_df_true$name_fs <- factor(a_df_true$name_fs)

  # filter out depending on the representativity in terms of the var value
 a_df_true$cumsum <- cumsum(a_df_true$V1)
 a_df_true$percent_cumsum <- a_df_true$cumsum/a_df_true$cumsum[nrow(a_df_true)] *100
 if(a_var=="effort") a_df_true <- a_df_true[a_df_true$percent_cumsum<50, ]

 # keep the corresponding fs in FALSE occurences
 a_df_false <- as.data.frame(prop[CLOSED==FALSE,])
 keys  <- unique(paste0(a_df_true$name_fs, "_", a_df_true$Region, "_", a_df_true$name_closure))
 a_df_false$key <- paste0(a_df_false$name_fs, "_", a_df_false$Region, "_", a_df_false$name_closure)
 a_df_false <- a_df_false[a_df_false$key %in% keys,]
 a_df_true_false <- rbind(a_df_true[,1:9], a_df_false[,1:9])

 
 # order fs per level of impact   
 a_df_true_false <- orderBy(~ - CLOSED - prop, a_df_true_false)
 a_df_true_false$name_fs <- factor (a_df_true_false$name_fs, levels=unique(a_df_true_false$name_fs)) # re-order
 
 # a trick to carry the same order whatever the var... (ordered on effort values)
 if(a_var=="effort") order_fs <- levels(a_df_true_false$name_fs )
 a_df_true_false <- a_df_true_false[a_df_true_false$name_fs %in% order_fs,]
 a_df_true_false$name_fs <- factor(a_df_true_false$name_fs)
 a_df_true_false$name_fs <- factor (a_df_true_false$name_fs, levels=order_fs) # re-order always from the effort var


 # per fs plots
 p1 <- ggplot(a_df_true_false, aes(x = prop, y=name_fs, fill=CLOSED))  + geom_bar(stat = "summary", fun = "mean", position = "fill")  + facet_wrap(~name_closure+Region, ncol=4) +
   xlab("Proportion" )    + ylab("Fleet-segment")  +   theme(axis.text.y=element_text(size=4))
 a_width <- 6000 ; a_height <- 8000
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY", a_folder, paste0("Proportion_",a_var,"_closed_per_fs_from_",years_span,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p1)
 dev.off()


 p2 <- ggplot(a_df_true_false, aes(x = prop, y=name_fs, fill=CLOSED))  + geom_bar(stat = "summary", fun = "mean", position = "fill")  + facet_wrap(~name_closure, ncol=5)  +
   xlab("Proportion" )    + ylab("Fleet-segment")  +   theme(axis.text.y=element_text(size=10))
 a_width <- 6000 ; a_height <- 6000
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY", a_folder, paste0("Proportion_",a_var,"_closed_per_fs_from_",years_span,"_allhab.tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p2)
 dev.off()



      

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 # get a few illustrative maps for the most impacted fs...


   #------
   fs <- "all_metiers"
   filepath       <- file.path(getwd(), a_folder, fs, "2018_2021")
   aer_layers     <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
   aer_layers     <- terra::rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
   GVA <- aer_layers$landings_aer_in_ctry_level6_csquare *  
                      (aer_layers$value_aer_in_ctry_level6_csquare / aer_layers$landings_aer_in_ctry_level6_csquare) +
                      aer_layers$other_income_in_csquare -
                      aer_layers$unpaid_labour_in_csquare - aer_layers$varcosts_in_ctry_level6_csquare    -  aer_layers$oth_non_var_costs_in_csquare
   names(GVA) <- "GVA"
   add(aer_layers) <- GVA

   #plot(log(aer_layers))
   #sum(aer_layers$FishingHour[], na.rm=TRUE)
   #sum(aer_layers$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)
   aer_layers$effort <- aer_layers$FishingHour # generic name

   ns_raster               <- resample(ns_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
   ns_raster_eea_terra     <- project(ns_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra    <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

   dd   <- aggregate_from_raster_overlay(save_a_plot=TRUE, a_reg_name="NorthSea", a_reg_layer=ns_raster_eea_terra, a_data_layers=aer_layers_eea_terra, a_closed_area_layer=mpas_3035_msfd_rast_terra, name_closure="mpas_3035_msfd", name_fs=fs)
 

   #------
   fs <- "IRL_FPO_VL1824"
   filepath       <- file.path(getwd(), a_folder, fs, "2019_2021")
   aer_layers     <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
   aer_layers     <- terra::rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
   GVA <- aer_layers$landings_aer_in_ctry_level6_csquare *  
                      (aer_layers$value_aer_in_ctry_level6_csquare / aer_layers$landings_aer_in_ctry_level6_csquare) +
                      aer_layers$other_income_in_csquare -
                      aer_layers$unpaid_labour_in_csquare - aer_layers$varcosts_in_ctry_level6_csquare   -  aer_layers$oth_non_var_costs_in_csquare
   names(GVA) <- "GVA"
   add(aer_layers) <- GVA
   #plot(log(aer_layers))
   #sum(aer_layers$FishingHour[], na.rm=TRUE)
   #sum(aer_layers$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)
   aer_layers$effort <- aer_layers$FishingHour # generic name

   bob_raster               <- resample(bob_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
   bob_raster_eea_terra     <- project(bob_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
   aer_layers_eea_terra    <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

   dd   <- aggregate_from_raster_overlay(save_a_plot=TRUE, a_reg_name="BoB", a_reg_layer=bob_raster_eea_terra, a_data_layers=aer_layers_eea_terra, a_closed_area_layer=mpas_3035_msfd_rast_terra, name_closure="mpas_3035_msfd", name_fs=fs)
 



