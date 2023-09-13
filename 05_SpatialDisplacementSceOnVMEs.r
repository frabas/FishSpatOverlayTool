# Author: Francois Bastardie (DTU-Aqua), June 2023

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# SPATIAL DISPLACEMENT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
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


 # Some overall specs--------
 #a_folder  <- "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs"   # a VMS-AER layer
 #a_folder2  <- "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs_plots"   # a VMS-AER layer
 a_folder  <- "OUTCOME_FISHERIES_DISTR_FDI_AER"  # a FDI-AER layer 
 a_folder2  <- "OUTCOME_FISHERIES_DISTR_FDI_AER_plots"  # a FDI-AER layer 

 years_span <- "2018_2021"
 #years_span <- "2019"
 a_reg      <- "NEA" 
 #a_reg      <- "ALL_REGIONS" # default
 #a_reg      <- "BoB" 
 #---------------------------

 # a FOR-LOOP to make sure to get all combis...
 #specs <- expand.grid(years_span=c("2019", "2019_2021"), a_reg=c("ALL_REGIONS", "BoB"), a_folder=c("OUTCOME_FISHERIES_DISTR_FDI_AER", "OUTCOME_FISHERIES_DISTR_VMS_AER")) 
 #specs <- cbind.data.frame(specs, a_folder2=paste0(specs$a_folder, "_plots")) 
 #for (ispec in 1:nrow(specs)){
 #  a_folder   <- specs[ispec, "a_folder"] 
 #  a_folder2  <- specs[ispec, "a_folder2"] 
 #  years_span <- specs[ispec, "years_span"] 
 #  a_reg      <- specs[ispec, "a_reg"] 
 

  
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!VMEs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

library(sf)

  # The recent shp sent by EC:
  VMEs_MARE <- st_read(file.path(getwd(),"INPUT_SPATIAL_LAYERS","VMEs","DGMARE_Sc2Opt1", "Scenario2_option1_DG_Mare_Poly.shp"))
  ICES_VMEs_SceC <- st_read(file.path(getwd(),"INPUT_SPATIAL_LAYERS","VMEs","ICES_VMEs_Coordinates_shp", "Scenario_C.shp"))
  ICES_VMEs_SceD <- st_read(file.path(getwd(),"INPUT_SPATIAL_LAYERS","VMEs","ICES_VMEs_Coordinates_shp", "Scenario_D.shp"))
  plot(st_geometry(VMEs_MARE))
  plot(st_geometry(ICES_VMEs_SceC), col=rgb(0.5,0.5,0.5,0.5), add=TRUE)
  plot(st_geometry(ICES_VMEs_SceD), col=rgb(0.8,0.2,0.5,0.8), add=TRUE)

  # a visual check 
 #library(leaflet)
 #leaflet(allclosures_sf %>% dplyr::filter(year==2022)) %>% addTiles() %>% addPolygons(popup=~htmltools::htmlEscape(paste(popup, name)))  
 #leaflet(VMEs_MARE) %>% addTiles() %>% addPolygons(popup=~htmltools::htmlEscape(paste(Poly_No, ISO_TER1)))

 # stacked:
 library(leaflet)
 map <- leaflet() %>%
  addTiles() 
 map <- map %>% addPolygons(data=VMEs_MARE, popup=~htmltools::htmlEscape(paste(Poly_No, ISO_TER1)), col="blue", group = "VMEs_MARE")  %>%
      addPolygons(data=ICES_VMEs_SceC, popup=~htmltools::htmlEscape(paste(poly_id)), col="green", group = "ICES_VMEs_SceC")  %>%
      addPolygons(data=ICES_VMEs_SceD, popup=~htmltools::htmlEscape(paste(poly_id)), col="red", group = "ICES_VMEs_SceD")  
 map <- map %>% addLayersControl(
     baseGroups = "",
     overlayGroups = c("VMEs_MARE","ICES_VMEs_SceC", "ICES_VMEs_SceD"),
    options = layersControlOptions(collapsed = FALSE))
 map


  # ...or my own building:
  #allclosures_sf <- st_read(file.path(getwd(),"INPUT_SPATIAL_LAYERS", "VMEs","fba_closure_VMEs_2022.shp"))  # build from the DGMARE coords

  # sf vect to terra::vect to do some extract with it
  library(terra)
  VMEs_MARE_vect_terra              <- vect(VMEs_MARE)
  ICES_VMEs_SceC_vect_terra         <- vect(ICES_VMEs_SceC)
  ICES_VMEs_SceD_vect_terra         <- vect(ICES_VMEs_SceD)

  #VMEs_MARE_vect_terra            <- project(VMEs_MARE_vect_terra, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  #ICES_VMEs_SceC_vect_terra       <- project(ICES_VMEs_SceC_vect_terra, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  #ICES_VMEs_SceD_vect_terra       <- project(ICES_VMEs_SceD_vect_terra, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
  #=> cause we do the overlay in Lambert proj


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!DISPLACEMENT SCENARIOS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## CREATE RASTERS FOR RESTRICTED AREAS
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

 
#align with whatever spatRast file that will be later used....
 library(terra)
 filepath      <- file.path(getwd(),a_folder, "all_metiers", years_span)
 aer_layers    <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content

 
   if(length(grep( "VMS", a_folder))!=0){
      n <- 2
     aer_layers$effort <- aer_layers$FishingHour
     a_comment <- "vms"
   }else{
     n <- 10
     aer_layers$effort <- aer_layers$fditotfishdays
     a_comment <- "fdi"
   }
   
   
   # VMEs_MARE------------------
   # rasterize the closed areas
   dd                          <- VMEs_MARE_vect_terra
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  
   aer_layers_eea_terra        <- aer_layers
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   VMEs_MARE_rast_terra        <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   VMEs_MARE_rast_terra        <- aggregate(VMEs_MARE_rast_terra, n, "modal")
   # visual check
   a_width <- 4000 ; a_height <- 4000
   tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", paste0("logeffort_from_",years_span,"sce_2022_MARE_",a_comment,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
      #plot(log(aer_layers_eea_terra$effort), xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), asp=0.75)   # Lambert
      #plot(VMEs_MARE_vect_terra, col=rgb(0.2,0.6,0.7,0.3), xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), add=TRUE)
      #plot(VMEs_MARE_rast_terra, col=rgb(0.2,0.2,0.2,0.3),  xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), add=TRUE, legend=FALSE)
      plot(log(aer_layers_eea_terra$effort), xlim=c(-17, 1), ylim=c(35, 62), asp=0.75)    # latlong
      plot(VMEs_MARE_vect_terra, col=rgb(0.2,0.6,0.7,0.1), border="red", xlim=c(-17, 1), ylim=c(35, 62), add=TRUE)
      #plot(VMEs_MARE_rast_terra, col=rgb(0.2,0.2,0.2,0.3),  xlim=c(-17, 1), ylim=c(35, 62), add=TRUE, legend=FALSE)
      library(rnaturalearth)
      spdf_europe <- ne_countries(continent = "europe", scale=10)
      sf_world <- ne_countries(returnclass='sf', scale=10)
      plot(vect(sf_world), col=grey(0.8), add=TRUE)
   dev.off()
  #----------------------------
 
   # ICES_VMEs_SceC------------------
   # rasterize the closed areas
   dd                          <- ICES_VMEs_SceC_vect_terra
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  
   aer_layers_eea_terra        <- aer_layers
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   ICES_VMEs_SceC_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   ICES_VMEs_SceC_rast_terra   <- aggregate(ICES_VMEs_SceC_rast_terra, n, "modal")
   # visual check
   a_width <- 4000 ; a_height <- 4000
  tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", paste0("logeffort_from_",years_span,"sce_ICES_VMEs_SceC_",a_comment,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
      #plot(log(aer_layers_eea_terra$effort), xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), asp=0.75)   # Lambert
      #plot(VMEs_MARE_vect_terra, col=rgb(0.2,0.6,0.7,0.3), xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), add=TRUE)
      #plot(VMEs_MARE_rast_terra, col=rgb(0.2,0.2,0.2,0.3),  xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), add=TRUE, legend=FALSE)
      plot(log(aer_layers_eea_terra$effort), xlim=c(-17, 1), ylim=c(35, 62), asp=0.75)    # latlong
      plot(ICES_VMEs_SceC_vect_terra, col=rgb(0.2,0.6,0.7,0.1), border="red", xlim=c(-17, 1), ylim=c(35, 62), add=TRUE)
      #plot(ICES_VMEs_SceC_rast_terra, col=rgb(0.2,0.2,0.2,0.3),  xlim=c(-17, 1), ylim=c(35, 62), add=TRUE, legend=FALSE)
      library(rnaturalearth)
      spdf_europe <- ne_countries(continent = "europe", scale=10)
      sf_world <- ne_countries(returnclass='sf', scale=10)
      plot(vect(sf_world), col=grey(0.8), add=TRUE)
   dev.off()
  #----------------------------
 
   # ICES_VMEs_SceD------------------
   # rasterize the closed areas
   dd                          <- ICES_VMEs_SceD_vect_terra
   dd$value                    <- 1
   #aer_layers_eea_terra        <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  
   aer_layers_eea_terra        <- aer_layers
   aer_layers_eea_terra_disagg <- disagg(aer_layers_eea_terra, n) # a trick for FDI for this rasterisation given the large difference in resolution: disaggregate to high resolution, rasterize then re-aggregate....
   ICES_VMEs_SceD_rast_terra   <- terra::rasterize(dd, y=aer_layers_eea_terra_disagg, field="value", fun=sum, na.rm=TRUE)
   ICES_VMEs_SceD_rast_terra   <- aggregate(ICES_VMEs_SceD_rast_terra, n, "modal")
   # visual check
   a_width <- 4000 ; a_height <- 4000
  tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", paste0("logeffort_from_",years_span,"sce_ICES_VMEs_SceD_",a_comment,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
      #plot(log(aer_layers_eea_terra$effort), xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), asp=0.75)   # Lambert
      #plot(ICES_VMEs_SceD_vect_terra, col=rgb(0.2,0.6,0.7,0.3), xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), add=TRUE)
      #plot(ICES_VMEs_SceD_rast_terra, col=rgb(0.2,0.2,0.2,0.3),  xlim=c(2e6, 3.5e6), ylim=c(1.5e6, 4e6), add=TRUE, legend=FALSE)
      plot(log(aer_layers_eea_terra$effort), xlim=c(-17, 1), ylim=c(35, 62), asp=0.75)    # latlong
      plot(ICES_VMEs_SceD_vect_terra, col=rgb(0.2,0.6,0.7,0.1), border="red", xlim=c(-17, 1), ylim=c(35, 62), add=TRUE)
      #plot(ICES_VMEs_SceD_rast_terra, col=rgb(0.2,0.2,0.2,0.3),  xlim=c(-17, 1), ylim=c(35, 62), add=TRUE, legend=FALSE)
      library(rnaturalearth)
      spdf_europe <- ne_countries(continent = "europe", scale=10)
      sf_world <- ne_countries(returnclass='sf', scale=10)
      plot(vect(sf_world), col=grey(0.8), add=TRUE)
   dev.off()
  #----------------------------
 
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## CREATE LOOKUP FOR RESTRICTION SPECS
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # read-in a layer and proceed to effort displacement in a systematic way
  library(terra)
  dir.create(file.path(getwd(),"OUTCOME_DISPLACEMENT_VMEs", a_folder), recursive=TRUE)
  lst_files <- list.files(file.path(getwd(), a_folder))

  # example of fishing-technique-specific specs
  
  #------------
  # Scenario VMEs_MARE_vect_terra
  dd <- NULL
  for(fs in lst_files)
     {
     areas <- NULL
     if(length(grep("TM", fs))>0 | length(grep("PS", fs))>0)   areas <- ""
     if(length(grep("DTS", fs))>0 | length(grep("TBB", fs))>0 | length(grep("DRB", fs))>0 | length(grep("DFN", fs))>0 | length(grep("FPO", fs))>0 | length(grep("HOK", fs))>0)  areas <- c("VMEs_MARE_rast_terra")
     if(length(areas)==0)  areas <- "" # default
     dd <- rbind.data.frame(
                                dd,
                                expand.grid(fs=fs, restricted_area=areas)
                                )
     }
    # filter out the fs not concerned by any closures...
    restriction_per_fs_sce_baseline2022 <- dd[dd[,"restricted_area"]!="",]
  #------------
     
  #------------
  # Scenario ICES_VMEs_SceC_rast_terra
  dd <- NULL
  for(fs in lst_files)
     {
     areas <- NULL
     if(length(grep("TM", fs))>0 | length(grep("PS", fs))>0)   areas <- ""
     if(length(grep("DTS", fs))>0 | length(grep("TBB", fs))>0 | length(grep("DRB", fs))>0 | length(grep("DFN", fs))>0 | length(grep("FPO", fs))>0 | length(grep("HOK", fs))>0)  areas <- c("ICES_VMEs_SceC_rast_terra")
     if(length(areas)==0)  areas <- "" # default
     dd <- rbind.data.frame(
                                dd,
                                expand.grid(fs=fs, restricted_area=areas)
                                )
     }
    # filter out the fs not concerned by any closures...
    restriction_per_fs_sceC <- dd[dd[,"restricted_area"]!="",]
  #------------
     
 
  #------------
  # Scenario ICES_VMEs_SceD_rast_terra
  dd <- NULL
  for(fs in lst_files)
     {
     areas <- NULL
     if(length(grep("TM", fs))>0 | length(grep("PS", fs))>0)   areas <- ""
     if(length(grep("DTS", fs))>0 | length(grep("TBB", fs))>0 | length(grep("DRB", fs))>0 | length(grep("DFN", fs))>0 | length(grep("FPO", fs))>0 | length(grep("HOK", fs))>0)  areas <- c("ICES_VMEs_SceD_rast_terra")
     if(length(areas)==0)  areas <- "" # default
     dd <- rbind.data.frame(
                                dd,
                                expand.grid(fs=fs, restricted_area=areas)
                                )
     }
    # filter out the fs not concerned by any closures...
    restriction_per_fs_sceD <- dd[dd[,"restricted_area"]!="",]
  #------------
     
     
  restriction_per_fs_per_sce <- list(NULL)   
  restriction_per_fs_per_sce[[1]] <- restriction_per_fs_sce_baseline2022
  restriction_per_fs_per_sce[[2]] <- restriction_per_fs_sceC
  restriction_per_fs_per_sce[[3]] <- restriction_per_fs_sceD
  names(restriction_per_fs_per_sce) <- c("Closure2022", "ICESSceC", "ICESSceD")



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## CROP ALL LAYERS TO A REGION (OPTIONAL)
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


if(a_reg!="ALL_REGIONS"){

   # restrict the analysis to a given region:
   if(a_reg=="BoB"){
     bob_raster_005            <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","BoB_raster_based_on_FAO_reg.tiff"))         # FAO 27.8
     bob_raster                <- terra::resample(bob_raster_005, aer_layers, method = 'bilinear') # if FDI because FDI is in 1x0.5 degree: resample to get the right matching resolution
     #bob_raster_eea_terra      <- terra::project(bob_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
     bob_raster_eea_terra      <- bob_raster
     bob_raster_eea_terra      <- trim(bob_raster_eea_terra)

     owf_msfd_rast_terra                      <- crop(owf_msfd_rast_terra, bob_raster_eea_terra)
     mpas_owf_msfd_rast_terra                 <- crop(mpas_owf_msfd_rast_terra, bob_raster_eea_terra)
     mpas_3035_msfd_rast_terra                <- crop(mpas_3035_msfd_rast_terra, bob_raster_eea_terra)
     mpas_3035_msfd_rast_terra_bottomtrawlers <- crop(mpas_3035_msfd_rast_terra_bottomtrawlers, bob_raster_eea_terra) 
     mpas_3035_msfd_rast_terra_gillnetters    <- crop(mpas_3035_msfd_rast_terra_gillnetters, bob_raster_eea_terra)
     mpas_3035_msfd_rast_terra_longliners     <- crop(mpas_3035_msfd_rast_terra_longliners, bob_raster_eea_terra) 
    # etc.
     
     #=> the data layer will then be cropped accordingly in the below fs LOOP
   }

   if(a_reg=="NEA"){
     nea_raster                <- terra::rast(file.path(getwd(),"INPUT_SPATIAL_LAYERS","REGION_CODING","NEA_raster_based_on_FAO_reg.tiff"))   # res already in 0.5 by 0.5
     #nea_raster_eea_terra      <- terra::project(nea_raster, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
     nea_raster_eea_terra      <- nea_raster
     nea_raster_eea_terra      <- trim(nea_raster_eea_terra)

     VMEs_MARE_rast_terra                           <- crop(VMEs_MARE_rast_terra, nea_raster_eea_terra)
     ICES_VMEs_SceC_rast_terra                      <- crop(ICES_VMEs_SceC_rast_terra, nea_raster_eea_terra)
     ICES_VMEs_SceD_rast_terra                      <- crop(ICES_VMEs_SceD_rast_terra, nea_raster_eea_terra)
   } 


} # end if reg




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## APPLY A DISPLACEMENT
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
  dir.create(file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, "Plots"), recursive=TRUE)
  fs_collector <- NULL
  for(sce in 1: length( restriction_per_fs_per_sce )) { # loop over Sces
     count <- 0
     fs_to_screen <- unique(as.character(restriction_per_fs_per_sce[[sce]][,1]))
     for(fs in fs_to_screen){  # loop over concerned fs
        count <- count+1
        scename <- names(restriction_per_fs_per_sce)[sce]
        restriction_per_fs <- restriction_per_fs_per_sce[[sce]]
        cat(paste0("this sce: ", scename, "\n"))
        cat(paste0(fs, "...", count, " out of ", nrow(restriction_per_fs)," files\n"))
    filepath       <- file.path(getwd(), a_folder, fs, years_span)
     er <- try(   {
     aer_layers   <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
     # re-project
     #aer_layers_eea_terra       <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
     aer_layers_eea_terra       <- aer_layers
     #=> caution: as a reprojection slightly interpolate some numbers it is needed to do it at the very beginining....
     
     if(a_folder=="OUTCOME_FISHERIES_DISTR_FDI_AER" || a_folder=="OUTCOME_FISHERIES_DISTR_FDI_AER_VMEs" ){
           aer_layers_eea_terra$effort          <- aer_layers_eea_terra$fditotfishdays # renaming for generic 
           aer_layers_eea_terra$lpue            <-  aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare/aer_layers_eea_terra$effort  # kg per fishing days
           #aer_layers_eea_terra$effort          <- aer_layers_eea_terra$daysatsea_aer_in_ctry_level6_csquare  
           #aer_layers_eea_terra$lpue            <-  aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare/aer_layers_eea_terra$daysatsea_aer_in_ctry_level6_csquare  # kg per daysatsea        
           values(aer_layers_eea_terra$lpue)    <- replace (values(aer_layers_eea_terra$lpue)[,1], is.infinite(values(aer_layers_eea_terra$lpue)[,1]), 0)  # fix Inf lpues       
           }
     if(a_folder=="OUTCOME_FISHERIES_DISTR_VMS_AER" || a_folder=="OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs"){
           aer_layers_eea_terra$effort          <- aer_layers_eea_terra$FishingHour # renaming for generic    
           #aer_layers_eea_terra$lpue            <- aer_layers_eea_terra$lpue_csquare_vms_kgperfhour
           aer_layers_eea_terra$lpue            <- aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare/aer_layers_eea_terra$effort
           }
   }, silent=TRUE)
   if(class(er)!="try-error"){
   
   # adding a variable that will be used for a weighted re-distribution of effort
   GVA <- aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare *  
                      (aer_layers_eea_terra$value_aer_in_ctry_level6_csquare / aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare) +
                      aer_layers_eea_terra$other_income_in_csquare -
                      aer_layers_eea_terra$unpaid_labour_in_csquare - aer_layers_eea_terra$varcosts_in_ctry_level6_csquare
   names(GVA)      <- "GVA"
   add(aer_layers_eea_terra) <- GVA
         
   
   # align
   names_restricting_lyrs_this_fs     <- as.character(restriction_per_fs[restriction_per_fs$fs==fs, "restricted_area"])
   if(a_reg!="ALL_REGIONS")   aer_layers_eea_terra <- crop(aer_layers_eea_terra, get(names_restricting_lyrs_this_fs[1])) # caution: required for matching spatiat extents

   # build a closed area spatRast specific to this fs
     er <- try(   {
        area_restricted_this_fs <- rast(nrow=dim(aer_layers_eea_terra)[1], ncol=dim(aer_layers_eea_terra)[2],
                                     extent=ext(aer_layers_eea_terra), res=res(aer_layers_eea_terra), crs=crs(aer_layers_eea_terra), vals=NA, names="value") # init
        for(a_layer_name in names_restricting_lyrs_this_fs)
        {                                                        
           a_lyr <-  get(a_layer_name) 
           area_restricted_this_fs <- sum(area_restricted_this_fs, a_lyr, na.rm=TRUE) # [caution: can make R crash if extents are incompatible]
        }
    }, silent=TRUE)
   if(class(er)!="try-error"){
    
    # build the complementary non-closed areas layer
    area_open_this_fs                       <- area_restricted_this_fs # init
    area_open_this_fs []                    <- NA
    values(area_open_this_fs) [is.na(values(area_restricted_this_fs))] <- 1
    # overlay the masks
    data_layers_on_area_restricted_this_fs  <- aer_layers_eea_terra * area_restricted_this_fs  #* a_reg_layer
    data_layers_on_area_open_this_fs        <- aer_layers_eea_terra * area_open_this_fs  #* a_reg_layer
    data_layers_on_all_areas_this_fs        <- aer_layers_eea_terra * sum(area_open_this_fs, area_restricted_this_fs, na.rm=TRUE) # useful for comparing...
    
    # check
    if(FALSE){
       par(mfrow=c(1,3))
       plot(log(data_layers_on_area_restricted_this_fs$effort), main="inside")
       plot(log(data_layers_on_area_open_this_fs$effort), main="outside")
       plot(log(data_layers_on_all_areas_this_fs$effort), main="all")
    }
    
    
    mean(data_layers_on_area_restricted_this_fs$lpue[], na.rm=TRUE)
    mean(data_layers_on_area_open_this_fs$lpue[], na.rm=TRUE)

    
    # displace effort uniformly
    amount_effort_displaced    <- sum(data_layers_on_area_restricted_this_fs$effort[], na.rm=TRUE)
    amount_landings_inside     <- sum(data_layers_on_area_restricted_this_fs$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)
    nb_cells_opened            <- length(which(!is.na(data_layers_on_area_open_this_fs$effort[])))
    uniform_redistribution     <- data_layers_on_area_open_this_fs$effort + amount_effort_displaced/nb_cells_opened   
 
    # displace effort with a weigthing (here the GVA)
    a_sum <- sum(data_layers_on_all_areas_this_fs$effort[], na.rm=TRUE) 
    if(a_sum>0) {
       # share on open areas only
       gvas  <- log(data_layers_on_area_open_this_fs$GVA[][,1])   
       a_sum <- sum(gvas[!is.na(gvas)& !is.infinite(gvas) & gvas>0])
       GVA_share <- log(data_layers_on_area_open_this_fs$GVA+1)  
       GVA_share[GVA_share<0] <- 0 # we don´t want to redistribute on areas with negative GVA
       GVA_share <- GVA_share/a_sum   # share key
       sum(GVA_share[], na.rm=TRUE)# => 1   
       # share over all cells
       gvas  <- log(data_layers_on_all_areas_this_fs$GVA[][,1])   
       a_sum <- sum(gvas[!is.na(gvas)& !is.infinite(gvas) & gvas>0])
       GVA_share_over_all_cells <- log(data_layers_on_all_areas_this_fs$GVA+1)  
       GVA_share_over_all_cells[GVA_share_over_all_cells<0] <- 0 # we don´t want to redistribute on areas with negative GVA
       a_sum <- sum(gvas[!is.na(gvas)& !is.infinite(gvas) & gvas>0])
       GVA_share_over_all_cells <- GVA_share_over_all_cells/a_sum   # share key   
       sum(GVA_share_over_all_cells[], na.rm=TRUE)# => 1   
       variable_used_to_redistribute   <- "GVA"
    } else{ # GVA info missing (i.e. all at 0) so assume re-distribution on effort info only
       efforts   <- log(data_layers_on_area_open_this_fs$effort[][,1])   
       a_sum     <- sum(efforts[!is.na(efforts)& !is.infinite(efforts) & efforts>0])
       GVA_share <- data_layers_on_area_open_this_fs$effort/a_sum   # share key
       sum(GVA_share[], na.rm=TRUE)# => 1   
       # share over all cells
       efforts_all_cells <- log(data_layers_on_all_areas_this_fs$effort[][,1])  
       a_sum     <- sum(efforts[!is.na(efforts)& !is.infinite(efforts) & efforts>0])
       GVA_share_over_all_cells <- GVA_share_over_all_cells/a_sum   # share key
       variable_used_to_redistribute   <- "effort"
       # check 
       sum(GVA_share_over_all_cells[], na.rm=TRUE)# => 1   
    }
    names(GVA_share) <- "GVA_share"
    add(data_layers_on_area_open_this_fs) <- GVA_share
    weighted_redistribution  <- sum(data_layers_on_area_open_this_fs$effort, (amount_effort_displaced*data_layers_on_area_open_this_fs$GVA_share), na.rm=TRUE)   
 
    # build a comparable baseline layer that already assume a distribution based on GVA
    names(GVA_share_over_all_cells) <- "GVA_share_over_all_cells"
    add(data_layers_on_all_areas_this_fs) <- GVA_share_over_all_cells
    nb_cells_all <- length(which(!is.na(data_layers_on_area_open_this_fs$effort[])))
   
    # but first it is required to assign 0 to detected effort cells with debt when removing some effort uniformly (a debt arises when a piece of removals is greater than actual effort on cell)
    uniform_effort_removals <- (data_layers_on_all_areas_this_fs$effort-(amount_effort_displaced/nb_cells_all))
    dd <- values(uniform_effort_removals)
    effort_debt <- abs(sum(c(dd)[!is.na(dd) & dd<0], na.rm=TRUE))
    dd[!is.na(dd) & dd<=0] <- 0
    values(uniform_effort_removals$effort) <- dd        
    actual_effort_displaced <-  (sum(data_layers_on_all_areas_this_fs$effort[], na.rm=TRUE)- sum(uniform_effort_removals$effort[], na.rm=TRUE))
    #weighted_distribution_baseline  <- sum(uniform_effort_removals,  ((amount_effort_displaced-effort_debt)*data_layers_on_all_areas_this_fs$GVA_share_over_all_cells), na.rm=TRUE )  
    weighted_distribution_baseline  <- sum(uniform_effort_removals,  (actual_effort_displaced)*data_layers_on_all_areas_this_fs$GVA_share_over_all_cells, na.rm=TRUE )  
    # check for effort conservation
    sum(data_layers_on_all_areas_this_fs$effort[], na.rm=TRUE)
    sum(uniform_effort_removals$effort[], na.rm=TRUE)
    sum(weighted_distribution_baseline$effort[], na.rm=TRUE)
    
  
 
   if(FALSE){
    # check 
    par(mfrow=c(2,2))
    plot(log(data_layers_on_area_open_this_fs$effort), breaks=seq(0, 12, by=1))
    #plot(log(uniform_redistribution$effort), breaks=seq(0, 12, by=1))
    plot(log(weighted_redistribution$effort), breaks=seq(0, 12, by=1))
    plot(log(weighted_distribution_baseline$effort), breaks=seq(0, 12, by=1))
    plot(log(weighted_redistribution$effort)-log(weighted_distribution_baseline$effort))#, breaks=seq(-0.5, 0.5, by=0.05)) # plot a diff
   
   } # end FALSE
      
    # add effort redistribution layers to the outcome
    names(uniform_redistribution) <- "EffortDisplUniform"
    add(data_layers_on_area_open_this_fs) <- uniform_redistribution
    names(weighted_redistribution) <- "EffortDisplWeighted"
    add(data_layers_on_area_open_this_fs) <- weighted_redistribution
    names(weighted_distribution_baseline) <- "EffortDistrWeighted"
    add(aer_layers_eea_terra) <- weighted_distribution_baseline
 
 
    # recompute other variables after the re-distribution i.e. catches and GVA deduced from the LPUEs 
    landings_base <-  
        aer_layers_eea_terra$effort * aer_layers_eea_terra$lpue  # or lpue_csquare_aer_kgperdayatsea if FDI(TODO: check)
    # add
    landings_after_uniform_redistrib <-  
        data_layers_on_area_open_this_fs$EffortDisplUniform * data_layers_on_area_open_this_fs$lpue  # or lpue_csquare_aer_kgperdayatsea if FDI(TODO: check)
    names(landings_after_uniform_redistrib) <- "landings_after_uniform_redistrib"
    # a quick check: should return same value if amount_effort_displaced is 0
     sum(landings_base[], na.rm=TRUE)
     sum(aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)
     sum(landings_after_uniform_redistrib[], na.rm=TRUE)   
    add(data_layers_on_area_open_this_fs) <- landings_after_uniform_redistrib
    # add
    landings_after_weigthed_redistrib <-  
        data_layers_on_area_open_this_fs$EffortDisplWeighted * data_layers_on_area_open_this_fs$lpue
    names(landings_after_weigthed_redistrib) <- "landings_after_weigthed_redistrib"
    add(data_layers_on_area_open_this_fs) <- landings_after_weigthed_redistrib
    # add
    landings_after_weigthed_distrib_baseline <-  
        aer_layers_eea_terra$EffortDistrWeighted * aer_layers_eea_terra$lpue
    names(landings_after_weigthed_distrib_baseline) <- "landings_after_weigthed_distrib_baseline"
     # a quick check: should return same value if amount_effort_displaced is 0
     sum(aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)
     sum(landings_after_weigthed_distrib_baseline[], na.rm=TRUE)   
    add(aer_layers_eea_terra) <- landings_after_weigthed_distrib_baseline
 
    GVArecomputed <- data_layers_on_area_open_this_fs$landings_after_weigthed_redistrib *  
                      (data_layers_on_area_open_this_fs$value_aer_in_ctry_level6_csquare / data_layers_on_area_open_this_fs$landings_aer_in_ctry_level6_csquare) +
                      data_layers_on_area_open_this_fs$other_income_in_csquare -
                      data_layers_on_area_open_this_fs$unpaid_labour_in_csquare - data_layers_on_area_open_this_fs$varcosts_in_ctry_level6_csquare
    names(GVArecomputed) <- "GVArecomputed"
    add(data_layers_on_area_open_this_fs) <- GVArecomputed
    
    GVArecomputed_u <- data_layers_on_area_open_this_fs$landings_after_uniform_redistrib *  
                      (data_layers_on_area_open_this_fs$value_aer_in_ctry_level6_csquare / data_layers_on_area_open_this_fs$landings_aer_in_ctry_level6_csquare) +
                      data_layers_on_area_open_this_fs$other_income_in_csquare -
                      data_layers_on_area_open_this_fs$unpaid_labour_in_csquare - data_layers_on_area_open_this_fs$varcosts_in_ctry_level6_csquare
    names(GVArecomputed_u) <- "GVArecomputed_u"
    add(data_layers_on_area_open_this_fs) <- GVArecomputed_u
 
    # a comparable counterfactual
    GVArecomputed_b <- aer_layers_eea_terra$landings_after_weigthed_distrib_baseline *  
                      (aer_layers_eea_terra$value_aer_in_ctry_level6_csquare / aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare) +
                      aer_layers_eea_terra$other_income_in_csquare -
                      aer_layers_eea_terra$unpaid_labour_in_csquare - aer_layers_eea_terra$varcosts_in_ctry_level6_csquare
    names(GVArecomputed_b) <- "GVArecomputed_b"
    add(aer_layers_eea_terra) <- GVArecomputed_b
 
   # check
    a_width <- 7000 ; a_height <- 4000
    tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, "Plots", paste0(fs, "GVArecomputed_from_",years_span,"_sce_",scename,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
       par(mfrow=c(1,4))
       par(mar=c(2,2,1,1))
       plot(log(aer_layers_eea_terra$GVA), breaks=seq(0, 16, by=2), main="Initial GVA")  
       plot(log(data_layers_on_area_open_this_fs$GVArecomputed), breaks=seq(0, 16, by=2), main="After displacing with weight")  
       plot(area_restricted_this_fs, col=rgb(0.2,0.2,0.2,0.2), add=TRUE)
       plot(log(data_layers_on_area_open_this_fs$GVArecomputed_u), breaks=seq(0, 16, by=2), main="After displacing uniformly")  
       plot(area_restricted_this_fs, col=rgb(0.2,0.2,0.2,0.2), add=TRUE)
       plot(log(aer_layers_eea_terra$GVArecomputed_b), breaks=seq(0, 16, by=2), main="After distributing with weight")  
       plot(area_restricted_this_fs, col=rgb(0.2,0.2,0.2,0.2), add=TRUE)
    dev.off()


     if(FALSE){
        par(mfrow=c(1,4))
        plot(aer_layers_eea_terra$GVA)  
        plot(data_layers_on_area_open_this_fs$GVArecomputed)
        plot(area_restricted_this_fs, col=rgb(0.2,0.2,0.2,0.2), add=TRUE)
        plot(data_layers_on_area_open_this_fs$GVArecomputed_u)  
        plot(area_restricted_this_fs, col=rgb(0.2,0.2,0.2,0.2), add=TRUE)
        plot(aer_layers_eea_terra$GVArecomputed_b)  
        plot(area_restricted_this_fs, col=rgb(0.2,0.2,0.2,0.2), add=TRUE)
     }
     
    
    # save
    filepath             <- file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder, a_reg, fs, years_span)  
    dir.create(filepath, recursive=TRUE)
    writeRaster(data_layers_on_area_open_this_fs, filename=file.path(filepath, "spatRaster.tif"), overwrite=TRUE)
  

    
    # a_logratio for GVA
    GAV_after   <- sum(data_layers_on_area_open_this_fs$GVArecomputed[], na.rm=TRUE) 
    GAV_base    <- sum(aer_layers_eea_terra$GVA[], na.rm=TRUE)   
    ## CAUTION THE LOG RATIO WHEN POTENTIAL NEGATIVE VALUES MAKES IT TRICKY:      
    if(GAV_base >=0  && GAV_after>=0 && abs(GAV_base)>= abs(GAV_after)){
         a_logratio     <- log(GAV_after/GAV_base) # this is in the positive interval
        }      
    if(GAV_base >=0 && GAV_after>=0 && abs(GAV_base)<= abs(GAV_after)){
         a_logratio     <- log(GAV_after/GAV_base) # this is in the positive interval
       }             
    if(GAV_base <0 && GAV_after <0 && abs(GAV_base)>= abs(GAV_after)) {
         a_logratio     <- -log(GAV_after/GAV_base) # this is worsening (in the negative interval)
       }
    if(GAV_base <0 && GAV_after <0 && abs(GAV_base)<= abs(GAV_after)) {
         a_logratio     <- log(GAV_after/GAV_base) # this is an improvement (in the negative interval)
       }
    if(GAV_base <0 && GAV_after >=0 ) {
         a_logratio   <- log((abs(GAV_base)+GAV_after)) # this is an improvement 
       }
    if(GAV_base >0 && GAV_after <=0 ) {
         a_logratio     <- -log((abs(GAV_after)+GAV_base)) # this is a worsening situation
       }
      
    # a_logratio_u for GVA
    GAV_after   <- sum(data_layers_on_area_open_this_fs$GVArecomputed_u[], na.rm=TRUE) 
    GAV_base    <- sum(aer_layers_eea_terra$GVA[], na.rm=TRUE)   
    ## CAUTION THE LOG RATIO WHEN POTENTIAL NEGATIVE VALUES MAKES IT TRICKY:      
    if(GAV_base >=0  && GAV_after>=0 && abs(GAV_base)>= abs(GAV_after)){
         a_logratio_u     <- log(GAV_after/GAV_base) # this is in the positive interval
        }      
    if(GAV_base >=0 && GAV_after>=0 && abs(GAV_base)<= abs(GAV_after)){
         a_logratio_u     <- log(GAV_after/GAV_base) # this is in the positive interval
       }             
    if(GAV_base <0 && GAV_after <0 && abs(GAV_base)>= abs(GAV_after)) {
         a_logratio_u     <- -log(GAV_after/GAV_base) # this is worsening (in the negative interval)
       }
    if(GAV_base <0 && GAV_after <0 && abs(GAV_base)<= abs(GAV_after)) {
         a_logratio_u     <- log(GAV_after/GAV_base) # this is an improvement (in the negative interval)
       }
    if(GAV_base <0 && GAV_after >=0 ) {
         a_logratio_u   <- log((abs(GAV_base)+GAV_after)) # this is an improvement 
       }
    if(GAV_base >0 && GAV_after <=0 ) {
         a_logratio_u     <- -log((abs(GAV_after)+GAV_base)) # this is a worsening situation
       }
    
    # a_logratio_b for GVA
    GAV_after   <- sum(data_layers_on_area_open_this_fs$GVArecomputed[], na.rm=TRUE) 
    GAV_base    <- sum(aer_layers_eea_terra$GVArecomputed_b[], na.rm=TRUE)   
    ## CAUTION THE LOG RATIO WHEN POTENTIAL NEGATIVE VALUES MAKES IT TRICKY:      
    if(GAV_base >=0  && GAV_after>=0 && abs(GAV_base)>= abs(GAV_after)){
         a_logratio_b     <- log(GAV_after/GAV_base) # this is in the positive interval
        }      
    if(GAV_base >=0 && GAV_after>=0 && abs(GAV_base)<= abs(GAV_after)){
         a_logratio_b     <- log(GAV_after/GAV_base) # this is in the positive interval
       }             
    if(GAV_base <0 && GAV_after <0 && abs(GAV_base)>= abs(GAV_after)) {
         a_logratio_b     <- -log(GAV_after/GAV_base) # this is worsening (in the negative interval)
       }
    if(GAV_base <0 && GAV_after <0 && abs(GAV_base)<= abs(GAV_after)) {
         a_logratio_b     <- log(GAV_after/GAV_base) # this is an improvement (in the negative interval)
       }
    if(GAV_base <0 && GAV_after >=0 ) {
         a_logratio_b   <- log((abs(GAV_base)+GAV_after)) # this is an improvement 
       }
    if(GAV_base >0 && GAV_after <=0 ) {
         a_logratio_b     <- -log((abs(GAV_after)+GAV_base)) # this is a worsening situation
       }

    # a cherry on top of the cake: search for required extra effort to break even if GVA <0
    extra_effort <- 1
    if(!is.na(a_logratio_b) & a_logratio_b<0)
    {
     cat(paste0("Brute search for ", fs, "...\n"))
        # brute search
        this_logratio <- a_logratio_b
        while(this_logratio<0){
          extra_effort <- extra_effort + 0.01
          dd <- extra_effort* data_layers_on_area_open_this_fs$lpue * data_layers_on_area_open_this_fs$EffortDisplWeighted * 
                      (data_layers_on_area_open_this_fs$value_aer_in_ctry_level6_csquare / data_layers_on_area_open_this_fs$landings_aer_in_ctry_level6_csquare) +
                      data_layers_on_area_open_this_fs$other_income_in_csquare -
                      data_layers_on_area_open_this_fs$unpaid_labour_in_csquare - (extra_effort*data_layers_on_area_open_this_fs$varcosts_in_ctry_level6_csquare)
            extra_varcosts <- (extra_effort*data_layers_on_area_open_this_fs$varcosts_in_ctry_level6_csquare)-data_layers_on_area_open_this_fs$varcosts_in_ctry_level6_csquare # proxy of most likely extra fuel use needed        
      a_sum <- sum(dd[], na.rm=TRUE)
         this_logratio <- log(a_sum/GAV_base)
           # a_logratio_b for GVA
         GAV_after   <- a_sum
         GAV_base    <- sum(aer_layers_eea_terra$GVArecomputed_b[], na.rm=TRUE)   
         ## CAUTION THE LOG RATIO WHEN POTENTIAL NEGATIVE VALUES MAKES IT TRICKY:      
        if(GAV_base >=0  && GAV_after>=0 && abs(GAV_base)>= abs(GAV_after)){
         this_logratio     <- log(GAV_after/GAV_base) # this is in the positive interval
        }      
        if(GAV_base >=0 && GAV_after>=0 && abs(GAV_base)<= abs(GAV_after)){
         this_logratio     <- log(GAV_after/GAV_base) # this is in the positive interval
       }             
       if(GAV_base <0 && GAV_after <0 && abs(GAV_base)>= abs(GAV_after)) {
         this_logratio     <- -log(GAV_after/GAV_base) # this is worsening (in the negative interval)
       }
       if(GAV_base <0 && GAV_after <0 && abs(GAV_base)<= abs(GAV_after)) {
         this_logratio     <- log(GAV_after/GAV_base) # this is an improvement (in the negative interval)
       }
       if(GAV_base <0 && GAV_after >=0 ) {
         this_logratio   <- log((abs(GAV_base)+GAV_after)) # this is an improvement 
       }
       if(GAV_base >0 && GAV_after <=0 ) {
         this_logratio     <- -log((abs(GAV_after)+GAV_base)) # this is a worsening situation
       }
  
       #browser()         
         if(extra_effort>2) break # exit if multiplier requirement >2
       }
     cat(paste0("Brute search done for ", fs, "...found extra_effort is ",  extra_effort, "\n"))
    
    }
  
      mean(data_layers_on_area_restricted_this_fs$lpue[], na.rm=TRUE)
    mean(data_layers_on_area_open_this_fs$lpue[], na.rm=TRUE)

    
    # collect
    fs_collector <- rbind.data.frame(fs_collector,
                                     cbind.data.frame(sce=scename, fs=fs, variable="effort_before", value=sum(aer_layers_eea_terra$effort[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="effort_after_uniform_redistr", value= sum(data_layers_on_area_open_this_fs$EffortDisplUniform[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="effort_after_weigthed_redistr", value=sum(data_layers_on_area_open_this_fs$EffortDisplWeighted[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="amount_effort_displaced", value=amount_effort_displaced, variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="amount_landings_inside", value=amount_landings_inside, variable_used_to_redistribute=variable_used_to_redistribute),                               
                                     cbind.data.frame(sce=scename, fs=fs, variable="lpue_inside", value=mean(data_layers_on_area_restricted_this_fs$lpue[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),                               
                                     cbind.data.frame(sce=scename, fs=fs, variable="lpue_outside", value=mean(data_layers_on_area_open_this_fs$lpue[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),                               
                                     cbind.data.frame(sce=scename, fs=fs, variable="landings_kg_before", value= sum(aer_layers_eea_terra$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="landings_kg_after_uniform_redistr", value= sum(data_layers_on_area_open_this_fs$landings_after_uniform_redistrib[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="landings_kg_after_weigthed_redistr", value= sum(data_layers_on_area_open_this_fs$landings_after_weigthed_redistrib[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="GVA_before", value= sum(aer_layers_eea_terra$GVA[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="GVA_after_uniform_redistr", value= sum(data_layers_on_area_open_this_fs$GVArecomputed_u[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="GVA_after_weigthed_redistr", value= sum(data_layers_on_area_open_this_fs$GVArecomputed[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="GVA_after_weigthed_distr", value= sum(aer_layers_eea_terra$GVArecomputed_b[], na.rm=TRUE), variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="logratio_u_GVA_after_before", value= a_logratio_u, variable_used_to_redistribute=variable_used_to_redistribute),                                                     
                                     cbind.data.frame(sce=scename, fs=fs, variable="logratio_b_GVA_after_before", value= a_logratio_b, variable_used_to_redistribute=variable_used_to_redistribute),                                                     
                                     cbind.data.frame(sce=scename, fs=fs, variable="logratio_GVA_after_before", value= a_logratio, variable_used_to_redistribute=variable_used_to_redistribute),                                                     
                                     cbind.data.frame(sce=scename, fs=fs, variable="extra_effort_multiplier", value= extra_effort, variable_used_to_redistribute=variable_used_to_redistribute),
                                     cbind.data.frame(sce=scename, fs=fs, variable="extra_varcosts", value= extra_varcosts, variable_used_to_redistribute=variable_used_to_redistribute)                                 
                                    )                   
   # caution about the effort metric depending on the source of data: VMS=> FishingHour; FDI=>fditotfishdays                             
                                      
       } else{
         cat(paste0("extent mismatch between data and restricted areas for ", fs, " (likely, data are unexpectely defined on a larger extent)...\n"))
      }
    
   } else{
     cat(paste0("no such a file for ", fs, "...\n"))
   }
  } # end fs
 } # end Sce


  
 # export the collector
 dir.create(file.path(getwd(),"OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg), recursive=TRUE)
 library(readr)
 print(fs_collector)
 dd <- knitr::kable(as.data.frame(fs_collector), format = "html")
 readr::write_file(dd, file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("before_after_output_",years_span,".html")))

 save(fs_collector, file=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("before_after_output_",years_span,".RData")))

 # Note:
 # some Inf popping up because log(0)=>Inf. It happens when effort is less than 1 in all cells of a fs...likely because the overall effort in these fs is very very low
 





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!PLOTTING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 
 library(ggplot2)
 library(doBy)

 load(file=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("before_after_output_",years_span,".RData")))

 # check
 fs_collector[fs_collector$fs=="GBR_FPO_VL0010" & fs_collector$sce=="Closure2022",]
 fs_collector[fs_collector$fs=="ESP_DFN_VL1218" & fs_collector$sce=="Closure2022",]
 fs_collector[fs_collector$fs=="ESP_DFN_VL1824" & fs_collector$sce=="Closure2022",]
 

 # filter out to keep the fs with large effort in the region
 effort    <-  fs_collector[fs_collector$variable=="effort_before" & fs_collector$variable_used_to_redistribute=="GVA",]
 effort    <- orderBy(~ -value, effort)  # order
 effort$fs <- factor (effort$fs, levels=unique(effort$fs)) # re-order
 
 is_fdi<-TRUE
 is_vms<-FALSE
 if(is_fdi) top20     <- unique(effort$fs)[1:40]
 if(is_vms) top20     <- unique(effort$fs)[1:20]
 
 # gva
 gva <- fs_collector[fs_collector$variable=="logratio_b_GVA_after_before" & fs_collector$variable_used_to_redistribute=="GVA",]
 #gva <- gva[gva$fs!="all_metiers" & gva$fs %in% top20,]
 
 load(file=file.path(getwd(), "OUTCOME_OVERLAY_VMEs","OUTCOME_FISHERIES_DISTR_FDI_AER", paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".RData"))) 
 gva <- gva[ gva$fs %in% unique(pd$fs),]
 #gva <- orderBy(~ -value, gva)  # order
 gva$fs <- factor (gva$fs, levels=unique(pd$fs)) # re-order
 library(stringr)
 temp <- as.data.frame(str_split_fixed(gva$fs,"_",3))  
 gva$country       <- temp[,1]
 gva$fishing_tech <- temp[,2]
 gva$vessel_size   <- temp[,3]

 # caution:
 gva$value [gva$value>1.5] <- 1.5 # limit to exp(1.1)=3.00 i.e. 3 times more, for readibility

 p1 <- ggplot(gva, aes(x = value, y=fs, fill=vessel_size))  + geom_bar(stat = "summary", fun = "mean", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Before/After in ", a_reg)) + xlab("log ratio of GVA/GVAinit") + ylab("AER Fleet-segments") +
                      scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))   
  a_width <- 7000 ; a_height <- 4500
       tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("Before_after_redistribution_from_",years_span,"_",a_comment,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p1)
 dev.off()

  # extract data corresponding to the plot
 pg <- ggplot_build(p1)
 pd <- pg$data[[1]]
 colnames(pd)[colnames(pd) %in% "group"] <- "fs"
 pd$fs <- factor(pd$fs) ; levels(pd$fs) <- levels(gva$fs)
 colnames(pd)[colnames(pd) %in% "PANEL"] <- "sce"
 pd$sce <- factor(pd$sce) ;  gva$sce <- factor(gva$sce) ; levels(pd$sce) <- levels(gva$sce)
 colnames(pd)[colnames(pd) %in% "fill"]  <- "vessel_size"
 pd$vessel_size <- factor(pd$vessel_size) ;  gva$vessel_size <- factor(gva$vessel_size); levels(pd$vessel_size) <- levels(gva$vessel_size)
 colnames(pd)[colnames(pd) %in% "x"]    <- "log-ratio"
 pd$"log-ratio" <- round(pd$"log-ratio", 2)
 # export
 library(readr)
 dd <- knitr::kable(as.data.frame(pd[, c("fs", "sce", "log-ratio")]), format = "html")
 readr::write_file(dd, file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".html"))) 
 save(pd, file=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, paste0("ggplot_data_logratio_of_GVA_after_weighted_displacement_","2018", "_","2021",".RData"))) 






  # gva after uniform redistrib 
 gva_u <- fs_collector[fs_collector$variable=="logratio_u_GVA_after_before" & fs_collector$variable_used_to_redistribute=="GVA",]
 #gva_u <- gva_u[gva_u$fs!="all_metiers" & gva_u$fs %in% top20,]
 load(file=file.path(getwd(), "OUTCOME_OVERLAY_VMEs","OUTCOME_FISHERIES_DISTR_FDI_AER", paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".RData"))) 
 gva_u <- gva_u[ gva_u$fs %in% unique(pd$fs),]
 gva_u$fs <- factor (gva_u$fs, levels=unique(pd$fs)) # re-order (according to the first plot...)
 library(stringr)
 temp <- as.data.frame(str_split_fixed(gva_u$fs,"_",3))  
 gva_u$country       <- temp[,1]
 gva_u$fishing_tech <- temp[,2]
 gva_u$vessel_size   <- temp[,3]

 # CAUTION:filter out really bad fs (because bad LPUEs lead to overly optimistic gain...)
 gva_u[gva_u$fs %in% c("ESP_DRB_VL0010"),"value"] <- 0

 # caution:
 gva_u$value [gva_u$value>2.6] <- 2.6 # limit to exp(1.1)=3.00 i.e. 3 times more, for readibility

 p2 <- ggplot(gva_u, aes(x = value, y=fs, fill=vessel_size))  + geom_bar(stat = "summary", fun = "mean", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Before/After in ", a_reg)) + xlab("log ratio of GVA/GVAinit") + ylab("AER Fleet-segments")  +
                                            scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))   
  a_width <- 7000 ; a_height <- 4500
       tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("Before_after_uniform_redistribution_from_",years_span,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p2)
 dev.off()


 # effort displaced (caution about the effort metric depending on the source of data: VMS=> FishingHour; FDI=>fditotfishdays)
 effdis <- fs_collector[fs_collector$variable=="amount_effort_displaced" & fs_collector$variable_used_to_redistribute=="GVA",]
 #effdis <- effdis[effdis$fs!="all_metiers" & effdis$fs %in% top20,]
 load(file=file.path(getwd(), "OUTCOME_OVERLAY_VMEs","OUTCOME_FISHERIES_DISTR_FDI_AER", paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".RData"))) 
 effdis <- effdis[ effdis$fs %in% unique(pd$fs),]
 #effdis <- orderBy(~ -value, effdis)  # order
 #effdis$fs <- factor (effdis$fs, levels=unique(effdis$fs)) # re-order (according to the first plot...)
 effdis$fs <- factor (effdis$fs, levels=unique(pd$fs)) # re-order (according to the first plot...)
 library(stringr)
 temp <- as.data.frame(str_split_fixed(effdis$fs,"_",3))  
 effdis$country       <- temp[,1]
 effdis$fishing_tech <- temp[,2]
 effdis$vessel_size   <- temp[,3]


 if(a_comment=="fdi") a_xlab <- "Effort displaced (thousand days)"
 if(a_comment=="vms") a_xlab <- "Effort displaced (thousand hours)"
 p3 <- ggplot(effdis, aes(x = value/1000, y=fs, fill=vessel_size))  + geom_bar(stat = "summary", fun = "mean", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Before/After in ", a_reg)) + xlab(a_xlab) + ylab("AER Fleet-segments")  +
                                            scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))   
  a_width <- 7000 ; a_height <- 4500
       tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("Effort_displaced_from_",years_span,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p3)
 dev.off()


  # landings inside 
 landin <- fs_collector[fs_collector$variable=="amount_landings_inside" & fs_collector$variable_used_to_redistribute=="GVA",]
 landin2 <- fs_collector[fs_collector$variable=="amount_landings_inside" & fs_collector$variable_used_to_redistribute=="effort",] # for info
 unique(landin2[landin2$value>0,"fs"])  # for info: fs with significant landings inside but no info on costs therefore GVA...
 #landin <- landin[landin$fs!="all_metiers" & landin$fs %in% top20,]
 load(file=file.path(getwd(), "OUTCOME_OVERLAY_VMEs","OUTCOME_FISHERIES_DISTR_FDI_AER", paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".RData"))) 
 landin <- landin[ landin$fs %in% unique(pd$fs),]
 #landin <- orderBy(~ -value, effdis)  # order
 #landin$fs <- factor (landin$fs, levels=unique(landin$fs)) # re-order
 landin$fs <- factor (landin$fs, levels=unique(pd$fs)) # re-order (according to the first plot...)
 
 library(stringr)
 temp <- as.data.frame(str_split_fixed(landin$fs,"_",3))  
 landin$country       <- temp[,1]
 landin$fishing_tech <- temp[,2]
 landin$vessel_size   <- temp[,3]


 p4 <- ggplot(landin, aes(x = value/1e6, y=fs, fill=vessel_size))  + geom_bar(stat = "summary", fun = "mean", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Before/After in ", a_reg)) + xlab("Landings inside (thousand tons)") + ylab("AER Fleet-segments")  +
                                                                 scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))   
  a_width <- 7000 ; a_height <- 4500
       tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("Landings_inside_from_",years_span,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p4)
 dev.off()



  # extra effort required to break even 
 extra <- fs_collector[fs_collector$variable=="extra_effort_multiplier" & fs_collector$variable_used_to_redistribute=="GVA",]
 #extra <- extra[extra$fs!="all_metiers" & extra$fs %in% top20,]
 load(file=file.path(getwd(), "OUTCOME_OVERLAY_VMEs","OUTCOME_FISHERIES_DISTR_FDI_AER", paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".RData"))) 
 extra <- extra[ extra$fs %in% unique(pd$fs),]
 extra$fs <- factor (extra$fs, levels=unique(pd$fs)) # re-order (according to the first plot...)
 
 library(stringr)
 temp <- as.data.frame(str_split_fixed(landin$fs,"_",3))  
 extra$country       <- temp[,1]
 extra$fishing_tech <- temp[,2]
 extra$vessel_size   <- temp[,3]

 extra <- extra[extra$value!=1,] # keep only the fs that would need to increase effort to break even

 p5 <- ggplot(extra, aes(x = value, y=fs, fill=vessel_size))  + geom_bar(stat = "summary", fun = "mean", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Before/After in ", a_reg)) + xlab("Multiplier on effort to break even") + ylab("AER Fleet-segments")  + xlim(c(0,1.5)) +
                                scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))   
  a_width <- 5000 ; a_height <- 3500
       tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("Extra_effort_multiplier_from_",years_span,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p5)
 dev.off()



   # extract data corresponding to the plot
 pg <- ggplot_build(p5)
 pd <- pg$data[[1]]
 colnames(pd)[colnames(pd) %in% "group"] <- "fs"
 pd$fs <- factor(pd$fs) ; levels(pd$fs) <- levels(factor(extra$fs))
 colnames(pd)[colnames(pd) %in% "PANEL"] <- "sce"
 pd$sce <- factor(pd$sce) ;  extra$sce <- factor(extra$sce) ; levels(pd$sce) <- levels(factor(extra$sce))
 colnames(pd)[colnames(pd) %in% "fill"]  <- "vessel_size"
 pd$vessel_size <- factor(pd$vessel_size) ;  extra$vessel_size <- factor(extra$vessel_size); levels(pd$vessel_size) <- levels(extra$vessel_size)
 colnames(pd)[colnames(pd) %in% "x"]    <- "Effort multiplier"
 pd$"Effort multiplier" <- round(pd$"Effort multiplier", 2)
 # export
 library(readr)
 dd <- knitr::kable(as.data.frame(pd[, c("fs", "sce", "Effort multiplier")]), format = "html")
 readr::write_file(dd, file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, paste0("ggplot_data_effort_multiplier_to_breakeven_-squares_","2018", "_","2021",".html"))) 
 save(pd, file=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, paste0("ggplot_data_effort_multiplier_to_breakeven_","2018", "_","2021",".RData"))) 








  # LPUEs 
 lpues_in <- fs_collector[fs_collector$variable=="lpue_inside" & fs_collector$variable_used_to_redistribute=="GVA",]
 lpues_in <- lpues_in[lpues_in$fs!="all_metiers" & lpues_in$fs %in% top20,]
 lpues_in$fs <- factor (lpues_in$fs, levels=unique(gva$fs)) # re-order (according to the first plot...)
 lpues_out <- fs_collector[fs_collector$variable=="lpue_outside" & fs_collector$variable_used_to_redistribute=="GVA",]
 lpues_out <- lpues_out[lpues_out$fs!="all_metiers" & lpues_out$fs %in% top20,]
 lpues_out$fs <- factor (lpues_out$fs, levels=unique(gva$fs)) # re-order (according to the first plot...)
 lpue <- rbind.data.frame(lpues_in, lpues_out)
 
 library(stringr)
 temp <- as.data.frame(str_split_fixed(landin$fs,"_",3))  
 lpue$country       <- temp[,1]
 lpue$fishing_tech <- temp[,2]
 lpue$vessel_size   <- temp[,3]

 lpue[is.na(lpue$value),"value"] <-0 # keep only the fs that would need to increase effort to break even

 
 p6 <- ggplot(lpue, aes(x = as.numeric(value), y=fs,  fill=variable))  + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Before/After in ", a_reg)) + xlab("LPUEs") + ylab("AER Fleet-segments") +
                                scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))   
  a_width <- 5000 ; a_height <- 3500
       tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT_VMEs", a_folder2, a_reg, paste0("LPUEs_from_",years_span,".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p6)
 dev.off()




# SIDE NOTE:
# the outcomes could be overly optimistic in showing mostly gain from the reallocation. 
# reallocation according to a log-GVA is firmly increasing the performance of the fleets compared to the baseline without
# it is is also assuming that any unit of extra effort on site will actually not be affecting LPUEs...

# It is therefore tried to compare with another artifically constructed baseline that consist of:
# same amount of displaced effort but displaced according to GVA across all initial areas i.e. irrespective of closures.... 


 
 # BASIC CHECK
 # load eco
 load(file=file.path(ROutputPathToDatasets, "agg_fdi_aer_eco_fs.RData"))  #agg_eco_fs
 agg_eco_fs[fs=="PRT_FPO_VL0010",]

 filepath       <- file.path(getwd(), a_folder, "PRT_FPO_VL0010", years_span)
 aer_layers   <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
 # re-project
 aer_layers_eea_terra       <- project(aer_layers, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs", method="sum")
 sum(aer_layers$fditotfishdays[], na.rm=TRUE)
 sum(aer_layers_eea_terra$fditotfishdays[], na.rm=TRUE)
 #=> A CRAZY DIFFERENCE!! => do not use Lambert proj for doing the computation... use only for visualisation.





#} # end specs

 