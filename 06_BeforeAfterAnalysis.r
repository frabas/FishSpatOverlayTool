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
 a_folder  <- "OUTCOME_FISHERIES_DISTR_VMS_AER"   # a VMS-AER layer
 a_folder2  <- "OUTCOME_FISHERIES_DISTR_VMS_AER_plots"   # a VMS-AER layer
 a_folder3  <- "OUTCOME_FISHERIES_DISTR_VMS_AER_reassembled"   # a VMS-AER layer
 #a_folder  <- "OUTCOME_FISHERIES_DISTR_FDI_AER"  # a FDI-AER layer
 #a_folder2  <- "OUTCOME_FISHERIES_DISTR_FDI_AER_plots"  # a FDI-AER layer
 #a_folder3  <- "OUTCOME_FISHERIES_DISTR_FDI_AER_reassembled"  # a FDI-AER layer

 #years_span <- "2018_2021"
 #years_span <- "2019"
 a_reg      <- "ALL_REGIONS" # default
 #a_reg      <- "BoB"
 #---------------------------


# a FOR-LOOP to make sure to get all combis...
 #specs <- expand.grid(years_span=c("2018_2021"), a_reg=c("ALL_REGIONS", "BoB"), a_folder=c("OUTCOME_FISHERIES_DISTR_FDI_AER", "OUTCOME_FISHERIES_DISTR_VMS_AER")) 
 specs <- expand.grid(years_span=c("2018_2021"), a_reg=c("ALL_REGIONS", "BoB"), a_folder=c("OUTCOME_FISHERIES_DISTR_VMS_AER")) 
 specs <- cbind.data.frame(specs, a_folder2=paste0(specs$a_folder, "_plots"),  a_folder3=paste0(specs$a_folder, "_reassembled")) 
 for (ispec in 1:nrow(specs)){
   a_folder   <- specs[ispec, "a_folder"] 
   a_folder2  <- specs[ispec, "a_folder2"] 
   a_folder3  <- specs[ispec, "a_folder3"] 
   years_span <- specs[ispec, "years_span"] 
   a_reg      <- specs[ispec, "a_reg"] 
 


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!BUILD AN OVERALL DISPLACEMENT LAYER!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  #---------------
  # example of a Before layer
  fs <- "all_metiers"
  filepath             <- file.path(getwd(), a_folder, fs, years_span)
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
  plot(log(dd))
  #sum(dd$FishingHour[], na.rm=TRUE)      # if VMS
  #sum(dd$fditotfishdays[], na.rm=TRUE)   # if FDI


  # example of a After (effort redistribution) layer
  fs <- "all_metiers"
  filepath             <- file.path(getwd(), "OUTCOME_DISPLACEMENT", a_folder, a_reg, fs, years_span, "OWF+MPAs")
  all_met <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
  plot(log(all_met$EffortDisplWeighted))
  #sum(all_met$FishingHour[], na.rm=TRUE)      # if VMS
  #sum(all_met$fditotfishdays[], na.rm=TRUE)   # if FDI
  
  # a diff
  plot(log(all_met$FishingHour) - log(all_met$EffortDisplWeighted)) # VMS
  #plot(log(all_met$fditotfishdays) - log(all_met$EffortDisplWeighted))    # FDI
  #---------------


  #---------------
  # example of a Before layer
  fs <- "FRA_DTS_VL1218" 
  #fs <- "NLD_TBB_VL1824"
  fs <- "all_metiers" 
  filepath             <- file.path(getwd(), a_folder, fs, years_span)
  dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
  plot(log(dd))
  #sum(dd$FishingHour[], na.rm=TRUE)      # if VMS
  #sum(dd$fditotfishdays[], na.rm=TRUE)   # if FDI


  # example of a After (effort redistribution) layer
  fs <- "FRA_DTS_VL1218"
  fs <- "NLD_TBB_VL1824"
  fs <- "all_metiers" 
  fs <- "FRA_DFN_VL2440" 
  fs <- "DEU_DTS_VL2440" 
  filepath             <- file.path(getwd(), "OUTCOME_DISPLACEMENT", a_folder, a_reg, fs, years_span, "OWF+MPAs")
  all_met <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
  plot(log(all_met$EffortDisplWeighted))
  #sum(all_met$FishingHour[], na.rm=TRUE)      # if VMS
  #sum(all_met$fditotfishdays[], na.rm=TRUE)   # if FDI
  
  # a diff
  #log(all_met$d$FishingHour) - log(all_met$EffortDisplWeighted) # VMS
  a_width <- 6000 ; a_height <- 8000
    tiff(filename=file.path(getwd(), "OUTCOME_DISPLACEMENT", a_folder2, a_reg, "Plots", paste0("Example_",fs,"_", years_span,"_sce_", "OWF+MPAs",".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=800, compression = c("lzw"))
  par(mfrow=c(5,3))
  par(mar=c(0.5,1,1,0))
  for (a_sce in sces){
     areas <- as.character(restriction_per_fs_per_sce[[a_sce]][restriction_per_fs_per_sce[[a_sce]]$fs==fs,"restricted_area"])
     filepath             <- file.path(getwd(), "OUTCOME_DISPLACEMENT", a_folder, a_reg, fs, years_span, a_sce)
     all_met <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content
     filepath             <- file.path(getwd(), a_folder, fs, years_span)
     dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder큦 name describes the content

     max_val <- 9
     plot(trim(log(dd$FishingHour)), breaks=seq(1,max_val,by=1), main=paste(fs," Before"))    # VMS
     for(a in areas) plot(trim(get(a)), add=TRUE, col=rgb(0.2,0.2,0.2,0.3), legend=FALSE)   
     plot(trim(log(all_met$EffortDisplWeighted)), breaks=seq(1, max_val, by=1), main=paste("After", a_sce)) # VMS
     for(a in areas) plot(trim(get(a)), add=TRUE, col=rgb(0.2,0.2,0.2,0.3), legend=FALSE)   
     plot(trim(log(all_met$EffortDisplWeighted)- log(dd$FishingHour) ), breaks=seq(1,max_val,by=1), main="Diff")   
     for(a in areas) plot(trim(get(a)), add=TRUE, col=rgb(0.2,0.2,0.2,0.3), legend=FALSE)   
  }
  dev.off() 
  #---------------



  sces <- c("OWF", "currentMPAs", "MPAs", "OWF+currentMPAs", "OWF+MPAs") 
  for(a_sce in sces)
  {


  # loop over files for all fs but "all_metiers"
  lst_files <- list.files(file.path(getwd(), "OUTCOME_DISPLACEMENT", a_folder, a_reg ))
  lst_files <- lst_files[!lst_files %in% "all_metiers"]

  count <- 0
  for(fs in lst_files){  # caution: this loop takes a while....
     count <- count+1
     cat(paste0(fs, "...", count, " out of ", length(lst_files)," files\n"))
     # STACK rasters
     filepath             <- file.path(getwd(), "OUTCOME_DISPLACEMENT", a_folder, a_reg, fs, years_span, a_sce)
      er <- try(   {
     this_met                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
     }, silent=TRUE)
    if(class(er)!="try-error"){
  
     if (count==1){
       all_fs  <- this_met
       } else{
       all_fs  <- sum(all_fs, this_met, na.rm=TRUE)
       }
  
    } else{
       cat(paste0("no such a file for ", fs, "...\n"))
    }
  } # end fs
 
  # check
  plot(log(all_fs))
  #sum(all_fs$FishingHour[], na.rm=TRUE)
  #=> small discrepencies in numbers likely arise from re-projection to EEA, plus rounding errors

  # export
  filepath             <- file.path(getwd(), "OUTCOME_DISPLACEMENT", a_folder3, a_reg, years_span, a_sce)
  dir.create(file.path(getwd(), "OUTCOME_DISPLACEMENT",a_folder3,  a_reg, years_span, a_sce), recursive=TRUE)
  writeRaster(all_fs, filename=file.path(filepath, "spatRaster.tif"), overwrite=TRUE)
 } # end a_sce



} # end specs






##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!DO SOME PLOTTING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# TODO
#...
#...
#...


