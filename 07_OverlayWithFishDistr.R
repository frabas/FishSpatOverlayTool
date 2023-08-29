



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# get the data.table linked to a master raster grid
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

      
   

my_raster_export <- function(master_dt, slave_dt, filepath){     
  # create a grdtable
  # first create sf of a df
  library(sf)
  library(raster)
  library(terra)
  a_distr                       <- st_as_sf(x = slave_dt,  
                                          coords = c("lon", "lat"),
                                          crs = 4326) # EPSG:4326 is  WGS 84 -- WGS84 - World Geodetic System 1984, used in GPS, see https://epsg.io/4326
  #a_distr                       <- st_transform(a_distr, 3035) # to lambert....but then adapt the rounding trick for grid alignement...

  xrange                        <- range(master_dt$lon, na.rm=TRUE)
  yrange                        <- range(master_dt$lat, na.rm=TRUE)
  resx                          <-  diff(unique(master_dt$lon)[order(unique(master_dt$lon))])  [3]  # master distr giving the bbox
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
 
  if(nrow(distr_with_grid)!=0){ # do nothing if no data left here...
  
  ## aggregate per grID
  some_cols_to_sweep <- c("density")
  some_cols_to_sweep <- some_cols_to_sweep [some_cols_to_sweep %in% colnames(master_dt)]         

  distr_with_grid$nbyquarter <- length(unique(master_dt$Year))*4
     
  # do a sweep() in advance of the sum to come to result into an average
  distr_with_grid <- data.table(cbind.data.frame(
                                distr_with_grid[,c("idx", "idy",  "grID")], 
                                 sweep(distr_with_grid[, ..some_cols_to_sweep], 1, distr_with_grid[, nbyquarter], FUN="/")))  
  
 some_cols_to_sum <- c("density") 
  some_cols_to_sum <- some_cols_to_sum [some_cols_to_sum %in% colnames(master_dt)]         
distr_with_grid_1 <- NULL 
  if(length(some_cols_to_sum)>0) 
     {
     distr_with_grid_1 <- 
       distr_with_grid[,lapply(.SD, sum, na.rm=TRUE),
         .SDcols=some_cols_to_sum,
          keyby=c("idx", "idy",  "grID")]
     }
  some_cols_to_average <- c("density")
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

  #distr_with_grid[idx==-3.575 & idy==45.975 & grID==256229,]
  
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
  cat(paste0(" error on this file......." ,"\n"))
  }
  
  
  
return()
}

##------------------------------------
## CALLS------------------------------
  years <- c(2018:2021)
  
 
  # first, load the spatial data
  load(file=file.path("C:","Users","fbas","Documents","Projects","SEAWise","WP5","T 5.2 Fish distribution","DATA","predictions_all.RData"))  # get grid (quarter based data)
  fishdis <- grid[Year %in% c("2018","2019","2020","2021"),]
  
  for(a_species in unique(fishdis$species)){
     for(a_sce in c("historical", "future")){
            cat(paste0(a_species,"  ", a_sce, "\n"))
  
     filepath             <- file.path(getwd(), "OUTCOME_FISH_DISTR", "2018_2021", a_sce, a_species)  
     dir.create(filepath, recursive=TRUE)

     fishdis_this_sp <- fishdis[species==a_species & stage=="all" & scenario==a_sce,]
  
     if(nrow(fishdis_this_sp>0)){ 
     # split cells over lon x-axis into two cells to get the same diff res like lat y-axis
     ddd1 <- fishdis_this_sp
     ddd2 <- fishdis_this_sp
     ddd1$lon <- an(ddd1$lon) -0.1666667/2
     ddd2$lon <- an(ddd2$lon) +0.1666667/2
     fishdis_this_sp                <- rbind.data.frame(ddd1, ddd2)

  
  
     my_raster_export(master_dt=fishdis_this_sp, slave_dt=fishdis_this_sp, filepath)
     # check: 
     dd <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content 
     plot(log(dd))
     sum(dd$density[], na.rm=TRUE) 
  
     a_width <- 3000 ;  a_height <- 4000
     tiff(filename=file.path(getwd(), "OUTCOME_FISH_DISTR", paste0(a_species, "_fish_density_2018_2021_", a_sce, ".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
       par(mar=c(1,1,1,1))
        e<- ext(-16, 10, 30, 62)
        plot(log(crop(dd$density, e)))
     dev.off()

    } else{
     cat(paste0("no such combinaison in data\n"))
    }
   } # end sce
  } # end sp
  




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
 aer_layers    <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content
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
                                           some_colnames=c("density"),
                                           a_data_layers=aer_layers,
                                           a_closed_area_layer=mpas_3035_msfd_rast_terra,
                                           name_closure="OWF",
                                           name_sp="")
{
  # raster layers projected in EEA are required in input
  a_reg_layer <- trim(a_reg_layer)
  a_data_layers <- crop(a_data_layers, a_reg_layer)
  
  # do a RASTER OVERLAY with closed areas per fished area per FAO region
  data_layers_this_reg                <- a_data_layers * a_reg_layer # filter out if not inside region
  # the closed areas layer
  a_closed_area_layer_c               <- crop(a_closed_area_layer, data_layers_this_reg) # align
  data_layers_this_reg_c               <- crop(data_layers_this_reg, a_closed_area_layer_c) # align
  a_reg_layer_c                        <- crop(a_reg_layer, a_closed_area_layer_c) # align
  # the overlay
  overlay                             <- data_layers_this_reg_c * a_closed_area_layer_c
  # the complementary non-closed areas layer
  a_non_closed_area_area_c            <- a_closed_area_layer_c # init
  a_non_closed_area_area_c [] <- 0
  #values(a_non_closed_area_area_c) <- NA
  values(a_non_closed_area_area_c) [is.na(values(a_closed_area_layer_c))] <- 1
  non_overlay                         <- data_layers_this_reg_c * a_non_closed_area_area_c    * a_reg_layer_c
  # trim to the region for better visualisation
  if(!all(is.na(overlay$density[]))){ 
     overlay_t                           <- terra::crop(overlay, a_reg_layer_c) # trim to stick to the region
     a_closed_area_layer_c               <- terra::crop(a_closed_area_layer_c, a_reg_layer_c)
     a_closed_area_layer_c               <- a_closed_area_layer_c * a_reg_layer_c
     non_overlay_c                       <- terra::crop(non_overlay, a_reg_layer_c)

     # visual check
     if(save_a_plot){
        a_width <- 4000 ; a_height <- 4000
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY_WITH_FISHDIS", paste0(name_closure,"-", name_sp, ".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
       plot(log(overlay_t$density))
       plot(log(non_overlay_c$density), add=TRUE, legend=FALSE) # caution: leg breaks can differ. But here we plot only for a quick visual check
       plot(a_closed_area_layer_c, col=rgb(0.2,0.2,0.2,0.2), add=TRUE, legend=FALSE)
       bi <- boundaries(a_closed_area_layer_c)
       #plot(bi, add=TRUE, col=rgb(0.1,0.1,0.1,0.1))
      dev.off()
      }

     # aggregate over the entire area and format
     library(data.table)
     mean_all_inside <- data.frame(data.table(as.data.frame(overlay_t))[,lapply(.SD, mean, na.rm=TRUE),
                                   .SDcols=c(some_colnames)])
   }else{  # capture the edge case of no impact of the closure...
     mean_all_inside <- as.data.frame(matrix(0, ncol=length(some_colnames)))
     colnames(mean_all_inside) <- some_colnames
   }  
     mean_all_outside <- data.frame(data.table(as.data.frame(non_overlay))[,lapply(.SD, mean, na.rm=TRUE),
                                   .SDcols=c(some_colnames)])
     mean_all <- rbind.data.frame(
                     cbind.data.frame(Sce=a_sce, Region=a_reg_name, variable=colnames(mean_all_outside), value= as.numeric(mean_all_outside[1,]), CLOSED=FALSE, name_closure=name_closure, name_sp=name_sp),
                     cbind.data.frame(Sce=a_sce, Region=a_reg_name, variable=colnames(mean_all_inside), value= as.numeric(mean_all_inside[1,]), CLOSED=TRUE, name_closure=name_closure, name_sp=name_sp)
                          )
 return(mean_all)
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

  idx                                 <- which(a_df["rmv_lns_S0"]==1 & a_df$reason_lns %in% c("current", "current_habitat","current_spp", "current_habitat_spp"))
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_lns_current <- mpas_vect_terra[idx]

  idx                                 <- which(a_df["rmv_nts_S0"]==1)
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_nts <- mpas_vect_terra[idx]

  idx                                 <- which(a_df["rmv_nts_S0"]==1 & a_df$reason_nts %in% c("current", "current_habitat","current_spp", "current_habitat_spp"))
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_nts_current <- mpas_vect_terra[idx]

  idx                                 <- which(a_df["rmv_bt__S0"]==1)
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_bt <- mpas_vect_terra[idx]

  idx                                 <- which(a_df["rmv_bt__S0"]==1 & a_df$reason_bt %in% c("current", "current_habitat","current_spp", "current_habitat_spp"))
  #plot(mpas_vect_terra)
  #plot(mpas_vect_terra[idx], add=TRUE, col=2)
  #=> TODO: Check with ArcGIS is everything in order
  mpas_rmv_bt_current <- mpas_vect_terra[idx]

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
 a_folder      <- "OUTCOME_FISH_DISTR"
 filepath      <- file.path(getwd(),a_folder, years_span, "historical","Ammodytes_tobianus")
 aer_layers    <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content

  
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
## CREATE LOOKUP FOR RESTRICTION SPECS
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  a_folder  <- "OUTCOME_FISH_DISTR" 
  dir.create(file.path(getwd(),"OUTCOME_OVERLAY_WITH_FISHDIS", a_folder), recursive=TRUE)
  lst_files <- list.files(file.path(getwd(), a_folder, "2018_2021", "future"))

  restriction_per_sp_per_sce <- list(NULL)
  sces <- c("OWF", "currentMPAs", "MPAs", "OWF+currentMPAs", "OWF+MPAs")
  count <- 0
  for(a_sce in sces)
  {
  count <- count+1
  restriction_per_sp <- NULL
  if(a_sce %in% c("OWF+currentMPAs", "OWF+MPAs")) for(sp in lst_files)
     {
     areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra", "mpas_rmv_bt_current_rast_terra", "mpas_rmv_nts_current_rast_terra", "mpas_rmv_lns_current_rast_terra")
     restriction_per_sp <- rbind.data.frame(
                                restriction_per_sp,
                                expand.grid(sp=sp, restricted_area=areas, scenario=a_sce)
                                )
     } 
  if(a_sce %in% c("OWF")) for(sp in lst_files)
     {
     areas <- c("owf_msfd_rast_terra","owf_miss_msfd_rast_terra")
     restriction_per_sp <- rbind.data.frame(
                                restriction_per_sp,
                                expand.grid(sp=sp, restricted_area=areas, scenario=a_sce)
                                )
     } 
  if(a_sce %in% c("currentMPAs")) for(sp in lst_files)
     {
     areas <- c( "mpas_rmv_bt_current_rast_terra", "mpas_rmv_nts_current_rast_terra", "mpas_rmv_lns_current_rast_terra")
     restriction_per_sp <- rbind.data.frame(
                                restriction_per_sp,
                                expand.grid(sp=sp, restricted_area=areas, scenario=a_sce)
                                )
     } 
  if(a_sce %in% c("MPAs")) for(sp in lst_files)
     {
     areas <- c( "mpas_rmv_bt_rast_terra", "mpas_rmv_nts_rast_terra", "mpas_rmv_lns_rast_terra")
     restriction_per_sp <- rbind.data.frame(
                                restriction_per_sp,
                                expand.grid(sp=sp, restricted_area=areas, scenario=a_sce)
                                )
     } 
        restriction_per_sp_per_sce[[  count ]] <- restriction_per_sp
 
    }  
    names(restriction_per_sp_per_sce) <- sces






##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!OVERLAY AND EXTRACT PER FISHING ACTIVITIES!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#



   # read-in a layer and proceed to overlay in a systematic way
   library(terra)
    a_folder  <- "OUTCOME_OVERLAY_WITH_FISHDIS" 
   dir.create(file.path(getwd(), a_folder), recursive=TRUE)
   lst_files <- list.files(file.path(getwd(), a_folder, "2018_2021",  "future"))


 output_per_sp <- NULL # collector
 for(a_sce in sces){
   cat(paste0(a_sce,"\n"))
   count <- 0
 for(sp in lst_files){  # caution: this loop takes a while....
   count <- count+1
   cat(paste0(sp, "...", count, " out of ", length(lst_files)," files\n"))
   filepath       <- file.path(getwd(), a_folder, "2018_2021",  "future", sp)
     er <- try(   {
     aer_layers                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
   }, silent=TRUE)
   if(class(er)!="try-error"){
    

    # stack fs specific restrictions to get one fs specific layer for closure
    dd <- restriction_per_sp_per_sce[[a_sce]]
    ddd <- dd[dd$sp==sp,]
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

 
   some_colnames <- c("density")

   if (!is.null(this_closed_area_layer))
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
     if(!all(is.na(ns_raster[]))) sum_all_ns_closure   <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="NorthSea",
                                                            a_reg_layer=ns_raster, some_colnames=some_colnames, a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_sp=sp)
     if(!all(is.na(bs_raster[]))) sum_all_bs_closure   <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="BalticSea",
                                                            a_reg_layer=bs_raster,  some_colnames=some_colnames,  a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_sp=sp)
     if(!all(is.na(cs_raster[]))) sum_all_cs_closure   <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="CelticSea",
                                                           a_reg_layer=cs_raster, some_colnames=some_colnames,  a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_sp=sp)
     if(!all(is.na(bob_raster[]))) sum_all_bob_closure  <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="BoB",
                                                           a_reg_layer=bob_raster, some_colnames=some_colnames,  a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce, name_sp=sp)
     #sum_all_port_closure <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="Portugal", a_reg_layer=port_raster, a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce,  name_sp=sp)
     #sum_all_mac_closure <- aggregate_from_raster_overlay(a_sce=a_sce, a_reg_name="Macronesie", a_reg_layer=port_raster, a_data_layers=aer_layers, a_closed_area_layer=this_closed_area_layer, name_closure=a_sce,  name_sp=sp)
     # etc. (i.e. for FDI, but not for VMS)


     obj_names <- c("sum_all_ns_closure", "sum_all_bs_closure", "sum_all_cs_closure", "sum_all_bob_closure")

     for (objn in obj_names) if(exists(objn)) if(length(get(objn))>0) output_per_sp <- rbind.data.frame(output_per_sp, get(objn) )

     cat(paste0(sp, "...OK\n"))
     } else{
     cat(paste0("no density info for this seg ", sp, ", or no closed area specified...\n"))
     }
     
     } else{
     cat(paste0("not such file for ", sp, "...\n"))
     }

} # end sp
} # end a_sce
  
# export--------
library(readr)
 print(output_per_sp)
 dd <- knitr::kable(as.data.frame(output_per_sp), format = "html")
 readr::write_file(dd, file.path(getwd(), "OUTCOME_OVERLAY_WITH_FISHDIS", paste0("average_density_inside_outside_closed_areas_from_",years_span,".html")))

 save(output_per_sp, file=file.path(getwd(), "OUTCOME_OVERLAY_WITH_FISHDIS",  paste0("average_density_inside_outside_closed_areas_from_",years_span,".RData")))
  





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 # do a ggplot barplot out of this..........

 # re-load
 load(file.path(getwd(), "OUTCOME_OVERLAY_WITH_FISHDIS", paste0("aggregate_inside_outside_closed_areas_from_",years_span,".RData")))  # output_per_fs


 library(ggplot2)
 library(doBy)

 a_var <- "density"

 a_dt <- data.table(output_per_sp)

 an <- function(x) as.numeric(x)
 agg <- a_dt[variable==a_var,.(density=mean(an(value), na.rm=TRUE)),by=c("name_sp", "CLOSED", "name_closure")]
 agg2 <- agg[,.(sumdensity=sum(an(density), na.rm=TRUE)),by=c("name_sp", "name_closure")]
 merged <- merge(agg, agg2, by=c("name_sp", "name_closure"))
 merged$multi <- log(merged$density/(merged$sumdensity-merged$density))
 
 multi <- orderBy(~ name_closure+CLOSED+multi, merged)
 multi$name_sp <- factor(multi$name_sp, levels=unique(multi$name_sp)) # re-order

 multi[multi>8,"multi"]  <- 8

 # all-metiers plot
 p1 <- ggplot(multi[multi>0,], aes(x = multi, y=name_sp, fill=CLOSED))  + geom_bar(stat = "summary", fun = "mean")  + facet_wrap(~name_closure, ncol=5)   +
                xlab("log(av.density_x/av.density_y)" )    + ylab("Species")   +xlim(0,8)

 a_width <- 7000 ; a_height <- 4000
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY_WITH_FISHDIS", a_folder, paste0("Proportion_closed_all_metiers_from_",years_span,".tif")), width = a_width, height = a_height,
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




