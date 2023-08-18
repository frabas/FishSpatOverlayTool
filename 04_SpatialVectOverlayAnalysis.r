
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# A quick spatial overlay!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

 
 
# utils 
what_is_inside <- function(filepath, allclosures_sf, reproject=FALSE)
{
 
 library(terra)
 library(raster)
 bb             <- st_bbox(allclosures_sf)
  
  er <- try(   {
     rstr                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
  }, silent=TRUE)
 if(class(er)!="try-error"){                                       
 
 if(reproject){
   newcrs               <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" # European EEA projection
   rstr_eea             <- project(rstr, newcrs) 
 } else{
   rstr_eea             <- rstr
 }

 if(length(grep( "FishingHour",  colnames(rstr[])))!=0){
   is_vms <- TRUE; is_fdi <- FALSE
  } else{
  is_vms <- FALSE ; is_fdi <- TRUE
  }


 # crop in a bbox define by the closure shp
 cr           <- as(extent(bb), 'SpatialPolygons')
 crs(cr)      <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
 if(reproject) cr <- spTransform(cr, crs(rstr_eea))
 rstr_eea_sub <- crop(rstr_eea, cr)
 
 library(viridis)
 #plot(rstr_eea_sub)
 #plot(rstr_eea_sub, col=viridis(50))
 #plot(log(rstr_eea_sub), col=viridis(50))
 
 # reproject closures      
 allclosures_sf_eea <- st_transform(allclosures_sf, crs(rstr_eea))
 a_width <- 3000 ;  a_height <- 4000
 tiff(filename=file.path(file.path(getwd(), a_folder, a_folder2, "VectorExtract", paste0("e_mpas_",sce,"_",fs,"_",y,".tif"))), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
  
   plot(log(rstr_eea_sub$landings_aer_in_ctry_level6_csquare), main="log of landings kg")
   plot(allclosures_sf_eea, add=TRUE, lwd=0.5, col=rgb(0.2,0.2,0.2,0.2))
 dev.off()
 
 
 # adding a csquare code
 r <- rast(nrow=dim(rstr_eea_sub)[1], ncol=dim(rstr_eea_sub)[2],
                                     extent=ext(rstr_eea_sub), res=res(rstr_eea_sub), crs=crs(rstr_eea_sub), vals=NA, names="value") # init
 cells <- 1:ncell(r)
 xy <- xyFromCell(r, cells)
 if(is_fdi) csquares <- vmstools::CSquare(xy[,"x"],xy[,"y"], degrees=0.5)
 if(is_vms) csquares <- vmstools::CSquare(xy[,"x"],xy[,"y"], degrees=0.05)
 values(r) <- csquares
 csquare_coding <- data.frame(id=c(values(r)), csquare=csquares, x=xy[,"x"], y=xy[,"y"])
 names(r)  <- "CSQUARE_ID"
 add(rstr_eea_sub) <- r
 

# extract what is inside...
 # Caution here...It is not simple because basically the polygons are much small that the raster cells (in this FDI case)...
 # applying a multiplier on GVA will be later necessary.
 allclosures_vect_terra  <- vect(allclosures_sf_eea) 
 if(is_fdi) col_names               <- c("daysatsea_aer_in_ctry_level6_csquare","landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare",
                                "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare", "other_income_in_csquare","unpaid_labour_in_csquare", "personnelcosts_in_ctry_level6_csquare", "KwFishingdays_aer_in_ctry_level6_csquare",
                                 "lpue_csquare_fdi_kgperfday", "CSQUARE_ID")
 if(is_vms) col_names               <- c("daysatsea_aer_in_ctry_level6_csquare","landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare",
                                "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare", "other_income_in_csquare","unpaid_labour_in_csquare", "personnelcosts_in_ctry_level6_csquare", "KwFishingdays_aer_in_ctry_level6_csquare",
                                 "lpue_csquare_vms_kgperfhour", "CSQUARE_ID")
 
 
 
 # extract
 if(is_vms){
   # with VMS data, some polygons lay on several csquares...
   e_mpas              <- extract(subset(rstr_eea_sub, col_names), vect(allclosures_sf_eea), cells=TRUE,  na.rm=TRUE) # extract the raster cell numbers from each polygon pts for all polygons
   e_mpas              <- e_mpas[!duplicated(e_mpas [,c("ID", "cell")]),]  # avoid double counting because polygons are by def having several pts... 

 }
 if(is_fdi){
   # do an extract (use the centroid...it is acceptable given the closed boxes are much smaller than the FDI c-squares)
   coord       <- crds(rstr_eea_sub, na.rm=FALSE)
   coords      <- st_coordinates(st_centroid(st_make_valid(allclosures_sf_eea$geometry)))
   e_mpas      <- extract(rstr_eea_sub, vect(SpatialPoints(coords)), cellnumbers=TRUE)
  }
 
 
  # retrieve the csquare coding
  e_mpas <- cbind.data.frame(e_mpas, csquare=csquare_coding[match(e_mpas[, "CSQUARE_ID"], csquare_coding$id), "csquare"]) 

 
 e_mpas              <- data.table(e_mpas)
# then aggregate per c-square 
 e_mpas_1              <- e_mpas[,lapply(.SD, sum, na.rm=TRUE),
                                   .SDcols=c("daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
                                      "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare",
                                          "other_income_in_csquare" ,"unpaid_labour_in_csquare", "personnelcosts_in_ctry_level6_csquare",
                                     "KwFishingdays_aer_in_ctry_level6_csquare"), by=c("ID", "csquare")]
 e_mpas_1               <- as.data.frame(e_mpas_1) 
 if(is_fdi) e_mpas_2              <- e_mpas[,lapply(.SD, sum, na.rm=TRUE),
                                   .SDcols=c("lpue_csquare_fdi_kgperfday"), by=c("ID","csquare")]
 if(is_vms) e_mpas_2              <- e_mpas[,lapply(.SD, sum, na.rm=TRUE),
                                   .SDcols=c("lpue_csquare_vms_kgperfhour"), by=c("ID","csquare")]
 e_mpas_2               <- as.data.frame(e_mpas_2) 
 e_mpas                    <- cbind.data.frame(what="inside_this_poly", e_mpas_1, e_mpas_2[,c(-c(1:2)), drop=FALSE])
 ## => export to TABULATE later!!!
 

 # what about outside? which outside to get a percentage?
 #=> need to do the analysis per fs to compare with total per fs....
 library(data.table)
 a_dt <- data.table(as.data.frame(rstr_eea))
 ## aggregate per grID
  a_dt_1 <- 
     a_dt[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare",
       "varcosts_in_ctry_level6_csquare", "oth_non_var_costs_in_csquare",
          "other_income_in_csquare" ,"unpaid_labour_in_csquare", "personnelcosts_in_ctry_level6_csquare",
           "KwFishingdays_aer_in_ctry_level6_csquare")]
  if(is_fdi) a_dt_2 <- 
     a_dt[,lapply(.SD, mean, na.rm=TRUE),
      .SDcols=c("lpue_csquare_fdi_kgperfday")]
  if(is_vms) a_dt_2 <- 
     a_dt[,lapply(.SD, mean, na.rm=TRUE),
      .SDcols=c("lpue_csquare_vms_kgperfhour")]
  a_dt   <- cbind(a_dt_1, a_dt_2)
  a_df   <- cbind.data.frame(what="all layer cells", ID=0,  csquare=0, data.frame(a_dt))
  e_mpas <- rbind.data.frame(a_df, e_mpas) # Adding a Marginal Total for ALL (i.e. INSIDE+OUTSIDE) as ID=0
 
 
 
         
 if(FALSE){
 # try this, for fun:
 library(mapview)
 mapviewOptions(fgb = FALSE) # needed when creating web pages
 mapview(st_geometry(allclosures_sf_eea))
      
 #  raster to terra
 #rstr_eea_sub_terra <- terra::rast(rstr_eea_sub)
 #vect_eea_sub_terra <- terra::as.polygons(rstr_eea_sub_terra, dissolve=FALSE, value=TRUE, na.rm=TRUE)  # broken...values are lost!
   
 # raster to polygon to sf
 vect_eea_sub       <- rasterToPolygons(raster(rstr_eea_sub))
 vect_eea_sub_sf    <- st_as_sf(vect_eea_sub)

 # transform in mercator to be compatible to mapview
 a_crs               <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs +type=crs"
 allclosures_sf_merc <- st_transform(allclosures_sf_eea, crs(a_crs))
 vect_sub_sf_merc    <- st_transform(vect_eea_sub_sf, crs(a_crs))
 
 # the fun:  
 mapview(st_geometry(allclosures_sf_merc), native.crs=FALSE)+
      mapview(vect_sub_sf_merc, col.regions = colorRampPalette(c("white", "green", "red")), native.crs=FALSE, alpha.regions=0.4, at=c())
 } # end FALSE  
 
   
return(e_mpas)
} else{ cat(paste0("no spatRast file found in ", filepath, "\n"))}
 
}


 ##------------------------------------
 ##--CALLS-----------------------------
 # read in closure shp
 #allclosures_sf <- st_read(file.path("D:","FBA","ADVICES","STECF","STECF_ad_hoc_2023_VMEs","Contract n2","fba_closure_VMEs_2022.shp"))
 #allclosures_sf <- st_read(file.path(getwd(), "INPUT_SPATIAL_LAYERS", "OWF", "EMODnet_HA_WindFarms_20221219", "EMODnet_HA_WindFarms_pg_20221219.shp"))
 # etc.
 
 
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
  
  owf_terra <- rbind(owf_msfd_vect_terra, owf_miss_msfd_vect_terra)
  
  # check
  graphics.off()
  plot(owf_miss_msfd_vect_terra)
  plot(owf_msfd_vect_terra, add=TRUE, col="red")
  
  plot(owf_terra, add=FALSE, col="red")

  #plot(mpas_vect_terra, add=TRUE, col="green")
                
 

 #
# # The recent shp sent by EC:
# library(sf)
# VMEs_MARE <- st_read(file.path(getwd(),"INPUT_SPATIAL_LAYERS","VMEs","DGMARE_Sc2Opt1", "Scenario2_option1_DG_Mare_Poly.shp"))
# ICES_VMEs_SceC <- st_read(file.path(getwd(),"INPUT_SPATIAL_LAYERS","VMEs","ICES_VMEs_Coordinates_shp", "Scenario_C.shp"))
# ICES_VMEs_SceD <- st_read(file.path(getwd(),"INPUT_SPATIAL_LAYERS","VMEs","ICES_VMEs_Coordinates_shp", "Scenario_D.shp"))
# plot(st_geometry(VMEs_MARE))
# plot(st_geometry(ICES_VMEs_SceC), col=rgb(0.5,0.5,0.5,0.5), add=TRUE)
# plot(st_geometry(ICES_VMEs_SceD), col=rgb(0.8,0.2,0.5,0.8), add=TRUE)
 
 
 # a stacked visual
 if(FALSE){
 library(leaflet)
 map <- leaflet() %>%
  addTiles() 
 map <- map %>% addPolygons(data=owf_msfd, popup=~htmltools::htmlEscape(paste(NAME)), col="blue", group = "OWF")  %>%
      addPolygons(data=owf_msfd_missing, popup=~htmltools::htmlEscape(paste(Name)), col="green", group = "OWFmiss")  %>%
      addPolygons(data=sf::st_as_sf(mpas_rmv_bt), popup=~htmltools::htmlEscape(paste(SiteName)), col="red", group = "MPA_btrawels") %>%
      addPolygons(data=sf::st_as_sf(mpas_rmv_nts), popup=~htmltools::htmlEscape(paste(SiteName)), col="red", group = "MPA_netters") %>%
      addPolygons(data=sf::st_as_sf(mpas_rmv_lns), popup=~htmltools::htmlEscape(paste(SiteName)), col="red", group = "MPA_longliners")  
 map <- map %>% addLayersControl(
     baseGroups = "",
     overlayGroups = c("OWF","OWFmiss", "MPA_btrawlers", "MPA_netters", "MPA_longliners"),
    options = layersControlOptions(collapsed = FALSE))
 map
 library(htmlwidgets)
 saveWidget(map, file=file.path(getwd(),"OVERLAY","leaflet_map_restrictions.html"))
 }

 #!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!#
 sces <- c("OWF", "currentMPAs", "MPAs", "OWF+currentMPAs", "OWF+MPAs") 
 years      <- 2018:2021
 a_folder   <- "OUTCOME_OVERLAY"
 a_folder2  <- "OUTCOME_FISHERIES_DISTR_VMS_AER"
 #!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!#
 
 
 if(length(grep( "VMS", a_folder2))!=0){
  is_vms <- TRUE; is_fdi <- FALSE
 } else{
 is_vms <- FALSE ; is_fdi <- TRUE
 }


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
#  FOR EACH IMPACTED PER C-SQUARE, 
# FIND THE SURFACE AREA, THE FISHABLE AREA, AND THE CLOSED AREA
# TO FIND OUT THE GVA PER FISHABLE AREA IN EACH IMPACTED C-SQUARE
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

# the goal is to obtain a multiplier to do: GVA impacted = GVA in the impacted csquare * CLOSED AREA / FISHABLE AREA

## GET A PREPARATORY TABLE
 library(terra)
 library(doBy)
 lookup_table_prop_fishable_area <-  NULL 
 an <- function(x) as.numeric(x)
 
 bathy_400_800        <- rast(file.path(getwd(), "INPUT_SPATIAL_LAYERS","GEBCO_May_2023","gebco.tif"))
 bathy_0_800_1000     <- rast(file.path(getwd(), "INPUT_SPATIAL_LAYERS","GEBCO_May_2023","GEBCO_bathy_coding_for_o800_and_o1000m.tif"))              

 years                  <- c("2018", "2019", "2020", "2021","2018_2021")
 if (is_vms) metiers              <- "all_metiers" 
 if (is_fdi) metiers              <- "all_bottom_metiers"
 for(yrs in years)
 {
 for (sce in sces){
    cat(paste0("Scenario ", sce, "\n"))
    if(sce=="OWF")               allclosures_terra <- owf_terra 
    if(sce=="currentMPAs")       allclosures_terra <- unique(rbind(mpas_rmv_bt_current, mpas_rmv_nts_current, mpas_rmv_lns_current))
    if(sce=="MPAs")              allclosures_terra <- unique(rbind(mpas_rmv_bt, mpas_rmv_nts, mpas_rmv_lns))
    if(sce=="OWF+currentMPAs")   allclosures_terra <- unique(rbind(owf_terra, mpas_rmv_bt_current, mpas_rmv_nts_current, mpas_rmv_lns_current))  # don´t forget to remove duplicates with unique()
    if(sce=="OWF+MPAs")          allclosures_terra <- unique(rbind(owf_terra, mpas_rmv_bt, mpas_rmv_nts, mpas_rmv_lns))  # don´t forget to remove duplicates with unique()
 
 filepath             <- file.path(getwd(), a_folder2, metiers, yrs)
 rstr                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
 rstr$GVA <-  (an(rstr$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(rstr$value_aer_in_ctry_level6_csquare)/an(rstr$landings_aer_in_ctry_level6_csquare))) + 
                      an(rstr$other_income_in_csquare) - # plus other income
                      an(rstr$unpaid_labour_in_csquare) - an(rstr$varcosts_in_ctry_level6_csquare) - an(rstr$oth_non_var_costs_in_csquare)  # minus var and fixed costs
    
 r <- rast(nrow=dim(rstr)[1], ncol=dim(rstr)[2],
                                     extent=ext(rstr), res=res(rstr), crs=crs(rstr), vals=NA, names="value") # init
 # adding a csquare code
 # note that a query tool for an individual c-square can be retrieved at http://www.cmar.csiro.au/csquares/about-mapper.htm
 cells <- 1:ncell(r)
 xy <- xyFromCell(r, cells)
 if(is_fdi) csquares <- vmstools::CSquare(xy[,"x"],xy[,"y"], degrees=0.5)
 if(is_vms) csquares <- vmstools::CSquare(xy[,"x"],xy[,"y"], degrees=0.05)
 values(r) <- 1:length(csquares)
 csquare_coding <- data.frame(id=c(values(r)), csquare=csquares, x=xy[,"x"], y=xy[,"y"])
 names(r)  <- "CSQUARE_ID"
 add(rstr) <- r
 
 if(is_fdi){
  # figuring out what is the csquare code of each polygon
  e_csquares <- extract(rstr, vect(allclosures_sf), fun=modal) # modal => get the more frequent
  csquare_per_polygon_id_for_this_sce <- cbind.data.frame(e_csquares[, c("ID", "CSQUARE_ID")], csquare=csquare_coding[match(e_csquares[, "CSQUARE_ID"], csquare_coding$id), c("csquare", "x", "y")]) 
 }
 if(is_vms){
  # figuring out what is the csquare codes of each polygon
  e_csquares <- extract(rstr, allclosures_terra) 
  csquare_per_polygon_id_for_this_sce <- cbind.data.frame(e_csquares[, c("ID", "CSQUARE_ID")], csquare=csquare_coding[match(e_csquares[, "CSQUARE_ID"], csquare_coding$id), c("csquare", "x", "y")]) 
 }
 
 
 # subsetting to only keep the info on csquares containing the closure polygons
 dd <- values(rstr$CSQUARE_ID)
 csquare_per_polygon_id_for_this_sce <- csquare_per_polygon_id_for_this_sce[!is.na(csquare_per_polygon_id_for_this_sce$CSQUARE_ID),] 
 idx_to_discard <- c(1:ncell(rstr))[!(c(dd) %in% csquare_per_polygon_id_for_this_sce$CSQUARE_ID)]
 values(rstr$GVA)[idx_to_discard] <- NA
 rstr <- trim(rstr)
 # check
 plot(log(rstr$GVA))
  
   
 # align both bathy and data rasters
 if(is_fdi) n <- 30
 if(is_vms) n <- 4 # caution: estimates are quite sensitive to this choice...
 bathy_0_800_1000_c          <- crop(bathy_0_800_1000, rstr)
 rstr$surface_area_csquare   <- cellSize(rstr$GVA, unit="km", names="surface_area")  # returns only for cells!=NA
 rstr_disagg                 <- disagg(rstr, n) # increase the resolution to line up with bathy
 bathy_0_800_1000_c          <- resample(bathy_0_800_1000_c, rstr_disagg, method="near") # align
 bathy_0_800_c               <- bathy_0_800_1000_c# init
 bathy_0_800_c[bathy_0_800_c > 1] <- NA # no bottom trawl allowed if bathy >-800
 rstr_in_bathy_0_800         <- rstr_disagg + bathy_0_800_c 
 
 # rasterize the closure shape
 if(is_fdi) dd               <- terra::vect(allclosures_sf)
 if(is_vms) dd               <- allclosures_terra # a SpatVect needed
 dd$value                    <- 1
 closure_rast_terra          <- terra::rasterize(dd, y=rstr_in_bathy_0_800, field="value", fun=sum, na.rm=TRUE)
 rstr_closure                <- rstr_disagg + closure_rast_terra  + bathy_0_800_c   ## CAUTION: PASSIVE GEARS ARE NOT PREVENTED FROM VISITING THESE AREAS....
 surface_area_of_closure_csquare    <- cellSize(rstr_closure$CSQUARE_ID, unit="km", names="surface_area_closure")  # returns only for cells!=NA
 surface_area_of_closure_csquare    <- aggregate(surface_area_of_closure_csquare, n, fun= "sum", by="CSQUARE_ID", na.rm=TRUE)
 #=> this gives the surface of the closure in the fishable area

 # find out the TOTAL closure surface for info
 rstr_closure_tot                <-  closure_rast_terra 
 surface_area_of_closure_tot_csquare    <- cellSize(rstr_closure_tot$value, unit="km", names="surface_area_closure_tot")  # returns only for cells!=NA
 surface_area_of_closure_tot_csquare    <- aggregate(surface_area_of_closure_tot_csquare, n, fun= "sum", by="CSQUARE_ID", na.rm=TRUE)
 totsurface <- sum(surface_area_of_closure_tot_csquare[], na.rm=TRUE)
   
 # find out the surface area per clipped Csquare
 surface_area_of_clipped_csquare    <- cellSize(rstr_in_bathy_0_800$CSQUARE_ID, unit="km", names="surface_area")  # returns only for cells!=NA
 surface_area_of_clipped_csquare    <- aggregate(surface_area_of_clipped_csquare, n, fun= "sum", by="CSQUARE_ID", na.rm=TRUE)
 
 # build a raster prop 
 prop_surface_area_of_clipped_csquare <- surface_area_of_clipped_csquare/rstr$surface_area_csquare
 names(prop_surface_area_of_clipped_csquare) <- "prop_fishable_area"
 add(prop_surface_area_of_clipped_csquare) <- rstr$CSQUARE_ID 
 add(prop_surface_area_of_clipped_csquare) <- rstr$surface_area_csquare
 add(prop_surface_area_of_clipped_csquare) <- surface_area_of_closure_csquare
 
 prop_surface_area_of_clipped_csquare_df <- prop_surface_area_of_clipped_csquare[]
 prop_surface_area_of_clipped_csquare_df <- prop_surface_area_of_clipped_csquare_df[complete.cases(prop_surface_area_of_clipped_csquare_df),]
 lookup_table_prop_fishable_area_per_csquare_and_ID_polygon <- merge(prop_surface_area_of_clipped_csquare_df, csquare_per_polygon_id_for_this_sce, all=TRUE) 
 lookup_table_prop_fishable_area_per_csquare_and_ID_polygon <- orderBy(~ID, lookup_table_prop_fishable_area_per_csquare_and_ID_polygon) 

 # collect for later use
 lookup_table_prop_fishable_area <- rbind.data.frame(lookup_table_prop_fishable_area,
                 cbind.data.frame(sce=sce, year=yrs, metier=metiers, lookup_table_prop_fishable_area_per_csquare_and_ID_polygon, totsurfaceclosure=totsurface)) 
 #=> some NAs because no fishable area into it...
  
 # check fishable area in each c-square
 plot(surface_area_of_clipped_csquare)
 plot(rstr_in_bathy_0_800$CSQUARE_ID, add=TRUE, col=rgb(0.2,0.2,0.2,0.2), leg=FALSE)
 
 plot(prop_surface_area_of_clipped_csquare)
 plot(rstr_in_bathy_0_800$CSQUARE_ID, add=TRUE, col=rgb(0.2,0.2,0.2,0.2), leg=FALSE)
  
 n2 <- rstr$CSQUARE_ID %in% 51853
 plot(n2)
  
  
 # a plot
 a_width <- 4000 ;  a_height <- 4000
 tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY", a_folder2,paste0("GVA_",metiers,"_in_impacted_csquare_from_", yrs,"_", sce, ".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
 par(oma=c(1,1,1,1))
 par(mar=c(2,2,2,3))
     if(is_vms) plot(rstr_in_bathy_0_800$GVA/1e6, main=paste0(yrs, "GVA (MEuro in 0.05 cells) ", sce), breaks=c(-2, -1, seq(0, 0.5, by=0.25), 5, 6),  lwd=0.1)
     if(is_fdi) plot(rstr_in_bathy_0_800$GVA/1e6, main=paste0(yrs, "GVA (MEuro) ", sce), breaks=c(-15, -5, -1, seq(0, 5, by=1), 50),  lwd=0.5)
     #plot(as.polygons(rstr$GVA), add=TRUE,  lwd=0.1)
     if(is_fdi) plot(vect(allclosures_sf), add=TRUE, lwd=0.5)
     if(is_vms) plot(allclosures_terra, add=TRUE, lwd=0.5, border="grey")
     library(rnaturalearth)
     sf_world <- ne_countries(returnclass='sf')
     spdf_europe <- ne_countries(continent = "europe", scale=10)
     plot(vect(spdf_europe), col=grey(0.5), add=TRUE)
  dev.off()

 
 } # end sce 
} # end yrs

save(lookup_table_prop_fishable_area,
           file=file.path(file.path(getwd(), "OUTCOME_OVERLAY", a_folder2, 
             paste0("lookup_prop_fishable_area.RData")))) 
    # prop of fishable area is constrained by bathymetry >-800
    # surface closure area specific to sce
    # area surface of csquares vary along latitudinal range if longlat coord

 
library(data.table) 
dd <- data.table(lookup_table_prop_fishable_area) 
dd <- dd[,.(impacted_surface_closed_km2=sum(surface_area_closure, na.rm=TRUE), tot_surface_closed_km2=mean(totsurfaceclosure, na.rm=TRUE)),by=c("sce","year","metier")] 
dd$impacted_surface_closed_km2 <- round(dd$impacted_surface_closed_km2)
dd$tot_surface_closed_km2 <- signif(an(dd$tot_surface_closed_km2),3)
#=> caution: it varies alongside years depending on the year footprint 
dd <- knitr::kable(as.data.frame(dd), format = "html")
library(readr)
readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("impacted_surface_closed_km2.html"))) 
 
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
# EXTRACT PER POLYGON ID
# TO FIND OUT THE GVA PER POLYGON IN EACH IMPACTED C-SQUARE
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##


 years                  <- c("2018_2021", "2018", "2019", "2020", "2021")
 #years                  <- c("2018_2021")

 load(file=file.path(file.path(getwd(), "OUTCOME_OVERLAY_VMEs", a_folder2, 
             paste0("lookup_prop_fishable_area.RData"))))   # lookup_table_prop_fishable_area
           
 

 dir.create(file.path(getwd(), a_folder, a_folder2, "VectorExtract"), recursive=TRUE)
 output <- NULL
 collector_extraction_per_fs_per_boxID <- NULL
 for (sce in sces){
    cat(paste0("Scenario ", sce, "\n"))
    if(sce=="Closure2022") allclosures_sf <- VMEs_MARE
    if(sce=="ICES_SceC")   allclosures_sf <- ICES_VMEs_SceC
    if(sce=="ICES_SceD")   allclosures_sf <- ICES_VMEs_SceD
    ## PER METIER----------
    # read-in the merged disagregated AER variables in shapfiles
    for(y in years){
     cat(paste0("Year ", y, "\n"))
     all_fs         <- list.files(file.path(getwd(),  a_folder2))
     idx_fs_to_keep <- c(grep("DTS", all_fs), grep("TBB", all_fs), grep("DRB", all_fs), grep("HOK", all_fs), grep("DFN", all_fs)) # bottom contacting gears + hooks(longline) and nets (in FDI). only bottom gears in the ICES VMS
     #idx_fs_to_keep <- c(grep("DTS", all_fs), grep("TBB", all_fs), grep("DRB", all_fs)) # bottom contacting gears only
     all_fs         <- all_fs[idx_fs_to_keep]
     all_fs         <- c("all_metiers", all_fs) #, "PRT_VL1218",     "PRT_VL1824",     "PRT_VL2440",     "PRT_VL40XX")
     for (fs in all_fs)
       {
       cat(paste0("Fleet-segment ", fs, "\n"))
       library(data.table)
       filepath             <- file.path(getwd(), a_folder2, fs, y)
       e_mpas_this_metier <- what_is_inside (filepath, allclosures_sf, reproject=FALSE) # caution with a reproject at TRUE cause the resampling will blur estimates....
  
  
       if(!is.null(e_mpas_this_metier)){
          an <- function (x) as.numeric(x)
          e_mpas_this_metier$GVA_cell <-  (an(e_mpas_this_metier$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(e_mpas_this_metier$value_aer_in_ctry_level6_csquare)/an(e_mpas_this_metier$landings_aer_in_ctry_level6_csquare))) + 
                      an(e_mpas_this_metier$other_income_in_csquare) - # plus other income
                      an(e_mpas_this_metier$unpaid_labour_in_csquare) - an(e_mpas_this_metier$varcosts_in_ctry_level6_csquare) - an(e_mpas_this_metier$oth_non_var_costs_in_csquare)  # minus var and fixed costs
         e_mpas_this_metier$GVA_all <-  e_mpas_this_metier[e_mpas_this_metier$what=="all layer cells","GVA_cell"]  # track the value to get a proportion later on
         
         e_mpas_this_metier$GrossProfit_cell     <-  e_mpas_this_metier$GVA_cell - an(e_mpas_this_metier$personnelcosts_in_ctry_level6_csquare)
         e_mpas_this_metier$GrossProfit_GVA_all  <- e_mpas_this_metier[e_mpas_this_metier$what=="all layer cells","GrossProfit_cell"]  # track the value to get a proportion later on
        
                      
       # merge with lookup_table_prop_fishable_area
       e_mpas_this_metier     <- cbind.data.frame(sce=sce, year=y, e_mpas_this_metier)
       e_mpas_this_metier_m   <- merge(e_mpas_this_metier, lookup_table_prop_fishable_area, by.x=c("sce","year","ID", "csquare"), by.y=c("sce","year","ID", "csquare.csquare"))
  
       # apply a multiplier accordingly
       is_bottom_trawls                              <- c(grep("DTS", fs), grep("TBB", fs), grep("DRB", fs)) 
       a_multiplier                                  <-  e_mpas_this_metier_m$surface_area_closure/(e_mpas_this_metier_m$surface_area_csquare)  # default e.g. longliners, netters, ie.not restricted by the -800m bathy
       if(length(is_bottom_trawls)!=0) a_multiplier  <-  e_mpas_this_metier_m$surface_area_closure/(e_mpas_this_metier_m$surface_area_csquare*e_mpas_this_metier_m$prop_fishable_area) # restricted by the -800m bathy
       
        a_multiplier[!is.na(a_multiplier) & a_multiplier[]>1] <- 1  # if VMS data
       
       e_mpas_this_metier_m$GVAimpacted              <-  e_mpas_this_metier_m$GVA_cell  * a_multiplier 
       e_mpas_this_metier_m$GrossProfitimpacted              <-  e_mpas_this_metier_m$GrossProfit_cell  * a_multiplier 
                                               
       # then the proportion impacted
       e_mpas_this_metier_m$prop_GVA_inside <-  e_mpas_this_metier_m$GVAimpacted/  e_mpas_this_metier_m$GVA_all  
       

       dd <- knitr::kable(as.data.frame(e_mpas_this_metier_m), format = "html")
       library(readr)
       readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, "VectorExtract", paste0("e_mpas_",sce,"_",fs,"_",y,".html"))) 

    
      # collector 
      collector_extraction_per_fs_per_boxID <- rbind.data.frame(collector_extraction_per_fs_per_boxID, 
                                                  cbind.data.frame(fs, e_mpas_this_metier_m))
      } # end 
    } # end fs
   } # end y   
} # end sce
  
                        
 
 # general outcome 
 library(readr)
 dd <- knitr::kable(as.data.frame(collector_extraction_per_fs_per_boxID), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("collector_extraction_per_fs_per_boxID_","2018", "_","2021",".html"))) 
 save(collector_extraction_per_fs_per_boxID, file=file.path(getwd(), a_folder, a_folder2, paste0("collector_extraction_per_fs_per_boxID_","2018", "_","2021",".RData"))) 
 # caution with this dataset here because of doubling counting if deduced from the fdi given there might be several boxes within a same coarse 0.5 c-square...remove duplicates if needed





##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 # do a ggplot barplot out of this..........

 library(ggplot2)
 
 #!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!#
 sces       <- c("Closure2022", "ICES_SceC", "ICES_SceD")
 years      <- 2018:2021
 a_folder   <- "OUTCOME_OVERLAY_VMEs"
 a_folder2  <- "OUTCOME_FISHERIES_DISTR_FDI_AER"
 #a_folder   <- "OUTCOME_OVERLAY_VMEs"
 #a_folder2  <- "OUTCOME_FISHERIES_DISTR_VMS_AER_VMEs"
 yrs <- "2018_2021" 
 #!!!!!!!!!!!!!!!!!!!!#
 #!!!!!!!!!!!!!!!!!!!!#
 

 load(file=file.path(getwd(), a_folder, a_folder2, paste0("collector_extraction_per_fs_per_boxID_","2018", "_","2021",".RData")))  # collector_extraction_per_fs_per_boxID

 extract_df <- collector_extraction_per_fs_per_boxID
 
 
 library(stringr)
 temp <- as.data.frame(str_split_fixed(extract_df$fs,"_",3))  
 extract_df$country       <- temp[,1]
 extract_df$fishing_tech  <- temp[,2]
 extract_df$vessel_size   <- temp[,3]
 

 extract_df <- extract_df[extract_df$year==yrs,]  # threshold for visualisation

 # check 
 head(extract_df[extract_df$sce==sce & extract_df$csquare=="3002:100:3" & extract_df$year ==yrs & extract_df$fs=="ESP_DTS_VL1824",])

 
 # Remove duplicates of values to avoid double counting if several box ID lying within the same a c-square
 extract_df <- extract_df [!duplicated(extract_df[,c("sce","csquare", "fs", "year")]),]


 dd <- aggregate(extract_df[extract_df$sce=="Closure2022",][,c("prop_GVA_inside")], by=list(extract_df[extract_df$sce=="Closure2022",]$fs), sum, na.rm=TRUE) # sum over ID
 library(doBy)
 dd <- dd[round(dd$x, 5)!=0,]
 dd <- orderBy(~ - x, dd)
 
 fs_to_keep <-  dd[,1] [1:40]
 fs_to_keep <-  fs_to_keep[fs_to_keep!="all_metiers"]
 
 extract_df <- extract_df[!is.na(extract_df$prop_GVA_inside) & extract_df$fs %in% fs_to_keep,]  # threshold for visualisation
 
 extract_df$fs <- factor (extract_df$fs, levels=fs_to_keep) # re-order in consistence with a_df


 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 ## GGPLOT 1
 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 p1 <- ggplot(extract_df[,c("prop_GVA_inside", "fs", "vessel_size", "sce")], aes(x =  prop_GVA_inside*100, y=fs, fill=vessel_size))  + geom_bar(stat = "summary", fun = "sum", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Extract in SWW and NWW")) + xlab(paste0("Percent in ",yrs, " of impacted GVA inside VMEs areas")) + ylab("AER Fleet-segments") +
                      scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))  
  a_width <- 6000 ; a_height <- 4500
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY_VMEs", a_folder2, paste0("Proportion_of_GVA_inside_impacted_c-squares.tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p1)
 dev.off()

 # extract data corresponding to the plot
 pg <- ggplot_build(p1)
 pd <- pg$data[[1]]
 colnames(pd)[colnames(pd) %in% "group"] <- "fs"
 pd$fs <- factor(pd$fs) ; levels(pd$fs) <- levels(extract_df$fs)
 colnames(pd)[colnames(pd) %in% "PANEL"] <- "sce"
 pd$sce <- factor(pd$sce) ;  extract_df$sce <- factor(extract_df$sce) ; levels(pd$sce) <- levels(extract_df$sce)
 colnames(pd)[colnames(pd) %in% "fill"]  <- "vessel_size"
 pd$vessel_size <- factor(pd$vessel_size) ;  extract_df$vessel_size <- factor(extract_df$vessel_size); levels(pd$vessel_size) <- levels(extract_df$vessel_size)
 colnames(pd)[colnames(pd) %in% "x"]    <- "Percent"
 pd$Percent <- round(pd$Percent, 2)
 # export
 library(readr)
 dd <- knitr::kable(as.data.frame(pd[, c("fs", "sce", "Percent")]), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".html"))) 
 save(pd, file=file.path(getwd(), a_folder, a_folder2, paste0("ggplot_data_percent_of_GVA_inside_impacted_c-squares_","2018", "_","2021",".RData"))) 





 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 ## GGPLOT 2
 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 
 a_df <- extract_df[,c("GVAimpacted", "fs", "vessel_size", "sce", "csquare")]
 a_df <- a_df[an(a_df$GVAimpacted)>15000,] # put a threshold of 100000 Euros to lighten the figure...
 p2 <- ggplot(a_df, aes(x =  GVAimpacted/1e6, y=csquare, fill=fs))  + geom_bar(stat = "summary", fun = "mean", position = "stack") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Extract in SWW and NWW")) + xlab(paste0("GVA (MEuro) of ",yrs, " estimated inside VMEs areas")) + ylab("Csquare code") +guides(fill=guide_legend(ncol =1))
  a_width <- 3000 ; a_height <- 5000
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY_VMEs", a_folder2, paste0("Absolute_GVA_inside_VMEs_per_csquare.tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=300, compression = c("lzw"))
 p2
 dev.off()
 
  # extract data corresponding to the plot
 pg <- ggplot_build(p2)
 pd <- pg$data[[1]]
 colnames(pd)[colnames(pd) %in% "y"] <- "csquare"
 pd$csquare <- factor(pd$csquare) ; extract_df$csquare <- factor(extract_df$csquare) ; levels(pd$csquare) <- levels(extract_df$csquare)
 colnames(pd)[colnames(pd) %in% "PANEL"] <- "sce"
 pd$sce <- factor(pd$sce) ;  extract_df$sce <- factor(extract_df$sce) ; levels(pd$sce) <- levels(extract_df$sce)
 colnames(pd)[colnames(pd) %in% "fill"]  <- "fs"
 pd$fs <- factor(pd$fs) ;  extract_df$fs <- factor(extract_df$fs); levels(pd$fs) <- levels(extract_df$fs)
 colnames(pd)[colnames(pd) %in% "x"]    <- "GVAimpactedMEuro"
 pd$GVAimpactedMEuro <- round(pd$GVAimpactedMEuro, 2)
 # export
 library(readr)
 dd <- knitr::kable(as.data.frame(pd[, c("fs", "sce", "csquare", "GVAimpactedMEuro")]), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("ggplot_data_GVAimpactedMEuro _inside_impacted_c-squares_",years[1], "_",years[length(years)],".html"))) 
 save(pd, file=file.path(getwd(), a_folder, a_folder2, paste0("ggplot_data_GVAimpactedMEuro_inside_impacted_c-squares_",years[1], "_",years[length(years)],".RData"))) 

  

 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 ## MAP 1
 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
  # try a mapping the most impacting squares
  for (sce in c("Closure2022","ICES_SceC", "ICES_SceD"))
  {
  if(sce=="Closure2022") allclosures_sf <- VMEs_MARE
  if(sce=="ICES_SceC")   allclosures_sf <- ICES_VMEs_SceC
  if(sce=="ICES_SceD")   allclosures_sf <- ICES_VMEs_SceD
 
  if(is_fdi) n <- 15
  if(is_vms) n <- 100
  
  impacting_sq <- tapply(extract_df[extract_df$sce==sce,]$GVAimpacted, factor(extract_df[extract_df$sce==sce,]$CSQUARE_ID), sum, na.rm=TRUE) # sum over ID
  impacting_sq <- impacting_sq[order(impacting_sq, decreasing=TRUE)] [1:n]

  filepath             <- file.path(getwd(), a_folder2, "all_metiers", "2018_2021")
  rstr                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
  
  r <- rast(nrow=dim(rstr)[1], ncol=dim(rstr)[2], extent=ext(rstr), res=res(rstr), crs=crs(rstr), vals=NA, names="value") # init

 # adding a csquare code
 cells <- 1:ncell(r)
 xy <- xyFromCell(r, cells)
 if(is_fdi) csquares <- vmstools::CSquare(xy[,"x"],xy[,"y"], degrees=0.5)
 if(is_vms) csquares <- vmstools::CSquare(xy[,"x"],xy[,"y"], degrees=0.05)
 values(r) <- csquares
 csquare_coding <- data.frame(id=c(values(r)), csquare=csquares, x=xy[,"x"], y=xy[,"y"])
 names(r)  <- "CSQUARE_ID"
 add(rstr) <- r
 
#  dd <- values(rstr$CSQUARE_ID)
#  # figuring out what is the csquare code of each polygon
#  e_csquares <- extract(rstr, vect(allclosures_sf), fun=modal) # modal => get the more frequent
#  csquare_per_polygon_id_for_this_sce <- cbind.data.frame(e_csquares[, c("ID", "CSQUARE_ID")], csquare=csquare_coding[match(e_csquares[, "CSQUARE_ID"], csquare_coding$id), "csquare"]) 
#  idx_to_discard <- c(1:ncell(rstr))[!c(dd) %in% csquare_per_polygon_id_for_this_sce$CSQUARE_ID]
#  rstr[idx_to_discard] <- NA
#  rstr <- trim(rstr)

  top5ImpactingCsquare <- rstr$CSQUARE_ID %in% as.numeric(names(impacting_sq)) [1:n]  # the first ten most impacting!!!!!!
     plot(top5ImpactingCsquare, main=paste0(yrs, "- most impacting csquares ", sce), breaks=c(-15, seq(0, 2, by=0.5), 15))
     plot(vect(allclosures_sf), add=TRUE)
     top5ImpactingCsquare[top5ImpactingCsquare<1] <- NA
    
    # reinject GVAimpacted data to the raster
     impacting_sq <- impacting_sq[!is.na(impacting_sq)]  
     idx <- match (as.numeric(names(impacting_sq)), rstr$CSQUARE_ID[])   
     values(top5ImpactingCsquare)[idx] <- impacting_sq 
    
    csq <- rstr$CSQUARE_ID
    values(csq)[-idx] <- NA 
    
    
  # a plot
  a_width <- 3000 ;  a_height <- 4000
  tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY_VMEs", a_folder2, paste0("most_impacted_csquares_on_GVA_from_", yrs,"_", sce, ".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     par(mar=c(0,0,0,0))
     par(oma=c(0,0,0,0))
     if(is_fdi) plot(trim(top5ImpactingCsquare)/1e6, main=paste0(yrs, "- 15 most impacted csquares ", sce),  breaks=c( -2, -1, seq(0, 1, by=0.25), 2))
     if(is_vms) plot(trim(top5ImpactingCsquare)/1e6, main=paste0(yrs, "- 100 most impacted csquares ", sce),  breaks=c(-2, seq(-1, 0, by=0.1), 2))
     #text(csq, cex=0.5, label="123")

     xy <- xyFromCell(top5ImpactingCsquare, idx)
     if(is_fdi) text(x = xy[,"x"], y = xy[,"y"], labels = csquare_coding[match(names(impacting_sq), csquare_coding[,1]),  2 ], cex=0.18, srt=90, col="white")
     
     library(rnaturalearth)
     #plot(as.polygons(rstr$landings_aer_in_ctry_level6_csquare), add=TRUE)
     plot(vect(allclosures_sf), add=TRUE, lwd=0.5)
     library(rnaturalearth)
     sf_world <- ne_countries(returnclass='sf')
     spdf_europe <- ne_countries(continent = "europe", scale=10)
     plot(vect(spdf_europe), col=grey(0.5), add=TRUE)
  dev.off()

 aa <-  cbind.data.frame("C-SQUARE"=csquare_coding[match(names(impacting_sq), csquare_coding[,1]),  2 ], "GVA impacted (Euro)"=round(impacting_sq))
 rownames(aa) <- NULL
 # export
 library(readr)
 dd <- knitr::kable(as.data.frame(aa), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("most_impacted_csquares_on_GVA_from_", yrs,"_", sce, ".html"))) 
 
 # export for later use with leaflet
 a_rstr <- trim(top5ImpactingCsquare)
 names(a_rstr) <- "MEuroGVA"
 a_sf <- st_as_sf(as.polygons(a_rstr/1e6, dissolve=FALSE, values=TRUE))
 assign(paste0("Impacted_CSquares_",sce), a_sf)                                                 
 
 } # end sce


##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
## INTERACTIVE MAP embedded in a HTML page ##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

  # a visual check 
  # stacked:
 library(leaflet)
 map <- leaflet() %>%
  addTiles() 
 map <- map %>% addPolygons(data=VMEs_MARE, popup=~htmltools::htmlEscape(paste(Poly_No)), col="blue", group = "VMEs_Closure2022")  %>%
      addPolygons(data=ICES_VMEs_SceC, popup=~htmltools::htmlEscape(paste(poly_id)), col="green", group = "ICES_VMEs_SceC")  %>%
      addPolygons(data=ICES_VMEs_SceD, popup=~htmltools::htmlEscape(paste(poly_id)), col="red", group = "ICES_VMEs_SceD")  %>% 
      addPolygons(data=Impacted_CSquares_Closure2022, popup=~htmltools::htmlEscape(paste(MEuroGVA)), col="purple", group = "Impacted_CSquares_Closure2022")  %>%
      addPolygons(data=Impacted_CSquares_ICES_SceC, popup=~htmltools::htmlEscape(paste(MEuroGVA)), col="cyan", group = "Impacted_CSquares_ICES_SceC")  %>%
      addPolygons(data=Impacted_CSquares_ICES_SceD, popup=~htmltools::htmlEscape(paste(MEuroGVA)), col="yellow", group = "Impacted_CSquares_ICES_SceD") 
 map <- map %>% addLayersControl(
     baseGroups = "",
     overlayGroups = c("VMEs_Closure2022","ICES_VMEs_SceC", "ICES_VMEs_SceD", "Impacted_CSquares_Closure2022", "Impacted_CSquares_ICES_SceC", "Impacted_CSquares_ICES_SceD"),
    options = layersControlOptions(collapsed = FALSE))
 map

 library(htmlwidgets)
 saveWidget(map, file=file.path(getwd(), a_folder, a_folder2, "map_impacted_csquares.html"))



##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# COMPUTE ECONOMY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

 library(data.table)
 ROutputPathToDatasets <- file.path(getwd(), "OUTCOME_DATASETS")
 if(is_fdi) load(file=file.path(ROutputPathToDatasets, "agg_2018_2021_fdi_2023aer_eco_fs.RData")) # get agg_eco_fs 
 if(is_vms) load(file=file.path(ROutputPathToDatasets, "agg_ices_vms_2018_2021_aer_eco_fs.RData")) # get agg_eco_fs 

 head(agg_eco_fs[fs=="ESP_DTS_VL2440",])


 load(file=file.path(getwd(), a_folder, a_folder2, paste0("collector_extraction_per_fs_per_boxID_2018_2021.RData")))  # collector_extraction_per_fs_per_boxID
 extract_df <- collector_extraction_per_fs_per_boxID
 library(stringr)
 temp <- as.data.frame(str_split_fixed(extract_df$fs,"_",3))  
 extract_df$country       <- temp[,1]
 extract_df$fishing_tech  <- temp[,2]
 extract_df$vessel_size   <- temp[,3]
 
 # Remove duplicates of values to avoid double counting if several box ID lying within the same a c-square
 extract_df <- extract_df [!duplicated(extract_df[,c("sce","csquare", "fs", "year")]),]

 
 dd <- aggregate(extract_df[extract_df$sce=="Closure2022",][,c("prop_GVA_inside")], by=list(extract_df[extract_df$sce=="Closure2022",]$fs), sum, na.rm=TRUE) # sum over ID
 library(doBy)
 dd <- dd[round(dd$x, 5)!=0,]
 dd <- orderBy(~ - x, dd)
 
 fs_to_keep <-  dd[,1] [1:40]
 fs_to_keep <-  fs_to_keep[fs_to_keep!="all_metiers"]
 
 extract_df <- extract_df[!is.na(extract_df$prop_GVA_inside) & extract_df$fs %in% fs_to_keep,]  # threshold for visualisation

 # check
 agg_eco_fs[agg_eco_fs$fs=="ESP_DTS_VL2440",]
 
 opportunity_interest_rate  <- 4.0 # ??
 annual_depreciation_rate   <- 2.0 # ??
 agg_eco_fs$CapitalOpportunityCosts    <- an(agg_eco_fs$value_of_physical_capital) * opportunity_interest_rate/100.0 


 extract_df[extract_df$fs=="ESP_DTS_VL2440",]
 
 extract_dt <- data.table(extract_df)
 extract_dt <- extract_dt[,.(GVAimpacted=sum(GVAimpacted, na.rm=TRUE), GrossProfitimpacted=sum(GrossProfitimpacted, na.rm=TRUE)),by=c("sce", "year", "fs")]
 
 extract_dt$year <- an(extract_dt$year)
 agg_eco_fs$year <- an(agg_eco_fs$year)
  
 agg_eco_fs_with_impacted_gva <- merge(extract_dt, agg_eco_fs) 

 # check
 agg_eco_fs_with_impacted_gva[agg_eco_fs_with_impacted_gva$fs=="ESP_DTS_VL1218",]
 
 # anticipated GVA
 agg_eco_fs_with_impacted_gva$anticipated_GVA <- agg_eco_fs_with_impacted_gva$GVA - agg_eco_fs_with_impacted_gva$GVAimpacted
 agg_eco_fs_with_impacted_gva$anticipated_GrossProfit <- agg_eco_fs_with_impacted_gva$GrossProfit - agg_eco_fs_with_impacted_gva$GrossProfitimpacted
 
 # then recompute net profit
 agg_eco_fs_with_impacted_gva$anticipated_OperatingProfit <-  agg_eco_fs_with_impacted_gva$anticipated_GrossProfit - an(agg_eco_fs_with_impacted_gva$cons_of_fixed_capital)
 agg_eco_fs_with_impacted_gva$anticipated_NetProfit       <-  agg_eco_fs_with_impacted_gva$anticipated_OperatingProfit - agg_eco_fs_with_impacted_gva$CapitalOpportunityCosts  - (an(agg_eco_fs_with_impacted_gva$value_of_physical_capital) * (1-((100.0-annual_depreciation_rate)/100.0)))

 # percent change:
 agg_eco_fs_with_impacted_gva$percent_change_GVA  <-  round(((agg_eco_fs_with_impacted_gva$anticipated_GVA/agg_eco_fs_with_impacted_gva$GVA)*100) -100, 2)
 agg_eco_fs_with_impacted_gva$percent_change_GrossProfit  <-  round(((agg_eco_fs_with_impacted_gva$anticipated_GrossProfit/agg_eco_fs_with_impacted_gva$GrossProfit)*100) -100, 2)
 agg_eco_fs_with_impacted_gva$percent_change_NetProfit  <-  round(((agg_eco_fs_with_impacted_gva$anticipated_NetProfit/agg_eco_fs_with_impacted_gva$NetProfit)*100) -100, 2)
 
 # correct sign
 agg_eco_fs_with_impacted_gva[agg_eco_fs_with_impacted_gva$anticipated_NetProfit<0 & agg_eco_fs_with_impacted_gva$anticipated_NetProfit<0,"percent_change_NetProfit"] <- 
                                            agg_eco_fs_with_impacted_gva[agg_eco_fs_with_impacted_gva$anticipated_NetProfit<0 & agg_eco_fs_with_impacted_gva$anticipated_NetProfit<0,"percent_change_NetProfit"] * -1
 
 # check
 agg_eco_fs_with_impacted_gva[agg_eco_fs_with_impacted_gva$fs=="ESP_DTS_VL1824"]
 agg_eco_fs_with_impacted_gva[agg_eco_fs_with_impacted_gva$fs=="FRA_DFN_VL2440"]
 agg_eco_fs_with_impacted_gva[agg_eco_fs_with_impacted_gva$fs=="ESP_DTS_VL2440"]

 

 agg_eco_fs_with_impacted_gva$GVAimpactedMEuro <- round(agg_eco_fs_with_impacted_gva$GVAimpacted/1e6,2) 
 agg_eco_fs_with_impacted_gva$GVAMEuro <- round(agg_eco_fs_with_impacted_gva$GVA/1e6,2) 
 agg_eco_fs_with_impacted_gva$GrossProfitMEuro <- round(agg_eco_fs_with_impacted_gva$GrossProfit/1e6,2) 
 agg_eco_fs_with_impacted_gva$NetProfitMEuro <- round(agg_eco_fs_with_impacted_gva$NetProfit/1e6,2) 
 
 agg_eco_fs_with_impacted_gva <- orderBy(~year + fs + sce, agg_eco_fs_with_impacted_gva)

 agg_eco_fs_with_impacted_gva <- orderBy(~  - year - GVAMEuro 	 + sce, as.data.frame(agg_eco_fs_with_impacted_gva))

 # export
 library(readr)
 dd <- knitr::kable(as.data.frame(agg_eco_fs_with_impacted_gva[, c("year","fs", "sce", "GVAMEuro", "GrossProfitMEuro", "NetProfitMEuro", "GVAimpactedMEuro", "percent_change_GVA", "percent_change_GrossProfit", "percent_change_NetProfit", "engagedCrew")]), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("agg_eco_fs_with_impacted_gva_","2018", "_","2021",".html"))) 
 save(agg_eco_fs_with_impacted_gva, file=file.path(getwd(), a_folder, a_folder2, paste0("agg_eco_fs_with_impacted_gva_","2018", "_","2021",".RData"))) 

 a_subset_of_agg_eco_fs_with_impacted_gva <- agg_eco_fs_with_impacted_gva[an(agg_eco_fs_with_impacted_gva$percent_change_GVA)< -1,]
 a_subset_of_agg_eco_fs_with_impacted_gva <- orderBy(~  - year - GVAMEuro 	 + sce, as.data.frame(a_subset_of_agg_eco_fs_with_impacted_gva))
 dd <- knitr::kable(as.data.frame(a_subset_of_agg_eco_fs_with_impacted_gva[, c("year","fs", "sce", "GVAMEuro", "GrossProfitMEuro", "NetProfitMEuro", "GVAimpactedMEuro", "percent_change_GVA", "percent_change_GrossProfit", "percent_change_NetProfit", "engagedCrew")]), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("agg_eco_fs_with_impacted_gva_","2018", "_","2021_a_subset",".html"))) 
 save(a_subset_of_agg_eco_fs_with_impacted_gva, file=file.path(getwd(), a_folder, a_folder2, paste0("agg_eco_fs_with_impacted_gva_","2018", "_","2021_a_subset",".RData"))) 


 agg_eco_fs_with_impacted_gva <- orderBy(~  - year - GVAMEuro 	 + sce, as.data.frame(agg_eco_fs_with_impacted_gva))
 dd <- knitr::kable(as.data.frame(agg_eco_fs_with_impacted_gva[agg_eco_fs_with_impacted_gva$fs %in% c("ESP_DTS_VL1824", "ESP_DTS_VL2440"), c("year","fs", "sce", "GVAMEuro", "GrossProfitMEuro", "NetProfitMEuro", "GVAimpactedMEuro", "percent_change_GVA", "percent_change_GrossProfit", "percent_change_NetProfit", "engagedCrew")]), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("agg_eco_fs_with_impacted_gva_","2018", "_","2021_MAIN_FLEETS",".html"))) 
 save(agg_eco_fs_with_impacted_gva, file=file.path(getwd(), a_folder, a_folder2, paste0("agg_eco_fs_with_impacted_gva_","2018", "_","2021_MAIN_FLEETS",".RData"))) 
 
 
 
 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 ## GGPLOT 4
 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 
 
 library(stringr)
 temp <- as.data.frame(str_split_fixed(agg_eco_fs_with_impacted_gva$fs,"_",3))  
 agg_eco_fs_with_impacted_gva$country       <- temp[,1]
 agg_eco_fs_with_impacted_gva$fishing_tech  <- temp[,2]
 agg_eco_fs_with_impacted_gva$vessel_size   <- temp[,3]
 
 agg_eco_fs_with_impacted_gva$fs <- factor (agg_eco_fs_with_impacted_gva$fs, levels=fs_to_keep) # re-order in consistence with a_df

 
 p4 <- ggplot(agg_eco_fs_with_impacted_gva[,c("percent_change_NetProfit", "fs", "vessel_size", "sce")], aes(x =  percent_change_NetProfit, y=fs, fill=vessel_size))  + geom_bar(stat = "summary", fun = "mean", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Anticipated change in fleet economy")) + xlab(paste0("Change in ",yrs, " of impacted Net Profit")) + ylab("AER Fleet-segments") +
                                           scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))  
 
  a_width <- 6000 ; a_height <- 4500
  tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY_VMEs", a_folder2, paste0("Anticipated_change_of_net_profit_in ",yrs, ".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p4)
 dev.off()

 # extract data corresponding to the plot
 pg <- ggplot_build(p4)
 pd <- pg$data[[1]]
 colnames(pd)[colnames(pd) %in% "group"] <- "fs"
 pd$fs <- factor(pd$fs) ;  extract_df$fs <- factor(extract_df$fs); levels(pd$fs) <- levels(extract_df$fs)
 colnames(pd)[colnames(pd) %in% "PANEL"] <- "sce"
 pd$sce <- factor(pd$sce) ;  extract_df$sce <- factor(extract_df$sce) ; levels(pd$sce) <- levels(extract_df$sce)
 colnames(pd)[colnames(pd) %in% "fill"]  <- "vessel_size"
 pd$vessel_size <- factor(pd$vessel_size) ;  extract_df$vessel_size <- factor(extract_df$vessel_size); levels(pd$vessel_size) <- levels(extract_df$vessel_size)
 colnames(pd)[colnames(pd) %in% "x"]    <- "Percent"
 pd$Percent <- round(pd$Percent, 2)
 # export
 library(readr)
 dd <- knitr::kable(as.data.frame(pd[, c("fs", "sce", "Percent")]), format = "html")
 readr::write_file(dd, file.path(getwd(), a_folder, a_folder2, paste0("ggplot_data_percent_of_Net_profit_change_","2018", "_","2021",".html"))) 
 save(pd, file=file.path(getwd(), a_folder, a_folder2, paste0("ggplot_data_percent_of_Net_profit_change_","2018", "_","2021",".RData"))) 

 

 
 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 ## GGPLOT 5
 ##-----------------------------------------------------------------------------
 ##-----------------------------------------------------------------------------
 
 
 
 dd <- agg_eco_fs_with_impacted_gva[,c("engagedCrew", "percent_change_GVA", "fs", "vessel_size", "sce")]
 dd <- data.table(dd)
 dd <- dd[,.(engagedCrew=mean(engagedCrew), percent_change_GVA=mean(percent_change_GVA)), by=c("fs", "sce", "vessel_size")]
 
 dd$cat_change_GVA <- cut(dd$percent_change_GVA, breaks=c(-100, seq(-10,10, by=1), 100))
 
 p5 <- ggplot(dd, aes(x =  engagedCrew, y=cat_change_GVA, fill=vessel_size))  + geom_bar(stat = "summary", fun = "sum", position = "dodge") + facet_wrap(~sce, ncol=3)  +
                      ggtitle(paste0("Anticipated change in fleet economy")) + xlab(paste0("Engaged crew number in ",yrs, " impacted by change in % GVA category")) + ylab("% change of GVA categories")  +
                                           scale_fill_manual(values=c(VL0010="#F8766D", VL1012="#B79F00", VL1218="#00BA38", VL1824="#00BFC4", VL2440="#619CFF", VL40XX="#F564E3"))  
  a_width <- 7000 ; a_height <- 3500
       tiff(filename=file.path(getwd(), "OUTCOME_OVERLAY_VMEs", a_folder2, paste0("Impacted_engaged_crew_in ",yrs, ".tif")), width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
     print(p5)
 dev.off()


 # extract data corresponding to the plot
 pg <- ggplot_build(p5)
 pd <- pg$data[[1]]









 
 
 
 
 
 
 
 
 
 
 
 
 
 
 if(FALSE){
 
 
 
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# DISPLACEMENT SCENARIOS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#


## TODO: DISPLACE ACCORDING TO THE EXPECTED PROFITS I:E: LPUE*EFFORT-COSTS?


 # TODO??
 # a buffer around
 effort_poly <- vect(sf::st_buffer(sf::st_as_sf(effort_poly), dist = 200))






   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
    #1. existing MPAs
    # existing MPAs: redistribute in a buffer around each MPA ... with brute force....
    # test a buffer around MPAs...
    a_buffer                   <- terra::buffer(mpas_in_ospar_vect_terra, 30000) # in meter...   
    
    e_sum                <- extract(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert, mpas_in_ospar_vect_terra, sum, na.rm=TRUE)
    #e_sum_buffer        <- extract(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert, a_buffer,  sum, na.rm=TRUE)
    e_idx_cells          <- extract(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert, mpas_in_ospar_vect_terra, cells=TRUE)
    e_idx_cells_buffer   <- extract(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert, a_buffer, cells=TRUE)
    e_idx_cells_at_sea   <- extract(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert, msfd_atlantic_terra_lambert, cells=TRUE)
    nb_idx_cells         <- ncell(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert)
    nrow(e_idx_cells_at_sea)
    print(nb_idx_cells)
    
    
    rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced <- rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert # init  
    rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced[is.na(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced)] <- 0
    
    # brute force...take a while....
    idx_cells_land    <- e_idx_cells_land[, "cell"] 
     for(i_mpa in unique(mpas_in_ospar_vect_terra$FID)){
       idx_cells_donors    <- e_idx_cells[e_idx_cells$ID==i_mpa, "cell"] 
       idx_cells_receivers <- e_idx_cells_buffer[e_idx_cells_buffer$ID==i_mpa, "cell"] 
       idx_cells_receivers <- idx_cells_receivers[! idx_cells_receivers %in% idx_cells_donors] # not on the donors... 
       idx_cells_receivers <- idx_cells_receivers[idx_cells_receivers %in% e_idx_cells_at_sea[,"cell"] ] # not on land cells...
      
       if(length(idx_cells_donors)!=0 && length(idx_cells_receivers)!=0){
          print(i_mpa)
          # receivers
          rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced[idx_cells_receivers] <- 
               (rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced[idx_cells_receivers]  + 
                (( e_sum[e_sum$ID==i_mpa,"mean"] )/ length(idx_cells_receivers))) [,1] 
          # donors
             rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced[idx_cells_donors] <- 0  
     
      }
    }    
    # export
    path <- file.path(getwd(),"GIS","VMS_and_ICES_data","OSPAR_2021") 
    if (FALSE) writeRaster(rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced,  file.path(path,"rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced_on_buffer30km.tiff"), overwrite=TRUE)
    
    # re-load
    rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced <- rast(file.path(path,"rast_terra_subsurface_swept_area_OSPAR_2018_2020_lambert_displaced_on_buffer30km.tiff"))
 

}

