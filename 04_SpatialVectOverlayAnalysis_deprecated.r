# Author: Francois Bastardie (DTU-Aqua), June 2023

##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!OVERLAY AND EXTRACT PER FISHING ACTIVITIES!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

    # read-in a AER layer
    library(terra)
    filepath      <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER", "all_metiers", "2018_2021")
    aer_layers    <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content
    plot(log(aer_layers))
    sum(dd$FishingHour[], na.rm=TRUE)
    sum(dd$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)


    #  read-in a VMS layer
    library(terra)
    filepath      <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS", "all_metiers", "2018_2021")
    vms_layer     <- rast(file.path(filepath, "spatRaster.tif")) # always named as spatRaster.tif... the folder´s name describes the content
    plot(log(vms_layer))
    sum(dd$FishingHour[], na.rm=TRUE)

    # Do some extract on the AER layer
    aer_layer_eea_terra <- project(aer_layers, crs(mpas_3035_msfd_vect_terra_region), method="bilinear")
    sum(aer_layer_eea_terra$FishingHour[], na.rm=TRUE)
    sum(aer_layer_eea_terra$landings_aer_in_ctry_level6_csquare[], na.rm=TRUE)

    ## BUT CAUTION HERE: THE TERRA REPROJECTION IS CREATING EFFORT BECAUSE APPLYING A SPATIAL INTERPOLATION....


    area_cell           <- prod(res(aer_layer_eea_terra)) # m^2 as the resolution of the raster is in meter
    e_sum               <- extract(aer_layer_eea_terra, mpas_3035_msfd_vect_terra_region, sum, na.rm=TRUE)
    e_sum[,2]           <-  e_sum[,2] / 1e3 # to thousand
    colnames(e_sum)     <- c("ID", "(thousand)")

    # a check
    sum(e_sum[,2]*1000)
    sum(aer_layer_eea_terra$FishingHour[], na.rm=TRUE)
    sum(vms_layer[], na.rm=TRUE)


    # Do some extract on the VMS layer
    vms_layer_eea_terra <- project(vms_layer$FishingHour, crs(mpas_3035_msfd_vect_terra_region))
    area_cell           <- prod(res(vms_layer_eea_terra)) # m^2 as the resolution of the raster is in meter
    e_sum2               <- extract(vms_layer_eea_terra, mpas_3035_msfd_vect_terra_region, sum, na.rm=TRUE)
    e_sum2[,2]           <-  e_sum2[,2] / 1e3 # to thousand
    colnames(e_sum2)     <- c("ID", "(thousand)")

    sum(e_sum2[,2]*1000)
    sum(vms_layer_eea_terra[], na.rm=TRUE)


   # check with a plot and export
   library(sf)
   library(terra)
    pol_lambert <- vect(aer_layer_eea_terra)
    g <- st_graticule(st_as_sf(pol_lambert))
    # check before log-transforming
    range((log(aer_layer_eea_terra)))
    b <- (seq(8, 12, length=10))
    leg_names <- paste0(round(exp(b)[]), "-", round(exp(b)[-1]-1)) ; leg_names[length(leg_names)] <- paste0(">",round(exp(12))) # legend names in natural scale
    library(viridis)
    a_width <- 5000 ; a_height <- 5000
    tiff(filename=file.path(a_path, "OUTCOME_FISHERIES_DISTR_VMS_AER", paste0("AER_and_MPAs.tiff")),   width = a_width, height = a_height,
                                   units = "px", pointsize = 12,  res=600, compression = c("lzw"))
    plot(log(aer_layer_eea_terra), breaks=b, col=rev(viridis(length(b))),
               mar=c(5,5,5,10),
                axes=FALSE,
               main = list("AER layer"),
                   plg=list(  # parameters for drawing legend
                    legend=leg_names[-1], cex=1,
                    title = expression("thousands")
                     ),
                pax=list( # parameters for drawing axes
                cex.axis = 1.5, las=2 # Axis text size
                ),
            cex.main = 2 # Title text size
            )
   plot(st_as_sf(mpas_3035_msfd_vect_terra_region), col=rgb(1,0,0,0.2), add=TRUE)  ## Adding transparency is costly in computer time....

  # add graticule
  plot(st_geometry(g), add = TRUE, col=grey(0.5))
  op <- par(xpd = NA)
  invisible(lapply(seq_len(nrow(g)), function(i) {
    if (g$type[i] == "N" && g$x_start[i] - min(g$x_start) < 1000)
    text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
        srt = g$angle_start[i], pos = 2, cex = .7)
    if (g$type[i] == "E" && g$y_start[i] - min(g$y_start) < 1000)
    text(g[i,"x_start"], g[i,"y_start"], labels = parse(text = g[i,"degree_label"]),
        srt = g$angle_start[i] - 90, pos = 1, cex = .7)
    if (g$type[i] == "N" && g$x_end[i] - max(g$x_end) > -1000)
    text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
        srt = g$angle_end[i], pos = 4, cex = .7)
    if (g$type[i] == "E" && g$y_end[i] - max(g$y_end) > -1000)
    text(g[i,"x_end"], g[i,"y_end"], labels = parse(text = g[i,"degree_label"]),
        srt = g$angle_end[i] - 90, pos = 3, cex = .7)
   }))
 dev.off()


  # for fun:
  b <- (seq(8, 12, length=10))
  leg_names <- paste0(round(exp(b)[]), "-", round(exp(b)[-1]-1)) ; leg_names[length(leg_names)] <- paste0(">",round(exp(12))) # legend names in natural scale
  library(viridis)
  library(mapview)
  library(sf)
  mapview(log(raster(aer_layer_eea_terra)), breaks=b, col=rev(viridis(length(b)))) +  mapview(st_as_sf(mpas_3035_msfd_vect_terra_region), col.regions=rgb(1,0,0,1))


















##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# A quick VECTOR spatial overlay!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#



# utils
what_is_inside <- function(filepath, allclosures_sf)
{

 library(terra)
 library(raster)
 bb             <- st_bbox(allclosures_sf)

  er <- try(   {
     rstr                 <- terra::rast(file.path(filepath, "spatRaster.tif")) # in "+proj=longlat +datum=WGS84"
  }, silent=TRUE)
 if(class(er)!="try-error"){


 newcrs               <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs" # European EEA projection
 rstr_eea             <- project(rstr, newcrs)


 # crop in a bbox define by the closure shp
 cr           <- as(extent(bb), 'SpatialPolygons')
 crs(cr)      <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
 cr           <- spTransform(cr, crs(rstr_eea))
 rstr_eea_sub <- crop(rstr_eea, cr)

 library(viridis)
 plot(rstr_eea_sub)
 plot(rstr_eea_sub, col=viridis(50))
 plot(log(rstr_eea_sub), col=viridis(50))

 # reproject closures
 allclosures_sf_eea <- st_transform(allclosures_sf, crs(rstr_eea))
 plot(rstr_eea_sub$landings_aer_in_ctry_level6_csquare, title="Landings kg")
 plot(allclosures_sf_eea, add=TRUE)

 # do an extract (use the centroid...it is acceptable given the closed boxes are much smaller than the FDI c-squares)
 #coord                   <- crds(rstr_eea_sub, na.rm=FALSE)
 #coords      <- st_coordinates(st_centroid(allclosures_sf_eea$geometry))
 #id.cells    <- extract(rstr_eea_sub, SpatialPoints(coords), cellnumbers=TRUE)

 # extract what is inside...
 allclosures_vect_terra  <- vect(allclosures_sf_eea)
 col_names               <- c("daysatsea_aer_in_ctry_level6_csquare","landings_aer_in_ctry_level6_csquare","value_aer_in_ctry_level6_csquare",
                                "varcosts_in_ctry_level6_csquare","other_income_in_csquare","unpaid_labour_in_csquare","KwFishingdays_aer_in_ctry_level6_csquare", "lpue_csquare_fdi_kgperfday")
# extract
 e_mpas              <- extract(subset(rstr_eea_sub, col_names), allclosures_vect_terra, cells=TRUE,  na.rm=TRUE) # extract the raster cell numbers from each polygon pts for all polygons
 e_mpas              <- e_mpas[!duplicated(e_mpas [,c("ID", "cell")]),]  # avoid double counting because polygons are by def having several pts...
 e_mpas              <- data.table(e_mpas)
# then aggregate per polygon ID if a polygon crosses several raster cells
 e_mpas_1              <- e_mpas[,lapply(.SD, sum, na.rm=TRUE),
                                   .SDcols=c("daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare", "varcosts_in_ctry_level6_csquare",
                                          "other_income_in_csquare" ,"unpaid_labour_in_csquare",
                                     "KwFishingdays_aer_in_ctry_level6_csquare"), by="ID"]
 e_mpas_1               <- as.data.frame(e_mpas_1)
 e_mpas_2              <- e_mpas[,lapply(.SD, sum, na.rm=TRUE),
                                   .SDcols=c("lpue_csquare_fdi_kgperfday"), by="ID"]
 e_mpas_2               <- as.data.frame(e_mpas_2)
 e_mpas                    <- cbind.data.frame(what="inside_this_poly", e_mpas_1, e_mpas_2[,c(-1), drop=FALSE])
 ## => export to TABULATE later!!!


 # what about outside? which outside to get a percentage?
 #=> need to do the analysis per fs to compare with total per fs....
 library(data.table)
 a_dt <- data.table(as.data.frame(rstr_eea))
 ## aggregate per grID
  a_dt_1 <-
     a_dt[,lapply(.SD, sum, na.rm=TRUE),
      .SDcols=c("daysatsea_aer_in_ctry_level6_csquare", "landings_aer_in_ctry_level6_csquare", "value_aer_in_ctry_level6_csquare", "varcosts_in_ctry_level6_csquare",
          "other_income_in_csquare" ,"unpaid_labour_in_csquare",
           "KwFishingdays_aer_in_ctry_level6_csquare")]
  a_dt_2 <-
     a_dt[,lapply(.SD, mean, na.rm=TRUE),
      .SDcols=c("lpue_csquare_fdi_kgperfday")]
  a_dt   <- cbind(a_dt_1, a_dt_2)
  a_df   <- cbind.data.frame(what="all layer cells", ID=0, data.frame(a_dt))
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
 allclosures_sf <- st_read(file.path("D:","FBA","ADVICES","STECF","STECF_ad_hoc_2023_VMEs","Contract n2","fba_closure_VMEs_2022.shp"))  # DGMARE-STECF adhoc
 allclosures_sf <- mpas_3035  # SeaWise

 ## ALL METIERS----------
 # read-in the merged disagregated AER variables in shapfiles
 output <- NULL
 for (y in years){
 filepath                  <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER", "all_metiers", y)
 e_mpas_all_metiers        <- what_is_inside (filepath, allclosures_sf)
 e_mpas_all_metiers$GVA    <-  (an(e_mpas_all_metiers$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                                 (an(e_mpas_all_metiers$value_aer_in_ctry_level6_csquare)/an(e_mpas_all_metiers$landings_aer_in_ctry_level6_csquare))) +
                                 an(e_mpas_all_metiers$other_income_in_csquare) - # plus other income
                                 an(e_mpas_all_metiers$unpaid_labour_in_csquare) - an(e_mpas_all_metiers$varcosts_in_ctry_level6_csquare)  # minus var costs
 library(readr)
 dd <- knitr::kable(as.data.frame(e_mpas_all_metiers), format = "html")
 readr::write_file(dd, file.path(filepath, paste0("e_mpas_all_metiers_",y,".html")))
 # order boxes to find out the most impacting polygons among them all (i.e. this gives the prop of GVA impacted by the box compared to the summed GVA in all those boxes)
 prop <- sweep(as.matrix(e_mpas_all_metiers[e_mpas_all_metiers$what!="all layer cells",-c(1:2)]) , 2,
          apply(as.matrix(e_mpas_all_metiers[e_mpas_all_metiers$what!="all layer cells",-c(1:2)]), 2, sum),
          FUN="/" )
 apply(prop, 2, sum) # check: should return 1s
 library(doBy)
 prop <- cbind(prop, ID=1:nrow(prop))
 impacts <- round(orderBy(~ -GVA, prop) [,c("ID", "GVA")], 3)
 # sum over polygons the year GVA made inside (only account for the polygons with positive GVA...)
 sum_inside      <- sum(e_mpas_all_metiers[e_mpas_all_metiers$what!="all layer cells" & e_mpas_all_metiers$GVA>0 , "GVA"])
 sum_all         <- sum(e_mpas_all_metiers[e_mpas_all_metiers$what=="all layer cells" , "GVA"])
 prop_inside     <- sum_inside/ sum_all
 output <- rbind.data.frame(output, cbind.data.frame(y, sum_inside, sum_all, prop_inside, most_impacting_id=impacts[1,1], prop_impact=impacts[1,2]))
 print(output)
 }  # end y

 # read-in the merged disagregated AER variables in shapfiles 2019_2021
 output <- NULL
 # 2019_2021
 filepath                  <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER", "all_metiers", "2019_2021")
 e_mpas_all_metiers        <- what_is_inside (filepath, allclosures_sf)
 e_mpas_all_metiers$GVA    <-  (an(e_mpas_all_metiers$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                                 (an(e_mpas_all_metiers$value_aer_in_ctry_level6_csquare)/an(e_mpas_all_metiers$landings_aer_in_ctry_level6_csquare))) +
                                 an(e_mpas_all_metiers$other_income_in_csquare) - # plus other income
                                 an(e_mpas_all_metiers$unpaid_labour_in_csquare) - an(e_mpas_all_metiers$varcosts_in_ctry_level6_csquare)  # minus var costs
 library(readr)
 dd <- knitr::kable(as.data.frame(e_mpas_all_metiers), format = "html")
 readr::write_file(dd, file.path(filepath, "e_mpas_all_metiers_2019_2021.html"))
 # sum over polygons the year GVA made inside (only account for the polygons with positive GVA...)
 sum_inside      <- sum(e_mpas_all_metiers[e_mpas_all_metiers$what!="all layer cells" & e_mpas_all_metiers$GVA>0 , "GVA"])
 sum_all         <- sum(e_mpas_all_metiers[e_mpas_all_metiers$what=="all layer cells" , "GVA"])
 prop_inside     <- sum_inside/ sum_all
 output <- rbind.data.frame(output, cbind.data.frame("2019_2021", sum_inside, sum_all, prop_inside))
 print(output)

 ## PER METIER----------
 # read-in the merged disagregated AER variables in shapfiles
 output <- NULL
 for(y in years){
  for (fs in unlist(unique(distr_allsp[,"fs"])))
    {
    filepath             <- file.path(getwd(), "OUTCOME_FISHERIES_DISTR_VMS_AER", fs, y)
    e_mpas_all_metiers <- what_is_inside (filepath, allclosures_sf)
    #if(sum(e_mpas_all_metiers[e_mpas_all_metiers$what!="all layer cells" , "daysatsea_aer_in_ctry_level6_csquare"])<1){
    if(!is.null(e_mpas_all_metiers)){
       e_mpas_all_metiers$GVA <-  (an(e_mpas_all_metiers$landings_aer_in_ctry_level6_csquare) *  # landing kg  * price
                      (an(e_mpas_all_metiers$value_aer_in_ctry_level6_csquare)/an(e_mpas_all_metiers$landings_aer_in_ctry_level6_csquare))) +
                      an(e_mpas_all_metiers$other_income_in_csquare) - # plus other income
                      an(e_mpas_all_metiers$unpaid_labour_in_csquare) - an(e_mpas_all_metiers$varcosts_in_ctry_level6_csquare)  # minus var costs

       dd <- knitr::kable(as.data.frame(e_mpas_all_metiers), format = "html")
       library(readr)
       readr::write_file(dd, file.path(filepath, paste0("e_mpas_",fs,"_",y,".html")))

       # order boxes to find out the most impacting polygons among them all (i.e. this gives the prop of GVA impacted by the box compared to the summed GVA in all those boxes)
       dd <- as.matrix(e_mpas_all_metiers[e_mpas_all_metiers$what!="all layer cells" & !is.na(e_mpas_all_metiers$GVA)  & e_mpas_all_metiers$GVA>0, -c(1:2)])
       prop <- sweep(dd , 2, apply(dd, 2, sum, na.rm=TRUE), FUN="/")
       apply(prop, 2, sum) # check: should return 1s
       library(doBy)
       if(nrow(prop)>0){
       prop <- cbind(prop, ID=as.numeric(rownames(prop)))
       impacts <- round(orderBy(~ -GVA, prop)[, c("ID", "GVA"), drop=FALSE],3)
       } else{
       impacts <- matrix(0,ncol=2)
       }
       # sum over polygons the 3y average GVA made inside (only account for the polygons with positive GVA...)
       sum_inside      <- sum(e_mpas_all_metiers[e_mpas_all_metiers$what!="all layer cells" & e_mpas_all_metiers$GVA>0 , "GVA"], na.rm=TRUE)
       sum_all         <- sum(e_mpas_all_metiers[e_mpas_all_metiers$what=="all layer cells" & e_mpas_all_metiers$GVA>0 , "GVA"], na.rm=TRUE)
       prop_inside     <- round(sum_inside/ sum_all,3)
       output <- rbind.data.frame(output, cbind.data.frame(y, fs, sum_inside, sum_all, prop_inside, most_impacting_id=impacts[1,1], prop_impact=impacts[1,2]))
       print(output)
      } # end
    } # end fs
   } # end y

 # a check
 head(output)
 sum(output$sum_inside)
 sum(output$sum_all)

 # general outcome
 #( caution: if assuming the entire Csquare surface that comprise a polygon is impacted...alternatively we could account for a proportion per csquare of >400m time proportion surface area of the polygon/caquare)
 # what to do with start negative GVA?
 filepath             <- file.path(getwd(), "OUTCOME_OVERLAY")
 dir.create(filepath, recursive=TRUE)
 library(readr)
 dd <- knitr::kable(as.data.frame(output), format = "html")
 readr::write_file(dd, file.path(filepath, "prop_inside_output_2019_2021.html"))

 save(output, file=file.path(filepath, "output_prop_inside_outside_per_fs.RData"))




##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
 # do a ggplot barplot out of this..........

 library(ggplot2)

 load(file=file.path(filepath, "output_prop_inside_outside_per_fs.RData"))

 a_df <- as.data.frame(output)

 a_df <-a_df[a_df$sum_all>0,]

 a_df <-a_df[a_df$prop_inside>0.1,]  # threshold for visualisation

 a_df <- orderBy(~ - prop_inside, a_df)

 a_df$fs <- factor (a_df$fs, levels=unique(a_df$fs)) # re-order

 ggplot(a_df, aes(x = prop_inside, y=fs)) + geom_col()  + facet_grid(~y)







