# Goals

Area-based management and associated spatial restrictions of marine space for fishing are likely to result in an effort displacement. Other uses of marine space will also induce some exclusion to fishing (e.g., the growing renewable energy sector). To document possible displacement and consequences on harvested living stocks and marine habitats, the present study initiated a tool to assist fisheries researchers and experts in short-term anticipation of possible effort displacement alongside alternative options for spatial management. This work aims to predict the effect of changes in ‘fishable’ areas on the socioeconomic of fisheries, at least on the short-term horizon, given that no prediction on the underlying fished stock trajectories is made. The fishable area is defined as the marine space left for fishing but also the space suitable for fishing given the physical constraints of the marine environment.

>[!IMPORTANT]
> The original data input to this workflow are not included here for confidentiality reason. Example input datasets are given instead. These datasets are very limited in purpose   

The study has merged several datasets to conduct an economic impact evaluation of the proposals for fishing restrictions at the fleet-segmentation level defined by the EU STECF AER dataset. The study applied this segmentation specific to the EU fleet and split the evaluation into two parts:
·         An evaluation of the available fishable areas and the impacted EU fleet segments in terms of GVA, gross and net profits, and the crew engaged in the impacted segments. This also disaggregates the possible socioeconomic impact of each restriction alongside the different scenarios in defining those restrictions.
·         An evaluation of the possibility for compensation and economic implications by displacing the fishing effort toward surrounding areas or other fishing grounds. In such effort displacement, the main driver was assumed to be the economic return the vessel operators may expect from the still-open fishing grounds.  

The present spatial tool can apply to the entire EU fleet or a regional subset (e.g. Baltic Sea, Celtic Seas, Bay of Biscay, North Sea, West and East Med). Steps for a socioeconomic analysis of the impacted fleets by the closed areas include:
* Merging EU STECF AER and VMS datasets
* Estimation of a change in fishable area from the overlay effect of restricted areas proposals estimated with the coupling of AER to VMS
* Displacement effect based on coupled AER-VMS datasets

# Displacement effects

Different scenarios on the possible effort allocation are proposed and analysed, with an effort reallocation differentiated between fishing gears. In case the effort displaced may not compensate for the loss, the minimum effort level required to break even is calculated. The limits and likelihoods of scenarios and the assumptions behind them are further discussed hereafter. It is, however, already recalled here that the “fishable area” is defined as the marine space left for fishing but also the space suitable for fishing given the physical constraints of the marine environment. The present study assumes that the already fished area defines the suitable environment for fishing and did not investigate further if the habitat extent suitable for fishing might possibly change from factors external to fishing (e.g., induced by climate change). With this laminar assumption, the fraction of the historical effort impacted by the proposals for the closed areas can be displaced to the surrounding areas that have already been visited by the fishing fleet in the past.

The study evaluates the possible change in catches and then in the economic return that such a displacement could induce. However, the method used (i.e. GIS raster layers) for the displacement effect study prevents distinguishing individual polygons' effect. It is therefore assumed that the effect results from implementing them all (alongside fleet segment specifications depending on the scenario).

The study investigated two ways for a hypothetical redistribution of the fishing effort in reaction to the closed areas:
* A uniform (i.e. profit-free) redistribution over areas of the impacted effort toward areas already visited by the fleet segment. In practice, the total impacted effort by the closed areas of a given scenario is evenly re-distributed over all the c-squares visited by the fleet segment during the period 2018-2021.
* A weighted redistribution of the impacted effort alongside the historical c-square GVAs, where more (i.e. on a log scale) effort is displaced toward historically high GVAs recorded for the fleet segment during the period 2018-2021 studied. It should be noted that to avoid bias in case the fleet segment is not used to optimise on expected economic return, this weighted redistribution is not compared to the historical one but to a recalculated baseline (i.e. a comparable counterfactual) that accounts for optimal redistribution of the same amount of the impacted effort alongside spatial GVAs, closed areas included.
After the redistribution occurs, the catches that were historically recorded inside the closed areas are cancelled, and new catches outside the closed areas are computed, accounting for the extra effort added to the c-squares and their specific LPUEs. The economic variables, including the GVA, are also recomputed based on the new catches and spatialised costs.

>[!WARNING]
> The study provides a workflow for a STATIC evaluation and could not replace a more elaborated dynamic approach. Bioeconomic models are best suited to evaluate the effect of spatial displacement of fishing efforts. See for example the [DISPLACE software](https://github.com/frabas/DISPLACE_GUI)  



