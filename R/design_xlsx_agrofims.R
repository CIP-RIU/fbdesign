#' Create metadata for hidap-agrofims
#' @export

add_metadata_agrofims <- function(agronomic_crop_template, col_name= "Value",
                         experimentId="",
                         experimentName="",
                         experimentProjectName="",
                         startDate = "startDate",
                         endDate = "endDate",
                         Duration ="",
                         typeExperiment="",
                         experimentObj="",

                         fundAgenType = "", #NEW
                         fundName="",
                         contCenter="",
                         contCRP="",
                         contResearcher="",

                         fundLeadAgency="",
                         leadName="",


                         npersonnel= 1, #number of personnel or researchers
                         personnel1Type="",
                         person1FirstName="",
                         person1LastName="",
                         person1Email="",
                         person1Afiliation="",
                         person1ORCID="",

                         personnel2Type="",
                         person2FirstName="",
                         person2LastName="",
                         person2Email="",
                         person2Afiliation="",
                         person2ORCID="",

                         personnel3Type="",
                         person3FirstName="",
                         person3LastName="",
                         person3Email="",
                         person3Afiliation="",
                         person3ORCID="",

                         personnel4Type="",
                         person4FirstName="",
                         person4LastName="",
                         person4Email="",
                         person4Afiliation="",
                         person4ORCID="",

                         personnel5Type="",
                         person5FirstName="",
                         person5LastName="",
                         person5Email="",
                         person5Afiliation="",
                         person5ORCID="",



                         #instAbreviation="", #deprecated

                         sytpetype ="",
                         syteName ="",
                         siteID="",
                         countryName="",
                         #admin1="",
                         #admin2="",
                         villageName="",
                         nearestPopupPlace = "", #Nearest populated place NEW
                         #elevation ="",
                         #latitude="",
                         #longitude="",
                         inHighLevel= "",
                         inSiteVegetation="",

                         inSiteDescNotes="",

                         croppingType= "",
                         cropCommonNameMono="",
                         cropLatinNameMono="",
                         cropVarietyNameMono="",
                         cultivarNameMono="",
                         monoCropLocalName="",

                         numPreviousCrop= "",
                         prevCropName = "",
                         prevCropVar = "",


                         subject="",
                         keywords="",
                         Embargo_date=""){

  Metadata <- agronomic_crop_template
  #ToDo: cambiar en el template por minusculas las letras siguientes
  names(Metadata) <- c ("Variable", "Value")

  npersonnel <- as.numeric(npersonnel)

  startDate <- startDate
  endDate <- endDate
  Duration <- Duration

  path <- fbglobal::get_base_dir()
  #print(path)
  geodb_file <- "table_sites_agrofims.rds"
  #path <- paste(path, geodb_file, sep = "\\")
  path <- file.path(path, geodb_file)
  sites_data <-  readRDS(file = path)

  geodata <- fbsites::filter_geodata_agrofims(sites_data = sites_data,
                                     country_input = countryName,
                                     trial_site_abbr = villageName)



  if(is.null( typeExperiment)){ typeExperiment <-""}

  if(is.null( fundAgenType )){ fundAgenType  <-""}

  if(is.null( contCenter )){ contCenter  <-""}

  if(is.null( contCRP )){ contCRP  <-""}

  if(is.null( fundLeadAgency )){fundLeadAgency  <-""}

  if(is.null( personnel1Type )){personnel1Type  <-""}
  if(is.null( personnel2Type )){personnel2Type  <-""}
  if(is.null( personnel3Type )){personnel3Type  <-""}
  if(is.null( personnel4Type )){personnel4Type  <-""}
  if(is.null( personnel5Type )){personnel5Type  <-""}

  if(is.null( cropCommonNameMono)){cropCommonNameMono  <- ""}

  if(is.null(inHighLevel)){inHighLevel <- ""}
  if(length(inHighLevel)>1){inHighLevel <- paste(inHighLevel, collapse = ",") } #multi select crop

  if(is.null(inSiteVegetation)){inSiteVegetation <- ""}
  if(length(inSiteVegetation)>1){inSiteVegetation <- paste(inSiteVegetation, collapse = ",") } #multi select crop

  if(is.null(cropVarietyNameMono)){cropVarietyNameMono <- ""}
  if(length(cropVarietyNameMono)>1){cropVarietyNameMono <- paste(cropVarietyNameMono, collapse = ",")}


  print(geodata)

  ###Experimental details tab ###
  Metadata[Metadata$Variable== 'Experiment ID', col_name]	<-	paste("fbagrofims_", sample(1:10000, 1), sep = "")
  Metadata[Metadata$Variable== 'Experiment name', col_name]	<-	paste(experimentName)
  Metadata[Metadata$Variable== 'Experiment project name', col_name]	<-	paste(experimentProjectName)
  Metadata[Metadata$Variable== 'Experiment start date', col_name]	<-	paste(startDate)
  Metadata[Metadata$Variable== 'Experiment end date', col_name]	<-	paste(endDate)
  Metadata[Metadata$Variable== 'Experiment duration', col_name]	<-	Duration
  Metadata[Metadata$Variable== 'Type of experiment', col_name]	<-	paste(typeExperiment)
  Metadata[Metadata$Variable== 'Experiment objective', col_name]	<-	paste(experimentObj)

  Metadata[Metadata$Variable== 'Funding agency type', col_name]	<-	paste(fundAgenType) #NEW
  Metadata[Metadata$Variable== 'Funding agency name', col_name]	<-	paste(fundName)
  Metadata[Metadata$Variable== 'Contribuitor center', col_name]	<-	paste(contCenter)
  Metadata[Metadata$Variable== 'Contribuitor CRP', col_name]	<-	paste(contCRP)
  Metadata[Metadata$Variable== 'Contribuitor researcher', col_name]	<-	paste(contResearcher)

  Metadata[Metadata$Variable== 'Experiment, lead organization type', col_name]	<-	paste(fundLeadAgency)
  Metadata[Metadata$Variable== 'Experiment, lead organization name', col_name]	<-	paste(leadName)


  ###Personnel Tab ###

  if(npersonnel >= 1){
    Metadata[Metadata$Variable== 'Person type', col_name]	<-	paste(personnel1Type)
    Metadata[Metadata$Variable== 'Person, first name', col_name]	<-	paste(person1FirstName)
    Metadata[Metadata$Variable== 'Person, last name', col_name]	<-	paste(person1LastName)
    Metadata[Metadata$Variable== 'Person, email', col_name]	<-	paste(person1Email)
    Metadata[Metadata$Variable== 'Person, affiliation', col_name]	<-	paste(person1Afiliation)
    Metadata[Metadata$Variable== 'Person, ORCID', col_name]	<-	paste(person1ORCID)
  }

    Metadata <- Metadata

  #Site Tab
  Metadata[Metadata$Variable== 'Site type', col_name]	<-	paste(as.character(geodata$Type))#""
  Metadata[Metadata$Variable== 'Site name', col_name]	<-	paste(as.character(geodata$local))#""
  Metadata[Metadata$Variable== 'Site ID', col_name]	<-	paste(as.character(geodata$id))
  Metadata[Metadata$Variable== 'Country name', col_name]	<-	paste(countryName)
  Metadata[Metadata$Variable== 'Site, first-level administrative division name', col_name]	<-	paste(as.character(geodata$adm1))
  Metadata[Metadata$Variable== 'Site, second-level administrative division name', col_name]	<-	paste(as.character(geodata$adm2))
  Metadata[Metadata$Variable== 'Village name', col_name]	<-	paste(villageName)
  Metadata[Metadata$Variable== 'Nearest populated place', col_name]	<-	paste(as.character(geodata$nearpop))
  Metadata[Metadata$Variable== 'Site elevation', col_name]	<-	paste(as.character(geodata$elev))
  Metadata[Metadata$Variable== 'Site latitude (in decimal degrees)', col_name]	<-	paste(as.character(geodata$latd))
  Metadata[Metadata$Variable== 'Site longitude (in decimal degrees)', col_name]	<-	paste(as.character(geodata$lond))


  Metadata[Metadata$Variable== 'Higher-level landform', col_name]	<-	paste(inHighLevel) #""  #NEW
  Metadata[Metadata$Variable== 'Vegetation surrounding the experimental site', col_name]	<- paste(inSiteVegetation)	#"" #NEW
  Metadata[Metadata$Variable== 'Site description notes', col_name]	<-	paste(inSiteDescNotes) #"" #NEW

  # Crop Tab
  Metadata[Metadata$Variable== 'Cropping type', col_name]	<-	paste(croppingType)
  Metadata[Metadata$Variable== 'Crop common name', col_name]	<-	paste(cropCommonNameMono)
  Metadata[Metadata$Variable== 'Crop latin name', col_name]	<-	paste(cropLatinNameMono)
  Metadata[Metadata$Variable== 'Crop variety name', col_name]	<-	paste(cropVarietyNameMono)
  Metadata[Metadata$Variable== 'Cultivar name', col_name]	<-	paste(cultivarNameMono)
  Metadata[Metadata$Variable== 'Crop local name', col_name]	<-	paste(monoCropLocalName) #NEW

  Metadata[Metadata$Variable== 'Number of previous crop', col_name]	<-	paste(numPreviousCrop) #NEW
  Metadata[Metadata$Variable== 'Previous crop name', col_name]	<-	paste(prevCropName) #NEW
  Metadata[Metadata$Variable== 'Previous crop variety', col_name]	<-	paste(prevCropVar) #NEW


  ## Open access inputs
  Metadata[Metadata$Variable== 'Subject', col_name]	<-	""
  Metadata[Metadata$Variable== 'Keywords', col_name]	<-	""
  Metadata[Metadata$Variable== 'Embargo end date', col_name]	<-	"" #paste(Embargo_date)

  if(npersonnel >= 2 ){




    metapersonnel2 <- data.frame(Variable = c('Person 2 type',  'Person 2, first name', 'Person 2, last name',
                                              'Person 2, email', 'Person 2, affiliation', 'Person 2, ORCID'),
                                 Value  =  c(personnel2Type, 	person2FirstName, person2LastName,  person2Email ,
                                             person2Afiliation , person2ORCID))
    print("metaperson2")
    print(metapersonnel2)

    Metadata <- rbind(Metadata,metapersonnel2 )
    Metadata <- Metadata[c(c(1:15), c(16:21), c(48:53), c(22:47)),   ]


   # print(Metadata)

  }

  if(npersonnel >= 3){


    metapersonnel3 <-   data.frame(Variable = c('Person 3 type',  'Person 3, first name', 'Person 3, last name',
                                                'Person 3, email', 'Person 3, affiliation', 'Person 3, ORCID'),
                                   Value  =  c(personnel3Type, 	person3FirstName, person3LastName,  person3Email ,
                                               person3Afiliation , person3ORCID))

    #Metadata <- rbind(Metadata, metapersonnel3)
    Metadata <- rbind(Metadata, metapersonnel3)


  }

  if(npersonnel >= 4){


    metapersonnel4 <-   data.frame(Variable = c('Person 4 type',  'Person 4, first name', 'Person 4, last name',
                                                'Person 4, email', 'Person 4, affiliation', 'Person 4, ORCID'),
                                   Value  =  c(personnel4Type, 	person4FirstName, person4LastName,  person4Email,
                                               person4Afiliation , person4ORCID))

    #Metadata <- rbind(Metadata, metapersonnel4)
    Metadata <- rbind(Metadata, metapersonnel4)


  }

  if(npersonnel >= 5){


    metapersonnel5 <-   data.frame(Variable = c('Person 5 type',  'Person 5, first name', 'Person 5, last name',
                                                'Person 5, email', 'Person 5, affiliation', 'Person 5, ORCID'),
                                   Value  =   c(personnel5Type, 	person5FirstName, person5LastName,  person5Email ,
                                                person5Afiliation , person5ORCID))

    #Metadata <- rbind(Metadata, metapersonnel5)
    Metadata <- rbind(Metadata, metapersonnel5)

  }


  Metadata

}
#' Create installation for hidap-agrofims
#' @export

add_installation_agrofims <- function(agronomic_crop_template, col_name = "Value",
                             crop = "Potato",
                             designFieldbook_agrofims="",  #all crops
                             designFieldbook_agrofims_r="", #all crops


                             numPlantsPerPlot="", #potato cassava sweetpotato
                             numRowsPerPlot="", #potato cassava sweetpotato
                             numPlantsPerRow="", #potato cassava sweetpotato
                             plotSize="", #potato cassava sweetpotato
                             distancebwPlants = "",
                             distanceBwRows = "",
                             spaceBwPlants="",#potato cassava sweetpotato
                             spaceBwRows="",#potato cassava sweetpotato
                             planDensity="", #potato cassava sweetpotato

                             plotSpacing="", #wehat maize soybean
                             rowSpacing = "", #wehat maize soybean
                             rowOrientation="", #wehat maize soybean
                             spaceBwPlantsRow= "", #wehat maize soybean
                             hillSpacing="", #wehat maize soybean
                             numsMsPlantPerPlot="", #wehat maize soybean
                             fieldArea= "", #wehat maize soybean
                             expFieldMaxWidth="", #wehat maize soybean
                             expFieldMaxLength="", #wehat maize soybean

                             factor_hdafims1="",
                             lvl_hdafims1="",
                             factor_hdafims2="",
                             lvl_hdafims2="",
                             factor_hdafims3="",
                             lvl_hdafims3="",
                             factor_hdafims4="",
                             lvl_hdafims4="",
                             factor_hdafims5="",
                             lvl_hdafims5=""
                             ){

  Installation <- agronomic_crop_template
  #ToDo: cambiar en el template por minusculas las letras siguientes
  names(Installation) <- c ("Variable", "Value")

  if(is.null(lvl_hdafims1)){lvl_hdafims1 <- ""}
  if(is.null(lvl_hdafims2)){lvl_hdafims2 <- ""}
  if(is.null(lvl_hdafims3)){lvl_hdafims3 <- ""}
  if(is.null(lvl_hdafims4)){lvl_hdafims4 <- ""}
  if(is.null(lvl_hdafims5)){lvl_hdafims5 <- ""}



  lvl1_hdafims1 <- lvl_hdafims1[1] #first level of the first factor
  lvl2_hdafims1 <- lvl_hdafims1[2]
  lvl3_hdafims1 <- lvl_hdafims1[3]
  lvl4_hdafims1 <- lvl_hdafims1[4]
  lvl5_hdafims1 <- lvl_hdafims1[5]

  lvl1_hdafims2 <- lvl_hdafims2[1] #first level of the first factor
  lvl2_hdafims2 <- lvl_hdafims2[2]
  lvl3_hdafims2 <- lvl_hdafims2[3]
  lvl4_hdafims2 <- lvl_hdafims2[4]
  lvl5_hdafims2 <- lvl_hdafims2[5]

  lvl1_hdafims3 <- lvl_hdafims3[1] #first level of the first factor
  lvl2_hdafims3 <- lvl_hdafims3[2]
  lvl3_hdafims3 <- lvl_hdafims3[3]
  lvl4_hdafims3 <- lvl_hdafims3[4]
  lvl5_hdafims3 <- lvl_hdafims3[5]


  lvl1_hdafims4 <- lvl_hdafims4[1] #first level of the first factor
  lvl2_hdafims4 <- lvl_hdafims4[2]
  lvl3_hdafims4 <- lvl_hdafims4[3]
  lvl4_hdafims4 <- lvl_hdafims4[4]
  lvl5_hdafims4 <- lvl_hdafims4[5]

  lvl1_hdafims5 <- lvl_hdafims5[1] #first level of the first factor
  lvl2_hdafims5 <- lvl_hdafims5[2]
  lvl3_hdafims5 <- lvl_hdafims5[3]
  lvl4_hdafims5 <- lvl_hdafims5[4]
  lvl5_hdafims5 <- lvl_hdafims5[5]

  #all crops
  Installation[Installation$Variable=='Experimental design' ,col_name]	<-	experimental_design_label(designFieldbook_agrofims)
  Installation[Installation$Variable=='Experimental design abbreviation' , col_name]	<-	paste(designFieldbook_agrofims )
  Installation[Installation$Variable=='Number of replications' ,col_name]	<-	paste(designFieldbook_agrofims_r)
  Installation[Installation$Variable=='Number of blocks' ,col_name]	<-	paste(designFieldbook_agrofims_r)

  #potato cassava sweetpotato
  crop <- crop
  print("cultivo")
  print(crop)
  print("cultivo")


  if(crop == "Potato" || crop == "Sweetpotato" || crop == "Cassava") {

  Installation[Installation$Variable=='Number of plants planted per plot' ,col_name]	<-	paste(numPlantsPerPlot )
  Installation[Installation$Variable=='Number of rows per plot' ,col_name]	<-	paste(numRowsPerPlot )
  Installation[Installation$Variable=='Number of plants per row' ,col_name]	<-	paste(numPlantsPerRow )
  Installation[Installation$Variable=='Plot size (m2)' ,col_name]	<-	paste(plotSize )

  Installation[Installation$Variable=='Distance between plants (m)' ,col_name]	<-	paste(distancebwPlants)
  Installation[Installation$Variable=='Distance between rows (m)' ,col_name]	<-	paste(distanceBwRows)


  Installation[Installation$Variable=='Space between plants (m)' ,col_name]	<-	paste(spaceBwPlants )
  Installation[Installation$Variable=='Space between rows (m)' ,col_name]	<-	paste(spaceBwRows )
  Installation[Installation$Variable=='Planting density (plants/Ha)' ,col_name]	<-	paste(planDensity )

  }

  if(crop == "Wheat" || crop == "Maize" || crop == "Soybean"){
  #wheat maize soybean
  Installation[Installation$Variable=='Plot spacing' ,col_name]	<-	paste(plotSpacing )
  Installation[Installation$Variable=='Row spacing' ,col_name]	<-	paste(rowSpacing )
  Installation[Installation$Variable=='Row orientation' ,col_name]	<-	paste(rowOrientation )
  Installation[Installation$Variable=='Space between plants in row' ,col_name]	<-	paste(spaceBwPlantsRow )
  Installation[Installation$Variable=='Hill spacing' ,col_name]	<-	paste(hillSpacing )
  Installation[Installation$Variable=='Number of measured plants per plot' ,col_name]	<-	paste(numsMsPlantPerPlot )
  #wehat maize soybean
  Installation[Installation$Variable=='Field area' ,col_name]	<-	paste(fieldArea )
  Installation[Installation$Variable=='Experimental field maximum width' ,col_name]	<-	paste(expFieldMaxWidth )
  Installation[Installation$Variable=='Experimental field maximum length' ,col_name]	<-	paste(expFieldMaxLength)

  }

  #factor 1
  Installation[Installation$Variable=='Factor 1' , col_name]	<-	paste(factor_hdafims1)
  Installation[Installation$Variable=='Factor 1 - level 1' ,col_name] <- paste(lvl1_hdafims1)
  Installation[Installation$Variable=='Factor 1 - level 2' ,col_name] <- paste(lvl2_hdafims1)
  Installation[Installation$Variable=='Factor 1 - level 3' ,col_name] <- paste(lvl3_hdafims1)
  Installation[Installation$Variable=='Factor 1 - level 4' ,col_name] <- paste(lvl4_hdafims1)
  Installation[Installation$Variable=='Factor 1 - level 5' ,col_name] <- paste(lvl5_hdafims1)

  #factor 2
  Installation[Installation$Variable=='Factor 2',  col_name]	<-	paste(factor_hdafims2)
  Installation[Installation$Variable=='Factor 2 - level 1' ,col_name] <- paste(lvl1_hdafims2)
  Installation[Installation$Variable=='Factor 2 - level 2' ,col_name] <- paste(lvl2_hdafims2)
  Installation[Installation$Variable=='Factor 2 - level 3' ,col_name] <- paste(lvl3_hdafims2)
  Installation[Installation$Variable=='Factor 2 - level 4' ,col_name] <- paste(lvl4_hdafims2)
  Installation[Installation$Variable=='Factor 2 - level 5' ,col_name] <- paste(lvl5_hdafims2)

  #factor 3
  Installation[Installation$Variable=='Factor 3',   col_name]	<-	paste(factor_hdafims3)
  Installation[Installation$Variable=='Factor 3 - level 1' ,col_name] <- paste(lvl1_hdafims3)
  Installation[Installation$Variable=='Factor 3 - level 2' ,col_name] <- paste(lvl2_hdafims3)
  Installation[Installation$Variable=='Factor 3 - level 3' ,col_name] <- paste(lvl3_hdafims3)
  Installation[Installation$Variable=='Factor 3 - level 4' ,col_name] <- paste(lvl4_hdafims3)
  Installation[Installation$Variable=='Factor 3 - level 5' ,col_name] <- paste(lvl5_hdafims3)

  #factor 4
  Installation[Installation$Variable=='Factor 4' ,  col_name]	<-	paste(factor_hdafims4)
  Installation[Installation$Variable=='Factor 4 - level 1' ,col_name] <- paste(lvl1_hdafims4)
  Installation[Installation$Variable=='Factor 4 - level 2' ,col_name] <- paste(lvl2_hdafims4)
  Installation[Installation$Variable=='Factor 4 - level 3' ,col_name] <- paste(lvl3_hdafims4)
  Installation[Installation$Variable=='Factor 4 - level 4' ,col_name] <- paste(lvl4_hdafims4)
  Installation[Installation$Variable=='Factor 4 - level 5' ,col_name] <- paste(lvl5_hdafims4)

  #factor 5
  Installation[Installation$Variable=='Factor 5',  col_name]	<-	paste(factor_hdafims5)
  Installation[Installation$Variable=='Factor 5 - level 1' ,col_name] <- paste(lvl1_hdafims5)
  Installation[Installation$Variable=='Factor 5 - level 2' ,col_name] <- paste(lvl2_hdafims5)
  Installation[Installation$Variable=='Factor 5 - level 3' ,col_name] <- paste(lvl3_hdafims5)
  Installation[Installation$Variable=='Factor 5 - level 4' ,col_name] <- paste(lvl4_hdafims5)
  Installation[Installation$Variable=='Factor 5 - level 5' ,col_name] <- paste(lvl5_hdafims5)

  Installation
}

#' Create land des for hidap-agrofims
#' @export

 land_des <-function(landLeveling_start_date,landLeveling_end_date,
                     numPasses,operationsOrder,impl_type,animal_traction,
                     humanPowered,motorized_traction,puddling_start_date,puddling_end_date,
                     Penetrometer_in_field,puddling_depth_val,pud_animal_traction,
                     pud_humanPowered,pud_motorized_traction,tillage_start_date,tillage_end_date,
                     till_technique,till_depth_method,till_depth,till_total_op_season,till_impl_type,
                     till_animal_traction,till_humanPowered,till_motorized_traction,liming_start_date,
                     liming_end_date,lim_material,lim_quantity,lim_description ){

   #a0 <- c("Group",	"Subgroup",	"Sublevel",	"Values")


   if(is.null( impl_type)){impl_type  <-""}
   if(is.null( animal_traction)){ animal_traction <-""}
   if(is.null( humanPowered)){ humanPowered <-""}

   if(is.null(pud_animal_traction)){pud_animal_traction <-""}
   if(is.null(pud_humanPowered)){ pud_humanPowered <-"" }
   if(is.null(pud_motorized_traction)){ pud_motorized_traction<-"" }



   if(is.null( till_animal_traction)){  till_animal_traction <-""}
   if(is.null(till_humanPowered )){  till_humanPowered<-""}
   if(is.null( till_technique)){ till_humanPowered <-""}

   if(is.null( till_motorized_traction  )){ till_motorized_traction <-""}

   a1	<-	c('Land preparation','Land levelling','Measurements: Operation start date', paste(landLeveling_start_date))
   a2	<-	c('Land preparation','Land levelling','Measurements: Operation end date', paste(landLeveling_end_date))
   a3	<-	c('Land preparation','Land levelling','Measurements: Land levelling number of passes',numPasses)
   a4	<-	c('Land preparation','Land levelling','Measurements: Land levelling operations order',operationsOrder)
   a5	<-	c('Land preparation','Land levelling','Description: Land levelling implement type',impl_type)
   a6	<-	c('Land preparation','Land levelling','Implement: Animal traction',animal_traction)
   a7	<-	c('Land preparation','Land levelling','Implement: Human powered',humanPowered)
   a8	<-	c('Land preparation','Land levelling','Implement: Motorized traction',motorized_traction)
   a9	<-	c('Land preparation','Puddling','Measurements: Operation start date', paste(puddling_start_date))
   a10	<-	c('Land preparation','Puddling','Measurements: Operation end date', paste(puddling_end_date))
   a11	<-	c('Land preparation','Puddling','Puddling depth method: Penetrometer in field',Penetrometer_in_field)
   a12	<-	c('Land preparation','Puddling','Measurements: Puddling depth (cm)',puddling_depth_val)
   a13	<-	c('Land preparation','Puddling','Implement: Animal traction',pud_animal_traction)
   a14	<-	c('Land preparation','Puddling','Implement: Human powered',pud_humanPowered)
   a15	<-	c('Land preparation','Puddling','Implement: Motorized traction',pud_motorized_traction)
   a16	<-	c('Land preparation','Tillage','Measurements: Operation start date', paste(tillage_start_date))
   a17	<-	c('Land preparation','Tillage','Measurements: Operation end date', paste(tillage_end_date))
   a18	<-	c('Land preparation','Tillage','Tillage technique',till_technique)
   a19	<-	c('Land preparation','Tillage','Tillage depth method',till_depth_method)
   a20	<-	c('Land preparation','Tillage','Tillage depth',till_depth)
   a21	<-	c('Land preparation','Tillage','Tillage total operation for a season',till_total_op_season)
   a22	<-	c('Land preparation','Tillage','Description: Tillage implement',till_impl_type)
   a23	<-	c('Land preparation','Tillage','Implement: Animal traction',till_animal_traction)
   a24	<-	c('Land preparation','Tillage','Implement: Human powered',till_humanPowered)
   a25	<-	c('Land preparation','Tillage','Implement: Motorized traction',till_motorized_traction)
   a26	<-	c('Land preparation','Liming','Measurements: Operation start date', paste(liming_start_date))
   a27	<-	c('Land preparation','Liming','Measurements: Operation end date', paste(liming_end_date))
   a28	<-	c('Land preparation','Liming','Measurements: Liming material',lim_material)
   a29	<-	c('Land preparation','Liming','Measurements: Quantity of liming material',lim_quantity)
   a30	<-	c('Land preparation','Liming','Measurements: Liming implement description',lim_description)


   land_data <- rbind(a1 ,a2,a3,a4, a5, a6, a7, a8, a9, a10, a11, a12,a13, a14,a15,a16,a17,a18,a19,
                      a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30)

   land_data <- as.data.frame(land_data, stringsAsFactors =FALSE)
   names(land_data) <- c("Group",	"Subgroup",	"Sublevel",	"Values")
   print(land_data)
   land_data
 }





# land_des(input$landLeveling_start_date,input$landLeveling_end_date,
#          input$numPasses,input$operationsOrder,input$impl_type,input$animal_traction,
#          input$humanPowered,input$motorized_traction,input$puddling_start_date,
#          input$puddling_end_date,input$Penetrometer_in_field,input$puddling_depth_val,
#          input$pud_animal_traction,input$pud_humanPowered,input$pud_motorized_traction,
#          input$tillage_start_date,input$tillage_end_date,input$till_technique,input$till_depth_method,
#          input$till_depth,input$till_total_op_season,input$till_impl_type,
#          input$till_animal_traction,input$till_humanPowered,input$till_motorized_traction,
#          input$liming_start_date,input$liming_end_date,input$lim_material,input$lim_quantity,
#          input$lim_description)
#
#
#
# # mulch -------------------------------------------------------------------
#
##' Create mulching for hidap-agrofims
 #' @export
 mulch<- function(mulch_start_date,mulch_end_date,mulch_type,mulch_thickness,
                  mulch_amountPerSq,mulch_color,mulch_percCoverage,mulch_remove_start_date,
                  mulch_remove_end_date,mulch_make,mulch_model,mulch_animal_traction,
                  mulch_humanPowered,mulch_motorized_traction,residue_cropType,
                  residue_technique,residue_incorp_depth,residue_aboveGroundMoisture,
                  residue_aboveGroundAmount){



   if(is.null(mulch_type)){mulch_type<-''}
   if(is.null(mulch_color)){mulch_color<-''}
   if(is.null(mulch_animal_traction)){mulch_animal_traction<-''}
   if(is.null(mulch_motorized_traction)){mulch_motorized_traction<-''}


   if(is.null(residue_cropType)){residue_cropType<-''}
   if(is.null(residue_technique)){residue_technique<-''}
   if(is.null(residue_aboveGroundMoisture)){residue_aboveGroundMoisture<-''}


   #m0 <- c("Group",	"Subgroup",	"Sublevel",	"Values")
   m1 <- c('Mulching and residue management','Mulch','Operation start date', paste(mulch_start_date))
   m2 <-c('Mulching and residue management','Mulch','Operation end date', paste(mulch_end_date))
   m3 <-c('Mulching and residue management','Mulch','Mulch type',mulch_type)
   m4 <-c('Mulching and residue management','Mulch','Mulch thickness',mulch_thickness)
   m5 <-c('Mulching and residue management','Mulch','Mulch amount per sq. m',mulch_amountPerSq)
   m6 <-c('Mulching and residue management','Mulch','Mulch color',mulch_color)
   m7 <-c('Mulching and residue management','Mulch','Mulch percentage of coverage',mulch_percCoverage)
   m8 <-c('Mulching and residue management','Mulch','Mulch removal start date', paste(mulch_remove_start_date))
   m9 <-c('Mulching and residue management','Mulch','Mulch removal end date', paste(mulch_remove_end_date))
   m10 <-c('Mulching and residue management','Mulch','Description: Mulching implement make',mulch_make)
   m11 <-c('Mulching and residue management','Mulch','Description: Mulching implement model',mulch_model)
   m12 <-c('Mulching and residue management','Mulch','Mulching Implement: Animal traction',mulch_animal_traction)
   m13 <-c('Mulching and residue management','Mulch','Mulching Implement: Human powered',mulch_humanPowered)
   m14 <-c('Mulching and residue management','Mulch','Mulching Implement: Motorized traction',mulch_motorized_traction)
   m15 <-c('Mulching and residue management','Residue management','Residue crop type',residue_cropType)
   m16 <-c('Mulching and residue management','Residue management','Residue management technique',residue_technique)
   m17 <-c('Mulching and residue management','Residue management','Residue incorporation depth',residue_incorp_depth)
   m18 <-c('Mulching and residue management','Residue management','Above ground residue moisture',residue_aboveGroundMoisture)
   m19 <-c('Mulching and residue management','Residue management','Above ground residue (amount)',residue_aboveGroundAmount)
   mulch_data <- rbind( m1,m2,m3,m4,m5,m6,m7,m8,m9,m10, m11, m12,m13, m14,m15,m16,m17,m18,m19)
   mulch_data <-as.data.frame(mulch_data, stringsAsFactors =FALSE)
   names(mulch_data) <- c("Group",	"Subgroup",	"Sublevel",	"Values")
   mulch_data
 }

##Plating

 #' Create planting for hidap-agrofims
 #' @export

plant <- function(planting_start_date,planting_end_date,planting_directSeeding,
                  planting_seedingTech,planting_ageSeeding,planting_manual,planting_animal_traction,
                  planting_motorized_traction,
                  planting_rowDistance,planting_seedingRate,
                  planting_seedPerhill,planting_distance,planting_distribution){

  if(is.null(planting_start_date)){planting_start_date<-''}
  if(is.null(planting_end_date)){planting_end_date<-''}
  if(is.null(planting_directSeeding)){planting_directSeeding<-''}
  if(is.null(planting_seedingTech)){planting_seedingTech<-''}
  if(is.null(planting_ageSeeding)){planting_ageSeeding<-''}
  if(is.null(planting_manual)){planting_manual<-''}
  if(is.null(planting_animal_traction)){planting_animal_traction<-''}
  if(is.null(planting_motorized_traction)){planting_motorized_traction<-''}
  if(is.null(planting_rowDistance)){planting_rowDistance<-''}
  if(is.null(planting_seedingRate)){planting_seedingRate<-''}
  if(is.null(planting_seedPerhill)){planting_seedPerhill<-''}
  if(is.null(planting_distance)){planting_distance<-''}
  if(is.null(planting_distribution)){planting_distribution<-''}


pl1	<-	c('Planting and Seeding','Planting method','Operation start date', paste(planting_start_date))
pl2	<-	c('Planting and Seeding','Planting method','Operation end date', paste(planting_end_date))
pl3	<-	c('Planting and Seeding','Planting method','Direct seedling',planting_directSeeding)
pl4	<-	c('Planting and Seeding','Planting method','Planting/seeding technique',planting_seedingTech)
pl5	<-	c('Planting and Seeding','Planting method','Age of the seedling',planting_ageSeeding)
pl6	<-	c('Planting and Seeding','Planting method','Planting/transplanting techniques: Manual',planting_manual)
pl7	<-	c('Planting and Seeding','Planting method','Planting/transplanting techniques: Animal traction',planting_animal_traction)
pl8	<-	c('Planting and Seeding','Planting method','Planting/transplanting techniques:  Motorized traction',planting_motorized_traction)
pl9	<-	c('Planting and Seeding','Planting arrangement (sowing)','Row distance',planting_rowDistance)
pl10	<-	c('Planting and Seeding','Planting arrangement (sowing)','Seedling rate (plant density) ',planting_seedingRate)
pl11	<-	c('Planting and Seeding','Planting arrangement (sowing)','Seed/seedling per hill',planting_seedPerhill)
pl12	<-	c('Planting and Seeding','Planting arrangement (sowing)','Planting distance',planting_distance)
pl13	<-	c('Planting and Seeding','Planting arrangement (sowing)','Arrangement type: Planting distribution',planting_distribution)


plant_data <- rbind(pl1 ,pl2,pl3,pl4,pl5,pl6,pl7,pl8,pl9,pl10, pl11, pl12,pl13)
plant_data <-as.data.frame(plant_data, stringsAsFactors =FALSE)
names(plant_data) <- c("Group",	"Subgroup",	"Sublevel",	"Values")
plant_data
}

#Harvest

#' Create harvest for hidap-agrofims
#' @export
#'
harvest <- function(harvest_start_date,harvest_end_date,
                    crop_component_harvested,harvest_implement,harvest_make,
                    harvest_model,harvest_animal_traction,harvest_humanPowered,
                    harvest_motorized_traction){


  if(is.null(harvest_start_date)){harvest_start_date<-''}
  if(is.null(harvest_end_date)){harvest_end_date<-''}
  if(is.null(crop_component_harvested)){crop_component_harvested<-''}
  if(is.null(harvest_implement)){harvest_implement<-''}
  if(is.null(harvest_make)){harvest_make<-''}
  if(is.null(harvest_model)){harvest_model<-''}
  if(is.null(harvest_animal_traction)){harvest_animal_traction<-''}
  if(is.null(harvest_humanPowered)){harvest_humanPowered<-''}
  if(is.null(harvest_motorized_traction)){harvest_motorized_traction<-''}


  harv1	<-	c('Harvest','Description Harvest','Operation start date', paste(harvest_start_date))
  harv2	<-	c('Harvest','Description Harvest','Operation end date', paste(harvest_end_date))
  harv3	<-	c('Harvest','Description Harvest','Crop component harvested',crop_component_harvested)
  harv4	<-	c('Harvest','Description Harvest','Description: Harvest implement',harvest_implement)
  harv5	<-	c('Harvest','Description Harvest','Description: Harvest implement make',harvest_make)
  harv6	<-	c('Harvest','Description Harvest','Description: Harvest implement model',harvest_model)
  harv7	<-	c('Harvest','Description Harvest','Harvest implement: Animal traction',harvest_animal_traction)
  harv8	<-	c('Harvest','Description Harvest','Harvest implement: Human powered',harvest_humanPowered)
  harv9	<-	c('Harvest','Description Harvest','Harvest implement: Motorized traction',harvest_motorized_traction)

  harv_data <- rbind(harv1 ,harv2,harv3,harv4,harv5,harv6,harv7,harv8,harv9)
  harv_data <-as.data.frame(harv_data, stringsAsFactors =FALSE)
  names(harv_data) <- c("Group",	"Subgroup",	"Sublevel",	"Values")
  harv_data
}


#Irrigation

#' Create irri for hidap-agrofims
#' @export
#'
#'
irrigation <- function(irrigationevent_start_date,irrigationevent_end_date,
                         irrigation_system_type,irrigation_technique,surface_irrigation_technique,
                         localized_irrigation_technique,irrigation_using_sprinkler_systems,
                         irrigation_system_picture="",irrigation_water_source,irrigation_water_source_distance,
                         irrigation_bund_height,irrigation_percolation_rate,
                         irrigation_equipment_depth,irrigation_well_depth,irrigation_area_covered_irrigation_system){


  if(is.null(irrigationevent_start_date)){irrigationevent_start_date<-''}
  if(is.null(irrigationevent_end_date)){irrigationevent_end_date<-''}
  if(is.null(irrigation_system_type)){irrigation_system_type<-''}
  if(is.null(irrigation_technique)){irrigation_technique<-''}
  if(is.null(surface_irrigation_technique)){surface_irrigation_technique<-''}
  if(is.null(localized_irrigation_technique)){localized_irrigation_technique<-''}
  if(is.null(irrigation_using_sprinkler_systems)){irrigation_using_sprinkler_systems<-''}
  if(is.null(irrigation_system_picture)){irrigation_system_picture<-''}
  if(is.null(irrigation_water_source)){irrigation_water_source<-''}
  if(is.null(irrigation_water_source_distance)){irrigation_water_source_distance<-''}
  if(is.null(irrigation_bund_height)){irrigation_bund_height<-''}
  if(is.null(irrigation_percolation_rate)){irrigation_percolation_rate<-''}
  if(is.null(irrigation_equipment_depth)){irrigation_equipment_depth<-''}
  if(is.null(irrigation_well_depth)){irrigation_well_depth<-''}
  if(is.null(irrigation_area_covered_irrigation_system)){irrigation_area_covered_irrigation_system<-''}

  paste0("surface_irrigation_technique_", order)


  irri1	<-	c('Irrigation event','Irrigation start date', paste(irrigationevent_start_date))
  irri2	<-	c('Irrigation event','Irrigation end date', paste(irrigationevent_end_date))
  irri3	<-	c('Irrigation event','Irrigation system type',irrigation_system_type)
  irri4	<-	c('Irrigation event','Irrigation technique',irrigation_technique)

  irri5	<-	c('Irrigation event','Surface irrigation technique',surface_irrigation_technique)
  irri6	<-	c('Irrigation event','Localized irrigation technique',localized_irrigation_technique)
  irri7	<-	c('Irrigation event','Irrigation using sprinkler systems',irrigation_using_sprinkler_systems)
  irri8	<-	c('Irrigation event','Irrigation system picture', "")#irrigation_system_picture)


  irri9	<-	c('Irrigation event','Water source',irrigation_water_source)
  irri10	<-	c('Irrigation event','Water source distance',irrigation_water_source_distance)
  irri11	<-	c('Irrigation event','Bund height',irrigation_bund_height)
  irri12	<-	c('Irrigation event','Percolation rate',irrigation_percolation_rate)
  irri13	<-	c('Irrigation event','Irrigation equipment depth',irrigation_equipment_depth)
  irri14	<-	c('Irrigation event','Well depth',irrigation_well_depth)
  irri15	<-	c('Irrigation event','Area covered by the irrigation system',irrigation_area_covered_irrigation_system)

  irri_data <- rbind(irri1 ,irri2,irri3,irri4,irri5,irri6,irri7,irri8,irri9,irri10, irri11, irri12,irri13, irri14, irri15)
  irri_data <-as.data.frame(irri_data, stringsAsFactors =FALSE)
  names(irri_data)<-c("Group"	,"Level"	,"Value")
  irri_data

}



##Biofer
#' Create biofer for hidap-agrofims
#' @export
#'
#'
biofer <- function(biofertilizer_landLeveling_start_date,
                   biofertilizer_landLeveling_end_date,biofertilizer_rhizobium_inoculum_strain,
                   biofertilizer_quantity_inoculated,biofertilizer_inoculation_method,
                   biofertilizer_product_formulation,
                   biofertilizer_days_sowing_after_rhizobium_inocculation){


  if(is.null(biofertilizer_landLeveling_start_date)){biofertilizer_landLeveling_start_date<-''}
  if(is.null(biofertilizer_landLeveling_end_date)){biofertilizer_landLeveling_end_date<-''}
  if(is.null(biofertilizer_rhizobium_inoculum_strain)){biofertilizer_rhizobium_inoculum_strain<-''}
  if(is.null(biofertilizer_quantity_inoculated)){biofertilizer_quantity_inoculated<-''}
  if(is.null(biofertilizer_inoculation_method)){biofertilizer_inoculation_method<-''}
  if(is.null(biofertilizer_product_formulation)){biofertilizer_product_formulation<-''}
  if(is.null(biofertilizer_days_sowing_after_rhizobium_inocculation)){biofertilizer_days_sowing_after_rhizobium_inocculation<-''}


  bio1	<-	c('Biofertilizer','Operation start date', paste(biofertilizer_landLeveling_start_date))
  bio2	<-	c('Biofertilizer','Operation end date', paste(biofertilizer_landLeveling_end_date))
  bio3	<-	c('Biofertilizer','Rhizobium inoculum strain',biofertilizer_rhizobium_inoculum_strain)
  bio4	<-	c('Biofertilizer','Biofertilizer quantity inoculated',biofertilizer_quantity_inoculated)
  bio5	<-	c('Biofertilizer','Inoculation method',biofertilizer_inoculation_method)
  bio6	<-	c('Biofertilizer','Product formulation',biofertilizer_product_formulation)
  bio7	<-	c('Biofertilizer','Days to sowing after Rhizobium inocculation',biofertilizer_days_sowing_after_rhizobium_inocculation)


  bio_data <- rbind(bio1 ,bio2,bio3,bio4,bio5,bio6,bio7)
  bio_data <-as.data.frame(bio_data, stringsAsFactors =FALSE)
  names(bio_data)<-c("Group"	,"Level"	,"Value")
  bio_data
  }


##nutrinet


#pest and disease

#' Create pest and disease for hidap-agrofims
#' @export
#'
#'
pestdis <- function(disease_observation_date,disease_name,
                    disease_plant_parts_affected,disease_percentage_experiement_affected,
                    disease_damages_notes,disease_notes,pest_type,
                    pest_name,pest_damage_notes,pest_notes,
                    pestcontrol_start_date,pestcontrol_end_date,
                    pest_control_technique,pesticide_application_depth,pesticide_amount,
                    pest_image="",pest_control_applications_totnumber,
                    pest_control_details,chemical_pest_control_equipment,
                    pesticide_implement_make,pesticide_implement_model,pesticide_animal_traction,
                    pesticide_humanPowered,pesticide_motorized_traction
){


  if(is.null(disease_observation_date)){disease_observation_date<-''}
  if(is.null(disease_name)){disease_name<-''}
  if(is.null(disease_plant_parts_affected)){disease_plant_parts_affected<-''}
  if(is.null(disease_percentage_experiement_affected)){disease_percentage_experiement_affected<-''}
  if(is.null(disease_damages_notes)){disease_damages_notes<-''}
  if(is.null(disease_notes)){disease_notes<-''}
  if(is.null(pest_type)){pest_type<-''}
  if(is.null(pest_name)){pest_name<-''}
  if(is.null(pest_damage_notes)){pest_damage_notes<-''}
  if(is.null(pest_notes)){pest_notes<-''}
  if(is.null(pestcontrol_start_date)){pestcontrol_start_date<-''}
  if(is.null(pestcontrol_end_date)){pestcontrol_end_date<-''}
  if(is.null(pest_control_technique)){pest_control_technique<-''}
  if(is.null(pesticide_application_depth)){pesticide_application_depth<-''}
  if(is.null(pesticide_amount)){pesticide_amount<-''}
  if(is.null(pest_image)){pest_image<-''}
  if(is.null(pest_control_applications_totnumber)){pest_control_applications_totnumber<-''}
  if(is.null(pest_control_details)){pest_control_details<-''}
  if(is.null(chemical_pest_control_equipment)){chemical_pest_control_equipment<-''}
  if(is.null(pesticide_implement_make)){pesticide_implement_make<-''}
  if(is.null(pesticide_implement_model)){pesticide_implement_model<-''}
  if(is.null(pesticide_animal_traction)){pesticide_animal_traction<-''}
  if(is.null(pesticide_humanPowered)){pesticide_humanPowered<-''}
  if(is.null(pesticide_motorized_traction)){pesticide_motorized_traction<-''}

  pdis1	<-	c('Disease observation','Disease','Disease observation date', paste(disease_observation_date))
  pdis2	<-	c('Disease observation','Disease','Disease name',disease_name)
  pdis3	<-	c('Disease observation','Disease','Plant parts affected',disease_plant_parts_affected)
  pdis4	<-	c('Disease observation','Disease','Percentage of the experiement affected',disease_percentage_experiement_affected)
  pdis5	<-	c('Disease observation','Disease','Disease damages, notes',disease_damages_notes)
  pdis6	<-	c('Disease observation','Disease','Disease, notes',disease_notes)
  pdis7	<-	c('Pest observation and control','Pest observation','Pest type',pest_type)
  pdis8	<-	c('Pest observation and control','Pest observation','Pest name',pest_name)
  pdis9	<-	c('Pest observation and control','Pest observation','Pest damage, notes',pest_damage_notes)
  pdis10	<-	c('Pest observation and control','Pest observation','Pest, notes',pest_notes)
  pdis11	<-	c('Pest observation and control','Pest control','Pest control start date', paste(pestcontrol_start_date))
  pdis12	<-	c('Pest observation and control','Pest control','Pest control end date', paste(pestcontrol_end_date))
  pdis13	<-	c('Pest observation and control','Pest control','Pest control technique',pest_control_technique)
  pdis14	<-	c('Pest observation and control','Pest control','Pesticide application depth',pesticide_application_depth)
  pdis15	<-	c('Pest observation and control','Pest control','Pesticide amount',pesticide_amount)
  pdis16	<-	c('Pest observation and control','Pest control','Pesticide box or bottle picture',pest_image)
  pdis17	<-	c('Pest observation and control','Pest control','Pest control applications total number',pest_control_applications_totnumber)
  pdis18	<-	c('Pest observation and control','Pest control','Pest control details (e.g. name of parasitoid etc), treatment evaluation',pest_control_details)
  pdis19	<-	c('Pest observation and control','Pest control','Description: Chemical pest control equipment',chemical_pest_control_equipment)
  pdis20	<-	c('Pest observation and control','Pest control','Description: Pesticide implement make',pesticide_implement_make)
  pdis21	<-	c('Pest observation and control','Pest control','Description: Pesticide implement model',pesticide_implement_model)
  pdis22	<-	c('Pest observation and control','Pest control','Pesticide implement: Animal traction',pesticide_animal_traction)
  pdis23	<-	c('Pest observation and control','Pest control','Pesticide implement: Human powered',pesticide_humanPowered)
  pdis24	<-	c('Pest observation and control','Pest control','Pesticide implement: Motorized traction',pesticide_motorized_traction)



  pdis_data <- rbind(pdis1 ,pdis2,pdis3,pdis4,pdis5,pdis6,pdis7, pdis8, pdis9, pdis10, pdis11, pdis12, pdis13, pdis14, pdis15,pdis16,
                     pdis17, pdis18, pdis19, pdis20, pdis21, pdis22, pdis23, pdis24)
  pdis_data <-as.data.frame(pdis_data, stringsAsFactors =FALSE)
  names(pdis_data)<-c("Group", "Subgroup","Level","Value")
  pdis_data

}





#   mulch(mulch_start_date,input$mulch_end_date,input$mulch_type,input$mulch_thickness,input$mulch_amountPerSq,input$mulch_color,input$mulch_percCoverage,input$mulch_remove_start_date,input$mulch_remove_end_date,input$mulch_make,input$mulch_model,input$mulch_animal_traction,input$mulch_humanPowered,input$mulch_motorized_traction,input$residue_cropType,input$residue_technique,input$residue_incorp_depth,input$residue_aboveGroundMoisture,input$residue_aboveGroundAmount)
#
# #harvanting
#
# planting <- function(planting_start_date, planting_end_date, planting_directSeeding,
#                      planting_seedingTech, planting_ageSeeding,
#                      planting_manual, planting_animal_traction,
#                      planting_motorized_traction,
#                      planting_rowDistance,planting_seedingRate,
#                      planting_seedPerhill,planting_distance,
#                      planting_distribution)
#
#
#
#   planting(input$planting_start_date,input$planting_end_date,
#            input$planting_directSeeding,input$planting_seedingTech,
#            input$planting_ageSeeding,input$planting_manual,input$planting_animal_traction,
#            input$planting_motorized_traction,input$planting_rowDistance,
#            input$planting_seedingRate,input$planting_seedPerhill,
#            input$planting_distance,input$planting_distribution)
#
# #harvest
#
# harvest <- function(harvest_start_date,harvest_end_date,crop_component_harvested,harvest_implement,harvest_make,harvest_model,harvest_animal_traction,harvest_humanPowered,harvest_motorized_traction)
#
#
#   harvest(input$harvest_start_date,input$harvest_end_date,input$crop_component_harvested,input$harvest_implement,input$harvest_make,input$harvest_model,input$harvest_animal_traction,input$harvest_humanPowered,input$harvest_motorized_traction)
#
#
# ##irrigation
#
# irrigation <-function(irrigationevent_start_date,irrigationevent_end_date,irrigation_system_type,irrigation_technique,surface_irrigation_technique,localized_irrigation_technique,irrigation_using_sprinkler_systems,Irrigation system picture,irrigation_water_source,irrigation_water_source_distance,irrigation_bund_height,irrigation_percolation_rate,irrigation_equipment_depth,irrigation_well_depth,irrigation_area_covered_irrigation_system)
#
#   irrigation(input$irrigationevent_start_date,input$irrigationevent_end_date,input$irrigation_system_type,input$irrigation_technique,input$surface_irrigation_technique,input$localized_irrigation_technique,input$irrigation_using_sprinkler_systems,input$Irrigation system picture,input$irrigation_water_source,input$irrigation_water_source_distance,input$irrigation_bund_height,input$irrigation_percolation_rate,input$irrigation_equipment_depth,input$irrigation_well_depth,input$irrigation_area_covered_irrigation_system)
#
#
# ##biofertilizer
#
# biofertilizer <- function (biofertilizer_landLeveling_start_date,biofertilizer_landLeveling_end_date,biofertilizer_rhizobium_inoculum_strain,biofertilizer_quantity_inoculated,biofertilizer_inoculation_method,biofertilizer_product_formulation,biofertilizer_days_sowing_after_rhizobium_inocculation)
#
#   biofertilizer(input$biofertilizer_landLeveling_start_date,input$biofertilizer_landLeveling_end_date,input$biofertilizer_rhizobium_inoculum_strain,input$biofertilizer_quantity_inoculated,input$biofertilizer_inoculation_method,input$biofertilizer_product_formulation,input$biofertilizer_days_sowing_after_rhizobium_inocculation)
#
#
# ## pesticide
#
#
# pesdis <- function(disease_observation_date,disease_name,disease_plant_parts_affected,disease_percentage_experiement_affected,disease_damages_notes,disease_notes,pest_type,pest_name,pest_damage_notes,pest_notes,pestcontrol_start_date,pestcontrol_end_date,pest_control_technique,pesticide_application_depth,pesticide_amount,pest_image,pest_control_applications_totnumber,pest_control_details,chemical_pest_control_equipment,pesticide_implement_make,pesticide_implement_model,pesticide_animal_traction,pesticide_humanPowered,pesticide_motorized_traction)
#
#   pesdis(input$disease_observation_date,input$disease_name,input$disease_plant_parts_affected,input$disease_percentage_experiement_affected,input$disease_damages_notes,input$disease_notes,input$pest_type,input$pest_name,input$pest_damage_notes,input$pest_notes,input$pestcontrol_start_date,input$pestcontrol_end_date,input$pest_control_technique,input$pesticide_application_depth,input$pesticide_amount,input$pest_image,input$pest_control_applications_totnumber,input$pest_control_details,input$chemical_pest_control_equipment,input$pesticide_implement_make,input$pesticide_implement_model,input$pesticide_animal_traction,input$pesticide_humanPowered,input$pesticide_motorized_traction)
#
#





























