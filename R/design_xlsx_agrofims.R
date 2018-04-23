#' Create metadata for hidap-agrofims
#' @export

add_metadata_agrofims <- function(agronomic_crop_template, col_name= "Value", experimentId="",
                         experimentName="",
                         experimentProjectName="",
                         startDate = "startDate",
                         endDate = "endDate",
                         Duration ="",
                         designFieldbook_typeExperiment="",
                         experimentObj="",
                         fundName="",
                         personnel1Type="",
                         person1FirstName="",
                         designFieldbook_fundLeadAgency="",
                         leadName="", person1LastName="",
                         person1Email="",
                         person1Afiliation="",
                         person1ORCID="",
                         instAbreviation="",
                         contCenter="",
                         contCRP="",
                         contResearcher="",
                         sytpetype ="",
                         syteName ="",
                         siteID="", countryName="", admin1="", admin2="",
                         villageName="", elevation ="", latitude="",
                         longitude="", descNotes="", croppingType, cropLatinNameMono="",
                         cropCommonNameMono="", cultivarNameMono="", cropVarietyNameMono="",
                         subject="", keywords="", Embargo_date=""){

  Metadata <- agronomic_crop_template
  #ToDo: cambiar en el template por minusculas las letras siguientes
  names(Metadata) <- c ("Variable", "Value")

  startDate <- startDate
  endDate <- endDate
  Duration <- Duration

  path <- fbglobal::get_base_dir()
  #print(path)
  geodb_file <- "table_sites.rds"
  #path <- paste(path, geodb_file, sep = "\\")
  path <- file.path(path, geodb_file)
  sites_data <-  readRDS(file = path)

  geodata <- fbsites::filter_geodata(sites_data = sites_data,
                                     country_input = countryName,
                                     trial_site_abbr = villageName)

  if(is.null(cropVarietyNameMono)){cropVarietyNameMono <-""}

  print(geodata)

  Metadata[Metadata$Variable== 'Experiment ID', col_name]	<-	"fbagrofims_001" #paste(experimentId)
  Metadata[Metadata$Variable== 'Experiment name', col_name]	<-	paste(experimentName)
  Metadata[Metadata$Variable== 'Experiment project name', col_name]	<-	paste(experimentProjectName)
  Metadata[Metadata$Variable== 'Experiment starting date', col_name]	<-	paste(startDate)
  Metadata[Metadata$Variable== 'End date', col_name]	<-	paste(endDate)
  Metadata[Metadata$Variable== 'Experiment duration', col_name]	<-	Duration #paste(fbDesign_project_time_line[2] - fbDesign_project_time_line[1])
  Metadata[Metadata$Variable== 'Type of experiment', col_name]	<-	paste(designFieldbook_typeExperiment)
  Metadata[Metadata$Variable== 'Experiment objective', col_name]	<-	paste(experimentObj)
  Metadata[Metadata$Variable== 'Funding agency name', col_name]	<-	paste(fundName)
  Metadata[Metadata$Variable== 'Person type', col_name]	<-	paste(personnel1Type)
  Metadata[Metadata$Variable== 'Experiment lead, first name', col_name]	<-	paste(person1FirstName)
  Metadata[Metadata$Variable== 'Experiment, lead organization type', col_name]	<-	paste(designFieldbook_fundLeadAgency)
  Metadata[Metadata$Variable== 'Experiment, lead organization name', col_name]	<-	paste(leadName)
  Metadata[Metadata$Variable== 'Experiment lead, last name', col_name]	<-	paste(person1LastName)
  Metadata[Metadata$Variable== 'Person, email', col_name]	<-	paste(person1Email)
  Metadata[Metadata$Variable== 'Person, affiliation', col_name]	<-	paste(person1Afiliation)
  Metadata[Metadata$Variable== 'Person, ORCID', col_name]	<-	paste(person1ORCID)
  Metadata[Metadata$Variable== 'Insitutional Identifier/Abbreviation', col_name]	<-	""
  Metadata[Metadata$Variable== 'Contributor center', col_name]	<-	paste(contCenter)
  Metadata[Metadata$Variable== 'Contributor CRP', col_name]	<-	paste(contCRP)
  Metadata[Metadata$Variable== 'Contributor researcher', col_name]	<-	paste(contResearcher)
  Metadata[Metadata$Variable== 'Site type', col_name]	<-	""
  Metadata[Metadata$Variable== 'Site name', col_name]	<-	""
  Metadata[Metadata$Variable== 'Site ID', col_name]	<-	""
  Metadata[Metadata$Variable== 'Country name', col_name]	<-	paste(countryName)
  Metadata[Metadata$Variable== 'Site, first-level administrative division name', col_name]	<-	paste(as.character(geodata$adm1))
  Metadata[Metadata$Variable== 'Site, second-level administrative division name', col_name]	<-	paste(as.character(geodata$adm2))
  Metadata[Metadata$Variable== 'Village name', col_name]	<-	paste(villageName)
  Metadata[Metadata$Variable== 'Site elevation', col_name]	<-	paste(as.character(geodata$elev))
  Metadata[Metadata$Variable== 'Site latitude (in decimal degrees)', col_name]	<-	paste(as.character(geodata$latd))
  Metadata[Metadata$Variable== 'Site longitude (in decimal degrees)', col_name]	<-	paste(as.character(geodata$lond))
  Metadata[Metadata$Variable== 'Site description notes', col_name]	<-	""
  Metadata[Metadata$Variable== 'Cropping type', col_name]	<-	paste(croppingType)
  Metadata[Metadata$Variable== 'Crop Latin name', col_name]	<-	paste(cropLatinNameMono)
  Metadata[Metadata$Variable== 'Crop common name', col_name]	<-	paste(cropCommonNameMono)
  Metadata[Metadata$Variable== 'Crop local name', col_name]	<-	paste(cultivarNameMono)
  Metadata[Metadata$Variable== 'Crop variety name', col_name]	<-	paste(cropVarietyNameMono,collapse = ",")
  Metadata[Metadata$Variable== 'Subject', col_name]	<-	""
  Metadata[Metadata$Variable== 'Keywords', col_name]	<-	""
  Metadata[Metadata$Variable== 'Embargo end date', col_name]	<-	paste(Embargo_date)

  Metadata

}


add_installation_agrofims <- function(agronomic_crop_template, col_name = "Value",
                             designFieldbook_agrofims="",
                             designFieldbook_agrofims_r="",
                             numPlantsPerPlot="",
                             numRowsPerPlot="",
                             numPlantsPerRow="",
                             plotSize="",
                             spaceBwPlants="",
                             spaceBwRows="",
                             planDensity="",
                             plotSpacing="",
                             rowOrientation="",
                             hillSpacing="",
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



  Installation[Installation$Variable=='Experimental_design' ,col_name]	<-	experimental_design_label(designFieldbook_agrofims)
  Installation[Installation$Variable=='Experimental design abbreviation' ,col_name]	<-	paste(designFieldbook_agrofims )
  Installation[Installation$Variable=='Number of replications or blocks' ,col_name]	<-	paste(designFieldbook_agrofims_r )
  Installation[Installation$Variable=='Number of plants planted per plot' ,col_name]	<-	paste(numPlantsPerPlot )
  Installation[Installation$Variable=='Number of rows per plot' ,col_name]	<-	paste(numRowsPerPlot )
  Installation[Installation$Variable=='Number of plants per row' ,col_name]	<-	paste(numPlantsPerRow )
  Installation[Installation$Variable=='Plot size (m2)' ,col_name]	<-	paste(plotSize )
  Installation[Installation$Variable=='Space between plants (m)' ,col_name]	<-	paste(spaceBwPlants )
  Installation[Installation$Variable=='Space between rows (m)' ,col_name]	<-	paste(spaceBwRows )
  Installation[Installation$Variable=='Planting density (plants/Ha)' ,col_name]	<-	paste(planDensity )
  Installation[Installation$Variable=='Plot spacing' ,col_name]	<-	paste(plotSpacing )
  Installation[Installation$Variable=='Row orientation' ,col_name]	<-	paste(rowOrientation )
  Installation[Installation$Variable=='Hill spacing' ,col_name]	<-	paste(hillSpacing )

  #factor 1
  Installation[Installation$Variable=='Factor1_name' ,col_name]	<-	paste(factor_hdafims1)
  Installation[Installation$Variable=='Factor1_name_1' ,col_name] <- paste(lvl1_hdafims1)
  Installation[Installation$Variable=='Factor1_name_2' ,col_name] <- paste(lvl2_hdafims1)
  Installation[Installation$Variable=='Factor1_name_3' ,col_name] <- paste(lvl3_hdafims1)
  Installation[Installation$Variable=='Factor1_name_4' ,col_name] <- paste(lvl4_hdafims1)
  Installation[Installation$Variable=='Factor1_name_5' ,col_name] <- paste(lvl5_hdafims1)

  #factor 2
  Installation[Installation$Variable=='Factor2_name',  col_name]	<-	paste(factor_hdafims2)
  Installation[Installation$Variable=='Factor2_name_1' ,col_name] <- paste(lvl1_hdafims2)
  Installation[Installation$Variable=='Factor2_name_2' ,col_name] <- paste(lvl2_hdafims2)
  Installation[Installation$Variable=='Factor2_name_3' ,col_name] <- paste(lvl3_hdafims2)
  Installation[Installation$Variable=='Factor2_name_4' ,col_name] <- paste(lvl4_hdafims2)
  Installation[Installation$Variable=='Factor2_name_5' ,col_name] <- paste(lvl5_hdafims2)

  #factor 3
  Installation[Installation$Variable=='Factor3_name',   col_name]	<-	paste(factor_hdafims3)
  Installation[Installation$Variable=='Factor3_name_1' ,col_name] <- paste(lvl1_hdafims3)
  Installation[Installation$Variable=='Factor3_name_2' ,col_name] <- paste(lvl2_hdafims3)
  Installation[Installation$Variable=='Factor3_name_3' ,col_name] <- paste(lvl3_hdafims3)
  Installation[Installation$Variable=='Factor3_name_4' ,col_name] <- paste(lvl4_hdafims3)
  Installation[Installation$Variable=='Factor3_name_5' ,col_name] <- paste(lvl5_hdafims3)

  #factor 4
  Installation[Installation$Variable=='Factor4_name' ,  col_name]	<-	paste(factor_hdafims4)
  Installation[Installation$Variable=='Factor4_name_1' ,col_name] <- paste(lvl1_hdafims4)
  Installation[Installation$Variable=='Factor4_name_2' ,col_name] <- paste(lvl2_hdafims4)
  Installation[Installation$Variable=='Factor4_name_3' ,col_name] <- paste(lvl3_hdafims4)
  Installation[Installation$Variable=='Factor4_name_4' ,col_name] <- paste(lvl4_hdafims4)
  Installation[Installation$Variable=='Factor4_name_5' ,col_name] <- paste(lvl5_hdafims4)

  #factor 5
  Installation[Installation$Variable=='Factor5_name',  col_name]	<-	paste(factor_hdafims5)
  Installation[Installation$Variable=='Factor5_name_1' ,col_name] <- paste(lvl1_hdafims5)
  Installation[Installation$Variable=='Factor5_name_2' ,col_name] <- paste(lvl2_hdafims5)
  Installation[Installation$Variable=='Factor5_name_3' ,col_name] <- paste(lvl3_hdafims5)
  Installation[Installation$Variable=='Factor5_name_4' ,col_name] <- paste(lvl4_hdafims5)
  Installation[Installation$Variable=='Factor5_name_5' ,col_name] <- paste(lvl5_hdafims5)

  Installation
}






