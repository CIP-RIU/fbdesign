metadata <- function(agronomic_crop_template,
                                  col_name= "Value",
                                  experimentId="",
                                  experimentName="",
                                  experimentProjectName="",
                                  startDate = "startDate",
                                  endDate = "endDate",
                                  Duration ="",
                                  typeExperiment="",
                                  experimentObj="",

                                  fundAgenType = "", #NEW #array
                                  fundName_1="",
                                  fundName_2="",
                                  fundName_3="",
                                  fundName_4="",
                                  fundName_5="",
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

  if(is.null(experimentId)){experimentId <-  ""}
  if(is.null(experimentName)){experimentName <-  ""}
  if(is.null(experimentProjectName)){experimentProjectName <-  ""}
  if(is.null(startDate )){startDate  <-  ""}
  if(is.null(endDate )){endDate  <-  ""}
  if(is.null(Duration )){Duration  <-  ""}
  if(is.null(typeExperiment)){typeExperiment <-  ""}
  if(is.null(experimentObj)){experimentObj <-  ""}
  if(is.null(fundAgenType )){fundAgenType  <-  ""}
  if(is.null(fundName)){fundName <-  ""}
  if(is.null(contCenter)){contCenter <-  ""}
  if(is.null(contCRP)){contCRP <-  ""}
  if(is.null(contResearcher)){contResearcher <-  ""}
  if(is.null(fundLeadAgency)){fundLeadAgency <-  ""}
  if(is.null(leadName)){leadName <-  ""}
  if(is.null(npersonnel)){npersonnel <-  ""}
  if(is.null(personnel1Type)){personnel1Type <-  ""}
  if(is.null(person1FirstName)){person1FirstName <-  ""}
  if(is.null(person1LastName)){person1LastName <-  ""}
  if(is.null(person1Email)){person1Email <-  ""}
  if(is.null(person1Afiliation)){person1Afiliation <-  ""}
  if(is.null(person1ORCID)){person1ORCID <-  ""}
  if(is.null(personnel2Type)){personnel2Type <-  ""}
  if(is.null(person2FirstName)){person2FirstName <-  ""}
  if(is.null(person2LastName)){person2LastName <-  ""}
  if(is.null(person2Email)){person2Email <-  ""}
  if(is.null(person2Afiliation)){person2Afiliation <-  ""}
  if(is.null(person2ORCID)){person2ORCID <-  ""}
  if(is.null(personnel3Type)){personnel3Type <-  ""}
  if(is.null(person3FirstName)){person3FirstName <-  ""}
  if(is.null(person3LastName)){person3LastName <-  ""}
  if(is.null(person3Email)){person3Email <-  ""}
  if(is.null(person3Afiliation)){person3Afiliation <-  ""}
  if(is.null(person3ORCID)){person3ORCID <-  ""}
  if(is.null(personnel4Type)){personnel4Type <-  ""}
  if(is.null(person4FirstName)){person4FirstName <-  ""}
  if(is.null(person4LastName)){person4LastName <-  ""}
  if(is.null(person4Email)){person4Email <-  ""}
  if(is.null(person4Afiliation)){person4Afiliation <-  ""}
  if(is.null(person4ORCID)){person4ORCID <-  ""}
  if(is.null(personnel5Type)){personnel5Type <-  ""}
  if(is.null(person5FirstName)){person5FirstName <-  ""}
  if(is.null(person5LastName)){person5LastName <-  ""}
  if(is.null(person5Email)){person5Email <-  ""}
  if(is.null(person5Afiliation)){person5Afiliation <-  ""}
  if(is.null(person5ORCID)){person5ORCID <-  ""}
  if(is.null(sytpetype )){sytpetype  <-  ""}
  if(is.null(syteName )){syteName  <-  ""}
  if(is.null(siteID)){siteID <-  ""}
  if(is.null(countryName)){countryName <-  ""}
  if(is.null(villageName)){villageName <-  ""}
  if(is.null(nearestPopupPlace )){nearestPopupPlace  <-  ""}
  if(is.null(inHighLevel)){inHighLevel <-  ""}
  if(is.null(inSiteVegetation)){inSiteVegetation <-  ""}
  if(is.null(inSiteDescNotes)){inSiteDescNotes <-  ""}
  if(is.null(croppingType)){croppingType <-  ""}
  if(is.null(cropCommonNameMono)){cropCommonNameMono <-  ""}
  if(is.null(cropLatinNameMono)){cropLatinNameMono <-  ""}
  if(is.null(cropVarietyNameMono)){cropVarietyNameMono <-  ""}
  if(is.null(cultivarNameMono)){cultivarNameMono <-  ""}
  if(is.null(monoCropLocalName)){monoCropLocalName <-  ""}
  if(is.null(numPreviousCrop)){numPreviousCrop <-  ""}
  if(is.null(prevCropName )){prevCropName  <-  ""}
  if(is.null(prevCropVar )){prevCropVar  <-  ""}
  if(is.null(subject)){subject <-  ""}
  if(is.null(keywords)){keywords <-  ""}
  if(is.null(Embargo_date)){Embargo_date <-  ""}

  #### funding agncy type
  nTypes <- length(fundAgenType)
  aux <- c(fundName_1, fundName_2, fundName_3, fundName_4, fundName_5)
  aux2 <- c(fund_1, fund_2, fun_3, fund_4, fund_5)
  aux3 <- c(fund_1, fund_2, fun_3, fund_4, fund_5)
  aux3 <- c(fund_1, fund_2, fun_3, fund_4, fund_5)
  var <- ""
  for (i in 1:nTypes) {
    xtypes <- paste0(var, ", ", aux[i])
    xnames <- paste0(var, ", ", aux2[])
    xcenter <- paste0(var, ", ", aux3[i])
    xcrp <- paste0(var, ", ", aux4[i])

  }
  fundAgenType <- xtypes
  fundName <- xnames
  contCenter <-xcenter
  contCRP <- xcrp



  c1 <- c('Experiment ID', experimentId)
  c2 <- c('Experiment name', experimentName )
  c3 <- c('Experiment project name', experimentProjectName)
  c4 <- c('Experiment start date', paste(startDate) )
  c5 <- c('Experiment end date', paste(endDate))
  c6 <- c('Experiment duration', Duration)
  c7 <- c('Type of experiment', typeExperiment)
  c8 <- c('Experiment objective', experimentObj)


  c9 <- c('Funding agency type', fundAgenType)
  c10 <- c('Funding agency name', fundName)
  c11 <- c('Number of project management entities',contCenter )
  c12 <- c('Project management entity',contCRP )
  c13 <- c('Contribuitor center', )
  c14 <- c('Contribuitor CRP', )
  c15 <- c('Project management entity name', )
  c16 <- c('Experiment, lead organization type', )
  c17 <- c('Experiment lead person / Primary Investigator', )
  c18 <- c('Experiment, lead organization name', )
  c19 <- c('Person type', )
  c20 <- c('Person, first name', )
  c21 <- c('Person, last name', )
  c22 <- c('Person, email', )
  c23 <- c('Person, affiliation', )
  c24 <- c('Person, ORCID', )
  c25 <- c('Country in which active', )
  c26 <- c('Person 2 type', )
  c27 <- c('Person 2, first name', )
  c28 <- c('Person 2, last name', )
  c29 <- c('Person 2, email', )
  c30 <- c('Person 2, affiliation', )
  c31 <- c('Person 2, ORCID', )
  c32 <- c('Country in which active', )
  c33 <- c('Site type', )
  c34 <- c('Site name', )
  c35 <- c('Site ID', )
  c36 <- c('Country name', )
  c37 <- c('Site, first-level administrative division name', )
  c38 <- c('Site, second-level administrative division name', )
  c39 <- c('Village name', )
  c40 <- c('Nearest populated place', )
  c41 <- c('Site elevation', )
  c42 <- c('Site latitude (in decimal degrees)', )
  c43 <- c('Site longitude (in decimal degrees)', )
  c44 <- c('Higher-level landform', )
  c45 <- c('Vegetation surrounding the experimental site', )
  c46 <- c('Site description notes', )
  c47 <- c('Cropping type', )
  c48 <- c('Crop common name', )
  c49 <- c('Crop latin name', )
  c50 <- c('Crop variety name', )
  c51 <- c('Cultivar name', )
  c52 <- c('Crop local name', )
  c53 <- c('Number of previous crop', )
  c54 <- c('Previous crop name', )
  c55 <- c('Previous crop variety', )
  c56 <- c('Subject', )
  c57 <- c('Keywords', )
  c58 <- c('Embargo end date', )

}

metadata2 <- function(planting_start_date,planting_end_date,planting_directSeeding,
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


  pl1	<-	c('','Planting method','Operation start date', paste(planting_start_date))
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
