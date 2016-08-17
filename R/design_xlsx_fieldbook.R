
#' Add values to Minimal Sheet Data
#'
#' @param file The file name of excel fieldbook
#' @param crop_template The crop template used to create Hidap Fieldbook
#' @param col_name The col name to paste values. By defaul is "Values"
## @param short_name The name of the excel fieldbook #deprecated for Trail_name
#' @param Trial_name The name of the excel fieldbook
#' @param crop The name of the crop
#' @param type_trial The type of trial
#' @param begin_date The begin date of the experiment
#' @param end_date The end date of the experiment
#' @param site_short_name The short name of the site
#' @param country The country name.
#' @description This function allow to paste values in the Minimal Excel (Fieldbook) Sheet
#' @export

#add_minimal_sheet <- function(file=NA, crop_template=NA, col_name="Value", short_name=NA, crop=NA,type_trial=NA,
 add_minimal_sheet <- function(file=NA, crop_template=NA, col_name="Value", Trial_name=NA, crop=NA,type_trial=NA,
                              begin_date=NA, end_date=NA, site_short_name=NA, country=NA){

  Minimal <- crop_template$Minimal

  #Minimal[Minimal$Factor=="Short_name",col_name] <- paste(isolate(short_name)) #old var for DC and early vrs of HD
  Minimal[Minimal$Factor=="Trial_name",col_name] <- paste(isolate(Trial_name)) #new var for DataVerse


  Minimal[Minimal$Factor=="Affiliation",col_name] <- paste("International Potato Center")
  Minimal[Minimal$Factor=="Identifier" , col_name] <- paste("CIP")
  Minimal[Minimal$Factor=="Contact_Affiliation" , col_name] <- paste("International Potato Center")

  Minimal[Minimal$Factor=="Crop",col_name] <- paste(crop)
  Minimal[Minimal$Factor=="Type_of_Trial",col_name] <- paste(type_trial)
  Minimal[Minimal$Factor=="Language",col_name] <- paste("English")


  #sites_data <- fbsites::get_site_table() #before
  sites_data <- site_table #extract from package

  Minimal[Minimal$Factor=="Begin_date",col_name] <- paste(as.character(begin_date))
  Minimal[Minimal$Factor=="End_date",col_name] <- paste(as.character(end_date))
  Minimal[Minimal$Factor=="Format",col_name] <- paste(as.character("xlsx"))

  Minimal[Minimal$Factor=="Software_name",col_name] <- paste(as.character("HIDAP"))


  Minimal[Minimal$Factor=="Site_short_name",col_name] <- paste(site_short_name)
  Minimal[Minimal$Factor=="Country",col_name] <- paste(country)

  geodata <- fbsites::filter_geodata(sites_data = sites_data,
                                     country_input = country,
                                     trial_site_abbr = site_short_name)
  print(geodata)
  #Minimal[Minimal$Factor=="Agroecological_zone",col_name] <- paste(as.character(geodata$aez))
  Minimal[Minimal$Factor=="CIP_Region",col_name] <- paste(as.character(geodata$creg))
  Minimal[Minimal$Factor=="Continent",col_name] <- paste(as.character(geodata$cont))
  Minimal[Minimal$Factor=="Country",col_name] <- paste(as.character(geodata$cntry))
  Minimal[Minimal$Factor=="Admin1",col_name] <- paste(as.character(geodata$adm1))
  Minimal[Minimal$Factor=="Admin2",col_name] <- paste(as.character(geodata$adm2))
  Minimal[Minimal$Factor=="Admin3",col_name] <- paste(as.character(geodata$adm3))
  Minimal[Minimal$Factor=="Locality",col_name] <- paste(as.character(geodata$local))
  Minimal[Minimal$Factor=="Elevation",col_name] <- paste(as.character(geodata$elev))
  Minimal[Minimal$Factor=="Latitude",col_name] <- paste(as.character(geodata$latd))
  Minimal[Minimal$Factor=="Longitude",col_name] <- paste(as.character(geodata$lond))

  crop_template$Minimal <- Minimal

  wb <- loadWorkbook(file)

  openxlsx::addWorksheet(wb, "Minimal")
  setColWidths(wb, sheet = "Minimal", cols = 1:200, widths = "auto")

  #headerStyle <- createStyle(fontSize = 14, wrapText = TRUE, fontColour = "#FFFFFF", halign = "center",
  #                           fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")
  headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
  openxlsx::writeDataTable(wb, "Minimal",x = crop_template$Minimal, withFilter = TRUE,headerStyle = headerStyle)
  saveWorkbook(wb, file = file , overwrite = TRUE)

}


#' Add values to Installation Sheet Data
#'
#' @param file The file name of excel fieldbook
#' @param crop_template The crop template used to create Hidap Fieldbook
#' @param col_name The col name to paste values. By defaul is "Values"
#' @param exp_design The name of the excel fieldbook
#' @param genetic_design The genetic design
#' @param rep The number of repetition
#' @param block The number of block
#' @param exp_env The environment of the experiment
#' @param plot_start_number The plot start number
#' @param n_plant_plot The number of plant per plot
#' @param n_plant_row The number of plant per row
#' @param plot_size The number plant per row
#' @param plant_density The plant density
#' @param distance_plants The distance between plants
#' @param distance_rows The distance between rows
#' @param factor_name The name of factor
#' @param factor_name_1 The first level of the factor
#' @param factor_name_2 The second level of the factor
#' @param factor_name_3 The third level of the factor
#' @param factor_name_4 The fourth level of the factor
#' @param factor_name_5 The fifth level of the factor
#' @description This function allows to paste values in the Installation Excel (Fieldbook) Sheet
#' @export


add_installation_sheet <- function(file=NA, crop_template=NA, col_name="Value",exp_design=NA,genetic_design=NA,
                                   rep=NA,
                                   block=NA,
                                   exp_env=NA,
                                   plot_start_number=NA,
                                   n_plant_plot=NA,
                                   n_plant_row=NA,
                                   plot_size=NA,
                                   plant_density=NA,
                                   distance_plants=NA,
                                   distance_rows=NA,
                                   factor_name=NA,
                                   factor_name_1=NA,
                                   factor_name_2=NA,
                                   factor_name_3=NA,
                                   factor_name_4=NA,
                                   factor_name_5=NA
                                   ){

  Installation <- crop_template$Installation
  #ToDo: Fix the plot and plant_density values when users do not intereract with Installation
  if(length(plot_size)==0) plot_size <- 2.7
  if(length(plant_density)==0) plot_size <- 37037.04

  #Installation[Installation$Factor=="Experimental_design",col_name] <- paste(exp_design)
  Installation[Installation$Factor=="Experimental_design_abbreviation",col_name] <- paste(exp_design)
  Installation[Installation$Factor=="Experimental_design",col_name] <- experimental_design_label(exp_design)
  Installation[Installation$Factor=="Labels_for_factor_genotypes",col_name] <- paste("Institutional number")
  Installation[Installation$Factor=="Block_size_(applicable_for_BIBD_only)",col_name] <- paste("NA")
  Installation[Installation$Factor=="Number_of_repetitions_or_blocks",col_name] <- paste(rep)
  Installation[Installation$Factor=="Block_number",col_name] <- paste(rep)
  Installation[Installation$Factor=="Experimental_Environment",col_name] <- paste(exp_env)
  Installation[Installation$Factor=="Plot_start_number",col_name] <- paste("NA")
  Installation[Installation$Factor=="Number_of_plants_planted_per_plot",col_name] <- paste(n_plant_plot)
  Installation[Installation$Factor=="Number_of_plants_per_sub-plot",col_name] <- paste("NA")
  Installation[Installation$Factor=="Number_of_rows_per_plot",col_name] <- paste("NA")
  Installation[Installation$Factor=="Number_of_rows_per_sub-plot",col_name] <-paste("NA")
  Installation[Installation$Factor=="Number_of_plants_per_row",col_name] <- paste(n_plant_row)
  Installation[Installation$Factor=="Plot_size_(m2)",col_name] <- paste(plot_size)
  Installation[Installation$Factor=="Distance_between_plants_(m)",col_name] <- paste(distance_plants)
  Installation[Installation$Factor=="Distance_between_rows_(m)",col_name] <- paste(distance_rows)
  Installation[Installation$Factor=="Planting_density_(plants/Ha)",col_name] <- paste(plant_density)
  Installation[Installation$Factor=="Factor_name",col_name] <- paste(factor_name)
  Installation[Installation$Factor=="Factor_name_1",col_name] <- paste(factor_name_1)
  Installation[Installation$Factor=="Factor_name_2",col_name] <- paste(factor_name_2)
  Installation[Installation$Factor=="Factor_name_3",col_name] <- paste(factor_name_3)
  Installation[Installation$Factor=="Factor_name_4",col_name] <- paste(factor_name_4)
  Installation[Installation$Factor=="Factor_name_4",col_name] <- paste(factor_name_5)

  crop_template$Installation <- Installation

  wb <- loadWorkbook(file)

  openxlsx::addWorksheet(wb, "Installation")
  setColWidths(wb, sheet = "Installation", cols = 1:200, widths = "auto")

#   headerStyle <- createStyle(fontSize = 14, wrapText = TRUE, fontColour = "#FFFFFF", halign = "center",
#                              fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")
  headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
  openxlsx::writeDataTable(wb, "Installation",x = crop_template$Installation, withFilter = TRUE, headerStyle = headerStyle)
  saveWorkbook(wb, file = file , overwrite = TRUE)
}


#' Add values to Material_List Sheet Data
#'
#' @param file The file name of excel fieldbook
#' @param crop_template The crop template used to create Hidap Fieldbook
#' @param crop The name of the crop
#' @param material_list The list of genotypes or materials
#' @description This function allows to paste values into Material_List Excel (Fieldbook) Sheet
#' @export

add_material_sheet <- function(file=NA, crop_template=NA, crop, material_list){

  #Material_List <- crop_template$Material_List

##! Hidden code because we are using reactive expresion material_list()
  #fn <- file.path(fbglobal::fname_material_lists(crop), material_list)
#  print(fn)
  #load(fn)
  #Material_List <- table_materials

  Material_List <- material_list #the parameter of the function. It is NOT the material_list dataset from this package.
  crop_template$Material_List <- Material_List

  wb <- loadWorkbook(file)

  openxlsx::addWorksheet(wb, "Material_List",gridLines = TRUE)
  #headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
  setColWidths(wb, sheet = "Material_List", cols = 1:200, widths = "auto")
  headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
  openxlsx::writeDataTable(wb, "Material_List", x = crop_template$Material_List, colNames = TRUE, withFilter = FALSE, headerStyle = headerStyle)
  saveWorkbook(wb, file = file , overwrite = TRUE)

}

#' Add metadata to Weather and Soil Sheet Data
#'
#' @param file The file name of excel fieldbook
#' @param crop_template The crop template used to create Hidap Fieldbook
#' @param soil_input Logical. If soil input is selected (related to soil checkbox).
#' @param weather_input Logical. If weather input is selected (related to weather checkbox).
#' @description This function allows to add weather and soil sheet data into Fieldbook Excel files.
#' @export
#'

add_metadata_sheet <- function(file=NA, crop_template=NA, soil_input, weather_input){

  Soil_analysis <- crop_template$Soil_analysis
  Weather_data <- crop_template$Weather_data

  wb <- loadWorkbook(file)

  if((soil_input)){
     openxlsx::addWorksheet(wb, "Soil_analysis", gridLines = TRUE)
    #setColWidths(wb, sheet = "Soil_analysis", cols = 1:200, widths = "auto")
    headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
    openxlsx::writeDataTable(wb, "Soil_analysis",x = Soil_analysis, colNames = TRUE, withFilter = FALSE,headerStyle = headerStyle)

  }

  if((weather_input)){
     openxlsx::addWorksheet(wb, "Weather_data", gridLines = TRUE)
    #setColWidths(wb, sheet = "Weather_data", cols = 1:200, widths = "auto")
    headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
    openxlsx::writeDataTable(wb, "Weather_data",x = Weather_data, colNames = TRUE, withFilter = FALSE,headerStyle = headerStyle)
  }

  saveWorkbook(wb, file = file , overwrite = TRUE)

}


#' Add values to Crop_Management Sheet Data
#'
#' @param file The file name of excel fieldbook
#' @param crop_template The crop template used to create Hidap Fieldbook
#' @param crop The name of the crop
#' @param trait_list The input trait abbreviations using shinyTree
#' @description This function allows to paste values into Crop_Management Excel (Fieldbook) Sheet
#' @export
#'

add_cmanagment_sheet <- function(file=NA, crop_template=NA, crop=NA, trait_list=NA){

    cmanagment <- crop_template$Crop_management
    #tbl <- fbmodule::get_module_table(crop)
    if(crop == "potato"){tbl <- table_module_potato    }
    if(crop == "sweetpotato"){tbl <- table_module_sweetpotato }
    vars <- get_tree_value(tree_input_value = trait_list, crop_selected = crop)
    #new_cmanagmente_rows <- dplyr::filter(tbl,variable %in% vars) %>% select(.,variables_name) HiDAP v1.0 Built 2
    new_cmanagmente_rows <- dplyr::filter(tbl,ABBR %in% vars) %>% select(.,VAR)
    new_cmanagmente_rows <- mutate(new_cmanagmente_rows,"Intervention_category" = "Measurement")
    names(new_cmanagmente_rows) <- c("Intervention_type","Intervention_category")
    new_cmanagmente_rows <- as.data.frame(new_cmanagmente_rows)
    new_cmanagmente_rows <- new_cmanagmente_rows[c(2,1)]

    crop_template$Crop_management <- data.table::rbindlist(list(cmanagment,new_cmanagmente_rows),fill = TRUE)


    wb <- loadWorkbook(file)
    openxlsx::addWorksheet(wb, "Crop_management")
    setColWidths(wb, sheet = "Crop_management", cols = 1:200, widths = "auto")
    headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
    openxlsx::writeDataTable(wb, "Crop_management", x = crop_template$Crop_management, colNames = TRUE, withFilter = FALSE,headerStyle = headerStyle)
    saveWorkbook(wb, file = file , overwrite = TRUE)

}

#' Add values to Var_List Sheet Data
#'
#' @param file The file name of excel fieldbook
#' @param crop_template The crop template used to create Hidap Fieldbook
#' @param crop The name of the crop
#' @param trait_list The input trait abbreviations using shinyTree
#' @description This function allows to paste values into Crop_Management Excel (Fieldbook) Sheet
#' @export
#'

add_varlist_sheet <- function(file=NA, crop_template=NA, crop=NA, trait_list=NA){


    #tbl <- fbmodule::get_module_table(crop)
    #trait_list<-input$designFieldbook_traits
    #crop<-input$designFieldbook_crop
    if(crop == "potato"){tbl <- table_module_potato    }
    if(crop == "sweetpotato"){tbl <- table_module_sweetpotato }
    vars <- get_tree_value(tree_input_value = trait_list,crop_selected = crop)
    #tbl <- fbmodule::get_module_table(crop)

    if(crop == "potato"){tbl <- table_module_potato    }
    if(crop == "sweetpotato"){tbl <- table_module_sweetpotato }

    #var_list <- dplyr::filter(tbl,variable %in% vars) %>% select(.,variables_name,variable) HiDAP v1.0 built 2
    var_list <- dplyr::filter(tbl,ABBR %in% vars) %>% select(.,VAR,ABBR)
    var_list <- as.data.frame(var_list)
    names(var_list) <- c("Factor_Variables","Abbreviations")
    crop_template$Var_List <- var_list

    wb <- loadWorkbook(file)

    openxlsx::addWorksheet(wb, "Var_List", gridLines = TRUE)
    setColWidths(wb, sheet = "Var_List", cols = 1:200, widths = "auto")
#     headerStyle <- createStyle(fontSize = 14, wrapText = TRUE, fontColour = "#FFFFFF", halign = "center",
#                                fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")
    headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
    openxlsx::writeDataTable(wb, "Var_List",x = crop_template$Var_List, colNames = TRUE, withFilter = TRUE,headerStyle = headerStyle)
    saveWorkbook(wb, file = file , overwrite = TRUE)
}



#' Add values to Fieldbook Sheet Data
#'
#' @param file The ID of fieldbook excel
#' @param fieldbook A data.frame with the fieldbook data
#' @description This function allows to paste values into Fieldbook Excel Sheet
#' @export
#'

add_fieldbook_sheet <-function(file,fieldbook){

  wb <- createWorkbook()
  openxlsx::addWorksheet(wb, "Fieldbook",gridLines = TRUE)
  fn_xlsx <- file
  fieldbook_data <- fieldbook
  #fn_xlsx <- paste(fbdesign_id(),".xlsx",sep="")
  #headerStyle <- createStyle(fontSize = 14, wrapText = TRUE, fontColour = "#FFFFFF", halign = "center",
   #                          fgFill = "#4F81BD", border="TopBottom", borderColour = "#4F81BD")

  openxlsx::setColWidths(wb, sheet = "Fieldbook", cols = 1:200, widths = "auto")
  headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
  freezePane(wb, "Fieldbook" , firstActiveRow = 2, firstActiveCol = 4)
  openxlsx::writeDataTable(wb, "Fieldbook", x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
  saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)

}

###

#' Crop List
#' @description This function gives a crop list
#' @export
#'
  crops <- function(){
          chc <- list(`potato (PT)`="potato",`sweetpotato (SP)`="sweetpotato")
  }

  #' Country List
  #' @description This function gives a country list
  #' @export
  #'

  country <- function(){
            #sites_data <- fbsites::get_site_table()
            sites_data <- site_table
            cntry <- fbsites::get_country_list(sites_data = sites_data)
  }

  #' Study List
  #' @description This function gives a study list
  #' @export
  #'
  #'
  study <- function(){
   chc <- list(`Bulking (BM)`=  "Bulking", `Dormancy (DS)` = "Dormancy",
        `Late blight (LB)` = "Late blight", `Yield (YL)` =   "Yield", `Drought (DT)`= "Drought")
  }


#' Add values to Fieldbook Sheet Data
#'
#' @param material_list A data.frame with the material list data
#' @description This function extract the control genotipes
#' @export
#'
  #Is_control  <-  NULL
  #is_control <- function(material_list){
  is_control <- function(material_list){
    material_list <- as.data.frame(material_list)
    p <- material_list %>%
      dplyr::filter(., Is_control=="x" | Is_control=="X") %>%
      #dplyr::select_(., "Institutional_number")
       dplyr::select_(., "Accession_Number")
    p <- as.character(p[[1]])
  }

#' Experimental design Label
#' @param abbr_design The abbreviation of the experimental design
#' @description Indicates which label correspond to statistical design abbreviation

 experimental_design_label <- function(abbr_design = "RCBD"){

   abbr_design <- stringr::str_trim(abbr_design,side="both")
   if(abbr_design == "UNRD")   {out <- "Unreplicated Design with No Randomization (UNDR)"  }
   if(abbr_design == "RCBD")   {out <- "Randomized Complete Block Design (RCBD)"}
   if(abbr_design == "CRD")    {out <- "Completely Randomized Design (CRD)" }
   if(abbr_design == "ABD")    {out <-  "Augmented Block Design (ABD)"}
   if(abbr_design == "LSD")    {out <- "Latin Square Design (LSD)"}
   if(abbr_design == "SPCRD")  {out <- "Split Plot with Plots in CRD (SPCRD)"}
   if(abbr_design == "SPRCBD") {out <- "Split Plot with Plots in RCBD (SPRCBD)"}
   if(abbr_design == "SPLSD")  {out <- "Split Plot with Plots in LSD (SPLSD)"}
   if(abbr_design == "STRIP")  {out <- "Strip Plot Design (STRIP)"}
   if(abbr_design == "F2CRD")  {out <- "Factorial Two-Way Design in CRD (F2CRD)"}
   if(abbr_design == "F2RCBD")  {out <- "Factorial Two-Way Design in RCBD (F2RCBD)"}
   if(abbr_design == "AD")     {out <- "Alpha Design(0,1) (AD)"}
   out


   #ToDo: Si no hay ningun diseno, votar un error.

#   "Randomized Complete Block Design (RCBD)" <- "RCBD",
#   "Completely Randomized Design (CRD)" <- "CRD",
#   "Augmented Block Design (ABD)" <- "ABD",
#   "Latin Square Design (LSD)" <- "LSD",
#   "Split Plot with Plots in CRD (SPCRD)" <- "SPCRD",
#   "Split Plot with Plots in RCBD (SPRCBD)" <- "SPRCBD",
#   "Split Plot with Plots in LSD (SPLSD)" <- "SPLSD",
#   "Strip Plot Design (STRIP)" <- "STRIP",
#   "Factorial Two-Way Design in CRD (F2CRD)" <- "F2CRD",
#   "Factorial Two-Way Design in RCBD (F2RCBD)" <- "F2RCBD",
#   "Alpha Design (AD)" <- "AD"
  #  "Balanced Incomplete Block Design (BIBD)" <- "BIBD",
  #  "Graeco-Latin Design (GLD)" <- "GLD",
  #  "Youden Design (YD)" <- "YD",
  #  "Cyclic Design (CD)" <- "CD",
  #  "Lattice Design (LD)" <- "LD" ,
}

##############################

 #' Add values to Fieldbook Sheet Data
 #'
 #' @param file The ID of fieldbook excel
 #' @param genotypes A vector with genotypes to fill organoleptic forms
 #' @param big_sheet The sheet used in Special or Big Trials
 #' @param fieldbook A data.frame with the fieldbook data
 #' @description This function allows to paste values into Fieldbook Excel Sheet
 #' @export
 #'

 big_add_fieldbook_sheet <-function(file, genotypes, big_sheet, fieldbook, big_crop_template){

   #print(big_sheet)

   wb <- createWorkbook()

   for(i in 1:length(big_sheet)){
   #print(big_sheet[i])
   #print(genotypes)
   genotypes <- as.character(genotypes)
   openxlsx::addWorksheet(wb, big_sheet[i], gridLines = TRUE)
   fn_xlsx <- file

#    if(big_sheet[i]=="F1_selection_criteria"){
#      fieldbook_data <- big_crop_template$F1_selection_criteria
#    }
#    else {
#       fieldbook_data <- fieldbook[[i]]
#    }

   if(big_sheet[i]=="F1_selection_criteria"){
     fieldbook_data <- big_crop_template$F1_selection_criteria
     openxlsx::setColWidths(wb, sheet = big_sheet[i], cols = 1:500, widths = "auto")
     headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
     freezePane(wb, big_sheet[i], firstActiveRow = 2, firstActiveCol = 4)
     openxlsx::writeDataTable(wb, big_sheet[i], x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
     saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)
   }
   if(big_sheet[i]=="F6_organoleptic_mother"){
         x <- 1
         y <- 1
         fieldbook_data <- fieldbook[[i]]
         chunk <- 13
         n <- nrow(fieldbook_data)
         r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
         fieldbook_data_form <- split(fieldbook_data,r)

         for(j in 1:length(fieldbook_data_form)){

             hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                                 border = "TopBottomLeftRight", fontColour = "white")

#              hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
#                                 border = "Bottom", fontColour = "white")


             print(big_sheet[i])

             writeData(wb, "F6_organoleptic_mother", fieldbook_data_form[[j]], startRow = x, startCol = y, headerStyle =hs1 ,
                       borders = "all",  borderStyle = "thin")

           #writeDataTable(wb, "F6_organoleptic_mother",x =  fieldbook_data_form[[j]], startRow =x , startCol = y, tableStyle = "TableStyleMedium21")

           x <- x+14

         }
         saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)
   }
   if(big_sheet[i]=="F7_organoleptic_baby"){

     x <- 1
     y <- 1

     fieldbook_data <- fieldbook[[i]]
     chunk <- 13
     n <- nrow(fieldbook_data)
     r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
     fieldbook_data_form <- split(fieldbook_data,r)

     for(j in 1:length(fieldbook_data_form)){

       hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                                              border = "TopBottomLeftRight", fontColour = "white")

       #print(fieldbook_data_form[[j]])
       print(big_sheet[i])

       writeData(wb, "F7_organoleptic_baby", fieldbook_data_form[[j]], startRow = x, startCol = y, headerStyle = hs1 ,
                 borders = "all",  borderStyle = "thin" )
       #writeDataTable(wb, "F6_organoleptic_mother",x =  fieldbook_data_form[[j]], startRow =x , startCol = y, tableStyle = "TableStyleMedium21")

       x <- x+14

     }
     saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)
   }

   if(big_sheet[i]=="F2_select_clones_flowering"){

       print(big_sheet[i])
       fieldbook_data <- fieldbook[[i]]
       openxlsx::setColWidths(wb, sheet = big_sheet[i], cols = 1:500, widths = "auto")
       headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
       freezePane(wb, big_sheet[i], firstActiveRow = 2, firstActiveCol = 4)
       openxlsx::writeDataTable(wb, big_sheet[i], x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
       saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)

   }

   if(big_sheet[i]=="F3_select_clones_harvest" ){

     print(big_sheet[i])
     fieldbook_data <- fieldbook[[i]]
     openxlsx::setColWidths(wb, sheet = big_sheet[i], cols = 1:500, widths = "auto")
     headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
     freezePane(wb, big_sheet[i], firstActiveRow = 2, firstActiveCol = 4)
     openxlsx::writeDataTable(wb, big_sheet[i], x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
     saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)

   }

   if(big_sheet[i]=="F4_harvest_mother"){

     print(big_sheet[i])
     fieldbook_data <- fieldbook[[i]]
     openxlsx::setColWidths(wb, sheet = big_sheet[i], cols = 1:500, widths = "auto")
     headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
     freezePane(wb, big_sheet[i], firstActiveRow = 2, firstActiveCol = 4)
     openxlsx::writeDataTable(wb, big_sheet[i], x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
     saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)

   }

   if(big_sheet[i]=="F5_harvest_baby"){

     print(big_sheet[i])
     fieldbook_data <- fieldbook[[i]]
     openxlsx::setColWidths(wb, sheet = big_sheet[i], cols = 1:500, widths = "auto")
     headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
     freezePane(wb, big_sheet[i], firstActiveRow = 2, firstActiveCol = 4)
     openxlsx::writeDataTable(wb, big_sheet[i], x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
     saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)

   }

   if(big_sheet[i]=="F8_postharvest_dormancy"){

     print(big_sheet[i])
     fieldbook_data <- fieldbook[[i]]
     openxlsx::setColWidths(wb, sheet = big_sheet[i], cols = 1:500, widths = "auto")
     headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
     freezePane(wb, big_sheet[i], firstActiveRow = 2, firstActiveCol = 4)
     openxlsx::writeDataTable(wb, big_sheet[i], x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
     saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)

   }

   if(big_sheet[i]=="F9_postharvest_clones_storage"){

     print(big_sheet[i])
     fieldbook_data <- fieldbook[[i]]
     openxlsx::setColWidths(wb, sheet = big_sheet[i], cols = 1:500, widths = "auto")
     headerStyle <- createStyle(fontSize = 13,halign = "center",valign = "center")
     freezePane(wb, big_sheet[i], firstActiveRow = 2, firstActiveCol = 4)
     openxlsx::writeDataTable(wb, big_sheet[i], x = fieldbook_data, colNames = TRUE, withFilter = FALSE,headerStyle =  headerStyle)
     saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)

   }

   }

   #saveWorkbook(wb, file = fn_xlsx , overwrite = TRUE)
 }

# study <- function(){
#
#   if(crops()=="potato"){
#
#   out <- list(`yield (YL)` = "yield",
#   `late blight (LB)` =  "late blight",
#   `dormancy (DS)`= "dormancy",
#   `drought (DT)` = "drought",
#   `bulking (BM)`="bulking")
#   }
#
#   if(crops()=="sweetpotato"){
#     out <- list(`yield (YL)` = "yield",
#                 `Morphology (YL)` =  "morphology")
#
#   }
#
# out
#   #`dormancy (DS)`= "dormancy"
#   #`drought (DT)` = "drought"
# #   `participatory variety selection (PV)`
# #   "participatory variety selection"
# }

# sites <- function(fbDesign_countryTrial="Peru"){
#   sites_data <-  fbsites::get_site_table()
#   #fbsites::get_filter_locality(sites_data = sites_data, country_input= input$fbDesign_countryTrial)
#   fbsites::get_filter_locality(sites_data = sites_data, country_input= fbDesign_countryTrial)
# }


