#' server_design
#'
#' Design field book for HIDAP-AGROFIMS
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites
#' @export
#'

server_design_agrofims <- function(input, output, session, values){
  # mLand_levelling =list(
  #   Land_levelling_start_date = ' ' ,Land_levelling_end_date= '',Land_levelling_number_of_passes= '',Land_levelling_operations_order= '', Land_levelling_implement_picture= '', Land_levelling_implement_type = '', Land_leveling_implement_make= '', Land_leveling_implement_model= '', Land_leveling_implement_traction= ''
  # )
  # mPuddling= list(Penetrometer_in_field = ' ' ,Puddling_start_date= '',Puddling_end_date= '',Puddling_depth= '',Puddling_implement_picture= '',Puddling_implement_name= '',Puddling_implement_make= '',Puddling_implement_model= '',Puddling_implement_traction= ''
  # )
  # mTillage = list(Conventional_tillage = ' ' ,Mulch_till= '',No_till= '',Other_specify= '',Puddling= '',Reduced_tillage= '',Strip_till= '',Tillage_implement_picture= '',Tillage_implement= '',Implement_make= '',Implement_model= '',Implement_traction= ''
  # )
  # mLiming= ''
  # mMulching_and_residue_management=' '
  # mRhizobium_inoculation =' '
  # mPlanting_and_Seeding=' '
  # mNutrient_management_event=' '
  # mIrrigation_event=' '
  # mDisease_observation=' '
  # mPest_observation_and_control=' '
  # mHarvest=' '
  # mWeather_information=' '
  # mWater_table_and_quality=' '
  # mSoil_Measurement=' '
  # mCrop_measurement=' '
  #
  # featList <- list(Land_preparation = list(Land_levelling = mLand_levelling,
  #               Puddling=mPuddling, Tillage=mTillage,
  #               Liming = mLiming) ,
  #               Mulching_and_residue_management=mMulching_and_residue_management)

  featNames <- names(Agronomic_features$`Agronomic features`)

  #events for buttons next in tabs for fieldbook creation
  # TO BE OPTIMIZED
  observeEvent(input$btnNextPersonnelInfo, {
      updateTabsetPanel(session, "inExpInfo", selected = "tabPersonnel")
  })
  observeEvent(input$btnNextCropInfo, {
    updateTabsetPanel(session, "inExpInfo", selected = "tabCropInfo")
  })
  observeEvent(input$btnDesign, {
    updateTabsetPanel(session, "inExpInfo", selected = "tabDesign")
  })
  observeEvent(input$btnNextPlotInfo, {
    updateTabsetPanel(session, "inExpInfo", selected = "tabPlotInfo")
  })
  observeEvent(input$btnNextAgro, {
    updateTabsetPanel(session, "inExpInfo", selected = "tabAgroFeat")
  })
  observeEvent(input$btnNextTraits, {
    updateTabsetPanel(session, "inExpInfo", selected = "tabTraits")
  })
  observeEvent(input$btnNextEnv, {
    updateTabsetPanel(session, "inExpInfo", selected = "tabEnvironment")
  })

  ## Agronomic Features Shiny Tree ###############################################

  # observeEvent( input$selectFeatures, {
  #   flist <- input$selectFeatures
  #   i <- 1
  #   n <- length(flist)
  #   mfeatList <- vector("list", n)
  #   names(mfeatList) <- flist
  #   tree <- Agronomic_features[["Agronomic features"]]
  #   total<- list()
  #
  #   for (feat in flist) {
  #     total<- c(total, tree[feat])
  #     #names(mfeatList[[feat]]) <- feat
  #     # i <- i+1
  #   }
  #   total<- total
  #    #saveRDS(total, "total.rds")

    output$treeFeatures <- shinyTree::renderTree({

      #total <- readRDS("total.rds")
      total <- Agronomic_features

    })
    # output$treeFeatures <- shinyTree::renderTree({
    #
    #   Agronomic_features
    #
    #   })

  # }) End agronomic trait shinyTree  ####################################

  #observe for selectize of crops for intercropping ####################################
  observe({
    if(!is.null(input$cropsSelected)){
      l <- input$cropsSelected
      n <- length(input$cropsSelected)
      if(n>0){
        updateTextInput(session, "cropCommonName1",  value = l[[1]])
      }
      if(n>1){
        updateTextInput(session, "cropCommonName2",  value = l[[2]])
      }
      if(n>2){
        updateTextInput(session, "cropCommonName3",  value = l[[3]])
      }
    }
  })


  # Reactive table. Get material list table #################################################################
  material_table <-  shiny::reactive({

    if(input$select_import=="Template") {

      #mtl_temp <- input$file
      mtl_temp <- input$file_mtlist

      if(is.null(mtl_temp)){return()}
      if(!is.null(mtl_temp)){

        file.copy(mtl_temp$datapath,paste(mtl_temp$datapath, ".xlsx", sep=""))
        mtl_temp <- readxl::read_excel(paste(mtl_temp$datapath, ".xlsx", sep=""), sheet = "Material_List")

        mtl_list <- as.list(mtl_temp) #mtl in list format
      }


    }

    if(input$select_import=="Local List"){

      sel_list <- input$designFieldbook_sel_mlist
      #print(sel_list)
      if(is.null(sel_list) || sel_list == ""){  return()  }
      if(length(sel_list)>0){

        #Just use the original code
        mtl_temp <- readRDS(sel_list)


        #is_parent_list <- is_parentList(sel_list)
        if(is_parentList(sel_list)==TRUE){
          #Case: parental list (female and male)
          mtl_list <- mtl_temp
        }
        else{
          #Case: standard material list (genotypes)
          mtl_list <- as.list(mtl_temp) #mtl in list format
        }

      }

    }

    mtl_list

  })

  # Approval Box ######################################################################################################
  output$approvalBox <- renderInfoBox({

    #data.frame is the data structue for the clonal and family list. In the parental and family module, we save lists in data.frame format

    # plos <<- material_table()
    #
    # lsus <<-  get_type_list_ds(material_table())

    # plos <<- material_table()
    #
    # lsus <<-  get_type_list_ds(material_table())



    # if( is.null(material_table()) ){
    #
    #   title <- "Upload"
    #   subtitle <-   paste("your material list file. Or, press the button below to download and fill the template.")
    #   color <- "blue"
    #   icon <- "upload"
    #   lib <- "glyphicon"
    #   fill <- TRUE
    #   width <- NULL
    #
    #   # infoBox(title="Upload", subtitle=
    #   #           paste("your material list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
    #   #         color = "blue",fill = TRUE, width = NULL)
    #
    # }

    #parent list
    if( get_type_list_ds(material_table()) == "clonal" ) {

      #germoplasm <- germoplasm$Accesssion_Number

      germoplasm <-material_table()$Accession_Number
      #detect duplications
      germ_duplicates <- anyDuplicated(germoplasm)


      if(is.null(germoplasm)){

        title <- "Upload"
        subtitle <-   paste("your material list file. Or, press the button below to download and fill the template.")
        color <- "blue"
        icon <- "upload"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL

        # infoBox(title="Upload", subtitle=
        #           paste("your material list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
        #         color = "blue",fill = TRUE, width = NULL)


      }
      else if(all(is.na(germoplasm))) {

        #if(all(is.na(germoplasm))) {
        title <- "ERROR"
        subtitle <- paste("Your material list", "is empty. Please check it")
        color <- "red"
        icon <- "warning-sign"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL

        # infoBox(title="ERROR", subtitle=
        #           paste("Your material list", "is empty. Please check it"), icon = icon("warning-sign", lib = "glyphicon"),
        #         color = "red",fill = TRUE, width = NULL)


      }
      else if(germ_duplicates>0){
        title <- "ERROR"
        subtitle <- paste("Your material list has duplicated genotypes/germoplasm names. Please, enter a correct file.")
        color <- "red"
        icon <- "warning-sign"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL


      }  else {

        title <- "GREAT!"
        subtitle <-  paste(" was successfully uploaded!")
        color <- "green"
        icon <- "ok"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL

        # infoBox(title="GREAT!", subtitle =
        #           paste(" was successfully uploaded!"),  icon = icon("ok", lib = "glyphicon"),
        #         color = "green",fill = TRUE, width = NULL)
      }


    }

    #list is the data structure for parental list. In the parental module, we save lists in list format
    if( get_type_list_ds(material_table()) == "parental" ) {

      germoplasm_fem <-  material_table()$female$Accession_Number
      germoplasm_male <- material_table()$male$Accession_Number

      if(is.null(germoplasm_fem) && is.null(germoplasm_male)){


        title <- "Upload"
        subtitle <-  paste(" your parental list file. Or, press the button below to download and fill the template.")
        color <- "blue"
        icon <- "upload"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL

        # infoBox(title="Upload", subtitle=
        #           paste("your parental list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
        #           color = "blue",fill = TRUE, width = NULL)

      }

      else if(all(is.na(germoplasm_fem))) {

        #if(all(is.na(germoplasm_fem))) {

        title <- "ERROR"
        subtitle <-  paste("The female's accession numbers are empty.", "Please check female's accesion number column")
        color <- "red"
        icon <- "warning-sign"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL

        # infoBox(title="ERROR", subtitle=
        #           paste("The female's accession numbers are empty.", "Please check female's accesion number column"), icon = icon("warning-sign", lib = "glyphicon"),
        #           color = "red",fill = TRUE, width = NULL)

      }

      else if(all(is.na(germoplasm_male))) {

        title <- "ERROR"
        subtitle <-  paste("The male's accession numbers are empty")
        color <- "red"
        icon <- "warning-sign"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL

        # infoBox(title="ERROR", subtitle=
        #           paste("The male's accession numbers are empty.", "Please check male's accesion number column"), icon = icon("warning-sign", lib = "glyphicon"),
        #           color = "red",fill = TRUE, width = NULL)

      }

      else {

        title <- "GREAT!"
        subtitle <-  paste(" your parental list file was successfully uploaded!")
        color <- "green"
        icon <- "ok"
        lib <- "glyphicon"
        fill <- TRUE
        width <- NULL

      }

    }


    shinydashboard::infoBox(title=title, subtitle =subtitle,  icon = icon(icon, lib = lib),
                            color = color, fill = TRUE, width = NULL)


  }) #################################################################

  # Design of variables #################################################################
  output$fbDesign_variables <- shiny::renderUI({

    crop <- input$designFieldbook_crop

    if(crop == "potato"){tbl <- table_module_potato } #dataset from fbdesign data folder
    if(crop == "sweetpotato"){tbl <- table_module_sweetpotato } #dataset from fbdesgin data folder

    #mdl <- tbl[tbl$crop == crop, c("module", "module_name")] #HiDAP v1.0 Built_1 (deprecated table form)
    #Filter by crop and select trial abbreviation and trial.
    mdl <- tbl[tbl$CROP == crop, c("TRIAL_ABBR", "TRIAL")] #HiDAP v1.0 Built_2

    mdl <- paste0(mdl[,2], " (", mdl[, 1],")")
    mdl <- sort(unique(mdl))

    #ids <- unlist(stringr::str_extract_all(mdl, "([A-Z]{2})"))
    ids <- str_trim(gsub("\\(.*","", mdl), side = "both")
    vls <- mdl
    mdl <- as.list(ids)
    names(mdl) <- vls
    #mdl1 <<- mdl
    #print(mdl)
    #shiny::selectInput("designFieldbook_module", label = "Assay (fieldbook module):",
    shiny::selectInput("designFieldbook_module", label = "Type of trial",
                       choices = mdl, selected = 1)
  })

  # ID or name of the field book #################################################################
  fbdesign_id <- shiny::reactive({

    if (!is.null(input$designFieldbook_crop)) {
      #tbl = fbcrops::get_crop_table()

      tbl <- table_crops
      nExp_id <- input$fbDesign_nExp
      #print(nExp_id)
      crop_id    <- tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
      program_id <- input$designFieldbook_program
      phase_id   <-  input$designFieldbook_phase
      module_id  <- input$designFieldbook_module


      date_book <- input$fbDesign_project_time_line[1]
      date_book <- unlist(str_split(date_book,"-"))
      date_book <- paste(date_book[2],date_book[1],sep="")

      #sites = input$designFieldbook_sites
      sites <- stringr::str_trim(input$designFieldbook_sites, side="both")

      if(nExp_id=="-"){
        out <- paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites)

      } else {
        out <-  paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites, "_", nExp_id)
      }

      paste(out, collapse = ", ")
    }
  })

  # RenderText for displaying the book's name #################################################################
  output$fbDesign_id <- shiny::renderText({
    fbdesign_id()
  })


  # Get Trait #################################################################
  output$designFieldbook_traits_agrofims <- shinyTree::renderTree({

    a<- agronomic_trait_list #data from fbdesign for hidap-agrofims

  })

  output$designFieldbook_traits_hdagrofims <- shinyTree::renderTree({
    list(
      # root1 = "",
      Agronomic_features = list(
        Groups = list(
          Land_preparation= list(
            Land_levelling =list(
              Land_levelling_start_date = ' ' ,Land_levelling_end_date= '',Land_levelling_number_of_passes= '',Land_levelling_operations_order= '', Land_levelling_implement_picture= '', Land_levelling_implement_type = '', Land_leveling_implement_make= '', Land_leveling_implement_model= '', Land_leveling_implement_traction= ''
            ),
            Puddling= list(Penetrometer_in_field = ' ' ,Puddling_start_date= '',Puddling_end_date= '',Puddling_depth= '',Puddling_implement_picture= '',Puddling_implement_name= '',Puddling_implement_make= '',Puddling_implement_model= '',Puddling_implement_traction= ''
),
            Tillage = list(Conventional_tillage = ' ' ,Mulch_till= '',No_till= '',Other_specify= '',Puddling= '',Reduced_tillage= '',Strip_till= '',Tillage_implement_picture= '',Tillage_implement= '',Implement_make= '',Implement_model= '',Implement_traction= ''
),
            Liming= ''),
          Mulching_and_residue_management=' ' , Rhizobium_inoculation=' ' , Planting_and_Seeding=' ' , Nutrient_management_event=' ' , Irrigation_event=' ' , Disease_observation=' ' , Pest_observation_and_control=' ' , Harvest=' ' , Weather_information=' ' , Water_table_and_quality=' ' , Soil_Measurement=' ' , Crop_measurement=' '
)
        # SubListB = list(leafA = "", leafB = "")
      )
    )
  })


  # Desing of Experiment deprecated

  # shiny::observeEvent(input$btnExport_fbdesign_agrofims, {
  #
  #   factor_name1 <- gsub("\\s+", replacement = "-" , input$factor_hdafims1) %>% stringr::str_trim(.,side = "both")
  #   factor_lvl1 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims1) %>% stringr::str_trim(.,side = "both")
  #
  #   factor_name2 <- gsub("\\s+", replacement = "-" , input$factor_hdafims2) %>% stringr::str_trim(.,side = "both")
  #   factor_lvl2 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims2) %>% stringr::str_trim(.,side = "both")
  #
  #   factor_name3 <- gsub("\\s+", replacement = "-" , input$factor_hdafims3) %>% stringr::str_trim(.,side = "both")
  #   factor_lvl3 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims3) %>% stringr::str_trim(.,side = "both")
  #
  #   factor_name4 <- gsub("\\s+", replacement = "-" , input$factor_hdafims4) %>% stringr::str_trim(.,side = "both")
  #   factor_lvl4 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims4) %>% stringr::str_trim(.,side = "both")
  #
  #   factor_name5 <- gsub("\\s+", replacement = "-" , input$factor_hdafims5) %>% stringr::str_trim(.,side = "both")
  #   factor_lvl5 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims5) %>% stringr::str_trim(.,side = "both")
  #
  #   nfactors <- as.numeric(input$nfactors_hdafims)
  #
  #   # print(factor_lvl1)
  #   # print(factor_lvl2)
  #   # print(factor_lvl3)
  #   # print(factor_lvl4)
  #   # print(factor_lvl5)
  #
  #   if(nfactors == 1){ #1 factor
  #     fb <- st4gi::cd.cr(geno = FA, nrep = 3, nc = 3)
  #
  #   } else if(nfactors == 2){ #2 factores
  #
  #     FA <-  factor_lvl1
  #     FB <-  factor_lvl2
  #     fb <- cd.factorial(A = FA, B = FB, design = "crd", nrep = 3,  nc = 3)
  #
  #   } else if(nfactors == 3){  #3 factores
  #     #
  #     FA <-  factor_lvl1
  #     FB <- factor_lvl2
  #     FC <- factor_lvl3
  #
  #     fb <- cd.factorial(A = FA, B = FB, C = FC, design = "crd", nrep = 3, nc = 3)
  #
  #   } else if(nfactors == 4){  #4 factores
  #
  #     FA <-  factor_lvl1
  #     FB <- factor_lvl2
  #     FC <- factor_lvl3
  #     FD <- factor_lvl4
  #
  #     fb <- cd.factorial(A = FA, B = FB, C = FC, D = FD, design = "crd", nrep = 3, nc = 3)
  #
  #   } else {  #5 factores
  #
  #     FA <- factor_lvl1
  #     FB <- factor_lvl2
  #     FC <- factor_lvl3
  #     FD <- factor_lvl4
  #     FE <- factor_lvl5
  #
  #     fb <- cd.factorial(A = FA, B = FB, C = FC, D = FD , E = FE, design = design, nrep = 3, nc = 3)
  #
  #   }
  #   fb <- fb$book
  #
  #   trait_agrofims <- unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
  #
  #   if(!is.null(trait_agrofims)){
  #     mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_agrofims) )
  #     nm  <-  c(names(fb), trait_agrofims)
  #     fb  <-  cbind(fb, mm)
  #     names(fb)  <-  nm
  #   }
  #
  #   fb
  #   #add_fieldbook_sheet_hdfims(file = "omar.xlsx", fieldbook =fb)
  #   #shell.exec("C://Users//obenites//Documents//omar.xlsx")
  #
  # } )



  ## Weather ShinyTree  #################################################################

  output$designFieldbook_weatherVar_agrofims <- shinyTree::renderTree({

    a<- weather_list #data from fbdesign for hidap-agrofims
    a
  })

  ## Weather ShinyTree #################################################################

  output$designFieldbook_soilVar_agrofims <- shinyTree::renderTree({

    a<- soil_list #data from fbdesign for hidap-agrofims
    a
  })



  # SelectInput for split plot designs #################################################################
  output$fbdesign_split_cb <- shiny::renderUI({

    choices <- c(input$factor_name, "INSTN")
    shiny::selectInput("designFieldbook_split_cb",label = "Factor to Plots", choices =  choices, selected= "INSTN")


  })

  # Observed value for geographical information #################################################################
  shiny::observe({
    path <- fbglobal::get_base_dir()
    geodb_file <- "table_sites.rds"
    path <- file.path(path, geodb_file)
    values$sites_data <-  readRDS(file = path)

  })

  # Country ###################################################################################
  output$fbDesign_country <- shiny::renderUI({
    #sites_data <- fbsites::get_site_table() #before

    # sites_data <- site_table #data from package fbdesign as an internal data BEFORE

    sites_data <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)


    cntry <- fbsites::get_country_list(sites_data = sites_data) #new code: use file fbsites


    shiny::selectizeInput("fbDesign_countryTrial", label = "Country name",
                          choices = cntry, selected = 1,  multiple = FALSE)

  })

  # Sites ##################################################################################################
  fbdesign_sites <- reactive({

    #sites_data <- site_table #using data from package #Former code before useing rective values

    sites_data <- values$sites_data

    fbsites::get_filter_locality(sites_data = sites_data, country_input= input$fbDesign_countryTrial)
  })

  # Country_site_select #####################################################################################
  output$fbDesign_countrySite <- shiny::renderUI({

    req(input$fbDesign_countryTrial)

    #locs <- site_table #using data from package fbsite (OLD CODE)
    locs <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    fbdesign_sites_selected <- fbdesign_sites()
    #print(locs)
    if (nrow(locs) > 0 ){
      #chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Village name",
                            choices = fbdesign_sites_selected, selected = 1,  multiple = FALSE)
    }
  })


  # Create an object with the list of file #####################################################################
  #Button for selecting material list
  output$fbDesign_selmlist <- shiny::renderUI({

    input$fdesign_list_refresh
    #res <- sel_list()
    res <- fbdesign_mtl_files() #this come from util.R fbdesign package

    selectizeInput(inputId = "designFieldbook_sel_mlist", label = "Select a factorial list", width="100%",
                   choices = res,
                   options = list(
                     placeholder = 'Select a material list',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))

  })

  #Conditional reactive value for displaying Standard Statistical Design or Genetic Design ########################
  output$condition_selmlist <-  shiny::reactive({

    mlist <- material_table()

    #mlist <- input$designFieldbook_sel_mlist
    if(is.null(mlist) || mlist == ""){  out <- 0  }
    #is_parent <- is_parentList(mlist)
    tp <- get_type_list_ds(mlist)
    if(tp=="parental") {out <- 1}
    if(tp=="clonal")   {out <- 0}
    return(out)

  })

  #The output object will be suspended (not execute) when it is hidden on the web page ############################
  outputOptions(output, 'condition_selmlist', suspendWhenHidden=FALSE)
  ### End of Create an object with the list of file


  # Number of plant per row (calculated variable) #################################################################
  react_plantxplot <-  shiny::reactive({

    plantxplot <- input$fbDesign_nplantsrow*input$fbDesign_nrowplot
    if(length(plantxplot)==0){plantxplot <- 0}
    plantxplot

  })


  #Shiny UI for number of plants per plot #################################################################
  output$fbPlant_plot <- shiny::renderUI({

    rpplot <- react_plantxplot()
    shiny::numericInput("fbDesign_nplants",
                        "Number of plants per plot", rpplot , rpplot, rpplot)

  })

  # Plot Size Values ########################################################################################
  react_psize <- reactive({
    plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    print(plot_size)
    if(length(plot_size)==0){plot_size <- 0}
    plot_size
  })

  # Plot Size ###################################################################################
  output$fbPlanting_psize <- shiny::renderUI({
    #plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    plot_size <- react_psize()
    #if(length(plot_size)==0) plot_size <- 2.7
    shiny::numericInput(inputId = "fbDesign_psize", label = "Plot size (m2)",
                        value = plot_size, min = plot_size,max = plot_size)
  })


  # Reactive Plant densisty #####################################################################
  react_pdensity <-  shiny::reactive({

    #plant_density <- (input$fbDesign_nplants/input$fbDesign_psize)*10000

    nplantxplot <- react_plantxplot()

    plant_density <- (nplantxplot/input$fbDesign_psize)*10000
    print(plant_density)
    if(length(plant_density)==0){plant_density <- 0}
    plant_density
  })

  #Select Plant density #########################################################################
  output$fbPlanting_pdensity <- shiny::renderUI({
    plant_density <- react_pdensity()
    #if(length(plant_density)==0) plant_density <- 37037.037
    shiny::numericInput(inputId = "fbDesign_pdensity", label = "Plant density (plants/Ha)",
                        value = plant_density, min = plant_density, max = plant_density)
  })


  # Message for Alpha Design ####################################################################
  output$alphaMessage <- shiny::renderText({

    germoplasm <-material_table()$Accession_Number
    if(!is.null(germoplasm)){

      print(germoplasm)
      n <- length(germoplasm)
      r <- as.numeric(input$designFieldbook_r)
      k <- as.numeric(input$designFieldbook_k)

      dach <- design.alpha.check(trt= germoplasm,k=k,r=r)
      if(!dach$alfares){
        paste(dach$res,". The combination of ",r," and", k, " is wrong using ",n ," genotypes.")
      } else {
        paste("The combination of replication (r) and block size (k) is perfect!")
      }
    }
    else{
      paste("Enter your genotypes in the Germoplams List.")
    }
  })

  # Reactive data.frame for designing fieldbook #################################################
  fbdraft <- shiny::reactive({

    if(input$select_import=="Template") {
      req(input$file_mtlist)
    }

    if(input$select_import=="Local List"){
      req( input$designFieldbook_sel_mlist)
    }


    try({
      withProgress(message = 'Fieldbook loading...',
                   detail = 'This may take a while...', value = 0, {

                     incProgress(3/15)

                     crp <- input$designFieldbook_crop # capture the type of crop

                     #table_materials <- germoplasm_list()
                     material_tbl = material_table()


                     incProgress(4/15)
                     #  #Passing function to detect parental lists (utils.R) -------------------
                     #For parental list and genetic designs

                     is_parental <- is_parentList(input$designFieldbook_sel_mlist)
                     #type of list base on data structure
                     tpds <- get_type_list_ds(material_tbl)

                     #if(is_parental==TRUE){ #It's a parental list.
                     if(tpds=="parental"){

                       #Declaration of standard statistical designs parameters equal to NULL
                       trt1 <- NULL
                       trt2 <- NULL

                       #Get genetic design
                       design <-  input$design_geneticFieldbook

                       # N.Caroline parameter
                       if(design=="NCI" || design=="NCII"){

                         male <- material_tbl$male$Accession_Number
                         female <- material_tbl$female$Accession_Number
                         set <- input$design_genetic_nc_set
                         r <- input$design_genetic_r
                         #r <- input$design_genetic_nc_r #deprecated code. Now works for lxt and NC
                         if(design == "NCI"){
                           trt1_label  = "MALE"
                           trt2_label  = "FEMALE"
                         }

                         if(design == "NCII"){
                           trt1_label  = "FEMALE"
                           trt2_label  = "MALE"
                         }
                       }

                       if(design=="LXT"){

                         male <- material_tbl$male$Accession_Number
                         female <- material_tbl$female$Accession_Number
                         trt1_label  <-  "LINE"
                         trt2_label  <-  "TESTER"
                         type_lxt_scheme <- input$design_genetic_lxt_type
                         r <- input$design_genetic_r
                       }

                     }

                     #if(is_parental==FALSE){ #It's not a parental list
                     if(tpds=="clonal"){

                       #Get material list for Treatment 1
                       trt1 <-  material_tbl$Accession_Number
                       trt1 <-  gsub("[[:space:]]","", trt1)
                       trt1 <-  setdiff(trt1,"")

                       #Put labels for the first and second treatment/factor
                       trt1_label <- "INSTN" #label 1 for genotypes
                       trt2_label <- "FACTOR" #label 2 for factors

                       #Get Statistical design
                       design <-  input$designFieldbook
                       #Get replications
                       r <- input$designFieldbook_r


                       #Use sub_design in case of factorials and split plot design
                       sub_design <-  as.character(input$sub_design)

                       #In case users do not select sub_design
                       if(is.null(sub_design)) sub_design <- NULL

                       # Capture of second factor
                       print(input$factor_lvl)
                       factor_lvl <- strsplit(x = input$factor_lvl, ",")[[1]]
                       factor_lvl <- factor_lvl %>% stringr::str_trim(.,side = "both")
                       factor_lvl <- gsub("\\s+", replacement = "_" , factor_lvl)
                       trt2 <- factor_lvl
                       trt2 <- trt2[!is.na(trt2) & trt2!=""]

                       #Split plot design under CRD AND RCBD
                       if(input$designFieldbook=="SPCRD" || input$designFieldbook=="SPRCBD"){

                         if(input$designFieldbook_split_cb=="INSTN"){
                           trt1 <- trt1
                           trt2 <- trt2

                         } else  {
                           #In case of selecting Factor to plot the second treatment
                           genotypes <- trt1
                           factor <- trt2
                           trt1 <- factor
                           trt2 <- genotypes
                           #Modify the labels for displaying fieldbook
                           trt1_label  <-  "FACTOR" #label 1 for genotypes
                           trt2_label  <-  "INSTN" #label 2 for factors

                         }
                       }

                       #Augmented block design
                       if(input$designFieldbook=="ABD"){
                         #NOTE: In ABD(Augmented Design)  design.dau(trt1 = checks, trt2= genotypes)
                         #For this reason : design.dau( trt1= checks =trt2 ; trt2 = genotypes=trt1)
                         trt1 <- is_control(material_tbl)
                         print(trt1)
                         trt2 <-  material_tbl$Accession_Number
                         trt2 <-  setdiff(trt2,trt1)
                         print(trt2)
                       }

                       #Wescott Design
                       if(input$designFieldbook=="WD"){
                         #NOTE: In wd you need two checks
                         #For this reason :

                         trt2 <- is_control(material_tbl) #control material
                         trt1 <- setdiff(trt1,trt2) #remove controls from material list

                         #Declaration of Genetic parameters equal to NULL
                         male <- NULL
                         female <- NULL
                         set <- NULL

                       }
                     }

                     incProgress(6/15)

                     #Get selected module
                     mdl <-  input$designFieldbook_module
                     mdl <-  stringr::str_extract(mdl, "([A-Z]{2})")[[1]]

                     ### Cleaning Trait List  ----------------------------------------
                     ## Remove all the trial names from the Trait List
                     #Ex: remove yield, late blight, etc from Trait List when used pick ones
                     #b <- input$designFieldbook_traits
                     vars <-  get_tree_value(input$designFieldbook_traits,crop_selected = crp)

                     #Redirecting to each crop.
                     if(crp == "potato"){tbl <- table_module_potato }
                     if(crp == "sweetpotato"){tbl <- table_module_sweetpotato }

                     #Extracting modules and traits/variables
                     mdl <- tbl[tbl$CROP == crp, c("TRIAL_ABBR", "TRIAL")] #HiDAP v1.0 Built_2
                     mdl <- paste0(mdl[,2], " (", mdl[, 1],")")
                     mdl <- sort(unique(mdl))
                     ids <- str_trim(gsub("\\(.*","",mdl),side = "both")
                     vars <- vars[!(vars %in% ids)]
                     ### End of cleaning the Trait List abbreviations ----------------------------------------

                     incProgress(8/15)

                     #Design of Fieldbook
                     fb = design_fieldbook(design = design,
                                           trt1 = trt1,
                                           trt2 = trt2,
                                           #is_rwcol = FALSE,
                                           sub_design=input$sub_design,
                                           trt1_label = trt1_label,
                                           trt2_label = trt2_label,
                                           r = as.integer(r),
                                           type_lxt = as.integer(type_lxt_scheme),
                                           k = as.integer(input$designFieldbook_k),
                                           # genetic design parameters
                                           number_col = as.integer(input$designFieldbook_wd_col), #wescott design
                                           number_colb = as.integer(input$designFieldbook_wd_colb), #wescott design
                                           set = set,
                                           male = male,
                                           female= female,
                                           ###designFieldbook_wd_col
                                           first=TRUE,
                                           cont = as.logical(input$designFieldbook_cont),
                                           series = as.integer(input$designFieldbook_serie),
                                           zigzag = FALSE,
                                           variables = vars)

                     print(fb)


                     ### Comb Factor ###
                     is_combfactor <- input$designFieldbook_combfactor  #designFieldbook_combfactor : checkbox, by default false
                     combfactorName <- input$combfactor_name
                     comfactorLvl <- input$combfactor_lvl
                     print(comfactorLvl)

                     if(is_combfactor){ #in case breeders requiere

                       comfactorLvl <- strsplit(x =  comfactorLvl, ",")[[1]]
                       comfactorLvl <- comfactorLvl %>% stringr::str_trim(.,side = "both")
                       comfactorLvl <- gsub("\\s+", replacement = "_" , comfactorLvl)
                       comfactorLvl <- comfactorLvl[!is.na(comfactorLvl) & comfactorLvl!=""]
                       fb_list <- vector(mode="list", length = length(comfactorLvl)) #initialization of book's list

                       for(i in 1:length(comfactorLvl)){
                         fb_list[[i]] <- add_cl(fb = fb, design_abr = design, factor_lvl = comfactorLvl[i])
                       }

                       fb <- data.table::rbindlist(fb_list,fill = TRUE) %>% as.data.frame()#combination of field books
                       #fb <- as.data.frame(fb)

                     }


                     is_ssample <- input$designFieldbook_cbssample
                     print(is_ssample)

                     if(is_ssample){

                       nsample <- input$designFieldbook_nsample
                       p1 <- as.list(rep(NA, times = nsample))
                       names(p1) <- 1:nsample
                       fb <- cbind(fb, p1)
                       #Hint: TSSAMPLE is the temporary name for the SUB SAMPLE factor (column). Then, we change for SUBSAMPLE
                       fb <- fb %>% tidyr::gather(key= "TSSAMPLE", value= "value", paste(1:nsample,sep="")) %>% select(-value)
                       inst_pos <- which(names(fb) == "INSTN")
                       print(inst_pos)
                       #fb2 <- fb
                       fb <- append_col(fb, list(SUBSAMPLE= fb$TSSAMPLE), after = inst_pos) %>% select(-TSSAMPLE)
                     }

                     ###
                     incProgress(12/15)
                     #from character to numeric
                     fb[, 1]  <-  as.integer(fb[, 1])

                     incProgress(15/15)
                     fb


                   })
    })

  })

  # Visualization of the field book #############################################################
  shiny::observeEvent(input$fbDesign_draft_agrofims, {

    withProgress(message = 'Fieldbook Preview', value = 0, {

      incProgress(1/10,message = "...")


    flag <- TRUE #temporary

    if(flag){

      #print(agro_var_dt())
      fb <- fb_agrofims()

      #fb <- fb[,1:129]
      output$fbDesign_table_agrofims <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(fb , readOnly = T)})

    }

      incProgress(9/10,message = "...")
      incProgress(10/10,message = "...")

    })

  })

  # Download or Create Fieldboon in Excel Format ################################################
  shiny::observeEvent(input$fbDesign_create, {


    withProgress(message = "Downloading Fieldbook...",value= 0,
                 {
                   incProgress(1/15)
                   fb = fbdraft()
                   print("fb in exporting")

                   try({

                     #passing parameters to vars
                     fn = paste0(fbdesign_id(), ".rda")
                     #fp = file.path(fbglobal::fname_fieldbooks(input$designFieldbook_crop), fn)

                     #### after fbglobal
                     path <- fbglobal::get_base_dir()
                     fp <- file.path(path, fn)
                     #### end before

                     #Detection of parentl list
                     is_parental <- is_parentList(input$designFieldbook_sel_mlist)

                     #In case of parental list

                     tpds <- get_type_list_ds(material_table())


                     #if(is_parental==TRUE){
                     if(tpds=="parental"){
                       r <- input$design_genetic_r
                       mtl_table <- material_table()$parental_table
                     }

                     #In case of other list (genotype and family)
                     #if(is_parental==FALSE){
                     if(tpds=="clonal"){
                       r <- input$designFieldbook_r
                       mtl_table <- as.data.frame(material_table())
                     }

                     mtl_table <-  mtl_table
                     #Get begin date
                     begin_date <- input$fbDesign_project_time_line[1]
                     begin_date <- unlist(str_split(begin_date,"-"))
                     begin_date1 <- paste(begin_date[3],begin_date[2],begin_date[1],sep="/")

                     #Get end date
                     end_date <- input$fbDesign_project_time_line[2]
                     end_date <- unlist(str_split(end_date,"-"))
                     end_date1 <- paste(end_date[3],end_date[2],end_date[1],sep="/")

                     #Setting genetic parameters NA
                     genetic_design <- NA#hidden in 7/10/2017 11:20
                     type_of_ploidy <- NA #hidden in 7/10/2017 11:20
                     set <- NA


                     #This variable serves as a flag if our fieldbook is correct.
                     flag <- TRUE

                     #Check if material list is empty
                     if(length(mtl_table)==0 ) {
                       flag <- FALSE
                       shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: You have not selected a material list. Please select/upload one"), styleclass = "danger")
                     }


                     #In case of augmented block design, it must verify the checks in the stistical designs
                     if(input$designFieldbook=="ABD"){

                       mtl <- mtl_table
                       mtl_instn <- as.character(mtl$Is_control)

                       mtl_checks_count <- is_control(mtl_table)
                       print(mtl_checks_count)

                       if(all(is.na(mtl_instn)) || length(mtl_checks_count)==1  ){
                         shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: in Augmented Design: At least two checks are needed in 'Is_Control' column. Verify Material List file"), styleclass = "danger")

                         flag <- FALSE

                       } else {

                         flag <- TRUE

                       }

                     }

                     if(input$designFieldbook=="WD"){

                       mtl <- mtl_table
                       mtl_instn <- as.character(mtl$Is_control)

                       mtl_checks_count <- is_control(mtl_table)

                       if(all(is.na(mtl_instn)) || length(mtl_checks_count)==1 || length(mtl_checks_count)>2  ){

                         shinysky::showshinyalert(session, "alert_fb_done",
                                                  paste("ERROR in Westcott Design: Just two checks are needed in 'Is_Control' column. Verify your Material List file"), styleclass = "danger")
                         flag <- FALSE
                       } else {
                         flag <- TRUE
                       }

                     }

                     if(input$designFieldbook=="AD"){

                       germoplasm <-material_table()$Accession_Number


                       print(germoplasm)
                       n <- length(germoplasm)
                       r <- as.numeric(input$designFieldbook_r)
                       k <- as.numeric(input$designFieldbook_k)

                       dach <- fbdesign::design.alpha.check(trt= germoplasm,k=k,r=r)

                       if(!dach$alfares){
                         ms <- paste(dach$res,". The combination of ",r," and", k, " is wrong using ",n ," genotypes.")
                         shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: ", ms), styleclass = "danger")
                         flag <- FALSE
                       } else{
                         flag <- TRUE
                       }

                     }

                     # To combine fieldbooks
                     if(input$designFieldbook_combfactor) {
                       #for combined designs. Specifically for CRD, RCBD, WescottD, AugmentedBD, AlphaD
                       factor_input <- input$combfactor_name
                       flevel_input <- input$combfactor_lvl
                     }
                     else { # for factorial/split designs
                       #for factorial designs, split and strip plot design
                       factor_input <- input$factor_name
                       flevel_input <- input$factor_lvl
                     }


                     if(tpds=="parental"){


                       genetic_design  <-  input$design_geneticFieldbook#hidden in 7/10/2017 11:20
                       type_of_ploidy  <-  input$design_genetic_ploidy #hidden in 7/10/2017 11:20
                       set  <-  input$design_genetic_nc_set



                       if(input$design_geneticFieldbook=="NCI"){

                         male <-  mtl_table$Male_AcceNumb
                         female <-  mtl_table$Female_AcceNumb
                         set <- as.numeric(input$design_genetic_nc_set)
                         r <- as.numeric(input$design_genetic_r)

                         print(male)
                         print(female)

                         if(length(male)!=length(female)){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: The length of males and females must be the same dimension"), styleclass = "danger")

                         } else if (r==1 || is.na(r)){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: You have entered just 1 replication or NA/NULL values."), styleclass = "danger")

                         } else if (is.null(male) || is.na(male)){
                           flag <- FALSE

                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 male"), styleclass = "danger")

                         } else if(length(female)==1 || is.null(female) || is.na(female)){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 female"), styleclass = "danger")

                         } else if (length(female) %% set !=0){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: data length is not a multiple of set variable. Please provide an accurate number of sets"), styleclass = "danger")

                         } else {

                           flag <- TRUE
                         }

                       }

                       if(input$design_geneticFieldbook=="NCII"){

                         male <-  mtl_table$Male_AcceNumb
                         female <-  mtl_table$Female_AcceNumb
                         set <- as.numeric(input$design_genetic_nc_set)
                         r <- as.numeric(input$design_genetic_r)
                         print(male)
                         print(female)


                         if(length(male)!=length(female)){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: The length of males and females must be the same dimension"), styleclass = "danger")

                         } else if (r==1 || is.na(r)){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: You have entered just 1 replication or NA/NULL values."), styleclass = "danger")

                         } else if (is.null(male) || is.na(male)){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 males"), styleclass = "danger")

                         } else if(length(female)==1 || is.null(female) || is.na(female)){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 females"), styleclass = "danger")

                         } else if (length(female) %% set !=0){
                           flag <- FALSE
                           shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: data length is not a multiple of set variable. Please provide an accurate number of sets"), styleclass = "danger")

                         } else {

                           flag <- TRUE
                         }

                       }


                     }


                     #If field book does exist then print warning message: It's already created it.
                     if(file.exists(fp))  {
                       shinysky::showshinyalert(session, "alert_fb_done", paste("WARNING: This fieldbook already exists in HiDAP. Please Select Experiment Number in Crop & Location"),
                                                styleclass = "warning")
                       flag <- FALSE
                     }

                     if(!file.exists(fp) && flag == TRUE) {

                       saveRDS(fb, fp)
                       values[["ph_fb_list"]] = NULL
                       shinysky::showshinyalert(session, "alert_fb_done", paste("GREAT: Fieldbook successfully created!"), styleclass = "success")

                       ##after fbglobal
                       #Set up the file path
                       xlsx_path <- fbglobal::get_base_dir()
                       xlsx_path <- file.path(xlsx_path, fbdesign_id())
                       fn_xlsx <- paste(xlsx_path, ".xlsx",sep= "")
                       ##

                       openxlsx::write.xlsx(fb, fn_xlsx,sheet="Fieldbook",overwrite=TRUE)

                       # Fieldbook Sheet ------------------------------------------------------
                       add_fieldbook_sheet(file = fn_xlsx, fieldbook = fb)

                       #load the crop_template_xlsx
                       crop_template <- crop_template_xlsx #dataset loaded from fbdesign package


                       ### Logical/flag value for users that combine fieldbooks
                       combine <- input$designFieldbook_combfactor #logical
                       ###

                       print("2")
                       incProgress(2/15)

                       #varsitos <- input$designFieldbook_traits
                       add_varlist_sheet(file=fn_xlsx, crop_template = crop_template,crop=input$designFieldbook_crop,
                                         trait_list = input$designFieldbook_traits)

                       incProgress(4/15)


                       # Minimal Sheet ------------------------------------------------------

                       add_minimal_sheet(file = fn_xlsx, crop_template = crop_template, col_name = "Value", Trial_name = fbdesign_id(),
                                         crop = input$designFieldbook_crop,
                                         type_trial = input$designFieldbook_module,
                                         begin_date = begin_date1,
                                         end_date = end_date1,
                                         site_short_name = input$designFieldbook_sites,
                                         country = input$fbDesign_countryTrial)

                       if(input$fbDesign_environment_type!="Field"){
                         #In case of screenhouse and greenhouse
                         n_plant_pot <- input$fbDesign_nplantxpot
                         n_pots <- input$fbDesign_npots
                         n_plot_row <- NA
                         n_plant_plot  <-  NA
                         n_plant_row  <-  NA
                         plot_size <- NA
                         plant_density <- NA
                         distance_plants <- NA
                         distance_rows <- NA
                       } else {
                         ### Greenhouse and screenhouse
                         n_plant_pot <- NA
                         n_pots <- NA
                         ### Field
                         n_plot_row <- input$fbDesign_nrowplot
                         n_plant_plot <- input$fbDesign_nplants
                         n_plant_row <- input$fbDesign_nplantsrow
                         plot_size <- input$fbDesign_psize
                         plant_density <- input$fbDesign_pdensity
                         distance_plants <- input$fbDesign_distPlants
                         distance_rows <- input$fbDesign_distRows
                       }

                       print("3")
                       incProgress(3/15)

                       # Installation Sheet ------------------------------------------------------

                       add_installation_sheet(file=fn_xlsx, crop_template = crop_template, col_name = "Value",
                                              exp_design = input$designFieldbook,
                                              #genetic_design = input$design_geneticFieldbook,#hidden in 7/10/2017 11:20
                                              #type_of_ploidy = input$design_genetic_ploidy, #hidden in 7/10/2017 11:20
                                              #set = input$design_genetic_nc_set, #hidden in 7/10/2017 11:20
                                              genetic_design = genetic_design, #hidden in 7/10/2017 11:20
                                              type_of_ploidy = type_of_ploidy, #hidden in 7/10/2017 11:20
                                              set = set, #hidden in 7/10/2017 11:20
                                              #rep = input$designFieldbook_r,#deprecated. Just for statistical design but not for genetic studies.
                                              r = r,
                                              block=NA,
                                              exp_env = input$fbDesign_environment_type,
                                              plot_start_number = NA,
                                              #n_plant_pot = input$fbDesign_nplantxpot, #(Deprecated)If screenhouse and greenhouse is selected
                                              #n_pots = input$fbDesign_npots, #(Deprecated) if screenhouse and greenhouse is selected
                                              ##### Greenhouse and Screenhouse
                                              n_plant_pot = n_plant_pot,
                                              n_pots = n_pots,
                                              n_plot_row = n_plot_row,
                                              n_plant_plot = n_plant_plot,
                                              n_plant_row = n_plant_row,
                                              plot_size = plot_size,
                                              plant_density = plant_density,
                                              distance_plants = distance_plants,
                                              distance_rows = distance_rows,
                                              factor_name = factor_input,
                                              factor_lvl = flevel_input,
                                              combine= combine
                       )

                       print("4")
                       incProgress(4/15)

                       # Weather and soil Sheets ------------------------------------------------------
                       add_metadata_sheet(file=fn_xlsx, crop_template = crop_template, soil_input = input$fbDesign_soil_cb,
                                          weather_input = input$fbDesign_weather_cb)


                       # Material list Sheet ------------------------------------------------------
                       print("5")
                       incProgress(5/15)
                       add_material_sheet(file=fn_xlsx, crop_template=crop_template, crop= input$designFieldbook_crop,
                                          material_list = mtl_table)


                       # Crop management list Sheet ------------------------------------------------------
                       print("6")
                       incProgress(6/15)
                       add_cmanagment_sheet(file=fn_xlsx,
                                            crop_template = crop_template,
                                            crop=input$designFieldbook_crop,
                                            trait_list = input$designFieldbook_traits)

                       print("11")
                       shell.exec(fn_xlsx)

                       incProgress(15/15)
                     }

                   })

                 })
  })

  # Material List Export, #######################################################################
  output$fbDesign_mlistExport <- downloadHandler(
    filename = function() {
      paste("Material_List", '.xlsx', sep='')
    },
    content = function(file) {

      mt_list<- crop_template_xlsx$Material_List
      #mt_list <- material_list ##internal dataset
      #       hs <- createStyle(fontColour = "#060505", fontSize=12,
      #                         fontName="Arial Narrow", fgFill = "#4F80BD")
      hs <- createStyle(fontColour = "#000000", fontSize=12,
                        fontName="Calibri", fgFill = "orange")
      openxlsx::write.xlsx(mt_list, file, headerStyle = hs, sheetName="Material_List", colWidths="auto")
    }
  )

  ###reactive data frame for design fieldbook  ################################################
  fb_agrofims <- shiny::reactive({

    nrep <- as.numeric(input$designFieldbook_agrofims_r)
    design <- input$designFieldbook_agrofims


    factor_name1 <- gsub("\\s+", replacement = "-" , input$factor_hdafims1) %>% stringr::str_trim(.,side = "both")
    factor_lvl1 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims1) %>% stringr::str_trim(.,side = "both")

    factor_name2 <- gsub("\\s+", replacement = "-" , input$factor_hdafims2) %>% stringr::str_trim(.,side = "both")
    factor_lvl2 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims2) %>% stringr::str_trim(.,side = "both")

    factor_name3 <- gsub("\\s+", replacement = "-" , input$factor_hdafims3) %>% stringr::str_trim(.,side = "both")
    factor_lvl3 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims3) %>% stringr::str_trim(.,side = "both")

    factor_name4 <- gsub("\\s+", replacement = "-" , input$factor_hdafims4) %>% stringr::str_trim(.,side = "both")
    factor_lvl4 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims4) %>% stringr::str_trim(.,side = "both")

    factor_name5 <- gsub("\\s+", replacement = "-" , input$factor_hdafims5) %>% stringr::str_trim(.,side = "both")
    factor_lvl5 <- gsub("\\s+", replacement = "-" , input$lvl_hdafims5) %>% stringr::str_trim(.,side = "both")

    nfactors <- as.numeric(input$nfactors_hdafims)

    if(design == "CRD") { design<- "crd"}
    if(design == "RCBD") { design<- "rcbd"}
    design <- design

    #checkbox to set CropVariety as a Factor
    isCropFactor <- input$setCropFactor
    crop_varietiesname <- input$cropVarietyNameMono

    if(is.null(crop_varietiesname)) crop_varietiesname <- ""

    # print(factor_lvl1)
    # print(factor_lvl2)
    # print(factor_lvl3)
    # print(factor_lvl4)
    # print(factor_lvl5)

    if(nfactors == 1){
      fb <- st4gi::cd.cr(geno = FA, nrep = nrep,  nc = 3)

    } else if(nfactors == 2){

      FA <-  factor_lvl1
      FB <-  factor_lvl2
      #print(FA)
      #print(FB)
      #print(design)
      #print(nrep)

      if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){

        fb <- cd.factorial(A = FA, B = FB, C= crop_varietiesname, design = design, nrep = nrep,  nc = 3)

        if(design == "crd"){
          names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, "CROP_VARIETY")
        } else{
          names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, "CROP_VARIETY")
        }


      } else {

        fb <- cd.factorial(A = FA, B = FB, design = design, nrep = nrep,  nc = 3)

        if(design == "crd"){
          names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
        } else{
          names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
        }


      }

#
#       if(design == "crd"){
#         names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
#       } else{
#         names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
#       }
      fb <- fb

    } else if(nfactors == 3){
      #
      FA <-  factor_lvl1
      FB <- factor_lvl2
      FC <- factor_lvl3

      if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){

        fb <- cd.factorial(A = FA, B = FB, C = FC,  design = design, nrep = nrep,  nc = 3)

        if(design == "crd"){
          names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, "CROP_VARIETY")
        } else{
          names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, "CROP_VARIETY")
        }



      } else {

      fb <- cd.factorial(A = FA, B = FB, C = FC, design = design, nrep = nrep, nc = 3)

      if(design == "crd"){
        names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3)
      } else {
        names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2,factor_name3)
      }
      fb <- fb


      }

      fb <- fb

    } else if(nfactors == 4){

      FA <-  factor_lvl1
      FB <- factor_lvl2
      FC <- factor_lvl3
      FD <- factor_lvl4

      fb <- cd.factorial(A = FA, B = FB, C = FC, D = FD, design = design, nrep = nrep, nc = 3)

      if(design == "crd"){
        names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4)
      } else{
        names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4)
      }
      fb <- fb

    } else {

      FA <- factor_lvl1
      FB <- factor_lvl2
      FC <- factor_lvl3
      FD <- factor_lvl4
      FE <- factor_lvl5

      fb <- cd.factorial(A = FA, B = FB, C = FC, D = FD , E = FE, design = design, nrep = nrep, nc = 3)

      if(design == "crd"){
        names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4, factor_name5)
      } else{
        names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4, factor_name5)
      }
      fb <- fb

    }
    fb <- fb$book

    trait_agrofims <- unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))

    if(!is.null(trait_agrofims)){
      mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_agrofims) )
      nm  <-  c(names(fb), trait_agrofims)
      fb  <-  cbind(fb, mm)
      names(fb)  <-  nm
    }

    fb


    #add_fieldbook_sheet_hdfims(file = "omar.xlsx", fieldbook =fb)
    #shell.exec("C://Users//obenites//Documents//omar.xlsx")

  })

  ##reactive table from agro features   ########################################################
  dt_agrofeatures <- reactive({

    tree <- input$treeFeatures
    p <- get_selected(tree)
    print(p)
    n <- length(p)
    print(p)
    #list_sel_values <- NULL
    print(n)
    #Generation of list of selected values from tree: list_sel_values
    #as <- readRDS("inst/examples/04-selected/selagro.rds")
    # for(i in 1:n){
    #   list_sel_values[[i]] <- p[i][[1]]
    # }
    list_sel_values <- p
    print(list_sel_values)

    total_dt <- data.frame() #acumulator of tables of agronomic variables and values

    list_sel_values <- list_sel_values
    saveRDS(list_sel_values ,"list_sel_va.rds")
    #value <- list_sel_values[[i]][1] #extrac values of each item


    for(i in 1:n){

      var_slevel <- attr(list_sel_values[i][[1]], "ancestry", TRUE)
      nmax <- length(var_slevel)

      if( nmax ==1){
        value <- NA #""
        lvl <-  NA #list_sel_values[[i]][1]
        subgroup <- NA #attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax] #max  =3
        group <- NA #attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax-1] #max =2

      } else if(nmax == 2){ #
        value <- NA #""
        lvl <-  NA #list_sel_values[[i]][1]
        subgroup <- NA #attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax] #max  =3
        group <- NA #attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax-1] #max =2
        #value <- list_sel_values[[i]][1] #extrac values of each item
      } else  if(nmax == 3){
        value <- ""
        lvl <-  list_sel_values[[i]][1]
        subgroup <- attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax] #max  =3
        group <- attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax-1] #max =2
        #value <- list_sel_values[[i]][1] #extrac values of each item
      }else {
        #if(n_max == 4){
        value <- list_sel_values[[i]][1]
        lvl <- attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax] #max =4
        subgroup <- attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax-1] #max -1 = 3
        group <- attr(list_sel_values[i][[1]], "ancestry", TRUE)[nmax-2] #max -2 =2
      }

      dt <- data.frame(Group  = group, Subgroup = subgroup, Level = lvl, Value = value)
      total_dt <- rbind(total_dt, dt)
    }
    #end bucle
    print(total_dt)
    #remove na
    total_dt <- total_dt[apply(total_dt,1,function(x)any(!is.na(x))),]
    total_dt

  })

  ##reactive table from metadata info   ########################################################
  dt_metadata_agrofims <- reactive({

    startDate_val <- input$fbDesign_project_time_line[1]
    print(startDate_val)
    endDate_val <- input$fbDesign_project_time_line[2]
    #print(endDate_val)
    x <- interval(ymd(startDate_val),ymd(endDate_val))
    x <- x %/% months(1)
    Duration_val <- x


    metadata<- fbdesign::add_metadata_agrofims( agronomic_crop_template = metadata_template_list, col_name= "Value",
                                     experimentId =  input$experimentId, experimentName = input$experimentName,
                                     experimentProjectName = input$experimentProjectName,
                                     startDate = startDate_val,
                                     endDate =  endDate_val,
                                     Duration = Duration_val, #input$fbDesign_project_time_line[2] - fbDesign_project_time_line[1],
                                     designFieldbook_typeExperiment = input$designFieldbook_typeExperiment,
                                     experimentObj = input$experimentObj, fundName = input$fundName, personnel1Type = input$personnel1Type,

                                     person1FirstName = input$person1FirstName, designFieldbook_fundLeadAgency = input$designFieldbook_fundLeadAgency,
                                     leadName = input$leadName, person1LastName = input$person1LastName, person1Email = input$person1Email,
                                     person1Afiliation =   input$person1Afiliation, person1ORCID = input$person1ORCID,
                                     instAbreviation = "", #input$instAbreviation,
                                     contCenter = input$contCenter, contCRP = input$contCRP,
                                     contResearcher =  input$contResearcher, sytpetype = input$sytpetype, syteName = input$syteName,
                                     siteID = input$siteID ,
                                     countryName= input$fbDesign_countryTrial,
                                     #admin1=  "", #input$admin1,
                                     #admin2="", #input$admin2,
                                     villageName= input$designFieldbook_sites,
                                     #elevation ="", #input$elevation,
                                     #latitude="", #input$latitude,
                                     #longitude= "", #input$longitude,
                                     descNotes= "", #input$descNotes,
                                     croppingType= input$croppingType,
                                     cropLatinNameMono=input$cropLatinNameMono,
                                     cropCommonNameMono= input$cropCommonNameMono, cultivarNameMono=input$cultivarNameMono,
                                     cropVarietyNameMono= input$cropVarietyNameMono,
                                     subject= "", #input$subject,
                                     keywords="", #input$keywords,
                                     Embargo_date=input$Embargo_date)

    metadata

  })

  ##reactive table for installation info ########################################################
  dt_installation_agrofims <- reactive({

    add_installation_agrofims(agronomic_crop_template= installation_template_list, col_name = "Value",
    designFieldbook_agrofims	=	input$designFieldbook_agrofims,
    designFieldbook_agrofims_r	=	input$designFieldbook_agrofims_r,
    numPlantsPerPlot	=	input$numPlantsPerPlot,
    numRowsPerPlot	=	input$numRowsPerPlot,
    numPlantsPerRow	=	input$numPlantsPerRow,
    plotSize	=	input$plotSize,
    spaceBwPlants	=	input$spaceBwPlants,
    spaceBwRows	=	input$spaceBwRows,
    planDensity	=	input$planDensity,
    plotSpacing	=	input$plotSpacing,
    rowOrientation	=	input$rowOrientation,
    hillSpacing	=	input$hillSpacing,

    factor_hdafims1	=	input$factor_hdafims1,
    lvl_hdafims1	=	input$lvl_hdafims1,

    factor_hdafims2	=	input$factor_hdafims2,
    lvl_hdafims2	=	input$lvl_hdafims2,

    factor_hdafims3	=	input$factor_hdafims3,
    lvl_hdafims3	=	input$lvl_hdafims3,

    factor_hdafims4	=	input$factor_hdafims4,
    lvl_hdafims4	=	input$lvl_hdafims4,

    factor_hdafims5	=	input$factor_hdafims5,
    lvl_hdafims5	=	input$lvl_hdafims5
    )

  })

  ##reactive weather
  dt_weather_agrofims <- shiny::reactive({

    weather_vars <- unlist(shinyTree::get_selected(input$designFieldbook_weatherVar_agrofims))
    weather_vars <- gsub(pattern = ":.*",replacement = "",x = weather_vars)
    weather_vars <- stringr::str_trim(weather_vars,side = "both")

    if(!is.null( weather_vars)){
      dt  <-  matrix(nrow = 50, ncol = length(weather_vars) )
      dt  <- data.frame(dt)
      names(dt)  <-  weather_vars
    } else{
      dt <- data.frame()
    }

    dt
  })


  ##reactive soil
  dt_soil_agrofims <- shiny::reactive({

    soil_vars <- unlist(shinyTree::get_selected(input$designFieldbook_soilVar_agrofims))

    if(!is.null(soil_vars)){
      dt  <-  matrix(nrow = 50, ncol = length(soil_vars))
      dt  <- data.frame(dt)
      names(dt)  <-  soil_vars
    } else{
      dt <- data.frame()
    }
    dt
  })


  ### donwload fieldbook ########################################################################
  output$downloadData <- downloadHandler(
    filename = "fileNameBook.xlsx",
    content = function(file) {

      withProgress(message = 'Downloading fieldbook', value = 0, {

        incProgress(1/10,message = "...")


      design <- input$designFieldbook_agrofims
      nrep <- input$designFieldbook_agrofims_r

      print("common variety name")
      #print(input$cropVarietyNameMono)
      print("end common var name")

      weather_vars <- unlist(shinyTree::get_selected(input$designFieldbook_weatherVar_agrofims))
      soil_vars <- unlist(shinyTree::get_selected(input$designFieldbook_soilVar_agrofims))
      print(weather_vars)
      # print(input$fbDesign_countryTrial)
      # print(input$designFieldbook_sites)

      fb <- fb_agrofims()

      agrofeatures <- dt_agrofeatures()
      metadata <- dt_metadata_agrofims()
      installation <-dt_installation_agrofims()
      weather <- dt_weather_agrofims()
      soil <- dt_soil_agrofims()


      fname <- paste(file,"xlsx",sep=".")
      #wb <- openxlsx::loadWorkbook(file = fname, create = TRUE)

      wb <- createWorkbook()

        incProgress(2/10,message = "Adding fieldbook data...")

      openxlsx::addWorksheet(wb, "Fieldbook", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Fieldbook", x = fb,
                               colNames = TRUE, withFilter = FALSE)


        incProgress(3/10,message = "Adding agronomic features...")



      openxlsx::addWorksheet(wb, "Agronomic_Features", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Agronomic_Features", x = agrofeatures,
                               colNames = TRUE, withFilter = FALSE)


      incProgress(6/10,message = "Metadata metadata sheet...")

      openxlsx::addWorksheet(wb, "Metadata", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Metadata", x = metadata,
                               colNames = TRUE, withFilter = FALSE)


      incProgress(7/10,message = "Adding installation sheet...")

      openxlsx::addWorksheet(wb, "Installation", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Installation", x = installation,
                               colNames = TRUE, withFilter = FALSE)



      #if(nrow(weather)==0){
      if(is.null(weather_vars)){
        print("there is no weather data")

      } else{

        incProgress(8/10,message = "Adding weather variables sheet...")

        openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Weather", x = weather,
                                 colNames = TRUE, withFilter = FALSE)
      }

      if(is.null(soil_vars)){
        print("there is no soil data")

      } else {

        incProgress(9/10,message = "Adding soil variables sheet...")
        openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Soil", x = soil,
                                 colNames = TRUE, withFilter = FALSE)
      }

      incProgress(10/10,message = "Downloading file...")

      saveWorkbook(wb, file = fname , overwrite = TRUE)

      file.rename(fname, file)


    })

    },
    contentType="application/xlsx"
  )


}










