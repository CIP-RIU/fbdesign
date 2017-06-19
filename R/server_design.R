#' server_design
#'
#' Design field book
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites
#' @export
#'

server_design <- function(input, output, session, values){

  # Reactive table. Get material list table ----------------------
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


        is_parent_list <- is_parentList(sel_list)
        if(is_parent_list==TRUE){
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

  # InfoBox to display messages of upload material list (genotype, family and parental list) ---------------------------------

  #CODE FOR GENETIC DESIGNS, enable after Africa's installer
#   output$approvalBox <- renderInfoBox({
#
#     # if(input$select_import=="Template") {
#     #   req(input$file_mtlist)
#     # }
#     #
#     # if(input$select_import=="Local List"){
#     #   req(input$designFieldbook_sel_mlist)
#     # }
#
#     #sel_list <- input$designFieldbook_sel_mlist
#
#     #print(sel_list)
#
#     if(is.null(sel_list) || sel_list == ""){  return()  }
#     if(length(sel_list)>0){
#
#     is_parent_list <- is_parentList(sel_list)
#
#     #depreacted code: germoplasm <-material_table()$Accession_Number
#     germoplasm <-material_table()
#     print(germoplasm)
#
#     print("pass false")
#
#     if(is_parent_list==FALSE){
#
#       germoplasm <- germoplasm$Accession_Number
#
#
#       if(is.null(germoplasm)){
#         infoBox(title="Upload", subtitle=
#                   paste("your material list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
#                 color = "blue",fill = TRUE, width = NULL)
#       }
#
#       else if(all(is.na(germoplasm))) {
#         infoBox(title="ERROR", subtitle=
#                   paste("The material list", "is empty. Please check it"), icon = icon("warning-sign", lib = "glyphicon"),
#                 color = "red",fill = TRUE, width = NULL)
#       }
#
#       else{
#
#         infoBox(title="GREAT!", subtitle =
#                   paste("Material list was successfully uploaded! gen"),  icon = icon("ok", lib = "glyphicon"),
#                 color = "green",fill = TRUE, width = NULL)
#       }
#
#     }
#
#
#     else {
#
#       germoplasm_fem <- germoplasm$female$Accession_Number
#       germoplasm_male <- germoplasm$male$Accession_Number
#
#       print(germoplasm_fem)
#       print(germoplasm_male)
#
#       if(is.null(germoplasm_fem) && is.null(germoplasm_male)){
#
#         infoBox(title="Upload", subtitle=
#                   paste("your parental list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
#                   color = "blue",fill = TRUE, width = NULL)
#
#       }
#
#       else if(all(is.na(germoplasm_fem))) {
#
#         infoBox(title="ERROR", subtitle=
#                   paste("The female's accession number are empty.", "Please check female's accesion number column"), icon = icon("warning-sign", lib = "glyphicon"),
#                   color = "red",fill = TRUE, width = NULL)
#
#       }
#
#       else if(all(is.na(germoplasm_male))) {
#
#         infoBox(title="ERROR", subtitle=
#                   paste("The male's accession number are empty.", "Please check male's accesion number column"), icon = icon("warning-sign", lib = "glyphicon"),
#                   color = "red",fill = TRUE, width = NULL)
#
#
#       }
#
#
#       else{
#
#             infoBox(title="GREAT!", subtitle =
#                       paste("Parental list was successfully uploaded! "),  icon = icon("ok", lib = "glyphicon"),
#                     color = "green",fill = TRUE, width = NULL)
#           }
#
#       }
#
#     }
#
#
# })

  output$approvalBox <- renderInfoBox({

    #germoplasm <-material_table()$Institutional_number
    germoplasm <-material_table()$Accession_Number
    #germoplasm <-germoplasm_list()$institutional_number
    print(germoplasm)

    if(is.null(germoplasm)){
      infoBox(title="Upload", subtitle=
                paste("your material list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
              color = "blue",fill = TRUE, width = NULL)

    }
    else if(all(is.na(germoplasm))) {
      infoBox(title="ERROR", subtitle=
                paste("Your material list", "is empty. Please check it"), icon = icon("warning-sign", lib = "glyphicon"),
              color = "red",fill = TRUE, width = NULL)
      #shell.exec(hot_path())

    } else {
      #       material <- paste(germoplasm, collapse = ",")
      #       message <-  paste("Material list imported: ", material)

      infoBox(title="GREAT!", subtitle =
                paste(" was successfully uploaded!"),  icon = icon("ok", lib = "glyphicon"),
              color = "green",fill = TRUE, width = NULL)
    }
  })

  # Design of variables ---------------------------------------------------------------
  output$fbDesign_variables <- shiny::renderUI({

    crop <- input$designFieldbook_crop

    if(crop == "potato"){tbl <- table_module_potato } #dataset from fbdesing data folder
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

  # ID or name of the field book ---------------------------------------------------------------
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
      sites <- stringr::str_trim(input$designFieldbook_sites,side="both")

      if(nExp_id=="-"){
        out = paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites)
      } else {
        out = paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites, "_",nExp_id)
      }

      paste(out, collapse = ", ")
    }
  })

  # RenderText for displaying the book's name ---------------------------------------------------------------
  output$fbDesign_id <- shiny::renderText({
    fbdesign_id()
  })


  # Get Trait ---------------------------------------------------------------
  output$designFieldbook_traits <- shinyTree::renderTree({
     #if(crp=="potato"){tbl <- crop_list$crop_list$potato}
     #if(crp=="sweetpotato"){tbl <- crop_list$crop_list$sweetpotato}

       #Trait_List is a fbdesign dataset. This data contains all the traits hierachy divided by crops.
       a <- Trait_List
       #saveRDS(object = a,file = "pina.rds")
       #a <- crop_list
      #a
  })

  # SelectInput for split plot designs ---------------------------------------------------------------
  output$fbdesign_split_cb <- shiny::renderUI({

    choices <- c(input$factor_name, "INSTN")
    shiny::selectInput("designFieldbook_split_cb",label = "Factor to Plots", choices =  choices, selected= "INSTN")


  })

  # Observed value for geographical information -----------------------------
  shiny::observe({
    path <- fbglobal::get_base_dir()
    geodb_file <- "table_sites.rds"
    path <- file.path(path, geodb_file)
    values$sites_data <-  readRDS(file = path)

  })


  output$fbDesign_country <- shiny::renderUI({
     #sites_data <- fbsites::get_site_table() #before

      # sites_data <- site_table #data from package fbdesing as an internal data BEFORE

     sites_data <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)


     cntry <- fbsites::get_country_list(sites_data = sites_data) #new code: use file fbsites


     shiny::selectizeInput("fbDesign_countryTrial", label = "Country",
                            choices = cntry, selected = 1,  multiple = FALSE)

  })


  fbdesign_sites <- reactive({

    #sites_data <- site_table #using data from package #Former code before useing rective values

    sites_data <- values$sites_data



    fbsites::get_filter_locality(sites_data = sites_data, country_input= input$fbDesign_countryTrial)
  })


  output$fbDesign_countrySite <- shiny::renderUI({

     req(input$fbDesign_countryTrial)

    #locs <- site_table #using data from package fbsite (OLD CODE)
    locs <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    fbdesign_sites_selected <<- fbdesign_sites()
    #print(locs)
    if (nrow(locs) > 0 ){
      #chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Location",
                            choices = fbdesign_sites_selected, selected = 1,  multiple = FALSE)
   }
  })


  # Create an object with the list of file ----------------------------------
  #Button for selecting material list
  output$fbDesign_selmlist <- shiny::renderUI({

    input$fdesign_list_refresh
    #res <- sel_list()
    res <- fbdesign_mtl_files() #this come from util.R fbdesign package

    selectizeInput(inputId = "designFieldbook_sel_mlist", label = "Select a material list", width="100%",
                   choices = res,
                   options = list(
                     placeholder = 'Select a material list',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))

  })

  #Conditional reactive value for displaying Standard Statistical Design or Genetic Design
  output$condition_selmlist <-  shiny::reactive({

    mlist <- input$designFieldbook_sel_mlist
    if(is.null(mlist) || mlist == ""){  return()  }
    is_parent <- is_parentList(mlist)
    if(is_parent==TRUE) {out <- 1}
    if(is_parent==FALSE){out <- 0}
    return(out)

  })

  #The output object will be suspended (not execute) when it is hidden on the web page
  outputOptions(output, 'condition_selmlist', suspendWhenHidden=FALSE)
  ### End of Create an object with the list of file


  # Number of plant per row (calculated variable) ####
  react_plantxplot <-  shiny::reactive({

    plantxplot <- input$fbDesign_nplantsrow*input$fbDesign_nrowplot
    if(length(plantxplot)==0){plantxplot <- 0}
    plantxplot

  })


  #Shiny UI for number of plants per plot
  output$fbPlant_plot <- shiny::renderUI({

    rpplot <- react_plantxplot()
    shiny::numericInput("fbDesign_nplants",
                        "Number of plants per plot", rpplot , rpplot, rpplot)

  })


  # Plot Size Values #####
  react_psize <- reactive({
    plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    print(plot_size)
    if(length(plot_size)==0){plot_size <- 0}
    plot_size
  })

  # Plot Size
  output$fbPlanting_psize <- shiny::renderUI({
    #plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    plot_size <- react_psize()
    #if(length(plot_size)==0) plot_size <- 2.7
    shiny::numericInput(inputId = "fbDesign_psize", label = "Plot size (m2)",
                        value = plot_size, min = plot_size,max = plot_size)
  })


  # Plant densisty ####
  react_pdensity <-  shiny::reactive({

    #plant_density <- (input$fbDesign_nplants/input$fbDesign_psize)*10000

    nplantxplot <- react_plantxplot()

    plant_density <- (nplantxplot/input$fbDesign_psize)*10000
    print(plant_density)
    if(length(plant_density)==0){plant_density <- 0}
    plant_density
  })

  output$fbPlanting_pdensity <- shiny::renderUI({
    plant_density <- react_pdensity()
    #if(length(plant_density)==0) plant_density <- 37037.037
    shiny::numericInput(inputId = "fbDesign_pdensity", label = "Plant density (plants/Ha)",
                        value = plant_density, min = plant_density, max = plant_density)
  })

  # Message for Alpha Design ------------------------------------------------
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

  # Reactive data.frame for designing fieldbook -----------------------------
  fbdraft <- shiny::reactive({

        if(input$select_import=="Template") {
          req(input$file_mtlist)
        }

        if(input$select_import=="Local List"){
          req(input$designFieldbook_sel_mlist)
        }


    try({
      withProgress(message = 'Fieldbook loading...',
                   detail = 'This may take a while...', value = 0, {

                    incProgress(3/15)

                     crp <- input$designFieldbook_crop # capture the type of crop

                     incProgress(3/15)

                     #table_materials <- germoplasm_list()
                     material_tbl = material_table()

                     #  #Passing function to detect parental lists (utils.R) -------------------
                     #For parental list and genetic designs
                     is_parental <- is_parentList(input$designFieldbook_sel_mlist)

                     if(is_parental==TRUE){ #It's a parental list.

                       #Declaration of standard statistical designs parameters equal to NULL
                       trt1 <- NULL
                       trt2 <- NULL

                       #Get genetic design
                       design <-  input$design_geneticFieldbook

                       # N.Caroline parameter
                       if(input$design_geneticFieldbook=="NCI" || input$design_geneticFieldbook=="NCII"){

                           male <- material_tbl$male$Accession_Number
                           female <- material_tbl$female$Accession_Number
                           set <- input$design_genetic_nc_set
                           r <- input$design_genetic_nc_r

                         }
                     }

                     if(is_parental==FALSE){ #It's not a parental list


                           #Get material list for Treatment 1
                           trt1 = material_tbl$Accession_Number
                           trt1 = gsub("[[:space:]]","", trt1)
                           trt1 = setdiff(trt1,"")

                           #Put labels for the first and second treatment/factor
                           trt1_label = "INSTN" #label 1 for genotypes
                           trt2_label = "FACTOR" #label 2 for factors

                           #Get Statistical design
                           design <-  input$designFieldbook
                           #Get replications
                           r <- input$designFieldbook_r


                           #Use sub_design in case of factorials and split plot design
                           sub_design = as.character(input$sub_design)

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

                           #Declaration of Genetic parameters equal to NULL
                           male <- NULL
                           female <- NULL
                           set <- NULL

                     }

                     incProgress(3/15)

                     #Get selected module
                     mdl = input$designFieldbook_module
                     mdl = stringr::str_extract(mdl, "([A-Z]{2})")[[1]]

                     ### Cleaning Trait List  ----------------------------------------
                     ## Remove all the trial names from the Trait List
                     #Ex: remove yield, late blight, etc from Trait List when used pick ones
                     #b <- input$designFieldbook_traits
                     vars = get_tree_value(input$designFieldbook_traits,crop_selected = crp)

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

                     incProgress(3/15)

                     #Design of Fieldbook
                     fb = design_fieldbook(design = design,
                                           trt1 = trt1,
                                           trt2 = trt2,
                                           sub_design=input$sub_design,
                                           #trt1_label = "INSTN",
                                           trt1_label = trt1_label,
                                           trt2_label = trt2_label,
                                           r = as.integer(input$designFieldbook_r),
                                           k = as.integer(input$designFieldbook_k),
                                           # genetic design parameters
                                           set = set,
                                           male = male,
                                           female= female,
                                           ###
                                           first=TRUE,
                                           #first = as.logical(input$designFieldbook_first),
                                           cont = as.logical(input$designFieldbook_cont),
                                           series = as.integer(input$designFieldbook_serie),
                                           zigzag = as.logical(input$designFieldbook_zigzag),
                                           variables = vars)

                     #from character to numeric
                     fb[, 1] = as.integer(fb[, 1])

                     fb

                    } )
    })
  })


  # Visualization of the field book -----------------------------------------
  shiny::observeEvent(input$fbDesign_draft, {

    # req(input$designFieldbook_sel_mlist)
    # req(input$designFieldbook)
    mtl_table <- as.data.frame(material_table())


    if(input$designFieldbook=="ABD"){

      #mtl_table <- as.data.frame(material_table())
      mtl <- mtl_table
      mtl_instn <- as.character(mtl$Is_control)

      mtl_checks_count <- is_control(mtl_table)
      #print(res)

      if(all(is.na(mtl_instn)) || length(mtl_checks_count)==1  ){
        shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: in Augmented Design: At least two checks is needed in 'Is_Control' column. Verify Material List file"), styleclass = "info")
        #break
      }
    } else {


    fb = fbdraft()
    print(fb)
    output$fbDesign_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(fb, readOnly = T)})


    }
  })


  # Trigger for Downloading field book --------------------------------------

  # observe({
  #
  #  # if(is.null(mlist) || mlist == ""){  return()  }
  #
  #   #toggleState("fbDesign_draft", !is.null(input$designFieldbook_sel_mlist) &&  length(input$tree_input_value)!=0  && !is.null(input$designFieldbook)
  #   toggleState("fbDesign_draft",  !is.null(input$designFieldbook_sel_mlist) || str_trim(input$designFieldbook_sel_mlist, side = "both")!= "")
  # })

#
#   observe({
#
#     #if(is.null(mlist) || mlist == ""){  return()  }
#
#       #toggleState("fbDesign_create", !is.null(input$designFieldbook_sel_mlist) &&  length(input$tree_input_value)!=0  && !is.null(input$designFieldbook)  )
#
#     toggleState("fbDesign_create",  !is.null(input$designFieldbook_sel_mlist) || str_trim(input$designFieldbook_sel_mlist, side = "both")!= "")
#
#
#   })


  shiny::observeEvent(input$fbDesign_create, {

    # validate(
    #   need( input$designFieldbook_sel_mlist  , "Please select a material list your material list")
    # )


    # if(input$select_import=="Template") {
    #   req(input$file_mtlist)
    # }
    #
    # if(input$select_import=="Local List"){
    #   req(input$designFieldbook_sel_mlist)
    # }
    #
    # req(input$designFieldbook)
    # req(input$tree_input_value)


    withProgress(message = "Downloading Fieldbook..",value= 0,
                 {
        incProgress(3/15)
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
          if(is_parental==TRUE){
            mtl_table <- material_table()$parental_table
          }

          #In case of other list (genotype and family)
          if(is_parental==FALSE){
            mtl_table <- as.data.frame(material_table())
          }

          #Get begin date
          begin_date <- input$fbDesign_project_time_line[1]
          begin_date <- unlist(str_split(begin_date,"-"))
          begin_date1 <- paste(begin_date[3],begin_date[2],begin_date[1],sep="/")

          #Get end date
          end_date <- input$fbDesign_project_time_line[2]
          end_date <- unlist(str_split(end_date,"-"))
          end_date1 <- paste(end_date[3],end_date[2],end_date[1],sep="/")

          #If field book does exist then print warning message: It's already created it.
          if(file.exists(fp))  {
              shinysky::showshinyalert(session, "alert_fb_done", paste("WARNING: This fieldbook already exists in HiDAP. Please Select Experiment Number in Crop & Location"),
                                     styleclass = "warning")
          }

          #If field book does not exist then create it.
          if(!file.exists(fp)) {


            if(input$designFieldbook=="ABD"){

                  mtl <- mtl_table
                  mtl_instn <- as.character(mtl$Is_control)

                  mtl_checks_count <- is_control(mtl_table)
                  #print(res)

                  if(all(is.na(mtl_instn)) || length(mtl_checks_count)==1  ){
                          shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: in Augmented Design: At least two checks is needed in 'Is_Control' column. Verify Material List file"), styleclass = "info")
                    #break
                  }
          } else {


          saveRDS(fb, fp)
          values[["ph_fb_list"]] = NULL
          shinysky::showshinyalert(session, "alert_fb_done", paste("GREAT: Fieldbook successfully created!", "success"),
                                   styleclass = "success")

          ##after fbglobal
          #Set up the file path
          xlsx_path <- fbglobal::get_base_dir()
          xlsx_path <- file.path(xlsx_path, fbdesign_id())
          fn_xlsx <- paste(xlsx_path, ".xlsx",sep= "")
          ##

          openxlsx::write.xlsx(fb, fn_xlsx,sheet="Fieldbook",overwrite=TRUE)

          add_fieldbook_sheet(file = fn_xlsx, fieldbook = fb)

          crop_template <- crop_template_xlsx #dataset loaded from fbdesign package

          varsitos <- input$designFieldbook_traits
          add_varlist_sheet(file=fn_xlsx, crop_template = crop_template,
                            crop=input$designFieldbook_crop,
                            trait_list = input$designFieldbook_traits)

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

        add_installation_sheet(file=fn_xlsx, crop_template = crop_template, col_name = "Value",
                               exp_design = input$designFieldbook,
                               #genetic_design = input$design_geneticFieldbook,
                               #type_of_ploidy = input$design_genetic_ploidy,
                               #set = input$design_genetic_nc_set,
                               rep = input$designFieldbook_r, block=NA,
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
                               factor_name = input$factor_name,
                               factor_lvl = input$factor_lvl
                               )
                               # factor_name_1 = input$factor_lvl1,
                               # factor_name_2 = input$factor_lvl2,
                               # factor_name_3 = input$factor_lvl3
                               #factor_name_4 = input$factor_lvl4
                               #factor_name_5 = input$factor_lvl5
                               #)
        print("4")
        add_metadata_sheet(file=fn_xlsx, crop_template = crop_template, soil_input = input$fbDesign_soil_cb,
                           weather_input = input$fbDesign_weather_cb)

        print("5")
        add_material_sheet(file=fn_xlsx, crop_template=crop_template, crop= input$designFieldbook_crop,
                           material_list = mtl_table)

        print("6")
        add_cmanagment_sheet(file=fn_xlsx,
                             crop_template = crop_template,
                             crop=input$designFieldbook_crop,
                             trait_list = input$designFieldbook_traits)
        print("7")

        shell.exec(fn_xlsx)


          }
      }



          })



         })
    })

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


}







#end of SERVER.R

# Deprecated code ---------------------------------------------------------

    #   shiny::observeEvent(input$fbDesign_mlistExport,{
    #     #print("Heyoo")
    #     withProgress(message = "Downloading Material List Template..",value= 0,
    #       {
    #       mt_list <- material_list
    #       try({
    #            openxlsx::write.xlsx(x = mt_list,file = "material_list.xlsx")
    #          })
    #     })
    #    })

    #   shiny::observe({
    #     shinyjs::toggle(condition = input$fbDesign_environment_type == "field",
    #                     selector = "#fbDesignNav li a[data-value=fbDesign_field]")
    #     shinyjs::toggle(condition = input$fbDesign_environment_type == "farmers_field",
    #                     selector = "#fbDesignNav li a[data-value=fbDesign_farmers_field]")
    #     shinyjs::toggle(condition = input$fbDesign_environment_type == "greenhouse",
    #                     selector = "#fbDesignNav li a[data-value=fbDesign_greenhouse]")
    #     shinyjs::toggle(condition = input$fbDesign_environment_type == "screenhouse",
    #                     selector = "#fbDesignNav li a[data-value=fbDesign_screenhouse]")
    #     shinyjs::toggle(condition = input$fbDesign_weather_cb,
    #                     selector = "#fbDesignNav li a[data-value=fbDesign_weather]")
    #     shinyjs::toggle(condition = input$fbDesign_soil_cb,
    #                     selector = "#fbDesignNav li a[data-value=fbDesign_soil]")
    #   })

    #   output$fbDesign_phase <- shiny::renderUI({
    #     if (!is.null(input$designFieldbook_crop)) {
    #       tbl = fbcrops::get_crop_table()
    #       crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
    #
    #       tbl = fbprstages::get_program_stage_table()
    #       prg = tbl[tbl$crop_id == crp, ]
    #
    #       if (nrow(prg) > 0 ) {
    #         lbl = paste0(prg$program_stage_name, " (", prg$program_stage_id, ")" )
    #         chc = as.list(prg$program_stage_id)
    #         names(chc) = lbl
    #         shiny::selectInput("designFieldbook_phase", "Study", chc)
    #       }
    #     }
    #   })

    #   material_table <- reactive({
    #       hot_file <- hot_path()
    #       if(length(hot_file)==0){return (NULL)}
    #       if(length(hot_file)>0){
    #
    #         mtl_list <- readxl::read_excel(path=hot_file , sheet = "material")
    #         #mtl_df <- as.data.frame(mtl_list) #mtl in data frame format
    #         mtl_list <- as.list(mtl_list) #mtl in list format
    #         #material_table <- list(material_list = mtl_list)
    #       }
    #     })

    #   volumes <- shinyFiles::getVolumes()
    #   shinyFileChoose(input, 'file', roots=volumes, session=session,
    #                   restrictions=system.file(package='base'),filetypes=c('xlsx'))

    #   hot_path <- reactive ({
    #     validate(
    #       need(input$file != "", label = "Enter an XLSX file")
    #     )
    #     if(length(input$file)==0){return (NULL)}
    #     if(length(input$file)>0){
    #       hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
    #     }
    #   })
