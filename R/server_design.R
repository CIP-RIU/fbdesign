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

          if(input$fbdesign_gentemp==TRUE){ #material list template for parentals


            file.copy(mtl_temp$datapath,paste(mtl_temp$datapath, ".xlsx", sep=""))
            mtl_parental <- readxl::read_excel(paste(mtl_temp$datapath, ".xlsx", sep=""), sheet = "Material_List_Parental")
            mtl_temp <- mtl_parental
            #mtl_temp <- as.data.frame(mtl_parental, stringsAsFactors=FALSE )

          } else { #material list template for clones

            file.copy(mtl_temp$datapath,paste(mtl_temp$datapath, ".xlsx", sep=""))
            mtl_temp <- readxl::read_excel(paste(mtl_temp$datapath, ".xlsx", sep=""), sheet = "Material_List")

          }

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
        if(is_parentList(sel_list)==TRUE ){
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

  output$approvalBox <- renderInfoBox({


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
         subtitle <- paste("Your material list has duplicated genotypes/germplasm names. Please, enter a correct file.")
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


    if(input$fbdesign_gentemp==TRUE){ #using template for parentals
        germoplasm_fem <-  material_table()$Accession_Number_Female #template
        germoplasm_male <- material_table()$Accession_Number_Male #template

        } else { #using hidap parental module

      germoplasm_fem <-  material_table()$female$Accession_Number
      germoplasm_male <- material_table()$male$Accession_Number

     }


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


  })

  # Design of variables ---------------------------------------------------------------
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
      sites <- stringr::str_trim(input$designFieldbook_sites, side="both")

      if(nExp_id=="-"){
        out <- paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites)

      } else {
        out <-  paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites, "_", nExp_id)
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

      # sites_data <- site_table #data from package fbdesign as an internal data BEFORE

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

    fbdesign_sites_selected <- fbdesign_sites()
    #print(locs)
    if (nrow(locs) > 0 ){
      #chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Location",
                            choices = fbdesign_sites_selected, selected = 1,  multiple = FALSE)
   }
  })


# FieldbookApp ------------------------------------------------------------

#FieldbookApp country -----------------------------------------------
  output$oufbDesign_country_fbapp <- shiny::renderUI({

    sites_data <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)
    cntry <- fbsites::get_country_list(sites_data = sites_data) #new code: use file fbsites
    div(style="display: inline-block;vertical-align:top; width: 200px;",
          shiny::selectInput("fbdesign_cntry_fbapp", label="Select Country",
                             choices = cntry, selected = 1, multiple = FALSE)
    )

  })

#FieldbookApp sites reactive -----------------------------------------------
  fbdesign_sites_app <- reactive({
    sites_data <- values$sites_data
    fbsites::get_filter_locality(sites_data = sites_data, country_input= input$fbdesign_cntry_fbapp)
  })

#FieldbookApp location
  output$oufbDesign_location_fbapp <- shiny::renderUI({

    req(input$fbdesign_cntry_fbapp)
    locs <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    fbdesign_sites_selected <- fbdesign_sites_app()
      if (nrow(locs) > 0 ){
       div(style="display: inline-block;vertical-align:top; width: 200px;",
          shiny::selectizeInput("fbdesign_location_fbapp", label = "Select Location",
                            choices = fbdesign_sites_selected, selected = 1,  multiple = FALSE)
       )

     }
  })

# --------------------------------------------------------------------------

# Create an object with the list of file -----------------------------------

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

    mlist <- material_table()

    #mlist <- input$designFieldbook_sel_mlist
    if(is.null(mlist) || mlist == ""){  out <- 0  }
    #is_parent <- is_parentList(mlist)
    tp <- get_type_list_ds(mlist)
    if(tp=="parental") {out <- 1}
    if(tp=="clonal")   {out <- 0}
    return(out)

  })

#The output object will be suspended (not execute) when it is hidden on the web page

  outputOptions(output, 'condition_selmlist', suspendWhenHidden=FALSE)
# End of Create an object with the list of file

# Number of plant per row (calculated variable) ---------------------------
  react_plantxplot <-  shiny::reactive({

    plantxplot <- input$fbDesign_nplantsrow*input$fbDesign_nrowplot
    if(length(plantxplot)==0){plantxplot <- 0}
    plantxplot

  })

#Shiny UI for number of plants per plot -----------------------------------
  output$fbPlant_plot <- shiny::renderUI({

    rpplot <- react_plantxplot()
    shiny::numericInput("fbDesign_nplants",
                        "Number of plants per plot", rpplot , rpplot, rpplot)

  })

# Plot Size Values --------------------------------------------------------
  react_psize <- reactive({
    plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    print(plot_size)
    if(length(plot_size)==0){plot_size <- 0}
    plot_size
  })

# Plot Size ---------------------------------------------------------------
  output$fbPlanting_psize <- shiny::renderUI({
    #plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    plot_size <- react_psize()
    #if(length(plot_size)==0) plot_size <- 2.7
    shiny::numericInput(inputId = "fbDesign_psize", label = "Plot size (m2)",
                        value = plot_size, min = plot_size,max = plot_size)
  })


# Plant densisty ----------------------------------------------------------
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

                           # male <- material_tbl$male$Accession_Number
                           # female <- material_tbl$female$Accession_Number

                         if(input$fbdesign_gentemp==TRUE){ #using template for parentals
                           male <-  material_tbl$Accession_Number_Female #template
                           female <- material_tbl$Accession_Number_Male #template

                         } else { #using hidap parental module

                            male <- material_tbl$male$Accession_Number
                            female <- material_tbl$female$Accession_Number
                         }

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

                         # male <- material_tbl$male$Accession_Number
                         # female <- material_tbl$female$Accession_Number

                         if(input$fbdesign_gentemp==TRUE){ #using template for parentals

                           male <-  material_tbl$Accession_Number_Female #template
                           female <- material_tbl$Accession_Number_Male #template

                         } else { #using hidap parental module

                           male <- material_tbl$male$Accession_Number
                           female <- material_tbl$female$Accession_Number

                         }


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

                     #print(fb)


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
                       ncol_fb <- ncol(fb)
                       p1 <- as.list(rep(NA, times = nsample))
                       names(p1) <- 1:nsample
                       fb <- cbind(fb, p1)
                       print("paso1")
                       print(fb)
                       #Hint: TSSAMPLE is the temporary name for the SUB SAMPLE factor (column). Then, we change for SUBSAMPLE
                       #fbook <- fb %>% tidyr::gather(key= "TSSAMPLE", value= "svalue", paste(1:nsample, sep=""))
                       fb <- reshape2::melt(fb, 1:ncol_fb)


                       print("paso2")
                       #names(fb)

                       #fb <- fb %>% dplyr::select(-svalue)
                       fb <- fb %>% dplyr::select(-value)
                       #fb <- within(fbook, rm(svalue))
                       inst_pos <- which(names(fb) == "INSTN")
                       print(inst_pos)
                       #fb2 <- fb
                       #fb <- append_col(fb, list(SUBSAMPLE= fb$TSSAMPLE), after = inst_pos) %>% dplyr::select(-TSSAMPLE)
                       fb <- append_col(fb, list(SUBSAMPLE= fb$variable), after = inst_pos) %>% dplyr::select(-variable)
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

# Visualization of the field book -----------------------------------------
  shiny::observeEvent(input$fbDesign_draft, {

    # req(input$designFieldbook_sel_mlist)
    # req(input$designFieldbook)
    #print(material_table())
    #print(fbdraft())
    mtl_table <- material_table()
    tpds <- get_type_list_ds(material_table())
    #Flag variable to check certain condition in fieldbooks. Initially declared TRUE
    flag <- TRUE

    if(length(material_table())==0 ) {
      flag <- FALSE
      shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: You have not selected a material list. Please select/upload one"), styleclass = "danger")
    }


    if(input$designFieldbook=="ABD"){ #for augmented design

      mtl <- as.data.frame(mtl_table)
      #mtl <- material_table()
      mtl_instn <- as.character(mtl$Is_control)
      mtl_checks_count <- is_control(mtl_table)

      if(all(is.na(mtl_instn)) || length(mtl_checks_count)==1) {
        flag <- FALSE
        shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: in Augmented Design: At least two checks is needed in 'Is_Control' column. Verify Material List file"), styleclass = "danger")

      } else {

        flag <- TRUE
       }

     #  flag <- TRUE
    }


    if(input$designFieldbook=="WD"){ #for wescott design

      #mtl <- mtl_table
      mtl <- as.data.frame(mtl_table, stringsAsFactors=FALSE)

      trt1 <-  mtl_table$Accession_Number
      trt1 <-  gsub("[[:space:]]","", trt1)
      trt1 <-  setdiff(trt1,"")

      mtl_instn <- as.character(mtl$Is_control)
      mtl_checks_count <- is_control(mtl_table) #number of checks

      trt2 <- is_control(mtl_table) #get checks from material list
      germoplasm <- setdiff(trt1,trt2) #remove controls from material list
      germoplasm <- trt1



      if(all(is.na(mtl_instn)) || length(mtl_checks_count)==1 || length(mtl_checks_count)>2 ) {
        flag <- FALSE
        shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Just two checks are needed in Westcott Design. Verify 'Is_Control' column in your material list"), styleclass = "danger")
      } else if(length(germoplasm)<10){
        shinysky::showshinyalert(session, "alert_fb_done", paste("You need at least 10 genotypes to perform Westcott Design." ), styleclass = "danger")

      } else {

        flag <- TRUE
      }

    }


    if(input$designFieldbook=="AD"){ # for alpha design

      germoplasm <-material_table()$Accession_Number

        print(germoplasm)
        n <- length(germoplasm)
        r <- as.numeric(input$designFieldbook_r)
        k <- as.numeric(input$designFieldbook_k)

        dach <- design.alpha.check(trt= germoplasm,k=k,r=r)

        if(!dach$alfares){
          flag <- FALSE
          ms <- paste(dach$res,". The combination of ",r," and", k, " is wrong using ",n ," genotypes.")
          shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: ", ms), styleclass = "danger")
        } else{
          flag <- TRUE
        }
    }

    #for genetic studies which requieres parental information
    if(tpds=="parental"){

        if(input$design_geneticFieldbook=="NCI"){
          #et_type_list_ds(mtl_table)

          if(input$fbdesign_gentemp==TRUE){ #using template for parentals

            male <-  mtl_table$Accession_Number_Female #template
            female <- mtl_table$Accession_Number_Male #template

          } else { #using hidap parental module

            male <-  mtl_table$male$Accession_Number
            female <-  mtl_table$female$Accession_Number

          }

          set <- as.numeric(input$design_genetic_nc_set)
          r <-  as.numeric(input$design_genetic_r)
          # print(male)
          # print(female)
          # print(length(male)!=length(female))
          print("pass")

          # if(length(male)!=length(female)){
          #
          #   print("1")
          #   flag <- FALSE
          #   shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: The length of males and females must be the same dimension"), styleclass = "danger")
          #
          # } else

            if (r==1 || is.na(r)) {
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: You have entered just 1 replication or NA/NULL values."), styleclass = "danger")

          } else if (is.null(male) || is.na(male)) {
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 males"), styleclass = "danger")

          } else if(length(female)==1 || is.null(female) || is.na(female)) {
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 females"), styleclass = "danger")

          } else if (length(male) %% length(female) == 1){
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: The number of males is not divisible by the number of females"), styleclass = "danger")

          } else if(length(female) %% length(male) == 1) {
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: Number of females must be proportional to number of males."), styleclass = "danger")

          } else if (length(male) %% set == 1) {
             flag <- FALSE
             shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: data length is not a multiple of set variable. Please provide an accurate number of sets"), styleclass = "danger")

          } else if ( (length(female)/length(male)) ==1) {

            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("The number of females must be at least twice the number of males. The material list does not meet this requirement"), styleclass = "danger")

          }



          else {

            flag <- TRUE
          }



        }

        if(input$design_geneticFieldbook=="NCII"){


          #get_type_list_ds(mtl_table)

          if(input$fbdesign_gentemp==TRUE){ #using template for parentals

            male <-  mtl_table$Accession_Number_Female #template
            female <- mtl_table$Accession_Number_Male #template

          } else { #using hidap parental module

            male <-  mtl_table$male$Accession_Number
            female <-  mtl_table$female$Accession_Number

          }

          # male <-  mtl_table$male$Accession_Number
          # female <-  mtl_table$female$Accession_Number

          set <- as.numeric(input$design_genetic_nc_set)
          r <-  as.numeric(input$design_genetic_r)


          # if(length(male)!=length(female)){
          #   flag <- FALSE
          #   shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: The length of males and females must be the same dimension"), styleclass = "danger")
          #
          # } else

          if (r==1 || is.na(r)){
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: You have entered just 1 replication or NA/NULL values."), styleclass = "danger")

          } else if (is.null(male) || is.na(male)){
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 males"), styleclass = "danger")

          } else if(length(female)==1 || is.null(female) || is.na(female)){
            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: At minimimun 1 females"), styleclass = "danger")

          } else if (length(male) %% set ==1){

            flag <- FALSE
            shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: data length is not a multiple of set variable. Please provide an accurate number of sets"), styleclass = "danger")

          } else {

            flag <- TRUE
          }


        }


      flag <- flag

    }


    if(flag){

    print(fbdraft())
    fb <-  fbdraft()

    #fb <- fb[,1:129]
    output$fbDesign_table <- rhandsontable::renderRHandsontable({

      rhandsontable::rhandsontable(fb, readOnly = T)})

    }

  })

# Download fieldbook ------------------------------------------------------
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

            if(input$fbdesign_gentemp==TRUE){ #using template for parentals

              # male <-  mtl_table$Accession_Number_Female #template
              # female <- mtl_table$Accession_Number_Male #template
              mtl_table <- material_table()

            } else { #using hidap parental module

              mtl_table <- material_table()$parental_table

            }

           #mtl_table <- material_table()$parental_table
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

              if(input$fbdesign_gentemp==TRUE){ #using template for parentals

                male <-  mtl_table$Accession_Number_Female #template
                female <- mtl_table$Accession_Number_Male #template

              } else { #using hidap parental module

                male <-  mtl_table$Male_AcceNumb
                female <-  mtl_table$Female_AcceNumb

              }
              #male <-  mtl_table$Male_AcceNumb
              #female <-  mtl_table$Female_AcceNumb



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

              }

              if(length(male) %% set == 1){
                flag <- FALSE
                shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: It is not possible divide the number of males per number of sets."), styleclass = "danger")
              } else if(length(male) %% length(female) == 1) {
                shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR: The number of males is not divisible by the number of females."), styleclass = "danger")
              } else if(length(female) %% length(male) == 1) {
                shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR:Number of females must be proportional to number of males."), styleclass = "danger")
              } else if((length(female)/length(male)) == 1) {
                shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR:The number of females must be at least 2 per male and you just got 1."), styleclass = "danger")
              } else if(length(female) %% set*length(male) == 1) {
                shinysky::showshinyalert(session, "alert_fb_done", paste("ERROR:The number of females, males and sets is not proportional,"), styleclass = "danger")
              } else {

                flag <- TRUE
              }

            }

            if(input$design_geneticFieldbook=="NCII"){



              if(input$fbdesign_gentemp==TRUE){ #using template for parentals

                male <-  mtl_table$Accession_Number_Female #template
                female <- mtl_table$Accession_Number_Male #template

              } else { #using hidap parental module

                male <-  mtl_table$Male_AcceNumb
                female <-  mtl_table$Female_AcceNumb

              }


              # male <-  mtl_table$Male_AcceNumb
              # female <-  mtl_table$Female_AcceNumb


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

              } else if(length(male) %% set == 1){
                shinysky::showshinyalert(session, "alert_fb_done", paste("ERRO: It is not possible divide the number of males per the number of set.") , styleclass = "danger")
              }  else {

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
                print(mtl_table)

                if(is.list(mtl_table) && is.element("Accession_Number_Female",names(mtl_table))
                                      && is.element("Accession_Number_Male",names(mtl_table)) )
                {
                  n <- max(unlist(lapply(mtl_table, function(x)length(x))))
                  length(mtl_table$Accession_Number_Female) <- n
                  length(mtl_table$Accession_Number_Male) <- n
                  length(mtl_table$IDX) <- n
                  res <- cbind(mtl_table$IDX, mtl_table$Accession_Number_Female, mtl_table$Accession_Number_Male) %>% as.data.frame(stringsAsFactors=FALSE)
                  names(res) <- names(mtl_table)
                  mtl_table <- res

                }


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

# Download material list --------------------------------------------------
  output$fbDesign_mlistExport <- downloadHandler(
    filename = function() {
      paste("Material_List", '.xlsx', sep='')
    },
    content = function(file) {

      mt_list<- crop_template_xlsx$Material_List
      hs <- createStyle(fontColour = "#000000", fontSize=12,
                              fontName="Calibri", fgFill = "orange")
      openxlsx::write.xlsx(mt_list, file, headerStyle = hs, sheetName="Material_List", colWidths="auto")
    }
  )

# Download parental material list -----------------------------------------
  output$fbDesign_mlistExportGenTemp <- downloadHandler(
    filename = "Material_list_Parental.xlsx",
    content = function(file) {
      mtlist_parental <- material_list_parental
      hs <- createStyle(fontColour = "#000000", fontSize=12,
                        fontName="Calibri", fgFill = "orange")

      openxlsx::write.xlsx(mtlist_parental, file, headerStyle = hs, sheetName= "Material_List_Parental", colWidths="auto")

    },
    contentType="application/xlsx"
  )

# Download fieldbookApp files for mobile data collection ------------------
  output$fbdesigin_downloadFbAppData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      path <- fbglobal::get_base_dir()
      shiny::withProgress(message = 'Downloading file', value = 0, {

        incProgress(1/6, detail = paste("Reading HIDAP data..."))
        fb <- fbdraft()

        year <- input$fbdesign_year_fbapp #todo : show error if users do not select anything
        country <- input$fbdesign_cntry_fbapp #todo : show error if users do not select anything
        abbr_user_val <- stringr::str_trim(input$fbdesign_abbruser_fbapp, side = "both")
        abbr_user_val <- stringr::str_replace(abbr_user_val, pattern = "[[:space:]]", replacement = "")
        abbr_user_val <- toupper(abbr_user_val) #ALL UPPERCASE
        location <- input$fbdesign_location_fbapp #todo : show error if users do not select anything
        yearcountry <- paste(year, country, sep="") #todo : show error if users do not select anything
        #Nomenclature: YEARLCOUNTR-USERABBR-LOCATION
        fbapp_id <- paste(yearcountry, abbr_user_val, location, sep="-")

        print(fbapp_id)
        print(abbr_user_val)

        if(fbapp_id==""){
          shinysky::showshinyalert(session, "alert_fb_done", paste("Enter trial abbreviationr" ), styleclass = "danger")
        }
        else if(abbr_user_val==""){
          shinysky::showshinyalert(session, "alert_fb_done", paste("Enter 'trial abbreviation'" ), styleclass = "danger")
        }
        else if(fbapp_id!="" && abbr_user_val!=""){

          print("paso")
        design <- stringr::str_trim(input$designFieldbook, side = "both")
        #ToDo: warning de dejar vacio el campo de abbreviation name
        fb <- fb %>% dplyr::mutate(abbr_user = fbapp_id)

        if(design=="UNDR"){

            fb <- fb[, c("abbr_user","PLOT","REP", "INSTN")]
            names(fb) <- c("abbr_user", "plot_number", "rep", "accession_name")
            #fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number", "rep", "accession_name" ) , sep = "_",remove = FALSE )
            fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number" ) , sep = "",remove = FALSE )

        } else  if(design=="CRD"){

            fb <- fb[, c("abbr_user","PLOT","REP", "INSTN")]
            names(fb) <- c("abbr_user", "plot_number", "rep", "accession_name")
            #fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number", "rep", "accession_name" ), sep = "_", remove = FALSE)
            fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number" ) , sep = "",remove = FALSE )


        } else  if(design=="RCBD"){

            fb <- fb[, c("abbr_user","PLOT","REP", "INSTN")]
            names(fb) <- c("abbr_user", "plot_number", "rep", "accession_name")
            #fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number", "rep", "accession_name" ), sep = "_", remove = FALSE)
            fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number") , sep = "",remove = FALSE )

        } else  if(design=="WD"){

            fb <- fb[, c("abbr_user","PLOT", "ROW" , "COLUMN" , "INSTN")]
            names(fb) <- c("abbr_user", "plot_number", "row", "column" , "accession_name")
            #fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number", "row", "column" , "accession_name" ), sep = "_", remove = FALSE)
            fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number") , sep = "",remove = FALSE )

        } else  if(design=="AD"){

            fb <- fb[, c("abbr_user","PLOT","REP", "BLOCK", "INSTN")]
            names(fb) <- c("abbr_user", "plot_number", "rep", "block", "accession_name")
            #fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number", "rep", "block", "accession_name" ), sep = "_", remove = FALSE)
            fb <- fb %>% tidyr::unite_("plot_name",  c("abbr_user", "plot_number") , sep = "",remove = FALSE )
        } else{

            shinysky::showshinyalert(session, "alert_fb_done", paste("This design is not available for the FieldBookApp format. It will be available soon." ), styleclass = "danger")
            fb<- NULL

        }

        fb$abbr_user <- NULL


        incProgress(3/6, detail = paste("Downloading FieldBookApp-SPBase file..."))
        incProgress(4/6, detail = paste("Refreshing HIDAP..."))
        Sys.sleep(3)
        incProgress(5/6, detail = paste("Refreshing HIDAP..."))

        write.csv(fb, con,row.names = FALSE)
        incProgress(6/6, detail = paste("Refreshing HIDAP..."))
        Sys.sleep(5)
      } #end if


      })
    }
  )

  observe({

    toggleState("fbdesigin_downloadFbAppData", stringr::str_trim(input$fbdesign_abbruser_fbapp, side = "both")!= "")

  })


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
