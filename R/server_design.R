#' server_design
#'
#' Design a fieldbook
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @author Omar Benites
#' @export
#'
  #server_design <- function(input, output, session, dom="hot_fieldbook_design", values){

  server_design <- function(input, output, session, values){

# Accession_Number, material_name,material_code,is_control,scale_audpc,family_number,pedigree,
# female_number,female_code,male_number,male_code,seed_source,simultanious_trials,previous_trials

  material_table <- reactive({

    if(input$select_import=="Template") {

        mtl_temp <- input$file

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
      #mtl_temp <- readrds ::read_excel(paste(mtl_temp$datapath, ".xlsx", sep=""), sheet = "Material_List")

      #path <- fbglobal::get_base_dir()
      #path <- paste(path, sel_list, sep = "\\")
      #mtl_temp <- readRDS(path)

      #Just use the original code
      mtl_temp <- readRDS(sel_list)
      #print(mtl_temp)
      mtl_list <- as.list(mtl_temp) #mtl in list format

    }

    }

    mtl_list
  })

#   output$fbDesign_crop <- shiny::renderUI({
#     ct = fbcrops::get_crop_table()
#     chc = as.list(ct$crop_name)
#     nms = paste0(ct$crop_name, " (", ct$crop_id, ")")
#     names(chc) = nms
#     #temp <<- chc
#     shiny::selectInput("designFieldbook_crop", "Crop", chc)
#   })

#   output$fbDesign_program <- shiny::renderUI({
#     if (!is.null(input$designFieldbook_crop)) {
#       tbl = fbcrops::get_crop_table()
#       crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
#
#       tbl = fbprogram::get_program_table()
#       prg = tbl[tbl$crop_id == crp, ]
#
#       if (nrow(prg) > 0 ) {
#         lbl = paste0(prg$program_name, " (", prg$program_id, ")" )
#         chc = as.list(prg$program_id)
#         names(chc) = lbl
#         shiny::selectInput("designFieldbook_program", "Investigation", chc)
#       }
#     }
#   })

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

  output$fbDesign_variables <- shiny::renderUI({
    #crp <- input$designFieldbook_crop
    #mdl <<- fbmodule::list_modules(crp)

    crop <- input$designFieldbook_crop

    if(crop == "potato"){tbl <- table_module_potato } #dataset from fbdesing data folder
    if(crop == "sweetpotato"){tbl <- table_module_sweetpotato } #dataset from fbdesgin data folder

    #mdl <- tbl[tbl$crop == crop, c("module", "module_name")] #HiDAP v1.0 Built_1 (deprecated table form)
    mdl <- tbl[tbl$CROP == crop, c("TRIAL_ABBR", "TRIAL")] #HiDAP v1.0 Built_2

    mdl <- paste0(mdl[,2], " (", mdl[, 1],")")
    mdl <- sort(unique(mdl))

    #ids <- unlist(stringr::str_extract_all(mdl, "([A-Z]{2})"))
    ids <- str_trim(gsub("\\(.*","",mdl),side = "both")
    vls <- mdl
    mdl = as.list(ids)
    names(mdl) <- vls
    #mdl1 <<- mdl
    #print(mdl)
    #shiny::selectInput("designFieldbook_module", label = "Assay (fieldbook module):",
    shiny::selectInput("designFieldbook_module", label = "Types of Trial",
                       choices = mdl, selected = 1)
  })

#   output$designFieldbook_genotypes <- shiny::renderUI({
#     chc <- fbmaterials::list_material_lists(input$designFieldbook_crop, short = TRUE)
#     shiny::selectInput("designFieldbook_trt1", "Genotype list", chc, width = 400)
#   })

  fbdesign_id <- shiny::reactive({
    if (!is.null(input$designFieldbook_crop)) {
      #tbl = fbcrops::get_crop_table()

      tbl <- table_crops
      nExp_id <- input$fbDesign_nExp
      #print(nExp_id)
      crop_id = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
      program_id = input$designFieldbook_program
      phase_id = input$designFieldbook_phase
      module_id = input$designFieldbook_module
      #ayear = input$designFieldbook_year
      #amonth = stringr::str_pad(input$designFieldbook_month, width = 2, side = "left",
      #                                                                      pad = "0")
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

  output$fbDesign_id <- shiny::renderText({
    fbdesign_id()
  })

  output$designFieldbook_traits <- shinyTree::renderTree({

# potato<- readRDS("C:/OMAR-2015/GitHubProjects/shinyTree/trial_module_list.rda")

    #crp <- input$designFieldbook_crop
    #tbl <- fbmodule::get_module_table(input$designFieldbook_crop)
#   if(crp=="potato"){tbl <- table_module_potato}
#   if(crp=="sweetpotato"){tbl <- table_module_sweetpotato}
#
#   tbl <- dplyr::as_data_frame(tbl)
#
#   trait_check_box <- paste(tbl$variable,": ",tbl$variables_name,sep="")
#   #agrego el nombre de los check boxes
#   tbl$trait_check_box <- trait_check_box
#
#   crop_table <- tbl
#
#   trial_module <- crop_table %>% select(.,module_name) %>% unique(.)
#   trial_module <- trial_module[[1]]
#
#   trial_abbr<- crop_table %>% select(.,module) %>% unique(.)
#   trial_abbr <- trial_abbr[[1]]
#
#   trial_module_list <- list()
#   trial_module_list_cb <- list()
#   n <- length(trial_module)
#
#           #for(i in trial_module){
#
#           for(i in 1:n){
#
#             #crop_table %>% filter(., module_name == "yield")
#             #trait_var <- crop_table %>% filter(., module_name == i) %>% select(., variable)
#             trait_var <- crop_table %>% filter(., module_name == trial_module[i]) %>% select(., variable)
#             trait_var <- trait_var[[1]]
#             trait_var <- as.list(trait_var)
#             #extract trait check box names
#             trait_names <- crop_table %>% filter(., module_name == trial_module[i]) %>% select(., trait_check_box)
#             trait_names <- trait_names[[1]]
#
#             names(trait_var) <- trait_names
#             trait_list <- list(trait_var)
#             names(trait_list) <- trial_module[i]
#
#             trial_module_list[[i]] <- trait_list
#           }
#
#           trial_module_list
#            a <- NULL
#           a1 <- trial_module_list[[1]]
#           for(i in 2:n){
#             a <- c(a1,trial_module_list[[i]])
#             a1 <- a
#           }
     #if(crp=="potato"){tbl <- crop_list$crop_list$potato}
     #if(crp=="sweetpotato"){tbl <- crop_list$crop_list$sweetpotato}

       #Trait_List is a fbdesign dataset. This data contains all the traits hierachy divided by crops.
       a <- Trait_List
       #saveRDS(object = a,file = "pina.rds")
       #a <- crop_list
      #a
  })

  output$fbDesign_country <- shiny::renderUI({
     #sites_data <- fbsites::get_site_table() #before
     sites_data <- site_table #data from package
     cntry <- fbsites::get_country_list(sites_data = sites_data)
     shiny::selectizeInput("fbDesign_countryTrial", label = "Field Country:",
                            choices = cntry, selected = 1,  multiple = FALSE)

  })

  fbdesign_sites <- reactive({
    #sites_data <<-  fbsites::get_site_table() #before
    sites_data <- site_table #using data from package
    fbsites::get_filter_locality(sites_data = sites_data, country_input= input$fbDesign_countryTrial)
  })

  output$fbDesign_countrySite <- shiny::renderUI({
    #locs = fbsites::get_site_table() #before
    locs <- site_table #using data from package
    fbdesign_sites_selected <- fbdesign_sites()
    #print(locs)
    if (nrow(locs) > 0 ){
      #chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Field locations:",
                            choices = fbdesign_sites_selected, selected = 1,  multiple = FALSE)
   }
  })


  ### Create an object with the list of file ----------------------------------


#   sel_list <- reactive ({
#     #dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".dbf|.rds")
#     dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".rds")
#     lg_dbf <- length(dbf_file_list)
#
#
#
#
#     if(lg_dbf == 0) { gmtfiles <- "" }
#     if(lg_dbf>0)    {
#       ignore_temps <- grepl(pattern = "~\\$",x =  dbf_file_list)
#       dbf_file_list <-  dbf_file_list[!ignore_temps]
#       short_name <- basename(dbf_file_list)
#       gmtfiles <- data.frame(short_name, dbf_file_list, stringsAsFactors = FALSE)
#       names(gmtfiles) <- c("short_name","full_name")
#
#       out_list <- c("dspotatotrials_dpassport.rds", "dssweettrials_dpassport.rds", "potato_pedigree.rds", "sweetpotato_pedigree.rds")
#       gmtfiles <- dplyr::filter(.data = gmtfiles, !(short_name %in% out_list))
#
#
#       gmtfiles
#     }
#
#     mtl_files <- gmtfiles$full_name
#     mtl_files
#
#   }) #first step

  #sel_list_value <- reactiveValues(sel_list_mtl = NULL) #second step

#   observe({
#      #input$fdesign_list_refresh
#     values$sel_list_mtl <- sel_list()
#   })

  output$fbDesign_selmlist <- shiny::renderUI({

    input$fdesign_list_refresh
    #res <- sel_list()
    res <- fbdesign_mtl_files()
    #observe({
    #chois <- fbdesgin_mtl_files()$full_name
#     shiny::selectInput(inputId = "designFieldbook_sel_mlist", label = "Select a Material List",
#                        choices =  res, width = "70%" )

    selectizeInput(inputId = "designFieldbook_sel_mlist", label = "Select a Material List", width="100%",
                   choices = res,
                   options = list(
                     placeholder = 'Please select a material list',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
    #shiny::updateSelectInput(session, inputId = "designFieldbook_sel_mlist", choices = chois)
  })


  ### End of Create an object with the list of file ----------------------------------

  #Plot Size Values
  react_psize <- reactive({
    plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    print(plot_size)
    if(length(plot_size)==0){plot_size <- 0}
    plot_size
  })

  #Plant Density Values
  output$fbPlanting_psize <- shiny::renderUI({
    #plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot*input$fbDesign_distRows
    plot_size <- react_psize()
    #if(length(plot_size)==0) plot_size <- 2.7
    shiny::numericInput(inputId = "fbDesign_psize", label = "Plot size (m2)",
                        value = plot_size, min = plot_size,max = plot_size)
  })

  react_pdensity <- reactive({

    plant_density <- (input$fbDesign_nplants/input$fbDesign_psize)*10000
    print(plant_density)
    if(length(plant_density)==0){plant_density <- 0}
    plant_density
  })

  output$fbPlanting_pdensity <- shiny::renderUI({
    plant_density <- react_pdensity()
    #if(length(plant_density)==0) plant_density <- 37037.037
    shiny::numericInput(inputId = "fbDesign_pdensity", label = "Plant Density (plants/Ha)",
                        value = plant_density, min = plant_density, max = plant_density)
  })

  output$alphaMessage <- shiny::renderText({
    #germoplasm <-material_table()$Institutional_number
    germoplasm <-material_table()$Accession_Number
    if(!is.null(germoplasm)){

    print(germoplasm)
    n <- length(germoplasm)
    r <- as.numeric(input$designFieldbook_r)
    k <- as.numeric(input$designFieldbook_k)
   # "omar benites alfaro"
    #paste(n,r,k,sep="")
    dach <- design.alpha.check(trt= germoplasm,k=k,r=r)
     if(!dach$alfares){
       paste(dach$res,". The combination of ",r," and", k, " is wrong using ",n ," genotypes.")
     } else {
       paste("The combination of replication (r) and block size (k) is perfect!")
     }
    }
    else{
      paste("Please enter your genotypes in the Germoplams List.")
    }
})

  fbdraft <- shiny::reactive({
    try({
      withProgress(message = 'Fieldbook loading...',
                   detail = 'This may take a while...', value = 0, {
                     incProgress(3/15)
                     crp <- input$designFieldbook_crop
                     # print(crp)
                     # print(fbglobal::fname_material_lists(crp))
                     # print(input$designFieldbook_trt1)
                     incProgress(3/15)

                 ### !Hidden for a while
                     #fn =file.path(fbglobal::fname_material_lists(crp), input$designFieldbook_trt1)
                 ### !Hidden for a while
                     # print(fn)
                     #load(fn)

                     #table_materials <- germoplasm_list()
                     material_tbl = material_table()
                     #trt1 <- table_materials$institutional_number
                     #trt1 = material_tbl$Institutional_number
                     trt1 = material_tbl$Accession_Number

                     incProgress(3/15)
                     mdl = input$designFieldbook_module
                     mdl = stringr::str_extract(mdl, "([A-Z]{2})")[[1]]
                     #vars = fbmodule::get_module_table(crp)
                     #vars = vars[vars$module == mdl, "variable"]
                     #if(is.null(input$tree)) {print("pass")}

                     ### Cleaning Trait List  ----------------------------------------
                     ## Remove all the trial names from the Trait List
                     #Ex: remove yield, late blight, etc from Trait List when used pick ones
                     vars = get_tree_value(input$designFieldbook_traits,crop_selected = crp)
                     if(crp == "potato"){tbl <- table_module_potato }
                     if(crp == "sweetpotato"){tbl <- table_module_sweetpotato }
                     mdl <- tbl[tbl$CROP == crp, c("TRIAL_ABBR", "TRIAL")] #HiDAP v1.0 Built_2
                     mdl <- paste0(mdl[,2], " (", mdl[, 1],")")
                     mdl <- sort(unique(mdl))
                     ids <- str_trim(gsub("\\(.*","",mdl),side = "both")
                     vars <- vars[!(vars %in% ids)]
                     ### End of cleaning the Trait List abbreviations ----------------------------------------

                     sub_design = as.character(input$sub_design)

                     if(is.null(sub_design)) sub_design <- NULL
                     factor_lvl1 <- input$factor_lvl1 %>% as.character() %>% str_trim(.,side = "both")
                     factor_lvl2 <- input$factor_lvl2 %>% as.character() %>% str_trim(.,side = "both")
                     factor_lvl3 <- input$factor_lvl3 %>% as.character() %>% str_trim(.,side = "both")
                     trt2 <- c(factor_lvl1,factor_lvl2,factor_lvl3)

                     trt2 <- trt2[!is.na(trt2) & trt2!=""]

                     if(input$designFieldbook=="ABD"){
                       #NOTE: In ABD(Augmented Design)  design.dau(trt1 = checks, trt2= genotypes)
                       #For this reason : design.dau( trt1= checks =trt2 ; trt2 = genotypes=trt1)
                       trt2 <- is_control(material_tbl)
                       print(trt2)
                     }

                     # print(trt1)
                     # print(input$designFieldbook)
                     incProgress(3/15)

                     fb = design_fieldbook(design = input$designFieldbook,
                                           trt1 = trt1,
                                           trt2 = trt2,
                                           sub_design=input$sub_design,
                                           trt1_label = "INSTN",
                                           r = as.integer(input$designFieldbook_r),
                                           k = as.integer(input$designFieldbook_k),
                                           first=TRUE,
                                           #first = as.logical(input$designFieldbook_first),
                                           cont = as.logical(input$designFieldbook_cont),
                                           series = as.integer(input$designFieldbook_serie),
                                           zigzag = as.logical(input$designFieldbook_zigzag),
                                           variables = vars)
                     #print(fb)
                     fb[, 1] = as.integer(fb[, 1])

                    fb
                    print(fb)
                    } )
    })
  })

  shiny::observeEvent(input$fbDesign_draft, {
    fb = fbdraft()

    #print(fb)
    output$fbDesign_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(fb, readOnly = T)})

  })

  shiny::observeEvent(input$fbDesign_create, {
    #print("Heyoo")
    withProgress(message = "Downloading Fieldbook..",value= 0,
                 {
        fb = fbdraft()
        try({

      #passing parameters to vars
      fn = paste0(fbdesign_id(), ".rda")
      #fp = file.path(fbglobal::fname_fieldbooks(input$designFieldbook_crop), fn)


      #before fbglobal
      print(getwd())
      path <- fbglobal::get_base_dir()

      fp <- file.path(path, fn)
      #end before


      #before fbglobal
      #print(getwd())
      #fp <- file.path(getwd(), fn)
      #end before



      mtl_table <- as.data.frame(material_table())

      begin_date <- input$fbDesign_project_time_line[1]
      begin_date <- unlist(str_split(begin_date,"-"))
      begin_date1 <- paste(begin_date[3],begin_date[2],begin_date[1],sep="/")

      end_date <- input$fbDesign_project_time_line[2]
      end_date <- unlist(str_split(end_date,"-"))
      end_date1 <- paste(end_date[3],end_date[2],end_date[1],sep="/")

      #trt1 <- table_materials$institutional_number

      if(!file.exists(fp)) {

        #path <- fbglobal::get_base_dir()
        #path <- paste(path, fp, sep="\\")
        #saveRDS(fb, path)

        saveRDS(fb, fp)
        values[["ph_fb_list"]] = NULL
        #shinyBS::createAlert(session, "alert_fb_done", "fbdoneAlert", title = "Success",
        #                     content = "Fieldbook created.", append = FALSE)
        shinysky::showshinyalert(session, "alert_fb_done", paste("GREAT: Fieldbook successfully created!", "success"),
                                 styleclass = "success")

        ##after fbglobal
        xlsx_path <- fbglobal::get_base_dir()
        xlsx_path <- file.path(xlsx_path, fbdesign_id())
        fn_xlsx <- paste(xlsx_path, ".xlsx",sep="")
        ##

        ## before fbglobal
          #fn_xlsx <- paste(fbdesign_id(),".xlsx",sep="")

      #



        openxlsx::write.xlsx(fb,fn_xlsx,sheet="Fieldbook",overwrite=TRUE)
        add_fieldbook_sheet(file = fn_xlsx, fieldbook = fb)

        #fn <- crop_template_xlsx
        crop_template <- crop_template_xlsx #dataset loaded from fbdesign package
        #crop_template <- readRDS(fn)

        add_varlist_sheet(file=fn_xlsx, crop_template = crop_template,
                          crop=input$designFieldbook_crop,
                          trait_list = input$designFieldbook_traits)

        #add_minimal_sheet(file=fn_xlsx, crop_template=crop_template,col_name="Value",short_name= fbdesign_id(),
        add_minimal_sheet(file=fn_xlsx, crop_template=crop_template, col_name="Value", Trial_name= fbdesign_id(),
                          crop= input$designFieldbook_crop,
                          type_trial= input$designFieldbook_module,
                          #begin_date=input$fbDesign_project_time_line[1],
                          begin_date = begin_date1,
                          #end_date = input$fbDesign_project_time_line[2],
                          end_date = end_date1,
                          site_short_name = input$designFieldbook_sites,
                          country = input$fbDesign_countryTrial)

        add_installation_sheet(file=fn_xlsx, crop_template = crop_template,col_name = "Value",
                               exp_design = input$designFieldbook,
                               genetic_design = NA,
                               rep = input$designFieldbook_r,block=NA,
                               exp_env = input$fbDesign_environment_type, plot_start_number = NA,
                               n_plant_plot = input$fbDesign_nplants,
                               n_plant_row = input$fbDesign_nplantsrow,
                               plot_size = input$fbDesign_psize,
                               plant_density = react_pdensity(),
                               distance_plants = input$fbDesign_distPlants,
                               distance_rows = input$fbDesign_distRows,
                               factor_name = input$factor_name,
                               factor_name_1 = input$factor_lvl1,
                               factor_name_2 = input$factor_lvl2,
                               factor_name_3 = input$factor_lvl3
                               #factor_name_4 = input$factor_lvl4
                               #factor_name_5 = input$factor_lvl5
                               )

        add_metadata_sheet(file=fn_xlsx, crop_template = crop_template, soil_input = input$fbDesign_soil_cb,
                           weather_input = input$fbDesign_weather_cb)
### !Hidden for a while
#         add_material_sheet(file=fn_xlsx, crop_template=crop_template, crop= input$designFieldbook_crop,
#                            material_list = input$designFieldbook_trt1)

        add_material_sheet(file=fn_xlsx, crop_template=crop_template, crop= input$designFieldbook_crop,
                           material_list = mtl_table)


        add_cmanagment_sheet(file=fn_xlsx,
                             crop_template = crop_template,
                             crop=input$designFieldbook_crop,
                             trait_list = input$designFieldbook_traits)


        shell.exec(fn_xlsx)
      }

      if(file.exists(fp)){
        #shinyjs::reset("designFieldbook_sel_mlist")
        #shinyBS::createAlert(session, "alert_fb_done", "fbdoneAlert", title = "Warning",style = "warning",
        #                     content = "This fieldbook already exists in HiDAP. Please Select Experiment Number in Crop & Location", append = FALSE)
        shinysky::showshinyalert(session, "alert_fb_done", paste("WARNING: This fieldbook already exists in HiDAP. Please Select Experiment Number in Crop & Location"),
                                 styleclass = "warning")


      }

               })
         })
    })

  output$fbDesign_mlistExport <- downloadHandler(
    filename = function() {
      paste("Material_list", '.xlsx', sep='')
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


} #end of SERVER.R

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
  #       need(input$file != "", label = "Please enter an XLSX file")
  #     )
  #     if(length(input$file)==0){return (NULL)}
  #     if(length(input$file)>0){
  #       hot_file <- as.character(parseFilePaths(volumes, input$file)$datapath)
  #     }
  #   })
