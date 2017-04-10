#' server_design_big
#'
#' Design a fieldbook
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
## @param dom target dom element name
#' @param values reactive values
#' @description Server_design_big provides a server functionalities for Special or Big modules like PVS and ABS
#' @author Omar Benites
#' @export
#'
#server_design <- function(input, output, session, dom="hot_fieldbook_design", values){

server_design_big <- function(input, output, session, values){

  # institutional_number,material_name,material_code,is_control,scale_audpc,family_number,pedigree,
  # female_number,female_code,male_number,male_code,seed_source,simultanious_trials,previous_trials

  material_table_big <- reactive({


    if(input$select_import_big=="Template") {

        mtl_temp <- input$file_big
        if(is.null(mtl_temp)){return()}
        if(!is.null(mtl_temp)){

          file.copy(mtl_temp$datapath,paste(mtl_temp$datapath, ".xlsx", sep=""))
          mtl_temp <- readxl::read_excel(paste(mtl_temp$datapath, ".xlsx", sep=""), sheet = "Material_List")

          mtl_list <- as.list(mtl_temp) #mtl in list format
          print(mtl_list)
        }

    }

    if(input$select_import_big=="Local List"){

      sel_list <- input$designFieldbook_sel_mlist_big
      #print(sel_list)
      if(is.null(sel_list) || sel_list == ""){  return() }
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





  output$approvalBox_big <- renderInfoBox({

    #germoplasm <-material_table_big()$Institutional_number
    germoplasm <-material_table_big()$Accession_Number

    print( germoplasm)

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

  output$fbDesign_variables_big <- shiny::renderUI({
     crop <- input$designFieldbook_crop_big

#     if(crop == "potato"){tbl <- table_module_potato }
#     if(crop == "sweetpotato"){tbl <- table_module_sweetpotato }
      if(crop == "potato"){tbl <- table_module_big }
      if(crop == "sweetpotato"){tbl <- table_module_big }

    #mdl <- tbl[tbl$crop == crop, c("module", "module_name")] #HiDAP v1.0 Built_1 (deprecated table form)
    mdl <- tbl[tbl$CROP == crop, c("TRIAL_ABBR", "TRIAL")] #HiDAP v1.0 Built_2

    mdl <- paste0(mdl[,2], " (", mdl[, 1],")")
    mdl <- sort(unique(mdl))

    #ids <- unlist(stringr::str_extract_all(mdl, "([A-Z]{2})"))
    ids <- str_trim(gsub("\\(.*","",mdl),side = "both")
    vls <- mdl
    mdl = as.list(ids)
    names(mdl) <- vls
    shiny::selectInput("designFieldbook_module_big", label = "Types of Trial",
                       choices = mdl, selected = 2)
  })

  fbdesign_id_big <- shiny::reactive({
    if (!is.null(input$designFieldbook_crop_big)) {
      #tbl = fbcrops::get_crop_table()

      tbl <- table_crops
      nExp_id <- input$fbDesign_nExp_big
      #print(nExp_id)
      crop_id  <-  tbl[tbl$crop_name == input$designFieldbook_crop_big, "crop_id"]
      program_id  <-  input$designFieldbook_program_big
      phase_id  <-  input$designFieldbook_phase_big
      module_id  <-  input$designFieldbook_module_big
      if(!is.null(module_id)){module_id <- unique(as.character(table_module_big[table_module_big$TRIAL==module_id,"TRIAL_ABBR"]))}

      #ayear = input$designFieldbook_year
      #amonth = stringr::str_pad(input$designFieldbook_month, width = 2, side = "left",
      #                                                                      pad = "0")
      date_book <- input$fbDesign_project_time_line_big[1]
      date_book <- unlist(str_split(date_book,"-"))
      date_book <- paste(date_book[2],date_book[1],sep="")

      #sites = input$designFieldbook_sites
      sites <- stringr::str_trim(input$designFieldbook_sites_big,side="both")

      if(nExp_id=="-"){
        out = paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites)
      } else {
        out = paste0(crop_id, program_id, phase_id, module_id, date_book,"_", sites, "_",nExp_id)
      }

      paste(out, collapse = ", ")
    }
  })
#
  output$fbDesign_id_big <- shiny::renderText({
    fbdesign_id_big()
  })

  output$designFieldbook_traits_big <- shinyTree::renderTree({

    a <- Evaluation_Forms #dataset included in data folder
    a
  })
#

  shiny::observe({
    path <- fbglobal::get_base_dir()
    #print(path)
    geodb_file <- "table_sites.rds"
    #path <- paste(path, geodb_file, sep = "\\")
    path <- file.path(path, geodb_file)
    values$sites_data_big <-  readRDS(file = path)
    #     values$geo_db <-  readRDS(file = "sites_table.rds")

  })

  output$fbDesign_country_big <- shiny::renderUI({
    #sites_data <- fbsites::get_site_table() #before

    #sites_data <- site_table #data from package fbdesing (old code)

    sites_data <- values$sites_data_big # new code:

    #cntry <- fbsites::get_country_list(sites_data = sites_data)
    cntry <- fbsites::get_country_list(sites_data = sites_data)

    shiny::selectizeInput("fbDesign_countryTrial_big", label = "Field Country:",
                          choices = cntry, selected = 1,  multiple = FALSE)

  })

  fbdesign_sites_big <- reactive({
    #sites_data <<-  fbsites::get_site_table() #before
    #sites_data <- site_table #using data from package
    sites_data <- values$sites_data_big # new code:

    fbsites::get_filter_locality(sites_data = sites_data, country_input= input$fbDesign_countryTrial_big)
  })

  output$fbDesign_countrySite_big <- shiny::renderUI({
    #locs = fbsites::get_site_table() #before

    req(input$fbDesign_countryTrial_big)

    locs <- values$sites_data #using data from package
    fbdesign_sites_selected <- fbdesign_sites_big()
    locs <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    #print(locs)
    if (nrow(locs) > 0 ){
      #chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites_big", label = "Field locations:",
                            choices = fbdesign_sites_selected, selected = 2,  multiple = FALSE)
    }
  })
#
  #Plot Size Values
  react_psize_big <- reactive({
    plot_size <- input$fbDesign_nplantsrow_big*input$fbDesign_distPlants_big*input$fbDesign_nrowplot_big*input$fbDesign_distRows_big
    print(plot_size)
    if(length(plot_size)==0){plot_size <- 0}
    plot_size
  })
#
  #Plant Density Values
  output$fbPlanting_psize_big <- shiny::renderUI({
    #plot_size <- input$fbDesign_nplantsrow*input$fbDesign_distPlants*input$fbDesign_nrowplot_big*input$fbDesign_distRows
    plot_size <- react_psize_big()
    #if(length(plot_size)==0) plot_size <- 2.7
    shiny::numericInput(inputId = "fbDesign_psize_big", label = "Plot size (m2)",
                        value = plot_size, min = plot_size,max = plot_size)
  })
#
  react_pdensity_big <- reactive({

    plant_density <- (input$fbDesign_nplants_big/input$fbDesign_psize_big)*10000
    print(plant_density)
    if(length(plant_density)==0){plant_density <- 0}
    plant_density
  })
#
  output$fbPlanting_pdensity_big <- shiny::renderUI({
    plant_density <- react_pdensity_big()
    #if(length(plant_density)==0) plant_density <- 37037.037
    shiny::numericInput(inputId = "fbDesign_pdensity_big", label = "Plant Density (plants/Ha)",
                        value = plant_density, min = plant_density, max = plant_density)
  })
#
  output$alphaMessage_big <- shiny::renderText({

    #germoplasm <-material_table_big()$Institutional_number
    germoplasm <-material_table_big()$Accession_Number

    if(!is.null(germoplasm)){

      print(germoplasm)
      n <- length(germoplasm)
      r <- as.numeric(input$designFieldbook_r_big)
      k <- as.numeric(input$designFieldbook_k_big)
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
      paste("Enter your genotypes in the Germoplams List.")
    }
  })
#
  fbdraft_big <- shiny::reactive({
    try({
      withProgress(message = 'Fieldbook loading...',
                   detail = 'This may take a while...', value = 0, {
                     incProgress(3/15)
                     crp <- input$designFieldbook_crop_big
                     incProgress(3/15)
                     #table_materials <- germoplasm_list()
                     material_tbl = material_table_big()
                     #trt1 <- table_materials$institutional_number

                     #trt1 = material_tbl$Institutional_number
                     trt1 = material_tbl$Accession_Number

                     incProgress(3/15)
                     mdl = input$designFieldbook_module_big
                     mdl = stringr::str_extract(mdl, "([A-Z]{2})")[[1]]
                     sub_design = as.character(input$sub_design_big)

                     sheet_list = big_get_tree_value(input$designFieldbook_traits_big, crop_selected = crp)

                     #v <- sheet_list
                     #saveRDS(input$designFieldbook_traits_big,"big_list.rda")

                     n_org_mother <- input$designFieldbook_n_org_mother
                     #n_org_mother <- as.numeric(gsub(pattern = "*.-",replacement = "",x = n_org_mother))
                     #print(n_org_mother)
                     n_org_baby <- input$designFieldbook_n_org_baby
                     #n_org_baby <- as.numeric(gsub(pattern = "*.-",replacement = "",x = n_org_baby))
                     #print(n_org_baby)

                     if(is.null(sub_design)) sub_design <- NULL
                     factor_lvl1 <- input$factor_lvl1_big %>% as.character() %>% str_trim(.,side = "both")
                     factor_lvl2 <- input$factor_lvl2_big %>% as.character() %>% str_trim(.,side = "both")
                     factor_lvl3 <- input$factor_lvl3_big %>% as.character() %>% str_trim(.,side = "both")
                     trt2 <- c(factor_lvl1,factor_lvl2,factor_lvl3)

                     trt2 <- trt2[!is.na(trt2) & trt2!=""]

                     if(input$designFieldbook_big=="ABD"){
                       #NOTE: In ABD(Augmented Design)  design.dau(trt1 = checks, trt2= genotypes)
                       #For this reason : design.dau( trt1= checks =trt2 ; trt2 = genotypes=trt1)
                       trt2 <- is_control(material_tbl())
                       #print(trt2)
                     }

                     incProgress(3/15)
                     sheet_names <- names(sheet_list)
                     #print(sheet_names)

                    fb <- list()
                    for(i in 1:length(sheet_names)){
                    #form <- sheet_list[[sheet_names]][["form"]]
                    vars <- sheet_list[[sheet_names[i]]][["var"]]


                    if(is.element(el = sheet_names[[i]], set = "F1_selection_criteria")){

                       fb[[i]] <- big_crop_template_xlsx$F1_selection_criteria

                    }
                    else if (is.element(el = sheet_names[[i]], set = "F5_harvest_baby")){
                      #In baby trial you must enter another replication paramterer for randomizations (r_baby)
                      fb[[i]]  <-  design_fieldbook(design = input$designFieldbook_big,
                                                    trt1 = trt1,
                                                    #trt2 = trt2,
                                                    #sub_design=input$sub_design_big,
                                                    trt1_label = "INSTN",
                                                    r = as.integer(input$designFieldbook_r_big_baby),
                                                    #k = as.integer(input$designFieldbook_k_big),
                                                    first = TRUE,
                                                    #first = as.logical(input$designFieldbook_first),
                                                    cont = as.logical(input$designFieldbook_cont_big),
                                                    series = as.integer(input$designFieldbook_serie_big),
                                                    zigzag = as.logical(input$designFieldbook_zigzag_big),
                                                    variables = vars)
                      #saveRDS(object = fb[[i]],file = "fb.rda")
                      fb[[i]][, 1] = as.integer(fb[[i]][, 1])

                    }

                    else if (is.element(el = sheet_names[[i]], set ="F6_organoleptic_mother")){

                      fb[[i]] <- organoleptic_form(form_template = big_crop_template_xlsx$F6_organoleptic_mother,
                                                   genotypes = trt1, n_forms = n_org_mother)

                    }
                    else if (is.element(el = sheet_names[[i]], set = "F7_organoleptic_baby")){
                      fb[[i]] <- organoleptic_form(form_template = big_crop_template_xlsx$F7_organoleptic_baby,
                                                   genotypes = trt1,n_forms = n_org_baby)
                    }
                    else {
                    fb[[i]]  <-  design_fieldbook(design = input$designFieldbook_big,
                                           trt1 = trt1,
                                           #trt2 = trt2,
                                           sub_design=input$sub_design_big,
                                           trt1_label = "INSTN",
                                           #r = as.integer(input$designFieldbook_r_big),
                                           r = as.integer(input$designFieldbook_r_big),
                                           k = as.integer(input$designFieldbook_k_big),
                                           first = TRUE,
                                           #first = as.logical(input$designFieldbook_first),
                                           cont = as.logical(input$designFieldbook_cont_big),
                                           series = as.integer(input$designFieldbook_serie_big),
                                           zigzag = as.logical(input$designFieldbook_zigzag_big),
                                           variables = vars)
                    #saveRDS(object = fb[[i]],file = "fb.rda")
                    fb[[i]][, 1] = as.integer(fb[[i]][, 1])
                   }
                  }
                  names(fb) <- sheet_names
                  fb
                  #save(fb,file="fb.rda")
          } )
    })
  })
#


  shiny::observeEvent(input$fbDesign_draft_big, {
    fb = fbdraft_big()
    #save(fb,"fb.rda")
    #print(fb)
    #datos <<- fb

    #genotypes <- material_table_big()$Institutional_number
    genotypes <- material_table_big()$Accession_Number


    # F1 Selection Criteria ---------------------------------------------------

    if(is.element(el = "F1_selection_criteria", set = names(fb))){
      output$fbDesign_table_big_f1 <- rhandsontable::renderRHandsontable({
       rhandsontable::rhandsontable(fb[["F1_selection_criteria"]], readOnly = T)
      })
    }

    if(!is.element(el = "F1_selection_criteria", set = names(fb))){
      output$fbDesign_table_big_f1 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }

    # F2 Select Clones Flowering --------------------------------------------------


    if(is.element(el = "F2_select_clones_flowering", set = names(fb))){
    output$fbDesign_table_big_f2 <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(fb[["F2_select_clones_flowering"]], readOnly = T)
      })
    }

    if(!is.element(el = "F2_select_clones_flowering", set = names(fb))){
      output$fbDesign_table_big_f2 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }

    # F3 Select Clones Harvest --------------------------------------------------


    if(is.element(el = "F3_select_clones_harvest", set = names(fb))){
      output$fbDesign_table_big_f3 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(fb[["F3_select_clones_harvest"]], readOnly = T)
      })
    }

    if(!is.element(el = "F3_select_clones_harvest", set = names(fb))){
      output$fbDesign_table_big_f3 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }

    # F4 Harvest Mother Form --------------------------------------------------

    if(is.element(el = "F4_harvest_mother", set = names(fb))){
          output$fbDesign_table_big_f4 <- rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(fb[["F4_harvest_mother"]], readOnly = T)
          })
    }

    if(!is.element(el =  "F4_harvest_mother", set = names(fb))){
      output$fbDesign_table_big_f4 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }


    # F5 Fieldbook Form -------------------------------------------------------

    if(is.element(el = "F5_harvest_baby", set = names(fb))){
        output$fbDesign_table_big_f5 <- rhandsontable::renderRHandsontable({
          rhandsontable::rhandsontable(fb[["F5_harvest_baby"]], readOnly = T)
        })
     }


    if(!is.element(el = "F5_harvest_baby", set = names(fb))){
      output$fbDesign_table_big_f5 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }

    # print(fb[["F6_organoleptic_mother"]])
    # print("f6")


    # F6 Fieldbook Form -------------------------------------------------------

    if(is.element(el = "F6_organoleptic_mother", set = names(fb))){

      #$fieldbook_form_f7 <- organoleptic_form(fb[["F6_organoleptic_mother"]],)
      output$fbDesign_table_big_f6 <- rhandsontable::renderRHandsontable({
       # rhandsontable::rhandsontable(fieldbook_form_f7, readOnly = T)
        rhandsontable::rhandsontable(fb[["F6_organoleptic_mother"]])
      })
    }

    # print("end f6")

    if(!is.element(el = "F6_organoleptic_mother", set = names(fb))){
      output$fbDesign_table_big_f6 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }
    # print("end f6")
    # print("begin f7")
    # print(fb[["F7_organoleptic_mother"]])


     # F7 Organoleptic baby ----------------------------------------------------

    if(is.element(el = "F7_organoleptic_baby", set = names(fb))){
      #fieldbook_form_f7 <- organoleptic_form(big_crop_template_xlsx$F7_organoleptic_baby, genotypes = trt1)
      output$fbDesign_table_big_f7 <- rhandsontable::renderRHandsontable({
        #rhandsontable::rhandsontable(fieldbook_form_f7, readOnly = T)
        rhandsontable::rhandsontable(fb[["F7_organoleptic_baby"]])
      })
    }
    # print("end f7")
    if(!is.element(el = "F7_organoleptic_baby", set = names(fb))){
      output$fbDesign_table_big_f7 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }

    # F8 Dormancy -------------------------------------------------

    if(is.element(el = "F8_postharvest_dormancy", set = names(fb))){
          output$fbDesign_table_big_f8 <- rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(fb[["F8_postharvest_dormancy"]], readOnly = T)
          })
    }

    if(!is.element(el = "F8_postharvest_dormancy", set = names(fb))){
      output$fbDesign_table_big_f8 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }

    # F9 PostHarvest dormancy -------------------------------------------------


    if(is.element(el = "F9_postharvest_clones_storage", set = names(fb))){
          output$fbDesign_table_big_f9 <- rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(fb[["F9_postharvest_clones_storage"]], readOnly = T)
          })
    }

    if(!is.element(el = "F9_postharvest_clones_storage", set = names(fb))){
      output$fbDesign_table_big_f9 <- rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(data.frame(), readOnly = T)
      })
    }

})


# Lista de materiales Local en BIG MODULES --------------------------------

  output$fbDesign_selmlist_big <- shiny::renderUI({

    input$fdesign_list_refresh_big

    res <- fbdesign_mtl_files()  #this come from util.R fbdesign package

    #observe({
    #chois <- fbdesgin_mtl_files()$full_name
    #     shiny::selectInput(inputId = "designFieldbook_sel_mlist", label = "Select a Material List",
    #                        choices =  res, width = "70%" )

    selectizeInput(inputId = "designFieldbook_sel_mlist_big", label = "Select a material list", width="100%",
                   choices = res,
                   options = list(
                     placeholder = 'Select a material list',
                     onInitialize = I('function() { this.setValue(""); }')
                   ))
    #shiny::updateSelectInput(session, inputId = "designFieldbook_sel_mlist", choices = chois)
  })

  shiny::observeEvent(input$fbDesign_create_big, {
    #print("Heyoo")
    withProgress(message = "Downloading Fieldbook..",detail = 'This may take a while...', value= 0,
                 {

                    fb  <-  fbdraft_big()
                    #saveRDS(fb,file="fbglobal.rda")
                    try({
                      incProgress(2/15)
                     #passing parameters to vars
                     fn = paste0(fbdesign_id_big(), ".rda")
                     #fp = file.path(fbglobal::fname_fieldbooks(input$designFieldbook_crop), fn)
                     #print("paso1")
                     fp <- file.path(getwd(), fn)

                     mtl_table <- as.data.frame(material_table_big())

                     begin_date <- input$fbDesign_project_time_line_big[1]
                     begin_date <- unlist(str_split(begin_date,"-"))
                     begin_date1 <- paste(begin_date[3],begin_date[2],begin_date[1],sep="/")

                     end_date <- input$fbDesign_project_time_line_big[2]
                     end_date <- unlist(str_split(end_date,"-"))
                     end_date1 <- paste(end_date[3],end_date[2],end_date[1],sep="/")
                     #print("paso2")
                     #print(end_date1)
                     incProgress(3/15)
                     if(file.exists(fp)) {

                       shinyBS::createAlert(session, "alert_fb_done_big", "fbdoneAlert", title = "Warning",style = "warning",
                                            content = "This fieldbook exists in HiDAP. Select Experiment Number in Crop", append = FALSE)
                     }

                     if(!file.exists(fp)) {
                       #saveRDS(fb, fp)
                       values[["ph_fb_list"]] = NULL
                       shinyBS::createAlert(session, "alert_fb_done", "fbdoneAlert", title = "Success",
                                            content = "Fieldbook created.", append = FALSE)

                       fn_big <- paste(fbdesign_id_big(),".xlsx",sep="")

                       print(getwd())
                       path <- fbglobal::get_base_dir()
                       fn_xlsx <- file.path(path, fn_big)


                       list_table_sheet <- names(fb)
                       print(fb)

                       crop_template <- big_crop_template_xlsx

                       #big_add_fieldbook_sheet(file = fn_xlsx, genotypes= mtl_table$Institutional_number,
                       #                       big_sheet=list_table_sheet, fieldbook = fb, big_crop_template = crop_template)

                       big_add_fieldbook_sheet(file = fn_xlsx, genotypes= mtl_table$Accession_Number,
                                               big_sheet=list_table_sheet, fieldbook = fb, big_crop_template = crop_template)


                       #print("paso4")
#                       fn <- crop_template_xlsx
#                       crop_template <- big_crop_template_xlsx #dataset loaded from fbdesign package
#                        #crop_template <- readRDS(fn)
#
#                        add_varlist_sheet(file=fn_xlsx, crop_template = crop_template,
#                                          crop=input$designFieldbook_crop_big,
#                                          trait_list = input$designFieldbook_traits_big)
#
                       #add_minimal_sheet(file=fn_xlsx, crop_template=crop_template,col_name="Value",short_name= fbdesign_id_big(),
                       add_minimal_sheet(file=fn_xlsx, crop_template=crop_template,col_name="Value",Trial_name= fbdesign_id_big(),
                                         crop= input$designFieldbook_crop_big,
                                         type_trial= input$designFieldbook_module_big,
                                         #begin_date=input$fbDesign_project_time_line[1],
                                         begin_date = begin_date1,
                                         #end_date = input$fbDesign_project_time_line[2],
                                         end_date = end_date1,
                                         site_short_name = input$designFieldbook_sites_big,
                                         country = input$fbDesign_countryTrial_big)
                       #print("paso5")
                       add_installation_sheet(file=fn_xlsx, crop_template = crop_template,col_name = "Value",
                                              exp_design = input$designFieldbook_big,
                                              genetic_design = NA,
                                              rep = input$designFieldbook_r_big,block=NA,
                                              exp_env = input$fbDesign_environment_type_big, plot_start_number = NA,
                                              n_plot_row = input$fbDesign_nrowplot_big,
                                              n_plant_plot = input$fbDesign_nplants_big,
                                              n_plant_row = input$fbDesign_nplantsrow_big,
                                              plot_size = input$fbDesign_psize_big,
                                              plant_density = react_pdensity_big(),
                                              distance_plants = input$fbDesign_distPlants_big,
                                              distance_rows = input$fbDesign_distRows_big,
                                              factor_name = input$factor_name_big,
                                              factor_name_1 = input$factor_lvl1_big,
                                              factor_name_2 = input$factor_lvl2_big,
                                              factor_name_3 = input$factor_lvl3_big
                                              #factor_name_4 = input$factor_lvl4
                                              #factor_name_5 = input$factor_lvl5
                       )
                       #print("paso6")
                       add_metadata_sheet(file=fn_xlsx, crop_template = crop_template, soil_input = input$fbDesign_soil_cb_big,
                                           weather_input = input$fbDesign_weather_cb_big)
#                        ### !Hidden for a while
#                        #         add_material_sheet(file=fn_xlsx, crop_template=crop_template, crop= input$designFieldbook_crop,
#                        #                            material_list = input$designFieldbook_trt1)
                        #print("paso7")
                       add_material_sheet(file=fn_xlsx, crop_template=crop_template, crop= input$designFieldbook_crop_big,
                                          material_list = mtl_table)
#
                       #print("paso8")
                       add_cmanagment_sheet(file=fn_xlsx,
                                            crop_template = crop_template,
                                            crop=input$designFieldbook_crop_big,
                                            trait_list = input$designFieldbook_traits_big)
                       #print("paso9")

                       shell.exec(fn_xlsx)
                     }



                   })
                 })
  })
#
  output$fbDesign_mlistExport_big <- downloadHandler(
    filename = function() {
      paste("Material_list", '.xlsx', sep='')
    },
    content = function(file) {
      #mt_list <- material_list #internal dataset
      mt_list<- crop_template_xlsx$Material_List
      #In case of PVS, we do not need late blight variables into the germoplams/material list.
      mt_list <- dplyr::select(mt_list, -Is_control, -Scale_audpc)

      #       hs <- createStyle(fontColour = "#060505", fontSize=12,
      #                         fontName="Arial Narrow", fgFill = "#4F80BD")
      hs <- createStyle(fontColour = "#000000", fontSize=12,
                        fontName="Calibri", fgFill = "orange")
      openxlsx::write.xlsx(mt_list, file, headerStyle = hs, sheetName="Material_List", colWidths="auto")
    }
  )

} #end of SERVER.R

