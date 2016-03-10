#' server_design
#'
#' Design a fieldbook
#'
#' @param input shinyserver input
#' @param output shinyserver output
#' @param session shinyserver session
#' @param dom target dom element name
#' @param values reactive values
#' @author Reinhard Simon
#' @export
server_design <- function(input, output, session, dom="hot_fieldbook_design", values){

  output$fbDesign_crop <- shiny::renderUI({
    ct = fbcrops::get_crop_table()
    chc = as.list(ct$crop_name)
    nms = paste0(ct$crop_name, " (", ct$crop_id, ")")
    names(chc) = nms
    shiny::selectInput("designFieldbook_crop", "Crop", chc)
  })

  shiny::observe({
    shinyjs::toggle(condition = input$fbDesign_environment_type == "field",
                    selector = "#fbDesignNav li a[data-value=fbDesign_field]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "farmers_field",
                    selector = "#fbDesignNav li a[data-value=fbDesign_farmers_field]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "greenhouse",
                    selector = "#fbDesignNav li a[data-value=fbDesign_greenhouse]")
    shinyjs::toggle(condition = input$fbDesign_environment_type == "screenhouse",
                    selector = "#fbDesignNav li a[data-value=fbDesign_screenhouse]")
    shinyjs::toggle(condition = input$fbDesign_weather_cb,
                    selector = "#fbDesignNav li a[data-value=fbDesign_weather]")
    shinyjs::toggle(condition = input$fbDesign_soil_cb,
                    selector = "#fbDesignNav li a[data-value=fbDesign_soil]")
  })

  output$fbDesign_program <- shiny::renderUI({
    if (!is.null(input$designFieldbook_crop)) {
      tbl = fbcrops::get_crop_table()
      crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]

      tbl = fbprogram::get_program_table()
      prg = tbl[tbl$crop_id == crp, ]

      if (nrow(prg) > 0 ) {
        lbl = paste0(prg$program_name, " (", prg$program_id, ")" )
        chc = as.list(prg$program_id)
        names(chc) = lbl
        shiny::selectInput("designFieldbook_program", "Investigation", chc)
      }
    }
  })

  output$fbDesign_phase <- shiny::renderUI({
    if (!is.null(input$designFieldbook_crop)) {
      tbl = fbcrops::get_crop_table()
      crp = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]

      tbl = fbprstages::get_program_stage_table()
      prg = tbl[tbl$crop_id == crp, ]

      if (nrow(prg) > 0 ) {
        lbl = paste0(prg$program_stage_name, " (", prg$program_stage_id, ")" )
        chc = as.list(prg$program_stage_id)
        names(chc) = lbl
        shiny::selectInput("designFieldbook_phase", "Study", chc)
      }
    }
  })

  output$designFieldbook_genotypes <- shiny::renderUI({
    chc <- fbmaterials::list_material_lists(input$designFieldbook_crop, short = TRUE)
    shiny::selectInput("designFieldbook_trt1", "Genotype list", chc, width = 400)
  })

  fbdesign_id <- shiny::reactive({
    if (!is.null(input$designFieldbook_crop)) {
      tbl = fbcrops::get_crop_table()
      crop_id = tbl[tbl$crop_name == input$designFieldbook_crop, "crop_id"]
      program_id = input$designFieldbook_program
      phase_id = input$designFieldbook_phase
      module_id = input$designFieldbook_module
      ayear = input$designFieldbook_year
      amonth = stringr::str_pad(input$designFieldbook_month, width = 2, side = "left",
                                pad = "0")
      #sites = input$designFieldbook_sites
      sites <- stringr::str_trim(input$designFieldbook_sites,side="both")
      out = paste0(crop_id, program_id, phase_id, module_id, ayear, amonth, "_", sites)
      paste(out, collapse = ", ")
    }
  })

  output$fbDesign_id <- shiny::renderText({
    fbdesign_id()
  })

  output$designFieldbook_traits <- shinyTree::renderTree({

#     potato<- readRDS("C:/OMAR-2015/GitHubProjects/shinyTree/trial_module_list.rda")
#     potato
    tbl <- fbmodule::get_module_table(input$designFieldbook_crop)
    tbl <-as_data_frame(tbl)


    trait_check_box <- paste(tbl$variable,": ",tbl$variables_name,sep="")
    #agrego el nombre de los check boxes
    tbl$trait_check_box <- trait_check_box

     crop_table <- tbl

     trial_module <- crop_table %>% select(.,module_name) %>% unique(.)
     trial_module <- trial_module[[1]]

     trial_abbr<- crop_table %>% select(.,module) %>% unique(.)
     trial_abbr <- trial_abbr[[1]]

          trial_module_list <- list()
          trial_module_list_cb <- list()
          n <- length(trial_module)

          #for(i in trial_module){

          for(i in 1:n){

            #crop_table %>% filter(., module_name == "yield")
            #trait_var <- crop_table %>% filter(., module_name == i) %>% select(., variable)
            trait_var <- crop_table %>% filter(., module_name == trial_module[i]) %>% select(., variable)
            trait_var <- trait_var[[1]]
            trait_var <- as.list(trait_var)
            #extract trait check box names
            trait_names <- crop_table %>% filter(., module_name == trial_module[i]) %>% select(., trait_check_box)
            trait_names <- trait_names[[1]]

            names(trait_var) <- trait_names
            trait_list <- list(trait_var)
            names(trait_list) <- trial_module[i]

            trial_module_list[[i]] <- trait_list
          }

          trial_module_list
           a <- NULL
          a1 <- trial_module_list[[1]]
          for(i in 2:n){

            a <- c(a1,trial_module_list[[i]])
            a1 <- a
          }

       a

  })

#   fbsignCountry_sides <- reactive({
#     country_sides = fbsites::get_site_table()
#     if (nrow(locs) > 0 ) {
#       country_sides = locs$shortn
#       shiny::selectizeInput("designFieldbook_sites", label = "Field locations:",
#                             choices = cntry, selected = 1,  multiple = FALSE)
#     }
#
#   })

  output$fbDesign_countryTrial <- shiny::renderUI({
     sites_data <- fbsites::get_site_table()
     cntry <- fbsites::get_country_list(sites_data = sites_data)
     shiny::selectizeInput("fbDesign_countryTrial", label = "Field Country:",
                            choices = cntry, selected = 1,  multiple = FALSE)

  })

  fbdesign_sites <- reactive({
    sites_data = fbsites::get_site_table()
    fbsites::get_filter_locality(sites_data = sites_data, country_input= input$fbDesign_countryTrial)

  })

  output$fbDesign_countrySite <- shiny::renderUI({
    locs = fbsites::get_site_table()
    fbdesign_sites_selected <- fbdesign_sites()
    #print(locs)
    if (nrow(locs) > 0 ){
      chc = locs$shortn
      shiny::selectizeInput("designFieldbook_sites", label = "Field locations:",
                            choices = fbdesign_sites_selected, selected = 1,  multiple = FALSE)

   }
  })

  output$fbDesign_variables <- shiny::renderUI({
    crp <- input$designFieldbook_crop
    mdl <- fbmodule::list_modules(crp)
    #ids <- unlist(stringr::str_extract_all(mdl, "([A-Z]{2})"))
    ids <- str_trim(gsub("\\(.*","",mdl),side = "both")
    vls <- mdl
    mdl = as.list(ids)
    names(mdl) <- vls

    #print(mdl)
    shiny::selectInput("designFieldbook_module", label = "Assay (fieldbook module):",
                       choices = mdl, selected = 1)
  })


  fbdraft <- shiny::reactive({
    try({
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 0, {
                     incProgress(3/15)
                     crp <- input$designFieldbook_crop
                     # print(crp)
                     # print(fbglobal::fname_material_lists(crp))
                     # print(input$designFieldbook_trt1)
                     incProgress(3/15)
                     fn =file.path(fbglobal::fname_material_lists(crp), input$designFieldbook_trt1)
                     # print(fn)
                     load(fn)
                     trt1 = table_materials$institutional_number
                     incProgress(3/15)
                     mdl = input$designFieldbook_module
                     mdl = stringr::str_extract(mdl, "([A-Z]{2})")[[1]]
                     #vars = fbmodule::get_module_table(crp)
                     #vars = vars[vars$module == mdl, "variable"]
                     if(is.null(input$tree)) {print("pass")}
                     vars <- get_tree_value(input$designFieldbook_traits,crop_selected = crp)

                     # print(trt1)
                     # print(input$designFieldbook)
                     incProgress(3/15)

                     fb = design_fieldbook(design = input$designFieldbook,
                                           trt1 = trt1, trt1_label = "GENOTYPE",
                                           r = as.integer(input$designFieldbook_r),
                                           k = as.integer(input$designFieldbook_k),
                                           first = as.logical(input$designFieldbook_first),
                                           cont = as.logical(input$designFieldbook_cont),
                                           series = as.integer(input$designFieldbook_serie),
                                           zigzag = as.logical(input$designFieldbook_zigzag),
                                           variables = vars)
                     #print(fb)
                     fb[, 1] = as.integer(fb[, 1])

                    fb
                    } )
    })
  })


  shiny::observeEvent(input$fbDesign_draft, {
    fb = fbdraft()
    output$fbDesign_table <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(fb, readOnly = T)
    })
  })

  shiny::observeEvent(input$fbDesign_create, {
    #print("Heyoo")
    fb = fbdraft()
    try({
      fn = paste0(fbdesign_id(), ".rda")
      fp = file.path(fbglobal::fname_fieldbooks(input$designFieldbook_crop), fn)
      # print(fn)
      # print(fp)
      # print(str(fb))
      # print(head(fb))
      if(!file.exists(fp)) {
        saveRDS(fb, fp)
        values[["ph_fb_list"]] = NULL
        shinyBS::createAlert(session, "alert_fb_done", "fbdoneAlert", title = "Success",
                             content = "Fieldbook created.", append = FALSE)

        fn_xlsx <- paste(fbdesign_id(),".xlsx")
        openxlsx::write.xlsx(fb,fn_xlsx,overwrite=TRUE)
      }
    #
    })
  })

}
