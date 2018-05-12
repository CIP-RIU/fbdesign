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

  # makeReactiveBinding("pkg.globals")

  observeEvent(input$titleId, {
    js$collapse("box1")
  })

  output$uiTest <- renderUI({

  })


  featNames <- names(Agronomic_features$`Agronomic features`)

  #events for buttons next in tabs for fieldbook creation
  # TO BE OPTIMIZED
  observeEvent(input$btnNextPersonnelInfo, {
      updateTabsetPanel(session, "fbDesignNav", selected = "tabPersonnel")
  })
  observeEvent(input$btnNextSite, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabSite")
  })
  observeEvent(input$btnNextCropInfo, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabCropInfo")
  })
  observeEvent(input$btnDesign, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabDesign")
  })
  observeEvent(input$btnNextPlotInfo, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabPlotInfo")
  })
  observeEvent(input$btnNextAgro, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabAgroFeat")
  })
  observeEvent(input$btnNextTraits, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabTraits")
  })
  observeEvent(input$btnNextEnv, {
    updateTabsetPanel(session, "fbDesignNav", selected = "tabEnvironment")
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

  ###### experiment details ###############################################################\

  observe({
    removeUI(
      selector = "#fl_agencies_assoc_exp_aux", immediate = T
    )
    if(!is.null(input$designFieldbook_fundAgencyType)){
        l <- input$designFieldbook_fundAgencyType
        insertUI(
          selector = "#fl_agencies_assoc_exp",
          where ="afterBegin",
          ui = column(id = "fl_agencies_assoc_exp_aux", width = 12)
        )
        count <- 1
        for (ag in l){
          insertUI(
            selector = "#fl_agencies_assoc_exp_aux",
            where = "beforeEnd",
            ui = textInput(paste0("fundName_", count), paste0(ag, "  name"))
          )
          count <- count+1
        }
    }
  })

  projectEntities <- reactiveValues()
  projectEntities$num <- 0
  projectEntities$numLeads <- 0

  observe({
    if(is.numeric(input$numProjEntity)){
      n <- input$numProjEntity
      if(projectEntities$num == 0){

        insertUI(
          selector = "#fl_entities_exp",
          where ="afterBegin",
          ui = column(id = "fl_entites_exp_aux", width = 12)
        )
      }

        if(projectEntities$num < n){
          start <-  projectEntities$num + 1
          count <- start
          for (num in start:n) {
                insertUI(
                  selector = "#fl_entites_exp_aux",
                  where = "beforeEnd",
                  ui =fluidRow(id = paste0("fl_box_exp_ent_", count),
                        box(title = paste0("Project management entity #", count), solidHeader = TRUE, status = "warning", width=12,
                            column(width = 4,
                                selectizeInput(paste0("projEntity_", count), "Project management entity", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                                 c("CGIAR center",
                                                   "Other"
                                                 )
                                )
                        ),
                        conditionalPanel(paste0("input.projEntity_", count, " == 'CGIAR center'"),
                                         column(width = 4,
                                           selectizeInput(paste0("contCenter_", count), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                             "Africa Rice Center",
                                             "Bioversity International",
                                             "Center for International Forestry Research (CIFOR)",
                                             "International Center for Agricultural Research (ICARDA)",
                                             "International Center for Tropical Agriculture (CIAT)",
                                             "International Crops Research Institute for the Semi-Arid (ICRISAT)",
                                             "International Food Policy Research Institute (IFPRI)",
                                             "International Institute of Tropical Agriculture (IITA)",
                                             "International Livestock Research Institure (ILRI)",
                                             "International Maize and Wheat Improvement Center (CIMMYT)",
                                             "International Potato Center (CIP)",
                                             "International Rice Research Institute (IRRI)",
                                             "International Water Management Institute (IWMI)",
                                             "World Agroforestry Centre (ICRAF)",
                                             "WorldFish",
                                             "None")
                                           )
                                          ),
                                         column(width = 4,
                                           selectizeInput(paste0("contCRP_", count), "Contributor CRP", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                             "CGIAR Research Program on Fish",
                                             "CGIAR Research Program on Forests, Trees and Agroforestry",
                                             "CGIAR Research Program on Grain Legumes and Dryland Cereals",
                                             "CGIAR Research Program on Wheat",
                                             "CGIAR Research Program on Livestock",
                                             "CGIAR Research Program on Maize",
                                             "CGIAR Research Program on Rice",
                                             "CGIAR Research Program on Roots, Tubers and Bananas",
                                             "CGIAR Research Program on Agriculture for Nutrition and Health",
                                             "CGIAR Research Program on Climate Change, Agriculture and Food Security",
                                             "CGIAR Research Program on Policies, Institutions, and Markets",
                                             "CGIAR Research Program on Water, Land and Ecosystems",
                                             "None")
                                           )
                                         )

                        ),
                        conditionalPanel(paste0("input.projEntity_", count, " == 'Other'"),
                                         column(width = 12,
                                                fluidRow(
                                                    column(width =4,
                                                          textInput(paste0("contOtherCenter_", count), "", value = "")

                                                   )
                                                )
                                         )
                        )

                  ) #end box
                  )

                )
            count <- count + 1
          }
        }
        else if(projectEntities$num > n){
          start <- n+1
          end <- projectEntities$num
          count <- start
          for(num in start:end){
            removeUI(
              selector = paste0("#fl_box_exp_ent_", count),
              immediate = T
            )
            count <- count +1
          }

        }
        projectEntities$num <- n
    }
    else{
      removeUI(selector = "#fl_entites_exp_aux", immediate = T)
      projectEntities$num <- 0
    }
  })


  observe({
    if(is.numeric(input$numLeads)){
      n <- input$numLeads
      if(projectEntities$numLeads == 0){

        insertUI(
          selector = "#fl_exp_leads",
          where ="afterBegin",
          ui = column(id = "fl_exp_leads_aux", width = 12)
        )
      }

      if(projectEntities$numLeads < n){
        start <-  projectEntities$numLeads + 1
        count <- start
        for (num in start:n) {
          insertUI(
            selector = "#fl_exp_leads_aux",
            where = "beforeEnd",
            ui =fluidRow(id = paste0("fl_box_exp_lead_", count),
                         box(title = paste0("Experiment, lead organization #", count), solidHeader = TRUE, status = "warning", width=12,
                             column(width = 6,

                                    selectizeInput(paste0("projLeadEnt_", count), "Experiment, lead organization type", multiple =T, options = list(maxItems =1, placeholder="Select one..."), choices=
                                                     c("CGIAR center",
                                                       "Other"
                                                     )
                                    ),

                             conditionalPanel(paste0("input.projLeadEnt_", count, " == 'CGIAR center'"),

                                                     selectizeInput(paste0("tLeadCenter_", count), "Choose CGIAR center", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                       "Africa Rice Center",
                                                       "Bioversity International",
                                                       "Center for International Forestry Research (CIFOR)",
                                                       "International Center for Agricultural Research (ICARDA)",
                                                       "International Center for Tropical Agriculture (CIAT)",
                                                       "International Crops Research Institute for the Semi-Arid (ICRISAT)",
                                                       "International Food Policy Research Institute (IFPRI)",
                                                       "International Institute of Tropical Agriculture (IITA)",
                                                       "International Livestock Research Institure (ILRI)",
                                                       "International Maize and Wheat Improvement Center (CIMMYT)",
                                                       "International Potato Center (CIP)",
                                                       "International Rice Research Institute (IRRI)",
                                                       "International Water Management Institute (IWMI)",
                                                       "World Agroforestry Centre (ICRAF)",
                                                       "WorldFish",
                                                       "None")
                                                     )

                             ),
                             conditionalPanel(paste0("input.projLeadEnt_", count, " == 'Other'"),


                                              selectizeInput(paste0("lead_org_type_1_", count), "",multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),  choices = c("University","University, main campus","Agricultural experimental extension", "Government research institution (NARS)","Government research institution, designated laboratory or center", "Private company", "Farm", "Farmer association or cooperative", "Non-governmental organization", "Extension organization", "CGIAR center", "Other" )),
                                              conditionalPanel(paste0("input.lead_org_type_1_", count, " == 'Other'"),
                                                               textInput(paste0("lead_org_type_1_other_", count), "")),

                                              textInput(paste0("leadNameOther_", count), "Experiment, lead organization name", value = "")

                             ),


                              textInput(inputId = paste0("expLead_", count), label = "Experiment lead person / Primary Investigator", value = "")
                             )

                         ) #end box
            )

          )
          count <- count + 1
        }
      }
      else if(projectEntities$numLeads > n){
        start <- n+1
        end <- projectEntities$numLeads
        count <- start
        for(num in start:end){
          removeUI(
            selector = paste0("#fl_box_exp_lead_", count),
            immediate = T
          )
          count <- count +1
        }

      }
      projectEntities$numLeads <- n
    }
    else{
      removeUI(selector = "#fl_exp_leads_aux", immediate = T)
      projectEntities$numLeads <- 0
    }
  })

  ###### end experiment detials ###########################################################


  #### factors ####################################################################################

  path <- fbglobal::get_base_dir()
  fp <- file.path(path, "listFactors.rds") # field operations agro features as list of factors

  lvl <- reactiveValues()
  factors <- as.data.frame(readRDS(fp))
  lvl$lv_1_1 <- unique(factors$GROUP)
  lvl$lv_1_2 <- NULL
  lvl$lv_1_3 <- NULL

  lvl$lv_2_1 <- unique(factors$GROUP)
  lvl$lv_2_2 <- NULL
  lvl$lv_2_3 <- NULL

  lvl$lv_3_1 <- unique(factors$GROUP)
  lvl$lv_3_2 <- NULL
  lvl$lv_3_3 <- NULL

  lvl$lv_4_1 <- unique(factors$GROUP)
  lvl$lv_4_2 <- NULL
  lvl$lv_4_3 <- NULL

  lvl$lv_5_1 <- unique(factors$GROUP)
  lvl$lv_5_2 <- NULL
  lvl$lv_5_3 <- NULL

  observe({
    if(!is.null(input$sel1_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel1_1)
      lvl$lv_1_2 <- unique(aux$SUBGROUP)
    }
    else{
      lvl$lv_1_2 <- NULL
    }
    removeUI(selector = "#fluid_levels_1", immediate = T)
    lvl$lv_1_3 <- NULL
    removeUI( selector ="#fl_title_factor_1", immediate = T )

  })
  observe( {

    if(!is.null(input$sel1_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel1_1 & SUBGROUP==input$sel1_2)

      lvl$lv_1_3 <- unique(aux$FACTOR)
    }
    else{
      lvl$lv1_3 <- NULL
    }
    removeUI(selector = "#fluid_levels_1", immediate = T)
    removeUI( selector ="#fl_title_factor_1", immediate = T )
  })

  observe({
    removeUI( selector ="#fl_title_factor_1", immediate = T )
    if(!is.null(input$sel1_3) && is.numeric(input$numLevels_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel1_1 & SUBGROUP==input$sel1_2 & FACTOR==input$sel1_3)
      if(nrow(aux) > 0){
        insertUI(
          selector = "#fl_title_factor_aux_1",
          where = "beforeBegin",
          ui = fluidRow(id="fl_title_factor_1", column(width = 12, br(), h4(HTML(paste0("<b>", input$sel1_3, "</b>")))))

        )

        if(aux$FORM == "combo box"){
          drawComboboxLevel(1,input$numLevels_1, aux$LEVEL)
        }
        else if(aux$FORM == "text input"){
          drawTextInputLevel(1,input$numLevels_1, aux$UNIT)
        }
        else if(aux$FORM == "numeric input"){
          drawNumericInputLevel(1,input$numLevels_1)
        }

        else if(aux$FORM == "date"){
          drawDateLevel(1,input$numLevels_1)
        }
      }
      else{
        removeUI(selector = "#fluid_levels_1", immediate = T)
      }

    }
    else{
      removeUI(selector = "#fluid_levels_1", immediate = T)
    }
  })


  observe({
    if(!is.null(input$sel2_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel2_1)
      lvl$lv_2_2 <- unique(aux$SUBGROUP)
    }
    else{
      lvl$lv_2_2 <- NULL
    }
    removeUI(selector = "#fluid_levels_2", immediate = T)
    lvl$lv_2_3 <- NULL
    removeUI( selector ="#fl_title_factor_2", immediate = T )

  })

  observe( {
    if(!is.null(input$sel2_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel2_1 & SUBGROUP==input$sel2_2)
      lvl$lv_2_3 <- unique(aux$FACTOR)
    }
    else{
      lvl$lv_2_3 <- NULL
    }
    removeUI(selector = "#fluid_levels_2", immediate = T)
    removeUI( selector ="#fl_title_factor_2", immediate = T )
  })

  observe({
    removeUI( selector ="#fl_title_factor_2", immediate = T )
    if(!is.null(input$sel2_3) && is.numeric(input$numLevels_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel2_1 & SUBGROUP==input$sel2_2 & FACTOR==input$sel2_3)

      if(nrow(aux) > 0){
        insertUI(
          selector = "#fl_title_factor_aux_2",
          where = "beforeBegin",
          ui = fluidRow(id="fl_title_factor_2", column(width = 12, br(), h4(HTML(paste0("<b>", input$sel2_3, "</b>")))))

        )

        if(aux$FORM == "combo box"){
          drawComboboxLevel(2,input$numLevels_2, aux$LEVEL)
        }
        else if(aux$FORM == "text input"){
          drawTextInputLevel(2,input$numLevels_2, aux$UNIT)
        }
        else if(aux$FORM == "numeric input"){
          drawNumericInputLevel(2,input$numLevels_2)
        }

        else if(aux$FORM == "date"){
          drawDateLevel(2,input$numLevels_2)
        }
      }
      else{
        removeUI(selector = "#fluid_levels_2", immediate = T)
      }

    }
    else{
      removeUI(selector = "#fluid_levels_2", immediate = T)
    }
  })


  observe({
    if(!is.null(input$sel3_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel3_1)
      lvl$lv_3_2 <- unique(aux$SUBGROUP)
    }
    else{
      lvl$lv_3_2 <- NULL
    }
    removeUI(selector = "#fluid_levels_3", immediate = T)
    lvl$lv_3_3 <- NULL
    removeUI( selector ="#fl_title_factor_3", immediate = T )

  })

  observe( {
    if(!is.null(input$sel3_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel3_1 & SUBGROUP==input$sel3_2)
      lvl$lv_3_3 <- unique(aux$FACTOR)
    }
    else{
      lvl$lv_3_3 <- NULL
    }
    removeUI(selector = "#fluid_levels_3", immediate = T)
    removeUI( selector ="#fl_title_factor_3", immediate = T )
  })

  observe({
    removeUI( selector ="#fl_title_factor_3", immediate = T )
    if(!is.null(input$sel3_3) && is.numeric(input$numLevels_3)){
      aux <- dplyr::filter(factors,GROUP==input$sel3_1 & SUBGROUP==input$sel3_2 & FACTOR==input$sel3_3)

      if(nrow(aux) > 0){
        insertUI(
          selector = "#fl_title_factor_aux_3",
          where = "beforeBegin",
          ui = fluidRow(id="fl_title_factor_3", column(width = 12, br(), h4(HTML(paste0("<b>", input$sel3_3, "</b>")))))

        )
        if(aux$FORM == "combo box"){
          drawComboboxLevel(3,input$numLevels_3, aux$LEVEL)
        }
        else if(aux$FORM == "text input"){
          drawTextInputLevel(3,input$numLevels_3, aux$UNIT)
        }
        else if(aux$FORM == "numeric input"){
          drawNumericInputLevel(3,input$numLevels_3)
        }

        else if(aux$FORM == "date"){
          drawDateLevel(3,input$numLevels_3)
        }
      }
      else{
        removeUI(selector = "#fluid_levels_3", immediate = T)
      }
    }
    else{
      removeUI(selector = "#fluid_levels_3", immediate = T)
    }
  })


  observe({
    if(!is.null(input$sel4_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel4_1)
      lvl$lv_4_2 <- unique(aux$SUBGROUP)
    }
    else{
      lvl$lv_4_2 <- NULL
    }
    removeUI(selector = "#fluid_levels_4", immediate = T)
    lvl$lv_4_3 <- NULL
    removeUI( selector ="#fl_title_factor_4", immediate = T )

  })

  observe({
    if(!is.null(input$sel4_2)){
      aux <- dplyr::filter(factors,GROUP==input$sel4_1 & SUBGROUP==input$sel4_2)
      lvl$lv_4_3 <- unique(aux$FACTOR)
    }
    else{
      lvl$lv_4_3 <- NULL
    }
    removeUI(selector = "#fluid_levels_4", immediate = T)
    removeUI( selector ="#fl_title_factor_4", immediate = T )
  })

  observe({
    removeUI( selector ="#fl_title_factor_4", immediate = T )
    if(!is.null(input$sel4_3) && is.numeric(input$numLevels_4)){
      aux <- dplyr::filter(factors,GROUP==input$sel4_1 & SUBGROUP==input$sel4_2 & FACTOR==input$sel4_3)

      if(nrow(aux) > 0){
        insertUI(
          selector = "#fl_title_factor_aux_4",
          where = "beforeBegin",
          ui = fluidRow(id="fl_title_factor_4", column(width = 12, br(), h4(HTML(paste0("<b>", input$sel4_3, "</b>")))))

        )
        if(aux$FORM == "combo box"){
          drawComboboxLevel(4,input$numLevels_4, aux$LEVEL)
        }
        else if(aux$FORM == "text input"){
          drawTextInputLevel(4,input$numLevels_4, aux$UNIT)
        }
        else if(aux$FORM == "numeric input"){
          drawNumericInputLevel(4,input$numLevels_4)
        }

        else if(aux$FORM == "date"){
          drawDateLevel(4,input$numLevels_4)
        }
      }
      else{
        removeUI(selector = "#fluid_levels_4", immediate = T)
      }
    }
    else{
      removeUI(selector = "#fluid_levels_4", immediate = T)
    }
  })


  observe({
    if(!is.null(input$sel5_1)){
      aux <- dplyr::filter(factors,GROUP==input$sel5_1)
      lvl$lv_5_2 <- unique(aux$SUBGROUP)
    }
    else{
      lvl$lv_5_2 <- NULL
    }
    removeUI(selector = "#fluid_levels_5", immediate = T)
    lvl$lv_5_3 <- NULL
    removeUI( selector ="#fl_title_factor_5", immediate = T )

  })

  observe( {
    if(!is.null(input$sel5_2)){
      aux <- dplyr::filter(factors, GROUP==input$sel5_1 & SUBGROUP==input$sel5_2)
      lvl$lv_5_3 <- unique(aux$FACTOR)
    }
    else{
      lvl$lv_5_3 <- NULL
    }
    removeUI(selector = "#fluid_levels_5", immediate = T)
    removeUI( selector ="#fl_title_factor_5", immediate = T )
  })

  observe({
    removeUI( selector ="#fl_title_factor_5", immediate = T )
    if(!is.null(input$sel5_3) && is.numeric(input$numLevels_5)){
      aux <- dplyr::filter(factors,GROUP==input$sel5_1 & SUBGROUP==input$sel5_2 & FACTOR==input$sel5_3)

      if(nrow(aux) > 0){
        insertUI(
          selector = "#fl_title_factor_aux_5",
          where = "beforeBegin",
          ui = fluidRow(id="fl_title_factor_5", column(width = 12, br(), h4(HTML(paste0("<b>", input$sel5_3, "</b>")))))

        )
        if(aux$FORM == "combo box"){
          drawComboboxLevel(5,input$numLevels_5, aux$LEVEL)
        }
        else if(aux$FORM == "text input"){
          drawTextInputLevel(5,input$numLevels_5, aux$UNIT)
        }
        else if(aux$FORM == "numeric input"){
          drawNumericInputLevel(5,input$numLevels_5)
        }

        else if(aux$FORM == "date"){
          drawDateLevel(5,input$numLevels_5)
        }
      }
      else{
        removeUI(selector = "#fluid_levels_5", immediate = T)
      }
    }
    else{
      removeUI(selector = "#fluid_levels_5", immediate = T)
    }
  })

  drawComboboxLevel <- function(order, num, lev){
    opt <- strsplit(lev, ";")
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    insertUI(selector = paste0("#levelSelection_", order),
             where = "afterEnd",
             ui = fluidRow( id= paste0("fluid_levels_", order),
                            column(width = 12,
                            selectizeInput(paste0("levels_", order), HTML("Select levels"),
                                           multiple =T,
                                           options = list(maxItems = num, placeholder = "Select ..." ),
                                           choices = opt[[1]]
                            )
                            )
             )
    )
  }

  drawTextInputLevel <- function(order, num, units){
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    if(is.na(units)){
      insertUI(selector = paste0("#levelSelection_", order),
               where = "afterEnd",
               ui = fluidRow( id= paste0("fluid_levels_", order),
                              column(width = 12,
                              selectizeInput(paste0("levels_", order), HTML("Enter levels"),
                                             multiple =T, choices = c(),
                                             options = list(maxItems = num, placeholder = "Write..." ,
                                                            'create' = TRUE,
                                                            'persist' = FALSE)
                              )
                              )
               )
      )
    }
    else{
      vunits <- strsplit(units, ",")
      insertUI(selector = paste0("#levelSelection_", order),
               where = "afterEnd",
               ui = fluidRow( id= paste0("fluid_levels_", order),
                              column(width = 6,
                                     selectizeInput(paste0("levels_",order), HTML("Enter levels"),
                                                    multiple =T, choices = c(),
                                                    options = list(maxItems = num, placeholder = "Write..." ,
                                                                   'create' = TRUE,
                                                                   'persist' = FALSE)
                                     )
                              ),
                              column(width = 6,
                                     selectizeInput(paste0("funits_", order), HTML("Unit"),
                                                    multiple =T, choices = vunits[[1]] ,
                                                    options = list(maxItems = 1, placeholder = "Select unit...")
                                     )
                              )
               )
      )
    }
  }

  drawNumericInputLevel <- function(order, num){
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    insertUI(selector = paste0("#levelSelection_", order),
             where = "afterEnd",
             ui = fluidRow( id= paste0("fluid_levels_", order),
                            column(width = 12,
                              numericInput(paste0("levels_", order), HTML("Levels"), min=1, max = num, value = 1)
                            )
             )
    )
  }

  drawDateLevel <- function(order, num){
    removeUI(selector = paste0("#fluid_levels_", order), immediate = T)
    insertUI(selector = paste0("#levelSelection_", order),
             where = "afterEnd",
             ui = fluidRow( id= paste0("fluid_levels_", order),
                            column(width = 12,
                                fluidRow( id = paste0("factor_dates_", order , "_1"),
                                  column(width = 6,
                                         dateInput(paste0("factor_start_date_", order, "_1"), HTML("#1 Start date"),format = "dd/mm/yyyy")
                                         ),
                                  column(width = 6,
                                         dateInput(paste0("factor_end_date_", order, "_1"), HTML("#1 End date"),format = "dd/mm/yyyy")
                                  )
                                )
                            )
             )
    )
    if(num > 1){
      for (i in 2:num) {
        insertUI(selector = paste0("#factor_dates_", order,"_", i-1),
                 where = "afterEnd",
                 ui = fluidRow(id = paste0("factor_dates_", order , "_", i) ,
                   column(width = 6,
                          dateInput(paste0("factor_start_date_", order, "_", i), HTML(paste0("#",i, " Start date")),format = "dd/mm/yyyy")
                   ),
                   column(width = 6,
                          dateInput(paste0("factor_end_date_", order, "_", i), HTML(paste0("#", i, " End date")),format = "dd/mm/yyyy")
                   )
                 )
                 # ui =  dateRangeInput(paste0("dates_",order ,"_", i), paste0("#" ,i, " Select dates"), startview = "year",format = "dd/mm/yyyy")
        )
      }}
  }

  output$ui_sel1_1 <- renderUI({
    selectizeInput("sel1_1", "", choices = lvl$lv_1_1, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
  })
  output$ui_sel1_2 <- renderUI({
    selectizeInput("sel1_2", HTML(""), choices =lvl$lv_1_2, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })

  output$ui_sel1_3 <- renderUI({
    selectizeInput("sel1_3", HTML(""),choices =lvl$lv_1_3, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })
  output$ui_numIn_1 <- renderUI({
    numericInput("numLevels_1", HTML("#levels"), max = 5, min = 2, value = 2)
  })

  output$ui_sel2_1 <- renderUI({
    selectizeInput("sel2_1", "", choices = lvl$lv_2_1, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
  })
  output$ui_sel2_2 <- renderUI({
    selectizeInput("sel2_2", "", choices =lvl$lv_2_2, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })

  output$ui_sel2_3 <- renderUI({
    selectizeInput("sel2_3", "", choices =lvl$lv_2_3, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })
  output$ui_numIn_2 <- renderUI({
    numericInput("numLevels_2", "#levels", max = 5, min = 2, value = 2)
  })

  output$ui_sel3_1 <- renderUI({
    selectizeInput("sel3_1","", choices = lvl$lv_3_1, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
  })
  output$ui_sel3_2 <- renderUI({
    selectizeInput("sel3_2", "", choices =lvl$lv_3_2, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
  })

  output$ui_sel3_3 <- renderUI({
    selectizeInput("sel3_3", "",choices =lvl$lv_3_3, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
  })
  output$ui_numIn_3 <- renderUI({
    numericInput("numLevels_3", "#levels", max = 5, min = 2, value = 2)
  })

  output$ui_sel4_1 <- renderUI({
    selectizeInput("sel4_1", "", choices = lvl$lv_4_1, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
  })
  output$ui_sel4_2 <- renderUI({
    selectizeInput("sel4_2","", choices =lvl$lv_4_2, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })

  output$ui_sel4_3 <- renderUI({
    selectizeInput("sel4_3", "",choices =lvl$lv_4_3, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })
  output$ui_numIn_4 <- renderUI({
    numericInput("numLevels_4", "#levels", max = 5, min = 2, value = 2)
  })

  output$ui_sel5_1 <- renderUI({
    selectizeInput("sel5_1", "", choices = lvl$lv_5_1, multiple =T, options = list(maxItems =1, placeholder ="Select..."))
  })
  output$ui_sel5_2 <- renderUI({
    selectizeInput("sel5_2","", choices =lvl$lv_5_2, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })

  output$ui_sel5_3 <- renderUI({
    selectizeInput("sel5_3", "",choices =lvl$lv_5_3, multiple =T, options = list(maxItems =1, placeholder ="Select..."))

  })
  output$ui_numIn_5 <- renderUI({
    numericInput("numLevels_5", "#levels", max = 5, min = 2, value = 2)
  })

  #### end factors ####################################################################################



  output$uiPreviousCrop1 <- renderUI({
    selectizeInput("prevCropName", "Previous crop name", c(), multiple = TRUE, options = list(
      placeholder = "ex.  crop1  crop2", maxItems = input$numPreviousCrop,
      'create' = TRUE,
      'persist' = FALSE)
    )



  })
  output$uiPreviousCrop2 <- renderUI({
           selectizeInput("prevCropVar", "Previous crop variety", c(), multiple = TRUE, options = list(
             placeholder = "ex.  var1  var2", maxItems = input$numPreviousCrop,
             'create' = TRUE,
             'persist' = FALSE)
           )#

  })
#
#   observeEvent(input$numPreviousCrop, {
#
#   })


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



  nutTabs = list ("Land preparation" = "tabLandPr",
                  "Mulching and residue management" ="tabMulching",
                  "Planting, transplanting" ="tabPlanting",
                  "Harvest" = "tabHarvest" ,
                  "Irrigation" = "tabIrrigation",
                  "Biofertilizer" = "tabBiofertilizer",
                  "Pest & disease" = "tabPestNDisease" ,
                  "Nutrient management" = "tabNutrient")
  observe({
    hideTab("nutrienTabPanels", "tabLandPr")
    hideTab("nutrienTabPanels", "tabMulching")
    hideTab("nutrienTabPanels", "tabPlanting")
    hideTab("nutrienTabPanels", "tabHarvest")
    hideTab("nutrienTabPanels", "tabIrrigation")
    hideTab("nutrienTabPanels", "tabBiofertilizer")
    hideTab("nutrienTabPanels", "tabPestNDisease")
    hideTab("nutrienTabPanels", "tabNutrient")

    if(!is.null(input$selectAgroFeature)){
      l <- input$selectAgroFeature
      n <- length(input$selectAgroFeature)

      for (mtab in l) {
        showTab("nutrienTabPanels", nutTabs[[mtab]])

      }

    }
  })

  ###########  biofertilizer ##########################################

  bioferVar <- reactiveValues()
  bioferVar$nApps <-1


  observeEvent(input$numApplicationsBiofert, {
    num <- input$numApplicationsBiofert
      if(is.numeric(num) &&  num>0){
          if(bioferVar$nApps == 1 && num  == 1 ){

            insertUI(selector ="#bio_description",
                     where = "afterEnd",
                     ui = drawBoxBiofertilizer(1))
          }
          else if(bioferVar$nApps == 0 && num  == 1 ){

            insertUI(selector ="#bio_description",
                     where = "afterEnd",
                     ui = drawBoxBiofertilizer(1))
          }
          else if(bioferVar$nApps > num){
            removeBoxesBiofert(num+1, bioferVar$nApps)
            bioferVar$nApps <- num
          }
          else if(bioferVar$nApps < num){
            start <- bioferVar$nApps + 1
            for (i in start:num ) {
                insertUI(selector = paste0("#box_bio_", i-1),
                         where = "afterEnd",
                         ui = drawBoxBiofertilizer(i)
                )
            }
            bioferVar$nApps <- num
          }

    }
    else{
      removeBoxesBiofert(1, bioferVar$nApps)
      bioferVar$nApps <- 0

    }

  })


  removeBoxesBiofert <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_bio_", i),
        immediate = T
      )
    }

  }



  drawBoxBiofertilizer <- function(order){
  fluidRow(id= paste0("box_bio_", order),
    box( title = paste0("Application #", order),
        width = 12,
        solidHeader = TRUE, status = "warning",
        column(width = 6,


               fluidRow(
                 column(width = 6,
                        dateInput(paste0("biofertilizer_landLeveling_start_date_", order), label ="Start date", format = "dd/mm/yyyy")
                 ),
                 column(width = 6,
                        dateInput(paste0("biofertilizer_landLeveling_end_date",  order), label ="End date", format = "dd/mm/yyyy")
                 )
               ),
               selectizeInput(paste0("biofertilizer_rhizobium_inoculum_strain_", order), label = "Rhizobium inoculum strain", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                c("Rhizobium japonicum",
                                  "Rhizobium leguminosarum",
                                  "Rhizobium loti",
                                  "Rhizobium meliloti",
                                  "Rhizobium spp.",
                                  "Rhizobium trifolii",
                                  "Other")
               ),

               conditionalPanel(paste0("input.biofertilizer_rhizobium_inoculum_strain_", order,  " == 'Other'"),
                                textInput(paste0("rhizobium_name_", order),"",value="")),
               fluidRow(
                 column(width = 6,
                        textInput(paste0("biofertilizer_quantity_inoculated_", order), value = "", label="Biofertilizer quantity inoculated")
                 ),
                 column(width = 6, #IMPLEMENTAR EN EXCEl
                        selectizeInput(paste0("biofertilizer_quantity_inoculated_unit_", order), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("kg/m2", "kg/ha", "t/ha"))
                 )

               )


      ),
      column(width = 6,

        selectizeInput(paste0("biofertilizer_inoculation_method_", order), label = "Inoculation method", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                         c("Seed coating (Seed application of inoculum)",
                           "Directly to the soil",
                           "Other")
        ),
        conditionalPanel(paste0("input.biofertilizer_inoculation_method_", order, " == 'Other'"),
                         textInput(paste0("inoculation_method_name_", order),"",value="")),

        selectizeInput(paste0("biofertilizer_product_formulation_", order), label = "Product formulation", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                         c("Soil application with granules/pellets",
                           "Soil application with slurry of liquid culture")
        ),
        textInput(paste0("biofertilizer_days_sowing_after_rhizobium_inocculation_", order), value="", label = "Days to sowing after Rhizobium inoculation")
      )
    ))


  }

  ####################################################################





  ###########  Pest and Disease ##########################################

  pestVar <- reactiveValues()
  pestVar$nApps <-1


  observeEvent(input$numApplicationsPestDisease, {
    num <- input$numApplicationsPestDisease
    if(is.numeric(num) &&  num>0){
      if(pestVar$nApps == 1 && num  == 1 ){

        insertUI(selector ="#pestNDisease_fluid",
                 where = "afterEnd",
                 ui = drawBoxPest(1))
      }
      else if(pestVar$nApps == 0 && num  == 1 ){

        insertUI(selector ="#pestNDisease_fluid",
                 where = "afterEnd",
                 ui = drawBoxPest(1))
      }
      else if(pestVar$nApps > num){
        removeBoxesPest(num+1, pestVar$nApps)
        pestVar$nApps <- num
      }
      else if(pestVar$nApps < num){
        start <- pestVar$nApps + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_pest_", i-1),
                   where = "afterEnd",
                   ui = drawBoxPest(i)
          )
        }
        pestVar$nApps <- num
      }

    }
    else{
      removeBoxesPest(1, pestVar$nApps)
      pestVar$nApps <- 0

    }

  })


  removeBoxesPest <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_pest_", i),
        immediate = T
      )
    }

  }



  drawBoxPest <- function(order){
    fluidRow(id= paste0("box_pest_", order),
             box( title = paste0("Application #", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  column(width = 6,

                                  fluidRow(
                                    column(width = 6,
                                           dateInput(paste0("pestcontrol_start_date_",order), label ="Start date", format = "dd/mm/yyyy")
                                    ),
                                    column(width = 6,
                                           dateInput(paste0("pestcontrol_end_date_",order), label ="End date", format = "dd/mm/yyyy")
                                    )
                                  ),
                                  selectizeInput(paste0("pest_control_technique_",order), label = "Pest control technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                   c("Biological pest control",
                                                     "Chemical pest control",
                                                     "Mechanical pest control")
                                  ),
                                  textInput(paste0("pest_name_form_",order), "Pest name/formulation")

                                  # fileInput("myFile", "Pesticide box or bottle picture", accept = c('image/png', 'image/jpeg')),
                                  # textInput("pest_control_applications_totnumber", value="", label = "Pest control applications total number"),
                                  # textInput("pest_control_details", value="", label = "Pest control details (e.g. name of parasitoid etc), treatment evaluation"),
                                  # selectizeInput("chemical_pest_control_equipment", label = "Chemical pest control equipment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                  #               c("Aerial applicator",
                                  #                 "Airblast sprayer",
                                  #                 "Backpack sprayer",
                                  #                 "Boom sprayer",
                                  #                 "Duster",
                                  #                 "Electrostatic sprayer",
                                  #                 "Fogger",
                                  #                 "Hand sprayer",
                                  #                 "Injection sprayer",
                                  #                 "Mist blower",
                                  #                 "Recirculating sprayer",
                                  #                 "Seed treater",
                                  #                 "Tree injector",
                                  #                 "Wiper")
                                  # )
                           # column(width = 6,
                           #        br(),
                           #        fluidRow(
                           #        box(
                           #          title = "Pesticide Implement", solidHeader = TRUE, status = "warning", width=12,
                           #          textInput("pesticide_implement_make", value="", label = "Implement make"),
                           #          textInput("pesticide_implement_model", value="", label = "Implement model"),
                           #          selectizeInput("pesticide_animal_traction", label = "Animal Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                           #                        c("Buffalo",
                           #                          "Camel",
                           #                          "Donkey",
                           #                          "Elephant",
                           #                          "Horse",
                           #                          "Mule",
                           #                          "Ox / Bullock / Steer",
                           #                          "Other"
                           #                        )
                           #          ),
                           #          textInput("pesticide_humanPowered", value="", label = "Human powered"),
                           #          selectizeInput("pesticide_motorized_traction", label = "Motorized Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                           #                        c("2 wheel tractor",
                           #                          "4 wheel tractor",
                           #                          "Other"
                           #                        )
                           #          )
                           #        ))
                           # )


             ),
             column(width = 6,
                    textInput(paste0("pesticide_application_depth_",order), value="", label = "Pesticide application depth, if applied to soil"),
                    fluidRow(
                      column(width = 6,
                             textInput(paste0("pesticide_amount_",order), value = "", label="Pesticide amount")
                      ),
                      column(width = 6,#IMPLEMENTAR EN EXCEL
                             selectizeInput(paste0("pesticide_amount_unit_",order), label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("kg/m2", "kg/ha", "t/ha"))
                      )
                    ),
                    selectizeInput(paste0("pesticide_control_equip",order), "Pest control equipment", multiple =T, options=list(maxItems=1, placeholder ="Select one..."),
                                   choices = c("Aerial applicator",
                                               "Airblast sprayer",
                                               "Backpack sprayer",
                                               "Boom sprayer",
                                               "Duster",
                                               "Electrostatic sprayer",
                                               "Fogger",
                                               "Hand sprayer",
                                               "Injection sprayer",
                                               "Injection sprayer",
                                               "Mist blower",
                                               "Recirculating sprayer" )
                              )
                  )
             ))


  }

  ####################################################################






  ###########  irrigation ##########################################

  irrigVar <- reactiveValues()
  irrigVar$nApps <-1


  observeEvent(input$numApplicationsIrrigation, {
    num <- input$numApplicationsIrrigation
    if(is.numeric(num) &&  num>0){
      if(irrigVar$nApps == 1 && num  == 1 ){

        insertUI(selector ="#irrig_description",
                 where = "afterEnd",
                 ui = drawBoxIrrigation(1))
      }
      else if(irrigVar$nApps == 0 && num  == 1 ){

        insertUI(selector ="#irrig_description",
                 where = "afterEnd",
                 ui = drawBoxIrrigation(1))
      }
      else if(irrigVar$nApps > num){
        removeBoxesIrrigation(num+1, irrigVar$nApps)
        irrigVar$nApps <- num
      }
      else if(irrigVar$nApps < num){
        start <- irrigVar$nApps + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_irrig_", i-1),
                   where = "afterEnd",
                   ui = drawBoxIrrigation(i)
          )
        }
        irrigVar$nApps <- num
      }

    }
    else{
      removeBoxesIrrigation(1, irrigVar$nApps)
      irrigVar$nApps <- 0
    }

  })


  removeBoxesIrrigation <- function(begin, end){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_irrig_", i),
        immediate = T
      )
    }

  }



  drawBoxIrrigation <- function(order){
    fluidRow(id= paste0("box_irrig_", order),
             box( title = paste0("Application #", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  column(width = 6,


                         fluidRow(
                           column(width = 6,
                                  dateInput(paste0("irrigationevent_start_date_", order), label ="Start date", format = "dd/mm/yyyy")
                           ),
                           column(width = 6,
                                  dateInput(paste0("irrigationevent_end_date_", order), label ="End date", format = "dd/mm/yyyy")
                           )
                         ),
                         selectizeInput(paste0("irrigation_technique_", order), label = "Irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                          c("Surface",
                                            "Localized",
                                            "Irrigation sprinker",
                                            "Sub-irrigation")
                         ),
                         conditionalPanel(paste0("input.irrigation_technique_", order, "== 'Surface'"),
                                          selectizeInput(paste0("surface_irrigation_technique_", order), label = "Surface irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                           c("Furrow irrigation",
                                                             "Uncontrolled flooding",
                                                             "Basin irrigation",
                                                             "Border irrigation",
                                                             "Continuous flood")
                                          )
                         ),
                         conditionalPanel(paste0("input.irrigation_technique_", order, "== 'Localized'"),

                                          selectizeInput(paste0("localized_irrigation_technique", order), label = "Localized irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                           c("Drip irrigation",
                                                             "Subsurface textile irrigation",
                                                             "Mist irrigation",
                                                             "Subsurface drip irrigation",
                                                             "Bubbler irrigation",
                                                             "Pitcher irrigation")
                                          )
                         ),
                         conditionalPanel(paste0("input.irrigation_technique_", order, "== 'Irrigation sprinker'"),

                                          selectizeInput(paste0("irrigation_using_sprinkler_systems_", order), label = "Irrigation using sprinkler systems", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                           c("Center pivot irrigation",
                                                             "Irrigation by lateral move",
                                                             "Irrigation by side move")
                                          )
                         ),


                         #Sacar myFile upload
                         # fileInput(paste0("myFile", "Irrigation system picture_", order), accept = c('image/png', 'image/jpeg')),
                         # textInput(paste0("irrigation_water_source_", order), value="", label = "Water source"),
                         selectizeInput(paste0("irrigation_water_source_", order), label = "Water source", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                          c("River",
                                            "Lake",
                                            "Reservoir",
                                            "Spring",
                                            "Drainage",
                                            "Groundwater",
                                            "Other")
                         ),
                         conditionalPanel(paste0("input.irrigation_water_source_", order ," == 'Other'"),

                                                 textInput(paste0("irrigation_water_source_", order,  "_other"), "")

                                          ),
                         fluidRow(
                            column(width = 6,
                              textInput(paste0("irrigation_water_source_distance_", order), value="", label = "Water source distance")
                            ),
                            column(width = 6,
                                   selectizeInput(paste0("irrigation_water_source_distance_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                  choices = c("m", "km")
                                   )
                            )
                         ),
                         fluidRow(
                           column(width = 6,
                                  textInput(paste0("irrigation_bund_height_", order), value="", label = "Bund height")
                           ),
                           column(width = 6,
                                  selectizeInput(paste0("irrigation_bund_height_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("cm", "in", "m")
                                  )
                           )
                         )

                  ),
                  column(width = 6,

                         fluidRow(
                             column(width = 6,
                                textInput(paste0("irrigation_percolation_rate_", order), value="", label = "Percolation rate")
                             ),
                            column(width = 6,
                                   selectizeInput(paste0("irrigation_percolation_rate_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                  choices = c("mm per day")
                                   )
                            )
                          ),
                         fluidRow(
                           column(width = 6,
                                textInput(paste0("irrigation_equipment_depth_", order), value="", label = "Irrigation equipment depth")
                           ),
                           column(width = 6,
                                  selectizeInput(paste0("irrigation_equipment_depth_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("cm", "in", "m")
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                              textInput(paste0("irrigation_well_depth_", order), value="", label = "Well depth")
                           ),
                           column(width = 6,
                                  selectizeInput(paste0("irrigation_well_depth_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("m")
                                  )
                           )
                         ),
                         fluidRow(
                           column(width = 6,
                                  textInput(paste0("irrigation_amount_", order), value="", label = "Irrigation amount")
                           ),
                           column(width = 6,
                                  selectizeInput(paste0("irrigation_amount_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("mm", "cm", "m", "in", "ft", "ml", "L", "gal", "cu m", "cu in", "cu ft")
                                  )
                           )
                         ),

                         fluidRow(
                           column(width = 6,
                                  textInput(paste0("irrigation_area_covered_irrigation_system_", order), value="", label = "Area covered by the irrigation system")
                           ),
                            column(width = 6,
                                  selectizeInput(paste0("irrigation_area_covered_irrigation_system_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("m2", "ha")
                                  )
                            )
                         )

                  )
             ))


  }

  ####################################################################


  ###########  nutrients ######################################################

  nutVar <- reactiveValues()
  nutVar$types <- list()

  observe( {
    if(is.null(input$appfTypeFertilizer) ){
      removeUI(
        selector = paste0("#Organic_fertilizer_box"),
        immediate = T
      )
      removeUI(
        selector = paste0("#Inorganic_fertilizer_box"),
        immediate = T
      )
      removeUI(
        selector = paste0("#Green_manure_fertilizer_box"),
        immediate = T
      )
      nutVar$types <- list()
    }
    else{
      l <- input$appfTypeFertilizer
        for (typ in l) {
          vtype = gsub(" ", "_", typ)
          if (!(vtype %in% nutVar$types)){
            insertUI(selector ="#fert123",
                     where = "beforeBegin",
                     ui = drawTypeFertBox(vtype))
          }
        }
      for (xvar in nutVar$types){
        vtype = gsub("_", " ", xvar)
        if (!(vtype %in% l)){
          removeUI(
            selector = paste0("#", gsub(" ","_", vtype), "_fertilizer_box"),
            immediate = T
          )
        }
      }
      nutVar$types <- gsub(" ", "_",l)

    }

  })

  drawTypeFertBox <- function(type){
    fluidRow(id = paste0(type, "_fertilizer_box"),
          box(title = gsub("_", " ", type),
                      width = 12,
                      solidHeader = TRUE, status = "primary", collapsible = T, collapsed = T,
              fluidRow(id = paste0(type, "_fertilizer_box_in"),
                column(width = 6,
                       numericInput(paste0("numApplications_", type), label  = "Number of applications", value = 1, min = 1, max = 5)

                )
            )
      )
    )

  }

  nutVar$nAppsOrg <- 1
  observeEvent(input$numApplications_Organic, {
    num <- input$numApplications_Organic
    if(is.numeric(num) &&  num>0){
      if(nutVar$nAppsOrg == 1 && num  == 1 ){

        insertUI(selector ="#Organic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Organic"))
      }
      else if(nutVar$nAppsOrg == 0 && num  == 1 ){

        insertUI(selector ="#Organic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Organic"))
      }
      else if(nutVar$nAppsOrg > num){
        removeBoxesNutrients(num+1, nutVar$nAppsOrg, "Organic")
        nutVar$nAppsOrg <- num
      }
      else if(nutVar$nAppsOrg < num){
        start <- nutVar$nAppsOrg + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_nut_Organic_", i-1),
                   where = "afterEnd",
                   ui = drawBoxNutrients(i, "Organic")
          )
        }
        nutVar$nAppsOrg <- num
      }

    }
    else{
      removeBoxesNutrients(1, nutVar$nAppsOrg, "Organic")
      nutVar$nAppsOrg <- 0
    }

  })


  nutVar$nAppsInorg <- 1
  observeEvent(input$numApplications_Inorganic, {
    num <- input$numApplications_Inorganic
    if(is.numeric(num) &&  num>0){
      if(nutVar$nAppsInorg == 1 && num  == 1 ){

        insertUI(selector ="#Inorganic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Inorganic"))
      }
      else if(nutVar$nAppsInorg == 0 && num  == 1 ){

        insertUI(selector ="#Inorganic_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Inorganic"))
      }
      else if(nutVar$nAppsInorg > num){
        removeBoxesNutrients(num+1, nutVar$nAppsInorg, "Inorganic")
        nutVar$nAppsInorg <- num
      }
      else if(nutVar$nAppsInorg < num){
        start <- nutVar$nAppsInorg + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_nut_Inorganic_", i-1),
                   where = "afterEnd",
                   ui = drawBoxNutrients(i, "Inorganic")
          )
        }
        nutVar$nAppsInorg <- num
      }

    }
    else{
      removeBoxesNutrients(1, nutVar$nAppsInorg, "Inorganic")
      nutVar$nAppsInorg <- 0
    }

  })

  nutVar$nAppsGreenManure <- 1
  observeEvent(input$numApplications_Green_manure, {
    num <- input$numApplications_Green_manure
    if(is.numeric(num) &&  num>0){
      if(nutVar$nAppsGreenManure == 1 && num  == 1 ){

        insertUI(selector ="#Green_manure_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Green_manure"))
      }
      else if(nutVar$nAppsGreenManure == 0 && num  == 1 ){

        insertUI(selector ="#Green_manure_fertilizer_box_in",
                 where = "afterEnd",
                 ui = drawBoxNutrients(1, "Green_manure"))
      }
      else if(nutVar$nAppsGreenManure > num){
        removeBoxesNutrients(num+1, nutVar$nAppsGreenManure, "Green_manure")
        irrigVar$nAppsGreenManure <- num
      }
      else if(nutVar$nAppsGreenManure < num){
        start <- nutVar$nAppsGreenManure + 1
        for (i in start:num ) {
          insertUI(selector = paste0("#box_nut_Green_manure_", i-1),
                   where = "afterEnd",
                   ui = drawBoxNutrients(i, "Green_manure")
          )
        }
        nutVar$nAppsGreenManure <- num
      }

    }
    else{
      removeBoxesNutrients(1, nutVar$nAppsGreenManure, "Green_manure")
      nutVar$nAppsGreenManure <- 0
    }

  })


  removeBoxesNutrients <- function(begin, end, type){
    for(i in begin:end){
      removeUI(
        selector = paste0("#box_nut_", type, "_", i),
        immediate = T
      )
    }

  }



  drawBoxNutrients <- function(order, type){
    fluidRow(id= paste0("box_nut_", type, "_", order),
             box( title = paste0("Application #", order),
                  width = 12,
                  solidHeader = TRUE, status = "warning",
                  fluidRow(
                    column(width = 6,
                          fluidRow(
                            column(width = 6,
                                   dateInput(paste0("nutrient_start_date_", type, "_", order), label ="Start date", format = "dd/mm/yyyy")
                            ),
                            column(width = 6,
                                   dateInput(paste0("nutrient_end_date_", type, "_", order), label ="End date", format = "dd/mm/yyyy")
                            )
                          ),
                          fluidRow(
                            column(width = 6,
                                   textInput(paste0("nutrient_app_rate_", type, "_", order),  label = "Total application rate for the season", value="")
                            ),
                            column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                   selectizeInput(paste0("nutrient_app_rate_unit", type, "_", order), label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                    c("g/sq m",
                                                      "kg/ha",
                                                      "lb/ac")
                                   )
                            )
                          )
                          #textInput(inputId= paste0("nutrient_app_rate_", type, "_", order), label="Total application rate for the season")

                    ),
                    column(width = 6,
                           fluidRow(
                              column(width = 6,
                                     textInput(inputId=paste0("nutrient_recommended_rate_", type, "_", order), label="Total recommended rate")
                              ),

                              column(width = 6,
                                     selectizeInput(paste0("nutrient_recommended_rate_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "Unit",
                                                    c("g/sq m",
                                                      "kg/ha",
                                                      "lb/ac")
                                      )
                              )
                            ),
                           textInput(paste0("perc_recomm_rate_", type, "_", order), "Percentage of recommended rate applied")
                    )
                  ),
                  h2(paste0("Fertilizer amount applied: ",gsub("_", " ", type))),

                  fluidRow(

                    column(width = 2,
                           fluidRow(
                             column(width = 5, br(),
                                           div(style="text-align:right", h4("Name"))),
                             column(width = 5,
                                    br(),
                                    div(style="text-align:center", h4("# of app"))
                              ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("#")) )
                           )
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Start date"))
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("End date"))
                    ),

                    column(width = 2,

                           column( width= 7,
                                   br(),
                            div(style="text-align:center", h4("Type"))
                           ),
                           column(width = 5,
                                  br(),
                                  div(style="text-align:center", h4("Type (Unit)"))
                            )
                    ),
                    column(width=3,
                           fluidRow(
                             column(width = 6,
                                    br(),
                                    div(style="text-align:center", h4("Technique"))
                             ),
                             column(width = 6,
                                    br(),
                                    div(style="text-align:center", h4("Implement"))
                             )
                           )
                    ),

                     column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Rate"))
                     ),
                     column(width = 1,
                            br(),
                            div(style="text-align:center", h4("Rate (unit)"))
                     ),
                     column(width = 1,
                            br(),
                            div(style="text-align:center", h4("Nutrient Content"))
                     )

                  ),
                  fluidRow(
                    column(width = 2,
                           fluidRow(
                             column(width = 5,
                                    br(),
                                           div(style="text-align:right", "Nitrogen")
                              ),
                             column(width = 5,
                                    selectInput(paste0("nutrientApplied_nit_numApps1_", type, "_", order),"",c(1,2,3))

                              ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("1"))
                              )
                           )
                      ),
                    column(width = 1,
                           dateInput(paste0("fert_nit_start_date1_", type, "_", order), label ="", format = "dd/mm/yyyy")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_nit_end_date1_", type, "_", order), label ="", format = "dd/mm/yyyy")
                    ),


                    column(width = 2,
                           if(type == "Green_manure"){
                             textInput(paste0("fert_nit_type1_", type, "_", order), "")

                           }
                           else if(type == "Inorganic"){
                             fluidRow(
                             column(width = 7,
                               selectizeInput(paste0("fert_nit_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                 'Ammonium nitrate',
                                 'Ammonium nitrate sulfate',
                                 'Ammonium polyphosphate',
                                 'Ammonium sulfate',
                                 'Anhydrous ammonia',
                                 'Aqua ammonia',
                                 'Calcitic limestone',
                                 'Calcium ammonium nitrate solution',
                                 'Calcium hydroxide',
                                 'Calcium nitrate',
                                 'Diammonium phosphate',
                                 'Dolomitic limestone',
                                 'Liquid phosphoric acid',
                                 'Monoammonium phosphate',
                                 'Potassium chloride',
                                 'Potassium nitrate',
                                 'Potassium sulfate',
                                 'Rock phosphate',
                                 'Single super phosphate',
                                 'Triple super phosphate',
                                 'Urea',
                                 'Urea ammonium nitrate solution',
                                 'Urea super granules',
                                 'NPK fertilizers',
                                 'Other')
                               )
                             ),
                               column(width = 5,
                                      selectizeInput(paste0("fert_nit_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                     c("g/sq m","kg/ha","lb/ac"))

                               ),
                               conditionalPanel(paste0("input.fert_nit_type1_", type, "_", order, " == 'Other'"),
                                                column(width=12,
                                                textInput(paste0("fert_nit_type1_other_", type, "_", order), "")
                                                )
                               )

                             )

                           }
                           else{
                             fluidRow(
                               column(width = 7,
                                 selectizeInput(paste0("fert_nit_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                   "Alfalfa Meal",
                                   "Bagasse",
                                   "Biochar",
                                   "Chicken litter (Poultry manure)",
                                   "Compost",
                                   "Farmyard manure",
                                   "Guano",
                                   "Liquid manure",
                                   "Oil cake",
                                   "Treated sewage sludge",
                                   "Vermicompost",
                                   "Fish fertilizer",
                                   "Other"
                                   )
                                 )
                               ),
                               column(width = 5,
                                      selectizeInput(paste0("fert_nit_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                     c("g/sq m","kg/ha","lb/ac"))
                               ),
                               conditionalPanel(paste0("input.fert_nit_type1_", type, "_", order, " == 'Other'"),
                                                column(width=12,
                                                textInput(paste0("fert_nit_type1_other_", type, "_", order), "")
                                                )
                               )
                             )
                            }
                    ),
                    column(width = 3,
                      fluidRow(
                        column(width = 6,
                             selectizeInput(paste0("fertilizer_nit_application_technique1_", type, "_", order), "", multiple = T,
                                            options = list(maxItems = 1, placeholder ="Select one"),
                                            choices = c(
                                                        "Band application on surface",
                                                        "Band application incorporated (Band application beneath surface)",
                                                        "Broadcast surface",
                                                        "Broadcast incorporated",
                                                        "Contact placement (seed placement)",
                                                        "Deep placement",
                                                        "Fertigation",
                                                        "Foliar application",
                                                        "Injection",
                                                        "Placed with seed (seed placement)",
                                                        "Side dressing",
                                                        "Sub-soil placement (injection)",
                                                        "Localized application (using mechanical or hand device)",
                                                        "Other")
                             ),
                             conditionalPanel(paste0("input.fertilizer_nit_application_technique1_", type, "_", order, " == 'Other'"),
                                              textInput(paste0("fertilizer_nit_application_technique1_other_", type, "_", order), "")
                             )
                        ),
                        column(width = 6,
                             selectizeInput(paste0("fertilizer_nit_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                            choices = c("Backpack sprayer (airblast sprayer)",
                                                        "Boom sprayer",
                                                        "Broadcast spreader",
                                                        "Hand sprayer",
                                                        "Manure spreader",
                                                        "Slurry injector",
                                                        "Manual application",
                                                        "Other")
                             ),
                             conditionalPanel(paste0("input.fertilizer_nit_implement1_", type, "_", order, " == 'Other'"),
                                              textInput(paste0("fertilizer_nit_implement1_other_", type, "_", order), "")
                             )
                        )
                      )
                    ),
                   column(width = 1,
                          textInput(paste0("fert_nit_amountApplied1_", type, "_", order),"")
                   ),
                   column(width = 1,
                          selectizeInput(paste0("fert_nit_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                         c("g/sq m","kg/ha","lb/ac"))
                   ),
                   column(width = 1,
                          textInput(paste0("fert_nit_nutrientContent1_", type, "_", order),"")
                   )

                  ),
                  conditionalPanel(paste0("input.nutrientApplied_nit_numApps1_", type, "_", order,  " == 2 |
                                   input.nutrientApplied_nit_numApps1_", type, "_", order, " == 3 "),
                                          fluidRow(
                                            column(width = 2,
                                                   fluidRow(
                                                     column(width = 5,
                                                            br(),
                                                            div(style="text-align:right", "")
                                                     ),
                                                     column(width = 5,
                                                            br(),
                                                            div(style="text-align:right", "")

                                                     ),
                                                     column(width = 2,
                                                            br(),
                                                            div(style="text-align:center", h4("2"))
                                                     )
                                                   )
                                            ),
                                            column(width = 1,
                                                   dateInput(paste0("fert_nit_start_date2_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                            ),

                                            column(width = 1,
                                                   dateInput(paste0("fert_nit_end_date2_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                            ),


                                            column(width = 2,
                                                   if(type == "Green_manure"){
                                                     textInput(paste0("fert_nit_type2_", type, "_", order), "")
                                                   }
                                                   else if(type == "Inorganic"){
                                                     fluidRow(
                                                       column(width = 7,

                                                       selectizeInput(paste0("fert_nit_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                         'Ammonium nitrate',
                                                         'Ammonium nitrate sulfate',
                                                         'Ammonium polyphosphate',
                                                         'Ammonium sulfate',
                                                         'Anhydrous ammonia',
                                                         'Aqua ammonia',
                                                         'Calcitic limestone',
                                                         'Calcium ammonium nitrate solution',
                                                         'Calcium hydroxide',
                                                         'Calcium nitrate',
                                                         'Diammonium phosphate',
                                                         'Dolomitic limestone',
                                                         'Liquid phosphoric acid',
                                                         'Monoammonium phosphate',
                                                         'Potassium chloride',
                                                         'Potassium nitrate',
                                                         'Potassium sulfate',
                                                         'Rock phosphate',
                                                         'Single super phosphate',
                                                         'Triple super phosphate',
                                                         'Urea',
                                                         'Urea ammonium nitrate solution',
                                                         'Urea super granules',
                                                         'NPK fertilizers',
                                                         "Other")
                                                       )
                                                        ),
                                                        column(width = 5,
                                                               selectizeInput(paste0("fert_nit_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                              c("g/sq m","kg/ha","lb/ac"))

                                                        ),
                                                       conditionalPanel(paste0("input.fert_nit_type2_", type, "_", order, " == 'Other'"),
                                                                        column(width=12,
                                                                        textInput(paste0("fert_nit_type2_other_", type, "_", order), "")
                                                                        )
                                                       )

                                                      )
                                                   }
                                                   else{
                                                     fluidRow(
                                                       column(width = 7,

                                                         selectizeInput(paste0("fert_nit_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                           "Alfalfa Meal",
                                                           "Bagasse",
                                                           "Biochar",
                                                           "Chicken litter (Poultry manure)",
                                                           "Compost",
                                                           "Farmyard manure",
                                                           "Guano",
                                                           "Liquid manure",
                                                           "Oil cake",
                                                           "Treated sewage sludge",
                                                           "Vermicompost",
                                                           "Fish fertilizer",
                                                           "Other")
                                                         )
                                                       ),
                                                       column(width = 5,
                                                              selectizeInput(paste0("fert_nit_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                             c("g/sq m","kg/ha","lb/ac"))

                                                       ),
                                                       conditionalPanel(paste0("input.fert_nit_type2_", type, "_", order, " == 'Other'"),
                                                                        column(width=12,
                                                                        textInput(paste0("fert_nit_type2_other_", type, "_", order), "")
                                                                        )
                                                       )

                                                     )



                                                     }
                                            ),
                                            column(width =3,
                                              fluidRow(
                                                column(width = 6,
                                                       selectizeInput(paste0("fertilizer_nit_application_technique2_", type, "_", order), "", multiple = T,
                                                                      options = list(maxItems = 1, placeholder ="Select one"),
                                                                      choices = c(
                                                                        "Band application on surface",
                                                                        "Band application incorporated (Band application beneath surface)",
                                                                        "Broadcast surface",
                                                                        "Broadcast incorporated",
                                                                        "Contact placement (seed placement)",
                                                                        "Deep placement",
                                                                        "Fertigation",
                                                                        "Foliar application",
                                                                        "Injection",
                                                                        "Placed with seed (seed placement)",
                                                                        "Side dressing",
                                                                        "Sub-soil placement (injection)",
                                                                        "Localized application (using mechanical or hand device)",
                                                                        "Other")
                                                       ),
                                                       conditionalPanel(paste0("input.fertilizer_nit_application_technique2_", type, "_", order, " == 'Other'"),
                                                                        textInput(paste0("fertilizer_nit_application_technique2_other_", type, "_", order), "")
                                                       )
                                                ),
                                                column(width = 6,
                                                       selectizeInput(paste0("fertilizer_nit_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                                       choices = c("Backpack sprayer (airblast sprayer)",
                                                                                            "Boom sprayer",
                                                                                            "Broadcast spreader",
                                                                                            "Hand sprayer",
                                                                                            "Manure spreader",
                                                                                            "Slurry injector",
                                                                                            "Manual application",
                                                                                            "Other")
                                                       ),
                                                       conditionalPanel(paste0("input.fertilizer_nit_implement2_", type, "_", order, " == 'Other'"),
                                                                        textInput(paste0("fertilizer_nit_implement2_other_", type, "_", order), "")
                                                       )
                                                )
                                              )
                                            ),
                                           column(width = 1,
                                                  textInput(paste0("fert_nit_amountApplied2_", type, "_", order),"")
                                           ),
                                           column(width = 1,
                                                  selectizeInput(paste0("fert_nit_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                 c("kg/m2","kg/ha","t/ha"))
                                           ),
                                           column(width = 1,
                                                  textInput(paste0("fert_nit_nutrientContent2_", type, "_", order),"")
                                           )
                                          )

                  ),


                  conditionalPanel(paste0("input.nutrientApplied_nit_numApps1_" , type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("3"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_nit_start_date3_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_nit_end_date3_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_nit_type3_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_nit_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_nit_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))

                                              ),
                                              conditionalPanel(paste0("input.fert_nit_type3_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                               textInput(paste0("fert_nit_type3_other_", type, "_", order), "")
                                                               )
                                              )

                                              )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                  selectizeInput(paste0("fert_nit_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                    "Alfalfa Meal",
                                                    "Bagasse",
                                                    "Biochar",
                                                    "Chicken litter (Poultry manure)",
                                                    "Compost",
                                                    "Farmyard manure",
                                                    "Guano",
                                                    "Liquid manure",
                                                    "Oil cake",
                                                    "Treated sewage sludge",
                                                    "Vermicompost",
                                                    "Fish fertilizer",
                                                    "Other")
                                                  )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_nit_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))
                                                ),
                                                conditionalPanel(paste0("input.fert_nit_type3_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                 textInput(paste0("fert_nit_type3_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )


                                              }
                                     ),
                                     column(width=3,
                                        fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_nit_application_technique3_", type, "_", order),label = "", multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                                 choices = c(
                                                                   "Band application on surface",
                                                                   "Band application incorporated (Band application beneath surface)",
                                                                   "Broadcast surface",
                                                                   "Broadcast incorporated",
                                                                   "Contact placement (seed placement)",
                                                                   "Deep placement",
                                                                   "Fertigation",
                                                                   "Foliar application",
                                                                   "Injection",
                                                                   "Placed with seed (seed placement)",
                                                                   "Side dressing",
                                                                   "Sub-soil placement (injection)",
                                                                   "Localized application (using mechanical or hand device)",
                                                                   "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_nit_application_technique3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_nit_application_technique3_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_nit_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_nit_implement3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_nit_implement3_other_", type, "_", order), "")
                                                )
                                         )
                                        )
                                     ),

                                    column(width = 1,
                                           textInput(paste0("fert_nit_amountApplied3_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_nit_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_nit_nutrientContent3_", type, "_", order),"")
                                    )
                                   )


                  ),#end conditional2

                  fluidRow(
                    column(width = 2,
                           fluidRow(
                             column(width = 5,
                                    br(),
                                    div(style="text-align:right", "Phosphorus")
                             ),
                             column(width = 5,
                                    selectInput(paste0("nutrientApplied_phos_numApps1_", type, "_", order),"",c(1,2,3))

                             ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("1"))
                             )
                           )
                    ),
                    column(width = 1,
                           dateInput(paste0("fert_phos_start_date1_", type, "_", order), label ="", format = "dd/mm/yyyy")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_phos_end_date1_", type, "_", order), label ="", format = "dd/mm/yyyy")
                    ),


                    column(width = 2,
                           if(type == "Green_manure"){
                             textInput(paste0("fert_phos_type1_", type, "_", order), "")
                           }
                           else if(type == "Inorganic"){
                             fluidRow(
                               column(width = 7,

                             selectizeInput(paste0("fert_phos_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                               'Ammonium nitrate',
                               'Ammonium nitrate sulfate',
                               'Ammonium polyphosphate',
                               'Ammonium sulfate',
                               'Anhydrous ammonia',
                               'Aqua ammonia',
                               'Calcitic limestone',
                               'Calcium ammonium nitrate solution',
                               'Calcium hydroxide',
                               'Calcium nitrate',
                               'Diammonium phosphate',
                               'Dolomitic limestone',
                               'Liquid phosphoric acid',
                               'Monoammonium phosphate',
                               'Potassium chloride',
                               'Potassium nitrate',
                               'Potassium sulfate',
                               'Rock phosphate',
                               'Single super phosphate',
                               'Triple super phosphate',
                               'Urea',
                               'Urea ammonium nitrate solution',
                               'Urea super granules',
                               'NPK fertilizers',
                               "Other")
                             )
                               ),
                             column(width = 5,
                                    selectizeInput(paste0("fert_phos_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                   c("g/sq m","kg/ha","lb/ac"))

                             ),
                             conditionalPanel(paste0("input.fert_phos_type1_", type, "_", order, " == 'Other'"),
                                              column(width=12,
                                              textInput(paste0("fert_phos_type1_other_", type, "_", order), "")
                                              )
                             )

                             )

                           }
                           else{
                             fluidRow(
                               column(width = 7,
                                 selectizeInput(paste0("fert_phos_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                   "Alfalfa Meal",
                                   "Bagasse",
                                   "Biochar",
                                   "Chicken litter (Poultry manure)",
                                   "Compost",
                                   "Farmyard manure",
                                   "Guano",
                                   "Liquid manure",
                                   "Oil cake",
                                   "Treated sewage sludge",
                                   "Vermicompost",
                                   "Fish fertilizer",
                                   "Other")
                                 )
                                ),
                                column(width = 5,
                                       selectizeInput(paste0("fert_phos_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                      c("g/sq m","kg/ha","lb/ac"))
                                ),
                               conditionalPanel(paste0("input.fert_phos_type1_", type, "_", order, " == 'Other'"),
                                                column(width=12,
                                                textInput(paste0("fert_phos_type1_other_", type, "_", order), "")
                                                )
                               )

                              )
                             }
                    ),
                    column(width=3 ,
                      fluidRow(
                        column(width = 6,
                               selectizeInput(paste0("fertilizer_phos_application_technique1_", type, "_", order), "", multiple = T,
                                              options = list(maxItems = 1, placeholder ="Select one"),
                                              choices = c(
                                                "Band application on surface",
                                                "Band application incorporated (Band application beneath surface)",
                                                "Broadcast surface",
                                                "Broadcast incorporated",
                                                "Contact placement (seed placement)",
                                                "Deep placement",
                                                "Fertigation",
                                                "Foliar application",
                                                "Injection",
                                                "Placed with seed (seed placement)",
                                                "Side dressing",
                                                "Sub-soil placement (injection)",
                                                "Localized application (using mechanical or hand device)",
                                                "Other")
                               ),
                               conditionalPanel(paste0("input.fertilizer_phos_application_technique1_", type, "_", order, " == 'Other'"),
                                                textInput(paste0("fertilizer_phos_application_technique1_other_", type, "_", order), "")
                               )
                        ),
                        column(width = 6,
                               selectizeInput(paste0("fertilizer_phos_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                              choices = c("Backpack sprayer (airblast sprayer)",
                                                          "Boom sprayer",
                                                          "Broadcast spreader",
                                                          "Hand sprayer",
                                                          "Manure spreader",
                                                          "Slurry injector",
                                                          "Manual application",
                                                          "Other")
                               ),
                               conditionalPanel(paste0("input.fertilizer_phos_implement1_", type, "_", order, " == 'Other'"),
                                                textInput(paste0("fertilizer_phos_implement1_other_", type, "_", order), "")
                               )
                        )
                      )
                    ),
                   column(width = 1,
                          textInput(paste0("fert_phos_amountApplied1_", type, "_", order),"")
                   ),
                   column(width = 1,
                          selectizeInput(paste0("fert_phos_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                         c("kg/m2","kg/ha","t/ha"))
                   ),
                   column(width = 1,
                          textInput(paste0("fert_phos_nutrientContent1_", type, "_", order),"")
                   )
                  ),

                  conditionalPanel(paste0("input.nutrientApplied_phos_numApps1_", type, "_", order,  " == 2 |
                                          input.nutrientApplied_phos_numApps1_", type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("2"))
                                              )
                                            )
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_start_date2_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_end_date2_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),
                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_phos_type2_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_phos_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_phos_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))
                                              ),
                                              conditionalPanel(paste0("input.fert_phos_type2_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                               textInput(paste0("fert_phos_type2_other_", type, "_", order), "")
                                                               )
                                              )
                                            )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                            selectizeInput(paste0("fert_phos_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                  "Alfalfa Meal",
                                                  "Bagasse",
                                                  "Biochar",
                                                  "Chicken litter (Poultry manure)",
                                                  "Compost",
                                                  "Farmyard manure",
                                                  "Guano",
                                                  "Liquid manure",
                                                  "Oil cake",
                                                  "Treated sewage sludge",
                                                  "Vermicompost",
                                                  "Fish fertilizer",
                                                  "Other")
                                                )
                                                ),
                                            column(width = 5,
                                                   selectizeInput(paste0("fert_phos_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                  c("g/sq m","kg/ha","lb/ac"))
                                            ),
                                            conditionalPanel(paste0("input.fert_phos_type2_", type, "_", order, " == 'Other'"),
                                                             column(width=12,
                                                             textInput(paste0("fert_phos_type2_other_", type, "_", order), "")
                                                             )
                                            )

                                              )

                                              }
                                     ),
                                     column(width=3 ,
                                        fluidRow(
                                           column(width =6,
                                                  selectizeInput(paste0("fertilizer_phos_application_technique2_", type, "_", order), "", multiple = T,
                                                                 options = list(maxItems = 1, placeholder ="Select one"),
                                                                 choices = c(
                                                                   "Band application on surface",
                                                                   "Band application incorporated (Band application beneath surface)",
                                                                   "Broadcast surface",
                                                                   "Broadcast incorporated",
                                                                   "Contact placement (seed placement)",
                                                                   "Deep placement",
                                                                   "Fertigation",
                                                                   "Foliar application",
                                                                   "Injection",
                                                                   "Placed with seed (seed placement)",
                                                                   "Side dressing",
                                                                   "Sub-soil placement (injection)",
                                                                   "Localized application (using mechanical or hand device)",
                                                                   "Other")
                                                  ),
                                                  conditionalPanel(paste0("input.fertilizer_phos_application_technique2_", type, "_", order, " == 'Other'"),
                                                                   column(width=12,
                                                                   textInput(paste0("fertilizer_phos_application_technique2_other_", type, "_", order), "")
                                                                   )
                                                  )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_phos_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_phos_implement2_", type, "_", order, " == 'Other'"),
                                                                   textInput(paste0("fertilizer_phos_implement2_other_", type, "_", order), "")
                                                )
                                         )
                                        )
                                     ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_amountApplied2_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_phos_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_nutrientContent2_", type, "_", order),"")
                                    )

                              )

                  ),


                  conditionalPanel(paste0("input.nutrientApplied_phos_numApps1_" , type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("3"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_phos_start_date3_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_phos_end_date3_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),


                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_phos_type3_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,


                                              selectizeInput(paste0("fert_phos_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_phos_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))

                                              ),
                                              conditionalPanel(paste0("input.fert_phos_type3_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                               textInput(paste0("fert_phos_type3_other_", type, "_", order), "")
                                                               )
                                              )

                                              )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                selectizeInput(paste0("fert_phos_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                  "Alfalfa Meal",
                                                  "Bagasse",
                                                  "Biochar",
                                                  "Chicken litter (Poultry manure)",
                                                  "Compost",
                                                  "Farmyard manure",
                                                  "Guano",
                                                  "Liquid manure",
                                                  "Oil cake",
                                                  "Treated sewage sludge",
                                                  "Vermicompost",
                                                  "Fish fertilizer",
                                                  "Other")
                                                )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_phos_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))
                                                ),
                                                conditionalPanel(paste0("input.fert_phos_type3_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                 textInput(paste0("fert_phos_type3_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )

                                            }
                                     ),
                                     column(width=3 ,
                                      fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_phos_application_technique3_", type, "_", order),label = "", multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c(
                                                                 "Band application on surface",
                                                                 "Band application incorporated (Band application beneath surface)",
                                                                 "Broadcast surface",
                                                                 "Broadcast incorporated",
                                                                 "Contact placement (seed placement)",
                                                                 "Deep placement",
                                                                 "Fertigation",
                                                                 "Foliar application",
                                                                 "Injection",
                                                                 "Placed with seed (seed placement)",
                                                                 "Side dressing",
                                                                 "Sub-soil placement (injection)",
                                                                 "Localized application (using mechanical or hand device)",
                                                                 "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_phos_application_technique3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_phos_application_technique3_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_phos_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_phos_implement3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_phos_implement3_other_", type, "_", order), "")
                                                )
                                         )
                                      )
                                     ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_amountApplied3_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_phos_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_phos_nutrientContent3_", type, "_", order),"")
                                    )

                                )


                  ),#end conditional2


                  fluidRow(
                    column(width = 2,
                           fluidRow(
                             column(width = 5,
                                    br(),
                                    div(style="text-align:right", "Potassium")
                             ),
                             column(width = 5,
                                    selectInput(paste0("nutrientApplied_potas_numApps1_", type, "_", order),"",c(1,2,3))

                             ),
                             column(width = 2,
                                    br(),
                                    div(style="text-align:center", h4("1"))
                             )
                           )
                    ),
                    column(width = 1,
                           dateInput(paste0("fert_potas_start_date1_", type, "_", order), label ="", format = "dd/mm/yyyy")
                    ),

                    column(width = 1,
                           dateInput(paste0("fert_potas_end_date1_", type, "_", order), label ="", format = "dd/mm/yyyy")
                    ),

                    column(width = 2,
                           if(type == "Green_manure"){
                             textInput(paste0("fert_potas_type1_", type, "_", order), "")
                           }
                           else if(type == "Inorganic"){
                             fluidRow(
                               column(width = 7,

                             selectizeInput(paste0("fert_potas_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                               'Ammonium nitrate',
                               'Ammonium nitrate sulfate',
                               'Ammonium polyphosphate',
                               'Ammonium sulfate',
                               'Anhydrous ammonia',
                               'Aqua ammonia',
                               'Calcitic limestone',
                               'Calcium ammonium nitrate solution',
                               'Calcium hydroxide',
                               'Calcium nitrate',
                               'Diammonium phosphate',
                               'Dolomitic limestone',
                               'Liquid phosphoric acid',
                               'Monoammonium phosphate',
                               'Potassium chloride',
                               'Potassium nitrate',
                               'Potassium sulfate',
                               'Rock phosphate',
                               'Single super phosphate',
                               'Triple super phosphate',
                               'Urea',
                               'Urea ammonium nitrate solution',
                               'Urea super granules',
                               'NPK fertilizers',
                               "Other")
                             )
                               ),
                             column(width = 5,
                                    selectizeInput(paste0("fert_potas_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                   c("g/sq m","kg/ha","lb/ac"))
                             ),
                             conditionalPanel(paste0("input.fert_potas_type1_", type, "_", order, " == 'Other'"),
                                              column(width=12,
                                              textInput(paste0("fert_potas_type1_other_", type, "_", order), "")
                                              )
                             )

                             )

                           }
                           else{
                             fluidRow(
                               column(width = 7,

                               selectizeInput(paste0("fert_potas_type1_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                 "Alfalfa Meal",
                                 "Bagasse",
                                 "Biochar",
                                 "Chicken litter (Poultry manure)",
                                 "Compost",
                                 "Farmyard manure",
                                 "Guano",
                                 "Liquid manure",
                                 "Oil cake",
                                 "Treated sewage sludge",
                                 "Vermicompost",
                                 "Fish fertilizer",
                                 "Other")
                               )
                               ),
                               column(width = 5,
                                      selectizeInput(paste0("fert_potas_type1_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                     c("g/sq m","kg/ha","lb/ac"))
                               ),
                               conditionalPanel(paste0("input.fert_potas_type1_", type, "_", order, " == 'Other'"),
                                                column(width=12,
                                                       textInput(paste0("fert_potas_type1_other_", type, "_", order), "")
                                                )
                               )

                             )

                             }
                    ),
                    column(width=3 ,
                      fluidRow(
                          column(width = 6,
                                 selectizeInput(paste0("fertilizer_potas_application_technique1_", type, "_", order), "", multiple = T,
                                                options = list(maxItems = 1, placeholder ="Select one"),
                                                choices = c(
                                                  "Band application on surface",
                                                  "Band application incorporated (Band application beneath surface)",
                                                  "Broadcast surface",
                                                  "Broadcast incorporated",
                                                  "Contact placement (seed placement)",
                                                  "Deep placement",
                                                  "Fertigation",
                                                  "Foliar application",
                                                  "Injection",
                                                  "Placed with seed (seed placement)",
                                                  "Side dressing",
                                                  "Sub-soil placement (injection)",
                                                  "Localized application (using mechanical or hand device)",
                                                  "Other")
                                 ),
                                 conditionalPanel(paste0("input.fertilizer_potas_application_technique1_", type, "_", order, " == 'Other'"),
                                                  textInput(paste0("fertilizer_potas_application_technique1_other_", type, "_", order), "")
                                 )
                          ),
                          column(width = 6,
                                 selectizeInput(paste0("fertilizer_potas_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                choices = c("Backpack sprayer (airblast sprayer)",
                                                            "Boom sprayer",
                                                            "Broadcast spreader",
                                                            "Hand sprayer",
                                                            "Manure spreader",
                                                            "Slurry injector",
                                                            "Manual application",
                                                            "Other")
                                 ),
                                 conditionalPanel(paste0("input.fertilizer_potas_implement1_", type, "_", order, " == 'Other'"),
                                                  textInput(paste0("fertilizer_potas_implement1__other_", type, "_", order), "")
                                 )

                          )
                        )
                    ),

                   column(width = 1,
                          textInput(paste0("fert_potas_amountApplied1_", type, "_", order),"")
                   ),
                   column(width = 1,
                          selectizeInput(paste0("fert_potas_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                         c("kg/m2","kg/ha","t/ha"))
                   ),
                   column(width = 1,
                          textInput(paste0("fert_potas_nutrientContent1_", type, "_", order),"")
                   )
                  ),

                  conditionalPanel(paste0("input.nutrientApplied_potas_numApps1_", type, "_", order,  " == 2 |
                                          input.nutrientApplied_potas_numApps1_", type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("2"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_potas_start_date2_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_potas_end_date2_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_potas_type2_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_potas_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_potas_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))
                                              ),
                                              conditionalPanel(paste0("input.fert_potas_type2_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                                      textInput(paste0("fert_potas_type2_other_", type, "_", order), "")
                                                               )
                                              )

                                              )
                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                  selectizeInput(paste0("fert_potas_type2_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                    "Alfalfa Meal",
                                                    "Bagasse",
                                                    "Biochar",
                                                    "Chicken litter (Poultry manure)",
                                                    "Compost",
                                                    "Farmyard manure",
                                                    "Guano",
                                                    "Liquid manure",
                                                    "Oil cake",
                                                    "Treated sewage sludge",
                                                    "Vermicompost",
                                                    "Fish fertilizer",
                                                    "Other")
                                                  )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_potas_type2_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))

                                                ),
                                                conditionalPanel(paste0("input.fert_potas_type2_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                        textInput(paste0("fert_potas_type2_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )
                                              }
                                     ),
                                     column(width=3 ,
                                      fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_application_technique2_", type, "_", order), "", multiple = T,
                                                               options = list(maxItems = 1, placeholder ="Select one"),
                                                               choices = c(
                                                                 "Band application on surface",
                                                                 "Band application incorporated (Band application beneath surface)",
                                                                 "Broadcast surface",
                                                                 "Broadcast incorporated",
                                                                 "Contact placement (seed placement)",
                                                                 "Deep placement",
                                                                 "Fertigation",
                                                                 "Foliar application",
                                                                 "Injection",
                                                                 "Placed with seed (seed placement)",
                                                                 "Side dressing",
                                                                 "Sub-soil placement (injection)",
                                                                 "Localized application (using mechanical or hand device)",
                                                                 "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_application_technique2_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_application_technique2_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_implement2_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_implement2__other_", type, "_", order), "")
                                                )
                                         )
                                      )
                                     ),
                                    column(width = 1,
                                           textInput(paste0("fert_potas_amountApplied2_", type, "_", order),"")
                                    ),
                                    column(width = 1,
                                           selectizeInput(paste0("fert_potas_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                          c("kg/m2","kg/ha","t/ha"))
                                    ),
                                    column(width = 1,
                                           textInput(paste0("fert_potas_nutrientContent2_", type, "_", order),"")
                                    )
                                   )

                  ),


                  conditionalPanel(paste0("input.nutrientApplied_potas_numApps1_" , type, "_", order, " == 3 "),
                                   fluidRow(
                                     column(width = 2,
                                            fluidRow(
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")
                                              ),
                                              column(width = 5,
                                                     br(),
                                                     div(style="text-align:right", "")

                                              ),
                                              column(width = 2,
                                                     br(),
                                                     div(style="text-align:center", h4("3"))
                                              )
                                            )
                                     ),
                                     column(width = 1,
                                            dateInput(paste0("fert_potas_start_date3_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 1,
                                            dateInput(paste0("fert_potas_end_date3_", type, "_", order), label ="", format = "dd/mm/yyyy")
                                     ),

                                     column(width = 2,
                                            if(type == "Green_manure"){
                                              textInput(paste0("fert_potas_type3_", type, "_", order), "")
                                            }
                                            else if(type == "Inorganic"){
                                              fluidRow(
                                                column(width = 7,

                                              selectizeInput(paste0("fert_potas_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                'Ammonium nitrate',
                                                'Ammonium nitrate sulfate',
                                                'Ammonium polyphosphate',
                                                'Ammonium sulfate',
                                                'Anhydrous ammonia',
                                                'Aqua ammonia',
                                                'Calcitic limestone',
                                                'Calcium ammonium nitrate solution',
                                                'Calcium hydroxide',
                                                'Calcium nitrate',
                                                'Diammonium phosphate',
                                                'Dolomitic limestone',
                                                'Liquid phosphoric acid',
                                                'Monoammonium phosphate',
                                                'Potassium chloride',
                                                'Potassium nitrate',
                                                'Potassium sulfate',
                                                'Rock phosphate',
                                                'Single super phosphate',
                                                'Triple super phosphate',
                                                'Urea',
                                                'Urea ammonium nitrate solution',
                                                'Urea super granules',
                                                'NPK fertilizers',
                                                "Other")
                                              )
                                                ),
                                              column(width = 5,
                                                     selectizeInput(paste0("fert_potas_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                    c("g/sq m","kg/ha","lb/ac"))
                                              ),
                                              conditionalPanel(paste0("input.fert_potas_type3_", type, "_", order, " == 'Other'"),
                                                               column(width=12,
                                                                      textInput(paste0("fert_potas_type3_other_", type, "_", order), "")
                                                               )
                                              )
                                            )

                                            }
                                            else{
                                              fluidRow(
                                                column(width = 7,

                                                  selectizeInput(paste0("fert_potas_type3_", type, "_", order), multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="", choices =c(
                                                    "Alfalfa Meal",
                                                    "Bagasse",
                                                    "Biochar",
                                                    "Chicken litter (Poultry manure)",
                                                    "Compost",
                                                    "Farmyard manure",
                                                    "Guano",
                                                    "Liquid manure",
                                                    "Oil cake",
                                                    "Treated sewage sludge",
                                                    "Vermicompost",
                                                    "Fish fertilizer",
                                                    "Other")
                                                  )
                                                ),
                                                column(width = 5,
                                                       selectizeInput(paste0("fert_potas_type3_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                                      c("g/sq m","kg/ha","lb/ac"))
                                                ),
                                                conditionalPanel(paste0("input.fert_potas_type3_", type, "_", order, " == 'Other'"),
                                                                 column(width=12,
                                                                        textInput(paste0("fert_potas_type3_other_", type, "_", order), "")
                                                                 )
                                                )

                                              )
                                            }
                                     ),
                                     column(width=3 ,
                                      fluidRow(
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_application_technique3_", type, "_", order),label = "", multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c(
                                                                 "Band application on surface",
                                                                 "Band application incorporated (Band application beneath surface)",
                                                                 "Broadcast surface",
                                                                 "Broadcast incorporated",
                                                                 "Contact placement (seed placement)",
                                                                 "Deep placement",
                                                                 "Fertigation",
                                                                 "Foliar application",
                                                                 "Injection",
                                                                 "Placed with seed (seed placement)",
                                                                 "Side dressing",
                                                                 "Sub-soil placement (injection)",
                                                                 "Localized application (using mechanical or hand device)",
                                                                 "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_application_technique3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_application_technique3_other_", type, "_", order), "")
                                                )
                                         ),
                                         column(width = 6,
                                                selectizeInput(paste0("fertilizer_potas_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                               choices = c("Backpack sprayer (airblast sprayer)",
                                                                           "Boom sprayer",
                                                                           "Broadcast spreader",
                                                                           "Hand sprayer",
                                                                           "Manure spreader",
                                                                           "Slurry injector",
                                                                           "Manual application",
                                                                           "Other")
                                                ),
                                                conditionalPanel(paste0("input.fertilizer_potas_implement3_", type, "_", order, " == 'Other'"),
                                                                 textInput(paste0("fertilizer_potas_implement3_other_", type, "_", order), "")
                                                )
                                         )
                                      )
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_potas_amountApplied3_", type, "_", order),"")
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fert_potas_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder=""), label= "",
                                                           c("kg/m2","kg/ha","t/ha"))
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_potas_nutrientContent3_", type, "_", order),"")
                                     )

                              )


                  )#end conditional2

             ))


  }

  ###################### end nutrients #####################################


  observeEvent(input$land_impl_type,{
    if(input$land_impl_type == "Other"){
      session$sendCustomMessage(type="focus",message="land_impl_type_other")
    }

  })


  ########### traits table ############################################

  traitsVals <- reactiveValues()
  traitsVals$aux <- data.frame()
  traitsVals$selectedRows <- list()
  traitsVals$Data <- data.table()
#   dict <- data.frame(stringsAsFactors = FALSE,
#          # c("<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>"
#          c("Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected"
# ),
# # ),
#          c('Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Soybean','Soybean','Soybean'),
#          c('Number of tubers planted','Number of emerged plants','Percentage plants emerged','Number of harvested plants','Percentage of plants harvested','Non-marketable tuber number','Total number of tubers','Number marketable tubers','Non-marketable tuber weight','Total tuber weight','Total tuber yield no adjusted','Total tuber yield adjusted','Marketable tuber weight','Marketable tuber yield no adjusted','Marketable tuber yield adjusted','Average of tuber weight','Average of marketable tuber weight','Sprouting','Initial Vigor','Plant Stands Harvested','Root Number','Storage root weight','Root Yield in fresh weight','Root Yield in dry weight','Root Yield in Fresh Aerial','Stem weight','Stem number','Marketable root weight','Non marketable root weight','Number of rotten stem','Storage root weight','Number of planted stakes','Seedling number','Non marketable root number','Marketable root number','Stock weight','Stem weight','Number of germinated stakes','Root Yield','Storage root weight ','Storage root weight Peel','Number of stakes','Aboveground biomass at maturity','Grain weight','Grain yield','Grain yield factor','Harvest index','In-season aboveground biomass','Grain row number','Grain weight','Grain yield adjusted','Grain yield in dry weight','Grain yield in fresh weight','Grain yield rank number','Grain yield relative to check','Grain yield relative','Shelled cob in fresh weight','Grain test weight ','Grain weight adjusted','Grain weight in fresh weight','Grain yield in fresh weight','Number of plants established','Number of plants planted','Number of plants harvested','Number of plants with storage roots','Number of commercial storage roots','Number of non-commercial storage roots ','Total number of root','Weight of commercial storage roots','Weight of non-commercial storage roots ','Weight of vines','Total root weight','Marketable root yield','Average commercial root weight','Yield of total roots','Percentage of marketable roots','Biomass yield','Relative Storage Root Yield','Storage Root Yield relative to check','Fodder Yield','Seed yield','Seed weight'),
#          c('CO_330:0000265-/plot','CO_330:0000268-/plot','CO_330:0000283- ','CO_330:0000287-/plot','CO_330:0000290- ','CO_330:0000300-/plot','CO_330:0000304-/plot,CO_330:0000305-/plant','CO_330:0000293-/plot,CO_330:0000297-/plant','CO_330:0000314-kg/plot','CO_330:0000317-kg/plot,CO_330:0000321-kg/plant','CO_330:0000324-t/ha','CO_330:0000323-t/ha','CO_330:0000308- ','CO_330:0000330-t/ha','CO_330:0000327-t/ha','CO_330:0000333-g','CO_330:0000336-g','CO_334:0000008-ratio','CO_334:0000009- ','CO_334:0000010-/plant','CO_334:0000011- ','CO_334:0000012- kg/plot','CO_334:0000013-t/ha','CO_334:0000014-t/ha','CO_334:0000017-t/ha','CO_334:0000127-kg/pl','CO_334:0000129-Stem','CO_334:0000131-kg/plot','CO_334:0000132-/plot','CO_334:0000133- ','CO_334:0000157-kg/pl,CO_334:0000158-kg/plot','CO_334:0000159- ','CO_334:0000166- ','CO_334:0000168-/plot','CO_334:0000169-/plot','CO_334:0000170-/kg','CO_334:0000171-/kg','CO_334:0000213-1 month,CO_334:0000214-3 months,CO_334:0000215-6 months,CO_334:0000216-9 months,CO_334:0000217-12 months','CO_334:0000230-kg/plant,CO_334:0000231-t/ha','CO_334:0000247-kg','CO_334:0000248-kg','CO_334:0000250- ','CO_321:0001034-m2/kg,CO_321:0001035-kg/ha,CO_321:0001036-t/ha,CO_321:0001037-g/plant,CO_321:0001038-g/plot,CO_321:0001039-kg/plot','CO_321:0001213-g/1000 grain,CO_321:0001214-g/100 grain,CO_321:0001215-g/200 grain','CO_321:0001217-g/m2,CO_321:0001218-kg/ha,CO_321:0001219-t/ha,CO_321:0001220-g/plant,CO_321:0001221-g/plot,CO_321:0001222-kg/plot,CO_321:0001223-%','CO_321:0001224- ','CO_321:0001231- ,CO_321:0001232-%','CO_321:0001246-m2/kg,CO_321:0001247-kg/ha,CO_321:0001248-t/ha,CO_321:0001249-g/plot,CO_321:0001250-kg/plot,CO_321:0001651- ','CO_322:0000694- ','CO_322:0000723-g/1000grain,CO_322:0000725-g/100grain,CO_322:0000727-g/200grain','CO_322:0000730-kg/ha,CO_322:0000731-t/ha','CO_322:0000734-g/plot,CO_322:0000737-kg/ha,CO_322:0000740-kg/plot,CO_322:0000742-t/ha','CO_322:0000744-g/plot,CO_322:0000747-kg/ha,CO_322:0000749-kg/plot,CO_322:0000751-t/ha','CO_322:0000754- ','CO_322:0000756-%','CO_322:0000757-%','CO_322:0000928-g/plot,CO_322:0000931-kg/plot','CO_322:0001008-lb/bsh','CO_322:0001009-g/1000grain,CO_322:0001010-g/100grain,CO_322:0001011-g/200grain','CO_322:0001012-g/200grain,CO_322:0001013-g/1000grain,CO_322:0001014-g/100grain','CO_322:0001016-lb/plot','CO_331:0000192-/plot','CO_331:0000678-/plot','CO_331:0000679-/plot','CO_331:0000211-/plot','CO_331:0000214-/plot','CO_331:0000217-/plot','CO_331:0000233-/plot,CO_331:0000230-/plant','CO_331:0000220-kg/plot','CO_331:0000223-kg/plot','CO_331:0000227-kg/plot','CO_331:0000237-kg/plot','CO_331:0000218-t/ha','CO_331:0000680-t/ha','CO_331:0000681-kg/plot,CO_331:0000296-t/ha','CO_331:0000682-%','CO_331:0000683-t/ha','CO_331:0000791- ','CO_331:0000792- ','CO_336:0000262-g/plot,CO_336:0000340-kg/ha','CO_336:0000261-g/plot,CO_336:0000337-kg/ha','CO_336:0000333-g'),
#          # c('', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '','','','','','','','','','','','','','','','','','', '','','',''),
#          # c('', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '','','','','','','','','','','','','','','','','','', '','','','')
#          c('CO_330:0000265','CO_330:0000268','CO_330:0000283','CO_330:0000287','CO_330:0000290','CO_330:0000300','CO_330:0000304','CO_330:0000293','CO_330:0000314','CO_330:0000317','CO_330:0000324','CO_330:0000323','CO_330:0000308','CO_330:0000330','CO_330:0000327','CO_330:0000333','CO_330:0000336','CO_334:0000008','CO_334:0000009','CO_334:0000010','CO_334:0000011','CO_334:0000012','CO_334:0000013','CO_334:0000014','CO_334:0000017','CO_334:0000127','CO_334:0000129','CO_334:0000131','CO_334:0000132','CO_334:0000133','CO_334:0000157','CO_334:0000159','CO_334:0000166','CO_334:0000168','CO_334:0000169','CO_334:0000170','CO_334:0000171','CO_334:0000213','CO_334:0000230','CO_334:0000247','CO_334:0000248','CO_334:0000250','CO_321:0001034','CO_321:0001213','CO_321:0001217','CO_321:0001224','CO_321:0001231','CO_321:0001246','CO_322:0000694','CO_322:0000723','CO_322:0000730','CO_322:0000734','CO_322:0000744','CO_322:0000754','CO_322:0000756','CO_322:0000757','CO_322:0000928','CO_322:0001008','CO_322:0001009','CO_322:0001012','CO_322:0001016','CO_331:0000192','CO_331:0000678','CO_331:0000679','CO_331:0000211','CO_331:0000214','CO_331:0000217','CO_331:0000233','CO_331:0000220','CO_331:0000223','CO_331:0000227','CO_331:0000237','CO_331:0000218','CO_331:0000680','CO_331:0000681','CO_331:0000682','CO_331:0000683','CO_331:0000791','CO_331:0000792','CO_336:0000262','CO_336:0000261','CO_336:0000333'),
#          c('/plot','/plot','','/plot','','/plot','/plot','/plot','kg/plot','kg/plot','t/ha','t/ha','','t/ha','t/ha','g','g','ratio','','/plant','',' kg/plot','t/ha','t/ha','t/ha','kg/pl','Stem','kg/plot','/plot','','kg/pl','','','/plot','/plot','/kg','/kg','1 month','kg/plant','kg','kg','','m2/kg','g/1000 grain','g/m2','','','m2/kg','','g/1000grain','kg/ha','g/plot','g/plot','','%','%','g/plot','lb/bsh','g/1000grain','g/200grain','lb/plot','/plot','/plot','/plot','/plot','/plot','/plot','/plot','kg/plot','kg/plot','kg/plot','kg/plot','t/ha','t/ha','kg/plot','%','t/ha','','','g/plot','g/plot','g')
#
#   )
#   colnames(dict) <- c("Status","Crop", "Crop trait management", "traitCode", "VariableId", "Scale")

  dict <- data.frame(stringsAsFactors = FALSE,
      # c("Not selected",'"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"','"Not selected"'),
      # c('Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Soybean','Soybean','Soybean','Soybean','Soybean'),
      # c('Number of tubers planted','Number of emerged plants','Plant emergence proportion','Number of harvested plants','Proportion of plants harvested','Non-marketable tuber number','Tuber number','Tuber number per plant','Number of marketable tubers','Number of marketable tubers per plant','Non-marketable tuber weight','Tuber weight','Tuber weight per plant','Tuber yield no adjusted','Tuber yield adjusted','Marketable tuber weight','Marketable tuber weight per plant','Marketable tuber yield no adjusted','Marketable tuber yield adjusted','Average of tuber weight','Average of marketable tuber weight','Sprouting','Initial Vigor','Plant Stands Harvested','Root Number','Storage root weight','Root Yield','Root Yield','Root Yield','Stem weight','Stem number','Marketable root weight','Non marketable root weight','Number of rotten stem','Storage root weight','Storage root weight','Number of planted stakes','Seedling number','Non marketable root number','Marketable root number','Stock weight','Stem weight','Sprout count','Root Yield','Root Yield','Storage root weight','Storage root weight','Number of stakes','Aboveground biomass at maturity','Aboveground biomass at maturity','Grain weight','Grain yield','Grain yield','Grain yield','Grain yield factor','Harvest index','In-season aboveground biomass','In-season aboveground biomass','In-season aboveground biomass','Grain row number','Grain weight','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Shelled cob weight','Grain test weight','Grain weight','Grain weight','Grain yield','Number of plants established','Number of plants planted','Number of plants harvested','Number of plants with storage roots','Number of commercial storage roots','Number of non-commercial storage roots','Total number of root','Total number of root','Weight of commercial storage roots','Weight of non-commercial storage roots','Weight of vines','Total root weight','Marketable root yield','Average commercial root weight','Yield of total roots','Yield of total roots','Percentage of marketable roots','Biomass yield','Relative Storage Root Yield','Storage Root Yield relative to check','Fodder Yield','Fodder Yield','Seed yield','Seed yield','Seed weight'),
      # c('Number of tubers planted - method','Number of emerged plants - method','Plant emergence proportion - method','Number of harvested plants - method','Proportion of plants harvested - method','Non-marketable tuber number - method','Tuber number - method','Tuber number per plant - method','Number of marketable tubers - method','Number of marketable tubers per plant - method','Non-marketable tuber weight - method','Tuber weight - method','Tuber weight per plant - method','Tuber yield no adjusted - method','Tuber yield adjusted - method','Marketable tuber weight - method','Marketable tuber weight per plant - method','Marketable tuber yield no adjusted - method','Marketable tuber yield adjusted - method','Average of tuber weight - method','Average of marketable tuber weight - method','Counting:Sprouting_method','Visual Rating:Initial Vigor_method','Counting:Plant Stands Harvested_method','Counting: Root number_method','Measurement :Fresh Weight of Storage Root_method','Calculation :Fresh Root Yield_method','Calculation :Dry Yield_method','Calculation :Top Yield_method','Measurement :Stem weight_method','Counting:stem number_method','Measurement :marketable root weight_method','Measurement : Non marketable root weight_method','Counting:Stem rot_method','Measurement :root weight in air_method','Measurement :root weight in water_method','Counting:number of planted stakes_method','Counting:germination count_method','Counting:non marketable root number_method','Counting:marketable root number_method','Measurement :stock weight_method','Counting:stem yield_method','Counting:sprout count at month_method','Estimation :plant yield_method','Estimation :yield per year_method','Measurement: root weight after washing_method','Measurement :root weight after peel_method','Counting:number of stakes_method','BM Computation','BM Measurement','GW Measurement','GY Computation','GY Measurement','GY Relative to check Computation','GYFac Computation','HI Computation','ISBM Computation','ISBM Measurement','ISBM Estimation','GCol - Estimation','GW DW - Measurement','Adjusted GY - Computation','DW GY - Measurement','DW GY - Computation','FW GY - Measurement','FW GY - Computation','RGY - Computation','GYRank - Computation','Relative to check - Computation','ShellCobW - Measurement (duplicate)','GTW - Computation','GW Adjusted - Computation','GW FW - Measurement','FW GY - Measurement','Evaluation of plant vine establishment','Recording planting materials','Evaluation of plants','Evaluation of plants','Evaluation of roots','Evaluation of roots','Estimated number per plot - Method','Estimated number per plant - Method','Measurements of root mass','Measurements of root mass','Measurements of vine mass','Estimated weight per plot - Method','Estimated marketable yield per hectare - Method','Estimated marketable yield per hectare - Method','Estimated yield of total roots per hectare - No adjusted, Method','Estimated yield per hectare - Adjusted, Method','Percentage of marketable - Method','Biomass yield','Observation of an average or all storage roots within a single plant or plot','Relative to check - Computation','Weighing of fodder per plot','Calculation of fodder yield','Weighing of grain per plot','grain yield calculation','100 weight'),
      # c('tuber/plot-CO_330:0000265','tuber/plot-CO_330:0000268','%-CO_330:0000283','plants/plot-CO_330:0000287','%-CO_330:0000290','tuber/plot-CO_330:0000300','tuber/ plot-CO_330:0000304','tuber /plant-CO_330:0000305','tuber/plot-CO_330:0000293','tuber/plant-CO_330:0000297','kg/plot-CO_330:0000314','kg/plot-CO_330:0000317','kg/plant-CO_330:0000321','t/ha-CO_330:0000324','t/ha-CO_330:0000323','kg/plot-CO_330:0000308','kg/plant-CO_330:0000311','t/ha-CO_330:0000330','t/ha-CO_330:0000327','g-CO_330:0000333','g-CO_330:0000336','ratio-CO_334:0000008','7 pt scale-CO_334:0000009','Plant-CO_334:0000010','Count-CO_334:0000011','kg/plot-CO_334:0000012','t/ha-CO_334:0000013','t/ha-CO_334:0000014','t/ha-CO_334:0000017','kg/pl-CO_334:0000127','Stem-CO_334:0000129','kg/plot-CO_334:0000131','kg/plot-CO_334:0000132','Number-CO_334:0000133','kg/pl-CO_334:0000157','kg/plot-CO_334:0000158','Number-CO_334:0000159','Seedling-CO_334:0000166','plot-CO_334:0000168','plot-CO_334:0000169','kg-CO_334:0000170','kg-CO_334:0000171','1 month-CO_334:0000213,3 months-CO_334:0000214,6 months-CO_334:0000215,9 months-CO_334:0000216','kg/plant-CO_334:0000230','t/ha-CO_334:0000231','kg-CO_334:0000247','kg-CO_334:0000248','Count-CO_334:0000250','m2/kg-CO_321:0001034,kg/ha-CO_321:0001035,t/ha-CO_321:0001036','g/plant-CO_321:0001037,g/plot-CO_321:0001038,kg/plot-CO_321:0001039','g/1000 grain-CO_321:0001213,g/100 grain-CO_321:0001214,g/200 grain-CO_321:0001215','g/m2-CO_321:0001217,kg/ha-CO_321:0001218,t/ha-CO_321:0001219','/plant-CO_321:0001220,g/plot-CO_321:0001221,kg/plot-CO_321:0001222','%-CO_321:0001223','num-CO_321:0001224','index-CO_321:0001231,%-CO_321:0001232','m2/kg-CO_321:0001246,kg/ha-CO_321:0001247,t/ha-CO_321:0001248','g/plot-CO_321:0001249,kg/plot-CO_321:0001250','1-5 scoring scale-CO_321:0001651','row/ear-CO_322:0000694','g/1000grain-CO_322:0000723,g/100grain-CO_322:0000725,g/200grain-CO_322:0000727','kg/ha-CO_322:0000730,t/ha-CO_322:0000731','g/plot-CO_322:0000734,kg/plot-CO_322:0000740','t/ha-CO_322:0000742,kg/ha-CO_322:0000737','g/plot-CO_322:0000744,kg/plot-CO_322:0000749','kg/ha-CO_322:0000747,t/ha-CO_322:0000751','%-CO_322:0000757','Rank number-CO_322:0000754','%-CO_322:0000756','/plot-CO_322:0000928,kg/plot-CO_322:0000931','lb/bsh-CO_322:0001008','g/1000grain-CO_322:0001009,g/100grain-CO_322:0001010,g/200grain-CO_322:0001011','g/200grain-CO_322:0001012,g/1000grain-CO_322:0001013,g/100grain-CO_322:0001014','lb/plot-CO_322:0001016','plants/plot-CO_331:0000192','plants/plot-CO_331:0000678','plants/plot-CO_331:0000679','plants/plot-CO_331:0000211','roots/plot-CO_331:0000214','roots/plot-CO_331:0000217','roots/ plot-CO_331:0000233','roots/ plant-CO_331:0000230','kg/plot-CO_331:0000220','kg/plot-CO_331:0000223','kg/plot-CO_331:0000227','kg/plot-CO_331:0000237','t/ha-CO_331:0000218','t/ha-CO_331:0000680','t/ha-CO_331:0000681','t/ha-CO_331:0000296','%-CO_331:0000682','t/ha-CO_331:0000683','RtYldR 5 pt. scale-CO_331:0000791','%-CO_331:0000792','g/plot-CO_336:0000262','kg/ha-CO_336:0000340','g/plot-CO_336:0000261','kg/ha-CO_336:0000337','g-CO_336:0000333'),
      # c('CO_330:0000265','CO_330:0000268','CO_330:0000283','CO_330:0000287','CO_330:0000290','CO_330:0000300','CO_330:0000304','CO_330:0000305','CO_330:0000293','CO_330:0000297','CO_330:0000314','CO_330:0000317','CO_330:0000321','CO_330:0000324','CO_330:0000323','CO_330:0000308','CO_330:0000311','CO_330:0000330','CO_330:0000327','CO_330:0000333','CO_330:0000336','CO_334:0000008','CO_334:0000009','CO_334:0000010','CO_334:0000011','CO_334:0000012','CO_334:0000013','CO_334:0000014','CO_334:0000017','CO_334:0000127','CO_334:0000129','CO_334:0000131','CO_334:0000132','CO_334:0000133','CO_334:0000157','CO_334:0000158','CO_334:0000159','CO_334:0000166','CO_334:0000168','CO_334:0000169','CO_334:0000170','CO_334:0000171','CO_334:0000213','CO_334:0000230','CO_334:0000231','CO_334:0000247','CO_334:0000248','CO_334:0000250','CO_321:0001034','CO_321:0001037','CO_321:0001213','CO_321:0001217','CO_321:0001220','CO_321:0001223','CO_321:0001224','CO_321:0001231','CO_321:0001246','CO_321:0001249','CO_321:0001651','CO_322:0000694','CO_322:0000723','CO_322:0000730','CO_322:0000734','CO_322:0000742','CO_322:0000744','CO_322:0000747','CO_322:0000757','CO_322:0000754','CO_322:0000756','CO_322:0000928','CO_322:0001008','CO_322:0001009','CO_322:0001012','CO_322:0001016','CO_331:0000192','CO_331:0000678','CO_331:0000679','CO_331:0000211','CO_331:0000214','CO_331:0000217','CO_331:0000233','CO_331:0000230','CO_331:0000220','CO_331:0000223','CO_331:0000227','CO_331:0000237','CO_331:0000218','CO_331:0000680','CO_331:0000681','CO_331:0000296','CO_331:0000682','CO_331:0000683','CO_331:0000791','CO_331:0000792','CO_336:0000262','CO_336:0000340','CO_336:0000261','CO_336:0000337','CO_336:0000333'),
      # c('tuber/plot','tuber/plot','%','plants/plot','%','tuber/plot','tuber/ plot','tuber /plant','tuber/plot','tuber/plant','kg/plot','kg/plot','kg/plant','t/ha','t/ha','kg/plot','kg/plant','t/ha','t/ha','g','g','ratio','7 pt scale','Plant','Count','kg/plot','t/ha','t/ha','t/ha','kg/pl','Stem','kg/plot','kg/plot','Number','kg/pl','kg/plot','Number','Seedling','plot','plot','kg','kg','1 month','kg/plant','t/ha','kg','kg','Count','m2/kg','g/plant','g/1000 grain','g/m2','g/plant','%','num','index','m2/kg','g/plot','1-5 scoring scale','row/ear','g/1000grain','kg/ha','g/plot','t/ha','g/plot','kg/ha','%','Rank number','%','g/plot','lb/bsh','g/1000grain','g/200grain','lb/plot','plants/plot','plants/plot','plants/plot','plants/plot','roots/plot','roots/plot','roots/ plot','roots/ plant','kg/plot','kg/plot','kg/plot','kg/plot','t/ha','t/ha','t/ha','t/ha','%','t/ha','RtYldR 5 pt. scale','%','g/plot','kg/ha','g/plot','kg/ha','g')
      c("Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected"),
      c('Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Soybean','Soybean','Soybean','Soybean','Soybean'),
      c('Number of tubers planted','Number of emerged plants','Plant emergence proportion','Number of harvested plants','Proportion of plants harvested','Non-marketable tuber number','Tuber number','Tuber number per plant','Number of marketable tubers','Number of marketable tubers per plant','Non-marketable tuber weight','Tuber weight','Tuber weight per plant','Tuber yield no adjusted','Tuber yield adjusted','Marketable tuber weight','Marketable tuber weight per plant','Marketable tuber yield no adjusted','Marketable tuber yield adjusted','Average of tuber weight','Average of marketable tuber weight','Sprouting','Initial Vigor','Plant Stands Harvested','Root Number','Storage root weight','Root Yield','Root Yield','Root Yield','Stem weight','Stem number','Marketable root weight','Non marketable root weight','Number of rotten stem','Storage root weight','Storage root weight','Number of planted stakes','Seedling number','Non marketable root number','Marketable root number','Stock weight','Stem weight','Sprout count','Root Yield','Root Yield','Storage root weight','Storage root weight','Number of stakes','Aboveground biomass at maturity','Grain weight','Grain yield','Grain yield','Grain yield','Grain yield factor','Harvest index','In-season aboveground biomass','In-season aboveground biomass','Grain weight','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Grain yield','Shelled cob weight','Grain test weight','Grain weight','Grain weight','Grain yield','Number of plants established','Number of plants planted','Number of plants harvested','Number of plants with storage roots','Number of commercial storage roots','Number of non-commercial storage roots','Total number of root','Total number of root','Weight of commercial storage roots','Weight of non-commercial storage roots','Weight of vines','Total root weight','Marketable root yield','Average commercial root weight','Yield of total roots','Yield of total roots','Percentage of marketable roots','Biomass yield','Relative Storage Root Yield','Storage Root Yield relative to check','Fodder Yield','Fodder Yield','Seed yield','Seed yield','Seed weight'),
      c('Count the number of planted tubers and record it','Count the number of emerged plants and record it','Compute the proportion of plants emerged over tubers planted using the formula','Count the number of harvested plants and record it','Compute the proportion of plant harvested over plant emerged using the formula','Count the number of non-marketable tubers per unit area and record it','Compute the total number of tubers per unit area using the formula','Compute the total number of tubers per plant using the formula','Compute the total number of marketable tubers per unit area using the formula','Compute the total number of marketable tubers per plant','Compute the weight of non-marketable tubers per unit area usihg the formula','Compute the total weight of tubers per unit area using the formula','Compute the total weight of tubers per plant using the formula','Compute the total tuber yield no adjusted per unit area using the formula','Compute the total tuber yield adjusted per unit area using the formula','Compute the total weight of marketable tubers per unit area using the formula','Compute the total weight of marketable tubers per plant using the formula','Compute the marketable tuber yield no adjusted per unit area using the formula','Compute the marketable tuber yield adjusted per unit area using the formula','Compute the average tuber weight in grams using the formula','Compute the average marketable tuber weight in grams using the formula','The number of germinated stakes divided by the total number of planted stakes scored one month after planting','Trait monitored by observing plant vigor one month after planting','Count the number of plant stands that are harvested in a plot','','Weigh harvested storage roots per plot at harvest','Calculated as weight of fresh storage roots expressed in tons per hectares per plant at harvest','Dry weight of harvested roots derived by multiplying fresh storage root yield by dry matter content expressed in tons per hectares.','Calculated as weight of foliage and stems expressed in tons per hectares per plot at harvest','Measured stem weight excluding leaves and stump','Count of the number of stems per plot','Measured weight of harvested cassava roots usually classified as large size and medium size excluding small sized roots','','Count of the rotted stems per plot at the time of harvest','Measured weight of cassava root samples (kg) between 4 - 5kg of each of the harvested plot to determine the dry matter by specific gravity','As part of the dry matter determination method by specific gravimetry','Count of the number of stakes planted per plot','Count of the number of emerging seedlings from each family in the pre-nursery done on a daily bases until its ready for transplanting','Count of the number of small or less than 1kg root size','Count of the number of big or more than 1kg root size','Measurement of the fresh weight of the planted part anchoring the storage root(kg)','Measurement of the fresh weight of harvested plant biomass excluding leaves','Count of the number of stakes germinated','Average yield per plant in a plot. It is estimated by dividing the total weight of roots by the number of plants harvested.','Annual root yield using yield per hectare as a function of the crop duration.','Fresh cassava roots are washed in water and weighed on a pan suspended to a weighing scale','This is the weight of peeled cassava roots using a pan suspended to a weighing scale','An estimated number of plantable stakes (about 20cm long','Cut all aboveground biomass in a predetermined area (A). Avoid border effects by sampling away from edges of plot. Biomass as other yield components can be calculated or measured individually (Bell and Fischer, 1994; Reynolds et al., 2001; Pask et al., 2012), decide which method suit better for your objectives.','Dry grains at 70oC and weigh.','Use formulae to calculate grain yield in g/m2','The weight of the grain harvested is registered on a scale, decide which method suit better for your objectives. In breeding trials, a sample area (rather than the whole plot) is generally used for estimating yield. Discard borders when combine harvest for a better estimation of yield.','Calculate grain yield of an entry as percentage over a local check.','Standard method for yield factor','Harvest index is expressed as a ratio and can be calculated as Harvest index = (Grain yield/Biomass).','Sampling is typically performed at sequential developmental stages/time intervals through crop growth. Cut all aboveground biomass in a predetermined area (A). Avoid border effects by sampling away from edges of plot.  In most cases, determinations of dry mass are made on representative sub-samples to reduce oven space requirement, take additional measurements (e.g., fertile culm count) etc. Several protocols available (Bell and Fischer, 1994; Reynolds et al., 2001; Pask et al., 2012), decide which method suit better for your objectives.','Standard method for In-season aboveground biomass.','Count and weigh grains randomly selected from the total grains.','Calculate shelled grain yield per unit area adjusted to 12.5% grain moisture.','Shell the grains (kernels) from the ears harvested per plot and put grains in a paper bag and  dry at 60-70∞C for 1-2 days, then measure and record the weight of dried grain.','Shell the grains (kernels) from the ears harvested per plot and record fresh (field) weight.','It is calculated as the numerical position of the progeny when yields are arrenged from highest to lowest.','Calculated as relative grain yield agaisnst the best check in percentage.','Relative grain yield expressed as percentage of the mean grain yield of the trial. Values above 100% indicate above-average performance; values below 100% indicate below-average performance.','Record shelled cob field weight.','Standard method for grain test weight.','Compute grain weigh adjuted to 12.5% moisture.','Count and weigh grains randomly selected from the total grains.','Shell the grains (kernels) from the ears harvested per plot and record fresh (field) weight.','Counting of established plants.','Counting plants/vines planted.','Visual estimation','Visual estimation','Visual estimation','Visual estimation','Number of commercial plus Number of non-commercial roots','Total number of root per plot / Number of plants harvested','Measured using scales','Measured using scales','Measured using scales','Weight of commercial storage roots plus weight of non-commercial storage roots','(Weight of commercial storage roots/ plot size)*10','(Weight of commercial storage roots/ Number of non-commercial roots','(Weight of commercial storage roots/ plot size)*10','(Weight total of root/ plot size)*10','Number of non-commercial roots/Total number of root after harvest*100','','','','Measure the dry weight of stover, haulm harvested after threshing plants harvested from the plot excluing the borders.','Measure the dry weight of stover, haulm harvested after threshing plants harvested from the plot excluing the borders. Then divide the measured harvested weight by the effectively harvested area of the plot','Weigh the amount of harvested grains (excluding the borders) adjusted to 13% moisture,','Weigh the amount of harvested grains (excluding the borders) adjusted to 13% moisture, and then divided by the area of the plot','Weigh 100 seeds'),
      c('tuber/plot-CO_330:0000265','tuber/plot-CO_330:0000268','%-CO_330:0000283','plants/plot-CO_330:0000287','%-CO_330:0000290','tuber/plot-CO_330:0000300','tuber/ plot-CO_330:0000304','tuber /plant-CO_330:0000305','tuber/plot-CO_330:0000293','tuber/plant-CO_330:0000297','kg/plot-CO_330:0000314','kg/plot-CO_330:0000317','kg/plant-CO_330:0000321','t/ha-CO_330:0000324','t/ha-CO_330:0000323','kg/plot-CO_330:0000308','kg/plant-CO_330:0000311','t/ha-CO_330:0000330','t/ha-CO_330:0000327','g-CO_330:0000333','g-CO_330:0000336','ratio-CO_334:0000008','7 pt scale-CO_334:0000009','Plant-CO_334:0000010','Count-CO_334:0000011','kg/plot-CO_334:0000012','t/ha-CO_334:0000013','t/ha-CO_334:0000014','t/ha-CO_334:0000017','kg/pl-CO_334:0000127','Stem-CO_334:0000129','kg/plot-CO_334:0000131','kg/plot-CO_334:0000132','Number-CO_334:0000133','kg/pl-CO_334:0000157','kg/plot-CO_334:0000158','Number-CO_334:0000159','Seedling-CO_334:0000166','plot-CO_334:0000168','plot-CO_334:0000169','kg-CO_334:0000170','kg-CO_334:0000171','1 month-CO_334:0000213,3 months-CO_334:0000214,6 months-CO_334:0000215,9 months-CO_334:0000216','kg/plant-CO_334:0000230','t/ha-CO_334:0000231','kg-CO_334:0000247','kg-CO_334:0000248','Count-CO_334:0000250','m2/kg-CO_321:0001034,kg/ha-CO_321:0001035,t/ha-CO_321:0001036,g/plant-CO_321:0001037,g/plot-CO_321:0001038,kg/plot-CO_321:0001039','g/1000 grain-CO_321:0001213,g/100 grain-CO_321:0001214,g/200 grain-CO_321:0001215','g/m2-CO_321:0001217,kg/ha-CO_321:0001218,t/ha-CO_321:0001219','g/plant-CO_321:0001220,g/plot-CO_321:0001221,kg/plot-CO_321:0001222','%-CO_321:0001223','num-CO_321:0001224','index-CO_321:0001231,%-CO_321:0001232','m2/kg-CO_321:0001246,kg/ha-CO_321:0001247,t/ha-CO_321:0001248,g/plot-CO_321:0001249,kg/plot-CO_321:0001250','1-5 scoring scale-CO_321:0001651','g/1000grain-CO_322:0000723,g/100grain-CO_322:0000725,g/200grain-CO_322:0000727','kg/ha-CO_322:0000730,t/ha-CO_322:0000731','g/plot-CO_322:0000734,kg/plot-CO_322:0000740,t/ha-CO_322:0000742,kg/ha-CO_322:0000737','g/plot-CO_322:0000744,kg/plot-CO_322:0000749,kg/ha-CO_322:0000747,t/ha-CO_322:0000751','%-CO_322:0000757','Rank number-CO_322:0000754','%-CO_322:0000756','g/plot-CO_322:0000928,kg/plot-CO_322:0000931','lb/bsh-CO_322:0001008','g/1000grain-CO_322:0001009,g/100grain-CO_322:0001010,g/200grain-CO_322:0001011','g/200grain-CO_322:0001012,g/1000grain-CO_322:0001013,g/100grain-CO_322:0001014','lb/plot-CO_322:0001016','plants/plot-CO_331:0000192','plants/plot-CO_331:0000678','plants/plot-CO_331:0000679','plants/plot-CO_331:0000211','roots/plot-CO_331:0000214','roots/plot-CO_331:0000217','roots/ plot-CO_331:0000233','roots/ plant-CO_331:0000230','kg/plot-CO_331:0000220','kg/plot-CO_331:0000223','kg/plot-CO_331:0000227','kg/plot-CO_331:0000237','t/ha-CO_331:0000218','t/ha-CO_331:0000680','t/ha-CO_331:0000681','t/ha-CO_331:0000296','%-CO_331:0000682','t/ha-CO_331:0000683','RtYldR 5 pt. scale-CO_331:0000791','%-CO_331:0000792','g/plot-CO_336:0000262','kg/ha-CO_336:0000340','g/plot-CO_336:0000261','kg/ha-CO_336:0000337','g-CO_336:0000333'),
      c('CO_330:0000265','CO_330:0000268','CO_330:0000283','CO_330:0000287','CO_330:0000290','CO_330:0000300','CO_330:0000304','CO_330:0000305','CO_330:0000293','CO_330:0000297','CO_330:0000314','CO_330:0000317','CO_330:0000321','CO_330:0000324','CO_330:0000323','CO_330:0000308','CO_330:0000311','CO_330:0000330','CO_330:0000327','CO_330:0000333','CO_330:0000336','CO_334:0000008','CO_334:0000009','CO_334:0000010','CO_334:0000011','CO_334:0000012','CO_334:0000013','CO_334:0000014','CO_334:0000017','CO_334:0000127','CO_334:0000129','CO_334:0000131','CO_334:0000132','CO_334:0000133','CO_334:0000157','CO_334:0000158','CO_334:0000159','CO_334:0000166','CO_334:0000168','CO_334:0000169','CO_334:0000170','CO_334:0000171','CO_334:0000213','CO_334:0000230','CO_334:0000231','CO_334:0000247','CO_334:0000248','CO_334:0000250','CO_321:0001034','CO_321:0001213','CO_321:0001217','CO_321:0001220','CO_321:0001223','CO_321:0001224','CO_321:0001231','CO_321:0001246','CO_321:0001651','CO_322:0000723','CO_322:0000730','CO_322:0000734','CO_322:0000744','CO_322:0000757','CO_322:0000754','CO_322:0000756','CO_322:0000928','CO_322:0001008','CO_322:0001009','CO_322:0001012','CO_322:0001016','CO_331:0000192','CO_331:0000678','CO_331:0000679','CO_331:0000211','CO_331:0000214','CO_331:0000217','CO_331:0000233','CO_331:0000230','CO_331:0000220','CO_331:0000223','CO_331:0000227','CO_331:0000237','CO_331:0000218','CO_331:0000680','CO_331:0000681','CO_331:0000296','CO_331:0000682','CO_331:0000683','CO_331:0000791','CO_331:0000792','CO_336:0000262','CO_336:0000340','CO_336:0000261','CO_336:0000337','CO_336:0000333'),
      c('tuber/plot','tuber/plot','%','plants/plot','%','tuber/plot','tuber/ plot','tuber /plant','tuber/plot','tuber/plant','kg/plot','kg/plot','kg/plant','t/ha','t/ha','kg/plot','kg/plant','t/ha','t/ha','g','g','ratio','7 pt scale','Plant','Count','kg/plot','t/ha','t/ha','t/ha','kg/pl','Stem','kg/plot','kg/plot','Number','kg/pl','kg/plot','Number','Seedling','plot','plot','kg','kg','1 month','kg/plant','t/ha','kg','kg','Count','m2/kg','g/1000 grain','g/m2','g/plant','%','num','index','m2/kg','1-5 scoring scale','g/1000grain','kg/ha','g/plot','g/plot','%','Rank number','%','g/plot','lb/bsh','g/1000grain','g/200grain','lb/plot','plants/plot','plants/plot','plants/plot','plants/plot','roots/plot','roots/plot','roots/ plot','roots/ plant','kg/plot','kg/plot','kg/plot','kg/plot','t/ha','t/ha','t/ha','t/ha','%','t/ha','RtYldR 5 pt. scale','%','g/plot','kg/ha','g/plot','kg/ha','g')


  )
  colnames(dict) <- c("Status","Crop", "Crop measurement", "Measurement method", "traitCode", "VariableId", "Scale")

  observe({
    if(!is.null(input$cropCommonNameMono)){
      traitsVals$selectedRows <- list()
      aux <- dplyr::filter(as.data.frame(dict),Crop==input$cropCommonNameMono[1])
      traitsVals$Data<-data.table(aux)
      output$uiTraitsList <- renderUI({
          #column(width=12,
              column(12,dataTableOutput("Main_table"),
                    #  tags$script(HTML("$(document).on('change', '#select_scale', function () {
                    #    var mod_value
                    #    mod_value =  $('.new_input' ).val()
                    #    Shiny.onInputChange('scaleChange', mod_value)
                    # });")),
                  #    tags$script("$(document).on('click', '#Main_table button', function () {
                  # Shiny.onInputChange('selectScaleClickId',this.id);
                  # Shiny.onInputChange('selectScaleClick', Math.random())
                  #     });"
                  #    )

                  # tags$script("$(document).on('click', '.selectRow', function () {
                  #     Shiny.onInputChange('selectRowClickId',this.id);
                  #     Shiny.onInputChange('selectRowClick', Math.random())
                  #     });"
                  # ),
                  tags$script("$(document).on('change', '.selectRow', function () {
                      Shiny.onInputChange('selectRowClickId',this.id);
                      Shiny.onInputChange('selectRowClickChecked',this.checked);
                      Shiny.onInputChange('selectRowClick', Math.random())
                      });"
                  ),
                  tags$script("$(document).on('change', '.select_scale', function () {
                          Shiny.onInputChange('selectScaleClickId',this.id);
                          Shiny.onInputChange('selectScaleClickValue',this.value);
                          Shiny.onInputChange('selectScaleClick', Math.random())
                      });"
                  )

                     )#,
#
#               tags$script("$(document).on('click', '#Main_table button', function () {
#                   Shiny.onInputChange('selectScaleClickId',this.id);
#                   Shiny.onInputChange('selectScaleClick', Math.random())
#                       });"
#               )

          #)

      })


    }
    else{
      traitsVals$Data <- data.table()
      traitsVals$selectedRows <- list()
      output$uiTraitsList <- renderUI({
        column(width = 10,

        h4("Select crop to show list of traits.")
        )

      })

    }

  })#end observe


  output$Main_table <-renderDataTable({
    # as.list(lapply(1:nrow(traitsVals$Data), "drawComboInTable"))

    DT= traitsVals$Data
    # DT[["Change scale"]] <- as.list(lapply(1:nrow(traitsVals$Data), "drawComboInTable"))
    DT[["Change scale"]] <- drawComboInTable()

    # DT[["Select variable"]] <- lapply(1:nrow(traitsVals$Data), "drawButtonSelect")
    DT[["Select variable"]] <- drawButtonSelect()
    DT[["Variable ID"]] <- traitsVals$Data[,6]
    datatable(DT,
              escape=F,
              # selection = list(mode = 'multiple', selected = traitsVals$selectedRows),
              selection = list(mode = 'none'),
              options = list(
                scrollX = TRUE,
                pageLength = 25,
                columnDefs = list(list(visible=FALSE, targets=c(1,5,6)),list(width = '30%', targets = c(1)), list(className = 'dt-center', targets = c(7,8)))
              )
    )}
  )

  # observeEvent(input$selectRowClick, {
  #   selectedRow  <- as.numeric(gsub("selectRow_","",input$selectRowClickId))
  #   row <- traitsVals$Data[selectedRow,]
  #   if(row[[1]] %like% "Not selected"){
  #     traitsVals$Data[[1]][selectedRow] <- "<font color='red'><b>Selected</b></font>"
  #   }
  #   else{
  #     traitsVals$Data[[1]][selectedRow] <- "<font color='black'>Not selected</font>"
  #
  #   }
  # })

  observeEvent(input$selectRowClick, {
    selectedRow  <- as.numeric(gsub("selectRow_","",input$selectRowClickId))
    row <- traitsVals$Data[selectedRow,]
    if(input$selectRowClickChecked){
      # traitsVals$Data[[1]][selectedRow] <- "<font color='red'><b>Selected</b></font>"
      traitsVals$Data[[1]][selectedRow] <- "Selected"
    }
    else{
      # traitsVals$Data[[1]][selectedRow] <- "<font color='black'>Not selected</font>"
      traitsVals$Data[[1]][selectedRow] <- "Not selected"

    }

  })

  observeEvent(input$selectScaleClick,{
    vv  <- strsplit(input$selectScaleClickValue, "-")[[1]]
    var <- list()
    if(length(vv) == 1){
      var[[1]] = vv
      var[[2]] = ""
    }
    else{
      var <- vv
    }

    traitsVals$Data[[6]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[1]]
    traitsVals$Data[[7]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[2]]
  })

  # drawButtonSelect <- function(index){
  #   old_row <- traitsVals$Data[index,]
  #   # str <-  paste0('<div class="btn-group" role="group" aria-label="Basic example">
  #   #   <button style="width:100px; background-color:green; color:white;" type="button" class="btn btn-secondary selectRow" id=selectRow_',index ,'>Select</button>
  #   #   </div>')
  #
  #   ckecked <-  ""
  #   # str <-  paste0('<button style="width:100px; background-color:green; color:white;" type="button" class="btn btn-secondary selectRow" id=selectRow_',index ,'>Select</button>')
  #   if(old_row[[1]] %like% "Selected"){
  #     # str <- gsub("green", "red", str)
  #     # str <- gsub("Select", "Deselect", str)
  #     ckecked <- "checked"
  #   }
  #
  #
  #   str <-  paste0('<div class="btn-group" role="group" aria-label="Basic example">
  #     <input style="width:100px; background-color:green; color:white;" type="checkbox" class="selectRow"  id=selectRow_',index ,' ',ckecked,  '></input>
  #     </div>')
  #
  #
  #
  #   return(str)
  #
  # }

  drawButtonSelect <- function(){
    n<- nrow(traitsVals$Data)
    l <- c()
    for(index in 1:n){

      old_row <- traitsVals$Data[index,]

      ckecked <-  ""
      if(old_row[[1]] %like% "Selected"){
        ckecked <- "checked"
      }

      str <-  paste0('<div class="btn-group" role="group" aria-label="Basic example">
        <input style="width:100px; background-color:green; color:white;" type="checkbox" class="selectRow"  id=selectRow_',index ,' ',ckecked,  '></input>
        </div>')
      l<- c(l,str)
    }
    return(l)
  }

  # drawComboInTable <- function(index){
  #   old_row = traitsVals$Data[index,]
  #   options = old_row[[4]]
  #
  #   row_change=list()
  #
  #   str  <- paste0('<select id="select_scale_' , index, '" class="select_scale" style="width:150px;">')
  #   arrOpt <- strsplit(options, ",")[[1]]
  #
  #   if(length(arrOpt) == 1){
  #     mval1  <- strsplit(arrOpt[1], "-")[[1]]
  #     if(mval1[[2]] == " ")
  #       return(" ")
  #   }
  #
  #   for(val in arrOpt){
  #
  #     mval  <- strsplit(val, "-")[[1]]
  #     # print("aaaa")
  #     # print(mval)
  #     if(mval[[2]] == old_row[[6]]) sel <- "selected" else sel <-""
  #     str <- paste0(str, '<option value="', mval[[1]], "-" , mval[[2]], '" ', sel,'> ', mval[[2]], '</option>')
  #   }
  #   str <- paste0(str, "</select>")
  #
  #   return(str)
  # }

  drawComboInTable <- function(){
    n<- nrow(traitsVals$Data)
    l <- c()
    for(index in 1:n){
      old_row = traitsVals$Data[index,]
      options = old_row[[5]]
      str  <- paste0('<select id="select_scale_' , index, '" class="select_scale" style="width:150px;">')
      arrOpt <- strsplit(options, ",")[[1]]

      if(length(arrOpt) == 1){
        str <- ""
      }
      else{
        for(val in arrOpt){
          mval  <- strsplit(val, "-")[[1]]
          # if(mval[[2]] == old_row[[6]]) sel <- "selected" else sel <-""
          #
          # str <- paste0(str, '<option value="', mval[[1]], "-" , mval[[2]], '" ', sel,'> ', mval[[2]], '</option>')
          if(mval[[1]] == old_row[[7]]) sel <- "selected" else sel <-""

          str <- paste0(str, '<option value="', mval[[2]], "-" , mval[[1]], '" ', sel,'> ', mval[[1]], '</option>')
        }
        str <- paste0(str, "</select>")
      }
      l <- c(l, str)
    }
    return(l)
  }

  ############ end traits table #############################################################


  # # Reactive table. Get material list table #################################################################
  # material_table <-  shiny::reactive({
  #
  #   if(input$select_import=="Template") {
  #
  #     #mtl_temp <- input$file
  #     mtl_temp <- input$file_mtlist
  #
  #     if(is.null(mtl_temp)){return()}
  #     if(!is.null(mtl_temp)){
  #
  #       file.copy(mtl_temp$datapath,paste(mtl_temp$datapath, ".xlsx", sep=""))
  #       mtl_temp <- readxl::read_excel(paste(mtl_temp$datapath, ".xlsx", sep=""), sheet = "Material_List")
  #
  #       mtl_list <- as.list(mtl_temp) #mtl in list format
  #     }
  #
  #
  #   }
  #
  #   if(input$select_import=="Local List"){
  #
  #     sel_list <- input$designFieldbook_sel_mlist
  #     #print(sel_list)
  #     if(is.null(sel_list) || sel_list == ""){  return()  }
  #     if(length(sel_list)>0){
  #
  #       #Just use the original code
  #       mtl_temp <- readRDS(sel_list)
  #
  #
  #       #is_parent_list <- is_parentList(sel_list)
  #       if(is_parentList(sel_list)==TRUE){
  #         #Case: parental list (female and male)
  #         mtl_list <- mtl_temp
  #       }
  #       else{
  #         #Case: standard material list (genotypes)
  #         mtl_list <- as.list(mtl_temp) #mtl in list format
  #       }
  #
  #     }
  #
  #   }
  #
  #   mtl_list
  #
  # })
  #
  # # Approval Box ######################################################################################################
  # output$approvalBox <- renderInfoBox({
  #
  #   #data.frame is the data structue for the clonal and family list. In the parental and family module, we save lists in data.frame format
  #
  #   # plos <<- material_table()
  #   #
  #   # lsus <<-  get_type_list_ds(material_table())
  #
  #   # plos <<- material_table()
  #   #
  #   # lsus <<-  get_type_list_ds(material_table())
  #
  #
  #
  #   # if( is.null(material_table()) ){
  #   #
  #   #   title <- "Upload"
  #   #   subtitle <-   paste("your material list file. Or, press the button below to download and fill the template.")
  #   #   color <- "blue"
  #   #   icon <- "upload"
  #   #   lib <- "glyphicon"
  #   #   fill <- TRUE
  #   #   width <- NULL
  #   #
  #   #   # infoBox(title="Upload", subtitle=
  #   #   #           paste("your material list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
  #   #   #         color = "blue",fill = TRUE, width = NULL)
  #   #
  #   # }
  #
  #   #parent list
  #   if( get_type_list_ds(material_table()) == "clonal" ) {
  #
  #     #germoplasm <- germoplasm$Accesssion_Number
  #
  #     germoplasm <-material_table()$Accession_Number
  #     #detect duplications
  #     germ_duplicates <- anyDuplicated(germoplasm)
  #
  #
  #     if(is.null(germoplasm)){
  #
  #       title <- "Upload"
  #       subtitle <-   paste("your material list file. Or, press the button below to download and fill the template.")
  #       color <- "blue"
  #       icon <- "upload"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #       # infoBox(title="Upload", subtitle=
  #       #           paste("your material list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
  #       #         color = "blue",fill = TRUE, width = NULL)
  #
  #
  #     }
  #     else if(all(is.na(germoplasm))) {
  #
  #       #if(all(is.na(germoplasm))) {
  #       title <- "ERROR"
  #       subtitle <- paste("Your material list", "is empty. Please check it")
  #       color <- "red"
  #       icon <- "warning-sign"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #       # infoBox(title="ERROR", subtitle=
  #       #           paste("Your material list", "is empty. Please check it"), icon = icon("warning-sign", lib = "glyphicon"),
  #       #         color = "red",fill = TRUE, width = NULL)
  #
  #
  #     }
  #     else if(germ_duplicates>0){
  #       title <- "ERROR"
  #       subtitle <- paste("Your material list has duplicated genotypes/germoplasm names. Please, enter a correct file.")
  #       color <- "red"
  #       icon <- "warning-sign"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #
  #     }  else {
  #
  #       title <- "GREAT!"
  #       subtitle <-  paste(" was successfully uploaded!")
  #       color <- "green"
  #       icon <- "ok"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #       # infoBox(title="GREAT!", subtitle =
  #       #           paste(" was successfully uploaded!"),  icon = icon("ok", lib = "glyphicon"),
  #       #         color = "green",fill = TRUE, width = NULL)
  #     }
  #
  #
  #   }
  #
  #   #list is the data structure for parental list. In the parental module, we save lists in list format
  #   if( get_type_list_ds(material_table()) == "parental" ) {
  #
  #     germoplasm_fem <-  material_table()$female$Accession_Number
  #     germoplasm_male <- material_table()$male$Accession_Number
  #
  #     if(is.null(germoplasm_fem) && is.null(germoplasm_male)){
  #
  #
  #       title <- "Upload"
  #       subtitle <-  paste(" your parental list file. Or, press the button below to download and fill the template.")
  #       color <- "blue"
  #       icon <- "upload"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #       # infoBox(title="Upload", subtitle=
  #       #           paste("your parental list file. Or, press the button below to download and fill the template."), icon = icon("upload", lib = "glyphicon"),
  #       #           color = "blue",fill = TRUE, width = NULL)
  #
  #     }
  #
  #     else if(all(is.na(germoplasm_fem))) {
  #
  #       #if(all(is.na(germoplasm_fem))) {
  #
  #       title <- "ERROR"
  #       subtitle <-  paste("The female's accession numbers are empty.", "Please check female's accesion number column")
  #       color <- "red"
  #       icon <- "warning-sign"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #       # infoBox(title="ERROR", subtitle=
  #       #           paste("The female's accession numbers are empty.", "Please check female's accesion number column"), icon = icon("warning-sign", lib = "glyphicon"),
  #       #           color = "red",fill = TRUE, width = NULL)
  #
  #     }
  #
  #     else if(all(is.na(germoplasm_male))) {
  #
  #       title <- "ERROR"
  #       subtitle <-  paste("The male's accession numbers are empty")
  #       color <- "red"
  #       icon <- "warning-sign"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #       # infoBox(title="ERROR", subtitle=
  #       #           paste("The male's accession numbers are empty.", "Please check male's accesion number column"), icon = icon("warning-sign", lib = "glyphicon"),
  #       #           color = "red",fill = TRUE, width = NULL)
  #
  #     }
  #
  #     else {
  #
  #       title <- "GREAT!"
  #       subtitle <-  paste(" your parental list file was successfully uploaded!")
  #       color <- "green"
  #       icon <- "ok"
  #       lib <- "glyphicon"
  #       fill <- TRUE
  #       width <- NULL
  #
  #     }
  #
  #   }
  #
  #
  #   shinydashboard::infoBox(title=title, subtitle =subtitle,  icon = icon(icon, lib = lib),
  #                           color = color, fill = TRUE, width = NULL)
  #
  #
  # }) #################################################################

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


  #output$designFieldbook_traits_agrofims <- shinyTree::renderTree({
  trait_agrofims <- reactive({

    trait_index <-  input$Main_table_rows_selected  #data from fbdesign for hidap-agrofims
    if(is.null(trait_index)){
      trait_selected <- data.table::data.table()
    } else {
      dt <- traitsVals$Data
      trait_selected <- dt[trait_index, ]
    }
    trait_selected
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


  ## Weather ShinyTree  #################################################################

  output$designFieldbook_weatherVar_agrofims <- shinyTree::renderTree({

    a<- weather_list #data from fbdesign for hidap-agrofims
    a
  })

  ## Soil ShinyTree #################################################################

  output$designFieldbook_soilVar_agrofims <- shinyTree::renderTree({

    a<- soil_list_prov #data from fbdesign for hidap-agrofims
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
    #geodb_file <- "table_sites.rds"
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    # values$sites_data <-  readRDS(file = path)
    x_sites_data <- readRDS(file = path)
    values$sites_data <-  dplyr::filter(x_sites_data, userId==0)

  })

  observe({
    if(pkg.globals$userSession$logged == T){
        print("Logged")
    }
    else if(pkg.globals$userSession$logged == F){
      print("Not logged")
    }
  })

  observeEvent(input$refreshSiteList,{
    x_user <- fbdesign::getUserSession()
    path <- fbglobal::get_base_dir()
    #geodb_file <- "table_sites.rds"
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    x_sites_data <- readRDS(file = path)
    if(x_user$logged){
      values$sites_data <- dplyr::filter(x_sites_data, userId==x_user$id)
    }
    else{
      values$sites_data <-  dplyr::filter(x_sites_data, userId==0)
      # values$sites_data <- readRDS(file = path)
    }


  })



  # Country ###################################################################################
  output$fbDesign_country <- shiny::renderUI({
    #sites_data <- fbsites::get_site_table() #before
    # sites_data <- site_table #data from package fbdesign as an internal data BEFORE

    # if(USER$Logged == FALSE){
    #   print("Asdaadas")
    # }

    sites_data <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    cntry <- fbsites::get_country_list(sites_data = sites_data) #new code: use file fbsites


    shiny::selectizeInput("fbDesign_countryTrial", label = "Country name",
                          choices = cntry , selected = 1,  multiple = FALSE)

  })

  # Sites ##################################################################################################
  fbdesign_sites <- reactive({

    #sites_data <- site_table #using data from package #Former code before useing rective values

    sites_data <- values$sites_data

    fbsites::get_filter_locality_agrofims(sites_data = sites_data, country_input= input$fbDesign_countryTrial)
  })

  # Country_site_select #####################################################################################
  output$fbDesign_countrySite <- shiny::renderUI({

    req(input$fbDesign_countryTrial)

    #locs <- site_table #using data from package fbsite (OLD CODE)
    locs <- values$sites_data # read trial sites using reactive values from xdata folder (NEW CODE)

    if(nrow(locs) == 0){
      fbdesign_sites_selected = c()

    }else{
      fbdesign_sites_selected <- fbdesign_sites()
    }


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

  #reactive value to show BookPreview/draft fieldbook table
  output$show_agrotable <- reactive({
    p <- input$fbDesign_draft_agrofims[1]
    if(p==0){
      k <- FALSE
    }else{
      k<-TRUE
    }
    return(k)
   # return(!is.null( (fb_agrofims()) ))
  })
  #
  #
  # #set options for show_mtable
   outputOptions(output, 'show_agrotable', suspendWhenHidden=FALSE)




  # Book preview #############################################################
  shiny::observeEvent(input$fbDesign_draft_agrofims, {

    # print(f1())
    # ben <<- f1()

    withProgress(message = 'Fieldbook Preview', value = 0, {

      incProgress(1/10,message = "...")

    flag <- TRUE #temporary

    if(flag){

      #print(agro_var_dt())
      fb <- fb_agrofims_traits()

      #fb <- fb[,1:129]
      output$fbDesign_table_agrofims <- rhandsontable::renderRHandsontable({

        rhandsontable::rhandsontable(fb , readOnly = T)})

    }

      incProgress(9/10,message = "...")
      incProgress(10/10,message = "...")

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

   # Reactive Factor and Levels #############################################################

   ### Factor 1  ############################################################
   factor1StartDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_1
     fl <- list()

     sel1_1 <-	input$sel1_1 #group
     sel1_2<-	input$sel1_2 #subgroup
     sel1_3<-	input$sel1_3 #factor
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 1, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor1EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 1, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor1NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 1)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor1TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 1)]] #i = order
     fl <- unlist(fl)

   })
   factor1TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 1)]] #i = order
     fl <- unlist(fl)

   })
   factor1ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 1)]] #i = order
     fl <- unlist(fl)

   })

   f1 <- reactive({

     out <- list(factor1StartDateInputs(),  factor1EndDateInputs(),  factor1NumericInputLevel(),  factor1TextInputLevel(),
                 factor1TextInputUnits(), factor1ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out
   })
   ##########################################################################

   ### Factor 2  ############################################################
   factor2StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_2
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 2, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor2EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_2
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 2, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor2NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 2)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor2TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 2)]] #i = order
     fl <- unlist(fl)

   })
   factor2TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 2)]] #i = order
     fl <- unlist(fl)

   })
   factor2ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 2)]] #i = order
     fl <- unlist(fl)

   })

   f2 <- reactive({

     out <- list(factor2StartDateInputs(),  factor2EndDateInputs(),  factor2NumericInputLevel(),  factor2TextInputLevel(),
                 factor2TextInputUnits(), factor2ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out
   })
   ##########################################################################

   ### Factor 3  ############################################################
   factor3StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_3
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 3, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor3EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_3
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 3, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor3NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 3)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor3TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 3)]] #i = order
     fl <- unlist(fl)

   })
   factor3TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 3)]] #i = order
     fl <- unlist(fl)

   })
   factor3ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 3)]] #i = order
     fl <- unlist(fl)

   })

   f3 <- reactive({

     out <- list(factor3StartDateInputs(),  factor3EndDateInputs(),  factor3NumericInputLevel(),  factor3TextInputLevel(),
                 factor3TextInputUnits(), factor3ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out

   })
   ##########################################################################

   ### Factor 4  ############################################################
   factor4StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_4
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 4, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor4EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_4
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 4, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor4NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 4)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor4TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 4)]] #i = order
     fl <- unlist(fl)

   })
   factor4TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 4)]] #i = order
     fl <- unlist(fl)

   })
   factor4ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 4)]] #i = order
     fl <- unlist(fl)

   })

   f4 <- reactive({

     out <- list(factor4StartDateInputs(),  factor4EndDateInputs(),  factor4NumericInputLevel(),  factor4TextInputLevel(),
                 factor4TextInputUnits(), factor4ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out

   })
   ##########################################################################


   ### Factor 5  ############################################################
   factor5StartDateInputs  <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_5
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_start_date_", 5, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor5EndDateInputs <- reactive({

     nfactors<- input$nfactors_hdafims
     nlevels <- input$numLevels_5
     fl <- list()
     #print(nlevels)
     #print(p2)
     for(i in 1:nlevels){
       fl[[i]] <-  paste(input[[paste0("factor_end_date_", 5, "_", i)]])
     }
     fl <- unlist(fl)
   })
   factor5NumericInputLevel <- reactive({
     #nfactors<- input$nfactors_hdafims
     #nlevels <- input$numLevels_1
     fl <- list()
     #print(nlevels)
     #print(p2)
     #for(i in 1:nfactors){
     fl[[1]] <-  input[[paste0("levels_", 5)]] #i = order
     #}
     fl <- unlist(fl)
   })
   factor5TextInputLevel <- reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 5)]] #i = order
     fl <- unlist(fl)

   })
   factor5TextInputUnits <-reactive({

     fl <- list()
     fl[[1]] <-  input[[paste0("units_", 5)]] #i = order
     fl <- unlist(fl)

   })
   factor5ComboboxLevel <- reactive({
     fl <- list()
     fl[[1]] <-  input[[paste0("levels_", 5)]] #i = order
     fl <- unlist(fl)

   })

   f5 <- reactive({

     out <- list(factor5StartDateInputs(),  factor5EndDateInputs(),  factor5NumericInputLevel(),  factor5TextInputLevel(),
                 factor5TextInputUnits(), factor5ComboboxLevel())
     names(out) <- c("Start date", "End date", "numeric", "text", "units", "combo")
     out

   })
   ##########################################################################




  ###fieldbook design ################################################

  fb_agrofims <- shiny::reactive({

    nrep <- as.numeric(input$designFieldbook_agrofims_r)
    design <- input$designFieldbook_agrofims

    nfactors <- as.numeric(input$nfactors_hdafims)

    if(design == "CRD") { design<- "crd"}
    if(design == "RCBD") { design<- "rcbd"}
    design <- design

    #checkbox to set CropVariety as a Factor
    isCropFactor <- input$setCropFactor
    crop_varietiesname <- input$cropVarietyNameMono

    if(is.null(crop_varietiesname)) crop_varietiesname <- ""


    if(nfactors == 1){
      # factor1
        sel1_1 <-	input$sel1_1 #group
        sel1_2<-	input$sel1_2 #subgroup
        sel1_3<-	input$sel1_3 #factor
        factorName1 <- paste(sel1_2, sel1_3, sep = "")

        F1 <- f1()
        if(sel1_3 == "Start date" ){
          lev1 <- F1[[sel1_3]]
        } else if( sel1_3 == "End date"){
          lev1 <- F1[[sel1_3]]
        } else{
          lev1<- F1[["text"]]
        }
        #To Do Units combos

     if(design == "crd"){

              fb <- st4gi::cd.cr(geno = lev1, nrep = nrep,  nc = 3)
              fb <- fb$book
              names(fb) <-  c("PLOT", "ROW", "COL", factorName1)

              if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){
                fb <- cd.factorial(A = lev1, B =crop_varietiesname , design = design, nrep = nrep,  nc = 3)
                fb <- fb$book
                names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT", factorName1, "VARIETIES")
              }
              fb

          }
     if(design == "rcbd"){
              fb <- st4gi::cd.rcb(geno = lev1, nb = nrep, nc = 3)
              fb <- fb$book
              names(fb) <-  c("PLOT", "BLOCK", "ROW", "COL", factorName1)

              if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){
                fb <- cd.factorial(A = lev1, B = crop_varietiesname , design = design, nrep = nrep,  nc = 3)
                fb <- fb$book
                names(fb) <-  c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factorName1, "VARIETIES")
              }
              fb
          }

    }

    if(nfactors == 2){
      # factor 1
      sel1_1 <-	input$sel1_1 #group
      sel1_2<-	input$sel1_2 #subgroup
      sel1_3<-	input$sel1_3 #factor
      factorName1 <- paste(sel1_2, sel1_3, sep = "")

      F1 <- f1()
      if(sel1_3 == "Start date" ){
        lev1 <- F1[[sel1_3]]
      } else if( sel1_3 == "End date"){
        lev1 <- F1[[sel1_3]]
      } else{
        lev1<- F1[["text"]]
      }

      # factor2
      sel2_1<-	input$sel2_1 #group
      sel2_2<-	input$sel2_2
      sel2_3<-	input$sel2_3
      factorName2 <- paste(sel2_2, sel2_3, sep = "") #factor 2 name
      print(factorName2)

      F2 <- f2()
      if(sel2_3 == "Start date" ){
        lev2 <- F1[[sel2_3]]
      } else if( sel2_3 == "End date"){
        lev2 <- F2[[sel2_3]]
      } else{
        lev2<- F2[["text"]]
      }

      if(design == "crd"){
        fb <- cd.factorial(A = lev1, B = lev2, design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT", factorName1, factorName2)


        if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){
          fb <- cd.factorial(A = lev1, B = lev2, C = crop_varietiesname , design = design, nrep = nrep,  nc = 3)
          fb <- fb$book
          names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT", factorName1, factorName2, "VARIETIES")
        }
        fb


      }
      if(design == "rcbd"){
        fb <- cd.factorial(A = lev1, B = lev2, design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factorName1, factorName2)

        if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){
          fb <- cd.factorial(A = lev1, B = lev2, C = crop_varietiesname , design = design, nrep = nrep,  nc = 3)
          fb <- fb$book
          names(fb) <-  c("PLOT","BLOCK" , "ROW", "COL", "TREATMENT",factorName1, factorName2, "VARIETIES")
        }
        fb



      }
    }

    if(nfactors == 3){
      # factor 1
      sel1_1 <-	input$sel1_1 #group
      sel1_2<-	input$sel1_2 #subgroup
      sel1_3<-	input$sel1_3 #factor
      factorName1 <- paste(sel1_2, sel1_3, sep = "")

      F1 <- f1()
      if(sel1_3 == "Start date" ){
        lev1 <- F1[[sel1_3]]
      } else if( sel1_3 == "End date"){
        lev1 <- F1[[sel1_3]]
      } else{
        lev1<- F1[["text"]]
      }
      #To Do Units combos
      # factor 2
      sel2_1<-	input$sel2_1 #group
      sel2_2<-	input$sel2_2
      sel2_3<-	input$sel2_3
      factorName2 <- paste(sel2_2, sel2_3, sep = "") #factor 2 name

      F2 <- f2()
      if(sel2_3 == "Start date" ){
        lev2 <- F1[[sel2_3]]
      } else if( sel2_3 == "End date"){
        lev2 <- F2[[sel2_3]]
      } else{
        lev2<- F2[["text"]]
      }
      # factor 2
      sel2_1<-	input$sel2_1 #group
      sel2_2<-	input$sel2_2
      sel2_3<-	input$sel2_3
      factorName2 <- paste(sel2_2, sel2_3, sep = "") #factor 2 name

      F2 <- f2()
      if(sel2_3 == "Start date" ){
        lev2 <- F1[[sel2_3]]
      } else if( sel2_3 == "End date"){
        lev2 <- F2[[sel2_3]]
      } else{
        lev2<- F2[["text"]]
      }
      # factor 3
      sel3_1 <-	input$sel3_1 #group
      sel3_2 <-	input$sel3_2
      sel3_3 <-	input$sel3_3
      factorName3 <- paste(sel3_2, sel3_3, sep = "")

      F3 <- f3()
      if(sel3_3 == "Start date" ){
        lev3 <- F3[[sel3_3]]
      } else if( sel3_3 == "End date"){
        lev3 <- F3[[sel3_3]]
      } else{
        lev3 <- F3[["text"]]
      }


      if(design == "crd"){
        fb <- cd.factorial(A = lev1, B = lev2, C = lev3,  design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT",factorName1, factorName2,factorName3)

        if( isCropFactor == TRUE  && length(crop_varietiesname)>=2 ){
          fb <- cd.factorial(A = lev1, B = lev2, C= lev3 ,D = crop_varietiesname , design = design, nrep = nrep,  nc = 3)
          fb <- fb$book
          names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT",factorName1, factorName2, factorName3, "VARIETIES")
        }
        fb



        #To Do rename headers
      }
      if(design == "rcbd"){
        fb <- cd.factorial(A = lev1, B = lev2, C =  lev3, design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT","BLOCK", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3)
        #To Do rename headers

        if( isCropFactor == TRUE  && length(crop_varietiesname)>=2 ){
          fb <- cd.factorial(A = lev1, B = lev2, C= lev3 ,D = crop_varietiesname , design = design, nrep = nrep,  nc = 3)
          fb <- fb$book
          names(fb) <-  c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3, "VARIETIES")
        }
        fb


      }
    }

    if(nfactors == 4){
      # factor
      sel1_1 <-	input$sel1_1 #group
      sel1_2<-	input$sel1_2 #subgroup
      sel1_3<-	input$sel1_3 #factor
      factorName1 <- paste(sel1_2, sel1_3, sep = "")

      F1 <- f1()
      if(sel1_3 == "Start date" ){
        lev1 <- F1[[sel1_3]]
      } else if( sel1_3 == "End date"){
        lev1 <- F1[[sel1_3]]
      } else{
        lev1<- F1[["text"]]
      }
      #To Do Units combos
      # factor
      sel2_1<-	input$sel2_1 #group
      sel2_2<-	input$sel2_2
      sel2_3<-	input$sel2_3
      factorName2 <- paste(sel2_2, sel2_3, sep = "") #factor 2 name
      # factor
      F2 <- f2()
      if(sel2_3 == "Start date" ){
        lev2 <- F1[[sel2_3]]
      } else if( sel2_3 == "End date"){
        lev2 <- F2[[sel2_3]]
      } else{
        lev2<- F2[["text"]]
      }

      sel2_1<-	input$sel2_1 #group
      sel2_2<-	input$sel2_2
      sel2_3<-	input$sel2_3
      factorName2 <- paste(sel2_2, sel2_3, sep = "") #factor 2 name
      # factor
      F2 <- f2()
      if(sel2_3 == "Start date" ){
        lev2 <- F1[[sel2_3]]
      } else if( sel2_3 == "End date"){
        lev2 <- F2[[sel2_3]]
      } else{
        lev2<- F2[["text"]]
      }
      # factor
      sel3_1 <-	input$sel3_1 #group
      sel3_2 <-	input$sel3_2
      sel3_3 <-	input$sel3_3
      factorName3 <- paste(sel3_2, sel3_3, sep = "")

      F3 <- f3()
      if(sel3_3 == "Start date" ){
        lev3 <- F3[[sel3_3]]
      } else if( sel3_3 == "End date"){
        lev3 <- F3[[sel3_3]]
      } else{
        lev3 <- F3[["text"]]
      }
      # factor
      sel4_1 <-	input$sel4_1 #group
      sel4_2 <-	input$sel4_2
      sel4_3 <-	input$sel4_3
      factorName4 <- paste(sel4_2, sel4_3, sep = "")

      F4 <- f4()
      if(sel4_3 == "Start date" ){
        lev4 <- F4[[sel4_3]]
      } else if( sel3_3 == "End date"){
        lev4 <- F4[[sel4_3]]
      } else{
        lev4 <- F4[["text"]]
      }

      if(design == "crd"){
        fb <- cd.factorial(A = lev1, B = lev2, C = lev3, D = lev4, design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3, factorName4)

        if( isCropFactor == TRUE  && length(crop_varietiesname)>=2 ){
          fb <- cd.factorial(A = lev1, B = lev2, C= lev3 , D = lev4, E = crop_varietiesname , design = design, nrep = nrep,  nc = 3)
          fb <- fb$book
          names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3, factorName4, "VARIETIES")
        }
        fb



        #To Do rename headers
      }
      if(design == "rcbd"){
        fb <- cd.factorial(A = lev1, B = lev2, C =  lev3, D = lev4, design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT","BLOCK", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3, factorName4)
        #To Do rename headers
        if( isCropFactor == TRUE  && length(crop_varietiesname)>=2 ){
          fb <- cd.factorial(A = lev1, B = lev2, C= lev3 , D = lev4, E = crop_varietiesname , design = design, nrep = nrep,  nc = 3)
          fb <- fb$book
          names(fb) <-  c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3, factorName4 ,"VARIETIES")
        }
        fb

      }
    }

    if(nfactors == 5){

      # factor 1
      sel1_1 <-	input$sel1_1 #group
      sel1_2<-	input$sel1_2 #subgroup
      sel1_3<-	input$sel1_3 #factor
      factorName1 <- paste(sel1_2, sel1_3, sep = "")

      F1 <- f1()
      if(sel1_3 == "Start date" ){
        lev1 <- F1[[sel1_3]]
      } else if( sel1_3 == "End date"){
        lev1 <- F1[[sel1_3]]
      } else{
        lev1<- F1[["text"]]
      }
      #To Do Units combos

      # factor 2
      sel2_1<-	input$sel2_1 #group
      sel2_2<-	input$sel2_2
      sel2_3<-	input$sel2_3
      factorName2 <- paste(sel2_2, sel2_3, sep = "") #factor 2 name

      F2 <- f2()
      if(sel2_3 == "Start date" ){
        lev2 <- F1[[sel2_3]]
      } else if( sel2_3 == "End date"){
        lev2 <- F2[[sel2_3]]
      } else{
        lev2<- F2[["text"]]
      }


      # factor 3
      sel3_1 <-	input$sel3_1 #group
      sel3_2 <-	input$sel3_2
      sel3_3 <-	input$sel3_3
      factorName3 <- paste(sel3_2, sel3_3, sep = "")

      F3 <- f3()
      if(sel3_3 == "Start date" ){
        lev3 <- F3[[sel3_3]]
      } else if( sel3_3 == "End date"){
        lev3 <- F3[[sel3_3]]
      } else{
        lev3 <- F3[["text"]]
      }


      # factor 4
      sel4_1 <-	input$sel4_1 #group
      sel4_2 <-	input$sel4_2
      sel4_3 <-	input$sel4_3
      factorName4 <- paste(sel4_2, sel4_3, sep = "")

      F4 <- f4()
      if(sel4_3 == "Start date" ){
        lev4 <- F4[[sel4_3]]
      } else if( sel4_3 == "End date"){
        lev4 <- F4[[sel4_3]]
      } else{
        lev4 <- F4[["text"]]
      }

      # factor 5
      sel5_1 <-	input$sel5_1 #group
      sel5_2 <-	input$sel5_2
      sel5_3 <-	input$sel5_3
      factorName4 <- paste(sel5_2, sel5_3, sep = "")

      F5 <- f5()
      if(sel5_3 == "Start date" ){
        lev5 <- F4[[sel5_3]]
      } else if( sel5_3 == "End date"){
        lev5 <- F4[[sel5_3]]
      } else{
        lev5 <- F5[["text"]]
      }

      if(design == "crd"){
        fb <- cd.factorial(A = lev1, B = lev2, C = lev3, D = lev4, E = lev5, design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3, factorName4 , factorName5)
        #To Do rename headers
      }
      if(design == "rcbd"){
        fb <- cd.factorial(A = lev1, B = lev2, C =  lev3, D = lev4, E = lev5, design = design, nrep = nrep,  nc = 3)
        fb <- fb$book
        names(fb) <-  c("PLOT","BLOCK", "ROW", "COL", "TREATMENT", factorName1, factorName2, factorName3, factorName4, factorName5)
        #To Do rename headers
      }
    }


   #fb

    # end temporal


    #nfactors <- input$nfactors_hdafims
    #numLevels_1 <- input$numLevels_1 # n levels



#     if(nfactors == 1){
#       fb <- st4gi::cd.cr(geno = FA, nrep = nrep,  nc = 3)
#
#     } else if(nfactors == 2){
#
#       FA <-  factor_lvl1
#       FB <-  factor_lvl2
#       #print(FA)
#       #print(FB)
#       #print(design)
#       #print(nrep)
#
#       if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){
#
#         fb <- cd.factorial(A = FA, B = FB, C= crop_varietiesname, design = design, nrep = nrep,  nc = 3)
#
#         if(design == "crd"){
#           names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, "CROP_VARIETY")
#         } else{
#           names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, "CROP_VARIETY")
#         }
#
#
#       } else {
#
#         fb <- cd.factorial(A = FA, B = FB, design = design, nrep = nrep,  nc = 3)
#
#         if(design == "crd"){
#           names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
#         } else{
#           names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
#         }
#
#
#       }
#
# #
# #       if(design == "crd"){
# #         names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
# #       } else{
# #         names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2)
# #       }
#       fb <- fb
#
#     } else if(nfactors == 3){
#       #
#       FA <-  factor_lvl1
#       FB <- factor_lvl2
#       FC <- factor_lvl3
#
#       if( isCropFactor==TRUE  && length(crop_varietiesname)>=2 ){
#
#         fb <- cd.factorial(A = FA, B = FB, C = FC,  design = design, nrep = nrep,  nc = 3)
#
#         if(design == "crd"){
#           names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, "CROP_VARIETY")
#         } else{
#           names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, "CROP_VARIETY")
#         }
#
#
#
#       } else {
#
#       fb <- cd.factorial(A = FA, B = FB, C = FC, design = design, nrep = nrep, nc = 3)
#
#       if(design == "crd"){
#         names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3)
#       } else {
#         names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2,factor_name3)
#       }
#       fb <- fb
#
#
#       }
#
#       fb <- fb
#
#     } else if(nfactors == 4){
#
#       FA <-  factor_lvl1
#       FB <- factor_lvl2
#       FC <- factor_lvl3
#       FD <- factor_lvl4
#
#       fb <- cd.factorial(A = FA, B = FB, C = FC, D = FD, design = design, nrep = nrep, nc = 3)
#
#       if(design == "crd"){
#         names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4)
#       } else{
#         names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4)
#       }
#       fb <- fb
#
#     } else {
#
#       FA <- factor_lvl1
#       FB <- factor_lvl2
#       FC <- factor_lvl3
#       FD <- factor_lvl4
#       FE <- factor_lvl5
#
#       fb <- cd.factorial(A = FA, B = FB, C = FC, D = FD , E = FE, design = design, nrep = nrep, nc = 3)
#
#       if(design == "crd"){
#         names(fb$book)<- c("PLOT", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4, factor_name5)
#       } else{
#         names(fb$book)<- c("PLOT", "BLOCK", "ROW", "COL", "TREATMENT", factor_name1, factor_name2, factor_name3, factor_name4, factor_name5)
#       }
#       fb <- fb
#
#     }
#     fb <- fb$book
#
#     if(design=="crd"){
#       fb <- fb[,-c(2,3)]
#     }
#     if(design == "rcbd"){
#       fb <- fb[,-c(3,4)]
#     }



    fb


    #add_fieldbook_sheet_hdfims(file = "omar.xlsx", fieldbook =fb)
    #shell.exec("C://Users//obenites//Documents//omar.xlsx")

  })

  ###fieldbook with traits ################################################

  fb_agrofims_traits <- reactive({


     fb <- fb_agrofims()

     trait <- traits_dt()
     #print(trait)
     cm <- trait$`Crop measurement`
     sc <- trait$Scale
     co <- trait$VariableId
     cs <- paste(cm, sc, co, sep="-")

     #trait_selected <- trait_agrofims() %>% as.data.frame(stringsAsFactors =FALSE) #unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
     trait_selected <- cs
     print("Trait selected")
     print(trait_selected)

     if(!is.null(trait_selected) || length(trait_selected)==0 ){
       mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
       nm  <-  c(names(fb), trait_selected)
       fb  <-  cbind(fb, mm)
       names(fb)  <-  nm
     }


     # trait_selected <- trait_agrofims() %>% as.data.frame(stringsAsFactors =FALSE) #unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
     # trait_selected <- trait_selected[,2]
     #
     # if(!is.null(trait_selected)){
     #   mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
     #   nm  <-  c(names(fb), trait_selected)
     #   fb  <-  cbind(fb, mm)
     #   names(fb)  <-  nm
     # }

     fb

   })


  ##reactive table from metadata info   ########################################################

  dt_metadata_agrofims <- reactive({

    startDate_val <- input$fbDesign_project_time_line[1]
    print(startDate_val)
    endDate_val <- input$fbDesign_project_time_line[2]
    #print(endDate_val)
    x <- interval(ymd(startDate_val),ymd(endDate_val))
    x <- x %/% months(1)
    Duration_val <- paste(x," months", sep = "")


    metadata<- fbdesign::add_metadata_agrofims( agronomic_crop_template = metadata_template_list, col_name= "Value",
                                                experimentId =  input$experimentId,
                                                experimentName = input$experimentName,
                                                experimentProjectName = input$experimentProjectName,
                                                startDate = startDate_val,
                                                endDate =  endDate_val,
                                                Duration = Duration_val,
                                                typeExperiment = input$designFieldbook_typeExperiment,
                                                experimentObj = input$experimentObj,

                                                fundAgenType = input$designFieldbook_fundAgencyType,
                                                fundName = input$fundName,
                                                contCenter = input$contCenter,
                                                contCRP = input$contCRP,
                                                contResearcher =  input$contResearcher,

                                                fundLeadAgency = input$designFieldbook_fundLeadAgency,
                                                leadName = input$leadName,

                                                npersonnel= input$npersons, ##NEW Inclur el input del n personnel

                                                personnel1Type = input$personnel1Type,
                                                person1FirstName = input$person1FirstName,
                                                person1LastName = input$person1LastName,
                                                person1Email = input$person1Email,
                                                person1Afiliation =   input$person1Afiliation,
                                                person1ORCID = input$person1ORCID,

                                                personnel2Type = input$personnel2Type,
                                                person2FirstName = input$person2FirstName,
                                                person2LastName = input$person2LastName,
                                                person2Email = input$person2Email,
                                                person2Afiliation =   input$person2Afiliation,
                                                person2ORCID = input$person2ORCID,

                                                personnel3Type = input$personnel3Type,
                                                person3FirstName = input$person3FirstName,
                                                person3LastName = input$person3LastName,
                                                person3Email = input$person3Email,
                                                person3Afiliation =   input$person3Afiliation,
                                                person3ORCID = input$person3ORCID,

                                                personnel4Type = input$personnel4Type,
                                                person4FirstName = input$person4FirstName,
                                                person4LastName = input$person4LastName,
                                                person4Email = input$person4Email,
                                                person4Afiliation =   input$person4Afiliation,
                                                person4ORCID = input$person4ORCID,

                                                personnel5Type = input$personnel5Type,
                                                person5FirstName = input$person5FirstName,
                                                person5LastName = input$person5LastName,
                                                person5Email = input$person5Email,
                                                person5Afiliation =   input$person5Afiliation,
                                                person5ORCID = input$person5ORCID,

                                                sytpetype = input$sytpetype,
                                                syteName = input$syteName,
                                                siteID = input$siteID ,
                                                countryName = input$fbDesign_countryTrial,
                                                villageName= input$designFieldbook_sites,
                                                nearestPopupPlace = "", #Nearest populated place NEW

                                                inHighLevel = input$fbDesign_inHighLevel,
                                                inSiteVegetation = input$fbDesign_inSiteVegetation,
                                                inSiteDescNotes= input$inSiteDescNotes, #input$descNotes,

                                                croppingType = input$croppingType,
                                                cropCommonNameMono = input$cropCommonNameMono,
                                                cropVarietyNameMono = input$cropVarietyNameMono,
                                                cropLatinNameMono = input$cropLatinNameMono,
                                                cultivarNameMono = input$cultivarNameMono,
                                                monoCropLocalName= input$monoCropLocalName,

                                                numPreviousCrop= input$numPreviousCrop,
                                                prevCropName = input$prevCropName,
                                                prevCropVar = input$prevCropVar,

                                                subject = "",
                                                keywords ="",
                                                Embargo_date = "")

    metadata

  })

  ##reactive table for installation info ########################################################
  dt_installation_agrofims <- reactive({


    crop <- input$cropCommonNameMono #monocrop
    #crop <- input$cropsSelected


    if(crop == "Wheat" || crop == "Maize" || crop == "Soybean"){
     agromfims_installation_sheet <- installation1_template_list
    }

    if(crop == "Potato" || crop == "Sweetpotato" || crop == "Cassava") {
      agromfims_installation_sheet <-installation2_template_list
    }

    add_installation_agrofims(agronomic_crop_template= agromfims_installation_sheet, col_name = "Value",


                crop = input$cropCommonNameMono,
                designFieldbook_agrofims	=	input$designFieldbook_agrofims,#all crops
                designFieldbook_agrofims_r	=	input$designFieldbook_agrofims_r, #all crops


                numPlantsPerPlot	=	input$numPlantsPerPlot,#potato cassava sweetpotato
                numRowsPerPlot	=	input$numRowsPerPlot,#potato cassava sweetpotato
                numPlantsPerRow	=	input$numPlantsPerRow,#potato cassava sweetpotato
                plotSize	=	input$plotSize,#potato cassava sweetpotato
                distancebwPlants = input$distancebwPlants,
                distanceBwRows = input$distanceBwRows,
                spaceBwPlants	=	input$spaceBwPlants,#potato cassava sweetpotato
                spaceBwRows	=	input$spaceBwRows,#potato cassava sweetpotato
                planDensity	=	input$planDensity,#potato cassava sweetpotato

                plotSpacing	=	input$plotSpacing,#wehat maize soybean
                rowSpacing = input$rowSpacing,#wehat maize soybean
                rowOrientation	=	input$rowOrientation,#wehat maize soybean
                spaceBwPlantsRow = input$spaceBwPlantsRow,#wehat maize soybean
                hillSpacing	=	input$hillSpacing,#wehat maize soybean
                numsMsPlantPerPlot = input$numsMsPlantPerPlot,#wehat maize soybean
                fieldArea = input$fieldArea,#wehat maize soybean
                expFieldMaxWidth = input$expFieldMaxWidth,#wehat maize soybean
                expFieldMaxLength = input$expFieldMaxLength,#wehat maize soybean


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

  ###############################Agrofeatures #############################################################

  ### Land description
  dt_land_description <- reactive({

    out <- fb_agrofims()


    landLeveling_start_date	<-	paste(input$landLeveling_start_date)
    landLeveling_end_date	<-	paste(input$landLeveling_end_date)
    numPasses	<-	input$numPasses

    land_impl_type	<-	input$land_impl_type
    if(is.null(land_impl_type)){
      land_impl_type	<-	""
    }
    if(land_impl_type=="Other"){
      land_impl_type	<-	input$land_impl_type_other
    }

    land_traction	<-	input$land_traction
    if(is.null(land_traction)){
      land_traction		<-	""
    }
    if(land_traction=="Other"){
      land_traction		<-	input$contOtherTraction
    }
    puddling_start_date	<-	paste(input$puddling_start_date)
    puddling_end_date	<-	paste(input$puddling_end_date)
    Penetrometer_in_field	<-	input$Penetrometer_in_field
    puddling_depth_val	<-	input$puddling_depth_val

    puddling_depth_unit	<-	input$puddling_depth_unit
    if(is.null(puddling_depth_unit)){
      puddling_depth_unit	<-	""
    }

    pud_impl_type	<-	input$pud_impl_type
    if(is.null(pud_impl_type)){
      pud_impl_type	<-	""
    }
    if(pud_impl_type=="Other"){
      pud_impl_type	<-	input$pud_impl_type_other
    }
    pud_traction	<-	input$pud_traction

    if(is.null(pud_traction)){
      pud_traction	<-	""
    }
    if(pud_traction=="Other"){
      pud_traction	<-	input$pud_contOtherTraction
    }

    tillage_start_date	<-	paste(input$tillage_start_date)
    tillage_end_date	<-	paste(input$tillage_end_date)

    till_technique	<-	input$till_technique
    if(is.null(till_technique)){
      till_technique	<-	""
    }
    if(is.null(till_technique)){
      till_technique	<-	""
    }
    if(till_technique=="Other"){
    till_technique	<-	input$till_technique_other
    }

    till_depth_method	<-	input$till_depth_method
    tillage_depth	<-	input$tillage_depth

    tillage_depth_unit	<-	input$tillage_depth_unit
    if(is.null(tillage_depth_unit)){
      tillage_depth_unit <- ""
    }

    total_number_tillage_passes	<-	input$total_number_tillage_passes

    till_impl_type	<-	input$till_impl_type
    if(is.null(till_impl_type)){
      till_impl_type	<-	""
    }
    if(till_impl_type=="Other"){
      till_impl_type	<-	input$contOthertill_impl_type
    }

    till_traction	<-	input$till_traction

    if(is.null(till_traction)){
      till_traction	<-	""
    }
    if(till_traction=="Other"){
      till_traction	<-	input$contOthertill_traction
    }

    liming_start_date	<-	paste(input$liming_start_date)
    liming_end_date	<-	paste(input$liming_end_date)
    lim_material	<-	input$lim_material
    lim_quantity	<-	input$lim_quantity

    lim_quantity_unit	<-	input$lim_quantity_unit
    if(is.null(lim_quantity_unit)){
      lim_quantity_unit	<-	""
    }

    lim_description	<-	input$lim_description

    liming_impl_type	<-	input$liming_impl_type
    if(is.null(liming_impl_type)){
      liming_impl_type	<-	""
    }
    if(liming_impl_type== "Other"){
      liming_impl_type	<-	input$contOtherliming_impl_type
    }

    dtLandprep <- data.frame(landLeveling_start_date,
                             landLeveling_end_date,
                             numPasses,
                             land_impl_type,
                             land_traction,

                             puddling_start_date,
                             puddling_end_date,
                             Penetrometer_in_field,
                             puddling_depth_val,
                             puddling_depth_unit,
                             pud_impl_type,
                             pud_traction,#)#,

                             tillage_start_date,
                             tillage_end_date,
                             till_technique,
                             till_depth_method,
                             tillage_depth,
                             tillage_depth_unit,
                             total_number_tillage_passes,
                             till_impl_type,
                             till_traction,#)#,

                             liming_start_date,
                             liming_end_date,
                             lim_material,
                             lim_quantity,
                             lim_quantity_unit,
                             lim_description,
                             liming_impl_type)

    landpreNames <- c('Land levelling start date',
                       'Land levelling end date',
                       'Total number of levelling passes',
                       'Land levelling implement type',
                       'Land levelling traction',

                       'Puddling start date',
                       'Puddling end date',
                       'Penetrometer in field',
                       'Puddling depth val',
                       'Puddling depth unit',
                       'Puddling implement type',
                       'Puddling traction',#)#,

                       'Tillage start date',
                       'Tillage end date',
                       'Tillage technique',
                       'Tillage depth method',
                       'Tillage depth',
                       'Tillage depth unit',
                       'Total number of tillage passes',
                       'Tillage implement',
                       'Tillage traction',#,
                       #
                       'Liming start date',
                       'Liming end date',
                       'Liming material',
                       'Quantity of liming material',
                       'Quantity of liming material unit',
                       'Liming description',
                       'Liming implement')


    names(dtLandprep) <- landpreNames


    out <- merge(out, dtLandprep, by = 0, all = TRUE)[-1]

    #print(out)
    # c(input$landLeveling_start_date, input$landLeveling_end_date,
    #   input$numPasses,input$land_impl_type,input$land_impl_type_other,
    #   input$land_traction,input$contOtherTraction,input$puddling_start_date,input$puddling_end_date,
    #   input$Penetrometer_in_field,input$puddling_depth_val,input$puddling_depth_unit,
    #   input$pud_impl_type,input$pud_impl_type_other,input$pud_traction,
    #   input$pud_contOtherTraction,input$tillage_start_date,input$tillage_end_date,
    #   input$till_technique,input$till_technique_other,input$till_depth_method,input$tillage_depth,
    #   input$tillage_depth_unit,input$total_number_tillage_passes,input$till_impl_type,
    #   input$contOthertill_impl_type,input$till_traction,input$contOthertill_traction,input$liming_start_date,input$liming_end_date,
    #   input$lim_material,input$lim_quantity,input$lim_quantity_unit,input$lim_description,
    #   input$liming_impl_type,input$contOtherliming_impl_type)



      # out<-  land_des(input$landLeveling_start_date,input$landLeveling_end_date,
      #             input$numPasses,input$operationsOrder,input$impl_type,
      #             input$animal_traction,input$humanPowered,input$motorized_traction,
      #             input$puddling_start_date,input$puddling_end_date,
      #             input$Penetrometer_in_field,input$puddling_depth_val,input$pud_animal_traction,
      #             input$pud_humanPowered,input$pud_motorized_traction,input$tillage_start_date,
      #             input$tillage_end_date,input$till_technique,input$till_depth_method,input$till_depth,
      #             input$till_total_op_season,input$till_impl_type,input$till_animal_traction,
      #             input$till_humanPowered,input$till_motorized_traction,input$liming_start_date,
      #             input$liming_end_date,input$lim_material,input$lim_quantity,input$lim_description
      #   )

     out
     # out <- dt
  })

  ### Mulching
  dt_mulching <- reactive({


    out <- fb_agrofims()


    mulch_start_date	<-	input$mulch_start_date
    mulch_end_date	<-	input$mulch_end_date

    mulch_type	<-	input$mulch_type
    if(is.null(mulch_type)){
      mulch_type	<-	""
    }
    if(mulch_type == "Other"){
      mulch_type <- input$mulch_type_other
    }

    mulch_thickness	<-	input$mulch_thickness
    mulch_thickness_unit	<-	input$mulch_thickness_unit #unit
    if(is.null(mulch_thickness_unit)){
      mulch_thickness_unit	<-	""
    }

    mulch_amountPerSq	<-	input$mulch_amountPerSq
    mulch_amountPerSq_unit	<-	input$mulch_amountPerSq_unit #unit
    if(is.null(mulch_amountPerSq_unit)){
      mulch_amountPerSq_unit	<-	""
    }

    mulch_color	<-	input$mulch_color
    if(is.null(mulch_color)){
      mulch_color	<-	""
    }

    mulch_percCoverage	<-	input$mulch_percCoverage
    mulch_remove_start_date	<-	input$mulch_remove_start_date
    mulch_remove_end_date	<-	input$mulch_remove_end_date

    mulch_traction	<-	input$mulch_traction
    if(is.null(mulch_traction)){
      mulch_traction	<-	""
    }
    if(mulch_traction == "Other"){
      mulch_traction <- input$mulch_type_other
    }

    residue_start_date	<-	input$residue_start_date
    residure_end_date	<-	input$residure_end_date

    residue_cropType	<-	input$residue_cropType
    if(is.null(residue_cropType)){
      residue_cropType	<-	""
    }
    if(residue_cropType == "Other"){
      residue_cropType <- input$residue_traction_other
    }


    residue_technique	<-	input$residue_technique
    if(is.null(residue_technique)){
      residue_technique	<-	""
    }

    residue_traction	<-	input$residue_traction
    if(is.null(residue_traction)){
      residue_traction	<-	""
    }
    if(residue_traction == "Other"){
      residue_traction <- input$residue_traction_other
    }

    crop_residue_thick	<-	input$crop_residue_thick
    crop_residue_thick_unit <- input$crop_residue_thick_unit #unit
    if(is.null(crop_residue_thick_unit)){
      crop_residue_thick_unit	<-	""
    }

    crop_residue_amount_sqm	<-	input$crop_residue_amount_sqm
    crop_residue_amount_sqm_unit	<-	input$crop_residue_amount_sqm_unit #unit
    if(is.null(crop_residue_amount_sqm_unit)){
      crop_residue_amount_sqm_unit	<-	""
    }
    crop_residue_perc_cov	<-	input$crop_residue_perc_cov
    residue_inc_depth	<-	input$residue_inc_depth

    above_ground_res_moisture	<-	input$above_ground_res_moisture
    if(is.null(above_ground_res_moisture)){
      above_ground_res_moisture	<-	""
    }

    above_ground_res_amount	<-	input$above_ground_res_amount

    above_ground_res_amount_unit	<-	input$above_ground_res_amount_unit #unit
    if(is.null(above_ground_res_amount_unit)){
      above_ground_res_amount_unit	<-	""
    }

    dtmulch <- data.frame(mulch_start_date,
                          mulch_end_date,
                          mulch_type,
                          mulch_thickness,
                          mulch_thickness_unit,
                          mulch_amountPerSq,
                          mulch_amountPerSq_unit,
                          mulch_color,
                          mulch_percCoverage,
                          mulch_remove_start_date,
                          mulch_remove_end_date,
                          mulch_traction,
                          residue_start_date,
                          residure_end_date,
                          residue_cropType,
                          residue_technique,
                          residue_traction,
                          crop_residue_thick,
                          crop_residue_thick_unit,
                          crop_residue_amount_sqm,
                          crop_residue_amount_sqm_unit,
                          crop_residue_perc_cov,
                          residue_inc_depth,
                          above_ground_res_moisture,
                          above_ground_res_amount,
                          above_ground_res_amount_unit)



    mulchresNames <-c(
                'Mulching start date',
                'Mulching end date',
                'Mulch type',
                'Mulch thickness',
                'Mulch thickness unit',
                'Mulch amount per sq. m',
                'Mulch amount per sq. m unit',
                'Mulch color',
                'Mulch percentage of coverage',
                'Mulch removal start date',
                'Mulch removal end date',
                'Mulching traction',
                'Residue start date',
                'Residue end date',
                'Crop residue type',
                'Residue management technique',
                'Residue management traction ',
                'Crop residue thickness',
                'Crop residue thickness unit',
                'Crop residue amount per sq. m',
                'Crop residue amount per sq. m unit',
                'Crop residue percentage of coverage',
                'Residue incorporation depth',
                'Above ground residue moisture',
                'Above ground residue (amount)',
                'Above ground residue (amount) unit')


    names(dtmulch) <- mulchresNames


    out <- merge(out, dtmulch, by = 0, all = TRUE)[-1]

    # out<-mulch(input$mulch_start_date,input$mulch_end_date,
    #       input$mulch_type,input$mulch_thickness,input$mulch_amountPerSq,
    #       input$mulch_color,input$mulch_percCoverage,input$mulch_remove_start_date,
    #       input$mulch_remove_end_date,input$mulch_make,input$mulch_model,
    #       input$mulch_animal_traction,input$mulch_humanPowered,
    #       input$mulch_motorized_traction,input$residue_cropType,
    #       input$residue_technique,input$residue_incorp_depth,
    #       input$residue_aboveGroundMoisture,
    #       input$residue_aboveGroundAmount)
     out

  })

  ### Planting
  dt_planting <- reactive({

    out <- fb_agrofims()

    plantNames<-c('Direct seeding  Start date',
                  'Direct seeding  End date',
                  'Direct seeding  Seeding environment',
                  'Direct seeding  Seeding technique',
                  'Direct seeding  Seed treatment',
                  'Direct seeding  Traction',


                  'Direct seeding  Distance between rows',
                  'Direct seeding  Distance between rows unit',
                  'Direct seeding/Seeding rate',
                  'Direct seeding/Seeding rate unit',
                  'Distance between plants',
                  'Distance between plants units',
                  'Transplanting Start date',
                  'Transplanting End date',
                  'Transplanting Age of seedling',
                  'Transplanting seeding Environment',
                  'Transplanting Seed treatment',
                  'Transplanting Technique',
                  'Transplanting Traction',

                  'Transplanting Seedling density',
                  'Transplanting Seedling density units',
                  'Transplanting Distance between rows',
                  'Transplanting Distance between rows units',
                  'Transplanting Distance between plants',
                  'Transplanting Distance between plants units',
                  'Transplanting number of row')



    planting_start_date	<-	input$planting_start_date
    planting_end_date	<-	input$planting_end_date

    seeding_environment	<-	input$seeding_environment
    if(is.null(seeding_environment )){
      seeding_environment	<-	""
    }

    seeding_technique	<-	input$seeding_technique
    if(is.null(seeding_technique )){
      seeding_technique	<-	""
    }


    seed_treatment	<-	input$seed_treatment

    seeding_traction	<-	input$seeding_traction
    if(is.null(seeding_traction )){
      seeding_traction	<-	""
    }
    if(seeding_traction== "Other"){
      seeding_traction	<-	input$seeding_traction_name
    }

    distance_rows	<-	input$distance_rows

    distance_rows_unit	<-	input$distance_rows_unit
    if(is.null(distance_rows_unit )){
      distance_rows_unit	<-	""
    }

    seeding_rate	<-	input$seeding_rate
    seeding_rate_unit	<-	input$seeding_rate_unit
    if(is.null(seeding_rate_unit )){
      seeding_rate_unit	<-	""
    }

    distance_plants	<-	input$distance_plants
    distance_plants_unit	<-	input$distance_plants_unit
    if(is.null(distance_plants_unit )){
      distance_plants_unit	<-	""
    }


    transplanting_start_date	<-	input$transplanting_start_date
    transplanting_end_date	<-	input$transplanting_end_date
    age_seedling	<-	input$age_seedling

    transplanting_environment	<-	input$transplanting_environment
    if(is.null(transplanting_environment )){
      transplanting_environment	<-	""
    }

    transplanting_treatment	<-	input$transplanting_treatment


    transplanting_technique	<-	input$transplanting_technique
    if(is.null(transplanting_technique )){
      transplanting_technique	<-	""
    }

    trans_traction	<-	input$trans_traction
    if(is.null(trans_traction )){
      trans_traction	<-	""
    }
    if(trans_traction== "Other"){
      trans_traction	<-	input$trans_traction_name
    }

    trans_seeding_density	<-	input$trans_seeding_density

    trans_seeding_density_unit	<-	input$trans_seeding_density_unit

    if(is.null(trans_seeding_density_unit )){
      trans_seeding_density_unit	<-	""
    }

    trans_distance_rows	<-	input$trans_distance_rows
    trans_distance_rows_unit	<-	input$trans_distance_rows_unit
    if(is.null(trans_distance_rows_unit )){
      trans_distance_rows_unit	<-	""
    }

    trans_distance_plants	<-	input$trans_distance_plants
    trans_distance_plants_unit	<-	input$trans_distance_plants_unit

    if(is.null(trans_distance_plants_unit )){
      trans_distance_plants_unit	<-	""
    }

    trans_num_rows	<-	input$trans_num_rows


    dtPlanting <- data.frame(
                      planting_start_date,
                      planting_end_date,
                      seeding_environment,
                      seeding_technique,

                      seed_treatment,
                      seeding_traction,
                      distance_rows,
                      distance_rows_unit,
                      seeding_rate,
                      seeding_rate_unit,
                      distance_plants,
                      distance_plants_unit,

                      transplanting_start_date,
                      transplanting_end_date,
                      age_seedling,
                      transplanting_environment,
                      transplanting_treatment,
                      transplanting_technique,
                      trans_traction,
                      trans_seeding_density,
                      trans_seeding_density_unit,
                      trans_distance_rows,
                      trans_distance_rows_unit,
                      trans_distance_plants,
                      trans_distance_plants_unit,
                      trans_num_rows
    )


    names(dtPlanting) <- plantNames


    out <- merge(out, dtPlanting, by = 0, all = TRUE)[-1]



    # out<- plant(input$planting_start_date,input$planting_end_date,
    #        input$planting_directSeeding,input$planting_seedingTech,
    #        input$planting_ageSeeding,input$planting_manual,
    #        input$planting_animal_traction,input$planting_motorized_traction,
    #        input$planting_rowDistance,input$planting_seedingRate,
    #        input$planting_seedPerhill,input$planting_distance,
    #        input$planting_distribution)
     out

  })


  ### Harvest
  dt_harvest <- reactive({

    out <- fb_agrofims()

    harvest_start_date	<-	input$harvest_start_date
    harvest_end_date	<-	input$harvest_end_date
    harvest_cut_height	<-	input$harvest_cut_height
    num_rows_harvested	<-	input$num_rows_harvested
    len_row_harvested	<-	input$len_row_harvested

    len_row_harvested_unit	<-	input$len_row_harvested_unit
    if(is.null(len_row_harvested_unit )){
      len_row_harvested_unit	<-	""
    }


    space_rows_harvested	<-	input$space_rows_harvested
    space_rows_harvested_unit	<-	input$space_rows_harvested_unit
    if(is.null(space_rows_harvested_unit )){
      space_rows_harvested_unit	<-	""
    }


    area_harvested	<-	input$area_harvested
    area_harvested_unit	<-	input$area_harvested_unit
    if(is.null(area_harvested_unit )){
      area_harvested_unit	<-	""
    }

    num_plants_area_harvested	<-	input$num_plants_area_harvested
    crop_component_harvested	<-	input$crop_component_harvested
    if(is.null(crop_component_harvested )){
      crop_component_harvested	<-	""
    }

    harvest_implement	<-	input$harvest_implement
    if(is.null(harvest_implement )){
      harvest_implement	<-	""
    }
    if(harvest_implement== "Other"){
      harvest_implement	<-	input$harvest_implement_other
    }


    harvest_make	<-	input$harvest_make
    harvest_model	<-	input$harvest_model

    harvest_traction	<-	input$harvest_traction
    if(is.null(harvest_traction )){
      harvest_traction	<-	""
    }
    if(harvest_traction== "Other"){
      harvest_traction	<-	input$harvest_traction_other
    }

    harvNames <- c('Harvest start date',
                  'Harvest end date',
                  'Harvest cut height',
                  'Number of rows harvested',
                  'Length of rows harvested',
                  'Length of rows harvested unit',
                  'Space between rows harvested',
                  'Space between rows harvested unit',
                  'Area harvested',
                  'Area harvested unit',
                  'Number of plants in area harvested',
                  'Crop component harvested',
                  'Harvest implement',

                  'Harvest implement make',
                  'Harvest implement model',
                  'Harvest implement traction'
    )

    dtHarv <- data.frame( harvest_start_date,
                          harvest_end_date,
                          harvest_cut_height,
                          num_rows_harvested,
                          len_row_harvested,
                          len_row_harvested_unit,
                          space_rows_harvested,
                          space_rows_harvested_unit,
                          area_harvested,
                          area_harvested_unit,
                          num_plants_area_harvested,
                          crop_component_harvested,
                          harvest_implement,

                          harvest_make,
                          harvest_model,
                          harvest_traction
    )

    names(dtHarv) <- harvNames
    out <- merge(out, dtHarv, by = 0, all = TRUE)[-1]

    out



    # out<-harvest(input$harvest_start_date,
    #         input$harvest_end_date,input$crop_component_harvested,
    #         input$harvest_implement,input$harvest_make,input$harvest_model,
    #         input$harvest_animal_traction,input$harvest_humanPowered,
    #         input$harvest_motorized_traction)
    # out

  })

  #irrigation
  dt_irrigation <- reactive({

    out <- fb_agrofims()

    nirri <- input$numApplicationsIrrigation

    sdate <- edate <-  tech <- vt <- vt_label <- list()
    ws <-   wsdist <-    wsuni <-    wb <-    wbu <-    wpr <-    wpru <-    depth <-    depthu <-    iwdepth <-    iwdepthu <- list()
    amount <- uamount <-  area <- uarea <- list()


    nirri <- input$numApplicationsIrrigation

    for(i in 1:nirri) {
      sdate[[i]] <-  paste(input[[	paste0("irrigationevent_start_date_", i)	]])
      edate[[i]] <-  paste(input[[	paste0("irrigationevent_end_date_", i)	]])
      tech[[i]] <-  paste(input[[	paste0("irrigation_technique_", i)	]])
      if(is.null( tech[[i]] ) || length(tech[[i]]) ==0) tech[[i]] <-  ""

      #if( tech[[i]] == "Other" ) tech[[i]] <-   paste(input[[  paste0("irrigation_water_source_", i,  "_other")  ]])
      if( tech[[i]] == "Surface" ) {
            vt[[i]] <-  paste(input[[ paste0("surface_irrigation_technique_", i) ]])
            vt_label[[i]] <-  "Surface irrigation technique"
          } else if( tech[[i]] == "Localized" ) {
            vt[[i]] <- "" #paste(input[[  paste0("localized_irrigation_technique", i) ]]){
            vt_label <- paste("foo",i, sep ="")
          } else if( tech[[i]]== "Irrigation sprinker" ) {
            vt[[i]] <-  paste(input[[ paste0("irrigation_using_sprinkler_systems_", i) ]])
            vt_label[[i]] <- "Irrigation using sprinkler systems"
          } else {
            vt[[i]] <- ""
            vt_label <- paste("foo",i , sep ="")
      }

      print(vt[[i]])

        ws[[i]] <-  paste(input[[	paste0("irrigation_water_source_", i)	]])
        if(is.null( ws[[i]] ) || length(ws[[i]]) ==0) ws[[i]] <-  ""
        wsdist[[i]] <-  paste(input[[	paste0("irrigation_water_source_distance_", i)	]])
        wsuni[[i]] <-  paste(input[[	paste0("irrigation_water_source_distance_", i, "unit")	]])
        if(is.null( wsuni[[i]] ) || length(wsuni[[i]]) ==0) wsuni[[i]] <-  ""

        wb[[i]] <-  paste(input[[	paste0("irrigation_bund_height_", i)	]])
        wbu[[i]] <-  paste(input[[	paste0("irrigation_bund_height_", i, "unit")	]])
        if(is.null( wbu[[i]] ) || length(wbu[[i]]) ==0) wbu[[i]] <-  ""

        wpr[[i]] <-  paste(input[[	paste0("irrigation_percolation_rate_", i)	]])
        wpru[[i]] <-  paste(input[[	paste0("irrigation_percolation_rate_", i, "unit")	]])
        if(is.null(wpru[[i]] ) || length(wpru[[i]]) ==0) wpru[[i]] <-  ""

        depth[[i]] <-  paste(input[[	paste0("irrigation_equipment_depth_", i)	]])
        depthu[[i]] <-  paste(input[[	paste0("irrigation_equipment_depth_", i, "unit")	]])
        if(is.null( depthu[[i]] ) || length(depthu[[i]]) ==0) depthu[[i]] <-  ""

        iwdepth[[i]] <-  paste(input[[	paste0("irrigation_well_depth_", i)	]])
        iwdepthu[[i]] <-  paste(input[[	paste0("irrigation_well_depth_", i, "unit")	]])
        if(is.null( iwdepthu[[i]] ) || length(iwdepthu[[i]]) ==0) iwdepthu[[i]] <-  ""

        amount[[i]] <-  paste(input[[	paste0("irrigation_amount_", i)	]])
        uamount[[i]] <-  paste(input[[	paste0("irrigation_amount_", i, "unit")	]])
        if(is.null( uamount[[i]] ) || length(uamount[[i]]) ==0) uamount[[i]] <-  ""

        area[[i]] <-  paste(input[[	paste0("irrigation_area_covered_irrigation_system_", i)	]])
        uarea[[i]] <-  paste(input[[	paste0("irrigation_area_covered_irrigation_system_", i, "unit")	]])
        if(is.null( uarea[[i]] ) || length(uarea[[i]]) ==0) uarea[[i]] <-  ""

    }


      sdate <- unlist(sdate)
      edate <- unlist(edate)
      tech <- unlist(tech )
      vt <- unlist(vt)
      vt_label <- unlist(vt_label) #no considerar

      ws <- unlist(ws )
      wsdist <- unlist(wsdist )
      wsuni <- unlist(wsuni )
      wb <- unlist(wb )
      wbu <- unlist(wbu )
      wpr <-unlist(wpr )
      wpru <-unlist(wpru)
      depth <- unlist(depth )
      depthu <- unlist(depthu)
      iwdepth <- unlist(iwdepth )
      iwdepthu <- unlist(iwdepthu )
      amount <- unlist(amount)
      uamount <- unlist(uamount )
      area <- unlist(area )
      uarea <- unlist(uarea )

        irri <- c( sdate , edate ,tech, vt,  ws, wsdist , wsuni,wb ,wbu ,wpr,wpru,depth ,depthu ,iwdepth ,iwdepthu ,amount ,uamount ,area ,uarea )
        print(irri)
        temp <- t(irri)
        irridt <- as.data.frame(temp, stringsAsFactors =FALSE)
        print(irridt)

        for(i in 1:nirri){
          a2	<-paste(	'Start date'	, 1:i)
          a3	<-paste(	'End date'	, 1:i)
          a4	<-paste(	'Irrigation technique'	, 1:i)
          a5	<-paste(	'Water source'	, 1:i)
          a6	<-paste(	'Water source distance'	, 1:i)
          a7	<-paste(	'Unit'	, 1:i)
          a8	<-paste(	'Bund height'	, 1:i)
          a9	<-paste(	'Bund height unit'	, 1:i)
          a10	<-paste(	'Percolation rate'	, 1:i)
          a11	<-paste(	'Percolation rate unit'	, 1:i)
          a12	<-paste(	'Irrigation equipment depth'	, 1:i)
          a13	<-paste(	'Irrigation equipment depth unit'	, 1:i)
          a14	<-paste(	'Well depth'	, 1:i)
          a15	<-paste(	'Well depth unit'	, 1:i)
          a16	<-paste(	'Irrigation amount'	, 1:i)
          a17	<-paste(	'Irrigation amount unit'	, 1:i)
          a18	<-paste(	'Area covered by the irrigation system'	, 1:i)
          a19	<-paste(	'Area covered by the irrigation system unit'	, 1:i)
        }


  irriNames <- c(a2, a3, a4, vt_label, a5, a6, a7, a8, a9, a10, a11, a12,a13, a14, a15, a16, a17, a18, a19)

  print(irriNames)

  names(irridt) <- irriNames
  as <- grepl("foo", names(irridt)) #detec foo variables
  irridt <- irridt[!as] #remove foo variables

  out <- merge(out, irridt, by = 0, all = TRUE)[-1]

  names(out) <- stringr::str_replace_na(names(out))
  as2 <- grepl("NA", names(out)) #detec foo variables
  out <- out[!as2]

 out

    # out<-irrigation(input$irrigationevent_start_date,
    #             input$irrigationevent_end_date,input$irrigation_system_type,
    #             input$irrigation_technique,input$surface_irrigation_technique,
    #             input$localized_irrigation_technique,input$irrigation_using_sprinkler_systems,
    #             irrigation_system_picture = "", #input$rrigation_system picture,
    #             input$irrigation_water_source,input$irrigation_water_source_distance,
    #             input$irrigation_bund_height,input$irrigation_percolation_rate,input$irrigation_equipment_depth,
    #             input$irrigation_well_depth,input$irrigation_area_covered_irrigation_system)
    #
    # out

  })

  ##biofertilization
  dt_bioferti <- reactive({

    out <- fb_agrofims()

    nbio <- input$numApplicationsBiofert

    sdate <- edate <-  strain <- quant <- uni <- method<- prodbiofer <- daysbiofer <- list()

    for(i in 1:nbio) {

      sdate[[i]] <-  paste(input[[  paste0("biofertilizer_landLeveling_start_date_", i)    ]])
      edate[[i]] <-  paste(input[[   paste0("biofertilizer_landLeveling_end_date",  i)      ]])
      strain[[i]] <- paste(  input[[   paste0("biofertilizer_rhizobium_inoculum_strain_", i)    ]])
      if(is.null( strain[[i]] ) || length(strain[[i]])==0 ) strain[[i]] <- ""
      if(is.null( strain[[i]]=="Other")) strain[[i]] <- paste(input[[ paste0("rhizobium_name_", i)  ]])
      quant[[i]] <- paste(input[[  paste0("biofertilizer_quantity_inoculated_", i) ]])
      uni[[i]] <-  paste(input[[ paste0("biofertilizer_quantity_inoculated_unit_", i) ]])
      if(is.null(uni[[i]]) || length(uni[[i]])==0 ) uni[[i]]  <- ""
      method[[i]]  <-  paste(input[[  paste0("biofertilizer_inoculation_method_", i)    ]])
      if(is.null( method[[i]] ) || length(  method[[i]])==0 ) method[[i]] <- ""
      if(is.null( method[[i]]=="Other")) method[[i]] <- paste(input[[  paste0("inoculation_method_name_", i)  ]])
      prodbiofer[[i]] <- paste( input[[   paste0("biofertilizer_product_formulation_", i) ]])
      if(is.null( prodbiofer[[i]] ) || length(  prodbiofer[[i]])==0 ) prodbiofer[[i]] <- ""
      if(is.null( prodbiofer[[i]]=="Other")) prodbiofer[[i]] <- paste(input[[  paste0("inoculation_method_name_", i)  ]])
      daysbiofer[[i]] <-   paste(input[[ paste0("biofertilizer_days_sowing_after_rhizobium_inocculation_", i) ]])

    }


    sdate <-unlist(sdate)
    edate <-  unlist(edate)
    strain <- unlist(strain)
    quant <- unlist(quant)
    uni <- unlist(uni)
    method <- unlist(method)
    prodbiofer <- unlist(prodbiofer)
    daysbiofer <- unlist(daysbiofer)

    biofer <- c(  sdate , edate ,  strain , quant , uni , method, prodbiofer , daysbiofer )
    temp <- t(biofer)
    bioferdt <- as.data.frame(temp)

    for(i in 1:nbio){
      #a1 <- paste('Number of applications', 1:i )
      a2 <-paste('Biofertilizer control Start date', 1:i)
      a3 <-paste('Biofertilizer control End date', 1:i )
      a4 <-paste('Rhizobium inoculum strain', 1:i )
      a5 <-paste('Biofertilizer quantity inoculated', 1:i)
      a6 <-paste('Biofertilizer quantity inoculated unit', 1:i)
      a7 <-paste('Biofertilizer Inoculation method', 1:i )
      a8 <-paste('Biofertilizer Product formulation', 1:i )
      a9<- paste('Days to sowing after Rhizobium inocculation', 1:i )
    }
    bioferNames <- c(a2, a3, a4, a5, a6, a7, a8, a9)

    names(bioferdt) <- bioferNames

    out <- merge(out, bioferdt, by = 0, all = TRUE)[-1]


  })

  ### nutrient

  dt_nutrient <- reactive ({

      out <- fb_agrofims()

      a1<- a2<-  a3<- a4<- a5<- a6<- a7<- a8<- a9<- " "
       h1<- c("Fertilizer amount applied: Organic Start date 1" ,
              "Fertilizer amount applied Organic End date 1",
              "Fertilizer amount applied Organic Type 1",
              "Fertilizer amount applied Organic Unit 1",
              "Fertilizer amount applied Organic Technique 1",
              "Fertilizer amount applied Organic Implement 1",
              "Fertilizer amount applied Organic Rate 1",
              "Fertilizer amount applied Organic Rate (unit) 1" ,
              "Fertilizer amount applied Organic Nutrient Content 1")

      dta <- data.frame(a1,a2,a3,a4,a5,a6,a7,a8,a9, stringsAsFactors = FALSE)
      names(dta) < h1


       b1<- b2<-  b3<- b4<- b5<- b6<- b7<- b8<- b9<- " "
      h2<- c("Fertilizer amount applied: Inorganic Start date 1" ,
            "Fertilizer amount applied: Inorganic End date 1",
             "Fertilizer amount applied: Inorganic Inorganic 1",
            "Fertilizer amount applied: Inorganic Unit 1",
               "Fertilizer amount applied: Inorganic Technique 1",
             "Fertilizer amount applied: Inorganic Implement 1",
               "Fertilizer amount applied: Inorganic Rate 1",
               "Fertilizer amount applied: Inorganic Rate (unit) 1",
              "Fertilizer amount applied: Inorganic Nutrient Content 1")

      dtb <- data.frame(b1,b2,b3,b4,b5,b6,b7,b8,b9, stringsAsFactors = FALSE)
      names(dtb) < h2


      c1<- c2<-  c3<- c4<- c5<- c6<- c7<- c8<- c9<- " "

      h3<- c(
        "Fertilizer amount applied: Green manure Start date 1",
        "Fertilizer amount applied: Green manure End date 1" ,
        "Fertilizer amount applied: Green manure Green manure 1" ,
        "Fertilizer amount applied: Green manure Unit 1",
        "Fertilizer amount applied: Green manure Technique 1",
        "Fertilizer amount applied: Green manure Implement 1",
        "Fertilizer amount applied: Green manure Rate 1",
        "Fertilizer amount applied: Green manure Rate (unit) 1" ,
        "Fertilizer amount applied: Green manure Nutrient Content 1"
      )

      dtc <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9, stringsAsFactors = FALSE)
      names(dtc) < h3

      dtotal <- data.frame(a1,a2,a3,a4,a5,a6,a7,a8,a9,b1,b2,b3,b4,b5,b6,b7,b8,b9,c1,c2,c3,c4,c5,c6,c7,c8,c9, stringsAsFactors = FALSE )
      names(dtotal)<- c(h1,h2,h3)
      # #organic
      # #norg <- input$numApplications_Organic
      # norg <- 1
      #
      # for(i in 1:norg){
      #
      #   norgsdate <-  paste(input[[  paste0("nutrient_start_date_", "Organic" , "_", norg)    ]])
      #   print(norgsdate)
      #   norgedate <-  paste(input[[  paste0("nutrient_end_date_", "Organic" , "_", norg)  ]])
      #   print(norgedate)
      #   norgtotino <- paste(input[[  paste0("nutrient_app_rate_", "Organic" , "_", norg)  ]])
      #   norgtotinounit <- paste(input[[ paste0("nutrient_app_rate_unit_", "Organic" , "_", norg) ]])
      #   if(is.null( norgtotinounit) || length(norgtotinounit)) norgtotinounit <- ""
      #   norgrecino <-  paste(input[[ paste0("nutrient_recommended_rate_", "Organic" , "_", norg) ]])
      #   norgrecinounit  <- paste(input[[ paste0("nutrient_recommended_rate_unit_", "Organic" , "_", norg) ]])
      #   if(is.null( norgrecinounit) || is.null( norgrecinounit)) norgrecinounit <- ""
      #   norgperecinounit  <- paste(input[[ paste0("perc_recomm_rate_", "Organic" , "_", norg) ]])
      #
      #   #nitrogen1
      #   #`Fertilizer amount applied: Organic # of app 1'  paste(input[[  paste0("nutrient_start_date_", type, "_", 1)    ]])
      #   `Fertilizer amount applied: Organic Start date 1` <- paste(input[[paste0("nutrient_start_date_", "Organic", "_", norg)    ]])
      #   `Fertilizer amount applied: Organic End date 1` <-  paste(input[[paste0("nutrient_end_date_", "Organic", "_", norg)  ]])
      #   `Fertilizer amount applied: Organic Type 1` <-  paste(input[[paste0("fert_nit_type1_", "Organic", "_", norg)  ]])
      #   if(is.null(  `Fertilizer amount applied: Organic Type 1` ) || length( `Fertilizer amount applied: Organic Type 1`)==0)  `Fertilizer amount applied: Organic Type 1` <- ""
      #
      #
      #   `Fertilizer amount applied: Organic Unit 1`<-  paste(input[[paste0("fert_nit_type1_unit_", "Organic", "_", norg) ]])
      #   if(is.null( `Fertilizer amount applied: Organic Unit 1`) || length(`Fertilizer amount applied: Organic Unit 1`)) `Fertilizer amount applied: Organic Unit 1` <- ""
      #
      #   `Fertilizer amount applied: Organic Technique 1` <- paste(input[[ paste0("fertilizer_nit_application_technique1_", "Organic", "_", norg)   ]])
      #   if(is.null( `Fertilizer amount applied: Organic Technique 1`) || length(`Fertilizer amount applied: Organic Technique 1`)) `Fertilizer amount applied: Organic Technique 1` <- ""
      #
      #   `Fertilizer amount applied: Organic Implement 1` <- paste(input[[paste0("fertilizer_nit_implement1_", "Organic", "_", norg)   ]])
      #
      #   `Fertilizer amount applied: Organic Rate 1`<-  paste(input[[ paste0("fert_nit_amountApplied1_", "Organic", "_", norg)    ]])
      #   `Fertilizer amount applied: Organic Rate (unit) 1` <- paste(input[[paste0("fert_nit_amountAppliedScale1_", "Organic", "_", norg)    ]])
      #   if(is.null( `Fertilizer amount applied: Organic Rate (unit) 1`) || length(`Fertilizer amount applied: Organic Rate (unit) 1`)==0) `Fertilizer amount applied: Organic Rate (unit) 1` <- ""
      #
      #   `Fertilizer amount applied: Organic Nutrient Content 1`<-  paste(input[[ paste0("fert_nit_nutrientContent1_", "Organic", "_", norg)   ]])
      # }
      # morg <- data.frame( norgsdate,norgedate,  norgtotino, norgtotinounit, norgrecino, norgrecinounit, norgperecinounit, stringsAsFactors = FALSE )
      # print(morg)
      # #`Fertilizer amount applied: Organic # of app 1'  paste(input[[  paste0("nutrient_start_date_", type, "_", 1)    ]])
      # dtorg <- data.frame(`Fertilizer amount applied: Organic Start date 1`,
      #                     `Fertilizer amount applied: Organic End date 1`,
      #                     `Fertilizer amount applied: Organic Type 1`,
      #                     `Fertilizer amount applied: Organic Unit 1`,
      #                     `Fertilizer amount applied: Organic Technique 1`,
      #                     `Fertilizer amount applied: Organic Implement 1`,
      #                     `Fertilizer amount applied: Organic Rate 1`,
      #                     `Fertilizer amount applied: Organic Rate (unit) 1` ,
      #                     `Fertilizer amount applied: Organic Nutrient Content 1`,stringsAsFactors = FALSE)
      #
      # print(dtorg)
      # tot_dtorg <- data.frame(morg, dtorg, stringsAsFactors = FALSE)
      #
      # #inorganic
      # #ninorg <- input$numApplications_Inorganic
      # ninorg <- 1
      # for(i in 1:ninorg){
      #
      #   #in1inor<- paste(input[[	numApplications_Inorganic	]])
      #   in2inor<- paste(input[[	paste0("nutrient_start_date_", "Inorganic", "_", ninorg)	]])
      #   in3inor<- paste(input[[	paste0("nutrient_end_date_", "Inorganic", "_", ninorg)	]])
      #   in3inor<- paste(input[[	paste0("nutrient_app_rate_", "Inorganic", "_", ninorg)	]])
      #   in4inor<- paste(input[[	paste0("nutrient_app_rate_unit_", "Inorganic", "_", ninorg)	]])
      #   if(is.null(in4inor)) in4inor <- ""
      #
      #   in5inor<- paste(input[[	paste0("nutrient_recommended_rate_", "Inorganic", "_", ninorg)	]])
      #   in6inor<- paste(input[[	paste0("fertilizer_recommended_rate_unit_", "Inorganic", "_", ninorg)	]])
      #   in7inor<- paste(input[[	paste0("perc_recomm_rate_", "Inorganic", "_", ninorg)	]])
      #   if(is.null(in7inor))  in7inor <- ""
      #
      #   #`Fertilizer amount applied: Inorganic # of app 1` <- paste(input[[	paste0("nutrientApplied_nit_numApps1_", "Inorganic", "_", ninorg) ]])
      #   `Fertilizer amount applied: Inorganic Start date 1` <- paste(input[[	paste0("fert_nit_start_date1_", "Inorganic", "_", ninorg) ]])
      #   `Fertilizer amount applied: Inorganic End date 1`<- paste(input[[	paste0("fert_nit_end_date1_", "Inorganic", "_", ninorg) ]])
      #   `Fertilizer amount applied: Inorganic "Inorganic" 1`<- paste(input[[	paste0("fert_nit_type1_", "Inorganic", "_", ninorg) ]])
      #   `Fertilizer amount applied: Inorganic Unit 1`<- paste(input[[	paste0("fert_nit_type1_unit_", "Inorganic", "_", ninorg) ]])
      #   `Fertilizer amount applied: Inorganic Technique 1`<- paste(input[[	paste0("fertilizer_nit_application_technique1_", "Inorganic", "_", ninorg) ]])
      #   if(is.null(`Fertilizer amount applied: Inorganic Technique 1`) || length(`Fertilizer amount applied: Inorganic Technique 1`)==0 ) `Fertilizer amount applied: Inorganic Technique 1`<- ""
      #   if(`Fertilizer amount applied: Inorganic Technique 1` == "Other") `Fertilizer amount applied: Inorganic Technique 1`<- ""
      #   `Fertilizer amount applied: Inorganic Implement 1`<- paste(input[[	paste0("fertilizer_nit_implement1_", "Inorganic", "_", ninorg) ]])
      #   `Fertilizer amount applied: Inorganic Rate 1`<- paste(input[[	paste0("fert_nit_amountApplied1_", "Inorganic", "_", ninorg) ]])
      #   `Fertilizer amount applied: Inorganic Rate (unit) 1`<- paste(input[[	paste0("fert_nit_amountAppliedScale1_", "Inorganic", "_", ninorg) ]])
      #   if(is.null(`Fertilizer amount applied: Inorganic Rate (unit) 1`) || length(`Fertilizer amount applied: Inorganic Rate (unit) 1`)==0  ) `Fertilizer amount applied: Inorganic Rate (unit) 1`<- ""
      #   if(`Fertilizer amount applied: Inorganic Rate (unit) 1`== "Other") `Fertilizer amount applied: Inorganic Rate (unit) 1` <- ""
      #   `Fertilizer amount applied: Inorganic Nutrient Content 1`<- paste(input[[	paste0("fert_nit_nutrientContent1_", "Inorganic", "_", ninorg) ]])
      #
      # }
      # minorg <- data.frame( in1inor , in2inor  ,  in3inor, in4inor, in5inor, in6inor, ininor, stringsAsFactors = FALSE )
      # dtinor<- data.frame(
      #                      `Fertilizer amount applied: Inorganic Start date 1` ,
      #                      `Fertilizer amount applied: Inorganic End date 1`,
      #                      `Fertilizer amount applied: Inorganic "Inorganic 1`,
      #                      `Fertilizer amount applied: Inorganic Unit 1`,
      #                      `Fertilizer amount applied: Inorganic Technique 1`,
      #                      `Fertilizer amount applied: Inorganic Implement 1`,
      #                      `Fertilizer amount applied: Inorganic Rate 1`,
      #                      `Fertilizer amount applied: Inorganic Rate (unit) 1`,
      #                      `Fertilizer amount applied: Inorganic Nutrient Content 1` ,stringsAsFactors = FALSE)
      #
      # tot_inor <- data.frame(minorg, dtinor, stringsAsFactors = FALSE)
      #
      # #Green manure ####
      # #ngreen<- input$numApplications_Green_manure
      # ngreen <- 1
      # for(i in 1:ngreen){
      #
      #   green1inor <- paste(input[[	paste0("nutrient_start_date_", "Green manure", "_", ngreen)	]])
      #   green2inor<- paste(input[[	paste0("nutrient_end_date_", "Green manure" , "_", ngreen)	]])
      #   green3inor<- paste(input[[	paste0("nutrient_app_rate_", "Green manure", "_", ngreen)	]])
      #   green4inor<- paste(input[[	paste0("nutrient_app_rate_unit_", "Green manure", "_", ngreen)	]])
      #   if(is.null(green4inor) ||  length(green4inor)==0 ) green4inor <- ""
      #   green5inor<- paste(input[[	paste0("nutrient_recommended_rate_", "Green manure", "_", ngreen)	]])
      #   green6inor<- paste(input[[	paste0("fertilizer_recommended_rate_unit_","Green manure", "_", ngreen)	]])
      #   if(is.null(green6inor) ||   length(green6inor)==0) green6inor <- ""
      #   green7inor<- paste(input[[	paste0("perc_recomm_rate_", "Green manure", "_", ngreen)	]])
      #
      #
      #   #`Fertilizer amount applied: Green manure # of app 1` <-	paste0("nutrientApplied_nit_numApps1_", "Green manure", "_", ngreen)
      #   `Fertilizer amount applied: Green manure Start date 1`<-	paste0("fert_nit_start_date1_", "Green manure", "_", ngreen)
      #   `Fertilizer amount applied: Green manure End date 1` <-	paste0("fert_nit_end_date1_", "Green manure", "_", ngreen)
      #   `Fertilizer amount applied: Green manure Green manure type 1` <-	paste0("fert_nit_type1_", "Green manure", "_", ngreen)
      #   if(is.null(`Fertilizer amount applied: Green manure Green manure type 1`) ||  length(`Fertilizer amount applied: Green manure Green manure type 1`)==0     ) `Fertilizer amount applied: Green manure Green manure type 1`<- ""
      #   if(`Fertilizer amount applied: Green manure Green manure type 1` == "Other") `Fertilizer amount applied: Green manure Green manure type 1`<- ""
      #
      #   `Fertilizer amount applied: Green manure Unit 1`<-	paste0("fert_nit_type1_unit_", "Green manure", "_", ngreen)
      #   `Fertilizer amount applied: Green manure Technique 1`<-	paste0("fertilizer_nit_application_technique1_", "Green manure", "_", ngreen)
      #   `Fertilizer amount applied: Green manure Implement 1` <-	paste0("fertilizer_nit_implement1_", "Green manure", "_", ngreen)
      #   `Fertilizer amount applied: Green manure Rate 1`<-	paste0("fert_nit_amountApplied1_", "Green manure", "_", ngreen)
      #   `Fertilizer amount applied: Green manure Rate (unit) 1` <-	paste0("fert_nit_amountAppliedScale1_", "Green manure", "_", ngreen)
      #   if(is.null(`Fertilizer amount applied: Green manure Rate (unit) 1`) || length(`Fertilizer amount applied: Green manure Rate (unit) 1`)==0 )`Fertilizer amount applied: Green manure Rate (unit) 1` <- ""
      #   `Fertilizer amount applied: Green manure Nutrient Content 1` <-	paste0("fert_nit_nutrientContent1_", "Green manure", "_", ngreen)
      # }
      # mgreen <- data.frame( green1inor , green2inor  ,  green3inor, green4inor, green5inor, green6inor, green7inor, stringsAsFactors = FALSE )
      # dtgreen <- data.frame (
      #                        `Fertilizer amount applied: Green manure Start date 1`,
      #                        `Fertilizer amount applied: Green manure End date 1` ,
      #                        `Fertilizer amount applied: Green manure Green manure 1` ,
      #                        `Fertilizer amount applied: Green manure Unit 1`,
      #                        `Fertilizer amount applied: Green manure Technique 1`,
      #                        `Fertilizer amount applied: Green manure Implement 1`,
      #                        `Fertilizer amount applied: Green manure Rate 1`,
      #                        `Fertilizer amount applied: Green manure Rate (unit) 1` ,
      #                        `Fertilizer amount applied: Green manure Nutrient Content 1` ,stringsAsFactors = FALSE)
      #
      # tot_green <- data.frame(mgreen, dtgreen, stringsAsFactors = FALSE)

      #subtot <- data.frame(tot_dtorg,tot_inor, dtgreen, stringsAsFactors = FALSE)
      out <- merge(out, dtotal, by = 0, all = TRUE)[-1]
      out

  })


  ### pest and disease
  dt_pestdis <- reactive({


    out <- fb_agrofims()

    npest<- input$numApplicationsPestDisease

    sdate <- edate <- tech<- forname <- depth <- amount <- uni <- list()
    for(i in 1:npest) {

      sdate[[i]] <-  paste(input[[paste0("pestcontrol_start_date_", i) ]])
      edate[[i]] <-  paste(input[[paste0("pestcontrol_start_date_", i) ]])
      tech[[i]]  <-  paste(input[[paste0("pest_control_technique_",i) ]])
      print(tech[[i]])
      if(is.null( tech[[i]] ) || length(tech[[i]]) ==0) tech[[i]] <-  ""
      #if(  tech[[i]]]=="Other"  )  tech[[i]] <-  ""
      forname[[i]] <- paste(input[[ paste0("pest_name_form_", i) ]])
      depth[[i]] <-  paste(input[[ paste0("pesticide_application_depth_", i) ]])
      amount[[i]] <- paste(input[[ paste0("pesticide_amount_", i) ]])
      uni[[i]] <-   paste(input[[ paste0("pesticide_amount_unit_", i) ]])
      print(uni[[i]])
      if(is.null(uni[[i]]) || length(uni[[i]])==0 ) uni[[i]]  <- ""

    }

    sdate <-unlist(sdate)
    edate  <-unlist( edate)
    tech  <-unlist(tech)
    forname<-unlist( forname)
    depth  <-unlist(depth)
    amount  <-unlist(amount)
    uni <-unlist(uni)

    pest <- c(sdate, edate, tech, forname, depth, amount, uni)
    temp <- t(pest)
    pestdt <- as.data.frame(temp)

    for(i in 1:npest){
      #a1 <- paste('Number of applications', 1:i )
      a2 <-paste('Pest control Start date', 1:i)
      a3 <-paste('Pest control End date', 1:i )
      a4 <-paste('Pest control Technique', 1:i )
      a5 <-paste('Pest control Pesticide name / formulation', 1:i)
      a6 <-paste('Pest control Pesticide application depth', 1:i)
      a7 <-paste('Pest control Pesticide amount', 1:i )
      a8 <-paste('Pest control Pesticide amount unit', 1:i )
    }
    pestNames <- c(a2, a3, a4, a5, a6, a7, a8)

    names(pestdt) <- pestNames

    out <- merge(out, pestdt, by = 0, all = TRUE)[-1]


    # out<-pestdis (input$disease_observation_date,input$disease_name,
    #           input$disease_plant_parts_affected,input$disease_percentage_experiement_affected,
    #           input$disease_damages_notes,input$disease_notes,input$pest_type,input$pest_name,
    #           input$pest_damage_notes,input$pest_notes,input$pestcontrol_start_date,input$pestcontrol_end_date,
    #           input$pest_control_technique,input$pesticide_application_depth,input$pesticide_amount,
    #           "", #input$pest_image,
    #           input$pest_control_applications_totnumber,input$pest_control_details,input$chemical_pest_control_equipment,
    #           input$pesticide_implement_make,input$pesticide_implement_model,input$pesticide_animal_traction,
    #           input$pesticide_humanPowered,input$pesticide_motorized_traction)
    # out


  })


  ################################End agrofeatures #############################################################






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



  metadata_dt2 <- reactive({

    c1 <- c('Experiment ID', input$experimentId)
    c2 <- c('Experiment name', input$experimentName )
    c3 <- c('Experiment project name', input$experimentProjectName)
    c4 <- c('Experiment start date', paste(input$fbDesign_project_time_line[1]) )
    c5 <- c('Experiment end date', paste(input$fbDesign_project_time_line[2]))

    xdur <- interval(ymd(input$fbDesign_project_time_line[1]),ymd(input$fbDesign_project_time_line[2]))
    xdur <- xdur %/% months(1)
    xdur <- paste(xdur," months", sep = "")


    c6 <- c('Experiment duration', xdur)
    vTypeExperiment <- ""
    if(!is.null(input$designFieldbook_typeExperiment)) vTypeExperiment <- input$designFieldbook_typeExperiment

    c7 <- c('Type of experiment', vTypeExperiment)
    c8 <- c('Experiment objective', input$experimentObj)

    vfundAgenType <- ""
    vfundName <-""
    if(!is.null(input$designFieldbook_fundAgencyType)) {
      vfundAgenType <- paste(input$designFieldbook_fundAgencyType, collapse = ",")
      vn <- length(input$designFieldbook_fundAgencyType)
      vfundName <- input[[paste0("fundName_", 1)]]

      for(i in 2:vn){
        vfundName <- paste(vfundName, ",", input[[paste0("fundName_", i)]])
      }
    }

    c9 <- c('Funding agency type', vfundAgenType)
    c10 <- c('Funding agency name', vfundName)

    vNumPrEnt <- ""
    vPrEnt <- c()
    vContCenter <- c()
    vcontCRP <- c()
    vPrName <- c()



    if(is.numeric(input$numProjEntity)){
      vNumPrEnt <- input$numProjEntity
      vn <- input$numProjEntity

      for(i in 1:vn){
        aux <- input[[paste0("projEntity_", i)]]
        if(!is.null(aux)){
          vPrEnt <- c(vPrEnt,aux)
          if(aux == "Other"){
            vPrName <- c(vPrName, input[[paste0("contOtherCenter_", i)]])
          }
          else{
            if(!is.null(input[[paste0("contCenter_", i)]])){
              vContCenter <- c(vContCenter, input[[paste0("contCenter_", i)]])
            }
            else{
              vContCenter <- c(vContCenter, "")
            }
            if(!is.null(input[[paste0("contCRP_", i)]])){
              vcontCRP <- c(vcontCRP, input[[paste0("contCRP_", i)]])
            }
            else{
              vcontCRP <- c(vcontCRP, "")
            }
          }
        }
      }
      vPrEnt <- paste(vPrEnt, collapse = ",")
      vContCenter <- paste(vContCenter, collapse = ",")
      vcontCRP <- paste(vcontCRP, collapse = ",")
      vPrName <- paste(vPrName, collapse = ",")
    }

    c11 <- c('Number of project management entities',vNumPrEnt )
    c12 <- c('Project management entity',vPrEnt )
    c13 <- c('Contribuitor center',vContCenter )
    c14 <- c('Contribuitor CRP', vcontCRP)
    c15 <- c('Project management entity name', vPrName )



    vleadOrgType <- c()
    vleadPerson <- c()
    vleadOrgName <- c()

    if(is.numeric(input$numLeads)){
      vn <- input$numProjEntity
      for(i in 1:vn){
        aux <- input[[paste0("projLeadEnt_", i)]]
        if(!is.null(aux)){
          if(aux == "Other"){
            if(is.null(input[[paste0("lead_org_type_1_", i)]])) vleadOrgType <- c(vleadOrgType,aux)
            else vleadOrgType <- c(vleadOrgType, input[[paste0("lead_org_type_1_", i)]])
            vleadOrgName <- c(vleadOrgName, input[[paste0("leadNameOther_", i)]])
          }
          else{
            vleadOrgType <- c(vleadOrgType,aux)
            if(!is.null(input[[paste0("tLeadCenter_", i)]])){
              vleadOrgName <- c(vleadOrgName, input[[paste0("tLeadCenter_", i)]])
            }
            else{
              vleadOrgName <- c(vleadOrgName, "")
            }
          }
          vleadPerson <- c(vleadPerson, input[[paste0("expLead_", i)]])
        }
      }

      vleadOrgType <- paste(vleadOrgType, collapse = ";")
      vleadOrgName <- paste(vleadOrgName, collapse = ";")
      vleadPerson <- paste(vleadPerson, collapse = ";")
    }

    c16 <- c('Experiment, lead organization type',vleadOrgType )
    c17 <- c('Experiment lead person / Primary Investigator', vleadOrgName)
    c18 <- c('Experiment, lead organization name',vleadPerson )

    np <- input$npersons
    vperType <- c()
    vperfname <- c()
    vperlname <- c()
    vperemail <- c()
    vperAff <- c()
    vperOrcid <- c()
    vpercountry <- c()

    for(i  in 1:np){
      if(is.null(input[[paste0("personnel",i,"Type")]])) vperType <- c(vperType, "")
      else vperType <- c(vperType, input[[paste0("personnel",i,"Type")]])

      vperfname <- c(vperfname, input[[paste0("person",i,"FirstName")]])
      vperlname <- c(vperlname, input[[paste0("person",i,"LastName")]])

      if(is.null(input[[paste0("person",i,"Affiliation")]])) vperAff <- c(vperAff, "")
      else{
        if(input[[paste0("person",i,"Affiliation")]] == "CGIAR Center"){
          if(is.null(input[[paste0("person",i,"Center")]])) vperAff <- c(vperAff, "CGIAR Center")
          else vperAff <- c(vperAff, input[[paste0("person",i,"Center")]])
        }
        else{
          vperAff <- c(vperAff, input[[paste0("person",i,"CenterOther")]])
        }
      }

      vperemail <- c(vperemail, input[[paste0("person",i,"Email")]])
      vperOrcid <- c(vperOrcid, input[[paste0("person",i,"ORCID")]])

      if(is.null(input[[paste0("person",i,"Country")]])) vpercountry <- c(vpercountry, "")
      else vpercountry <- c(vpercountry, input[[paste0("person",i,"Country")]])
    }

    vperType <- paste(vperType, collapse = ",")
    vperfname <-paste(vperfname, collapse = ",")
    vperlname <- paste(vperlname, collapse = ",")
    vperemail <- paste(vperemail, collapse = ",")
    vperAff <- paste(vperAff, collapse = ",")
    vperOrcid <- paste(vperOrcid, collapse = ",")
    vpercountry <- paste(vpercountry, collapse = ",")

    c19 <- c('Person type',vperType )
    c20 <- c('Person, first name',vperfname )
    c21 <- c('Person, last name', vperlname)
    c22 <- c('Person, email', vperemail)
    c23 <- c('Person, affiliation', vperAff)
    c24 <- c('Person, ORCID',vperOrcid )
    c25 <- c('Country in which active', vpercountry)


    vsitetype <- ""
    vsitename <- ""
    vsiteId <- ""
    vsiteCountry <- ""
    vsiteadmin1 <- ""
    vsiteadmin2 <- ""
    vsiteVillage <- ""
    vsitenear <- ""
    vsiteElev <- ""
    vsiteLat <- ""
    visteLon <- ""

    if(!is.null(input$fbDesign_countryTrial) && !is.null(input$designFieldbook_sites)){
      vsiteCountry <- input$fbDesign_countryTrial

      xpath <- fbglobal::get_base_dir()
      xfp <- file.path(path, "table_sites_agrofims.rds")

      xaux <- input$designFieldbook_sites
      # print(xaux)
      # xstart <- which(strsplit(xaux, "")[[1]]=="(")
      # vsiteId <- substr(xaux, xstart+1, nchar(xaux)-1)
      vsiteId <- xaux

      x_sites_data <- readRDS(file = xfp)
      data <- dplyr::filter(x_sites_data, shortn==vsiteId)
      if(nrow(data) != 0){
        xsite <- data[1,]
        vsitetype <- xsite$Type
        vsitename <- xsite$local
        vsiteadmin1 <- xsite$adm1
        vsiteadmin2 <- xsite$adm2
        vsiteVillage <- xsite$village
        vsitenear <- xsite$nearpop
        vsiteElev <- xsite$elev
        vsiteLat <- xsite$latd
        visteLon <- xsite$lond
      }


    }

    c26 <- c('Site type',vsitetype)
    c27 <- c('Site name',vsitename)
    c28 <- c('Site ID', vsiteId)
    c29 <- c('Country name', vsiteCountry)
    c30 <- c('Site, first-level administrative division name',vsiteadmin1 )
    c31 <- c('Site, second-level administrative division name',vsiteadmin2 )
    c32 <- c('Village name', vsiteVillage)
    c33 <- c('Nearest populated place', vsitenear)
    c34 <- c('Site elevation',vsiteElev )
    c35 <- c('Site latitude (in decimal degrees)', vsiteLat)
    c36 <- c('Site longitude (in decimal degrees)',visteLon )

    vHighLevel <- ""
    if(!is.null(input$fbDesign_inHighLevel)) vHighLevel <- input$fbDesign_inHighLevel

    c37 <- c('Higher-level landform',vHighLevel)

    vSiteVegetation <- ""
    if(!is.null(input$fbDesign_inSiteVegetation)) vSiteVegetation <- paste(input$fbDesign_inSiteVegetation, collapse = ",")

    c38 <- c('Vegetation surrounding the experimental site', vSiteVegetation)
    c39 <- c('Site description notes', input$inSiteDescNotes)

    c40 <- c('Cropping type', input$croppingType )

    vCropCommon <- ""
    if(!is.null(input$cropCommonNameMono)) vCropCommon <- input$cropCommonNameMono

    c41 <- c('Crop common name',vCropCommon )
    c42 <- c('Crop latin name', input$cropLatinNameMono)

    vCropVarName <- ""
    if(!is.null(input$cropVarietyNameMono)) vCropVarName <- paste(input$cropVarietyNameMono, collapse = ",")

    c43 <- c('Crop variety name', vCropVarName)
    c44 <- c('Cultivar name',input$cultivarNameMono )
    c45 <- c('Crop local name', input$monoCropLocalName)

    nCropPrevCrop <- ""
    nprevCropName <- ""
    nprevCropVar <- ""

    if(is.numeric(input$numPreviousCrop)) nCropPrevCrop <-input$numPreviousCrop
    if(!is.null(input$prevCropName)) nprevCropName <- paste(input$prevCropName, collapse = ",")
    if(!is.null(input$prevCropVar)) nprevCropVar <- paste(input$prevCropVar, collapse = ",")

    c46 <- c('Number of previous crop', nCropPrevCrop)
    c47 <- c('Previous crop name', nprevCropName)
    c48 <- c('Previous crop variety',nprevCropVar )

    # c56 <- c('Subject', )
    # c57 <- c('Keywords', )
    # c58 <- c('Embargo end date', )

    df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,
                              c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                              c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                              c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,
                              c41,c42,c43,c44,c45,c46,c47, c48)
    var_metadata <-  t(df_metadata)
    # print(df_metadata)

  })


  factor_dt2 <- reactive({


    vinfExp <-""
    if(!is.null(input$info_experiment_unit)) vinfExp <- input$info_experiment_unit


    c1 <- c('Information on experimental unit',vinfExp )

    vfarea <-""
    vfexpmaxwidth <- ""
    vfexpmaxlength <- ""
    vpdiam <- ""
    vpdpth <- ""
    if(vinfExp == "plot"  ){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_plot_width_unit))  wunit <- input$expt_plot_width_unit
      if(!is.null(input$expt_plot_length_unit))  lunit <- input$expt_plot_length_unit
      vfexpmaxwidth <- paste0(input$expt_plot_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_plot_length, " " , lunit)
    }
    else if(vinfExp == "field"){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_field_width_unit))  wunit <- input$expt_field_width_unit
      if(!is.null(input$expt_field_length_unit))  lunit <- input$expt_field_length_unit
      vfexpmaxwidth <- paste0(input$expt_field_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_field_length, " " , lunit)
    }
    else if(vinfExp == "pot"){
      wunit <- ""
      lunit <- ""
      if(!is.null(input$pot_diameter_unit))  wunit <- input$pot_diameter_unit
      if(!is.null(input$pot_depth_unit))  lunit <- input$pot_depth_unit
      vpdiam <- paste0(input$pot_diameter, " " , wunit)
      vpdpth <- paste0(input$pot_depth, " " , lunit)

    }

    c2 <- c('Field area',vfarea )
    c3 <- c('Experimental field maximum width', vfexpmaxwidth)
    c4 <- c('Experimental field maximum length', vfexpmaxlength)
    c5 <- c('Pot diameter',vpdiam )
    c6 <- c('Pot depth',vpdpth )
    c7 <- c('Experimental design', input$designFieldbook_agrofims)
    c8 <- c('Experimental design abbreviation', "")
    c9 <- c('Number of replications', input$designFieldbook_agrofims_r)
    c40 <- c('Number of factors', input$nfactors_hdafims)

    levels1 <- c("NA", "NA", "NA", "NA","NA")
    levels2 <- c("NA", "NA", "NA", "NA","NA")
    levels3 <- c("NA", "NA", "NA", "NA","NA")
    levels4 <- c("NA", "NA", "NA", "NA","NA")
    levels5 <- c("NA", "NA", "NA", "NA","NA")
    levelsDt <- data.table(levels1,levels2,levels3,levels4,levels5)


    nf <- input$nfactors_hdafims

    factors <- c("NA", "NA", "NA", "NA","NA")
    for(i in 1:nf){
      g1 <- input[[paste0("sel" , i, "_2" )]]
      g2 <- input[[paste0("sel" , i, "_3" )]]
      if(!is.null(g1) && !is.null(g2)){
        factors[i] <- paste0(g1, " ", g2)

        g3 <- input[[paste0("sel" , i, "_3" )]]
        ls1 <- input[[paste0("numLevels_", i)]]
        if(is_numeric(ls1) && !is.null(g3)){
          if (ls1>5) ls1 <- 5 #max5
          if(g3 %like% "date"){
            for(j in 1:ls1){
              sdate <- input[[paste0("factor_start_date_",i, "_", j)]]
              edate <- input[[paste0("factor_end_date_",i, "_", j)]]
              levelsDt[i,j] <- paste0(sdate, " - ", edate)
            }
          }
          else{
            nl <- input[[paste0("levels_",i)]]
            count <- 1
            for(lv in nl){
              if(count <= 5){
                if(is.null(input[[paste0("funits_", i)]])){
                  levelsDt[i,count] <- lv
                }
                else{
                  levelsDt[i,count]<- paste0(lv, " ", input[[paste0("funits_", i)]])
                }
              }
              count <- count + 1
            }
          }
        }

      }

    }


    c10 <- c('Factor 1',factors[1])
    c11 <- c('Factor 1 - level 1',levelsDt[1,1])
    c12 <- c('Factor 1 - level 2',levelsDt[1,2] )
    c13 <- c('Factor 1 - level 3',levelsDt[1,3])
    c14 <- c('Factor 1 - level 4',levelsDt[1,4] )
    c15 <- c('Factor 1 - level 5',levelsDt[1,5] )

    c16 <- c('Factor 2', factors[2])
    c17 <- c('Factor 2 - level 1',levelsDt[2,1])
    c18 <- c('Factor 2 - level 2',levelsDt[2,2])
    c19 <- c('Factor 2 - level 3',levelsDt[2,3])
    c20 <- c('Factor 2 - level 4',levelsDt[2,4] )
    c21 <- c('Factor 2 - level 5',levelsDt[2,5] )

    c22 <- c('Factor 3', factors[3])
    c23 <- c('Factor 3 - level 1',levelsDt[3,1] )
    c24 <- c('Factor 3 - level 2',levelsDt[3,2] )
    c25 <- c('Factor 3 - level 3',levelsDt[3,3] )
    c26 <- c('Factor 3 - level 4',levelsDt[3,4] )
    c27 <- c('Factor 3 - level 5',levelsDt[3,5] )

    c28 <- c('Factor 4', factors[4] )
    c29 <- c('Factor 4 - level 1',levelsDt[4,1])
    c30 <- c('Factor 4 - level 2',levelsDt[4,2] )
    c31 <- c('Factor 4 - level 3',levelsDt[4,3])
    c32 <- c('Factor 4 - level 4',levelsDt[4,4] )
    c33 <- c('Factor 4 - level 5',levelsDt[4,5])

    c34 <- c('Factor 5', factors[5])
    c35 <- c('Factor 5 - level 1',levelsDt[5,1] )
    c36 <- c('Factor 5 - level 2',levelsDt[5,2])
    c37 <- c('Factor 5 - level 3',levelsDt[5,3])
    c38 <- c('Factor 5 - level 4',levelsDt[5,4] )
    c39 <- c('Factor 5 - level 5',levelsDt[5,5])


    df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c40,c10,
                              c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                              c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                              c31,c32,c33,c34,c35,c36,c37,c38,c39)
    var_metadata <-  t(df_metadata)
    # print(df_metadata)

  })

  # traits_dt2 <- reactive({
  #
  #   print("trait dt")
  #   print(traits_dt())
  #
  #   a<- traitsVals$Data
  #   if(nrow(traitsVals$Data) >0){
  #     aux_dt <- dplyr::filter(traitsVals$Data, Status=="Selected")
  #     a<- aux_dt
  #   }
  #   a
  #
  # })




  ### donwload fieldbook ########################################################################
  output$downloadData <- downloadHandler(
    filename = "fileNameBook.xlsx",
    content = function(file) {

      withProgress(message = 'Downloading fieldbook', value = 0, {

        incProgress(1/10, message = "...")

        print(input$cropCommonNameMono)

      #print( input$cropsSelected)
      design <- input$designFieldbook_agrofims
      nrep <- input$designFieldbook_agrofims_r
      #
      # print("common variety name")
      # #print(input$cropVarietyNameMono)
      # print("end common var name")
      #
      weather_vars <- unlist(shinyTree::get_selected(input$designFieldbook_weatherVar_agrofims))
      print(weather_vars)
      # soil_vars <- unlist(shinyTree::get_selected(input$designFieldbook_soilVar_agrofims))
      # print(soil_vars)

      print("trait dt")
      print(traits_dt())


      fb_traits <- fb_agrofims_traits()
      #print(fb_traits)

      # agrofeatures <- dt_agrofeatures()
      #metadata <- dt_metadata_agrofims()
      metadata <- as.data.frame(metadata_dt2())
      names(metadata) <- c("Variable", "Value")
      installation <- as.data.frame(factor_dt2())
      names(installation) <- c("Variable", "Value")

      print(installation)
      trait_agrofims_dt <- trait_agrofims()
      trait_agrofims_dt<- trait_agrofims_dt[,-3]

      weather <- dt_weather_agrofims()
      print(weather)
      soil_vars <- dt_soil_agrofims()
       print(soil_vars)

      fname <- paste(file,"xlsx",sep=".")
      #wb <- openxlsx::loadWorkbook(file = fname, create = TRUE)

      wb <- createWorkbook()

        incProgress(2/20,message = "Adding fieldbook data...")

      incProgress(6/20,message = "Metadata metadata sheet...")

      openxlsx::addWorksheet(wb, "Metadata", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Metadata", x = metadata,
                               colNames = TRUE, withFilter = FALSE)


      # incProgress(7/20,message = "Adding installation sheet...")
      #
      openxlsx::addWorksheet(wb, "Variables", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Variables", x = installation,
                               colNames = TRUE, withFilter = FALSE)



      #write agrofeatures sheet
      agroFeaSelected <- input$selectAgroFeature
      agrofea_sheets <- c("Land preparation", "Mulching", "Planting","Irrigation event", "Biofertilizer", "Pest & disease", "Nutrient management event","Harvest")

      if(is.element("Land preparation", agroFeaSelected)) {

      incProgress(10/20,message = "Adding land preparation sheet...")

      dt_land <- dt_land_description()
      print(dt_land)
      openxlsx::addWorksheet(wb, "Land preparation", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Land preparation", x = dt_land ,
                               colNames = TRUE, withFilter = FALSE)

      }

      if(is.element("Mulching and residue management", agroFeaSelected)) {

      incProgress(11/20,message = "Adding mulching data...")

      dt_mulch <- dt_mulching()

      openxlsx::addWorksheet(wb, "Mulching and residue management", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Mulching and residue management", x = dt_mulch,
                               colNames = TRUE, withFilter = FALSE)


      }

      if(is.element("Planting, transplanting", agroFeaSelected)) {

        incProgress(12/20,message = "Adding planting data...")

        dt_plant <- dt_planting()

        openxlsx::addWorksheet(wb, "Planting, transplanting", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Planting, transplanting", x = dt_plant,
                                 colNames = TRUE, withFilter = FALSE)

      }

      if(is.element("Nutrient management", agroFeaSelected)) {
        dt_nut<-  dt_nutrient()
        #print(dt_nut)
        openxlsx::addWorksheet(wb, "Nutrient management", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Nutrient management", x = dt_nut,
                                 colNames = TRUE, withFilter = FALSE)

      }


      if(is.element("Biofertilizer", agroFeaSelected)) {

      incProgress(15/20,message = "Adding biofertilizer data...")

      dt_biof <- dt_bioferti()

      openxlsx::addWorksheet(wb, "Biofertilizer", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Biofertilizer", x = dt_biof,
                               colNames = TRUE, withFilter = FALSE)

      }

      if(is.element("Irrigation", agroFeaSelected)) {

        incProgress(14/20,message = "Adding irrigation data...")

        dt_irri <- dt_irrigation()
        #print(dt_irri)
        openxlsx::addWorksheet(wb, "Irrigation", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Irrigation", x = dt_irri,
                                 colNames = TRUE, withFilter = FALSE)

      }


      if(is.element("Harvest", agroFeaSelected)) {

        incProgress(13/20,message = "Adding harvest data...")

        dt_harv <- dt_harvest()

        openxlsx::addWorksheet(wb, "Harvest", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Harvest", x = dt_harv,
                                 colNames = TRUE, withFilter = FALSE)

     }

      if(is.element("Pest & disease", agroFeaSelected)) {

      incProgress(16/20,message = "Adding pest and disease data...")

      dt_pestd <- dt_pestdis()

      openxlsx::addWorksheet(wb, "Pest and disease", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Pest and disease", x = dt_pestd,
                               colNames = TRUE, withFilter = FALSE)

      }

      incProgress(9/20,message = "Adding trait list sheet...")

      # openxlsx::addWorksheet(wb, "Trait list", gridLines = TRUE)
      # openxlsx::writeDataTable(wb, "Trait list", x = trait_agrofims_dt,
      #                          colNames = TRUE, withFilter = FALSE)

      incProgress(8/20,message = "Adding fieldbook sheet...")

      openxlsx::addWorksheet(wb, "Fieldbook", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Fieldbook", x = fb_traits,
                               colNames = TRUE, withFilter = FALSE)


      if(is.null(weather) || length(weather)==0 || nrow(weather)==0  ){
        print("there is no weather data")

      } else {

        incProgress(8/10,message = "Adding weather variables sheet...")

        openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Weather", x = weather,
                                 colNames = TRUE, withFilter = FALSE)
      }

      if(is.null(soil_vars) || length(soil_vars)==0 || nrow(soil_vars)==0 ){
        print("there is no soil data")

      } else {

        incProgress(9/10,message = "Adding soil variables sheet...")
        openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Soil", x = soil_vars,
                                 colNames = TRUE, withFilter = FALSE)
      }

      incProgress(19/20,message = "Downloading file...")

      saveWorkbook(wb, file = fname , overwrite = TRUE)

      file.rename(fname, file)


    })

    },
    contentType="application/xlsx"
  )



  metadata_dt <- function(){

    c1 <- c('Experiment ID', input$experimentId)
    c2 <- c('Experiment name', input$experimentName )
    c3 <- c('Experiment project name', input$experimentProjectName)
    c4 <- c('Experiment start date', paste(input$fbDesign_project_time_line[1]) )
    c5 <- c('Experiment end date', paste(input$fbDesign_project_time_line[2]))

    xdur <- interval(ymd(input$fbDesign_project_time_line[1]),ymd(input$fbDesign_project_time_line[2]))
    xdur <- xdur %/% months(1)
    xdur <- paste(xdur," months", sep = "")


    c6 <- c('Experiment duration', xdur)
    vTypeExperiment <- ""
    if(!is.null(input$designFieldbook_typeExperiment)) vTypeExperiment <- input$designFieldbook_typeExperiment

    c7 <- c('Type of experiment', vTypeExperiment)
    c8 <- c('Experiment objective', input$experimentObj)

    vfundAgenType <- ""
    vfundName <-""
    if(!is.null(input$designFieldbook_fundAgencyType)) {
      vfundAgenType <- paste(input$designFieldbook_fundAgencyType, collapse = ",")
      vn <- length(input$designFieldbook_fundAgencyType)
      vfundName <- input[[paste0("fundName_", 1)]]

      for(i in 2:vn){
        vfundName <- paste(vfundName, ",", input[[paste0("fundName_", i)]])
      }
    }

    c9 <- c('Funding agency type', vfundAgenType)
    c10 <- c('Funding agency name', vfundName)

    vNumPrEnt <- ""
    vPrEnt <- c()
    vContCenter <- c()
    vcontCRP <- c()
    vPrName <- c()



    if(is.numeric(input$numProjEntity)){
      vNumPrEnt <- input$numProjEntity
      vn <- input$numProjEntity

      for(i in 1:vn){
        aux <- input[[paste0("projEntity_", i)]]
        if(!is.null(aux)){
          vPrEnt <- c(vPrEnt,aux)
          if(aux == "Other"){
            vPrName <- c(vPrName, input[[paste0("contOtherCenter_", i)]])
          }
          else{
            if(!is.null(input[[paste0("contCenter_", i)]])){
              vContCenter <- c(vContCenter, input[[paste0("contCenter_", i)]])
            }
            else{
              vContCenter <- c(vContCenter, "")
            }
            if(!is.null(input[[paste0("contCRP_", i)]])){
              vcontCRP <- c(vcontCRP, input[[paste0("contCRP_", i)]])
            }
            else{
              vcontCRP <- c(vcontCRP, "")
            }
          }
        }
      }
      vPrEnt <- paste(vPrEnt, collapse = ",")
      vContCenter <- paste(vContCenter, collapse = ",")
      vcontCRP <- paste(vcontCRP, collapse = ",")
      vPrName <- paste(vPrName, collapse = ",")
    }

    c11 <- c('Number of project management entities',vNumPrEnt )
    c12 <- c('Project management entity',vPrEnt )
    c13 <- c('Contribuitor center',vContCenter )
    c14 <- c('Contribuitor CRP', vcontCRP)
    c15 <- c('Project management entity name', vPrName )



    vleadOrgType <- c()
    vleadPerson <- c()
    vleadOrgName <- c()

    if(is.numeric(input$numLeads)){
      vn <- input$numProjEntity
      for(i in 1:vn){
        aux <- input[[paste0("projLeadEnt_", i)]]
        if(!is.null(aux)){
          if(aux == "Other"){
            if(is.null(input[[paste0("lead_org_type_1_", i)]])) vleadOrgType <- c(vleadOrgType,aux)
            else vleadOrgType <- c(vleadOrgType, input[[paste0("lead_org_type_1_", i)]])
            vleadOrgName <- c(vleadOrgName, input[[paste0("leadNameOther_", i)]])
          }
          else{
            vleadOrgType <- c(vleadOrgType,aux)
            if(!is.null(input[[paste0("tLeadCenter_", i)]])){
              vleadOrgName <- c(vleadOrgName, input[[paste0("tLeadCenter_", i)]])
            }
            else{
              vleadOrgName <- c(vleadOrgName, "")
            }
          }
          vleadPerson <- c(vleadPerson, input[[paste0("expLead_", i)]])
        }
      }

      vleadOrgType <- paste(vleadOrgType, collapse = ";")
      vleadOrgName <- paste(vleadOrgName, collapse = ";")
      vleadPerson <- paste(vleadPerson, collapse = ";")
    }

    c16 <- c('Experiment, lead organization type',vleadOrgType )
    c17 <- c('Experiment lead person / Primary Investigator', vleadOrgName)
    c18 <- c('Experiment, lead organization name',vleadPerson )

    np <- input$npersons
    vperType <- c()
    vperfname <- c()
    vperlname <- c()
    vperemail <- c()
    vperAff <- c()
    vperOrcid <- c()
    vpercountry <- c()

    for(i  in 1:np){
      if(is.null(input[[paste0("personnel",i,"Type")]])) vperType <- c(vperType, "")
      else vperType <- c(vperType, input[[paste0("personnel",i,"Type")]])

      vperfname <- c(vperfname, input[[paste0("person",i,"FirstName")]])
      vperlname <- c(vperlname, input[[paste0("person",i,"LastName")]])

      if(is.null(input[[paste0("person",i,"Affiliation")]])) vperAff <- c(vperAff, "")
      else{
        if(input[[paste0("person",i,"Affiliation")]] == "CGIAR Center"){
          if(is.null(input[[paste0("person",i,"Center")]])) vperAff <- c(vperAff, "CGIAR Center")
          else vperAff <- c(vperAff, input[[paste0("person",i,"Center")]])
        }
        else{
          vperAff <- c(vperAff, input[[paste0("person",i,"CenterOther")]])
        }
      }

      vperemail <- c(vperemail, input[[paste0("person",i,"Email")]])
      vperOrcid <- c(vperOrcid, input[[paste0("person",i,"ORCID")]])

      if(is.null(input[[paste0("person",i,"Country")]])) vpercountry <- c(vpercountry, "")
      else vpercountry <- c(vpercountry, input[[paste0("person",i,"Country")]])
    }

    vperType <- paste(vperType, collapse = ",")
    vperfname <-paste(vperfname, collapse = ",")
    vperlname <- paste(vperlname, collapse = ",")
    vperemail <- paste(vperemail, collapse = ",")
    vperAff <- paste(vperAff, collapse = ",")
    vperOrcid <- paste(vperOrcid, collapse = ",")
    vpercountry <- paste(vpercountry, collapse = ",")

    c19 <- c('Person type',vperType )
    c20 <- c('Person, first name',vperfname )
    c21 <- c('Person, last name', vperlname)
    c22 <- c('Person, email', vperemail)
    c23 <- c('Person, affiliation', vperAff)
    c24 <- c('Person, ORCID',vperOrcid )
    c25 <- c('Country in which active', vpercountry)


    vsitetype <- ""
    vsitename <- ""
    vsiteId <- ""
    vsiteCountry <- ""
    vsiteadmin1 <- ""
    vsiteadmin2 <- ""
    vsiteVillage <- ""
    vsitenear <- ""
    vsiteElev <- ""
    vsiteLat <- ""
    visteLon <- ""

    if(!is.null(input$fbDesign_countryTrial) && !is.null(input$designFieldbook_sites)){
      vsiteCountry <- input$fbDesign_countryTrial

      xpath <- fbglobal::get_base_dir()
      xfp <- file.path(path, "table_sites_agrofims.rds")

      xaux <- input$designFieldbook_sites
      # print(xaux)
      # xstart <- which(strsplit(xaux, "")[[1]]=="(")
      # vsiteId <- substr(xaux, xstart+1, nchar(xaux)-1)
      vsiteId <- xaux

      x_sites_data <- readRDS(file = xfp)
      data <- dplyr::filter(x_sites_data, shortn==vsiteId)
      if(nrow(data) != 0){
        xsite <- data[1,]
        vsitetype <- xsite$Type
        vsitename <- xsite$local
        vsiteadmin1 <- xsite$adm1
        vsiteadmin2 <- xsite$adm2
        vsiteVillage <- xsite$village
        vsitenear <- xsite$nearpop
        vsiteElev <- xsite$elev
        vsiteLat <- xsite$latd
        visteLon <- xsite$lond
      }


    }

    c26 <- c('Site type',vsitetype)
    c27 <- c('Site name',vsitename)
    c28 <- c('Site ID', vsiteId)
    c29 <- c('Country name', vsiteCountry)
    c30 <- c('Site, first-level administrative division name',vsiteadmin1 )
    c31 <- c('Site, second-level administrative division name',vsiteadmin2 )
    c32 <- c('Village name', vsiteVillage)
    c33 <- c('Nearest populated place', vsitenear)
    c34 <- c('Site elevation',vsiteElev )
    c35 <- c('Site latitude (in decimal degrees)', vsiteLat)
    c36 <- c('Site longitude (in decimal degrees)',visteLon )

    vHighLevel <- ""
    if(!is.null(input$fbDesign_inHighLevel)) vHighLevel <- input$fbDesign_inHighLevel

    c37 <- c('Higher-level landform',vHighLevel)

    vSiteVegetation <- ""
    if(!is.null(input$fbDesign_inSiteVegetation)) vSiteVegetation <- paste(input$fbDesign_inSiteVegetation, collapse = ",")

    c38 <- c('Vegetation surrounding the experimental site', vSiteVegetation)
    c39 <- c('Site description notes', input$inSiteDescNotes)

    c40 <- c('Cropping type', input$croppingType )

    vCropCommon <- ""
    if(!is.null(input$cropCommonNameMono)) vCropCommon <- input$cropCommonNameMono

    c41 <- c('Crop common name',vCropCommon )
    c42 <- c('Crop latin name', input$cropLatinNameMono)

    vCropVarName <- ""
    if(!is.null(input$cropVarietyNameMono)) vCropVarName <- paste(input$cropVarietyNameMono, collapse = ",")

    c43 <- c('Crop variety name', vCropVarName)
    c44 <- c('Cultivar name',input$cultivarNameMono )
    c45 <- c('Crop local name', input$monoCropLocalName)

    nCropPrevCrop <- ""
    nprevCropName <- ""
    nprevCropVar <- ""

    if(is.numeric(input$numPreviousCrop)) nCropPrevCrop <-input$numPreviousCrop
    if(!is.null(input$prevCropName)) nprevCropName <- paste(input$prevCropName, collapse = ",")
    if(!is.null(input$prevCropVar)) nprevCropVar <- paste(input$prevCropVar, collapse = ",")

    c46 <- c('Number of previous crop', nCropPrevCrop)
    c47 <- c('Previous crop name', nprevCropName)
    c48 <- c('Previous crop variety',nprevCropVar )

    # c56 <- c('Subject', )
    # c57 <- c('Keywords', )
    # c58 <- c('Embargo end date', )

    df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,
                              c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                              c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                              c31,c32,c33,c34,c35,c36,c37,c38,c39,c40,
                              c41,c42,c43,c44,c45,c46,c47, c48)
    var_metadata <-  t(df_metadata)
    # print(df_metadata)

  }

  factor_dt <- function(){


    vinfExp <-""
    if(!is.null(input$info_experiment_unit)) vinfExp <- input$info_experiment_unit


    c1 <- c('Information on experimental unit',vinfExp )

    vfarea <-""
    vfexpmaxwidth <- ""
    vfexpmaxlength <- ""
    vpdiam <- ""
    vpdpth <- ""
    if(vinfExp == "plot"  ){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_plot_width_unit))  wunit <- input$expt_plot_width_unit
      if(!is.null(input$expt_plot_length_unit))  lunit <- input$expt_plot_length_unit
      vfexpmaxwidth <- paste0(input$expt_plot_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_plot_length, " " , lunit)
    }
    else if(vinfExp == "field"){
      vfarea <- vinfExp
      wunit <- ""
      lunit <- ""
      if(!is.null(input$expt_field_width_unit))  wunit <- input$expt_field_width_unit
      if(!is.null(input$expt_field_length_unit))  lunit <- input$expt_field_length_unit
      vfexpmaxwidth <- paste0(input$expt_field_width, " " , wunit)
      vfexpmaxlength <- paste0(input$expt_field_length, " " , lunit)
    }
    else if(vinfExp == "pot"){
      wunit <- ""
      lunit <- ""
      if(!is.null(input$pot_diameter_unit))  wunit <- input$pot_diameter_unit
      if(!is.null(input$pot_depth_unit))  lunit <- input$pot_depth_unit
      vpdiam <- paste0(input$pot_diameter, " " , wunit)
      vpdpth <- paste0(input$pot_depth, " " , lunit)

    }

    c2 <- c('Field area',vfarea )
    c3 <- c('Experimental field maximum width', vfexpmaxwidth)
    c4 <- c('Experimental field maximum length', vfexpmaxlength)
    c5 <- c('Pot diameter',vpdiam )
    c6 <- c('Pot depth',vpdpth )
    c7 <- c('Experimental design', input$designFieldbook_agrofims)
    c8 <- c('Experimental design abbreviation', "")
    c9 <- c('Number of replications', input$designFieldbook_agrofims_r)
    c40 <- c('Number of factors', input$nfactors_hdafims)

    levels1 <- c("NA", "NA", "NA", "NA","NA")
    levels2 <- c("NA", "NA", "NA", "NA","NA")
    levels3 <- c("NA", "NA", "NA", "NA","NA")
    levels4 <- c("NA", "NA", "NA", "NA","NA")
    levels5 <- c("NA", "NA", "NA", "NA","NA")
    levelsDt <- data.table(levels1,levels2,levels3,levels4,levels5)


    nf <- input$nfactors_hdafims

    factors <- c("NA", "NA", "NA", "NA","NA")
    for(i in 1:nf){
      g1 <- input[[paste0("sel" , i, "_2" )]]
      g2 <- input[[paste0("sel" , i, "_3" )]]
      if(!is.null(g1) && !is.null(g2)){
        factors[i] <- paste0(g1, " ", g2)

        g3 <- input[[paste0("sel" , i, "_3" )]]
        ls1 <- input[[paste0("numLevels_", i)]]
        if(is_numeric(ls1) && !is.null(g3)){
          if (ls1>5) ls1 <- 5 #max5
          if(g3 %like% "date"){
            for(j in 1:ls1){
              sdate <- input[[paste0("factor_start_date_",i, "_", j)]]
              edate <- input[[paste0("factor_end_date_",i, "_", j)]]
              levelsDt[i,j] <- paste0(sdate, " - ", edate)
            }
          }
          else{
            nl <- input[[paste0("levels_",i)]]
            count <- 1
            for(lv in nl){
              if(count <= 5){
                if(is.null(input[[paste0("funits_", i)]])){
                  levelsDt[i,count] <- lv
                }
                else{
                  levelsDt[i,count]<- paste0(lv, " ", input[[paste0("funits_", i)]])
                }
              }
              count <- count + 1
            }
          }
        }

      }

    }


    c10 <- c('Factor 1',factors[1])
    c11 <- c('Factor 1 - level 1',levelsDt[1,1])
    c12 <- c('Factor 1 - level 2',levelsDt[1,2] )
    c13 <- c('Factor 1 - level 3',levelsDt[1,3])
    c14 <- c('Factor 1 - level 4',levelsDt[1,4] )
    c15 <- c('Factor 1 - level 5',levelsDt[1,5] )

    c16 <- c('Factor 2', factors[2])
    c17 <- c('Factor 2 - level 1',levelsDt[2,1])
    c18 <- c('Factor 2 - level 2',levelsDt[2,2])
    c19 <- c('Factor 2 - level 3',levelsDt[2,3])
    c20 <- c('Factor 2 - level 4',levelsDt[2,4] )
    c21 <- c('Factor 2 - level 5',levelsDt[2,5] )

    c22 <- c('Factor 3', factors[3])
    c23 <- c('Factor 3 - level 1',levelsDt[3,1] )
    c24 <- c('Factor 3 - level 2',levelsDt[3,2] )
    c25 <- c('Factor 3 - level 3',levelsDt[3,3] )
    c26 <- c('Factor 3 - level 4',levelsDt[3,4] )
    c27 <- c('Factor 3 - level 5',levelsDt[3,5] )

    c28 <- c('Factor 4', factors[4] )
    c29 <- c('Factor 4 - level 1',levelsDt[4,1])
    c30 <- c('Factor 4 - level 2',levelsDt[4,2] )
    c31 <- c('Factor 4 - level 3',levelsDt[4,3])
    c32 <- c('Factor 4 - level 4',levelsDt[4,4] )
    c33 <- c('Factor 4 - level 5',levelsDt[4,5])

    c34 <- c('Factor 5', factors[5])
    c35 <- c('Factor 5 - level 1',levelsDt[5,1] )
    c36 <- c('Factor 5 - level 2',levelsDt[5,2])
    c37 <- c('Factor 5 - level 3',levelsDt[5,3])
    c38 <- c('Factor 5 - level 4',levelsDt[5,4] )
    c39 <- c('Factor 5 - level 5',levelsDt[5,5])


    df_metadata <- data.frame(c1,c2,c3,c4,c5,c6,c7,c8,c9,c40,c10,
                              c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,
                              c21,c22,c23,c24,c25,c26,c27,c28,c29,c30,
                              c31,c32,c33,c34,c35,c36,c37,c38,c39)
    var_metadata <-  t(df_metadata)
    # print(df_metadata)

  }

  # observeEvent(input$btTest,{
  #   traits_dt()
  # })

  traits_dt <- function(){
    a<- traitsVals$Data
    if(nrow(traitsVals$Data) >0){
      aux_dt <- dplyr::filter(traitsVals$Data, Status=="Selected")
      a<- aux_dt
    }

    return(a)
  }



}

