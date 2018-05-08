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


  observeEvent(input$titleId, {
    js$collapse("box1")
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
                                    )
                             ),
                             conditionalPanel(paste0("input.projLeadEnt_", count, " == 'CGIAR center'"),
                                              column(width = 6,
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
                                              )

                             ),
                             conditionalPanel(paste0("input.projLeadEnt_", count, " == 'Other'"),

                                              selectizeInput(paste0("lead_org_type_1_", count), "",multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),  choices = c("University","University, main campus","Agricultural experimental extension", "Government research institution (NARS)","Government research institution, designated laboratory or center", "Private company", "Farm", "Farmer association or cooperative", "Non-governmental organization", "Extension organization", "CGIAR center", "Other" )),
                                              conditionalPanel(paste0("input.lead_org_type_1", count, " == 'Other'"),
                                                               textInput(paste0("lead_org_type_1_other_", count), "")),

                                              textInput(paste0("leadNameOther_", count), "Experiment, lead organization name", value = "")

                             ),
                             textInput(inputId = paste0("expLead_", count), label = "Experiment lead person / Primary Investigator", value = "")

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
  fp <- file.path(path, "listFactors.rds")

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
      aux <- dplyr::filter(factors,SUBGROUP==input$sel1_2)

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
    if(!is.null(input$sel1_3)){
      aux <- dplyr::filter(factors,SUBGROUP==input$sel1_2, FACTOR==input$sel1_3)
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
      aux <- dplyr::filter(factors,SUBGROUP==input$sel2_2)
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
    if(!is.null(input$sel2_3)){
      aux <- dplyr::filter(factors,SUBGROUP==input$sel2_2, FACTOR==input$sel2_3)

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
      aux <- dplyr::filter(factors,SUBGROUP==input$sel3_2)
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
    if(!is.null(input$sel3_3)){
      aux <- dplyr::filter(factors,SUBGROUP==input$sel3_2, FACTOR==input$sel3_3)

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
      aux <- dplyr::filter(factors,SUBGROUP==input$sel4_2)
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
    if(!is.null(input$sel4_3)){
      aux <- dplyr::filter(factors,SUBGROUP==input$sel4_2, FACTOR==input$sel4_3)

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
      aux <- dplyr::filter(factors,SUBGROUP==input$sel5_2)
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
    if(!is.null(input$sel5_3)){
      aux <- dplyr::filter(factors,SUBGROUP==input$sel5_2, FACTOR==input$sel5_3)

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
                                     selectizeInput(paste0("units_", order), HTML("Unit"),
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
    numericInput("numLevels_1", HTML("#levels"), max = 5, min = 1, value = 1)
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
    numericInput("numLevels_2", "#levels", max = 5, min = 1, value = 1)
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
    numericInput("numLevels_3", "#levels", max = 5, min = 1, value = 1)
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
    numericInput("numLevels_4", "#levels", max = 5, min = 1, value = 1)
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
    numericInput("numLevels_5", "#levels", max = 5, min = 1, value = 1)
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
                        dateInput("biofertilizer_landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
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
        textInput(paste0("biofertilizer_days_sowing_after_rhizobium_inocculation_", order), value="", label = "Days to sowing after Rhizobium inocculation")
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
                                            "Localized ",
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
                         )



                  ),
                  column(width = 6,
                         fluidRow(
                           column(width = 6,
                               textInput(paste0("irrigation_bund_height_", order), value="", label = "Bund height")
                           ),
                           column(width = 6,
                                  selectizeInput(paste0("irrigation_bund_height_", order, "unit"), "Unit", multiple=T, options=list(maxItems=1, placeholder="Select one..."),
                                                 choices = c("cm", "in", "m")
                                  )
                           )
                         ),
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
                                   dateInput(paste0("nutrient_end_date_", type, "_", order), label ="Start date", format = "dd/mm/yyyy")
                            )
                          ),
                          fluidRow(
                            column(width = 6,
                                   textInput("nutrient_app_rate_",  label = "Total application rate for the season", value="")
                            ),
                            column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                   selectizeInput("nutrient_app_rate_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                    c("cm",
                                                      "in",
                                                      "m")
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
                                     selectizeInput(paste0("fertilizer_recommended_rate_unit_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "Unit", c("kg/m2","kg/ha","t/ha"))
                              )
                            ),
                           textInput("perc_recomm_rate", "Percentage of recommended rate applied")
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
                    # column(width = 1,
                    #        br(),
                    #        div(style="text-align:right", h4("Name"))
                    # ),
                    # column(width = 1,
                    #        br(),
                    #        div(style="text-align:center", h4("Number of applications"))
                    # ),
                    #
                    # column(width = 1,
                    #        br(),
                    #        div(style="text-align:center", h4("Order"))
                    # ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Start date"))
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("End date"))
                    ),

                    column(width = 2,
                           br(),
                           div(style="text-align:center", h4("Type"))
                    ),
                    column(width = 2,
                           br(),
                           div(style="text-align:center", h4("Application technique"))
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Implement"))
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Application rate"))
                    ),
                    column(width = 1,
                           br(),
                           div(style="text-align:center", h4("Application rate (unit)"))
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
                               'NPK fertilizers')
                             )
                           }
                           else{
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
                                   "Fish fertilizer"
                                   )
                                 )
                             }
                    ),
                    column(width = 2,
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
                           )
                    ),
                    column(width = 1,
                           selectizeInput(paste0("fertilizer_nit_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                          choices = c("Backpack sprayer (airblast sprayer)",
                                                      "Boom sprayer",
                                                      "Broadcast spreader",
                                                      "Hand sprayer",
                                                      "Manure spreader",
                                                      "Slurry injector",
                                                      "Manual application",
                                                      "Other")
                           )
                    ),
                    column(width = 1,
                           textInput(paste0("fert_nit_amountApplied1_", type, "_", order),"")
                    ),
                    column(width = 1,
                           selectizeInput(paste0("fert_nit_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
                                          c("kg/m2","kg/ha","t/ha"))
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
                                                       'NPK fertilizers')
                                                     )
                                                   }
                                                   else{
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
                                                           "Fish fertilizer"
                                                         )
                                                         )
                                                     }
                                            ),
                                            column(width = 2,
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
                                                   )
                                            ),
                                            column(width = 1,
                                                   selectizeInput(paste0("fertilizer_nit_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                                   choices = c("Backpack sprayer (airblast sprayer)",
                                                                                        "Boom sprayer",
                                                                                        "Broadcast spreader",
                                                                                        "Hand sprayer",
                                                                                        "Manure spreader",
                                                                                        "Slurry injector",
                                                                                        "Manual application",
                                                                                        "Other")
                                                   )
                                            ),
                                            column(width = 1,
                                                   textInput(paste0("fert_nit_amountApplied2_", type, "_", order),"")
                                            ),
                                            column(width = 1,
                                                   selectizeInput(paste0("fert_nit_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
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
                                                'NPK fertilizers')
                                              )
                                            }
                                            else{
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
                                                    "Fish fertilizer"
                                                  )
                                                  )
                                              }
                                     ),
                                     column(width = 2,
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
                                            )
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fertilizer_nit_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                           choices = c("Backpack sprayer (airblast sprayer)",
                                                                       "Boom sprayer",
                                                                       "Broadcast spreader",
                                                                       "Hand sprayer",
                                                                       "Manure spreader",
                                                                       "Slurry injector",
                                                                       "Manual application",
                                                                       "Other")
                                            )
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_nit_amountApplied3_", type, "_", order),"")
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fert_nit_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
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
                               'NPK fertilizers')
                             )
                           }
                           else{
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
                                   "Fish fertilizer"
                                 )
                                 )
                             }
                    ),
                    column(width = 2,
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
                           )
                    ),
                    column(width = 1,
                           selectizeInput(paste0("fertilizer_phos_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                          choices = c("Backpack sprayer (airblast sprayer)",
                                                      "Boom sprayer",
                                                      "Broadcast spreader",
                                                      "Hand sprayer",
                                                      "Manure spreader",
                                                      "Slurry injector",
                                                      "Manual application",
                                                      "Other")
                           )
                    ),
                    column(width = 1,
                           textInput(paste0("fert_phos_amountApplied1_", type, "_", order),"")
                    ),
                    column(width = 1,
                           selectizeInput(paste0("fert_phos_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
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
                                                'NPK fertilizers')
                                              )
                                            }
                                            else{
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
                                                  "Fish fertilizer"
                                                )
                                                )
                                              }
                                     ),
                                     column(width =2,
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
                                            )
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fertilizer_phos_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                           choices = c("Backpack sprayer (airblast sprayer)",
                                                                       "Boom sprayer",
                                                                       "Broadcast spreader",
                                                                       "Hand sprayer",
                                                                       "Manure spreader",
                                                                       "Slurry injector",
                                                                       "Manual application",
                                                                       "Other")
                                            )
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_phos_amountApplied2_", type, "_", order),"")
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fert_phos_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
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
                                                'NPK fertilizers')
                                              )
                                            }
                                            else{
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
                                                  "Fish fertilizer"
                                                )
                                                )
                                            }
                                     ),
                                     column(width = 2,
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
                                            )
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fertilizer_phos_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                           choices = c("Backpack sprayer (airblast sprayer)",
                                                                       "Boom sprayer",
                                                                       "Broadcast spreader",
                                                                       "Hand sprayer",
                                                                       "Manure spreader",
                                                                       "Slurry injector",
                                                                       "Manual application",
                                                                       "Other")
                                            )
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_phos_amountApplied3_", type, "_", order),"")
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fert_phos_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
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
                               'NPK fertilizers')
                             )
                           }
                           else{
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
                                 "Fish fertilizer"
                               )
                               )
                             }
                    ),
                    column(width = 2,
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
                           )
                    ),
                    column(width = 1,
                           selectizeInput(paste0("fertilizer_potas_implement1_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                          choices = c("Backpack sprayer (airblast sprayer)",
                                                      "Boom sprayer",
                                                      "Broadcast spreader",
                                                      "Hand sprayer",
                                                      "Manure spreader",
                                                      "Slurry injector",
                                                      "Manual application",
                                                      "Other")
                           )
                    ),
                    column(width = 1,
                           textInput(paste0("fert_potas_amountApplied1_", type, "_", order),"")
                    ),
                    column(width = 1,
                           selectizeInput(paste0("fert_potas_amountAppliedScale1_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
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
                                                'NPK fertilizers')
                                              )
                                            }
                                            else{
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
                                                    "Fish fertilizer"
                                                  )
                                                  )
                                              }
                                     ),
                                     column(width = 2,
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
                                            )
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fertilizer_potas_implement2_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                           choices = c("Backpack sprayer (airblast sprayer)",
                                                                       "Boom sprayer",
                                                                       "Broadcast spreader",
                                                                       "Hand sprayer",
                                                                       "Manure spreader",
                                                                       "Slurry injector",
                                                                       "Manual application",
                                                                       "Other")
                                            )
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_potas_amountApplied2_", type, "_", order),"")
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fert_potas_amountAppliedScale2_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
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
                                                'NPK fertilizers')
                                              )
                                            }
                                            else{
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
                                                    "Fish fertilizer"
                                                  )
                                                  )
                                              }
                                     ),
                                     column(width = 2,
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
                                            )
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fertilizer_potas_implement3_", type, "_", order), label = "",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                           choices = c("Backpack sprayer (airblast sprayer)",
                                                                       "Boom sprayer",
                                                                       "Broadcast spreader",
                                                                       "Hand sprayer",
                                                                       "Manure spreader",
                                                                       "Slurry injector",
                                                                       "Manual application",
                                                                       "Other")
                                            )
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_potas_amountApplied3_", type, "_", order),"")
                                     ),
                                     column(width = 1,
                                            selectizeInput(paste0("fert_potas_amountAppliedScale3_", type, "_", order), multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "",
                                                           c("kg/m2","kg/ha","t/ha"))
                                     ),
                                     column(width = 1,
                                            textInput(paste0("fert_potas_nutrientContent3_", type, "_", order),"")
                                     )
                                   )


                  )#end conditional2



             ))


  }

  ####################################################################


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
  dict <- data.frame(stringsAsFactors = FALSE,
         # c("<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>","<font color='black' > Not selected </font>"
         c("Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected","Not selected"
),
# ),
         c('Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Potato','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Cassava','Wheat','Wheat','Wheat','Wheat','Wheat','Wheat','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Maize','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Sweetpotato','Soybean','Soybean','Soybean'),
         c('Number of tubers planted','Number of emerged plants','Percentage plants emerged','Number of harvested plants','Percentage of plants harvested','Non-marketable tuber number','Total number of tubers','Number marketable tubers','Non-marketable tuber weight','Total tuber weight','Total tuber yield no adjusted','Total tuber yield adjusted','Marketable tuber weight','Marketable tuber yield no adjusted','Marketable tuber yield adjusted','Average of tuber weight','Average of marketable tuber weight','Sprouting','Initial Vigor','Plant Stands Harvested','Root Number','Storage root weight','Root Yield in fresh weight','Root Yield in dry weight','Root Yield in Fresh Aerial','Stem weight','Stem number','Marketable root weight','Non marketable root weight','Number of rotten stem','Storage root weight','Number of planted stakes','Seedling number','Non marketable root number','Marketable root number','Stock weight','Stem weight','Number of germinated stakes','Root Yield','Storage root weight ','Storage root weight Peel','Number of stakes','Aboveground biomass at maturity','Grain weight','Grain yield','Grain yield factor','Harvest index','In-season aboveground biomass','Grain row number','Grain weight','Grain yield adjusted','Grain yield in dry weight','Grain yield in fresh weight','Grain yield rank number','Grain yield relative to check','Grain yield relative','Shelled cob in fresh weight','Grain test weight ','Grain weight adjusted','Grain weight in fresh weight','Grain yield in fresh weight','Number of plants established','Number of plants planted','Number of plants harvested','Number of plants with storage roots','Number of commercial storage roots','Number of non-commercial storage roots ','Total number of root','Weight of commercial storage roots','Weight of non-commercial storage roots ','Weight of vines','Total root weight','Marketable root yield','Average commercial root weight','Yield of total roots','Percentage of marketable roots','Biomass yield','Relative Storage Root Yield','Storage Root Yield relative to check','Fodder Yield','Seed yield','Seed weight'),
         c('CO_330:0000265-/plot','CO_330:0000268-/plot','CO_330:0000283- ','CO_330:0000287-/plot','CO_330:0000290- ','CO_330:0000300-/plot','CO_330:0000304-/plot,CO_330:0000305-/plant','CO_330:0000293-/plot,CO_330:0000297-/plant','CO_330:0000314-kg/plot','CO_330:0000317-kg/plot,CO_330:0000321-kg/plant','CO_330:0000324-t/ha','CO_330:0000323-t/ha','CO_330:0000308- ','CO_330:0000330-t/ha','CO_330:0000327-t/ha','CO_330:0000333-g','CO_330:0000336-g','CO_334:0000008-ratio','CO_334:0000009- ','CO_334:0000010-/plant','CO_334:0000011- ','CO_334:0000012- kg/plot','CO_334:0000013-t/ha','CO_334:0000014-t/ha','CO_334:0000017-t/ha','CO_334:0000127-kg/pl','CO_334:0000129-Stem','CO_334:0000131-kg/plot','CO_334:0000132-/plot','CO_334:0000133- ','CO_334:0000157-kg/pl,CO_334:0000158-kg/plot','CO_334:0000159- ','CO_334:0000166- ','CO_334:0000168-/plot','CO_334:0000169-/plot','CO_334:0000170-/kg','CO_334:0000171-/kg','CO_334:0000213-1 month,CO_334:0000214-3 months,CO_334:0000215-6 months,CO_334:0000216-9 months,CO_334:0000217-12 months','CO_334:0000230-kg/plant,CO_334:0000231-t/ha','CO_334:0000247-kg','CO_334:0000248-kg','CO_334:0000250- ','CO_321:0001034-m2/kg,CO_321:0001035-kg/ha,CO_321:0001036-t/ha,CO_321:0001037-g/plant,CO_321:0001038-g/plot,CO_321:0001039-kg/plot','CO_321:0001213-g/1000 grain,CO_321:0001214-g/100 grain,CO_321:0001215-g/200 grain','CO_321:0001217-g/m2,CO_321:0001218-kg/ha,CO_321:0001219-t/ha,CO_321:0001220-g/plant,CO_321:0001221-g/plot,CO_321:0001222-kg/plot,CO_321:0001223-%','CO_321:0001224- ','CO_321:0001231- ,CO_321:0001232-%','CO_321:0001246-m2/kg,CO_321:0001247-kg/ha,CO_321:0001248-t/ha,CO_321:0001249-g/plot,CO_321:0001250-kg/plot,CO_321:0001651- ','CO_322:0000694- ','CO_322:0000723-g/1000grain,CO_322:0000725-g/100grain,CO_322:0000727-g/200grain','CO_322:0000730-kg/ha,CO_322:0000731-t/ha','CO_322:0000734-g/plot,CO_322:0000737-kg/ha,CO_322:0000740-kg/plot,CO_322:0000742-t/ha','CO_322:0000744-g/plot,CO_322:0000747-kg/ha,CO_322:0000749-kg/plot,CO_322:0000751-t/ha','CO_322:0000754- ','CO_322:0000756-%','CO_322:0000757-%','CO_322:0000928-g/plot,CO_322:0000931-kg/plot','CO_322:0001008-lb/bsh','CO_322:0001009-g/1000grain,CO_322:0001010-g/100grain,CO_322:0001011-g/200grain','CO_322:0001012-g/200grain,CO_322:0001013-g/1000grain,CO_322:0001014-g/100grain','CO_322:0001016-lb/plot','CO_331:0000192-/plot','CO_331:0000678-/plot','CO_331:0000679-/plot','CO_331:0000211-/plot','CO_331:0000214-/plot','CO_331:0000217-/plot','CO_331:0000233-/plot,CO_331:0000230-/plant','CO_331:0000220-kg/plot','CO_331:0000223-kg/plot','CO_331:0000227-kg/plot','CO_331:0000237-kg/plot','CO_331:0000218-t/ha','CO_331:0000680-t/ha','CO_331:0000681-kg/plot,CO_331:0000296-t/ha','CO_331:0000682-%','CO_331:0000683-t/ha','CO_331:0000791- ','CO_331:0000792- ','CO_336:0000262-g/plot,CO_336:0000340-kg/ha','CO_336:0000261-g/plot,CO_336:0000337-kg/ha','CO_336:0000333-g'),
         # c('', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '','','','','','','','','','','','','','','','','','', '','','',''),
         # c('', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '', '','', '','', '','', '','', '','', '','', '','', '','','','','','','','','','','','','','','','','','', '','','','')
         c('CO_330:0000265','CO_330:0000268','CO_330:0000283','CO_330:0000287','CO_330:0000290','CO_330:0000300','CO_330:0000304','CO_330:0000293','CO_330:0000314','CO_330:0000317','CO_330:0000324','CO_330:0000323','CO_330:0000308','CO_330:0000330','CO_330:0000327','CO_330:0000333','CO_330:0000336','CO_334:0000008','CO_334:0000009','CO_334:0000010','CO_334:0000011','CO_334:0000012','CO_334:0000013','CO_334:0000014','CO_334:0000017','CO_334:0000127','CO_334:0000129','CO_334:0000131','CO_334:0000132','CO_334:0000133','CO_334:0000157','CO_334:0000159','CO_334:0000166','CO_334:0000168','CO_334:0000169','CO_334:0000170','CO_334:0000171','CO_334:0000213','CO_334:0000230','CO_334:0000247','CO_334:0000248','CO_334:0000250','CO_321:0001034','CO_321:0001213','CO_321:0001217','CO_321:0001224','CO_321:0001231','CO_321:0001246','CO_322:0000694','CO_322:0000723','CO_322:0000730','CO_322:0000734','CO_322:0000744','CO_322:0000754','CO_322:0000756','CO_322:0000757','CO_322:0000928','CO_322:0001008','CO_322:0001009','CO_322:0001012','CO_322:0001016','CO_331:0000192','CO_331:0000678','CO_331:0000679','CO_331:0000211','CO_331:0000214','CO_331:0000217','CO_331:0000233','CO_331:0000220','CO_331:0000223','CO_331:0000227','CO_331:0000237','CO_331:0000218','CO_331:0000680','CO_331:0000681','CO_331:0000682','CO_331:0000683','CO_331:0000791','CO_331:0000792','CO_336:0000262','CO_336:0000261','CO_336:0000333'),
         c('/plot','/plot','','/plot','','/plot','/plot','/plot','kg/plot','kg/plot','t/ha','t/ha','','t/ha','t/ha','g','g','ratio','','/plant','',' kg/plot','t/ha','t/ha','t/ha','kg/pl','Stem','kg/plot','/plot','','kg/pl','','','/plot','/plot','/kg','/kg','1 month','kg/plant','kg','kg','','m2/kg','g/1000 grain','g/m2','','','m2/kg','','g/1000grain','kg/ha','g/plot','g/plot','','%','%','g/plot','lb/bsh','g/1000grain','g/200grain','lb/plot','/plot','/plot','/plot','/plot','/plot','/plot','/plot','kg/plot','kg/plot','kg/plot','kg/plot','t/ha','t/ha','kg/plot','%','t/ha','','','g/plot','g/plot','g')

  )
  colnames(dict) <- c("Status","Crop", "Crop trait management", "traitCode", "VariableId", "Scale")
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

    DT= traitsVals$Data
    DT[["Change scale"]] <- lapply(1:nrow(traitsVals$Data), "drawComboInTable")
    DT[["Variable ID"]] <- traitsVals$Data[,5]
    DT[["Select variable"]] <- lapply(1:nrow(traitsVals$Data), "drawButtonSelect")
    datatable(DT,
              escape=F,
              # selection = list(mode = 'multiple', selected = traitsVals$selectedRows),
              selection = list(mode = 'none'),
              options = list(
                scrollX = TRUE,
                pageLength = 25,
                columnDefs = list(list(visible=FALSE, targets=c(1,4,5)), list(width = '15%', targets = c(8)), list(className = 'dt-center', targets = c(3, 5, 7)))
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

    traitsVals$Data[[5]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[1]]
    traitsVals$Data[[6]][as.numeric(gsub("select_scale_","",input$selectScaleClickId))]<- var[[2]]
  })

  drawButtonSelect <- function(index){
    old_row <- traitsVals$Data[index,]
    # str <-  paste0('<div class="btn-group" role="group" aria-label="Basic example">
    #   <button style="width:100px; background-color:green; color:white;" type="button" class="btn btn-secondary selectRow" id=selectRow_',index ,'>Select</button>
    #   </div>')

    ckecked <-  ""
    # str <-  paste0('<button style="width:100px; background-color:green; color:white;" type="button" class="btn btn-secondary selectRow" id=selectRow_',index ,'>Select</button>')
    if(old_row[[1]] %like% "Selected"){
      # str <- gsub("green", "red", str)
      # str <- gsub("Select", "Deselect", str)
      ckecked <- "checked"
    }


    str <-  paste0('<div class="btn-group" role="group" aria-label="Basic example">
      <input style="width:100px; background-color:green; color:white;" type="checkbox" class="selectRow"  id=selectRow_',index ,' ',ckecked,  '></input>
      </div>')



    return(str)

  }

  drawComboInTable <- function(index){
    old_row = traitsVals$Data[index,]
    options = old_row[[4]]
    row_change=list()

    str  <- paste0('<select id="select_scale_' , index, '" class="select_scale" style="width:150px;">')
    arrOpt <- strsplit(options, ",")[[1]]
    if(length(arrOpt) == 1){
      # mval1  <- strsplit(arrOpt[1], "-")[[1]]
      # if(mval1[[2]] == " ")
        return(" ")
    }

    for(val in arrOpt){
      mval  <- strsplit(val, "-")[[1]]
      if(mval[[2]] == old_row[[6]]) sel <- "selected" else sel <-""
      str <- paste0(str, '<option value="', mval[[1]], "-" , mval[[2]], '" ', sel,'> ', mval[[2]], '</option>')
    }
    str <- paste0(str, "</select>")

    return(str)
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


  # Get Trait #################################################################
  # output$designFieldbook_traits_agrofims <- shinyTree::renderTree({
  #
  #   a<- agronomic_trait_list #data from fbdesign for hidap-agrofims
  #
  # })

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
    #geodb_file <- "table_sites.rds"
    geodb_file <- "table_sites_agrofims.rds"
    path <- file.path(path, geodb_file)
    values$sites_data <-  readRDS(file = path)

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

    if(design=="crd"){
      fb <- fb[,-c(2,3)]
    }
    if(design == "rcbd"){
      fb <- fb[,-c(3,4)]
    }


    trait_selected <- trait_agrofims() %>% as.data.frame(stringsAsFactors =FALSE) #unlist(shinyTree::get_selected(input$designFieldbook_traits_agrofims))
    trait_selected <- trait_selected[,2]

    if(!is.null(trait_selected)){
      mm  <-  matrix(nrow = nrow(fb), ncol = length(trait_selected) )
      nm  <-  c(names(fb), trait_selected)
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

      out<-  land_des(input$landLeveling_start_date,input$landLeveling_end_date,
                  input$numPasses,input$operationsOrder,input$impl_type,
                  input$animal_traction,input$humanPowered,input$motorized_traction,
                  input$puddling_start_date,input$puddling_end_date,
                  input$Penetrometer_in_field,input$puddling_depth_val,input$pud_animal_traction,
                  input$pud_humanPowered,input$pud_motorized_traction,input$tillage_start_date,
                  input$tillage_end_date,input$till_technique,input$till_depth_method,input$till_depth,
                  input$till_total_op_season,input$till_impl_type,input$till_animal_traction,
                  input$till_humanPowered,input$till_motorized_traction,input$liming_start_date,
                  input$liming_end_date,input$lim_material,input$lim_quantity,input$lim_description
        )


      out
  })

  ### Mulching

  dt_mulching <- reactive({

    out<-mulch(input$mulch_start_date,input$mulch_end_date,
          input$mulch_type,input$mulch_thickness,input$mulch_amountPerSq,
          input$mulch_color,input$mulch_percCoverage,input$mulch_remove_start_date,
          input$mulch_remove_end_date,input$mulch_make,input$mulch_model,
          input$mulch_animal_traction,input$mulch_humanPowered,
          input$mulch_motorized_traction,input$residue_cropType,
          input$residue_technique,input$residue_incorp_depth,
          input$residue_aboveGroundMoisture,
          input$residue_aboveGroundAmount)
    out

  })

  ### Planting


  dt_planting <- reactive({

    out<- plant(input$planting_start_date,input$planting_end_date,
           input$planting_directSeeding,input$planting_seedingTech,
           input$planting_ageSeeding,input$planting_manual,
           input$planting_animal_traction,input$planting_motorized_traction,
           input$planting_rowDistance,input$planting_seedingRate,
           input$planting_seedPerhill,input$planting_distance,
           input$planting_distribution)
    out

  })


  ### Harvest

  dt_harvest <- reactive({

    out<-harvest(input$harvest_start_date,
            input$harvest_end_date,input$crop_component_harvested,
            input$harvest_implement,input$harvest_make,input$harvest_model,
            input$harvest_animal_traction,input$harvest_humanPowered,
            input$harvest_motorized_traction)
    out

  })

  #irrigation
  dt_irrigation <- reactive({

    out<-irrigation(input$irrigationevent_start_date,
                input$irrigationevent_end_date,input$irrigation_system_type,
                input$irrigation_technique,input$surface_irrigation_technique,
                input$localized_irrigation_technique,input$irrigation_using_sprinkler_systems,
                irrigation_system_picture = "", #input$rrigation_system picture,
                input$irrigation_water_source,input$irrigation_water_source_distance,
                input$irrigation_bund_height,input$irrigation_percolation_rate,input$irrigation_equipment_depth,
                input$irrigation_well_depth,input$irrigation_area_covered_irrigation_system)

    out

  })

  ##biofertilization
  dt_bioferti <- reactive({

    out<-biofer( input$biofertilizer_landLeveling_start_date,input$biofertilizer_landLeveling_end_date,
            input$biofertilizer_rhizobium_inoculum_strain,input$biofertilizer_quantity_inoculated,
            input$biofertilizer_inoculation_method,input$biofertilizer_product_formulation,
            input$biofertilizer_days_sowing_after_rhizobium_inocculation)

    out

  })

  ### nutrient


  ### pest and disease


  dt_pestdis <- reactive({

    out<-pestdis (input$disease_observation_date,input$disease_name,
              input$disease_plant_parts_affected,input$disease_percentage_experiement_affected,
              input$disease_damages_notes,input$disease_notes,input$pest_type,input$pest_name,
              input$pest_damage_notes,input$pest_notes,input$pestcontrol_start_date,input$pestcontrol_end_date,
              input$pest_control_technique,input$pesticide_application_depth,input$pesticide_amount,
              "", #input$pest_image,
              input$pest_control_applications_totnumber,input$pest_control_details,input$chemical_pest_control_equipment,
              input$pesticide_implement_make,input$pesticide_implement_model,input$pesticide_animal_traction,
              input$pesticide_humanPowered,input$pesticide_motorized_traction)
    out


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


  # ##reactive soil
  # dt_soil_agrofims <- shiny::reactive({
  #
  #   soil_vars <- unlist(shinyTree::get_selected(input$designFieldbook_soilVar_agrofims))
  #
  #   if(!is.null(soil_vars)){
  #     dt  <-  matrix(nrow = 50, ncol = length(soil_vars))
  #     dt  <- data.frame(dt)
  #     names(dt)  <-  soil_vars
  #   } else{
  #     dt <- data.frame()
  #   }
  #   dt
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

      fb <- fb_agrofims()

      # agrofeatures <- dt_agrofeatures()
      metadata <- dt_metadata_agrofims()
      installation <-dt_installation_agrofims()

      trait_agrofims_dt <- trait_agrofims()
      trait_agrofims_dt<- trait_agrofims_dt[,-3]

      weather <- dt_weather_agrofims()
      print(weather)
      # soil <- dt_soil_agrofims()
      # print(soil)

      fname <- paste(file,"xlsx",sep=".")
      #wb <- openxlsx::loadWorkbook(file = fname, create = TRUE)

      wb <- createWorkbook()

        incProgress(2/20,message = "Adding fieldbook data...")



      # incProgress(3/10,message = "Adding agronomic features...")
      # openxlsx::addWorksheet(wb, "Agronomic_Features", gridLines = TRUE)
      # openxlsx::writeDataTable(wb, "Agronomic_Features", x = agrofeatures,
      #                          colNames = TRUE, withFilter = FALSE)


      incProgress(6/20,message = "Metadata metadata sheet...")

      openxlsx::addWorksheet(wb, "Metadata", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Metadata", x = metadata,
                               colNames = TRUE, withFilter = FALSE)


      incProgress(7/20,message = "Adding installation sheet...")

      openxlsx::addWorksheet(wb, "Installation", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Installation", x = installation,
                               colNames = TRUE, withFilter = FALSE)



      #write agrofeatures sheet
      agroFeaSelected <-input$selectAgroFeature
      #agrofea_sheets <- c("Land preparation", "Mulching", "Planting","Irrigation event", "Biofertilizer", "Pest & disease", "Nutrient management event","Harvest")

      if(is.element("Land preparation", agroFeaSelected)) {

      incProgress(10/20,message = "Adding land preparation sheet...")

      dt_land <- dt_land_description()
      print(dt_land)
      openxlsx::addWorksheet(wb, "Land preparation", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Land preparation", x = dt_land ,
                               colNames = TRUE, withFilter = FALSE)

      }

      if(is.element("Mulching", agroFeaSelected)) {

      incProgress(11/20,message = "Adding mulching data...")

      dt_mulch <- dt_mulching()

      openxlsx::addWorksheet(wb, "Mulching", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Mulching", x = dt_mulch,
                               colNames = TRUE, withFilter = FALSE)


      }

      if(is.element("Planting", agroFeaSelected)) {

      incProgress(12/20,message = "Adding planting data...")

      dt_plant <- dt_planting()

      openxlsx::addWorksheet(wb, "Planting", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Planting", x = dt_plant,
                               colNames = TRUE, withFilter = FALSE)

      }

      if(is.element("Irrigation event", agroFeaSelected)) {

      incProgress(14/20,message = "Adding irrigation data...")

      dt_irri <- dt_irrigation()

      openxlsx::addWorksheet(wb, "Irrigation", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Irrigation", x = dt_irri,
                               colNames = TRUE, withFilter = FALSE)

      }

      if(is.element("Biofertilizer", agroFeaSelected)) {

      incProgress(15/20,message = "Adding biofertilizer data...")

      dt_biof <- dt_bioferti()

      openxlsx::addWorksheet(wb, "Biofertilizer", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Biofertilizer", x = dt_biof,
                               colNames = TRUE, withFilter = FALSE)

      }

      # openxlsx::addWorksheet(wb, "Nutrient", gridLines = TRUE)
      # openxlsx::writeDataTable(wb, "Nutrient", x = ,
      #                          colNames = TRUE, withFilter = FALSE)

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

      openxlsx::addWorksheet(wb, "PestDisease", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "PestDisease", x = dt_pestd,
                               colNames = TRUE, withFilter = FALSE)

      }

      incProgress(9/20,message = "Adding trait list sheet...")

      openxlsx::addWorksheet(wb, "Trait list", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Trait list", x = trait_agrofims_dt,
                               colNames = TRUE, withFilter = FALSE)

      incProgress(8/20,message = "Adding fieldbook sheet...")

      openxlsx::addWorksheet(wb, "Fieldbook", gridLines = TRUE)
      openxlsx::writeDataTable(wb, "Fieldbook", x = fb,
                               colNames = TRUE, withFilter = FALSE)


      if(is.null(weather_vars)){
        print("there is no weather data")

      } else {

        incProgress(8/10,message = "Adding weather variables sheet...")

        openxlsx::addWorksheet(wb, "Weather", gridLines = TRUE)
        openxlsx::writeDataTable(wb, "Weather", x = weather,
                                 colNames = TRUE, withFilter = FALSE)
      }

      # if(is.null(soil_vars)){
      #   print("there is no soil data")
      #
      # } else {
      #
      #   incProgress(9/10,message = "Adding soil variables sheet...")
      #   openxlsx::addWorksheet(wb, "Soil", gridLines = TRUE)
      #   openxlsx::writeDataTable(wb, "Soil", x = soil,
      #                            colNames = TRUE, withFilter = FALSE)
      # }

      incProgress(19/20,message = "Downloading file...")

      saveWorkbook(wb, file = fname , overwrite = TRUE)

      file.rename(fname, file)


    })

    },
    contentType="application/xlsx"
  )


}

