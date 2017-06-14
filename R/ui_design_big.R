design_choices_big <- c(
  #"Unreplicated Design with No Randomization (UNDR)" = "UNDR",
  "Randomized Complete Block Design (RCBD)" = "RCBD"#,
  #"Completely Randomized Design (CRD)" = "CRD",
  #"Augmented Block Design (ABD)" = "ABD",
  #"Latin Square Design (LSD)" = "LSD",
  #"Split Plot with Plots in CRD (SPCRD)" = "SPCRD",
  #"Split Plot with Plots in RCBD (SPRCBD)" = "SPRCBD",
  #"Split Plot with Plots in LSD (SPLSD)" = "SPLSD",
  #"Strip Plot Design (STRIP)" = "STRIP",
  #"Factorial Two-Way Design in CRD (F2CRD)" = "F2CRD",
  #"Factorial Two-Way Design in RCBD (F2RCBD)" = "F2RCBD",
  #  "Balanced Incomplete Block Design (BIBD)" = "BIBD",
  #  "Graeco-Latin Design (GLD)" = "GLD",
  #  "Youden Design (YD)" = "YD",
  #  "Cyclic Design (CD)" = "CD",
  #  "Lattice Design (LD)" = "LD",
  #"Alpha Design (AD)" = "AD" #,
  # #"Augmented Partially Replicated Design (APRD)" = "APRD",
  # #"Factorial Design (F2SPPD)" = "F2SPPD",
  # #"North Carolina Design I" = "NCI",
  # #"North Carolina Design II" = "NCII",
  # #"North Carolina Design III" = "NCIII"
)

design_conditional_panels_big <- function(){
  list(
    shiny::conditionalPanel(
     "input.designFieldbook_big == 'RCBD'  |
      input.designFieldbook_big == 'CRD' |
      input.designFieldbook_big == 'F2CRD'  |
      input.designFieldbook_big == 'F2RCBD' |
      input.designFieldbook_big == 'ABD' |
      input.designFieldbook_big == 'DAU' |
      input.designFieldbook_big == 'AD'  |
      input.designFieldbook_big == 'SPCRD'|
      input.designFieldbook_big == 'SPRCBD'|
      input.designFieldbook_big == 'SPLSD'|
      input.designFieldbook_big == 'STRIP'",


      # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_r_big", "Number of replications in mother plot", 2:100, 2),
      shiny::selectInput("designFieldbook_r_big_baby", "Number of baby plots", 3:100, 3)
    ),

    shiny::conditionalPanel(
      "input.designFieldbook_big == 'SPCRD' |
      input.designFieldbook_big == 'SPRCBD'|
      input.designFieldbook_big == 'F2CRD' |
      input.designFieldbook_big == 'F2RCBD'|
      input.designFieldbook_big == 'SPLSD'|
      input.designFieldbook_big == 'STRIP'
      ",

      #shiny::selectInput("designFieldbook_big_r", "Replications:", 1:5, 2 ),
      textInput(inputId = "factor_name_big", label = "Enter Additional Factor Name",""),
      br(),
      textInput(inputId = "factor_lvl_big", label = "Type levels of factors (separated by commas ',')", value = "")#,
      # textInput(inputId = "factor_lvl1_big", label = "First Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl2_big", label = "Second Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl3_big", label = "Third Level for Additional Factor",value = "")

    ),

    shiny::conditionalPanel(
      "input.designFieldbook_big == 'ABD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Check if your material list have an 'x' mark in Is_control column.
              Otherwise, Augmented Design does not work.",
              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),

    shiny::conditionalPanel(
      "input.designFieldbook_big == 'LD'", # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_r_big", "Replications:", 2:100, 2 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook_big == 'BIBD'",
      shiny::selectInput("designFieldbook_maxR_big", "Repetition maximum (k):", 3:100, 20 )
    ) ,
    shiny::conditionalPanel(
      "input.designFieldbook_big == 'AD' |
      input.designFieldbook_big == 'BIBD'",
      # TODO do server side checking of permitted combinations (based on number of treatments)
      # trt = k * s
      # s = number of blocks
      # k = size of block (max is trt)
      #
      # r = 2: k <= s
      # r = 3: s odd; k <= s
      # r = 3: s even; k <= (s - 1)
      # r = 4: s odd but not multiple of 3; k <= s
      #shiny::selectInput("designFieldbook_big_r", "Replications:", 2:6000, 2 ),
      shiny::selectInput("designFieldbook_k_big", "Block size (k):", 2:100, 4 )
    ),

    shiny::conditionalPanel(

      "input.designFieldbook_big == 'AD'", # TODO: ab - factorial, split
      fluidRow(
        shiny::wellPanel(
          shiny::HTML("<b>ALPHA CONDITION:</b>"),
          textOutput("alphaMessage_big")
        )
      )
    ),

    #     shiny::conditionalPanel(
    #       "input.designFieldbook == 'CD'",
    #       # TODO do server side checking of permitted combinations (based on number of treatments)
    #       # number of treatments 6:30
    #       shiny::selectInput("designFieldbook_r", "Replications:", 2:10, 2 ),
    #       shiny::selectInput("designFieldbook_k", "Block size (k):", 2:10,3 )
    #     ),

    #     shiny::conditionalPanel(
    #       "input.designFieldbook == 'LSD' |
    #       input.designFieldbook == 'RCBD' |
    #       input.designFieldbook == 'SPPD' |
    #       input.designFieldbook == 'BIBD'
    #       "#,
    #       #shiny::checkboxInput("designFieldbook_first", "Randomize first repetition", TRUE )
    #     )
    #     ,
    shiny::conditionalPanel(
      "input.designFieldbook_big == 'RCBD'"
      # shiny::checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
    )
    )
}




#############
#' shiny UI element big
#'
#' returns a re-usable user interface element big
#'
#' @author Reinhard Simon
#' @param type of ui Element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @description  User interface function to build Special Modules like PVS or ABS in HIDAP.
#' @export


ui_design_big <- function(type="tab",title="Special Modules",name="phenotype_fieldbook_design"){

             #tabsetPanel( #Begin Master tabSetPanel

          #fluidRow(


            tabPanel(title, icon = icon("tags", lib = "glyphicon"),
                       br(),
                          shiny::wellPanel(
                          shiny::HTML("<b>Fieldbook identifiers:</b>"),
                          shiny::textOutput("fbDesign_id_big")
                          ),

                          fluidRow(
                               box(
                                title = " ", status = "success", solidHeader = TRUE,
                                collapsible = TRUE, width = 12,
                                       #shiny::tabsetPanel(# id = "fbDesignNav",

                                       shinydashboard::tabBox(id = "fbDesignNav_big1", height = NULL, width = 12,

                                          shiny::tabPanel("Crop & Location", value = "crop_big", icon = shiny::icon("leaf"),
                                                           br(),
                                                          fluidRow(
                                                            column(width = 6,
                                                                   #box(
                                                                   #title = "Step: 1", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = NULL,height = NULL,

                                                                   shiny::selectInput("designFieldbook_crop_big", "Crop", crops()),
                                                                   shiny::uiOutput("fbDesign_factor2_big", inline = TRUE),
                                                                   shiny::uiOutput("fbDesign_variables_big", inline = TRUE),
                                                                   shiny::dateRangeInput("fbDesign_project_time_line_big", "Date of Experiment", start = Sys.Date() - 2,
                                                                                       end = Sys.Date() + 20, startview = "year",format = "dd/mm/yyyy"),
                                                                                       # shiny::selectizeInput("fbDesign_countryTrial_big", label = "Field Country:",
                                                                                       #                       choices = country(), selected = 1,  multiple = FALSE),
                                                                                       shiny::uiOutput("fbDesign_country_big", inline = TRUE, width = 500),
                                                                                       shiny::uiOutput("fbDesign_countrySite_big", inline = TRUE, width = 500), #,#locality
                                                                                       selectInput('fbDesign_nExp_big', 'Experiment number', c("-",paste("exp",1:100,sep="")), selectize=TRUE)

                                                                                     )
                                                                              )
                                                              ),

                                                shiny::tabPanel("Material List", value = "plants_big", icon = shiny::icon("list-alt"),
                                                                    br(),
                                                                    # fluidRow(
                                                                    #  column(width = 6,
                                                                    #        box(
                                                                    #       title = "Step: 2", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                                                    #shiny::uiOutput("designFieldbook_genotypes", inline = TRUE),
                                                                    # h4("Define Genotypes",style = "font-family: 'Arial', cursive;
                                                                    #    font-weight: 500; line-height: 1.1; color: #4d3a7d;"),
                                                                    # br(),


                                                                radioButtons("select_import_big", label = h4("Define Genotypes",style = "font-family: 'Arial', cursive;
                                                                 font-weight: 500; line-height: 1.1;
                                                                 color: #4d3a7d;"),
                                                                             choices = c("Local List", "Template"),
                                                                             selected = "Local List"),

                                                                br(),

                                                                 conditionalPanel(
                                                                   condition = "input.select_import_big == 'Local List'",


                                                                   shiny::uiOutput("fbDesign_selmlist_big"),
                                                                   shiny::actionButton("fdesign_list_refresh_big","Refresh List"),

                                                                   br()#,
                                                                 ),

                                                                conditionalPanel(
                                                                  condition = "input.select_import_big == 'Template'",
                                                                  fileInput(inputId = 'file_big',label =  'Upload material list file',accept = ".xlsx"),
                                                                  downloadButton(outputId = "fbDesign_mlistExport_big", label = "Material List Template")

                                                                ),
                                                                    #fileInput(inputId = 'file_big',label =  'Upload material list file',accept = ".xlsx"),
                                                                    fluidRow(
                                                                            infoBoxOutput("approvalBox_big")#,
                                                                    )#,
                                                                    #downloadButton(outputId = "fbDesign_mlistExport_big", label = "Material List Template")#,

                                                              ),

                                                  shiny::tabPanel("Evaluation Forms", value = "traits_big", icon = shiny::icon("star"),
                                                                              br(),
                                                                              shinyTree::shinyTree("designFieldbook_traits_big",search = TRUE,checkbox = TRUE)

                                                              ),

                                                   shiny::tabPanel("Statistical design", value = "design_big", icon = shiny::icon("pie-chart"),

                                                                              br(),
                                                                              shiny::selectInput("designFieldbook_big", "Design", choices = design_choices_big, selected = "RCBD", multiple = FALSE),
                                                                              #shiny::checkboxInput("designFieldbook_random", "Use randomization", TRUE),
                                                                              design_conditional_panels_big(),
                                                                              #shiny::selectInput("designFieldbook_n_org_mother", "Number of panelist (Organoleptic Mother)",
                                                                              #                   choices = list("1-10","1-20","1-30","1-40","1-50","1-60","1-70","1-80","1-90","1-100"),selected = 1),
                                                                              shiny::numericInput("designFieldbook_n_org_mother", label = "Number of panelists (Organoleptic Mother)",
                                                                                                    value = 10, min =10 , max =100),

                                                                              #shiny::selectInput("designFieldbook_n_org_baby"  , "Number of panelist (Organoleptic Baby))",
                                                                              #                   choices = list("1-10","1-20","1-30","1-40","1-50","1-60","1-70","1-80","1-90","1-100"),selected = 1)
                                                                              shiny::numericInput("designFieldbook_n_org_baby", label = "Number of panelists (Organoleptic Baby)",
                                                                                                    value = 10, min =10 , max =100)

                                                              ),

                                                  shiny::tabPanel("Environment", value = 'environment_big', icon = shiny::icon("recycle"),
                                                                              br(),

                                                                              shiny::checkboxInput("designFieldbook_zigzag_big", "Zigzag", FALSE),
                                                                              shiny::radioButtons("designFieldbook_serie_big", "Label series:",
                                                                                                  #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]],
                                                                                                  1:3, 1,
                                                                                                  inline = TRUE)
                                                                              ,
                                                                              shiny::radioButtons("fbDesign_environment_type_big", "Environment type",choices = list(
                                                                                "Field" = "Field"),
                                                                                #"Farmers field" = "farmers_field",
                                                                                #"Greenhouse" = "Greenhouse",
                                                                                #"Screenhouse" = "Screenhouse"),
                                                                                selected = "Field" , inline = TRUE),
                                                                              shiny::checkboxInput("fbDesign_weather_cb_big", label = "Register weather data"),
                                                                              shiny::checkboxInput("fbDesign_soil_cb_big", label = "Register soil data")
                                                                              # )
                                                                              #)
                                                                              #)
                                                              ),

                                                   shiny::tabPanel("Plant Installation", value = "fbDesign_planting_big", icon = shiny::icon("th-large"),
                                                                              br(),

                                                                              # shiny::numericInput("fbDesign_nplants_big",
                                                                              #                     "Number of plants per plot/pot", 10 , 1, 100),



                                                                              shiny::numericInput("fbDesign_nplantsrow_big",
                                                                                                  "Number of plants per row", 10, 1, 100),

                                                                              shiny::numericInput("fbDesign_nrowplot_big",
                                                                                                  "Number of rows per plot", 1, 1, 100),

                                                                              shiny::numericInput("fbDesign_distPlants_big",
                                                                                                  "Distance between plants (m)", .3, .1, 1),

                                                                              shiny::numericInput("fbDesign_distRows_big",
                                                                                                  "Distance between rows (m)", .9, .1, 1),

                                                                              shiny::uiOutput("fbPlanting_psize_big", inline=TRUE),

                                                                              shiny::uiOutput("fbPlanting_pdensity_big", inline=TRUE),

                                                                              shiny::uiOutput("fbPlant_plot_big", inline = TRUE),

                                                                              tags$head(tags$style("#fbDesign_pdensity_big{color:#191919;
                                                                                                   background-color:#ecc464;
                                                                                                   #font-size: 20px;
                                                                                                   #font-style: italic;
                                                                                                   }"
                                                                              )
                                                                              ),
                                                                              tags$head(tags$style("#fbDesign_psize_big{color:#191919;
                                                                                                    background-color:#88e3a5;
                                                                                                    #font-size: 20px;
                                                                                                    #font-style: italic;
                                                                                                    }"
                                                                              )
                                                                              )

                                                              )#,

                                       )#,
                                       #shinyBS::bsAlert("alert_fb_done")
                                     )
                                   ),

                                   fluidRow(
                                     HTML('<div style="float: right; margin: 0 15px 18px 0px;">'),
                                     shiny::actionButton("fbDesign_draft_big", "Book preview", icon("table"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                     shiny::actionButton("fbDesign_create_big", "Download", icon("file-excel-o"), style="color: #fff; background-color: #51a351; border-color: #51a351"),
                                     shinyBS::bsAlert("alert_fb_done_big"),
                                     HTML('</div>')
                                   ),

                                   shiny::fluidRow(
                                     #box(
                                     #column(width = 12,height=6,
                                     shinydashboard::box(title = title,
                                                         status = "primary",
                                                         #height = 500,
                                                         #width = NULL,
                                                         solidHeader = TRUE,
                                                         width = 12, collapsible = TRUE,
                                                         #shiny::actionButton("butNewFieldbook", "New fieldbook", inline = TRUE)#,

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F1_selection_criteria</b>"),
                                                           rhandsontable::rHandsontableOutput("fbDesign_table_big_f1",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F2_select_clones_flowering</b>"),
                                                         rhandsontable::rHandsontableOutput("fbDesign_table_big_f2",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F3_select_clones_harvest</b>"),
                                                         rhandsontable::rHandsontableOutput("fbDesign_table_big_f3",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F4_select_clones_harvest</b>"),
                                                         rhandsontable::rHandsontableOutput("fbDesign_table_big_f4",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F5_select_clones_harvest</b>"),
                                                         rhandsontable::rHandsontableOutput("fbDesign_table_big_f5",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F6_organoleptic_mother</b>"),
                                                           rhandsontable::rHandsontableOutput("fbDesign_table_big_f6",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F7_organoleptic_baby</b>"),
                                                           rhandsontable::rHandsontableOutput("fbDesign_table_big_f7",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F8_select_clones_harvest</b>"),
                                                         rhandsontable::rHandsontableOutput("fbDesign_table_big_f8",width = 700, height = 400)
                                                         ),
                                                         shiny::br(),

                                                         shiny::wellPanel(
                                                           shiny::HTML("<b>F9_postharvest_clones_storage</b>"),
                                                         rhandsontable::rHandsontableOutput("fbDesign_table_big_f9",width = 700, height = 400)
                                                         ),
                                                         shiny::br()

                                     ),
                                     br(),
                                     br(),
                                     br()

                                   ),
                                   br(),
                                   br(),
                                   br()

                        #  )

                )

             #)
          }
