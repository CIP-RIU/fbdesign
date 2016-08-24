
design_choices <- c(


  "Unreplicated Design with No Randomization (UNDR)" = "UNDR",
  "Randomized Complete Block Design (RCBD)" = "RCBD",
  "Completely Randomized Design (CRD)" = "CRD",
  "Augmented Block Design (ABD)" = "ABD",
  "Latin Square Design (LSD)" = "LSD",
  "Split Plot with Plots in CRD (SPCRD)" = "SPCRD",
  "Split Plot with Plots in RCBD (SPRCBD)" = "SPRCBD",
  "Split Plot with Plots in LSD (SPLSD)" = "SPLSD",
  "Strip Plot Design (STRIP)" = "STRIP",
  "Factorial Two-Way Design in CRD (F2CRD)" = "F2CRD",
  "Factorial Two-Way Design in RCBD (F2RCBD)" = "F2RCBD",
#  "Balanced Incomplete Block Design (BIBD)" = "BIBD",
#  "Graeco-Latin Design (GLD)" = "GLD",
#  "Youden Design (YD)" = "YD",
#  "Cyclic Design (CD)" = "CD",
#  "Lattice Design (LD)" = "LD" ,
  "Alpha Design (AD)" = "AD" #,
  # #"Augmented Partially Replicated Design (APRD)" = "APRD",
  # #"Factorial Design (F2SPPD)" = "F2SPPD",
  # #"North Carolina Design I" = "NCI",
  # #"North Carolina Design II" = "NCII",
  # #"North Carolina Design III" = "NCIII"
)

design_conditional_panels <- function(){
  list(
    shiny::conditionalPanel(
      "input.designFieldbook == 'RCBD'  |
      input.designFieldbook == 'CRD' |
      input.designFieldbook == 'F2CRD'  |
      input.designFieldbook == 'F2RCBD' |
      input.designFieldbook == 'ABD' |
      input.designFieldbook == 'DAU' |
      input.designFieldbook == 'AD'  |
      input.designFieldbook == 'SPCRD'|
      input.designFieldbook == 'SPRCBD'|
      input.designFieldbook == 'SPLSD'|
      input.designFieldbook == 'STRIP'",


       # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_r", "Replications (r):", 2:100, 2 )

    ),

    shiny::conditionalPanel(
      "input.designFieldbook == 'SPCRD' |
       input.designFieldbook == 'SPRCBD'|
       input.designFieldbook == 'F2CRD' |
       input.designFieldbook == 'F2RCBD'|
       input.designFieldbook == 'SPLSD'|
       input.designFieldbook == 'STRIP'
      ",

      #shiny::selectInput("designFieldbook_r", "Replications (r):", 1:5, 2 ),
      textInput(inputId = "factor_name", label = "Enter Additional Factor Name",""),
      br(),
      textInput(inputId = "factor_lvl1", label = "First Level for Additional Factor",value = ""),
      textInput(inputId = "factor_lvl2", label = "Second Level for Additional Factor",value = ""),
      textInput(inputId = "factor_lvl3", label = "Third Level for Additional Factor",value = "")

    ),

    shiny::conditionalPanel(
      "input.designFieldbook == 'ABD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Check if your material list have an 'x' mark in Is_control column.
              Otherwise, Augmented Design does not work.",
              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),

    shiny::conditionalPanel(
      "input.designFieldbook == 'LD'", # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_r", "Replications (r):", 2:100, 2 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook == 'BIBD'",
      shiny::selectInput("designFieldbook_maxR", "Repetition maximum (k):", 3:100, 20 )
    ) ,
    shiny::conditionalPanel(
      "input.designFieldbook == 'AD' |
       input.designFieldbook == 'BIBD'",
      # TODO do server side checking of permitted combinations (based on number of treatments)
      # trt = k * s
      # s = number of blocks
      # k = size of block (max is trt)
      #
      # r = 2: k <= s
      # r = 3: s odd; k <= s
      # r = 3: s even; k <= (s - 1)
      # r = 4: s odd but not multiple of 3; k <= s
      #shiny::selectInput("designFieldbook_r", "Replications (r):", 2:6000, 2 ),
      shiny::selectInput("designFieldbook_k", "Block size (k):", 2:100, 4 )
    ),

    shiny::conditionalPanel(

      "input.designFieldbook == 'AD'", # TODO: ab - factorial, split
      fluidRow(
        shiny::wellPanel(
        shiny::HTML("<b>ALPHA CONDITION:</b>"),
        textOutput("alphaMessage")
          )
        )
 ),

#     shiny::conditionalPanel(
#       "input.designFieldbook == 'CD'",
#       # TODO do server side checking of permitted combinations (based on number of treatments)
#       # number of treatments 6:30
#       shiny::selectInput("designFieldbook_r", "Replications (r):", 2:10, 2 ),
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
      "input.designFieldbook == 'RCBD'"
     # shiny::checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
    )
  )
}

#' shiny UI element
#'
#' returns a re-usable user interface element
#'
#' @author Reinhard Simon
#' @param type of ui Element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name

#' @export


################## Begin Simple Modules

ui_fieldbook <- function(type="tab",title="Design Fieldbook",name="phenotype_fieldbook_design"){

#############
shinydashboard::tabItem(tabName = name,


                shinyjs::useShinyjs(),

                tabsetPanel( #Begin Master tabSetPanel
                  tabPanel("Standart Modules",icon = icon("tag", lib = "glyphicon"),
                           br(),
                           shiny::wellPanel(
                           shiny::HTML("<b>Fieldbook identifiers:</b>"),
                           shiny::textOutput("fbDesign_id")
                          ),

                         fluidRow(
                         box(
                           title = " ", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = 12,

                           #shiny::tabsetPanel(# id = "fbDesignNav",
                           shinydashboard::tabBox(id = "fbDesignNav", height = NULL, width = 12,

                                  shiny::tabPanel("Crop & Location", value = "crop", icon = shiny::icon("leaf"),
                                                  br(),
                                                  fluidRow(
                                                    column(width = 6,
                                                   #        box(
                                                    #         title = "Step: 1", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = NULL,height = NULL,

                                  shiny::selectInput("designFieldbook_crop", "Crop", crops()),
                                  shiny::uiOutput("fbDesign_factor2", inline = TRUE),
                                                              shiny::uiOutput("fbDesign_variables", inline = TRUE),
                                  shiny::dateRangeInput("fbDesign_project_time_line", "Date of Experiment", start = Sys.Date() - 2,
                                                                  end = Sys.Date() + 20, startview = "year",format = "dd/mm/yyyy"),


                                 #shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),#country
                                  #shiny::selectizeInput("fbDesign_country", inline = TRUE, width = 500),#country
                                  shiny::selectizeInput("fbDesign_countryTrial", label = "Field Country:",
                                                                  choices = country(), selected = 1,  multiple = FALSE),
                                  shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500), #,#locality
                                  selectInput('fbDesign_nExp', 'Experiment number', c("-",paste("exp",1:100,sep="")), selectize=TRUE)

                                   #    )
                                              #  shiny::tabPanel("Project", value = "project", icon = shiny::icon("book"),
                                              #  #shiny::uiOutput("fbDesign_project", inline = TRUE),
                                              #  shiny::textInput("fbDesign_project_name", "Project name"),
                                              #  shiny::textInput("fbDesign_project_objective", "Project objective"),
                                              #  #shiny::textInput("fbDesign_comments", "Project comments"),
                                              #  shiny::dateRangeInput("fbDesign_project_time_line", "Date range")
                                              #
                                              #   ),
                                              #   shiny::tabPanel("Plants", value = "plants", icon = shiny::icon("star"),
                                              #   shiny::uiOutput("designFieldbook_genotypes", inline = TRUE)
                                              #   ),
                                                  )
                                  )
                          ),
                          shiny::tabPanel("Material List", value = "plants", icon = shiny::icon("list-alt"),
                                          br(),
                                          # fluidRow(
                                          #  column(width = 6,
                                          #        box(
                                          #       title = "Step: 2", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = NULL,
                                          #shiny::uiOutput("designFieldbook_genotypes", inline = TRUE),
                                          h4("Define Genotypes",style = "font-family: 'Arial', cursive;
                                                                 font-weight: 500; line-height: 1.1;
                                                                 color: #4d3a7d;"),

                                          radioButtons("select_import", label = h4("Select Type of Import"),
                                                       choices = c("Local List", "Template"),
                                                       selected = "Local List"),

                                          br(),

                                         conditionalPanel(
                                           condition = "input.select_import == 'Local List'",


                                          shiny::uiOutput("fbDesign_selmlist"),
                                          shiny::actionButton("fdesign_list_refresh","Refresh List"),

                                          br()#,
                                         ),

                                         conditionalPanel(
                                           condition = "input.select_import == 'Template'",
                                           downloadButton(outputId = "fbDesign_mlistExport", label = "Material List Template"),
                                          fileInput(inputId = 'file',label =  'Upload material list file',accept = ".xlsx")#,
                                          ),




                                          #shinyFilesButton('file', 'File select', 'Upload material list', FALSE),
                                          #bsAlert("alert"),
                                          #shiny::actionLink('fbDesign_mlistExport', 'Material List Template'),
                                          #textOutput("germplasm_alert")
                                          fluidRow(
                                            #br(),
                                            #shiny::actionLink('exportButton', 'Material List Template'),
                                            infoBoxOutput("approvalBox")#,
                                            #HTML('<div style="float: left; margin: 0px 15px 5px 5px;">'),
                                            #HTML('</div>'),
                                            #tags$style(type='text/css', "#fbDesign_mlistExport { width:300px; margin-top: 25px;}"),
                                            #tags$style(type='text/css', "#approvalBox { width:300px; margin-top: 25px;}")
                                          )#,
                                       #,

                                          #shinyalert("shinyalert1", FALSE, auto.close.after = 5)
                                          #actionButton("id_success","Save",styleclass="success",icon = "ok")
                                          #  )
                                          #)
                                          #)
                                  ),

                          shiny::tabPanel("Fieldbook Traits", value = "traits", icon = shiny::icon("star"),

                                          br(),
                                          #fluidRow(
                                           # column(width = 6,
                                            #       box(
                                             #        title = "Step: 3", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = NULL,


                                                              #shiny::uiOutput("designFieldbook_traits", inline = TRUE)
                                          shinyTree::shinyTree("designFieldbook_traits",search = TRUE,checkbox = TRUE)

                                            #        )
                                           #      )
                                          #  )
                                      ),

                           shiny::tabPanel("Statistical design", value = "design", icon = shiny::icon("pie-chart"),

                                           br(),
                                           shiny::selectInput("designFieldbook", "Design method:", design_choices, multiple = FALSE),
                                           #shiny::checkboxInput("designFieldbook_random", "Use randomization", TRUE),
                                           design_conditional_panels()

                           ),

                             shiny::tabPanel("Environment", value = 'environment', icon = shiny::icon("recycle"),
                                          br(),

                                             shiny::checkboxInput("designFieldbook_zigzag", "Zigzag", TRUE),
                                             shiny::radioButtons("designFieldbook_serie", "Label series:",
                                                                 #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]],
                                                                 1:3, 2,
                                                                 inline = TRUE)
                                             ,
                                             # shiny::conditionalPanel(
                                             #  "input.designFieldbook == 'RCBD'",
                                             #shiny::checkboxInput("designFieldbook_cont", "Continuous numbering of plots", FALSE)
                                             #),

                                             shiny::radioButtons("fbDesign_environment_type", "Environment type",choices = list(
                                               "Field" = "Field",
                                               #"Farmers field" = "farmers_field",
                                               "Greenhouse" = "Greenhouse",
                                               "Screenhouse" = "Screenhouse"),selected = "Field" , inline = TRUE),
                                             shiny::checkboxInput("fbDesign_weather_cb", label = "Register weather data"),
                                             shiny::checkboxInput("fbDesign_soil_cb", label = "Register soil data")
                                             # )
                                             #)
                                             #)
                             ),
#                                        shiny::tabPanel("Field", value = "fbDesign_field", icon = shiny::icon("male"),
#                                           shiny::numericInput("fbDesign_field_size (ha)",
#                                                               "Field size", 1, 1, 100)
#                                        ),
#                                        shiny::tabPanel("Farmers field", value = "fbDesign_farmers_field"
#                                        ),
# shiny::tabPanel("Greenhouse", value = "fbDesign_greenhouse"
# ),
# shiny::tabPanel("Screenhouse", value = "fbDesign_screenhouse"
# ),
                           shiny::tabPanel("Plant Installation", value = "fbDesign_planting", icon = shiny::icon("th-large"),
                                         br(),
                                          # fluidRow(
                                           #  column(width = 6,
                                            #        box(
                                              #        title = "Step: 6", status = "warning", solidHeader = TRUE, collapsible = TRUE, width = NULL,

                                         shiny::numericInput("fbDesign_nplants",
                                                             "Number of plants per plot/pot", 10 , 1, 100),
                                         shiny::numericInput("fbDesign_nplantsrow",
                                                             "Number of plants per row", 10, 1, 100),
                                         shiny::numericInput("fbDesign_nrowplot",
                                                             "Number of row per plot/pot", 1, 1, 100),

                                         #shiny::numericInput("fbDesign_psize","Plot size", 30, 1, 100),


                                         shiny::numericInput("fbDesign_distPlants",
                                                             "Distance between plants (m)", .3, .1, 1),
                                         shiny::numericInput("fbDesign_distRows",
                                                             "Distance between rows (m)", .9, .1, 1),
                                         shiny::uiOutput("fbPlanting_psize", inline=TRUE),
                                         shiny::uiOutput("fbPlanting_pdensity", inline=TRUE),

                                         tags$head(tags$style("#fbDesign_pdensity{color:#191919;
                                                                                   background-color:#ecc464;
                                                                                   #font-size: 20px;
                                                                                   #font-style: italic;
                                                                                   }"
                                         )
                                         ),
                                         tags$head(tags$style("#fbDesign_psize{color:#191919;
                                                               background-color:#88e3a5;
                                                               #font-size: 20px;
                                                               #font-style: italic;
                                                                }"
                                         )
                                         )
                                                     #     )
                                                    #  )
                                                   # )
                                            )#,

                          )#,
                          #shinyBS::bsAlert("alert_fb_done")
                    )
              ),

                fluidRow(
                  HTML('<div style="float: right; margin: 0 15px 18px 0px;">'),
                  #shiny::actionButton(inputId = "refresh", label = "Refresh", icon = icon("fa fa-refresh")),
                  #shinyBS::bsButton( "fbDesign_draft", "BookView" ),
                  shiny::actionButton("fbDesign_draft", "Book Preview", icon("table"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                  shinysky::actionButton2("fbDesign_create", label = "Download", icon ="file-excel-o", icon.library = "bootstrap", styleclass= "color: #fff; background-color: #51a351; border-color: #51a351"),
                  #shiny::actionButton("fbDesign_create", "Download", icon("file-excel-o"), style="color: #fff; background-color: #51a351; border-color: #51a351"),
                  #shinyBS::bsAlert("alert_fb_done"),
                  shinysky::shinyalert("alert_fb_done", FALSE, auto.close.after = 5),
                  HTML('</div>')
                ),

                  shiny::fluidRow(
                    #box(
                    #column(width = 12,height=6,
                               shinydashboard::box(title = "Fieldbook Preview",
                                                   status = "primary",
                                                   #height = 500,
                                                   #width = NULL,
                                                   solidHeader = TRUE,
                                                   width = 12, collapsible = TRUE,
                                                   #shiny::actionButton("butNewFieldbook", "New fieldbook", inline = TRUE)#,
                                                   rhandsontable::rHandsontableOutput("fbDesign_table", height = 400)
                          ),
                          br(),
                          br(),
                          br()



                    #)
                  #)
               ),
                br(),
                br(),
                br()

          ),

ui_design_big(type="tab",title="Special Modules",name="phenotype_fieldbook_design")

######
##################  End Simple Modules

    )# End of Master tabSetPanel
 )
}

######



# shiny::tabPanel("Weather", value = "fbDesign_weather"
# ),
# shiny::tabPanel("Soil", value = "fbDesign_soil"
# ),
