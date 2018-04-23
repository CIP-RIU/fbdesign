# choices for statistical design input for HIDAP-AGROFIMS

design_choices_agrofims <- c(
  #"Unreplicated Design (UNDR)" = "UNDR",
  #"Westcott Design (WD)" = "WD",#
  "Completely Randomized Design (CRD)" = "CRD",
  "Randomized Complete Block Design (RCBD)" = "RCBD"#,
  #"Augmented Block Design (ABD)" = "ABD",
  #"Alpha Design (AD)" = "AD",
  #"Latin Square Design (LSD)" = "LSD",
  #"Factorial Two-Way Design in CRD (F2CRD)" = "F2CRD",
  #"Factorial Two-Way Design in RCBD (F2RCBD)" = "F2RCBD",
  #"Split Plot with Plots in CRD (SPCRD)" = "SPCRD",
  #"Split Plot with Plots in RCBD (SPRCBD)" = "SPRCBD", #R.Eyzaguirre recommends just one Split Design
  #"Split Plot with Plots Design" = "SPRCBD", #

  #"Split Plot with Plots in LSD (SPLSD)" = "SPLSD",
  #"Strip Plot Design (STRIP)" = "STRIP"
  # "Balanced Incomplete Block Design (BIBD)" = "BIBD",
  # "Graeco-Latin Design (GLD)" = "GLD",
  # "Youden Design (YD)" = "YD",
  # "Cyclic Design (CD)" = "CD",
  # "Lattice Design (LD)" = "LD" ,
  # #"Augmented Partially Replicated Design (APRD)" = "APRD",
  # #"Factorial Design (F2SPPD)" = "F2SPPD",
  # "North Carolina Design I" = "NCI"#,
  # #"North Carolina Design II" = "NCII",
  # #"North Carolina Design III" = "NCIII",
)

#choices for statistical design input
# genetic_design_choices <- c(
#   "North Carolina Design I" = "NCI",
#   "North Carolina Design II" = "NCII",
#   "Line by Tester" = "LXT"
# )

# Conditional panels for genetic design
# genetic_design_conditional_panels <- function(){
#   list(
#
#     shiny::conditionalPanel(
#       "input.design_geneticFieldbook == 'NCI'|
#       input.design_geneticFieldbook == 'NCII'",
#
#       shiny::selectInput("design_genetic_nc_set", "Set", 2:100, 2),
#       #shiny::selectInput("design_genetic_nc_r", "Replications", 2:100, 2),
#       shiny::selectInput("design_genetic_ploidy", "Type of ploidy", choices = c("Diploid","Tetraploid"))
#
#     ) ,
#
#     shiny::conditionalPanel(
#       "input.design_geneticFieldbook == 'LXT'|
#       input.design_geneticFieldbook == 'NCI'|
#       input.design_geneticFieldbook == 'NCII'",
#
#       #shiny::selectInput("design_genetic_nc_set", "Set", 2:100, 2),
#       shiny::selectInput("design_genetic_r", "Replications", 2:100, 2)#,
#       #shiny::selectInput("design_genetic_ploidy", "Type of ploidy", choices = c("Diploid","Tetraploid"))
#
#     ) ,
#
#     shiny::conditionalPanel(
#       "input.design_geneticFieldbook == 'LXT'",
#
#       #shiny::selectInput("design_genetic_nc_set", "Set", 2:100, 2),
#       shiny::selectInput("design_genetic_lxt_type", "Type of scheme", list("progenitors and progenie"= 1, "progenie" =2))#
#       #shiny::selectInput("design_genetic_ploidy", "Type of ploidy", choices = c("Diploid","Tetraploid"))
#
#     )
#
#
#   )
# }

# Conditional Panels or features according to each statistical design
design_conditional_panels_agrofims <- function(){
  list(
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'RCBD'  |
      input.designFieldbook_agrofims == 'CRD' |
      input.designFieldbook_agrofims == 'F2CRD'  |
      input.designFieldbook_agrofims == 'F2RCBD' |
      input.designFieldbook_agrofims == 'ABD' |
      input.designFieldbook_agrofims == 'DAU' |
      input.designFieldbook_agrofims == 'AD'  |
      input.designFieldbook_agrofims == 'SPCRD'|
      input.designFieldbook_agrofims == 'SPRCBD'|
      input.designFieldbook_agrofims == 'SPLSD'|
      input.designFieldbook_agrofims == 'STRIP'",
      # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_agrofims_r", "Replications", 2:100, 2 )

    ),

    # shiny::conditionalPanel(
    #      "input.designFieldbook_agrofims == 'UNDR'  |
    #       input.designFieldbook_agrofims == 'CRD'   |
    #       input.designFieldbook_agrofims == 'RCBD'  |
    #       input.designFieldbook_agrofims == 'F2CRD' |
    #       input.designFieldbook_agrofims == 'F2RCBD'|
    #       input.designFieldbook_agrofims == 'SPRCBD'",
    #
    #   shiny::checkboxInput("designFieldbook_agrofims_cbrwcol", "Add row and column", value = FALSE)#,

    # shiny::conditionalPanel(
    #   "input.designFieldbook_agrofims_cbrwcol == true",
    #   shiny::selectInput("designFieldbook_agrofims_expdis_colb", "Number of columns", 2:100, 10)
    # )
    # ),



    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'SPCRD' |
      input.designFieldbook_agrofims == 'SPRCBD'|
      input.designFieldbook_agrofims == 'F2CRD' |
      input.designFieldbook_agrofims == 'F2RCBD'|
      input.designFieldbook_agrofims == 'SPLSD' |
      input.designFieldbook_agrofims == 'STRIP'
      ",

      #shiny::selectInput("designFieldbook_agrofims_r", "Replications:", 1:5, 2 ),
      textInput(inputId = "factor_name", label = "Enter Additional Factor Name", ""),
      br(),
      textInput(inputId = "factor_lvl", label = "Type levels of factors (separated by commas ',')", value = "")#,
      # textInput(inputId = "factor_lvl1", label = "First Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl2", label = "Second Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl3", label = "Third Level for Additional Factor",value = "")

    ),

    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'SPCRD' |
      input.designFieldbook_agrofims == 'SPRCBD'",

      shiny::uiOutput("fbdesign_split_cb")
    ),

    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'WD'",
      shiny::selectInput("designFieldbook_agrofims_wd_col",  "Number of columns", 50:5000, 100 ),
      shiny::selectInput("designFieldbook_agrofims_wd_colb", "Number of columns between two check columns", 2:100, 10)

    ),


    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'ABD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Augmented design needs at least 2 checks. Verify if your material have these checks and put an 'x' mark in 'Is_control' column for each check.
              Otherwise, Augmented Design does not work.",

              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),

    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'WD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Westcott design needs 2 checks and at least 10 genotypes. Verify if your material list has these checks and put an 'x' mark in 'Is_control' column for each check.
              Otherwise, Westcott Design does not work.",

              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),


    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'LD'", # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_agrofims_r", "Replications", 2:100, 2 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'BIBD'",
      shiny::selectInput("designFieldbook_agrofims_maxR", "Repetition maximum (k)", 3:100, 20 )
    ) ,
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'AD' |
      input.designFieldbook_agrofims == 'BIBD'",
      # TODO do server side checking of permitted combinations (based on number of treatments)
      # trt = k * s
      # s = number of blocks
      # k = size of block (max is trt)
      #
      # r = 2: k <= s
      # r = 3: s odd; k <= s
      # r = 3: s even; k <= (s - 1)
      # r = 4: s odd but not multiple of 3; k <= s
      #shiny::selectInput("designFieldbook_agrofims_r", "Replications:", 2:6000, 2 ),
      shiny::selectInput("designFieldbook_agrofims_k", "Block size (k)", 2:100, 4 )
    ),

    shiny::conditionalPanel(

      "input.designFieldbook_agrofims == 'AD'", # TODO: ab - factorial, split
      fluidRow(
        shiny::wellPanel(
          shiny::HTML("<b>ALPHA CONDITION:</b>"),
          textOutput("alphaMessage")
        )
      )
    ),

    #     shiny::conditionalPanel(
    #       "input.designFieldbook_agrofims == 'CD'",
    #       # TODO do server side checking of permitted combinations (based on number of treatments)
    #       # number of treatments 6:30
    #       shiny::selectInput("designFieldbook_agrofims_r", "Replications:", 2:10, 2 ),
    #       shiny::selectInput("designFieldbook_agrofims_k", "Block size (k):", 2:10,3 )
    #     ),

    ### Combine factors in statistical designs ########################
    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'UNDR' |
      input.designFieldbook_agrofims == 'CRD'   |
      input.designFieldbook_agrofims == 'RCBD'  |
      input.designFieldbook_agrofims == 'WD'    |
      input.designFieldbook_agrofims == 'AD'    |
      input.designFieldbook_agrofims == 'LSD'   |
      input.designFieldbook_agrofims == 'ABD'",
      shiny::checkboxInput("designFieldbook_agrofims_combfactor", "Add Factor",value = FALSE  ),

      shiny::conditionalPanel(
        "input.designFieldbook_agrofims_combfactor == true",

        textInput(inputId = "combfactor_name", label = "Combine Factor Name", ""),
        br(),
        textInput(inputId = "combfactor_lvl", label = "Type levels of factors (separated by commas ',')", value = "")#,

      )

    ),
    #########################################

    ###############

    shiny::conditionalPanel(
      "input.designFieldbook_agrofims == 'UNDR'  |
      input.designFieldbook_agrofims == 'CRD'   |
      input.designFieldbook_agrofims == 'RCBD'  |
      input.designFieldbook_agrofims == 'WD'    |
      input.designFieldbook_agrofims == 'AD'    |
      input.designFieldbook_agrofims == 'LSD'   |
      input.designFieldbook_agrofims == 'ABD'",
      shiny::checkboxInput("designFieldbook_agrofims_cbssample", "Add sub samples", value = FALSE  ),

      shiny::conditionalPanel(
        "input.designFieldbook_agrofims_cbssample == true",
        shiny::numericInput(inputId = "designFieldbook_agrofims_nsample", label = "Enter samples", value = 10, min = 1, max = 1000)
      )

    )#,



  )
}




#' shiny UI element for HIDAP-AGROFIMS
#'
#' returns a re-usable user interface element
#'
#' @author Reinhard Simon
#' @param type type of ui element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @export


################## Begin Simple Modules

ui_fieldbook_agrofims <- function(type="tab",title="Design Fieldbook",name="phenotype_fieldbook_design"){

  #############
  shinydashboard::tabItem(tabName = name,
                          h1("Experiment information"),

                          shinyjs::useShinyjs(), #to reset panels and UI

                          tags$style(HTML("

                              .box.box-solid.box-warning>.box-header {
                                color:#000;
                                background:#f5f5f5;
                                /*padding-top:0px*/
                              }

                              .box.box-solid.box-warning>.box-body {
                                color:#000;
                              background:#f5f5f5
                              }

                              .box.box-solid.box-warning{
                              border-bottom-color:#f5f5f5;
                              border-left-color:#f5f5f5;
                              border-right-color:#f5f5f5;
                              border-top-color:#f5f5f5;
                              }


                      ")),


                          tags$head(
                            tags$style(HTML("
                                            .shiny-output-error-validation {
                                            color: green;
                                            font-size: 120%;
                                            font-family: Arial, Helvetica, sans-serif;
                                            }
                                            "))
                            ),

                          tags$head(tags$style(
                            HTML('
                               #sidebar {
                                  padding: 19px 20px 20px;
                                  margin-top: 20px;
                                  margin-bottom: 20px;
                                  background-color: #f5f5f5;
                                  border-top: 1px solid #e5e5e5;
                               }
                              .well {
                                  border-radius: 0px;

                                  border: 0px solid #e3e3e3;
                                 }'
                            )
                          )),

                          # fluidRow(
                          #   box()
                          # ),
                          # tabsetPanel(id = "inExpInfo", #Begin Master tabSetPanel

                           # tabPanel(" ", #primer tab principal
                                    # column(width = 12,

                            # tabPanel("Create fieldbook",icon = icon("tag", lib = "glyphicon"),
                                     # br(),
                                     # shiny::wellPanel(
                                     #   shiny::HTML("<b>Fieldbook ID </b>"),
                                     #   shiny::textOutput("fbDesign_id")
                                     # ),

                                     fluidRow(
                                       box(
                                         title = tagList(shiny::icon("plus-circle"), "Create fieldbook"), status = "primary", solidHeader = TRUE, icon = icon("plus-circle"),
                                         collapsible = TRUE, width = 12,

                                         #shiny::tabsetPanel(# id = "fbDesignNav",
                                         fluidRow(
                                         shinydashboard::tabBox(id = "fbDesignNav", height = NULL, width = 12,
                                         # box(title = "", solidHeader = TRUE, status = "primary", width=12,
                                            #tabsetPanel(id = "inExpInfo", #Begin Master tabSetPanel

                                              shiny::tabPanel("Experiment", value = "experiment", icon = shiny::icon("info"),
                                                  fluidRow(
                                                    column(width = 6,
                                                           h2("Experiment details"),
                                                           disabled(textInput(inputId = "experimentId", label = "Experiment ID", value = "")),
                                                           textInput(inputId = "experimentName", label = "Experiment name", value = ""),
                                                           textInput(inputId = "experimentProjectName", label = "Experiment project name", value = ""),

                                                           shiny::dateRangeInput("fbDesign_project_time_line", "Experiment date", start = Sys.Date() - 2,
                                                                                 end = Sys.Date() + 20, startview = "year",format = "dd/mm/yyyy"),
                                                           dateInput("Embargo_date", label ="Embargo end date", format = "dd/mm/yyyy"),
                                                           # shiny::selectInput("designFieldbook_expSeason", "Experiment season", choices = c("Summer","Spring","Winter", "Autumn")),
                                                           shiny::selectInput("designFieldbook_typeExperiment", "Type of experiment", choices = c("Select one...","Controlled treatment trial","Observation trial","Varietal trial", "Demonstration trial", "Germplasm screening trial")),


                                                           textAreaInput(inputId = "experimentObj", label = "Experiment objective", value = ""),

                                                           # hr(),
                                                           br(),
                                                           h2("Funding agency associated with experiment"),

                                                           # shiny::selectInput("designFieldbook_fundAgency", "Funding agency type", choices = c("Government","National Agricultural Extension","Advisory Services", "International NGO", "Farmers Organization")),


                                                           textInput(inputId = "fundName", label = "Funding agency name", value = ""),
                                                           textInput(inputId = "contCenter", label = "Contribuitor Center", value = ""),
                                                           textInput(inputId = "contCRP", label = "Contribuitor CRP", value = ""),
                                                           textInput(inputId = "contResearcher", label = "Contribuitor researcher", value = ""),

                                                           br(),
                                                           h2("Institution/Entity associated with experiment"),

                                                           shiny::selectInput("designFieldbook_fundLeadAgency", "Experiment, lead organization type", choices = c("Select one...","University","University, main campus","Agricultural experimental extension", "Government research institution (NARS)","Government research institution, designated laboratory or center", "PRivate company", "Farm", "Farmer association or cooperative", "Non-governmental organization", "Extension organization", "CGIAR center" )),
                                                           # shiny::selectInput("designFieldbook_fundHostAgency", "Experiment, host organization type", choices = c("University","University, main campus","Agricultural experimental extension", "Government research institution (NARS)")),
                                                           textInput(inputId = "leadName", label = "Experiment, lead organization name", value = ""),

                                                           br(),
                                                           h2("Site information"),

                                                           shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),
                                                           shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500) #,#locality
                                                    )
                                                  ),
                                                  fluidRow(
                                                    sidebarPanel(id="sidebar", width = 12,
                                                                 actionButton("btnNextPersonnelInfo", "Next", class = "btn-primary",style="color: #fff;")
                                                    )
                                                  )
                                              ),

                                              tabPanel("Personnel", value = "tabPersonnel", icon = shiny::icon("user"),
                                                # column( width = 12,
                                                  fluidRow(
                                                    # fluidRow(
                                                      column( width = 6,
                                                      h2("Personnel associated with the experiment"),
                                                      selectInput(inputId = "npersons", label = "Number of personnel", choices = 1:5)
                                                      ),
                                                    # ),

                                                    # fluidRow(
                                                           box(
                                                             title = tagList(shiny::icon("user"), "Personnel #1"), solidHeader = TRUE, status = "warning", width=12,

                                                              fluidRow(
                                                                column(width = 6, shiny::selectInput("personnel1Type", "Personnel type", choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                column(width = 6, textInput(inputId = "person1Email", label = "Person email", value = ""))
                                                              ),
                                                             fluidRow(
                                                               column(width=6,

                                                                      textInput(inputId = "person1FirstName", label = "Person first name", value = ""),
                                                                      textInput(inputId = "person1LastName", label = "Person last name", value = "")
                                                                      # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                               ),
                                                               column(width=6,

                                                                      textInput(inputId = "person1Afiliation", label = "Person afiliation", value = ""),
                                                                      textInput(inputId = "person1ORCID", label = "Person ORCID", value = "")
                                                               )
                                                              )
                                                         ),
                                                    # ),


                                                    conditionalPanel("input.npersons == 2  |
                                                                     input.npersons == 3  |
                                                                     input.npersons == 4 |
                                                                     input.npersons == 5",
                                                      # fluidRow(
                                                        box(
                                                          title = tagList(shiny::icon("user"), "Personnel #2"), solidHeader = TRUE, status = "warning", width=12,

                                                                fluidRow(
                                                                  column(width = 6, shiny::selectInput("personnel2Type", "Personnel type", choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                  column(width = 6, textInput(inputId = "person2Email", label = "Person email", value = ""))
                                                                ),

                                                          fluidRow(
                                                              column(width=6,

                                                                     textInput(inputId = "person2FirstName", label = "Person first name", value = ""),
                                                                     textInput(inputId = "person2LastName", label = "Person last name", value = "")
                                                                     # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                              ),
                                                              column(width=6,
                                                                     textInput(inputId = "person2Afiliation", label = "Person afiliation", value = ""),
                                                                     textInput(inputId = "person2ORCID", label = "Person ORCID", value = "")
                                                              )
                                                          )
                                                      )#)

                                                    ),

                                                    conditionalPanel("input.npersons == 3  |
                                                                     input.npersons == 4 |
                                                                     input.npersons == 5",
                                                                     # column(width = 12, br()),
                                                                     # fluidRow(
                                                                       box(
                                                                         title = tagList(shiny::icon("user"), "Personnel #3"), solidHeader = TRUE, status = "warning", width=12,
                                                                             fluidRow(
                                                                               column(width = 6, shiny::selectInput("personnel3Type", "Personnel type", choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                               column(width = 6, textInput(inputId = "person3Email", label = "Person email", value = ""))
                                                                             ),
                                                                         fluidRow(
                                                                            column(width=6,
                                                                                   textInput(inputId = "person3FirstName", label = "Person first name", value = ""),
                                                                                   textInput(inputId = "person3LastName", label = "Person last name", value = "")
                                                                                   # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                            ),
                                                                            column(width=6,
                                                                                   textInput(inputId = "person3Afiliation", label = "Person afiliation", value = ""),
                                                                                   textInput(inputId = "person3ORCID", label = "Person ORCID", value = "")
                                                                            )
                                                                         )
                                                                      )
                                                                     # )

                                                    ),


                                                    conditionalPanel("input.npersons == 4 |
                                                                     input.npersons == 5",

                                                                     # fluidRow(
                                                                       box(
                                                                         title = tagList(shiny::icon("user"), "Personnel #4"), solidHeader = TRUE, status = "warning", width=12,
                                                                           fluidRow(
                                                                             column(width = 6, shiny::selectInput("personnel4Type", "Personnel type", choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                             column(width = 6, textInput(inputId = "person4Email", label = "Person email", value = ""))
                                                                           ),
                                                                            fluidRow(
                                                                              column(width=6,
                                                                                     textInput(inputId = "person4FirstName", label = "Person first name", value = ""),
                                                                                     textInput(inputId = "person4LastName", label = "Person last name", value = "")
                                                                                     # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                              ),
                                                                              column(width=6,
                                                                                     textInput(inputId = "person4Afiliation", label = "Person afiliation", value = ""),
                                                                                     textInput(inputId = "person4ORCID", label = "Person ORCID", value = "")
                                                                              )
                                                                            )
                                                                     )#)

                                                    ),
                                                    conditionalPanel("input.npersons == 5",

                                                                     # fluidRow(
                                                                       box(
                                                                         title = tagList(shiny::icon("user"), "Personnel #5"), solidHeader = TRUE, status = "warning", width=12,

                                                                         fluidRow(
                                                                           column(width = 6, shiny::selectInput("personnel5Type", "Personnel type", choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                           column(width = 6, textInput(inputId = "person5Email", label = "Person email", value = ""))
                                                                         ),

                                                                         fluidRow(
                                                                           column(width=6,

                                                                                  textInput(inputId = "person5FirstName", label = "Person first name", value = ""),
                                                                                  textInput(inputId = "person5LastName", label = "Person last name", value = "")
                                                                                  # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                           ),
                                                                           column(width=6,
                                                                                  textInput(inputId = "person5Afiliation", label = "Person afiliation", value = ""),
                                                                                  textInput(inputId = "person5ORCID", label = "Person ORCID", value = "")
                                                                           )
                                                                         )
                                                                       )
                                                                     #)

                                                    )


                                                  ),
                                                # fluidRow(
                                                #   column(width = 6, align = "left",
                                                #          br(),
                                                #          actionButton(inputId = "btnNextCropInfo", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                # )
                                                fluidRow(
                                                  sidebarPanel(id="sidebar", width = 12,
                                                               actionButton("btnNextCropInfo", "Next", class = "btn-primary",style="color: #fff;")
                                                  )
                                                )
                                                # )
                                              ),

                                              shiny::tabPanel("Crop", value = "tabCropInfo", icon = shiny::icon("pagelines"),
                                                  fluidRow(
                                                    column( width = 6,
                                                            h2("Description of crops sown"),

                                                            shiny::selectInput("croppingType", "Cropping type", choices = c("Monocrop", "Intercrop"))
                                                    ),
                                                    column(width = 12,

                                                           conditionalPanel("input.croppingType == 'Monocrop'",
                                                                            fluidRow(
                                                                                  column(width = 12,

                                                                                  h2("Crop information"),

                                                                                    fluidRow(


                                                                                      column(width = 6,
                                                                                             # textInput(inputId = "cropCommonNameMono", label = "Crop common name", value = "")
                                                                                             selectInput("cropCommonNameMono", "Crop common name", choices = c("Maize", "Bean", "Potato", "Cassava", "Sweetpotato"))

                                                                                      ),
                                                                                      column(width = 6,
                                                                                             column(width = 10,
                                                                                                    #textInput(inputId = "cropVarietyNameMono", label = "Crop variety name", value = "")
                                                                                                    selectizeInput("cropVarietyNameMono", "Crop variety name", c(), multiple = TRUE, options = list(
                                                                                                      placeholder = "ex.  variety1   variety2    ",
                                                                                                      'create' = TRUE,
                                                                                                      'persist' = FALSE)
                                                                                                    )#,


                                                                                             ),

                                                                                             column(width = 2, br(),
                                                                                                    checkboxInput("setCropFactor", "Set as factor", FALSE)
                                                                                             )
                                                                                      )
                                                                                    ),
                                                                                    fluidRow(
                                                                                      column(width = 6,
                                                                                             textInput(inputId = "cropLatinNameMono", label = "Crop latin name", value = "")
                                                                                      ),
                                                                                      column(width = 6,
                                                                                             textInput(inputId = "cultivarNameMono", label = "Cultivar Name", value = "")
                                                                                      )
                                                                                    )
                                                                                )
                                                                            ),
                                                                            br(),
                                                                            h2("Previous crop"),
                                                                            fluidRow(
                                                                              column( width = 6,
                                                                                      textInput(inputId = "numPreviousCrop", label = "Number of previous crop", value = ""),
                                                                                      textInput(inputId = "prevCropName", label = "Previous crop name", value = ""),
                                                                                      textInput(inputId = "prevCropVar", label = "Previous crop variety", value = "")
                                                                              )
                                                                            )

                                                           ),


                                                          conditionalPanel("input.croppingType == 'Intercrop'",
                                                                 fluidRow(
                                                                   column( width = 6,
                                                                           selectizeInput("cropsSelected",label="Select crops (Max. 3)", selected=NULL, multiple = TRUE, choices=c("Maize", "Soybean", "Potato", "Cassava"), options = list(maxItems = 3))
                                                                   )

                                                                 ),

                                                                 conditionalPanel("input.cropsSelected != null && input.cropsSelected.length > 0",
                                                                                  h2("Crop information"),
                                                                                  # print(input.cropsSelected),
                                                                                  box(title = "Crop #1", solidHeader = TRUE, status = "warning", width=12,
                                                                                      fluidRow(
                                                                                        column(width = 6,
                                                                                               disabled(textInput(inputId = "cropCommonName1", label = "Crop common name", value = "")),
                                                                                               textInput(inputId = "cropLatinName", label = "Crop latin name", value = "")

                                                                                        ),
                                                                                        column(width = 6,
                                                                                               textInput(inputId = "cropVarietyName", label = "Crop variety name", value = ""),
                                                                                               textInput(inputId = "cultivarName", label = "Cultivar Name", value = "")
                                                                                        )
                                                                                      )
                                                                                  )
                                                                 ),

                                                                 conditionalPanel("input.cropsSelected != null && input.cropsSelected.length > 1",
                                                                                  box(title = "Crop #2", solidHeader = TRUE, status = "warning", width=12,
                                                                                      column(width = 6,
                                                                                             disabled(textInput(inputId = "cropCommonName2", label = "Crop common name", value = "")),
                                                                                             textInput(inputId = "cropLatinName2", label = "Crop latin name", value = "")

                                                                                      ),
                                                                                      column(width = 6,
                                                                                             textInput(inputId = "cropVarietyName2", label = "Crop variety name", value = ""),
                                                                                             textInput(inputId = "cultivarName2", label = "Cultivar Name", value = "")
                                                                                      )
                                                                                  )
                                                                 ),

                                                                 conditionalPanel("input.cropsSelected != null && input.cropsSelected.length == 3",
                                                                                  box(title = "Crop #3", solidHeader = TRUE, status = "warning", width=12,
                                                                                      column(width = 6,
                                                                                             disabled(textInput(inputId = "cropCommonName3", label = "Crop common name", value = "")),
                                                                                             textInput(inputId = "cropLatinName3", label = "Crop latin name", value = "")

                                                                                      ),
                                                                                      column(width = 6,
                                                                                             textInput(inputId = "cropVarietyName3", label = "Crop variety name", value = ""),
                                                                                             textInput(inputId = "cultivarName3", label = "Cultivar Name", value = "")
                                                                                      )
                                                                                  )
                                                                 )
                                                          ),



                                                          # fluidRow(
                                                          #   column(width = 6, align = "left",
                                                          #          br(),
                                                          #          actionButton(inputId = "btnDesign", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                          # )
                                                          fluidRow(
                                                            sidebarPanel(id="sidebar", width = 12,
                                                                         actionButton("btnDesign", "Next", class = "btn-primary",style="color: #fff;")
                                                            )
                                                          )
                                                    )
                                                  )
                                              ),

                                              #BEGIN (STATISTICAL DESGIGN)
                                              shiny::tabPanel("Design", value = "tabDesign", icon = shiny::icon("th-list"),
                                                    # br(),
                                                    # br(),
                                                    h2("Design information"),

                                                    tags$style(HTML("

                                                              #lvl_hdafims1 + div> div>.item {
                                                              background:   #337ab7 !important;
                                                              color: white;
                                                              }

                                                              #lvl_hdafims1 + div> div>.selectize-dropdown-content .active {
                                                              background:   #337ab7 !important;
                                                              }

                                                              #lvl_hdafims2 + div> div>.item {
                                                              background:   #f3217a !important;
                                                              color: white;
                                                              }

                                                              #lvl_hdafims2 + div> div>.selectize-dropdown-content .active {
                                                              background:   #f3217a !important;
                                                              }

                                                              #lvl_hdafims3 + div> div>.item {
                                                              background:   #777 !important;
                                                              color: white;
                                                              }

                                                              #lvl_hdafims3 + div> div>.selectize-dropdown-content .active {
                                                              background:   #777 !important;
                                                              }

                                                              #lvl_hdafims4 + div> div>.item {
                                                              background:   #5cb85c !important;
                                                              color: white;
                                                              }

                                                              #lvl_hdafims4 + div> div>.selectize-dropdown-content .active {
                                                              background:   #5cb85c !important;
                                                              }

                                                              #lvl_hdafims5 + div> div>.item {
                                                              background:   #FBBF09 !important;
                                                              color: white;
                                                              }

                                                              #lvl_hdafims5 + div> div>.selectize-dropdown-content .active {
                                                              background:   #FBBF09 !important;
                                                              }"

                                                    )),

                                                    fluidRow(
                                                      column(width = 6,
                                                             #Select statistical design
                                                             shiny::selectInput("designFieldbook_agrofims", "Select Design/Arragement",  c("Choose one" = "", design_choices_agrofims), selected = 'CRD',
                                                                                multiple = FALSE),

                                                             shiny::selectInput("designFieldbook_agrofims_r", "Replications", 2:100, 2 ),



                                                             #select number of factors
                                                             selectInput(inputId = "nfactors_hdafims", label = "Number of factor", choices = 2:5)
                                                      )
                                                    ),

                                                    # desing of experiment with 1 factors
                                                    conditionalPanel("input.nfactors_hdafims == 1  |
                                                                     input.nfactors_hdafims == 2  |
                                                                     input.nfactors_hdafims == 3  |
                                                                     input.nfactors_hdafims == 4  |
                                                                     input.nfactors_hdafims == 5",


                                                                     fluidRow(
                                                                       column(6, #begin column

                                                                              # div(style="display:inline-block",
                                                                              textInput(inputId = "factor_hdafims1", label = "#1 Factor name", placeholder = "ex.  Fertilization"
                                                                              )
                                                                              # selectizeInput("factor_hdafims1", "Factor name", c(), multiple = TRUE, options = list(
                                                                              #                                    'maxItems' = 2,
                                                                              #                                    'create' = TRUE,
                                                                              #                                    'persist' = FALSE)
                                                                              # )
                                                                              #  ),
                                                                       )  , #end column



                                                                       #div(style="display:inline-block",
                                                                       column(6, #begin column
                                                                              selectizeInput("lvl_hdafims1", "Levels", c(), multiple = TRUE, options = list(
                                                                                placeholder = "ex.  0mg 200mg 500mg",
                                                                                'create' = TRUE,
                                                                                'persist' = FALSE)
                                                                              )#,
                                                                       ) #end column
                                                                     ) #end fluidrow
                                                                     #)
                                                    ),
                                                    # desing of experiment with 2 factors
                                                    conditionalPanel("input.nfactors_hdafims == 2  |
                                                                     input.nfactors_hdafims == 3  |
                                                                     input.nfactors_hdafims == 4  |
                                                                     input.nfactors_hdafims == 5 ",

                                                                     #div(style="display:inline-block",
                                                                     fluidRow(
                                                                       column(6, #begin column

                                                                              textInput(inputId = "factor_hdafims2", label = "#2 Factor name", placeholder = "ex.  Irrigation")


                                                                              # selectizeInput("factor_hdafims2", "Factor name", c(), multiple = FALSE, options = list(
                                                                              #                                   'maxItems' = 1,
                                                                              #                                   'create' = TRUE,
                                                                              #                                   'persist' = FALSE)
                                                                              # )
                                                                              #),
                                                                       ), #end column

                                                                       column(6,#begin column
                                                                              #div(style="display:inline-block",
                                                                              selectizeInput("lvl_hdafims2", "Levels", c(), multiple = TRUE, options = list(
                                                                                placeholder = "ex.  deficit irrigation   normal irrigation ",
                                                                                'create' = TRUE,
                                                                                'persist' = FALSE)
                                                                              )#,
                                                                       ) #end column
                                                                     ) #end fluidrow
                                                                     #)
                                                    ),

                                                    # desing of experiment with 3 factors
                                                    conditionalPanel("input.nfactors_hdafims == 3 |
                                                                     input.nfactors_hdafims == 4 |
                                                                     input.nfactors_hdafims == 5 ",

                                                                     #div(style="display:inline-block",
                                                                     fluidRow(
                                                                       column(6, #begin column

                                                                              textInput(inputId = "factor_hdafims3", label = "#3 Factor name", placeholder = "ex.  Time growing")
                                                                              # selectizeInput("factor_hdafims3", "Factor name", c(), multiple = FALSE, options = list(
                                                                              #                                   'maxItems' = 1,
                                                                              #                                   'create' = TRUE,
                                                                              #                                   'persist' = FALSE)
                                                                              # )
                                                                       ) ,#end column
                                                                       #),

                                                                       column(6, #begin column
                                                                              #div(style="display:inline-block",
                                                                              selectizeInput("lvl_hdafims3", "Levels", c(), multiple = TRUE, options = list(
                                                                                placeholder = "ex.  0days  60days  90days",
                                                                                'create' = TRUE,
                                                                                'persist' = FALSE)
                                                                              )#,
                                                                       ) #end column
                                                                     ) #end fluidrow

                                                    ),
                                                     # desing of experiment with 4 factors
                                                    conditionalPanel("input.nfactors_hdafims == 4 |
                                                                     input.nfactors_hdafims == 5 ",

                                                                     #div(style="display:inline-block",
                                                                     fluidRow(
                                                                       column(6, #begin column

                                                                              textInput(inputId = "factor_hdafims4", label = "#4 Factor name", placeholder = "ex.  Green Tree retention")
                                                                              # selectizeInput("factor_hdafims4", "Factor name", c(), multiple = FALSE, options = list(
                                                                              #                                   'maxItems' = 1,
                                                                              #                                   'create' = TRUE,
                                                                              #                                   'persist' = FALSE)
                                                                              # )
                                                                              #),
                                                                       ),

                                                                       column(6, #begin column
                                                                              #div(style="display:inline-block",
                                                                              selectizeInput("lvl_hdafims4", "Levels", c(), multiple = TRUE, options = list(
                                                                                placeholder = "ex.  50m3/ha   10m3/ha    0m3/ha    no-cutting",
                                                                                'create' = TRUE,
                                                                                'persist' = FALSE)
                                                                              )#,
                                                                       ) #end column
                                                                     ) #end fluidrow
                                                    ),
                                                    # desing of experiment with 5 factors
                                                    conditionalPanel("input.nfactors_hdafims == 5",

                                                                     fluidRow(
                                                                       column(6, #begin column

                                                                              #div(style="display:inline-block",
                                                                              textInput(inputId = "factor_hdafims5", label = "#5 Factor name", placeholder = "ex.  Water conditions")
                                                                              # selectizeInput("factor_hdafims5", "Factor name", c(), multiple = TRUE, options = list(
                                                                              #                                   'maxItems' = 1,
                                                                              #                                   'create' = TRUE,
                                                                              #                                   'persist' = FALSE)
                                                                              # )
                                                                              #),
                                                                       ),

                                                                       column(6, #begin column
                                                                              #div(style="display:inline-block",
                                                                              selectizeInput("lvl_hdafims5", "Levels", c(), multiple = TRUE, options = list(
                                                                                placeholder = "ex.   confined waters    freshly inundated_areas",
                                                                                'create' = TRUE,
                                                                                'persist' = FALSE)
                                                                              )#,
                                                                       ) #end column
                                                                     ) #end fluidrow


                                                    )#,
                                                    ,
                                                    # fluidRow(
                                                    #   column(width = 6, align = "left",
                                                    #          br(),
                                                    #          actionButton(inputId = "btnNextPlotInfo", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                    # )
                                                    fluidRow(
                                                      sidebarPanel(id="sidebar", width = 12,
                                                                   actionButton("btnNextPlotInfo", "Next", class = "btn-primary",style="color: #fff;")
                                                      )
                                                    )




                                                    #downloadLink("downloadData", "Download")



                                             ), #END DESIGN (STATISTICAL DESIGN)

                                             shiny::tabPanel("Plot", value = "tabPlotInfo", icon = shiny::icon("th"),

                                                 h2("Plot information"),
                                                 fluidRow(
                                                   column(width = 6,
                                                          textInput(inputId = "plotSpacing", label = "Plot spacing", value = ""),

                                                          textInput(inputId = "numPlantsPerPlot", label = "Number of plants planted per plot", value = ""),
                                                          textInput(inputId = "numRowsPerPlot", label = "Number of rows per plot", value = ""),
                                                          textInput(inputId = "numPlantsPerRow", label = "Number of plants per row", value = ""),
                                                          textInput(inputId = "plotSize", label = "Plot size (m2)", value = ""),

                                                          textInput(inputId = "rowSpacing", label = "Row spacing", value = ""),
                                                          textInput(inputId = "rowOrientation", label = "Row orientation", value = ""),

                                                          textInput(inputId = "spaceBwPlants", label = "Space between plants", value = ""),
                                                          textInput(inputId = "spaceBwRows", label = "Space between rows", value = ""),
                                                          textInput(inputId = "planDensity", label = "Plant density (plant/Ha)", value = ""),

                                                          textInput(inputId = "hillSpacing", label = "Hill spacing", value = ""),
                                                          textInput(inputId = "numPlantsPerPlot", label = "Number of measured plants per plot", value = "")
                                                   )
                                                 ),
                                                 # fluidRow(
                                                 #   column(width = 6, align = "left",
                                                 #          br(),
                                                 #          actionButton(inputId = "btnNextAgro", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                 # )
                                                 fluidRow(
                                                   sidebarPanel(id="sidebar", width = 12,
                                                                actionButton("btnNextAgro", "Next", class = "btn-primary",style="color: #fff;")
                                                   )
                                                 )

                                             ),

                                             # shiny::tabPanel("Agro features", value = "tabAgroFeat", icon = shiny::icon("leaf"),
                                             #      br(),
                                             #      # shinyTree::shinyTree("designFieldbook_traits_hdagrofims",search = TRUE,checkbox = TRUE)
                                             #      #list <- names(Agronomic_features$`Agronomic features`),
                                             #      #selectizeInput(inputId = "selectFeatures", label = "Select features", choices =list, options = list(maxItems = 8, minItems=1)),
                                             #      shinyTree::shinyTree("treeFeatures",search = TRUE,checkbox = TRUE, theme = "proton"),
                                             #      fluidRow(
                                             #        column(width = 6, align = "left",
                                             #               br(),
                                             #               actionButton(inputId = "btnNextTraits", label = "Next", style="color: #fff; background-color: #35b872;"))
                                             #      )
                                             #
                                             #  ),

                                             shiny::tabPanel("Agro-features", value = "tabAgroFeat2", icon = shiny::icon("truck"),
                                                    h2("Agronomic features"),
                                                    fluidRow(
                                                      # fluidRow(
                                                      #br(),

                                                        column(width = 6,
                                                               selectizeInput("selectAgroFeature", "Agronomic feature", c(), multiple = TRUE, choices=c("Land preparation", "Mulching"),options = list(maxItems = 8, placeholder = "Select some...")
                                                                )
                                                        ),
                                                      # ),

                                                      br(),
                                                      column(width = 12,

                                                      br(),
                                                      # tabsetPanel(
                                                      fluidRow(
                                                      tabBox(height = NULL, width = 12,
                                                        tabPanel("Land Preparation", value="tabLandPr",
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                 #br(),
                                                                 h2("Land Preparation"),
                                                                 fluidRow(
                                                                 box(title = "Land Levelling",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,


                                                                    fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            textInput("numPasses", value="", label = "Number of Passes"),
                                                                            textInput("operationsOrder", value="", label = "Operations order")
                                                                          ),
                                                                     column(width = 6,
                                                                            br(),
                                                                        fluidRow(
                                                                         box(
                                                                           title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                             selectInput("impl_type", label = "Type", choices = c("Drag bucket",
                                                                                                                                  "Harrow",
                                                                                                                                  "Laser-controlled",
                                                                                                                                  "Leveling board",
                                                                                                                                  "Other - specify",
                                                                                                                                  "Tractor blade",
                                                                                                                                  "Disk harrow")
                                                                             ),
                                                                             selectInput("animal_traction", label = "Animal Traction", choices =
                                                                                                                                c("Buffalo",
                                                                                                                                  "Camel",
                                                                                                                                  "Donkey",
                                                                                                                                  "Elephant",
                                                                                                                                  "Horse",
                                                                                                                                  "Mule",
                                                                                                                                  "Ox / Bullock / Steer"
                                                                                                                                )
                                                                             ),
                                                                             textInput("humanPowered", value="", label = "Human powered"),
                                                                             selectInput("animal_traction", label = "Motorized Traction", choices =
                                                                                                                                 c("2 wheel tractor",
                                                                                                                                    "4 wheel tractor"
                                                                                                                                 )
                                                                             )
                                                                        ))
                                                                    ))
                                                                )),
                                                                fluidRow(
                                                                 box(title = "Puddling",
                                                                     solidHeader = TRUE,
                                                                     status = "primary",
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                        column(width = 6,
                                                                               fluidRow(
                                                                                  column(width = 6,
                                                                                         dateInput("puddling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                  ),
                                                                                  column(width = 6,
                                                                                         dateInput("puddling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                                  )
                                                                               )
                                                                         )
                                                                      ),
                                                                     fluidRow(
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                             box(
                                                                               title = "Method", solidHeader = TRUE, status = "warning", width=12,
                                                                                      textInput("Penetrometer_in_field", value="", label = "Penetrometer in field"),
                                                                                      fluidRow(
                                                                                        column(width = 6,
                                                                                               textInput("puddling_depth_val", label="Puddling depth", value="")
                                                                                        ),
                                                                                        column(width = 6,
                                                                                              selectInput("puddling_depth_unit", label="Unit", choices = c("cm", "m"))
                                                                                        )
                                                                                      )
                                                                             ))
                                                                         ),
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                             box(
                                                                               title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                                 selectInput("pud_animal_traction", label = "Animal traction", choices =
                                                                                                                                             c("Buffalo",
                                                                                                                                               "Camel",
                                                                                                                                               "Donkey",
                                                                                                                                               "Elephant",
                                                                                                                                               "Horse",
                                                                                                                                               "Mule",
                                                                                                                                               "Ox / Bullock / Steer"
                                                                                                                                             )
                                                                                 ),
                                                                                 textInput("pud_humanPowered", value="", label = "Human powered"),
                                                                                 selectInput("pud_animal_traction", label = "Motorized traction", choices =
                                                                                                                                         c("2 wheel tractor",
                                                                                                                                           "4 wheel tractor"
                                                                                                                                         )
                                                                                 )
                                                                             )
                                                                          ))
                                                                     )


                                                                 )),
                                                                fluidRow(
                                                                 box(title = "Tillage",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("tillage_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("tillage_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectInput("till_technique",  label = "Technique", choices =
                                                                                                                        c("Conventional tillage",
                                                                                                                          "Mulch-till",
                                                                                                                          "No-till",
                                                                                                                          "Other - specify",
                                                                                                                          "Puddling",
                                                                                                                          "Reduced tillage",
                                                                                                                          "Strip-till"
                                                                                                                        )
                                                                                          ),
                                                                            textInput("till_depth_method", value="", label = "Depth method"),
                                                                            textInput("till_depth", value="", label = "Depth"),
                                                                            textInput("till_total_op_season", value="", label = "Total operation for a season")

                                                                     ),
                                                                     column(width = 6,
                                                                            br(),
                                                                            fluidRow(
                                                                         box(
                                                                           title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                               selectInput("till_impl_type", label = "Type", choices = c("Chisel plough",
                                                                                                                                         "Clod crusher",
                                                                                                                                         "Cultivator",
                                                                                                                                         "Disc plough",
                                                                                                                                         "Hand plough",
                                                                                                                                         "Other - specify",
                                                                                                                                         "Paraplow",
                                                                                                                                         "Ridging plough",
                                                                                                                                         "Spade plough",
                                                                                                                                         "Subsoiler")
                                                                               ),
                                                                               selectInput("till_animal_traction", label = "Animal traction", choices =
                                                                                             c("Buffalo",
                                                                                               "Camel",
                                                                                               "Donkey",
                                                                                               "Elephant",
                                                                                               "Horse",
                                                                                               "Mule",
                                                                                               "Ox / Bullock / Steer"
                                                                                             )
                                                                               ),

                                                                               textInput("till_humanPowered", value="", label = "Human powered"),
                                                                               selectInput("till_animal_traction", label = "Motorized traction", choices =
                                                                                             c("2 wheel tractor",
                                                                                               "4 wheel tractor"
                                                                                             )
                                                                               )
                                                                         ))
                                                                    )

                                                                  )
                                                                 )),
                                                                fluidRow(
                                                                 box(title = "Liming",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("liming_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("liming_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            textInput("lim_material", label="Material", value=""),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     textInput("lim_quantity", value = "", label="Quantity")
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectInput("lim_quantity_unit", label="Unit", choices=c("kg/m2", "kg/ha", "t/ha"))
                                                                              )

                                                                            ),
                                                                            textAreaInput("lim_description", value="", label = "Description")
                                                                    ))#,
                                                                     # column(width = 6,
                                                                     #
                                                                     #        fluidRow(
                                                                     #          column(width = 6,
                                                                     #                 textInput("lim_quantity", value = "", label="Quantity")
                                                                     #          ),
                                                                     #          column(width = 6,
                                                                     #            selectInput("lim_quantity_unit", label="Unit", choices=c("kg/m2", "kg/ha", "t/ha"))
                                                                     #          )
                                                                     #
                                                                     #        ),
                                                                     #        textAreaInput("lim_description", value="", label = "Description")
                                                                     #
                                                                     #
                                                                     # )
                                                                 ))


                                                          ))),
                                                        tabPanel("Mulching", value="tabMulching",
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Mulching"),
                                                                 #br(),
                                                                 fluidRow(
                                                                 box(title = "Mulch",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectInput("mulch_type", label = "Type", choices = c("Bark chips",
                                                                                                                                  "Compost",
                                                                                                                                  "Foil (Landscape fabric)",
                                                                                                                                  "Grass clippings",
                                                                                                                                  "Leaves",
                                                                                                                                  "Paper",
                                                                                                                                  "Pine needles (pine straw)",
                                                                                                                                  "Plastic",
                                                                                                                                  "Gravel",
                                                                                                                                  "Straw",
                                                                                                                                  "Saw dust",
                                                                                                                                  "Cacao husk",
                                                                                                                                  "Wood chips")
                                                                            ),
                                                                            textInput("mulch_thickness", value="", label = "Mulch thickness"),
                                                                            textInput("mulch_amountPerSq", value="", label = "Amount per sq. m"),
                                                                            selectInput("mulch_color", label = "Mulch color", choices = c("Black",
                                                                                                                                          "Brown",
                                                                                                                                          "Gray",
                                                                                                                                          "Transparent",
                                                                                                                                          "White",
                                                                                                                                          "Yellow")
                                                                            ),
                                                                            textInput("mulch_percCoverage", value="", label = "Percentage of coverage"),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("mulch_remove_start_date", label ="Mulch removal start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("mulch_remove_end_date", label ="Mulch removal end date", format = "dd/mm/yyyy")
                                                                              )
                                                                            )

                                                                     ),
                                                                     column(width = 6,
                                                                            br(),
                                                                      fluidRow(
                                                                       box(
                                                                         title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                         textInput("mulch_make", value="", label = "Make"),
                                                                         textInput("mulch_model", value="", label = "Model"),
                                                                                selectInput("mulch_animal_traction", label = "Animal traction", choices =
                                                                                              c("Buffalo",
                                                                                                "Camel",
                                                                                                "Donkey",
                                                                                                "Elephant",
                                                                                                "Horse",
                                                                                                "Mule",
                                                                                                "Ox / Bullock / Steer"
                                                                                              )
                                                                                ),
                                                                                textInput("mulch_humanPowered", value="", label = "Human powered"),
                                                                                selectInput("mulch_animal_traction", label = "Motorized traction", choices =
                                                                                              c("2 wheel tractor",
                                                                                                "4 wheel tractor"
                                                                                              )
                                                                                )
                                                                       ))
                                                                   ))


                                                                 )),
                                                                 fluidRow(
                                                                 box(title = "Residue management",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("residue_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("residure_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            )
                                                                     ),
                                                                     column(width = 6,
                                                                            br(),
                                                                            fluidRow(
                                                                       box(
                                                                         title = "Residue management", solidHeader = TRUE, status = "warning", width=12,
                                                                         #column(width = 6,
                                                                                selectInput("residue_cropType", label = "Crop type", choices =
                                                                                              c("Stubble",
                                                                                                "Stem/Leaf",
                                                                                                "Seed Pod - Cob - Fruit",
                                                                                                "Husk",
                                                                                                "Roots")
                                                                                ),
                                                                                selectInput("residue_technique", label = "Technique", choices =
                                                                                              c("Burning",
                                                                                                "No-tillage",
                                                                                                "Spreading",
                                                                                                "Tillage"
                                                                                              )
                                                                                ),
                                                                                textInput("residue_incorp_depth", value="", label="Residue incorporation depth"),
                                                                                selectInput("residue_aboveGroundMoisture", label = "Above ground residue moisture", choices =
                                                                                              c("Dry",
                                                                                                "Moist",
                                                                                                "Wet")
                                                                                ),
                                                                                fluidRow(
                                                                                    column(width = 6,
                                                                                      textInput("residue_aboveGroundAmount",  label = "Above ground residue amount", value="")
                                                                                    ),
                                                                                    column(width = 6,
                                                                                      selectInput("residue_aboveGroundAmount_unit", label = "Unit", choices =
                                                                                                    c("kg/m2",
                                                                                                      "kg/ha",
                                                                                                      "t/ha")
                                                                                      )
                                                                                    )
                                                                                )


                                                                         #)

                                                                       ))
                                                                     ))
                                                                 )) #end box residue
                                                        ))),#end tab mulching
                                                        tabPanel("Planting", value="tabPlanting",
                                                                 #br(),
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Planting"),
                                                                 fluidRow(
                                                                 box(title = "Planting method",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                       column(width = 6,
                                                                              fluidRow(
                                                                         column(width = 6,
                                                                                dateInput("planting_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                         ),
                                                                         column(width = 6,
                                                                                dateInput("planting_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                         ))
                                                                       )
                                                                     ),
                                                                     fluidRow(

                                                                         column(width = 6,
                                                                                fluidRow(
                                                                                box(
                                                                                  title = "Planting method", solidHeader = TRUE, status = "warning", width=12,

                                                                                      textInput("planting_directSeeding", value="", label = "Direct seeding"),
                                                                                      selectInput("planting_seedingTech", label = "Seeding technique", choices =
                                                                                                                                          c("Flat seed bed",
                                                                                                                                            "On hill",
                                                                                                                                            "On ridge")
                                                                                      ),
                                                                                      textInput("planting_ageSeeding", value="", label = "Age of seeding")
                                                                                ))
                                                                         ),
                                                                         column(width = 6,
                                                                                fluidRow(
                                                                                box(
                                                                                  title = "Transplanting techniques", solidHeader = TRUE, status = "warning", width=12,
                                                                                  selectInput("planting_manual", label = "Manual", choices =
                                                                                                c("Hand broadcasting",
                                                                                                  "Mechanical broadcasting",
                                                                                                  "Line planting by hand",
                                                                                                  "Jab planting",
                                                                                                  "Dibbling stick",
                                                                                                  "Drum seeding")
                                                                                  ),
                                                                                  selectInput("planting_animal_traction", label = "Animal traction", choices =
                                                                                                c("Buffalo",
                                                                                                  "Camel",
                                                                                                  "Donkey",
                                                                                                  "Elephant",
                                                                                                  "Horse",
                                                                                                  "Mule",
                                                                                                  "Ox / Bullock / Steer"
                                                                                                )
                                                                                  ),
                                                                                  selectInput("mulch_animal_traction", label = "Motorized traction", choices =
                                                                                                c("2 wheel tractor",
                                                                                                  "4 wheel tractor"
                                                                                                )
                                                                                  )
                                                                                ))
                                                                         )
                                                                     )


                                                                 )),
                                                                 fluidRow(
                                                                 box(title = "Planting arrangement (sowing)",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                        column(width = 6,
                                                                               fluidRow(
                                                                                column(width = 6,
                                                                                       dateInput("sowing_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                ),
                                                                                column(width = 6,
                                                                                       dateInput("sowing_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                                ))
                                                                        )
                                                                     ),
                                                                     fluidRow(
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                               box(
                                                                                 title = "Arrangement", solidHeader = TRUE, status = "warning", width=12,
                                                                                 textInput("planting_rowDistance", value="", label="Row distance"),
                                                                                 textInput("planting_seedingRate", value="", label="Seeding rate (plant density)"),
                                                                                 textInput("planting_seedPerhill", value="", label="Seed/seeding per hill"),
                                                                                 textInput("planting_distance", value="", label="Plantong distance")

                                                                               ))
                                                                         ),
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                                box(
                                                                                  title = "Planting distribution", solidHeader = TRUE, status = "warning", width=12,
                                                                                  selectInput("planting_animal_traction", label = "Type", choices =
                                                                                                c("Broadcast",
                                                                                                  "Row planting")
                                                                                  )


                                                                                ))
                                                                         )
                                                                     )
                                                                 )) #end box sowing
                                                        ))),#end tab planting
                                                        tabPanel("Harvest", value="tabHarvest",
                                                                 #br(),
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Harvest"),
                                                                  fluidRow(
                                                                 box(title = "Description Harvest",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectInput("crop_component_harvested", label = "Crop component harvested", choices =
                                                                                          c("Select one...",
                                                                                            "Canopy",
                                                                                            "Fruit",
                                                                                            "Leaves",
                                                                                            "Seed",
                                                                                            "Tuber")
                                                                            )
                                                                     ),
                                                                     column(width = 6,
                                                                            br(),
                                                                            fluidRow(
                                                                            box(
                                                                              title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                              selectInput("harvest_implement", label = "Harvest implement", choices =
                                                                                                                                 c("Select one...",
                                                                                                                                   "Baler",
                                                                                                                                   "Chopper",
                                                                                                                                   "Cotton stalk puller",
                                                                                                                                   "Grass slasher",
                                                                                                                                   "Manual fruit harvester",
                                                                                                                                   "Mower",
                                                                                                                                   "Sickle",
                                                                                                                                   "Simple treadle thresher",
                                                                                                                                   "Threshing rack",
                                                                                                                                   "Digger",
                                                                                                                                   "Reaper")
                                                                              ),
                                                                              textInput("implement_make", value="", label = "Implement make"),
                                                                              textInput("implement_model", value="", label = "Implement model"),
                                                                              selectInput("animal_traction", label = "Animal Traction", choices =
                                                                                            c("Select one...",
                                                                                              "Buffalo",
                                                                                              "Camel",
                                                                                              "Donkey",
                                                                                              "Elephant",
                                                                                              "Horse",
                                                                                              "Mule",
                                                                                              "Ox / Bullock / Steer"
                                                                                            )
                                                                              ),
                                                                              textInput("humanPowered", value="", label = "Human powered"),
                                                                              selectInput("motorized_traction", label = "Motorized Traction", choices =
                                                                                            c("Select one...",
                                                                                              "2 wheel tractor",
                                                                                              "4 wheel tractor"
                                                                                            )
                                                                              )
                                                                            ))
                                                                     ))
                                                                 ))#end box description harvest
                                                        ))),#end tab harvest
                                                        tabPanel("Irrigation event", value="tabIrrigation",
                                                                 #br(),
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Irrigation event"),
                                                                 fluidRow(
                                                                 box(title = "Description Irrigation",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("irrigationevent_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("irrigationevent_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectInput("irrigation_system_type", label = "Irrigation system type", choices =
                                                                                          c("Select one...",
                                                                                            "Automatic irrigation",
                                                                                            "Manual irrigation")
                                                                            ),
                                                                            selectInput("irrigation_technique", label = "Irrigation technique", choices =
                                                                                          c("Select one...",
                                                                                            "Surface irrigation technique",
                                                                                            "Localized irrigation technique",
                                                                                            "Irrigation using sprinkler systems",
                                                                                            "Sub-irrigation")
                                                                            ),
                                                                            selectInput("surface_irrigation_technique", label = "Surface irrigation technique", choices =
                                                                                          c("Select one...",
                                                                                            "Furrow irrigation",
                                                                                            "Uncontrolled flooding",
                                                                                            "Basin irrigation",
                                                                                            "Border irrigation",
                                                                                            "Continuous flood")
                                                                            ),
                                                                            selectInput("localized_irrigation_technique", label = "Localized irrigation technique", choices =
                                                                                          c("Select one...",
                                                                                            "Drip irrigation",
                                                                                            "Subsurface textile irrigation",
                                                                                            "Mist irrigation",
                                                                                            "Subsurface drip irrigation",
                                                                                            "Bubbler irrigation",
                                                                                            "Pitcher irrigation")
                                                                            ),
                                                                            selectInput("irrigation_using_sprinkler_systems", label = "Irrigation using sprinkler systems", choices =
                                                                                          c("Select one...",
                                                                                            "Center pivot irrigation",
                                                                                            "Irrigation by lateral move",
                                                                                            "Irrigation by side move")
                                                                            ),
                                                                            fileInput("myFile", "Irrigation system picture", accept = c('image/png', 'image/jpeg')),
                                                                            textInput("water_source", value="", label = "Water source"),
                                                                            textInput("water_source_distance", value="", label = "Water source distance"),
                                                                            textInput("bund_height", value="", label = "Bund height"),
                                                                            textInput("percolation_rate", value="", label = "Percolation rate"),
                                                                            textInput("irrigation_equipment_depth  ", value="", label = "Irrigation equipment depth"),
                                                                            textInput("well_depth", value="", label = "Well depth"),
                                                                            textInput("area_covered_irrigation_system", value="", label = "Area covered by the irrigation system")
                                                                     ))
                                                                 ))#end box description irrigation
                                                        ))),#end tab irrigation
                                                        tabPanel("Biofertilizer", value="tabBiofertilizer",
                                                                 #br(),
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Biofertilizer"),
                                                                          fluidRow(
                                                                 box(title = "Description Biofertilizer",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectInput("rhizobium_inoculum_strain", label = "Rhizobium inoculum strain", choices =
                                                                                          c("Select one...",
                                                                                            "Rhizobium japonicum",
                                                                                            "Rhizobium leguminosarum",
                                                                                            "Rhizobium loti",
                                                                                            "Rhizobium meliloti",
                                                                                            "Rhizobium spp.",
                                                                                            "Rhizobium trifolii",
                                                                                            "Other")
                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     textInput("quantity_inoculated", value = "", label="Quantity inoculated")
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectInput("quantity_inoculated_unit", label="Unit", choices=c("kg/m2", "kg/ha", "t/ha"))
                                                                              )

                                                                            ),
                                                                            selectInput("inoculation_method", label = "Inoculation method", choices =
                                                                                          c("Select one...",
                                                                                            "Seed coating (Seed application of inoculum)",
                                                                                            "Directly to the soil",
                                                                                            "Other")
                                                                            ),
                                                                            selectInput("product_formulation", label = "Product formulation", choices =
                                                                                          c("Select one...",
                                                                                            "Soil application with granules/pellets",
                                                                                            "Soil application with slurry of liquid culture")
                                                                            ),
                                                                            textInput("days_sowing_after_rhizobium_inocculation", value="", label = "Days to sowing after Rhizobium inocculation")
                                                                     ))
                                                                 ))#end box description biofertilizer
                                                        ))),#end tab biofertilizer
                                                        tabPanel("Pest & Disease", value="tabPest&Disease",
                                                                 #br(),
                                                                 fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Pest & Disease"),
                                                                          fluidRow(
                                                                 box(title = "Disease observation",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            # fluidRow(
                                                                            #   column(width = 6,
                                                                            #          dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                            #   ),
                                                                            #   column(width = 6,
                                                                            #          dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                            #   )
                                                                            # ),
                                                                            textInput("disease_observation_date", value="", label = "Disease observation date"),
                                                                            textInput("disease_name", value="", label = "Disease name"),
                                                                            textInput("plant_parts_affected", value="", label = "Plant parts affected"),
                                                                            textInput("percentage_experiement_affected", value="", label = "Percentage of the experiement affected"),
                                                                            textInput("disease_damages_notes", value="", label = "Disease damages, notes"),
                                                                            textInput("disease_notes", value="", label = "Disease, notes")
                                                                     ))
                                                                 )),#end box disease
                                                                 fluidRow(
                                                                 box(title = "Pest observation",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            # fluidRow(
                                                                            #   column(width = 6,
                                                                            #          dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                            #   ),
                                                                            #   column(width = 6,
                                                                            #          dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                            #   )
                                                                            # ),
                                                                            selectInput("pest_type", label = "Pest type", choices =
                                                                                          c("Select one...",
                                                                                            "Bacteria",
                                                                                            "Bird",
                                                                                            "Fungi",
                                                                                            "Gastropod",
                                                                                            "Insect",
                                                                                            "Mammal",
                                                                                            "Nematode",
                                                                                            "Rodent",
                                                                                            "Virus",
                                                                                            "Weeds")
                                                                            ),
                                                                            textInput("pest_name", value="", label = "Pest name"),
                                                                            textInput("pest_damage_notes", value="", label = "Pest damage, notes"),
                                                                            textInput("pest_notes", value="", label = "Pest, notes")
                                                                     ))
                                                                 )),#end box pest observation
                                                                 fluidRow(
                                                                 box(title = "Pest control",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectInput("pest_control_technique", label = "Pest control technique", choices =
                                                                                          c("Select one...",
                                                                                            "Biological pest control",
                                                                                            "Chemical pest control",
                                                                                            "Mechanical pest control")
                                                                            ),
                                                                            textInput("pesticide_application_depth", value="", label = "Pesticide application depth"),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     textInput("pesticide_amount", value = "", label="Pesticide amount")
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectInput("pesticide_amount_unit", label="Unit", choices=c("kg/m2", "kg/ha", "t/ha"))
                                                                              )
                                                                            ),
                                                                            fileInput("myFile", "Pesticide box or bottle picture", accept = c('image/png', 'image/jpeg')),
                                                                            textInput("pest_control_applications_", value="", label = "Pest control applications total number"),
                                                                            textInput("pest_control_details", value="", label = "Pest control details (e.g. name of parasitoid etc), treatment evaluation"),
                                                                            selectInput("chemical_pest_control_equipment", label = "Chemical pest control equipment", choices =
                                                                                          c("Select one...",
                                                                                            "Aerial applicator",
                                                                                            "Airblast sprayer",
                                                                                            "Backpack sprayer",
                                                                                            "Boom sprayer",
                                                                                            "Duster",
                                                                                            "Electrostatic sprayer",
                                                                                            "Fogger",
                                                                                            "Hand sprayer",
                                                                                            "Injection sprayer",
                                                                                            "Mist blower",
                                                                                            "Recirculating sprayer",
                                                                                            "Seed treater",
                                                                                            "Tree injector",
                                                                                            "Wiper")
                                                                            )
                                                                     ),
                                                                     column(width = 6,
                                                                            br(),
                                                                            fluidRow(
                                                                            box(
                                                                              title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                              textInput("implement_make", value="", label = "Implement make"),
                                                                              textInput("implement_model", value="", label = "Implement model"),
                                                                              selectInput("animal_traction", label = "Animal Traction", choices =
                                                                                            c("Select one...",
                                                                                              "Buffalo",
                                                                                              "Camel",
                                                                                              "Donkey",
                                                                                              "Elephant",
                                                                                              "Horse",
                                                                                              "Mule",
                                                                                              "Ox / Bullock / Steer"
                                                                                            )
                                                                              ),
                                                                              textInput("humanPowered", value="", label = "Human powered"),
                                                                              selectInput("motorized_traction", label = "Motorized Traction", choices =
                                                                                            c("Select one...",
                                                                                              "2 wheel tractor",
                                                                                              "4 wheel tractor"
                                                                                            )
                                                                              )
                                                                            ))
                                                                     ))
                                                                 ))#end box pest control
                                                        )#end tab pest&disease
                                                      )),
                                                      tabPanel("Nutrient management event", value="tabNutrient",
                                                               fluidRow(
                                                                 column(width = 12,
                                                                        #br(),
                                                                        h2("Nutrient management event"),
                                                                        fluidRow(
                                                                          tabBox(height = NULL, width = 12,
                                                                                 tabPanel("Fertilization details", value="tab111",
                                                                                          fluidRow(
                                                                                            column(width = 12,
                                                                                                   #br(),
                                                                                                   fluidRow(
                                                                                                     box(title = "Fertilization details",
                                                                                                         status = "primary",
                                                                                                         solidHeader = TRUE,
                                                                                                         width = 12, collapsible = TRUE,
                                                                                                         column(width = 6,
                                                                                                            selectInput(inputId = "napplications", label = "Number of applications", choices = 1:5)
                                                                                                         ),

                                                                                                         box(
                                                                                                           title = tagList(shiny::icon("edit"), "Application #1"), solidHeader = TRUE, status = "warning", width=12,

                                                                                                           fluidRow(
                                                                                                             column(width = 6,
                                                                                                                    fluidRow(
                                                                                                                      column(width = 6,
                                                                                                                             dateInput("fertilization_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                                                      ),
                                                                                                                      column(width = 6,
                                                                                                                             dateInput("fertilization_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                                                                      )
                                                                                                                    )
                                                                                                             )
                                                                                                           ),

                                                                                                           fluidRow(
                                                                                                             column(width=6,

                                                                                                                    selectizeInput("app1fTypeFertilizer", "Type of fertilizer", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select type..."),
                                                                                                                                   choices=c("Inorganic", "Organic")
                                                                                                                    )
                                                                                                             ),
                                                                                                             column(width=6,
                                                                                                                    textInput(inputId = "person2Afiliation", label = "Person afiliation", value = ""),
                                                                                                                    textInput(inputId = "person2ORCID", label = "Person ORCID", value = "")
                                                                                                             )
                                                                                                           )
                                                                                                         ),



                                                                                                         conditionalPanel("input.napplications == 2  |
                                                                                                                           input.napplications == 3  |
                                                                                                                           input.napplications == 4 |
                                                                                                                           input.napplications == 5",
                                                                                                                          box(
                                                                                                                            title = tagList(shiny::icon("user"), "Personnel #2"), solidHeader = TRUE, status = "warning", width=12,

                                                                                                                            fluidRow(
                                                                                                                              column(width = 6, shiny::selectInput("personnel2Type", "Personnel type", choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                                                                              column(width = 6, textInput(inputId = "person2Email", label = "Person email", value = ""))
                                                                                                                            ),

                                                                                                                            fluidRow(
                                                                                                                              column(width=6,

                                                                                                                                     textInput(inputId = "person2FirstName", label = "Person first name", value = ""),
                                                                                                                                     textInput(inputId = "person2LastName", label = "Person last name", value = "")
                                                                                                                                     # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                                                                              ),
                                                                                                                              column(width=6,
                                                                                                                                     textInput(inputId = "person2Afiliation", label = "Person afiliation", value = ""),
                                                                                                                                     textInput(inputId = "person2ORCID", label = "Person ORCID", value = "")
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )

                                                                                                         )

                                                                                                     )#end box
                                                                                                  )
                                                                                            )
                                                                                          )

                                                                                 ),
                                                                                 tabPanel("Nutrient amount applied", value="tab222",
                                                                                          fluidRow(
                                                                                            column(width = 12,
                                                                                                   #br(),
                                                                                                   fluidRow(
                                                                                                     box(title = "Nutrient amount applied",
                                                                                                         status = "primary",
                                                                                                         solidHeader = TRUE,
                                                                                                         width = 12, collapsible = TRUE,
                                                                                                         fluidRow(
                                                                                                           column(width = 6,
                                                                                                                  fluidRow(
                                                                                                                    column(width = 6,
                                                                                                                           dateInput("landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                                                    ),
                                                                                                                    column(width = 6,
                                                                                                                           dateInput("landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                                                                    )
                                                                                                                  )
                                                                                                           ))
                                                                                                     ))#end box
                                                                                            )
                                                                                          )
                                                                                 )
                                                                          )
                                                                        )
                                                                 )))#end tab nutrient management event
                                                      ))
                                                    )#asd

                                                    ),
                                                    fluidRow(
                                                      sidebarPanel(id="sidebar", width = 12,
                                                                   actionButton("btnNextAgro", "Next", class = "btn-primary",style="color: #fff;")
                                                      )
                                                    )



                                             ),

                                              shiny::tabPanel("Traits", value = "tabTraits", icon = shiny::icon("leaf"),
                                                  h2("Traits"),
                                                  fluidRow(
                                                  #br(),
                                                    column(width = 12,
                                                  shinyTree::shinyTree("designFieldbook_traits_agrofims",search = TRUE,checkbox = TRUE)
                                                  )),

                                                  # fluidRow(
                                                  #   column(width = 6, align = "left",
                                                  #          br(),
                                                  #          actionButton(inputId = "btnNextEnv", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                  # )
                                                  fluidRow(
                                                    sidebarPanel(id="sidebar", width = 12,
                                                                 actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;")
                                                    )
                                                  )
                                              ),
                                              shiny::tabPanel("Weather & Soil", value = 'tabEnvironment', icon = shiny::icon("bolt"),
                                                  #br(),
                                                  h2("Weather & Soil"),
                                                  fluidRow(
                                                    column(width = 12,

                                                  shinyTree::shinyTree("designFieldbook_weatherVar_agrofims",search = TRUE,checkbox = TRUE),
                                                  shinyTree::shinyTree("designFieldbook_soilVar_agrofims",search = TRUE,checkbox = TRUE)
                                                    )),
                                                  #shiny::checkboxInput("fbDesign_weather_cb", label = "Register weather data"),
                                                  #shiny::checkboxInput("fbDesign_soil_cb", label = "Register soil data")
                                                  fluidRow(
                                                    sidebarPanel(id="sidebar", width = 12,
                                                                 actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;")
                                                    )
                                                  )
                                              )#,
                                         #) #end master

                                      ))# end tabbox,
                                    ) #end box
                                  ), #end fluid row


                                     fluidRow(
                                       HTML('<div style="float: right; margin: 0 15px 18px 0px;">'),
                                       #shiny::actionButton(inputId = "refresh", label = "Refresh", icon = icon("fa fa-refresh")),
                                       #shinyBS::bsButton( "fbDesign_draft", "BookView" ),
                                       shiny::actionButton("fbDesign_draft_agrofims", "Book Preview", icon("table"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                       downloadButton("downloadData", "Download", class = "color: #fff; background-color: #51a351; border-color: #51a351"),
                                       #shinysky::actionButton2("fbDesign_create_agrofims", label = "Download", icon ="file-excel-o", icon.library = "bootstrap", styleclass= "color: #fff; background-color: #51a351; border-color: #51a351"),
                                       #shiny::actionButton("fbDesign_create", "Download", icon("file-excel-o"), style="color: #fff; background-color: #51a351; border-color: #51a351"),
                                       #shinyBS::bsAlert("alert_fb_done"),
                                       shinysky::shinyalert("alert_fb_done", FALSE, auto.close.after = 4),
                                       HTML('</div>')
                                     ),
                          rHandsontableOutput("hot", width = 1000),
                                     shiny::fluidRow(
                                         #box(
                                         #column(width = 12,height=6,
                                         # shinydashboard::box(title = "Fieldbook Preview",
                                         #                     status = "primary",
                                         #                     #height = 500,
                                         #                     #width = NULL,
                                         #                     solidHeader = TRUE,
                                         #                     width = 12, collapsible = TRUE,
                                         #                     #shiny::actionButton("butNewFieldbook", "New fieldbook", inline = TRUE)#,
                                         #                     rhandsontable::rHandsontableOutput("fbDesign_table_agrofims", height = 400)
                                         # ),





                                         br(),
                                         br(),
                                         br()
                                     ),
                                    br(),
                                    br(),
                                    br()


                           # ) #fin primer tab

                                       # ) #end

                            ######
                            ##################  End Simple Modules

                                       # )# End of Master tabSetPanel
                                     )
  }



