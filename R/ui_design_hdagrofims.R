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

ui_fieldbook_agrofims <- function(type="tab",title="Design Fieldbook",name="phenotype_fieldbook_design" ){

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



                              .box.box-solid.box-info>.box-header {
                                color:#000;
                                background:#f2dede;
                                /*padding-top:0px*/
                              }

                              .box.box-solid.box-info>.box-body {
                                color:#000;
                              background:#f2dede
                              }

                              .box.box-solid.box-info{
                              border-bottom-color:#f2dede;
                              border-left-color:#f2dede;
                              border-right-color:#f2dede;
                              border-top-color:#f2dede;
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

                          tags$style(HTML("
                            .nav-tabs>li.active>a, .nav-tabs>li.active>a:focus, .nav-tabs>li.active>a:hover {
                            color: #fff;
                            cursor: default;
                            background-color: #0081c2;
                            border: 1px solid #ddd;
                            border-bottom-color: transparent;}
                          ")),

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
                                         #fluidRow(
                                         tabsetPanel(id= "fbDesignNav",
                                         #shinydashboard::tabBox(id = "fbDesignNav", height = NULL, width = 12,
                                         # box(title = "", solidHeader = TRUE, status = "primary", width=12,
                                            #tabsetPanel(id = "inExpInfo", #Begin Master tabSetPanel

                                              shiny::tabPanel("Experiment", value = "experiment", icon = shiny::icon("info"),
                                                  #fluidRow(
                                                    column(width = 6,
                                                           h2("Experiment details"),
                                                           disabled(textInput(inputId = "experimentId", label = "Experiment ID", value = "")),
                                                           textInput(inputId = "experimentName", label = "Experiment name", value = ""),
                                                           textInput(inputId = "experimentProjectName", label = "Experiment project name", value = ""),

                                                           shiny::dateRangeInput("fbDesign_project_time_line", "Experiment date", start = Sys.Date() - 2,
                                                                                 end = Sys.Date() + 20, startview = "year",format = "dd/mm/yyyy"),
                                                           # dateInput("Embargo_date", label ="Embargo end date", format = "dd/mm/yyyy"),
                                                           # shiny::selectInput("designFieldbook_expSeason", "Experiment season", choices = c("Summer","Spring","Winter", "Autumn")),
                                                           # shiny::selectInput("designFieldbook_typeExperiment", "Type of experiment", choices = c("Select one...","Controlled treatment trial","Observation trial","Varietal trial", "Demonstration trial", "Germplasm screening trial")),
                                                           selectizeInput("designFieldbook_typeExperiment", "Type of experiment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Controlled treatment trial","Observation trial","Varietal trial", "Demonstration trial", "Germplasm screening trial")),





                                                            textAreaInput(inputId = "experimentObj", label = "Experiment objective", value = ""),





                                                           # hr(),
                                                           br(),
                                                           h2("Funding agency associated with experiment"),

                                                           selectizeInput("designFieldbook_fundAgencyType", "Funding agency type",multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Government	National", "Agricultural Extension", "Advisory Services", "International NGO", 	"National NGO",	"Farmers Organization",	"Regional Organization",	"International Organization",	"Financing Institution",	"Foundation",	"Private Company",	"Academic Institution",	"National Research Institution",	"International Research Center")),
                                                           textInput(inputId = "fundName", label = "Funding agency name", value = ""),
                                                           selectizeInput("contCenter", "Contribuitor Center", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                             "Africa Rice Center",	"Biodiversity International",	"Center for International Forestry Research (CIFOR)",	"International Center for Agricultural Research (ICARDA)",	"International Center for Tropical Agriculture (CIAT)",	"International Crops Research Institute for the Semi-Arid (ICRISAT)",	"International Food Policy Research Institute (IFPRI)",	"International Institute of Tropical Agriculture (IITA)",	"International Livestock Research Institure (ILRI)",	"International Maize and Wheat Improvement Center (CIMMYT)",	"International Potato Center (CIP)",	"International Rice Research Institute (IRRI)",	"International Water Management Institute (IWMI)",	"World Agroforestry Centre (ICRAF)",	"WorldFish",	"Other"

                                                           )),
                                                           selectizeInput("contCRP", "Contribuitor CRP", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
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
                                                                          "CGIAR Research Program on Water, Land and Ecosystems")
                                                            ),


                                                           textInput(inputId = "contResearcher", label = "Contribuitor researcher", value = ""),

                                                           br(),
                                                           h2("Institution/Entity associated with experiment"),

                                                           selectizeInput("designFieldbook_fundLeadAgency", "Experiment, lead organization type",multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),  choices = c("University","University, main campus","Agricultural experimental extension", "Government research institution (NARS)","Government research institution, designated laboratory or center", "Private company", "Farm", "Farmer association or cooperative", "Non-governmental organization", "Extension organization", "CGIAR center", "Other" )),
                                                           # shiny::selectInput("designFieldbook_fundHostAgency", "Experiment, host organization type", choices = c("University","University, main campus","Agricultural experimental extension", "Government research institution (NARS)")),
                                                           textInput(inputId = "leadName", label = "Experiment, lead organization name", value = "")


                                                    ),
                                                  #),
                                                  #fluidRow(
                                                    sidebarPanel(id="sidebar", width = 12,
                                                                 actionButton("btnNextPersonnelInfo", "Next", class = "btn-primary",style="color: #fff;")
                                                    )
                                                  #)
                                              ),

                                              tabPanel("Personnel", value = "tabPersonnel", icon = shiny::icon("user"),
                                                # column( width = 12,
                                                  #fluidRow(
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
                                                                column(width = 6, selectizeInput("personnel1Type", "Person type", multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                column(width = 6, textInput(inputId = "person1Email", label = "Person email", value = ""))
                                                              ),
                                                             fluidRow(
                                                               column(width=6,

                                                                      textInput(inputId = "person1FirstName", label = "Person, first name", value = ""),
                                                                      textInput(inputId = "person1LastName", label = "Person, last name", value = "")
                                                                      # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                               ),
                                                               column(width=6,

                                                                      textInput(inputId = "person1Afiliation", label = "Person, affiliation", value = ""),
                                                                      textInput(inputId = "person1ORCID", label = "Person, ORCID", value = "")
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
                                                                  column(width = 6, selectizeInput("personnel2Type", "Person type",multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                  column(width = 6, textInput(inputId = "person2Email", label = "Person email", value = ""))
                                                                ),

                                                          fluidRow(
                                                              column(width=6,

                                                                     textInput(inputId = "person2FirstName", label = "Person, first name", value = ""),
                                                                     textInput(inputId = "person2LastName", label = "Person, last name", value = "")
                                                                     # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                              ),
                                                              column(width=6,
                                                                     textInput(inputId = "person2Afiliation", label = "Person, affiliation", value = ""),
                                                                     textInput(inputId = "person2ORCID", label = "Person, ORCID", value = "")
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
                                                                               column(width = 6, selectizeInput("personnel3Type", "Person type",multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                               column(width = 6, textInput(inputId = "person3Email", label = "Person email", value = ""))
                                                                             ),
                                                                         fluidRow(
                                                                            column(width=6,
                                                                                   textInput(inputId = "person3FirstName", label = "Person, first name", value = ""),
                                                                                   textInput(inputId = "person3LastName", label = "Person, last name", value = "")
                                                                                   # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                            ),
                                                                            column(width=6,
                                                                                   textInput(inputId = "person3Afiliation", label = "Person, affiliation", value = ""),
                                                                                   textInput(inputId = "person3ORCID", label = "Person, ORCID", value = "")
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
                                                                             column(width = 6, selectizeInput("personnel4Type", "Person type",multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                             column(width = 6, textInput(inputId = "person4Email", label = "Person email", value = ""))
                                                                           ),
                                                                            fluidRow(
                                                                              column(width=6,
                                                                                     textInput(inputId = "person4FirstName", label = "Person, first name", value = ""),
                                                                                     textInput(inputId = "person4LastName", label = "Person, last name", value = "")
                                                                                     # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                              ),
                                                                              column(width=6,
                                                                                     textInput(inputId = "person4Afiliation", label = "Person, affiliation", value = ""),
                                                                                     textInput(inputId = "person4ORCID", label = "Person, ORCID", value = "")
                                                                              )
                                                                            )
                                                                     )#)

                                                    ),
                                                    conditionalPanel("input.npersons == 5",

                                                                     # fluidRow(
                                                                       box(
                                                                         title = tagList(shiny::icon("user"), "Personnel #5"), solidHeader = TRUE, status = "warning", width=12,

                                                                         fluidRow(
                                                                           column(width = 6, selectizeInput("personnel5Type", "Person type",multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other"))),
                                                                           column(width = 6, textInput(inputId = "person5Email", label = "Person email", value = ""))
                                                                         ),

                                                                         fluidRow(
                                                                           column(width=6,

                                                                                  textInput(inputId = "person5FirstName", label = "Person, first name", value = ""),
                                                                                  textInput(inputId = "person5LastName", label = "Person, last name", value = "")
                                                                                  # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                           ),
                                                                           column(width=6,
                                                                                  textInput(inputId = "person5Afiliation", label = "Person, affiliation", value = ""),
                                                                                  textInput(inputId = "person5ORCID", label = "Person, ORCID", value = "")
                                                                           )
                                                                         )
                                                                       )
                                                                     #)

                                                    ),


                                                  #),
                                                # fluidRow(
                                                #   column(width = 6, align = "left",
                                                #          br(),
                                                #          actionButton(inputId = "btnNextCropInfo", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                # )
                                                #fluidRow(
                                                  sidebarPanel(id="sidebar", width = 12,
                                                               actionButton("btnNextSite", "Next", class = "btn-primary",style="color: #fff;")
                                                  )
                                                #)
                                                # )
                                              ),
#
                                              tabPanel("Site", value="tabSite",  icon = shiny::icon("location-arrow"),
                                                       #fluidRow(
                                                         column(width = 6,
                                                                h2("Site information"),

                                                                shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),
                                                                shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500), #,#locality
                                                                br(),
                                                                h2("Site surrounding description"),
                                                                selectizeInput("fbDesign_inHighLevel", label="Higher-level landform", multiple = TRUE,
                                                                               choices = c("Plain",	"Basin",	"Valley",	"Plateau","Upland",	"Hill",	"Mountain"),
                                                                               options = list(maxItems = 1, placeholder = 'Select  one...')),
                                                                selectizeInput("fbDesign_inSiteVegetation", label="Vegetation surrounding the experiment site", multiple = TRUE,
                                                                               choices = c("Grasslan", "Crops", "Forest", "Woodland", "Shrub land", "Savanna", "Other"),
                                                                               options = list(maxItems = 5, placeholder = 'Select one... ')),

                                                                textAreaInput("inSiteDescNotes", label="Site description notes", value="")),
                                                       sidebarPanel(id="sidebar", width = 12,
                                                                    actionButton("btnNextCropInfo", "Next", class = "btn-primary",style="color: #fff;")
                                                       )
                                                       #)


                                              ),

                                              shiny::tabPanel("Crop", value = "tabCropInfo", icon = shiny::icon("pagelines"),
                                                  #fluidRow(
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
                                                                                             selectizeInput("cropCommonNameMono", "Crop common name", multiple = TRUE, options = list(maxItems =1, placeholder="Select one..."), choices = c("Potato", "Cassava", "Wheat", "Maize","Sweetpotato", "Soybean")),
                                                                                             textInput(inputId = "cropLatinNameMono", label = "Crop latin name", value = ""),
                                                                                             fluidRow(
                                                                                               column(width = 10,
                                                                                                      #textInput(inputId = "cropVarietyNameMono", label = "Crop variety name", value = "")
                                                                                                      selectizeInput("cropVarietyNameMono", "Crop variety name", c(), multiple = TRUE, options = list(
                                                                                                        placeholder = "ex.  variety1   variety2    ",
                                                                                                        'create' = TRUE,
                                                                                                        'persist' = FALSE)
                                                                                                      )#,


                                                                                               ),

                                                                                               column(width = 2, br(),
                                                                                                      checkboxInput("setCropFactor", "Factor", FALSE)
                                                                                               )
                                                                                             )

                                                                                      ),
                                                                                      column(width = 6,
                                                                                             textInput(inputId = "cultivarNameMono", label = "Cultivar name", value = ""),
                                                                                             textInput(inputId = "monoCropLocalName", label = "Crop local name", value = "")
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
                                                                                               textInput(inputId = "cultivarName", label = "Cultivar name", value = "")
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
                                                                                             textInput(inputId = "cultivarName2", label = "Cultivar name", value = "")
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
                                                                                             textInput(inputId = "cultivarName3", label = "Cultivar name", value = "")
                                                                                      )
                                                                                  )
                                                                 )
                                                          )



                                                          # fluidRow(
                                                          #   column(width = 6, align = "left",
                                                          #          br(),
                                                          #          actionButton(inputId = "btnDesign", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                          # )
                                                          #fluidRow(
                                                            # sidebarPanel(id="sidebar", width = 12,
                                                            #              actionButton("btnDesign", "Next", class = "btn-primary",style="color: #fff;")
                                                            # )
                                                          #)
                                                    ),
                                                    sidebarPanel(id="sidebar", width = 12,
                                                                   actionButton("btnDesign", "Next", class = "btn-primary",style="color: #fff;")
                                                    )
                                                  #)
                                              ),

                                              #BEGIN (STATISTICAL DESGIGN)
                                              shiny::tabPanel("Design", value = "tabDesign", icon = shiny::icon("th-list"),
                                                    # br(),
                                                    # br(),
                                                    #h2("Design information"),

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

                                                    #fluidRow(

                                                      column(width = 6,
                                                             h2("Design information"),
                                                             #Select statistical design
                                                             shiny::selectInput("designFieldbook_agrofims", "Select design/arrangement",  c("Choose one" = "", design_choices_agrofims), selected = 'CRD',
                                                                                multiple = FALSE),

                                                             shiny::selectInput("designFieldbook_agrofims_r", "Replications", 2:100, 2 ),



                                                             #select number of factors
                                                             selectInput(inputId = "nfactors_hdafims", label = "Number of factor", choices = 2:5)
                                                      ),
                                                    #),

                                                    column(width = 12,
                                                           fluidRow(
                                                    # desing of experiment with 1 factors
                                                    conditionalPanel("input.nfactors_hdafims == 1  |
                                                                     input.nfactors_hdafims == 2  |
                                                                     input.nfactors_hdafims == 3  |
                                                                     input.nfactors_hdafims == 4  |
                                                                     input.nfactors_hdafims == 5",


                                                                     #fluidRow(
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
                                                                     #) #end fluidrow
                                                                     #)
                                                    ),
                                                    # desing of experiment with 2 factors
                                                    conditionalPanel("input.nfactors_hdafims == 2  |
                                                                     input.nfactors_hdafims == 3  |
                                                                     input.nfactors_hdafims == 4  |
                                                                     input.nfactors_hdafims == 5 ",

                                                                     #div(style="display:inline-block",
                                                                     #fluidRow(
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
                                                                     #) #end fluidrow
                                                                     #)
                                                    ),

                                                    # desing of experiment with 3 factors
                                                    conditionalPanel("input.nfactors_hdafims == 3 |
                                                                     input.nfactors_hdafims == 4 |
                                                                     input.nfactors_hdafims == 5 ",

                                                                     #div(style="display:inline-block",
                                                                     #fluidRow(
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
                                                                     #) #end fluidrow

                                                    ),
                                                     # desing of experiment with 4 factors
                                                    conditionalPanel("input.nfactors_hdafims == 4 |
                                                                     input.nfactors_hdafims == 5 ",

                                                                     #div(style="display:inline-block",
                                                                     #fluidRow(
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
                                                                     #) #end fluidrow
                                                    ),
                                                    # desing of experiment with 5 factors
                                                    conditionalPanel("input.nfactors_hdafims == 5",

                                                                     #fluidRow(
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
                                                                     #) #end fluidrow


                                                    )))#,
                                                    ,
                                                    # fluidRow(
                                                    #   column(width = 6, align = "left",
                                                    #          br(),
                                                    #          actionButton(inputId = "btnNextPlotInfo", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                    # )
                                                    #fluidRow(
                                                      sidebarPanel(id="sidebar", width = 12,
                                                                   actionButton("btnNextPlotInfo", "Next", class = "btn-primary",style="color: #fff;")
                                                      )
                                                    #)




                                                    #downloadLink("downloadData", "Download")



                                             ), #END DESIGN (STATISTICAL DESIGN)

                                             shiny::tabPanel("Plot", value = "tabPlotInfo", icon = shiny::icon("th"),
                                                conditionalPanel("input.cropCommonNameMono == null",
                                                                 column(width = 6,
                                                                 h2("Select a crop to show form")
                                                                 )),
                                                conditionalPanel("input.cropCommonNameMono != null && (
                                                                input.cropCommonNameMono == 'Potato' |
                                                                 input.cropCommonNameMono == 'Sweetpotato' |
                                                                 input.cropCommonNameMono == 'Cassava')",
                                                      #fluidRow(
                                                      column(width = 6,
                                                        h2("Plot information"),
                                                        textInput(inputId = "numPlantsPerPlot", label = "Number of plants planted per plot", value = ""),
                                                        textInput(inputId = "numRowsPerPlot", label = "Number of rows per plot", value = ""),
                                                        textInput(inputId = "numPlantsPerRow", label = "Number of plants per row", value = ""),
                                                        textInput(inputId = "plotSize", label = "Plot size (m2)", value = ""),
                                                        textInput(inputId = "distancebwPlants", label = "Distance between plants (m)", value = ""),
                                                        textInput(inputId = "distanceBwRows", label = "Distance between rows", value = ""),
                                                        textInput(inputId = "planDensity", label = "Planting density (plants/Ha)", value = "")
                                                      )#)
                                                ),
                                                conditionalPanel("input.cropCommonNameMono != null && (
                                                                input.cropCommonNameMono == 'Wheat' |
                                                                 input.cropCommonNameMono == 'Maize' |
                                                                 input.cropCommonNameMono == 'Soybean')",
                                                                 #fluidRow(
                                                                 column(width = 6,
                                                                        h2("Plot information"),
                                                                        textInput(inputId = "plotSpacing", label = "Plot spacing", value = ""),
                                                                        textInput(inputId = "rowSpacing", label = "Row spacing", value = ""),
                                                                        textInput(inputId = "rowOrientation", label = "Row orientation", value = ""),
                                                                        textInput(inputId = "spaceBwPlantsRow", label = "Space between plants in row", value = ""),
                                                                        textInput(inputId = "hillSpacing", label = "Hill spacing", value = ""),
                                                                        textInput(inputId = "numsMsPlantPerPlot", label = "Number of measured plants per plot", value = ""),
                                                                        br(),
                                                                        h2("Field information"),
                                                                        textInput("fieldArea", "Field area", value =""),
                                                                        textInput("expFieldMaxWidth", "Experimental field maximum width", value=""),
                                                                        textInput("expFieldMaxLength", "Experimental field maximum length", value="")

                                                                 )#)
                                                ),
                                                 # h2("Plot information"),
                                                 # fluidRow(
                                                 #   column(width = 6,
                                                 #          textInput(inputId = "plotSpacing", label = "Plot spacing", value = ""),
                                                 #
                                                 #          textInput(inputId = "numPlantsPerPlot", label = "Number of plants planted per plot", value = ""),
                                                 #          textInput(inputId = "numRowsPerPlot", label = "Number of rows per plot", value = ""),
                                                 #          textInput(inputId = "numPlantsPerRow", label = "Number of plants per row", value = ""),
                                                 #          textInput(inputId = "plotSize", label = "Plot size (m2)", value = ""),
                                                 #
                                                 #          textInput(inputId = "rowSpacing", label = "Row spacing", value = ""),
                                                 #          textInput(inputId = "rowOrientation", label = "Row orientation", value = ""),
                                                 #
                                                 #          textInput(inputId = "spaceBwPlants", label = "Space between plants", value = ""),
                                                 #          textInput(inputId = "spaceBwRows", label = "Space between rows", value = ""),
                                                 #          textInput(inputId = "planDensity", label = "Plant density (plant/Ha)", value = ""),
                                                 #
                                                 #          textInput(inputId = "hillSpacing", label = "Hill spacing", value = ""),
                                                 #          textInput(inputId = "numPlantsPerPlot", label = "Number of measured plants per plot", value = "")
                                                 #   )
                                                 # ),
                                                 # fluidRow(
                                                 #   column(width = 6, align = "left",
                                                 #          br(),
                                                 #          actionButton(inputId = "btnNextAgro", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                 # )
                                                 #fluidRow(
                                                   sidebarPanel(id="sidebar", width = 12,
                                                                actionButton("btnNextAgro", "Next", class = "btn-primary",style="color: #fff;")
                                                   )
                                                 #)

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

                                             shiny::tabPanel("Agro-features", value = "tabAgroFeat", icon = shiny::icon("truck"),
                                                    #h2("Agronomic features"),
                                                    #fluidRow(
                                                      # fluidRow(
                                                      #br(),

                                                        column(width = 6,
                                                               h2("Agronomic features"),
                                                               selectizeInput("selectAgroFeature", "Agronomic feature", c(), multiple = TRUE, choices=c("Land preparation", "Mulching", "Planting","Irrigation event", "Biofertilizer", "Pest & disease", "Nutrient management event","Harvest" ), options = list(maxItems = 8, placeholder = "Select some...")
                                                                )
                                                        ),
                                                      # ),

                                                      br(),
                                                      column(width = 12,

                                                      br(),
                                                      # tabsetPanel(
                                                      #fluidRow(
                                                      # tabBox(height = NULL, width = 12,
                                                        tabsetPanel(id= "nutrienTabPanels",
                                                        tabPanel("Land preparation", value="tabLandPr",
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                 #br(),
                                                                 h2("Land preparation"),
                                                                 fluidRow(
                                                                 box(title = "Land Levelling",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,


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
                                                                             selectizeInput("impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Drag bucket",
                                                                                                                                  "Harrow",
                                                                                                                                  "Laser-controlled",
                                                                                                                                  "Leveling board",
                                                                                                                                  "Other - specify",
                                                                                                                                  "Tractor blade",
                                                                                                                                  "Disk harrow")
                                                                             ),
                                                                             selectizeInput("animal_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), label = "Animal Traction", choices =
                                                                                                                                c("Buffalo",
                                                                                                                                  "Camel",
                                                                                                                                  "Donkey",
                                                                                                                                  "Elephant",
                                                                                                                                  "Horse",
                                                                                                                                  "Mule",
                                                                                                                                  "Ox / Bullock / Steer",
                                                                                                                                  "Other"
                                                                                                                                )
                                                                             ),
                                                                             textInput("humanPowered", value="", label = "Human powered"),
                                                                             selectizeInput("motorized_traction", label = "Motorized Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                                 c("2 wheel tractor",
                                                                                                                                    "4 wheel tractor",
                                                                                                                                   "Other"
                                                                                                                                 )
                                                                             )
                                                                        ))
                                                                    ))
                                                                )),
                                                                fluidRow(
                                                                 box(title = "Puddling",
                                                                     solidHeader = TRUE,
                                                                     status = "primary",
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
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
                                                                                        column(width = 6,##IMPLEMENTAR EN EL EXCEL
                                                                                              selectizeInput("puddling_depth_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("cm", "mm"))
                                                                                        )
                                                                                      )
                                                                             ))
                                                                         ),
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                             box(
                                                                               title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                                 selectizeInput("pud_animal_traction", label = "Animal traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                                             c("Buffalo",
                                                                                                                                               "Camel",
                                                                                                                                               "Donkey",
                                                                                                                                               "Elephant",
                                                                                                                                               "Horse",
                                                                                                                                               "Mule",
                                                                                                                                               "Ox / Bullock / Steer",
                                                                                                                                               "Other"
                                                                                                                                             )
                                                                                 ),
                                                                                 textInput("pud_humanPowered", value="", label = "Human powered"),
                                                                                 selectizeInput("pud_motorized_traction", label = "Motorized traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                                         c("2 wheel tractor",
                                                                                                                                           "4 wheel tractor",
                                                                                                                                           "Other"
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
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
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
                                                                            selectizeInput("till_technique",  label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
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
                                                                               selectizeInput("till_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Chisel plough",
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
                                                                               selectizeInput("till_animal_traction", label = "Animal traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                             c("Buffalo",
                                                                                               "Camel",
                                                                                               "Donkey",
                                                                                               "Elephant",
                                                                                               "Horse",
                                                                                               "Mule",
                                                                                               "Ox / Bullock / Steer",
                                                                                               "Other"
                                                                                             )
                                                                               ),

                                                                               textInput("till_humanPowered", value="", label = "Human powered"),
                                                                               selectizeInput("till_motorized_traction", label = "Motorized traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                             c("2 wheel tractor",
                                                                                               "4 wheel tractor",
                                                                                               "Other"
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
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
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
                                                                                     selectizeInput("lim_quantity_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("kg/m2", "kg/ha", "t/ha"))
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


                                                          )),#),
                                                        tabPanel("Mulching", value="tabMulching",
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Mulching"),
                                                                 #br(),
                                                                 fluidRow(
                                                                 box(title = "Mulch",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("mulch_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("mulch_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectizeInput("mulch_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Bark chips",
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
                                                                            selectizeInput("mulch_color", label = "Mulch color", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Black",
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
                                                                         textInput("mulch_make", value="", label = "Implement make"),
                                                                         textInput("mulch_model", value="", label = "Implement model"),
                                                                                selectizeInput("mulch_animal_traction", label = "Animal traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                              c("Buffalo",
                                                                                                "Camel",
                                                                                                "Donkey",
                                                                                                "Elephant",
                                                                                                "Horse",
                                                                                                "Mule",
                                                                                                "Ox / Bullock / Steer",
                                                                                                "Other"
                                                                                              )
                                                                                ),
                                                                                textInput("mulch_humanPowered", value="", label = "Human powered"),
                                                                                selectizeInput("mulch_motorized_traction", label = "Motorized traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                              c("2 wheel tractor",
                                                                                                "4 wheel tractor",
                                                                                                "Other"
                                                                                              )
                                                                                )
                                                                       ))
                                                                   ))


                                                                 )),
                                                                 fluidRow(
                                                                 box(title = "Residue management",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow( ##IMPLEMENTAR EN EL EXCEL
                                                                              column(width = 6,
                                                                                     dateInput("residue_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,##IMPLEMENTAR EN EL EXCEL
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
                                                                                selectizeInput("residue_cropType", label = "Crop type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                              c("Stubble",
                                                                                                "Stem/Leaf",
                                                                                                "Seed Pod - Cob - Fruit",
                                                                                                "Husk",
                                                                                                "Roots")
                                                                                ),
                                                                                selectizeInput("residue_technique", label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                              c("Burning",
                                                                                                "No-tillage",
                                                                                                "Spreading",
                                                                                                "Tillage"
                                                                                              )
                                                                                ),
                                                                                textInput("residue_incorp_depth", value="", label="Residue incorporation depth"),
                                                                                selectizeInput("residue_aboveGroundMoisture", label = "Above ground residue moisture", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                              c("Dry",
                                                                                                "Moist",
                                                                                                "Wet")
                                                                                ),
                                                                                fluidRow(
                                                                                    column(width = 6,
                                                                                      textInput("residue_aboveGroundAmount",  label = "Above ground residue amount", value="")
                                                                                    ),
                                                                                    column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                                                                      selectizeInput("residue_aboveGroundAmount_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
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
                                                        )),#),#end tab mulching
                                                        tabPanel("Planting", value="tabPlanting",
                                                                 #br(),
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Planting"),
                                                                 fluidRow(
                                                                 box(title = "Planting method",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
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
                                                                                      selectizeInput("planting_seedingTech", label = "Seeding technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
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
                                                                                  selectizeInput("planting_manual", label = "Manual", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                c("Hand broadcasting",
                                                                                                  "Mechanical broadcasting",
                                                                                                  "Line planting by hand",
                                                                                                  "Jab planting",
                                                                                                  "Dibbling stick",
                                                                                                  "Drum seeding")
                                                                                  ),
                                                                                  selectizeInput("planting_animal_traction", label = "Animal traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                c("Buffalo",
                                                                                                  "Camel",
                                                                                                  "Donkey",
                                                                                                  "Elephant",
                                                                                                  "Horse",
                                                                                                  "Mule",
                                                                                                  "Ox / Bullock / Steer",
                                                                                                  "Other"
                                                                                                )
                                                                                  ),
                                                                                  selectizeInput("planting_motorized_traction", label = "Motorized traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                c("2 wheel tractor",
                                                                                                  "4 wheel tractor",
                                                                                                  "Other"
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
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(
                                                                        column(width = 6,
                                                                               fluidRow(
                                                                                column(width = 6, #IMPLEMENTAR EN EXCEL
                                                                                       dateInput("sowing_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                ),
                                                                                column(width = 6, #IMPLEMENTAR EN EXCEL
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
                                                                                 textInput("planting_distance", value="", label="Planting distance")

                                                                               ))
                                                                         ),
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                                box(
                                                                                  title = "Planting distribution", solidHeader = TRUE, status = "warning", width=12,
                                                                                  selectizeInput("planting_distribution", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                c("Broadcast",
                                                                                                  "Row planting")
                                                                                  )


                                                                                ))
                                                                         )
                                                                     )
                                                                 )) #end box sowing
                                                        )),#),#end tab planting

                                                        tabPanel("Irrigation event", value="tabIrrigation",
                                                                 #br(),
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Irrigation event"),
                                                                 fluidRow(
                                                                 box(title = "Irrigation description",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
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
                                                                            selectizeInput("irrigation_system_type", label = "Irrigation system type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Automatic irrigation",
                                                                                            "Manual irrigation")
                                                                            ),
                                                                            selectizeInput("irrigation_technique", label = "Irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Surface irrigation technique",
                                                                                            "Localized irrigation technique",
                                                                                            "Irrigation using sprinkler systems",
                                                                                            "Sub-irrigation")
                                                                            ),
                                                                            selectizeInput("surface_irrigation_technique", label = "Surface irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Furrow irrigation",
                                                                                            "Uncontrolled flooding",
                                                                                            "Basin irrigation",
                                                                                            "Border irrigation",
                                                                                            "Continuous flood")
                                                                            ),
                                                                            selectizeInput("localized_irrigation_technique", label = "Localized irrigation technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Drip irrigation",
                                                                                            "Subsurface textile irrigation",
                                                                                            "Mist irrigation",
                                                                                            "Subsurface drip irrigation",
                                                                                            "Bubbler irrigation",
                                                                                            "Pitcher irrigation")
                                                                            ),
                                                                            selectizeInput("irrigation_using_sprinkler_systems", label = "Irrigation using sprinkler systems", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Center pivot irrigation",
                                                                                            "Irrigation by lateral move",
                                                                                            "Irrigation by side move")
                                                                            ),
                                                                            #Sacar myFile upload
                                                                            fileInput("myFile", "Irrigation system picture", accept = c('image/png', 'image/jpeg')),
                                                                            textInput("irrigation_water_source", value="", label = "Water source"),
                                                                            textInput("irrigation_water_source_distance", value="", label = "Water source distance"),
                                                                            textInput("irrigation_bund_height", value="", label = "Bund height"),
                                                                            textInput("irrigation_percolation_rate", value="", label = "Percolation rate"),
                                                                            textInput("irrigation_equipment_depth", value="", label = "Irrigation equipment depth"),
                                                                            textInput("irrigation_well_depth", value="", label = "Well depth"),
                                                                            textInput("irrigation_area_covered_irrigation_system", value="", label = "Area covered by the irrigation system")
                                                                     ))
                                                                 ))#end box description irrigation
                                                        )),#),#end tab irrigation
                                                        tabPanel("Biofertilizer", value="tabBiofertilizer",
                                                                 #br(),
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Biofertilizer"),
                                                                          fluidRow(
                                                                 box(title = "Description Biofertilizer",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("biofertilizer_landLeveling_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("biofertilizer_landLeveling_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectizeInput("biofertilizer_rhizobium_inoculum_strain", label = "Rhizobium inoculum strain", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Rhizobium japonicum",
                                                                                            "Rhizobium leguminosarum",
                                                                                            "Rhizobium loti",
                                                                                            "Rhizobium meliloti",
                                                                                            "Rhizobium spp.",
                                                                                            "Rhizobium trifolii",
                                                                                            "Other")
                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     textInput("biofertilizer_quantity_inoculated", value = "", label="Quantity inoculated")
                                                                              ),
                                                                              column(width = 6, #IMPLEMENTAR EN EXCEl
                                                                                     selectizeInput("biofertilizer_quantity_inoculated_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("kg/m2", "kg/ha", "t/ha"))
                                                                              )

                                                                            ),
                                                                            selectizeInput("biofertilizer_inoculation_method", label = "Inoculation method", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Seed coating (Seed application of inoculum)",
                                                                                            "Directly to the soil",
                                                                                            "Other")
                                                                            ),
                                                                            selectizeInput("biofertilizer_product_formulation", label = "Product formulation", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Soil application with granules/pellets",
                                                                                            "Soil application with slurry of liquid culture")
                                                                            ),
                                                                            textInput("biofertilizer_days_sowing_after_rhizobium_inocculation", value="", label = "Days to sowing after Rhizobium inocculation")
                                                                     ))
                                                                 ))#end box description biofertilizer
                                                        )),#),#end tab biofertilizer
                                                        tabPanel("Pest & disease", value="tabPestNDisease",
                                                                 #br(),
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Pest & disease"),
                                                                          fluidRow(
                                                                 box(title = "Disease observation",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
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

                                                                            dateInput("disease_observation_date", label ="Disease observation date", format = "dd/mm/yyyy"),
                                                                            textInput("disease_name", value="", label = "Disease name"),
                                                                            textInput("disease_plant_parts_affected", value="", label = "Plant parts affected"),
                                                                            textInput("disease_percentage_experiement_affected", value="", label = "Percentage of the experiment affected"),
                                                                            textInput("disease_damages_notes", value="", label = "Disease damages, notes"),
                                                                            textInput("disease_notes", value="", label = "Disease, notes")
                                                                     ))
                                                                 )),#end box disease
                                                                 fluidRow(
                                                                 box(title = "Pest observation",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
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
                                                                            selectizeInput("pest_type", label = "Pest type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Bacteria",
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
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("pestcontrol_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("pestcontrol_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                              )
                                                                            ),
                                                                            selectizeInput("pest_control_technique", label = "Pest control technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Biological pest control",
                                                                                            "Chemical pest control",
                                                                                            "Mechanical pest control")
                                                                            ),
                                                                            textInput("pesticide_application_depth", value="", label = "Pesticide application depth"),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     textInput("pesticide_amount", value = "", label="Pesticide amount")
                                                                              ),
                                                                              column(width = 6,#IMPLEMENTAR EN EXCEL
                                                                                     selectizeInput("pesticide_amount_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("kg/m2", "kg/ha", "t/ha"))
                                                                              )
                                                                            ),
                                                                            fileInput("myFile", "Pesticide box or bottle picture", accept = c('image/png', 'image/jpeg')),
                                                                            textInput("pest_control_applications_totnumber", value="", label = "Pest control applications total number"),
                                                                            textInput("pest_control_details", value="", label = "Pest control details (e.g. name of parasitoid etc), treatment evaluation"),
                                                                            selectizeInput("chemical_pest_control_equipment", label = "Chemical pest control equipment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                          c("Aerial applicator",
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
                                                                              title = "Pesticide Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                              textInput("pesticide_implement_make", value="", label = "Implement make"),
                                                                              textInput("pesticide_implement_model", value="", label = "Implement model"),
                                                                              selectizeInput("pesticide_animal_traction", label = "Animal Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                            c("Buffalo",
                                                                                              "Camel",
                                                                                              "Donkey",
                                                                                              "Elephant",
                                                                                              "Horse",
                                                                                              "Mule",
                                                                                              "Ox / Bullock / Steer",
                                                                                              "Other"
                                                                                            )
                                                                              ),
                                                                              textInput("pesticide_humanPowered", value="", label = "Human powered"),
                                                                              selectizeInput("pesticide_motorized_traction", label = "Motorized Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                            c("2 wheel tractor",
                                                                                              "4 wheel tractor",
                                                                                              "Other"
                                                                                            )
                                                                              )
                                                                            ))
                                                                     ))
                                                                 ))#end box pest control
                                                        )#end tab pest&disease
                                                      ),#),
                                                      tabPanel("Nutrient management event", value="tabNutrient",
                                                               #fluidRow(
                                                                 column(width = 12,
                                                                        #br(),
                                                                        h2("Nutrient management event"),
                                                                        #fluidRow(
                                                                          ## here goes the nutrients prototype panel

                                                                        fluidRow(
                                                                          column(width = 6,
                                                                                 textInput("fertilizer_application_number", "Fertilizer application number"),
                                                                                 fluidRow(
                                                                                   column(width = 6,
                                                                                          textInput(inputId="fertilizer_total_quantity", label="Fertilizer total quantity")
                                                                                   ),
                                                                                   column(width = 6,
                                                                                          selectizeInput("fertilizer_total_quantity_unit", multiple =T, options = list(maxItems=1, placeholder="Select one..."),"Unit", c("kg/m2","kg/ha","t/ha"))
                                                                                   )
                                                                                 )
                                                                          )
                                                                        ),
                                                                        fluidRow(
                                                                          box(title = "Fertilization details",
                                                                              status = "primary",
                                                                              solidHeader = TRUE,
                                                                              width = 12, collapsible = TRUE,  collapsed = T,
                                                                              fluidRow(
                                                                                column(width = 6,
                                                                                       fluidRow(
                                                                                         column(width = 6,
                                                                                                dateInput("fertilization_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                         ),
                                                                                         column(width = 6,
                                                                                                dateInput("fertilization_end_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                         )
                                                                                       ),
                                                                                       fluidRow(
                                                                                         column(width = 6,
                                                                                                selectizeInput("appfTypeFertilizer", "Type of fertilizer used", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."),
                                                                                                               choices=c("Inorganic", "Organic")
                                                                                                )
                                                                                         ),
                                                                                         column(width = 6,
                                                                                                conditionalPanel("input.appfTypeFertilizer == 'Inorganic'",
                                                                                                                 selectizeInput("typeInorganic",multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="Inorganic", choices =c(
                                                                                                                   "Ammonium nitrate",
                                                                                                                   "Ammonium nitrate sulfate",
                                                                                                                   "Ammonium polyphosphate",
                                                                                                                   "Ammonium sulfate",
                                                                                                                   "Anhydrous ammonia",
                                                                                                                   "Aqua ammonia",
                                                                                                                   "Calcitic limestone",
                                                                                                                   "Calcium ammonium nitrate solution",
                                                                                                                   "Calcium hydroxide",
                                                                                                                   "Calcium nitrate",
                                                                                                                   "Diammnoium phosphate",
                                                                                                                   "Dolomitic limestone",
                                                                                                                   "Liquid phosphoric acid",
                                                                                                                   "Monoammonium phosphate",
                                                                                                                   "Potassium chloride",
                                                                                                                   "Potassium nitrate",
                                                                                                                   "Potassium sulfate",
                                                                                                                   "Rock phosphate",
                                                                                                                   "Single super phosphate",
                                                                                                                   "Triple super phosphate",
                                                                                                                   "Urea",
                                                                                                                   "Urea ammonium nitrate solution",
                                                                                                                   "Urea super granules",
                                                                                                                   "Other")
                                                                                                                 )
                                                                                                ),

                                                                                                conditionalPanel("input.appfTypeFertilizer == 'Organic'",
                                                                                                                 selectizeInput("typeOrganic", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."), label ="Organic", choices =c(
                                                                                                                   "Alfalfa Meal",
                                                                                                                   "Bagasse",
                                                                                                                   "Biochar",
                                                                                                                   "Blood meal",
                                                                                                                   "Bone meal",
                                                                                                                   "Chicken litter",
                                                                                                                   "Compost",
                                                                                                                   "Cottonseed Meal",
                                                                                                                   "Farmyard manure",
                                                                                                                   "Fish emulsion",
                                                                                                                   "Fish manure",
                                                                                                                   "Fish meal",
                                                                                                                   "Green manure",
                                                                                                                   "Guano",
                                                                                                                   "Hydrolyzed Fish",
                                                                                                                   "Liquid manure",
                                                                                                                   "Oil cake",
                                                                                                                   "Peat",
                                                                                                                   "Spent mushroom compost",
                                                                                                                   "Treated sewage sludge",
                                                                                                                   "Other")
                                                                                                                 )
                                                                                                )
                                                                                         )
                                                                                       ),
                                                                                       fileInput("fert_picture", "Fertilizer picture", accept = c('image/png', 'image/jpeg')),
                                                                                       selectizeInput("fertilizer_application_technique", "Fertilizer application technique", multiple = F,
                                                                                                      options = list(maxItems = 1, placeholder ="Select one"),
                                                                                                      choices = c("Band application beneath surface",
                                                                                                                  "Band application on surface",
                                                                                                                  "Broadcast incorporated",
                                                                                                                  "Contact placement",
                                                                                                                  "Deep placement",
                                                                                                                  "Fertigation",
                                                                                                                  "Foliar application",
                                                                                                                  "Injection",
                                                                                                                  "Placed with seed",
                                                                                                                  "Plough sole placement",
                                                                                                                  "Side dressing",
                                                                                                                  "Sub-soil placement",
                                                                                                                  "Topdressing",
                                                                                                                  "Other")
                                                                                       ),
                                                                                       textInput(inputId="fertilizer_application_depth", label="Fertilizer application depth"),
                                                                                       fluidRow(
                                                                                         column(width = 6,
                                                                                                textInput(inputId="fertilizer_recommended_rate", label="Fertilizer recommended rate")
                                                                                         ),

                                                                                         column(width = 6,
                                                                                                selectizeInput("fertilizer_recommended_rate_unit", multiple =T, options = list(maxItems=1, placeholder="Select one..."), label= "Unit", c("kg/m2","kg/ha","t/ha"))
                                                                                         )
                                                                                       ),
                                                                                       textInput(inputId="percentage_of_fertilizer_recommended_rate_applied", label="Percentage of fertilizer recommended rate applied")
                                                                                ),
                                                                                column(width = 6,
                                                                                       br(),
                                                                                       fluidRow(
                                                                                         box(
                                                                                           title = "Fertilizer implement description", solidHeader = TRUE, status = "warning", width=12,
                                                                                           selectizeInput("fertilizer_implement", label = "Fertilizer implement",multiple =T, options = list(maxItems=1, placeholder="Select one..."),
                                                                                                       choices = c("Airblast sprayer",
                                                                                                                             "Backpack sprayer",
                                                                                                                             "Boom sprayer",
                                                                                                                             "Broadcast spreader",
                                                                                                                             "Hand sprayer",
                                                                                                                             "Manure spreader",
                                                                                                                             "Slurry injector",
                                                                                                                             "Other")
                                                                                           ),
                                                                                           fileInput("fert_implementPicture", "Fertilizer implement picture", accept = c('image/png', 'image/jpeg')),
                                                                                           textInput("fertilizer_implement_make", value="", label = "Fertilizer implement make"),
                                                                                           textInput("fertilizer_implement_model", value="", label = "Fertilizer implement model"),
                                                                                           box(
                                                                                             title = "Fertilizer implement traction", solidHeader = TRUE, status = "info", width=12,
                                                                                             selectizeInput("fert_animalTraction", "Animal traction", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."),
                                                                                                            choices = c(
                                                                                                              "Buffalo",
                                                                                                              "Camel",
                                                                                                              "Donkey",
                                                                                                              "Elephant",
                                                                                                              "Horse",
                                                                                                              "Mule",
                                                                                                              "Ox / Bullock / Steer",
                                                                                                              "Other")
                                                                                             ),
                                                                                             textInput("fert_humanPowered", "Human powered", value= ""),
                                                                                             selectizeInput("fert_motorizedTraction", "Motorized traction", multiple = TRUE, options = list(maxItems = 1, placeholder = "Select one..."),
                                                                                                            choices = c(
                                                                                                              "2 wheel tractor",
                                                                                                              "4 wheel tractor",
                                                                                                              "Other")
                                                                                             )
                                                                                           )
                                                                                         )
                                                                                       )
                                                                                )
                                                                              )
                                                                          )
                                                                        ),
                                                                        fluidRow(
                                                                          box(title = "Nutrient amount applied",
                                                                              status = "primary",
                                                                              solidHeader = TRUE,
                                                                              width = 12, collapsible = TRUE,  collapsed = T,
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", h4("Name"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("Number of applications"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("Order"))
                                                                                ), column(width = 2,
                                                                                          br(),
                                                                                          div(style="text-align:center", h4("Amount applied"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("Amount applied (scale)"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("Nutrient concentration in fertilizer applied"))
                                                                                )
                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Nitrogen")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_nit_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_nit_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_nit_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_nit_numApps1 == 2 |
                                                                                               input.nutrientApplied_nit_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_nit_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_nit_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_nit_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_nit_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_nit_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_nit_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_nit_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Phosphorus")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_phosp_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_phosp_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_phosp_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_phosp_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_phosp_numApps1 == 2 |
                                                                                               input.nutrientApplied_phosp_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_phosp_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_phosp_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_phosp_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_phosp_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_phosp_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_phosp_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_phosp_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Potassium")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_potass_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_potass_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_potass_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_potass_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_potass_numApps1 == 2 |
                                                                                               input.nutrientApplied_potass_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_potass_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_potass_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_potass_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_potass_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_potass_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_potass_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_potass_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Calcium")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_calcium_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_calcium_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_calcium_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_calcium_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_calcium_numApps1 == 2 |
                                                                                               input.nutrientApplied_calcium_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_calcium_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_calcium_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_calcium_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_calcium_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_calcium_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_calcium_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_calcium_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Sulphur")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_sulph_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_sulph_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_sulph_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_sulph_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_sulph_numApps1 == 2 |
                                                                                               input.nutrientApplied_sulph_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_sulph_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_sulph_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_sulph_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_sulph_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_sulph_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_sulph_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_sulph_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Magnesium")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Nitrate")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Ammonium")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Iron")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Zinc")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Copper")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Boron")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Molybdenum")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Manganese")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_mangan_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_mangan_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 2 |
                                                                                               input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_mangan_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_mangan_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_mangan_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              ),
                                                                              fluidRow(
                                                                                column(width = 1,
                                                                                       br(),
                                                                                       div(style="text-align:right", "Chlorine")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectInput("nutrientApplied_nit_numApps1","",c(1,2,3))
                                                                                ),
                                                                                column(width = 2,
                                                                                       br(),
                                                                                       div(style="text-align:center", h4("1"))
                                                                                ),

                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied _clh_amountApplied1","")
                                                                                ),
                                                                                column(width = 2,
                                                                                       selectizeInput("nutrientApplied_clh_amountScale1","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                ),
                                                                                column(width = 2,
                                                                                       textInput("nutrientApplied_ clh_concentFertApplied1","")
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.nutrientApplied_clh_numApps1 == 2 |
                                                                                               input.nutrientApplied_clh_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("2"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_clh_amountApplied2","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_clh_amountScale2","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_clh_concentFertApplied2","")
                                                                                                 )
                                                                                               )

                                                                              ),


                                                                              conditionalPanel("input.nutrientApplied_clh_numApps1 == 3 ",
                                                                                               fluidRow(
                                                                                                 column(width = 1,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", "")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:right", " ")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        br(),
                                                                                                        div(style="text-align:center", h4("3"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_clh_amountApplied3","")
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        selectizeInput("nutrientApplied_clh_amountScale3","", multiple=T, options=list(maxItems=1, placeholder="Select one..."),c("kg/m2","kg/ha","t/ha"))
                                                                                                 ),
                                                                                                 column(width = 2,
                                                                                                        textInput("nutrientApplied_clh_concentFertApplied3","")
                                                                                                 )
                                                                                               )

                                                                              )


                                                                          )
                                                                        )



                                                                        #)
                                                                 )),#)#end tab nutrient management event
                                                      tabPanel("Harvest", value="tabHarvest",
                                                               #br(),
                                                               #fluidRow(
                                                               column(width = 12,
                                                                      #br(),
                                                                      h2("Harvest"),
                                                                      fluidRow(
                                                                        box(title = "Description Harvest",
                                                                            status = "primary",
                                                                            solidHeader = TRUE,
                                                                            width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     fluidRow(
                                                                                       column(width = 6,
                                                                                              dateInput("harvest_start_date", label ="Start date", format = "dd/mm/yyyy")
                                                                                       ),
                                                                                       column(width = 6,
                                                                                              dateInput("harvest_end_date", label ="End date", format = "dd/mm/yyyy")
                                                                                       )
                                                                                     ),
                                                                                     selectizeInput("crop_component_harvested", label = "Crop component harvested", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                      c("Canopy",
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
                                                                                         selectizeInput("harvest_implement", label = "Harvest implement", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                          c("Baler",
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
                                                                                         textInput("harvest_make", value="", label = "Implement make"),
                                                                                         textInput("harvest_model", value="", label = "Implement model"),
                                                                                         selectizeInput("harvest_animal_traction", label = "Animal Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                          c("Buffalo",
                                                                                                            "Camel",
                                                                                                            "Donkey",
                                                                                                            "Elephant",
                                                                                                            "Horse",
                                                                                                            "Mule",
                                                                                                            "Ox / Bullock / Steer",
                                                                                                            "Other"
                                                                                                          )
                                                                                         ),
                                                                                         textInput("harvest_humanPowered", value="", label = "Human powered"),
                                                                                         selectizeInput("harvest_motorized_traction", label = "Motorized Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                          c("2 wheel tractor",
                                                                                                            "4 wheel tractor",
                                                                                                            "Other"
                                                                                                          )
                                                                                         )
                                                                                       ))
                                                                              ))
                                                                        ))#end box description harvest
                                                               ))#),#end tab harvest
                                                      )#end tabbox
                                                      #)
                                                    ),#asd

                                                    #),
                                                    #fluidRow(
                                                      sidebarPanel(id="sidebar", width = 12,
                                                                   actionButton("btnNextTraits", "Next", class = "btn-primary",style="color: #fff;")
                                                      )
                                                    #)



                                             ),

                                              shiny::tabPanel("Traits", value = "tabTraits", icon = shiny::icon("leaf"),
                                                  #h2("Traits"),
                                                  #fluidRow(
                                                  #br(),
                                                    column(width = 12,
                                                           h2("Traits"),
                                                           br()
                                                          # shinyTree::shinyTree("designFieldbook_traits_agrofims",search = TRUE,checkbox = TRUE)

                                                          #uiOutput("uiTraitsList")
                                                  ),#),
                                                  uiOutput("uiTraitsList"),

                                                  # fluidRow(
                                                  #   column(width = 6, align = "left",
                                                  #          br(),
                                                  #          actionButton(inputId = "btnNextEnv", label = "Next", style="color: #fff; background-color: #35b872;"))
                                                  # )
                                                  #fluidRow(
                                                    sidebarPanel(id="sidebar", width = 12,
                                                                 actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;")
                                                    )
                                                  #)
                                              ),
                                              shiny::tabPanel("Weather & Soil", value = 'tabEnvironment', icon = shiny::icon("bolt"),
                                                  #br(),
                                                  # h2("Weather & Soil"),
                                                  #fluidRow(
                                                    column(width = 12,
                                                           h2("Weather & Soil"),
                                                  shinyTree::shinyTree("designFieldbook_weatherVar_agrofims",search = TRUE,checkbox = TRUE),
                                                  shinyTree::shinyTree("designFieldbook_soilVar_agrofims",search = TRUE,checkbox = TRUE)
                                                    )#),
                                                  #shiny::checkboxInput("fbDesign_weather_cb", label = "Register weather data"),
                                                  #shiny::checkboxInput("fbDesign_soil_cb", label = "Register soil data")
                                                  #fluidRow(
                                                    # sidebarPanel(id="sidebar", width = 12,
                                                    #              actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;")
                                                    # )
                                                  #)
                                              )#,
                                         #) #end master

                                      )#) end tabbox,
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
                          #rHandsontableOutput("hot", width = 1000),
          conditionalPanel( condition = "output.show_agrotable",
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
                                                             rhandsontable::rHandsontableOutput("fbDesign_table_agrofims", height = 400)
                                               # )#,
                                              #)
                                          ),
                                         br(),
                                         br(),
                                         br()
                                     )
  ),#end conditional panel

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



