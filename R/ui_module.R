# choices for statistical design input
design_choices <- c(
  "Unreplicated Design (UNDR)" = "UNDR",
  "Westcott Design (WD)" = "WD",#
  "Completely Randomized Design (CRD)" = "CRD",
  "Randomized Complete Block Design (RCBD)" = "RCBD",
  "Augmented Block Design (ABD)" = "ABD",
  "Alpha Design (AD)" = "AD",
  "Latin Square Design (LSD)" = "LSD",
  "Factorial Two-Way Design in CRD (F2CRD)" = "F2CRD",
  "Factorial Two-Way Design in RCBD (F2RCBD)" = "F2RCBD",
  #"Split Plot with Plots in CRD (SPCRD)" = "SPCRD",
  #"Split Plot with Plots in RCBD (SPRCBD)" = "SPRCBD", #R.Eyzaguirre recommends just one Split Design
  "Split Plot with Plots Design" = "SPRCBD", #

  #"Split Plot with Plots in LSD (SPLSD)" = "SPLSD",
  "Strip Plot Design (STRIP)" = "STRIP"
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
genetic_design_choices <- c(
  "North Carolina Design I" = "NCI",
  "North Carolina Design II" = "NCII",
  "Line by Tester" = "LXT"
)

# Conditional panels for genetic design
genetic_design_conditional_panels <- function(){
  list(

    shiny::conditionalPanel(
      "input.design_geneticFieldbook == 'NCI'|
       input.design_geneticFieldbook == 'NCII'",

       shiny::selectInput("design_genetic_nc_set", "Set", 2:100, 2),
       #shiny::selectInput("design_genetic_nc_r", "Replications", 2:100, 2),
       shiny::selectInput("design_genetic_ploidy", "Type of ploidy", choices = c("Diploid","Tetraploid"))

    ) ,

    shiny::conditionalPanel(
      "input.design_geneticFieldbook == 'LXT'|
       input.design_geneticFieldbook == 'NCI'|
       input.design_geneticFieldbook == 'NCII'",

      #shiny::selectInput("design_genetic_nc_set", "Set", 2:100, 2),
      shiny::selectInput("design_genetic_r", "Replications", 2:100, 2)#,
      #shiny::selectInput("design_genetic_ploidy", "Type of ploidy", choices = c("Diploid","Tetraploid"))

    ) ,

    shiny::conditionalPanel(
      "input.design_geneticFieldbook == 'LXT'",

      #shiny::selectInput("design_genetic_nc_set", "Set", 2:100, 2),
      shiny::selectInput("design_genetic_lxt_type", "Type of scheme", list("progenitors and progenie"= 1, "progenie" =2))#
      #shiny::selectInput("design_genetic_ploidy", "Type of ploidy", choices = c("Diploid","Tetraploid"))

    )


  )
}

# Conditional Panels or features according to each statistical design
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
      shiny::selectInput("designFieldbook_r", "Replications", 2:100, 2 )

    ),

    # shiny::conditionalPanel(
    #      "input.designFieldbook == 'UNDR'  |
    #       input.designFieldbook == 'CRD'   |
    #       input.designFieldbook == 'RCBD'  |
    #       input.designFieldbook == 'F2CRD' |
    #       input.designFieldbook == 'F2RCBD'|
    #       input.designFieldbook == 'SPRCBD'",
    #
    #   shiny::checkboxInput("designFieldbook_cbrwcol", "Add row and column", value = FALSE)#,

      # shiny::conditionalPanel(
      #   "input.designFieldbook_cbrwcol == true",
      #   shiny::selectInput("designFieldbook_expdis_colb", "Number of columns", 2:100, 10)
      # )
    # ),



   shiny::conditionalPanel(
      "input.designFieldbook == 'SPCRD' |
       input.designFieldbook == 'SPRCBD'|
       input.designFieldbook == 'F2CRD' |
       input.designFieldbook == 'F2RCBD'|
       input.designFieldbook == 'SPLSD' |
       input.designFieldbook == 'STRIP'
      ",

      #shiny::selectInput("designFieldbook_r", "Replications:", 1:5, 2 ),
      textInput(inputId = "factor_name", label = "Enter Additional Factor Name", ""),
      br(),
      textInput(inputId = "factor_lvl", label = "Type levels of factors (separated by commas ',')", value = "")#,
      # textInput(inputId = "factor_lvl1", label = "First Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl2", label = "Second Level for Additional Factor",value = ""),
      # textInput(inputId = "factor_lvl3", label = "Third Level for Additional Factor",value = "")

    ),

    shiny::conditionalPanel(
      "input.designFieldbook == 'SPCRD' |
      input.designFieldbook == 'SPRCBD'",

      shiny::uiOutput("fbdesign_split_cb")
    ),

    shiny::conditionalPanel(
      "input.designFieldbook == 'WD'",
      shiny::selectInput("designFieldbook_wd_col",  "Number of columns", 50:5000, 100 ),
      shiny::selectInput("designFieldbook_wd_colb", "Number of columns between two check columns", 2:100, 10)

    ),


    shiny::conditionalPanel(
      "input.designFieldbook == 'ABD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Augmented design needs at least 2 checks. Verify if your material have these checks and put an 'x' mark in 'Is_control' column for each check.
              Otherwise, Augmented Design does not work.",

              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),

    shiny::conditionalPanel(
      "input.designFieldbook == 'WD'", # TODO: ab - factorial, split
      infoBox(title = "IMPORTANT NOTE", subtitle = "Westcott design needs 2 checks and at least 10 genotypes. Verify if your material list has these checks and put an 'x' mark in 'Is_control' column for each check.
              Otherwise, Westcott Design does not work.",

              icon = icon("bullhorn"), color = "blue", fill = TRUE, width = NULL)
    ),


    shiny::conditionalPanel(
      "input.designFieldbook == 'LD'", # TODO: ab - factorial, split
      shiny::selectInput("designFieldbook_r", "Replications", 2:100, 2 )
    ),
    shiny::conditionalPanel(
      "input.designFieldbook == 'BIBD'",
      shiny::selectInput("designFieldbook_maxR", "Repetition maximum (k)", 3:100, 20 )
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
      #shiny::selectInput("designFieldbook_r", "Replications:", 2:6000, 2 ),
      shiny::selectInput("designFieldbook_k", "Block size (k)", 2:100, 4 )
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
#       shiny::selectInput("designFieldbook_r", "Replications:", 2:10, 2 ),
#       shiny::selectInput("designFieldbook_k", "Block size (k):", 2:10,3 )
#     ),

### Combine factors in statistical designs ########################
    shiny::conditionalPanel(
      "input.designFieldbook == 'UNDR' |
       input.designFieldbook == 'CRD'   |
       input.designFieldbook == 'RCBD'  |
       input.designFieldbook == 'ABD'",
      shiny::checkboxInput("designFieldbook_combfactor", "Add experimental conditions",value = FALSE  ),

      shiny::conditionalPanel(
        "input.designFieldbook_combfactor == true",

        textInput(inputId = "combfactor_name", label = "Experimental conditions label", ""),
        br(),
        textInput(inputId = "combfactor_lvl", label = "Type experimental conditions (separated by commas ',')", value = "")#,

      )

    ),
#########################################

###############

    shiny::conditionalPanel(
         "input.designFieldbook == 'UNDR'  |
          input.designFieldbook == 'CRD'   |
          input.designFieldbook == 'RCBD'  |
          input.designFieldbook == 'ABD'",
      shiny::checkboxInput("designFieldbook_cbssample", "Add sub samples", value = FALSE  ),

      shiny::conditionalPanel(
        "input.designFieldbook_cbssample == true",
        shiny::numericInput(inputId = "designFieldbook_nsample", label = "Enter samples", value = 10, min = 1, max = 1000)
      )

    )#,



  )
}




#' shiny UI element
#'
#' returns a re-usable user interface element
#'
#' @author Omar Benites
#' @param type type of ui element; default is a tab in a shiny dashboard
#' @param title display title
#' @param name a reference name
#' @export


################## Begin Simple Modules

ui_fieldbook <- function(type="tab",title="Design Fieldbook",name="phenotype_fieldbook_design"){

#############
shinydashboard::tabItem(tabName = name,
                h2("Design of Field Experiments"),

                shinyjs::useShinyjs(), #to reset panels and UI


                tags$head(
                  tags$style(HTML("
                                .shiny-output-error-validation {
                                  color: green;
                                  font-size: 120%;
                                  font-family: Arial, Helvetica, sans-serif;
                                }
                              "))
                  ),


                tabsetPanel( #Begin Master tabSetPanel
                  tabPanel("Standard Modules",icon = icon("tag", lib = "glyphicon"),
                           br(),
                               shiny::wellPanel(
                               shiny::HTML("<b>Fieldbook ID </b>"),
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
                                             #shiny::uiOutput("fbDesign_factor2", inline = TRUE),
                                                                        #shiny::uiOutput("fbDesign_variables", inline = TRUE),
                                             shiny::dateRangeInput("fbDesign_project_time_line", "Date of experiment", start = Sys.Date() - 2,
                                                                            end = Sys.Date() + 20, startview = "year",format = "dd/mm/yyyy"),



                                 #shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),#country
                                  #shiny::selectizeInput("fbDesign_country", inline = TRUE, width = 500),#country
                                            # shiny::selectizeInput("fbDesign_countryTrial", label = "Field country",
                                            #                                 choices = country(), selected = 1,  multiple = FALSE),

                                            shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),
                                            shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500), #,#locality
                                            selectInput('fbDesign_nExp', 'Experiment number', c("-",paste("exp",1:100,sep="")), selectize=TRUE)

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
#                                           h4("Define Genotypes",style = "font-family: 'Arial', cursive;
#                                                                  font-weight: 500; line-height: 1.1;
#                                                                  color: #4d3a7d;"),
                                          br(),
                                          radioButtons("select_import", label = h4("Define Genotypes",style = "font-family: 'Arial', cursive;
                                                                 font-weight: 500; line-height: 1.1;
                                                                 color: #4d3a7d;"),
                                                       choices = c("Local List", "Template"),
                                                       selected = "Local List"),

                                          br(),

                                          #selectInput("fbdesign_gentemp", "Type",c("Clones" = "clones","Parental" = "parental")),

                                        conditionalPanel(
                                           condition = "input.select_import == 'Local List'",

                                          shiny::uiOutput("fbDesign_selmlist"),
                                          shiny::actionButton("fdesign_list_refresh", "Refresh List"),

                                          br()#,
                                         ),


                                        shinyWidgets::awesomeCheckbox(inputId = "fbdesign_gentemp",
                                                                      label = "Load parental study template",
                                                                      value = FALSE, status = "danger"),


                                         conditionalPanel(
                                           condition = "input.select_import == 'Template'",

                                           conditionalPanel(
                                             condition = "input.fbdesign_gentemp",
                                             downloadButton(outputId = "fbDesign_mlistExportGenTemp", label = "Download parental template")

                                           ),

                                           conditionalPanel(
                                             condition = "!input.fbdesign_gentemp",
                                             #downloadButton(outputId = "fbDesign_mlistExportGenTemp", label = "Download Template 2")
                                             downloadButton(outputId = "fbDesign_mlistExport", label = "Download template")
                                           ),




                                          fileInput(inputId = 'file_mtlist',label =  'Upload filled template',accept = ".xlsx")#,
                                          ),


                                          fluidRow(
                                            #br(),
                                            #shiny::actionLink('exportButton', 'Material List Template'),
                                            infoBoxOutput("approvalBox")#,

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

                           shiny::tabPanel("Statistical Design", value = "design", icon = shiny::icon("pie-chart"),

                                            conditionalPanel( condition = "output.condition_selmlist==0",

                                                             br(),
                                                             shiny::selectInput("designFieldbook", "Design",  c("Choose one" = "", design_choices), selected = 'RCBD',
                                                                                multiple = FALSE),
                                                             design_conditional_panels()
                                                      ),


                                      conditionalPanel( condition = "output.condition_selmlist!=0",
                                           br(),
                                           #shiny::selectInput("design_geneticFieldbook", "Genetic design",  c("Choose one" = "", genetic_design_choices) ,multiple = FALSE),
                                           shiny::selectInput("design_geneticFieldbook", "Genetic design",  c(genetic_design_choices) ,multiple = FALSE),
                                           genetic_design_conditional_panels()

                                      )

                           ),

                             shiny::tabPanel("Environment", value = 'environment', icon = shiny::icon("recycle"),
                                          br(),

                                             #shiny::checkboxInput("designFieldbook_zigzag", "Zigzag", FALSE),
                                             shiny::radioButtons("designFieldbook_serie", "Label series",
                                                                 #get_series_labels(), "101, 102, ...", #get_series_labels()[[2]],
                                                                 1:3, 1,
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

                                             shiny::conditionalPanel(
                                                 "input.fbDesign_environment_type == 'Field'",


                                             shiny::numericInput("fbDesign_nplantsrow",
                                                                 "Number of plants per row", 10, 1, 100),

                                             shiny::numericInput("fbDesign_nrowplot",
                                                                 "Number of rows per plot", 1, 1, 100),

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

                                             #,

                           ),

                           shiny::conditionalPanel(

                             "input.fbDesign_environment_type == 'Field'",

                             # Deprecated code for number of plant per plot
                             # shiny::numericInput("fbDesign_nplants",
                             #                     "Number of plants per plot/pot", 10 , 1, 100)#,

                             #It replace the code above.
                             shiny::uiOutput("fbPlant_plot", inline = TRUE)
                             #

                           ),

                           shiny::conditionalPanel(
                                          "input.fbDesign_environment_type == 'Greenhouse'|
                                           input.fbDesign_environment_type == 'Screenhouse'",

                                          shiny::numericInput("fbDesign_nplantxpot",
                                                              "Number of plants per pot", 10 , 1, 10000),

                                          shiny::numericInput("fbDesign_npots",
                                                              "Number of pots", 1, 1, 10000)#,
                                        )

                                  )#,

                          )#,
                          #shinyBS::bsAlert("alert_fb_done")
                    )
              ),



                #fluidRow( #begin fluidrow


               shinyWidgets::prettyRadioButtons(inputId = "fbdesign_book_export",
                                                   label = "Choose exportation:", choices = c("HIDAP format","FieldBookApp format"), icon = icon("check"),
                                                   bigger = TRUE, status = "info",inline = TRUE,
                                                   animation = "jelly"),
                #),

                shiny::conditionalPanel(
                           "input.fbdesign_book_export == 'HIDAP format'|
                           input.fbdesign_book_export == 'FieldBookApp format'",
                           HTML('<div style="float: right; margin: 0 16px 18px 0px;">'),
                           shiny::actionButton("fbDesign_draft", "Book Preview", icon("table"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           HTML('</div>')#,
                ),

                  #HIDAP format
                  shiny::conditionalPanel(
                    "input.fbdesign_book_export == 'HIDAP format'",
                   shinysky::actionButton2("fbDesign_create", label = "Download", icon ="file-excel-o", icon.library = "bootstrap", styleclass= "color: #fff; background-color: #51a351; border-color: #51a351")
                  ),


                  shiny::conditionalPanel(
                    "input.fbdesign_book_export == 'FieldBookApp format'",

                    div(style="display: inline-block;vertical-align:top; width: 150px;", shiny::selectInput("fbdesign_year_fbapp",
                                                                                                            "Select year", choices = 2000:3000, selected = 2018)),


                    # div(style="display: inline-block;vertical-align:top; width: 150px;",  shiny::selectInput("fbdesign_cntry_fbapp",
                    #                                                                                          label="Select Country", choices =  value = 0.5)),

                    div(style="display: inline-block;vertical-align:top; width: 200px;", uiOutput("oufbDesign_country_fbapp")),

                    div(style="display: inline-block;vertical-align:top; width: 150px;", shiny::textInput('fbdesign_abbruser_fbapp',
                                                                                                          label = "Enter trial abbreviation",
                                                                                                          value = "", placeholder = "ex.:STO (storage)")),

                    div(style="display: inline-block;vertical-align:top; width: 200px;", uiOutput("oufbDesign_location_fbapp"),
                        shiny::downloadButton(outputId = 'fbdesigin_downloadFbAppData', label = 'Download FieldBookApp File')
                        )
                  ),

br(),


                  shinysky::shinyalert("alert_fb_done", FALSE, auto.close.after = 4),



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

          ), #end

 ui_design_big(type="tab",title="Special Modules",name="phenotype_fieldbook_design")

######
##################  End Simple Modules

    )# End of Master tabSetPanel
 )
}


