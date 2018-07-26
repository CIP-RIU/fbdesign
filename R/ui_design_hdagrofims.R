shiny::HTML("<a name ='top'></a>")
# choices for statistical design input for HIDAP-AGROFIMS
listCountries <- c('Aruba','Afghanistan','Angola','Anguilla','Albania','Andorra','United Arab Emirates','Argentina','Armenia','American Samoa','Antarctica','French Southern Territories','Antigua and Barbuda','Australia','Austria','Azerbaijan','Burundi','Belgium','Benin','Bonaire','Burkina Faso','Bangladesh','Bulgaria','Bahrain','Bahamas','Bosnia and Herzegowina','Belarus','Belize','Bermuda','Bolivia','Brazil','Barbados','Brunei','Bhutan','Burma','Bouvet Island','Botswana','Byelorusian SSR (Former)','Central African Republic','Canada','Cocos (Keeling) Islands','Switzerland','Chile','China','CIPHQ','Cote dIvoire','Cameroon','Congo','Congo','Cook Islands','Colombia','Comoros','Cape Verde','Costa Rica','Czechoslovakia (Former)','Cuba','Curacao','Christmas Island (Australia)','Cayman Islands','Cyprus','Czech Republic','German Democratic Republic','Germany','Djibouti','Dominica','Denmark','Dominican Republic','Algeria','Ecuador','Egypt','Eritrea','Western Sahara','Spain','Estonia','Ethiopia','Finland','Fiji','Falkland Islands (Malvinas)','France','Faroe Islands','Micronesia','Gabon','United Kingdom','Georgia','Ghana','Gibraltar','Guinea','Guadeloupe','Gambia','Guinea-Bissau','Equatorial Guinea','Greece','Grenada','Greenland','Guatemala','French Guiana','Guam','Guyana','Hong Kong','Heard and Mc Donald Islands','Honduras','Croatia','Haiti','Hungary','Indonesia','India','British Indian Ocean Territory','Ireland','Iran','Iraq','Iceland','Israel','Italy','Jamaica','Jordan','Japan','Kazakhstan','Kenya','Kyrgyzstan','Cambodia','Kiribati','Saint Kitts and Nevis','Korea','Kuwait','Lao People s Democratic Republic','Lebanon','Liberia','Libyan Arab Jamahiriya','Saint Lucia','Liechtenstein','Sri Lanka','Lesotho','Lithuania','Luxemburg','Latvia','Macau','Saint Martin (French part)','Macedonia','Morocco','Monaco','Moldova','Madagascar','Maldives','Mexico','Marshall Islands','Mali','Malta','Myanmar','Mongolia','Northern Mariana Islands','Mozambique','Mauritania','Montserrat','Martinique','Mauritius','Malawi','Malaysia','Mayotte','Namibia','New Caledonia','Niger','Norfolk Island','Nigeria','Nicaragua','Niue','Netherlands','Norway','Nepal','Nauru','Neutral Zone (Former)','New Zealand','Oman','Pakistan','Palestine','Panama','Pitcairn Islands','Peru','Philippines','Palau','Papua New Guinea','Poland','Puerto Rico','Korea','Portugal','Paraguay','French Polynesia','Qatar','Reunion','Romania','Russian Federation','Rwanda','Saudi Arabia','Serbia and Montenegro','Scotland','Sudan','Senegal','Singapore','Saint Helena','Svalbard and Jan Mayen Islands','Solomon Islands','Sierra Leone','El Salvador','San Marino','Somalia','Saint Pierre and Miquelon','Serbia','Sao Tome e Principe','Union of Soviet Socialist Republics (Former)','Surinam','Slovakia','Slovenia','Sweden','Swaziland','Seychelles','Syrian Arab Republic','Turks and Caicos Islands','Chad','Togo','Thailand','Tajikistan','Tokelau','Turkmenistan','East Timor','Tonga','Trinidad and Tobago','Tunisia','Turkey','Tuvalu','Taiwan','Tanzania','Uganda','Ukraine','United States Misc. Pacific Islands','unknown','Uruguay','United States of America','Uzbekistan','Vatican City State','Saint Vincent and the Grenadines','Venezuela','British Virgin Islands','Virgin Islands (US)','Viet Nam','Vanuatu','Wallis and Fortuna Islands_','Samoa','Yemen','Yugoslavia (Former)','South Africa','Zaire','Zambia','Zimbabwe'
)
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
                          tags$script(
                            'Shiny.addCustomMessageHandler("focus",
                                    function(a) {
                                    document.getElementById(a).focus();
                                    });'
                          ),
                   #        tags$style(HTML("
                   #          #landLeveling_start_date .form-control {
                   #          background-color: #cce5ff;
                   #    }
                   #
                   # ")),


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
                                                           # actionButton("btTest", "test"),

                                                           disabled(textInput(inputId = "experimentId", label = "Experiment ID", value = "")),
                                                           textInput(inputId = "experimentName", label = "Experiment name", value = ""),
                                                           textInput(inputId = "experimentProjectName", label = "Experiment project name", value = ""),

                                                           shiny::dateRangeInput("fbDesign_project_time_line", "Experiment date", start = Sys.Date() - 2,
                                                                                 end = Sys.Date() + 20, startview = "year",format = "yyyy/mm/dd"),
                                                           selectizeInput("designFieldbook_typeExperiment", "Type of experiment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Controlled treatment trial","Observation trial","Varietal trial", "Demonstration trial", "Germplasm screening trial")),

                                                            textAreaInput(inputId = "experimentObj", label = "Experiment objective", value = ""),

                                                           # hr(),
                                                           br(),
                                                           h2("Institutions/Organizations/Agencies associated with experiment"),

                                                           selectizeInput("designFieldbook_fundAgencyType", "Funding agency type",multiple = TRUE, options = list(placeholder ="Select..."), choices =
                                                                            c("Government/National",
                                                                              "Agricultural Extension",
                                                                              "Advisory Services",
                                                                              "International NGO",
                                                                              "National NGO",
                                                                              "Farmers Organization",
                                                                              "Regional Organization",
                                                                              "International Organization",
                                                                              "Financing Institution",
                                                                              "Foundation",
                                                                              "Private Company",
                                                                              "Academic Institution",
                                                                              "National Research Institution",
                                                                              "International Research Center")
                                                                          ),
                                                           fluidRow(id="fl_agencies_assoc_exp"),
                                                           numericInput("numProjEntity", "Number of project management entities", min = 1, max=5, value = 1)
                                                           # textInput(inputId = "fundName", label = "Funding agency name", value = ""),
                                                    ),
                                                  column(width = 12,
                                                         fluidRow(id="fl_entities_exp")
                                                  ),
                                                  column(width = 6,


                                                           br(),

                                                           h2("Experiment leads"),

                                                           numericInput("numLeads", "Number of experiment leads", min = 1, max=5, value = 1)
                                                  ),
                                                  column(width = 12,
                                                         fluidRow(id="fl_exp_leads")
                                                  ),
                                                  #),
                                                  #fluidRow(
                                                    sidebarPanel(id="sidebar", width = 12,
                                                                 actionButton("btnNextPersonnelInfo", "Next", class = "btn-primary",style="color: #fff;", href="#top")
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
                                                               column(width=6,
                                                                      selectizeInput("personnel1Type", "Person type", multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other")),
                                                                      conditionalPanel("input.personnel1Type == 'Other'",
                                                                                       textInput(inputId = "personnel1TypeOther", label = "", value = "")
                                                                                       ),

                                                                      textInput(inputId = "person1FirstName", label = "Person, first name", value = ""),
                                                                      textInput(inputId = "person1LastName", label = "Person, last name", value = ""),
                                                                      selectizeInput("person1Affiliation", "Person, affiliation", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                                                                       c("CGIAR Center",
                                                                                         "Other"
                                                                                       )
                                                                      ),
                                                                      conditionalPanel("input.person1Affiliation == 'CGIAR Center'",
                                                                                       selectizeInput("person1Center", "Organization name", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                                                         "Africa Rice Center",	"Bioversity International",	"Center for International Forestry Research (CIFOR)",	"International Center for Agricultural Research (ICARDA)",	"International Center for Tropical Agriculture (CIAT)",	"International Crops Research Institute for the Semi-Arid (ICRISAT)",	"International Food Policy Research Institute (IFPRI)",	"International Institute of Tropical Agriculture (IITA)",	"International Livestock Research Institure (ILRI)",	"International Maize and Wheat Improvement Center (CIMMYT)",	"International Potato Center (CIP)",	"International Rice Research Institute (IRRI)",	"International Water Management Institute (IWMI)",	"World Agroforestry Centre (ICRAF)",	"WorldFish"

                                                                                       ))
                                                                                       # textInput(inputId = "leadName", label = "Experiment, lead organization name", value = "")

                                                                      ),
                                                                      conditionalPanel("input.person1Affiliation == 'Other'",
                                                                                       textInput("person1CenterOther", "", value = "")

                                                                      )
                                                                      # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                               ),
                                                               column(width=6,



                                                                      textInput(inputId = "person1Email", label = "Person email", value = ""),


                                                                      # textInput(inputId = "person1Affiliation", label = "Person, affiliation", value = ""),
                                                                      textInput(inputId = "person1ORCID", label = HTML("Person, ORCID id if available (if not, consider <a href='https://orcid.org/register' target='_blank'>registering</a>!)"), value = ""),
                                                                      selectizeInput("person1Country", label="Country in which active", multiple = TRUE,
                                                                                     choices = listCountries,
                                                                                     options = list(maxItems = 1, placeholder = 'Select one... '))
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
                                                            column(width=6,
                                                                   selectizeInput("personnel2Type", "Person type", multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other")),
                                                                   conditionalPanel("input.personnel2Type == 'Other'",
                                                                                    textInput(inputId = "personnel2TypeOther", label = "", value = "")
                                                                   ),

                                                                   textInput(inputId = "person2FirstName", label = "Person, first name", value = ""),
                                                                   textInput(inputId = "person2LastName", label = "Person, last name", value = ""),
                                                                   selectizeInput("person2Affiliation", "Person, affiliation", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                                                                    c("CGIAR Center",
                                                                                      "Other"
                                                                                    )
                                                                   ),
                                                                   conditionalPanel("input.person2Affiliation == 'CGIAR Center'",
                                                                                    selectizeInput("person2Center", "Organization name", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                                                      "Africa Rice Center",	"Bioversity International",	"Center for International Forestry Research (CIFOR)",	"International Center for Agricultural Research (ICARDA)",	"International Center for Tropical Agriculture (CIAT)",	"International Crops Research Institute for the Semi-Arid (ICRISAT)",	"International Food Policy Research Institute (IFPRI)",	"International Institute of Tropical Agriculture (IITA)",	"International Livestock Research Institure (ILRI)",	"International Maize and Wheat Improvement Center (CIMMYT)",	"International Potato Center (CIP)",	"International Rice Research Institute (IRRI)",	"International Water Management Institute (IWMI)",	"World Agroforestry Centre (ICRAF)",	"WorldFish"

                                                                                    ))
                                                                                    # textInput(inputId = "leadName", label = "Experiment, lead organization name", value = "")

                                                                   ),
                                                                   conditionalPanel("input.person2Affiliation == 'Other'",
                                                                                    textInput("person2CenterOther", "", value = "")

                                                                   )
                                                                   # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                            ),
                                                            column(width=6,




                                                                   textInput(inputId = "person2Email", label = "Person email", value = ""),

                                                                   # textInput(inputId = "person1Affiliation", label = "Person, affiliation", value = ""),
                                                                   textInput(inputId = "person2ORCID", label = HTML("Person, ORCID id if available (if not, consider <a href='https://orcid.org/register' target='_blank'>registering</a>!)"), value = ""),
                                                                   selectizeInput("person2Country", label="Country in which active", multiple = TRUE,
                                                                                  choices = listCountries,
                                                                                  options = list(maxItems = 1, placeholder = 'Select one... '))
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
                                                                           column(width=6,

                                                                                  selectizeInput("personnel3Type", "Person type", multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other")),
                                                                                  conditionalPanel("input.personnel3Type == 'Other'",
                                                                                                   textInput(inputId = "personnel3TypeOther", label = "", value = "")
                                                                                  ),

                                                                                  textInput(inputId = "person3FirstName", label = "Person, first name", value = ""),
                                                                                  textInput(inputId = "person3LastName", label = "Person, last name", value = ""),
                                                                                  selectizeInput("person3Affiliation", "Person, affiliation", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                                                                                   c("CGIAR Center",
                                                                                                     "Other"
                                                                                                   )
                                                                                  ),
                                                                                  conditionalPanel("input.person3Affiliation == 'CGIAR Center'",
                                                                                                   selectizeInput("person3Center", "Organization name", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                                                                     "Africa Rice Center",	"Bioversity International",	"Center for International Forestry Research (CIFOR)",	"International Center for Agricultural Research (ICARDA)",	"International Center for Tropical Agriculture (CIAT)",	"International Crops Research Institute for the Semi-Arid (ICRISAT)",	"International Food Policy Research Institute (IFPRI)",	"International Institute of Tropical Agriculture (IITA)",	"International Livestock Research Institure (ILRI)",	"International Maize and Wheat Improvement Center (CIMMYT)",	"International Potato Center (CIP)",	"International Rice Research Institute (IRRI)",	"International Water Management Institute (IWMI)",	"World Agroforestry Centre (ICRAF)",	"WorldFish"

                                                                                                   ))
                                                                                                   # textInput(inputId = "leadName", label = "Experiment, lead organization name", value = "")

                                                                                  ),
                                                                                  conditionalPanel("input.person3Affiliation == 'Other'",
                                                                                                   textInput("person3CenterOther", "", value = "")

                                                                                  )
                                                                                  # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                           ),
                                                                           column(width=6,


                                                                                  textInput(inputId = "person3Email", label = "Person email", value = ""),

                                                                                  # textInput(inputId = "person1Affiliation", label = "Person, affiliation", value = ""),
                                                                                  textInput(inputId = "person3ORCID", label = HTML("Person, ORCID id if available (if not, consider <a href='https://orcid.org/register' target='_blank'>registering</a>!)"), value = ""),
                                                                                  selectizeInput("person3Country", label="Country in which active", multiple = TRUE,
                                                                                                 choices = listCountries,
                                                                                                 options = list(maxItems = 1, placeholder = 'Select one... '))
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
                                                                           column(width=6,

                                                                                  selectizeInput("personnel4Type", "Person type", multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other")),
                                                                                  conditionalPanel("input.personnel4Type == 'Other'",
                                                                                                   textInput(inputId = "personnel4TypeOther", label = "", value = "")
                                                                                  ),

                                                                                  textInput(inputId = "person4FirstName", label = "Person, first name", value = ""),
                                                                                  textInput(inputId = "person4LastName", label = "Person, last name", value = ""),
                                                                                  selectizeInput("person4Affiliation", "Person, affiliation", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                                                                                   c("CGIAR Center",
                                                                                                     "Other"
                                                                                                   )
                                                                                  ),
                                                                                  conditionalPanel("input.person4Affiliation == 'CGIAR Center'",
                                                                                                   selectizeInput("person1Center", "Organization name", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                                                                     "Africa Rice Center",	"Bioversity International",	"Center for International Forestry Research (CIFOR)",	"International Center for Agricultural Research (ICARDA)",	"International Center for Tropical Agriculture (CIAT)",	"International Crops Research Institute for the Semi-Arid (ICRISAT)",	"International Food Policy Research Institute (IFPRI)",	"International Institute of Tropical Agriculture (IITA)",	"International Livestock Research Institure (ILRI)",	"International Maize and Wheat Improvement Center (CIMMYT)",	"International Potato Center (CIP)",	"International Rice Research Institute (IRRI)",	"International Water Management Institute (IWMI)",	"World Agroforestry Centre (ICRAF)",	"WorldFish"

                                                                                                   ))
                                                                                                   # textInput(inputId = "leadName", label = "Experiment, lead organization name", value = "")

                                                                                  ),
                                                                                  conditionalPanel("input.person4Affiliation == 'Other'",
                                                                                                   textInput("person4CenterOther", "", value = "")

                                                                                  )
                                                                                  # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                           ),
                                                                           column(width=6,

                                                                                  textInput(inputId = "person4Email", label = "Person email", value = ""),

                                                                                  # textInput(inputId = "person1Affiliation", label = "Person, affiliation", value = ""),
                                                                                  textInput(inputId = "person4ORCID", label = HTML("Person, ORCID id if available (if not, consider <a href='https://orcid.org/register' target='_blank'>registering</a>!)"), value = ""),
                                                                                  selectizeInput("person4Country", label="Country in which active", multiple = TRUE,
                                                                                                 choices = listCountries,
                                                                                                 options = list(maxItems = 1, placeholder = 'Select one... '))
                                                                           )
                                                                         )
                                                                       )#)

                                                    ),
                                                    conditionalPanel("input.npersons == 5",

                                                                     # fluidRow(
                                                                       box(
                                                                         title = tagList(shiny::icon("user"), "Personnel #5"), solidHeader = TRUE, status = "warning", width=12,

                                                                         fluidRow(
                                                                           column(width=6,
                                                                                  selectizeInput("personnel5Type", "Person type", multiple=TRUE, options = list(maxItems =1, placeholder= "Select one..."), choices = c("Farmer","Researcher","Student", "Research station worker", "Extension agent", "Faculty member", "Other")),
                                                                                  conditionalPanel("input.personnel5Type == 'Other'",
                                                                                                   textInput(inputId = "personnel5TypeOther", label = "", value = "")
                                                                                  ),

                                                                                  textInput(inputId = "person5FirstName", label = "Person, first name", value = ""),
                                                                                  textInput(inputId = "person5LastName", label = "Person, last name", value = ""),
                                                                                  selectizeInput("person5Affiliation", "Person, affiliation", multiple =T, options = list(maxItems =1, placeholder="Select one.."), choices=
                                                                                                   c("CGIAR Center",
                                                                                                     "Other"
                                                                                                   )
                                                                                  ),
                                                                                  conditionalPanel("input.person5Affiliation == 'CGIAR Center'",
                                                                                                   selectizeInput("person5Center", "Organization name", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c(
                                                                                                     "Africa Rice Center",	"Bioversity International",	"Center for International Forestry Research (CIFOR)",	"International Center for Agricultural Research (ICARDA)",	"International Center for Tropical Agriculture (CIAT)",	"International Crops Research Institute for the Semi-Arid (ICRISAT)",	"International Food Policy Research Institute (IFPRI)",	"International Institute of Tropical Agriculture (IITA)",	"International Livestock Research Institure (ILRI)",	"International Maize and Wheat Improvement Center (CIMMYT)",	"International Potato Center (CIP)",	"International Rice Research Institute (IRRI)",	"International Water Management Institute (IWMI)",	"World Agroforestry Centre (ICRAF)",	"WorldFish"

                                                                                                   ))
                                                                                                   # textInput(inputId = "leadName", label = "Experiment, lead organization name", value = "")

                                                                                  ),
                                                                                  conditionalPanel("input.person5Affiliation == 'Other'",
                                                                                                   textInput("person5CenterOther", "", value = "")

                                                                                  )
                                                                                  # actionButton(inputId = "addAnotherPerson",label="Add another")
                                                                           ),
                                                                           column(width=6,
                                                                                  textInput(inputId = "person5Email", label = "Person email", value = ""),

                                                                                  # textInput(inputId = "person1Affiliation", label = "Person, affiliation", value = ""),
                                                                                  textInput(inputId = "person5ORCID", label = HTML("Person, ORCID id if available (if not, consider <a href='https://orcid.org/register' target='_blank'>registering</a>!)"), value = ""),
                                                                                  selectizeInput("person5Country", label="Country in which active", multiple = TRUE,
                                                                                                 choices = listCountries,
                                                                                                 options = list(maxItems = 1, placeholder = 'Select one... '))
                                                                           )
                                                                         )
                                                                       )
                                                                     #)

                                                    ),


                                                  sidebarPanel(id="sidebar", width = 12,
                                                               actionButton("btnNextSite", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                                  )

                                              ),

                                              tabPanel("Site", value="tabSite",  icon = shiny::icon("location-arrow"),
                                                       #fluidRow(
                                                         column(width = 6,
                                                                h2("Site information"),

                                                                uiOutput("uiTest"),

                                                                shiny::uiOutput("fbDesign_country", inline = TRUE, width = 500),
                                                                shiny::uiOutput("fbDesign_countrySite", inline = TRUE, width = 500), #,#locality
                                                                actionButton("refreshSiteList", "Refresh sites"),
                                                                br(),
                                                                h2("Site surrounding description"),
                                                                selectizeInput("fbDesign_inHighLevel", label="Higher-level landform", multiple = TRUE,
                                                                               choices = c("Plain",	"Basin",	"Valley",	"Plateau","Upland",	"Hill",	"Mountain"),
                                                                               options = list(maxItems = 1, placeholder = 'Select  one...')),
                                                                selectizeInput("fbDesign_inSiteVegetation", label="Vegetation surrounding the experiment site", multiple = TRUE,
                                                                               choices = c("Grassland", "Crops", "Forest", "Woodland", "Shrubs", "Savanna", "Other"),
                                                                               options = list(maxItems = 5, placeholder = 'Select one... ')),


                                                                textAreaInput("inSiteDescNotes", label="Site description notes", value="")),
                                                       # sidebarPanel(id="sidebar", width = 12,
                                                       #              actionButton("btnNextCropInfo", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                                       # )
                                                       sidebarPanel(id="sidebar", width = 12,
                                                                    actionButton("btnNextCropInfo", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                                       )
                                                       #)


                                              ),




                                         tabPanel("Crop" , value="tabCrop", icon = shiny::icon("pagelines"),
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

                                                                                                selectizeInput("cropCommonNameMono", "Crop common name", multiple = TRUE, options = list(maxItems =1, placeholder="Select one..."), choices = c("Cassava","Common bean", "Maize","Potato", "Sweetpotato", "Soybean", "Wheat", "Other")),
                                                                                                conditionalPanel("input.cropCommonNameMono == 'Other'",
                                                                                                                 textInput("cropCommonNameMono_other", "")
                                                                                                                 )
                                                                                                # textInput(inputId = "cropLatinNameMono", label = "Crop latin name", value = ""),
                                                                                                # fluidRow(
                                                                                                #   column(width = 10,
                                                                                                #          #textInput(inputId = "cropVarietyNameMono", label = "Crop variety name", value = "")
                                                                                                #          selectizeInput("cropVarietyNameMono", "Crop variety name", c(), multiple = TRUE, options = list(
                                                                                                #            placeholder = "ex.  variety1   variety2    ",
                                                                                                #            'create' = TRUE,
                                                                                                #            'persist' = FALSE)
                                                                                                #          )#,
                                                                                                #
                                                                                                #
                                                                                                #   ),

                                                                                                  # column(width = 2, br(),
                                                                                                  #        checkboxInput("setCropFactor", "Factor", FALSE)
                                                                                                  # )
                                                                                                # )

                                                                                         ),
                                                                                         column(width = 6,
                                                                                                textInput(inputId = "cultivarNameMono", label = "Variety name", value = "")
                                                                                                # textInput(inputId = "monoCropLocalName", label = "Crop local name", value = "")
                                                                                         )
                                                                                       )
                                                                                )
                                                                              )

                                                             ),


                                                             conditionalPanel("input.croppingType == 'Intercrop'",
                                                                              fluidRow(
                                                                                column( width = 6,
                                                                                        selectizeInput("cropsSelected",label="Select crops", selected=NULL, multiple = TRUE, choices=c("Cassava","Common bean", "Maize","Potato", "Sweetpotato", "Soybean", "Wheat", "Other"))
                                                                                )
                                                                              ),

                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length > 0",
                                                                                               h2("Crop information"),

                                                                                               box(title = "Crop #1", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   fluidRow(
                                                                                                     column(width = 6,
                                                                                                            disabled(textInput(inputId = "cropCommonName1", label = "Crop common name", value = ""))
                                                                                                     ),
                                                                                                     column(width = 6,
                                                                                                            textInput(inputId = "cropVarietyName2", label = "Crop variety name", value = "")

                                                                                                     )
                                                                                                   )
                                                                                               )
                                                                              ),

                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length > 1",
                                                                                               box(title = "Crop #2", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   column(width = 6,
                                                                                                          disabled(textInput(inputId = "cropCommonName2", label = "Crop common name", value = ""))
                                                                                                          # textInput(inputId = "cropLatinName2", label = "Crop latin name", value = "")

                                                                                                   ),
                                                                                                   column(width = 6,
                                                                                                          textInput(inputId = "cropVarietyName2", label = "Crop variety name", value = "")

                                                                                                   )

                                                                                               )
                                                                              ),

                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length >2",
                                                                                               box(title = "Crop #3", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   column(width = 6,
                                                                                                          disabled(textInput(inputId = "cropCommonName3", label = "Crop common name", value = ""))
                                                                                                   ),
                                                                                                   column(width = 6,
                                                                                                          textInput(inputId = "cropVarietyName3", label = "Crop variety name", value = "")

                                                                                                   )
                                                                                               )
                                                                              ),
                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length >3",
                                                                                               box(title = "Crop #4", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   column(width = 6,
                                                                                                          disabled(textInput(inputId = "cropCommonName4", label = "Crop common name", value = ""))
                                                                                                   ),
                                                                                                   column(width = 6,
                                                                                                          textInput(inputId = "cropVarietyName4", label = "Crop variety name", value = "")

                                                                                                   )
                                                                                               )
                                                                              ),
                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length >4",
                                                                                               box(title = "Crop #5", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   column(width = 6,
                                                                                                          disabled(textInput(inputId = "cropCommonName5", label = "Crop common name", value = ""))
                                                                                                   ),
                                                                                                   column(width = 6,
                                                                                                          textInput(inputId = "cropVarietyName5", label = "Crop variety name", value = "")

                                                                                                   )
                                                                                               )
                                                                              ),
                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length >5",
                                                                                               box(title = "Crop #6", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   column(width = 6,
                                                                                                          disabled(textInput(inputId = "cropCommonName6", label = "Crop common name", value = ""))
                                                                                                   ),
                                                                                                   column(width = 6,
                                                                                                          textInput(inputId = "cropVarietyName6", label = "Crop variety name", value = "")

                                                                                                   )
                                                                                               )
                                                                              ),
                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length > 6",
                                                                                               box(title = "Crop #7", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   column(width = 6,
                                                                                                          disabled(textInput(inputId = "cropCommonName7", label = "Crop common name", value = ""))
                                                                                                   ),
                                                                                                   column(width = 6,
                                                                                                          textInput(inputId = "cropVarietyName7", label = "Crop variety name", value = "")

                                                                                                   )
                                                                                               )
                                                                              ),
                                                                              conditionalPanel("input.cropsSelected != null && input.cropsSelected.length >7",
                                                                                               box(title = "Crop #8", solidHeader = TRUE, status = "warning", width=12,
                                                                                                   column(width = 6,
                                                                                                          disabled(textInput(inputId = "cropCommonName8", label = "Crop common name", value = ""))
                                                                                                   ),
                                                                                                   column(width = 6,
                                                                                                          textInput(inputId = "cropVarietyName8", label = "Crop variety name", value = "")

                                                                                                   )
                                                                                               )
                                                                              )
                                                             ),
                                                             br(),
                                                             h2("Previous crop"),
                                                             fluidRow(
                                                               column( width = 6,
                                                                       #textInput(inputId = "numPreviousCrop", label = "Number of previous crop", value = ""),
                                                                       # numericInput(inputId = "numPreviousCrop", label = "Number of previous crop", value = "1", min = 1, max = 10),

                                                                       selectInput("prevCropName", "Previous crop name", c("Cassava","Common bean", "Maize","Potato", "Sweetpotato", "Soybean", "Wheat", "Other")
                                                                       ),
                                                                       conditionalPanel("input.prevCropName == 'Other'",
                                                                                        textInput("prevCropName_other", "")
                                                                       )
                                                                       # uiOutput("uiPreviousCrop2")
                                                               )
                                                             )

                                                      ),
                                                  sidebarPanel(id="sidebar", width = 12,
                                                               actionButton("btnDesign", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                                  )
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
                                                             h2("Information on experimental unit"),
                                                             selectizeInput("info_experiment_unit", "Information on experimental unit", multiple = T, options = list(maxItems =1, placeholder="Select one..."), choices = c("plot", "field", "pot")),
                                                             conditionalPanel("input.info_experiment_unit == 'plot'",
                                                               fluidRow(
                                                                 column(width = 3,
                                                                        textInput("expt_plot_width", label="Width", value="")
                                                                 ),
                                                                 column(width = 3,
                                                                        selectizeInput("expt_plot_width_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("m", "ft"))
                                                                 ),
                                                                 column(width = 3,
                                                                        textInput("expt_plot_length", label="Length", value="")
                                                                 ),
                                                                 column(width = 3,
                                                                        selectizeInput("expt_plot_length_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("m", "ft"))
                                                                 )
                                                               )
                                                             ),
                                                             conditionalPanel("input.info_experiment_unit == 'field'",
                                                                              fluidRow(
                                                                                column(width = 3,
                                                                                       textInput("expt_field_width", label="Width", value="")
                                                                                ),
                                                                                column(width = 3,
                                                                                       selectizeInput("expt_field_width_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("m", "km", "ft", "mi"))
                                                                                ),
                                                                                column(width = 3,
                                                                                       textInput("expt_field_length", label="Length", value="")
                                                                                ),
                                                                                column(width = 3,
                                                                                       selectizeInput("expt_field_length_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("m", "km", "ft", "mi"))
                                                                                )
                                                                              )
                                                             ),
                                                             conditionalPanel("input.info_experiment_unit == 'pot'",
                                                             fluidRow(
                                                               column(width = 3,
                                                                      textInput("pot_diameter", label="Diameter", value="")
                                                               ),
                                                               column(width = 3,
                                                                      selectizeInput("pot_diameter_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("cm", "in"))
                                                               ),
                                                               column(width = 3,
                                                                      textInput("pot_depth", label="Depth", value="")
                                                               ),
                                                               column(width = 3,
                                                                      selectizeInput("pot_depth_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("cm", "in"))
                                                               )
                                                             )),
                                                             br(),
                                                             h2("Design information"),
                                                             #Select statistical design
                                                             shiny::selectInput("designFieldbook_agrofims", "Select experimental design",  c("Choose one" = "", design_choices_agrofims), selected = 'CRD',
                                                                                multiple = FALSE),

                                                             br(),
                                                             h2("Treatment description"),

                                                             radioButtons("fullFactorialRB", "Full Factorial?", choices=c("Yes", "No"), selected = "Yes", inline = T)
                                                      ),
                                                    column(width = 12,
                                                           fluidRow( id = "fluid_treatment_description"),

                                                           fluidRow(id="fluid_factor_input")

                                                         # conditionalPanel("input.fullFactorialRB == 'Yes'",
                                                         #
                                                         #        column(width = 6,
                                                         #               shiny::selectInput("designFieldbook_agrofims_r_y", "Replications", 2:100, 2 )
                                                         #        ),
                                                         #        column(width = 6,
                                                         #              selectInput(inputId = "nfactors_hdafims_y", label = "Number of factors", choices = 1:5)
                                                         #        ),
                                                         #
                                                         #        fluidRow(id="full_factor_input")
                                                         #  ),
                                                         #
                                                         # conditionalPanel("input.fullFactorialRB == 'No'",
                                                         #
                                                         #          column(width = 4,
                                                         #              shiny::selectInput("designFieldbook_agrofims_t_n", "Number of treatment", 2:100, 1 )
                                                         #          ),
                                                         #          column(width = 4,
                                                         #                shiny::selectInput("designFieldbook_agrofims_r_n", "Number Replications", 2:100, 2 )
                                                         #          ),
                                                         #          column(width = 4,
                                                         #                selectInput(inputId = "nfactors_hdafims_n", label = "Number of factors", choices = 1:5)
                                                         #          ),
                                                         #          fluidRow(id="not_full_factor_input")
                                                         #
                                                         # )

                                                    ),


                                                      sidebarPanel(id="sidebar", width = 12,
                                                                   actionButton("btnNextAgro", "Next", class = "btn-primary",style="color: #fff;" , href="#top")
                                                      )

                                             ), #END DESIGN (STATISTICAL DESIGN)

                                             shiny::tabPanel("Field operations", value = "tabAgroFeat", icon = shiny::icon("truck"),
                                                        column(width = 6,
                                                               h2("Field operations"),
                                                               selectizeInput("selectAgroFeature", "Field operations", c(), multiple = TRUE, choices=c(
                                                                  # "Crop",
                                                                  "Land preparation",
                                                                  "Mulching and residue management",
                                                                  "Planting, transplanting",
                                                                  "Weeding",
                                                                  "Nutrient management",
                                                                  "Biofertilizer",
                                                                  "Irrigation",
                                                                  "Harvest",
                                                                  "Pest & disease"
                                                                  ),
                                                                  options = list(maxItems = 8, placeholder = "Select some...")
                                                                )
                                                        ),
                                                      # ),

                                                      br(),
                                                      column(width = 12,

                                                      br(),

                                                        tabsetPanel(id= "nutrienTabPanels",

                                                        tabPanel("Land preparation", value="tabLandPr",
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                 #br(),
                                                                 h2("Land preparation"),
                                                                 fluidRow(
                                                                 box(id="land_levelling_boxid",
                                                                     title = actionLink("land_levelling_titleId", "Land Levelling"),
                                                                     #title = "Land Levelling",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,


                                                                    fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_start_date", label ="Start date 1", format = "yyyy/mm/dd")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("landLeveling_end_date", label ="End date", format = "yyyy/dd/mm")
                                                                              )
                                                                            ),
                                                                            textInput("numPasses", value="", label = "Total number of levelling passes")#,
                                                                            #textInput("operationsOrder", value="", label = "Operations order")
                                                                          ),
                                                                     column(width = 6,
                                                                            br(),
                                                                        fluidRow(
                                                                         box(
                                                                           title = "Implement", solidHeader = TRUE, status = "warning", width=12,

                                                                             selectizeInput("land_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Disk harrow",
                                                                                                                                    "Drag bucket",
                                                                                                                                    "Harrow",
                                                                                                                                    "Laser-controlled",
                                                                                                                                    "Leveling board",
                                                                                                                                    "Tractor blade",
                                                                                                                                    "Other")
                                                                             ),
                                                                             conditionalPanel("input.land_impl_type == 'Other'",
                                                                                              textInput("land_impl_type_other", "")

                                                                             ),
                                                                             selectizeInput("land_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), label = "Traction", choices =
                                                                                                                                c("Buffalo",
                                                                                                                                  "Camel",
                                                                                                                                  "Donkey",
                                                                                                                                  "Elephant",
                                                                                                                                  "Horse",
                                                                                                                                  "Mule",
                                                                                                                                  "Ox / Bullock / Steer",
                                                                                                                                  "Human",
                                                                                                                                  "2 wheel tractor",
                                                                                                                                  "4 wheel tractor",
                                                                                                                                  "Other"
                                                                                                                                )
                                                                             ),
                                                                             conditionalPanel("input.land_traction == 'Other'",
                                                                                              textInput("contOtherTraction", "", value = "")

                                                                             )
                                                                        ))
                                                                    ))
                                                                )),
                                                                fluidRow(
                                                                 box(id="puddling_boxid",
                                                                     title = actionLink("puddling_titleId", "Puddling"),
                                                                     solidHeader = TRUE,
                                                                     status = "primary",
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                     fluidRow(
                                                                        column(width = 6,
                                                                               fluidRow(
                                                                                  column(width = 6,
                                                                                         dateInput("puddling_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                                  ),
                                                                                  column(width = 6,
                                                                                         dateInput("puddling_end_date", label ="End date", format = "yyyy/mm/dd")
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
                                                                                              selectizeInput("puddling_depth_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("cm", "mm", "ft", "in"))
                                                                                        )
                                                                                      )
                                                                             ))
                                                                         ),
                                                                         column(width = 6,
                                                                            fluidRow(
                                                                             box(
                                                                               title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                               selectizeInput("pud_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."),
                                                                                              choices = c("Disk harrow",
                                                                                                          "Drag bucket",
                                                                                                          "Harrow",
                                                                                                          "Laser-controlled",
                                                                                                          "Leveling board",
                                                                                                          "Tractor blade",
                                                                                                          "Other")
                                                                               ),
                                                                               conditionalPanel("input.pud_impl_type == 'Other'",
                                                                                                textInput("pud_impl_type_other", "")

                                                                               ),
                                                                               selectizeInput("pud_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), label = "Traction", choices =
                                                                                                c("Buffalo",
                                                                                                  "Camel",
                                                                                                  "Donkey",
                                                                                                  "Elephant",
                                                                                                  "Horse",
                                                                                                  "Mule",
                                                                                                  "Ox / Bullock / Steer",
                                                                                                  "Human",
                                                                                                  "2 wheel tractor",
                                                                                                  "4 wheel tractor",
                                                                                                  "Other"
                                                                                                )
                                                                               ),
                                                                               conditionalPanel("input.pud_traction == 'Other'",
                                                                                                textInput("contOtherTraction", "", value = "")

                                                                               )
                                                                             )
                                                                          ))
                                                                     )


                                                                 )),
                                                                fluidRow(
                                                                 box(id="tillage_boxid",
                                                                     title = actionLink("tillage_titleId", "Tillage"),
                                                                     #title = "Tillage",
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("tillage_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("tillage_end_date", label ="End date", format = "yyyy/mm/dd")
                                                                              )
                                                                            ),
                                                                            selectizeInput("till_technique",  label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                        c("Conventional tillage",
                                                                                                                          "Deep till",
                                                                                                                          "No-till",
                                                                                                                          "Reduced till",
                                                                                                                          "Ridge-till",
                                                                                                                          "Strip-till",
                                                                                                                          "Other"
                                                                                                                        )
                                                                                          ),
                                                                            conditionalPanel("input.till_technique == 'Other'",
                                                                                             textInput("till_technique", "")

                                                                            ),
                                                                            textInput("till_depth_method", value="", label = "Depth of tillage - measurement method"),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     textInput("tillage_depth", value = "", label="Tillage depth")
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectizeInput("tillage_depth_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("cm", "in", "m"))
                                                                              )

                                                                            ),
                                                                            #textInput("till_depth", value="", label = "Depth"),
                                                                            textInput("total_number_tillage_passes", value="", label = "Total number of tillage passes")

                                                                     ),
                                                                     column(width = 6,
                                                                            br(),
                                                                            fluidRow(
                                                                         box(
                                                                           title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                           selectizeInput("till_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Chisel plough",
                                                                                                                                                                                                                   "Cultivator",
                                                                                                                                                                                                                   "Disc plough",
                                                                                                                                                                                                                   "Hand-held hoe",
                                                                                                                                                                                                                   "Mouldboard/ridging plough",
                                                                                                                                                                                                                   "Paraplow",
                                                                                                                                                                                                                   "Spade plough",
                                                                                                                                                                                                                   "Subsoiler",
                                                                                                                                                                                                                   "Other")
                                                                           ),
                                                                           conditionalPanel("input.till_impl_type == 'Other'",
                                                                                            textInput("contOthertill_impl_type", "", value = "")

                                                                           ),
                                                                           selectizeInput("till_traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), label = "Traction", choices =
                                                                                            c("Buffalo",
                                                                                              "Camel",
                                                                                              "Donkey",
                                                                                              "Elephant",
                                                                                              "Horse",
                                                                                              "Mule",
                                                                                              "Ox / Bullock / Steer",
                                                                                              "Human",
                                                                                              "2 wheel tractor",
                                                                                              "4 wheel tractor",
                                                                                              "Other"
                                                                                            )
                                                                           ),
                                                                           conditionalPanel("input.till_traction == 'Other'",
                                                                                            textInput("contOthertill_traction", "", value = "")

                                                                           )
                                                                         ))
                                                                    )

                                                                  )
                                                                 )),
                                                                fluidRow(
                                                                 box(id="liming_boxid",
                                                                     title = actionLink("liming_titleId", "Liming"),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("liming_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("liming_end_date", label ="End date", format = "yyyy/mm/dd")
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
                                                                    ),
                                                                    column(width = 6,
                                                                           br(),
                                                                           fluidRow(
                                                                             box(
                                                                               title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                               selectizeInput("liming_impl_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Chisel plough",
                                                                                                                                                                                                                        "Cultivator",
                                                                                                                                                                                                                        "Disc plough",
                                                                                                                                                                                                                        "Hand-held hoe",
                                                                                                                                                                                                                        "Mouldboard/ridging plough",
                                                                                                                                                                                                                        "Paraplow",
                                                                                                                                                                                                                        "Spade plough",
                                                                                                                                                                                                                        "Subsoiler",
                                                                                                                                                                                                                        "Other")
                                                                               ),
                                                                               conditionalPanel("input.liming_impl_type == 'Other'",
                                                                                                textInput("contOtherliming_impl_type", "", value = "")

                                                                               )

                                                                             ))
                                                                    )
                                                                    )#,
                                                                 ))


                                                          )),#),
                                                        tabPanel("Mulching and residue management", value="tabMulching",
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Mulching and residue management"),
                                                                 #br(),
                                                                 fluidRow(
                                                                 box(id="mulch_management_boxid",
                                                                     title = actionLink("mulch_management_titleId", "Mulch management"),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     dateInput("mulch_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("mulch_end_date", label ="End date", format = "yyyy/mm/dd")
                                                                              )
                                                                            ),
                                                                            selectizeInput("mulch_type", label = "Type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices = c("Bark chips",
                                                                                                                                                                                                                 "Cacao husk",
                                                                                                                                                                                                                 "Compost",
                                                                                                                                                                                                                 "Foil (Landscape fabric)",
                                                                                                                                                                                                                 "Grass clippings",
                                                                                                                                                                                                                 "Gravel",
                                                                                                                                                                                                                 "Leaves",
                                                                                                                                                                                                                 "Paper",
                                                                                                                                                                                                                 "Pine needles (pine straw)",
                                                                                                                                                                                                                 "Plastic",
                                                                                                                                                                                                                 "Saw dust",
                                                                                                                                                                                                                 "Straw",
                                                                                                                                                                                                                 "Wood chips",
                                                                                                                                                                                                                 "Other")

                                                                            ),
                                                                            conditionalPanel("input.mulch_type == 'Other'",
                                                                                             textInput("mulch_type_other", "", value = "")

                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                textInput("mulch_thickness", value="", label = "Mulch thickness")
                                                                              ),
                                                                              column(width = 6,
                                                                                selectizeInput("mulch_thickness_unit","Unit", multiple=T, options=list(maxItems=1, placeholder="Selecte one..."),
                                                                                               choices = c("cm", "in", "m"))
                                                                              )
                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,

                                                                                  textInput("mulch_amountPerSq", value="", label = "Amount per sq. m")
                                                                              ),
                                                                              column(width = 6,
                                                                                selectizeInput("mulch_amountPerSq_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Selecte one..."),
                                                                                               choices = c("g", "kg"))
                                                                              )
                                                                            ),
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
                                                                                     dateInput("mulch_remove_start_date", label ="Mulch removal start date", format = "yyyy/mm/dd")
                                                                              ),
                                                                              column(width = 6,
                                                                                     dateInput("mulch_remove_end_date", label ="Mulch removal end date", format = "yyyy/mm/dd")
                                                                              )
                                                                            )
                                                                          ),
                                                                     column(width = 6,
                                                                            br(),
                                                                      fluidRow(
                                                                       box(
                                                                         title = "Implement", solidHeader = TRUE, status = "warning", width=12,
                                                                         # textInput("mulch_make", value="", label = "Implement make"),
                                                                         # textInput("mulch_model", value="", label = "Implement model"),
                                                                                selectizeInput("mulch_traction", label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                              c("Buffalo",
                                                                                                "Camel",
                                                                                                "Donkey",
                                                                                                "Elephant",
                                                                                                "Horse",
                                                                                                "Mule",
                                                                                                "Ox / Bullock / Steer",
                                                                                                "Human",
                                                                                                "2 wheel tractor",
                                                                                                "4 wheel tractor",
                                                                                                "Other"
                                                                                              )
                                                                                ),
                                                                                 conditionalPanel("input.mulch_traction == 'Other'",
                                                                                                  textInput("mulch_traction_other", "", value = "")

                                                                                 )
                                                                       ))
                                                                   ))


                                                                 )),
                                                                 fluidRow(
                                                                 box(id="residue_management_boxid",
                                                                     title = actionLink("residue_management_titleId", "Residue management"),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE,  collapsed = TRUE,
                                                                     fluidRow(
                                                                     column(width = 6,
                                                                            fluidRow( ##IMPLEMENTAR EN EL EXCEL
                                                                              column(width = 6,
                                                                                     dateInput("residue_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                              ),
                                                                              column(width = 6,##IMPLEMENTAR EN EL EXCEL
                                                                                     dateInput("residure_end_date", label ="End date", format = "yyyy/mm/dd")
                                                                              )
                                                                            ),
                                                                            selectizeInput("residue_cropType", label = "Crop type", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                             c("Husk",
                                                                                               "Roots",
                                                                                               "Seed Pod - Cob - Fruit",
                                                                                               "Stem/Leaf",
                                                                                               "Stubble",
                                                                                               "Other")
                                                                            ),
                                                                            conditionalPanel("input.residue_cropType == 'Other'",
                                                                                             textInput("residue_cropType_other", "", value = "")

                                                                            ),

                                                                            selectizeInput("residue_technique", label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                             c("Burning of previous crop residue",
                                                                                               "No-till, previous crop residue",
                                                                                               "No-till, spreading of residue",
                                                                                               "Tillage, previous crop residue",
                                                                                               "Tillage, spreading of residue"
                                                                                             )
                                                                            ),
                                                                            selectizeInput("residue_traction", label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                             c("Buffalo",
                                                                                               "Camel",
                                                                                               "Donkey",
                                                                                               "Elephant",
                                                                                               "Horse",
                                                                                               "Mule",
                                                                                               "Ox / Bullock / Steer",
                                                                                               "Human",
                                                                                               "2 wheel tractor",
                                                                                               "4 wheel tractor",
                                                                                               "Other"
                                                                                             )
                                                                            ),
                                                                            conditionalPanel("input.residue_traction == 'Other'",
                                                                                             textInput("residue_traction_other", "", value = "")

                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,

                                                                                     textInput("crop_residue_thick", value="", label = "Crop residue thickness")
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectizeInput("crop_residue_thick_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Selecte one..."),
                                                                                                    choices = c("cm", "in", "m"))
                                                                              )
                                                                            ),
                                                                            fluidRow(
                                                                              column(width = 6,

                                                                                     textInput("crop_residue_amount_sqm", value="", label = "Crop residue amount per sq. m")
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectizeInput("crop_residue_amount_sqm_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Selecte one..."),
                                                                                                    choices = c("g", "kg"))
                                                                              )
                                                                            ),
                                                                            textInput("crop_residue_perc_cov","Crop residue percentage of coverage", value=""),
                                                                            textInput("residue_inc_depth", "Residue incorporation depth", value=""),
                                                                            selectizeInput("above_ground_res_moisture", "Above ground residue moisture", multiple=T, options = list(maxItems=1, placeholder="Select one..."),
                                                                                           choices = c("Dry", "Moist", "Wet")),
                                                                            fluidRow(
                                                                              column(width = 6,

                                                                                     textInput("above_ground_res_amount", value="", label = "Above ground residue (amount)")
                                                                              ),
                                                                              column(width = 6,
                                                                                     selectizeInput("above_ground_res_amount_unit", "Unit",multiple=T, options=list(maxItems=1, placeholder="Selecte one..."),
                                                                                                    choices = c("g", "kg"))
                                                                              )
                                                                            )

                                                                        )
                                                                     )
                                                                 )) #end box residue
                                                        )),#),#end tab mulching
                                                        tabPanel("Planting, transplanting", value="tabPlanting",
                                                                 #br(),
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Planting, transplanting"),
                                                                 fluidRow(
                                                                       box(id="direct_seeding_boxid",
                                                                           title = actionLink("direct_seeding_titleId", "Direct seeding"),
                                                                           status = "primary",
                                                                           solidHeader = TRUE,
                                                                           width = 12, collapsible = TRUE,  collapsed = TRUE,

                                                                           fluidRow(
                                                                             column(width = 6,
                                                                                    fluidRow(
                                                                               column(width = 6,
                                                                                      dateInput("planting_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                               ),
                                                                               column(width = 6,
                                                                                      dateInput("planting_end_date", label ="End date", format = "yyyy/mm/dd")
                                                                               ))
                                                                             )
                                                                           ),
                                                                           fluidRow(

                                                                               column(width = 6,
                                                                                      fluidRow(
                                                                                      box(
                                                                                        title = "Planting, transplanting method", solidHeader = TRUE, status = "warning", width=12,

                                                                                            # textInput("planting_directSeeding", value="", label = "Direct seeding"),
                                                                                        selectizeInput("seeding_environment", label = "Seeding environment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                         c("Flat seed bed",
                                                                                                           "On hill",
                                                                                                           "On ridge")
                                                                                        ),
                                                                                            selectizeInput("seeding_technique", label = "Seeding technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                                                c("Dibbling stick",
                                                                                                                                                  "Drum seeding",
                                                                                                                                                  "Hand broadcasting",
                                                                                                                                                  "Jab planting",
                                                                                                                                                  "Line sowing by hand",
                                                                                                                                                  "Mechanical broadcasting",
                                                                                                                                                  "Other")
                                                                                            ),
                                                                                        conditionalPanel("input.seeding_technique == 'Other'",
                                                                                                         textInput("technique_name", "", value="")

                                                                                        ),
                                                                                            textInput("seed_treatment", value="", label = "Seed treatment")

                                                                                      ))
                                                                               ),
                                                                               column(width = 6,
                                                                                      fluidRow(
                                                                                        box(
                                                                                          title = "Seeding density", solidHeader = TRUE, status = "warning", width=12,
                                                                                          fluidRow(
                                                                                            column(width = 6,
                                                                                                   textInput("distance_rows",  label = "Distance between rows", value="")
                                                                                            ),
                                                                                            column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                                                                                   selectizeInput("distance_rows_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                    c("cm",
                                                                                                                      "in",
                                                                                                                      "m")
                                                                                                   )
                                                                                            )
                                                                                          ),
                                                                                          fluidRow(
                                                                                            column(width = 6,
                                                                                                   textInput("seeding_rate",  label = "Seeding rate", value="")
                                                                                            ),
                                                                                            column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                                                                                   selectizeInput("seeding_rate_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                    c("kg/ha",
                                                                                                                      "plants/hill",
                                                                                                                      "plants/pot",
                                                                                                                      "plants/row")
                                                                                                   )
                                                                                            )
                                                                                          ),
                                                                                          textInput("seeds_per_hil", "Seeds/seedlings per hill", value =""),
                                                                                          fluidRow(
                                                                                            column(width = 6,
                                                                                                   textInput("distance_plants",  label = "Distance between plants", value="")
                                                                                            ),
                                                                                            column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                                                                                   selectizeInput("distance_plants_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                                    c("cm",
                                                                                                                      "in",
                                                                                                                      "m")
                                                                                                   )
                                                                                            )
                                                                                          )

                                                                                        )
                                                                                      )
                                                                               ),
                                                                               column(width = 6,
                                                                                      fluidRow(
                                                                                        box(
                                                                                          title = "Traction", solidHeader = TRUE, status = "warning", width=12,
                                                                                          selectizeInput("seeding_traction", label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                        c("Buffalo",
                                                                                                          "Camel",
                                                                                                          "Donkey",
                                                                                                          "Elephant",
                                                                                                          "Horse",
                                                                                                          "Mule",
                                                                                                          "Ox / Bullock / Steer",
                                                                                                          "Human",
                                                                                                          "2 wheel tractor",
                                                                                                          "4 wheel tractor",
                                                                                                          "Other"
                                                                                                          )
                                                                                          ),
                                                                                          conditionalPanel("input.seeding_traction == 'Other'",
                                                                                              textInput("traction_name", "", value="")

                                                                                          )
                                                                                        )
                                                                                      )
                                                                               )
                                                                           # ),
                                                                               # fluidRow(
                                                                           )


                                                                       )
                                                                 ),
                                                                 fluidRow(
                                                                   box(id="transplanting_boxid",
                                                                       title = actionLink("transplanting_titleId", "Transplanting"),
                                                                       status = "primary",
                                                                       solidHeader = TRUE,
                                                                       width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                       fluidRow(

                                                                       column(width = 6,
                                                                              fluidRow(
                                                                                column(width = 6,
                                                                                       dateInput("transplanting_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                                ),
                                                                                column(width = 6,
                                                                                       dateInput("transplanting_end_date", label ="End date", format = "yyyy/mm/dd")
                                                                                )),
                                                                              textInput("age_seedling", value="", label = "Age of seeding (days)"),
                                                                              selectizeInput("transplanting_environment", label = "Seedling environment", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                               c("Flat seed bed",
                                                                                                 "On hill",
                                                                                                 "On ridge")
                                                                              ),
                                                                              selectizeInput("transplanting_technique", label = "Technique", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                               c("Line sowing by hand",
                                                                                                 # "Drum seeding",
                                                                                                 # "Direct seeding by machine",
                                                                                                 "Mechanical transplanting")
                                                                              ),
                                                                              textInput("transplanting_treatment", value="", label = "Seed treatment"),
                                                                              selectizeInput("trans_traction", label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                               c("Buffalo",
                                                                                                 "Camel",
                                                                                                 "Donkey",
                                                                                                 "Elephant",
                                                                                                 "Horse",
                                                                                                 "Mule",
                                                                                                 "Ox / Bullock / Steer",
                                                                                                 "Human",
                                                                                                 "2 wheel tractor",
                                                                                                 "4 wheel tractor",
                                                                                                 "Other"
                                                                                               )
                                                                              ),
                                                                              conditionalPanel("input.trans_traction == 'Other'",
                                                                                               textInput("trans_traction_name", "", value="")
                                                                              )
                                                                       ),


                                                                       column(width = 6,
                                                                              fluidRow(
                                                                                box(
                                                                                  title = "Tranplanting density", solidHeader = TRUE, status = "warning", width=12,
                                                                                  br(),
                                                                                  fluidRow(
                                                                                    column(width = 6,
                                                                                           textInput("trans_distance_rows",  label = "Distance between rows", value="")
                                                                                    ),
                                                                                    column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                                                                           selectizeInput("trans_distance_rows_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                            c("cm",
                                                                                                              "in",
                                                                                                              "m")
                                                                                           )
                                                                                    )
                                                                                  ),
                                                                                  fluidRow(
                                                                                    column(width = 6,
                                                                                           textInput("trans_seeding_density",  label = "Seeding density", value="")
                                                                                    ),
                                                                                    column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                                                                           selectizeInput("trans_seeding_density_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                            c("plants/hill",
                                                                                                              "plants/pot",
                                                                                                              "plants/sq m",
                                                                                                              "plants/row")
                                                                                           )
                                                                                    )
                                                                                  ),
                                                                                  textInput("trans_num_rows", "Number of rows", value =""),
                                                                                  fluidRow(
                                                                                    column(width = 6,
                                                                                           textInput("trans_distance_plants",  label = "Distance between plants", value="")
                                                                                    ),
                                                                                    column(width = 6, ##IMPLENTAR EN EXCEL o concatenar
                                                                                           selectizeInput("trans_distance_plants_unit", label = "Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                            c("cm",
                                                                                                              "in",
                                                                                                              "m")
                                                                                           )
                                                                                    )
                                                                                  )

                                                                                )
                                                                              )


                                                                       )
                                                                       ) #end fluidrow,


                                                                   ) #end box sowing
                                                                 )
                                                        )),#),#end tab planting


                                                        tabPanel("Weeding", value="tabWeeding",

                                                               column(width = 12,
                                                                  h2("Weeding"),
                                                                    fluidRow(
                                                                             column(width = 6,
                                                                                    numericInput("numWeeding", label  = "Weeding number", value = 1, min = 1, max = 10)

                                                                             )
                                                                    ),
                                                                  fluidRow(id = "weeding_description")

                                                        )),#),#end tab weeding


                                                        tabPanel("Irrigation", value="tabIrrigation",
                                                                 #br(),
                                                                 #fluidRow(
                                                                   column(width = 12,
                                                                          #br(),
                                                                          h2("Irrigation"),
                                                                 fluidRow(
                                                                 box(id="irrigation_desc_boxid",
                                                                     title = actionLink("irrigation_desc_titleId", "Irrigation description"),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(id = "irrig_description",
                                                                     column(width = 6,
                                                                            numericInput("numApplicationsIrrigation", label  = "Number of irrigations", value = 3, min = 3, max = 10)

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
                                                                 box(id="desc_biofertilizer_boxid",
                                                                     title = actionLink("desc_biofertilizer_titleId", "Description Biofertilizer"),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                     fluidRow(id = "bio_description",
                                                                       column(width = 6,
                                                                              numericInput("numApplicationsBiofert", label  = "Number of applications", value = 1, min = 1, max = 5)

                                                                       )
                                                                     )
                                                                 ))#end box description biofertilizer
                                                        )),#),#end tab biofertilizer
                                                        tabPanel("Pest & disease", value="tabPestNDisease",

                                                            column(width = 12,
                                                                h2("Pest & disease"),
                                                                 box(id="pest_control_boxid",
                                                                     title = actionLink("pest_control_titleId", "Pest control"),
                                                                     status = "primary",
                                                                     solidHeader = TRUE,
                                                                     width = 12, collapsible = TRUE, collapsed = TRUE,

                                                                     fluidRow(id ="pestNDisease_fluid",
                                                                       column(width = 6,
                                                                              numericInput("numApplicationsPestDisease", label  = "Number of applications", value = 1, min = 1, max = 5)
                                                                      )

                                                                    )
                                                                  )#end box pest control
                                                        )
                                                      ),#end tab pest&disease
                                                      tabPanel("Nutrient management", value="tabNutrient",
                                                               #fluidRow(
                                                                 column(width = 12,
                                                                        #br(),
                                                                        h2("Nutrient management"),
                                                                        #fluidRow(
                                                                          ## here goes the nutrients prototype panel

                                                                        fluidRow(id="typeFertilizerUsed",
                                                                          column(width = 6,
                                                                                 selectizeInput("appfTypeFertilizer", "Type of fertilizer used", multiple = TRUE, options = list(placeholder = "Select one..."),
                                                                                                choices=c("Inorganic", "Organic", "Green manure")
                                                                                 )
                                                                          )
                                                                        ),
                                                                        fluidRow(id="fert123")


                                                                        #)
                                                                 )),#)#end tab nutrient management event
                                                      tabPanel("Harvest", value="tabHarvest",
                                                               #br(),
                                                               #fluidRow(
                                                               column(width = 12,
                                                                      #br(),
                                                                      h2("Harvest"),
                                                                      fluidRow(
                                                                        box(id="desc_harvest_boxid",
                                                                            title = actionLink("desc_harvest_titleId", "Description Harvest"),
                                                                            status = "primary",
                                                                            solidHeader = TRUE,
                                                                            width = 12, collapsible = TRUE, collapsed = TRUE,
                                                                            fluidRow(
                                                                              column(width = 6,
                                                                                     fluidRow(
                                                                                       column(width = 6,
                                                                                              dateInput("harvest_start_date", label ="Start date", format = "yyyy/mm/dd")
                                                                                       ),
                                                                                       column(width = 6,
                                                                                              dateInput("harvest_end_date", label ="End date", format = "yyyy/mm/dd")
                                                                                       )
                                                                                     ),
                                                                                     textInput("harvest_cut_height", "Harvest cut height"),
                                                                                     selectizeInput("crop_component_harvested", label = "Crop component harvested", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                      c("Canopy",
                                                                                                        "Fruit",
                                                                                                        "Leaves",
                                                                                                        "Seed",
                                                                                                        "Tuber")
                                                                                     ),
                                                                                     textInput("num_rows_harvested", "Number of rows harvested"),
                                                                                     fluidRow(
                                                                                       column(width = 6,
                                                                                              textInput("len_row_harvested", value = "", label="Length of rows harvested")
                                                                                       ),
                                                                                       column(width = 6,#IMPLEMENTAR EN EXCEL
                                                                                              selectizeInput("len_row_harvested_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("m", "in", "ft"))
                                                                                       )
                                                                                     ),
                                                                                     fluidRow(
                                                                                       column(width = 6,
                                                                                              textInput("space_rows_harvested", value = "", label="Space between rows harvested")
                                                                                       ),
                                                                                       column(width = 6,#IMPLEMENTAR EN EXCEL
                                                                                              selectizeInput("space_rows_harvested_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("m", "in", "ft"))
                                                                                       )
                                                                                     ),
                                                                                     fluidRow(
                                                                                       column(width = 6,
                                                                                              textInput("area_harvested", value = "", label="Area harvested")
                                                                                       ),
                                                                                       column(width = 6,#IMPLEMENTAR EN EXCEL
                                                                                              selectizeInput("area_harvested_unit", label="Unit", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices=c("sq m", "sq in", "sq ft", "ha", "ac"))
                                                                                       )
                                                                                     ),
                                                                                     textInput("num_plants_area_harvested", "Number of plants in area harvested")

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
                                                                                                            "Manual harvesting",
                                                                                                            "Mower",
                                                                                                            "Sickle",
                                                                                                            "Simple treadle thresher",
                                                                                                            "Threshing rack",
                                                                                                            "Digger",
                                                                                                            "Reaper",
                                                                                                            "Other")
                                                                                         ),
                                                                                         conditionalPanel("input.harvest_implement == 'Other'",
                                                                                                          textInput("harvest_implement_other", "")
                                                                                          ),
                                                                                         textInput("harvest_make", value="", label = "Implement make"),
                                                                                         textInput("harvest_model", value="", label = "Implement model"),
                                                                                         selectizeInput("harvest_traction", label = "Traction", multiple = TRUE, options = list(maxItems =1, placeholder ="Select one..."), choices =
                                                                                                          c("Buffalo",
                                                                                                            "Camel",
                                                                                                            "Donkey",
                                                                                                            "Elephant",
                                                                                                            "Horse",
                                                                                                            "Mule",
                                                                                                            "Ox / Bullock / Steer",
                                                                                                            "Human",
                                                                                                            "2 wheel tractor",
                                                                                                            "4 wheel tractor",
                                                                                                            "Other"
                                                                                                          )
                                                                                         ),
                                                                                         conditionalPanel("input.harvest_traction == 'Other'",
                                                                                                          textInput("harvest_traction_other", ""))
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
                                                                   actionButton("btnNextCropPheno", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                                      )
                                                    #)



                                             ),

                                         shiny::tabPanel("Crop phenology", value = "tabCropPheno", icon = shiny::icon("envira"),

                                                         br(),
                                                         box( title ="Sowing and emergence", solidHeader = TRUE, status = "warning", width=12,
                                                              fluidRow(
                                                                column(6,
                                                                    fluidRow(
                                                                       column(6,
                                                                              dateInput("cropPheno_sowing_date", label ="Sowing date", format = "yyyy/mm/dd")
                                                                       ),
                                                                       column(6,
                                                                              dateInput("cropPheno_emergence_date", label ="Emergence date", format = "yyyy/mm/dd")
                                                                       )
                                                                    )

                                                                ),

                                                                column(6, textAreaInput("cropPheno_sowEmerg_notes", "Notes", width = '100%'))
                                                              )

                                                          ),

                                                         box( title ="Flowering", solidHeader = TRUE, status = "warning", width=12,
                                                              fluidRow(
                                                                column(6,
                                                                       fluidRow(
                                                                         column(4,
                                                                                dateInput("cropPheno_flowering_sdate", label ="Start date", format = "yyyy/mm/dd")
                                                                         ),
                                                                         column(4,
                                                                                dateInput("cropPheno_flowering_50date", label ="50% flowering date", format = "yyyy/mm/dd")
                                                                         ),
                                                                         column(4,
                                                                                dateInput("cropPheno_flowering_edate", label ="End date", format = "yyyy/mm/dd")
                                                                         )
                                                                       )

                                                                ),

                                                                column(6, textAreaInput("cropPheno_flowering_notes", "Notes", width = '100%'))
                                                              )

                                                         ),

                                                         box( title ="Grain filling", solidHeader = TRUE, status = "warning", width=12,
                                                              fluidRow(
                                                                column(6,
                                                                       fluidRow(
                                                                         column(6,
                                                                                dateInput("cropPheno_grainFilling_sdate", label ="Start date", format = "yyyy/mm/dd")
                                                                         ),
                                                                         column(6,
                                                                                dateInput("cropPheno_grainFilling_edate", label ="End date", format = "yyyy/mm/dd")
                                                                         )
                                                                       )

                                                                ),

                                                                column(6, textAreaInput("cropPheno_grainFilling_notes", "Notes"))
                                                              )

                                                         ),

                                                         box( title ="Fruit development", solidHeader = TRUE, status = "warning", width=12,
                                                              fluidRow(
                                                                column(6,
                                                                       fluidRow(
                                                                         column(6,
                                                                                dateInput("cropPheno_fruitDev_date", label ="Fruit development date", format = "yyyy/mm/dd")
                                                                         ),
                                                                         column(6,
                                                                                dateInput("cropPheno_fruitRip_date", label ="Fruit ripening date", format = "yyyy/mm/dd")
                                                                         )
                                                                       )

                                                                ),

                                                                column(6, textAreaInput("cropPheno_fruitDev_notes", "Notes"))
                                                              )

                                                         ),

                                                         box( title ="Maturity and development", solidHeader = TRUE, status = "warning", width=12,
                                                              fluidRow(
                                                                column(6,
                                                                       fluidRow(
                                                                         column(6,
                                                                                dateInput("cropPheno_maturity_date", label ="Maturity date", format = "yyyy/mm/dd")
                                                                         ),
                                                                         column(6,
                                                                                dateInput("cropPheno_senescence_date", label ="Senescence date", format = "yyyy/mm/dd")
                                                                         )
                                                                       )

                                                                ),

                                                                column(6, textAreaInput("cropPheno_maturity_notes", "Notes"))
                                                              )

                                                         ),
                                                         box( title ="Other phenological stage", solidHeader = TRUE, status = "warning", width=12,
                                                              fluidRow(
                                                                column(6,
                                                                       fluidRow(
                                                                         column(6,
                                                                                textInput("cropPheno_otherPheno_name", "Name")
                                                                         ),
                                                                         column(6,
                                                                                dateInput("cropPheno_otherPheno_date", label ="Date", format = "yyyy/mm/dd")
                                                                         )

                                                                       )

                                                                ),

                                                                column(6, textAreaInput("cropPheno_otherPheno_notes", "Notes"))
                                                              )

                                                         ),

                                                         sidebarPanel(id="sidebar", width = 12,
                                                                      actionButton("btnNextTraits", "Next", class = "btn-primary",style="color: #fff;", href="#top")
                                                         )

                                              ),

                                              shiny::tabPanel("Crop measurement", value = "tabTraits", icon = shiny::icon("leaf"),

                                                    column(width = 12,
                                                           h2("Crop measurement"),
                                                           br()

                                                  ),
                                                  uiOutput("uiTraitsList"),

                                                    sidebarPanel(id="sidebar", width = 12,
                                                                 actionButton("btnNextEnv", "Next", class = "btn-primary",style="color: #fff;", href="#top")
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
                                                    )
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
