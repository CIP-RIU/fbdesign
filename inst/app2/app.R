library(shinyTree)
library(dplyr)
library(magrittr)
library(stringr)
library(shinyFiles)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFiles)
library(rhandsontable)
library(shinyBS)
library(openxlsx)
library(data.table)
#library(shinysky)

#new library 6/3/2016


#tabNameS <- "resource_fieldbook_design"
tabNameS <- "phenotype_fieldbook_design"

server <- function(input, output, session,values) {
  values = shiny::reactiveValues()
  fbdesign::server_design(input, output, session, values = values)
  fbdesign::server_design_big(input, output, session, values = values)
}

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Design Fieldbook"),
                    dashboardSidebar(width = 200,
                                     menuItem("Resources",
                                              sidebarMenu(id = "menu",
                                                          menuSubItem("Design Fieldbook", icon = icon("star"),
                                                                      tabName = tabNameS)
                                              )
                                     )
                    ),
                    dashboardBody(

                      tabItems(
                        fbdesign::ui_fieldbook(name = tabNameS)#$,

                      )
                    )
)

shinyApp(ui = ui, server = server)

# server <- function(input, output, session) {
#   values = shiny::reactiveValues()
#   fbdesign::server_design(input, output, session, values = values)
# }
#
# ui <- shinydashboard::dashboardPage(skin = "yellow",
#
#      shinydashboard::dashboardHeader(title = "Fieldbooks"
#      ),
#
#      shinydashboard::dashboardSidebar(width = 300,
#        shinydashboard::sidebarMenu(id = "menu",
#         shinydashboard::menuItem("Fieldbook",
#           shinydashboard::menuSubItem("New fieldbok", icon = shiny::icon("star"),
#                              tabName = "phenotype_fieldbook_design")
#           #,
#           #fbdesign::ui_fieldbook_params()
#
#         )
#        )
#
#      ),
#
#      shinydashboard::dashboardBody(
#        shinydashboard::tabItems(
#          fbdesign::ui_fieldbook()
#        )
#      )
# )
#
# shinyApp(ui = ui, server = server)

