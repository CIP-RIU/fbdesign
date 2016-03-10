# #Minimal Sheet
# short name <- fbdesign_id()
# crop <- input$designFieldbook_crop
# Type of Trial <- designFieldbook_module
# Begin date <- fbDesign_project_time_line[1]
# End date <- fbDesign_project_time_line[2]
# Site short name <- designFieldbook_sites
# Country <- fbDesign_countryTrial
#
# Experimental design <- input$designFieldbook
# Genetic design	<- NULL
# Labels for factor genotypes <- "Institutional number"
# Number of repetitions or blocks <-
# Block size (applicable for BIBD only) <- NULL
# Block number <- input$designFieldbook_r
# Experimental Environment <- input$fbDesign_environment_type
# Plot start number <- 1
#
# Number of plants planted per plot <- input$fbDesign_nplants
# Number of plants per sub-plot <-
# Number of plants per row <- input$fbDesign_nplantsrow
# Number of rows per plot <- NULL
# Number of rows per sub-plot <- NULL
# Plot start number <- NULL
# Plot size (m2) <- input$fbDesign_psize
# Distance between plants (m) <- input$fbDesign_distPlants
# Distance between rows (m) <- input$fbDesign_distRows
# Planting density (plants/Ha) <- 300000

# add.vals.to.fb <- function(to, col.name, reactive_value,input_value,fb_reactive,germ_list){
#
#   wb <- openxlsx::loadWorkbook(to)
#
#   data_hidap <- readxl::read_excel(path = to,sheet = "Minimal")
#   data_hidap[data_hidap$Factor=="Short name or Title",col.name] <- paste(isolate(reactive_value))
#
#
#   print("Short Name Pass")
#
#   input_val <- input_value
#   #wb <- openxlsx::loadWorkbook(to)
#   #data_hidap <- readxl::read_excel(path = to,sheet = sheetName)
#   data_hidap[data_hidap$Factor=="Begin date",col.name] <- paste(as.character(input_val[1]))
#   data_hidap[data_hidap$Factor=="End date",col.name] <- paste(as.character(input_val[2]))
#
#   print("Dates Pass")
#
#   #geographic information
#   cntry <- input$CNTRY
#   tsites <- input$doe_trialSite %>% gsub("\\s*\\([^\\)]+\\)","",.)
#
#   #tsites <- tsites %>% stringr::str_trim(.,side = "both")
#   print(cntry)
#   print(tsites)
#   if(is.null(cntry)){return()}
#   if(is.null(tsites)){return()}
#   if(!is.null(cntry) && !is.null(tsites)){
#     print(data_sites())
#     geodata <- filter_geodata(data_sites = data_sites(),country_input = cntry,trail_site = tsites)
#     print(geodata)
#     data_hidap[data_hidap$Factor=="Site short name",col.name] <- paste(as.character(geodata$SHORTN))
#     data_hidap[data_hidap$Factor=="Agroecological zone",col.name] <- paste(as.character(geodata$AEZ))
#     data_hidap[data_hidap$Factor=="CIP Region",col.name] <- paste(as.character(geodata$CREG))
#     data_hidap[data_hidap$Factor=="Continent",col.name] <- paste(as.character(geodata$CONT))
#     data_hidap[data_hidap$Factor=="Country",col.name] <- paste(as.character(geodata$CNTRY))
#     data_hidap[data_hidap$Factor=="Admin1",col.name] <- paste(as.character(geodata$ADM1))
#     data_hidap[data_hidap$Factor=="Admin2",col.name] <- paste(as.character(geodata$ADM2))
#     data_hidap[data_hidap$Factor=="Admin3",col.name] <- paste(as.character(geodata$ADM3))
#     data_hidap[data_hidap$Factor=="Locality",col.name] <- paste(as.character(geodata$LOCAL))
#     data_hidap[data_hidap$Factor=="Elevation",col.name] <- paste(as.character(geodata$ELEV))
#     data_hidap[data_hidap$Factor=="Latitude",col.name] <- paste(as.character(geodata$LATD))
#     data_hidap[data_hidap$Factor=="Longitude",col.name] <- paste(as.character(geodata$LOND))
#   }##end geographic information
#
#   print("Geographic Information Pass")
#   openxlsx::writeDataTable(wb,sheet = "Minimal",x = data_hidap,colNames = TRUE,withFilter = FALSE)
#   #END MINIMAL
#   #openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
#   print("Minimal Finished")
#
#
#   #BEGIN Sheet Installation
#   expenv <- input$doe_expenv
#   stat_design <- input$design
#   if(is.null(stat_design)){return()}
#   if(!is.null(stat_design)){
#     #declare parameter from statistical des
#     data_hidap <- readxl::read_excel(path = to,sheet = "Installation")
#
#     if(stat_design=="RCBD"){
#       sdesign_name <- "Randomized Complete Block Design (RCBD)"
#       nrep <- input$rcbd_r
#     }
#     if(stat_design=="CRD"){
#       sdesign_name <- "Completely Randomized Design (CRD)"
#       nrep <- input$crd_r
#     }
#     if(stat_design=="LSD"){
#       sdesign_name <- "Latin Square Design (LSD)"
#       nrep <- ""
#     }
#     if(stat_design=="ABD"){
#       sdesign_name <- "Augmented Block Desing (ABD)"
#       nrep <- input$abd_r
#     }
#     if(stat_design=="SPPD"){
#
#       if(input$sppd_stat_design=="crd"){
#         sdesign_name <- "Split Plot with Plots in CRD (CRD)"
#         nrep <- input$sppd_r
#       }
#
#       if(input$sppd_stat_design=="rcbd"){
#         sdesign_name <- "Split Plot with Plots in RCBD (SPRCBD)"
#         nrep <- input$sppd_r
#       }
#
#       if(input$sppd_stat_design=="lsd"){
#         sdesign_name <- "Split Plot with Plots in LSD (LSD)"
#         nrep <- input$sppd_r
#       }
#
#     }
#     if(stat_design=="STPD"){
#       #sdesign_name <- "Strip Plot Design (STPD)"
#       sdesign_name <- "Strip Plot Design (STRIP)"
#       nrep <- input$stpd_r
#     }
#     if(stat_design=="BIBD"){
#       sdesign_name <- "Balanced Incomplete Block Design (BIBD)"
#       nrep <- input$bibd_r
#     }
#
#     print("Disenos Ok")
#
#     data_hidap[data_hidap$Factor=="Experimental design",col.name] <- paste(as.character(sdesign_name))
#     data_hidap[data_hidap$Factor=="Number of repetitions or blocks",col.name] <- paste(as.character(nrep))
#     #   data_hidap[data_hidap$Factor=="Experimental design",col.name] <- paste(as.character(stat_design))
#     #   data_hidap[data_hidap$Factor=="Number of repetitions or blocks",col.name] <- paste(as.character(input$rcbd_r))
#   }
#
#   if(is.null(expenv)){return()}
#   if(!is.null(expenv)){
#     data_hidap[data_hidap$Factor=="Experimental Environment",col.name] <- paste(as.character(expenv))
#   }
#   #openxlsx::writeDataTable(wb,sheet = "Installation",x = data_hidap,colNames = TRUE,withFilter = FALSE)
#   openxlsx::writeDataTable(wb,sheet = "Installation",x = data_hidap,colNames = TRUE,withFilter = FALSE)
#   #END INSTALLATION
#
#   print("Installation Pass")
#   ##Sheet Material List sheet
#   file1 <- input$doe_germ_inputfile
#   if(is.null(file1)){return()}
#   if(!is.null(file1)){
#     mat_list_sheet <- openxlsx::read.xlsx(xlsxFile = to,sheet = "Material List",colNames = TRUE,skipEmptyRows = TRUE)
#     ##
#     print(mat_list_sheet)
#     ##
#     names_mat_list <- names(mat_list_sheet)
#     ##
#     print(names_mat_list)
#     ##
#     germ_list_user<- read.csv(file = file1$datapath,header = TRUE)
#     print(germ_list_user)
#     Numeration <- 1:nrow(germ_list_user)
#     print(Numeration)
#     datos <- cbind(Numeration,germ_list_user)
#     print(datos)
#     names(datos) <- stringr::str_replace_all(string = names(mat_list_sheet),pattern = "\\.",replacement = " ")
#     print("final datos Installation")
#     print(datos)
#     openxlsx::writeDataTable(wb = wb,sheet = "Material List",x = datos,colNames = TRUE,withFilter = FALSE)
#   }
#   print("Crop Material List")
#   #Sheet Crop_management
#   rm(data_hidap)
#   data_hidap <- readxl::read_excel(path = to,sheet = "Crop_management")
#   data_hidap[data_hidap[,"Intervention type"]=="Planting","Date"] <- paste(input_val[1])
#   data_hidap[data_hidap[,"Intervention type"]=="Vine cutting / killing","Date"] <- paste(input_val[1])
#   data_hidap[data_hidap[,"Intervention type"]=="Harvest","Date"] <- paste(input_val[1])
#
#   if(input$doe_type_crop=="Potato"){
#     if(!is.null(input$vars_doe_pt)){
#       v_input <- input$vars_doe_pt
#       v_list <- var_list(crop="Potato")
#       vars <- v_list %in% v_input
#       vars <- names(v_list[vars])
#       #print(vars)
#       vars <- gsub("\\[[^\\]]*\\]: ", "", vars, perl=TRUE)
#       vars <- stringr::str_trim(vars,side = "both")
#       #print(vars)
#     }
#   }
#   if(input$doe_type_crop=="Sweetpotato"){
#     if(!is.null(input$vars_doe_sp)){vars <- input$vars_doe_sp
#     v_input <- input$vars_doe_pt
#     v_list <- var_list(crop="Sweetpotato")
#     vars <- v_list %in% v_input
#     vars <- names(v_list[vars])
#     #print(vars)
#     vars <- gsub("\\[[^\\]]*\\]: ", "", vars, perl=TRUE)
#     vars <- stringr::str_trim(vars,side = "both")
#     #print(vars)
#     }
#   }
#   n_row <- length(vars)
#   col1 <- rep("Measure",n_row)
#   #var <- ddict %>% dplyr::filter(.,ABBR %in% a)
#   col2 <- vars
#   col3 <- rep(as.character(input_val[1]),n_row)
#   col4 <- rep(NA,n_row)
#   col5 <- rep(NA,n_row)
#   col6 <- rep(NA,n_row)
#   col7 <- rep(NA,n_row)
#   col8 <- rep(NA,n_row)
#   col9 <- rep(NA,n_row)
#
#   temp_data <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9)
#   names(temp_data) <- names(data_hidap)
#   data_hidap <- rbind(data_hidap,temp_data)
#   openxlsx::writeDataTable(wb = wb,sheet = "Crop_management",x = data_hidap,colNames = TRUE,withFilter = FALSE)
#
#   print("Crop Management Pass")
#   #Sheet Fieldbook
#   openxlsx::addWorksheet(wb, "Fieldbook")
#   openxlsx::writeDataTable(wb = wb,sheet = "Fieldbook",x = isolate({fb_reactive}))
#   openxlsx::saveWorkbook(wb,file = to, overwrite = TRUE)
#   print("Fieldbook Pass")
#   shell.exec(to)
# }




#
