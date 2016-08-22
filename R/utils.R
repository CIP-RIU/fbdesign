#' List of the dbf files
#' @describeIn Read the table from M.List DB an write into the computer
#'
#'
fbdesign_mtl_files <- function(){

#   #dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".dbf|.rds")
#   dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".rds")
#   lg_dbf <- length(dbf_file_list)
#
#   if(lg_dbf == 0){ gmtfiles <- "" }
#   if(lg_dbf>0)   {
#     ignore_temps <- grepl(pattern = "~\\$",x =  dbf_file_list)
#     dbf_file_list <-  dbf_file_list[!ignore_temps]
#     short_name <- basename(dbf_file_list)
#     gmtfiles <- data.frame(short_name, dbf_file_list, stringsAsFactors = FALSE)
#     names(gmtfiles) <- c("short_name","full_name")
#     gmtfiles
#   }
#
#   mtl_files <- gmtfiles
#   mtl_files

  #usando fbglobal
  path <- fbglobal::get_base_dir()
  dbf_file_list <- list.files(path, full.names = TRUE, pattern = ".rds")

  #sin fbglobal
  #dbf_file_list <- list.files(getwd(), full.names = TRUE, pattern = ".rds")

  lg_dbf <- length(dbf_file_list)

  if(lg_dbf == 0) { gmtfiles <- "" }
  if(lg_dbf>0)    {
    ignore_temps <- grepl(pattern = "~\\$",x =  dbf_file_list)
    dbf_file_list <-  dbf_file_list[!ignore_temps]
    short_name <- basename(dbf_file_list)
    gmtfiles <- data.frame(short_name, dbf_file_list, stringsAsFactors = FALSE)
    names(gmtfiles) <- c("short_name","full_name")

    out_list <- c("hot_fieldbook.rds","dspotatotrials_dpassport.rds", "dssweettrials_dpassport.rds", "potato_pedigree.rds", "sweetpotato_pedigree.rds")
    gmtfiles <- dplyr::filter(.data = gmtfiles, !(short_name %in% out_list))


    gmtfiles
  }

  mtl_files <- gmtfiles$full_name
  mtl_files

}
