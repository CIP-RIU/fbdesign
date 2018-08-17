#' List of the dbf files
#' @describeIn Read the table from M.List DB an write into the computer
#'
#'
fbdesign_mtl_files <- function(){

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

    out_list <- c("hot_fieldbook.rds","dspotatotrials_dpassport.rds", "dssweettrials_dpassport.rds", "potato_pedigree.rds",
                  "sweetpotato_pedigree.rds", "table_sites.rds", "potato_db_distribution.rds")

    gmtfiles <- dplyr::filter(.data = gmtfiles, !(short_name %in% out_list))

    gmtfiles
  }

  mtl_files <- gmtfiles$full_name
  mtl_files

}



#' Detection of parent list
#' @describeIn Logical. Say TRUE if your material belongs to parental list.
#' @param mlist_name SelectInput value (commonly),
#' @author Omar Benites


is_parentList <- function(mlist_name){
  mlist <- mlist_name
  cond <- stringr::str_detect(mlist,"_parent_")
  return(cond)
}


#' Get the type of list (clonal, family or parental list) according to the file name.
#' @describeIn Character. Say \code{parent} wheter it is a parental list. Otherwise, \code{standard} whether it's a clonal or family list.
#' @param mlist SelectInput value (commonly),
#' @author Omar Benites
#' @export


get_type_list_fname <- function(mlist){

  mlist <- mlist

  cond <- stringr::str_detect(mlist,"_parent_")
  if(cond==TRUE){
    type <- "parental"

  } else {
    #clonal or family list.
    type <- "clonal"
  }
  return(type)
}


#' Detection of parent list
#' @describeIn Character. Say \code{parent} wheter it is a parental list. Otherwise, \code{standard} whether it's a clonal or family list.
#' @param type_import shiny input value. SelectInput value for type of import
#' @param ml_file_name shiny input value. SelecInput value for file names
#' @author Omar Benites
#' @export


get_mlist_file_name <- function(type_import, ml_file_name){

  if(type_import == "Template") {

    mtlist_file_name <- ml_file_name
    if(is.null(mtlist_file_name)){mtlist_file_name <- NULL}

  }

  if(type_import == "Local List"){

    mtlist_file_name <- ml_file_name
    if(is.null(mtlist_file_name) || mtlist_file_name == ""){  mtlist_file_name <- NULL  }

  }

  mlist_file_name
}

#' Type of material list (clonal, family or parental list) according to the data structure.
#' @description Logical. Say TRUE if your material belongs to parental list.
#' @param mlist list. List of attributes based on breeding material tables.
#' @author Omar Benites


get_type_list_ds <- function(mlist){

   list_names <- names(mlist)
   #Parental tables are included in parental list files. For this reason, we search in the argument of the function.

   if(is.element("parental_table", list_names)){
      # parental list.
      type <- "parental"
   } else {
      #clonal or family list.
      type <- "clonal"
   }
  return(type)

}



# The original version of this function is
# https://stackoverflow.com/questions/15956931/how-to-add-a-new-column-between-other-dataframe-columns
# We make some modification to use in the context of fbdesign/hidap
#'Append column using position.
#'@description Insert column between columns based on positions.
#'@param x data frame.
#'@param cols column to insert into data frame.
#'@param after the inserted column would be located after `(i)` position. `If \code{after=1}, the column will be located in the 2nd position.
#'@export
#'

append_col <- function(x, cols, after=length(x)) {
  x <- as.data.frame(x)
  if (is.character(after)) {
    ind <- which(colnames(x) == after)
    if (any(is.null(ind))) stop(after, "not found in colnames(x)\n")
  } else if (is.numeric(after)) {
    ind <- after
  }
  stopifnot(all(ind <= ncol(x)))
  cbind(x, cols)[, append(1:ncol(x), ncol(x) + 1:length(cols), after=ind)]
}



#' Get treatment and factor inputs from design of experiments
#'
#'@description Insert column between columns based on positions.
#'@param group group
#'@param subgroup subgroup
#'@param fct factor
#'@param dfr data frame with the inputs
#'@export
#'

getTrtInputs <- function(group, subgroup, fct, dfr){

  gp <-	  group #group
  sgp <-	subgroup #subgroup
  fct <-	fct #factor
  lblFct <- paste(gp, fct, sep = "_")

  if( !is.null(gp) ||  !is.null(sgp) || !is.null(fct) ){

    dfTrt <- dfr
    if(fct == "Start date" ){
      lvl <- dfTrt[[fct]]
    } else if( fct == "End date"){
      lvl <- dfTrt[[fct]]
    } else{
      lvl<- dfTrt[["text"]]
    }
  } else {
    lblFct <- ""
    lvl <- ""
  }
  out <- list(label = lblFct, level= lvl)

}

