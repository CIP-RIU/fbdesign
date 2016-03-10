#' Get values from shinyTree object
#'
#' @param tree_input_value Input values selected from shinyTree objects
#' @param crop_selected the name of the crop
#' @description This function gets all the selected values from shinyTree checkboxes
#' @export
#'

#if(is.null(input$tree)) {print("omar")}
# get_selected(tree)
get_tree_value <- function(tree_input_value,crop_selected){

  trait_selected <<- unlist(get_selected(tree_input_value))
  trait_selected <- stringr::str_replace_all(string = trait_selected,pattern = ":.*" ,replacement = "")
  trial_headers <- fbmodule::list_modules(crop=crop_selected)
  trial_headers <- str_trim(gsub("\\(.*","", trial_headers ),side = "both")
  trait_selected <- trait_selected[!is.element(el = trait_selected, set = trial_headers)]

}

