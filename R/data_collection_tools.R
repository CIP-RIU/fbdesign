#'Organoleptic forms to collect sensorial data in participatory varietal selection of potato and sweetpotato trials.
#'
#' @param form_template Data frame containing the form template to collect sensorial data.
#' @param geno The genotypes.
#' @param geno_alias The alias of genotypes.
#' @param n_forms Number of forms. By default is 1.
#'
#' @description On farm methodologies requiere short and large organoleptic screenings where agricultors give sensorial scores
#' (taste, apparearence and texture) #' for selecting best clones or genotypes. In order to achive, this function
#' @return A data frame with the organoleptic form
#' @export

#be careful with melt function in mefa package. It masks melt function from data.table

organoleptic_form <- function(form_template, genotypes, geno_alias, n_forms = 1){

   geno_alias <- NULL

   if(is.null(genotypes)) org_form <- form_template

   matrix_form <- matrix(nrow = nrow(form_template), ncol = length(genotypes) )
   matrix_form_names <- c(names(form_template), genotypes)
   org_form <-  cbind(form_template, matrix_form)
   names(org_form) <-  matrix_form_names

   if(n_forms>1){
     #empty df will bind org_form to create a separation  between organoleptic forms
     #empty_df <- matrix(nrow = 5,ncol = length(org_form))
     #empty_df[5,] <- colnames(org_form)
     #colnames(empty_df) <-  matrix_form_names
     #colnames(empty_df) <- colnames(org_form)
     #org_form <- rbind(org_form, empty_df)
     org_form <- do.call("rbind", replicate(n_forms, org_form , simplify = FALSE))
     #org_form <- org_form[-nrow(org_form),]
   }

   org_form
}



