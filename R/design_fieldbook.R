#' Generic fieldbook design function
#'
#' @param design a statistical design
#' @param trt1 vector treatment one
#' @param trt2 vector second treatment; a controlled factor
#' @param trt1_label string
#' @param trt2_label string
#' @param factor_name The name of the factor
#' @param factor_lvl1 The first level of the factor
#' @param factor_lvl2 The second level of the factor
#' @param factor_lvl3 The third level of the factor
#' @param sub_design The sub design used by Split or Strip plot design.
#' @param r number of repetitions
#' @param k number of blocks
#' @param series label series type
#' @param random to randomize or not
#' @param zigzag order plot in serpentine
#' @param first to randomize or not the first repetition
#' @param maxRep maximum number of repetitions
#' @param cont continuouse labeling
#' @param variables set of variables
#'
#' @return a dataframe
#' @export
#'
design_fieldbook <- function(design = "(RCBD)", trt1 = letters[1:5], trt2=NULL,
                             r = 2, k = 2,
                             trt1_label = "trt1",
                             trt2_label = "trt2",
                             factor_name="FACTOR",
                             factor_lvl1="level1",factor_lvl2="level2",factor_lvl3="level3",
                             sub_design="crd",
                             maxRep = 20,
                             series = 2 , random = TRUE, first = TRUE, cont = FALSE,
                             zigzag = FALSE,
                             variables = NULL){

  design = stringr::str_extract(design, "([A-Z2]{2,10})")
  # if (design == "LD" && !(length(trt1) %% r == 0 ))
  #   stop("Incorrect paramter combinations for LD design.")
  fb <- switch(design,

     UNDR = fbdesign::design.undr(trt1,r=1),
     RCBD = agricolae::design.rcbd(trt1, r, series, randomization = random, first = first), #ok
     CRD = agricolae::design.crd(trt1, r, series, randomization = random), #ok
     LSD = agricolae::design.lsd(trt1, series, randomization = random, first = first),
     SPCRD = agricolae::design.split(trt1 = trt1, trt2 = trt2, r = r, design = "crd", series,
                                    first, randomization = random, kinds = "Super-Duper"),
     SPRCBD = agricolae::design.split(trt1 = trt1, trt2 = trt2, r = r, design = "rcbd", series,
                                     first, randomization = random, kinds = "Super-Duper"),

     SPLSD = agricolae::design.split(trt1 = trt1, trt2 = trt2, r = r, design = "lsd", series,
                                      first, randomization = random, kinds = "Super-Duper"),

     STRIP = agricolae::design.strip(trt1 = trt1, trt2 = trt2, r = r, series,
                                    kinds ="Super-Duper" ,randomization =random),

     F2CRD = fbdesign::design.f2crd(trt1, trt2, r = r, series=series, random=random),
     F2RCBD = fbdesign::design.f2rcbd(trt1, trt2, r = r, series=series, random=random),

     ##! ABD design tip: trt2::genotypes & trt:: genotypes
     ABD = agricolae::design.dau(trt2, trt1, r = r, serie=series,
                                   kinds ="Super-Duper" ,randomization = random),

     GLD = agricolae::design.graeco(trt1, trt2, serie = series, randomization = random),
     YD = agricolae::design.youden(trt1, r, serie = series, first = first, randomization = random),
     LD = agricolae::design.lattice(trt1, r, serie = series, randomization = random),
     BIBD = agricolae::design.bib(trt1, k, r = NULL, serie = series, maxRep = maxRep, randomization = random,
                                      seed = 0, kinds = "Super-Duper"),
     AD = agricolae::design.alpha(trt1, k, r, serie = series, randomization = random),
     CD = agricolae::design.cyclic(trt1, k, r, serie = series, randomization = random)
     #SPPD = agricolae::design.split(trt1 = trt1, trt2 = trt2, r = r, design = sub_design, series,
     #                               first, randomization = random, kinds = "Super-Duper"),

       )
  #nc = ncol(fb$book)
  names(fb$book)[1] = "PLOT"
  #names(fb$book)[nc] = toupper(trt1_label)

  if(design == "UNDR"){
    fb$book <-  fb$book[,c(1,2,3)]
    print(fb$book)
    names(fb$book) <- c("PLOT","REP","INSTN")
  }

  if (design == "RCBD") {
    if(zigzag) fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    names(fb$book)[3] = toupper(trt1_label)
  }

  if (design == "LSD") {
    if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    fb$book = fb$book[, c(1, 2, 4)]
    names(fb$book)[3] = toupper(trt1_label)
  }

  if (design == "CRD") {
    #if(zigzag)fb$book = agricolae::zigzag(fb)
    names(fb$book)[2] = "REP"
    names(fb$book)[3] = toupper(trt1_label)
  }

  if (design == "SPCRD"){
      fb$book <-  fb$book[,c(1,3,5,4)]
      names(fb$book) <- c("PLOT","REP","FACTOR","INSTN") #column block
  }

  if (design == "SPRCBD"){
    fb$book <-  fb$book[,c(1,3,5,4)]
    names(fb$book) <- c("PLOT","REP","FACTOR","INSTN") #column block
  }

  if (design == "SPLSD"){
    fb$book <-  fb$book[,c(1,3,4,6,5)]
    names(fb$book) <- c("PLOT","REP","CBLOCK","FACTOR","INSTN") #column block
  }

  if(design == "F2CRD"){
      fb$book <-  fb$book[,c(1,2,3,4)]
      names(fb$book) <- c("PLOT","REP","FACTOR","INSTN") #column block
  }

  if(design == "F2RCBD"){
    fb$book <-  fb$book[,c(1,2,3,4)]
    names(fb$book) <- c("PLOT","REP","FACTOR","INSTN") #column block
  }

  if (design == "STRIP"){
    fb$book <-  fb$book[,c(1,2,4,3)]
    names(fb$book) <- c("PLOT","REP","FACTOR","INSTN")
  }

  if (design == "ABD"){
    fb$book <-  fb$book[,c(1,2,3)]
    names(fb$book) <- c("PLOT","REP","INSTN")
  }

  if (design == "AD") {
    fb$book = fb$book[,c(1, 5, 3, 4)]
    #     names(fb$book)[2] = "REP"
    #     names(fb$book)[3] = "BLOCK"
    #     names(fb$book)[4] = toupper(trt1_label)
    names(fb$book) <- c("PLOT","REP","BLOCK","INSTN")
  }


#   if (design == "GLD") {
#     if(zigzag)fb$book = agricolae::zigzag(fb)
#     names(fb$book)[2] = "REP"
#     fb$book = fb$book[, c(1, 2, 4, 5)]
#     names(fb$book)[3] = toupper(trt1_label)
#     names(fb$book)[4] = toupper(trt2_label)
#   }

#   if (design == "YD") {
#     names(fb$book)[2] = "REP"
#     names(fb$book)[3] = "BLOCK"
#     names(fb$book)[3] = toupper(trt1_label)
#   }
#
#   if (design == "BIBD") {
#     if(zigzag)fb$book = agricolae::zigzag(fb)
#     names(fb$book)[2] = "BLOCK"
#     names(fb$book)[3] = toupper(trt1_label)
#   }

#   if (design == "CD") {
#     if(zigzag)fb$book = agricolae::zigzag(fb)
#     names(fb$book)[2] = "REP"
#     names(fb$book)[3] = "BLOCK"
#     names(fb$book)[4] = toupper(trt1_label)
#     sk = list()
#     ns = length(fb$sketch)
#     for(i in 1:ns) sk[[i]] = fb$sketch[[i]]
#     fb$sketch = sk
#   }

##! Deprecated code
  #   if (design == "SPPD") {
  #     #if(zigzag)fb$book = agricolae::zigzag(fb)
  #     #names(fb$book)[2] = "REP"
  #     #names(fb$book)[3] = toupper(trt1_label)
  #
  # #     if(sub_design == "crd"){
  # #       fb$book <-  fb$book[,c(1,3,5,4)]
  # #       names(fb$book) <- c("PLOT","REP","FACTOR","GENOTYPE") #column block
  # #     }
  #     if(sub_design == "rcbd"){
  #       fb$book <-  fb$book[,c(1,3,5,4)]
  #       names(fb$book) <- c("PLOT","REP","FACTOR","GENOTYPE") #column block
  #     }
  #     if(sub_design == "lsd"){
  #       fb$book <-  fb$book[,c(1,3,4,6,5)]
  #       names(fb$book) <- c("PLOT","REP","CBLOCK","FACTOR","GENOTYPE") #column block
  #     }
  #
  #   }


  out = fb$book
  print(out)
  # Adding variables
  if(!is.null(variables)){
    mm = matrix(nrow = nrow(out), ncol = length(variables) )
    nm = c(names(out), variables)
    out = cbind(out, mm)
    names(out) = nm
  }

  # Adding meta data


  attr(out, "params") = fb$parameters
  attr(out, "sketch") = fb$sketch
  attr(out, "statistics") = fb$statistics

  out
}

