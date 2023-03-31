#' Create a dataset (tibble) for an ECG indicator.
#'
#' From the ECG database, a dataset is created whose structure is
#' years by countries.
#'
#' If the indicator_code is equal to "METADATA_ECG" then information on
#' available indicators is provided as a dataframe (tibble):
#' names of indicators are contained in the variable "Worksheet name".
#'
#'
#' @param indicator_code one of the following strings:"dataset2", "demo_magec_AND_demo_pjan"
#' @param fromTime first year to be considered
#' @param toTime last year to be considered
#' @param countries a collection of strings representing countries
#'                   in the standard two letters format
#' @param type_flag if FALSE data are returned, otherwise the type of indicator
#'                  is returned; if METADATA_ECG is selected, NA is returned
#' @return  a dataset (tibble) years by countries
#'
#'
#'
#' @examples
#'
#'
#' # Extract metadata:
#' myTB1 <- extract_indicator_ECG(
#'     indicator_code = "METADATA_ECG" #Code_in_database
#'     )
#'
#' # Extract indicator "dataset2" from 2007 to 2021:
#' myTB2 <- extract_indicator_ECG(
#'     indicator_code = "dataset2", #Code_in_database
#'     fromTime=2007,
#'     toTime=2021)
#'
#'
#'
#' @export
#'
#'
extract_indicator_ECG <- function(
    indicator_code, #Code_in_database
    fromTime,
    toTime,
    countries =  convergEU_glb()$EU27$memberStates$codeMS,
    type_flag = FALSE){
  #
  # available indicators on March 2021
  indi_all_ECG <- c( "demo_magec_AND_demo_pjan","dataset2",
                     "METADATA")
  indi_type_ECG <- c("lowbest", "lowBest",NA)
  indi_minmax_ECG<- c("minimisation","minimisation",NA)
  #
  out_obj <- convergEU_glb()$tmpl_out
  myTB <- NULL
  # data available?
  if(!(indicator_code %in% indi_all_ECG)){
    out_obj$err <- "Error: data not available."
    return(out_obj)
  }
  # type of indicator
  if(type_flag){
    punta <- which(indicator_code == indi_all_ECG)
    out_obj$res <- c(indi_type_ECG[punta],
                     indi_minmax_ECG[punta])
    return(out_obj)
  }
  #
  tmpenv <- new.env()
  sourceFile1 <- system.file("ECG", "ECGdata.RData", package = "convergEU")
  tmpdataenv <- load(sourceFile1, envir=tmpenv)
  myTB <- get(indicator_code,envir = tmpenv)
  if(is.null(myTB)){
    out_obj$err <- "Error: data not included into the ECG database."
    return(out_obj)
  }
  # if metadata then exit
  if(indicator_code == "METADATA_ECG"){# nothing else to do
    out_obj$res <-  myTB
    return(out_obj)
  }
  # else it is an actual indicator
  # check time windows
  test1 <-  fromTime >= 1960
  test2 <-  toTime >  fromTime
  if ((!test1) | (!test2)) {
    out_obj$err <- "Error: wrong time window."
    return(out_obj)
  }
  # check selected countries
  namesDB <- names(myTB[-1])# which is always "time" variable
  test4 <-  sapply(countries,function(vx){vx %in% namesDB})
  if (any(!test4)) {
    out_obj$err <- "Error: at least one country not available."
    return(out_obj)
  }
  ## create final database by selecting columns
  myTB <-  myTB[c("time",countries)]
  #estrattore <- estrattore & ((myTB$time >= fromTime) & (myTB$time <= toTime))
  # do extract
  resTB <- dplyr::filter(myTB, (myTB$time >= fromTime) & (myTB$time <= toTime))
  out_obj$res <-  resTB
  return(out_obj)
}

