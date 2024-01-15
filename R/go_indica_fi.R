#' Create an indicator fiche for a given aggregation of countries.
#'
#' An auxiliary function to compile a rmarkdown file to produce the indicator fiche in
#' html format within the output directory.
#'
#' Note that most of function arguments are passed as strings of characters
#' instead of object names. For example, if the object of a dataset
#' in the workspace is myTB, the parameter is set like  workDF='myTB'
#' instead of workDF=myTB as one may expect.
#' Furthermore, the dataset must be complete, that is without missing values.
#' Note also that Internet connection should be available when invoking the function
#' to properly rendering the results in the html file. The fiches have been tested with
#' the browsers Mozilla Firefox and Google Chrome.
#'
#' @param time_0 starting time.
#' @param time_t ending time.
#' @param timeName name of the variable containing times (years).
#' @param workDF name (string) of the dataset in the global environment containing
#'                    all countries contributing to average.
#' @param indicaT name of the  considered indicator.
#' @param indiType type of indicator "lowBest" or "highBest" (default).
#' @param seleMeasure set of measures of convergence; this is a
#'            subset of the following collection of strings: "beta","delta",
#'             "gamma","sigma"; "all" is a shortcut for the whole set.
#' @param seleAggre  selection of member states, default 'EU27' ('custom'
#'                    if not pre-coded).
#' @param x_angle axis orientation for time labels, default 45.
#' @param data_res_download  should data and results be downloaded, default FALSE.
#' @param auth  author of this report, default 'A.Student'.
#' @param dataNow date of production of this country fiche, default is
#'                 current time.
#' @param outFile name of the output file (without path), without extension.
#' @param outDir  output directory, eventually not existing (only one level allowed).
#' @param pdf_out should the output be saved as PDF file? The default is  FALSE.
#' @param workTB   a tibble containing data.
#' @param selfContained  TRUE if just one file is desired
#' @param eige_layout TRUE if the EIGE layout is desired
#' @param memStates Member States visualizations, default is set as quintiles.
#'
#'
#' @references{\url{https://www.eurofound.europa.eu/system/files/2022-04/introduction-to-the-convergeu-package-0.6.4-tutorial-v2-apr2022.pdf}}
#'
#'
#'
#' @export
#'
#'
go_indica_fi <-  function(
  time_0 = NA,
  time_t = NA,
  timeName = NA,
  workDF = NA ,
  indicaT = NA, # 'emp_20_64_MS'
  indiType = c('highBest','lowBest')[1],
  seleMeasure = "all",
  seleAggre = 'EU27',
  x_angle =  45,
  data_res_download =  FALSE,
  auth = 'A.Student',
  dataNow =  Sys.time(), #'2019/01/31',
  outFile = NA,
  outDir = NA,
  pdf_out = FALSE,
  workTB = NULL,
  selfContained = FALSE,
  eige_layout = FALSE,
  memStates = 'quintiles'# ('quintiles', 'default', 'custom')
){
  if(is.na(workDF) & (!is.null(workTB))){
    curTB <-  workTB
  }else if(!is.na(workDF) & is.null(workTB)){
    curTB <- get(workDF,envir = .GlobalEnv)
    workTB <- curTB
  }else{
    stop("Error while specifying data.")
  }
  # check if missing values are present
  if( any(!stats::complete.cases(curTB))){
    obj_out <- convergEU_glb()$tmpl_out
    obj_out$err <- paste("Error: one or more missing values (NAs) in the dataframe. ",
                    "Please perform imputation of missing values (NA) before making fiches.")
    return(obj_out)
  }
  #
  EUst <- c('EU27', 'EU28', 'EU27_2019', 'EU27', 'EU25', 'EU19', 'EU12', 'EU15')
  sourceFilecss <- system.file("extdata", "EUF.css", package = "convergEU")
  if(eige_layout){
     sourceFile1 <- system.file("extdata", "indica_fi_2_eige.Rmd", package = "convergEU")
  }else{
    if(memStates == 'quintiles' && seleAggre %in% EUst)
    {
      sourceFile1 <- system.file("extdata", "indica_fi_3.Rmd", package = "convergEU")
    }else if(memStates == 'default' && seleAggre %in% EUst)
    {
      sourceFile1 <- system.file("extdata", "indica_fi_21.Rmd", package = "convergEU")
    }else if(memStates == 'custom')
    {
      sourceFile1 <- system.file("extdata", "indica_fi_4.Rmd", package = "convergEU")
    }
  }
  sourceFile2 <- system.file("extdata", "eurofound.jpg", package = "convergEU")
  formula1 <- system.file("extdata", "beta_convergence.png", package = "convergEU")
  formula2 <- system.file("extdata", "coef_of_variation.png", package = "convergEU")
  formula3 <- system.file("extdata", "delta_convergence.png", package = "convergEU")
  formula4 <- system.file("extdata", "standard_deviation.png", package = "convergEU")
  formula5 <- system.file("extdata", "legend.png", package = "convergEU")
  sourceFilePatt1 <- system.file("extdata", "gg_patt_conv_annotated.png", package = "convergEU")
  sourceFilePatt2 <- system.file("extdata", "gg_patt_div_annotated.png", package = "convergEU")
  sourceFilePatt3 <- system.file("extdata", "gg_patt_same_annotated.png", package = "convergEU")
  sourceFile2eige <- system.file("extdata", "eige_logo-share.jpg", package = "convergEU")
  sourceFile3eige <- system.file("extdata", "white_rectangle.png", package = "convergEU")
  # conditional files
  sourceFile71 <- system.file("extdata", "indica_fi_2_sigma.Rmd", package = "convergEU")
  sourceFile72 <- system.file("extdata", "indica_fi_2_beta.Rmd", package = "convergEU")
  sourceFile73 <- system.file("extdata", "indica_fi_2_nobeta.Rmd", package = "convergEU")
  sourceFile74 <- system.file("extdata", "indica_fi_2_delta.Rmd", package = "convergEU")
  sourceFile75 <- system.file("extdata", "indica_fi_2_gamma.Rmd", package = "convergEU")
  #
  if (is.na(outFile)) {
    outFile2 <- paste0("indica-fie-",seleAggre,"-", indicaT, "-", time_0,"-",time_t, ".html")
    outFile <- paste0("indica-fi-",seleAggre,"-", indicaT, "-", time_0,"-",time_t)
  }else{
    outFile2 <- paste0(outFile,".html")
  }
  if (is.na(outDir)) {
    outDir <- file.path(getwd(),"out_dir_counvergEU")
  }
  # check
  resDE <- dir.exists(outDir)
  if (!resDE) {
    dir.create(outDir, FALSE)
  }
  # full path
  outPF <- file.path(outDir,outFile2)
  outFcss <- file.path(outDir,"EUF.css")
  if(memStates == 'quintiles' || seleAggre %in% EUst)
  {
    sourcePF1 <- file.path(outDir,"indica_fi_3.Rmd")
  }else if(memStates == 'default')
  {
    sourcePF1 <- file.path(outDir,"indica_fi_21.Rmd")
  }else if(memStates == 'custom')
  {
    sourcePF1 <- file.path(outDir,"indica_fi_4.Rmd")
  }
  sourcePF2 <- file.path(outDir,"eurofound.jpg")
  sourceF1 <- file.path(outDir,"beta_convergence.png")
  sourceF2 <- file.path(outDir,"coef_of_variation.png")
  sourceF3 <- file.path(outDir,"delta_convergence.png")
  sourceF4 <- file.path(outDir,"standard_deviation.png")
  sourceF5 <- file.path(outDir,"legend.png")
  sourcePFP1 <- file.path(outDir,"gg_patt_conv_annotated.png")
  sourcePFP2 <- file.path(outDir,"gg_patt_div_annotated.png")
  sourcePFP3 <- file.path(outDir,"gg_patt_same_annotated.png")
  sourcePF2eige   <- file.path(outDir,"eige_logo-share.jpg")
  sourcePF2eige_wt   <- file.path(outDir,"white_rectangle.png")
  #
  sourcePF71 <- file.path(outDir,"indica_fi_2_sigma.Rmd")
  sourcePF72 <- file.path(outDir,"indica_fi_2_beta.Rmd")
  sourcePF73 <- file.path(outDir,"indica_fi_2_nobeta.Rmd")
  sourcePF74 <- file.path(outDir,"indica_fi_2_delta.Rmd")
  sourcePF75 <- file.path(outDir,"indica_fi_2_gamma.Rmd")
    file.copy(from = sourceFile1,
              to = sourcePF1,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile2,
              to = sourcePF2,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = formula1,
              to = sourceF1,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = formula2,
              to = sourceF2,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = formula3,
              to = sourceF3,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = formula4,
              to = sourceF4,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = formula5,
              to = sourceF5,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFilePatt1,
              to = sourcePFP1,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFilePatt2,
              to = sourcePFP2,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFilePatt3,
              to = sourcePFP3,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile2eige,
              to = sourcePF2eige,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile3eige,
              to = sourcePF2eige_wt,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile71,
              to = sourcePF71,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile72,
              to = sourcePF72,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile73,
              to = sourcePF73,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile74,
              to = sourcePF74,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFile75,
              to = sourcePF75,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
    file.copy(from = sourceFilecss,
              to = outFcss,
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE, copy.date = FALSE);
  #
  if(selfContained){
     myOutOpt <- list(self_contained = TRUE,
                      mathjax ="default")
  }else{
    myOutOpt <- list(self_contained = FALSE,
         mathjax = 'local')
  }
  # go with rendering
  rmarkdown::render(sourcePF1,
                    params = list(
                      dataNow = dataNow,
                      workingDF = workDF,
                      time_0 = time_0,
                      time_t = time_t,
                      timeName = timeName,
                      indiType = indiType,
                      indicaT = indicaT,
                      seleMeasure = seleMeasure,
                      seleAggre = seleAggre,
                      x_angle =  x_angle,
                      data_res_download =  data_res_download,
                      auth = auth,
                      outFile = outFile,
                      outDir = outDir,
                      pdf_out = FALSE,
                      workTB = workTB,
                      memStates = memStates
                    ),
                    output_options = myOutOpt,
                    output_file = outPF,
                    encoding = "UTF-8")
  rmarkdown::pandoc_convert(outPF, output = paste0(outFile2,".pdf"),
                            options=c("-V geometry:margin=0.1in -V geometry:paperwidth=7in"))
}

