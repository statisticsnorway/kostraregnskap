

# stackoverflow questions 30357330
pkg_env_kr_data <- new.env(parent=emptyenv())


#' Return a data set
#'
#' @encoding UTF8
#'
#' @param dataset Name of data set within the kostraregnskap package 
#' @param periode To specify `periode` (see examples)
#' @param path When non-NULL the data set is read from "path/dataset.RData"
#'
#' @return The data set
#' @export
#' @importFrom utils data
#'
#' @examples
#' z  <- kr_data("balanseRegnskapDataPen")
#' z  <- kr_data("balanseRegnskapData")
#' z  <- kr_data("kostraRegnskapDataPen")
#' z  <- kr_data("kostraRegnskapData")
#'
#' aar <- 2015  # 2016 kan også velges 
#' inputdata <- kr_data("data", aar)
#' funksjonshierarki <- kr_data("funksjonshierarki", aar)
#' artshierarki <- kr_data("artshierarki", aar)
#' inputdata_saer <- kr_data("data_saer", aar)
#' artshierarki_nettinger <- kr_data("artshierarki_nettinger", aar)
#' artshierarki_nettinger_kasse <- kr_data("artshierarki_nettinger_kasse", aar)
#' stjerne <- kr_data("stjernetabell", aar)
#' formler <- kr_data("formler", aar)
#' 
#' 
#' # bare aar 2016 fins i balansedata
#' balansedata <- kr_data("balansedata")
#' kapittelhierarki <- kr_data("kapittelhierarki")
#'
kr_data <- function(dataset, periode = NULL, path = NULL) {
  
  if (dataset == "balansedata") {
    return(kr_data("balanseRegnskapDataPen")$data[, -1])
  }
  if (dataset == "kapittelhierarki") {
    return(kr_data("balanseRegnskapDataPen")$kapittelhierarki[, -1])
  }
  
  if (!is.null(periode)) {
    return(kr_data_periode(dataset, periode))
  }
  if (!is.null(path)) {
    filename <- paste(path, dataset, ".RData", sep = "")
    return(get(load(filename, envir = environment())))
  }
  if (!exists(dataset, pkg_env_kr_data))
    data(list = dataset, package = "kostraregnskap", envir = pkg_env_kr_data)
  return(pkg_env_kr_data[[dataset]])
  return(NULL)
}



kr_data_periode <- function(dataset, periode) {
  d <- kr_data("kostraRegnskapDataPen")
  DataPeriode(d[[dataset]], as.character(periode), rm_periode = TRUE)
}






#' Datasets returned by kr_data()
#'
#' @docType data
#' @keywords datasets internal
#' @name kostraRegnskapData
NULL

#' @rdname kostraRegnskapData
#' @name kostraRegnskapDataPen
NULL


#' @rdname kostraRegnskapData
#' @name balanseRegnskapData
NULL


#' @rdname kostraRegnskapData
#' @name balanseRegnskapDataPen
NULL


