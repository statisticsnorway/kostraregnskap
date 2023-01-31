

# stackoverflow questions 30357330
pkg_env_kr_data <- new.env(parent=emptyenv())


#' Return a data set
#'
#' @encoding UTF8
#'
#' @param dataset Name of data set within the kostraregnskap package 
#' @param path When non-NULL the data set is read from "path/dataset.RData"
#'
#' @return The data set
#' @export
#' @importFrom utils data
#'
#' @examples
#'  z  <- kr_data("balanseRegnskapDataPen")
#'  z  <- kr_data("balanseRegnskapData")
#'  z  <- kr_data("kostraRegnskapDataPen")
#'  z  <- kr_data("kostraRegnskapData")
#'
kr_data <- function(dataset, path = NULL) {
  if (!is.null(path)) {
    filename <- paste(path, dataset, ".RData", sep = "")
    return(get(load(filename, envir = environment())))
  }
  if (!exists(dataset, pkg_env_kr_data))
    data(list = dataset, package = "kostraregnskap", envir = pkg_env_kr_data)
  return(pkg_env_kr_data[[dataset]])
  return(NULL)
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


