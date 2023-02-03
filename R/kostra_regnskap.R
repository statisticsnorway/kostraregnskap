
#' Forbehandling av input  
#'
#' @param ... Input til \code{\link{KostraRegnskapEnPeriode}}
#'
#' @return Liste med data som er forberedt
#' @export
#'
beredt = function(...){
  KostraRegnskapEnPeriode(..., output = "beredt")
}



#' Fire sett av matrisekomponenter 
#' 
#' Med input fra \code{\link{beredt}}-output
#'
#' @param data data
#' @param data_saer data_saer 
#' @param funksjonshierarki funksjonshierarki 
#' @param artshierarki artshierarki 
#' @param arts32 artshierarki_nettinger omgjort til hierarki som kan beregnes fra `data`
#' @param arts41 artshierarki_nettinger_kasse omgjort til hierarki som kan beregnes fra `data`
#' @param regioner regioner 
#' @param kombinasjoner kombinasjoner 
#' @param rowsInputArt Denne ble beregnet som `kombinasjoner$art %in% unique(c(unique(data$art), unique(data_saer$art)))`. 
#'                     Kan sannsynligvis fjernes som parameter og heller beregnes på nytt.
#'                     Brukes for å ta bort koder for å hindre fordobling fra både hierarki og nettinghierarki.  
#' @param handleDuplicated handleDuplicated
#' @param useMatrixToDataFrame useMatrixToDataFrame 
#' @param ... Ubrukte parametere 
#'
#' @return Fire sett `HierarchyCompute`-output ved bruk av `output = "matrixComponents"`
#' @export
#'
get_a1234 <- function(data, data_saer = NULL, funksjonshierarki, artshierarki, 
                      arts32 = NULL, arts41 = NULL, regioner, 
                      kombinasjoner, rowsInputArt, 
                      handleDuplicated = "stop", useMatrixToDataFrame = TRUE,
                      ...) {
  onlyB <- is.null(data_saer)
  isnk <- !is.null(arts41)
  
  a1 <- KostraRegnskap1(data, funksjonshierarki, artshierarki, regioner = regioner, 
                        kombinasjoner = kombinasjoner, output = "matrixComponents", 
                        useMatrixToDataFrame = useMatrixToDataFrame,
                        handleDuplicated = handleDuplicated)
  
  a2 <- NULL
  a3 <- NULL
  a4 <- NULL
  
  if (!onlyB) {
    a2 <- KostraRegnskap1(data_saer, funksjonshierarki, artshierarki, regioner = regioner, 
                          kombinasjoner = kombinasjoner, output = "matrixComponents", 
                          colNotInDataWarning = FALSE,
                          useMatrixToDataFrame = useMatrixToDataFrame, 
                          handleDuplicated = handleDuplicated)
    
    
    a3 <- KostraRegnskap1(data_saer, funksjonshierarki, arts32, regioner = regioner, 
                          kombinasjoner = kombinasjoner, output = "matrixComponents", 
                          colNotInDataWarning = FALSE, 
                          useMatrixToDataFrame = useMatrixToDataFrame,
                          handleDuplicated = handleDuplicated)
    
    
    rowsInputArt <- kombinasjoner$art %in% unique(c(unique(data$art), unique(data_saer$art)))
    
    if (any(rowsInputArt))
      a3$dataDummyHierarchy[rowsInputArt, ] <- 0
    
    if (isnk) {
      a4 <- KostraRegnskap1(data, funksjonshierarki, arts41, regioner = regioner, 
                            kombinasjoner = kombinasjoner, output = "matrixComponents", 
                            colNotInDataWarning = FALSE, useMatrixToDataFrame = useMatrixToDataFrame,
                            handleDuplicated = handleDuplicated)
      
      if (any(rowsInputArt))
        a4$dataDummyHierarchy[rowsInputArt, ] <- 0
    }
  }
  list(a1 = a1, a2 = a2, a3 = a3, a4 = a4)
}