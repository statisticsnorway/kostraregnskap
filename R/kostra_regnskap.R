
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



#' Sett av matrisekomponenter slås sammen
#' 
#' Når det bare er summen av hierarki og nettinghierarki for samme datainput som skal brukes til slutt, er det fordelaktig å slå disse sammen.  
#'
#' @param aij To deler av output fra \code{\link{get_a1234}}. Dvs. enten a2 og a3 sammen eller a1 og a4 sammen. 
#'
#' @return matrisekomponenter
#' @export
#'
sum_netting <- function(aij) {
  if (is.null(aij[[2]])) {
    return(aij[[1]])
  }
  ma <- Match(aij[[2]]$fromCrossCode, aij[[1]]$fromCrossCode)
  if (any(!is.na(ma))) {
    aij[[1]]$dataDummyHierarchy[, ma[!is.na(ma)]] <- aij[[1]]$dataDummyHierarchy[, ma[!is.na(ma)], drop = FALSE] + aij[[2]]$dataDummyHierarchy[, !is.na(ma), drop = FALSE]
  }
  if (any(is.na(ma))) {
    aij[[1]]$dataDummyHierarchy <- cbind(aij[[1]]$dataDummyHierarchy, aij[[2]]$dataDummyHierarchy[, is.na(ma), drop = FALSE])
    aij[[1]]$valueMatrix <- rbind(aij[[1]]$valueMatrix, aij[[2]]$valueMatrix[is.na(ma), , drop = FALSE])
    aij[[1]]$fromCrossCode <- rbind(aij[[1]]$fromCrossCode, aij[[2]]$fromCrossCode[is.na(ma), , drop = FALSE])
  }
  aij[[1]]
}


#' Formel-korreksjon av sett av matrisekomponenter
#'
#' @param a Sett av matrisekomponenter
#' @param af Korresponderende  sett av matrisekomponenter med kombinasjoner som trengs for formler
#' @param fm Output fra `FormelMatrise`  (gammel intern funksjon for beregningstester)
#'
#' @return Korrigert versjon av input `a`
#' @export
#'
formel_korreksjon <- function(a, af, fm) {
  id <- Match(fm$codesLeft, a[[1]]$toCrossCode)
  formelMatrise <- fm$formelMatrise[!is.na(id), , drop = FALSE]
  id <- id[!is.na(id)]
  for (i in which(!sapply(a, is.null))) {
    a[[i]]$dataDummyHierarchy[id, ] <- 0
    dummyExtra <- Matrix(0, nrow(a[[i]]$dataDummyHierarchy), nrow(af[[i]]$valueMatrix))
    dummyExtra[id, ] <- formelMatrise %*% af[[i]]$dataDummyHierarchy
    a[[i]]$dataDummyHierarchy <- cbind(a[[i]]$dataDummyHierarchy, dummyExtra)
    a[[i]]$valueMatrix <- rbind(a[[i]]$valueMatrix, af[[i]]$valueMatrix)
    a[[i]]$fromCrossCode <- NULL  # trengs ikke
  }
  a
}




regnskap_from_matrix <- function(matA, matB, periode, regnskapsomfang = NULL, valueVar = "belop", kombinasjoner,  storkOrder = NULL, 
                               storkombinasjoner = NULL,  colVar="region", integerInOutput){
  regnskapsomfanger <- regnskapsomfang
  stringsAsFactors = FALSE
  forceStringsAsFactors = FALSE
  if(length(regnskapsomfanger) == 1){
    if(regnskapsomfanger=="A"){
      z=data.frame(a=as.vector(as.vector(as.matrix(matA))),stringsAsFactors=stringsAsFactors)
      regnskapsomfang =  data.frame(a=rep(regnskapsomfanger, times = 1, each = cumprod(dim(matA))[2]  ),stringsAsFactors=stringsAsFactors)
    } else {
      z=data.frame(a=as.vector(as.vector(as.matrix(matB))),stringsAsFactors=stringsAsFactors)
      regnskapsomfang =  data.frame(a=rep(regnskapsomfanger, times = 1, each = cumprod(dim(matB))[2]  ),stringsAsFactors=stringsAsFactors)
    }
    
  } else{
    if(length(regnskapsomfanger) != 2){
      stop("galt antall regnskapsomfan")
    }
    z=data.frame(a=as.vector(c(as.vector(as.matrix(matA)),as.vector(as.matrix(matB)))),stringsAsFactors=stringsAsFactors)
    regnskapsomfang =  data.frame(a=rep(regnskapsomfanger, times = 1, each = cumprod(dim(matA))[2]  ),stringsAsFactors=stringsAsFactors)
  }
  names(z) = valueVar
  names(regnskapsomfang) = "regnskapsomfang"
  rownames(regnskapsomfang) = NULL
  
  
  if(integerInOutput){
    for(i in seq_len(length(z))){
      z[[i]] = LagInteger(z[[i]])
    }
  }
  
  
  if(is.null(matA)){
    mat1 = matB
  } else {
    mat1 = matA
  }
  
  colDataSelected =  data.frame(a=rep(colnames(mat1), times = 1, each =dim(mat1)[1]),stringsAsFactors=stringsAsFactors)
  names(colDataSelected) = colVar
  
  # Sikre mot rownames warning
  rownames(colDataSelected) = NULL
  rownames(kombinasjoner) = NULL
  rownames(z) = NULL
  
  
  if(is.null(storkOrder)){
    w=cbind(periode=periode,regnskapsomfang,colDataSelected,kombinasjoner,z)
    return(w)
  } else{
    
    
    w=cbind(periode=periode, regnskapsomfang)
    
    if(length(regnskapsomfanger)==2)
      storkOrder = c(storkOrder,round(NROW(w)/2)+storkOrder)
    
    w  = w[storkOrder, ,drop=FALSE]
    cat("..")
    flush.console()
    z  = z[storkOrder, ,drop=FALSE]
    
    cat("..")
    flush.console()
  
    nCOLstorkombinasjoner = NCOL(storkombinasjoner)
    namesStorkombinasjoner = names(storkombinasjoner)
    storkombinasjoner = storkombinasjoner[ , c(rep(1,NCOL(w)),seq_len(nCOLstorkombinasjoner),rep(1,NCOL(z))) , drop=FALSE]
    
    ncolw = NCOL(w)
    names(storkombinasjoner)[seq_len(ncolw)] = names(w)
    names(storkombinasjoner)[(nCOLstorkombinasjoner+ncolw +seq_len(NCOL(z)))  ]  = names(z)
    names(storkombinasjoner)[ncolw+seq_len(nCOLstorkombinasjoner)] = namesStorkombinasjoner
    
    reprow = round(NROW(w)/NROW(storkombinasjoner))
    
    if( !(reprow==1 | reprow==2))
      stop("Noe er galt")
    
    cat(":")
    flush.console()
    
    if(reprow==2){
      storkombinasjoner = rbind(storkombinasjoner,storkombinasjoner)
      
    }
    cat(".")
    flush.console()
  
    storkombinasjoner[,seq_len(ncolw)] = w
    rm("w")
    cat(".")
    flush.console()
    
    storkombinasjoner[, (nCOLstorkombinasjoner+ncolw +seq_len(NCOL(z)))  ]  = z
    rm("z")
    
    cat(".")
    flush.console()
    
    cat("]\n\n")
    flush.console()
    
    
    if(length(regnskapsomfanger)==1)
      storkombinasjoner = storkombinasjoner[storkombinasjoner$regnskapsomfang==regnskapsomfanger , ,drop=FALSE]
    
    rownames(storkombinasjoner) = NULL
    
    return(storkombinasjoner)}
  
}  


#' kostra_regnskap
#'
#' @param ... dots
#' @param output output 
#'
#' @return output
#' @export
#'
kostra_regnskap <- function(..., output = "en") {

  if(output != "en"){
    return(KostraRegnskapEnPeriode(..., output = output))
  }
  
  b <- beredt(...)
  
  my_call <- as.call(c(get_a1234, b))
  
  q <- eval(my_call)
  
  q2 <- list(sum_netting(q[c(1, 4)]), sum_netting(q[c(2, 3)]))
  
  if (!is.null(b$formler)) {
    fm <- kostraregnskap:::FormelMatrise(b$formler$formel)
    my_call$kombinasjoner <- fm$codesRight
    qf <- eval(my_call)
    q2f <- list(sum_netting(qf[c(1, 4)]), sum_netting(qf[c(2, 3)]))
    
    qq <- formel_korreksjon(c(q[1], q2), c(qf[1], q2f), fm)
  } else {
    qq <- c(q[1], q2)
  }
  
  
  
  if ("B" %in% b$regnskapsomfang | "C" %in% b$regnskapsomfang) {
    matB <- qq[[1]]$dataDummyHierarchy %*% qq[[1]]$valueMatrix
  } else {
    matB <- NULL
  }
  
  
  if ("A" %in% b$regnskapsomfang) {
    matA <- qq[[2]]$dataDummyHierarchy %*% qq[[2]]$valueMatrix + qq[[3]]$dataDummyHierarchy %*% qq[[3]]$valueMatrix
  } else {
    matA <- NULL
  }
  
  
  k=regnskap_from_matrix(matA, matB, periode = b$periode, regnskapsomfang = b$regnskapsomfang, valueVar = "belop", kombinasjoner = b$kombinasjoner, storkOrder = b$storkOrder, storkombinasjoner = b$storkombinasjoner,
                     integerInOutput = b$integerInOutput)
  
  
}






