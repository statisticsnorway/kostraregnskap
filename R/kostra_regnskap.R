
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
#' @param handleDuplicated Denne bør være `stop` når ved beregning av id-bidrag siden rad-nummer ikke kan summeres.  
#' @param useMatrixToDataFrame useMatrixToDataFrame 
#' @param ... Ubrukte parametere 
#'
#' @return Fire sett `HierarchyCompute`-output ved bruk av `output = "matrixComponents"`
#' @export
#'
get_a1234 <- function(data, data_saer = NULL, funksjonshierarki, artshierarki, 
                      arts32 = NULL, arts41 = NULL, regioner, 
                      kombinasjoner, rowsInputArt, 
                      handleDuplicated, useMatrixToDataFrame = TRUE,
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




regnskap_from_matrix <- function(matA, matB, periode, regnskapsomfang = NULL, value_var = "belop", kombinasjoner,  storkOrder = NULL, 
                               storkombinasjoner = NULL,  colVar="region", integerInOutput, bidragA, bidragB, 
                               bidrag_var = "source", id_var = "UUID", 
                               fun_generer_id){
  regnskapsomfanger <- regnskapsomfang
  stringsAsFactors = FALSE
  forceStringsAsFactors = FALSE
  if(length(regnskapsomfanger) == 1){
    if(regnskapsomfanger=="A"){
      if(is.null(bidragA)){
        z=data.frame(a=as.vector(as.vector(as.matrix(matA))))
      } else {
        z=data.frame(a=as.vector(as.vector(as.matrix(matA))), bidrag=as.vector(as.vector(as.matrix(bidragA))))
      }
      regnskapsomfang =  data.frame(a=rep(regnskapsomfanger, times = 1, each = cumprod(dim(matA))[2]  ),stringsAsFactors=stringsAsFactors)
    } else {
      if(is.null(bidragA)){
        z=data.frame(a=as.vector(as.vector(as.matrix(matB))))
      } else {
        z=data.frame(a=as.vector(as.vector(as.matrix(matB))),  bidrag=as.vector(as.vector(as.matrix(bidragB))))
      }
      regnskapsomfang =  data.frame(a=rep(regnskapsomfanger, times = 1, each = cumprod(dim(matB))[2]  ),stringsAsFactors=stringsAsFactors)
    }
    
  } else{
    if(length(regnskapsomfanger) != 2){
      stop("galt antall regnskapsomfan")
    } 
    if(is.null(bidragA)){
      z=data.frame(a=as.vector(c(as.vector(as.matrix(matA)),as.vector(as.matrix(matB)))))
    } else {
      z=data.frame(a=as.vector(c(as.vector(as.matrix(matA)),as.vector(as.matrix(matB)))), bidrag=as.vector(c(as.vector(as.matrix(bidragA)),as.vector(as.matrix(bidragB)))))
    }
    regnskapsomfang =  data.frame(a=rep(regnskapsomfanger, times = 1, each = cumprod(dim(matA))[2]  ),stringsAsFactors=stringsAsFactors)
  }
  names(z) = c(value_var, bidrag_var)[seq_len(ncol(z))]
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
  
  if (is.null(periode)) {
    # cbind not working with NULL
    periode <- matrix(0, nrow(regnskapsomfang), 0)
  }
  
  if(is.null(storkOrder)){
    if (!is.null(fun_generer_id)) {
      z <- z[c(1, seq_len(length(z)))]
      names(z)[2] <- id_var
      z[[2]] <- fun_generer_id(nrow(z))
    }
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
    if (!is.null(fun_generer_id)) {
      z <- z[c(1, seq_len(length(z)))]
      names(z)[2] <- id_var
      z[[2]] <- fun_generer_id(nrow(z))
    }
    
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


#' Bevilgningsregnskapet
#' 
#' Fornyet versjon av  \code{\link{KostraRegnskap}} med nye muligheter.
#' 
#' Denne funksjonen gjenbruker den gamle funksjonen \code{\link{KostraRegnskapEnPeriode}} 
#' til å forbehandle input slik som i \code{\link{beredt}}. 
#' Selve hovedberegningene foregår utenfor den gamle funksjonen.
#' 
#' I motsetning til \code{\link{KostraRegnskap}} så kjører denne funksjonen bare på data for én periode.
#' Dersom variabelen `"periode"` finnes i input, blir `"periode"` også med i output.
#' Dersom `periode` i input ikke er unik, blir det feilmelding. 
#'
#' @inheritParams KostraRegnskapEnPeriode
#' @param handleDuplicated  Parameter til \code{\link{HierarchyCompute}}.  
#'        Bare `"stop"` mulig  når `bidrag = TRUE`.
#' @param ... Flere parametere til \code{\link{KostraRegnskapEnPeriode}}. 
#' @param output Dersom annet enn `"standard"` spesifiseres vil resultatet fra
#'               \code{\link{KostraRegnskapEnPeriode}} returneres. 
#' @param bidrag Ved `TRUE` vil det genereres tekststreng 
#'               med funksjonen spesifisert med parameteren `fun_id_bidrag`.  
#' @param generer_id Ved `TRUE` vil det genereres nye id-er i output-data 
#'               med funksjonen spesifisert med parameteren `fun_generer_id`.  
#' @param bidrag_var    Navn på variabel i output som genereres når `bidrag = TRUE`.   
#' @param id_var Navn på id-variabel i input/output. 
#' @param fun_id_bidrag  \code{\link{id_bidrag}} eller en tilsvarende funksjon.  
#' @param fun_generer_id \code{\link{uuid_generate_time}} eller en tilsvarende funksjon.
#'
#' @return En data frame
#' @export
#' 
#' @examples 
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
#' inputdata$UUID <- paste0("ce1f9682-data-data-8000-", 
#'                           SSBtools::Number(seq_len(nrow(inputdata)), 13))
#' inputdata_saer$UUID <- paste0("ca3679e0-data-saer-8000-", 
#'                                SSBtools::Number(seq_len(nrow(inputdata_saer)), 13))
#' 
#' z1 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
#'                       artshierarki = artshierarki, data_saer = inputdata_saer, 
#'                       artshierarki_nettinger = artshierarki_nettinger, 
#'                       artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
#'                       stjernetabell = stjerne, formler = formler, 
#'                       arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
#'                       funksjoner = c("FG2", "FG1"))
#' z1[1:10, 1:7]
#' z1[5, ]
#' z1[7:10, ]
#' 
#' 
#' # Samme, men bruker id_bidrag2 som ikke tar med belop i "source"
#' z2 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
#'                       artshierarki = artshierarki, data_saer = inputdata_saer, 
#'                       artshierarki_nettinger = artshierarki_nettinger, 
#'                       artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
#'                       stjernetabell = stjerne, formler = formler, 
#'                       arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
#'                       funksjoner = c("FG2", "FG1"), fun_id_bidrag = id_bidrag2)
#' z2[5, ]
#' 
#' # Samme uten generering av bidrag
#' z3 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
#'                       artshierarki = artshierarki, data_saer = inputdata_saer, 
#'                       artshierarki_nettinger = artshierarki_nettinger, 
#'                       artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
#'                       stjernetabell = stjerne, formler = formler, 
#'                       arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
#'                       funksjoner = c("FG2", "FG1"), bidrag = FALSE)
#' z3[c(3:7, 51:55),  ]
#' 
#' 
#' # Samme med C istedenfor B
#' z4 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
#'                       artshierarki = artshierarki, data_saer = inputdata_saer, 
#'                       artshierarki_nettinger = artshierarki_nettinger, 
#'                       artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
#'                       stjernetabell = stjerne, formler = formler, 
#'                       arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
#'                       funksjoner = c("FG2", "FG1"), bidrag = FALSE,
#'                       useC = TRUE)
#' z4[c(3:7, 51:55),  ]
#' 
#' 
#' # Samme med C istedenfor B på annen måte. Samt ikke generering av id. 
#' z5 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
#'                       artshierarki = artshierarki, data_saer = inputdata_saer, 
#'                       artshierarki_nettinger = artshierarki_nettinger, 
#'                       artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
#'                       stjernetabell = stjerne, formler = formler, 
#'                       arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
#'                       funksjoner = c("FG2", "FG1"), bidrag = FALSE,
#'                       regnskapsomfang = c("A", "C"), generer_id = FALSE)
#' z5[c(3:7, 51:55),  ]
#' 
#' 
#' # Bare C i output 
#' z5 <- kostra_regnskap(data = inputdata, funksjonshierarki = funksjonshierarki, 
#'                       artshierarki = artshierarki, data_saer = inputdata_saer, 
#'                       artshierarki_nettinger = artshierarki_nettinger, 
#'                       artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
#'                       stjernetabell = stjerne, formler = formler, 
#'                       arter = c("AGD9", "AGID1", "AGI14", "AGD32", "AGD65"), 
#'                       funksjoner = c("FG2", "FG1"), bidrag = FALSE,
#'                       regnskapsomfang = "C", generer_id = FALSE)
#' z5[1:10,  ]
#' 
#' 
kostra_regnskap <- function(data,
                            funksjonshierarki,
                            artshierarki,
                            data_saer = NULL,
                            artshierarki_nettinger = NULL,
                            artshierarki_nettinger_kasse = NULL,
                            kombinasjoner = NULL,
                            regioner = NULL,
                            storkombinasjoner = NULL,
                            stjernetabell = NULL,
                            funksjoner = NULL,
                            arter = NULL,
                            kontoklasser = NULL,
                            formler = NULL,
                            regnskapsomfang = NULL,
                            useC = any(c(grepl("C", regnskapsomfang))),
                            handleDuplicated = "stop",
                            ..., 
                            output = "standard",
                            bidrag = TRUE,
                            generer_id = TRUE,
                            bidrag_var = "source",
                            id_var = "UUID",
                            fun_id_bidrag = id_bidrag,
                            fun_generer_id = uuid_generate_time) {

  
  periode <- unique(c(as.character(data$periode),
                      as.character(funksjonshierarki$periode),
                      as.character(artshierarki$periode),
                      as.character(data_saer$periode),
                      as.character(artshierarki_nettinger_kasse$periode),
                      as.character(artshierarki_nettinger$periode)))
  
  if (length(periode) > 1) {
    stop(paste("periode er ikke unik:", paste(periode, collapse = ", ")))
  }
  
  gammel_rutine = !(output %in% "standard")
  
  
  if (output == "standard") {
    output <- "beredt"
  }
  
  
  b <- KostraRegnskapEnPeriode(data = data,
                               funksjonshierarki = funksjonshierarki,
                               artshierarki = artshierarki,
                               data_saer = data_saer,
                               artshierarki_nettinger = artshierarki_nettinger,
                               artshierarki_nettinger_kasse = artshierarki_nettinger_kasse,
                               kombinasjoner = kombinasjoner,
                               regioner = regioner,
                               storkombinasjoner = storkombinasjoner,
                               stjernetabell = stjernetabell,
                               funksjoner = funksjoner,
                               arter = arter,
                               kontoklasser = kontoklasser,
                               formler = formler,
                               regnskapsomfang = regnskapsomfang,
                               useC = useC,
                               handleDuplicated = handleDuplicated, 
                               ..., output = output)
  
  
  if (gammel_rutine) {
    return(b)
  }
  if (is.na(b$periode)) {
    b$periode <- NULL
  }
  
  if (bidrag) {
    if(handleDuplicated != "stop"){
      stop('Bare handleDuplicated="stop" mulig når bidrag=TRUE')
    }
    df <- rbind(b$data["belop"], b$data_saer["belop"])
    if (id_var %in% names(b$data) & id_var %in% names(b$data_saer)) {
      if(!is.null(b$data_saer)){
        df$id <- c(b$data[[id_var]], b$data_saer[[id_var]]) 
      } else {
        df$id <- b$data[[id_var]]
      }
    } else {
      if(!is.null(b$data_saer)){
        df$id <- c(paste0("data_row_", seq_len(nrow(b$data))), 
                   paste0("data_saer_row_", seq_len(nrow(b$data_saer)))) 
      } else {
        df$id <- paste0("data_row_", seq_len(nrow(b$data)))
      }
    }
    b$data$belop <- seq_len(nrow(b$data))
    if(!is.null(b$data_saer)){
      b$data_saer$belop <- nrow(b$data) + seq_len(nrow(b$data_saer))
    }
  }
  
  # Triks for å kalle get_a1234 uten å skrive alle parametere 
  my_call <- as.call(c(get_a1234, b))
  q <- eval(my_call)
  
  if (!is.null(b$formler)) {
    fm <- FormelMatrise(b$formler$formel)
    my_call$kombinasjoner <- fm$codesRight
    qf <- eval(my_call)
  }
  
  matA <- NULL
  matB <- NULL
  bidragA <- NULL
  bidragB <- NULL
  
  if (!bidrag) {
    if (!is.null(b$formler)) {
      q <- formel_korreksjon(q, qf, fm)
    } 
    
    if ("B" %in% b$regnskapsomfang | "C" %in% b$regnskapsomfang) {
      matB <- q[[1]]$dataDummyHierarchy %*% q[[1]]$valueMatrix
    } 
    
    if ("A" %in% b$regnskapsomfang) {
      matA <- (q[[1]]$dataDummyHierarchy %*% q[[1]]$valueMatrix + 
               q[[2]]$dataDummyHierarchy %*% q[[2]]$valueMatrix + 
               q[[3]]$dataDummyHierarchy %*% q[[3]]$valueMatrix)
      if (!is.null(q[[4]])) {
        matA <- matA + q[[4]]$dataDummyHierarchy %*% q[[4]]$valueMatrix
      }  
    } 
    
  } else {
    q2 <- list(sum_netting(q[c(1, 4)]), sum_netting(q[c(2, 3)]))
    if (!is.null(b$formler)) {
      q2f <- list(sum_netting(qf[c(1, 4)]), sum_netting(qf[c(2, 3)]))
      q <- formel_korreksjon(c(q[1], q2), c(qf[1], q2f), fm)
    } else {
      q <- c(q[1], q2)
    }
    if ("A" %in% b$regnskapsomfang) {
      q[[2]]$dataDummyHierarchy <- cbind(q[[2]]$dataDummyHierarchy, q[[3]]$dataDummyHierarchy)
      q[[2]]$valueMatrix <- as.matrix(rbind(q[[2]]$valueMatrix, q[[3]]$valueMatrix))
      q[3] <- NULL
      value_matrix_A <- q[[2]]$valueMatrix
      value_matrix_A[q[[2]]$valueMatrix > 0] <- df[["belop"]][q[[2]]$valueMatrix]
      matA <- q[[2]]$dataDummyHierarchy %*% value_matrix_A
      cat(" [bidragA..")
      flush.console()
      bidragA <- id_bidrag_matrix(Matrix::t(q[[2]]$dataDummyHierarchy), q[[2]]$valueMatrix, df, 
                                  fun_id_bidrag = fun_id_bidrag)
      cat(".]")
      flush.console()
    }
    if ("B" %in% b$regnskapsomfang | "C" %in% b$regnskapsomfang) {
      q[[1]]$valueMatrix <- as.matrix(q[[1]]$valueMatrix)
      value_matrix_B <- q[[1]]$valueMatrix
      value_matrix_B[q[[1]]$valueMatrix > 0] <- df[["belop"]][q[[1]]$valueMatrix]
      matB <- q[[1]]$dataDummyHierarchy %*% value_matrix_B
      cat(" [bidragB..")
      flush.console()
      bidragB <- id_bidrag_matrix(Matrix::t(q[[1]]$dataDummyHierarchy), q[[1]]$valueMatrix, df,
                                  fun_id_bidrag = fun_id_bidrag)
      cat(".]")
      flush.console()
    }
  }
  
  if (!generer_id) {
    fun_generer_id <- NULL
  }
  
  regnskap_from_matrix(matA, matB, periode = b$periode, regnskapsomfang = b$regnskapsomfang, 
                       value_var = "belop", kombinasjoner = b$kombinasjoner, 
                       storkOrder = b$storkOrder, storkombinasjoner = b$storkombinasjoner,
                       integerInOutput = b$integerInOutput, 
                       bidragA = bidragA, bidragB = bidragB, 
                       bidrag_var = bidrag_var,
                       id_var = id_var,
                       fun_generer_id = fun_generer_id)
}







