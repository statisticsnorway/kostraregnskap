#' balanse_regnskap
#'
#' Funksjon som ligner på KostraRegnskap, men mye enklere.
#'
#' @encoding UTF8
#'
#' @param data inputdata
#' @param kapittelhierarki kapittelhierarki
#' @param kombinasjoner kombinasjoner kapitler og regnskapsomfang som skal med i output
#' @param regioner regioner som skal med i output. Kan også gjøre bruk av *, ?, ! og –
#' @param storkombinasjoner  kombinasjoner kapitler,regnskapsomfang og region som skal med i output
#' @param kapitler kapitler som skal med i output
#' @param omfang  regnskapsomfang som skal med i output
#' @param output kode for type output som (brukes av BalanseRegnskapBeregningHierarki og BalanseRegnskapBeregningInput)
#' @param printData Ved TRUE printes to første og to siste rader av alle inputdataene
#' @param lag0300 Ved TRUE kopieres region 0301 til 0300 i inputdata
#' @param fixRegionkode Ved TRUE (default): Sørger for blanke i starten/slutten fjernes og at regionkoder får 4 eller 6 plasser og ledende nuller (gir warning ved endring av input)
#' @inheritParams kostra_regnskap
#'
#' @return Data frame med samme variabler som data i input og med kapitler/regnskapsomfang i henhold til annen input
#' @export
#' @importFrom SSBtools CharacterDataFrame WildcardGlobbingVector Match RowGroups CrossCodeFrames HierarchyCompute HierarchyFix ForceCharacterDataFrame DummyHierarchy Number matlabColon
#' @importFrom SSBtools RbindAll Stack MatrixPaste
#' @importFrom utils flush.console head tail
#' @importFrom stringr str_replace_all str_replace
#'
#' @seealso \code{\link{KostraRegnskap}}, \code{\link{HierarchyCompute}}, \code{\link{HierarchicalWildcardGlobbing}}
#'
#' @examples
#'
#' # Her brukes data som ligger i pakken der bare noen regioner er med.
#'
#' inputdata <- kr_data("balansedata")  
#' hierarki <- kr_data("kapittelhierarki")  
#'
#' z <- balanse_regnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301"))
#' 
balanse_regnskap <- function(data,kapittelhierarki,
                                    kombinasjoner=NULL,
                                    regioner=NULL,
                                    storkombinasjoner=NULL,
                                    kapitler  = NULL,
                                    omfang = NULL,
                                    output = "standard",
                                    printData = TRUE,
                                    lag0300 = FALSE,
                                    fixRegionkode = TRUE,
                             bidrag = TRUE,
                             generer_id = TRUE,
                             bidrag_var = "source",
                             id_var = "UUID",
                             fun_id_bidrag = id_bidrag,
                             fun_generer_id = uuid_generate_time){
  
  
  if (output != "standard") {
    bidrag <- FALSE
    generer_id <- FALSE
  }
  
  StopOrWarning = stop
  
  if(printData){
    PrintHeadTail(data)
    PrintHeadTail(kapittelhierarki)
    PrintHeadTail(kombinasjoner)
    PrintHeadTail(regioner)
    PrintHeadTail(storkombinasjoner)
    PrintHeadTail(kapitler)
    PrintHeadTail(omfang)
  }
  
  
  if(is.null(omfang))
    omfang = "*"
  
  
  warningTextRegionskoder = "Regionskoder endret"
  useMatrixToDataFrame =TRUE
  
  
  
  
  if(!is.null(storkombinasjoner)){
    if(sum(names(storkombinasjoner)=="region")==0){
      if(any(names(storkombinasjoner)=="fylkesregion")){
        names(storkombinasjoner)[names(storkombinasjoner)=="fylkesregion"] <- "region"
        warning("'fylkesregion' omkodet til 'region' i storkombinasjoner")
      }
    }
    if(fixRegionkode)
      storkombinasjoner$region = FixRegionkode(storkombinasjoner$region,warningTextRegionskoder)
  }
  
  
  if(fixRegionkode & !is.null(regioner))
    regioner = FixRegionkode(regioner,warningTextRegionskoder)
  
  
  
  if(NROW(data)==0)
    stop("Det er ingen rader i data")
  
  if(output == "barePrintData")
    return(NULL)
  
  
  returnMatrixComponents=FALSE
  beregningInput = FALSE
  
  
  if(sum(names(data)=="region")==0){
    if(any(names(data)=="fylkesregion")){
      names(data)[names(data)=="fylkesregion"] <- "region"
      warning("'fylkesregion' omkodet til 'region' i data")
    }
  }
  if(fixRegionkode)
    data$region = FixRegionkode(data$region,warningTextRegionskoder)
  
  
  periode = unique(c(as.character(data$periode),
                     as.character(kapittelhierarki$periode)))
  
  if(length(periode) == 0 & !(output %in% c("standard", "matrixComponents")))
    stop("periode finnes ikke i input")
  if(length(periode) > 1)
    stop(paste("periode er ikke unik:",paste(periode,collapse=", ")))
  
  
  varIdata = c("region", "kapittel", "regnskapsomfang", "belop") %in% names(data)
  if(any(!varIdata)){
    stop(paste("Disse variablene mangler i data:",
               paste(c("region", "kapittel", "regnskapsomfang", "belop")[!varIdata],collapse=", ")))
  }
  
  
  ####omfangAll  = unique(data$regnskapsomfang)#### Hardkoder isteden  siden beregnigstest kan feile hvis alle ikke er med
  omfangAll  =  c("B","sbedr","lanefond")    # A skal ikke med her.
  omfangshierarki <- kapittelhierarki[rep(1,length(omfangAll)),c("to","from","sign"),drop=FALSE]
  omfangshierarki$to = "A"
  omfangshierarki$from = omfangAll
  omfangshierarki$sign = "+"
  
  omfangAll = c("A",omfangAll) # Brukes nedenfor
  
  
  integerInOutput = TRUE
  
  if(!is.integer(data$belop))
    integerInOutput = FALSE
  
  
  #
  #  Bedre å passe på en gang (eller 10 ganger) for mye dette med factor og character enn en for lite ...
  #
  data[,c("region", "kapittel", "regnskapsomfang")] = CharacterDataFrame(data[,c("region", "kapittel", "regnskapsomfang")])
  
  
  warning0301 = FALSE
  
  if(lag0300 & !is.numeric(regioner)){
    if("0300" %in% data$region)
      warning("0300 finnes allerede i data. Bedre med lag0300=FALSE?")
    
    
    data0300 = data[data$region=="0301", ,drop=FALSE]
    
    if(NROW(data0300)==0)
      warning0301 = TRUE
    else{
      data0300$region = "0300"
      data = rbind(data,data0300)
    }
  }
  
  
  if(is.numeric(regioner)){
    uregioner = unique(data$region)
    if(regioner[1] < length(uregioner))
      regioner = uregioner[seq_len(regioner[1])]
    else
      regioner = NULL
  } else {
    if(any(c(Fgrepl("*",regioner), Fgrepl("?",regioner),Fgrepl("!",regioner),Fgrepl("-",regioner)))){
      regioner = WildcardGlobbingVector(unique(data$region),regioner)
    }
  }
  
  
  if(warning0301)
    warning("0301 finnes ikke i input. 0300 blir ikke laget")
  
  
  if(!is.null(omfang)){
    omfang = unique(omfang)
    
    
    if(any(c(Fgrepl("*",omfang), Fgrepl("?",omfang),Fgrepl("!",omfang),Fgrepl("-",omfang)))){
      omfang = WildcardGlobbingVector(omfangAll,omfang)
    } else {
      omfangIn =  omfang %in% omfangAll
      if(any(!omfangIn)){
        StopOrWarning(paste("Disse regnskapsomfangene finnes ikke i data eller hierarkier:",
                            paste(omfang[!omfangIn],collapse=", ")))
      }
    }
    
  }
  if(!is.null(kapitler)){
    kapitler = unique(kapitler)
    kapitlerAll = unique(c(kapittelhierarki$to,kapittelhierarki$from,
                           unique(c(unique(data$kapittel)))))
    
    if(any(c(Fgrepl("*",kapitler), Fgrepl("?",kapitler),Fgrepl("!",kapitler),Fgrepl("-",kapitler)))){
      kapitler = WildcardGlobbingVector(kapitlerAll,kapitler)
    } else {
      kapitlerIn = kapitler  %in% kapitlerAll
      if(any(!kapitlerIn)){
        StopOrWarning(paste("Disse kapittelene finnes ikke i data eller hierarkier:",
                            paste(kapitler[!kapitlerIn],collapse=", ")))
      }
    }
    
  }
  
  
  
  storkOrder = NULL
  cat("\n [ Kombinasjonsberegninger...")
  flush.console()
  
  # Reduserer storkombinasjoner dersom regioner,kombinasjoner, omfang , kapitler, kontoklasser finnes
  if(!is.null(storkombinasjoner)) {
    if(!is.null(regioner)){
      rr = storkombinasjoner$region %in% regioner
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(!is.null(omfang)){
      rr = storkombinasjoner$regnskapsomfang %in% omfang
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(!is.null(kapitler)){
      rr = storkombinasjoner$kapittel %in% kapitler
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    
    
    if(!is.null(kombinasjoner)){
      rr = !is.na(Match(storkombinasjoner[,names(kombinasjoner) ,drop=FALSE],kombinasjoner))
      storkombinasjoner = storkombinasjoner[rr, ,drop=FALSE]
    }
    if(NROW(storkombinasjoner)==0)
      stop("Matching av storkombinasjoner med annen input gir 0 rader")
  } else{
    if(!is.null(kombinasjoner)) { # Reduserer kombinasjoner dersom omfang , kapitler, kontoklasser finnes
      if(!is.null(omfang)){
        rr = kombinasjoner$regnskapsomfang %in% omfang
        kombinasjoner = kombinasjoner[rr, ,drop=FALSE]
      }
      if(!is.null(kapitler)){
        rr = kombinasjoner$kapittel %in% kapitler
        kombinasjoner = kombinasjoner[rr, ,drop=FALSE]
      }
      
      if(NROW(kombinasjoner)==0)
        stop("Matching av kombinasjoner med annen input gir 0 rader")
    }
  }
  
  
  if(!is.null(storkombinasjoner)){
    if(NROW(storkombinasjoner)==0)
      stop(" 0 storkombinasjoner.")
    
    cat("] [ Fra til storkombinasjoner til kombinasjoner og regioner ...")
    flush.console()
    
    regioner = as.character(storkombinasjoner$region)
    regionerInt = as.integer(factor(regioner))
    regioner = unique(regioner)
    
    cat(" RowGroups(...")
    flush.console()
    
    rgStork = RowGroups(storkombinasjoner[, !(names(storkombinasjoner) %in% "region")],returnGroups = TRUE)
    cat(")")
    flush.console()
    
    storkOrder = matrix(NaN,NROW(rgStork$groups),length(regioner))
    storkOrder[cbind(rgStork$idx,regionerInt)] = seq_len(NROW(storkombinasjoner))
    storkOrder = order(as.vector(storkOrder))[seq_len(NROW(storkombinasjoner))]
    kombinasjoner = rgStork$groups
    rm(rgStork)
    rm(storkombinasjoner) # trengs ikke mer
    
  }
  
  
  if(is.null(kombinasjoner) ){
    cat("] [ Lager kombinasjoner ...")
    flush.console()
    
    
    if(is.null(kapitler))
      kapitler = unique(c(kapittelhierarki$to,kapittelhierarki$from,
                          unique(c(unique(data$kapittel)))))
    if(is.null(omfang))
      omfang = unique(c(omfangshierarki$to))  # input ike her
    
    
    kapitler = data.frame(kapittel = kapitler, stringsAsFactors = FALSE)
    omfang = data.frame(regnskapsomfang = omfang, stringsAsFactors = FALSE)
    
    kombinasjoner = CrossCodeFrames(omfang,kapitler, useMatrixToDataFrame=useMatrixToDataFrame)
    
  }
  
  if(!is.null(kombinasjoner))
    if(NROW(kombinasjoner)==0)
      stop("Noe er galt det er 0 kombinasjoner")
  
  if(!is.null(regioner))
    if(length(regioner)==0)
      stop("Noe er galt det er 0 regioner")
  
  
  
  cat("]\n")
  flush.console()
  
  
  
  if(output=="beregningInput"){ #if(beregningInput){
    print(kombinasjoner)
    if(NROW(kombinasjoner) %in% 2:4 ){
      kombinasjoner[, "regnskapsomfang"] = "A"
      kombinasjoner = unique(kombinasjoner)
    }
    if(NROW(kombinasjoner) != 1)
      stop("Kun en kombinasjon mulig ved beregningInput")
  }
  
  if(output=="beregningHierarki" | output== "beregningHierarkiInput"){
    beregnetHierarki = BeregnetKapittelHierarki(data=data,kapittelhierarki=kapittelhierarki,
                                                kombinasjoner=kombinasjoner,
                                                periode = periode,
                                                regioner=regioner)
    if(output=="beregningHierarki")
      return(beregnetHierarki)
    
    if(TRUE){
      
      beregnetHierarki =  SelectAndRename(beregnetHierarki,
                                          oldNames= c("periode", "region", "kapittel_from", "regnskapsomfang_from", "to", "from", "sign", "belop"),
                                          newNames= c("periode", "region", "kapittel",      "regnskapsomfang",      "to", "from", "sign",    "belop"))
      
      if(length(unique(beregnetHierarki$region)) != 1)
        stop("Må være 1 region når beregningHierarkiInput")
      #  beregnetHierarki  = zBH[[2]][zBH[[2]]$region=="0301", ]
      rgBH =RowGroups(beregnetHierarki[,c("kapittel", "regnskapsomfang")], returnGroups = TRUE, returnGroupsId = TRUE)
      
      
      krMC = BalanseRegnskap(data, kapittelhierarki,    # krMC = BalanseRegnskap(a$data, a$kapittelhierarki,
                             kombinasjoner = rgBH$groups,
                             regioner = unique(beregnetHierarki$region),
                             output = "matrixComponents",
                             printData = FALSE, lag0300 = FALSE,
                             fixRegionkode = FALSE)
      hierarkiInput = cbind(periode=periode, region= unique(beregnetHierarki$region),
                            MultiRowHierarchyComputations(krMC, valueName = "belop", indName = "from",
                                                          ind =  beregnetHierarki$from[rgBH$idg]),
                            stringsAsFactors = FALSE)
      return(list(beregnetHierarki=beregnetHierarki,
                  hierarkiInput= IntegerDataFrame(hierarkiInput[, c("periode", "region", "kapittel", "regnskapsomfang", "sign", "belop", "from")],
                                                  makeWarning = TRUE)))
      
    }
  } # end if(output=="beregningHierarki" | output== "beregningHierarkiInput")
  
  if(output=="matrixComponents" | output=="beregningInput")
    output1 = "matrixComponents"
  else
    output1 = "data.frame"
  
  
  hierarkier = list(kapittel = kapittelhierarki, regnskapsomfang=omfangshierarki, region = "colFactor")
  
  if (length(periode)) {
    constantsInOutput <- data.frame(periode = periode, stringsAsFactors = FALSE)
  } else {
    constantsInOutput <- NULL
  }
  
  if (bidrag) {
    data$row_nr <- seq_len(nrow(data))
    value_var <- "row_nr"
    output1 <- "matrixComponents"
    if (id_var %in% names(data)) {
      data$id <- data[[id_var]]
    } else {
      data$id <- c(paste0("data_row_", seq_len(nrow(data))))
    }
    
  } else {
    value_var <- "belop"
  }
  
  w = HierarchyCompute(data=data,
                       hierarchies=hierarkier,
                       valueVar = value_var,
                       rowSelect = kombinasjoner,
                       colSelect = regioner,
                       autoLevel = TRUE,
                       unionComplement=FALSE,
                       constantsInOutput = constantsInOutput,
                       hierarchyVarNames=c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"),
                       inputInOutput=TRUE,
                       output=output1)
  
  bidragB <- NULL
  if (bidrag) {
    w$valueMatrix <- as.matrix(w$valueMatrix)
    value_matrix_B <- w$valueMatrix
    value_matrix_B[w$valueMatrix > 0] <- data[["belop"]][w$valueMatrix]
    matB <- w$dataDummyHierarchy %*% value_matrix_B
    if (!is.null(fun_id_bidrag)) {
      cat(" [bidrag..")
      flush.console()
      bidragB <- id_bidrag_matrix(Matrix::t(w$dataDummyHierarchy), w$valueMatrix, data, fun_id_bidrag = fun_id_bidrag)
      bidragB <- as.vector(as.matrix(bidragB))
      cat(".]")
      flush.console()
    }
    w <- cbind(data.frame(region = rep(colnames(w$valueMatrix), each = nrow(w$toCrossCode))), 
               w$toCrossCode, 
               belop = as.vector(as.matrix(matB)))
    
    if (length(periode)) {
      w <- cbind(periode = periode, w)
    }
  }
  
  if(output=="matrixComponents")
    return(w)
  
  if(output=="beregningInput")
    return(LagBeregningInputBalanse(a1=w, periode=periode))
  
  if(integerInOutput)
    w$belop = LagInteger(w$belop)
  
  if(!is.null(storkOrder)){
    cat(" [ utvalg til storkombinasjoner...")
    flush.console()
    w = w[storkOrder, ,drop=FALSE]
    if (!is.null(bidragB)) {
      bidragB <- bidragB[storkOrder]
    }
    cat("]\n")
    flush.console()
  }
  
  if (generer_id) {
    w[[id_var]] <- fun_generer_id(nrow(w))
  } 
  if (!is.null(bidragB)) {
    w[[bidrag_var]] <- bidragB
  }
  
  rownames(w) = NULL
  flush.console()
  
  if(printData)
    PrintHeadTail(w, title = "output")
  
  
  return(w)
  
  
}
