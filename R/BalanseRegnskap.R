#' @rdname BalanseRegnskap
#' @note BalanseRegnskapBeregningInput og BalanseRegnskapBeregningHierarki går via funksjonen BalanseRegnskapBeregning.
#' Parametere til den funksjonen trengs bortsett fra «output».
#' Spesielt må altså parameterne kapittelhierarki og kapitler med.
#' Det skal (i utgangspunktet) være en verdi på hver av disse.
#' Funksjonen BalanseRegnskapBeregningHierarkiALT er alternativ til BalanseRegnskapBeregningHierarki med andre variabler i output.
#' @export
BalanseRegnskapBeregning = function(data,kapittelhierarki,
                                   kapitler,
                                   ...,
                                   perioder = NULL, output){

  BalanseRegnskap(data=data,kapittelhierarki=kapittelhierarki, kapitler=kapitler, ..., perioder=perioder,
                 output = output)
}


CharacterReCodeRegnskapsomfang = function(x){
  x$regnskapsomfang = CharacterReCode(x$regnskapsomfang, c("A","B"), c("konsern", "kasse"))
  x
}


#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningInput = function(...){
  CharacterReCodeRegnskapsomfang(BalanseRegnskapBeregning(output = "beregningInput", ...))
}

#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningHierarkiALT = function(...){
  BalanseRegnskapBeregning(output = "beregningHierarki", ...)
}



#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningHierarki = function(...){
  CharacterReCodeRegnskapsomfang(SelectAndRename(BalanseRegnskapBeregning(output = "beregningHierarki", ...),
                  oldNames= c("periode", "region", "kapittel_from", "regnskapsomfang_from", "to", "from", "sign", "belop"),
                  newNames= c("periode", "region", "kapittel",      "regnskapsomfang",      "to", "from", "sign",    "belop")))
}


#' @rdname BalanseRegnskap
#' @export
BalanseRegnskapBeregningHierarkiInput = function(...){
  CharacterReCodeRegnskapsomfang(BalanseRegnskapBeregning(output = "beregningHierarkiInput", ...)$hierarkiInput)
}



#' @rdname BalanseRegnskap
#' @param perioder Perioder for beregningen. Ved NULL (default) brukes alle perioder som finnes i data.
#' @param ... Parametere som sendes videre til BalanseRegnskapEnPeriode
#' @export
BalanseRegnskap = function(data,kapittelhierarki,
                          ...,
                          perioder = NULL){
  if(is.null(perioder)){
    perioder = sort(unique(as.character(data$periode)))
    if(length(perioder) == 0)
      stop("periode finnes ikke i data")
  } else {
    perioder = unique(as.character(perioder))
    SjekkDataPerioder(data, "data", perioder)
  }

  SjekkDataPerioder(kapittelhierarki, "kapittelhierarki", perioder)

  n = length(perioder)
  a <- vector("list",n)
  for(i in seq_len(n)){
    if(n>1){
      cat("================================================================\n")
      cat("==============    periode = ",perioder[i],"    ================\n")
      cat("==============================================================\n")
    }
    ai =  BalanseRegnskapEnPeriode(data = DataPeriode(data,perioder[i]),
                                  kapittelhierarki = DataPeriode(kapittelhierarki,perioder[i]),
                                  ...)
    if(n>1) a[[i]] = ai
  }
  if(n>1)
    return(RbindAll(a))
  ai
}



#' BalanseRegnskap
#'
#' Funksjon som ligner på KostraRegnskap, men mye enklere.
#'
#' @rdname BalanseRegnskap
#'
#' @encoding UTF8
#' 
#' @inheritParams balanse_regnskap
#'
#' @return Data frame med samme variabler som data i input og med kapitler/regnskapsomfang i henhold til annen input
#' @export
#'
#' @seealso \code{\link{KostraRegnskap}}, \code{\link{HierarchyCompute}}, \code{\link{HierarchicalWildcardGlobbing}}
#'
#' @examples
#'
#' # Her brukes data som ligger i pakken der bare noen regioner er med.
#'
#' bData <- kr_data("balanseRegnskapDataPen") # kr_data("balanseRegnskapDataPen") er alternativ der
#' inputdata <- bData$data                    #    automatisk omkoding trengs (som fixRegionkode)
#' hierarki <- bData$kapittelhierarki         #    Fungerer like bra, men med flere warning
#'
#' z <- BalanseRegnskap(inputdata, hierarki, regioner = c("2021", "2022", "0301", "1100"))
#' xA <- BalanseRegnskapBeregningInput(inputdata, hierarki, regioner = "0301", omfang = "A", 
#'                                     kapitler = "KG3")
#' xB <- BalanseRegnskapBeregningInput(inputdata, hierarki, regioner = "0301", omfang = "B", 
#'                                     kapitler = "KG3")
#' yA <- BalanseRegnskapBeregningHierarki(inputdata, hierarki, regioner = "0301", omfang = "A", 
#'                                     kapitler = "KG3")
#' yB <- BalanseRegnskapBeregningHierarki(inputdata, hierarki, regioner = "0301", omfang = "B", 
#'                                     kapitler = "KG3")
BalanseRegnskapEnPeriode = function(data,kapittelhierarki,
                          kombinasjoner=NULL,
                          regioner=NULL,
                          storkombinasjoner=NULL,
                          kapitler  = NULL,
                          omfang = NULL,
                          output="en",
                          printData = TRUE,
                          lag0300 = FALSE,
                          fixRegionkode = TRUE
){
  balanse_regnskap(data = data,
                   kapittelhierarki = kapittelhierarki,
                   kombinasjoner =  kombinasjoner,
                   regioner = regioner,
                   storkombinasjoner = storkombinasjoner,
                   kapitler  = kapitler,
                   omfang = omfang,
                   output = output,
                   printData = printData,
                   lag0300 = lag0300,
                   fixRegionkode = fixRegionkode)
}


LagBeregningInputBalanse = function(a1, periode, doStack = TRUE){
  x = SingleRowHierarchyComputations(a1, doStack = doStack,  valueName = "belop", indName = "region")
  x$region <- FixRegionkode(x$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  cbind(periode=periode, IntegerDataFrame(x, makeWarning = TRUE), stringsAsFactors = FALSE)[,c("periode", "region", "kapittel", "regnskapsomfang",  "sign", "belop")]
}


LagBeregningInputBalanseOld = function(a1, periode, doStack = TRUE){
  print("Gammel funksjon")
  fromCrossCode = a1$fromCrossCode

  dataDummyHierarchy =
    t(as.matrix(a1$dataDummyHierarchy))

  valueMatrix =
    as.matrix(a1$valueMatrix)

  nonZero = rowSums(abs(valueMatrix))>0  & abs(dataDummyHierarchy)>0

  z = cbind(fromCrossCode, sign = dataDummyHierarchy, valueMatrix)

  output1 = as.matrix(a1$dataDummyHierarchy%*%a1$valueMatrix)

  out1 = cbind(a1$toCrossCode, sign = 0,output1, stringsAsFactors = FALSE)

  x = cbind(periode=periode, IntegerDataFrame(rbind(
    out1, #
    z[nonZero, ,drop=FALSE]),makeWarning = TRUE), stringsAsFactors = FALSE)

  rownames(x) = NULL


  if(doStack){
    x <- Stack(x,stackVar=5:NCOL(x),blockVar = 1:4,  valueName = "belop",indName = "region")[,c("periode", "region", "kapittel", "regnskapsomfang",  "sign", "belop")]
  }
  x$region <- FixRegionkode(x$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")
  x

}



NyNavnHierarki = function(hierarki, pre="A",sep="_"){
  hierarki$kapittel_to = hierarki$mapsTo
  hierarki$kapittel_from = hierarki$mapsFrom
  hierarki$mapsTo = paste(pre, hierarki$mapsTo, sep=sep)
  hierarki$mapsFrom = paste(pre, hierarki$mapsFrom, sep=sep)
  hierarki$regnskapsomfang_to = pre
  hierarki$regnskapsomfang_from = pre
  hierarki$hierarki = pre
  hierarki
}


EttBalanseHierarki = function(hierarki, fixHierarchy = TRUE, omfanger  = c("A","B","sbedr","lanefond")){

  if(fixHierarchy)
  hierarki = HierarchyFix(hierarki,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)

  hierarkiC = NULL
  hierarkiA = NULL


  for(i in seq_len(length(omfanger)-1)){
    hierarkiC = rbind(hierarkiC ,NyNavnHierarki(hierarki,omfanger[i+1]))
    hierarkiA = rbind(hierarkiA ,NyNavnHierarki(hierarki,omfanger[1]))
  }

  hierarkiA$mapsFrom = hierarkiC$mapsTo
  hierarkiA$kapittel_from = hierarkiC$kapittel_to
  hierarkiA$regnskapsomfang_from = hierarkiC$regnskapsomfang_to
  hierarkiA$sign = 1L
  hierarkiA = unique(hierarkiA)


  hi_Kon_mapsTo = unique(hierarki$mapsTo)
  hihi_Kon = hierarkiA[rep(1,length(hi_Kon_mapsTo)), ,drop=FALSE]
  hihi_Kon$hierarki = "OUTPUT"
  hihi_Kon$regnskapsomfang_from = "A"
  hihi_Kon$regnskapsomfang_to = "A"
  hihi_Kon$kapittel_to = hi_Kon_mapsTo
  hihi_Kon$kapittel_from = hi_Kon_mapsTo
  hihi_Kon$mapsTo = paste("KONSERN_",hi_Kon_mapsTo,sep="")
  hihi_Kon$mapsFrom   = paste("A_",hi_Kon_mapsTo,sep="")
  hihi_Kon$sign = 1L


  rbind(hierarkiC,hierarkiA,hihi_Kon)
}

EttBalanseHierarkiB = function(hierarki, fixHierarchy = TRUE){

  if(fixHierarchy)
    hierarki = HierarchyFix(hierarki,c(mapsFrom="from", mapsTo ="to", sign="sign", level="level"), autoLevel = FALSE)

  hierarkiB = NyNavnHierarki(hierarki,"B")

  hi_Kon_mapsTo = unique(hierarki$mapsTo)
  hihi_Kon = hierarkiB[rep(1,length(hi_Kon_mapsTo)), ,drop=FALSE]
  hihi_Kon$hierarki = "OUTPUT"

  hihi_Kon$kapittel_to = hi_Kon_mapsTo
  hihi_Kon$kapittel_from = hi_Kon_mapsTo
  hihi_Kon$mapsTo = paste("KASSE_",hi_Kon_mapsTo,sep="")
  hihi_Kon$mapsFrom   = paste("B_",hi_Kon_mapsTo,sep="")
  hihi_Kon$sign = 1L

  rbind(hierarkiB,hihi_Kon)
}


BeregnetKapittelHierarki =
  function(data,kapittelhierarki,
           kombinasjoner=NULL,
           kapitler= NULL,
           periode = "2016",
           regioner=NULL)
  {

  if(is.null(kapitler))
    kapitler= unique(kombinasjoner$kapittel)


  regnskapsomfang = unique(kombinasjoner$regnskapsomfang)

  onlyB = FALSE
  if(length(regnskapsomfang)==1)
    if(regnskapsomfang=="B")
      onlyB = TRUE

  onlyB = FALSE #################   Kutter bruk av EttBalanseHierarkiB ....  det virker for "sbedr" og "lanefond" også (filtrerer til slutt)

  if(onlyB)
    hiRelevant = RelevantHierarki(EttBalanseHierarkiB(kapittelhierarki),paste("KASSE",kapitler,sep="_"))
  else
    hiRelevant = RelevantHierarki(EttBalanseHierarki(kapittelhierarki),paste("KONSERN",kapitler,sep="_"))

  if(NROW(hiRelevant)==0)
    stop(paste("Det blir ingenting med utgangspunkt i", paste(HeadEnd(kapitler),collapse=", ")))

  #return(hiRelevant)

  data$kapittel = paste(data$regnskapsomfang,data$kapittel,sep="_")


  w=HierarchyCompute(data, list(kapittel = hiRelevant, region = "colFactor"), "belop",
                     rowSelect = data.frame(kapittel = hiRelevant$mapsFrom),colSelect = regioner,
                     unionComplement = FALSE,
                     output = "outputMatrixWithCrossCode", colNotInDataWarning=FALSE)


  ma = match(hiRelevant$mapsFrom,w[[2]]$kapittel)



  hierarki_from = paste("Input",hiRelevant$hierarki,sep="_")



  hierarki_from_ind = match(hiRelevant$mapsFrom,hiRelevant$mapsTo)

  hierarki_from[!is.na(hierarki_from_ind)] =  hiRelevant$hierarki[hierarki_from_ind[!is.na(hierarki_from_ind)]]


  z = cbind(cbind(periode=periode,hierarki_from=hierarki_from,hiRelevant,stringsAsFactors=FALSE ))



  #### LAger output med samme variabler som formelhierarki..
  names(z)[names(z)=="mapsFrom"] = "from"
  names(z)[names(z)=="mapsTo"] = "to"

  names(z)[names(z)=="hierarki"] = "type_to"
  names(z)[names(z)=="hierarki_from"] = "type_from"



  z$sign[z$sign=="1"] = "+"
  z$sign[z$sign=="-1"] = "-"

  z = cbind(z,as.matrix(w[[1]][ma, ,drop=FALSE]) ,stringsAsFactors=FALSE)

  z = IntegerDataFrame(z)
  rownames(z) = NULL

  doStack = TRUE
  if(doStack){   # level er ikke med
      z <- Stack(z,stackVar=11:NCOL(z),blockVar = 1:10,  valueName = "belop",indName = "region") [,c("periode", "region",
                                                                                                   "kapittel_to", "kapittel_from",
                                                                                                   "regnskapsomfang_to", "regnskapsomfang_from",
                                                                                                   "to", "from",
                                                                                                   "sign",
                                                                                                   "type_to", "type_from",
                                                                                                   "belop")]

  }
  z$region <- FixRegionkode(z$region, warningText = "Uventet problem med regionkoder etter Stack ved beregningstester")

  if("A" %in% regnskapsomfang)
    return(z)

  z[z$regnskapsomfang_from %in% regnskapsomfang, ]
}













