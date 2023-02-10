


HierarchyFromDummy = function(d){
  x  = data.frame(
    mapsFrom   = colnames(d)[as.vector(col(d))],
    mapsTo = rownames(d)[as.vector(row(d))],
    sign = as.vector(d),
    stringsAsFactors = FALSE)
  x[x$sign!=0 , ,drop=FALSE]
}




RemoveDuplicated = function(x,cols,printAndWarning=TRUE){
  if(printAndWarning){
    dp1 = duplicated(x[,cols, drop=FALSE])
    dp2 = rev(duplicated(x[rev(seq_len(NROW(x))),cols, drop=FALSE]))
    if(any(dp1 | dp2)){
      cat("\n\n:::::::::::::: Warning: Duplicates removed. First in this table kept :::::::::::::::\n")
      print(x[dp2 | dp1,])
      cat("\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::\n\n\n")
      warning("Duplicates removed (first in printed table kept)")
    }
  }
  x[!duplicated(x[,cols, drop=FALSE]),]
}




LagInteger = function(x){
  if(!is.numeric(x)){
    return(x)
  }
  z = as.integer(x)
  if(identical( as.numeric(as.vector(z)),as.numeric(as.vector(x))))
    return(z)
  warning("Ikke integer i output siden omgjøring til integer ikke ga identisk resultat")
  return(x)
}




#' PrintHeadTail
#'
#' @param x data.frame
#' @param n n
#' @param title title
#'
#' @return None (invisible NULL)
#' @export
#' @keywords internal
#'
PrintHeadTail = function(x,n=2, title = NULL){
  cat("\n")

  if(!is.null(title))
    cat(title,"\n")
  else
    print(as.list(match.call())[[2]])

  if(is.null(x))
    print(NULL)
  else{
    if(!is.data.frame(x)  & is.list(x)){
      z = vector("list", length(x))
      names(z) = names(x)
      for(i in seq_len(length(x))){
        if(is.null(x[[i]])){
          z[[i]] = NULL
        } else {
          z[[i]] = as.data.frame(x[[i]])
          rownames(z[[i]]) = NULL
          rows = seq_len(NROW(x[[i]]))
          rows = unique(c(head(rows,n),tail(rows,n)))
          z[[i]] = z[[i]][rows, ,drop=FALSE]
        }
      }
      print(z)
    } else {
      x = as.data.frame(x)
      rownames(x) = NULL
      rows = seq_len(NROW(x))
      rows = unique(c(head(rows,n),tail(rows,n)))
      print(x[rows, ,drop=FALSE])
    }
  }
  cat("\n")
}


#' AutoNetting
#'
#' Ulike varianter som ble utprøvd. AutoNettingCopy er løsningen som ble valgt.
#'
#' @param nettinghierarki nettinghierarki
#' @param hierarki hierarki
#' @param title title
#'
#' @return Ny versjon av nettinghierarki
#' @export
#' @keywords internal
#'
AutoNetting = function(nettinghierarki,hierarki, title ="nettinghierarki", toRemove = TRUE, singleLoop=FALSE){
  if(toRemove)
    hierarki  = hierarki[!(hierarki$to %in% nettinghierarki$to), ,drop=FALSE]

  n1 = NROW(nettinghierarki)

  loop = TRUE
  while(loop){
    hierarkiA  = hierarki[(hierarki$from %in% nettinghierarki$to), ]
    hierarkiB  = hierarki[!(hierarki$from %in% nettinghierarki$to), ]
    #print(hierarkiA)
    loop = NROW(hierarkiA)>0
    hierarki = hierarkiB
    nettinghierarki =rbind(nettinghierarki,hierarkiA)
    if(singleLoop) loop=FALSE
  }
  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}



#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingNy = function(nettinghierarki,hierarki, title ="nettinghierarki",loop=TRUE){

  n1 = NROW(nettinghierarki)

  hierarkiW  = hierarki[(hierarki$from %in% nettinghierarki$to), ,drop=FALSE]
  hierarkiW$from = paste("w",hierarkiW$from,sep="_")

  hTo = unique(nettinghierarki$to)
  netthTo = nettinghierarki[rep(1,length(hTo)), ,drop=FALSE]
  netthTo$from = paste("w",hTo,sep="_")
  netthTo$to   = hTo
  netthTo$sign   = "+"

  nettinghierarki$to= paste("w",nettinghierarki$to,sep="_")

  nettinghierarki = rbind(nettinghierarki,netthTo,hierarkiW)


  hierarki  = hierarki[!(hierarki$to %in% nettinghierarki$to), ,drop=FALSE]


  #loop = TRUE
  while(loop){
    hierarkiA  = hierarki[(hierarki$from %in% nettinghierarki$to), ]
    hierarkiB  = hierarki[!(hierarki$from %in% nettinghierarki$to), ]
    #print(hierarkiA)
    loop = NROW(hierarkiA)>0
    hierarki = hierarkiB
    nettinghierarki =rbind(nettinghierarki,hierarkiA)
  }
  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}




#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingIngenEndering = function(nettinghierarki,hierarki=NULL, title = NULL){
  nettinghierarki
}



#' @rdname AutoNetting
#' @export
#' @keywords internal
AutoNettingCopy = function(nettinghierarki,hierarki, title ="nettinghierarki",copyKode = "COPY"){

  n1 = NROW(nettinghierarki)

  rCopy = nettinghierarki$from == copyKode
  toCopy = nettinghierarki$to[rCopy]

  nettinghierarkiTo = unique(nettinghierarki$to)

  nettinghierarki = nettinghierarki[!rCopy, ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$to %in% toCopy), ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$from %in% nettinghierarkiTo), ]

  nettinghierarki =rbind(nettinghierarki,hierarki)

  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}


AutoNettingCopyOld = function(nettinghierarki,hierarki, title ="nettinghierarki",copyKode = "COPY"){

  n1 = NROW(nettinghierarki)

  rCopy = nettinghierarki$from == copyKode
  toCopy = nettinghierarki$to[rCopy]

  nettinghierarki = nettinghierarki[!rCopy, ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$to %in% toCopy), ,drop=FALSE]

  hierarki  =  hierarki[(hierarki$from %in% nettinghierarki$to), ]

  nettinghierarki =rbind(nettinghierarki,hierarki)

  n2 = NROW(nettinghierarki)

  cat("\n AutoNetting: ", title, " fra ", n1, " til ", n2, " rader\n")
  nettinghierarki
}









#' Sørger for at regiontall blir character med 4 eller 6 plasser og ledende nuller
#'
#' Input som ikke representerer tall forblir uendret, bortsett fra eventuell omgjøring ved \code{\link{as.character}}.
#'
#' Funksjonen kaller \code{\link{AddLeadingZeros}} med \code{places=6} for store tall (\code{>9999}) og ellers med \code{places=4}.
#'
#' @param region Vektor med regionkoder. Kan være  integer, numeric, character eller factor.
#' @param warningText Ved ikke-NULL skrives warning med warningText dersom endring gjøres.
#'
#' @return Endrede regionkoder som en character vektor
#'
#' @note Funksjonen er ekstra grundig dokumentert siden den brukes som eksempel i workshop i bygging av  r-pakker.
#'
#' @seealso \code{\link{Number}}
#'
#' @importFrom SSBtools AddLeadingZeros
#'
#' @export
#' @author Øyvind Langsrud
#' @examples
#' FixRegionkode(c(101, 301, 30107))
#' FixRegionkode(c("101", "301", "30107"))
#' FixRegionkode(c("101", "301", "30107", "Oslo"))
#' FixRegionkode(c("101", "301", "030107"), "Regionskoder endret")
#' FixRegionkode(c("0101", "0301", "030107"), "Regionskoder endret")
#'
FixRegionkode <- function(region, warningText = NULL) {
  region <- as.character(region)
  seksSiffer <- suppressWarnings(as.integer(region)) > 9999
  fireSiffer <- !seksSiffer
  seksSiffer[is.na(seksSiffer)] <- FALSE
  fireSiffer[is.na(fireSiffer)] <- FALSE
  region[fireSiffer] <- AddLeadingZeros(region[fireSiffer], 4, warningText = warningText, viaFactor = TRUE)
  region[seksSiffer] <- AddLeadingZeros(region[seksSiffer], 6, warningText = warningText, viaFactor = TRUE)
  region
}





















