#' Flatt hierarki
#' 
#' Omkoder hierarki slik at alle from-koder er koder fra input-data
#'
#' @param hierarki hierarki 
#' @param copy_hierarki Ekstra hierarki som skal brukes når `hierarki` er et nettinghierarki som skal utvides basert på from-koden `"COPY"`.
#' @param periode Ved flere perioder i `hierarki`, må en periode velges. 
#' @param fix Ved `TRUE` (default): Sørger for blanke i starten/slutten fjernes og at koder som er tall får 3 plasser og ledende nuller (gir warning ved endring av input).
#' @param flatt Hvorvidt flatt hierarki skal returneres. Ved `FALSE` returneres resultat etter `fix` og bruk av `copy_hierarki`.
#' @param g Flatt hierarki begrenses til `abs(sign)>g`.
#'
#' @return Et (flatt) hierarki 
#' @export
#' 
#' @seealso \code{\link{ett_flatt_hierarki}}
#'
#' @examples
#' kdata <- kr_data("kostraRegnskapDataPen")
#' funksjonshierarki <- kdata$funksjonshierarki
#' artshierarki <- kdata$artshierarki          
#' inputdata_saer <- kdata$data_saer
#' artshierarki_nettinger <- kdata$artshierarki_nettinger
#' artshierarki_nettinger_kasse <- kdata$artshierarki_nettinger_kasse
#' 
#' head(flatt_hierarki(funksjonshierarki, periode = "2015"))
#' flatt_hierarki(artshierarki, periode = "2016", g = 2)
#' flatt_hierarki(artshierarki_nettinger, artshierarki, periode = "2016", g = 1)
#' head(flatt_hierarki(artshierarki_nettinger_kasse, artshierarki, periode = "2016", flatt = FALSE))
flatt_hierarki <- function(hierarki, copy_hierarki = NULL, periode = NULL, 
                           fix = TRUE, flatt = TRUE, g = 0) {
  if (!is.null(periode)) {
    hierarki <- hierarki[hierarki$periode == periode, , drop = FALSE]
  } else {
    if (!is.null(hierarki$periode)) {
      if (length(unique(hierarki$periode) > 1)) {
        stop("Bruk periode-parameter når det er flere perioder")
      }
    }
  }
  if (fix) {
    hierarki$from <- AddLeadingZeros(hierarki$from, 3, warningText = "Koder endret")
  }
  if (!is.null(copy_hierarki)) {
    if (!is.null(periode)) {
      copy_hierarki <- copy_hierarki[hierarki$periode == periode, , drop = FALSE]
    }
    if (fix) {
      copy_hierarki$from <- AddLeadingZeros(copy_hierarki$from, 3, warningText = "Koder endret")
    }
    hierarki <- AutoNettingCopy(hierarki, copy_hierarki)
  }
  if (!flatt) {
    return(hierarki)
  }
  hi <- HierarchyFromDummy(DummyHierarchies(AutoHierarchies(list(a = hierarki), 
        hierarchyVarNames = c(mapsFrom = "from", mapsTo = "to", sign = "sign", level = "level")))$a)
  names(hi)[names(hi) == "mapsFrom"] <- "from"
  names(hi)[names(hi) == "mapsTo"] <- "to"
  hi <- hi[abs(hi$sign) > g, , drop = FALSE]
  rownames(hi) <- NULL
  hi
}


#' Ett flatt artshierarki
#' 
#' artshierarki, artshierarki_nettinger og artshierarki_nettinger_kasse 
#' settes sammen til ett hierarki. Deretter genereres flatt hierarki fra dette.  
#' 
#' 
#' Underliggende funksjon er samme funksjon som brukes til beregningsteter. 
#' Et unntak er at denne funksjonen ikke håndterer formler. 
#'
#' @param artshierarki artshierarki 
#' @param artshierarki_nettinger artshierarki_nettinger 
#' @param artshierarki_nettinger_kasse artshierarki_nettinger_kasse 
#' @param periode Ved flere perioder i hierarkier, må en periode velges.
#' @param fix Ved `TRUE` (default): Som `fix` i \code{\link{flatt_hierarki}}
#'            samt utvidelser basert på from-koden `"COPY"`
#' @param flatt Hvorvidt flatt hierarki skal returneres. 
#'              Ved `FALSE` returneres resultat etter `fix` og
#'              kombinering til ett hierarki.
#' @param g Flatt hierarki begrenses til `abs(sign)>g`.
#'
#' @return Ett (flatt) hierarki kodet på spesiell måte slik at alt samles.
#' @export
#' 
#' @seealso \code{\link{flatt_hierarki}}
#'
#' @examples
#' kdata <- kr_data("kostraRegnskapDataPen") 
#' funksjonshierarki <- kdata$funksjonshierarki 
#' artshierarki <- kdata$artshierarki          
#' inputdata_saer <- kdata$data_saer
#' artshierarki_nettinger <- kdata$artshierarki_nettinger
#' artshierarki_nettinger_kasse <- kdata$artshierarki_nettinger_kasse
#' 
#' head(ett_flatt_hierarki(artshierarki, artshierarki_nettinger, 
#'              artshierarki_nettinger_kasse, periode = "2015", g = 1))
#' head(ett_flatt_hierarki(artshierarki, artshierarki_nettinger, 
#'              artshierarki_nettinger_kasse, periode = "2015", flatt = FALSE))
#' ett_flatt_hierarki(artshierarki, artshierarki_nettinger, 
#'              artshierarki_nettinger_kasse, periode = "2016", g = 2)
ett_flatt_hierarki <- function(artshierarki, artshierarki_nettinger, 
                               artshierarki_nettinger_kasse, 
                               periode = NULL, fix = TRUE, flatt = TRUE, g = 0) {
  if (!is.null(periode)) {
    artshierarki <- artshierarki[artshierarki$periode == periode, , drop = FALSE]
    artshierarki_nettinger <- artshierarki_nettinger[artshierarki_nettinger$periode == periode, , drop = FALSE]
    artshierarki_nettinger_kasse <- artshierarki_nettinger_kasse[artshierarki_nettinger_kasse$periode == periode, , drop = FALSE]
  } else {
    if (!is.null(artshierarki$periode)) {
      if (length(unique(artshierarki$periode) > 1)) {
        stop("Bruk periode-parameter når det er flere perioder")
      }
    }
  }
  hi <- EttHierarki(artshierarki, artshierarki_nettinger, artshierarki_nettinger_kasse, fixHierarchy = fix)
  names(hi)[names(hi) == "mapsFrom"] <- "from"
  names(hi)[names(hi) == "mapsTo"] <- "to"
  flatt_hierarki(hi, fix = FALSE, flatt = flatt, g = g)
}
