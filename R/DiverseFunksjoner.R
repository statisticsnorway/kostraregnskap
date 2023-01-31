


#' Change numeric variables in data frame to integer
#'
#' Only done when the integer values are exact the same as the original values
#'
#' @param x data frame
#' @param makeWarning When TRUE warning made when not all numeric variables could be changed
#' @param allNumeric When TRUE no change is made unless all could be changed
#'
#' @return data frame
#' @export
#'
IntegerDataFrame = function(x,makeWarning = FALSE, allNumeric =TRUE){
  #cat("\n [ IntegerDataFrame ...")
  #flush.console()

   toInteger = rep(FALSE,NCOL(x))
  notInteger = rep(FALSE,NCOL(x))

  for (i in seq_len(NCOL(x))){
   if(is.numeric(x[[i]]))
     if(!is.integer(x[[i]])){
       if(identical( as.numeric(as.vector(as.integer(x[[i]]))),as.numeric(as.vector(x[[i]]))))
         toInteger[i] = TRUE
       else
         notInteger[i] = TRUE
     }
  }

  #cat(paste(names(x)[toInteger],collapse="+"))
  #flush.console()


  if(allNumeric){
    if(any(notInteger)){
      if(makeWarning){
            warning(paste("Integer not forced for any variable since identical result not obtained for these variables:",
                          paste(names(x)[notInteger],collapse=", ")))
      }
    } else {
      if(any(toInteger))
        for(i in which(toInteger))
          x[[i]] = as.integer(x[[i]])
    }

  } else{
    if(any(notInteger)){
      if(makeWarning){
        warning(paste("Integer not forced for some variables since identical result not obtained:",
                      paste(names(x)[notInteger],collapse=", ")))
      }
    }
    if(any(toInteger))
      for(i in which(toInteger))
        x[[i]] = as.integer(x[[i]])
  }
  #cat("]\n")
  #flush.console()
  x
}



SelectAndRename = function(data, oldNames, newNames=NULL){
  z = data[, oldNames,drop=FALSE]
  names(z) = newNames
  z
}

#' CharacterReCode
#'
#' @param x  (character) vector
#' @param oldLevels oldLevels
#' @param newLevels newLevels
#'
#' @return character vector
#' @export
#'
#' @examples
#' CharacterReCode(rep(1:4,2),c("2","3"),c("B","C"))
#'
CharacterReCode = function(x,oldLevels, newLevels){
  x = as.factor(x)
  le = levels(x)
  ma = match(le,oldLevels)
  isMatch = !is.na(ma)
  le[isMatch] = newLevels[ma[isMatch]]
  levels(x) = le
  as.character(x)
}




# HeadEnd(1:1000) '1' '2' '3' '4' '...'  '1000'
HeadEnd <- function(x, n = 4L) {
  x <- as.character(x)
  if (length(x) > (n + 2))
    x <- c(head(x, n = n), "...", tail(x, n = 1))
  x
}









