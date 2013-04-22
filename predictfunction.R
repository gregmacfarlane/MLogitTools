# Prediction function for mlogit objects

utilities.mlogit <- function(object, alts){
  model.formula <- mFormula(formula(object))
  
  fixed <- attr(object$coefficients, "fixed")
  coefficients <- coef(object)[!fixed]
  coefficientslist <- list()
  
  generics <- coefficients[grep(":", names(coefficients), invert=TRUE)]
  for(i in alts){
    coefficientslist[[i]] <- c(generics, 
                               coefficients[grep(paste(i, ":", sep=""),
                                                names(coefficients))])
    names(coefficientslist[[i]]) <- gsub("^.*?:", "", names(coefficientslist[[i]]))
  }
  
  names(coefficientslist) <- alts
  return(coefficientslist)
}


compute.Utility <- function(mf, utilities, type=c("U", "P")){
  alts <- names(utilities)
  UTILITY <- vector("numeric", length=nrow(mf))
  
  # compute utility
  for(i in alts){
    UTILITY[mf$alt== i] <- 
      apply(sapply(names(mf[names(mf) %in% names(utilities[[i]] ) ]),
                    function(x) mf[mf$alt==i, x] * utilities[[i]][[x]] ),
             1, sum)
  }
  
  if(type=="U") return(UTILITY)
  else{
    # compute probability
    expU <- exp(UTILITY)
    sumU <- aggregate(expU, by=list(mf$ID), sum)
    names(sumU) <- c("ID", "sumU")
    mf <- merge(mf, sumU)
    PROBABILITY <- expU / mf$sumU
    return(PROBABILITY)
  }
}


