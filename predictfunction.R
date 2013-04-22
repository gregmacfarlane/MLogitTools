# Prediction function for mlogit objects


# This function gets the utility equations from an mlogit object.
# - inputs: estimated mlogit output, list of J alternatives.
# - outputs: a list of length J with the elements for a utility equations
utilities.mlogit <- function(object, alts){
	# pull formula from mlogit object
  model.formula <- mFormula(formula(object))
  
	# get list of coefficients
  fixed <- attr(object$coefficients, "fixed")
  coefficients <- coef(object)[!fixed]
  coefficientslist <- list()
  
	# Generic coefficients
  generics <- coefficients[grep(":", names(coefficients), invert=TRUE)]
  for(i in alts){
		# join generics to alternative-specifics
    coefficientslist[[i]] <- c(generics, 
                               coefficients[grep(paste(i, ":", sep=""),
                                                names(coefficients))])
    names(coefficientslist[[i]]) <- gsub("^.*?:", "", names(coefficientslist[[i]]))
  }
  
  # name list elements with alternatives 
  names(coefficientslist) <- alts
  return(coefficientslist)
}

# This function computes either the utility for each alternative and each
# observation, or the probabilities corresponding to those alternatives.
# - inputs: a model frame with columns named with the coefficients of the
# utility equations, a list of utility equations (as from `utilities.mlogit`),
# and whether the output should be a utility or a probability.
# - outputs: either the utility for each row, or the probability.
compute.Utility <- function(mf, utilities, type=c("U", "P")){
  alts <- names(utilities)
  UTILITY <- vector("numeric", length=nrow(mf))
  
  # compute utility
  for(i in alts){
		# apply the appropriate utility equation to each row
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


