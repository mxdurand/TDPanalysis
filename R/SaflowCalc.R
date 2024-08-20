#' Convert tension into sap flow density
#'
#'Use the Granier formula to convert tension into sap flow density using daily or mean Tmax
#' @param tension Vector with tension.
#' @param Tmax Vector with corresponding maximums of tension.
#' @return Return a numerical vector containing the sap flow density
#' @references Granier A. 1985. A new method of sap flow measurement in tree stems. Annales Des Sciences Forestieres 42(2): 193-200. 
#' @references Granier A. 1987. Evaluation of transpiration in a douglas-fir stand by means of sap flow measurements. Tree Physiology 3(4): 309-319.
#' @examples 
#' Tmax = c(rep(2.5, times=5), rep(2.7, times=5), rep(3.2, times=5), rep(3.4, times=5))
#' tension = c(5:25)
#' tens.to.sapflow(tension=tension, Tmax=Tmax)
#' @export
tens.to.sapflow <- function(tension, Tmax) {
  a = 119*10^-6
  b = 1.23
  c = 3600*10
   Sapflow <- a*(((Tmax-tension)/tension)^b)*c
  return(Sapflow)
}

#' Sapwood area calculation
#'
#'Calculate sapwood area based on diameter, heartwood diameter and sapwood fraction 
#' @param diam Vector with diameter.
#' @param SpWd_frac Numerical (from 0 to 1). Indicate the fraction of the diameter which is sapwood
#' @param HtWd_diam Vector with diameter of the heartwood.
#' @return Return a numerical vector containing the sapwood area
#' @details If SpWD_frac and HtWd_diam are both entered, the function will return an error. Units of "diam" and "HtWd_diam" should be the same.
#' @examples 
#' diam = c(12,14,16,13,15)
#' SpWd_Area_calc(diam=diam, SpWd_frac=0.2)
#' @export
SpWd_Area_calc <- function (diam, SpWd_frac = 1, HtWd_diam = 0) {
  if (SpWd_frac == 1 & HtWd_diam == 0) {
    SpWd_Area <- pi*((diam/2)^2)
  } else if (SpWd_frac < 1) {
    SpWd_Area <- pi*((diam/2)^2) - pi*(((diam*SpWd_frac)/2)^2)
  } else if (HtWd_diam > 0) {
    SpWd_Area <- pi*((diam/2)^2) - pi*((HtWd_diam)/2)^2
  }
  return(SpWd_Area)
}

#' Calculate daily transpiration
#'
#'Calculate daily transpiration for each sub-group inputed
#' @param Sapflow Vector with sap flow.
#' @param days Vector containg the days for which to calculate transpiration
#' @param ID Character vector containing identification for each sub-group
#' @return Return a data frame with transpiration for each day and sub-group inputed
#' @details !!Beware of the units!! The Granier formula usually convert tension into sap flow density (in kg.dm-2.h-1). So, you should first convert sap flow density into sap flow (in kg.h-1). Moreover, if you take measurment every 30 minutes sap flow should be corrected by dividing the value by 2.
#' @examples 
#' ID = c(rep("A", times=5), rep("B", times=5), rep("A", times=5), rep("B", times=5))
#' Sapflow = c(rep(2.5, times=5), rep(2.7, times=5), rep(3.2, times=5), rep(3.4, times=5))
#' days = c(rep(102, times=10), rep(103, times=10))
#' Wat.transp(Sapflow=Sapflow, days=days, ID=ID)
#' @export
Wat.transp <- function (Sapflow, days, ID) {
  
  df.fun <- data.frame(Sapflow, days, ID)
  transp.per.day <- vector()
  DOY <- vector()
  Tree <- vector()
  
  for (i in levels(as.factor(df.fun$ID))) {
    sub.i <- subset(df.fun, ID==i)
    for (j in levels(as.factor(sub.i$days))) {
      sub.j <- subset(sub.i, days==j)
      transp <- sum(sub.j$Sapflow, na.rm=T)
      transp.per.day <- append(transp.per.day, values=transp)
      DOY <- append(DOY, values=j)
      Tree <- append(Tree, values=i)
    }
  }
  
  df.transp <- data.frame(Tree, DOY, transp.per.day)
  names(df.transp) <- c("ID", "DOY", "Daily transpiration")
  return(df.transp)
}

#' Sap flow dataset
#'
#'Exemple dataset exemple for the TDPanalysis package
#' @name SpFl
#' @docType data
#' @details "DATE" is dates in dd/mm/yyyy format. "TIME" is time in hh:mm:ss format, "ID" is sub-groups and "tension" is the measured tension from the TDP probe.
"SpFl"
