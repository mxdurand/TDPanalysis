#' Find Tmax
#'
#'Find the daily maximum of tension 
#' @param tension Vector with tension.
#' @param dates Vector with dates in the DOY format.
#' @param ID  Character vector for specifying which group the tension is assigned to (e.g. trees)
#' @return Return a vector containing daily Tmax for each group specified in the ID argument
#' @import plyr
#' @examples 
#' tension = c(1:20)
#' dates = c(rep(102, times=10), rep(103, times=10))
#' ID = c(rep("A", times=5), rep("B", times=5), rep("A", times=5), rep("B", times=5))
#' Tmax.find(tension=tension, dates=dates, ID=ID)
#' @export
Tmax.find <- function (tension, dates, ID) {
    df <- data.frame(ID, dates, tension)
    df.Tmax <- ddply(df, .(ID,dates), summarize, Tmax = max(tension, na.rm=T))
    names(df.Tmax) <- c("ID", "DOY", "Tmax")
  return(df.Tmax)
}

#' Plot the Tmax
#'
#'Plot the Tmax with indications of extreme values
#' @param df Data frame containing Tmax, identification of sub-groups and DOY.
#' @return Return a plot of Tmax by days for each sub-group
#' @details The dataframe should contain at least 3 columns named "Tmax" (daily maximums of tension), "DOY" (day of the year) and "ID" (sub-groups). The red horizontal lines reprensents 3 times the inter-quartile range (3*IQR) of all the Tmax of the data. The blue horizontal line reprensent the 1.5*IQR without the Tmax outside the red lines.
#' @import stats
#' @import graphics
#' @examples 
#' DOY = c(rep(102, times=10), rep(103, times=10))
#' ID = c(rep("A", times=5), rep("B", times=5), rep("A", times=5), rep("B", times=5))
#' Tmax = c(rep(0.7512, times=5), rep(0.7359, times=5),rep(0.7644, times=5),rep(0.7666, times=5))
#' df <- data.frame(DOY, ID, Tmax, stringsAsFactors = FALSE)
#' Tmaxplot(df)
#' @export
Tmaxplot <- function (df) {
  
  for (i in 1:length(unique(df$ID))){
    sub.i <- df[df$ID == unique(df$ID)[i],]
    
    IQR3 <- 3*((quantile(sub.i$Tmax)[4])-quantile(sub.i$Tmax)[2])
    moy3 <- mean(sub.i$Tmax)
    
    plot(sub.i$DOY, sub.i$Tmax, ylim=c(moy3*0.9,moy3*1.1), pch=16, col="red", type="o", main=paste(unique(df$ID)[i]), xlab="DOY", ylab="Tmax")
    abline(h=moy3+IQR3, col="red")
    abline(h=moy3-IQR3, col="red")
    
    sub.i <- sub.i[!sub.i$Tmax<=as.numeric(moy3-IQR3),]
    sub.i <- sub.i[!sub.i$Tmax>=as.numeric(moy3+IQR3),]
    
    IQR15 <- 1.5*((quantile(sub.i$Tmax)[4])-quantile(sub.i$Tmax)[2])
    moy15 <- mean(sub.i$Tmax)
    
    points(sub.i$DOY, sub.i$Tmax, ylim=c(moy3*0.9,moy3*1.1), pch=16, col="blue", type="o", main=paste(unique(df$ID)[i]))
    abline(h=moy15+IQR15, col="blue")
    abline(h=moy15-IQR15, col="blue")
    
    sub.i <- sub.i[!sub.i$Tmax<=as.numeric(moy15-IQR15),]
    sub.i <- sub.i[!sub.i$Tmax>=as.numeric(moy15+IQR15),]
    
    points(sub.i$DOY, sub.i$Tmax, ylim=c(moy3*0.9,moy3*1.1), pch=16, type="o", main=paste(unique(df$ID)[i]))
    legend("topright", bty="n", legend=c("Points kept", "Points filtered by 1.5*IQR", "Points filtered by 3*IQR"), col=c("black", "blue","red"), pch=16, lty=1, lwd=1)
  }
}

#' Remove unwanted dates
#'
#'Remove all data for the corresponding date argument
#' @param df Data frame containing a DOY column named "DOY".
#' @param dates Character vector containing the DOY to remove from the data frame.
#' @return Return the inputed data frame without the date corresponding the the "dates" argument. 
#' @details This function is primarely used to remove days for which Tmax is too extreme.
#' @examples 
#' DOY = c(rep(102, times=10), rep(103, times=10))
#' ID = c(rep("A", times=5), rep("B", times=5), rep("A", times=5), rep("B", times=5))
#' Tmax = c(rep(2.5, times=5), rep(2.7, times=5), rep(3.2, times=5), rep(3.4, times=5))
#' df <- data.frame(DOY, ID, Tmax)
#' dates = c("103")
#' remove.fun(df=df, dates=dates) 
#' @export
remove.fun <- function(df, dates) {
  for (i in dates){
    df <- df[!df$DOY==i,]
  }
  return(df)
}

#' Calculate a mean of Tmax
#'
#'Calculate a mean Tmax for each sub-group
#' @param df Data frame containing all Tmax for each sub-group.
#' @return Return the inputed data frame with a new column names "Tmax_mean". 
#' @details The data frame should contain a column named "Tmax" whith all Tmax and a column named "ID" to identify which Tmax belong to which sug-group.
#' @examples 
#' ID = c(rep("A", times=5), rep("B", times=5), rep("A", times=5), rep("B", times=5))
#' Tmax = c(rep(2.5, times=5), rep(2.7, times=5), rep(3.2, times=5), rep(3.4, times=5))
#' DOY = c(rep(102, times=10), rep(103, times=10))
#' df <- data.frame(DOY, ID, Tmax)
#' Tmax.mean(df)
#' @export
Tmax.mean <- function (df) {
  Tmean <- vector()
  ID <- vector()
  for (i in levels(df$ID)) {
    sub.i <- subset(df, ID==i)
    Tmean <- append(Tmean, values=mean(unique(sub.i$Tmax)))
    ID <- append(ID, values=i)
  }
  df.Tmean <- data.frame(ID, Tmean)
  names(df.Tmean) <- c("ID", "Tmax_mean")
  df <- merge(df, df.Tmean, all.x=T)
  return(df)
}
