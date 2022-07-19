
##############################################################################################################
# revision:
# Julia Liu 2020-03-05     : added Transform_panel
#                               for cross sectional/panel dataset
# Julia Liu 2020-03-09     : added "MC" (mean centered) transformation type 
# Julia Liu 2020-03-18     : record the mean center parameters (scale&center) by add them to the mod_obj$data
#                               mod_obj$data$scl
#                               mod_obj$data$cen
# Julia Liu 2020-03-26     : added "adstockv2" transformation
# Julia Liu 2020-04-27     : added "adstockv3" transformation. This transformation has 3 parameters
#                               decay 
#                               peak
#                               length
# Rawley Cooper and 
# Julia Liu     2020-12-18 : added "AdstockGamma" transformation.
# Rawley Cooper 2021-01-11 : minor adjustments to "AdstockGamma" transformation
# Julia Liu     2021-03-24 : added "atan" transformation : atan(x*scale)
# Julia Liu     2021-07-11 : updated the Transform_Panel() function. Added bypanel (defaults to FALSE)
#                            This updates allows user to specify transformation at variable and panel level.
#                            To do this, user needs to create a 
# Julia Liu     2022-05-10 : added a new transformation called lognormal
# Julia Liu     2022-05-16 : made a change to mc transformation. Now it uses the the data between BeginDate 
#                            and EndDate; and it records all the mc parameters for both 
#                            right hand side and left hand side. 
# Julia Liu     2022-05-23 : made a change to if and else if
# Julia Liu     2022-06-16 : added a new transformation type "adrNew"
# Julia Liu     2022-07-15 : added capability of handling multiple reach curves for different variables.
##############################################################################################################

library(compiler)
library(RcppRoll)

#' Transform data
#'
#' @param obj : data.frame
#'
#' @return
#' @export
#'
#' @description
#'
#'
#' @examples
Reach <- function(fa, fb, fc, fGRPs) {
  # Return reach data subject to fitted formula to a value r=a/(1+b*(GRPs/1000)^c)

  # fa = Alpha Coefficient in reach model
  # fb = Beta Coefficient in reach model
  # fc = Gamma Coefficient in reach model
  # fGRPs = single data point of GRPs at which to calculate reach

  fReach <- as.numeric(fGRPs > 0) * fa/(1 + fb * (fGRPs/1000)^fc)
  # Return calculated reach value
  return(fReach)

  # Example Use of Reach Function (solution=1.222065) test=Reach(0.79,-1,0.5,125)

}
RollingSum <- function(afGRPs, fDecay, nPeriod) {
  # Create a rolling sum of an AdStock to a vector of data

  # afGRPsMat = vector of GRP data
  # fDecay = single data point, decimal decay rate of media
  # nPeriod = integer value of number of observations to sum over

  afRollingSum <- roll_sumr(afGRPs, weights=sapply(nPeriod, function(x) ((1-fDecay)^((nPeriod-1):0))), normalize=F)
  afRollingSum[1:(nPeriod-1)] <- afGRPs[1:(nPeriod-1)]

  # Return the rolling sum of data
  return(afRollingSum)

  # Example use of Function test=RollingSum(afGRPs, 0.15, 4)
}
AdStock <- function(afGRPs, fdecayRate) {
  # Generate vector of AdStocked/Decayed GRPs as a function of input GRPs and decay rate to a value
  # y(t)=y(t-1)*d + x(t)

  # afGRPs = matrix (vertical vector) of GRP Data
  # fdecayRate = decimal version of decay rate

  afAdStockedGRPs <- Reduce(function(v,x) v *(1-fdecayRate) + x, x = afGRPs, accumulate = TRUE)
  if(sum(afAdStockedGRPs) != 0) {
    afAdStockedGRPs <- afAdStockedGRPs * sum(afGRPs)/sum(afAdStockedGRPs)   # scale it so the sum of the total stays the same
  }
  #afAdStockedGRPsMat <- as.matrix(t(afAdStockedGRPsMat))
  # Return AdStocked GRPs Vector
  return(afAdStockedGRPs)

  # Example use of AdStock Function test=AdStock(data.matric(GRPs[3]),0.15)
}
adstock = cmpfun(AdStock)

adstockv3 <- function(afGRPs, fdecayRate, peak=1, length=600) {

  # sanity check
  if(fdecayRate <0 | fdecayRate> 1) {
    cat("the specified decay rate is", fdecayRate, "\n")
    stop("please specify decay a number between [0,1]")
  }
  if(peak < 1 | peak >= length(afGRPs)) {
    cat("the specified length is", length, "\n")
    stop("please specify peak a number >= to 1. 1 meaning the peak strength is at the current week. ")
  }
  if(length <= peak) {
    stop("please specify length to be greater than peak")
  }
  
  # do this element by element
  afAdStockedGRPs <- 0
  for (i in 1:length(afGRPs)) {
    value <- afGRPs[i] # record the original value
    if(value>0 ) {
      tmp1 <- afGRPs
      tmp2 <- afGRPs
      tmp2[i] <- 0
      tmp = tmp1-tmp2

      tmp3 <- lag(tmp, (peak-1), default=0)
    
      res <- Reduce(function(v,x) v *(1-fdecayRate) + x, x = tmp3, accumulate = TRUE)
    
      if(peak > 1) {
        k=i-1
        for (j in 1:(peak-1)) {
          if( (k+j) <= length(afGRPs)) { # making sure it won't pass the max length of the variable
            res[k+j] <- j/peak*value
          }
        }
      }
      if(length(res) >= (i+length)) {
        res[(i+length) : length(res)] <- 0   # zero out all the carry-over afer the i+length
      }
      if(sum(res) !=0 ) {
        res <- res * sum(tmp)/sum(res)
      }
      afAdStockedGRPs <- afAdStockedGRPs + res
    }
  }  
  afAdStockedGRPs2 <- Reduce(function(v,x) v *(1-fdecayRate) + x, x = afGRPs, accumulate = TRUE)
  # normalize it 
  if(sum(afAdStockedGRPs2) != 0) {
    afAdStockedGRPs2 <- afAdStockedGRPs2 * sum(afGRPs)/sum(afAdStockedGRPs2)   # scale it so the sum of the total stays the same
  }
  return(afAdStockedGRPs)
}

adstock = cmpfun(AdStock)


##########################################################################################################
#                                                                                                        #
# The "AdstockGamma" function allows you to calculate a wide range of adstock patterns with one          #
#    function.  It can produce rolling average adstocks. Or "half-life" (exponential) adstocks where     #
#    the first week has the highest effect and the effect decays consistantly every week after. Or       #
#    "build and decay" adstocks were you build (usually slowly) to a peak and then decay after that.     #
#                                                                                                        #
# It uses a "gamma" distribution for all this, and estimates the area under the "gamma curve" based on   #
#    two main parameters:  peak (where is the curve the highest) and spread (how slow or fast is the     #
#    build and decay.  It calculates the curve first (the hard part) and applies it to every element of  #
#    the data (so only the adstock that happens in the period of the data will be outputed).             #
#                                                                                                        #
# This function has 3 main inputs, and 4 optional inputs (all seven described below).                    #
#                                                                                                        #
#    data:  The data whos elements are being adstocked.  The data could be a data.frame (but only the    #
#       first column will be effected) a vector or a list.  But the data should have more than one       #
#       element.  To adstock only one value, input a list with that one value and as many zeros as       #
#       you want to see the adstock effects.                                                             #
#                                                                                                        #
#    peak:  a number 0+, fractions are OK.  Where do you want/expect the adstock to have the most        #
#       effect. peak = 1 means the first week (and specifically the middle of the first week) will       #
#       have the most effect.  peak = 0 means it starts right away with the highest effect (the first    #
#       hour of the first day of the first week) and decays after that. peak = 6 means it builds from    #
#       the first week to the 6th, and then decays after that.  If the peak is an interger               #
#       peak is an integer, we assume the adstock effects are greatest in the middle of that week,       #
#       while .5 means the most effect happens at the beginning of the first week and 1.5 means the      #
#       most effect is seen at the end of the first week, and so on.  If the peak is not given, the      #
#       function assumes the peak is middle of the first week                                            #
#                                                                                                        #
#    spred (spread): a number between 0-200.  Does the adstock build and decay quickly or slowly.        #
#       usually the spread is between .2 to 10 except in extream cases.  A spred = 0 means no spread     #
#       so really no adstock. A spred = .2 means a very fast decay or build and only the few weeks       #
#       around the peak have any effect.  A spred = 1 means an average build and decay or about 1/2      #
#       the effect on weeks before and after the peak. A spred = 10 means a slow build and decay         #
#       with only 10% loss of effect for every week from the peak.  Finally, a spred = 200 means a       #
#       rolling average, the effect is distributed equally for the entire period.                        #
#                                                                                                        #
#    decay (optional): instead of giving a spred, you can specify a decay instead, and the spred will    #
#       be estimated from the decay and peak.  Decay is a number 0-1 where .1 indicates that only 10%    #
#       of the effect is lost after one week and so on.  A decay = 0 is the same as spred = 200,         #
#       decay = 1 is the asme as spred = 0.  Note that the spred is just a estimate.  Since only if      #
#       the peak <= .5 will the decay be consistent from week to week.  So the function estimates a      #
#       spred that will give you "about" that decay on average.                                          #
#                                                                                                        #
#    Period (optional): for how many weeks do you want to calcuate the adstock.  If this is rolling      #
#       averages (spred=200), you likely will to give a period here (3 weeks, 10 weeks, etc.) but if     #
#       you have a low spred, you likely don't need a period (the effects decay within a small number    #
#       of weeks anyway).  For larger spred values, you might want to give a period so to cut off the    #
#       effects of the adstock after 10 weeks or 20 weeks, etc.  If now period is given, the defualt     #
#       is to spread the adstock for as mny weeks as it is large enough to have an effect.               #
#                                                                                                        #
#    PreEst (optional): a TRUE or FALSE input (FALSE is the default).  If you have 2 years of data on    #
#       some media or brand health measure, likley you have over 2 years of using that media, but you    #
#       cut it off your input data at 2 years just for data gathering reason.  If you adstock those      #
#       two years of data, you will effectively be assuming that that media/brand health was dark        #
#       before those two years (which will mean your models will not accurately account for the first    #
#       few weeks).  Some companies solve this by putting adjustments in the first few weeks of the      #
#       base. Other companies ignore this and accept that the first few weeks will incorrectly effect    #
#       the model.  The last method is to estimate the effects before those first few weeks by assuming  #
#       the media before is about at the same volume as the first 3 weeks that you do have.   If you     #
#       set PreEst=TRUE, this funtion will make that assumption and hopefully create more accurate       #
#       adstocks overall. Setting it to TRUE is a good choice for media or brand health measures that    #
#       run every week, and you are using a static base/intercept.                                       #
#                                                                                                        #
#    Print (optional):  This functions prints out messages as it calculates the adstock to help the      #
#       user.  If Print=TRUE, those messages will print out in R Studio.  If Print=FALSE (the default)   #
#       it will only print out error message (and very few of those)                                     #
#                                                                                                        #
# To help understand this, below are some examples of the adstock curves produced by different inputs:   #
#                                                                                                        #
#    Peak=0.5,   Spred=1.44                                                                              #
#       This is identical to the adstock we usually use, with decay=.5.  The curve starts at its         #
#       highest point and decays by 50% ever week very consistently.  This only reflects reality if      #
#       the ads/media all have exactly 50% decay and all drop on the first day of the week               #
#                                                                                                        #
#    Peak=1,     Spred=1.16                                                                              #
#       This is very similar to the first example, but it assumes the adstock peaks in the middle of     #
#       first week.  The big difference is the second week decays only 25%, then the third 47% and       #
#       after that it has about 56% decay per week (which all averages to about decay=0.5 or 50% decay)  #
#                                                                                                        #
#    Peak=1,     Spred=1.44                                                                              #
#       This is like the first two examples, with assuming the adstock peaks in the middle of the        #
#       first week.  But the second week decays only 21% and over time the decay goes down to 50% (but   #
#       averages more like a 55% decay overall)                                                          #
#                                                                                                        #
#    Peak=1.333, Spred=1.14                                                                              #
#        This has the first and the second week having about the same effects, with the third week       #
#        decaying by 38% and the decay goes down to about 55% (so 50% overall on average).  This is      #
#        also the closest curve to the reality if all the ads for the week happened in the middle of     #
#        the first week (and all had a 50% half-life of effect).                                         #
#                                                                                                        #
#    Peak=1.4,   Spred=1.44                                                                              #
#        This is having the peak in the second week, but the first week is only 10% less then the        #
#        second, the thrid week decays 30% from the second and slowly increases to 50% decay in the      #
#        end.   Why this is significant, if you have ads/media with a 50% decay (for each ad) then       #
#        this is the pattern you see when the ads/media are distributed equally over the days of the     #
#        first week. So this is much more a reflection of reality then the first example.                #
#                                                                                                        #
#    Peak=2,     Spred=0.91                                                                              #
#        This is having the peak in the second week with the first week being 50% of the second week,    #
#        the third week decays 20% from the second and the decay goes down to about 62% (but 50% decay   #
#        on average).                                                                                    #
#                                                                                                        #
#    peak=6,     Spred=0.46                                                                              #
#        This peaks in week 6, and looks a lot like a "normal bell" curve.  The sixth week is the        #
#        highest, the 5th and 7th weeks are about 15% less, the 4th and 8th weeks are about 40% less     #
#        then the 6th week, and overall the decay is about 50% on average.                               #
#                                                                                                        #
#    peak=6,     Sped=1.44                                                                               #
#        This also peaks in week 6, and also looks like "normal bell" curve.  The difference from the    #
#        last example is it is more spread out.  The 5th and 7th week are about 5% less then the 6th     #
#        and the decay moves down from there to about 50% (but averages more of a 28% decay)             #
#                                                                                                        #
##########################################################################################################

AdstockGamma <- function(data, peak=1, spred=-1, decay=-1,period=0,PreEst=FALSE,print=FALSE) {
  #############################################################
  # This section just makes sure all the inputs are usable    #
  #    when not, the code tries to replace them with default  #
  #    values                                                 #
  #############################################################
  
  if (class(data) == "data.frame") {
    if ((print) & ncol(data)) {
      cat("Only the first column of the data is being adstocked \n")  
    }
    coll <- names(data)[[1]]
    data <- data[[coll]]
  }
  data  <- as.vector(data)
  rows  <- length(data)
  if ((period >  0) & (period > rows)) period <- rows
  if (period  <= 0)                    period <- rows
  if (peak  <  0.5)                    peak   <- 0.5
  if (peak    >  period)               peak   <- period
  if ((rows < 1) | (class(data) == "character")) {
    stop("Data must be numeric and a list, vector, or column of a data frame")  
  }
  if ((period < 1) | (period > rows)) {
    period = rows
  }
  if ((is.na(spred)) | (!is.numeric(spred))) spred <- -1
  if ((is.na(decay)) | (!is.numeric(decay))) decay <- -1
  if (spred < 0) {
    if (decay < 0) {
      spred <- 1
      if (print) {
        cat("The spread must be equal or greater than 0, a default of 1 is being used \n")
      }
    } else {    
      #############################################################
      # If a "decay" is given and no "spred" the code uses a big  #
      #    table and interpolation to find the best "spred" that  #
      #    will give that decay (on average)                      #
      #############################################################
      if (decay > .99) {
        decay <- 1
        spred <- 0
      }
      if (decay < .01) {
        decay <- 0
        spred <- 200
      }
      if ((decay > 0) & (decay < 1)) {
        dtab        <- data.frame(c(0,0.5,1,1.333333,2,3,4,6,8,12,16,24,32,48,64,100))
        names(dtab) <- "peak"
        dtn         <- length(dtab$peak)
        dtab$C1     <- c(0.01,99.49707,91.13379,89.99414,82.38184,75.90527,
                         70.74121,62.74609,56.67676,47.83008,41.56152,33.10938,
                         27.59863,20.77148,16.67871,11.57422)
        dtab$C2     <- c(0.05,19.49512,17.84766,17.62793,16.12598,14.85156,
                         13.83691,12.26562,11.07422,9.337891,8.109375,6.455078,
                         5.37793,4.044922,3.246094,2.250977)
        dtab$C3     <- c(0.1,9.491211,8.683594,8.579102,7.84082,7.216797,
                         6.720703,5.952148,5.371094,4.524414,3.926758,3.12207,
                         2.598633,1.953125,1.566406,1.084961)
        dtab$C4     <- c(0.15,6.15332,5.625,5.55957,5.076172,4.668945,4.345703,
                         3.845703,3.467773,2.917969,2.530273,2.009766,1.670898,
                         1.254883,1.005859,0.6959635)
        dtab$C5     <- c(0.2,4.481445,4.09375,4.046875,3.691406,3.371094,
                         3.105469,2.702148,2.385742,1.946289,1.666992,1.301758,
                         1.041992,0.7682292,0.5891927,0.3854167)
        dtab$C6     <- c(0.25,3.475586,3.119141,3.0816075454,2.74707,2.486328,
                         2.255859,1.916992,1.65625,1.34375,1.101562,0.8398438,
                         0.6796875,0.4730903,0.3710938,0.25)
        dtab$C7     <- c(0.3,2.803711,2.473633,2.443867115,2.138672,1.885742,
                         1.708984,1.376953,1.201172,0.9541016,0.7942708,
                         0.6028646,0.4563802,0.3261719,0.2534722,0.1698495)
        dtab$C8     <- c(0.35,2.321289,1.999023,1.974968224,1.685547,1.480469,
                         1.296875,1.042969,0.9248047,0.6946615,0.5416667,
                         0.3977865,0.3151042,0.2222222,0.1594329,0.1059028)
        dtab$C9     <- c(0.4,1.958008,1.65625,1.6363199025,1.37207,1.162109,
                         1.027344,0.8408203,0.6822917,0.5232205,0.3990885,
                         0.2890625,0.2092014,0.1458333,0.1119792,0.07349537)
        dtab$C10    <- c(0.45,1.672852,1.384766,1.368102742,1.120117,0.9277344,
                         0.8105469,0.6516927,0.515408,0.360026,0.311849,
                         0.2239583,0.1414931,0.1102431,0.08449074,0.05555556)
        dtab$C11    <- c(0.5,1.442383,1.15918,1.145231278,0.9101562,0.7317708,
                         0.6295573,0.4607205,0.3782552,0.3040365,0.2213542,
                         0.1568287,0.1206597,0.08333333,0.06385031,0.04166667)
        dtab$C12    <- c(0.55,1.251953,0.9658203,0.954198327,0.7272135,
                         0.5963542,0.5075955,0.3925781,0.2929687,0.2126736,
                         0.1675347,0.1024306,0.09027778,0.05343364,0.0474537,
                         0.0308642)
        dtab$C13    <- c(0.6,1.091797,0.8154297,0.805617417,0.5957031,0.4798177,
                         0.3691406,0.3072917,0.2222222,0.1371528,0.1241319,
                         0.07349537,0.05613426,0.03819444,0.02893519,0.01851852)
        dtab$C14    <- c(0.65,0.9521484,0.6842448,0.6760111,0.4802517,0.3789062,
                         0.3144531,0.1666667,0.1629051,0.09548611,0.07291667,
                         0.04996142,0.046875,0.0316358,0.02391975,0.0154321)
        dtab$C15    <- c(0.7,0.8300781,0.5664063,0.559590582,0.3769531,
                         0.2534722,0.203125,0.1458333,0.1362847,0.07928241,
                         0.06037809,0.05497685,0.0316358,0.01646091,0.01234568,
                         0.01028807)
        dtab$C16    <- c(0.75,0.7213542,0.4570313,0.451531721,0.2832031,
                         0.1744792,0.1689815,0.09635417,0.07407407,0.05054012,
                         0.03819444,0.02623457,0.01929012,0.01311728,
                         0.009773663,0.00617284)
        dtab$C17    <- c(0.8,0.6210937,0.3541667,0.349904918,0.1970486,
                         0.1423611,0.1111111,0.05671296,0.05902778,0.02893519,
                         0.0308642,0.01466049,0.0154321,0.01028807,0.005486968,
                         0.003315042)
        dtab$C18    <- c(0.85,0.5271267,0.2543403,0.251279755,0.15625,0.1111111,
                         0.08622685,0.04398148,0.03356481,0.02237654,0.01697531,
                         0.01157407,0.008744856,0.005658436,0.004115226,
                         0.002386596)
        dtab$C19    <- c(0.9,0.4342448,0.2028356,0.200394825,0.09143519,0.0625,
                         0.0474537,0.03240741,0.01646091,0.0162037,0.01234568,
                         0.008230453,0.00617284,0.004115226,0.002057613,
                         0.001373493)
        dtab$C20    <- c(0.95,0.3339844,0.1119792,0.110631724,0.04456019,
                         0.03009259,0.02237654,0.0154321,0.01157407,0.007716049,
                         0.005658436,0.003600823,0.002743484,0.001828989,
                         0.001371742,0.000909628)
        dtab$C21    <- c(0.99,0.2170139,0.05150463,0.050884861,0.02546296,
                         0.01774691,0.01311728,0.009259259,0.006687243,
                         0.00462963,0.003429355,0.002057613,0.001371742,
                         0.001371742,0.0009144947,0.000606043)
        i0  <- 0
        for (i in 1:21) {
          col0 <- paste("C",as.character(i),sep="")
          val0 <- dtab[1,col0]
          if (val0 <= decay) {
            i0  <- i
            col <- col0  
          }
        }
        j0 <- 0
        for (j in 2:dtn) {
          val0 <- dtab$peak[j]
          if (val0 <= peak) {
            j0  <- j
          }
        }
        i1 <- i0 + 1
        if (i1 > 21) i1 <- 21
        col0 <- paste("C",as.character(i1),sep="")
        j1 <- j0 + 1
        if (j1 > dtn) j1 <- dtn
        d0 <- dtab[1,col]
        d1 <- dtab[1,col0]
        p0 <- dtab$peak[[j0]]
        p1 <- dtab$peak[[j1]]
        w0 <- dtab[j0,col]
        w1 <- dtab[j0,col0]
        w2 <- dtab[j1,col]
        w3 <- dtab[j1,col0]
        if ((d0 == decay) | (d0 == d1)) {
          dpct <- 0
        } else {
          dpct <- (decay - d0) / (d1 - d0)
        }
        w4 <- w0 + ((w1 - w0) * dpct)
        w5 <- w2 + ((w3 - w2) * dpct)
        if ((p0 == peak) | (p0 == p1)) {
          ppct <- 0
        } else {
          ppct <- (peak - p0) / (p1 - p0)
        }
        spred <- w4 + ((w5 - w4) * ppct)
      }
    }
  }
  ################################################################
  # Set up the variables needed to run the "gamma" distribution  #
  #    equation.  Note the main work of this function is to run  #
  #    this equation and find points on a "curve" to calculate   #
  #    the adstock                                               #
  ################################################################
  peak0 <- peak - 0.5
  if (peak0 <= 0.0001) peak0 <- 0.0001
  pk0   <- floor(peak)
  pk1   <- pk0 + 0.5
  pk2   <- pk0 + 1
  pp0   <- 0
  pp2   <- 0
  if (peak <= pk1) {
    pp0 <- 1
    pp2 <- (peak - pk0) * 2
  } else {
    pp0 <- (pk2 - peak) * 2
    pp2 <- 1
  }
  if (pk0 < 1) {
    pk0 <- 1
    pk1 <- 1.5
    pk2 <- 2
    pp0 <- 1
    pp2 <- 0
  }
  alpha <- peak0 / spred
  beta  <- (peak + 50) / spred
  an    <- floor(beta / 60) + 1
  gamma <- rep(1,an)
  alp   <- floor(alpha) + 1
  i0    <- 1
  for (i in 1:alp) {
    gamma[i0] <- gamma[i0] * i * spred
    i0        <- i0 + 1
    if (i0 > an) i0 <- 1
  }
  if (an > 1) alpha <- alpha / an
  curve <- rep(0,rows)
  e0    <- 1
  e1    <- 1
  e2    <- 1
  e3    <- (0 - (peak0/spred)/an)
  pa0   <- peak0 - 1
  pa1   <- peak0 + 1
  if (pa0 <= 0) pa0 <- pa1
  e4    <- (0 - (pa0/spred)/an)
  e5    <- (0 - (pa1/spred)/an)
  for (j0 in 1:an) {
    e6         <- (peak0 ^ alpha) * exp(e3) / gamma[j0]
    e0         <- e0 * e6
    e6         <- (pa0   ^ alpha) * exp(e4) / gamma[j0]
    e1         <- e1 * e6
    e6         <- (pa1   ^ alpha) * exp(e5) / gamma[j0]
    e2         <- e2 * e6
  }
  #############################################################
  # A lot of this code is just to handle extreme cases where  #
  #    R might "blow up" when trying to calculate the points  #
  #    on the curve (only happens 0.003% of the time but we   #
  #    still need to adjust the code to avoid it)             #
  #############################################################
  if (is.infinite(e0 * gamma[1]) | (e0 <= 0))  {
    e0 <- 1
    e1 <- 0
    e2 <- 0
  }
  e3    <- (e1 + e2) / e0
  if (e3 < 0.00001) {
    curve[pk0] <- pp0
    curve[pk2] <- pp2
  } else {
    gamma[1] <- gamma[1] * e0
    if (spred > 180) {
      for (i in pk0:period) {
        curve[i] <- 1
      }
    } else {
      if (spred < 0.001) {
        curve[pk0] <- pp0
        curve[pk2] <- pp2
      } else {
        if (period <= 1) {
          curve[pk0] <= 1
        } else {
          for (i in 1:period) {
            e1   <- 0
            prob <- 0
            #############################################################
            # We calculate 5 points for each week and use them to make  #
            #    an estimation of the area under the curve for each     #
            #    week.                                                  #
            #############################################################
            for (j in 1:5) {
              i0         <- i + (j / 5) - 1.1
              e0         <- 1
              e2         <- (0 - (i0 / spred)) / an
              #############################################################
              # The numbers sometime get to large for R too handle, so we #
              #    calculate them in multiple steps when needed just to   #
              #    keep them small enough not to cause problems with R    #
              #############################################################
              for (j0 in 1:an) {
                e3         <- (i0 ^ alpha) * exp(e2) / gamma[j0]
                e0         <- e0 * e3
              }
              #############################################################
              # And even with all the precautions we still check every    #
              #    number to be sure R calculated it without issues.      #
              #############################################################
              if ((is.nan(e0)) | (is.infinite(e0))) {
                prob <- 1
                e0   <- 0
              }
              e1 <- e1 + e0
              if ((is.nan(e1)) | (is.infinite(e1))) {
                prob  <- 1
                e1    <- 0
              }
            }
            if (prob == 1) {
              if (i == pk0) {
                e1 <- pp0 * 5
              } else {
                if (i == pk2) {
                  e1 <- pp2 * 5
                } else {
                  e0 <- 0
                }
              }
            }
            #############################################################
            # We finally like the number and store it to use            #
            #############################################################
            curve[[i]] <- e1 / 5
          }
        }
      }
    }
  }  
  #############################################################
  # And some final checks that it was all calculated without  #
  #    any explosive round-off errors by R.                   #
  #############################################################
  prob <- 0
  csum <- sum(curve)
  cent <- curve[pk0] + curve[pk2]
  if ((csum <= 0.00000001) | (cent <= 0.00000001)) {
    prob <- 1
  } else {
    if (period > 1) {
      per  <- period - 1
      for (i in 1:per) {
        e0 <- curve[i]
        e1 <- curve[i + 1]
        if (i < pk0) {
          if (e0 > (e1 + 0.00001)) {
            prob         <- 1
            curve[i + 1] <- e0
          }
        }
        if (i >= pk2) {
          if (e1 > (e0 + 0.00001)) {
            prob         <- 1
            curve[i + 1] <- e0
          }
        } 
      }
    }
  }
  if (prob == 1) {
    if (print) {
      cat("Check the gamma curve, seems it having some problems \n")
      cat("   Will try to fix it \n")
    }
    if (spred < 0.1) {
      curve <- rep(0,rows)
      curve[pk0] <- pp0
      curve[pk0] <- pp2
    } else {
      if (spred > 100) {
        curve <- rep(0,rows)
        for (i in pk0:period) {
          curve[i] <- 1
        }
      }
    }
    if (print) print(curve)
    csum <- sum(curve)
  }
  #############################################################
  # The next line is critical.  The curve must sum up to 1 so #
  #    applying the adstock does not increase or decrease the #
  #    effects of the ad/media                                #
  #############################################################
  curve <- curve / csum 
  #############################################################
  # If "print=TRUE" print out statistics on the adstock curve #
  # created                                                   #
  #############################################################
  if (print) {
    low  <- 0
    i0   <- 1
    done <- 0
    if (peak >= 2) {
      while((i0 < peak) & (i0 <= period) & (done == 0)) {
        w0 <- low + curve[[i0]]
        if (w0 <= 0.05) {
          low  <- w0
          i0   <- i0 + 1
        } else {
          done <- 1
        }
      }
    }
    high <- 0
    i1   <- rows
    if ((period > 0) & (period < rows)) {
      i1 <- period
    }
    done <- 0
    while((i1 > peak) & (i1 >= 1) & (done == 0)) {
      w0 <- high + curve[[i1]]
      if (w0 <= (0.1 - low)) {
        high <- w0
        i1   <- i1 - 1
      } else {
        done <- 1
      }
    } 
    if (i0 < 1)      i0 <- 1
    if (i0 > peak)   i0 <- peak
    if (i1 > period) i1 <- period
    if (i1 < peak)   i1 <- peak
    avgb <- 0
    ib   <- 0
    avgd <- 0
    id   <- 0
    for (i in i0:i1) {
      if (i < peak) {
        w0   <- curve[[i]]
        w1   <- curve[[i + 1]]
        w2   <- 1 - (w0 / w1)
        ib   <- ib + 1
        avgb <- avgb + w2
      } else {
        if (i < i1) {
          w0   <- curve[[i]]
          w1   <- curve[[i + 1]]
          w2   <- 1 - (w1 / w0)
          id   <- id + 1
          avgd <- avgd + w2
        }
      }
    }
    if ((ib > 0) & (avgb > 0)) {
      avgb <- avgb * 100 / ib
    } else {
      avgb <- 0
    }
    avgb <- floor((avgb * 10) + 0.5) / 10
    if ((id > 0) * (avgd > 0)) {
      avgd <- avgd * 100 / id
    } else {
      avgd <- 0
    }
    avgd <- floor((avgd * 10) + 0.5) / 10
    mid  <- floor((curve[[peak]] * 1000) + 0.5) / 10
    mpct <- floor(((1 - high - low) * 100) + 0.5) 
    cat(mpct,"% of the curve is in the range ",i0," to ",i1," \n",sep = "")
    if ((ib > 0) & (avgb > 0)) {
      cat("With average biuld of ",avgb,"% \n",sep="")
    }
    cat("A peak at ",peak," with a value of ",mid,"% \n",sep="")
    if (id > 0) {
      cat("And with average decay of ",avgd,"% \n",sep="")    
    }
  }
  #############################################################
  # Now that we have the curve the last few line do the real  #
  #    job of this function, to apply that curve to every     #
  #    element of the data and sum up the results for every   #
  #    week                                                   #
  #############################################################
  pre   <- 0
  if (PreEst) {
    pre = ((data[[1]] * 3) + (data[[2]] * 2) + data[[3]]) / 6
  }
  work0 <- c(rep(pre,rows - 1), data)
  work1 <- embed(work0,rows)
  work2 <- as.vector(work1 %*% curve)
  return(work2)
}


AdResponse <- function(afGRPsMat, afCoeffsMat, params){
  # Generate the Effective Cover of a vector of input GRPs

  # afGRPs = vector of GRP data
  # afCoeffsMat = matrix (10 cols by 3 rows) of coefficients for reach models from 1+ to 10+
  # nEffFreq = integer value of Effective Frequency Parameter
  # nRecFreq = integer value of Recency Frequency Parameter
  # nPeriod = integer value of Response Period Parameter
  # fDecay = decimal value of decay rate parameter

  nEffFreq <- params[1]
  nRecFreq <- params[2]
  nPeriod <- params[3]
  fDecay <- params[4]
  if(is.null(nEffFreq) | is.null(nRecFreq) | is.null(nPeriod) | is.null(fDecay)) {
    stop("adr transformation requires you to specify Effective, Recency, Decay, and Period. 
         Please make sure they are specified. \n")
  }
  # Define output matrix size
  fEffGRPs <- RollingSum(afGRPsMat, fDecay, nPeriod)
  fTotalEffGRPs <- adstock(afGRPsMat, fDecay)

  # sets a,b,c values based on Effective Frequency
  a <- afCoeffsMat[[1, nEffFreq]]
  b <- afCoeffsMat[[2, nEffFreq]]
  c <- afCoeffsMat[[3, nEffFreq]]

  # Edge case, if Recency Frequency is 0
  if(nRecFreq == 0){
    afAdResponse <- Reach(a, b, c, fTotalEffGRPs)
    return(afAdResponse);
  }

  # Edge case, if Recency Frequency and Effective Frequency is ==
  if(nRecFreq == nEffFreq){
    afAdResponse <- Reach(a, b, c, fEffGRPs)
    return(afAdResponse);
  }

  # a,b,c of fit curves
  a_s <- afCoeffsMat[1,]
  b_s <- afCoeffsMat[2,]
  c_s <- afCoeffsMat[3,]

  # create matrix for recency window grps
  recency_window_grps = matrix(0, nrow=length(afGRPsMat), ncol=1)
  # seqence of reach curves needed to run in order to find recency period
  reach_curves <- nRecFreq:(nEffFreq - 1)

  # recency GRP calculation
  one <- sapply(reach_curves, function(x) Reach(a_s[[x]], b_s[[x]], c_s[[x]], fEffGRPs))
  two <- sapply(reach_curves, function(x) Reach(a_s[[x + 1]], b_s[[x + 1]], c_s[[x + 1]], fEffGRPs))
  three <- sapply(reach_curves, function(x) Reach(a_s[[nEffFreq - x]], b_s[[nEffFreq - x]], c_s[[nEffFreq - x]], fTotalEffGRPs - fEffGRPs) )
  recency_window_grps <- rowSums(((one-two)*three/100))

  # calculate whether or not to add recency window grps
  equal_total_eff_grps <- (fTotalEffGRPs - fEffGRPs) > .001
  # effective GRPs
  eff_reach = Reach(a, b, c, fEffGRPs)
  # if within recency window, add recency window grps
  afAdresponse <- ifelse(equal_total_eff_grps, eff_reach + recency_window_grps, eff_reach)

  return(afAdresponse)
}
adr = cmpfun(AdResponse)
AdStockPD <- function(data, i, p){
  rowSums(as.data.frame(embed(c(rep(NA, p), data), p + 1) %*% ((1 - i) ^ seq(0,p,1))),na.rm = F)->output
  output[is.na(output)] <- 0
  return(output)
}

# data = mod_obj1$data$BROOKS_MAG
# i = .3
# p = 4
# rowSums(as.data.frame(embed(c(rep(NA, p), data), p + 1) %*% ((1 - i) ^ seq(0,p,1))),na.rm = F)
#obj = mod_obj

#####################
# ABC function
#####################
abc <- function(x, a, b, c) {
  d <- a/(1+b*x^c)
  return(d)
}
abcNew <- function(x, a, b, c) {
  d <- a/(1+(x/b)^c)
  return(d)
}

my_lognormal <- function(x, meanlog, sdlog) {
  horizon <- 200
  time <- seq(1, horizon, 1)
  y <- matrix(nrow = length(x)+horizon, ncol=length(x))
  distribute <- dlnorm(time, meanlog, sdlog)
  distribute <- distribute/sum(distribute)
  fillit <- rep(0, length(x)+horizon)
  for(i in 1:length(x)) {
    fillit <- rep(0, length(x)+horizon)
    fillit[i:(i+horizon-1)] <- x[i]* distribute
    y[,i] <- fillit
  }
  out <- apply(y, 1, sum)
  out <- out[1:length(x)]
  return(out)
}

Transform = function(obj, print=TRUE) {
  x <- obj$data
  spec <- obj$spec
  fit_curves <- obj$fit_curves
  output <- list()
  for (i in 1:nrow(spec)) {
    if(spec$Transform[i] == "Y") {
      if(spec$Orig_Variable[i] %in% names(x)) {
      if(print) { cat("transform ", spec$Orig_Variable[i], "\n") }
      type <- unlist(strsplit(spec$TransformType[i], "_"))
      type <- toupper(type)
      data_vector <- x[[spec$Orig_Variable[i]]]
      for(j in 1:length(type)) {
        data_vector_transform <- list()
        if (type[j] == "ADSTOCK"){
          data_vector_transform <- AdStockPD(data_vector, spec$Decay[i], spec$Period[i])
        } else if (type[j] == "ADSTOCKV2"){
          data_vector_transform <- adstock(data_vector, spec$Decay[i])
        } else if (type[j] == "ADSTOCKV3"){
          data_vector_transform <- adstockv3(data_vector, spec$Decay[i], spec$Peak[i], spec$Length[i])
        } else if (type[j] == "ADSTOCKG"){
          data_vector_transform <- 
            AdstockGamma(data_vector, decay=spec$Decay[i], peak=spec$Peak[i], spred=spec$Spred[i], period=spec$Period[i])
        } else if (type[j] == "ADR"){
          if(is.null(fit_curves)) {
            stop("Please make sure you have fit curves (mod_obj$fit_curves) for adr transformation. \n")
          } else {
            data_vector_transform <- adr(data_vector, fit_curves, c(spec$Effective[i], spec$Recency[i], spec$Period[i], spec$Decay[i]))
          }
        } else if (type[j] == "ADRNEW"){
          #data_vector_transform <- adrnew(data_vector, fit_curves, E=spec$Effective[i], R=spec$Recency[i], P=spec$Period[i], D=spec$Decay[i])
          if("Reach_Curve" %in% names(spec)) {
            data_vector_transform <- adrnew(data_vector, fit_curves[[spec$Reach_Curve[i]]], E=spec$Effective[i], R=spec$Recency[i], P=spec$Period[i], D=spec$Decay[i])
          } else {
            data_vector_transform <- adrnew(data_vector, fit_curves, E=spec$Effective[i], R=spec$Recency[i], P=spec$Period[i], D=spec$Decay[i])
          }
        } else if (type[j] == "ABC"){
            data_vector_transform <- abcNew(data_vector, 1, spec$B[i], spec$C[i])
        } else if (type[j] == "ATAN") {
            if(!is.na(spec$Scale[i]) & is.numeric(spec$Scale[i])) {
              data_vector_transform <- atan(data_vector* spec$Scale[i])
            } else {
              stop("you need to specify Scale for variable", spec$Orig_Variable[i], "\n")
              #data_vector_transform <- atan(data_vector)
            }
        } else if (type[j] == "LAG") {
          data_vector_transform <- lag(data_vector, spec$Lag[i], default = 0)
        } else if (type[j] == "LOG") {
          data_vector_transform <- log(data_vector*spec$Scale[i] + 1)
        } else if (type[j] == "POLY") {
          data_vector_transform <- myPoly(data_vector, spec$Alpha[i])
        } else if (type[j] == "MA") {
          data_vector_transform <- rollmean(data_vector, spec$Window[i], align="right", fill=NA)
          data_vector_transform[which(is.na(data_vector_transform))] <- data_vector[which(is.na(data_vector_transform))]
        } else if (type[j] == "MC") {
          #data_vector_transform <- scale(data_vector)
          #scl <- attr(data_vector_transform, "scaled:scale")
          #cen <- attr(data_vector_transform, "scaled:center")
          mod_period <- x[[obj$Time]] >= obj$BeginDate & x[[obj$Time]] <= obj$EndDate
          scl <- sd(data_vector[mod_period])
          cen <- mean(data_vector[mod_period])
          data_vector_transform <- (data_vector-cen)/scl
        } else if (type[j] == "STEIN") {
          data_vector_transform <- shrinker(data_vector, bw=spec$Window[i], trim=spec$Trim[i])
        } else if (type[j] == "CPT") {
          data_vector_transform <-
            cpt.meanvar(data_vector, minseglen = 6, penalty = "CROPS", pen.value = c(0, 100), method = "PELT")
        } else if (type[j] == "POWER") {
          data_vector_transform <- (data_vector)^spec$Power[i]
        } else if (type[j] == "LOGNORMAL") {
          data_vector_transform <- my_lognormal(data_vector, spec$meanlog[i], spec$sdlog[i])
        } else if (type[j] == "NONE") {
          data_vector_transform <- data_vector
        } else {
          stop("TransformType ", type[j], "not found. \n")
        }
        data_vector <- data_vector_transform
      }
      x[spec$Trans_Variable[i]] <- data_vector
      if (type[j] == "MC") {
        if(tolower(spec$Variable_Type[i]) == "dependent") {
          x$scl <- scl
          x$cen <- cen
        } else {
          tmp1 <- paste("scl", spec$Trans_Variable[i], sep="_")
          tmp2 <- paste("cen", spec$Trans_Variable[i], sep="_")
          x[[tmp1]] <- scl
          x[[tmp2]] <- cen
        }
      }
      #if (type[j] == "MC") {
      #  x$scl <- scl
      #  x$cen <- cen
      #}
    }    #if(spec$Orig_Variable[i] %in% names(x))
    } else {    
      #cat("Variable", spec$Orig_Variable[i], "can not be found in the data set. \n")
    }  #if(spec$Transform[i] == "Y")
    
    output <- x
  }
  obj$data <- output
  return(obj)
}

Transform_panel = function(obj, print=TRUE, par_bypanel=FALSE) {
  if(par_bypanel) {
    spec <- obj$spec_transform
    split_spec = base::split(spec, spec[[obj$CS]])
  } else {
    spec <- obj$spec
  }
  fit_curves <- obj$fit_curves
  split_data <- base::split(obj$data, obj$data[[obj$CS]])

  output <- list()
  for (k in 1:length(split_data)) {
    x <- split_data[[k]]
    x <- x[order(x[[obj$Time]]), ]  
    cat("\n\n", "transform cross section", x[[obj$CS]][1], "...\n")
    if(par_bypanel) {
      spec <- split_spec[[x[[obj$CS]][1]]]
    }
    for (i in 1:nrow(spec)) {
      if(spec$Transform[i] == "Y") {
        if(spec$Orig_Variable[i] %in% names(x)) {
        if(print) { cat("transform ", spec$Orig_Variable[i], "\n") }
        type <- unlist(strsplit(spec$TransformType[i], "_"))
        type <- toupper(type)
        data_vector <- x[[spec$Orig_Variable[i]]]
        for(j in 1:length(type)) {
          data_vector_transform <- list()
          if (type[j] == "ADSTOCK"){
            data_vector_transform <- AdStockPD(data_vector, spec$Decay[i], spec$Period[i])
          } else if (type[j] == "ADSTOCKV2"){
            data_vector_transform <- adstock(data_vector, spec$Decay[i])
          } else if (type[j] == "ADSTOCKV3"){
            data_vector_transform <- adstockv3(data_vector, spec$Decay[i], spec$Peak[i], spec$Length[i])
          } else if (type[j] == "ADSTOCKG"){
            data_vector_transform <- 
              AdstockGamma(data_vector, decay=spec$Decay[i], peak=spec$Peak[i], spred=spec$Spred[i], period=spec$Period[i],print=print)
          } else if (type[j] == "ADR"){
            data_vector_transform <- adr(data_vector, fit_curves, c(spec$Effective[i], spec$Recency[i], spec$Period[i], spec$Decay[i]))
          } else if (type[j] == "ADRNEW"){
            if("Reach_Curve" %in% names(spec)) {
              data_vector_transform <- adrnew(data_vector, fit_curves[[spec$Reach_Curve[i]]], E=spec$Effective[i], R=spec$Recency[i], P=spec$Period[i], D=spec$Decay[i])
            } else {
              data_vector_transform <- adrnew(data_vector, fit_curves, E=spec$Effective[i], R=spec$Recency[i], P=spec$Period[i], D=spec$Decay[i])
            }
          } else if (type[j] == "ABC"){
            data_vector_transform <- abcNew(data_vector, 1, spec$B[i], spec$C[i])
          } else if (type[j] == "ATAN") {
            data_vector_transform <- atan(data_vector * spec$Scale[i])
          } else if (type[j] == "LAG") {
            data_vector_transform <- lag(data_vector, spec$Lag[i], default = 0)
          } else if (type[j] == "LOG") {
            data_vector_transform <- log(data_vector*spec$Scale[i] + 1)
          } else if (type[j] == "POLY") {
            data_vector_transform <- myPoly(data_vector, spec$Alpha[i])
          } else if (type[j] == "MA") {
            data_vector_transform <- rollmean(data_vector, spec$Window[i], align="right", fill=NA)
            data_vector_transform[which(is.na(data_vector_transform))] <- data_vector[which(is.na(data_vector_transform))]
          } else if (type[j] == "MC") {

            mod_period <- x[[obj$Time]] >= obj$BeginDate & x[[obj$Time]] <= obj$EndDate
            scl <- sd(data_vector[mod_period])
            cen <- mean(data_vector[mod_period])
            data_vector_transform <- (data_vector-cen)/scl
          } else if (type[j] == "STEIN") {
            data_vector_transform <- shrinker(data_vector, bw=spec$Window[i], trim=spec$Trim[i])
          } else if (type[j] == "CPT") {
            data_vector_transform <-
              cpt.meanvar(data_vector, minseglen = 6, penalty = "CROPS", pen.value = c(0, 100), method = "PELT")
          } else if (type[j] == "POWER") {
            data_vector_transform <- (data_vector)^spec$Power[i]
          } else if (type[j] == "NONE") {
            data_vector_transform <- data_vector
          } else {
            stop("TransformType ", type[j], "not found. \n")
          }
          data_vector <- data_vector_transform
        }
        x[spec$Trans_Variable[i]] <- data_vector
        if (type[j] == "MC") {
          if(tolower(spec$Variable_Type[i]) == "dependent") {
            x$scl <- scl
            x$cen <- cen
          } else {
            tmp1 <- paste("scl", spec$Trans_Variable[i], sep="_")
            tmp2 <- paste("cen", spec$Trans_Variable[i], sep="_")
            x[[tmp1]] <- scl
            x[[tmp2]] <- cen
          }
          
        }
        #if (type[j] == "MC") {
        #  x$scl <- scl
        #  x$cen <- cen
        #}
        }    #if(spec$Orig_Variable[i] %in% names(x))
      }
      output[[k]] <- x
  
    }
    
  }
  output <- do.call("rbind", output)
  obj$data <- output
  return(obj)
}