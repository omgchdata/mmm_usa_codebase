AdjustSeasonality=function(Data, Seasonality){
  output <- Data * Seasonality

return(output)}

AdStockCP=function(afDecayRateGRPs){
  # designed to be used with apply function on per column basis- that's why column's read in as lists with first value of list = decay rate
  
  # Generate matrix of AdStocked/Decayed GRPs as a function of input GRPs and decay rate to a value
  #  y(t)=y(t-1)*d + x(t)
  
  # read in afDecayRateGRPs as a list
  afDecayRateGRPsList <- as.list(afDecayRateGRPs)

  # set memory decay
  fdecayRate <- afDecayRateGRPsList[[1]]
  fdecayRate <- as.numeric(fdecayRate)
  
  # create list of JUST GRPs
  afAdStockedGRPs <- afDecayRateGRPsList[-1]
  
  # create output list, which looks just like GRP list
  output <- afAdStockedGRPs
  
  # first observations are equal
  output[[1]] <- afAdStockedGRPs[[1]]
  
  #loop through calculating AdStocked GRPs
  for (x in 2:length(afAdStockedGRPs)) {
    afAdStockedGRPs[[x]]=(afAdStockedGRPs[[x-1]]* (1-fdecayRate))+ afAdStockedGRPs[[x]]
  }
  
  output <- data.frame(matrix(unlist(afAdStockedGRPs), nrow=length(output), byrow=T))
  
  # return AdStocked GRPs matrix
  return(output)
  
  # Example use of AdStock Function
  #  test=AdStock(data.matric(GRPs[3]),0.15)
  
}

ReachCP=function(fGRPs, fa, fb, fc){
  
  # calculate point on reach curve using total effective GRPs and curve's A,B,C coefficients
  fReach=fa/(1+fb*(fGRPs/1000)^fc)
  
  return(fReach)
}

DeriveNonCover=function(fS, fT, nR){
  #       s(s+1)(s+2)...(s+r-1)
  # k(r)=-----------------------
  #       t(t+1)(t+2)...(t+r-1)
  
  fNumerator=1
  fDenominator=1
  
  for (i in 1:nR){
    fNumerator=fNumerator*(fS+i-1)
    fDenominator=fDenominator*(fT+i-1)
  }
  
  fFinal=fNumerator/fDenominator
  return(fFinal)
}

DeriveS=function(fKOne, fKTwo){
  #     k(1)^2-k(1)k(2)
  # s=--------------------
  #     k(2)-k(1)^2
  
  fS=((fKOne^2)-fKOne*fKTwo)/(fKTwo-fKOne^2)
  return(fS)
}

DeriveT=function(fS, fKOne){
  #     s
  # t=---------
  #     k(1)
  fT=fS/fKOne
  return(fT)
}

CombinedReachReduction=function(fProbA, fProbB, fOverlapReduction){
  
  fProb=fProbA+fProbB-(fOverlapReduction*fProbA*fProbB)
  
  return(fProb)
}

CountNonZero=function(afMatrix){
  # count nonzero values
  nCount <- length(which(afMatrix!=0))
  
  return(nCount)
}

SumNonReach=function(afMatrix){
  
  fNonReach=matrix(data=0,nrow=nrow(afMatrix), ncol=1)
  for (i in 1:nrow(afMatrix)){
    for(j in 1:ncol(afMatrix)){
      if (afMatrix[i,j]!=0){
        fNonReach[i,1]=fNonReach[i,1]+(1-afMatrix[i,j])
      }
    }
  }
  
  return(fNonReach)
}

FindAdStock = function(afGRPsMat, afCoeffs){

# make afGRPs a dataframe (was a matrix before)
afGRPsdf <- as.data.frame(afGRPsMat)

# in Channel Planning where response period is always 1, effective GRPs are always the weekly GRPs calculated by multichannel reach
# allocation
fEffGRPs <- afGRPsdf

# prep to feed into apply function by making first row of afDecayRateGRPs the decay rate
afDecayRate <- afCoeffs[4, ]
colnames(afGRPsdf) <- colnames(afDecayRate)
afDecayRateGRPs <- rbind(afDecayRate, afGRPsdf)

# calculate AdStock using apply function
afAdStock <- apply(afDecayRateGRPs, 2, AdStockCP)
afTotalEffGRPs <- as.data.frame(afAdStock)

return(afTotalEffGRPs)
}

# calculate Net Reach using Ian and Guy Hearn's formula
# change afIndividualNetandGrossReach later to just say 'afReach'
# February 28, 2015: Jon Krohn adds bAverage functionality, similar to in FindIndividualandGrossReach
#                   - this change enables output of unaveraged Individual, Net.Reach and Gross.Reach
CalculateNetReach= function(afIndividualNetandGrossReach, nWeeks, bAverage = TRUE){

  # count columns so we can subset to remove the net and gross placeholders
  
  colCount <- ncol(afIndividualNetandGrossReach)
  colCountDesired <- colCount - 2
  
  # subset individual and gross reach so you just have each channel's individual reach
  if (colCountDesired==1){

    afIndividualReach <- as.data.frame(afIndividualNetandGrossReach[, 1])

  }else{
  
  afIndividualReach <- as.data.frame(afIndividualNetandGrossReach[, 1:colCountDesired])  }
  
  # Set size of calculation matrix
  afNetReach = as.data.frame(matrix(data = NA, nrow = nrow(afIndividualReach), ncol= 1))
  afGrossReach = as.data.frame(matrix(data = NA, nrow = nrow(afIndividualReach), ncol= 1))
  
  # Calculate net reach for each week
  
  # if only calculating reach for 1 media, don't need to iterate through each column to find net
  # and gros reach
  if(ncol(afIndividualReach)==1){
    
    afNetReach = afIndividualReach
    afGrossReach = afIndividualReach
    
  }else{
    
  for(i in 1:nrow(afNetReach)){
    
    #If Sum of Reach =0 then total =0 and skip for speed
    if (rowSums(afIndividualReach[i,])==0){
      
      afNetReach[i,1] = 0
      afGrossReach[i,1]=0
      
    }else{
      
      afWeekReach=as.matrix(afIndividualReach[i,])
      
      fCount = length(afWeekReach[afWeekReach != 0])
      afGrossReach[i,1]=rowSums(afIndividualReach[i,])
      
      if (fCount==1){
        # Only one media active - combined media sum = total
        afNetReach[i,1]=rowSums(afIndividualReach[i,])
        
      }else{
        
        #Multiple media active - calculate duplication    
        #P(A and B)=P(A) + (B) - Factor*P(A)*P(B)
        fReach=afWeekReach[1,1]
        
        for (j in 2:ncol(afWeekReach)){
          
          fReach=fReach+afWeekReach[1,j]-fReach*afWeekReach[1,j]
        }
  
        afNetReach[i,1]=fReach
      }
    }
  }
  }
  
  afNetReach[afNetReach >=0.9999999] <- 0.9999999
  
  # Create output dataframe that returns reach of individual media and combined reach
  afOutput=as.data.frame(cbind(afIndividualReach,afNetReach, afGrossReach))
  colnames(afOutput)=c(colnames(afIndividualReach),'Net.Reach', 'Gross.Reach')
  
  # Jon Krohn added this if-else statement akin to in FindIndividualandGrossReach
  if (bAverage==TRUE){
    afOutput=as.data.frame(t(colMeans(afOutput[(nrow(afOutput)-nWeeks+1):nrow(afOutput),])))
  } else{
    # afOutput=as.data.frame(afWeeklyReach[(nrow(afWeeklyReach)-nWeeks+1):nrow(afWeeklyReach),])
    afOutput=as.data.frame(afOutput)
  }
  
  return(afOutput)
}

## Find reach function finds reach using a modified, Channel Planning specific version of AdStockTransforms with vectors
FindIndividualandGrossReach= function(afInputs, afGRPs, afSeasonality, nEffFreq, nRecFreq, nPeriod, afCoeffs, nWeeks, afTYGRPs, bAverage) {
  # afGRPs holds GRP data for each media type by week
  # afCoeffs holds the ABC values where each row is a mediaType and row 1 =a, row 2= b, row 3 = c
  # nEffFreq = 1, nRecFreq = 0, nPeriod = 1 (hardcoded from front end)
  
  # find total effective GRPs using adstock- those are the GRPs used to calculate reach
  afTotalEffGRPs <- FindAdStock(afGRPs, afCoeffs)
  
  ### prep for mapply: create dataframe for each total effective GRP's corresponding A B and C value
  fa <- afCoeffs[1, ]
  fb <- afCoeffs[2, ]
  fc <- afCoeffs[3, ]  
  
  # pass total effective GRPs and its corresponding A, B, C values into reach function using mapply- find reach
  afReach = mapply(ReachCP, afTotalEffGRPs, fa, fb, fc)

  # if reach = na, make it 0
  afReach[is.na(afReach)] <- 0

  # multiply each cell of reach output by influence factor
  # vectorized by JG and JK
  # afReach=afReach*afInputs[,5]
  afReachdf <- as.data.frame(t(apply(afReach, 1, function(x) x*afInputs[,5])))

  # begin computing NetReach and GrossReach
  
  # create dataframes of the correct size for afNetReach and afGrossReach; each are the size of 1 column of afGRPs
  afNetReach=as.data.frame(afReachdf[,1])
  afGrossReach=as.data.frame(afReachdf[,1])
  
  afNetReach[, 1] <- 0
  
  # column 1 of GrossReach shows the sum of each week's reach
  afGrossReach[,1]=rowSums(afReachdf)
    
  #colnames(afOutput)=c((afInputs[, 1]),'Net.Reach', 'Gross.Reach')
  afOutput=as.data.frame(cbind(afReach,afNetReach, afGrossReach))
  colnames(afOutput)=c(colnames(afReach),'Net.Reach', 'Gross.Reach')
    
  afSeasonality <- as.data.frame(afSeasonality)
  
  #Seasonally adjust data if needed (just last n weeks)
  for (i in (nrow(afOutput)-nrow(afTYGRPs)+1):nrow(afOutput)){
    for (j in 1:ncol(afOutput)){
      afOutput[i,j]=afOutput[i,j]*afSeasonality[nrow(afTYGRPs)-(nrow(afOutput)-i),1]
    }
  }

  
    # calculate average of reach for the current time period (1 row, a column for each MediaType)
  if (bAverage==TRUE){
    afOutput=as.data.frame(t(colMeans(afOutput[(nrow(afOutput)-nWeeks+1):nrow(afOutput),])))
     } else{
    # afOutput=as.data.frame(afWeeklyReach[(nrow(afWeeklyReach)-nWeeks+1):nrow(afWeeklyReach),])
       afOutput=as.data.frame(afOutput)
  }
  
  return(afOutput) 
}

## FindNetReach uses FindIndividualReach with bAverage = FALSE (return values to average later) as well as  Ian's net reach formula.
# it's used when we need to run multichannelreachopt in order to correctly allocate planned GRPs
FindNetReach=function(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage){
  #'FindNetReach')
  # FindNetReach is a function called by the MultiChannelExclusiveReach function (used by PHD Source).
  
  # It was created by Jackie Goldschmidt and Jon Krohn on June 2, 2014. Jackie and Jon significantly sped up the ChannelPlanning 
  # AllOrNoneMultiChannel optimization function, in part by restructuring the function's code. Previously, each iteration of the 
  # optimization's process calculated net reach. This was inefficient for two reasons. First, optimizing to produce the highest 
  # gross reach (sum of each channel's reach) versus optimizing to produce the highest randomly de-duplicated net reach (
  # 1 - the product of each channel's reach) should always produce the same results-- so it dictated unnecessary calculations. 
  # Second, the unnecessary calculations created a series of for loops within for loops, which are particularly difficult 
  # for R to execute. 
  
  # Jackie and Jon commented out any code involving the calculation of net reach during the optimization and moved that code into this 
  # function. Net reach is now only calculated when it is necessary-- when finding net reach, weekly net reach, or the exclusive reach of each media channel.
  
  # set bAverage to FALSE so multichannelreach returns each week's calculations, which we need to compute average gross and net reach
  #bAverage <- FALSE
  afIndividualNetandGrossReach <- MultiChannelReachOpt(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, 
                                                       bAverage = FALSE)

  afOutput <- CalculateNetReach(afIndividualNetandGrossReach, nWeeks, bAverage)

  return(afOutput)
}

# MultiChannelReach is NOT used in the optimization.
# It is called by the R handlers, then calls FindNetReach to return a campaign's individual, net and gross reach
MultiChannelReach=function(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage){

  #calculates this year's GRPs( as GRPs) = CPP/ investment * audience delivery factor
  afTYTotalGRPs=t(as.data.frame((afInputs[,2]/afInputs[,3])* afInputs[,4]))
  
  # multiply last year's GRPs * audience delivery factor
  # Jon vectorized this to increase the optimization speed
  # afLYDelGRPs=afLYGRPs  
  #   for (i in 1:nrow(afLYDelGRPs)){
  #     afLYDelGRPs[i,]=afLYGRPs[i,]*afInputs[,4]
  #   }
  
  afLYDelGRPs = as.data.frame(t(apply(afLYGRPs, 1, function(x) x*afInputs[,4])))
  
  # set bAverage to false since we want to calculate net reach
  # bAverage <- FALSE
  # calls MultiChannelReachAllocation- its result is individual and gross reach
  weeklyReach=MultiChannelReachAllocation(afLYDelGRPs, afTYTotalGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, 
                                          bAverage = FALSE)
  
  # calculate the campaign's weekly net reach  
  netReach <- CalculateNetReach(weeklyReach, nWeeks, bAverage)
  
  return(netReach)
}

# USED For optimization
# Processes inputs into dataframes with GRPs per week for last year (as many cols as MediaTypes, as many rows as weeks of last year's data)
# and and total GRPs for this year (as many cols as mediaTypes, 1 row) then passes data to MultiChannelReachAllocation
# and MultiChannelWeeklyReach, returning MultiChannelWeeklyReach's answer (the average reach per media vehicle) as the output
MultiChannelReachOpt=function(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage){
  
  #Process Inputs for Multi Channel Reach Calculation
  #calculates this year's GRPs( as GRPs) = CPP/ investment * audience delivery factor
  afTYTotalGRPs=t(as.data.frame((afInputs[,2]/afInputs[,3])* afInputs[,4]))
  
  afLYDelGRPs = as.data.frame(t(apply(afLYGRPs, 1, function(x) x*afInputs[,4])))
  
  # calls MultiChannelReachAllocation- its result is the result for MultiChannelReach  
  afOutput=MultiChannelReachAllocation(afLYDelGRPs, afTYTotalGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage)

  return(afOutput)
}

# Takes input GRPs already adjusted for audience delivery, distributes each media channel investment 
# evenly over weeks and calculates the individual and combined weekly reach including adjustments for influence and seasonality.
# Can report either week-by-week reach (bAverage = FALSE) or average reach for the activity (bAverage = TRUE)
MultiChannelReachAllocation=function(afLYGRPs, afTYTotalGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage){
  #print('MultiChannelReachAllocation')
  # code immediately below commented out by JG + JK; they vectorized it to remove for loop

  #Distributed annual Total GRPs to weeks evenly, calculate weekly
  afTYGRPs=data.frame(matrix(nrow=nWeeks, ncol=ncol(afTYTotalGRPs)))
  
  for (i in 1:nrow(afTYGRPs)){
    afTYGRPs[i,]=afTYTotalGRPs[]/nWeeks
  }
  
  # result of MultiChannelWeeklyReach is the result for MultiChannelReachAllocation and thus MultiChannelReach
  afOutput=MultiChannelWeeklyReachOpt(afLYGRPs, afTYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nEffFreq, nRecFreq, nPeriod, bAverage)
  
  return(afOutput)
}

MultiChannelWeeklyReach=function(afLYGRPs, afTYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nEffFreq, nRecFreq, nPeriod, bAverage){
  # calculate Week-by-week multichannel reach

  # number of weeks = the number of rows in this year's GRPs
  nWeeks=nrow(afTYGRPs)
  
  # bind historical and current year GRPs into 1 matrix
  colnames(afLYGRPs)=colnames(afTYGRPs)
  afGRPs=as.matrix(rbind(afLYGRPs, afTYGRPs))
  
  # prep to calculate net reach (need individual and gross reach returned as week by week values, rather than average)
  # bAverage <- FALSE
  # find each channel's weekly reach
  weeklyReach <-  FindIndividualandGrossReach(afInputs, afGRPs, afSeasonality, nEffFreq, nRecFreq, nPeriod, afCoeffs, nWeeks, afTYGRPs, 
                                              bAverage = FALSE)
  
  # calculate the campaign's weekly net reach  
  netReach <- CalculateNetReach(weeklyReach, nWeeks, bAverage)
  
  return(netReach)  
}

# Takes input of Weekly GRPs already adjusted for audience delivery and calculates the individual, net and gross average reach over a series 
# of weeks. It calls the AdResponse function (in flighting), which calls Rolling Sum, Adstock and Reach.

# It includes adjustment for Influence factor of media 

# ! Move adjustment of influence factor
MultiChannelWeeklyReachOpt=function(afLYGRPs, afTYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nEffFreq, nRecFreq, nPeriod, bAverage){
  #Calculate Week-by-week multichannel reach

  # number of weeks = the number of rows in this year's GRPs
  nWeeks=nrow(afTYGRPs)

  # bind historical and current year GRPs into 1 matrix
  colnames(afLYGRPs)=colnames(afTYGRPs)
  afGRPs=as.matrix(rbind(afLYGRPs, afTYGRPs))

  # create a dataframe for reach output, called 'afReach' which is the same size as afGRPs
  # (same # of media types (cols), same no of weeks (rows))

  #afReach <- FindNetReach(afInputs, afGRPs, afSeasonality, nEffFreq, nRecFreq, nPeriod, afCoeffs, nWeeks, afTYGRPs, bAverage)
  afReach <-  FindIndividualandGrossReach(afInputs, afGRPs, afSeasonality, nEffFreq, nRecFreq, nPeriod, afCoeffs, nWeeks, afTYGRPs, bAverage)
}
  
# MultiChannelExclusiveReach is a function called by PHD Source, not Annalect's Scenario tool.
# The function is called after a user or the optimization have determined the desired investment for each media channel.
# It calculates the exclusive reach of each media channel by first finding the net reach of the total campaign, then looping
# through each channel to calculate the change in net reach if the channel was not included in the campaign (investment == 0).
# Exclusive reach is defined as the reach of a campaign WITH the media channel at its current level of investment 
# minus the reach of a campaign WITHOUT the media channel at its current level of investment.

# Steps:
# 1. Calculate net reach with ALL of the channels (using Ian's for loop)
# 2. For each channel, calculate net reach without the media channel
# 3. Exclusive reach = net reach of campaign WITH investment in channel - net reach of campaign WITHOUT investmentin channel
MultiChannelExclusiveReach=function(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage){

  # 1. Calculate net reach with ALL of the channels (using Ian's for loop)
  afTotal<- MultiChannelReach(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage)
  afFinal <- afTotal
  
  # 2. For each channel, calculate net reach without the media channel
  for (i in 1:nrow(afInputs)){
    if (afInputs[i,2]==0){
      # No investment - exclusive reach =0
      afFinal[,i]=0
    }else{
      afDeltaInputs=afInputs
      afDeltaInputs[i,2]=0
      afExclusive=FindNetReach(afLYGRPs, afDeltaInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage)
      
      # 3. Exclusive reach = net reach of campaign WITH investment in channel - net reach of campaign WITHOUT investmentin channel
      afFinal[,i]=afTotal[,nrow(afInputs)+1]-afExclusive[,nrow(afInputs)+1]
    }
    
  # format outputs  
  }
  afFinal=afFinal[,1:nrow(afInputs)]
  afFinal <- as.data.frame(afFinal)
  colnames(afFinal) <- c(1:nrow(afInputs))
  
  return(afFinal)
}

# Function returns marginal step size and corresponding investment increase for each media channel.
MultiChannelMarginals=function(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross, fStep){

  # optimization, so bAverage = TRUE
  #bAverage <- TRUE
  # afCurrent returns current reach with investment set to min
  # columns= MediaTypes + Net.Reach + Gross.Reach, rows = 1 (if bAverage=true), and obs = average reach
  afCurrent=MultiChannelReachOpt(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, 
                                 bAverage = TRUE)
  
  # create afMarginal dataframe of 2 cols, as many rows as mediaTypes
  afMarginal=afInputs[,7:8]
  
  # initially set all values in afMarginal df to 0
  afMarginal[]=0
  
  # if optimizing against gross reach
  #if (bGross){
    
    # set base score to the far right value (gross reach) in the afCurrent df
    fBaseScore=afCurrent[,ncol(afCurrent)]
  
  # set afTest to afInputs
  afTest=afInputs
  
  # for each mediaType (each row of inputs)
  for (j in 1:nrow(afInputs)){
    
    # if the change flag is TRUE
    if (afInputs[j,10]=='TRUE'){

      # if the current investment + one incremental step of increase is less than max
      if ((afInputs[j,2]+fStep)<=afInputs[j,7]){
        
        # there's room to increase investment in that media type a full step so
        # set test investment to the current investment + step worth of investment in that media
        afTest[j,2]=afInputs[j,2]+fStep
        
        # and set prior marginal (second column of afMarginal) to the investment step
        afMarginal[j,2]=fStep
        
        # if maximum investment is in danger of being met with full step 
      } else {
        
        # if the current level of investment for the media = the maximum for that media
        if (afInputs[j,2]==afInputs[j,7]){
          
          # set afTest investment to the investment in inputs- you can't increase investment beyond max
          afTest[j,2]=afInputs[j,2]
          
          # AND set prior marginal step size to 0- nothing changes
          afMarginal[j,2]=0  
          
          # if current level of investment NOT equal to the max
        }else{
          
          # set prior marginal step size to maximum investment - current investment 
          afMarginal[j,2]=afInputs[j,7]-afInputs[j,2]
          
          # the investment amount in afTest is equal to the max amount
          afTest[j,2]=afInputs[j,7]     
        }
      }
    
      # afScenarioTest is a matrix of average reach data; columns= MediaTypes + Net.Reach + Gross.Reach, rows = 1 (if bAverage=true)
      afScenarioTest=MultiChannelReachOpt(afLYGRPs, afTest, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, 
                                          bAverage = TRUE)
      
      # this is also called earlier-- should just make it a function
      # if optimizing against gross reach
      #if (bGross){
        
      # set base score to the far right value (gross reach) in the afCurrent df
        fTestScore=afScenarioTest[,ncol(afScenarioTest)]
      #} else {
        
      # set base score to the value one to the left of the far right (net reach) of the afCurrent df
        #fTestScore=afScenarioTest[,ncol(afScenarioTest)-1]
      #}
      
      # make column 1 in afMarginals = testscore - basescore (the difference in reach when you calculate the current scenario vs a new one
      # with increased investment)
      afMarginal[j,1]=fTestScore-fBaseScore
      
      # reset the test matrix's investment back to the investment input into the function
      afTest[j,2]=afInputs[j,2]

    } else {
      
      # change flag is FALSE (no change made), column 8 in afInputs = how much the budget has changed from previous iteration
      #  col 9 (prior marginal step) set at prior marginal step in afMarginals
      afMarginal[j,1]=afInputs[j,8]
      afMarginal[j,2]=afInputs[j,9]
    }
  }
  
  return(afMarginal)
  #Column 1 = Marginal Step Size
  #Column 2 = $ Increase
}

OptimizeMultiChannelReach=function(fTotalBudget,afLYGRPs,afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross){
  # print the real and CPU time (in seconds) that the function takes

  # added by Jon and Jackie when we commented out net reach calculations
  # in optimization, always optimize based on gross reach
  bGross <- TRUE
  
  # optimization, so bAverage = TRUE
  #bAverage <- TRUE     # JK Feb 28 2015
  
  # set investment for each media type to its minimum 
  afInputs[,2]=afInputs[,6]
  
  # create new columns in afInputs
  # (col 8 = Prior Marginal, 9 = Prior Marginal Step, 10=ChangeFlag)
  # initially sets them to 0, 0 and TRUE
  afInputs[,8]=0
  afInputs[,9]=0
  afInputs[,10]=TRUE
  
  # Current Budget= sum of user input for investment (0 during first optimization)
  fCurrentBudget=sum(afInputs[,2])
  
  # creates Remaining Budget var, which is total budget - sum of user inputted investment for media types
  fRemaining=fTotalBudget-fCurrentBudget
  
  #Ian says: fStep=fRemaining/(4*nrow(afInputs))
  # create a Step var, which is the Total budget - current budget / number of steps (hardcoded to 50)
  fStep=fRemaining/50
  
  #hillclimb to an approximate solution

  for (i in 1:50){    
    
    # # retrieve marginal information
    #  notice that each time you run this, it gives you the marginal of an increase for the level of investment you are currently at, 
    # which increases throughout the loop - marginals should decrease given diminishing marginal returns
    afMarginals=MultiChannelMarginals(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross, fStep)
    
    # change flag set to FALSE (no change)
    afInputs[,10]=FALSE
    
    # (col 8 = Prior Marginal set to afMarginals marginal step size
    afInputs[,8]=afMarginals[,1]
    
    #(col 9 = Prior Marginal Step set to afMarginals $ increase
    afInputs[,9]=afMarginals[,2]
    
    # if the max marginal step is positive (aka, the biggest of the marginals is a positive return)
    if(max(afMarginals[,1])>=0){
      
      # set the investment of the media with the max marginal step size to the investment so far + the $ increase
      afInputs[which.max(afMarginals[,1]),2]=afInputs[which.max(afMarginals[,1]),2]+afMarginals[which.max(afMarginals[,1]),2]
      
      # the change flag becomes true- change has been made
      afInputs[which.max(afMarginals[,1]),10]=TRUE
      
      #fStep is equal to the step - any difference between steps caused by a less than complete investment step size
      fStep=fStep+(fStep-afMarginals[which.max(afMarginals[,1]),2])
    }

  }

  #  afScenarioTest is matrix; columns= MediaTypes + Net.Reach + Gross.Reach; rows = 1 (if bAverage=true); obs = average reach
  # this test shows you the reach of the current allocation, as found by the above loop
  afScenarioTest=MultiChannelReachOpt(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage = TRUE)
  
  # if we want to maximize based on gross reach
  #if (bGross){
    
    # test score A is the far right (Gross Reach) of the afScenarioTest matrix
    fTestScorea=afScenarioTest[,ncol(afScenarioTest)]
    
    # if we want to maximize based on net reach
 # } else {
    
    # test score A is one to the left of the far right (Net Reach) of the afScenarioTest matrix
    #fTestScorea=afScenarioTest[,ncol(afScenarioTest)-1]
  #}

  # Phase 2: Adjust Hillclimbed response by ratio of marginals to get to optimal solution (Quasi-Newton Process)
  
  # step size = total budget / 100
  fStep=fTotalBudget/100
  
  i=1

  # (col 8 = Prior Marginal, 9 = Prior Marginal Step, 10=ChangeFlag) reset to original values
  afInputs[,8]=0
  afInputs[,9]=0
  afInputs[,10]=TRUE
  
  # from 1 to 50:
  while (i <50){
    
    #Ian says: Calculate Marginals
    afMarginals=MultiChannelMarginals(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross, fStep)

    # col 8 of afInputs = marginal step size of prior calculation, 
    # col 9 = $ Increase, ChangeFlag says YES, change has been made
    afInputs[,10]=TRUE   
    afInputs[,8]=afMarginals[,1]
    afInputs[,9]=afMarginals[,2]

    # afMarginals is now a two column df with corresponding info
    # column 1 = Marginals
    # column 2 = Investment
    afMarginals=cbind(afMarginals,afInputs[,2])    

    # check if there's room to decrease investment (room to increase by full step has been handeled by MultiChannelMarginals)
    
    # for each row of afMarginals
    for (j in 1:nrow(afMarginals)){
      
      # if the investment level - 1 step <= minimum investment
      if (afInputs[j,2]-fStep<=afInputs[j,6]){
        
        # set marginals to 0- there's no room to decrease these, so we have to leave them alone
        afMarginals[j,1]=0
      }
    }
    
    # subset marginals that are greater than 0 (the ones we could decrease)
    afSubsetMarginals=as.data.frame(subset(afMarginals[,1], afMarginals[,1]>0))
    
    # 3rd column of afMarginals = marginal / colmeans(marginal) - 1 
    # ! need to review this
    afMarginals[,3]=afMarginals[,1]/colMeans(afSubsetMarginals)[1]-1
    
    # if marginals = 0, make third column of afMarginals = 0
    # ! should vectorize this
    for (j in 1:nrow(afMarginals)){
      if (afMarginals[j,1]==0){
        afMarginals[j,3]=0
      }
    }
    
    # set investment for each media type to the previously 
    # allocated investment +  suggested increase in investment
    afInputs[,2]=afInputs[,2]+afMarginals[,3]*fStep
    
    # size of the next step is equal to the prior step * the largest value in the third column of afMarginals
    fStep=fStep*max(afMarginals[,3])
    
    # Check Tolerance on improvement in overall optimal
    afScenarioTest=MultiChannelReachOpt(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, TRUE)
    
    # if / else commented out since we have hard coded bGross as true
    #if (bGross){
      fTestScoreb=afScenarioTest[,ncol(afScenarioTest)]
    #} else {
      #fTestScoreb=afScenarioTest[,ncol(afScenarioTest)-1]
    #}
    
    # if we get super close to the optimal solution
    if (fTestScoreb-fTestScorea<0.00001){
      
      # just cut the loop short
      i=50
      
      # otherwise keep going through that optimization loop
    }else {
      i=i+1
    }
    fTestScorea=fTestScoreb
    
  }
  
  # output is the investment per mediatype (row)
  afOutput=as.data.frame(afInputs[,2]) 
  
  return(afOutput)
}

# This is the main optimization function- it starts everything off.
OptimizeAllOrNoneMultiChannel=function(fTotalBudget,afLYGRPs,afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross){

  # Jon and Jackie inserted this variable. Previously the tool calculated using if/else statements and for loops
  # so that it could optimize against gross (TRUE) or net (FALSE) reach. We are hard coding the calculations
  # to true during the optimization.
  bGrossInput <- bGross
  bGross <- TRUE
  
  # optimization, so bAverage = TRUE
  # bAverage <- TRUE     JK Feb. 28 2015
  
  # Does BASE calculations- first round of calculating investment per media
  afBase=OptimizeMultiChannelReach(fTotalBudget,afLYGRPs,afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross)
  
  # begins next round of optimization by setting the cost per media type to result of afBase
  afInputs[,2]=afBase
  
  # record starting investment levels
  afResults=as.data.frame(afInputs[,2])
  
  # create columns in afInputs to Record whether the budget has changed from previous iteration 
  #(col 8 = Prior Marginal, 9 = Prior Marginal Step, 10=ChangeFlag)
  afInputs[,8]=0
  afInputs[,9]=0
  afInputs[,10]=TRUE
  
  # fstep becomes total budget divided by the number of media types being allocated 
  fStep=fTotalBudget/nrow(afInputs)
  
  # find Marginals of each media
  afMarginals=MultiChannelMarginals(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross, fStep)
  
# for each media being optimized
  for (i in 1:nrow(afInputs)){
    
    # if the initial optimization suggests you allocate the minimum amount of investment specified for the mediaType
    if (afInputs[i,2]==afInputs[i,6]){
      
      # set change flag to FALSE
      bHigher=FALSE
      
      # for each row (MediaType) in afInputs
      for (j in 1:nrow(afInputs)){
        
        # if every other MediaType's marginals are greater than the current MediaType
        if (afMarginals[j,1]>afMarginals[i,1]){
          
          # set change flag to TRUE
          bHigher=TRUE
        }
      }
      
      # if change flag is true 
      if (bHigher){
        
        # set min and max to 0 of the current MediaType to 0- no more changes in investment
        afInputs[i,6]=0
        afInputs[i,7]=0
        
        # set marginal to 0- to make sure we don't increase this investment any further
        afMarginals[i,1]=0
      }
      
      # if  sum of the max inputs is still greater than the total budget
      if(sum(afInputs[,7])>=fTotalBudget){
        
        # run through optimize multichannelreach
        afInputs[,2]=OptimizeMultiChannelReach(fTotalBudget,afLYGRPs,afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bGross)
        
        # afresults is a df with each row being a suggested allocation of media investment
        afResults=cbind(afResults, afInputs[,2])
      }
      
    }
  }
  
  #Evauate each iteration of investment
  
  # set MaxScore to 0 and MaxCol to 1
  fMaxScore=0
  fMaxCol=1
  
  # for each row of afResults (aka each column with a suggested allocation of media investment based on marginals)
  for (i in 1:ncol(afResults)){
    
    # set investment to the suggested allocation of media investment
    afInputs[,2]=afResults[,i]
    
    # set output to the media's reach
    output=MultiChannelReachOpt(afLYGRPs, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, TRUE)
    
    # Score = far right hand side and bottom of reach (I think that's total gross reach)
    fScore=output[nrow(output), ncol(output)]
    
    # if the investment's total gross reach is greater than the MaxScore (initially set to 0)
    if (fScore>fMaxScore){
      
      # set the maxScore ot the investment's total gross reach
      fMaxScore=fScore
      
      # check out the next column
      fMaxCol=i
    }
  }
  
  # the optimizations resulting suggested investment per media type (rows) is the one with the highest total gross reach
  afOutput=as.data.frame(afResults[,fMaxCol]) 
  
  
  return(afOutput)
  
}

