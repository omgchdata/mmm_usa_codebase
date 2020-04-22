# Dependencies:
#   EffCoverSeas
#   AdResponse

CurrentScenario=function(afGRPsMatLY, afGRPsMatCY, 
		afSeasonIndexLY, afSeasonIndexCY, 
		afPromoIndexLY, afPromoIndexCY, 
		afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, bSeasonal){

	#Generate Base Scenario for Comparison Scenario screen - either level or seasonally adjusted

	afGRPsMat=rbind(as.data.frame(afGRPsMatLY), as.data.frame(afGRPsMatCY))
	afSeasonIndex=rbind(as.data.frame(afSeasonIndexLY), as.data.frame(afSeasonIndexCY))
	afPromoIndex=rbind(as.data.frame(afPromoIndexLY), as.data.frame(afPromoIndexCY))

	output=as.data.frame(afGRPsMat)

	#Calculate AdResponse
	if (bSeasonal==TRUE){
		# Calculate Seasonaly Adjusted AdResponse
		output=cbind(output, EffCoverSeas(output, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, afSeasonIndex, afPromoIndex))
	}else{
		# Calculate Straight AdResponse
		output=cbind(output, AdResponse(output, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay))
	}


	#output just 2nd Year response
	nStart=1
	nEnd=nrow(output)
	output=cbind(output[nStart:nEnd,1], output[nStart:nEnd,2])
	colnames(output)=c(paste(colnames(output[1])," GRPs"), paste(colnames(output[1])," AdResponse"))

	return(output)

}


# Dependencies:
#   AdResponse

DimRets=function(afGRPsMatLY, afGRPsMatCY, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, fMinGRPs, fMaxGRPs){
		
	#calculate the sum of CY GRPs
	fCYTotal = cumsum(afGRPsMatCY)[nrow(afGRPsMatCY), 1]
  
	#Set output matrix size
	afDimRets=matrix(nrow=51, ncol=3)
	
	#first values of output to be zero (always go through the origin)
	afDimRets[1,1]=0
	afDimRets[1,2]=0
	afDimRets[1,3]=0
  
	
  if (fCYTotal!=0){
    #There are at least some GRPs
    
    #Change CY GRPs to be % of flight
    afGRPsMatCY = afGRPsMatCY/fCYTotal
    
    #Create 50 steps on the diminishing returns chart
    for (i in 1:50){
      fTotal=i/50*(fMaxGRPs-fMinGRPs)+fMinGRPs
      
      afTest = as.data.frame(afGRPsMatCY) * fTotal
      afTest = rbind(afGRPsMatLY, afTest)
      
      afAdResponse = AdResponse(afTest, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay)
      
      #exclude first year from calculation
      for (j in 1:nrow(afAdResponse)){
        if (j <= nrow(afGRPsMatLY)){
          afAdResponse[j,1]=0
        }
      }
      #GRPs
      afDimRets[i+1,1]=fTotal
      #AdResponse
      afDimRets[i+1,2]=cumsum(afAdResponse)[nrow(afAdResponse)]
      #Marginal
      afDimRets[i+1,3]=afDimRets[i+1,2]-afDimRets[i,2]
    }
  } else {
    #Total GRPs = 0 - return matrix of zeros
    for (i in 1:50){
      fTotal=i/50*(fMaxGRPs-fMinGRPs)+fMinGRPs
      afDimRets[i+1,1]=fTotal
      afDimRets[i+1,2]=0
      afDimRets[i+1,3]=0
    }
  }
  
	return(afDimRets)

}

# Dependencies:
#   AdResponse

EffCoverSeas=function(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, afSeasonIndex, afPromoIndex){
	# Generate Seasonally Adjusted AdResponse
	
	# Generate Regular AdResponse
	
	output=AdResponse(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay)
	
	# Normalize the seasonal Indices
	
	if (sum(afSeasonIndex)!=0){
		afNormSeas=afSeasonIndex/colMeans(afSeasonIndex)
	}else{
		afNormSeas=afSeasonIndex
		afNormSeas=1
	}
	
	if (sum(afPromoIndex)!=0){
		afNormPromo=afPromoIndex/colMeans(afPromoIndex)
	}else{
		afNormPromo=afPromoIndex
		afNormPromo=1
	}
	
	afFinalIndex=(afNormSeas + afNormPromo)/2
	
	output=output*afFinalIndex
	
	return(output)
	
}



# Dependencies:
#   EffCoverSeas

SeasEffCover=function(afMatrix, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay){
  
	output=EffCoverSeas(afMatrix[1], afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, afMatrix[2], afMatrix[3])
	return(output)
  
}


# Dependencies:
#   TotalSeasonalEffCover

SeasEffCoverSum=function(afMatrix, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay){

  # Call the sum of Seasonal Effective Cover using combined matrix
	output=TotalSeasonalEffCover(afMatrix[1], afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, afMatrix[2], afMatrix[3]) 
	return(output)
  
}



# Dependencies:
#   AdResponse

TotalAdResponse=function(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay){
	#Calculate Sum of The weeks of the AdResponse
	
	output=AdResponse(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay)
	output=sum(output)
	return(output)
}


TotalCost=function(afGRPsMat, afCPPMat){
  
	# Calculate Total Cost of the media flight
	output=afGRPsMat %*% t(afCPPMat)
	return(sum(output))
  
}


TotalMatrixSeries=function(afGRPsMat){
	output=sum(afGRPsMat)
	return(output)
}


# Dependencies:
#   EffCoverSeas

TotalSeasonalEffCover=function(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, afSeasonIndex, afPromoIndex){
  # Sum the Seasonally Adjusted AdRepsponse
  output=sum(EffCoverSeas(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, afSeasonIndex, afPromoIndex))
}



# Dependencies:
#   EffCoverSeas
#   AdResponse

ComparisonScenario=function(nScenario, afScenarios, 
		afGRPsMatLY, afGRPsMatCY, 
		afCPPMatLY, afCPPMatCY, 
		afSeasonIndexLY, afSeasonIndexCY, 
		afPromoIndexLY, afPromoIndexCY, 
		afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, bCost, bSeasonal){

	#Generate Comparison Scenario - either AdResponse or GRPs

	afScenarioWeeks=afScenarios[nScenario]

	afSeasonIndex=rbind(as.data.frame(afSeasonIndexLY), as.data.frame(afSeasonIndexCY))
	afPromoIndex=rbind(as.data.frame(afPromoIndexLY), as.data.frame(afPromoIndexCY))
 
	#Edit current year GRPs
	if (bCost==TRUE){
		#If Cost-constant scenario
		#output=afGRPsMatCY*afCPPMatCY

		totalyear=t(afGRPsMatCY) %*% afCPPMatCY
    
    if (totalyear!=0){
      dollaralloc=afScenarioWeeks*afCPPMatCY
      outputcurrency=totalyear*(dollaralloc/sum(dollaralloc))
      output=outputcurrency/afCPPMatCY  
    }else{
      #No spend recorded - set equal to current year
      output=afGRPsMatCY  
    }
		
	}else{
		#Else Constant GRP Scenario

		output=afGRPsMatCY
    if (sum(afGRPsMatCY)!=0){
      output=sum(afGRPsMatCY)*(afScenarioWeeks/sum(afScenarioWeeks))  
    } else {
      #No GRPs entered - set equal to current year (All zeros)
      output=afGRPsMatCY
    }
		
	}
  
  for (i in 1:nrow(output)){
    if (is.na(output[i,1])){
      output[i,1]=0
    }
    if (is.nan(output[i,1])){
      output[i,1]=0
    }
    if (is.infinite(output[i,1])){
      output[i,1]=0
    }

  }
  
	#Combine both year's GRPs
	output=as.data.frame(rbind(as.matrix(afGRPsMatLY), as.matrix(output)))

	#Calculate AdResponse
	if (bSeasonal==TRUE){
		# Calculate Seasonaly Adjusted AdResponse
		output=cbind(output, EffCoverSeas(output, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay, afSeasonIndex, afPromoIndex))
	}else{
		# Calculate Straight AdResponse
		output=cbind(output, AdResponse(output, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay))
	}


	#chart just 2nd Year response
	nStart=1
	nEnd=nrow(output)

	output=cbind(output[nStart:nEnd,1], output[nStart:nEnd,2])
	colnames(output)=c(paste(colnames(output[1])," GRPs"), paste(colnames(output[1])," AdResponse"))

	return(output)

}



Reach=function(fa, fb, fc, fGRPs) {
	# Return reach data subject to fitted formula to a value
	#	r=a/(1+b*(GRPs/1000)^c)
	#
	
	# fa = Alpha Coefficient in reach model
	# fb = Beta Coefficient in reach model
	# fc = Gamma Coefficient in reach model
	# fGRPs = single data point of GRPs at which to calculate reach
		
	if (fGRPs==0){
		#If GRPs=0 then set Reach=0 to avoid error
		fReach=0
	} else {
		fReach=fa/(1+fb*(fGRPs/1000)^fc)
	}
		
	#Return calculated reach value
	return(fReach)
	
	#Example Use of Reach Function (solution=1.222065)
	#test=Reach(0.79,-1,0.5,125)	
		
	}


	RollingSum=function(afGRPsMat, fDecay, nPeriod){
	# Create a rolling sum of an AdStock to a vector of data
	#
	#
	
	# afGRPsMat = matrix (vertical vector) of GRP data
	# fDecay = single data point, decimal decay rate of media
	# nPeriod = integer value of number of observations to sum over
	
	afRollingSum=matrix(1:nrow(afGRPsMat))
	for (i in 1:nrow(afGRPsMat)){
		afRollingSum[i,1]=0
		#Loop through each observation in turn
		for (j in 1:i){
			if (j<=nPeriod){
				#If within the recency period then Calculate the AdStocked GRPs within the period
				afRollingSum[i,1] = afRollingSum[i,1] + afGRPsMat[i-j+1,1] * (1-fDecay) ^ (j-1)				
			}
		}
	}
	
	#Return the rolling sum of data
	return(afRollingSum)
	
	#Example use of Function
	#test=RollingSum(afGRPs, 0.15, 4)
	}	


SumProduct=function(afGRPs, afWeights){
	# Calculate the weighted sum of a series of variables (GRPs)
	
	# afGRPs = matrix of GRPs, variables as rows, observations as columns
	# afWeights = matrix - one row by number of variables columns, or weights to apply to GRPs
	
	afProduct=afGRPs %*% t(afWeights)
	
	# Return the Sum
	return(afProduct)
	
	# example use of function
	#test=SumProduct(afGRPs, afWeights)
	
	}



			AdResponse=function(afGRPsMat, afCoeffsMat, nEffFreq, nRecFreq, nPeriod, fDecay){

                # Generate the Effective Cover of a vector of input GRPs


                # afGRPsMat = matrix (vertical vector) of GRP data
                # afCoeffsMat = matrix (10 cols by 3 rows) of coefficients for reach models from 1+ to 10+
                # nEffFreq = integer value of Effective Frequency Parameter
                # nRecFreq = integer value of Recency Frequency Parameter
                # nPeriod = integer value of Response Period Parameter
                # fDecay = decimal value of decay rate parameter

                #Define output matrix size
                afAdResponse=matrix(1:nrow(afGRPsMat))

                fEffGRPs=RollingSum(afGRPsMat, fDecay, nPeriod)
                fTotalEffGRPs=AdStock(afGRPsMat, fDecay)

                for (i in 1:nrow(afGRPsMat)){
                                if (nRecFreq==0){
                                                #Calculate the x+0 model
                                                afAdResponse[i,1]=Reach(afCoeffsMat[1, nEffFreq], afCoeffsMat[2, nEffFreq], afCoeffsMat[3, nEffFreq], fTotalEffGRPs[i,1])
                                } else {
                                                if (nRecFreq==nEffFreq){
                                                                #Calculate the x+x model
                                                                afAdResponse[i,1] = Reach(afCoeffsMat[1, nEffFreq], afCoeffsMat[2, nEffFreq], afCoeffsMat[3, nEffFreq], fEffGRPs[i,1])
                                                } else {
                                                                #Calculate the x+y(<x) Model
                                                                if (round(fTotalEffGRPs[i,1], digits=6)==round(fEffGRPs[i,1], digits=6)){
                                                                                # There is no extra history outside of the recency window
                                                                                afAdResponse[i,1]=Reach(afCoeffsMat[1, nEffFreq], afCoeffsMat[2, nEffFreq], afCoeffsMat[3, nEffFreq], fEffGRPs[i,1])
                                                                } else {
                                                                                #There is extra history outside of the recency window to be considered
                                                                                afAdResponse[i,1]=0
                                                                                for (k in nRecFreq:(nEffFreq-1)){
                                                                                                afAdResponse[i,1]=afAdResponse[i,1]+(Reach(afCoeffsMat[1,k], afCoeffsMat[2,k], afCoeffsMat[3,k], fEffGRPs[i,1])-Reach(afCoeffsMat[1,k+1], afCoeffsMat[2,k+1], afCoeffsMat[3,k+1], fEffGRPs[i,1]))*Reach(afCoeffsMat[1,nEffFreq-k], afCoeffsMat[2,nEffFreq-k], afCoeffsMat[3,nEffFreq-k], (fTotalEffGRPs[i,1]-fEffGRPs[i,1]))/100
                                                                                }
                                                                                afAdResponse[i,1]=afAdResponse[i,1]+Reach(afCoeffsMat[1,nEffFreq], afCoeffsMat[2,nEffFreq], afCoeffsMat[3,nEffFreq], fEffGRPs[i,1])
                                                                }
                                                }
                                }
                }

                #Return the calculated AdResponse
                return(afAdResponse)

                # example use of function
                #test=AdResponse(afGRPs, afCoeffs, 5, 1, 2, 0.25)
                }



					AdStock=function(afGRPsMat, fdecayRate){
	# Generate matrix of AdStocked/Decayed GRPs as a function of input GRPs and decay rate to a value
	#	y(t)=y(t-1)*d + x(t)
	# 
	
	# afGRPs = matrix (vertical vector) of GRP Data
	# fdecayRate = decimal version of decay rate
	
	# Create output matrix base on size of input
	afAdStockedGRPsMat=matrix(1:nrow(afGRPsMat))

	# first observations are equal
	afAdStockedGRPsMat[1,1]=afGRPsMat[1,1]

	#loop through calculating AdStocked GRPs
	for (x in 2:nrow(afGRPsMat)){
		afAdStockedGRPsMat[x,1]=afAdStockedGRPsMat[x-1,1]*(1-fdecayRate)+afGRPsMat[x,1]
	}

	#Return AdStocked GRPs matrix
	return(afAdStockedGRPsMat)
	
	# Example use of AdStock Function
	#
	#	test=AdStock(data.matric(GRPs[3]),0.15)
	
	}


AwarenessModel=function(afGRPs, afWeights, fPctAware, fPctBase, fPctLimit, fRetBase, fRetAboveBase, fAffectGRPs){
	# Calculate the Awareness Model for a Series of GRPs

	# afGRPs = matrix of GRPs - rows as observations, columns as media types
	# afWeights = matrix (row vector) of weights by media type to calculate weighted GRPs
	# fPctAware = value (0-100) of percentage of population already aware
	# fPctBase= value (0-100) of percentage of population in the base awareness
	# fPctLimit = value (0-100) of the limit of the percetage of the population who could become aware
	# fRetBase = decimal (0-1) of the retention rate to apply to base awareness
	# fRetAboveBase = decimal (0-1) of the retention rate to apply to awareness above base levels
	# fAffectGRPs = value of the coefficiency effect on GRPs
	
	
	
	#Create Weighted GRPs - note dependancy on prior function
	afWeightedGRPs=SumProduct(afGRPs, afWeights)
	#Generate calculation materices based on size of input GRP Matrix
	afBaseRetAdj=matrix(1:nrow(afWeightedGRPs))
	afAwWIP=matrix(1:nrow(afWeightedGRPs))
	afOutput=matrix(1:nrow(afWeightedGRPs))
	
	#Set initial observations
	afBaseRetAdj[1,1]=fPctBase
	afAwWIP[1,1]=afBaseRetAdj[1,1]+(fPctAware-fPctBase)+fAffectGRPs*(afWeightedGRPs[1,1]/100)*(fPctLimit-afBaseRetAdj[1,1])/fPctLimit
	afOutput[1,1]=0
	
	#Calculate subsequent observations
	for(i in 2:nrow(afWeightedGRPs)){
		afBaseRetAdj[i,1]=afBaseRetAdj[i-1,1]*fRetBase	
		afAwWIP[i,1]=fRetAboveBase*(afAwWIP[i-1,1]-afBaseRetAdj[i-1,1])+afBaseRetAdj[i,1]+fAffectGRPs*(afWeightedGRPs[i,1]/100)*(fPctLimit-(fRetAboveBase*(afAwWIP[i-1,1]-afBaseRetAdj[i-1,1])+afBaseRetAdj[i,1]))/fPctLimit
		afOutput[i,1]=0
		#Output is an 8-week rolling average of the data
		if (i>=8){
			for(j in 1:8){
				afOutput[i,1]=afOutput[i,1]+(afAwWIP[i-j+1,1]/8)
			}
		}
	}
	
	# Return Output
	return(afOutput)
	
	# example use of function
	# test=AwarenessModel(afGRPs, afWeights, 55,,10025,1.00,0.9,5.0)
	
	}	



