
POST <- maml.mapInputPort(1) # class: data.frame

source("src/CompleteFunctions.r")


starttime <- Sys.time()

Media <- unlist(strsplit(as.character(POST$Media),","))

Parameters <- unlist(strsplit(as.character(POST$Coeffs),";"))
Parameters <- unlist(strsplit(Parameters,","))
Parameters <- as.numeric(Parameters)
Parameters <- matrix(Parameters,ncol=length(Media))
colnames(Parameters) <- Media

CPP <- unlist(strsplit(as.character(POST$InputsCPP),","))
CPP <- t(as.numeric(CPP))
colnames(CPP) <- Media

afGRPsLY<-unlist(strsplit(as.character(POST$GRPs),","))
afGRPsLY<-as.numeric(afGRPsLY)
afGRPsLY<-matrix(afGRPsLY,ncol=length(Media))
colnames(afGRPsLY)<-Media

afOverlapCoeffs<-unlist(strsplit(as.character(POST$OverlapCoeffs),";"))
afOverlapCoeffs<-unlist(strsplit(afOverlapCoeffs,","))
afOverlapCoeffs<-as.numeric(afOverlapCoeffs)
afOverlapCoeffs<-matrix(afOverlapCoeffs,ncol=length(Media))
colnames(afOverlapCoeffs)<-Media

afCurves<-unlist(strsplit(as.character(POST$Curves),","))
afCurves<-as.numeric(afCurves)
nEffFreq<-afCurves[1]
nRecFreq<-afCurves[2]
nPeriod<-afCurves[3]

afInputsInvestment<-unlist(strsplit(as.character(POST$InputsInvestment),","))
afInputsInvestment<-as.numeric(afInputsInvestment)

afInputsADF<-unlist(strsplit(as.character(POST$InputsADF),","))
afInputsADF<-as.numeric(afInputsADF)

afInputsIF<-unlist(strsplit(as.character(POST$InputsIF),","))
afInputsIF<-as.numeric(afInputsIF)

afInputsMin<-unlist(strsplit(as.character(POST$InputsMin),","))
afInputsMin<-as.numeric(afInputsMin)

afInputsMax<-unlist(strsplit(as.character(POST$InputsMax),","))
afInputsMax<-as.numeric(afInputsMax)

afInputsMedia<-rep(0,length(afInputsInvestment))

afInputs8<-rep(0,length(afInputsInvestment))
afInputs9<-rep(0,length(afInputsInvestment))
afInputs10<-rep(0,length(afInputsInvestment))

afInputs<-rbind(afInputsMedia,afInputsInvestment,CPP,afInputsADF,afInputsIF,afInputsMin,afInputsMax,afInputs8,afInputs9,afInputs10)
colnames(afInputs)=Media
afInputs<-t(afInputs)
afInputs<-data.frame(afInputs)

nWeeks<-as.numeric(as.character(POST$NumWeeks))

bAverage<-as.logical(as.character(POST$bAverage))

OptimizeSalesRevenue<-as.logical(as.character(POST$OptimizeSalesRevenue))

bGross<-as.logical(as.character(POST$bGross))

bAllOrNone<-as.logical(as.character(POST$bAllOrNone))

afSeasonality<-unlist(strsplit(as.character(POST$Seasonality),","))
afSeasonality<-as.numeric(afSeasonality)
afSeasonality<-matrix(afSeasonality,ncol=length(afSeasonality))

Budget <- as.numeric(as.character(POST$TotalBudget))

n <- Budget/10^max(min(OrderOfMagnitude(Budget)-4,OrderOfMagnitude(min(afInputsMax))-1),1)

stepsize <- Budget/n

maxSpend <- floor(afInputsMax/stepsize)

spend <- rep(floor(Budget/stepsize/length(Media)),length(Media))

minSpend <- ceiling(afInputsMin/stepsize)

spend[1] <- spend[1] + floor(n) - sum(spend)

spend <- apply(cbind(spend,minSpend),1,max)




spend <- OptimizerMO(spend,minSpend,maxSpend,stepsize,Budget,Parameters,CPP,OptimizeSalesRevenue,n)




spend <- data.frame(spend)
rownames(spend) <- Media
colnames(spend) <- c('Optimum Spend')


afInputs[,2] = spend
OptimizationOutput=t(spend)
colnames(OptimizationOutput) = colnames(afGRPsLY)

MultiChannelReachOutput=MultiChannelReach(afGRPsLY, afInputs, Parameters, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage)
colnames(MultiChannelReachOutput) = c(colnames(afGRPsLY), "Net.Reach", "Gross.Reach")

MultiChannelExclusiveReachOutput=MultiChannelExclusiveReach(afGRPsLY, afInputs, Parameters, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage = TRUE)
MultiChannelExclusiveReachOutput = as.data.frame(MultiChannelExclusiveReachOutput)
colnames(MultiChannelExclusiveReachOutput) = colnames(afGRPsLY)

endtime <- Sys.time()
timediff <- difftime(endtime, starttime, units="secs")


Output <- list(
      			"MultiChannelReach"=format(as.matrix(MultiChannelReachOutput), scientific=F, trim=T),
			"MultiChannelExclusiveReach"=format(as.matrix(MultiChannelExclusiveReachOutput), scientific=F, trim=T),
			"Optimization"=format(as.matrix(OptimizationOutput), scientific=F, trim=T),
			Diagnostics=list("StartTime"=format(starttime, "%F %H:%M:%S"), 
				"EndTime"=format(endtime, "%F %H:%M:%S"), 
				"TimeDiff"=format(timediff))
		      )
              
Output <- data.frame(Output)



maml.mapOutputPort("Output");