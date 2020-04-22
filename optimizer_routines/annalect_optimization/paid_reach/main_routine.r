library(RJSONIO)

# Map 1-based optional input ports to variables
 POST <- maml.mapInputPort(1) # class: data.frame
# dataset2 <- maml.mapInputPort(2) # class: data.frame

# Contents of optional Zip port are in ./src/
 install.packages("src/RJSONIO.zip", lib = ".", repos = NULL, verbose = TRUE)
 source("src/rFlighting.R");
 source("src/rChannelPlanning.R");
 source("src/rAdjustmentFactors.R");
 source("src/OptimizeMultiChannelReach.R");
 source("src/OptimizeAllOrNoneMultiChannel.R");
 source("src/MultiChannelWeeklyReach.R");
 source("src/MultiChannelReachAllocation.R");
 source("src/MultiChannelReach.R");
 source("src/MultiChannelExclusiveReach.R");
 source("src/MultiChannelConsolidatedReach.R");

starttime <- Sys.time() 

PostNew<-POST

outputnew<-data.frame()

for (i in 1:nrow(PostNew)) {

POST <- PostNew[i,]
 
afMedia<-unlist(strsplit(as.character(POST$Media),","))

afGRPsLY<-unlist(strsplit(as.character(POST$GRPsLY),";"))
afGRPsLY<-unlist(strsplit(afGRPsLY,","))
afGRPsLY<-as.numeric(afGRPsLY)
afGRPsLY<-matrix(afGRPsLY,ncol=length(afMedia))
colnames(afGRPsLY)<-afMedia

afCoeffs<-unlist(strsplit(as.character(POST$Coeffs),";"))
afCoeffs<-unlist(strsplit(afCoeffs,","))
afCoeffs<-as.numeric(afCoeffs)
afCoeffs<-matrix(afCoeffs,ncol=length(afMedia))
colnames(afCoeffs)<-afMedia

afOverlapCoeffs<-unlist(strsplit(as.character(POST$OverlapCoeffs),";"))
afOverlapCoeffs<-unlist(strsplit(afOverlapCoeffs,","))
afOverlapCoeffs<-as.numeric(afOverlapCoeffs)
afOverlapCoeffs<-matrix(afOverlapCoeffs,ncol=length(afMedia))
colnames(afOverlapCoeffs)<-afMedia

afCurves<-unlist(strsplit(as.character(POST$Curves),","))
afCurves<-as.numeric(afCurves)
nEffFreq<-afCurves[1]
nRecFreq<-afCurves[2]
nPeriod<-afCurves[3]

afInputsInvestment<-unlist(strsplit(as.character(POST$InputsInvestment),","))
afInputsInvestment<-as.data.frame(as.numeric(afInputsInvestment))

afInputsCPP<-unlist(strsplit(as.character(POST$InputsCPP),","))
afInputsCPP<-as.data.frame(as.numeric(afInputsCPP))

afInputsADF<-unlist(strsplit(as.character(POST$InputsADF),","))
afInputsADF<-as.data.frame(as.numeric(afInputsADF))

afInputsIF<-unlist(strsplit(as.character(POST$InputsIF),","))
afInputsIF<-as.data.frame(as.numeric(afInputsIF))

afInputsMin<-unlist(strsplit(as.character(POST$InputsMin),","))
afInputsMin<-as.data.frame(as.numeric(afInputsMin))

afInputsMax<-unlist(strsplit(as.character(POST$InputsMax),","))
afInputsMax<-as.data.frame(as.numeric(afInputsMax))

#afInputsMedia<-rep(0,length(afInputsInvestment))
afInputsMedia<-unlist(strsplit(as.character(POST$Media),","))

#afInputs8<-rep(0,length(afInputsInvestment))
#afInputs9<-rep(0,length(afInputsInvestment))
#afInputs10<-rep(0,length(afInputsInvestment))

afInputs<-cbind(afInputsMedia,afInputsInvestment,afInputsCPP,afInputsADF,afInputsIF,afInputsMin,afInputsMax)
#colnames(afInputs)=afMedia
#afInputs<-t(afInputs)
afInputs<-as.data.frame(afInputs)

nWeeks<-as.numeric(as.character(POST$NumWeeks))

bAverage<-as.logical(as.character(POST$bAverage))

fTotalBudget<-as.numeric(as.character(POST$TotalBudget))

bGross<-as.logical(as.character(POST$bGross))

bAllOrNone<-as.logical(as.character(POST$bAllOrNone))

afSeasonality<-unlist(strsplit(as.character(POST$Seasonality),","))
afSeasonality<-as.numeric(afSeasonality)
afSeasonality<-matrix(afSeasonality,nrow=length(afSeasonality))


output=MultiChannelReach(afGRPsLY, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage)
colnames(output) = c(colnames(afGRPsLY), "Net.Reach", "Gross.Reach")

outputnew<-rbind(outputnew,output)

}

endtime <- Sys.time()
timediff <- difftime(endtime, starttime, units="secs")

Output<-list("MultiChannelReach"=as.matrix(outputnew),
Diagnostics=list("StartTime"=format(starttime, "%F %H:%M:%S"), 
"EndTime"=format(endtime, "%F %H:%M:%S"), 
"TimeDiff"=format(timediff)))
              
Output <- data.frame(Output)

maml.mapOutputPort("Output");