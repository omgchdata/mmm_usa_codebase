
library(RJSONIO);

################################

GetMultiChannelReachAllocation <- function() {

	starttime <- Sys.time()

	strGRPsLY <- as.character(POST$GRPsLY)
        oGRPsLY <- fromJSON(strGRPsLY)
        allGRPdata <- oGRPsLY$afGRPsLY
        GRPcolnames <- names(allGRPdata)
        afGRPsLY <- matrix(unlist(allGRPdata[GRPcolnames]), byrow=FALSE, ncol=length(GRPcolnames))
        colnames(afGRPsLY) = GRPcolnames
#print("afGRPsLY: ")
#print(afGRPsLY)


        strCoeffs <- as.character(POST$coeffs)
        oCoeffs <- fromJSON(strCoeffs)
        allcoeffsdata <- oCoeffs$afCoeffs
        coeffcolnames <- names(allcoeffsdata)
        afCoeffs <- matrix(unlist(allcoeffsdata[coeffcolnames]), byrow=FALSE, ncol=length(coeffcolnames))
        colnames(afCoeffs) = coeffcolnames
#print("afCoeffs: ")
#print(afCoeffs)


        strOverlapCoeffs <- as.character(POST$overlapCoeffs)
        oOverlapCoeffs <- fromJSON(strOverlapCoeffs)
        alloverlapdata <- oOverlapCoeffs$afOverlapCoeffs
        overlapcolnames <- names(alloverlapdata)
        afOverlapCoeffs <- matrix(unlist(alloverlapdata[overlapcolnames]), byrow=FALSE, ncol=length(overlapcolnames))
        colnames(afOverlapCoeffs) = overlapcolnames
#print("afOverlapCoeffs: ")
#print(afOverlapCoeffs)


        curves <- as.character(POST$curves)
        curves <- as.double(unlist(strsplit(curves,",")))
#print(paste("Curves: ", curves))

        nEffFreq <- curves[1]
        nRecFreq <- curves[2]
        nPeriod <- curves[3]
#print(paste("nEffFreq: ", nEffFreq))
#print(paste("nRecFreq: ", nRecFreq))
#print(paste("nPeriod: ", nPeriod))

        nWeeks <- as.numeric(POST$NumWeeks)
#print(paste("NumWeeks: ", nWeeks))
        
        bAverage <- as.logical(POST$bAverage)
#print(paste("bAverage: ", bAverage))

        strInputs <- as.character(POST$inputs)
        oInputsTemp <- fromJSON(strInputs)
        allInputsdata <- oInputsTemp$afInputs
        Inputscolnames <- names(allInputsdata)
        afInputsStep <- matrix(unlist(allInputsdata[Inputscolnames]), byrow=FALSE, ncol=length(Inputscolnames))

        #This is happening because fromJSON was encountering mixed data types in the JSON object and casting everything as strings
        afInputsMedia <- afInputsStep[,1]
        afInputsInvestment <- as.data.frame(as.numeric(afInputsStep[,2]))
        afInputsCPP <- as.data.frame(as.numeric(afInputsStep[,3]))
        afInputsADF <- as.data.frame(as.numeric(afInputsStep[,4]))
        afInputsIF <- as.data.frame(as.numeric(afInputsStep[,5]))
        afInputsMin <- as.data.frame(as.numeric(afInputsStep[,6]))
        afInputsMax <- as.data.frame(as.numeric(afInputsStep[,7]))
        afInputs <- as.data.frame(cbind(afInputsMedia,
                                        afInputsInvestment,
                                        afInputsCPP,
                                        afInputsADF,
                                        afInputsIF,
                                        afInputsMin,
                                        afInputsMax))

        colnames(afInputs) = Inputscolnames
#print("afInputs: ")
#print(afInputs)

        #Adjust LY GRPs for audience delivery
        afLYDelGRPs=afGRPsLY
        for (i in 1:nrow(afLYDelGRPs)){
          afLYDelGRPs[i,]=afGRPsLY[i,]*t(afInputs[,4])
        }

        afTotalGRPsCY = t(as.data.frame((afInputs[,2]/afInputs[,3]) * afInputs[,4]))
#print("afTotalGRPsCY: ")
#print(afTotalGRPsCY)

        strSeasonality <- as.character(POST$seasonality)
        oSeasonality <- fromJSON(strSeasonality)
        allSeasoanlitydata <- oSeasonality$afSeasonality
        Seasonalitycolnames <- names(allSeasoanlitydata)
        afSeasonality <- matrix(unlist(allSeasoanlitydata[Seasonalitycolnames]), byrow=FALSE, ncol=length(Seasonalitycolnames))
        colnames(afSeasonality) = Seasonalitycolnames
#print(afSeasonality)
	
#	output=MultiChannelReachAllocation(afGRPsLY, afTotalGRPsCY, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage)
        output=MultiChannelReachAllocation(afLYDelGRPs, afTotalGRPsCY, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nWeeks, nEffFreq, nRecFreq, nPeriod, bAverage)
	colnames(output) = c(colnames(afGRPsLY), "Net.Reach", "Gross.Reach")

	endtime <- Sys.time()
	timediff <- difftime(endtime, starttime, units="secs")

	cat(toJSON(list("MultiChannelReachAllocation"=as.matrix(output),
			Diagnostics=list("StartTime"=format(starttime, "%F %H:%M:%S"), 
				"EndTime"=format(endtime, "%F %H:%M:%S"), 
				"TimeDiff"=format(timediff))
		), digits = 12, .escapeEscapes = FALSE))

}

printError <- function(e){

        print(e$message)

}

tryCatch(GetMultiChannelReachAllocation(), error=printError)


