
library(RJSONIO);

################################

GetMultiChannelWeeklyReach <- function() {

	starttime <- Sys.time()

        strGRPsLY <- as.character(POST$GRPsLY)
        oGRPsLY <- fromJSON(strGRPsLY)
        allGRPLYdata <- oGRPsLY$afGRPsLY
        GRPLYcolnames <- names(allGRPLYdata)
        afGRPsLY <- matrix(unlist(allGRPLYdata[GRPLYcolnames]), byrow=FALSE, ncol=length(GRPLYcolnames))
        colnames(afGRPsLY) = GRPLYcolnames
#print("afGRPsLY: ")
#print(afGRPsLY)

        strGRPsCY <- as.character(POST$GRPsCY)
        oGRPsCY <- fromJSON(strGRPsCY)
        allGRPCYdata <- oGRPsCY$afGRPsCY
        GRPCYcolnames <- names(allGRPCYdata)
        afGRPsCY <- matrix(unlist(allGRPCYdata[GRPCYcolnames]), byrow=FALSE, ncol=length(GRPCYcolnames))
        colnames(afGRPsCY) = GRPCYcolnames
#print("afGRPsCY: ")
#print(afGRPsCY)
	
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
#print(paste("Number of Weeks: ", nWeeks))

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

        strSeasonality <- as.character(POST$seasonality)
        oSeasonality <- fromJSON(strSeasonality)
        allSeasonalitydata <- oSeasonality$afSeasonality
        Seasonalitycolnames <- names(allSeasonalitydata)
        afSeasonality <- matrix(unlist(allSeasonalitydata[Seasonalitycolnames]), byrow=FALSE, ncol=length(Seasonalitycolnames))
        colnames(afSeasonality) = Seasonalitycolnames
#print(afSeasonality)

#	afTotalGRPsCY = t(as.data.frame((afInputs[,2]/afInputs[,3]) * afInputs[,4]))
##print("afTotalGRPsCY: ")
##print(afTotalGRPsCY)
     
#        afGRPsCY = data.frame(matrix(nrow=nWeeks, ncol=ncol(afTotalGRPsCY)))
#        for (i in 1:nrow(afGRPsCY)){
#          afGRPsCY[i,]=afTotalGRPsCY[]/nWeeks
#        }

#print("afGRPsCY: ")
#print(afGRPsCY)

	if (nrow(afGRPsCY) != nrow(afSeasonality))
	{
		endtime <- Sys.time()
	        timediff <- difftime(endtime, starttime, units="secs")

        	cat(toJSON(list("MultiChannelWeeklyReach"=emptyNamedList,
                	"Error"="Error! The length of the Seasonality array must match the length of the Current Year GRPs array. Please correct your data and try again.",
			Diagnostics=list("StartTime"=format(starttime, "%F %H:%M:%S"), 
				"EndTime"=format(endtime, "%F %H:%M:%S"), 
				"TimeDiff"=format(timediff))
                	), digits = 12, .escapeEscapes = FALSE))

	}
	else
	{
		outputWeekly=MultiChannelWeeklyReach(afGRPsLY, afGRPsCY, afInputs, afCoeffs, afOverlapCoeffs, afSeasonality, nEffFreq, nRecFreq, nPeriod, bAverage)
		colnames(outputWeekly) = c(colnames(afGRPsLY), "Net.Reach", "Gross.Reach")
	
		endtime <- Sys.time()
	        timediff <- difftime(endtime, starttime, units="secs")

		cat(toJSON(list("MultiChannelWeeklyReach"=as.matrix(outputWeekly),
			Diagnostics=list("StartTime"=format(starttime, "%F %H:%M:%S"), 
				"EndTime"=format(endtime, "%F %H:%M:%S"), 
				"TimeDiff"=format(timediff))
			), digits = 12, .escapeEscapes = FALSE))
	}

}

printError <- function(e){

	cat(toJSON(list("MultiChannelWeeklyReach"=emptyNamedList,
		"Error"=e$message
		), digits = 12, .escapeEscapes = FALSE))

}

tryCatch(GetMultiChannelWeeklyReach(), error=printError)

