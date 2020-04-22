
Media <- "Blogs|Sponsored content,Online|Display,Radio|Ads,OOH|Small Size,All Social Sites|Search - SEM (Social)"

GRPs <- "0.0,0.0,0.0,0.0,0.0"

Coeffs <- "8807078.97447934,9088280.14785807,-1.0739105551891,0.5635;4926478.34728476,74147.0808798532,-0.725033733226152,0.429679886166189;1275948.69510097,208309.943077592,-0.988156807518162,0.600456861559979;1285156.46231801,3002606.55158777,-1.21963697036664,0.529500297348794;7111363.41559106,383341506790.747,-1.87521104370894,0.681789004509247" 

OverlapCoeffs <- "1.0;1.0;1.0;1.0;1.0"

Curves <- "1,0,1"

InputsInvestment <- "0.0,0.0,0.0,0.0,0.0"

InputsCPP <- "335.0032,502.5049,476.9025,341.637,157.6891"

InputsADF <- "1.0,1.0,1.0,1.0,1.0"

InputsIF <- "1.0,1.0,1.0,1.0,1.0"

InputsMin <- "0,0,0,0,0"

InputsMax <- "1000000,1000000,1000000,1000000,1000000"

NumWeeks <- "1"

bAverage <- "TRUE"

OptimizeSalesRevenue <- "TRUE"

TotalBudget <- "1000000"

bGross <- "TRUE"

bAllOrNone <- "FALSE"

Seasonality<-"1"


POST <- data.frame(Media, GRPs, Coeffs, OverlapCoeffs, Curves, InputsInvestment, InputsCPP, InputsADF, InputsIF, InputsMin, InputsMax, NumWeeks, bAverage, OptimizeSalesRevenue, TotalBudget, bGross, bAllOrNone, Seasonality)


maml.mapOutputPort("POST");