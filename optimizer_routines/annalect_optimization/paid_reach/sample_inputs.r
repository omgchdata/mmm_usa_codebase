
Media<-c(
"TV Total TV|In-Break Ads TV,
OOH Large Sizes|Large Size OOH,
Newspaper Total|National - Ads Print,
Online Display|Display Online
","TV Total TV|In-Break Ads TV,
OOH Large Sizes|Large Size OOH,
Newspaper Total|National - Ads Print,
Online Display|Display Online
")

GRPsLY<-c("
0.0000000000000000000000000000;
0.0000000000000000000000000000;
0.0000000000000000000000000000;
0.0000000000000000000000000000
","0.0000000000000000000000000000;
0.0000000000000000000000000000;
0.0000000000000000000000000000;
0.0000000000000000000000000000
")

Coeffs<-c("
0.76500,0.0711,-1.00129,0.39;
0.78023700,0.26292,-0.9,0.52966;
0.63227700,0.07707,-0.83871,0.57966;
0.655200,0.19601,-0.68385,0.5635
","
0.76500,0.0711,-1.00129,0.39;
0.78023700,0.26292,-0.9,0.52966;
0.63227700,0.07707,-0.83871,0.57966;
0.655200,0.19601,-0.68385,0.5635
")

OverlapCoeffs<-c("
1.0,1.0,1.0,1.0;
1.0,1.0,1.0,1.0;
1.0,1.0,1.0,1.0;
1.0,1.0,1.0,1.0
","
1.0,1.0,1.0,1.0;
1.0,1.0,1.0,1.0;
1.0,1.0,1.0,1.0;
1.0,1.0,1.0,1.0
")

Curves<-c("1,0,1","1,0,1")
InputsInvestment<-c("2235085.0,3188565.0,1274184.0,1270363.0","2235085.0,3188565.0,1274184.0,1270363.0")
InputsCPP<-c("8376.835,11486.14,3658.02417,1536.36267","8376.835,11486.14,3658.02417,1536.36267")
InputsADF<-c("1.0,1.0,1.0,1.0","1.0,1.0,1.0,1.0")
InputsIF<-c("1.0,1.0,1.0,1.0","1.0,1.0,1.0,1.0")
InputsMin<-c("0.0,0.0,0.0,0.0","0.0,0.0,0.0,0.0")
InputsMax<-c("7968197.0,7968197.0,7968197.0,7968197.0","7968197.0,7968197.0,7968197.0,7968197.0")
NumWeeks<-c("1","1")
bAverage<-c("TRUE","TRUE")
TotalBudget<-c("7968197","7968197")
bGross<-c("TRUE","TRUE")
bAllOrNone<-c("FALSE","FALSE")
Seasonality<-c("1.0","1.0")

POST <- data.frame(Media,GRPsLY,Coeffs,OverlapCoeffs,Curves,InputsInvestment,InputsCPP,InputsADF,InputsIF,InputsMin,InputsMax,NumWeeks,bAverage,TotalBudget,bGross,bAllOrNone,Seasonality)

# Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("POST");