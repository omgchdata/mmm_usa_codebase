
msmp_setup <- function(obj) {
  Model_setup <- obj$setup
  obj$ModelForm <- Model_setup$Value[tolower(Model_setup$Parameter)=="modelform"]
  obj$Panel <- Model_setup$Value[tolower(Model_setup$Parameter)=="panel"]
  obj$CS <- Model_setup$Value[tolower(Model_setup$Parameter) == "cs"]
  obj$Time <- Model_setup$Value[tolower(Model_setup$Parameter)=="time"]
  obj$BeginDate <- mdy(Model_setup$Value[tolower(Model_setup$Parameter)=="begindate"])
  obj$EndDate <- mdy(Model_setup$Value[tolower(Model_setup$Parameter)=="enddate"])
  obj$SimStart <- mdy(Model_setup$Value[tolower(Model_setup$Parameter)=="simstart"])
  obj$SimEnd <- mdy(Model_setup$Value[tolower(Model_setup$Parameter)=="simend"])
  obj$mroi_step <- as.numeric(Model_setup$Value[tolower(Model_setup$Parameter) == "mroi"])
  return(obj)
}

