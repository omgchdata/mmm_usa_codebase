
##########################################################################
#             Hierarchical Data Functions                                #
#                                                                        #
#   Written by Rawley Cooper  6/22/2022                                  #
#                                                                        #
#   These functions are used in the Hybrid (Agile/Bayesian) Model Code   #
#                                                                        #
#   Minor updates were made by Rawley Cooper 8/11/2022                   #
#      (Correcting for bugs that weren't caught in earlier RStudio       #
#       releases/settings) Also minor changes on 8/24/2022 for issues    #
#       with "non-panel" models                                          #
#                                                                        #
##########################################################################


Check_Hybrid_Spec_File                  <- function(obj, 
                                                    megaprint = FALSE,
                                                    print     = FALSE,
                                                    timelag   = 10) {
### This function has 4 inputs:
###    obj               --- A list with 2 elements
###                             data      = is the model input data
###                             full_spec = The spec file (including variables  
###                                         in the model and related variables
###                                         in the hierarchy)
###    megaprint = TRUE  --- prints everything in the R Log.  Just a continuous
###                             record of where the run is and what is happening
###    print     = TRUE  --- Just a limited print of where things are in the run
###                             (just enough to see what is happening)
###                             Also it activates the "timelag" timing see below.
###    timelag   = # sec --- A number in seconds.  If you set timelag = 10 then
###                             very 10 seconds or so they show you where the
###                             run is at that time
###  
### The rules and interrelationships around the Variables/Model_Spec file are very
###    complex and important to get perfect.   So this long piece of code is set
###    up to fix the Full Model Specs where possible including adding additional 
###    columns that make things easier.   Where problems can't be fixed by the 
###    machine it will point the user to the problems so they can fix it.
###
### This code will also work for regular Variables/Spec files, but likely wont
###    effect it much.

  
  if (megaprint)                           cat("Spec Check Step  1 \n")
  if (print)                               cat("Check and update the spec (variable) file \n")
  mt1                                   <- Sys.time() + timelag
  
  w                                     <- obj$full_spec
  w                                     <- filter(w,(Include == 1) | (w$Variable_Type != "Dependent")) 
  sn                                    <- nrow(w)
  x                                     <- obj$data
  
  ### Fill in missing columns to use the level structure
  if (is.null(w$Next_Level_Down)) {
    w$Next_Level_Down                   <- "End"
  }
  w[is.na(w$Next_Level_Down),
    "Next_Level_Down"]                  <- "End"
  
  if (is.null(w$Level)) {
    w$Level                             <- 1
  }
  
  if (is.null(w$PreWeight)) {
    w$PreWeight                         <- 1
  }
  
  w[is.na(w$Level),"Level"]             <- 0
  
  w[w$Variable_Type == "Dependent",
    "Next_Level_Down"]                  <- "End"
  
  
  ### Check if variables in the level structure are missing
  
  
  mlev                                  <- max(w$Level)
  

  nxt                                   <- unique(w$Next_Level_Down[w$Next_Level_Down != "End"])
  orig                                  <- unique(w$Orig_Variable)
  miss                                  <- nxt[!(nxt %in% orig)]
  nm                                    <- length(miss)
  if (nm > 0) {
    if (nm == 1) {
      cat("Error:  You need to define the",miss[1],"variable in the spec file \n")
    } else {
      cat("Error:  These variables need to be defined in the spec file: \n")
      print(miss)
    }
    stop("Program terminated due to error")
  }
  
  levtop                                <- orig[!(orig %in% nxt)]
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  2 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ln                                    <- length(levtop)
  w$check                               <- 0
  w$num                                 <- 1:sn
  
  ### Check each tree branch in the level structure for missing variables
  
  for (i in 1:ln) {
    var                                 <- levtop[i]
    var0                                <- w[w$Orig_Variable==var,"Next_Level_Down"]
    var0                                <- var0$Next_Level_Down
    chk                                 <- w[w$Orig_Variable==var,"check"]
    chk                                 <- chk$check
    temp                                <- w[w$Orig_Variable==var,"Orig_Variable"]
    temp                                <- temp$Orig_Variable
    tn                                  <- length(temp)
    num                                 <- w[w$Orig_Variable==var,"num"]
    num                                 <- num$num
    if (!(tn == 1)) {
      if (tn == 0) {
        cat("Error:  You need to define the",var,"variable in the spec file \n")
      } else {
        cat("Error:  The variable",var,"is defined mulitiple times in the spec file: \n")
      }
      stop("Program terminated due to error") 
    }
    while((var0 != "End") & (chk == 0)) {
      w[w$Orig_Variable==var,"check"]   <- 2
      var                               <- var0
      var0                              <- w[w$Orig_Variable==var,"Next_Level_Down"]
      var0                              <- var0$Next_Level_Down
      chk                               <- w[w$Orig_Variable==var,"check"]
      chk                               <- chk$check
      temp                              <- w[w$Orig_Variable==var,"Orig_Variable"]
      temp                              <- temp$Orig_Variable
      tn                                <- length(temp)
      if (!(tn == 1)) {
        if (tn == 0) {
          cat("Error:  You need to define the",var,"variable in the spec file \n")
        } else {
          cat("Error:  The variable",var,"is defined mulitiple times in the spec file: \n")
        }
        stop("Program terminated due to error") 
      }
    }
    if (chk == 2) {
      cat("Error:  Variable",var0,"references itself \n")
      stop("Program terminated due to error")
    }
    if (chk == 0) {
      w[w$Orig_Variable==var,"check"]   <- 2
    }
    w[w$Orig_Variable==var,"num"]       <- num
    w[w$check == 2,"check"]             <- 1
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  3 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Trance the Next_Level_Down variable through each branch to see how many levels we need
  ### Put the new levels in a intermediary variable Level0 for now.
  
  w$Level0                              <- 0
  levx                                  <- levtop
  lxn                                   <- ln
  cn                                    <- mlev
  
  while(lxn > 0) {
    w[w$Orig_Variable %in% levx,
      "Level0"]                         <- cn
    cn                                  <- cn - 1
    levx                                <- w[(w$Orig_Variable %in% levx) & (w$Next_Level_Down != "End"),"Next_Level_Down"]
    levx                                <- unique(levx$Next_Level_Down)
    lxn                                 <- length(levx)
  }
  
  w$Level0                              <- w$Level0 - cn
  w[w$Level < w$Level0,"Level0"]        <- w[w$Level < w$Level0,"Level"]
  w$Level                               <- w$Level0
  w$Level0                              <- NULL
  elev                                  <- min(w[w$Include==1,"Level"])
  w[w$Include==1,"Level"]               <- elev
  mlev                                  <- max(w$Level)
  tlev                                  <- mlev * 2
  
  for (i in 1:tlev) {
    i0                                  <- mlev - i + 1
    i1                                  <- i0 - 1
    lxn                                 <- nrow(w[w$Level==i1,])
    if (lxn > 0) {
      levx                              <- w[(w$Level == i0) & (w$Next_Level_Down != "End"),"Next_Level_Down"]
      levx                              <- unique(levx$Next_Level_Down)
      lxn                               <- length(levx)
      if (lxn > 0) {
        w[(w$Level > i1) & (w$Orig_Variable %in% levx),"Level"] <- i1
      }
    }
  }
  
  cn                                    <- min(w$Level) - 1
  w$Level                               <- w$Level - cn
  elev                                  <- max(w[w$Include==1,"Level"])
  mlev                                  <- max(w$Level)
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  4 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Check that we are only modeling on a single level (that no modeling variable has a level above the others)
  
  levx                                  <- w[(w$Level   != elev) & 
                                             (w$Include == 1)    ,"Orig_Variable"]
  levx                                  <- unique(levx$Orig_Variable)
  lxn                                   <- length(levx)
  if (lxn > 0) {
    cat("Error: You can only model on one level of variables. \n")
    if (tn == 1) {
      cat("Error:  Variable",levx[1],"violates that rule. \n")
    } else {
      cat("Error:  The below variables violate that rule: \n")
      print(levx)
    }
    stop("Program terminated due to error") 
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  5 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Check that every branch of the tree covers every level between the 1 and the highest level
  ### do this by creating extra versions of some variables but on different levels
  ###    So if a level 1 variable is followed by a level 3 variable,
  ###    have the level 1 variable point to a duplicate of itself on level 2 and have
  ###    the level 2 version of the variable be followed by the level 3 variable
  
  levx                                  <- w[,c("Orig_Variable","Level")]
  names(levx)                           <- c("Next_Level_Down","NLevel")
  w                                     <- left_join(w,levx,by="Next_Level_Down")
  w[is.na(w$NLevel),"NLevel"]           <- 0
  
  if (mlev > 1) {
    w0                                  <- w[(w$Level < mlev) & (w$Orig_Variable %in% levtop),]
    w0n                                 <- nrow(w0)
    if (w0n > 0) {
      w0$NLevel                         <- w0$Level
      w0$Level                          <- mlev
      w0$Next_Level_Down                <- w0$Orig_Variable
      w0[w0$Transform=="Y",
         "TransformType"]               <- "Blank"
      w                                 <- rbind(w,w0)
      sn                                <- nrow(w)
    }
    w0                                  <- w[(w$Level > 1) & (w$NLevel == 0),]
    w0n                                 <- nrow(w0)
    if (w0n > 0) {
      w[(w$Level > 1) & 
        (w$NLevel == 0),
        "Next_Level_Down"]              <- w[(w$Level > 1) & (w$NLevel == 0),"Orig_Variable"]
      w[(w$Level > 1) & 
        (w$NLevel == 0),
        "NLevel"]                       <- 1
      w0$Level                          <- 1
      w0[w0$Transform=="Y",
         "TransformType"]               <- "Blank"
      w                                 <- rbind(w,w0)
      sn                                <- nrow(w)
    }
    for (i in 1:mlev) {
      i0                                <- mlev - i + 1
      i1                                <- i0 - 1
      levx                              <- w[(w$Level == i0) & (w$NLevel < i1),"Orig_Variable"]
      levx                              <- unique(levx$Orig_Variable)
      w0                                <- w[(w$Orig_Variable %in% levx) & (w$Level == i0) & (w$NLevel < i1),]
      w0n                               <- nrow(w0)
      if (w0n > 0) {
        w[(w$Orig_Variable %in% levx) & 
          (w$Level == i0)             & 
          (w$NLevel < i1)             ,
          "Level"]                      <- i1
        w0$NLevel                       <- i1
        w0$Next_Level_Down              <- w0$Orig_Variable
        w0[w0$Transform=="Y",
           "TransformType"]             <- "Blank"
        w                               <- rbind(w,w0)
        sn                              <- nrow(w)
      }
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  6 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Divide all transformations into "adstock" transformation and "Diminishing Returns" transformations
  ###    We are defining "adstock" as any transformation that effects the timing of the effect but not
  ###       the total volume of the effect
  ###    Diminishing returns (which includes threshold levels) which is transforms that effect the volume
  ###       of effect (diminishing them at extreme high or low levels -- the standard S curve)
  ###       but diminishing returns does not effect the timing of the effect
  ###    Some transforms actually are both adstock and dim. returns. In those cases you need to break the 
  ###       transformation into its two parts (Not in the code yet)
  ###    "Adstock" is linear, and so does not change as you move up and down levels
  ###    "Dim. Returns" is non-linear, so the code needs to stabilize the values to allow the optimization
  ###       to work without changing the Bayesian priors.
  
  aslist                                <- c("ADSTOCK","ADSTOCKV2","ADSTOCKV3","ADSTOCKG","LAG","MA","MC","STEIN","CPT","ASDOWN","ASUP")
  drlist                                <- c("ADR","ABC","ATAN","LOG","POLY","POWER","DRDOWN","DRUP")
  asnlist                               <- data.frame(as  =c("ADSTOCK"  ,"ADSTOCK"  ,"ADSTOCKV2",
                                                             "ADSTOCKV3","ADSTOCKV3","ADSTOCKV3",
                                                             "ADSTOCKG" ,"ADSTOCKG" ,"ADSTOCKG" ,
                                                             "ADSTOCKG" ,"LAG"      ,"MA"       ,
                                                             "STEIN"    ,"STEIN"                ),
                                                      need=c("Decay"    ,"Period"   ,"Decay"    ,
                                                             "Decay"    ,"Peak"     ,"Length"   ,
                                                             "Decay"    ,"Peak"     ,"Spred"    ,
                                                             "Period"   ,"Lag"      ,"Window"   ,
                                                             "Window"   ,"Trim"                 ))
  drnlist                               <- data.frame(dr  =c("ADR"      ,"ADR"      ,"ADR"      ,
                                                             "ADR"      ,"ABC"      ,"ABC"      , 
                                                             "ATAN"     ,"LOG"      ,"POLY"     ,
                                                             "POWER"                            ),
                                                      need=c("Effective","Recency"  ,"Period"   ,
                                                             "Decay"    ,"B"        ,"C"        ,
                                                             "Scale"    ,"Scale"    ,"Alpha"    ,
                                                             "Power"                            )) 
  
  w$Dim_Returns                         <- "Blank"
  w$Adstock                             <- "Blank"
  ord                                   <- c(elev)
  for (i in 1:mlev) {
    nxt                                 <- elev - i
    if ((nxt <= mlev) & (nxt >= 1)) ord <- c(ord,nxt)
    nxt                                 <- elev + i
    if ((nxt <= mlev) & (nxt >= 1)) ord <- c(ord,nxt)
  }
  w[w$Transform=="N","TransformType"]   <- "None"
  for (i in 1:sn) {
    tran                                <- w$TransformType[i]
    if (is.na(tran)) tran               <- "Blank"
    if (toupper(tran) != "BLANK") {
      type                              <- unlist(strsplit(tran, "_"))
      utype                             <- toupper(type)
      tn                                <- length(type)
      for (j in 1:tn) {
        trn                             <- type[j]
        utrn                            <- utype[j]           
        if (utrn %in% aslist) {
          trn0                          <- w$Adstock[i]
          utrn0                         <- toupper(trn0)
          if ((utrn0 == "NONE")   | 
              (utrn0 == "BLANK")  | 
              (utrn  == "ASDOWN") |
              (utrn  == "ASUP")   |
              (utrn  == "WEIGHT"))                     {
            trn0                        <- trn
          } else {
            if ((utrn0 != "ASDOWN") & 
                (utrn0 != "ASUP"))  {
              trn0                      <- paste(trn0,"_",trn,sep="")
            }
          }
          w$Adstock[i]                  <- trn0
        }
        if (utrn %in% drlist) {
          trn0                          <- w$Dim_Returns[i]
          utrn0                         <- toupper(trn0)
          if ((utrn0 == "NONE")   | 
              (utrn0 == "BLANK")  | 
              (utrn  == "DRDOWN") |
              (utrn  == "DRUP")   |
              (utrn  == "WEIGHT"))                     {
            trn0                        <- trn
          } else {
            if ((utrn0 != "DRDOWN")  & 
                (utrn0 != "DRUP"  )) {
              trn0                      <- paste(trn0,"_",trn,sep="")
            }
          }
          w$Dim_Returns[i]              <- trn0
        }      
      }
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  7 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Find what parameters in the spec/variables files are needed for the transforms
  ###   chosen, so we can be sure they are filled in and so we can reproduce them
  ###   at all levels above where they are used.
  
  err                                   <- 0
  temp                                  <- w$Adstock
  temp                                  <- toupper(temp)
  temp                                  <- unique(temp)
  
  tn                                    <- length(temp)
  nms                                   <- names(w)
  asnlist$Status                        <- 0
  asn                                   <- nrow(asnlist)
  for (i in 1:tn) {
    as                                  <- temp[i]
    asnlist[asnlist$as==as,"Status"]    <- 1
  }
  for (i in 1:asn) {
    stt                                 <- asnlist$Status[i]
    if (stt == 1) {
      parm                              <- asnlist$need[i]
      if (!(parm %in% nms)) {
        asnlist$Status                  <- 2
        err                             <- 1
      }
    }
  }
  temp                                  <- w$Dim_Returns
  temp                                  <- toupper(temp)
  temp                                  <- unique(temp)
  
  tn                                    <- length(temp)
  drnlist$Status                        <- 0
  drn                                   <- nrow(drnlist)
  for (i in 1:tn) {
    dr                                  <- temp[i]
    drnlist[drnlist$dr==dr,"Status"]    <- 1
  }
  for (i in 1:drn) {
    stt                                 <- drnlist$Status[i]
    if (stt == 1) {
      parm                              <- drnlist$need[i]
      if (!(parm %in% nms)) {
        drnlist$Status                  <- 2
        err                             <- 1
      }
    }
  }
  
  if (err == 1) {
    cat("Error: Some of the Parameters needed for your transformations are not in your file. \n")
    temp                                <- asnlist[asnlist$Status == 2,]
    tn                                  <- nrow(temp)
    if (tn > 0) {
      for (i in 1:tn) {
        as                              <- temp$as[i]
        parm                            <- temp$need[i]
        cat("    The adstock transform",as,"requires the",parm,"parameter in the Variables file. \n")
      }
    }
    temp                                <- drnlist[asnlist$Status == 2,]
    tn                                  <- nrow(temp)
    if (tn > 0) {
      for (i in 1:tn) {
        dr                              <- temp$dr[i]
        parm                            <- temp$need[i]
        cat("    The diminishing returns transform",dr,"requires the",parm,"parameter in the Variables file. \n")
      }
    }  
    stop("Program terminated due to error") 
  }
  
  asneed                                <- asnlist[asnlist$Status==1,"need"]
  asneed                                <- unique(asneed)
  asnn                                  <- length(asneed)
  if (asnn > 0) {
    asneed                              <- data.frame(need=asneed)
    asneed$parms                        <- paste("asp",1:asnn,sep="")
  }
  
  drneed                                <- drnlist[drnlist$Status==1,"need"]
  drneed                                <- unique(drneed)
  drnn                                  <- length(drneed)
  if (drnn > 0) {
    drneed                              <- data.frame(need=drneed)
    drneed$parms                        <- paste("drp",1:drnn,sep="")
  }
  
  cols                                  <- c("asz","drz")
  
  if (asnn > 0) {
    cols                                <- c(cols,asneed$parms)
  }
  if (drnn > 0) {
    cols                                <- c(cols,drneed$parms)
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  8 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  
  ### We now create a "Branch" file with a line for each branch
  ###    A branch is the chain of variables from the top to the bottom
  ###    every variable at the top has a branch but the variable at the 
  ###    bottom (the level 1 variables) are each in many branches.
  ###    THis file is needed to check that all branch rules are being
  ###    followed.
  
  vars                                  <- paste("var",  1:mlev,sep="")
  ass                                   <- paste("as",   1:mlev,sep="")
  drs                                   <- paste("dr",   1:mlev,sep="")
  branch                                <- data.frame(top=(c(levtop)))
  branch$inc                            <- 0
  nxt                                   <- levtop
  for (i in 1:mlev) {
    i0                                  <- mlev + 1 - i
    var                                 <- vars[i]
    as                                  <- ass[i]
    dr                                  <- drs[i]
    branch[,var]                        <- nxt
    w0                                  <- w[(w$Orig_Variable %in% nxt) & (w$Level == i0),
                                             c("Orig_Variable","Next_Level_Down","Adstock","Dim_Returns","Include")]
    names(w0)                           <-   c(var,            "Next_Level_Down","Adstock","Dim_Returns","Include")
    branch                              <- left_join(branch,w0,by=var)
    branch[,as]                         <- branch$Adstock
    branch[,dr]                         <- branch$Dim_Returns
    branch[branch$Include == 1,"inc"]   <- 1
    nxt                                 <- branch$Next_Level_Down
    branch$Adstock                      <- NULL
    branch$Dim_Returns                  <- NULL
    branch$Next_Level_Down              <- NULL
    branch$Include                      <- NULL
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step  9 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Continue to create the branch file, since the rules are there must be only one
  ###    adstock and only one dim. returns formula in each branch, check if this is
  ###    true (and fix it if it isn't true)
  
  branch$as0                            <- "Blank"
  branch$dr0                            <- "Blank"
  branch$asz                            <- 0
  branch$drz                            <- 0
  
  for (i in 1:mlev) {
    i0                                  <- ord[i]
    as                                  <- ass[i0]
    dr                                  <- drs[i0]
    branch[toupper(branch[,as]) == "ASDOWN",
           as]                          <- "Blank"
    branch[toupper(branch[,as]) == "ASUP",
           as]                          <- "Blank"
    branch[toupper(branch[,as]) == "NONE",
           as]                          <- "Blank"
    branch[toupper(branch[,as]) == "WEIGHT",
           as]                          <- "Blank"
    branch[toupper(branch[,as]) != "BLANK" & 
           toupper(branch$as0)  == "BLANK" ,
           "asz"]                       <- i0
    branch[toupper(branch[,as]) != "BLANK" & 
           toupper(branch$as0)  == "BLANK" ,
           "as0"]                       <- branch[toupper(branch[,as]) != "BLANK" &
                                                  toupper(branch$as0)  == "BLANK", as]
    branch[toupper(branch[,dr]) == "DRDOWN",
           dr]                          <- "Blank"
    branch[toupper(branch[,dr]) == "DRUP",
           dr]                          <- "Blank"
    branch[toupper(branch[,dr]) == "NONE",
           dr]                          <- "Blank"
    branch[toupper(branch[,dr]) == "WEIGHT",
           dr]                          <- "Blank"
    branch[toupper(branch[,dr]) != "BLANK" & 
           toupper(branch$dr0)  == "BLANK" ,
           "drz"]                       <- i0
    branch[toupper(branch[,dr]) != "BLANK" & 
             toupper(branch$dr0)  == "BLANK" ,
           "dr0"]                       <- branch[toupper(branch[,dr]) != "BLANK" &
                                                  toupper(branch$dr0)  == "BLANK", dr]
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 10 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Fill in the adstock up and adstock down transforms from the single chosen transform per branch
  
  for (i in 1:mlev) {
    as                                  <- ass[i]
    branch[(branch$asz  >  0) & 
           (branch$asz  >  i) &
           (branch[,as] != branch$as0),
           as]                          <- "asdown"   
    branch[(branch$asz  >  0) &
           (branch$asz  <  i) &
           (branch[,as] != branch$as0),
           as]                          <- "asup" 
    dr                                  <- drs[i]
    branch[(branch$drz>0) & 
             (branch$drz>i),dr]         <- "drdown"   
    branch[(branch$drz>0) & 
             (branch$drz<i),dr]         <- "drup"   
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 11 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  if ("Com_New_Off_Sales_PC_t" %in% w$Trans_Variable) cat("Error Here \n")
  
  ### Find where there are conflicting adstock directions for the same variable
  ###    (in other words, a variable is asdown and asup at the same time)
  
  temp0                                 <- branch[,c("var1","as1")]
  temp0$lev                             <- 1
  names(temp0)                          <- c("var","as","lev")
  if (mlev > 1) {
    for (i in 2:mlev) {
      var                               <- vars[i]
      as                                <- ass[i]
      temp1                             <- branch[,c(var,as)]
      temp1$lev                         <- i
      names(temp1)                      <- c("var","as","lev")
      temp0                             <- rbind(temp0,temp1)
    }
  }
  
  temp0                                 <- unique(temp0)
  temp0$levvar                          <- paste(temp0$lev,"@",temp0$var,sep="")
  temp0$ones                            <- 1
  temp1                                 <- data.frame(aggregate(temp0$ones,by=list(temp0$levvar),FUN=sum))
  names(temp1)                          <- c("levvar","count")
  mcn                                   <- max(temp1$count)
  if (mcn > 1) {
    temp0                               <- left_join(temp0,temp1,by="levvar")
    temp0                               <- temp0[temp0$count       >  1,]
    temp0                               <- temp0[toupper(temp0$as) != "BLANK",]
    temp1                               <- data.frame(aggregate(temp0$ones,by=list(temp0$levvar),FUN=sum))
    names(temp1)                        <- c("levvar","err")
    mcn                                 <- max(temp1$err)
    if (mcn > 1) {
      temp0                             <- left_join(temp0,temp1,by="levvar")
      temp0                             <- temp0[temp0$err         >  1,]
      temp0                             <- unique(temp0$var)
      tn                                <- length(temp0)
      if (tn == 1) {
        cat("Error:  The adstocks for the",temp0[1],"variable seems to be conflicting  \n")
      } else {
        cat("Error:  The adstocks for the below variables seems to be conflicting  \n")
        print(temp0)
      }
      stop("Program terminated due to error")
    } else {
      tn                                <- nrow(temp0)
      for (i in 1:tn) {
        as                              <- temp0$as[i]
        lev                             <- temp0$lev[i]
        val                             <- temp0$var[i]
        var                             <- vars[lev]
        as0                             <- ass[lev]
        branch[(branch[,var]==val) & 
               (toupper(branch[,as0])=="BLANK")
               ,as0]                    <- as
      }
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 12 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Find where there are conflicting dim. returns directions for the same variable
  ###    (in other words, a variable is drdown and drup at the same time)
  
  temp0                                 <- branch[,c("var1","dr1")]
  temp0$lev                             <- 1
  names(temp0)                          <- c("var","dr","lev")
  if (mlev > 1) {
    for (i in 2:mlev) {
      var                               <- vars[i]
      dr                                <- drs[i]
      temp1                             <- branch[,c(var,dr)]
      temp1$lev                         <- i
      names(temp1)                      <- c("var","dr","lev")
      temp0                             <- rbind(temp0,temp1)
    }
  }
  temp0                                 <- unique(temp0)
  temp0$levvar                          <- paste(temp0$lev,"@",temp0$var,sep="")
  temp0$ones                            <- 1
  temp1                                 <- data.frame(aggregate(temp0$ones,by=list(temp0$levvar),FUN=sum))
  names(temp1)                          <- c("levvar","count")
  mcn                                   <- max(temp1$count)
  
  temp0                                 <- left_join(temp0,temp1,by="levvar")
  temp0                                 <- temp0[temp0$count       >  1,]
  temp0                                 <- temp0[toupper(temp0$as) != "BLANK",]
  
  mcn                                   <- max(temp1$err)
  
  if (mcn > 1) {
    temp0                               <- left_join(temp0,temp1,by="levvar")
    temp0                               <- temp0[temp0$count       >  1,]
    temp0                               <- temp0[toupper(temp0$dr) != "BLANK",]
    temp1                               <- data.frame(aggregate(temp0$ones,by=list(temp0$levvar),FUN=sum))
    names(temp1)                        <- c("levvar","err")
    mcn                                 <- max(temp1$err)
    if (mcn > 1) {
      temp0                             <- left_join(temp0,temp1,by="levvar")
      temp0                             <- temp0[temp0$err         >  1,]
      temp0                             <- unique(temp0$var)
      tn                                <- length(temp0)
      if (tn == 1) {
        cat("Error:  The diminishing returns for the",temp0[1],"variable seems to be conflicting  \n")
      } else {
        cat("Error:  The diminishing returns for the below variables seems to be conflicting  \n")
        print(temp0)
      }
      stop("Program terminated due to error")
    } else {
      tn                                <- nrow(temp0)
      for (i in 1:tn) {
        dr                              <- temp0$dr[i]
        lev                             <- temp0$lev[i]
        val                             <- temp0$var[i]
        var                             <- vars[lev]
        dr0                             <- drs[lev]
        branch[(branch[,var]==val) & 
               (toupper(branch[,dr0])=="BLANK"),
               dr0]                     <- dr
      }
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 13 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Finish this section by changing "blanks" with "None" transforms
  ###    Also change all "asdown" by the actual adstock formula, since for adstock
  ###    formulas using the same adstock is the same as adstock down and is
  ###    easier to code.  This is not true for adstock up or dim return up or down.
  
  for (i in 1:mlev) {
    as                                  <- ass[i]
    dr                                  <- drs[i]
    branch[toupper(branch[,as]) == "BLANK",
           as]                          <- "None"
    branch[toupper(branch[,as]) == "ASDOWN",
           as]                          <- branch[toupper(branch[,as]) == "ASDOWN","as0"]
    branch[toupper(branch[,dr]) == "BLANK",
           dr]                          <- "None"
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 14 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Mark every branch were modeling is being done and mark it 
  ###    For example if the level 1 of a branch is "category x" then any
  ###    branch that starts with "category x" and has a model variable in it
  ###    is marked with "inc=1" but any branch that starts with "category x"
  ###    but doesn't have a variable in it is marked with "inc=2".  These 
  ###    inc=2 branches don't effect the model directly, but do effect the
  ###    weighting and so the optimization, and so indirectly effect the 
  ###    model.
  
  temp0                                 <- branch[branch$inc==1,"var1"]
  if (mlev > 1) {
    for (i in 2:mlev) {
      var                               <- vars[i]
      temp1                             <- branch[branch$inc==1,var]
      temp0                             <- c(temp0,temp1)
    }
  }
  temp0                                 <- unique(temp0)
  for (i in 1:mlev) {
    var                                 <- vars[i]
    branch[(branch[,var] %in% temp0) & 
           (branch$inc == 0),"inc"]     <- 2
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 15 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Find the parameters of the adstocks and dim returns for each branch
  ###    and add them to the branch file. 
  
  if (asnn > 0) {
    branch$Orig_Variable                <- "None"
    branch$Level                        <- mlev + 1 - branch$asz
    for (i in 1:mlev) {
      var                               <- vars[i]
      branch[branch$asz == i,
             "Orig_Variable"]           <- branch[branch$asz == i,var]
    }
    temp0                               <- w[,c("Orig_Variable","Level",asneed$need)]
    branch                              <- left_join(branch,temp0,by=c("Orig_Variable","Level"))
    for (i in 1:asnn) {
      need                              <- asneed$need[i]
      parm                              <- asneed$parms[i]
      branch[,parm]                     <- 0
      branch[branch$Level > 0,parm]     <- branch[branch$Level > 0,need]
      branch[,need]                     <- NULL
    }
    branch[,"Orig_Variable"]            <- NULL
    branch[,"Level"]                    <- NULL
  }
  
  if (drnn > 0) {
    branch$Orig_Variable                <- "None"
    branch$Level                        <- mlev + 1 - branch$drz
    for (i in 1:mlev) {
      var                               <- vars[i]
      branch[branch$drz == i,
             "Orig_Variable"]           <- branch[branch$drz == i,var]
    }
    temp0                               <- w[,c("Orig_Variable","Level",drneed$need)]
    branch                              <- left_join(branch,temp0,by=c("Orig_Variable","Level"))
    for (i in 1:drnn) {
      need                              <- drneed$need[i]
      parm                              <- drneed$parms[i]
      branch[,parm]                     <- 0
      branch[branch$Level > 0,parm]     <- branch[branch$Level > 0,need]
      branch[,need]                     <- NULL
    }
    branch[,"Orig_Variable"]            <- NULL
    branch[,"Level"]                    <- NULL
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 16 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Finally, transfer all the data from the branch file back to the Full_Spec file
  
  w$levvar                              <- paste(w$Level,"@",w$Orig_Variable,sep="")
  cn                                    <- length(cols)
  cn0                                   <- cn - 2
  if (cn0 > 0)   cols0                  <- paste("Col",1:cn0,sep="")
  
  for (i in 1:mlev) {
    i0                                  <- mlev + 1 - i
    var                                 <- vars[i]
    as                                  <- ass[i]
    dr                                  <- drs[i]
    temp0                               <- branch[,c(var,as,dr,"inc",cols)]
    temp0[,var]                         <- paste(i0,"@",temp0[,var],sep="")
    names(temp0)                        <- c("levvar","as","dr","inc",cols)
    temp1                               <- data.frame(aggregate(temp0[,c("inc",cols)],by=list(temp0$levvar),FUN=mean))
    names(temp1)[1:4]                   <- c("levvar","inc","asz","drz")
    if (cn0 > 0)         names(temp1)   <- c("levvar","inc","asz","drz",cols0)
    temp0$inc                           <- NULL
    temp0$asz                           <- NULL
    temp0$drz                           <- NULL
    temp0                               <- left_join(temp0,temp1,by="levvar")
    if (cn0 > 0) {
      for (j in 1:cn0) {
        col0                            <- cols0[j]
        col                             <- cols[j + 2]
        temp0[,col]                     <- temp0[,col0]
        temp0[,col0]                    <- NULL
      }
    }
    temp0                               <- unique(temp0)
    w                                   <- left_join(w,temp0,by="levvar")
    w[(w$Level == i0),  "Adstock"]      <- w[(w$Level == i0),"as"]
    w[(w$Level == i0),  "Dim_Returns"]  <- w[(w$Level == i0),"dr"]
    w[(w$Level == i0)   & 
        (w$Include == 0)  & 
        (w$inc > 0),      "Include"]    <- 2
    if (asnn > 0) {
      for (j in 1:asnn) {
        need                            <- asneed$need[j]
        parm                            <- asneed$parms[j]
        w[(w$Level == i0) &
            (w$asz > 0),need]           <- w[(w$Level == i0) & (w$asz > 0),parm]
        w[,parm]                        <- NULL
      }
    }
    if (drnn > 0) {
      for (j in 1:drnn) {
        need                            <- drneed$need[j]
        parm                            <- drneed$parms[j]
        w[(w$Level == i0) &
            (w$drz > 0),need]           <- w[(w$Level == i0) & (w$drz > 0),parm]
        w[,parm]                        <- NULL
      }
    }  
    w$as                                <- NULL
    w$dr                                <- NULL
    w$inc                               <- NULL
    w$asz                               <- NULL
    w$drz                               <- NULL
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 17 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Do the final clean-up to the Full_Spec file
  
  w$TransformType                       <- paste(w$Adstock,"_",w$Dim_Returns,sep="")
  w[w$TransformType == "None_None",
    "TransformType"]                    <- "None"
  w$levvar                              <- NULL
  w$sort                                <- 1
  w$sort[w$AggregateVariable == "KPI"]  <- 0
  w                                     <- w[order(w$sort,w$Level,w$num),]
  w$num                                 <- NULL
  w$sort                                <- NULL
  w[(w$Level != elev) & 
      (w$Include == 1),
    "Include"]                          <- 2
  
  if (mlev > 1) {
    w$temp                              <- paste(w$Orig_Variable,"_t",sep="")
    w[(w$Include > 0)                        &  
      (w$Transform == "N")                   &
      (w$Variable_Type != "Dependent")       &
      (w$Orig_Variable != "Intercept")       ,
      "Trans_Variable"]                 <- w[(w$Include > 0)                       &  
                                             (w$Variable_Type != "Dependent")      &
                                             (w$Orig_Variable != "Intercept")      &
                                             (w$Transform == "N"),"temp"]
    w[(w$Include > 0)                        & 
        (w$Transform == "N")                   &
        (w$Orig_Variable != "Intercept")       &
        (w$Variable_Type != "Dependent")       ,
      "Transform"]                      <- "Y"
    w[(w$Include > 0)                        & 
        (w$Trans_Variable == w$Orig_Variable)  &
        (w$Variable_Type != "Dependent")       &
        (w$Orig_Variable != "Intercept")       ,
      "Trans_Variable"]                 <- w[(w$Include > 0)                       & 
                                             (w$Trans_Variable == w$Orig_Variable) &
                                             (w$Variable_Type != "Dependent")      &
                                             (w$Orig_Variable != "Intercept")      ,
                                             "temp"]
    w[(w$Include > 0)                        & 
        (toupper(w$TransformType) == "NONE")   ,
      "TransformType"]                  <- "Weight"
    w[(w$Include > 0)                        & 
        (toupper(w$Dim_Returns)   == "NONE")   ,
      "Dim_Returns"]                    <- "Weight"
    w[(w$Include > 0)                        & 
        (toupper(w$Adstock)       == "NONE")   ,
      "Adstock"]                        <- "Weight"
    w$temp                              <- NULL
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 18 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Now check if that all the variable in the top level of the full_Spec/Variable
  ###    file that are needed for the model and the weighting are in the data file.
  ###    Variables below the top level can be created but the top level variables
  ###    can't be.
  
  defined                               <- names(x)
  defined                               <- c(defined,"Intercept")
  temp                                  <- w[(w$Orig_Variable %in% levtop) & 
                                             (w$Include > 0)               ,"Orig_Variable"]
  temp                                  <- unique(temp$Orig_Variable)
  notdef                                <- temp[!(temp %in% defined)]
  dn                                    <- length(notdef)
  
  if (dn > 0) {
    if (dn == 1) {
      cat("Error:  You need to define the",notdef[1],"variable in the modeling data file \n")
    } else {
      cat("Error:  These variables need to be defined in the modeling data file: \n")
      print(notdef)
    }
    stop("Program terminated due to error")
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 19 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### For variables below the top level that are needed in the weighting or model
  ###    either create them, or check if the sum up to the values they should.
  
  xn                                    <- nrow(x)
  
  if (mlev > 1) {
    for (i in 2:mlev) {
      i0                                <- mlev + 1 - i
      i1                                <- i0 + 1
      temp                              <- w[(w$Orig_Variable != w$Next_Level_Down) &
                                             (w$Include > 0)                        &
                                             (w$Level == i0)                        &
                                             (!(w$Orig_Variable %in% levtop))       ,
                                             c("Orig_Variable","PreWeight")]
      tn                                <- nrow(temp)
      if (tn > 0) {
        un                              <- 0
        en                              <- 0
        qn                              <- 0
        nn                              <- 0
        for (j in 1:tn) {
          var                           <- temp$Orig_Variable[j]
          prewgt                        <- temp$PreWeight[j]
          x$temp                        <- 0
          temp0                         <- w[(w$Next_Level_Down == var) &
                                             (w$Orig_Variable   != var) &
                                             (w$Level           == i1),
                                             c("Orig_Variable","Level","PreWeight")]
          tnn                           <- nrow(temp0)
          if (tnn > 0) {
            for (k in 1:tnn) {
              var0                      <- temp0$Orig_Variable[k]
              prewgt0                   <- temp0$PreWeight[k]
              if (var0 %in% defined) {
                x$temp                  <- x$temp + (x[,var0] * prewgt0)
              } else {
                if (qn == 0) {
                  missing               <- c(var0)
                } else {
                  missing               <- c(missing,var0)
                }
                qn                      <- qn + 1
              }
            }
          } 
          if (var %in% defined) {
            dff                         <- sum(abs((x[,var] * prewgt) - x$temp)) / xn
            if (dff > 0.001) {
              if (all((x[,var] * wgt) > (x$temp - 0.001))) {
                if (un == 0) {
                  under                 <- c(var)
                } else {
                  under                 <- c(under,var)
                }
                un                      <- un + 1
              } else {
                if (en == 0) {
                  error                 <- c(var)
                } else {
                  error                 <- c(error,var)
                }
                en                      <- en + 1
              }
            }
          } else {
            x[,var]                     <- x$temp / prewgt
            defined                     <- c(defined,var)
            nn                          <- nn + 1
          }
        }
      }
    }
  }
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 20 \n")
  if (nextprint)             mt1        <- mt2 + timelag

  if (mlev > 1) {
    if (nn > 0) {
      cat(nn,"new variables were added to the model dataset. \n")
    }
    
    if ((en > 0) | (un > 0) | (qn > 0)) {
      if (qn > 0) {
        if (qn == 1) {
          cat("Error:  The variable",missing[1],"is not able to be created in the modeling data file \n")
        } else {
          cat("Error:  These variables need to be able to be created in the modeling data file: \n")
          print(missing)
        }
      }
      if (un > 0) {
        if (un == 1) {
          cat("Error:  The variable",under[1],"is missing at least one of its sub-variables in the heirarchy \n")
        } else {
          cat("Error:  These variables are missing at least one of their sub-variables in te heirarchy: \n")
          print(under)
        }
      }
      if (en > 0) {
        if (en == 1) {
          cat("Error:  THe variable",error[1],"does not match the sum of its sub-variables \n")
        } else {
          cat("Error:  These variables do not match the sum or their sub-variables: \n")
          print(error)
        }
      }
      stop("Program terminated due to error")
    }
  }
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Spec Check Step 21 \n")
  
  w$check                               <- NULL
  w$NLevel                              <- NULL
  obj$full_spec                         <- w
  obj$data                              <- x
  return(obj)
}


Check_Hybrid_Control_Files                <- function(obj) {

### This function has 1 input:
###    obj               --- A list with 5 elements
##                              data      = is the model input data
###                             meta      = the meta file (including elements
###                                         to be weighted, a composate of the
###                                         weights, and the highest level of
###                                         variables related to the model)
###                             wgts      = The weight control file (including
###                                         the groups and elements to be
###                                         weighted, and the max, min, and
###                                         current weights)
###                             Panel     = Either "Y" or "N" to indicate if
###                                         the model has panel subdivisions
###                             CS        = is the variable that drives the 
###                                         Panel subdivisions.  If there is
###                                         no Panel subdivison, this should be
###                                         black (= "").
###  
### This code checks the Meta and Weights file for errors, fixing where it can
###    It also creates an "initial raw weight" value in the Meta file
  
  x                                     <- obj$data
  w                                     <- obj$full_spec
  meta                                  <- obj$meta
  wgts                                  <- obj$weights
  panel                                 <- obj$Panel
  cs                                    <- obj$CS
  if (is.null(panel))           panel   <- "N"
  if (toupper(panel) == "Y") {
    panel                               <- "Y"
    cflag                               <- nchar(cs)
    if (cflag > 0) {
      nmx                               <- names(x)
      if (!(cs %in% nmx)) {
         cat("Error:  The panel control variable",cs,"is not in the data \n")
         stop("Program terminated due to error")
      }
    } else {
      panel                             <- "N"
      cs                                <- NULL
    }
  } else {
    if (toupper(panel) == "N") {
      panel                            <- "N"
      cs                               <- NULL
    } else {
      cflag                            <- nchar(cs)
      if (cflag > 0) {
        nmx                            <- names(x)
        if (cs %in% nmx) {
          panel                        <- "Y"
        } else {
          panel                        <- "N"
          cs                           <- NULL
        }
      } else {
        panel                          <- "N"
        cs                             <- NULL
      }
    }
  }
  
  grps                                  <- names(meta)
  gn                                    <- length(grps)
  gn0                                   <- match("Orig_Variable",grps)
  gn1                                   <- match("Weight",grps)
  if (is.na(gn0)) {
    cat("Error:  The Meta file must start with the variable name. \n")
    stop("Program terminated due to error")
  } 
  gn0                                   <- gn0 + 1
  if (is.na(gn1)) gn1                   <- gn  + 1
  gn1                                   <- gn1 - 1
  if (gn1 < gn0) {
    cat("Error:  The Meta file must start with a variable name, then have several groups of categories to weight. \n")
    cat("        And then have a Weight column that will be filled out by the code from the Weight file \n")
    stop("Program terminated due to error")
  } 
  grps                                  <- grps[gn0:gn1]
  gn                                    <- length(grps)
  for (i in 1:gn) {
    grp                                 <- grps[i]
    temp0                               <- unique(wgts[wgts$Group==grp,"Value"])
    tn                                  <- length(temp0)
    temp0                               <- data.frame(Group=rep(grp,tn),Value=temp0)
    if (i == 1) {
      temp                              <- temp0
    } else {  
      temp                              <- rbind(temp,temp0)
    }
  }
  wgts                                  <- left_join(temp,wgts,by=c("Group","Value"))
  wgts[is.na(wgts$Lower_Weight),
       "Lower_Weight"]                  <- 1
  wgts[is.na(wgts$Upper_Weight),
       "Upper_Weight"]                  <- 1
  wgts[is.na(wgts$Weight),  
       "Weight"]                        <- 1
  wn                                    <- nrow(wgts)
  meta$Weight                           <- 1
  
  for (i in 1:wn) {
    wgt                                 <- wgts$Weight[i]
    grp                                 <- wgts$Group[i]
    val                                 <- wgts$Value[i]
    up                                  <- wgts$Upper_Weight[i]
    low                                 <- wgts$Lower_Weight[i]
    if (up < low) {
      up                                <- (up + low + wgt) / 3
      low                               <- up
      wgt                               <- up
      wgts$Upper_Weight[i]              <- up
      wgts$Lower_Weight[i]              <- up
      wgts$Weight[i]                    <- up
    }
    if (wgt > up) {
      wgt                               <- up
      wgts$Weight[i]                    <- up      
    }
    if (wgt < low) {
      wgt                               <- low
      wgts$Weight[i]                    <- low     
    }
    meta[meta[,grp] == val,"Weight"]    <- meta[meta[,grp] == val,"Weight"] * wgt
  }
  obj$meta                              <- meta
  obj$weights                           <- wgts
  obj$Panel                             <- panel
  obj$CS                                <- cs
  return(obj)
 
}

Distribute_Weights                      <- function(obj,
                                                    Optional_Scaler = 1,
                                                    Scale_Types     = "None",
                                                    print           = FALSE,
                                                    megaprint       = FALSE,
                                                    timelag         = 10) {
  
### This function has 6 inputs:
###    obj                     --- A list with 3 elements
###                                   data      = is the model input data
###                                   full_spec = The spec file
###                                   meta      = the meta file (including elements
###                                               to be weighted, a composate of the
###                                               weights, and the highest level of
###                                               variables related to the model)
###    Optional_Scaler = #     --- This is an optional number that has no effect
###                                   on the final model but might make the
###                                   model coefficients and the Bayesian priors 
###                                   easier to deal with.
###                                For example if the coefficients of the model
###                                   are small fractions (like 0.0000632) you
###                                   can make the Optional_Scaler = 100000
###                                   which will cause the coefficients (and so
###                                   the Bayesian priors) appear as a number
###                                   like 6.32. Just easier to deal with.
###    Scale_Types     = list  --- Is a list of "Types" the Optional_Scaler is 
###                                   applied to.  The types are values of the 
###                                   Variable_Type column of the spec file. 
###                                   if not in use you can set the Scale_Types
###                                   = "None" and the Optional_Scaler = 1
###    megaprint       = TRUE  --- Prints everything in the R Log.  Just a continuous
###                                   record of where the run is and what is happening
###    print           = TRUE  --- Just a limited print of where things are in the run
###                                    (just enough to see what is happening)
###                                    Also it activates the "timelag" timing see below.
###    timelag         = # sec --- A number in seconds.  If you set timelag = 10 then
###                             very 10 seconds or so they show you where the
###                             run is at that time
###  
### This code takes the weights (from the meta file) which are only on the top
###    level and distributes them to every level.  It also stablizes (normalizes)
###    the transforms and weights for every level between the model level and the
###    maximum (mega level)
###
### This is done by the applying the weights to the highest level directly
###    from the weights loaded into the "Meta" file.  Then these "raw weights"
###    will be applied to the levels below that (up to the modeling level).
###    Then the weights on the modeling level are "flattened" out so the
###    Bayesian priors will still be usable and unchanged. After that the raw
###    weights will be adjusted so all the levels above the modeling will
###    reflect these adjusted weights.  For example, weighted average of the 
###    weights on level 6 will match be the weight of the variable on level 5
###    that those level 6 variables add up to.  And finally the levels below the 
###    modeling level will be changed to agree with the modeling level (below
###    the modeling level all the weights are "1")
###
### This first sub-step just sets things up and transfers the weights from
###    the meta file to the highest level.

  
  if (megaprint)                           cat("Distribute Weights Step  1 \n")
  if (print)                               cat("Distribute and normalize weights on every level of transformed variables \n")
  mt1                                   <- Sys.time() + timelag
  x                                     <- obj$data
  spec                                  <- obj$spec
  full_spec                             <- obj$full_spec
  meta                                  <- obj$meta
  
  nmx                                   <- names(x)
  mlev                                  <- 1
  elev                                  <- 1
  nfs                                   <- names(full_spec)
  mlev                                  <- max(full_spec[full_spec$Include>0 ,"Level"])
  elev                                  <- max(full_spec[full_spec$Include==1,"Level"])
  use_spec                              <- full_spec[full_spec$Include>0,]
  nmx                                   <- names(x)
  xn                                    <- nrow(x)
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Distribute Weights Step  2 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  
  # Load the weights
  temp                                  <- meta[,c("Orig_Variable","Weight")]
  names(temp)                           <- c("Orig_Variable","Raw_Weight")
  temp$Level                            <- mlev
  use_spec$Raw_Weight                   <- NULL
  use_spec                              <- left_join(use_spec,temp,by=c("Orig_Variable","Level"))
  use_spec[use_spec$Level < mlev,
           "Raw_Weight"]                <- 1
  use_spec$Var_Adj_Times                <- 1
  use_spec$Var_Adj_Plus                 <- 0
  use_spec$Centered_Weight              <- 1
  spec$Var_Adj_Times                    <- 1
  spec$Var_Adj_Plus                     <- 0
  spec$Centered_Weight                  <- 1
  # Check that there is a unique transform variable to load weights into
  for (i in 1:mlev) {
    temp                                <- use_spec[(use_spec$Level         == i)           &
                                                    (use_spec$Variable_Type != "Dependent") &
                                                    (use_spec$Orig_Variable != "Intercept"),]
    tn                                  <- nrow(temp)
    for (j in 1:tn) {
      var                               <- temp$Orig_Variable[j]
      vart                              <- temp$Trans_Variable[j]
      tran                              <- temp$Transform[j]
      trant                             <- toupper(temp$TransformType[j])
      if ((tran  == "N")      | 
          (trant == "WEIGHT") | 
          (trant == "NONE")   | 
          (var   == vart))    {
        use_spec[(use_spec$Orig_Variable == var)   &
                 (use_spec$Level == i0),
                 "Transform"]           <- "Y"
        use_spec[(use_spec$Orig_Variable == var)   &
                 (use_spec$Level == i0),
                 "TransformType"]       <- "Weight"
        use_spec[(use_spec$Orig_Variable == var)   & 
                 (use_spec$Level == i0),
                 "Adstock"]             <- "Weight"      
        use_spec[(use_spec$Orig_Variable == var)   &
                 (use_spec$Level == i0),
                 "Dim_Returns"]         <- "Weight"
        if (var == vart) {
          vart                          <- paste(var,"_t",sep="")
          use_spec[(use_spec$Orig_Variable == var) &
                   (use_spec$Level == i),
                   "Trans_Variable"]    <- vart
          temp$Trans_Variable[j]        <- vart
        }
      }
    }
  }
  #Make sure the trans variable are all removed so they can be recreated
  temp                                  <- data.frame(use_spec[(use_spec$Variable_Type != "Dependent") &
                                                                 (use_spec$Orig_Variable != "Intercept"),
                                                               "Trans_Variable"])
  names(temp)                           <- "Trans_Variable"
  temp                                  <- unique(temp$Trans_Variable)
  tn                                    <- length(temp)
  for (i in (1:tn)) {
    var                                 <- temp[i]
    if (var %in% nmx) {
      x[,var]                           <- NULL
    }
  }
  nmx                                   <- names(x)
  
  #use the weights to transform the top level
  temp                                  <- use_spec[(use_spec$Level==mlev),]
  tn                                    <- nrow(temp)
  for (j in 1:tn) {
    var                                 <- temp$Orig_Variable[j]
    vart                                <- temp$Trans_Variable[j]
    prewgt                              <- temp$PreWeight[j]
    wgt                                 <- temp$Raw_Weight[j]
    adjb                                <- temp$DR_Adj_Plus[j]
    varas                               <- paste(var,"_as",sep="")
    if (!(varas %in% nmx))      varas   <- var
    vardr                               <- paste(var,"_uw",sep="")
    if (!(vardr %in% nmx))      vardr   <- varas
    x[,vart]                            <- x[,vardr] * wgt
    adjb                                <- adjb * wgt
    use_spec[(use_spec$Orig_Variable == var) & 
             (use_spec$Level         == mlev),
             "Var_Adj_Plus"]            <- adjb 
    if (vardr == var)
      x[,vart]                          <- x[,vart] * prewgt
    nmx                                 <- c(nmx,vart)
  }  

  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Distribute Weights Step  3 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### This next sub-step distributes down the raw weights to the levels below
  ###    up to the modeling level.  This is mostly just adding higher levels up
  ###    to make lower levels, but also document what weights were added.
  
  if (elev < mlev) {
    elev0                               <- mlev - elev
    for (i in 1:elev0) {
      i0                                <- mlev - i
      i1                                <- i0 + 1
      temp                              <- use_spec[(use_spec$Level         == i0)          &
                                                    (use_spec$Variable_Type != "Dependent") &
                                                    (use_spec$Orig_Variable != "Intercept"),]
      tn                                <- nrow(temp)
      for (j in 1:tn) {
        var                             <- temp$Orig_Variable[j]
        vart                            <- temp$Trans_Variable[j]
        prewgt                          <- temp$PreWeight[j]
        adjbb                           <- temp$DR_Adj_Plus[j]
        varas                           <- paste(var,"_as",sep="")
        if (!(varas %in% nmx))  varas   <- var
        vardr                           <- paste(var,"_uw",sep="")
        if (!(vardr %in% nmx))  vardr   <- varas
        temp0                           <- data.frame(use_spec[(use_spec$Level           == i1) & 
                                                               (use_spec$Next_Level_Down == var),])
        rn                              <- nrow(temp0)
        adjb0                           <- 0
        if (rn > 0) {
          if (!(vart %in% nmx)) {
            x[,vart]                    <- 0
            adjb0                       <- 0
            for (k in 1:rn) {
              var0                      <- temp0$Orig_Variable[k]
              vart0                     <- temp0$Trans_Variable[k]
              adjb                      <- temp0$Var_Adj_Plus[k]
              x[,vart]                  <- x[,vart] + x[,vart0]
              adjb0                     <- adjb0    + adjb
            }
          } else {
            adjb0                       <- temp0[temp0$Trans_Variable==vart,"Var_Adj_Plus"]
            if (length(adjb0) > 1) 
              adjb0                     <- adjb0[1]
          }
          mndr                          <- abs(mean(x[,vardr]))
          mnt                           <- abs(mean(x[,vart]))
          if (mndr < .000001) {
            mndr                        <- abs(max(x[,vardr]))
            mnt                         <- abs(max(x[,vart]))
          }
          if (vardr == var) 
            mndr                        <- mndr * prewgt
          wgt                           <- 1
          if (mndr > 0)             wgt <- mnt / mndr 
          nmx                           <- unique(c(nmx,vart))
          use_spec[(use_spec$Orig_Variable == var) &
                   (use_spec$Level         == i0),
                   "Raw_Weight"]        <- wgt
          use_spec[(use_spec$Orig_Variable == var) &
                   (use_spec$Level         == i0),
                   "Var_Adj_Plus"]      <- adjb0
        }
      }
    }
  }

  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Distribute Weights Step  4 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### This next sub-step "normalizes" the transforms in the modeling level so
  ###    the priors on the model don't change even as the weights change.
  ###    The last two steps will then normalize the levels above and below the
  ###    modeling step so everything agrees.

  temp                                  <- use_spec[(use_spec$Level         == elev)        &
                                                    (use_spec$Variable_Type != "Dependent") &
                                                    (use_spec$Orig_Variable != "Intercept"),]
  tn                                    <- nrow(temp)
  for (j in 1:tn) {
    var                                 <- temp$Orig_Variable[j]
    vart                                <- temp$Trans_Variable[j]
    adja0                               <- temp$DR_Adj_Times[j]
    adjb0                               <- temp$DR_Adj_Plus[j]
    adjc0                               <- temp$Var_Adj_Plus[j]
    prewgt                              <- temp$PreWeight[j]
    wgt                                 <- temp$Raw_Weight[j]
    varas                               <- paste(var,"_as",sep="")
    if (!(varas %in% nmx))      varas   <- var
    vardr                               <- paste(var,"_uw",sep="")
    if (!(vardr %in% nmx))      vardr   <- varas
    data_vector                         <- x[,vart]
    if (vart  == var)   data_vector     <- data_vector    * prewgt
    data_vector                         <- data_vector - adjc0
    data_vector                         <- data_vector / adja0
    data_vector_as                      <- x[,varas]
    if (varas == var)   data_vector_as  <- data_vector_as * prewgt
    sdt                                 <- sd(data_vector)
    sdas                                <- sd(data_vector_as)
    mnt                                 <- mean(data_vector)
    mnas                                <- mean(data_vector_as)
    mnt2                                <- mean(data_vector * data_vector)
    mnad                                <- mean(data_vector * data_vector_as)
    adja                                <- -1
    mnt1                                <- mnt2   - (mnt * mnt)
    mnad1                               <- mnad   - (mnt * mnas)
    if ((mnt1  != 0) &
        (mnad1 != 0))              adja <- mnad1 / mnt1
    if (adja <= 0) {
      adja                              <- 1
      if ((sdt  > 0) & 
          (sdas > 0))              adja <- sdas  / sdt
    }
    adjb                                <- mnas - (adja * mnt)
    x[,vart]                            <- (data_vector * adja) + adjb
    use_spec[(use_spec$Orig_Variable == var) &
             (use_spec$Level == elev),
             "Var_Adj_Times"]           <- adja
    use_spec[(use_spec$Orig_Variable == var) &
             (use_spec$Level == elev),
             "Var_Adj_Plus"]            <- adjb
    use_spec[(use_spec$Orig_Variable == var) &
             (use_spec$Level == elev),
             "Centered_Weight"]         <- 1
  }  

  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Distribute Weights Step  5 \n")
  if (nextprint)             mt1        <- mt2 + timelag

  ### Now that we have "normalized" the modeling level we need to "stabilize" the
  ###    levels above and below so they all agree.  We will start with the levels
  ###    below since that is just the simple task of adding more detailed data to
  ###    to create less detailed variables.  Also, since the weight at the 
  ###    modeling level is now "1" the weight at the levels below that are also "1"

  if (elev > 1) {
    elev0                               <- elev - 1
    for (i in 1:elev0) {
      i0                                <- elev - i
      i1                                <- i0 + 1
      temp                              <- use_spec[(use_spec$Level         == i0)          &
                                                    (use_spec$Variable_Type != "Dependent") &
                                                    (use_spec$Orig_Variable != "Intercept"),]
      qn                                <- nrow(temp)
      if (qn > 0) {
        for (j in 1:qn) {
          var                           <- temp$Orig_Variable[j]
          prewgt                        <- temp$PreWeight[j]
          adjb0                         <- temp$Var_Adj_Plus[j]
          vart                          <- temp$Trans_Variable[j]
          varas                         <- paste(var,"_as",sep="")
          if (!(varas %in% nmx))  varas <- var
          vardr                         <- paste(var,"_uw",sep="")
          if (!(vardr %in% nmx))  vardr <- varas
          temp0                         <- use_spec[(use_spec$Next_Level_Down == var) &
                                                    (use_spec$Level           == i1),]
          rn                            <- nrow(temp0)
          if (rn > 0) {
            temp2                       <- data.frame(x[,vardr])
            names(temp2)                <- c("base")
            temp2$baset                 <- rep(0,xn)
            if (var == vardr) 
              temp2$base                <- temp2$base * prewgt
            adjb0                       <- 0
            for (k in 1:rn) {
              var0                      <- temp0$Orig_Variable[k]
              vart0                     <- temp0$Trans_Variable[k]
              adjb                      <- temp0$Var_Adj_Plus[k]
              temp2$baset               <- temp2$baset + x[,vart0]
              adjb0                     <- adjb0 + adjb
            }
            x[,vart]                    <- temp2$baset
            if (!(vart %in% nmx)) nmx   <- c(nmx,vart)
            use_spec[(use_spec$Orig_Variable   == var) &
                     (use_spec$Level           == i0),
                     "Var_Adj_Times"]   <- 1
            use_spec[(use_spec$Orig_Variable   == var) &
                     (use_spec$Level           == i0),
                     "Var_Adj_Plus"]    <- adjb0
            use_spec[(use_spec$Orig_Variable   == var) &
                     (use_spec$Level           == i0),
                     "Centered_Weight"] <- 1
          }
        }
      }  
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Distribute Weights Step  6 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### We finish this section by "normalizing" the level above the modeling level
  ###    and calculate the new weights.   At the end the each variable at any 
  ###    level (except the top level) is made up of the variables above it and 
  ###    the weights are normalized so the weights at the modeling level is "1".
  substep                               <- 0    
  if ((wflag == 1) & (elev < mlev)) {
    elev0                               <- mlev - elev
    for (i in 1:elev0) {
      i0                                <- elev + i - 1
      i1                                <- i0 + 1
      substep                           <- substep + 1
      mt2                               <- Sys.time()
      nextprint                         <- FALSE
      if (megaprint)          nextprint <- TRUE
      if (print & (mt2 > mt1)) 
        nextprint                       <- TRUE
      if (nextprint)                        cat("Distribute Weights Step  7 substep",substep,"\n")
      if (nextprint)           mt1      <- mt2 + timelag
      temp                              <- use_spec[(use_spec$Level         == i0)          &
                                                    (use_spec$Variable_Type != "Dependent") &
                                                    (use_spec$Orig_Variable != "Intercept"),]
      qn                                <- nrow(temp)
      if (qn > 0) {
        for (j in 1:qn) {
          var                           <- temp$Orig_Variable[j]
          mvar                          <- temp$Next_Level_Down[j]
          prewgt                        <- temp$PreWeight[j]
          adjbb                         <- temp$Var_Adj_Plus[j]
          vart                          <- temp$Trans_Variable[j]
          wgtt                          <- temp$Centered_Weight[j]
          varas                         <- paste(var,"_as",sep="")
          if (!(varas %in% nmx))  varas <- var
          vardr                         <- paste(var,"_uw",sep="")
          if (!(vardr %in% nmx))  vardr <- varas
          temp0                         <- use_spec[(use_spec$Next_Level_Down == var) &
                                                    (use_spec$Level           == i1),]
          rn                            <- nrow(temp0)
          if (rn > 0) {
            temp2                       <- data.frame(x[,vardr],x[,vart])
            names(temp2)                <- c("base","baset")
            if (varas == var)
              temp2$base                <- temp2$base   * prewgt
            if (vart  == var)
              temp2$baset               <- temp2$baset  * prewgt
            temp2$baset                 <- temp2$baset  - adjbb
            subs                        <- paste("sub",1:rn,sep="")
            subts                       <- paste("subt",1:rn,sep="")
            adjbs                       <- c(rep(0,rn))
            mints                       <- rep(0,rn)
            temp2$sub0                  <- 0
            temp2$subt0                 <- 0
            adjb0                       <- 0
            for (k in 1:rn) {
              var0                      <- temp0$Orig_Variable[k]
              prewgt0                   <- temp0$PreWeight[k]
              vart0                     <- temp0$Trans_Variable[k]
              adjb                      <- temp0$Var_Adj_Plus[k]
              wgt                       <- temp0$Raw_Weight[k]
              varas0                    <- paste(var0,"_as",sep="")
              if (!(varas0 %in% nmx)) 
                varas0                  <- var0
              vardr0                    <- paste(var0,"_uw",sep="")
              if (!(vardr0 %in% nmx)) 
                vardr0                  <- varas0
              sub                       <- subs[k]
              subt                      <- subts[k]
              temp2[,sub]               <- x[,vardr0]
              temp2[,subt]              <- x[,vart0] - adjb
              if (vardr0 == var0)
                temp2[,sub]             <- temp2[,sub]  * prewgt0
              temp2$sub0                <- temp2$sub0   + temp2[,sub]
              temp2$subt0               <- temp2$subt0  + temp2[,subt]
              adjbs[k]                  <- adjb
              adjb0                     <- adjb0 + adjb
              mints[k]                  <- sum(abs(temp2[,subt]))
            }
            adj                         <- 0
            temp2$adj                   <- 0
            temp2[(temp2$baset != 0) & 
                  (temp2$subt0 != 0),
                  "adj"]                <- temp2[(temp2$baset != 0) & 
                                                 (temp2$subt0 != 0),"baset"] / 
              temp2[(temp2$baset != 0) & 
                    (temp2$subt0 != 0),"subt0"]
            adjb1                       <- 0
            
            if ((adjbb != 0) & 
                (adjb0 != 0) & 
                (adjb0 * adjbb > 0)) {
              adjb1                     <-         adjbb / adjb0
              adjbs                     <- adjbs * adjbb / adjb0
            } else {
              adjb1                     <- sum(mints)
              if (adjb1 > 0) {
                adjbs                   <- mints * adjbb / adjb1
              } else {
                adjbs                   <- adjbs * 0
              }
            }
            for (k in 1:rn) {
              subt                      <- subts[k]
              sub                       <- subs[k]
              temp2[,subt]              <- temp2[,subt] * temp2$adj
            }
            for (k in 1:rn) {
              var0                      <- temp0$Orig_Variable[k]
              vart0                     <- temp0$Trans_Variable[k]
              imp0                      <- temp0$Raw_Weight[k]
              adjb                      <- adjbs[k]
              sub                       <- subs[k]
              subt                      <- subts[k]
              temp2[,subt]              <- temp2[,subt] + adjb
              x[,vart0]                 <- temp2[,subt]
              if (!(vart0 %in% nmx)) 
                nmx                     <- c(nmx,vart0)
              mndr                      <- abs(mean(temp2[,sub]))
              mnt                       <- abs(mean(temp2[,subt]))
              wf                        <- 1
              if (mndr < 0.000001) {
                mndr                    <- abs(max(temp2[,sub]))
                mnt                     <- abs(max(temp2[,subt]))
                wf                      <- 0
              }
              wgt                       <- 1
              if (mndr > 0)         wgt <-  mnt / mndr
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == i1),
                       "Var_Adj_Times"] <- 1
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == i1),
                       "Var_Adj_Plus"]  <- adjb
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == i1),
                       "Centered_Weight"] <- wgt
            }
          }
        }
      }
    }  
  }
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Distribute Weights Step  8 \n")
  if (nextprint)             mt1        <- mt2 + timelag  
  
  un                                    <- nrow(use_spec)

  full_spec$Var_Adj_Times               <- 1
  full_spec$Var_Adj_Plus                <- 0
  full_spec$Centered_Weight             <- 1
  full_spec$DR_Adj_Plus                 <- 0
  full_spec$DR_Adj_Times                <- 1
  full_spec$Raw_Weight                  <- 1

  done                                    <- c("Intercept")
  
  for (i in 1:un) {
    var                                 <- use_spec$Orig_Variable[i]
    vart                                <- use_spec$Trans_Variable[i]
    varm                                <- use_spec$Model_Var[i]
    lev                                 <- use_spec$Level[i]
    prewgt                              <- use_spec$PreWeight[i]
    type                                <- use_spec$Variable_Type[i]
    adja0                               <- use_spec$DR_Adj_Times[i]
    adja1                               <- use_spec$Var_Adj_Times[i]
    adjb0                               <- use_spec$DR_Adj_Plus[i]
    adjb1                               <- use_spec$Var_Adj_Plus[i]
    wgt                                 <- use_spec$Centered_Weight[i]
    wgt0                                <- use_spec$Raw_Weight[i]
    varas                               <- paste(var,"_as",sep="")
    if (!(varas %in% nmx))       varas  <- var
    vardr                               <- paste(var,"_uw",sep="")
    if (wflag == 0)              vardr  <- vart
    if (!(vardr %in% nmx))       vardr  <- varas
    if (!(vart  %in% nmx))       vart   <- vardr
    if (vart == vardr) {
      adja1                             <- adja0
      adjb1                             <- adjb0
    }  
    if (is.na(wgt))              wgt    <- 1
    if (!(var %in% done)) {
      x[,vart]                          <- (x[,vart] - adjb1) / prewgt
      x[(abs(x[,vart])  < 0.00000001) &
        (abs(x[,varas]) < 0.00000001),
        vart]                           <- 0
      if (type %in% Scale_Types) 
        x[,vart]                        <-  x[,vart]          / Optional_Scaler
      done                              <- c(done,var)
      nmx                               <- names(x)
    }  
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "Model_Var"]              <- varm
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "DR_Adj_Times"]           <- adja0
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "DR_Adj_Plus"]            <- adjb0
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "Var_Adj_Times"]          <- adja1
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "Var_Adj_Plus"]           <- adjb1
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "Centered_Weight"]        <- wgt
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "Raw_Weight"]             <- wgt0
  }
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Distribute Weights Step  9 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  Full_Model_Spec                       <- full_spec
  Model_Spec                            <- Full_Model_Spec %>% filter(Include == 1)
  obj$data                              <- x
  obj$spec                              <- Model_Spec
  obj$full_spec                         <- full_spec
  obj$Optional_Scaler                   <- Optional_Scaler
  obj$Scale_Types                       <- Scale_Types
  return(obj)
}


Hybrid_Variable_Tranformations          <- function(obj,
                                                    MultiLevelDR    = TRUE,
                                                    par_bypanel     = FALSE,
                                                    print           = FALSE,
                                                    megaprint       = FALSE,
                                                    timelag         = 10) {
### This function has 4 inputs:
###    obj                  --- A list with 6 elements
###                                data      = is the model input data
###                                full_spec = The spec file
###                                meta      = the meta file (including elements
###                                            to be weighted, a composate of the
###                                            weights, and the highest level of
###                                            variables related to the model)
###                                Panel     = Is it a panel model (Y or N)
###                                CS        = In panel models - the name of the
###                                            variable that defines the splits 
###                                            for the panels (region, DMAs, etc.)
###                                fit_curve = Curve parameters used in some
###                                            transforms.
###    MultiLevelDR = FALSE --- Means that the diminishing returns are only on one
###                             variable per branch level and every variable
###                             above and below that variable agrees as close as
###                             possible to the curves of that one variable.
###                 = TRUE  --- Means that the diminishing returns are mainly on
###                             one variable per branch level but the Dim Ret. 
###                             curve at every level trys to mimic the curve of
###                             the main variable as much as possible.
###                             TRUE or FALSE is a business decision based on how
###                             you view diminishing returns (not a modeling
###                             choice)
###    par_bypanel  = TRUE  --- Used in panel models for some transformations
###    megaprint    = TRUE  --- prints everything in the R Log.  Just a continuous
###                                record of where the run is and what is happening
###    print        = TRUE  --- Just a limited print of where things are in the run
###                               (just enough to see what is happening)
###                               Also it activates the "timelag" timing see below.
###    timelag     = # sec --- A number in seconds.  If you set timelag = 10 then
###                               very 10 seconds or so they show you where the
###                               run is at that time
###  
### This function creates the various transforms for a hypbrid (hierachical)
###    group of variables.
###    it starts with the adstock transforms, then the unweighted transforms
###    (which is the combined adstock and dim. returns transformations) and
###    sets things up for the well as the weighted transform (applying the
###    weights).
###    Note this function replaces the Transform and Transform_panel functions
###    even if the weighting work isn't being used.  Note the difference with using
###    this code, is it stabilizes the transform so that if the transform changes
###    the bayesian priors don't need to change.

  ### Start by setting up the data tables and flags
  mt1                                   <- Sys.time() + timelag
  if (megaprint)                           cat("Transforms Step  1 \n")
  
  x                                     <- obj$data
  spec                                  <- obj$spec
  full_spec                             <- obj$full_spec
  fit_curves                            <- obj$fit_curves
  panel                                 <- obj$Panel
  cs                                    <- obj$CS
  
  output                                <- list()
  csflag                                <- 0
  splts                                 <- 1
  
  if (panel == "Y") {
     csflag                             <- nchar(cs)
     if ((csflag > 0)                         & 
         (length(unique(obj$data[[cs]]))<=1)) {
        csflag                          <- 0
        panel                           <- "N"
        cs                              <- NULL
     }
     if  (csflag > 0)     csflag        <- 1
  }
  
  if (csflag > 0) {
    if(par_bypanel) {
      spec                              <- obj$spec_transform
      split_spec                        <- base::split(spec, spec[[obj$CS]])
    }
    split_data                          <- base::split(obj$data, obj$data[[obj$CS]])
    splts                               <- length(split_data)
  }
  
  panel                                 <- "N"
  wflag                                 <- 0
  mlev                                  <- 1
  elev                                  <- 1
  nfs                                   <- names(full_spec)
  if (length(nfs) > 1) {
    if (("Level" %in% nfs)            & 
        ("Next_Level_Down" %in% nfs)) {
      mlev                              <- max(full_spec[full_spec$Include>0 ,"Level"])
      elev                              <- max(full_spec[full_spec$Include==1,"Level"])
      wflag                             <- 1
      if (mlev == 1)    wflag           <- 0
    } else {
      full_spec$Level                   <- 1
      full_spec$Next_Level_Down         <- "End"
    }
  }
  
  if (wflag == 1) {
    meta                                <- obj$meta
  }
  
  if (wflag == 0) {
    use_spec                            <- spec
  } else {
    use_spec                            <- full_spec[full_spec$Include>0,]
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step  2 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### If the weight/level variable/spec file check was run, this part isn't needed
  ###    but we are including it anyway just to be sure the Transform_type is divided
  ###    into adstocks and diminishing return transforms.  Much of this function won't
  ###    work without this division.
  
  nfs                                   <- names(use_spec)
  
  aslist                                <- c("ADSTOCK","ADSTOCKV2","ADSTOCKV3","ADSTOCKG","LAG","MA","MC","STEIN","CPT","ASDOWN","ASUP")
  drlist                                <- c("ADR","ABC","ATAN","LOG","POLY","POWER","DRDOWN","DRUP")
  
  if ((!("Dim_Returns" %in% nfs)) | 
      (!("Adstock" %in% nfs)))    {
    use_spec$Dim_Returns                <- "None"
    use_spec$Adstock                    <- "None"
    use_spec[use_spec$Transform=="N",
             "TransformType"]           <- "None"
    sn                                  <- nrow(use_spec)
    for (i in 1:sn) {
      tran                              <- use_spec$TransformType[i]
      if (is.na(tran)) tran             <- "None"
      if (toupper(tran) == "WIEGHT") 
          tran                          <- "None"
      if (toupper(tran) != "NONE") {
        type                            <- unlist(strsplit(tran, "_"))
        utype                           <- toupper(type)
        tn                              <- length(type)
        for (j in 1:tn) {
          trn                           <- type[j]
          utrn                          <- utype[j]
          if ((utrn == "ASDOWN")   | 
              (utrn == "ASUP")     | 
              (utrn == "DRDOWN")   |
              (utrn == "DRUP"))    {
            trn                         <- "None"
            utrn                        <- "NONE"
          }
          if (utrn %in% aslist) {
            trn0                        <- use_spec$Adstock[i]
            utrn0                       <- toupper(trn0)
            if (utrn0 == "NONE") {
              trn0                      <- trn
            } else {
              trn0                      <- paste(trn0,"_",trn,sep="")
            }
            use_spec$Adstock[i]         <- trn0
          }
          if (utrn %in% drlist) {
            trn0                        <- use_spec$Dim_Returns[i]
            utrn0                       <- toupper(trn0)
            if (utrn0 == "NONE") {
              trn0                      <- trn
            } else {
              trn0                      <- paste(trn0,"_",trn,sep="")
            }
            use_spec$Dim_Returns[i]     <- trn0
          }
        }
      }
      use_spec[toupper(use_spec$TransformType) == "WIEGHT",
               "Adstock"]               <- "Weight"
      use_spec[toupper(use_spec$TransformType) == "WIEGHT",
               "Dim_Returns"]           <- "Weight"
    }
  }
  
  mt1                                   <- Sys.time() + timelag
  if (megaprint)                           cat("Transforms Step  3 \n")
  if (print)                               cat("Transform the Adstocks \n")    
  
  ### Calculate the adstock transform of variables where a standard transform is given
  ###    if the file has "panel" splits, this might take a while
  
  temp                                  <- c("NONE","ASUP","DRDOWN","WEIGHT")
  temp                                  <- use_spec[(!(toupper(use_spec$Adstock) %in% temp)) & 
                                                      (use_spec$Transform == "Y"),]
  tn                                    <- nrow(temp)
  substep                               <- 0
  
  if (tn > 0) {
    nmy                                 <- temp$Orig_Variable
    nmy                                 <- c(nmy,obj$Time)
    if (csflag == 1)            nmy     <- c(nmy,obj$CS)
    nmy                                 <- unique(nmy)
    y                                   <- x[,c(nmy)]
    for (k in 1:splts) {
      temp0                             <- temp
      y0                                <- y
      nmy0                              <- names(y0)
      substep                           <- substep + 1
      mt2                               <- Sys.time()
      nextprint                         <- FALSE
      if (megaprint)           
          nextprint                     <- TRUE
      if (print & (mt2 > mt1))
          nextprint                     <- TRUE
      if (nextprint)                       cat("Transforms Step  4 substep",substep,"\n")
      if (nextprint)             mt1    <- mt2 + timelag
      if (csflag == 1) {
        split_data                      <- split(y, y[[obj$CS]])
        y0                              <- split_data[[k]]
        y0                              <- y0[order(y0[[obj$Time]]), ]
        nmy0                            <- names(y0)
        if(par_bypanel) {
          split_spec                    <- split(temp0, temp0[[obj$CS]])
          temp0                         <- split_spec[[y[[obj$CS]][1]]]
        }
      }
      for (i in 1:tn) {
        adst                            <- toupper(temp0$Adstock[i])
        var                             <- temp0$Orig_Variable[i]
        prewgt                          <- 1
        if (wflag == 1) prewgt          <- temp0$PreWeight[i]
        varas                           <- paste(var,"_as",sep="")
        if (  (var     %in% nmy0)   & 
              (!(varas %in% nmy0))) {
          type                          <- unlist(strsplit(adst, "_"))
          data_vector                   <- y0[[var]] * prewgt
          for(j in 1:length(type)) {
            data_vector_transform       <- list()
            if (type[j] == "ADSTOCK"){
              data_vector_transform     <- AdStockPD(data_vector,
                                                     temp0$Decay[i],
                                                     temp0$Period[i])
            }
            if (type[j] == "ADSTOCKV2"){
              data_vector_transform     <- adstock(data_vector, 
                                                   temp0$Decay[i])
            }
            if (type[j] == "ADSTOCKV3"){
              data_vector_transform     <- adstockv3(data_vector,
                                                     temp0$Decay[i], 
                                                     temp0$Peak[i], 
                                                     temp0$Length[i])
            }
            if (type[j] == "ADSTOCKG"){
              data_vector_transform     <- AdstockGamma(data_vector, 
                                                        decay      = temp0$Decay[i], 
                                                        peak       = temp0$Peak[i],
                                                        spred      = temp0$Spred[i],
                                                        period     = temp0$Period[i],
                                                        print      = FALSE)
            }
            if (type[j] == "LAG") {
              data_vector_transform     <- lag(data_vector,
                                               temp0$Lag[i],
                                               default = 0)
            }
            if (type[j] == "MA") {
              data_vector_transform     <- rollmean(data_vector, 
                                                    temp0$Window[i], 
                                                    align="right", fill=NA)
              data_vector_transform[which(is.na(data_vector_transform))] <- data_vector[which(is.na(data_vector_transform))]
            }
            if (type[j] == "MC") {
              data_vector_transform     <- scale(data_vector)
              scl                       <- attr(data_vector_transform, "scaled:scale")
              cen                       <- attr(data_vector_transform, "scaled:center")
            }
            if (type[j] == "STEIN") {
              data_vector_transform     <- shrinker(data_vector,
                                                    bw=temp0$Window[i],
                                                    trim=temp0$Trim[i])
            }
            if (type[j] == "CPT") {
              data_vector_transform     <- cpt.meanvar(data_vector, 
                                                       minseglen = 6,
                                                       penalty = "CROPS",
                                                       pen.value = c(0, 100),
                                                       method = "PELT")
            }
            data_vector <- data_vector_transform
          }
          y0[,varas]                    <- data_vector
          nmy0                          <- c(nmy0,varas)
          if (type[j] == "MC") {
            varscl                      <- paste(var,"_scl",sep="")
            varcenl                     <- paste(var,"_cen",sep="")
            y0[,varscl]                 <- scl
            y0[,varcen]                 <- cen
          }
        }
      }
      output[[k]]                       <- y0
    }
    y                                   <- do.call("rbind", output)
    nmy0                                <- names(y)
    nmy0                                <- setdiff(nmy0,nmy)
    nn                                  <- length(nmy0)
    if (nn > 0) {
      for (i in 1:nn) {
        var                             <- nmy0[i]
        x[,var]                         <- y[,var]
      }
    }
  }
  
  nmx                                   <- names(x)
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step  5 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### This section just sets up some new variables that are used to stabalize the
  ###    transformed data so the bayesian priors don't need to change if the 
  ###    transform changes.  This is necessary to make sure the priors don't need
  ###    to change when the weights change, (which allows the optimizer to work)
  ###    but it also helps to not have to change the priors when anything about
  ###    the transform changes.
  
  
  use_spec$DR_Adj_Times                 <- 1
  use_spec$DR_Adj_Plus                  <- 0
  use_spec$asdone                       <- 0
  use_spec$drdone                       <- 0
  
  nmx                                   <- names(x)
  un                                    <- nrow(use_spec)
  
  for (i in 1:un) {
    adst                                <- toupper(use_spec$Adstock[i])
    drst                                <- toupper(use_spec$Dim_Returns[i])                                
    var                                 <- use_spec$Orig_Variable[i]
    if (wflag == 0) {
      lev                               <- 1
    } else {
      lev                               <- use_spec$Level[i]
    }
    varas                               <- paste(var,"_as",sep="")
    vardr                               <- paste(var,"_dr",sep="")
    as                                  <- 0
    if (varas %in% nmx)             as  <- 1
    if (use_spec$Transform[i] == "N")
        as                              <- 1  
    if (adst == "NONE")             as  <- 1
    if (adst == "WEIGHT")           as  <- 1
    use_spec$asdone[i]                  <- as
    dr                                  <- 0
    if (vardr %in% nmx)             dr  <- 1
    if (use_spec$Transform[i] == "N")
        dr                              <- 1  
    if (drst == "NONE")             dr  <- 1
    if (drst == "WEIGHT")           dr  <- 1
    use_spec$drdone[i]                  <- dr
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step  6 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ###  Now finish up the adstock transform by combining adstock transforms at higher
  ###     levels to making adstock transforms at lower levels (so for example the
  ###     level 1 broader variable is made up of several level 2 more detailed
  ###     variables)
  
  if (wflag == 1) {
    for (i in 1:mlev) {
      temp                              <- use_spec[(use_spec$asdone==0) &
                                                    (toupper(use_spec$Adstock) == "ASUP"),]
      qn                                <- nrow(temp)
      if (qn > 0) {
        for (j in 1:qn) {
          var                           <- temp$Orig_Variable[j]
          lev                           <- temp$Level[j]
          lev0                          <- lev + 1
          varas                         <- paste(var,"_as",sep="")
          temp0                         <- use_spec[(use_spec$Next_Level_Down == var) &
                                                    (use_spec$Orig_Variable   != var) &
                                                    (use_spec$Level           == lev0),]
          rn                            <- nrow(temp0)
          if ((rn > 0) & (!(varas %in% nmx))) { 
            if (sum(temp0$asdone) == rn) {
              x[,varas]                 <- 0
              kf                        <- 0
              for (k in 1:rn) {
                var0                    <- temp0$Orig_Variable[k]
                prewgt0                 <- temp0$PreWeight[k]
                varas0                  <- paste(var0,"_as",sep="")
                if (varas0 %in% nmx) {
                  kf                    <- 1
                  prewgt0               <- 1
                } else {
                  varas0                <- var0
                }
                x[,varas]               <- x[,varas] + (x[,varas0] * prewgt0)
              }
              if (kf == 0) x[,varas]    <- NULL
              if (kf == 1) nmx          <- c(nmx,varas)
              use_spec[(use_spec$Orig_Variable == var) & 
                       (use_spec$Level == lev),
                       "asdone"]        <- 1
            }
          }
        }
      }
    }
  }
  
  mt1                                   <- Sys.time() + timelag
  if (megaprint)                           cat("Transforms Step  7 \n")
  if (print)                               cat("Transform the Dimishing Returns \n")
  
  ### Calculate the diminishing returns transform of variables where a standard
  ###    transform is given. Then the diminishing return is further transformed
  ###    to "match" the adstock so the bayesian priors will be the same regardless
  ###    of any change in transform.   Also this keeps the variables up and down
  ###    the level branches to somewhat sum up the same, so when weights are used
  ###    and adjusted they change things as little as possible.
  
  use_spec$DR_Adj_Plus                  <- 0
  use_spec$DR_Adj_Times                 <- 1
  use_spec$DR_Var                       <- "None"
  use_spec$DR_Lev                       <- 0
  use_spec$Model_Var                    <- "None"
  
  temp                                  <- c("NONE","DRUP","DRDOWN","WEIGHT")
  temp                                  <- use_spec[(!(toupper(use_spec$Dim_Returns) %in% temp)) & 
                                                      (use_spec$Transform == "Y"),]
  tn                                    <- nrow(temp)
  
  if (tn > 0) {
    for (i in 1:tn) {
      dmrt                              <- toupper(temp$Dim_Returns[i])
      var                               <- temp$Orig_Variable[i]
      if (wflag == 0) {
        lev                             <- 1
        prewgt                          <- 1
      } else {
        lev                             <- temp$Level[i]
        prewgt                          <- temp$PreWeight[i]
      }
      varas                             <- paste(var,"_as",sep="")
      if (!(varas %in% nmx))   varas    <- var
      vardr                             <- paste(var,"_uw",sep="")
      if (wflag == 0) vardr             <- temp$Trans_Variable[i]
      if ((  var   %in% nmx)   & 
          (!(vardr %in% nmx))) {
        type                            <- unlist(strsplit(dmrt, "_"))
        data_vector                     <- x[[varas]]
        if (varas != var) data_vector   <- data_vector / prewgt
        data_vector_as                  <- data_vector * prewgt
        for(j in 1:length(type)) {
          data_vector_transform         <- list()
          if (type[j] == "ADR"){
            data_vector_transform       <- adr(data_vector, 
                                               fit_curves, 
                                               c(temp$Effective[i],
                                                 temp$Recency[i], 
                                                 temp$Period[i], 
                                                 temp$Decay[i]))
          }
          if (type[j] == "ABC"){
            data_vector_transform       <- abcNew(data_vector, 1, temp$B[i], temp$C[i])
          }
          if (type[j] == "ATAN") {
            data_vector_transform       <- atan(data_vector * temp$Scale[i])
          }
          if (type[j] == "LOG") {
            data_vector_transform       <- log(data_vector * temp$Scale[i] + 1)
          }
          if (type[j] == "POLY") {
            data_vector_transform       <- myPoly(data_vector, temp$Alpha[i])
          }
          
          if (type[j] == "POWER") {
            data_vector_transform       <- data_vector ^ temp$Power[i]
          }
          data_vector <- data_vector_transform
        }
        data_vector                     <- data_vector * prewgt
        sdas                            <- sd(data_vector_as)
        sddr                            <- sd(data_vector)
        mnas                            <- mean(data_vector_as)
        mndr                            <- mean(data_vector)
        mndr2                           <- mean(data_vector    * data_vector)
        mnad                            <- mean(data_vector_as * data_vector)
        adja                            <- -1
        mndr1                           <- mndr2 - (mndr * mndr)
        mnad1                           <- mnad  - (mnas * mndr)
        tst                             <- 0
        if ((mnad1 != 0) &
            (mnad1 != 0))   adja        <- mnad1 / mndr1
        if (adja <= 0) {
          adja                          <- 1
          if ((sdas > 0) & 
              (sddr > 0))   adja        <- sdas / sddr
        }
        adjb                            <- mnas - (adja * mndr)
        data_vector                     <- (data_vector * adja) + adjb
        x[,vardr]                       <- data_vector
        nmx                             <- c(nmx,vardr)
        use_spec[use_spec$Orig_Variable     == var,
                 "DR_Adj_Times"]        <- adja
        use_spec[use_spec$Orig_Variable     == var,
                 "DR_Adj_Plus"]         <- adjb
        use_spec[use_spec$Orig_Variable     == var,
                 "drdone"]              <- 1
        use_spec[use_spec$Orig_Variable     == var,
                 "DR_Var"]              <- var
        use_spec[use_spec$Orig_Variable     == var,
                 "DR_Lev"]              <- lev
        use_spec[use_spec$Orig_Variable     == var,
                 "drdone"]              <- 1
        if (lev < mlev) {
          lev0                          <- mlev - lev
          temp0                         <- c(var)
          for (k in 1:lev0) {
            lev1                        <- lev + k
            temp0                       <- use_spec[(use_spec$Next_Level_Down %in% temp0) &
                                                    (use_spec$Level == lev1),"Orig_Variable"]
            temp0                       <- temp0$Orig_Variable
            use_spec[(use_spec$Orig_Variable %in% temp0) & 
                     (use_spec$Level == lev1),
                     "DR_Var"]          <- var
            use_spec[(use_spec$Orig_Variable %in% temp0) & 
                     (use_spec$Level == lev1),
                     "DR_Lev"]          <- lev
          }
        }
        if (lev > 1) {
          lev0                          <- lev - 1
          var0                          <- var
          tnn                           <- 0
          for (k in 1:lev0) {
            lev1                        <- lev - k
            lev2                        <- lev1 + 1
            var0                        <- use_spec[(use_spec$Orig_Variable == var0) &
                                                    (use_spec$Level == lev2),"Next_Level_Down"]
            var0                        <- var0$Next_Level_Down[[1]]
            temp0                       <- use_spec[use_spec$Next_Level_Down == var0,"Orig_Variable"]
            tn                          <- nrow(temp0)
            if ((tn > 1) | (tnn == 1)) {
              use_spec[(use_spec$Orig_Variable == var0) & 
                       (use_spec$Level == lev1),
                       "DR_Var"]        <- "Multi"
              tnn                       <- 1
            } else {
              use_spec[(use_spec$Orig_Variable %in% temp0) & 
                       (use_spec$Level == lev1),
                       "DR_Var"]        <- var
              use_spec[(use_spec$Orig_Variable %in% temp0) & 
                       (use_spec$Level == lev1),
                       "DR_Lev"]        <- lev            
            }
          }
        }
      }
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step  8 \n")
  if (nextprint)             mt1        <- mt2 + timelag

  temp                                  <- use_spec[(use_spec$Level == elev) & 
                                                    (use_spec$Include == 1),]
  tn                                    <- nrow(temp)
  
  if ((tn > 0) & (wflag == 1)) {
    for (i in 1:tn) {
      var                               <- temp$Orig_Variable[i]
      lev                               <- elev
      use_spec[(use_spec$Orig_Variable == var) & 
               (use_spec$Level == elev),
               "Model_Var"]             <- var
      if (lev < mlev) {
        lev0                            <- mlev - lev
        temp0                           <- c(var)
        for (k in 1:lev0) {
          lev1                          <- lev + k
          temp0                         <- use_spec[(use_spec$Next_Level_Down %in% temp0) &
                                                    (use_spec$Level == lev1),"Orig_Variable"]
          temp0                         <- temp0$Orig_Variable
          use_spec[(use_spec$Orig_Variable %in% temp0) & 
                   (use_spec$Level == lev1),
                   "Model_Var"]         <- var
        }
      }
      if (lev > 1) {
        lev0                            <- lev - 1
        var0                            <- var
        tnn                             <- 0
        for (k in 1:lev0) {
          lev1                          <- lev - k
          lev2                          <- lev1 + 1
          var0                          <- use_spec[(use_spec$Orig_Variable == var0) &
                                                    (use_spec$Level == lev2),"Next_Level_Down"]
          var0                          <- var0$Next_Level_Down[[1]]
          temp0                         <- use_spec[use_spec$Next_Level_Down == var0,"Orig_Variable"]
          tn                            <- nrow(temp0)
          if ((tn > 1) | (tnn == 1)) {
            use_spec[(use_spec$Orig_Variable == var0) & 
                     (use_spec$Level == lev1),
                     "Model_Var"]       <- "Multi"
            tnn                         <- 1
          } else {
            use_spec[(use_spec$Orig_Variable %in% temp0) & 
                    (use_spec$Level == lev1),
                     "Model_Var"]       <- var
          }
        }
      }
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step  9 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### We finish the dim. returns calculation in two steps.
  ### The first is to sum up the more detailed variables at higher levels to
  ###    create the more broad variables at the lower levels. But again we
  ###    stabilize it by both mean and standard deviation to stabilize the whole
  ###    branch of data.  We also work to make sure the data continues to follow
  ###    the basic ABC S-curve as much as possible.
  
  if (wflag == 1) {
    for (i in 1:mlev) {
      temp                              <- use_spec[(use_spec$drdone==0) & 
                                                    (toupper(use_spec$Dim_Returns) == "DRUP"),]
      qn                                <- nrow(temp)
      if (qn > 0) {
        for (j in 1:qn) {
          var                           <- temp$Orig_Variable[j]
          lev                           <- temp$Level[j]
          prewgt                        <- temp$PreWeight[j]
          lev0                          <- lev + 1
          varas                         <- paste(var,"_as",sep="")
          if (!(varas %in% nmx))  varas <- var
          vardr                         <- paste(var,"_uw",sep="")        
          temp0                         <- use_spec[(use_spec$Next_Level_Down == var) &
                                                    (use_spec$Level           == lev0),]
          rn                            <- nrow(temp0)
          if ((rn > 0) & 
              (!(vardr %in% nmx))) { 
            if (sum(temp0$drdone) == rn) {
              x[,vardr]                 <- 0
              adjb0                     <- 0
              kf                        <- 0
              for (k in 1:rn) {
                var0                    <- temp0$Orig_Variable[k]
                prewgt0                 <- temp0$PreWeight[k]
                adjb                    <- temp0$DR_Adj_Plus[k]
                varas0                  <- paste(var0,"_as",sep="")
                if (!(varas0 %in% nmx))
                  varas0    <- var0
                vardr0                  <- paste(var0,"_uw",sep="")
                if (vardr0 %in% nmx) {
                  kf                    <- 1
                } else {
                  vardr0                <- varas0
                }
                if (var0 != varas0)
                  prewgt0               <- 1
                if (var0 != vardr0) 
                  prewgt0               <- 1
                x[,vardr]               <- x[,vardr] + (x[,vardr0] * prewgt0)
                adjb0                   <- adjb0 + adjb
              }
              if (kf == 0) {
                x[,vardr]               <- NULL
              } else {
                x[,vardr]               <- x[,vardr] - adjb0
                data_vector_as          <- x[,varas] * prewgt
                data_vector             <- x[,vardr]
                sdas                    <- sd(data_vector_as)
                sddr                    <- sd(data_vector)
                mnas                    <- mean(data_vector_as)
                mndr                    <- mean(data_vector)
                mndr2                   <- mean(data_vector    * data_vector)
                mnad                    <- mean(data_vector_as * data_vector)
                adja                    <- -1
                mndr1                   <- mndr2 - (mndr * mndr)
                mnad1                   <- mnad  - (mnas * mndr)
                tst                     <- 0
                if ((mnad1 != 0) &
                    (mndr1 != 0)) adja  <- mnad1 / mndr1
                if (adja <= 0) {
                  adja                  <- 1
                  if ((sdas > 0) & 
                      (sddr > 0)) adja  <- sdas / sddr
                }
                adjb                    <- mnas - (adja * mndr)
                data_vector             <- (data_vector * adja) + adjb
                x[,vardr]               <- data_vector
                nmx                     <- c(nmx,vardr)
                use_spec[use_spec$Orig_Variable   == var,
                   "DR_Adj_Times"]      <- adja
                use_spec[use_spec$Orig_Variable   == var,
                   "DR_Adj_Plus"]       <- adjb
              }
              use_spec[use_spec$Orig_Variable == var,
                       "drdone"]        <- 1
            }
          }
        }
      }
    }
  }  
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step 10 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### We finish off the diminishing returns by subdividing variables at a detailed
  ###    level to create variables at a more detailed (higher) level.  We use two
  ###    methods to do this (which method you use is a business choice not a
  ###    statistical choice - based on how you view dim returns and its causes).
  ###    The first is to subdivide each time period equally (so the are always
  ###    at the same point of the S curve).  For example small amount in one
  ###    subgroup and a large amount of another will get diminished the same if
  ###    they are at the same time.
  ### The second method is to make every more detailed subdivision follow the
  ###    same dim. returns curve as the high level as much as possible while 
  ###    still letting the subdivisions add up to the total.  For example, a  
  ###    a small amount in one subgroup will be diminished less and a large
  ###    amount in another subgroup will be diminished more.  Again this is only
  ###    as much as possible while still having them sum up to the higher level.
  
  substep                               <- 0
  if (wflag == 1) {
    xn                                  <- nrow(x)
    temp6                               <- data.frame(num=1:xn)
    nmt6                                <- c("num")
    for (i in 1:mlev) {
      substep                           <- substep + 1
      mt2                               <- Sys.time()
      nextprint                         <- FALSE
      if (megaprint)           
        nextprint                       <- TRUE
      if (print & (mt2 > mt1)) 
        nextprint                       <- TRUE
      if (nextprint)                       cat("Transforms Step 11 substep",substep,"\n")
      if (nextprint)             mt1    <- mt2 + timelag    
      temp                              <- use_spec[(use_spec$drdone==0) &
                                                    (toupper(use_spec$Dim_Returns) == "DRDOWN"),]
      qn                                <- nrow(temp)
      if (qn > 0) {
        temp0                           <- temp[,c("Next_Level_Down","Level")]
        temp0$Level                     <- temp0$Level - 1
        names(temp0)                    <- c("Orig_Variable","Level")
        temp0                           <- unique(temp0)
        temp1                           <- use_spec[,c("Orig_Variable","Level","PreWeight","drdone","DR_Var","DR_Lev")]
        temp0                           <- left_join(temp0,temp1,by=c("Orig_Variable","Level"))
        temp0                           <- temp0[temp0$drdone==1,]
        pn                              <- nrow(temp0)
        for (j in 1:pn) {
          var                           <- temp0$Orig_Variable[j]
          lev                           <- temp0$Level[j]
          prewgt                        <- temp0$PreWeight[j]
          vare                          <- temp0$DR_Var[j]
          leve                          <- temp0$DR_Lev[j]
          lev0                          <- lev + 1
          varas                         <- paste(var,"_as",sep="")
          if (!(varas %in% nmx)) varas  <- var
          vardr                         <- paste(var,"_uw",sep="")
          if (!(vardr %in% nmx)) vardr  <- varas
          vareas                        <- paste(vare,"_as",sep="")
          if (!(vareas %in% nmx)) 
            vareas                      <- vare
          varedr                        <- paste(vare,"_uw",sep="")
          if (!(varedr %in% nmx)) 
            varedr                      <- vareas
          prewgt1                       <- use_spec[(use_spec$Orig_Variable == vare) &
                                                    (use_spec$Level == leve),
                                                     "PreWeight"]
          temp1                         <- use_spec[(use_spec$Next_Level_Down == var) &
                                                    (use_spec$Level == lev0),]
          rn                            <- nrow(temp1)
          if (rn > 0) {
            temp2                       <- data.frame(x[,varas],x[,vardr])
            names(temp2)                <- c("base","basedr")
            if (varas == var)
              temp2$base                <- temp2$base   * prewgt
            if (vardr == var)
              temp2$basedr              <- temp2$basedr * prewgt
            subs                        <- paste("sub",1:rn,sep="")
            subdrs                      <- paste("subdr",1:rn,sep="")
            temp2$sub1                  <- temp2$base
            temp2$subdr1                <- temp2$basedr
            if (vardr %in% nmx)     rn  <- 0
          }  
          if (rn > 1) {
            minas                       <- min(temp2$base)
            mindr                       <- min(temp2$basedr)
            temp2$base                  <- temp2$base   - minas
            temp2$basedr                <- temp2$basedr - mindr
            for (k in 1:rn) {
              var0                      <- temp1$Orig_Variable[k]
              prewgt0                   <- temp1$PreWeight[k]
              varas0                    <- paste(var0,"_as",sep="")
              if (!(varas0 %in% nmx)) 
                varas0  <- var0
              sub                       <- subs[k]
              temp2[,sub]               <- x[,varas0]
              if (varas0 == var0)
                temp2[,sub]             <- temp2[,sub] * prewgt0
              minsb                     <- min(temp2[,sub])
              temp2[,sub]               <- temp2[,sub] - minsb
              sumsb                     <- sum(temp2[,sub])
              if (k == 1) {
                minsbs                  <- minsb
                minsdrs                 <- sumsb 
                sumdr                   <- sumsb 
              } else {
                minsbs                  <- c(minsbs,minsb)
                minsdrs                 <- c(minsdrs,sumsb)
                subdr                   <- sumdr + sumsb
              }
            }
            if (sumdr == 0) {
              sumdr                     <- rn
              minsdrs                   <- rep(1,rn)
            }
            minsdrs                     <- minsdrs * mindr / sumdr
            if (MultiLevelDR) {
              if (!(vareas %in% nmt6)) {
                temp3                   <- x[,c(vareas,varedr)]
                minas                   <- min(temp3[,vareas])
                mindr                   <- min(temp3[,varedr])
                temp3[,vareas]          <- temp3[,vareas] - minas
                temp3[,varedr]          <- temp3[,varedr] - mindr
                temp3                   <- temp3[order(temp3[,vareas]),]
                temp3                   <- unique(temp3)
                temp3                   <- data.frame(aggregate(temp3[,varedr],by=list(temp3[,vareas]),FUN=mean))
                names(temp3)            <- c(vareas,varedr)
                t3n                     <- nrow(temp3)
                temp6[temp6$num <= t3n,
                      vareas]           <- temp3[,vareas]
                temp6[temp6$num <= t3n,
                      varedr]           <- temp3[,varedr]
                if (t3n < xn) {
                  temp6[temp6$num > t3n,
                        vareas]         <- temp3[t3n,vareas]
                  temp6[temp6$num > t3n,
                        varedr]         <- temp3[t3n,varedr]
                }
                nmt6                    <- c(nmt6,vareas,varedr)                     
              }
              maxas                     <- max(temp6[,vareas])
              num                       <- min(temp6[temp6[,vareas]>=maxas,"num"])
              temp3                     <- temp6[1:num,c(vareas,varedr)]
              names(temp3)              <- c("base","basedr")
              temp4                     <- data.frame(sub = temp2$sub1)
              temp4$link                <- paste("1@",1:xn,sep="")
              for (k in 2:rn) {
                sub                     <- subs[k]
                temp5                   <- data.frame(sub=temp2[,sub],link=paste(k,"@",1:xn,sep=""))
                temp4                   <- rbind(temp4,temp5)
              } 
              temp4                     <- temp4[order(temp4$sub),]
              maxas0                    <- max(temp4$sub)
              if ((maxas > 0)   & 
                  (maxas0 > 0)) {
                temp3$base              <- temp3$base   * maxas0 / maxas
                temp3$basedr            <- temp3$basedr * maxas0 / maxas
              }
              tn                        <- nrow(temp3)
              temp4$base1               <- 0
              temp4$basedr1             <- 0
              temp4$base2               <- 0
              temp4$basedr2             <- 0
              temp4$subdr               <- 0
              for (k in 1:tn) {
                k0                      <- tn - k + 1
                base                    <- temp3$base[k]
                basedr                  <- temp3$basedr[k]
                temp4[(temp4$sub <= base) & 
                      (temp4$base2   == 0),
                      "base2"]          <- base
                temp4[(temp4$sub <= base) & 
                      (temp4$basedr2 == 0),
                      "basedr2"]        <- basedr
                base                    <- temp3$base[k0]
                basedr                  <- temp3$basedr[k0]
                temp4[(temp4$sub >= base) & 
                      (temp4$base1   == 0),
                      "base1"]          <- base
                temp4[(temp4$sub >= base) & 
                      (temp4$basedr1 == 0),
                      "basedr1"]        <- basedr
              }
              temp4[temp4$base2 <= temp4$base1,
                    "base2"]            <- temp4[temp4$base2   <= temp4$base1,"base1"] + 1
              temp4$subdr               <-       temp4$basedr1                         +
                                               ((temp4$sub      - temp4$base1)         * 
                                                (temp4$basedr2  - temp4$basedr1)       /
                                                (temp4$base2    - temp4$base1))
              temp4$base1               <- NULL
              temp4$basedr1             <- NULL
              temp4$base2               <- NULL
              temp4$basedr2             <- NULL
              
              for (k in 1:rn) {
                temp2$link              <- paste(k,"@",1:xn,sep="")
                subdr                   <- subdrs[k]
                temp2                   <- left_join(temp2,temp4[,c("subdr","link")],by="link")
                temp2[,subdr]           <- temp2$subdr
                temp2$subdr             <- NULL
                temp2$link              <- NULL
              } 
            } else {
              temp2$adj                 <- 0
              temp2[temp2$base > 0,
                    "adj"]              <- temp2[temp2$base > 0,"basedr"] /
                                           temp2[temp2$base > 0,"base"]
              for (k in 1:rn) {
                subdr                   <- subdrs[k]
                sub                     <- subs[k]
                temp2[,subdr]           <- temp2[,sub] * temp2$adj
              }
              temp2$adj                 <- NULL
            }
            temp2$adj                   <- 0
            temp2$subdr                 <- 0
            for (k in 1:rn) {
              subdr                     <- subdrs[k]
              temp2$subdr               <- temp2$subdr + temp2[,subdr]
            }
            temp2[temp2$subdr > 0,
                  "adj"]                <- temp2[temp2$subdr > 0,"basedr"] /
                                           temp2[temp2$subdr > 0,"subdr"]
            for (k in 1:rn) {
              subdr                     <- subdrs[k]
              temp2[,subdr]             <- temp2[,subdr] * temp2$adj
            }
            temp2$base                  <- temp2$base   + minas
            temp2$basedr                <- temp2$basedr + mindr
            for (k in 1:rn) {
              sub                       <- subs[k]
              subdr                     <- subdrs[k]
              minsb                     <- minsbs[k]
              minsdr                    <- minsdrs[k]
              temp2[,sub]               <- temp2[,sub]   + minsb
              temp2[,subdr]             <- temp2[,subdr] + minsdr
            }
          }
          if (rn > 0) {
            for (k in 1:rn) {
              var0                      <- temp1$Orig_Variable[k]
              sub                       <- subs[k]
              vardr0                    <- paste(var0,"_uw",sep="")
              subdr                     <- subdrs[k]
              sdas                      <- sd(  temp2[,sub])
              sddr                      <- sd(  temp2[,subdr])
              mnas                      <- mean(temp2[,sub])
              mndr                      <- mean(temp2[,subdr])
              mndr2                     <- mean(temp2[,subdr] * temp2[,subdr])
              mnad                      <- mean(temp2[,subdr] * temp2[,sub])
              adja                      <- -1
              mndr1                     <- mndr2 - (mndr * mndr)
              mnad1                     <- mnad  - (mnas * mndr)
              if ((mndr1 != 0) &
                  (mnad1 != 0))    adja <- mnad1 / mndr1
              if (adja <= 0) {
                adja                    <- 1
                if ((sdas > 0) & 
                    (sddr > 0))    adja <- sdas / sddr
              }
              adjb                      <- mnas - (adja * mndr)
              temp2[,subdr]             <- (temp2[,subdr] * adja) + adjb
              x[,vardr0]                <- temp2[,subdr]
              nmx                       <- unique(c(nmx,vardr0))
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == lev0),
                       "DR_Adj_Times"]  <- adja
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == lev0),
                       "DR_Adj_Plus"]   <- adjb
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == lev0),
                       "drdone"]        <- 1
            }
          }
        }
      }
    }  
  }
  
  mt1                                   <- Sys.time() + timelag
  if (nextprint)                           cat("Transforms Step 12 \n")
  if (print)                               cat("Stabilize the transforms over all the levels \n")
  
  ### The next two steps are designed to "stabilize" the variables in the branch
  ###    structure.  This means to make sure the modeling variables have the same
  ###    mean and standard deviation regardless of diminishing returns or weights. 
  ###    The result is the Bayesian priors will remain the same regardless of what
  ###    weights are created by the optimization function.  It also helps by
  ###    making sure changes that are made to transforms will also not effect the
  ###    Bayesian priors).  In addition the variables above a variable in the
  ###    branch add up exactly to the below variable they branch out of (so the
  ###    model coefficients produce the same results regardless what level they
  ###    are applied to)
  ### Most of this work was done in the previous steps so the model level
  ###    variables are perfectly stabilized (before the weights are applied) and
  ###    the other variables are close to stabilized.  These next two steps just
  ###    finish the process by guaranteeing that the variables above and below
  ###    the model level agree with model level variables and also finalize the
  ###    the diminishing returns adjustment values.
  ### In this first part we do the easy task of stabilizing variables below the 
  ###    model level.
  
  if ((wflag == 1) & (elev > 1)) {
    elev0                               <- elev - 1
    for (i in 1:elev0) {
      i0                                <- elev - i
      i1                                <- i0 + 1
      temp                              <- use_spec[use_spec$Level==i0,]
      qn                                <- nrow(temp)
      if (qn > 0) {
        for (j in 1:qn) {
          var                           <- temp$Orig_Variable[j]
          prewgt                        <- temp$PreWeight[j]
          adjb0                         <- temp$DR_Adj_Plus[j]
          varas                         <- paste(var,"_as",sep="")
          if (!(varas %in% nmx))  varas <- var
          vardr                         <- paste(var,"_uw",sep="")
          if (!(vardr %in% nmx))  vardr <- varas
          temp0                         <- use_spec[(use_spec$Next_Level_Down == var) &
                                                    (use_spec$Level           == i1),]
          rn                            <- nrow(temp0)
          if (rn > 0) {
            temp2                       <- data.frame(x[,varas],x[,vardr])
            names(temp2)                <- c("base","basedr")
            if (var == varas) 
              temp2$base                <- temp2$base   * prewgt
            
            if (var == vardr) 
              temp2$basedr              <- temp2$basedr * prewgt
            subs                        <- paste("sub",1:rn,sep="")
            subdrs                      <- paste("subdr",1:rn,sep="")
            adjbs                       <- rep(0,rn)
            adjbb                       <- use_spec[(use_spec$Orig_Variable == var) &
                                                    (use_spec$Level == i0),"DR_Adj_Plus"]
            adjbb                       <- adjbb$DR_Adj_Plus[1]
            temp2$sub0                  <- 0
            temp2$subdr0                <- 0
            adjb0                       <- 0
            for (k in 1:rn) {
              var0                      <- temp0$Orig_Variable[k]
              prewgt0                   <- temp0$PreWeight[k]
              varas0                    <- paste(var0,"_as",sep="")
              if (!(varas0 %in% nmx))  
                varas0   <- var0
              vardr0                    <- paste(var0,"_uw",sep="")
              if (!(vardr0 %in% nmx)) 
                vardr0   <- varas0
              sub                       <- subs[k]
              subdr                     <- subdrs[k]
              temp2[,sub]               <- x[,varas0]
              temp2[,subdr]             <- x[,vardr0]
              if (varas0 == var0)
                temp2[,sub]             <- temp2[,sub]   * prewgt0
              if (vardr0 == var0)
                temp2[,subdr]           <- temp2[,subdr] * prewgt0
              temp2$sub0                <- temp2$sub0   + temp2[,sub]
              temp2$subdr0              <- temp2$subdr0 + temp2[,subdr]
              adjb                      <- use_spec[(use_spec$Orig_Variable == var0) &
                                                    (use_spec$Level == i1),"DR_Adj_Plus"]
              adjb                      <- adjb$DR_Adj_Plus[1]
              adjbs[k]                  <- adjb
              adjb0                     <- adjb0 + adjb
            }
            if ((vardr != varas) & (vardr %in% nmx)) {
              x[,vardr]                 <- temp2$subdr0
              use_spec[(use_spec$Orig_Variable   == var) &
                       (use_spec$Level           == i0),
                       "DR_Adj_Times"]  <- 1
              use_spec[(use_spec$Orig_Variable   == var) &
                       (use_spec$Level           == i0),
                       "DR_Adj_Plus"]   <- adjb0
            }
          }
        }
      }  
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step 13 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  
  ### In this second part we do the much harder task of stabilizing variable above 
  ###    the model level.  It is more difficult because we need to ensure that
  ###    the subdivide the lower level variables in ways that best fit the upper
  ###    level (more detailed) variable.   It is also hard since we want to
  ###    preserve the "ABC" S-curve shape of the Diminishing Returns as much as
  ###    possible.
  
  if ((wflag == 1) & (elev < mlev)) {
    elev0       <- mlev - elev
    for (i in 1:elev0) {
      i0                                <- elev + i - 1
      i1                                <- i0 + 1
      temp                              <- use_spec[use_spec$Level==i0,]
      qn                                <- nrow(temp)
      if (qn > 0) {
        for (j in 1:qn) {
          var                           <- temp$Orig_Variable[j]
          prewgt                        <- temp$PreWeight[j]
          adjbb                         <- temp$DR_Adj_Plus[j]
          varas                         <- paste(var,"_as",sep="")
          if (!(varas %in% nmx))  varas <- var
          vardr                         <- paste(var,"_uw",sep="")
          if (!(vardr %in% nmx))  vardr <- varas
          temp0                         <- use_spec[(use_spec$Next_Level_Down == var) &
                                                    (use_spec$Level           == i1),]
          rn                            <- nrow(temp0)
          if (rn > 0) {
            temp2                       <- data.frame(x[,varas],x[,vardr])
            names(temp2)                <- c("base","basedr")
            if (varas == var)
              temp2$base                <- temp2$base   * prewgt
            if (vardr == var)
              temp2$basedr              <- temp2$basedr * prewgt
            temp2$basedr                <- temp2$basedr - adjbb
            subs                        <- paste("sub",1:rn,sep="")
            subdrs                      <- paste("subdr",1:rn,sep="")
            adjbs                       <- rep(0,rn)
            mindrs                      <- rep(0,rn)
            temp2$sub0                  <- 0
            temp2$subdr0                <- 0
            adjb0                       <- 0
            for (k in 1:rn) {
              var0                      <- temp0$Orig_Variable[k]
              prewgt0                   <- temp0$PreWeight[k]
              adjb                      <- temp0$DR_Adj_Plus[k]
              varas0                    <- paste(var0,"_as",sep="")
              if (!(varas0 %in% nmx)) 
                 varas0                 <- var0
              vardr0                    <- paste(var0,"_uw",sep="")
              if (!(vardr0 %in% nmx)) 
                vardr0                  <- varas0
              sub                       <- subs[k]
              subdr                     <- subdrs[k]
              temp2[,sub]               <- x[,varas0]
              temp2[,subdr]             <- x[,vardr0]   - adjb
              if (varas0 == var0)
                temp2[,sub]             <- temp2[,sub]   * prewgt0
              if (vardr0 == var0)
                temp2[,subdr]           <- temp2[,subdr] * prewgt0
              temp2[,subdr]             <- temp2[,subdr] + adjb
              temp2$sub0                <- temp2$sub0    + temp2[,sub]
              temp2$subdr0              <- temp2$subdr0  + temp2[,subdr]
            }
            temp2$adj                   <- 0
            temp2[(temp2$basedr != 0) & 
                  (temp2$subdr0 != 0),
                  "adj"]                <- temp2[(temp2$basedr        != 0) & 
                                                 (temp2$subdr0        != 0),"basedr"] / 
                                                  temp2[(temp2$basedr != 0) & 
                                                 (temp2$subdr0        != 0),"subdr0"]
            for (k in 1:rn) {
              sub                       <- subs[k]
              subdr                     <- subdrs[k]
              temp2[,subdr]             <- temp2[,subdr] * temp2$adj
              mnas                      <- mean(temp2[,sub])
              mndr                      <- mean(temp2[,subdr])
              mindrs[k]                 <- sum(abs(temp2[,subdr]))
              adjb                      <- mnas - mndr
              adjbs[k]                  <- adjb
              adjb0                     <- adjb0 + adjb
            }
            if ((adjbb != 0) & 
                (adjb0 != 0) & 
                (adjb0 * adjbb > 0)) {
              adjbs                     <- adjbs * adjbb / adjb0
            } else {
              adjb1                     <- sum(mindrs)
              if (adjb1 > 0) {
                adjbs                   <- mindrs * adjbb / adjb1
              } else {
                adjbs                   <- adjbs * 0
              }
            }
          }
          if (rn > 0) {
            for (k in 1:rn) {
              var0                      <- temp0$Orig_Variable[k]
              vardr0                    <- paste(var0,"_uw",sep="")
              subdr                     <- subdrs[k]
              adjb                      <- adjbs[k]
              x[,vardr0]                <- temp2[,subdr] + adjb
              if (!(vardr0 %in% nmx)) {
                nmx                     <- c(nmx,vardr0)
              }
              
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == i1),
                       "DR_Adj_Times"]  <- 1
              use_spec[(use_spec$Orig_Variable   == var0) &
                       (use_spec$Level           == i1),
                       "DR_Adj_Plus"]   <- adjb
            }
          }
        }
      }
    }  
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Transforms Step 14 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Store the work so far and run the function to distribute the weights over
  ### all levels of the spec file and the transforms in the data file
  
  un                                    <- nrow(use_spec)
  full_spec$Model_Var                   <- "None"
  full_spec$Var_Adj_Times               <- 1
  full_spec$Var_Adj_Plus                <- 0
  full_spec$Centered_Weight             <- 1
  
  done                                  <- c("Intercept")
  
  for (i in 1:un) {
    var                                 <- use_spec$Orig_Variable[i]
    vart                                <- use_spec$Trans_Variable[i]
    varm                                <- use_spec$Model_Var[i]
    lev                                 <- use_spec$Level[i]
    prewgt                              <- use_spec$PreWeight[i]
    type                                <- use_spec$Variable_Type[i]
    adja0                               <- use_spec$DR_Adj_Times[i]
    adjb0                               <- use_spec$DR_Adj_Plus[i]
    varm                                <- use_spec$Model_Var[i]
    varas                               <- paste(var,"_as",sep="")
    if (!(varas %in% nmx))       varas  <- var
    vardr                               <- paste(var,"_uw",sep="")
    if (wflag == 0)              vardr  <- vart
    if (!(vardr %in% nmx))       vardr  <- varas
    if (!(vart  %in% nmx))       vart   <- vardr
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "Model_Var"]              <- varm
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "DR_Adj_Times"]           <- adja0
    full_spec[(full_spec$Orig_Variable == var) &
              (full_spec$Level         == lev),
              "DR_Adj_Plus"]            <- adjb0
  }
  
  Full_Model_Spec                       <- full_spec
  Model_Spec                            <- Full_Model_Spec %>% filter(Include == 1)
  obj$data                              <- x
  obj$spec                              <- Model_Spec
  obj$full_spec                         <- full_spec
  
  if (megaprint)                           cat("Transforms Step 15 \n")
  return(obj)
}


Measures_of_Extreme_Transformations     <- function(obj,
                                                    print           = FALSE,
                                                    megaprint       = FALSE,
                                                    timelag         = 10) {
  
### This function has 4 inputs:
###    obj               --- A list with 2 elements
###                             data      = is the model input data
###                             full_spec = The spec file
###    megaprint = TRUE  --- prints everything in the R Log.  Just a continuous
###                             record of where the run is and what is happening
###    print     = TRUE  --- Just a limited print of where things are in the run
###                             (just enough to see what is happening)
###                             Also it activates the "timelag" timing see below.
###    timelag   = # sec --- A number in seconds.  If you set timelag = 10 then
###                             very 10 seconds or so they show you where the
###                             run is at that time
###  
### In this function we produce "informational" values to put in the spec file
###    for reference.  They simply help you understand your transforms better.
### The first is AS_Extreme is an indicator of how extreme your adstock is for
###    each variable.  This is number from 0 to 1.  Any number close to 0 is 
###    not at all extreme where the adstock is very similar to the original
###    variable.  Any number close to 1 is an extreme adstock and is close to a
###    straight.  Any number from 0 to .7 is likely fine and just what you think
###    is best.  But if AS_Extreme is greater then .7 then likely you should
###    try using a better variable or adstock formula to begin with.
### The second is DR_Extreme is an indicator of how extreme your diminishing
###    returns formula is.  Also a number from 0 to 1.  Any number < .1 is
###    close to no diminishing returns at all and likely is unrealistic (but
###    there are exceptions).  Any number > .7 is an extreme diminishing
###    returns (with most of the data past the point where you get very little
###    effect from increasing spending).  You should avoid any DR_Extreme
###    greater then .7.
### The next is the average diminishing return which is defined as the average
###    slope of the diminishing returns curve over every line of data (compared)
###    to the average slope over the first 10% of the lines of data.  So it will
###    be a percent mostly between 0% and 100%. But if the diminishing returns
###    follows an "S" curve it might be above 100%, and since the data at some
###    levels might be "noisy" the curve might follow unusual shapes.  This is
###    another measure of a "extreme" diminishing returns. But while all our
###    "Extreme" measures are between 0% and 100%, the DR_Extreme measure has
###    0% being not extreme (a straight line) while 100% is very extreme.  But 
###    with this Average_Slope" measure, 0% is very extreme and 100% is not at
###    all extreme (a straight line).
### The last measure is the top diminishing returns which is defined as the 
###    average slope in the top 10% of lines of data over the average slope
###    for the bottom 10% of lines. It is also between 0% and 100% and is also
###    a measure of extremity (but 0% is very extreme and 100% is not at all
###    extreme).

  mt1                                   <- Sys.time() + timelag
  done                                  <- c("Intercept")
  use_spec                              <- obj$full_spec
  use_spec                              <- use_spec[use_spec$Include > 0,]
  full_spec                             <- obj$full_spec
  x                                     <- obj$data
  
  nmx                                   <- names(x)
  xn                                    <- nrow(x)
  temp                                  <- use_spec[(use_spec$Variable_Type != "Dependent") &
                                                    (use_spec$Orig_Variable != "Intercept"),]
  tnn                                   <- nrow(temp)
  tnm                                   <- tnn / 20
  if (tnm < 40)             tnm         <- 40
  sq2                                   <- sqrt(2)
  
  use_spec$Adstock_Extremity            <- 0
  use_spec$Dim_Ret_Extremity            <- 0
  use_spec$Dim_Ret_Avg_Slope            <- 1
  use_spec$Dim_Ret_End_Slope            <- 1
  full_spec$Adstock_Extremity           <- 0
  full_spec$Dim_Ret_Extremity           <- 0
  full_spec$Dim_Ret_Avg_Slope           <- 1
  full_spec$Dim_Ret_End_Slope           <- 1
  
  tnc                                   <- 0
  for (i in 1:tnn) {
    exas                                <- 0
    exdr                                <- 0
    avgslp                              <- 1
    lastslp                             <- 1
    var                                 <- temp$Orig_Variable[i]
    prewgt                              <- temp$PreWeight[i]
    lev                                 <- temp$Level[i]
    if (!(var %in% done)) {
      done                              <- c(done,var)
      lev                               <- temp$Level[i]
      adjbb                             <- temp$Var_Adj_Plus[i]
      vart                              <- temp$Trans_Variable[i]
      varas                             <- paste(var,"_as",sep="")
      if (!(varas %in% nmx))     varas  <- var
      vardr                             <- paste(var,"_uw",sep="")
      if (wflag == 0)            vardr  <- vart
      if (!(vardr %in% nmx))     vardr  <- varas
      if (var != varas) {
        temp2                           <- data.frame(x[,c(var,varas)])
        names(temp2)                    <- c("base","baseas")
        temp2$base                      <- temp2$base * prewgt
        mn0                             <- mean(temp2$base)
        mnas                            <- mean(temp2$baseas)
        if ((mn0 != 0) & (mnas != 0)) {
          temp2$baseas                  <- temp2$baseas * mn0 / mnas
        }
        sd                              <- sd(temp2$base)
        sdas                            <- sd(temp2$baseas)
        mxsd                            <- max(sd,sdas)
        mnsd                            <- min(sd,sdas)
        exas                            <- 1
        if (mnsd > 0)           exas    <- 1 - (mnsd / mxsd)
        temp2$res                       <- abs(temp2$base     - temp2$baseas)
        sdres                           <- sqrt(sum(temp2$res    * temp2$res)    / xn)
        sdtop                           <- sqrt(sum(temp2$base   * temp2$base)   / xn)
        sdtop0                          <- sqrt(sum(temp2$baseas * temp2$baseas) / xn)
        if (sdtop < sdtop0)     sdtop   <- sdtop
        exas0                           <- 0
        if (sdtop > 0)          exas0   <- (sdres / sdtop)
        exas1                           <- exas
        exas2                           <- min(exas,exas0)
        exas3                           <- (exas + exas0) / 2
        exas4                           <- max(exas,exas0)
        if (exas3 <= 0.5)      exas     <- exas4 - (2 * exas3 * (exas4 - exas3))
        if (exas3 >  0.5)      exas     <- (2 * exas3) - exas2 - (2 * exas3 * (exas3 - exas2))
      }  
      if (vardr != varas) {
        temp2                           <- data.frame(x[,c(varas,vardr)])
        names(temp2)                    <- c("baseas","basedr")
        if (varas == var) temp2$baseas  <- temp2$baseas * prewgt
        mnas                            <- min(temp2$baseas)
        mndr                            <- min(temp2$basedr)
        temp2$sqras                     <- temp2$baseas - mnas
        temp2$sqrdr                     <- temp2$basedr - mndr
        mxas                            <- max(temp2$sqras)
        mxdr                            <- max(temp2$sqrdr)
        if ((mxas > 0) & (mxdr > 0)) {
          temp2$sqras                   <- temp2$sqras / mxas
          temp2$sqrdr                   <- temp2$sqrdr / mxdr
        }
        temp2$sqras                     <- floor((temp2$sqras * 100000) + .5) / 100000
        temp2$sqrdr                     <- floor((temp2$sqrdr * 100000) + .5) / 100000
        temp2$imp                       <- 1/(xn * 2)
        temp2$imp0                      <- 1/ xn
        temp3                           <- data.frame(aggregate(temp2$sqrdr,by=list(temp2$sqras),FUN=mean))
        names(temp3)                    <- c("sqras","sqrdr")
        temp4                           <- data.frame(aggregate(temp2$imp  ,by=list(temp2$sqras),FUN=sum))
        names(temp4)                    <- c("sqras","imp")
        temp5                           <- data.frame(aggregate(temp2$imp0 ,by=list(temp2$sqras),FUN=sum))
        names(temp5)                    <- c("sqras","imp0")
        temp3                           <- left_join(temp3,temp4,by="sqras")
        temp3                           <- left_join(temp3,temp5,by="sqras")
        tn                              <- nrow(temp3)
        temp3                           <- temp3[order(temp3$sqras),]
        temp3$num                       <- c(1:tn)
        if (tn <= 11) {
          temp3$imp                     <- temp3$imp + (1 / (2 * tn))
        } else {
          empimp                        <- c(1,0,0, 0,0,0, 0,0,0, 0,tn)
          for (j in 1:9) {
            j0                          <- j + 1
            lim                         <- j * 0.1
            w1                          <- min(temp3[temp3$sqras > lim,"num"])
            w0                          <- w1 - 1
            w2                          <- temp3[temp3$num == w0,"sqras"]
            w3                          <- temp3[temp3$num == w1,"sqras"]
            w4                          <- lim - w2
            w5                          <- w3 - lim
            if (w5 < w4)          w0    <- w1
            empimp[j0]                  <- w0
          }
          for (j in 1:11) {
            num                         <- empimp[j]
            temp3[temp3$num == num,
                  "imp"]                <- temp3[temp3$num == num,"imp"] + (1 / 22)
          }
        }
        temp3$res                       <- abs(temp3$sqrdr - temp3$sqras) / sq2
        temp3[temp3$sqras <= 0.5,
              "top"]                    <-  (temp3[temp3$sqras <= 0.5,"sqras"]  + 
                                             temp3[temp3$sqras <= 0.5,"sqrdr"]) /
          sq2
        temp3[temp3$sqras >  0.5,
              "top"]                    <-   sq2                                -
                                           ((temp3[temp3$sqras >  0.5,"sqras"]  + 
                                             temp3[temp3$sqras >  0.5,"sqrdr"]) /
                                             sq2)
        err                             <- sum(temp3$res * temp3$res * temp3$imp)
        errmax                          <- sum(temp3$top * temp3$top * temp3$imp)
        exdr                            <- sqrt(err / errmax)
        avg                             <- 0
        top                             <- 16
        while (top <= tn) {
          top                           <- top + top
        }
        if (tn < 21) {
          grpn                          <- tn
          temp3$grp                     <- temp3$num
        } else {
          grpn                          <- 20
          temp3$grp                     <- 0
          jt                            <- 0
          for (j in 1:11) {
            for (jt in 0:1) {
              imps                      <- sum(temp3[temp3$grp == 0,"imp0"])
              tmx                       <- max(temp3[temp3$grp == 0, "num"])
              tmn                       <- min(temp3[temp3$grp == 0, "num"])
              if (imps > 0) {
                numz                    <- 21 - length(unique(temp3$grp))
                want                    <- imps / numz
                try                     <- top / 2
                inc                     <- top / 4
                while (inc >= 1) {
                  if (jt == 0) {
                    imp0                <- sum(temp3[(temp3$grp ==   0) &
                                                     (temp3$num <= try),"imp0"])
                    imp1                <- sum(temp3[(temp3$grp ==   0) &
                                                     (temp3$num <  try),"imp0"])
                  } else {
                    imp0                <- sum(temp3[(temp3$grp ==   0) &
                                                     (temp3$num >= try),"imp0"])
                    imp1                <- sum(temp3[(temp3$grp ==   0) &
                                                     (temp3$num >  try),"imp0"])             
                  }
                  fg                    <- 0
                  if (try  >  tmx) {
                    fg                  <- 1
                  } else {
                    if (try < tmn) {
                      fg                <- 2
                    } else {
                      if ((imp0 >= want) &
                          (imp1 < want)) {
                        fg              <- 3
                      } else {
                        if (imp0 < want) {
                          if (jt == 0)
                            fg          <- 2
                          if (jt == 1) 
                            fg          <- 1
                        } else {
                          if (jt == 0) 
                            fg          <- 1
                          if (jt == 1) 
                            fg          <- 2
                        }
                      }
                    }
                  }
                  if (fg == 1)    try   <- try - inc
                  if (fg == 2)    try   <- try + inc
                  if (fg <= 2)    inc   <- inc / 2
                  if (fg == 3)    inc   <- 0
                }
                if (try < tmn)    try   <- tmn
                if (try > tmx)    try   <- tmx
                if (jt == 0) {
                  j0                    <- j
                  j1                    <- j - 1
                  temp3[(temp3$grp ==   0) & 
                          (temp3$num <= try),
                        "grp"]         <- j
                  
                } else {
                  j0                    <- 21 - j
                  j1                    <- j0 + 1
                  temp3[(temp3$grp ==   0) & 
                        (temp3$num >= try),
                        "grp"]          <- j0         
                }
                
                j2                      <- nrow(temp3[temp3$grp == j0,])
                j3                      <- nrow(temp3[temp3$grp ==  0,])
                if ((j1 > 0)  &
                    (j1 < 21) &
                    (j2 > 0)) {
                  j4                    <- j0 - j1
                  cn                    <- length(temp3[temp3$grp == j0,"grp"])
                  y0                    <- sum(temp3[temp3$grp    == j0,"sqrdr"]) / cn
                  cn                    <- length(temp3[temp3$grp == j1,"grp"])
                  y1                    <- sum(temp3[temp3$grp    == j1,"sqrdr"]) / cn
                  y2                    <- j4 * (y0 - y1)
                  if (y2 < 0) {
                    if (jt == 0) {
                      k                 <- max(temp3[temp3$grp == j0,"num"])
                      k0                <- k + 1
                    } else {
                      k                 <- min(temp3[temp3$grp == j0,"num"])
                      k0                <- k - 1
                    }
                    k2                  <- temp3[temp3$num == k0,"grp"]
                    while ((k < tn)   & 
                           (k > 1)    &
                           (y2 < 0)   &
                           (k2 == 0)) {
                      temp3[temp3$num == k0,
                            "grp"]      <- j0
                      cn                <- length(temp3[temp3$grp == j0,"grp"])
                      y0                <- sum(temp3[temp3$grp    == j0,"sqrdr"]) / cn
                      k                 <- k0
                      k0                <- k  + j4
                      y2                <- j4 * (y0 - y1)
                      k2                <- temp3[temp3$num == k0,"grp"]
                    }
                  }
                }
              }
            }
          }
          k                             <- 0
          wgts0                         <- sum(temp3[temp3$grp == 0,"imp0"])
          for (j in 1:20) {
            if (k < 22) {
              imps                      <- sum(temp3[temp3$grp == j,"imp0"])
              if (imps == 0) {
                if (wgts0 > 0) {
                  temp3[temp3$grp == 0,
                        "grp"]          <- j
                  wgts0                 <- 0
                } else {
                  k                     <- min(temp3[temp3$grp > j,"grp"])
                  if (k < 22) {
                    temp3[temp3$grp == k,
                          "grp"]        <- j
                  } 
                }
              }
            }
          }
          grpn                          <- max(temp3$grp)
        }
        w0                              <- nrow(temp3[temp3[1:tn-1,"grp"] > 
                                                      temp3[2:tn,"grp"]   ,])
        if ((min(temp3$grp)            >     1)  |
            (length(unique(temp3$grp)) != grpn)  |
            (max(temp3$grp)            <  grpn)  |
            (w0                        >     0)) 
          cat("Check",tn,grpn,w0,min(temp3$grp),length(unique(temp3$grp)),max(temp3$grp),"\n")
        qrps                            <- data.frame(grp   = 1:grpn     ,
                                                      x     = rep(0,grpn),
                                                      y     = rep(0,grpn),
                                                      count = rep(0,grpn),
                                                      imp   = rep(0,grpn),
                                                      slope = rep(0,grpn),
                                                      mark  = rep(".",grpn))
        avg                             <- 0
        k                               <- 1
        cnt                             <- 0
        first                           <- 10
        j0                              <- 1
        if (grpn > 2) {
          for (j in 1:grpn) {
            cn                          <- length(temp3[temp3$grp == j,"grp"])
            qrps$x[j]                   <- sum(temp3[temp3$grp == j,"sqras"]) / cn
            qrps$y[j]                   <- sum(temp3[temp3$grp == j,"sqrdr"]) / cn
            qrps$count[j]               <- cn
            qrps$imp[j]                 <- sum(temp3[temp3$grp == j,"imp0"])
            if (j > 1) {
              xq1                       <- qrps$x[j0]  
              yq1                       <- qrps$y[j0] 
              xq2                       <- qrps$x[j] 
              yq2                       <- qrps$y[j]
              xq0                       <- xq2 - xq1
              yq0                       <- yq2 - yq1
              imp                       <- qrps$imp[j] + qrps$imp[j - 1]
              slp                       <- 0
              if (xq0 >     0)    slp   <- yq0 / xq0
              if ((slp >= 0) & 
                  (slp <= (first + 1))) {
                j0                      <- j
                if (first == 10)  first <- slp
              } else {
                imp                     <- qrps$imp[j]
                imp0                    <- qrps$imp[j0]
                imp1                    <- imp + imp0
                qrps$x[j0]              <- ((qrps$x[j0] * imp0) + (qrps$x[j] * imp)) / imp1
                qrps$x[j]               <- 0
                qrps$y[j0]              <- ((qrps$y[j0] * imp0) + (qrps$y[j] * imp)) / imp1
                qrps$y[j]               <- 0
                qrps$imp[j0]            <- imp1
                qrps$imp[j]             <- 0
                qrps$count[j0]          <- qrps$count[j0] + qrps$count[j]
                qrps$count[j]           <- 0
              }
            }
          }
          j0                            <- 1
          first                         <- 10
          for (j in 2:grpn) {
            xq1                         <- qrps$x[j0]  
            yq1                         <- qrps$y[j0] 
            xq2                         <- qrps$x[j] 
            yq2                         <- qrps$y[j]
            xq0                         <- xq2 - xq1
            yq0                         <- yq2 - yq1
            imp                         <- qrps$imp[j] + qrps$imp[j0]
            slp                         <- 0
            if (xq0 >     0)      slp   <- yq0 / xq0
            if ((slp >= 0)           &
                (slp <= (first + 1)) & 
                (xq0 > 0))           {
              if (first == 10)    first <- slp
              last                      <- slp
              cnt                       <- cnt +  imp
              avg                       <- avg + (imp * slp)
              qrps$mark[j]              <- "X"
              j0                        <- j
            }
            qrps$slope[j]               <- slp
          }
        }
        if ((first < 10) & (first > 0)) {  
          avgslp                        <- (avg / cnt) / first
          lastslp                       <- last / first
        }
        var0                            <- temp$DR_Var[i]
        imps                            <- max(qrps$imp)
      }
      use_spec[(use_spec$Orig_Variable   == var),
               "Adstock_Extremity"]     <- exas
      use_spec[(use_spec$Orig_Variable   == var),
               "Dim_Ret_Extremity"]     <- exdr
      use_spec[(use_spec$Orig_Variable   == var),
               "Dim_Ret_Avg_Slope"]     <- avgslp
      use_spec[(use_spec$Orig_Variable   == var),
               "Dim_Ret_End_Slope"]     <- lastslp
      full_spec[(full_spec$Orig_Variable == var),
                "Adstock_Extremity"]    <- exas
      full_spec[(full_spec$Orig_Variable == var),
                "Dim_Ret_Extremity"]    <- exdr
      full_spec[(full_spec$Orig_Variable == var),
                "Dim_Ret_Avg_Slope"]    <- avgslp
      full_spec[(full_spec$Orig_Variable == var),
                "Dim_Ret_End_Slope"]    <- lastslp
    }
    tnc                                 <- tnc + 1
    if (tnc > tnm) {
      mt2                               <- Sys.time()
      nextprint                         <- FALSE
      if (megaprint)         nextprint  <- TRUE
      if (print & (mt2 > mt1))
        nextprint                       <- TRUE
      pct                               <- floor(((i * 1000) / tnn) + 0.5) / 10
      pcts                              <- as.character(pct)
      if (pct < 10)        pcts         <- paste(" ",pcts,sep="")
      pcts                              <- paste(pcts,"%",sep="")
      if (nextprint)                       cat("Calculating Extreme Scores   ",pcts,"done \n")
      if (nextprint)             mt1    <- mt2 + timelag
      tnc                               <- tnc - tnm
    }
  } 
  Full_Model_Spec                       <- full_spec
  Model_Spec                            <- Full_Model_Spec %>% filter(Include == 1) 
  obj$spec                              <- Model_Spec
  obj$full_spec                         <- full_spec
  return(obj)
}

Update_Control_Files                    <- function(obj) {

### This function has one input:
###    obj               --- A list with 4 elements
###                             data      = is the model input data
###                             full_spec = The spec file
###                             Panel     = Is it panel data Y or N
###                             CS        = The variable that subdivides the
###                                         panel if any
###                             meta      = the meta file (including elements
###                                         to be weighted, a composate of the
###                                         weights, and the highest level of
###                                         variables related to the model)
###                             wgts      = The weight control file (including
###                                         the groups and elements to be
###                                         weighted, and the max, min, and
###                                         current weights)
###  
### This step adds the "size" and "Centered Weight" variables to the meta file
###    and the some sizing variables to both the meta and weight files. 

  x                                     <- obj$data
  w                                     <- obj$full_spec
  meta                                  <- obj$meta
  wgts                                  <- obj$weight
  panel                                 <- obj$Panel
  
  grps                                  <- names(meta)
  gn                                    <- length(grps)
  gn0                                   <- match("Orig_Variable",grps)
  gn1                                   <- match("Weight",grps)
  meta$Centered_Weight                  <- 1
  meta$size                             <- 0
  meta$Model_Var                        <- "None"
  wgts$size                             <- 0
  mn                                    <- nrow(meta)
  wn                                    <- nrow(wgts)
  gn0                                   <- gn0 + 1
  if (is.na(gn1)) gn1                   <- gn  + 1
  gn1                                   <- gn1 - 1
  grps                                  <- grps[gn0:gn1]
  gn                                    <- length(grps)
  nmx                                   <- names(x)

  for (i in 1:mn) {
    var                                 <- meta$Orig_Variable[i]
    wgt0                                <- meta$Weight[i]
    temp                                <- w[(w$Orig_Variable == var) &
                                             (w$Level         == mlev),
                                             c("Orig_Variable"        ,
                                               "PreWeight"            ,
                                               "Centered_Weight"      ,
                                               "Model_Var"            )]
    var                                 <- temp$Orig_Variable[1]
    prewgt                              <- temp$PreWeight[1]
    wgt                                 <- temp$Centered_Weight[1]
    varm                                <- temp$Model_Var[1]
    varas                               <- paste(var,"_as",sep="")
    if (!(varas %in% nmx))      varas   <- var
    size                                <- 0
    if (varm != "None") {
      size                              <- mean(x[,varas])
      if (var == varas)        size     <- size * prewgt
      if (size < 1)            size     <- 1
    }
    meta$Centered_Weight[i]             <- wgt
    meta$size[i]                        <- size
    meta$Model_Var[i]                   <- varm
  }
  wgts$Use                              <- 0
  meta$CenWgt                           <- 1
  meta$Count                            <- 0
  
  for (i in 1:gn) { 
    grp                                 <- grps[i]
    temp                                <- data.frame(unique(meta[,c("Model_Var",grp)]))
    tn                                  <- nrow(temp)
    temp$ones                           <- rep(1,tn)
    temp0                               <- data.frame(aggregate(temp$ones,by=list(temp$Model_Var),FUN=sum))
    names(temp0)                        <- c("Model_Var","num")
    temp0                               <- temp0[temp0$num > 1,]
    temp0                               <- temp0[temp0$Model_Var != "None",]
    tn0                                 <- nrow(temp0)
    if (tn0 > 0)
      wgts[wgts$Group == grp,"Use"]     <- 1
  }
  
  for (i in 1:wn) {
    grp                                 <- wgts$Group[i]
    val                                 <- wgts$Value[i]
    wgt                                 <- wgts$Weight[i]
    use                                 <- wgts$Use[i]
    low                                 <- wgts$Lower_Weight[i]
    up                                  <- wgts$Upper_Weight[i]
    size                                <- sum(meta[meta[,grp]==val,"size"])
    if ((up == 1) & (low == 1)) { 
      use                               <- 0
      size                              <- 0
    }
    if (size == 0)                use   <- 0
    temp                                <- meta[(meta[,grp]     == val)   &
                                                  (meta$Model_Var != "None"),"Model_Var"]
    temp                                <- unique(temp$Model_Var)
    tn0                                 <- length(temp)
    maxi                                <- 0
    if (tn0 > 0) {
      for (j in 1:tn0) {
        var                             <- temp[j]
        temp0                           <- meta[meta$Model_Var == var,grp]
        names(temp0)                    <- "Value"
        temp0                           <- unique(temp0$Value)
        tn1                             <- length(temp0)
        if (tn1 > maxi)           maxi  <- tn1
      }
    }
    if ((use == 1) & (maxi == 1)) use   <- 0
    if (use  == 0)                size  <- 0
    wgts$size[i]                        <- size
    wgts$Use[i]                         <- use
  }
  
  for (i in 1:wn) {
    grp                                 <- wgts$Group[i]
    val                                 <- wgts$Value[i]
    use                                 <- wgts$Use[i]
    meta[meta[,grp] == val,"CenWgt"]    <- meta[meta[,grp] == val,"CenWgt"] * wgt
    meta[meta[,grp] == val,"Count"]     <- meta[meta[,grp] == val,"Count"]  + use
  }
  
  use                                   <- sum(wgts$Use)
  if (use == 0) {
    cat("Error:  The Groups in the Meta file and the Model variables in the Variables (Spec) file \n ")
    cat("        seen together indicate that the weights will have no effect on the model results. \n")
    cat("        The weights in the weight file will therefore not change and no optimization of weights \n")
    cat("        will be done. \n")
  }
  
  meta$CenWgt                           <- NULL
  meta$Count                            <- NULL
  
  obj$meta                              <- meta
  obj$weights                           <- wgts
  if (panel == "N")   obj$CS            <- NULL
  return(obj)
  
}

Decomposition_Levels                    <- function(obj,
                                                    incl_spent      = FALSE,
                                                    print           = FALSE,
                                                    megaprint       = FALSE,
                                                    timelag         = 10) {
### This function has 4 inputs:
###    obj               --- A list with 9 elements
###                             data            = is the model input data
###                             full_spec       = The spec file
###                             Model           = Model results.  Specifically
###                                coefficients = The coefficients found for
###                                               the model
###                             Time            = The time unit for the model
###                             Panel           = If a panel is being used
###                             CS              = the subdivision for a panel
###                                               model
###                             ModelForm       = the form of the model 
###                                               (Currently only LIN-LIN is
###                                               working)
###                             Decomposition_level 
###                                             = The decomposition files 
###                                               (The output of this function)
###                             Decomposition_level_panel 
###                                             = The panel decomposition files
###    incl_spent = TRUE  --- Include spending in the decomp
###    megaprint  = TRUE  --- prints everything in the R Log.  Just a continuous
###                             record of where the run is and what is happening
###    print      = TRUE  --- Just a limited print of where things are in the run
###                             (just enough to see what is happening)
###                             Also it activates the "timelag" timing see below.
###    timelag    = # sec --- A number in seconds.  If you set timelag = 10 then
###                             very 10 seconds or so they show you where the
###                             run is at that time
###  
### This function creates the same decomps, panel decomps, and summary decomps
###    That other functions create but also create them for all the level
###    variables related to the model variables (instead of just the model
###    variables).  This is needed for the optimizaton.  It is based on the
###    Code originally created by Julia Liu
###
### This function works equally well for both panel models or simple models.
  mt1                                   <- Sys.time() + timelag
  if (megaprint)                           cat("Level Decomps Step  1 \n")
  
  ### First we just do a lot of checks and setup.  This is mostly the decomps
  ###    for the model variables and the variables above them (subdivisions
  ###    of the model variables).
  
  x                                     <- obj$data
  w                                     <- obj$full_spec
  b                                     <- obj$Model$coefficients
  tm                                    <- obj$Time
  panel                                 <- obj$Panel
  cs                                    <- obj$CS
  mf                                    <- obj$ModelForm
  
  csflag                                <- 0
  if (panel == "Y") {
    csflag                              <- nchar(cs)
    if ((csflag > 0) & 
        (length(unique(obj$data[[cs]]))<=1)) {
      csflag                            <- 0
    }
    if  (csflag > 0)    csflag          <- 1
  }
    
  wflag                                 <- 0
  if ("Level" %in% names(w)) {
    mlev                                <- max(w[w$Include>0 ,"Level"])
    nlev                                <- min(w[w$Include>0 ,"Level"])
    elev                                <- max(w[w$Include==1,"Level"])
    if (mlev > nlev) wflag              <- 1
  }
  
  if (wflag == 0) {
    stop("The 'Decomp_Levels' function is only needed when the Specs have multiple levels")
  }
  
  use_spec                              <- w[(w$Variable_Type != "Dependent") &
                                             (w$Include        > 0)           &
                                             (w$Model_Var     != "None")      &
                                             (w$Model_Var     != "Multi"),]
  calc_var                              <- use_spec$Trans_Variable
  org_mod_var                           <- use_spec$Model_Var
  calc_lev                              <- use_spec$Level
  calc_pw                               <- use_spec$PreWeight
  if(incl_spent)
    spend_var                           <- use_spec$Spend_Variable
  
  dcmp_var                              <- paste("d_", calc_var, "_Lev", calc_lev,sep="")
  temp                                  <- data.frame(Orig_Variable=org_mod_var)
  temp0                                 <- unique(w[,c("Orig_Variable","Trans_Variable","PreWeight")])
  temp1                                 <- left_join(temp,temp0,by="Orig_Variable")
  mod_var                               <- temp1$Trans_Variable
  mod_pw                                <- temp1$PreWeight
  calc_pw                               <- calc_pw / mod_pw
  
  if (csflag == 1) {
    bt                                  <- b
    bt$Error                            <- NULL
    bt$Tvalue                           <- NULL
    bt$Variables                        <- paste("beta", bt$Variables, sep="_")
    bt                                  <- tidyr::spread(bt, Variables, Estimate)
    beta_var                            <- paste("beta",mod_var,sep="_")
  }
  
  depvar                                <- w$Trans_Variable[(w$Variable_Type == "Dependent") &
                                                            (w$Include       == 1)]
  depvar_orig                           <- w$Orig_Variable[ (w$Variable_Type == "Dependent") &
                                                            (w$Include       == 1)]
  
  if (csflag == 0) {
    decomp                              <- x[, c(tm,     depvar_orig)]
  } else {
    decomp                              <- x[, c(tm, cs, depvar_orig)]
    x                                   <- left_join(x, bt, by = cs )
  }         
  
  dcmp_base                             <- names(decomp)
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Level Decomps Step  2 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Now of do the actual work of creating applying to coefficients of the model
  ###    of the model to the model variables and the variables above them
  ###    (subdivisions of the model variables) and use that to create the decomps
  
  llev                                  <- 0
  substep                               <- 0
  if(toupper(mf) == "LIN_LIN") {
    for(i in 1:length(calc_var)) {
      lev                               <- calc_lev[i]
      if (lev != llev) {
        substep                         <- substep + 1
        nlev                            <- length(calc_lev[calc_lev == lev])
        mt2                             <- Sys.time()
        nextprint                       <- FALSE
        if (megaprint)       nextprint  <- TRUE
        if (print & (mt2 > mt1))
          nextprint                     <- TRUE
        if (nextprint)                     cat("Level Decomps Step  3 substep",substep,"  \n")
        if (nextprint)       mt1        <- mt2 + timelag
        llev                            <- lev
      }
      if (csflag == 0) 
        decomp[[dcmp_var[i] ]]          <- b$Estimate[b$Variables == mod_var[i]] * 
          calc_pw[i] * x[[calc_var[i] ]]
      if (csflag == 1) 
        decomp[[dcmp_var[i] ]]          <- x[[beta_var[i]]]                      *
          calc_pw[i] * x[[calc_var[i] ]]
      if ((toupper(w$Transform[    (w$Trans_Variable == depvar) & (w$Level == elev)]) == "Y")   &
          (toupper(w$TransformType[(w$Trans_Variable == depvar) & (w$Level == elev)]) == "MC")) {
        if(tolower(dcmp_var[i]) == "d_intercept") {
          decomp[[dcmp_var[i] ]]        <- decomp[[dcmp_var[i]]] * x$scl + x$cen
        } else {
          decomp[[dcmp_var[i] ]]        <- decomp[[dcmp_var[i]]] * x$scl
        }
      }
    }
  } else if (toupper(mf) == "LOG_LOG") {
    stop("Sorry, Log_log haven't been implemented yet. ")
    #cat("The reference point is based on ", Reference, "\n")
    #for(i in 1:length(calc_var)) {
    #}
  }
  
  # include spent variable
  if (incl_spent) {
    for (i in 1:length(spend_var)) {
      if( !is.na(spend_var) ) {
        decomp[[spend_var[i] ]]         <- x[[spend_var[i] ]]
      }
    }
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Level Decomps Step  4 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### Now that we are finished with the modeling variables and above we need to 
  ###    find the decomps for all the relevant variables below the model
  ###    variable which are the variables that are based on combinations of 
  ###    different modeling variables.  The process here is simple (in concept)
  ###    we just add the decomps together to make the lower level decomps.
  
  org_mod_var0                          <- unique(org_mod_var)
  elev0                                 <- elev - 1
  
  if (elev0 > 0) {
    for (i in 1:elev0) {
      i0                                <- elev - i
      i1                                <- i0   + 1
      org_mod_var1                      <- w$Next_Level_Down[(w$Orig_Variable %in% org_mod_var0) &
                                                               (w$Level == i1)]
      org_mod_var0                      <- unique(org_mod_var1)
      zn                                <- length(org_mod_var0)
      for (j in 1:zn) {
        var                             <- org_mod_var0[j]
        vart                            <- w$Trans_Variable[(w$Orig_Variable == var) &
                                                              (w$Level == i0)]
        vard                            <- paste("d_", vart, "_Lev", i0,sep="")
        var_parts                       <- w$Trans_Variable[(w$Next_Level_Down == var) &
                                                              w$Level == i1]
        dcmp_parts                      <- paste("d_", var_parts, "_Lev", i1,sep="")
        dcmp_parts                      <- dcmp_parts[dcmp_parts %in% dcmp_var]
        pn                              <- length(dcmp_parts)
        decomp[,vard]                   <- 0
        for (k in 1:pn) {
          vard0                         <- dcmp_parts[k]
          decomp[,vard]                 <- decomp[,vard] + decomp[,vard0]
        }
        if (j == 1) {
          calc_lev                      <- c(vart)
          dcmp_lev                      <- c(vard)
        } else {
          calc_lev                      <- c(calc_lev,vart)
          dcmp_lev                      <- c(dcmp_lev,vard)
        }
      }
      calc_var                          <- c(calc_lev,calc_var)
      dcmp_var                          <- c(dcmp_lev,dcmp_var)
    }
    decomp                              <- decomp[,c(dcmp_base,dcmp_var)]
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Level Decomps Step  5 \n")
  if (nextprint)             mt1        <- mt2 + timelag
  
  ### If this a panel level model, we want a non-panel level version of the decomps
  ###    These will be created from the panel level decomps
  
  if (csflag == 1) {
    decompp                             <- decomp
    obj$Decomposition_level_panel       <- decomp
    nmd                                 <- names(decomp)
    nmd                                 <- nmd[!(nmd %in% c(cs))]
    decomp$time                         <- as.numeric(decomp[,tm])
    temp                                <- unique(decomp[,c("time",tm)])
    decomp[,tm]                         <- NULL
    decomp[,cs]                         <- NULL
    decomp                              <- data.frame(aggregate(decomp,by=list(decomp$time),FUN=sum))
    decomp$time                         <- NULL
    names(decomp)[1]                    <- "time"
    decomp                              <- left_join(decomp,temp,by="time")
    decomp$time                         <- NULL
    decomp                              <- decomp[,c(nmd)]
    
  }
  
  obj$Decomposition_level               <- decomp
  return(obj)
}

Random_Weights_CMAES_Opt                <- function(wobj,
                                                    Minutesleft     =    40,
                                                    RandomTrys      =  1000,
                                                    RandomSteps     =     8,
                                                    SpeedVsAccuracy =     5,
                                                    print           = FALSE,
                                                    megaprint       = FALSE,
                                                    timelag         = 10) {
  
### This function has 8 inputs:
###    wobj                     --- A list with 10 elements
###                                    data            = is the model input data
###                                    meta            = A reduced down meta file of
###                                                      all the high level variables
###                                    wgts            = A reduced down weights file
###                                                      of all the important weights
###                                    grps            = A file of the groups that
###                                                      can effect the model and
###                                                      the sizes (effect) of each
###                                                      group.
###                                    modvars         = Additional data about the 
###                                                      variables in the model
###                                    decomps         = the model decomps on the 
###                                                      variables in the meta file
###                                                      that effect the outcome
###                                    basemsr         = the models mean square
###                                                      residuals before the any
###                                                      optimization.
###                                    depvar          = the variance of the dependent
###                                                      variable being modeled
###                                    depmn           = the mean of the dependent
###                                                      variable
###                                    dep             = dependent variable 
###    Minutesleft    = # mins   --- Number of minutes left in the total minutes that 
###                                     given to do the optimization.  If the
###                                     optimization takes over that number of
###                                     minutes the process is spead up.
###    RandomTrys      = # trys  --- Number of random values picked in each step 
###                                     to test out
###    RandomSteps     = # steps --- Number of steps in testing random values.
###                                     each step will get tighter around the current
###                                     best value.
###    SpeedVsAccuracy = #       --- A number 1-10 where 1 means very fast (not that
###                                     accurate with a lower R-squared) and 10 means 
###                                     very accurate but also very slow. 
###    megaprint       = TRUE    --- prints everything in the R Log.  Just a 
###                                     continuous record of where the run is and
###                                     what is happening
###    print           = TRUE    --- Just a limited print of where things are in 
###                                     the run 
###                                     (just enough to see what is happening)
###                                     Also it activates the "timelag" timing 
###                                     see below.
###    timelag         = # sec   --- A number in seconds.  If you set timelag = 10
###                                    then very 10 seconds or so they show you
###                                    where the run is at that time
###  
### This is random (CMAES) section of the code.  It gives every important
###    weight a random value and test which combination of random values are
###    The best.
### It is done in multiple steps (how many is given by the "RandomSteps" input
###    from 1 to 16).   And in every step it calculates multiple random
###    combinations (how many is given by the "RandomTrys" input).
### In the first step it is mostly uniformly random but centered on the last
###    best values of each weight.  The second step it is somewhat normally
###    distributed over the whole range.  The 3rd time through it is more
###    normally distributed over half the range.  And it each step it gets more
###    of a normal distribution over a smaller range (with all steps centered on
###    the last best value)
### The weights in each weighting group also average around one as much as
###    possible given the upper and lower limits.  This makes the process more
###    efficient at finding the best weights overall.
### The advantage of the random picks is to handle complex interactions between
###    the elements that are weighted and to test as many local minimums as
###    possible.  The more traditional optimization assumes a simpler
###    relationship between the elements and no local minimums.
### But this random method might find the optimum weighting or it might not
###    (likely not if there are a lot of elements to be weighted).  It also has
###    the disadvantage of finding the best weight for about 80% of the elements,
###    but being totally off for 20% of the weights (more often it is off for
###    weights that have little effect on the model, but sometimes an important
###    weight is off).  This leads to very good goodness-of-fit measures but 
###    very inconsistent results (every time you run it you get different
###    weights).
### The more traditional "fine-tuning" method used after this section, should
###    get you a better and more consistent weighting assuming the random
###    methods got close to the best weights and assuming the random algorithm
###    picked the best local minimum.
### We strongly suggest you do not skip the the "fine-tuning" section of this
###    weighting.  However, this random weight step works best  for the first
###    iteration of optimization. If you run this code again with the previous
###    iteration's weights, then using this random weighting step will only 
###    make minor improvements on the previous best weights (so it is optional).
  mss                                   <- Sys.time()
  mt1                                   <- mss + timelag
  
  meta                                  <- wobj$meta
  wgts                                  <- wobj$wgts
  modvars                               <- wobj$modvars
  d                                     <- wobj$decomps
  x                                     <- wobj$data
  grps                                  <- wobj$grps
  basemsr                               <- wobj$basemsr
  depvar                                <- wobj$depvar
  depmn                                 <- wobj$depmn
  dep                                   <- wobj$dep
  mrz                                   <- Minutesleft         
  rt                                    <- RandomTrys
  rs                                    <- RandomSteps
  sa                                    <- SpeedVsAccuracy
  rts                                   <- c(300,400,600,750,1000,1400,1900,2500,3200,4000)
  if (sa < 1)                sa         <-  1
  if (sa > 10)               sa         <- 10
  sa                                    <- floor(sa + 0.5)
  if  (rs <=   0)            rs         <-  sa + 3
  if  (rs <    1)            rs         <-   1
  if  (rs >   16)            rs         <-  16
  rs                                    <- floor(rs + 0.5)
  if  (rt <=   0)            rt         <- rts[sa]
  if  (rt <  100)            rt         <- 100
  rt                                    <- floor(rt + 0.5)
  rq                                    <- rs * rt
  wn                                    <- nrow(wgts)
  mn                                    <- nrow(meta)
  c0                                    <- 0
  ca                                    <- 0
  e0                                    <- 0
  ea                                    <- 0
  cb                                    <- 0
  gn                                    <- nrow(grps)
  r0                                    <- 1 - (basemsr / depvar)
  r0                                    <- floor((r0 * 100000) + .5) / 1000
  if (print) cat("Pre-Opt         -   Estimated R-Squared", r0,
                 "  -                          -   Mean Squared Residuals",basemsr,"\n")
  go                                    <- 1
  r2                                    <- 0
  r3                                    <- 0
  mvn                                   <- nrow(modvars)
  for (i in 1:rs) {
    ### Start by adjusting the upper and lower limits based on the step you are on
    ###    and then pick a random weight based on that range (centering it on the
    ###    current best weight).
    if (go == 1) {
      w0                                <-  1.167
      w1                                <-  0.444
      w2                                <-  0.8
      w3                                <-  1.27
      w4                                <- (w0 ^ sa) * w1
      w5                                <- (w2 ^ i ) * w3
      buff                              <- 1 + (0.006 * w4 * w5)
      low                               <- wgts$Lower_Weight
      upp                               <- wgts$Upper_Weight
      cur                               <- wgts$Weight
      low[low > cur]                    <- cur[low > cur]
      upp[upp < cur]                    <- cur[upp < cur]
      if (i > 2) {
        low                             <- cur - ((cur - low) / (i - 1))
        upp                             <- cur + ((upp - cur) / (i - 1))
      }
      wgts$Lower_Cur                    <- low
      wgts$Upper_Cur                    <- upp
      for (j in 1:rt) {
        meta$Try                        <- meta$Weight
        wgts$Try                        <- wgts$Weight
        # Find the new random weight
        for (k in 1:wn) {
          grp                           <- wgts$Group[k]
          val                           <- wgts$Value[k]
          low                           <- wgts$Lower_Weight[k]
          upp                           <- wgts$Upper_Weight[k]
          cur                           <- wgts$Weight[k]
          ran                           <- sum(runif(i)) / i
          if (ran < .5) {
            new                         <- low + ((cur - low) *  ran        * 2)
          } else {
            new                         <- cur + ((upp - cur) * (ran - 0.5) * 2)
          }
          wgts$Try[k]                   <- new
          # Apply the weights to the meta file
          diff                          <- new / cur
          meta[meta[,grp] == val,"Try"] <- meta[meta[,grp] == val,"Try"] * diff
        }
        # Center the random weights (Short-cut Method)
        for (k in 1:mvn) {
          var                           <- modvars$var[k]
          sz                            <- modvars$size[k]
          meta[meta$Model_Var == var,
               "centry"]                <- meta$Try[            meta$Model_Var == var]  *
                                           meta$Centered_Weight[meta$Model_Var == var]  /
                                           meta$Weight[         meta$Model_Var == var]
          adj                           <- sum(meta$centry[     meta$Model_Var == var]  *
                                               meta$size[       meta$Model_Var == var]) / sz
          meta[meta$Model_Var == var,
               "centry"]                <- meta[meta$Model_Var == var,"centry"] / adj
        }
        # Find the Mean Squared Residuals of the new weights
        d$try                           <- 0
        for (k in 1:mn) {
          var                           <- meta$Trans_Variable[k]
          cwgt                          <- meta$Centered_Weight[k]
          twgt                          <- meta$centry[k]
          d$try                         <- d$try + (d[,var] * twgt / cwgt)
        }
        trymn                           <- mean(d$try)
        d$try                           <- d$try       + depmn - trymn
        tryres                          <- d[,dep]     - d$try
        trymsr                          <- mean(tryres * tryres)
        cz                              <- 0
        c0                              <- c0 + 1
        if (trymsr < (basemsr * buff)) {
          # Center the random weights (Longer Method)
          meta$Mean_Temp_DR             <- (meta$Mean_DR - meta$DR_Adj_Plus) * meta$Try
          meta$W_DR_Adj_Plus            <- meta$DR_Adj_Plus * meta$Try
          
          meta$centry                   <- meta$Try
          
          for (k in 1:mvn) {
            var                         <- modvars$var[k]
            varas                       <- modvars$varas[k]
            size                        <- modvars$size[k]
            sdas                        <- modvars$SD_as[k]
            mnas                        <- modvars$Mean_as[k]
            temp                        <- meta[meta$Model_Var == var,]
            data_vector_as              <- x[,varas]
            cont                        <- sum(temp$Mean_Temp_DR)
            adjc0                       <- sum(temp$W_DR_Adj_Plus)
            data_vector                 <- data_vector_as * 0
            adja0                       <- modvars$DR_Adj_Times[k]
            tn                          <- nrow(temp)
            for (h in 1:tn) {
              vardr0                    <- temp$UW_Variable[h]
              wgt                       <- temp$Try[h]
              data_vector0              <- x[,vardr0]  * wgt
              data_vector               <- data_vector + data_vector0
            }  
            data_vector0                <- data_vector
            data_vector                 <- data_vector - adjc0
            data_vector                 <- data_vector / adja0
            sdt                         <- sd(data_vector)
            mnt                         <- mean(data_vector)
            mnt2                        <- mean(data_vector * data_vector)
            mnad                        <- mean(data_vector * data_vector_as)
            adja                        <- -1
            mnt1                        <- mnt2   - (mnt * mnt)
            mnad1                       <- mnad   - (mnt * mnas)
            if ((mnt1  != 0) &
                (mnad1 != 0))      adja <- mnad1 / mnt1
            if (adja <= 0) {
              adja                      <- 1
              if ((sdt  > 0) & 
                  (sdas > 0))      adja <- sdas  / sdt
            }
            adjb                        <- mnas - (mnt * adja)
            data_vector                 <- (data_vector * adja)
            data_vector0                <- data_vector0 - adjc0
            adje                        <- 1
            mn1                         <- mean(data_vector)
            mn0                         <- mean(data_vector0)
            if (mn0 != 0)         adje  <- mn1 / mn0
            temp$adjd2                  <- 0
            if (adjb * adjc0 > 0) {
              temp$adjd2                <- temp$W_DR_Adj_Plus * adjb / adjc0
            } else {
              if (cont != 0) {
                temp$adjd2              <- temp$Mean_Temp_DR * adjb / cont
              }
            }
            temp$mnt                    <- abs((temp$Mean_Temp_DR * adje) + temp$adjd2)
            temp$wgt1                   <- 1
            temp[(temp$Abs_Mean_DR) > 0.00001,
                 "wgt1"]                <- abs(temp[temp$Abs_Mean_DR > 0.00001,"mnt"])         /
              abs(temp[temp$Abs_Mean_DR > 0.00001,"Abs_Mean_DR"])
            for (h in 1:tn) {
              var0                      <- temp$Orig_Variable[h]
              wgt1                      <- temp$wgt1[h]
              meta[meta$Orig_Variable == var0,
                   "centry"]            <- wgt1
            }
          }
          # Find the Mean Squared Residuals of the new weights
          d$try                         <- 0
          for (k in 1:mn) {
            var                         <- meta$Trans_Variable[k]
            cwgt                        <- meta$Centered_Weight[k]
            twgt                        <- meta$centry[k]
            d$try                       <- d$try + (d[,var] * twgt / cwgt)
          }
          trymn                         <- mean(d$try)
          d$try                         <- d$try   + depmn - trymn
          tryres                        <- d[,dep] - d$try
          trymsr                        <- mean(tryres * tryres)
          if (trymsr < basemsr) {
            basemsr                     <- trymsr
            for (k in 1:gn) {
              grp                       <- grps$Group[k]
              size                      <- grps$size[k]
              low                       <- max(wgts[wgts$Group == grp,"Lower_Weight"] /
                                               wgts[wgts$Group == grp,"Try"])
              upp                       <- min(wgts[wgts$Group == grp,"Upper_Weight"] /
                                               wgts[wgts$Group == grp,"Try"])
              cen                       <- sum(wgts[wgts$Group == grp,"size"]         *
                                               wgts[wgts$Group == grp,"Try"])         / size
              cen                       <- 1 / cen
              if (cen < low)       cen  <- low
              if (cen > upp)       cen  <- upp
              wgts[wgts$Group == grp,
                   "Try"]               <- wgts[wgts$Group == grp,"Try"] * cen
            }        
            meta$Try                    <- meta$Weight
            for (k in 1:wn) {
              grp                       <- wgts$Group[k]
              val                       <- wgts$Value[k]
              cur                       <- wgts[(wgts$Group == grp) &
                                                  (wgts$Value == val),"Weight"]
              new                       <- wgts[(wgts$Group == grp) &
                                                  (wgts$Value == val),"Try"]
              diff                      <- new / cur
              meta[meta[,grp] == val,
                   "Try"]               <- meta[meta[,grp] == val,"Try"] * diff
            }
            meta$Mean_Temp_DR           <- (meta$Mean_DR - meta$DR_Adj_Plus) * meta$Try
            meta$W_DR_Adj_Plus          <- meta$DR_Adj_Plus * meta$Try
            
            meta$centry                 <- meta$Try
            # Center the random weights
            for (k in 1:mvn) {
              var                       <- modvars$var[k]
              varas                     <- modvars$varas[k]
              size                      <- modvars$size[k]
              sdas                      <- modvars$SD_as[k]
              mnas                      <- modvars$Mean_as[k]
              temp                      <- meta[meta$Model_Var == var,]
              data_vector_as            <- x[,varas]
              cont                      <- sum(temp$Mean_Temp_DR)
              adjc0                     <- sum(temp$W_DR_Adj_Plus)
              data_vector               <- data_vector_as * 0
              adja0                     <- modvars$DR_Adj_Times[k]
              tn                        <- nrow(temp)
              for (h in 1:tn) {
                vardr0                  <- temp$UW_Variable[h]
                wgt                     <- temp$Try[h]
                data_vector0            <- x[,vardr0]  * wgt
                data_vector             <- data_vector + data_vector0
              }  
              data_vector0              <- data_vector
              data_vector               <- data_vector - adjc0
              data_vector               <- data_vector / adja0
              sdt                       <- sd(data_vector)
              mnt                       <- mean(data_vector)
              mnt2                      <- mean(data_vector * data_vector)
              mnad                      <- mean(data_vector * data_vector_as)
              adja                      <- -1
              mnt1                      <- mnt2   - (mnt * mnt)
              mnad1                     <- mnad   - (mnt * mnas)
              if ((mnt1  != 0) &
                  (mnad1 != 0))    adja <- mnad1 / mnt1
              if (adja <= 0) {
                adja                    <- 1
                if ((sdt  > 0) & 
                    (sdas > 0))    adja <- sdas  / sdt
              }
              adjb                      <- mnas - (mnt * adja)
              data_vector               <- (data_vector * adja)
              data_vector0              <- data_vector0 - adjc0
              adje                      <- 1
              mn1                       <- mean(data_vector)
              mn0                       <- mean(data_vector0)
              if (mn0 != 0)       adje  <- mn1 / mn0
              temp$adjd2                <- 0
              if (adjb * adjc0 > 0) {
                temp$adjd2              <- temp$W_DR_Adj_Plus * adjb / adjc0
              } else {
                if (cont != 0) {
                  temp$adjd2            <- temp$Mean_Temp_DR * adjb / cont
                }
              }
              temp$mnt                  <- abs((temp$Mean_Temp_DR * adje) + temp$adjd2)
              temp$wgt1                 <- 1
              temp[(temp$Abs_Mean_DR) > 0.00001,
                   "wgt1"]              <- abs(temp[temp$Abs_Mean_DR > 0.00001,"mnt"])         /
                                           abs(temp[temp$Abs_Mean_DR > 0.00001,"Abs_Mean_DR"])
              for (h in 1:tn) {
                var0                    <- temp$Orig_Variable[h]
                wgt1                    <- temp$wgt1[h]
                meta[meta$Orig_Variable == var0,
                     "centry"]          <- wgt1
              }
            }
            d$try                       <- 0
            for (k in 1:mn) {
              var                       <- meta$Trans_Variable[k]
              cwgt                      <- meta$Centered_Weight[k]
              twgt                      <- meta$centry[k]
              d[,var]                   <- d[,var] * twgt / cwgt
              d$try                     <- d$try   + d[,var]
            }
            trymn                       <- mean(d$try)
            d$try                       <- d$try       + depmn - trymn
            tryres                      <- d[,dep] - d$try
            trymsr                      <- mean(tryres * tryres)
            cz                          <- 1
            meta$Centered_Weight        <- meta$centry
            meta$Weight                 <- meta$Try
            wgts$Weight                 <- wgts$Try
            low                         <- wgts$Lower_Weight
            upp                         <- wgts$Upper_Weight
            cur                         <- wgts$Weight
            low[low > cur]              <- cur[low > cur]
            upp[upp < cur]              <- cur[upp < cur]
            if (i > 2) {
              low                       <- cur - ((cur - low) / (i - 1))
              upp                       <- cur + ((upp - cur) / (i - 1))
            }
            wgts$Lower_Cur              <- low
            wgts$Upper_Cur              <- upp
          }
          r1                            <- c0 / rq
          r1                            <- floor((r1 * 10000) + .5) / 100
          if ((c0 > ca) | 
              (cz == 1) | 
              (r1 >= ea)) {
            r0                          <- 1 - (basemsr / depvar)
            r0                          <- floor((r0 * 100000) + .5) / 1000
            if (r2 > r1)          r1    <- r2
            if (r3 > r1)          r1    <- r3
            if (r1 > 99.9)        r1    <- 99.9
            prt                         <- 0
            if (megaprint)        prt   <- 1
            if ((print)     & 
                (r1 >= ea))       prt   <- 1
            # Note the "Estimate R-Squared" uses a different formula then we normally
            #    use, but it is an indication in how the R-Squared based on any
            #    formula will improve.
            if (prt == 1) {
              mt2                       <- Sys.time()
              nextprint                 <- FALSE
              if (megaprint)            
                nextprint               <- TRUE
              if (print & (mt2 > mt1))  
                nextprint               <- TRUE
              if (nextprint) {
                mt1                     <- mt2 + timelag
                r1s                     <- as.character(r1)
                if (r1 < 100) {
                  if (r1 < 10)    r1s   <- paste(" ",r1s,sep="")
                  r1s                   <- paste(" ",r1s,sep="")
                }
                r1s                     <- substr(paste(r1s,"       ",sep=""),1,7)
                r0s                     <- as.character(r0)
                r0s                     <- substr(paste(r0s,"       ",sep=""),1,6)
                cat("Random Opt      -   Estimated R-Squared", r0s,"  -  ",r1s,"Percent Done   -   Mean Squared Residuals",basemsr,"\n")
              }
              e0                        <- floor(r1 / 5) * 5
              ea                        <- e0 + 5
            }
            ca                          <- ca +  50
          }
        }
        r1                              <- c0 / rq
        r1                              <- floor((r1 * 10000) + .5) / 100
        if ((c0 > ca)   |   
            (r1 >= ea)) {
          r0                          <- 1 - (basemsr / depvar)
          r0                          <- floor((r0 * 100000) + .5) / 1000
          if (r2 > r1)          r1    <- r2
          if (r3 > r1)          r1    <- r3
          if (r1 > 99.9)        r1    <- 99.9
          prt                         <- 0
          if (megaprint)        prt   <- 1
          if ((print)     & 
              (r1 >= ea))       prt   <- 1
          # Note the "Estimate R-Squared" uses a different formula then we normally
          #    use, but it is an indication in how the R-Squared based on any
          #    formula will improve.
          if (prt == 1) {
            mt2                       <- Sys.time()
            nextprint                 <- FALSE
            if (megaprint)            
              nextprint               <- TRUE
            if (print & (mt2 > mt1))  
              nextprint               <- TRUE
            if (nextprint) {
              mt1                     <- mt2 + timelag
              r1s                     <- as.character(r1)
              if (r1 < 100) {
                if (r1 < 10)    r1s   <- paste(" ",r1s,sep="")
                r1s                   <- paste(" ",r1s,sep="")
              }
              r1s                     <- substr(paste(r1s,"       ",sep=""),1,7)
              r0s                     <- as.character(r0)
              r0s                     <- substr(paste(r0s,"       ",sep=""),1,6)
              cat("Random Opt      -   Estimated R-Squared", r0s,"  -  ",r1s,"Percent Done   -   Mean Squared Residuals",basemsr,"\n")
            }
            e0                        <- floor(r1 / 5) * 5
            ea                        <- e0 + 5
          }
          ca                          <- ca +  50
        } 
      }
    }
    ms8                                 <- Sys.time()
    ms9                                 <- as.numeric(difftime(ms8,mss,units="mins"))
    if (ms9 > mrz)                  go  <- 0
    r2                                  <- i / rs
    r2                                  <- floor((r2 *  10000) + .5) / 100
    r3                                  <- ms9 / mrz
    r3                                  <- floor((r3 *  10000) + .5) / 100
    r0                                  <- 1 - (basemsr / depvar)
    r0                                  <- floor((r0 * 100000) + .5) / 1000
    r1                                  <- c0 / rq
    r1                                  <- floor((r1 *  10000) + .5) / 100
    if (r2 > r1)                    r1  <- r2
    if (r3 > r1)                    r1  <- r3
    if (r1 > 99.9)                  r1  <- 99.9
    prt                                 <- 0
    if (megaprint)                prt   <- 1
    if ((print) & (r1 >= ea))     prt   <- 1
    if (prt == 1) {
      mt2                               <- ms8
      nextprint                         <- FALSE
      if (megaprint)    nextprint       <- TRUE
      if (print & (mt2 > mt1))  
        nextprint                       <- TRUE
      if (nextprint) {
        mt1                             <- mt2 + timelag
        r1s                             <- as.character(r1)
        if (r1 < 100) {
          if (r1 < 10)            r1s   <- paste(" ",r1s,sep="")
          r1s                           <- paste(" ",r1s,sep="")
        }
        r1s                             <- substr(paste(r1s,"       ",sep=""),1,7)
        r0s                             <- as.character(r0)
        r0s                             <- substr(paste(r0s,"       ",sep=""),1,6)
        cat("Random Opt      -   Estimated R-Squared", r0s,"  -  ",r1s,"Percent Done   -   Mean Squared Residuals",basemsr,"\n")
      }
      e0                                <- floor(r1 / 20) * 20
      ea                                <- e0 + 5
    }
    ca                                  <- ca + 50
  }
  wobj$meta                             <- meta
  wobj$wgts                             <- wgts
  wobj$decomps                          <- d
  wobj$basemsr                          <- basemsr
  wobj$numtrys                          <- c0
  return(wobj)
}  


Incremental_Improvement_Opt             <- function(wobj,
                                                    Minutesleft     =    40,
                                                    Trysleft        =  8000,
                                                    SpeedVsAccuracy =     5,
                                                    Penalty         =   0.5,
                                                    print           = FALSE,
                                                    megaprint       = FALSE,
                                                    timelag         = 10,
                                                    Label           = "Fine-Tune Opt") {
### This function has 7 inputs:
###    wobj                     --- A list with 9 elements
###                                    data            = is the model input data
###                                    meta            = A reduced down meta file of
###                                                      all the high level variables
###                                    wgts            = A reduced down weights file
###                                                      of all the important weights
###                                    grps            = A file of the groups that
###                                                      can effect the model and
###                                                      the sizes (effect) of each
###                                                      group.
###                                    modvars         = Additional data about the 
###                                                      variables in the model
###                                    decomps         = the model decomps on the 
###                                                      variables in the meta file
###                                                      that effect the outcome
###                                    basemsr         = the models mean square
###                                                      residuals before the any
###                                                      optimization.
###                                    depvar          = the variance of the dependent
###                                                      variable being modeled
###                                    depmn           = the mean of the dependent
###                                                      variable
###                                    dep             = dependent variable
###    Penalty         = #       --- A number 0 - 1 that sets the limits on the
###                                     significant levels needed to fine tune
###                                     the weights.  If unsure keep it at the
###                                     level given.  If the results seem to be
###                                     erratic for very small (unimportant) 
###                                     elements, increase this number slightly.
###                                     If the R-squared seems to increase to
###                                     much from the Random weights, increase
###                                     this number.  To test out "Uncontrolled"
###                                     weights (highly overfilled) then set this
###                                     number to zero
###    Minutesleft     = # mins  --- Number of minutes left in the total minutes that 
###                                     given to do the optimization.  If the
###                                     optimization takes over that number of
###                                     minutes the process is spead up.
###    Trysleft        = # trys  --- Number of tries before the procees speeds up 
###                                     to finish faster.
###    SpeedVsAccuracy = #       --- A number 1-10 where 1 means very fast (not that
###                                     accurate with a lower R-squared) and 10 means 
###                                     very accurate but also very slow. 
###    megaprint       = TRUE    --- prints everything in the R Log.  Just a 
###                                     continuous record of where the run is and
###                                     what is happening
###    print           = TRUE    --- Just a limited print of where things are in 
###                                     the run 
###                                     (just enough to see what is happening)
###                                     Also it activates the "timelag" timing 
###                                     see below.
###    timelag         = # sec   --- A number in seconds.  If you set timelag = 10
###                                    then very 10 seconds or so they show you
###                                    where the run is at that time
###    Label          = text     --- A label to put in the output based on the 
###                                     type of optimization.
###  
### The Random optimization will get you 95% towards the best weights/r-squared
###    but it will also be sub-optimal for a small random selection of the
###    weights.  It will also give highly inconsistent weights for elements that
###    have very little effect on the final r-squared.
### To correct these problems, this routine checks every weight and tests if it
###    can be improved to get better r-squared and more consistent results.  It
###    also might lower the r-squared (slightly) to avoid over-fitting.
### Always run this routine even if the random one seems to have done most of the
###    work.  If you are already starting at a close to optimum weights, run 
###    this routine to fine-tune them, without runing the random routine.
  mss                                   <- Sys.time()
  ms9                                   <- 0
  mt1                                   <- mss + timelag
  
  meta                                  <- wobj$meta
  wgts                                  <- wobj$wgts
  modvars                               <- wobj$modvars
  d                                     <- wobj$decomps
  x                                     <- wobj$data
  grps                                  <- wobj$grps
  basemsr                               <- wobj$basemsr
  depvar                                <- wobj$depvar
  depmn                                 <- wobj$depmn
  dep                                   <- wobj$dep
  mrz                                   <- Minutesleft         
  rc                                    <- Trysleft
  sa                                    <- SpeedVsAccuracy
  penalty                               <- Penalty
  if (sa < 1)                sa         <-  1
  if (sa > 10)               sa         <- 10
  sa                                    <- floor(sa + 0.5)
  wn                                    <- nrow(wgts)
  mn                                    <- nrow(meta)
  c0                                    <- 0
  ea                                    <- 0
  go                                    <- 1
  inc                                   <- 0.5
  lim                                   <- 0.01 / (sa * sa * sa)
  inclim                                <- log2(inc / lim)
  incdiv                                <- 2 ^ (1 + (1/18) - (sa / 18))
  guardrail                             <- penalty * 0.015
  mvn                                   <- nrow(modvars)
  gn                                    <- length(grps)
  
  
  wgts$Count                            <- 0
  c1                                    <- 0
  ta                                    <- 0
  while (go == 1) {
    ### Start by adjusting the upper and lower limits based on the step you are on
    ###    and then pick a random weight based on that range (centering it on the
    ###    current best weight).
    ### First estimate how much improvement is significant.  Significant is defined
    ###    as any improvement that is over a certain % (based on the Speed vs
    ###    accuracy input and then Total tries limit)
    i0                                  <- 1
    bests                               <- c(basemsr)
    dis                                 <- sum(abs(wgts$Weight - 1) * wgts$Eff_Wgt)
    inc0                                <- inc
    if (inc0  < 0.01)           inc0    <- 0.01
    dispi                               <- dis      * penalty
    basemsr0                            <- basemsr  + dispi
    bestmsr0                            <- basemsr0
    best0s                              <- c(bestmsr0)
    refs                                <- c(0)
    dirs                                <- c(0)
    wgts$best1                          <- wgts$Weight
    r2                                  <- inclim - log2(inc / lim)
    w0                                  <-  1.167
    w1                                  <-  0.444
    w2                                  <-  0.8
    w3                                  <-  1.27
    w4                                  <- (w0 ^ sa) * w1
    w5                                  <- (w2 ^ r2) * w3
    buff                                <- 1 + (0.006 * w4 * w5)
    for (i in 1:wn) {
      grp                               <- wgts$Group[i]
      val                               <- wgts$Value[i]
      low                               <- wgts$Lower_Weight[i]
      upp                               <- wgts$Upper_Weight[i]
      cur                               <- wgts$Weight[i]
      size                              <- wgts$size[i]
      eff                               <- wgts$Effect[i]
      buf                               <- 1 + (0.00014 * (5 + sa) * (size ^ 0.03))
      pi                                <- eff * sqrt(inc0) * guardrail
      dis                               <- sum(abs(wgts$Weight - 1) * wgts$Eff_Wgt)
      dispi                             <- dis * penalty
      basemsr0                          <- basemsr + dispi
      bestmsr                           <- basemsr
      bestmsr0                          <- basemsr0
      basepi                            <- basemsr0 - pi
      bestj                             <- 0
      for (j in 1:4) {
        meta$Try                        <- meta$Weight
        wgts$Try                        <- wgts$Weight
        no                              <- 0
        j9                              <- 1
        if (j == 1) {
          if (((upp - cur) * inc)  <= lim)    
            no                          <- 1
          new                           <- cur + (inc * (upp - cur))
        } 
        if (j == 2) {
          if (((cur - low) * inc)  <= lim)    
            no                          <- 1
          new                           <- cur - (inc * (cur - low))
          j9                            <- 2
        }
        if (j == 3) {
          if ((abs(1 - cur) * inc) <= lim)
            no                          <- 1
          new                           <- cur - (inc * (cur - 1))
          j9                            <- 3
        }
        if (j == 4) {
          w0                            <- (upp - cur) * inc
          w1                            <- (cur - low) * inc
          if (w0 <= lim)            no  <- 1
          if (w1 <= lim)            no  <- 1
          if (w0 > w1) {
            if ((w0 - w1) <= lim)   no  <- 1
            new                         <- cur + w1
            j9                          <- 4
          } else {
            if ((w1 - w0) <= lim)   no  <- 1
            new                         <- cur - w0
            j9                          <- 5
          }
        }
        if (no == 0) {
          c0                            <- c0 + 1
          ta                            <- ta + 1
          wgts$Try[i]                   <- new
          size0                         <- grps[grps$Group == grp,"size"]
          low0                          <- max(wgts[wgts$Group == grp,"Lower_Weight"] /
                                               wgts[wgts$Group == grp,"Try"])
          upp0                          <- min(wgts[wgts$Group == grp,"Upper_Weight"] /
                                               wgts[wgts$Group == grp,"Try"])
          cen                           <- sum(wgts[wgts$Group == grp,"size"]         *
                                               wgts[wgts$Group == grp,"Try"])         / size0
          cen                           <- 1 / cen
          if (cen < low0)        cen    <- low0
          if (cen > upp0)        cen    <- upp0
          wgts[wgts$Group == grp,"Try"] <- wgts[wgts$Group == grp,"Try"] * cen
          dis                           <- sum(abs(wgts$Try - 1) * wgts$Eff_Wgt)
          dispi                         <- dis * penalty
          # Apply the weights to the meta file
          for (k in 1:wn) {
            if (wgts$Group[k] == grp) {
              grp0                      <- wgts$Group[k]
              val                       <- wgts$Value[k]
              cur0                      <- wgts$Weight[k]
              new0                      <- wgts$Try[k]
              if ((cur0 < (new0 - lim))  | 
                  (cur0 > (new0 + lim))) {
                diff                    <- new0 / cur0
                meta[meta[,grp0] == val,
                     "Try"]             <- meta[meta[,grp0] == val,"Try"] * diff
              }
            }
          }
          # Center the random weights (Short-cut Method)
          for (k in 1:mvn) {
            var                         <- modvars$var[k]
            sz                          <- modvars$size[k]
            meta[meta$Model_Var == var,
                 "centry"]              <- meta$Try[            meta$Model_Var == var]  *
                                           meta$Centered_Weight[meta$Model_Var == var]  /
                                           meta$Weight[         meta$Model_Var == var]
            adj                         <- sum(meta$centry[     meta$Model_Var == var]  *
                                               meta$size[       meta$Model_Var == var]) / sz
            meta[meta$Model_Var == var,
                 "centry"]              <- meta[meta$Model_Var == var,"centry"] / adj
          }
          # Find the Mean Squared Residuals of the new weights
          d$try                         <- 0
          for (k in 1:mn) {
            var                         <- meta$Trans_Variable[k]
            cwgt                        <- meta$Centered_Weight[k]
            twgt                        <- meta$centry[k]
            d$try                       <- d$try + (d[,var] * twgt / cwgt)
          }
          trymn                         <- mean(d$try)
          d$try                         <- d$try       + depmn - trymn
          tryres                        <- d[,dep] - d$try
          trymsr                        <- mean(tryres * tryres)
          trymsr0                       <- trymsr + dispi
          # If short-cut MSR are small enough, redo with longer method
          if ((trymsr0 <= basepi   * buf)  & 
              (trymsr0 <  bestmsr0 * buf)) {
            meta$Mean_Temp_DR           <- (meta$Mean_DR - meta$DR_Adj_Plus) * meta$Try
            meta$W_DR_Adj_Plus          <- meta$DR_Adj_Plus * meta$Try
            
            meta$centry                 <- meta$Try
            
            for (k in 1:mvn) {
              var                       <- modvars$var[k]
              varas                     <- modvars$varas[k]
              size                      <- modvars$size[k]
              sdas                      <- modvars$SD_as[k]
              mnas                      <- modvars$Mean_as[k]
              temp                      <- meta[meta$Model_Var == var,]
              data_vector_as            <- x[,varas]
              cont                      <- sum(temp$Mean_Temp_DR)
              adjc0                     <- sum(temp$W_DR_Adj_Plus)
              data_vector               <- data_vector_as * 0
              adja0                     <- modvars$DR_Adj_Times[k]
              tn                        <- nrow(temp)
              for (h in 1:tn) {
                vardr0                  <- temp$UW_Variable[h]
                wgt                     <- temp$Try[h]
                data_vector0            <- x[,vardr0]  * wgt
                data_vector             <- data_vector + data_vector0
              }  
              data_vector0              <- data_vector
              data_vector               <- data_vector - adjc0
              data_vector               <- data_vector / adja0
              sdt                       <- sd(data_vector)
              mnt                       <- mean(data_vector)
              mnt2                      <- mean(data_vector * data_vector)
              mnad                      <- mean(data_vector * data_vector_as)
              adja                      <- -1
              mnt1                      <- mnt2   - (mnt * mnt)
              mnad1                     <- mnad   - (mnt * mnas)
              if ((mnt1  != 0) &
                  (mnad1 != 0))   adja  <- mnad1 / mnt1
              if (adja <= 0) {
                adja                    <- 1
                if ((sdt  > 0) & 
                    (sdas > 0))   adja  <- sdas  / sdt
              }
              adjb                      <- mnas - (mnt * adja)
              data_vector               <- (data_vector * adja)
              data_vector0              <- data_vector0 - adjc0
              adje                      <- 1
              mn1                       <- mean(data_vector)
              mn0                       <- mean(data_vector0)
              if (mn0 != 0)       adje  <- mn1 / mn0
              temp$adjd2                <- 0
              if (adjb * adjc0 > 0) {
                temp$adjd2              <- temp$W_DR_Adj_Plus * adjb / adjc0
              } else {
                if (cont != 0) {
                  temp$adjd2            <- temp$Mean_Temp_DR  * adjb / cont
                }
              }
              temp$mnt                  <- abs((temp$Mean_Temp_DR * adje) + temp$adjd2)
              temp$wgt1                 <- 1
              temp[(temp$Abs_Mean_DR) > 0.00001,
                   "wgt1"]              <- abs(temp[temp$Abs_Mean_DR > 0.00001,"mnt"])         /
                                           abs(temp[temp$Abs_Mean_DR > 0.00001,"Abs_Mean_DR"])
              for (h in 1:tn) {
                var0                    <- temp$Orig_Variable[h]
                wgt1                    <- temp$wgt1[h]
                meta[meta$Orig_Variable == var0,
                     "centry"]          <- wgt1
              }
            }        
            # Find the Mean Squared Residuals of the new weights
            d$try                       <- 0
            for (k in 1:mn) {
              var                       <- meta$Trans_Variable[k]
              cwgt                      <- meta$Centered_Weight[k]
              twgt                      <- meta$centry[k]
              d$try                     <- d$try + (d[,var] * twgt / cwgt)
            }
            trymn                       <- mean(d$try)
            d$try                       <- d$try       + depmn - trymn
            tryres                      <- d[,dep] - d$try
            trymsr                      <- mean(tryres * tryres)
            trymsr0                     <- trymsr + dispi
            if (trymsr0 <= basepi) {
              cnt                       <- wgts$Count[i]
              if ((trymsr0 <  bestmsr0)   &
                  (cnt     <= (sa+9)))    {
                bestmsr                 <- trymsr
                bestmsr0                <- trymsr0
                bestj                   <- j9
                wgts$best               <- wgts$Try
              }
            }
          }
        }     
      }
      if (bestj > 0) {
        i0                              <- i0 + 1
        var                             <- paste("best",i0,sep="")
        wgts[,var]                      <- wgts$best
        bests                           <- c(bests,bestmsr)
        best0s                          <- c(best0s,bestmsr0)
        refs                            <- c(refs,i)
        dirs                            <- c(dirs,bestj)
      }
    }
    if (i0 == 1) {
      # Calculate Timing and % Done
      inc                               <- inc / incdiv
      wgts$Count                        <- 0
      if (inc < lim)             go     <- 0
      ms8                               <- Sys.time()
      ms9                               <- as.numeric(difftime(ms8,mss,units="mins"))
      r0                                <- 1 - (basemsr / depvar)
      r0                                <- floor((r0 * 100000) + .5) / 1000
      r1                                <- c0 / rc
      r6                                <- r1
      r2                                <- log2(inc / lim)
      r7                                <- r2
      if (r2 <= 0)                r2    <- 0
      r2                                <- 1 - (r2 / inclim)
      r3                                <- ms9 / mrz
      r8                                <- r3
      if (r1 < r2)                r1    <- r2
      if (r1 < r3)                r1    <- r3
      if (r1 >  0.999)            r1    <- .999
      r1                                <- floor((r1 * 10000) + .5) / 100
      if ((r1 > ea) & (print)) {
        r1s                             <- as.character(r1)
        if (r1 < 100) {
          if (r1 < 10)            r1s   <- paste(" ",r1s,sep="")
          r1s                           <- paste(" ",r1s,sep="")
        }
        r1s                             <- substr(paste(r1s,"       ",sep=""),1,7)
        r0s                             <- as.character(r0)
        r0s                             <- substr(paste(r0s,"       ",sep=""),1,6)
        cat(Label,"  -   Estimated R-Squared", r0s,"  -  ",r1s,"Percent Done   -   Mean Squared Residuals",basemsr,"\n")
        e0                              <- floor(r1 / 5) * 5
        ea                              <- e0 + 5
      }
    } else {
      wgts$Try8                         <- wgts$Weight
      for (i in 1:i0) {
        var                             <- paste("best",i,sep="")
        wgts[wgts$Weight <= (wgts[,var] - lim),
             "Try8"]                    <- wgts[wgts$Weight <= (wgts[,var] - lim),var]
        wgts[wgts$Weight >= (wgts[,var] + lim),
             "Try8"]                    <- wgts[wgts$Weight >= (wgts[,var] + lim),var]
      }
      wgts$Try4                         <- (wgts$Weight + wgts$Try8) / 2
      wgts$Try2                         <- (wgts$Weight + wgts$Try4) / 2
      wgts$Try6                         <- (wgts$Try4   + wgts$Try8) / 2
      wgts$Try1                         <- (wgts$Weight + wgts$Try2) / 2
      wgts$Try3                         <- (wgts$Try2   + wgts$Try4) / 2
      wgts$Try5                         <- (wgts$Try4   + wgts$Try6) / 2
      wgts$Try7                         <- (wgts$Try6   + wgts$Try8) / 2
      besti                             <- 0
      for (i in 1:8) {
        var                             <- paste("Try",i,sep="")
        wgts$Try                        <- wgts[,var]
        for (j in 1:gn) {
          grp                           <- grps$Group[j]
          size0                         <- grps$size[j]
          toteff                        <- sum(wgts[wgts$Group == grp,"Effect"])
          low0                          <- max(wgts[wgts$Group == grp,"Lower_Weight"] /
                                               wgts[wgts$Group == grp,"Try"])
          upp0                          <- min(wgts[wgts$Group == grp,"Upper_Weight"] /
                                               wgts[wgts$Group == grp,"Try"])
          cen                           <- sum(wgts[wgts$Group == grp,"size"]         *
                                               wgts[wgts$Group == grp,"Try"])       / size0
          cen                           <- 1 / cen
          if (cen < low0)        cen    <- low0
          if (cen > upp0)        cen    <- upp0
          wgts[wgts$Group == grp,"Try"] <- wgts[wgts$Group == grp,"Try"] * cen
        }  
        wgts[,var]                      <- wgts$Try
        dis                             <- sum(abs(wgts$Try - 1) * wgts$Eff_Wgt)
        dispi                           <- dis * penalty  
        meta$Try                        <- meta$Weight
        c0                              <- c0 + 1
        ta                              <- ta + 1
        for (j in 1:wn) {
          grp                           <- wgts$Group[j]
          val                           <- wgts$Value[j]
          cur                           <- wgts$Weight[j]
          new                           <- wgts$Try[j]
          if ((cur < (new - lim))  |
              (cur > (new + lim))) {
            # Apply the weights to the meta file
            diff                        <- new / cur
            meta[meta[,grp] == val,
                 "Try"]                 <- meta[meta[,grp] == val,"Try"] * diff
          }
        }
        # Center the weights
        for (j in 1:mvn) {
          var                           <- modvars$var[j]
          sz                            <- modvars$size[j]
          meta[meta$Model_Var == var,
               "centry"]                <- meta$Try[            meta$Model_Var == var]  *
                                           meta$Centered_Weight[meta$Model_Var == var]  /
                                           meta$Weight[         meta$Model_Var == var]
          adj                           <- sum(meta$centry[     meta$Model_Var == var]  *
                                               meta$size[       meta$Model_Var == var]) / sz
          meta[meta$Model_Var == var,
               "centry"]                <- meta[meta$Model_Var == var,"centry"] / adj
        }
        # Find the Mean Squared Residuals of the new weights
        d$try                           <- 0
        for (j in 1:mn) {
          var1                          <- meta$Trans_Variable[j]
          cwgt                          <- meta$Centered_Weight[j]
          twgt                          <- meta$centry[j]
          d$try                         <- d$try + (d[,var1] * twgt / cwgt)
        }
        trymn                           <- mean(d$try)
        d$try                           <- d$try       + depmn - trymn
        tryres                          <- d[,dep] - d$try
        trymsr                          <- mean(tryres * tryres)
        trymsr0                         <- trymsr + dispi
        if ((trymsr0 <= basepi   * buff)  &
            (trymsr0 <  bestmsr0 * buff)) {
          meta$Mean_Temp_DR             <- (meta$Mean_DR - meta$DR_Adj_Plus) * meta$Try
          meta$W_DR_Adj_Plus            <- meta$DR_Adj_Plus * meta$Try
          
          meta$centry                   <- meta$Try
          
          for (k in 1:mvn) {
            var                         <- modvars$var[k]
            varas                       <- modvars$varas[k]
            size                        <- modvars$size[k]
            sdas                        <- modvars$SD_as[k]
            mnas                        <- modvars$Mean_as[k]
            temp                        <- meta[meta$Model_Var == var,]
            data_vector_as              <- x[,varas]
            cont                        <- sum(temp$Mean_Temp_DR)
            adjc0                       <- sum(temp$W_DR_Adj_Plus)
            data_vector                 <- data_vector_as * 0
            adja0                       <- modvars$DR_Adj_Times[k]
            tn                          <- nrow(temp)
            for (h in 1:tn) {
              vardr0                    <- temp$UW_Variable[h]
              wgt                       <- temp$Try[h]
              data_vector0              <- x[,vardr0]  * wgt
              data_vector               <- data_vector + data_vector0
            }  
            data_vector0                <- data_vector
            data_vector                 <- data_vector - adjc0
            data_vector                 <- data_vector / adja0
            sdt                         <- sd(data_vector)
            mnt                         <- mean(data_vector)
            mnt2                        <- mean(data_vector * data_vector)
            mnad                        <- mean(data_vector * data_vector_as)
            adja                        <- -1
            mnt1                        <- mnt2   - (mnt * mnt)
            mnad1                       <- mnad   - (mnt * mnas)
            if ((mnt1  != 0) &
                (mnad1 != 0))     adja  <- mnad1 / mnt1
            if (adja <= 0) { 
              adja                      <- 1
              if ((sdt  > 0) & 
                  (sdas > 0))     adja  <- sdas  / sdt
            } 
            adjb                        <- mnas - (mnt * adja)
            data_vector                 <- (data_vector * adja)
            data_vector0                <- data_vector0 - adjc0
            adje                        <- 1
            mn1                         <- mean(data_vector)
            mn0                         <- mean(data_vector0)
            if (mn0 != 0)         adje  <- mn1 / mn0
            temp$adjd2                  <- 0
            if (adjb * adjc0 > 0) {
              temp$adjd2                <- temp$W_DR_Adj_Plus * adjb / adjc0
            } else {
              if (cont != 0) {
                temp$adjd2              <- temp$Mean_Temp_DR * adjb / cont
              }
            }
            temp$mnt                    <- abs((temp$Mean_Temp_DR * adje) + temp$adjd2)
            temp$wgt1                   <- 1
            temp[(temp$Abs_Mean_DR) > 0.00001,
                 "wgt1"]                <- abs(temp[temp$Abs_Mean_DR > 0.00001,"mnt"])         /
              abs(temp[temp$Abs_Mean_DR > 0.00001,"Abs_Mean_DR"])
            for (h in 1:tn) {
              var0                      <- temp$Orig_Variable[h]
              wgt1                      <- temp$wgt1[h]
              meta[meta$Orig_Variable == var0,
                   "centry"]            <- wgt1
            }
          }              
          d$try                         <- 0
          for (j in 1:mn) {
            var1                        <- meta$Trans_Variable[j]
            cwgt                        <- meta$Centered_Weight[j]
            twgt                        <- meta$centry[j]
            d$try                       <- d$try + (d[,var1] * twgt / cwgt)
          }
          trymn                         <- mean(d$try)
          d$try                         <- d$try       + depmn - trymn
          tryres                        <- d[,dep] - d$try
          trymsr                        <- mean(tryres * tryres)
          trymsr0                       <- trymsr + dispi
          if (trymsr0 < bestmsr0) {
            bestmsr                     <- trymsr
            bestmsr0                    <- trymsr0
            besti                       <- i
            wgts$best                   <- wgts$Try
          }
        }
      }
      if (besti > 0) {
        i0                              <- i0 + 1
        var                             <- paste("best",i0,sep="")
        wgts[,var]                      <- wgts$best
        bests                           <- c(bests ,bestmsr)
        best0s                          <- c(best0s,bestmsr0)
        refs                            <- c(refs,1 + wn)
        dirs                            <- c(dirs,besti)
      }
      bestmsr                           <- basemsr
      bestmsr0                          <- basemsr0
      bestr                             <- 0
      bestd                             <- 0
      besti                             <- 0
      for (i in 1:i0) {
        trymsr                          <- bests[i]
        trymsr0                         <- best0s[i]
        if (trymsr0 < bestmsr0) {
          bestmsr                       <- trymsr
          bestmsr0                      <- trymsr0
          bestr                         <- refs[i]
          bestd                         <- dirs[i]
          besti                         <- i
        }
      }
      if (bestr == 0) {
        inc                             <- inc / incdiv
        wgts$Count                      <- 0
        if (inc < lim)           go     <- 0
        ms8                             <- Sys.time()
        ms9                             <- as.numeric(difftime(ms8,mss,units="mins"))
        r0                              <- 1 - (basemsr / depvar)
        r0                              <- floor((r0 * 100000) + .5) / 1000
        r1                              <- c0 / rc
        r6                              <- r1
        r2                              <- log2(inc / lim)
        r7                              <- r2
        if (r2 <= 0)              r2    <- 0
        r2                              <- 1 - (r2 / inclim)
        r3                              <- ms9 / mrz
        if (r1 < r2)              r1    <- r2
        if (r1 < r3)              r1    <- r3
        if (r1 >  0.999)          r1    <- .999
        r1                              <- floor((r1 * 10000) + .5) / 100
        if ((r1 > ea) & (print)) {
          mt2                           <- Sys.time()
          nextprint                     <- FALSE
          if (megaprint)            
            nextprint                   <- TRUE
          if (print & (mt2 > mt1))  
            nextprint                   <- TRUE
          if (nextprint) {
            mt1                         <- mt2 + timelag     
            r1s                         <- as.character(r1)
            if (r1 < 100) {
              if (r1 < 10)        r1s   <- paste(" ",r1s,sep="")
              r1s                       <- paste(" ",r1s,sep="")
            }
            r1s                         <- substr(paste(r1s,"       ",sep=""),1,7)
            r0s                         <- as.character(r0)
            r0s                         <- substr(paste(r0s,"       ",sep=""),1,6)
            cat(Label,"  -   Estimated R-Squared", r0s,"  -  ",r1s,"Percent Done   -   Mean Squared Residuals",basemsr,"\n")
            e0                          <- floor(r1 / 5) * 5
            ea                          <- e0 + 5
          }
        }
      } 
      if (bestr  > 0 ) {
        if (bestr <= wn) {
          var                           <- paste("best",besti,sep="")
          cnt                           <- wgts$Count[bestr] + 1
          wgts$Count[bestr]             <- cnt
        } else {
          var                           <- paste("Try",bestd,sep="")
          for (i in 1:i0) {
            ref                         <- refs[i]
            if ((ref > 0)    & 
                (ref <= wn)) {
              wgts$Count[ref]           <- wgts$Count[ref] + 1
            }
          }
          cnt                           <- 0
        }
        wgts$Try                        <- wgts[,var]
        
        dis                             <- sum(abs(wgts$Try - 1) * wgts$Eff_Wgt)
        dispi                           <- dis * penalty
        basemsr                         <- bestmsr
        basemsr0                        <- bestmsr0
        meta$Try                        <- meta$Weight
        for (k in 1:wn) {
          grp                           <- wgts$Group[k]
          val                           <- wgts$Value[k]
          cur                           <- wgts$Weight[k]
          new                           <- wgts$Try[k]
          if ((cur < (new - lim))  | 
              (cur > (new + lim))) {
            diff                        <- new / cur
            meta[meta[,grp] == val,
                 "Try"]                 <- meta[meta[,grp] == val,"Try"] * diff
          }
        }
        meta$Mean_Temp_DR               <- (meta$Mean_DR - meta$DR_Adj_Plus) * meta$Try
        meta$W_DR_Adj_Plus              <- meta$DR_Adj_Plus * meta$Try
        
        meta$centry                     <- meta$Try
        for (k in 1:mvn) {
          var                           <- modvars$var[k]
          varas                         <- modvars$varas[k]
          size                          <- modvars$size[k]
          sdas                          <- modvars$SD_as[k]
          mnas                          <- modvars$Mean_as[k]
          temp                          <- meta[meta$Model_Var == var,]
          data_vector_as                <- x[,varas]
          cont                          <- sum(temp$Mean_Temp_DR)
          adjc0                         <- sum(temp$W_DR_Adj_Plus)
          data_vector                   <- data_vector_as * 0
          adja0                         <- modvars$DR_Adj_Times[k]
          tn                            <- nrow(temp)
          for (h in 1:tn) {
            vardr0                      <- temp$UW_Variable[h]
            wgt                         <- temp$Try[h]
            data_vector0                <- x[,vardr0]  * wgt
            data_vector                 <- data_vector + data_vector0
          }    
          data_vector0                  <- data_vector
          data_vector                   <- data_vector - adjc0
          data_vector                   <- data_vector / adja0
          sdt                           <- sd(data_vector)
          mnt                           <- mean(data_vector)
          mnt2                          <- mean(data_vector * data_vector)
          mnad                          <- mean(data_vector * data_vector_as)
          adja                          <- -1
          mnt1                          <- mnt2   - (mnt * mnt)
          mnad1                         <- mnad   - (mnt * mnas)
          if ((mnt1  != 0) &
              (mnad1 != 0))        adja <- mnad1 / mnt1
          if (adja <= 0) { 
            adja                        <- 1
            if ((sdt  > 0) & 
                (sdas > 0))        adja <- sdas  / sdt
          } 
          adjb                          <- mnas - (mnt * adja)
          data_vector                   <- (data_vector * adja)
          data_vector0                  <- data_vector0 - adjc0
          adje                          <- 1
          mn1                           <- mean(data_vector)
          mn0                           <- mean(data_vector0)
          if (mn0 != 0)           adje  <- mn1 / mn0
          temp$adjd2                    <- 0
          if (adjb * adjc0 > 0) {
            temp$adjd2                  <- temp$W_DR_Adj_Plus * adjb / adjc0
          } else {
            if (cont != 0) {
              temp$adjd2                <- temp$Mean_Temp_DR * adjb / cont
            }
          }
          temp$mnt                      <- abs((temp$Mean_Temp_DR * adje) + temp$adjd2)
          temp$wgt1                     <- 1
          temp[(temp$Abs_Mean_DR) > 0.00001,
               "wgt1"]                  <- abs(temp[temp$Abs_Mean_DR > 0.00001,"mnt"])         /
            abs(temp[temp$Abs_Mean_DR > 0.00001,"Abs_Mean_DR"])
          for (h in 1:tn) {
            var0                        <- temp$Orig_Variable[h]
            wgt1                        <- temp$wgt1[h]
            meta[meta$Orig_Variable == var0,
                 "centry"]              <- wgt1
          }
        }
        d$try                           <- 0
        for (k in 1:mn) {
          var                           <- meta$Trans_Variable[k]
          cwgt                          <- meta$Centered_Weight[k]
          twgt                          <- meta$centry[k]
          d[,var]                       <- d[,var] * twgt  / cwgt
          d$try                         <- d$try   + d[,var]
        }
        trymn                           <- mean(d$try)
        d$try                           <- d$try       + depmn - trymn
        tryres                          <- d[,dep] - d$try
        trymsr                          <- mean(tryres * tryres)
        trymsr0                         <- trymsr + dispi
        wgts$Weight                     <- wgts$Try
        meta$Centered_Weight            <- meta$centry
        meta$Weight                     <- meta$Try
      }
      ms8                               <- Sys.time()
      ms9                               <- as.numeric(difftime(ms8,mss,units="mins"))
      r0                                <- 1 - (basemsr / depvar)
      r0                                <- floor((r0 * 100000) + .5) / 1000
      r1                                <- c0 / rc
      r6                                <- r1
      r2                                <- log2(inc / lim)
      r7                                <- r2
      if (r2 <= 0)                r2    <- 0
      r2                                <- 1 - (r2 / inclim)
      r3                                <- ms9 / mrz
      if (r1 < r2)                r1    <- r2
      if (r1 < r3)                r1    <- r3
      if (r1 >  0.999)            r1    <- .999
      r1                                <- floor((r1 * 10000) + .5) / 100
      if (bestr > 0) {
        # Note the "Estimate R-Squared" uses a different formula then we normally
        #    use, but it is an indication in how the R-Squared based on any
        #    formula will improve.
        prt                             <- 0
        if (megaprint)             prt  <- 1
        if ((print) & (r1 > ea))   prt  <- 1
        if ((print) & (inc < lim)) prt  <- 1
        if (prt == 1) {
          mt2                           <- Sys.time()
          nextprint                     <- FALSE
          if (megaprint)            
            nextprint                   <- TRUE
          if (print & (mt2 > mt1))  
            nextprint                   <- TRUE
          if (nextprint) {
            mt1                         <- mt2 + timelag          
            r1s                         <- as.character(r1)
            if (r1 < 100) {
              if (r1 < 10)        r1s   <- paste(" ",r1s,sep="")
              r1s                       <- paste(" ",r1s,sep="")
            }
            r1s                         <- substr(paste(r1s,"       ",sep=""),1,7)
            r0s                         <- as.character(r0)
            r0s                         <- substr(paste(r0s,"       ",sep=""),1,6)
            cat(Label,"  -   Estimated R-Squared", r0s,"  -  ",r1s,"Percent Done   -   Mean Squared Residuals",basemsr,"\n")
          }
          e0                            <- floor(r1 / 5) * 5
          ea                            <- e0 + 5
        }
      } else {
        if ((r1 > ea) & (print)) {
          mt2                           <- Sys.time()
          nextprint                     <- FALSE
          if (megaprint)            
            nextprint                   <- TRUE
          if (print & (mt2 > mt1))  
            nextprint                   <- TRUE
          if (nextprint) {
            mt1                         <- mt2 + timelag
            r1s                         <- as.character(r1)
            if (r1 < 100) {
              if (r1 < 10)        r1s   <- paste(" ",r1s,sep="")
              r1s                       <- paste(" ",r1s,sep="")
            }
            r1s                         <- substr(paste(r1s,"       ",sep=""),1,7)
            r0s                         <- as.character(r0)
            r0s                         <- substr(paste(r0s,"       ",sep=""),1,6)
            cat(Label,"  -   Estimated R-Squared", r0s,"  -  ",r1,"Percent Done   -   Mean Squared Residuals",basemsr,"\n")
          }
          e0                            <- floor(r1 / 5) * 5
          ea                            <- e0 + 5
        }
      }
      inc0                              <- inc
      if (ms9  > mrz)             inc   <- inc / sqrt(incdiv)
      if (c0   > rc)              inc   <- inc / sqrt(incdiv)
      if (inc  < lim)             go    <- 0
      if (inc0 > inc)      wgts$Count   <- 0
    }
    c1                                  <- c1 + 1
  }
  wobj$meta                             <- meta
  wobj$wgts                             <- wgts
  wobj$decomps                          <- d
  wobj$basemsr                          <- basemsr
  wobj$numtrys                          <- c0
  return(wobj)
}    



Weights_Optimization                    <- function(obj,
                                                    Seed            =     0,
                                                    Penalty         =     0.5,
                                                    ROpt            =  TRUE,
                                                    FTOpt           =  TRUE,
                                                    UCOpt           = FALSE,
                                                    SpeedVsAccuracy =     5,
                                                    MinutesRun      =     0,
                                                    TotalTrys       =     0,
                                                    RandomTrys      =     0,
                                                    RandomSteps     =     0,
                                                    print           = FALSE,
                                                    megaprint       = FALSE,
                                                    timelag         = 10)    {
### This function has 8 inputs:
###    obj                      --- A list with 10 elements
###                                    data                = The model input data
###                                    meta                = The meta file of all the 
###                                                          high level variables
###                                    wgts                = The weights file of all
###                                                          weighting elements and
###                                                          weights
###                                    full_spec           = the full spec file of all
###                                                          the modeling variables 
###                                                          their related variables
###                                    Panel               = Is this a Panel Model Y or N
###                                    CS                  = An indication of it this is
###                                                          a panel model or not and 
###                                                          how the panels are defined.
###                                    Optional_Scaler     = The Optional_Scaler value
###                                                          was used when the weights
###                                                          were distributed (a number
###                                                          that just makes the coeffients
###                                                          and bayesian priors easier to
###                                                          work with)
###                                    Scale_Types         = The list of where the value
###                                                          of the Optional_Scaler is
###                                                          applied, also set when the
###                                                          weights were first distributed.
###                                    Decomposition_level_panel or
###                                    Decomposition_level = the model decomps on the 
###                                                          variables in the meta file
###                                                          that effect the outcome
###    Seed       = #            --- A number seed for the random numbers used in the 
###                                     random weights algorithm.  Most of the time
###                                     you want seed = 0, so it will pick a seed
###                                     randomly.  But if you want to run the code
###                                     multiple times and get the same random weights
###                                     pick a number you can remember and use that for
###                                     your testing.
###    Penalty    = # (0-1)      --- A number that helps reduce over-fitting of the
###                                     weights and gives more consitant results 
###                                     (meaning the out of the many equally good
###                                     model parameters it will pick close to these
###                                     same ones each time).
###                                     The penalty is also called a "regulator" and
###                                     is commonly used in ridge, lasso, and elastic
###                                     net regression for the same purpose.
###                                     If the penalty is two small (close to 0) the
###                                     coefficients will be very consistant but also
###                                     over-fit drastically,  If penalty is to large
###                                     (1 or greater) all the weights will be "1" and
###                                     there is no point is weighting.
###                                     Hopefully a penalty of about .5 will be just
###                                     the right mix, but you might have to experiment
###                                     some to get it right (try and see)
###    ROpt            = TRUE    --- A TRUE or FALSE statement That controls if the 
###                                     random weights are calculated.   Likely you
###                                     should set this TRUE the first time you
###                                     calculate weights if you have more then 5 
###                                     elements to weights.  After that it won't help
###                                     much.
###    FTOpt           = TRUE    --- A TRUE or FALSE statements that controls if the 
###                                     fine-tuning weights are calculated.  We strongly
###                                     you set this to true every time for both getting
###                                     the best and most consistant weights.
###    UCOpt           = TRUE    --- A TRUE or FALSE statement that controls if the 
###                                     uncontrolled (very over-fitted) weights are
###                                     created.  These weights are kept separately and
###                                     only used to help you test and analyze the real
###                                     weights.  So run this option as TRUE once, and
###                                     keep it in the weight file for when you need it.
###                                     But setting it to TRUE increased the time by
###                                     +50%, so do it only once.
###    SpeedVsAccuracy = #       --- A number 1-10 where 1 means very fast but might
###                                     not be very accurate (lower R-squared) or very
###                                     consistent based on random chance.  The number
###                                     10 means very accurate and consistent every time
###                                     but also very slow.  Any other option (below)
###                                     you leave out will be picked for you based on
###                                     the SpeedVsAccuracy number.
###                                     if SpeedVsAccuracy = 1 (and all the other
###                                     options below default) this function will take
###                                     under 5 minutes.  At SpeedVsAccuracy = 5
###                                     it should take under 15 minutes.  If it = 10
###                                     expect it to take 30 minutes to an hour.
###                                     Note a lot of factors effect the time, so
###                                     the actual time will not be consistent regardless
###                                     of the setting.
###    MinutesRun      = # mins  --- Since the time this takes changes based on subtle
###                                     difference, it is a good idea to set an upper
###                                     limit on how long (in real time minutes) it will
###                                     take.  Note that this is not an absolute upper
###                                     limit, rather if the routine starts to go over
###                                     this limit, it will speed up (with lower accuracy)
###                                     but not stop.  That way you get results, and can
###                                     rerun it again to improve the accuracy.
###    TotalTrys       = # trys  --- A number of trys limit to keep this function from
###                                     running to long.  As in the "MinutesRun" it does
###                                     just stop, but still keeps trying (a little) to
###                                     get results. Set to zero if you don't know what
###                                     value to give.
###    RandomTrys      = # trys  --- Number of random values picked to test in each step 
###                                     of the random algorithm.  Should be 100 - 10,000
###                                     with 1,000 being a good middle value.
###    RandomSteps     = # steps --- Number of steps in testing random values.
###                                     each step will get tighter around the current
###                                     best value and will run for "RandomTrys" weight
###                                     combinationd.   A good run of thumb is
###                                     TotalTrys = RandomTrys * RandomSteps * 2.5
###    megaprint       = TRUE    --- prints everything in the R Log.  Just a 
###                                     continuous record of where the run is and
###                                     what is happening
###    print           = TRUE    --- Just a limited print of where things are in 
###                                     the run 
###                                     (just enough to see what is happening)
###                                     Also it activates the "timelag" timing 
###                                     see below.
###    timelag         = # sec   --- A number in seconds.  If you set timelag = 10
###                                    then very 10 seconds or so they show you
###                                    where the run is at that time
###  
### This function tries to optimize the weights that give the best decomp 
###    R-squared (using the mean-squared residuals as a substitute).
### First the code reduces down the meta, weights, decomp, and data files to
###    only the essential elements and variables.
### Then it creates random weights and tests for the best (how many random
###    weights it tests is up to the user).  It does this in multiple rounds
###    (steps) of random weights closer and closer to the previous best so we
###    are able to slowly improving the weights randomly (how many rounds/steps
###    is again up to the user).
### Finally we take the best random weights and try to incrementally improve
###    things till the weights can't be improved. The incremental improvement
###    is not only trying to improve the accuracy (mean-squared residuals) but
###    also improve the consistency and avoid over-fitting.  So while the
###    R-Squared should improve in this step (or stay about the same) most of
###    the improvements are with consistency and less over-fitting.
### Note, when have tweaked the model and it all seems to work, you should
###    run this code again with the "best" weights you found to improve on the
###    "best" model.  As the weights change, the Bayesian regression improves,
###    which will mean the weights can change slightly again and so on.  With
###    each run you get closer and closer to a final stable model.  And every
###    time you run it the Random algorithm gets less effective and the
###    incremental improvement algorithm is more needed.
  
  
  ### Set up and test the control variables
  ms                                    <- Sys.time()
  mt1                                   <- Sys.time() + timelag
  if (megaprint)                           cat("Optimization Step  1 \n")
  if (print)                               cat("Start the Optimization \n")
  x                                     <- obj$data
  w                                     <- data.frame(obj$full_spec)
  panel                                 <- obj$Panel
  cs                                    <- obj$CS
  meta                                  <- data.frame(obj$meta)
  wgts                                  <- data.frame(obj$weights)
  Optional_Scaler                       <- obj$Optional_Scaler
  Scale_Types                           <- obj$Scale_Types
  
  use_spec                              <- w[w$Include > 0,]
  outmeta                               <- meta
  outwgts                               <- wgts
  penalty                               <- Penalty
  guardrail                             <- penalty * 0.015
  if (Seed > 0)                            set.seed(Seed)  
  opt                                   <- 1
  rt                                    <- RandomTrys
  rs                                    <- RandomSteps
  sa                                    <- SpeedVsAccuracy
  mr                                    <- MinutesRun
  tt                                    <- TotalTrys
  if (megaprint)                print   <- TRUE
  rts                                   <- c(300,400,600,750,1000,1400,1900,2500,3200,4000)
  tts                                   <- c(0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5)
  if (sa  <=  0)               sa       <-   5
  if (sa  <   1)               sa       <-   1
  if (sa  >  10)               sa       <-  10
  sa                                    <- floor(sa + 0.5)
  if  (rs <=   0)              rs       <-  sa + 3
  if  (rs <    1)              rs       <-   1
  if  (rs >   16)              rs       <-  16
  rs                                    <- floor(rs + 0.5)
  if (!ROpt)                   rs       <- 1
  if  (rt <=   0)              rt       <- rts[sa]
  if  (rt <  100)              rt       <- 100
  rt                                    <- floor(rt + 0.5)
  if (!ROpt)                   rt       <- 1
  rq                                    <- rs * rt
  if  (mr <= 0)                mr       <- 10 + (sa * sa / 2)
  if  (mr <  1)                mr       <-  1
  rc                                    <- tt - rq
  if  (rc <= 0) {
    if (ROpt)                  rc       <- rq * tts[sa]
    if (!ROpt)                 rc       <- rts[sa] * 5
  }
  if (!FTOpt)                  rc       <- 1
  rc                                    <- floor(rc + 0.5)
  tt                                    <- rq + rc
  mrz                                   <- mr
  nmx                                   <- names(x)
  omn                                   <- nrow(outmeta)
  own                                   <- nrow(outwgts)
  
  csflag                                <- 0
  if (panel == "Y") {
    csflag                              <- nchar(cs)
    if ((csflag > 0) &
        (length(unique(obj$data[[cs]]))<=1)) {
      csflag                            <- 0
    }
    if  (csflag > 0)    csflag          <- 1
  }
  
  wflag                                 <- 0
  if ("Level" %in% names(w)) {
    mlev                                <- max(w[w$Include>0 ,"Level"])
    nlev                                <- min(w[w$Include>0 ,"Level"])
    elev                                <- max(w[w$Include==1,"Level"])
    if (mlev > nlev) wflag              <- 1
  }
  
  if (wflag == 0) {
    FTOpt                               <- FALSE
    ROpt                                <- FALSE
    c("Error:  The level struture (hyerarchy) of variables makes weighting \n")
    c("        unnecessary.  So no optimization will be done. \n")
    opt                                 <- 0
  }

  if (csflag == 1) {
    d                                   <- obj$Decomposition_level_panel
    ds                                  <- 3
  } else {
    d                                   <- obj$Decomposition_level
    ds                                  <- 2
  }
  
  nmd                                   <- names(d)
  dep                                   <- nmd[ds]
  lev                                   <- paste("_Lev",mlev,sep="")
  vars                                  <- nmd[grepl(lev,nmd)]
  d                                     <- d[,c(nmd[1:ds],vars)]
  levn                                  <- nchar(lev)
  vars                                  <- substr(vars,3,nchar(vars)-levn)
  vn                                    <- length(vars)
  names(d)                              <- c(nmd[1:ds],vars)
  use                                   <- sum(wgts$Use)
  if (use == 0) {
    cat("Error:  The Groups in the Meta file and the Model variables in the Variables (Spec) file \n ")
    cat("        seen together indicate that the weights will have no effect on the model results. \n")
    cat("        The weights in the weight file will therefore not change and no optimization of weights \n")
    cat("        will be done. \n")
    FTOpt                               <- FALSE
    ROpt                                <- FALSE
    opt                                 <- 0
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Optimization Step  2 \n")
  if (nextprint)             mt1        <- mt2 + timelag  
  ### Now start reducing down the files to make things go faster
  
  if (opt == 1) {
    wgts                                <- wgts[(wgts$Use  == 1) &
                                                  (wgts$size != 0),
                                                c("Group","Value","Lower_Weight",
                                                  "Upper_Weight","Weight","size")]
    totsize                             <- sum(wgts$size)
    wgts$size                           <- wgts$size / totsize
    
    wn                                  <- nrow(wgts)
    temp                                <- w[(w$Trans_Variable %in% vars) &
                                               (w$Level          ==   mlev) ,
                                             c("Orig_Variable","Trans_Variable")]
    meta                                <- left_join(temp,meta,by="Orig_Variable")
    
    grps                                <- unique(wgts$Group)
    gn                                  <- length(grps)
    meta                                <- meta[,c("Orig_Variable","Trans_Variable",
                                                   grps,"Weight","Centered_Weight",
                                                   "size","Model_Var")]
    for (i in 1:gn) {
      grp                               <- grps[i]
      temp                              <- unique(wgts$Value[wgts$Group == grp])
      temp0                             <- unique(meta[,grp])
      temp1                             <- setdiff(temp0,temp)
      tn                                <- length(temp1)
      if (tn > 0) 
        meta[meta[,grp] %in% temp1,grp] <- "None"
      if (i == 1)  {
        meta$Group0                     <- meta[,grp]
        none                            <- "None"
      } else {
        meta$Group0                     <- paste(meta$Group0,"@",meta[,grp],sep="")
        none                            <- paste(none,"@","None",sep="")
      }
    }
  }
  
  ### Adjust the data file so you can rework the weights while optimizing.
  
  if (opt == 1) {
    
    un                                  <- nrow(use_spec)

    done                                <- c("Intercept")
    
    for (i in 1:un) {
      var                               <- use_spec$Orig_Variable[i]
      vart                              <- use_spec$Trans_Variable[i]
      varm                              <- use_spec$Model_Var[i]
      lev                               <- use_spec$Level[i]
      prewgt                            <- use_spec$PreWeight[i]
      type                              <- use_spec$Variable_Type[i]
      adja0                             <- use_spec$DR_Adj_Times[i]
      adja1                             <- use_spec$Var_Adj_Times[i]
      adjb0                             <- use_spec$DR_Adj_Plus[i]
      adjb1                             <- use_spec$Var_Adj_Plus[i]
      wgt                               <- use_spec$Centered_Weight[i]
      wgt0                              <- use_spec$Raw_Weight[i]
      varm                              <- use_spec$Model_Var[i]
      varas                             <- paste(var,"_as",sep="")
      if (!(varas %in% nmx))     varas  <- var
      vardr                               <- paste(var,"_uw",sep="")
      if (wflag == 0)            vardr  <- vart
      if (!(vardr %in% nmx))     vardr  <- varas
      if (!(vart  %in% nmx))     vart   <- vardr
      if (vart == vardr) {
        adja1                           <- adja0
        adjb1                           <- adjb0
      }
      if (is.na(wgt))            wgt    <- 1
      if (!(var %in% done)) {
        if (type %in% Scale_Types) 
          x[,vart]                      <-  x[,vart] * Optional_Scaler
        x[,vart]                        <- (x[,vart] * prewgt) + adjb1
        x[(abs(x[,vart])  < 0.00000001) &
            (abs(x[,varas]) < 0.00000001),
          vart]                         <- 0
        done                            <- c(done,var)
      }
    }
  }  
  
  ### Continue to reduce down the meta file and also reduce the variables in the
  ###    decomp file (again, to speed up the optimization)
  
  if (opt == 1) {  
    
    meta[(meta$Group0     == none)       & 
           (meta$Model_Var != "Intercept"),
         "Model_Var"]                   <- "All_Other"
    meta$Ones                           <- 1
    meta$Group0                         <- paste(meta$Group0,"@",meta$Model_Var,sep="")
    temp                                <- meta[,c("Group0","size","Ones")]
    temp$Weight0                        <- meta$size * meta$Weight
    temp$Centered_Weight0               <- meta$size * meta$Centered_Weight
    temp                                <- data.frame(aggregate(temp[,
                                                                     c("size","Ones","Weight0","Centered_Weight0")],
                                                                by=list(temp$Group0),FUN=sum))
    names(temp)                         <- c("Group0","size","Ones","Weight","Centered_Weight")
    temp$DR_Adj_Plus                    <- 0
    temp$Weight                         <- temp$Weight          / temp$size
    temp$Centered_Weight                <- temp$Centered_Weight / temp$size
    temp$DR_Adj_Plus                    <- 0
    tn                                  <- nrow(temp)
    
    vars                                <- c("Orig_Variable","Trans_Variable",grps,"size","Model_Var")
    mlevvars                            <- meta[,vars]
    mlevvars$Meta_Variable              <- mlevvars$Orig_Variable
    mlevvars$AS_Variable                <- mlevvars$Orig_Variable
    mlevvars$UW_Variable                <- mlevvars$Orig_Variable
    mlevvars$DR_Adj_Plus                <- 0
    temp$AS_Variable                    <- ""
    temp$UW_Variable                    <- ""
    temp$DR_Adj_Plus                    <- 0
    vn                                  <- gn + 4
    dz                                  <- d
    for (i in 1:tn) {
      grp                               <- temp$Group0[i]
      num                               <- temp$Ones[i]
      temp0                             <- meta[meta$Group0 == grp,]
      for (j in 1:vn) {
        var1                            <- vars[j]
        val                             <- temp0[1,c(var1)]
        temp[i,var1]                    <- val
      }
      var                               <- temp$Orig_Variable[i]
      adjb                              <- w$DR_Adj_Plus[(w$Orig_Variable == var) &
                                                           (w$Level         == mlev)]
      prewgt0                           <- w$PreWeight[  (w$Orig_Variable == var) &
                                                           (w$Level         == mlev)]
      mlevvars[mlevvars$Orig_Variable == var,
               "DR_Adj_Plus"]           <- adjb
      if (num > 1) {
        temp$Orig_Variable[i]           <- paste(temp0$Orig_Variable[1],"_Plus",sep="")
        temp$Trans_Variable[i]          <- paste(temp0$Orig_Variable[1],"_Plus_t",sep="")
        var                             <- temp$Orig_Variable[i]
        vart                            <- temp$Trans_Variable[i]
        varas                           <- paste(var,"_as",sep="")
        vardr                           <- paste(var,"_uw",sep="")
        temp$UW_Variable[i]             <- vardr
        temp$AS_Variable[i]             <- varas
        d[,vart]                        <- 0
        x[,var]                         <- 0
        x[,vart]                        <- 0
        x[,varas]                       <- 0
        x[,vardr]                       <- 0
        adjz                            <- 0
        for (j in 1:num) {
          var0                          <- temp0$Orig_Variable[j]
          vart0                         <- temp0$Trans_Variable[j]
          adjb                          <- w$DR_Adj_Plus[(w$Orig_Variable == var0) & 
                                                         (w$Level         == mlev)]
          prewgt                        <- w$PreWeight[  (w$Orig_Variable == var0) &
                                                         (w$Level         == mlev)]
          mlevvars[mlevvars$Orig_Variable == var0,
                   "Meta_Variable"]     <- var
          mlevvars[mlevvars$Orig_Variable == var0,
                   "DR_Adj_Plus"]       <- adjb
          varas0                        <- paste(var0,"_as",sep="")
          if (!(varas0 %in% nmx)) 
            varas0                      <- var0
          vardr0                        <- paste(var0,"_uw",sep="")
          if (!(vardr0 %in% nmx))
            vardr0                      <- varas0
          mlevvars[mlevvars$Orig_Variable == var0,
                   "AS_Variable"]       <- varas0
          mlevvars[mlevvars$Orig_Variable == var0,
                   "UW_Variable"]       <- vardr0
          mlevvars[mlevvars$Orig_Variable == var0,
                   "DR_Adj_Plus"]       <- adjb
          d[,vart]                      <- d[,vart] + d[,vart0]
          d[,var0]                      <- NULL
          x[,var]                       <- x[,var] + x[,var0]
          data_vector_as                <- x[,varas0]
          if (varas0 == var0) 
            data_vector_as              <- data_vector_as * prewgt
          x[,varas]                     <- x[,varas] + data_vector_as
          data_vector_uw                <- x[,vardr0]
          if (vardr0 == varas0) 
            data_vector_uw              <- data_vector_as
          x[,vardr]                     <- x[,vardr] + data_vector_uw
          x[,vart]                      <- x[,vart]  + x[,vart0]
          adjz                          <- adjz + adjb
        }
      } else {
        adjz                            <- w$DR_Adj_Plus[(w$Orig_Variable == var) & 
                                                           (w$Level         == mlev)]
        varas                           <- paste(var,"_as",sep="")
        if (!(varas %in% nmx))   varas  <- var
        vardr                           <- paste(var,"_uw",sep="")
        if (!(vardr %in% nmx))   vardr  <- varas
        temp$AS_Variable[i]             <- varas
        temp$UW_Variable[i]             <- vardr
        mlevvars[mlevvars$Orig_Variable == var,
                 "AS_Variable"]         <- varas
        mlevvars[mlevvars$Orig_Variable == var,
                 "UW_Variable"]         <- vardr
      }
      prewgt                            <- 1
      if (vardr == var)      prewgt     <- prewgt0
      temp$DR_Adj_Plus[i]               <- adjz
      temp$Mean_DR[i]                   <-     mean(x[,vardr] * prewgt)
      temp$Abs_Mean_DR[i]               <- abs(mean(x[,vardr] * prewgt))
    }
    
    meta                                <- temp[,c("Orig_Variable","Trans_Variable",
                                                   grps,"Weight","Centered_Weight",
                                                   "size","Model_Var","DR_Adj_Plus",
                                                   "AS_Variable","UW_Variable",
                                                   "Mean_DR","Abs_Mean_DR")]
    modvars                             <- meta[,c("Model_Var","size")]
    modvars                             <- data.frame(aggregate(modvars$size,
                                                                by=list(modvars$Model_Var),
                                                                FUN=sum))
    names(modvars)                      <- c("var","size")
    modvars$numsub                      <- 0
    mvn                                 <- nrow(modvars)
    modvars$vart                        <- 0
    modvars$varas                       <- 0
    modvars$vardr                       <- 0
    modvars$DR_Adj_Plus                 <- 0
    modvars$Mean_as                     <- 0
    for (i in 1:mvn) {
      var                               <- modvars$var[i]
      temp                              <- mlevvars[mlevvars$Model_Var == var  ,
                                                    c("Orig_Variable","Trans_Variable")]
      num                               <- nrow(temp)
      modvars$numsub[i]                 <- num
      varas                             <- paste(var,"_as",sep="")
      vardr                             <- paste(var,"_uw",sep="")
      vart                              <- paste(var,"_t" ,sep="")
      if (var %in% nmx) {
        prewgt                          <- w$PreWeight[   (w$Orig_Variable == var) &
                                                          (w$Level         == elev)]
        if (!(varas %in% nmx))   varas  <- var
        if (!(vardr %in% nmx))   vardr  <- varas
        adjz                            <- w$DR_Adj_Plus[ (w$Orig_Variable == var) &
                                                          (w$Level         == elev)]
        adja                            <- w$DR_Adj_Times[(w$Orig_Variable == var) &
                                                          (w$Level         == elev)]
      } else {
        perwgt                          <- 1
        x[,var]                         <- 0
        x[,vart]                        <- 0
        x[,varas]                       <- 0
        x[,vardr]                       <- 0
        x$temp                          <- 0
        adjz                            <- 0
        for (j in 1:num) {
          var0                          <- temp$Orig_Variable[j]
          vart0                         <- temp$Trans_Variable[j]
          adja                          <- w$DR_Adj_Times[(w$Orig_Variable == var0) & 
                                                          (w$Level         == mlev)]
          adjb                          <- w$DR_Adj_Plus[ (w$Orig_Variable == var0) & 
                                                          (w$Level         == mlev)]
          prewgt0                       <- w$PreWeight[   (w$Orig_Variable == var0) &
                                                          (w$Level         == mlev)]
          varas0                        <- paste(var0,"_as",sep="")
          if (!(varas0 %in% nmx)) 
            varas0                      <- var0
          vardr0                        <- paste(var0,"_uw",sep="")
          if (!(vardr0 %in% nmx)) 
            vardr0                      <- varas0
          x[,var]                       <- x[,var] + x[,var0]
          data_vector_as                <- x[,varas0]
          if (varas0 == var0) 
            data_vector_as              <- data_vector_as * prewgt0
          x[,varas]                     <- x[,varas] + data_vector_as
          data_vector_uw                <- x[,vardr0]
          if (vardr0 == varas0) 
            data_vector_uw              <- data_vector_as
          x[,vardr]                     <- x[,vardr] + data_vector_uw
          data_vector_uw                <- data_vector_uw - adjb0
          data_vector_uw                <- data_vector_uw / adja0
          x$temp                        <- x$temp    + data_vector_uw
          x[,vart]                      <- x[,vart]  + x[,vart0]
          adjz                          <- adjz + adjb
        }
        mndr                            <- mean(x$temp)
        adja                            <- 1
        if (mndr != 0)          adja    <- mean(x[,vardr] - adjz) / mndr
      }
      print
      modvars$vart[i]                   <- vart
      modvars$varas[i]                  <- varas
      modvars$vardr[i]                  <- vardr
      modvars$DR_Adj_Plus[i]            <- adjz
      modvars$DR_Adj_Times[i]           <- adja
      modvars$Mean_as[i]                <- mean(x[,varas])
      if (varas == var)
        modvars$Mean_as[i]              <- mean(x[,varas] * prewgt)
      modvars$SD_as[i]                  <- sd(  x[,varas])
      if (varas == var)
        modvars$SD_as[i]                <- sd(  x[,varas] * prewgt)
    }
    mln                                 <- nrow(mlevvars)
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Optimization Step  3 \n")
  if (nextprint)             mt1        <- mt2 + timelag  
  ### To start the real work,  we create a baseline of what the mean squared
  ###    residuals currently are (Before we improve them by optimization).
  
  if (opt == 1) {
    mn                                  <- nrow(meta)
    d$base                              <- 0
    for (i in 1:mn) {
      var                               <- meta$Trans_Variable[i]
      d$base                            <- d$base + d[,var]
    }
    depmn                               <- mean(d[,dep])
    trymn                               <- mean(d$base)
    d$Intercept                         <- d$Intercept   + depmn - trymn
    d$base                              <- d$base        + depmn - trymn
    baseres                             <- d[,dep]       - d$base
    basemsr                             <- mean(baseres  * baseres)
    depvar                              <- sd(  d[,dep]) * sd(d[,dep])
    grps                                <- unique(wgts$Group)
    gn                                  <- length(grps)
    grps                                <- data.frame(Group=grps)
    grps$size                           <- 1
    for (i in 1:gn) {
      grp                               <- grps$Group[i]
      grps$size[i]                      <- sum(wgts[wgts$Group == grp,"size"])
    }
    
    ms0                                 <- Sys.time()
    ms9                                 <- as.numeric(difftime(ms0,ms,units="mins"))
    #if (megaprint) cat("Setup   -   Time in Minutes =",ms9,"\n")
    mrz                                 <- mr - ms9
  }
  
  meta$Try                              <- meta$Weight
  meta                                  <- meta[order(meta$Model_Var),]
  meta$Mean_Temp_DR                     <- (meta$Mean_DR - meta$DR_Adj_Plus) * meta$Try
  meta$W_DR_Adj_Plus                    <- meta$DR_Adj_Plus * meta$Try
  
  meta[meta$Model_Var == "None",
       "Centered_Weight"]               <- 1
  
  wgts$Start_Weight                     <- wgts$Weight
  
  wobj                                  <- list()
  wobj$meta                             <- meta
  wobj$wgts                             <- wgts
  wobj$modvars                          <- modvars
  wobj$decomps                          <- d
  wobj$data                             <- x
  wobj$grps                             <- grps
  wobj$basemsr                          <- basemsr
  wobj$depvar                           <- depvar
  wobj$depmn                            <- depmn
  wobj$dep                              <- dep
  Minutesleft                           <- mrz
  RandomTrys                            <- rt
  RandomSteps                           <- rs
  SpeedVsAccuracy                       <- sa
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Optimization Step  4 \n")
  if (print)                               cat("Try random weights to get the best weights \n")
  
  if ((opt == 1) & (ROpt)) {
    
    wobj                                <- Random_Weights_CMAES_Opt(wobj,
                                                                    Minutesleft     = Minutesleft,
                                                                    RandomTrys      = RandomTrys,
                                                                    RandomSteps     = RandomSteps,
                                                                    SpeedVsAccuracy = SpeedVsAccuracy,
                                                                    print           = print,
                                                                    megaprint       = megaprint,
                                                                    timelag         = timelag)
    
    meta                                <- wobj$meta
    wgts                                <- wobj$wgts
    d                                   <- wobj$decomps
    basemsr                             <- wobj$basemsr
    wgts$RanWeight                      <- wgts$Weight
    numtrys                             <- wobj$numtrys
    ms1                                 <- Sys.time()
    ms9                                 <- as.numeric(difftime(ms1,ms0,units="mins"))
    if (print) cat("Random Opt   -   Time in Minutes =",ms9,"   -   And",numtrys,"weights tested. \n")
    mrz                                 <- mrz - ms9
    
  }
  
  
  mt1                                   <- Sys.time() + timelag
  if (megaprint)                           cat("Optimization Step  5 \n")
  if (print)                               cat("Now fine tune the weights for accuracy and consistancy \n")
  
  ### Next we use a "Incremental Improvement" optimization.  It is more likely
  ###    to find improvements to the Mean Squared Error, but will mostly look for
  ###    univariate local minimums, so might not find the "best" weights if 
  ###    complex interactions exist (the random weights hopefully will).
  ### It starts with a step to find an estimate of the standard deviation of
  ###    weighting changes.  This way it can avoid over-fitting the weighting.
  ###    We also Avoid over-fitting and create consistency by a applying a lasso
  ###    like penalty to the weighting centered at a weight of one.
  
  if (opt == 1) {
    
    wgts$Effect                         <- 0
    
    for (i in 1:3)  {
      if (i == 1)        wgts$TempWgt   <-  wgts$Weight
      if (i == 2)        wgts$TempWgt   <-  1
      if (i == 3)        wgts$TempWgt   <-  wgts$Lower_Weight + wgts$Upper_Weight - wgts$Weight 
      for (j in 1:wn) {
        grp                             <- wgts$Group[j]
        val                             <- wgts$Value[j]
        low                             <- wgts$Lower_Weight[j]
        upp                             <- wgts$Upper_Weight[j]
        cur                             <- wgts$TempWgt[j]
        size                            <- wgts$size[j]
        eff                             <- wgts$Effect[j]
        if (size < 0.001)       size    <- 0.001
        
        topmsr                          <- basemsr
        botmsr                          <- basemsr
        topk                            <- 0
        botk                            <- 0
        k0                              <- 0
        k1                              <- 999
        for (k in 1:5) {
          meta$Try                      <- meta$Weight
          wgts$Try                      <- wgts$TempWgt
          if (k == 1)           new     <-  low 
          if (k == 2)           new     <- (low + 1) / 2 
          if (k == 3)           new     <-        1 
          if (k == 4)           new     <- (upp + 1) / 2 
          if (k == 5)           new     <-  upp
          k2                            <- abs(new - cur)
          if (k1 > k2) {
            k1                          <- k2
            k0                          <- k
          }
          wgts$Try[j]                   <- new
          diff                          <- new / cur
          meta[meta[,grp] == val,"Try"] <- meta[meta[,grp] == val,"Try"] * diff
          # Center the random weights (Longer Method)
          meta$Mean_Temp_DR             <- (meta$Mean_DR - meta$DR_Adj_Plus) * meta$Try
          meta$W_DR_Adj_Plus            <- meta$DR_Adj_Plus * meta$Try
          meta$centry                   <- meta$Try
          for (k in 1:mvn) {
            var                         <- modvars$var[k]
            varas                       <- modvars$varas[k]
            size                        <- modvars$size[k]
            sdas                        <- modvars$SD_as[k]
            mnas                        <- modvars$Mean_as[k]
            temp                        <- meta[meta$Model_Var == var,]
            data_vector_as              <- x[,varas]
            cont                        <- sum(temp$Mean_Temp_DR)
            adjc0                       <- sum(temp$W_DR_Adj_Plus)
            data_vector                 <- data_vector_as * 0
            adja0                       <- modvars$DR_Adj_Times[k]
            tn                          <- nrow(temp)
            for (h in 1:tn) {
              vardr0                    <- temp$UW_Variable[h]
              wgt                       <- temp$Try[h]
              data_vector0              <- x[,vardr0]  * wgt
              data_vector               <- data_vector + data_vector0
            }  
            data_vector0                <- data_vector
            data_vector                 <- data_vector - adjc0
            data_vector                 <- data_vector / adja0
            sdt                         <- sd(data_vector)
            mnt                         <- mean(data_vector)
            mnt2                        <- mean(data_vector * data_vector)
            mnad                        <- mean(data_vector * data_vector_as)
            adja                        <- -1
            mnt1                        <- mnt2   - (mnt * mnt)
            mnad1                       <- mnad   - (mnt * mnas)
            
            
            if ((mnt1  != 0) &
                (mnad1 != 0))      adja <- mnad1 / mnt1
            if (adja <= 0) {
              adja                      <- 1
              if ((sdt  > 0) & 
                  (sdas > 0))      adja <- sdas  / sdt
            }
            adjb                        <- mnas - (mnt * adja)
            data_vector                 <- (data_vector * adja)
            data_vector0                <- data_vector0 - adjc0
            adje                        <- 1
            mn1                         <- mean(data_vector)
            mn0                         <- mean(data_vector0)
            if (mn0 != 0)          adje <- mn1 / mn0
            temp$adjd2                  <- 0
            if (adjb * adjc0 > 0) {
              temp$adjd2                <- temp$W_DR_Adj_Plus * adjb / adjc0
            } else {
              if (cont != 0) {
                temp$adjd2              <- temp$Mean_Temp_DR * adjb / cont
              }
            }
            temp$mnt                    <- abs((temp$Mean_Temp_DR * adje) + temp$adjd2)
            temp$wgt1                   <- 1
            temp[(temp$Abs_Mean_DR) > 0.00001,
                 "wgt1"]                <- abs(temp[temp$Abs_Mean_DR > 0.00001,"mnt"])         /
              abs(temp[temp$Abs_Mean_DR > 0.00001,"Abs_Mean_DR"])
            for (h in 1:tn) {
              var0                      <- temp$Orig_Variable[h]
              wgt1                      <- temp$wgt1[h]
              meta[meta$Orig_Variable == var0,
                   "centry"]            <- wgt1
            }
          }
          # Find the Mean Squared Residuals of the new weights
          d$try                         <- 0
          for (h in 1:mn) {
            var                         <- meta$Trans_Variable[h]
            cwgt                        <- meta$Centered_Weight[h]
            twgt                        <- meta$centry[h]
            d$try                       <- d$try + (d[,var] * twgt / cwgt)
          }
          trymn                         <- mean(d$try)
          d$try                         <- d$try       + depmn - trymn
          tryres                        <- d[,dep] - d$try
          trymsr                        <- mean(tryres * tryres)
          if (trymsr > topmsr)   topk   <- k
          if (trymsr < botmsr)   botk   <- k
          if (trymsr > topmsr)   topmsr <- trymsr
          if (trymsr < botmsr)   botmsr <- trymsr
        }
        rng                             <- topmsr - botmsr
        if (rng > eff)   wgts$Effect[j] <- rng
        if (botk == 0)           botk   <- k0
      }
    }
    
    TotEffect                           <- sum(wgts$Effect)
    wgts$Eff_Wgt                        <- sqrt(wgts$Effect)
    toteff                              <- sum(wgts$Eff_Wgt)
    wgts$Eff_Wgt                        <- wgts$Eff_Wgt / toteff
    
    ms2                                 <- Sys.time()
    ms9                                 <- as.numeric(difftime(ms2,ms1,units="mins"))
    mrz                                 <- mrz - ms9
    
  }
  
  mt2                                   <- Sys.time()
  nextprint                             <- FALSE
  if (megaprint)             nextprint  <- TRUE
  if (print & (mt2 > mt1))   nextprint  <- TRUE
  if (nextprint)                           cat("Optimization Step  6 \n")
  if (nextprint)             mt1        <- mt2 + timelag  
  
  wobj$meta                             <- meta
  wobj$wgts                             <- wgts
  wobj$modvars                          <- modvars
  wobj$decomps                          <- d
  wobj$data                             <- x
  wobj$grps                             <- grps
  wobj$basemsr                          <- basemsr
  wobj$depvar                           <- depvar
  wobj$depmn                            <- depmn
  wobj$dep                              <- dep
  Minutesleft                           <- mrz
  Trysleft                              <- rc
  SpeedVsAccuracy                       <- sa
  
  if (UCOpt) {
    if (print)                             cat("Create uncontrolled weights for comparison and to help prevent over-fitting \n")
    uobj                                <- Incremental_Improvement_Opt(wobj,
                                                                       Penalty         =     0,
                                                                       Minutesleft     = Minutesleft,
                                                                       Trysleft        = Trysleft,
                                                                       SpeedVsAccuracy =     5,
                                                                       print           = print,
                                                                       megaprint       = megaprint,
                                                                       timelag         = timelag,
                                                                       Label           = "Uncontrol Opt")
    wgts                                <- wobj$wgts
    wgts$Uncontrolled_Weight            <- uobj$wgts$Weight
    numtrys                             <- uobj$numtrys
    wobj$wgts                           <- wgts
    ms3                                 <- Sys.time()
    ms9                                 <- as.numeric(difftime(ms3,ms2,units="mins"))
    if (print) cat("Uncontrol Opt   -   Time in Minutes =",ms9,"   -   And",numtrys,"weights tested. \n")
  }
  
  ms3                                   <- Sys.time()
  
  if ((opt == 1) & (FTOpt)) {
    if (print)                             cat("Now use a Incremental Improvement algorithm to improve consistancy and accuracy \n")
    if (nextprint)                         cat("Optimization Step  7 \n")
    
    wobj                                <- Incremental_Improvement_Opt(wobj,
                                                                       Minutesleft     = Minutesleft,
                                                                       Trysleft        = Trysleft,
                                                                       SpeedVsAccuracy = SpeedVsAccuracy,
                                                                       Penalty         = penalty,
                                                                       print           = print,
                                                                       megaprint       = megaprint,
                                                                       timelag         = timelag,
                                                                       Label           = "Fine-Tune Opt")
    
    meta                                <- wobj$meta
    wgts                                <- wobj$wgts
    d                                   <- wobj$decomps
    basemsr                             <- wobj$basemsr
    numtrys                             <- wobj$numtrys
    ms4                                 <- Sys.time()
    ms9                                 <- as.numeric(difftime(ms4,ms3,units="mins"))
    if (print) cat("Fine Tune Opt   -   Time in Minutes =",ms9,"   -   And",numtrys,"weights tested. \n")
    mrz                                 <- mrz - ms9
    
  }
  
  mt1                                   <- Sys.time() + timelag
  if (megaprint)                           cat("Optimization Step  8 \n")
  if (print)                               cat("Finalize the Optimization and save the results \n")
  
  ### Now we move the new weightings into are main weighting and meta file
  ###    (so we can distribute it over all the levels of data in the last steps).
  nmow                                  <- names(outwgts)
  nmw                                   <- names(wgts)
  nmom                                  <- names(outmeta)
  nmm                                   <- names(meta)
  vars                                  <- outmeta$Orig_Variable
  outwgts$Start_Weight                  <- outwgts$Weight
  if (!("Effect" %in% nmow))
    outwgts$Effect                      <- 0
  uwgtf                                 <- 0
  if ("Uncontrolled_Weight" %in% nmw) 
    uwgtf                               <- 1
  if (uwgtf == 1)
    outwgts$Uncontrolled_Weight         <- 1
  outwgts$RanWeight                     <- 1
  for (i in 1:wn) {
    grp                                 <- wgts$Group[i]
    val                                 <- wgts$Value[i]
    wgt                                 <- wgts$Weight[i]
    eff                                 <- wgts$Effect[i]
    if (uwgtf == 1) 
      uwgt                              <- wgts$Uncontrolled_Weight[i]
    rwgt                                <- wgts$RanWeight[i]
    outwgts[(outwgts$Group   == grp) & 
            (outwgts$Value   == val),
            "Weight"]                   <- wgt
    outwgts[(outwgts$Group   == grp) & 
            (outwgts$Value   == val),
            "Effect"]                   <- eff
    if (uwgtf == 1)
      outwgts[(outwgts$Group == grp) & 
              (outwgts$Value == val),
              "Uncontrolled_Weight"]    <- uwgt
    outwgts[(outwgts$Group   == grp) & 
              (outwgts$Value   == val),
            "RanWeight"]                <- rwgt
  }
  
  for (i in 1:mn) {
    var                                 <- meta$Orig_Variable[i]
    wgt                                 <- meta$Weight[i]
    cwgt                                <- meta$Centered_Weight[i]
    if (var %in% vars) {
      outmeta[outmeta$Orig_Variable == var,
              "Weight"]                 <- wgt
      outmeta[outmeta$Orig_Variable == var,
              "Centered_Weight"]        <- cwgt
      
    } else {
      temp                              <- mlevvars[mlevvars$Meta_Variable == var,]
      tn                                <- nrow(temp)
      for (j in 1:tn) {
        var0                            <- temp$Orig_Variable[j]
        outmeta[outmeta$Orig_Variable == var0,
                "Weight"]               <- wgt
        outmeta[outmeta$Orig_Variable == var0,
                "Centered_Weight"]      <- cwgt
      }
    }
  }
  
  obj$weights                           <- outwgts
  obj$meta                              <- outmeta
  use_spec                              <- w[w$Include > 0,]
  full_spec                             <- w
  spec                                  <- w[w$Include == 1,]
  obj$data                              <- x
  obj$spec                              <- spec
  obj$full_spec                         <- full_spec
  ms5                                   <- Sys.time()
  ms9                                   <- as.numeric(difftime(ms5,ms,units="mins"))
  if (print) cat("All of Opt Code -   Time in Minutes =",ms9,"\n")
  return(obj)  
  
}