
########## Extracting and preparing PISA 2015 data for response time modeling analysis
## extracting a reading (or other) cluster from a country (canada)
source("01_functions/realdata_preparation.R")
source("01_functions/tools.R")

# 01) prepare data ------------------------------------------------------------
### load list of reading items
allItems <- read.csv("c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/math_items.csv", header = T,
                      stringsAsFactors = FALSE)
allItems <- allItems[, 1]

### load data
dat <- readRDS("c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_canada_engl.rds")
dim(dat)

### inspect data
table(dat$BOOKID)
table(dat$ADMINMODE)
table(dat$Option_UH)
table(dat$Option_Read)
table(dat$Option_Math)
table(dat$Option_CPS)
table(dat$Option_FL)
# no pba booklets were used in canada, no un heure and no easier versions!
table(dat$LANGTEST_COG)
# all in English!

## drop irrelevant variables (background)
pisa <- dat[, -c(1:20)]

### pick persons with booklets with a certain item cluster
## math items
forms <- list()
forms[["form_M01"]] <- c(43, 48, 49, 53, 55, 61, 67, 73) # M01
forms[["form_M02"]] <- c(43, 44, 50, 54, 56, 62, 68, 74)
forms[["form_M03"]] <- c(44, 45, 49, 51, 57, 63, 69, 75)
forms[["form_M04"]] <- c(45, 46, 50, 52, 58, 64, 70, 76)
forms[["form_M05"]] <- c(46, 47, 51, 53, 59, 65, 71, 77)
forms[["form_M06ab"]] <- c(47, 48, 52, 54, 60, 66, 72, 78)

## reading items
# itemcluster R01 (p.38, PISA 2015) in form 31, 36, 37, 41
forms <- list()
forms[["form_R01"]] <- c(31, 36, 37, 41, 55, 61, 79, 85)
forms[["form_R02"]] <- c(31, 32, 38, 42, 56, 62, 80, 86)
forms[["form_R03"]] <- c(32, 33, 37, 39, 57, 63, 81, 87)
forms[["form_R04"]] <- c(33, 34, 38, 40, 58, 64, 82, 88)
forms[["form_R05"]] <- c(34, 35, 39, 41, 59, 65, 83, 89)
forms[["form_R06ab"]] <- c(35, 36, 40, 42, 60, 66, 84, 90)


"CR404Q10T"
table(is.na(pisa$DR404Q10AC))
table(is.na(pisa$DR404Q10BC))


par(mfrow = c(2, 3))

subDat_list <- lapply(forms, function(forms) {
  pisa <- pisa[pisa$BOOKID %in% forms, ]
  # table(pisa$BOOKID)
  
  # 02) find items with few NAs with people having these booklets -------------------------------------------
  ### count NAs per item
  NAcount <- by(data = pisa, INDICES = pisa$BOOKID, countNAs)
  hist(unlist(NAcount), ylim = c(0, 200))
  
  ## check form 2 for math
  lapply(NAcount, function(x) x["CM192Q01T"] )
  lapply(NAcount, function(x) x["CM192Q01A"] )
  lapply(NAcount, function(x) x["CM192Q01S"] )
  
  ## check form 5 for reading
  lapply(NAcount, function(x) x["DR404Q10AC"] )
  lapply(NAcount, function(x) x["DR404Q10BC"] )
  lapply(NAcount, function(x) x["CR404Q10T"] )
  
  
  ### pick items based on NA counting
  itemSets <- lapply(NAcount, function(x) pickItems(NAvec = x, cutOff = 0.4))
  ## why differentiate results by cutoff so strongly? 
  # because very different combinations with other clusters
  # unlist(lapply(itemSets, length))
  
  
  # 03) find overlapping items between forms -------------------------------------------
  ## find number of overlapping items to other test forms
  #lapply(itemSets, function(booklet) getOverlapp(booklet = booklet, itemNames = itemSets))
  
  ## get 50 relevant items
  comItems <- Reduce(intersect, itemSets)
  length(comItems)
  
  ## are there items in there, that should not?
  sum(comItems %in% allItems)
  sum(!comItems %in% allItems)
  
  scored <- which(comItems %in% allItems)
  timing <- grep("T\\>", comItems)
  noActions <- grep("A\\>", comItems)
  rawResponse <- grep("R\\>", comItems)
  
  items <- comItems[c(scored, timing, noActions, rawResponse)]
  comItems[!comItems %in% items]
  
  # further comparison
  i1 <- sort(comItems)
  i2 <- sort(items)
  #cbind(i1, i2, i1==i2)
  
  ### reading 01
  # all items can stay: Employment item DR219Q01 by mistake (A-E should be in the reading list or are raw responses)
  # DR220Q02RB is a raw item response (see code book)
  
  ## math 01:
  # all items can stay (43)
  # "DM155Q04RB" "DM155Q04RC" "DM155Q04RD" belong to "DM155Q04RA", 4 T/F answers 
  
  
  # 04) subset data accordingly ---------------------------------------------
  cluster <- pisa[ , c("BOOKID", comItems)]
  dim(cluster)
  
  ## keep only final scored responses and timings in data set
  names(cluster)
  
  pickVars <- nchar(names(cluster)) < 10 & (grepl("T\\>", names(cluster)) | grepl("S\\>", names(cluster)) | grepl("C\\>", names(cluster)))
  
  subDat <- cluster[, pickVars]
  # special: CM192Q01; was probably administered but not scored, remove from data
  # special: DR404Q10AC and DR404Q10BC are both coded responses but have only 1 timing data; remove timing from data
  rem_vars <- grep("^CM192Q01|^CR404Q10", names(subDat), value = T)
  subDat <- subDat[, !names(subDat) %in% rem_vars]
  
  ## check Scored/timing fit
  stems <- substr(names(subDat), 2, 8)
  leftovers <- stems[!duplicated(stems)&!duplicated(stems, fromLast = T)]
  if(length(leftovers) != 0) browser()
  #stopifnot(length(leftovers) == 0)
  
  # 05) recode partial credit items -----------------------------------------
  subDat <- dichoPC(subDat, "T\\>")
  subDat
})

### save data for further analysis
# saveRDS(subDat, "c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_analysisDat.rds")
#saveRDS(subDat, "c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_analysisDat_math.rds")
#saveRDS(subDat, "N:/PISA_analysisDat_math.rds")

#saveRDS(subDat_list, "c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_analysisDat_math_list.rds")
#saveRDS(subDat_list, "N:/PISA_analysisDat_math_list.rds")

saveRDS(subDat_list, "c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_analysisDat_read_list.rds")
saveRDS(subDat_list, "N:/PISA_analysisDat_read_list.rds")






