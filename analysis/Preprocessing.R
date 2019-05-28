# NS01 Preprocessing
# C. E. R. Edmunds - Created - 1-2-2019

# Setup --------------------------------------------------------------------------------------------
require(data.table)

combine=F
exclusions=T

if (combine){
  # Getting eye-tracking files ---------------------------------------------------------------------
  filenames <- list.files(path = "../data/fixations", full.names=T)
  files <- lapply(filenames, fread, header = TRUE)
  data <- do.call(rbind, files)
  rm(files, filenames)

  # Getting behavioural files ----------------------------------------------------------------------
  filenames <- list.files(path = "../data/", full.names=T, pattern="*.csv")
  files <- lapply(filenames, fread, header = TRUE)
  behavData <- do.call(rbind, files)
  rm(files, filenames)

  setnames(behavData, old=c("participantN", "lValue", "rValue"),
           new=c("participantNo", "lValueIAPS", "rValueIAPS"))
  behavData[task=="likert", `:=`(task="valuation")]
  behavData[, c("rowN", "taskOrder", "xBoxPos", "yBoxPos", "response", "RT"):= NULL]
  behavData[, `:=`(trial=1:300, taskOrder=c(rep(1, 50), rep(2, 50), rep(3, 200)), blockTrial=1:50)]

  fixations <- merge(data, behavData, allow=TRUE, by=c("participantNo", "trial", "task"))
  fixations[, blockTrial.x:=NULL]

  # Get subjective values
  values <- as.data.table(dcast(fixations[task=="valuation",],
                                participantNo + rImage + response ~ .)[, 1:3])


  values[, meanValue:=mean(response), by=c("participantNo", "rImage")]
  values[, response:=NULL]
  values <- unique(values)

  fixations <- merge(fixations, values, all.x=TRUE, allow=TRUE, by=c("participantNo", "rImage"))
  fixations <- merge(fixations, values, all.x=TRUE, allow=TRUE, by.x=c("participantNo", "lImage"),
                     by.y=c("participantNo", "rImage"))
  setnames(fixations, old=c("meanValue.x", "meanValue.y", "blockTrial.y"),
           new=c("rValue", "lValue", "blockTrial"))

  setcolorder(fixations,
              c("DAU", "participantNo", "age", "gender", "eye", "trial", "task", "taskOrder", "block",
                "blockTrial", "trialStartTime", "trialFinishTime", "trialStage", "lImage",
                "lValueIAPS", "lValue", "rImage", "rValueIAPS", "rValue", "response", "rt",
                "fixStart", "fixEnd", "fixLength", "dilation", "fixPosX", "fixPosY", "aoi"))
  setorder(fixations, participantNo, trial)

  # Correct fixation onset and offset times --------------------------------------------------------
  fixations[, fixStartTr:= ifelse(fixStart<trialStartTime, trialStartTime, fixStart)]
  fixations[, fixStartTr:= ifelse(fixStartTr>trialFinishTime, trialFinishTime, fixStartTr)]
  fixations[, fixEndTr:=ifelse(fixEnd<trialStartTime, trialStartTime, fixEnd)]
  fixations[, fixEndTr:=ifelse(fixEndTr>trialFinishTime, trialFinishTime, fixEndTr)]
  fixations[, fixLengthTr:=fixEndTr-fixStartTr]
  fixations[, fixLengthProp:=fixLengthTr/rt]

  # Flag fixations during choice
  fixations[, intra_choice:=ifelse(fixLengthTr>0, 1, 0)]

  # Tidy aoi
  fixations[is.na(aoi), aoi:="out"]

  fixations[, response:=as.double(response)]
  fixations[task=="binary", response:= response-1]
  fixations[task=="continuous", response:= (response-1.0)/99.0]

  # Write data
  fwrite(fixations, "/Users/arlo/Documents/DAUs/NS01/analysis/NS01fixationsLong.csv", row.names=F)

  if (nrow(data)==nrow(fixations)){
    rm(list=ls())
  }
}
if (exclusions){
  fixations <- fread('NS01fixationsLong.csv', stringsAsFactors=F)
  badCalib <- c(6, 9, 11, 14, 15, 24, 26, 34, 10, 22, 42, 47) # Wrong eye
  fixations <- fixations[!participantNo %in% badCalib,] # Exclude participants

  # Exclusions based on time on task in binary condition -------------------------------------------
  binary <- fixations[task=='binary' & intra_choice==1,]
  binary[, onTask:= ifelse(aoi=="left" | aoi=="right" | aoi=="likertHorizontal", 1, 0)]
  binary[, onTaskProp:= onTask*fixLengthTr/rt]
  binary.summary <- as.data.table(aggregate(onTaskProp ~ participantNo + trial, data=binary,
                                            FUN=sum))

  binary.summary.plot <- as.data.table(aggregate(onTaskProp ~ participantNo, data=binary.summary,
                                                 FUN=mean))

  # boxplot(binary.summary.plot$onTaskProp)
  # title("Proportion of time spent on task in the binary task.")
  # # One outlier at around 0.53, propose cut off of 60%
  #
  # binary.histogram <- ggplot(binary.summary.plot, aes(x=onTaskProp)) +
  #   geom_histogram(binwidth=0.01) +
  #   theme_minimal() +
  #   geom_vline(xintercept=0.6, colour="blue") +
  #   labs(title="Histogram of proportion of time on task for every participant in binary task",
  #        x="Proportion of trial on task", y="Count") +
  #   xlim(c(0.5, 1))
  # binary.histogram

  # Get excluded participant
  binaryExclusions <- c(binary.summary.plot[onTaskProp<0.60, participantNo])

  # Remove participant
  fixations[, binaryExclusions:=ifelse(participantNo %in% binaryExclusions, 1, 0)]

  # Exclusions based on time on task in continuous condition ---------------------------------------
  continuous <- fixations[task=='continuous' & intra_choice==1,]
  continuous[, onTask:= ifelse(aoi=="left" | aoi=="right" | aoi=="likertHorizontal", 1, 0)]
  continuous[, onTaskProp:= onTask*fixLengthTr/rt]
  continuous.summary <- as.data.table(aggregate(onTaskProp ~ participantNo + trial,
                                                data=continuous, FUN=sum))

  continuous.summary.plot <- as.data.table(aggregate(onTaskProp ~ participantNo,
                                                     data=continuous.summary, FUN=mean))

  # boxplot(continuous.summary.plot$onTaskProp)
  # title("Proportion of time spent on task in the continuous task.")
  # No outliers according to boxplot

  # continuous.histogram <- ggplot(continuous.summary.plot, aes(x=onTaskProp)) +
  #   geom_histogram(binwidth=0.01) +
  #   theme_minimal() +
  #   labs(title="Histogram of time on task for every participant in continuous task",
  #        x="Time on task", y="Count") +
  #   xlim(c(0.5, 1))
  # continuous.histogram

  # Not really needed, due to lack of exclusions. There in case change mind.
  # Get excluded participant(s)
  continuousExclusions <- c(continuous.summary.plot[onTaskProp<0, participantNo])
  fixations[, continuousExclusions:=ifelse(participantNo %in% continuousExclusions, 1, 0)]

  #  Exclusions based on reaction times ------------------------------------------------------------
  fixations[, rtExclusions:= ifelse((rt<200 | rt>mean(rt)+3*sd(rt)) & task!="valuation", 1, 0)]

  # Get break down of exclusions per task and per participant
  rt.exclusions <- fixations[task!="valuation", list(excludeN=sum(rtExclusions)/.N),
                             by=list(participantNo, trial, task)]
  rt.exclusions.by.task <- rt.exclusions[, list(propExclude=sum(excludeN)/.N), by=task]
  rt.exclusions.by.ppt <- rt.exclusions[, list(propExclude=sum(excludeN)/.N), by=.(participantNo)]

  # All exclusions
  fixations[, exclude:=ifelse(binaryExclusions + continuousExclusions + rtExclusions>0, 1, 0)]

  rm(binary, binary.summary, binary.summary.plot, binaryExclusions,
     continuous, continuous.summary, continuous.summary.plot, continuousExclusions, badCalib)
}
