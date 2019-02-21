# NS01 Preprocessing
# C. E. R. Edmunds - Created - 1-2-2019

# Setup --------------------------------------------------------------------------------------------
rm(list=ls()) # Clear all variables in the workspace

require(data.table)

# Getting eye-tracking files -----------------------------------------------------------------------
filenames <- list.files(path = "../data/fixations", full.names=T)
files <- lapply(filenames, fread, header = TRUE)
data <- do.call(rbind, files)
rm(files, filenames)

# Getting behavioural files ------------------------------------------------------------------------
filenames <- list.files(path = "../data/", full.names=T, pattern="*.csv")
files <- lapply(filenames, fread, header = TRUE)
behavData <- do.call(rbind, files)
rm(files, filenames)

setnames(behavData, old=c("participantN"), new=c("participantNo"))
behavData[, c("rowN", "taskOrder", "xBoxPos", "yBoxPos", "response", "RT"):=NULL]
behavData[task=="likert", task:="valuation"]

combinedData <- merge(data, behavData, by=c("participantNo", "task", "trial"))
setcolorder(combinedData,
            c("DAU", "participantNo", "age", "gender", "eye", "trial", "task", "block", "blockTrial",
              "trialStartTime", "trialFinishTime", "trialStage", "lImage", "lValue", 'rImage',
              "rValue", "response", "rt", "fixStart", "fixEnd", "fixLength", "dilation", "fixPosX",
              "fixPosY", "aoi"))

# Correct fixation onset and offset times ----------------------------------------------------------
combinedData[, fixStartTr:= ifelse(fixStart<trialStartTime, trialStartTime, fixStart)]
combinedData[, fixStartTr:= ifelse(fixStartTr>trialFinishTime, trialFinishTime, fixStartTr)]
combinedData[, fixEndTr:=ifelse(fixEnd<trialStartTime, trialStartTime, fixEnd)]
combinedData[, fixEndTr:=ifelse(fixEndTr>trialFinishTime, trialFinishTime, fixEndTr)]
combinedData[, fixDurationTr:=fixEndTr-fixStartTr]
combinedData[, fixDurationProp:=fixDurationTr/rt]

# Flag fixations during choice
combinedData[, intra_choice:=ifelse(fixDurationTr>0, 1, 0)]

fwrite(combinedData, "NS01fixationsLong.csv", row.names=F)
