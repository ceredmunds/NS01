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

# Correct fixation onset and offset times ----------------------------------------------------------
fixations[, fixStartTr:= ifelse(fixStart<trialStartTime, trialStartTime, fixStart)]
fixations[, fixStartTr:= ifelse(fixStartTr>trialFinishTime, trialFinishTime, fixStartTr)]
fixations[, fixEndTr:=ifelse(fixEnd<trialStartTime, trialStartTime, fixEnd)]
fixations[, fixEndTr:=ifelse(fixEndTr>trialFinishTime, trialFinishTime, fixEndTr)]
fixations[, fixDurationTr:=fixEndTr-fixStartTr]
fixations[, fixDurationProp:=fixDurationTr/rt]

# Flag fixations during choice
fixations[, intra_choice:=ifelse(fixDurationTr>0, 1, 0)]

# Tidy aoi
fixations[aoi=="", aoi:=NA]

# Write data
fwrite(fixations, "NS01fixationsLong.csv", row.names=F)
