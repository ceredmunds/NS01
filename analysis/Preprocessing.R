# NS01 Preprocessing
# C. E. R. Edmunds - Created - 1-2-2019

# Setup --------------------------------------------------------------------------------------------
rm(list=ls()) # Clear all variables in the workspace

require(data.table)

filenames <- list.files(path = "../data/fixations", full.names=T)
files <- lapply(filenames, fread, header = TRUE)
data <- do.call(rbind, files)
rm(files, filenames)

# Correct fixation onset and offset times ----------------------------------------------------------

data[, fixStartTr:= ifelse(fixStart<trialStartTime, trialStartTime, fixStart)]
data[, fixStartTr:= ifelse(fixStartTr>trialFinishTime, trialFinishTime, fixStartTr)]
data[, fixEndTr:=ifelse(fixEnd<trialStartTime, trialStartTime, fixEnd)]
data[, fixEndTr:=ifelse(fixEndTr>trialFinishTime, trialFinishTime, fixEndTr)]
data[, fixDurationTr:=fixEndTr-fixStartTr]
data[, fixDurationProp:=fixDurationTr/rt]

# Flag fixations during choice
data[, intra_choice:=ifelse(fixDurationTr>0, 1, 0)]

fwrite(data, "NS01fixationsLong.csv", row.names=F)
