# NS01 Generate choices and get stimuli.
# Original data from Tim Mullett
# C. E. R. Edmunds - Created - 7-12-2018

# Setup --------------------------------------------------------------------------------------------
rm(list=ls()) # Clear all variables in the workspace

require(data.table)

femaleRatings <- fread("IAPSratings/femratings.csv")
maleRatings <- fread("IAPSratings/maleratings.csv")
allRatings <- fread("IAPSratings/allratings.csv")

maxValue <- 7
minValue <- 5
halfRange <- (maxValue-minValue)/2
midPoint <- halfRange + minValue
nPpts <- 80
nTrials <- 100

NS01ratings <- data.table(IAPS = as.integer(femaleRatings$IAPS),
                          fValue = femaleRatings$valmn,
                          mValue = maleRatings$valmn,
                          aValue = allRatings$valmn)
NS01ratings <- NS01ratings[!IAPS %in% c(3005, 2745, 2055, 2375),]

# Subset to get the items we want (i.e. within (minValue, maxValue), and with less than 1.5 diff
# between genders
NS01ratings[, keep:=ifelse(abs(fValue-midPoint) < halfRange &
                           abs(mValue-midPoint) < halfRange &
                           abs(aValue-midPoint) < halfRange &
                           abs(mValue-fValue) < 1.5, 1, 0)]
# Remove unwanted items
NS01ratings <- NS01ratings[!keep==0,]

# Clear unneeded data from workspace
rm(allRatings, femaleRatings, maleRatings)

NS01choices <- data.table(ParticipantNo = rep(1:nPpts, each=nTrials),
                          lChoice = 0L,
                          lValue = 0,
                          rChoice = 0L,
                          rValue = 0)

# Generate choices
for(iPpt in 1:nPpts){
  itemSample <- sample(1:nrow(NS01ratings), 2*nTrials, replace=F)
  NS01choices[ParticipantNo==iPpt, c("lChoice", "lValue", "rChoice", "rValue") :=
                .(NS01ratings$IAPS[itemSample[1:nTrials]],
                  NS01ratings$aValue[itemSample[1:nTrials]],
                  NS01ratings$IAPS[itemSample[(nTrials+1):(2*nTrials)]],
                  NS01ratings$aValue[itemSample[(nTrials+1):(2*nTrials)]])]
}

# Saving choices
fwrite(NS01choices, "NS01choices.csv")
fwrite(NS01choices, "../experimentCode/Data/NS01choices.csv")

filenames <- paste("IAPSpictures/",unique(c(NS01choices$lChoice, NS01choices$rChoice)), ".jpg",
                   sep="")
file.copy(filenames, "../experimentCode/Stimuli")
