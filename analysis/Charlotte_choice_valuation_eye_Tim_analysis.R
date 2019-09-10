# Analysis of NS01

## Setup -------------------------------------------------------------------------------------------
rm(list=ls())

require(data.table); require(lmerTest)

source("Preprocessing.R"); source('bsci.R')

# Remove excluded trials
fixations <- fixations[exclude==0,]
# Remove fixations outside of trial
fixations <- fixations[intra_choice==1,]

## Get data per trial ------------------------------------------------------------------------------
data <- dcast(fixations, participantNo + task + taskOrder + block + trial + response + rt +
                lValue + rValue ~ aoi,
              fun = sum, value.var="fixLengthTr")

data[, value.difference:=rValue-lValue]
data[, attention.difference:=right-left]

# Different types of attention
data[, attentionByOptions:=(right-left)/(right+left)]
data[, attentionByTimeOnTask:=(right-left)/(right+left+likertHorizontal)]


# Recode responses to get binary responses from continuous task
data[, recodedResponse:= response]
data[task=="continuous", recodedResponse:= ifelse(recodedResponse<=0.5, 0, 1)]

data[, `:=`(participantNo=factor(participantNo), task=factor(task))]

# Choice --------
data[, propAttentionLeft:= left/(left+right)]
data[, propAttentionRight:= 1-propAttentionLeft]
data[, Timintr:= lValue*propAttentionLeft-rValue*propAttentionRight]

value.cont.Timintr = lmer(response ~ attention.difference + value.difference + Timintr + 
                              (1 + attention.difference + value.difference ||participantNo),
                            data=data[task=="continuous",])

summary(value.cont.Timintr)





value.binary.Timintr = glmer(recodedResponse ~ attention.difference + value.difference + Timintr + 
                           (1 + attention.difference + value.difference ||participantNo), family="binomial",
                         data=data[task=="continuous",])

summary(value.binary.Timintr)


choice.only.Timintr = glmer(recodedResponse ~ attention.difference + value.difference + Timintr + 
                           (1 + attention.difference + value.difference ||participantNo), family="binomial",
                         data=data[task=="binary",])

summary(choice.only.Timintr)























summary(choice.full) # Model summary
confint(choice.full, method="Wald") # Get 0.95 confidence intervals
stargazer(choice.full, type="latex",
          title="Summary of coefficients of model predicting choice (both blocks).",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          out="../techReport/tables/choiceModelAll.tex", style="default", ci=T, single.row=T,
          label="table:choiceModelAll", table.placement="!b",
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

choice.full.block1 <- glmer(recodedResponse ~ task*attention.difference*value.difference +
                              (1 + attention.difference + value.difference||participantNo),
                            data=data[task!="valuation" & block==1,], family="binomial",
                            control=glmerControl(optimizer="Nelder_Mead",
                                                 check.conv.grad=.makeCC("warning", tol=1e-1)))
summary(choice.full.block1) # Model summary
confint(choice.full.block1, method="Wald") # Get 0.95 confidence intervals
stargazer(choice.full.block1, type="latex",
          title="Summary of coefficients of model predicting choice (Block 1 only).",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          out="../techReport/tables/choiceModelBlock1.tex", style="default", ci=T, single.row=T,
          label="table:choiceModelBlock1", table.placement="!b",
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

choice.full.block2 <- glmer(recodedResponse ~ task*attention.difference*value.difference +
                              (1 + attention.difference + value.difference||participantNo),
                            data=data[task!="valuation" & block==2,], family="binomial",
                            control=glmerControl(optimizer="Nelder_Mead",
                                                 check.conv.grad=.makeCC("warning", tol=1e-1)))
summary(choice.full.block2) # Model summary
confint(choice.full.block2, method="Wald") # Get 0.95 confidence intervals
stargazer(choice.full.block2, type="latex",
          title="Summary of coefficients of model predicting choice (Block 2 only).",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          out="../techReport/tables/choiceModelBlock2.tex", style="default", ci=T, single.row=T,
          label="table:choiceModelBlock2", table.placement="!b",
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference;",
          notes.append=F, notes.align="l") # Print table to file

# Distributions of continuous responses ------------------------------------------------------------
labels <- c("1" = "Block 1", "2" = "Block 2")

ggplot(data[task=="continuous",], aes(x=response)) +
  geom_histogram(binwidth=0.02) +
  facet_grid(.~taskOrder, labeller=labeller(taskOrder=labels)) +
  labs(x="Response", y="Count") +
  theme_bw()
ggsave("../techReport/images/continuousResponses.pdf", units="in", width=6, height=4)

pdf("../techReport/images/contResponsesPerPpt.pdf", onefile = TRUE, width=7, height=10)
cont.response.blk1 <- ggplot(data[task=="continuous" & taskOrder==1,], 
                             aes(x=response)) +
  geom_histogram(binwidth=0.02, color=wes_palette(n=5, name="Zissou1")[1]) +
  scale_fill_manual(values=) +
  facet_wrap(~participantNo) +
  labs(x="Response", y="Count") +
  theme_bw()

cont.response.blk2 <- ggplot(data[task=="continuous" & taskOrder==2,], 
                             aes(x=response, color=as.factor(taskOrder))) +
  geom_histogram(binwidth=0.02, color=wes_palette(n=5, name="Zissou1")[4]) +
  facet_wrap(~participantNo) +
  labs(x="Response", y="Count") +
  theme_bw()
grid.arrange(cont.response.blk1, cont.response.blk2, nrow=2)
dev.off()

# Plot predicted choices ---------------------------------------------------------------------------
summary(choice.full.block1)

predict.choice <- as.data.table(emmeans(choice.full.block1, ~ attention.difference * value.difference * task,
                                        at=list(attention.difference =
                                                  quantile(data[task!="valuation" & block==1,
                                                                attention.difference], c(.25, .5, .75)),
                                                value.difference=-6:6)))
predict.choice[, attention.difference:=factor(round(attention.difference, 2))]

ggplot(predict.choice, aes(x=value.difference, y=emmean, group=attention.difference,
                           color=attention.difference)) +
  geom_point(size=2) +
  geom_line(size=1) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.4)+
  scale_y_continuous(name="Choice", breaks=seq(-7.5, 7.5, by=2.5), labels=round(invlogit(seq(-7.5, 7.5, by=2.5)), 2)) +
  facet_grid(.~task, labeller=labeller(task=task.labs)) +
  labs(x="Value difference",color="Attention \ndifference") +
  theme_bw() +
  scale_color_manual(values=wes_palette(n=4, name="Zissou1")) +
  theme(legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black"))
ggsave('../techReport/images/predictedChoiceGraph.pdf', height=3.5, width=6, units="in")

# Number of fixations ------------------------------------------------------------------------------
nFixData <- dcast(fixations,
                  participantNo + task + taskOrder + block + trial + response + rt + lValue + rValue
                  ~ aoi, fun=length, value.var="aoi")
nFixData[, relativeFixN:= right/(left+right)]

nFixData[, value.difference:=rValue-lValue]
nFixData[, attention.difference:=right-left]

# Different types of attention
nFixData[, attentionByOptions:=(right-left)/(right+left)]
nFixData[, attentionByTimeOnTask:=(right-left)/(right+left+likertHorizontal)]

# Recode responses to get binary responses from continuous task
nFixData[, recodedResponse:= response]
nFixData[task=="continuous", recodedResponse:= ifelse(recodedResponse<=0.5, 0, 1)]

nFixData[, `:=`(participantNo=factor(participantNo), task=factor(task))]

# Random intercepts
fixN.intercept.only <- lm(relativeFixN ~ 1, data=nFixData[task!="valuation" & block==1,])

fixN.random.intercept.only <- lmer(relativeFixN ~ 1 + 1|participantNo,
                                   data=nFixData[task!="valuation" & block==1,],
                                   control=lmerControl(optimizer="Nelder_Mead"))

fixN.random.intercept.slope.att <- lmer(relativeFixN ~ (attention.difference||participantNo),
                                        data=nFixData[task!="valuation" & block==1,],
                                        control=lmerControl(optimizer="Nelder_Mead"))

fixN.random.intercept.slope.value <- lmer(relativeFixN ~ (value.difference||participantNo),
                                          data=nFixData[task!="valuation" & block==1,],
                                          control=lmerControl(optimizer="Nelder_Mead"))
# singular...
fixN.random.intercept.slope.task <- lmer(relativeFixN ~ (task)||participantNo,
                                         data=nFixData[task!="valuation" & block==1,],
                                         control=lmerControl(optimizer="Nelder_Mead"))

anova(fixN.random.intercept.only, fixN.intercept.only, fixN.random.intercept.slope.att,
      fixN.random.intercept.slope.value)


fixN.model <- lmer(relativeFixN ~ task*attention.difference*value.difference +
                     (1|participantNo),
                   data=nFixData[task!="valuation" & block==1,])
summary(fixN.model)
stargazer(fixN.model, type="latex",
          title="Summary of coefficients of model predicting number of fixations on each choice",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          out="../techReport/tables/nFixModel.tex", style="default", ci=T, single.row=T,
          label="table:nFixModel", table.placement="!b",
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

# Duration of fixations ----------------------------------------------------------------------------
durData <- dcast(fixations,
                 participantNo + task + taskOrder + block + trial + response + rt + lValue + rValue
                 ~ aoi, fun=mean, value.var="fixLengthTr")
durData[, relativeFixDur:= right/(left+right)]

durData[, value.difference:=rValue-lValue]
durData[, attention.difference:=right-left]

# Different types of attention
durData[, attentionByOptions:=(right-left)/(right+left)]
durData[, attentionByTimeOnTask:=(right-left)/(right+left+likertHorizontal)]

# Recode responses to get binary responses from continuous task
durData[, recodedResponse:= response]
durData[task=="continuous", recodedResponse:= ifelse(recodedResponse<=0.5, 0, 1)]

durData[, `:=`(participantNo=factor(participantNo), task=factor(task))]

# Random effects
fixD.intercept.only <- lm(relativeFixDur ~ 1, data=durData[task!="valuation" & block==1,])

fixD.random.intercept.only <- lmer(relativeFixDur ~ 1 + (1|participantNo),
                                   data=durData[task!="valuation" & block==1,],
                                   control=lmerControl(optimizer="Nelder_Mead"))

fixD.random.intercept.slope.value <- lmer(relativeFixDur ~ (value.difference||participantNo),
                                          data=durData[task!="valuation" & block==1,],
                                          control=lmerControl(optimizer="Nelder_Mead"))

fixD.random.intercept.slope.att <- lmer(relativeFixDur ~ (attention.difference||participantNo),
                                        data=durData[task!="valuation" & block==1,],
                                        control=lmerControl(optimizer="Nelder_Mead"))
# odd?
fixD.random.intercept.slope.task <- lmer(relativeFixDur ~ (task)||participantNo,
                                         data=durData[task!="valuation" & block==1,],
                                         control=lmerControl(optimizer="Nelder_Mead"))

anova(fixD.random.intercept.only, fixD.intercept.only, fixD.random.intercept.slope.value,
      fixD.random.intercept.slope.att)


fix.duration.model <- lmer(relativeFixDur ~ task*attention.difference*value.difference +
                             (1|participantNo),
                           data=durData[task!="valuation" & block==1,])
summary(fix.duration.model)
stargazer(fix.duration.model, type="latex",
          title="Summary of coefficients of model predicting duration of time on options",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          out="../techReport/tables/durFixModel.tex", style="default", ci=T, single.row=T,
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference; ",
          label="table:durFixModel", table.placement="!b",
          notes.append=F, notes.align="l") # Print table to file

# Looking at other metrics of attention ------------------------------------------------------------
dcast(data[task!="valuation",], task ~ ., fun = mean, value.var="likertHorizontal.x")

data[, attention:=attention.difference]
choice.full <- glmer(recodedResponse ~ task*attention*value.difference +
                       (1 + attention + value.difference||participantNo),
                     data=data[task!="valuation" & block==1,], family="binomial",
                     control=glmerControl(optimizer="Nelder_Mead",
                                          check.conv.grad=.makeCC("warning", tol=1e-1)))
summary(choice.full) # Model summary
confint(choice.full, method="Wald") # Get 0.95 confidence intervals

rt.full <- lmer(rt ~ task*abs(attention)*abs(value.difference) +
                  (abs(value.difference)||participantNo),
                data=data[task!="valuation" & block==1,],
                control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full)

data[, attention:=attentionByOptions]
choice.full.options <- glmer(recodedResponse ~ task*attention*value.difference +
                               (1 + attention + value.difference||participantNo),
                             data=data[task!="valuation" & block==1,], family="binomial",
                             control=glmerControl(optimizer="Nelder_Mead",
                                                  check.conv.grad=.makeCC("warning", tol=1e-1)))
summary(choice.full.options)

rt.full.options <- lmer(rt ~ task*abs(attention)*abs(value.difference) +
                          (abs(value.difference)||participantNo),
                        data=data[task!="valuation" & block==1,],
                        control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full.options)

data[, attention:=attentionByTimeOnTask]
choice.full.on.task<- glmer(recodedResponse ~ task*attention*value.difference +
                              (1 + attention + value.difference||participantNo),
                            data=data[task!="valuation" & block==1,], family="binomial",
                            control=glmerControl(optimizer="Nelder_Mead",
                                                 check.conv.grad=.makeCC("warning", tol=1e-1)))
summary(choice.full.on.task)

rt.full.on.task <- lmer(rt ~ task*abs(attention)*abs(value.difference) +
                          (abs(value.difference)||participantNo),
                        data=data[task!="valuation" & block==1,],
                        control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full.on.task)

stargazer(choice.full, choice.full.options, choice.full.on.task, type="latex",
          title="Summary of coefficients of model predicting choice, comparing different attentions",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          column.labels=c("by reaction time", "by time on choices", "by time on task"),
          out="../techReport/tables/choiceModelAttention.tex", style="default", ci=T, single.row=F,
          label="table:choiceModelAttention", table.placement="h",
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

stargazer(rt.full, rt.full.options, rt.full.on.task, type="latex",
          title="Summary of coefficients of model predicting reaction time, comparing different attentions",
          covariate.labels=c("Task", "$\\vert\\Delta_A\\vert$", "$\\vert\\Delta_V\\vert$",
                             "Task : $\\vert\\Delta_A\\vert$", "Task : $\\vert\\Delta_V\\vert$",
                             "$\\vert\\Delta_A\\vert$ : $\\vert\\Delta_V\\vert$",
                             "Task : $\\vert\\Delta_A\\vert$ :  $\\vert\\Delta_V\\vert$"),
          column.labels=c("by reaction time", "by time on choices", "by time on task"),
          out="../techReport/tables/rtModelAttention.tex", style="default", ci=T, single.row=F,
          label="table:choiceModelAttention", table.placement="h",
          notes="\\footnotesize $\\vert\\Delta_A\\vert$ = absolute attention difference; $\\vert\\Delta_V\\vert$ = absolute value difference; ",
          notes.append=F, notes.align="l") # Print table to file

ggpairs(data[task!="valuation", c("attention.difference", "attentionByOptions", "attentionByTimeOnTask")],
        title="Correlations between attentions.")
ggsave('../techReport/images/attentionCorrelations.pdf', height=5, width=5, units="in")

# Run with all fixations ---------------------------------------------------------------------------
# Setup
rm(list=ls())

require(data.table); require(plyr); require(ggplot2); require(MuMIn);
require(GGally); require(stargazer); require(BaylorEdPsych); require(lme4)

require(RePsychLing) # install.packages('devtools'); devtools::install_github("dmbates/RePsychLing")

source("Preprocessing.R"); source('bsci.R')

# Remove fixations outside of trial
fixations <- fixations[intra_choice==1,]

# Get by trial data
data <- dcast(fixations, participantNo + task + taskOrder + block + trial + response + rt +
                lValue + rValue ~ aoi,
              fun = sum, value.var="fixLengthProp")

data[, value.difference:=rValue-lValue]
data[, attention.difference:=right-left]

# Different types of attention
data[, attentionByOptions:=(right-left)/(right+left)]
data[, attentionByTimeOnTask:=(right-left)/(right+left+likertHorizontal)]

# Recode responses to get binary responses from continuous task
data[, recodedResponse:= response]
data[task=="continuous", recodedResponse:= ifelse(recodedResponse<=0.5, 0, 1)]

data[, `:=`(participantNo=factor(participantNo), task=factor(task))]

# Choice
# Random intercepts
choice.intercept.only <- glm(recodedResponse ~ 1,
                             data=data[task!="valuation" & block==1,], family="binomial")

choice.random.intercept.only <- glmer(recodedResponse ~ 1 + 1|participantNo,
                                      data=data[task!="valuation" & block==1,], family="binomial",
                                      control=glmerControl(optimizer="Nelder_Mead"))

choice.random.intercept.slope.att <- glmer(recodedResponse ~ (attention.difference)||participantNo,
                                           data=data[task!="valuation" & block==1,], family="binomial")

choice.random.intercept.slope.att.value <- glmer(recodedResponse ~
                                                   (attention.difference +
                                                      value.difference)||participantNo,
                                                 data=data[task!="valuation" & block==1,], family="binomial",
                                                 control=glmerControl(optimizer="Nelder_Mead"))

choice.random.intercept.slope.att.value.task <- glmer(recodedResponse ~
                                                        (attention.difference +  value.difference + task)||participantNo,
                                                      data=data[task!="valuation" & block==1,], family="binomial",
                                                      control=glmerControl(optimizer="Nelder_Mead"))

anova(choice.random.intercept.only, choice.random.intercept.slope.att,
      choice.random.intercept.slope.att.value, choice.random.intercept.slope.att.value.task,
      test="Chisq")

choice.full <- glmer(recodedResponse ~ task*attention.difference*value.difference +
                       (1 + attention.difference + value.difference||participantNo),
                     data=data[task!="valuation" & block==1,], family="binomial",
                     control=glmerControl(optimizer="Nelder_Mead",
                                          check.conv.grad=.makeCC("warning", tol=1e-1)))
summary(choice.full) # Model summary
confint(choice.full, method="Wald") # Get 0.95 confidence intervals
stargazer(choice.full, type="latex",
          title="Summary of coefficients of model predicting choice including all trials",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          out="../techReport/tables/choiceModelAllTrials.tex", style="default", ci=T, single.row=T,
          label="table:choiceModelAllTrials", table.placement="!b",
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

# Overall choice value -----------------------------------------------------------------------------
# Setup
rm(list=ls())

require(data.table); require(plyr); require(ggplot2); require(MuMIn);
require(GGally); require(stargazer); require(BaylorEdPsych); require(lme4)

require(RePsychLing) # install.packages('devtools'); devtools::install_github("dmbates/RePsychLing")

source("Preprocessing.R"); source('bsci.R')

# Remove fixations outside of trial
fixations <- fixations[intra_choice==1,]
# Remove excluded trials
fixations <- fixations[exclude==0,]

# Get by trial data
data <- dcast(fixations, participantNo + task + taskOrder + block + trial + response + rt +
                lValue + rValue ~ aoi,
              fun = sum, value.var="fixLengthProp")

data[, value.difference:=rValue-lValue]
data[, total.value:=rValue+lValue]
data[, attention.difference:=right-left]

# Different types of attention
data[, attentionByOptions:=(right-left)/(right+left)]
data[, attentionByTimeOnTask:=(right-left)/(right+left+likertHorizontal)]

# Recode responses to get binary responses from continuous task
data[, recodedResponse:= response]
data[task=="continuous", recodedResponse:= ifelse(recodedResponse<=0.5, 0, 1)]

data[, `:=`(participantNo=factor(participantNo), task=factor(task))]

# Looking at choice value predicting RT - model taken from Smith & Krajbich (2018)
rt.smith <- lm(rt ~ abs(value.difference) + total.value,
               data=data[task!="valuation" & block==1,],
               control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.smith)

# Including task
rt.smith.task <- lm(rt ~ task*total.value*abs(value.difference),
                    data=data[task!="valuation" & block==1,],
                    control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.smith.task)
stargazer(rt.smith.task, type="latex",
          title="Summary of coefficients of model predicting reaction time from total value and value difference",
          covariate.labels=c("Task", "$\\sum_A$", "$\\Delta_V$",
                             "Task : $\\sum_A$", "Task : $\\Delta_V$",
                             "$\\sum_A$ : $\\Delta_V$",
                             "Task : $\\sum_A$ :  $\\Delta_V$"),
          out="../techReport/tables/rtModelSmithTask.tex", style="default", ci=T, single.row=T,
          label="table:rtModelSmithTask", table.placement="!b",
          notes="\\footnotesize $\\sum_A$ = total value; $\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file


rt.smith.cont <- lm(rt ~ total.value*abs(value.difference),
                    data=data[task=="continuous" & block==1,])
summary(rt.smith.cont)

rt.smith.binary <- lm(rt ~ total.value*abs(value.difference),
                      data=data[task=="binary" & block==1,])
summary(rt.smith.binary)
confint(rt.smith.binary)
