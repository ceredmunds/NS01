# Analysis of NS01

## Setup -------------------------------------------------------------------------------------------
rm(list=ls())

require(data.table); require(plyr); require(ggplot2); require(MuMIn); require(wesanderson)
require(GGally); require(stargazer); require(BaylorEdPsych); require(lme4); library(merTools)

require(RePsychLing) # install.packages('devtools'); devtools::install_github("dmbates/RePsychLing")

source("Preprocessing.R"); source('bsci.R')

# Remove excluded trials
fixations <- fixations[exclude==0,]
# Remove fixations outside of trial
fixations <- fixations[intra_choice==1,]

## Get data per trial ------------------------------------------------------------------------------
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

## Reaction times ----------------------------------------------------------------------------------
# Order effects
rt.order <- lm(rt ~ task*taskOrder, data=data[task!="valuation",], method="ML")
rt.order.all <- lm(rt ~ task*taskOrder*attention.difference*value.difference,
                    data=data[task!="valuation",], method="ML")

rt.task.1 <-  gls(rt ~ task, data=data[task!="valuation" & block==1,], method="ML")
rt.task.2 <-  gls(rt ~ task, data=data[task!="valuation" & block==2,], method="ML")

rt.task.cont <-  gls(rt ~ taskOrder, data=data[task!="valuation" & task=="continuous",], method="ML")
rt.task.binary <-  gls(rt ~ taskOrder, data=data[task!="valuation" & task=="binary",], method="ML")

rt.order.graph.data <- data[task!="valuation", list(mean=mean(rt), sd=sd(rt)), by=.(task, taskOrder)]
rt.order.graph.data[, `:=`(lower=mean-sd, upper=mean+sd)]

rt.order.graph.data <- data.table()
for(order in 1:2){
  tmp <- bsci(as.data.frame(data[taskOrder==order,]), group.var=c("task"), dv.var="rt",
              difference=T, pooled.error=T)
  tmp <- cbind(c('Binary','Continuous'), order, tmp)
  rt.order.graph.data <- rbind(rt.order.graph.data, tmp)
}
colnames(rt.order.graph.data) <- c("Task", "Block", "lower", "mean", "upper")

ggplot(rt.order.graph.data, aes(x=Block, y=as.numeric(mean), group=Task)) +
  geom_point(aes(shape=Task), size=3) +
  geom_line(aes(linetype=Task)) +
  coord_cartesian(ylim=c(2200, 3700)) +
  labs(x="Block", y="Reaction time (ms)") +
  geom_errorbar(aes(ymin=as.numeric(lower), ymax=as.numeric(upper)), width=0.1) +
  theme_bw() +
  theme(legend.justification=c(1,1),
        legend.position=c(0.99, 0.99))
ggsave('../techReport/images/RTorderEffects.pdf', height=3.5, width=4, units="in")

# Considering random effects
# Intercept only
rt.intercept.only <- lm(rt ~ 1, data=data[task!="valuation" & block==1,])

# Random intercepts
rt.random.intercept.only <- lmer(rt ~ 1 + 1|participantNo,
                                data=data[task!="valuation" & block==1,], REML=F)

# Random intercepts and slopes based on attention
rt.random.intercept.slope.att <- lmer(rt ~ 1 + abs(attention.difference)||participantNo,
                                          data=data[task!="valuation" & block==1,], REML=F)

# Random intercepts and slopes based on attention and value
rt.random.intercept.slope.att.val <- lmer(rt ~ 1 + abs(attention.difference)+abs(value.difference)||participantNo,
                                     data=data[task!="valuation" & block==1,], REML=F)

# Comparing random models
anova(rt.random.intercept.only, rt.intercept.only, rt.random.intercept.slope.att,
      rt.random.intercept.slope.att.val)

# Full model (i.e. including fixed effects)
rt.full <- lmer(rt ~ task*abs(attention.difference)*abs(value.difference) +
                  (abs(value.difference)||participantNo),
               data=data[task!="valuation" & block==1,],
               control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full) # Model summary
confint(rt.full) # Get 0.95 confidence intervals
stargazer(rt.full, type="latex",
  title="Summary of coefficients of model predicting reaction time",
  covariate.labels=c("Task", "$\\vert\\Delta_A\\vert$", "$\\vert\\Delta_V\\vert$",
                     "Task : $\\vert\\Delta_A\\vert$", "Task : $\\vert\\Delta_V\\vert$",
                     "$\\vert\\Delta_A\\vert$ : $\\vert\\Delta_V\\vert$",
                     "Task : $\\vert\\Delta_A\\vert$ :  $\\vert\\Delta_V\\vert$"),
  out="../techReport/tables/RTmodels.tex", style="default", ci=T, single.row=T,
  label="table:rtModel", table.placement="t",
  notes="\\footnotesize $\\vert\\Delta_A\\vert$ = absolute attention difference; $\\vert\\Delta_V\\vert$ = absolute value difference; ",
  notes.append=F, notes.align="l") # Print table to file

# Graph of attention and value on reaction time (think by quartiles)
rt.graph.data <- data[task!="valuation" & block==1,]
rt.graph.data[, att.quartile:= cut(abs(attention.difference),
                                       quantile(abs(attention.difference), probs=0:4/4),
                                       include.lowest=TRUE, labels=FALSE),
                  by=task]
rt.graph.data[, abs.value:=abs(value.difference)]
rt.graph.summary.data <- dcast(rt.graph.data, task + att.quartile + abs.value ~ .,
                                   fun=mean, value.var="rt")
colnames(rt.graph.summary.data)[4] <- "rt"

rt.graph.summary.data <- rt.graph.data[, .(meanRT=mean(rt), sd=sd(rt), N=.N), by=.(task, att.quartile, abs.value)]
rt.graph.summary.data[, ci:=qnorm(0.95)*sd/sqrt(N)]

rt.graph.summary.data <- rt.graph.summary.data[abs.value%%1==0,]

# New facet label names for task variable
task.labs <- c("Binary choice", "Strength-of-preference")
names(task.labs) <- c("binary", "continuous")

ggplot(rt.graph.summary.data, aes(group=as.factor(att.quartile), y=meanRT, x=abs.value)) +
  geom_point(aes(color=as.factor(att.quartile)), position=position_dodge(width=0.5)) +
  geom_line(aes(color=as.factor(att.quartile)), position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=meanRT-ci, ymax=meanRT+ci, color=as.factor(att.quartile)),
                position=position_dodge(width=0.5)) +
  facet_grid(. ~ task,
             labeller=labeller(task=task.labs)) +
  labs(x="Value difference", y="Reaction time (ms)", color="Attention \ndifference") +
  theme_bw() +
  coord_cartesian(ylim=c(1000, 4500)) +
  scale_color_manual(values=wes_palette(n=4, name="Darjeeling2")) +
  theme(legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black"))
ggsave('../techReport/images/RTattentionValueGraph.pdf', height=3.5, width=6, units="in")

# Model fit graph
predict.rt.graph.data <- cbind(rt.graph.data, predict(rt.full, newdata = rt.graph.data, type="response"))

# predict.rt.graph.summary.data <- dcast(predict.rt.graph.data, task + att.quartile + abs.value ~ .,
#                                fun=mean, value.var="rt")
# colnames(predict.rt.graph.summary.data)[4] <- "rt"

predict.rt.graph.summary.data <- predict.rt.graph.data[, .(meanRT=mean(fit), sd=sd(fit), N=.N), by=.(task, att.quartile, abs.value)]
predict.rt.graph.summary.data[, ci:=qnorm(0.95)*sd/sqrt(N)]

predict.rt.graph.summary.data <- predict.rt.graph.summary.data[abs.value%%1==0,]

# New facet label names for task variable
task.labs <- c("Binary choice", "Strength-of-preference")
names(task.labs) <- c("binary", "continuous")

ggplot(predict.rt.graph.summary.data, aes(group=att.quartile, y=meanRT, x=abs.value)) +
  geom_point(aes(color=att.quartile), position=position_dodge(width=0.5)) +
  geom_line(aes(color=att.quartile), position=position_dodge(width=0.5), linetype = "dashed") +
  geom_errorbar(aes(ymin=meanRT-ci, ymax=meanRT+ci, color=att.quartile),
                position=position_dodge(width=0.5)) +
  facet_grid(. ~ task,
             labeller=labeller(task=task.labs)) +
  labs(x="Value difference", y="Reaction time (ms)", color="Attention \ndifference") +
  theme_bw() +
  coord_cartesian(ylim=c(0, 5000)) +
  scale_color_gradientn(colours = rainbow(4)) +
  theme(legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black"))
ggsave('../techReport/images/predictedRTattentionValueGraph.pdf', height=3.5, width=6, units="in")

## Choice ------------------------------------------------------------------------------------------
# Considering random effects
# Order effects
choice.order <- glm(recodedResponse ~ task*taskOrder, data=data[task!="valuation"],
                    family="binomial")
choice.order.all <- glm(recodedResponse ~ task*taskOrder*attention.difference*value.difference,
                        data=data[task!="valuation"], family="binomial")

# Random intercepts
choice.intercept.only <- glm(recodedResponse ~ 1,
                              data=data[task!="valuation",], family="binomial")

choice.random.intercept.only <- glmer(recodedResponse ~ 1 + 1|participantNo,
                                    data=data[task!="valuation",], family="binomial",
                                    control=glmerControl(optimizer="Nelder_Mead"))

choice.random.intercept.slope.att <- glmer(recodedResponse ~ (attention.difference)||participantNo,
                                      data=data[task!="valuation",], family="binomial",
                                      control=glmerControl(optimizer="Nelder_Mead"))

choice.random.intercept.slope.att.value <- glmer(recodedResponse ~
                                                 (attention.difference +
                                                    value.difference)||participantNo,
                                           data=data[task!="valuation",], family="binomial",
                                           control=glmerControl(optimizer="Nelder_Mead"))

choice.random.intercept.slope.att.value.task <- glmer(recodedResponse ~
  (attention.difference +  value.difference + task)||participantNo,
  data=data[task!="valuation",], family="binomial",
  control=glmerControl(optimizer="Nelder_Mead"))

anova(choice.random.intercept.only, choice.random.intercept.slope.att,
      choice.random.intercept.slope.att.value, choice.random.intercept.slope.att.value.task,
      test="Chisq")

choice.full <- glmer(recodedResponse ~ task*attention.difference*value.difference +
                      (1 + attention.difference + value.difference||participantNo),
                    data=data[task!="valuation",], family="binomial",
                    control=glmerControl(optimizer="Nelder_Mead",
                                         check.conv.grad=.makeCC("warning", tol=1e-1)))
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

# Plot predicted choices ---------------------------------------------------------------------------
summary(choice.full)

graph.data <- data[task!="valuation" & block==1,]
graph.data[, att.quartile:=cut(attention.difference,
                               quantile(attention.difference, probs=0:4/4),
                               include.lowest=T, labels=F), by=task]
graph.data[, predictChoice:= predict(choice.full, newdata=graph.data, type="response")]

graph.summary.data <- graph.data[, .(meanChoice=mean(predictChoice), sd=sd(predictChoice), N=.N), by=.(task, att.quartile, value.difference)]
graph.summary.data[, ci:=qnorm(0.95)*sd/sqrt(N)]

ggplot(graph.summary.data, aes(x=value.difference, y=meanChoice, group=as.factor(att.quartile),
                               color=as.factor(att.quartile))) +
  geom_point() +
  geom_line() +
  facet_grid(.~task) +
  geom_errorbar(aes(ymax=meanChoice+ci, ymin=meanChoice-ci)) +
  labs(x="Value difference", y="Reaction time (ms)", color="Attention \ndifference") +
  theme_bw() +
  scale_color_manual(values=wes_palette(n=4, name="Darjeeling2"))
ggsave("../techReport/images/predictedChoiceGraph.pdf", units="in", width=7, height=4)

# Number of fixations ------------------------------------------------------------------------------
nFixData <- dcast(fixations[task!="valuation"], participantNo + task + trial ~ aoi, fun=length, value.var="aoi")
nFixData[, relativeFixN:= right/(left+right)]
data <- merge(data, nFixData, by=c("participantNo", "task", "trial"))

# Random intercepts
fixN.intercept.only <- lm(relativeFixN ~ 1, data=data[task!="valuation",])

fixN.random.intercept.only <- lmer(relativeFixN ~ 1 + 1|participantNo,
                                   data=data[task!="valuation",],
                                   control=lmerControl(optimizer="Nelder_Mead"))

fixN.random.intercept.slope.att <- lmer(relativeFixN ~ (attention.difference)||participantNo,
                                        data=data[task!="valuation",],
                                        control=lmerControl(optimizer="Nelder_Mead"))
# Singular...
fixN.random.intercept.slope.value <- lmer(relativeFixN ~ (value.difference)||participantNo,
                                          data=data[task!="valuation",],
                                          control=lmerControl(optimizer="Nelder_Mead"))
fixN.random.intercept.slope.task <- lmer(relativeFixN ~ (task)||participantNo,
                                              data=data[task!="valuation",],
                                              control=lmerControl(optimizer="Nelder_Mead"))

anova(fixN.random.intercept.only, fixN.intercept.only, fixN.random.intercept.slope.att)


fixN.model <- lmer(relativeFixN ~ task*attention.difference*value.difference +
                             (1|participantNo),
                           data=data[task!="valuation" & block==1,])
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
durData <- dcast(fixations[task!="valuation"], participantNo + task + trial ~ aoi, fun=mean, value.var="fixLengthTr")
durData[, relativeFixDur:= right/(left+right)]
data <- merge(data, durData, by=c("participantNo", "task", "trial"))

# Random effects
fixD.intercept.only <- lm(relativeFixDur ~ 1, data=data[task!="valuation",])

fixD.random.intercept.only <- lmer(relativeFixDur ~ 1 + 1|participantNo,
                                   data=data[task!="valuation",],
                                   control=lmerControl(optimizer="Nelder_Mead"))

fixD.random.intercept.slope.value <- lmer(relativeFixDur ~ (value.difference)||participantNo,
                                          data=data[task!="valuation",],
                                          control=lmerControl(optimizer="Nelder_Mead"))
# Singular...
fixD.random.intercept.slope.att <- lmer(relativeFixDur ~ (attention.difference)||participantNo,
                                        data=data[task!="valuation",],
                                        control=lmerControl(optimizer="Nelder_Mead"))

fixD.random.intercept.slope.task <- lmer(relativeFixDur ~ (task)||participantNo,
                                         data=data[task!="valuation",],
                                         control=lmerControl(optimizer="Nelder_Mead"))

anova(fixD.random.intercept.only, fixD.intercept.only, fixD.random.intercept.slope.value)


fix.duration.model <- lm(relativeFixDur ~ task*attention.difference*value.difference,
                           data=data[task!="valuation" & block==1,])
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
