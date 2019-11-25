# Analysis of NS01

## Setup -------------------------------------------------------------------------------------------
rm(list=ls())

require(data.table); require(lme4); require(emmeans);
require(ggplot2); require(wesanderson); require(stargazer); require(GGally);

# require(plyr);  require(MuMIn); 
#   require(BaylorEdPsych); 
# require(nlme);  require(gridExtra)
# 
# require(RePsychLing) # install.packages('devtools'); devtools::install_github("dmbates/RePsychLing")

source("Preprocessing.R"); source('bsci.R'); source('NS01functions.R')

# Remove excluded trials
fixations <- fixations[exclude==0,]
# Remove fixations outside of trial
fixations <- fixations[intra_choice==1,]

## Get data per trial ------------------------------------------------------------------------------
data <- dcast(fixations, participantNo + task + taskOrder + block + trial + response + rt +
                lValue + rValue ~ aoi,
              fun = sum, value.var="fixLengthTr")

# Get predictors
data[, propAttentionLeft:=left/(right + left)]
data[, propAttentionRight:=1-propAttentionLeft]
data[, value.difference:=lValue-rValue]
data[, interaction:= lValue*propAttentionLeft - rValue*propAttentionRight]

# Recode responses to get binary responses from continuous task
data[, recodedResponse:= response]
data[task=="continuous", recodedResponse:= ifelse(recodedResponse<=0.5, 0, 1)]

data[, `:=`(participantNo=factor(participantNo), task=factor(task))]

data <- data[!(is.na(propAttentionLeft) & task!="valuation"),] # Remove one odd trial from one ppt.

## Reaction times ----------------------------------------------------------------------------------
data[, abs.value.diff:= abs(value.difference)]
data[, abs.attention.diff:= abs(propAttentionLeft-0.5)]
data[, abs.interaction:= abs(Timintr)]

# Order effects
rt.order <- lm(rt ~ task*taskOrder, data=data[task!="valuation",])

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
rt.random.intercept.slope.att <- lmer(rt ~ abs.attention.diff||participantNo,
                                          data=data[task!="valuation" & block==1,], REML=F)

rt.random.intercept.slope.val <- lmer(rt ~ abs.value.diff||participantNo,
                                      data=data[task!="valuation" & block==1,], REML=F)

# Random intercepts and slopes based on attention and value
rt.random.intercept.slope.att.val <- lmer(rt ~ (abs.attention.diff+abs.value.diff)||participantNo,
                                     data=data[task!="valuation" & block==1,], REML=F)

# Comparing random models
anova(rt.random.intercept.only, rt.intercept.only, rt.random.intercept.slope.att,
      rt.random.intercept.slope.att.val)

# Full model (i.e. including fixed effects)
rt.full <- lmer(rt ~ task + abs.attention.diff + abs.value.diff + Timintr +
                  task:abs.attention.diff + task:abs.value.diff + task:Timintr + 
                  task:abs.attention.diff:abs.value.diff + 
                  (abs.attention.diff + abs.value.diff||participantNo),
                data=data[task!="valuation",], 
                control=lmerControl(optimizer="Nelder_Mead"))

summary(rt.full) # Model summary
confint(rt.full) # Get 0.95 confidence intervals
class(rt.full) <- "lmerMod"

stargazer(rt.full, type="latex",
  title="Summary of coefficients of model predicting reaction time across both blocks.",
  covariate.labels=c("Task", "$\\vert A \\vert$", "$\\vert\\Delta_V\\vert$",
                     "Interaction",
                     "Task : $\\vert A \\vert$", "Task : $\\vert\\Delta_V\\vert$", 
                     "Task : Interaction",
                     "TaskBinary : $\\vert A \\vert$ :  $\\vert\\Delta_V\\vert$",
                     "TaskContinuous : $\\vert A \\vert$ :  $\\vert\\Delta_V\\vert$"),
  out="../techReport/tables/RTmodels.tex", style="default", ci=T, single.row=T,
  label="table:rtModel", table.placement="t",
  notes="\\footnotesize $\\vert\\Delta_A\\vert$ = absolute attention difference; $\\vert\\Delta_V\\vert$ = absolute value difference; ",
  notes.append=F, notes.align="l") # Print table to file

# Block 1
rt.full1 <- lmer(rt ~ task + abs.attention.diff + abs.value.diff + Timintr +
                   task:abs.attention.diff + task:abs.value.diff + task:Timintr + 
                   task:abs.attention.diff:abs.value.diff + 
                   (abs.attention.diff + abs.value.diff||participantNo),
                 data=data[task!="valuation" & block==1,], 
                 control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full1) # Model summary
confint(rt.full1) # Get 0.95 confidence intervals
class(rt.full1) <- "lmerMod"
stargazer(rt.full1, type="latex",
          title="Summary of coefficients of model predicting reaction time in Block 1.",
          covariate.labels=c("Task", "$\\vert A \\vert$", "$\\vert\\Delta_V\\vert$",
                             "Interaction",
                             "Task : $\\vert A \\vert$", "Task : $\\vert\\Delta_V\\vert$", 
                             "Task : Interaction",
                             "TaskBinary : $\\vert A \\vert$ :  $\\vert\\Delta_V\\vert$",
                             "TaskContinuous : $\\vert A \\vert$ :  $\\vert\\Delta_V\\vert$"),
          out="../techReport/tables/RTmodelBlock1.tex", style="default", ci=T, single.row=T,
          label="table:rtModelBlock1", table.placement="t",
          notes="\\footnotesize $\\vert\\Delta_A\\vert$ = absolute attention difference; $\\vert\\Delta_V\\vert$ = absolute value difference; ",
          notes.append=F, notes.align="l") # Print table to file

# Block 2 only 
rt.full2 <- lmer(rt ~ task + abs.attention.diff + abs.value.diff + Timintr +
                   task:abs.attention.diff + task:abs.value.diff + task:Timintr + 
                   task:abs.attention.diff:abs.value.diff + 
                   (abs.attention.diff + abs.value.diff||participantNo),
                 data=data[task!="valuation" & block==2,], 
                 control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full2) # Model summary
confint(rt.full2) # Get 0.95 confidence intervals
class(rt.full2) <- "lmerMod"
stargazer(rt.full2, type="latex",
          title="Summary of coefficients of model predicting reaction time in Block 2.",
          covariate.labels=c("Task", "$\\vert A \\vert$", "$\\vert\\Delta_V\\vert$",
                             "Interaction",
                             "Task : $\\vert A \\vert$", "Task : $\\vert\\Delta_V\\vert$", 
                             "Task : Interaction",
                             "TaskBinary : $\\vert A \\vert$ :  $\\vert\\Delta_V\\vert$",
                             "TaskContinuous : $\\vert A \\vert$ :  $\\vert\\Delta_V\\vert$"),
          out="../techReport/tables/RTmodelBlock2.tex", style="default", ci=T, single.row=T,
          label="table:rtModelBlock2", table.placement="t",
          notes="\\footnotesize $\\vert\\Delta_A\\vert$ = absolute attention difference; $\\vert\\Delta_V\\vert$ = absolute value difference; ",
          notes.append=F, notes.align="l") # Print table to file

# Full model (i.e. including fixed effects) & main effect of taskOrder
rt.full.order <- lmer(rt ~ task*abs.attention.diff*abs.value.diff*taskOrder +
                  (abs.attention.diff + abs.value.diff||participantNo),
                data=data[task!="valuation",],
                control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full.order) # Model summary
confint(rt.full.order) # Get 0.95 confidence intervals

# Graph of attention and value on reaction time (think by quartiles) -------------------------------
rt.graph.data <- data[task!="valuation" & block==1,]
rt.graph.data[, att.quartile:= cut(abs.attention.diff,
                                       quantile(abs.attention.diff, probs=0:4/4),
                                       include.lowest=TRUE, labels=FALSE),
                  by=task]

rt.graph.summary.data <- rt.graph.data[, .(meanRT=mean(rt), sd=sd(rt), N=.N), by=.(task, att.quartile, abs.value.diff)]
rt.graph.summary.data[, ci:=qnorm(0.95)*sd/sqrt(N)]

rt.graph.summary.data <- rt.graph.summary.data[abs.value.diff%%1==0,]

# New facet label names for task variable
task.labs <- c("Binary choice", "Strength-of-preference")
names(task.labs) <- c("binary", "continuous")

ggplot(rt.graph.summary.data, aes(group=as.factor(att.quartile), y=meanRT, x=abs.value.diff)) +
  geom_point(aes(color=as.factor(att.quartile)), position=position_dodge(width=0.5)) +
  geom_line(aes(color=as.factor(att.quartile)), position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=meanRT-ci, ymax=meanRT+ci, color=as.factor(att.quartile)),
                position=position_dodge(width=0.5)) +
  facet_grid(. ~ task,
             labeller=labeller(task=task.labs)) +
  labs(x="Value difference", y="Reaction time (ms)", color="Attention \ndifference") +
  theme_bw() +
  coord_cartesian(ylim=c(1000, 4500)) +
  scale_color_manual(values=wes_palette(n=4, name="Zissou1")) +
  theme(legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black"))
ggsave('../techReport/images/RTattentionValueGraph.pdf', height=3.5, width=6, units="in")

# RT model fit graph -------------------------------------------------------------------------------
predict.rt <- as.data.table(emmeans(rt.full, ~ abs.attention.diff * abs.value.diff * task,
                      at = list(abs.attention.diff =
                                  quantile(data[task!="valuation" & block==1,
                                                abs.attention.diff], c(.25, .5, .75)),
                                abs.value.diff = 0:6)))
predict.rt[, abs.attention.diff:=factor(abs.attention.diff, labels=c('small', 'med', 'large'))]

ggplot(predict.rt, aes(x=abs.value.diff, y=emmean, group=abs.attention.diff,
                       color=abs.attention.diff)) +
  geom_point(size=2) +
  geom_line( size=1) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), size=0.4) +
  facet_grid(.~task, labeller=labeller(task=task.labs)) +
  labs(x="Value difference", y="Reaction time (ms)", color="Attention \ndifference") +
  theme_bw() +
  coord_cartesian(ylim=c(1000, 4500)) +
  scale_color_manual(values=wes_palette(n=4, name="Zissou1")) +
  theme(legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black"))
ggsave('../techReport/images/predictedRTattentionValueGraph.pdf', height=3.5, width=6, units="in")

## RT by value graph -------------------------------------------------------------------------------
ggplot(data[task!="valuation",], aes(x=value.difference, y=rt)) +
  geom_point() +
  facet_grid(~ task)

ggplot(data[task!="valuation",], aes(x=factor(value.difference), y=rt)) +
  geom_boxplot() +
  facet_grid(~ task)

## Choice ------------------------------------------------------------------------------------------
# Considering random effects
# Order effects
choice.order <- glm(recodedResponse ~ task*taskOrder, data=data[task!="valuation"],
                    family="binomial")
choice.order.all <- glm(recodedResponse ~ task*taskOrder*propAttentionLeft*value.difference,
                        data=data[task!="valuation"], family="binomial")

data <- data[taskOrder==1,]

# Determining random effects
choice.intercept.only <- glm(recodedResponse ~ 1,
                              data=data[task!="valuation",], family="binomial")

choice.random.intercept.only <- glmer(recodedResponse ~ 1|participantNo,
                                    data=data[task!="valuation",], family="binomial",
                                    control=glmerControl(optimizer="Nelder_Mead"))

choice.random.intercept.slope.value <- glmer(recodedResponse ~ value.difference||participantNo,
                                           data=data[task!="valuation",], family="binomial",
                                           control=glmerControl(optimizer="Nelder_Mead"))

choice.random.intercept.slope.att.value <- glmer(recodedResponse ~
                                                 (propAttentionLeft + value.difference)||participantNo,
                                           data=data[task!="valuation",], family="binomial",
                                           control=glmerControl(optimizer="Nelder_Mead"))

anova(choice.random.intercept.only, choice.random.intercept.slope.value,
      choice.random.intercept.slope.att.value,
      test="Chisq")

choice.full <- glmer(recodedResponse ~ task + propAttentionLeft + value.difference + 
                       task:propAttentionLeft + task:value.difference + interaction + task:interaction +
                      (value.difference||participantNo),
                    data=data[task!="valuation",], family="binomial",
                    control=glmerControl(optimizer="bobyqa",
                                         check.conv.grad=.makeCC("warning", tol=1)))

summary(choice.full) # Model summary
confint(choice.full, method="Wald") # Get 0.95 confidence intervals
stargazer(choice.full, type="latex",
          title="Summary of coefficients of model predicting choice (both blocks).",
          covariate.labels=c("Task", "$\\Delta_A$", "$\\Delta_V$",
                             "Task : $\\Delta_A$", "Task : $\\Delta_V$",
                             "$\\Delta_A$ : $\\Delta_V$",
                             "Task : $\\Delta_A$ :  $\\Delta_V$"),
          style="default", ci=T, single.row=T, out="../paper/tables/NS01choiceModel.tex",
          label="table:choiceModelAll", table.placement="!b",
          notes="\\footnotesize $\\Delta_A$ = attention difference; $\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

choice.full.block1 <- glmer(recodedResponse ~ task + propFixLeft + value.difference + 
                              task:propFixLeft + task:value.difference + Timintr + task:Timintr +
                              (1 + propFixLeft + value.difference||participantNo),
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

choice.full.block2 <- glmer(recodedResponse ~ task + propFixLeft + value.difference + 
                              task:propFixLeft + task:value.difference + Timintr + task:Timintr +
                              (1 + propFixLeft + value.difference||participantNo),
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

# Choice: by task ----------------------------------------------------------------------------------
# Continuous
evaluation.continuous <- lmer(response ~ propAttentionLeft + value.difference + Timintr + 
                                (value.difference ||participantNo),
                              data=data[task=="continuous",])
summary(evaluation.continuous)
confint(evaluation.continuous)

evaluation.binary <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr +
                             (1 + value.difference ||participantNo), family="binomial",
                           data=data[task=="continuous",])
summary(evaluation.binary)

choice.binary <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr +
                         (1 + value.difference ||participantNo), family="binomial",
                       data=data[task=="binary",])
summary(choice.binary)

choice <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr + task +
                  propAttentionLeft*task + value.difference*task + Timintr*task +
                  (1 + value.difference ||participantNo), family="binomial",
                data=data[task!="valuation",])
summary(choice)

stargazer(evaluation.binary, choice.binary, type="latex",
          out="../techReport/tables/choiceByTask.tex", 
          title="Summary of coefficients of model predicting choice (both blocks).",
          covariate.labels=c("Attention", "$\\Delta_V$", "Interaction"),
          column.labels = c("Strength-of-prefence", "Binary choice"), dep.var.labels = "Choice",
          style="default", ci=T, single.row=T,
          label="table:choiceByTask", table.placement="!b",
          notes="\\footnotesize$\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

# Just first block 
valuation.continuous.block1 <- lmer(response ~ propAttentionLeft + value.difference + Timintr + 
                               (1 + value.difference ||participantNo),
                             data=data[task=="continuous" & block==1,])
summary(valuation.continuous.block1)

evaluation.binary.block1 <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr +
                             (1 + value.difference ||participantNo), family="binomial",
                           data=data[task=="continuous" & block==1,])
summary(evaluation.binary.block1)

choice.binary.block1 <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr +
                         (1 + value.difference ||participantNo), family="binomial",
                       data=data[task=="binary" & block==1,])
summary(choice.binary.block1)

choice.block1 <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr + task +
                  propAttentionLeft*task + value.difference*task + Timintr*task +
                  (1 + value.difference ||participantNo), family="binomial",
                data=data[task!="valuation",])
summary(choice.block1)

stargazer(evaluation.binary.block1, choice.binary.block1, type="latex",
          out="../techReport/tables/choiceByTaskBlock1.tex", 
          title="Summary of coefficients of model predicting choice by task (Block 1).",
          covariate.labels=c("Attention", "$\\Delta_V$", "Interaction"),
          column.labels = c("Strength-of-prefence", "Binary choice"), dep.var.labels = "Choice",
          style="default", ci=T, single.row=T,
          label="table:choiceByTaskBlock1", table.placement="!b",
          notes="\\footnotesize$\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

# Just second block 
valuation.continuous.block2 <- lmer(response ~ propAttentionRight + value.difference + Timintr + 
                                      (1 + value.difference ||participantNo),
                                    data=data[task=="continuous" & block==2,])
summary(valuation.continuous.block2)

evaluation.binary.block2 <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr +
                                    (1 + value.difference ||participantNo), family="binomial",
                                  data=data[task=="continuous" & block==2,])
summary(evaluation.binary.block2)

choice.binary.block2 <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr +
                                (1 + value.difference ||participantNo), family="binomial",
                              data=data[task=="binary" & block==2,])
summary(choice.binary.block2)

choice.block2 <- glmer(recodedResponse ~ propAttentionRight + value.difference + Timintr + task +
                         propAttentionLeft*task + value.difference*task + Timintr*task +
                         (1 + value.difference ||participantNo), family="binomial",
                       data=data[task!="valuation",])
summary(choice.block2)

stargazer(evaluation.binary.block2, choice.binary.block2, type="latex",
          out="../techReport/tables/choiceByTaskBlock2.tex", 
          title="Summary of coefficients of model predicting choice by task (Block 2).",
          covariate.labels=c("Attention", "$\\Delta_V$", "Interaction"),
          column.labels = c("Strength-of-prefence", "Binary choice"), dep.var.labels = "Choice",
          style="default", ci=T, single.row=T,
          label="table:choiceByTaskBlock2", table.placement="!b",
          notes="\\footnotesize$\\Delta_V$ = value difference; ",
          notes.append=F, notes.align="l") # Print table to file

# Graph of coefficients and confidence intervals ---------------------------------------------------
models <- as.data.table(expand.grid(predictor=c("Intercept", "Attention", "Value", "Interaction"),
                                    block=c("Block 1", "Block 2", "Both"),
                                    task=c("continuous", "continuousRecode", "binary")))

models[, model:=rep(1:9, each=4)]
models[, c("b", "lower", "upper", "bScaled", "lowerScaled", "upperScaled"):=0]

for (i in 1:9){
  if (models[model==i, block][1]=="Block 1"){
    tmp <- data[block==1,]
  } else if (models[model==i, block][1]=="Block 2"){
    tmp <- data[block==2,]
  } else {
    tmp <- data
  }
  if (models[model==i, task][1]=="binary"){
    tmp <- tmp[task=="binary",]
  } else {
    tmp <- tmp[task=="continuous",]
  }
  if (models[model==i, task][1]=="continuous"){
    m <- lmer(response ~ propAttentionLeft + value.difference + Timintr + 
                 (value.difference ||participantNo), tmp)
    mScaled <- lmer(response ~ scale(propAttentionLeft) + scale(value.difference) + scale(Timintr) + 
                       (scale(value.difference)||participantNo), 
                     tmp)
  } else {
    m <- glmer(recodedResponse ~ propAttentionLeft + value.difference + Timintr + 
                 (value.difference ||participantNo), tmp, family="binomial", glmerControl(tol=1e-8))
    mScaled <- glmer(recodedResponse ~ scale(propAttentionLeft) + scale(value.difference) + scale(Timintr) + 
                       (scale(value.difference)||participantNo), 
                     tmp, family="binomial", glmerControl(tol=1e-8))
  }
  
  models[model==i, "b"] <- as.double(as.list(getME(m, "beta")))
  models[model==i, "bScaled"] <- as.double(as.list(getME(mScaled, "beta")))
  
  CI <- confint(m)
  models[model==i, lower:=as.double(as.list(CI[c("(Intercept)", "propAttentionLeft", 
                                                 "value.difference", "Timintr"), 1]))]
  models[model==i, upper:=as.double(as.list(CI[c("(Intercept)", "propAttentionLeft", 
                                                 "value.difference", "Timintr"), 2]))]
  
  CIscaled <- confint(mScaled)
  models[model==i, lowerScaled:=as.double(as.list(CIscaled[c("(Intercept)", 
                                                             "scale(propAttentionLeft)",
                                                             "scale(value.difference)", 
                                                             "scale(Timintr)"), 1]))]
  models[model==i, upperScaled:=as.double(as.list(CIscaled[c("(Intercept)", 
                                                             "scale(propAttentionLeft)",
                                                             "scale(value.difference)", 
                                                             "scale(Timintr)"), 2]))]
}

ggplot(models, aes(x=task, y=b, fill=predictor)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(block~.) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(), size=0.3) +
  scale_fill_manual(values=wes_palette(n=4, name="Zissou1")) +
  theme_bw()
ggsave("../techReport/images/parameterEstimates.pdf", units="in", width=8, height=12)


ggplot(models, aes(x=task, y=bScaled, fill=predictor)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_grid(block~.) + 
  geom_errorbar(aes(ymin=lowerScaled, ymax=upperScaled), position=position_dodge(), size=0.3) +
  scale_fill_manual(values=wes_palette(n=4, name="Zissou1")) +
  theme_bw()
ggsave("../techReport/images/parameterEstimatesScaled.pdf", units="in", width=8, height=12)



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
