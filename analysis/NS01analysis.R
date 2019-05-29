# Analysis of NS01

## Setup -------------------------------------------------------------------------------------------
rm(list=ls())

require(data.table); require(plyr); require(ggplot2); require(MuMIn);
require(GGally); require(stargazer); require(BaylorEdPsych); require(lme4)

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
rt.intercept.only <- lm(rt ~ 1, data=data[task!="valuation" & block==1,], method="ML")

# Random intercepts
rt.random.intercept.only <- lmer(rt ~ 1 + 1|participantNo,
                                data=data[task!="valuation" & block==1,], method="ML")

# Random intercepts and slopes based on attention
rt.random.intercept.slope.att <- lmer(rt ~ 1 + abs(attention.difference)||participantNo,
                                          data=data[task!="valuation" & block==1,], method="ML")

# Random intercepts and slopes based on attention and value
rt.random.intercept.slope.att.val <- lmer(rt ~ 1 + abs(attention.difference)+abs(value.difference)||participantNo,
                                     data=data[task!="valuation" & block==1,], method="ML")

# Comparing random models
anova(rt.random.intercept.only, rt.intercept.only, rt.random.intercept.slope.att,
      rt.random.intercept.slope.att.val)

# Full model (i.e. including fixed effects)
rt.full <- lmer(rt ~ task*abs(attention.difference)*abs(value.difference) +
                  (abs(attention.difference) + abs(value.difference)||participantNo),
               data=data[task!="valuation" & block==1,], method="ML",
               control=lmerControl(optimizer="Nelder_Mead"))
summary(rt.full) # Model summary
confint(rt.full) # Get 0.95 confidence intervals
stargazer(rt.full, type="latex",
  title="Summary of coefficients of model predicting reaction time",
  covariate.labels=c("Task", "Attention", "Value", "Task:Attention", "Task:Value",
                     "Attention:Value", "Task:Attention:Value"),
  out="../techReport/tables/RTmodels.tex", style="default", ci=T, single.row=T,
  label="table:rtModel", table.placement="!b") # Print table to file

# Graph of attention and value on reaction time (think by quartiles)
choice.graph.data <- data[task!="valuation" & block==1,]
choice.graph.data[, att.quartile:= cut(abs(attention.difference),
                                       quantile(abs(attention.difference), probs=0:4/4),
                                       include.lowest=TRUE, labels=FALSE),
                  by=task]
choice.graph.data[, abs.value:=abs(value.difference)]
choice.graph.summary.data <- dcast(choice.graph.data, task + att.quartile + abs.value ~ .,
                                   fun=mean, value.var="rt")
colnames(choice.graph.summary.data)[4] <- "rt"

# New facet label names for task variable
task.labs <- c("Binary choice", "Strength-of-preference")
names(task.labs) <- c("binary", "continuous")

ggplot(choice.graph.summary.data, aes(x=att.quartile, y=rt, group=abs.value)) +
  geom_point(aes(color=abs.value)) +
  geom_line(aes(color=abs.value)) +
  facet_grid(. ~ task,
             labeller=labeller(task=task.labs)) +
  labs(x="Attention quartiles", y="Reaction time (ms)", color="Attention \ndifference") +
  theme_bw() +
  coord_cartesian(ylim=c(0, 8000)) +
  scale_color_gradientn(colours = rainbow(7)) +
  theme(legend.background = element_rect(size=0.2, linetype="solid",
                                         colour ="black"))
ggsave('../techReport/images/RTattentionValueGraph.pdf', height=3.5, width=6, units="in")

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

choice.random.intercept.slope.att.value <- glmer(recodedResponse ~ (attention.difference +  value.difference)||participantNo,
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
                    control=glmerControl(optimizer="Nelder_Mead", check.conv.grad=.makeCC("warning", tol=1e-1)))
summary(choice.full) # Model summary
confint(choice.full, method="Wald") # Get 0.95 confidence intervals
stargazer(choice.full, type="latex",
          title="Summary of coefficients of model predicting choice",
          covariate.labels=c("Task", "Attention", "Value", "Task:Attention", "Task:Value",
                             "Attention:Value", "Task:Attention:Value"),
          out="../techReport/tables/choiceModel.tex", style="default", ci=T, single.row=T,
          label="table:choiceModel", table.placement="!b") # Print table to file
