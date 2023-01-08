library(utils)
workdir <- 'S:/MNHS-Psych/ANDL-Lab-DGS-Study/data_for_manuscripts/tim_task_paper'
dgs_hr <- read.csv(file.path(workdir,"/dgs_hr.csv", fsep=""))
View(dgs_hr)
DGS_saliva_raw <- read.csv(file.path(workdir,"/DGS_saliva_raw.csv", fsep="")) 
View(DGS_saliva_raw)

############ Heart rate ############ 
dgs_hr$task_hr <- rowMeans(dgs_hr[ , c("selfconn_hr","selfdis_hr","other_hr")])
mean(dgs_hr$bl_hr) #87.66667
mean(dgs_hr$twombl_hr) # 87.73333
mean(dgs_hr$task_hr) # 84.91111
mean(dgs_hr$selfconn_hr) # 85.77778
mean(dgs_hr$selfdis_hr) #  84.95556
mean(dgs_hr$other_hr) #  84
mean(dgs_hr$questionnaire_hr, na.rm = TRUE) #80.63636

### The "baseline" period is high, even at the end of the 7 min baseline. This 
# is likely due to poor study design where it 
# is the first thing they do after they have just travelled in and sat down at 
# the lab. However, we also measured a backup "baseline" at the end 
# of the session, taken when they were doing questionnaires. Although this is 
# after they have experienced the task, there was some time 
# between the task and this "recovery", and physiologically make more sense to 
# use as a baseline after they have been sitting for awhile.
# It also makes the heart rate during the task unlikely to be influenced by 
# purely physiological things because they have been sitting and "relaxing" 
# for 7 minutes first. But it also a lesson in study design where sitting for 
# any length of time likely has a compounding effect on physio.


library(tidyr)
dgs_hr_long <- gather(dgs_hr, condition, hr, bl_hr:task_hr, factor_key = TRUE)
hr_long_reduced <- dgs_hr_long[which(dgs_hr_long$condition=="task_hr" | 
                                       dgs_hr_long$condition=="selfconn_hr" | 
                                       dgs_hr_long$condition=="selfdis_hr" | 
                                       dgs_hr_long$condition=="other_hr" |
                                       dgs_hr_long$condition=="questionnaire_hr"), ]

ggplot(hr_long_reduced, aes(x = condition, y = hr, group = dgs_id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

# Plot responses (and test differences between conditions) 
# for those that subjectively found the task stressful vs not
# (e.g., maybe group people who said "Not at all" to "How stressful was 
# it to watch your own video when you knew the other person WAS watching")


# paired t-tests

### Questionnaire baseline vs. task
qbl_task <-  na.omit(dgs_hr_long[which(dgs_hr_long$condition=="questionnaire_hr" | dgs_hr_long$condition=="task_hr"), ])
qbl_task <- filter(qbl_task, dgs_id!="DGS130")
t.test(hr ~ condition, data = qbl_task, paired = TRUE)
#t = -5.5882, df = 43, p-value < .001

### Questionnaire baseline vs. self-connected
qbl_selfc <-  na.omit(dgs_hr_long[which(dgs_hr_long$condition=="questionnaire_hr" | dgs_hr_long$condition=="selfconn_hr"), ])
qbl_selfc <- filter(qbl_selfc, dgs_id!="DGS130")
t.test(hr ~ condition, data = qbl_selfc, paired = TRUE)
#t = 5.6515, df = 43, p-value < .001

### Self-connected vs. self-disconnected
selfc_selfdis <-  dgs_hr_long[which(dgs_hr_long$condition=="selfconn_hr" | dgs_hr_long$condition=="selfdis_hr"), ]
t.test(hr ~ condition, data = selfc_selfdis, paired = TRUE)
#t = 1.5158, df = 44, p-value = .1367

### Self-connected vs. other
selfc_other <-  dgs_hr_long[which(dgs_hr_long$condition=="selfconn_hr" | dgs_hr_long$condition=="other_hr"), ]
t.test(hr ~ condition, data = selfc_other, paired = TRUE)
#t = 3.7552, df = 44, p-value = .0005051
# This is compelling because these conditions were counterbalanced, so should not be influenced simply by how long they were sitting

# Plot these: see http://www.sthda.com/english/articles/32-r-graphics-essentials/132-plot-grouped-data-box-plot-bar-plot-and-more/


############ Saliva ############ 
summary(DGS_saliva_raw)

# Note: Raw concentrations for DHEA, CRP, and IL-6 are all in pg/ml, but 
# cortisol is in micrograms/dL. Note that DHEA and CRP were primary
# analyses and IL-6 and cortisol were exploratory


# Because IL-6 had some that were not detected, these vars are char
# We can impute the "not detected" (as they were too low) as half LLD.
# For salimetrics IL-6, the lower limit of sensitivity is 0.07 pg/ml.
# Therefore we can impute "not detected" as 0.07/2

DGS_saliva_raw$IL6_1[DGS_saliva_raw$IL6_1 == 'Not detect'] <- 0.07/2
DGS_saliva_raw$IL6_2[DGS_saliva_raw$IL6_2 == 'Not detect'] <- 0.07/2
DGS_saliva_raw$IL6_3[DGS_saliva_raw$IL6_3 == 'Not detect'] <- 0.07/2

DGS_saliva_raw$IL6_1 <- as.numeric(DGS_saliva_raw$IL6_1)
DGS_saliva_raw$IL6_2 <- as.numeric(DGS_saliva_raw$IL6_2)
DGS_saliva_raw$IL6_3 <- as.numeric(DGS_saliva_raw$IL6_3)

library(ggplot2)

ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_1)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_2)) +   
  geom_boxplot() +
  ylab("DHEA 2 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_3)) +   
  geom_boxplot() +
  ylab("DHEA 3 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

ggplot(DGS_saliva_raw, aes(x = "", y = CRP_1)) +   
  geom_boxplot() +
  ylab("CRP 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_2)) +   
  geom_boxplot() +
  ylab("CRP 2 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_3)) +   
  geom_boxplot() +
  ylab("CRP 3 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

ggplot(DGS_saliva_raw, aes(x = "", y = IL6_1)) +   
  geom_boxplot() +
  ylab("IL-6 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = IL6_2)) +   
  geom_boxplot() +
  ylab("IL-6 2 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = IL6_3)) +   
  geom_boxplot() +
  ylab("IL-6 3 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

ggplot(DGS_saliva_raw, aes(x = "", y = CortMcgdL_1)) +   
  geom_boxplot() +
  ylab("Cortisol 1 (mcg/dl)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CortMcgdL_2)) +   
  geom_boxplot() +
  ylab("Cortisol 2 (mcg/dl)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CortMcgdL_3)) +   
  geom_boxplot() +
  ylab("Cortisol 3 (mcg/dl)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

library(psych)
describe(DGS_saliva_raw$DHEA_1)
describe(DGS_saliva_raw$DHEA_2)
describe(DGS_saliva_raw$DHEA_3)
describe(DGS_saliva_raw$CRP_1)
describe(DGS_saliva_raw$CRP_2)
describe(DGS_saliva_raw$CRP_3)
describe(DGS_saliva_raw$IL6_1)
describe(DGS_saliva_raw$IL6_2)
describe(DGS_saliva_raw$IL6_3)
describe(DGS_saliva_raw$CortMcgdL_1)
describe(DGS_saliva_raw$CortMcgdL_2)
describe(DGS_saliva_raw$CortMcgdL_3)

library(rcompanion)

# Why we didn't use transformTukey:
# The problem with the Tukey transforms is that every "variable" uses a slightly 
# different transformation. This is problematic when calculating magnitudes of
# change for the same analyte. Therefore, the decision was made to log transform
# everything instead to be consistent. 

# Another note: why we didn't sue wilcox test - too conservative and sample size is small

DGS_saliva_raw$DHEA_ln_1 <- log(DGS_saliva_raw$DHEA_1)
DGS_saliva_raw$DHEA_ln_2 <- log(DGS_saliva_raw$DHEA_2)
DGS_saliva_raw$DHEA_ln_3 <- log(DGS_saliva_raw$DHEA_3)

DGS_saliva_raw$CRP_ln_1 <- log(DGS_saliva_raw$CRP_1)
DGS_saliva_raw$CRP_ln_2 <- log(DGS_saliva_raw$CRP_2)
DGS_saliva_raw$CRP_ln_3 <- log(DGS_saliva_raw$CRP_3)

DGS_saliva_raw$IL6_ln_1 <- log(DGS_saliva_raw$IL6_1)
DGS_saliva_raw$IL6_ln_2 <- log(DGS_saliva_raw$IL6_2)
DGS_saliva_raw$IL6_ln_3 <- log(DGS_saliva_raw$IL6_3)

DGS_saliva_raw$Cort_ln_1 <- log(DGS_saliva_raw$CortMcgdL_1)
DGS_saliva_raw$Cort_ln_2 <- log(DGS_saliva_raw$CortMcgdL_2)
DGS_saliva_raw$Cort_ln_3 <- log(DGS_saliva_raw$CortMcgdL_3)

describe(DGS_saliva_raw$DHEA_ln_1)
describe(DGS_saliva_raw$DHEA_ln_2)
describe(DGS_saliva_raw$DHEA_ln_3)
describe(DGS_saliva_raw$CRP_ln_1)
describe(DGS_saliva_raw$CRP_ln_2)
describe(DGS_saliva_raw$CRP_ln_3)
describe(DGS_saliva_raw$IL6_ln_1)
describe(DGS_saliva_raw$IL6_ln_2)
describe(DGS_saliva_raw$IL6_ln_3)
describe(DGS_saliva_raw$Cort_ln_1)
describe(DGS_saliva_raw$Cort_ln_2)
describe(DGS_saliva_raw$Cort_ln_3)

ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_ln_1)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_ln_2)) +   
  geom_boxplot() +
  ylab("DHEA 2 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_ln_3)) +   
  geom_boxplot() +
  ylab("DHEA 3 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_ln_1)) +   
  geom_boxplot() +
  ylab("CRP 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_ln_2)) +   
  geom_boxplot() +
  ylab("CRP 2 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_ln_3)) +   
  geom_boxplot() +
  ylab("CRP 3 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = IL6_ln_1)) +   
  geom_boxplot() +
  ylab("IL-6 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = IL6_ln_2)) +   
  geom_boxplot() +
  ylab("IL-6 2 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = IL6_ln_3)) +   
  geom_boxplot() +
  ylab("IL-6 3 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = Cort_ln_1)) +   
  geom_boxplot() +
  ylab("Cortisol 1 (mcg/dl)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = Cort_ln_2)) +   
  geom_boxplot() +
  ylab("Cortisol 2 (mcg/dl)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = Cort_ln_3)) +   
  geom_boxplot() +
  ylab("Cortisol 3 (mcg/dl)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

# However, winsorising fixes skew without need for transforming!
high_DHEA1 <- mean(DGS_saliva_raw$DHEA_1) + (3*sd(DGS_saliva_raw$DHEA_1))
high_DHEA2 <- mean(DGS_saliva_raw$DHEA_2) + (3*sd(DGS_saliva_raw$DHEA_2))
high_DHEA3 <- mean(DGS_saliva_raw$DHEA_3, na.rm = TRUE) + (3*sd(DGS_saliva_raw$DHEA_3, na.rm = TRUE))

DGS_saliva_raw$DHEA_wi_1 <- ifelse(DGS_saliva_raw$DHEA_1 > high_DHEA1, high_DHEA1, DGS_saliva_raw$DHEA_1) 
DGS_saliva_raw$DHEA_wi_2 <- ifelse(DGS_saliva_raw$DHEA_2 > high_DHEA2, high_DHEA2, DGS_saliva_raw$DHEA_2)
DGS_saliva_raw$DHEA_wi_3 <- ifelse(DGS_saliva_raw$DHEA_3 > high_DHEA3, high_DHEA3, DGS_saliva_raw$DHEA_3)

describe(DGS_saliva_raw$DHEA_wi_1)
describe(DGS_saliva_raw$DHEA_wi_2)
describe(DGS_saliva_raw$DHEA_wi_3)

high_CRP1 <- mean(DGS_saliva_raw$CRP_1) + (3*sd(DGS_saliva_raw$CRP_1))
high_CRP2 <- mean(DGS_saliva_raw$CRP_2) + (3*sd(DGS_saliva_raw$CRP_2))
high_CRP3 <- mean(DGS_saliva_raw$CRP_3, na.rm = TRUE) + (3*sd(DGS_saliva_raw$CRP_3, na.rm = TRUE))

DGS_saliva_raw$CRP_wi_1 <- ifelse(DGS_saliva_raw$CRP_1 > high_CRP1, high_CRP1, DGS_saliva_raw$CRP_1) 
DGS_saliva_raw$CRP_wi_2 <- ifelse(DGS_saliva_raw$CRP_2 > high_CRP2, high_CRP2, DGS_saliva_raw$CRP_2)
DGS_saliva_raw$CRP_wi_3 <- ifelse(DGS_saliva_raw$CRP_3 > high_CRP3, high_CRP3, DGS_saliva_raw$CRP_3)

describe(DGS_saliva_raw$CRP_wi_1)
describe(DGS_saliva_raw$CRP_wi_2)
describe(DGS_saliva_raw$CRP_wi_3)

high_IL61 <- mean(DGS_saliva_raw$IL6_1) + (3*sd(DGS_saliva_raw$IL6_1))
high_IL62 <- mean(DGS_saliva_raw$IL6_2) + (3*sd(DGS_saliva_raw$IL6_2))
high_IL63 <- mean(DGS_saliva_raw$IL6_3, na.rm = TRUE) + (3*sd(DGS_saliva_raw$IL6_3, na.rm = TRUE))

DGS_saliva_raw$IL6_wi_1 <- ifelse(DGS_saliva_raw$IL6_1 > high_IL61, high_IL61, DGS_saliva_raw$IL6_1) 
DGS_saliva_raw$IL6_wi_2 <- ifelse(DGS_saliva_raw$IL6_2 > high_IL62, high_IL62, DGS_saliva_raw$IL6_2)
DGS_saliva_raw$IL6_wi_3 <- ifelse(DGS_saliva_raw$IL6_3 > high_IL63, high_IL63, DGS_saliva_raw$IL6_3)

describe(DGS_saliva_raw$IL6_wi_1)
describe(DGS_saliva_raw$IL6_wi_2)
describe(DGS_saliva_raw$IL6_wi_3)

high_Cort1 <- mean(DGS_saliva_raw$CortMcgdL_1) + (3*sd(DGS_saliva_raw$CortMcgdL_1))
high_Cort2 <- mean(DGS_saliva_raw$CortMcgdL_2) + (3*sd(DGS_saliva_raw$CortMcgdL_2))
high_Cort3 <- mean(DGS_saliva_raw$CortMcgdL_3, na.rm = TRUE) + (3*sd(DGS_saliva_raw$CortMcgdL_3, na.rm = TRUE))

DGS_saliva_raw$Cort_wi_1 <- ifelse(DGS_saliva_raw$CortMcgdL_1 > high_Cort1, high_Cort1, DGS_saliva_raw$CortMcgdL_1) 
DGS_saliva_raw$Cort_wi_2 <- ifelse(DGS_saliva_raw$CortMcgdL_2 > high_Cort2, high_Cort2, DGS_saliva_raw$CortMcgdL_2)
DGS_saliva_raw$Cort_wi_3 <- ifelse(DGS_saliva_raw$CortMcgdL_3 > high_Cort3, high_Cort3, DGS_saliva_raw$CortMcgdL_3)

describe(DGS_saliva_raw$Cort_wi_1)
describe(DGS_saliva_raw$Cort_wi_2)
describe(DGS_saliva_raw$Cort_wi_3)

library(panelr)
salivalong <- long_panel(DGS_saliva_raw, prefix = "_", begin = 1, end = 3, id="id", wave="timepoint", label_location = "end")


ggplot(salivalong, aes(x = timepoint, y = DHEA_wi)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:3) +
  facet_wrap("id", labeller = label_both)

ggplot(salivalong, aes(x = timepoint, y = CRP_wi)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:3) +
  facet_wrap("id", labeller = label_both)

ggplot(salivalong, aes(x = timepoint, y = IL6_wi)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:3) +
  facet_wrap("id", labeller = label_both)

ggplot(salivalong, aes(x = timepoint, y = Cort_wi)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = 1:3) +
  facet_wrap("id", labeller = label_both)

ggplot(salivalong, aes(x = timepoint, y = DHEA_wi, colour = id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

ggplot(salivalong, aes(x = timepoint, y = CRP_wi, colour = id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

ggplot(salivalong, aes(x = timepoint, y = IL6_wi, colour = id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

ggplot(salivalong, aes(x = timepoint, y = Cort_wi, colour = id)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")


T1_T2 <-  salivalong[which(salivalong$timepoint=="1" | salivalong$timepoint=="2"), ]

t.test(DHEA_wi ~ timepoint, data = T1_T2, paired = TRUE)
t.test(CRP_wi ~ timepoint, data = T1_T2, paired = TRUE)
t.test(IL6_wi ~ timepoint, data = T1_T2, paired = TRUE)
t.test(Cort_wi ~ timepoint, data = T1_T2, paired = TRUE)


T1_T3 <-  salivalong[which(salivalong$timepoint=="1" | salivalong$timepoint=="3"), ]
T1_T3 <- filter(T1_T3, ID !="DGS165")

t.test(DHEA_wi ~ timepoint, data = T1_T3, paired = TRUE)
t.test(CRP_wi ~ timepoint, data = T1_T3, paired = TRUE)
t.test(IL6_wi ~ timepoint, data = T1_T3, paired = TRUE)
t.test(Cort_wi ~ timepoint, data = T1_T3, paired = TRUE)


T2_T3 <-  salivalong[which(salivalong$timepoint=="2" | salivalong$timepoint=="3"), ]
T2_T3 <- filter(T2_T3, ID !="DGS165")

t.test(DHEA_wi ~ timepoint, data = T2_T3, paired = TRUE)
t.test(CRP_wi ~ timepoint, data = T2_T3, paired = TRUE)
t.test(IL6_wi ~ timepoint, data = T2_T3, paired = TRUE)
t.test(Cort_wi ~ timepoint, data = T2_T3, paired = TRUE)


# Plot the differences
library(skimr)
skim(salivalong)
T1 <- salivalong[which(salivalong$timepoint=="1"),]
T2 <- salivalong[which(salivalong$timepoint=="2"),]
T3 <- salivalong[which(salivalong$timepoint=="3"),]
library(plotrix)
se_dheawi1 <- std.error(T1$DHEA_wi)
se_dheawi2 <- std.error(T2$DHEA_wi)
se_dheawi3 <- std.error(T3$DHEA_wi)

mean_dheawi1 <- mean(T1$DHEA_wi)
mean_dheawi2 <- mean(T2$DHEA_wi)
mean_dheawi3 <- mean(T3$DHEA_wi, na.rm = TRUE)

ggplot(salivalong, aes(x=timepoint, y=mean_dheawi1)) + # add colour = group if want to separate
  geom_errorbar(aes(ymin=mean_dheawi1-se_dheawi1, ymax=mean_dheawi1+se_dheawi1), width=.1) +
  geom_line() +
  geom_point()

# ALSO!! Check if anyone needs to be excluded due to saliva questions!!

# Plot responses (and test differences between timepoints) 
# for those that subjectively found the task stressful vs not
# (e.g., maybe group people who said "Not at all" to "How stressful was 
# it to watch your own video when you knew the other person WAS watching")

# Next - calculate area under the curve - even if it didn't "work" overall,
# it may just be that it's an individual difference - some don't respond. 
# Could also try limit to people that subjectively found it stressful



