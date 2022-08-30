library(readr)
dgs_hr <- read_csv("~/dgs_hr.csv")
View(dgs_hr)
DGS_saliva_raw <- read_csv("C:/Users/michelle/Downloads/DGS_saliva_raw.csv")
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

### The "baseline" period is high, even at the end of the 7 min baseline. This is likely due to poor study design where it 
# is the first thing they do after they have just travelled in and sat down at the lab. However, we also measured a backup "baseline" at the end 
# of the session, taken when they were doing questionnaires. Although this is after they have experienced the task, there was some time 
# between the task and this "recovery", and physiologically make more sense to use as a baseline after they have been sitting for awhile.
# It also makes the heart rate during the task unlikley to be influenced by purely physiological things because they have been sitting and "relaxing" 
# for 7 minutes first. But it also a lesson in study design where sitting for any length of time likely has a compounding effect on physio.


library(tidyr)
dgs_hr_long <- gather(dgs_hr, condition, hr, bl_hr:avgbl_hr, factor_key = TRUE)


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


############ Saliva ############ 
summary(DGS_saliva_raw)

library(ggplot2)

ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_1)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_2)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_3)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_1)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_2)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_3)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

library(psych)
describe(DGS_saliva_raw$DHEA_1)
describe(DGS_saliva_raw$DHEA_2)
describe(DGS_saliva_raw$DHEA_3)
describe(DGS_saliva_raw$CRP_1)
describe(DGS_saliva_raw$CRP_2)
describe(DGS_saliva_raw$CRP_3)

library(rcompanion)
DGS_saliva_raw$DHEA_1_tu <- transformTukey(DGS_saliva_raw$DHEA_1)
DGS_saliva_raw$DHEA_2_tu <- transformTukey(DGS_saliva_raw$DHEA_2)
DGS_saliva_raw$DHEA_3_tu <- transformTukey(DGS_saliva_raw$DHEA_3)

DGS_saliva_raw$CRP_1_tu <- transformTukey(DGS_saliva_raw$CRP_1)
DGS_saliva_raw$CRP_2_tu <- transformTukey(DGS_saliva_raw$CRP_2)
DGS_saliva_raw$CRP_3_tu <- transformTukey(DGS_saliva_raw$CRP_3)

describe(DGS_saliva_raw$DHEA_1_tu)
describe(DGS_saliva_raw$DHEA_2_tu)
describe(DGS_saliva_raw$DHEA_3_tu)
describe(DGS_saliva_raw$CRP_1_tu)
describe(DGS_saliva_raw$CRP_2_tu)
describe(DGS_saliva_raw$CRP_3_tu)
# multiply by -1 to keep direction the same

DGS_saliva_raw$DHEA_1_tu_f <- DGS_saliva_raw$DHEA_1_tu * -1
DGS_saliva_raw$DHEA_2_tu_f <- DGS_saliva_raw$DHEA_2_tu * -1
DGS_saliva_raw$DHEA_3_tu_f <- DGS_saliva_raw$DHEA_3_tu * -1
DGS_saliva_raw$CRP_1_tu_f <- DGS_saliva_raw$CRP_1_tu * -1
DGS_saliva_raw$CRP_2_tu_f <- DGS_saliva_raw$CRP_2_tu * -1
DGS_saliva_raw$CRP_3_tu_f <- DGS_saliva_raw$CRP_3_tu * -1

ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_1_tu_f)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_2_tu_f)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_3_tu_f)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_1_tu_f)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_2_tu_f)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")
ggplot(DGS_saliva_raw, aes(x = "", y = CRP_3_tu_f)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

describe(DGS_saliva_raw$DHEA_1_tu_f)
describe(DGS_saliva_raw$DHEA_2_tu_f)
describe(DGS_saliva_raw$DHEA_3_tu_f)
describe(DGS_saliva_raw$CRP_1_tu_f)
describe(DGS_saliva_raw$CRP_2_tu_f)
describe(DGS_saliva_raw$CRP_3_tu_f)

ggplot(DGS_saliva_raw, aes(x = "", y = DHEA_1_tu_f)) +   
  geom_boxplot() +
  ylab("DHEA 1 (pg/ml)") +
  ggtitle("DGS") +
  geom_smooth(method='lm', color="black")

library(reshape2)
saliva <- melt(DGS_saliva_raw, id.vars = "ID")
