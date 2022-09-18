library(ggplot2)

################################### Question 0 ###################################

students <- read.csv('StudentsPerformance.csv')
colMeans(is.na(students))

################################### Question 1 ################################### 

# --------- part A
ggplot(students, aes(x=G3)) + 
  geom_histogram(aes(y=..density..), 
                 bins=21, color='#475c53', fill='white'
                 ) + 
  geom_density(alpha=0.5, fill='#6c8c7e')

# --------- part B
ggplot(students, aes(sample=G3)) + 
  stat_qq(color='#6c8c7e') + 
  stat_qq_line(color='#475c53') + 
  theme(legend.position="none")

# --------- part C
skweness = (mean(students$G3) - median(students$G3)) / (sd(students$G3))
print(skweness)

# --------- part D
ggplot(students, aes(y=G3)) + geom_boxplot(fill='#6c8c7e')

# --------- part E
mean_ = mean(students$G3)
median_ = median(students$G3)
var_ = var(students$G3)
sd_ = sd(students$G3)

# --------- part F
ggplot(students, aes(x=G3)) + 
  geom_density(alpha=0.5, fill='#6c8c7e') +
  geom_vline(aes(xintercept=mean_, linetype='mean'), color='blue') +
  geom_vline(aes(xintercept=median_, linetype='median'), color='red') +
  scale_linetype_manual(
    name = 'lines',
    values = c('mean' = 1, 'median' = 1),
    guide = guide_legend(override.aes = list(colour = c('blue', 'red')))
    )

# --------- part G
mean_q1 = mean_/2
mean_q3 = mean_*1.5

g1_p = nrow(subset(students, G3 <= mean_q1)) / nrow(students)
g2_p = nrow(subset(students, G3 > mean_q1 & G3 <= mean_)) / nrow(students)
g3_p = nrow(subset(students, G3 > mean_ & G3 <= mean_q3)) / nrow(students)
g4_p = nrow(subset(students, G3 > mean_q3)) / nrow(students)

mean_seperated_df = data.frame(
  "group"=c('below mean/2', 'between mean/2 and mean', 'between mean and 3mean/2', 'above 3mean/2'),
  "porportion"=c(g1_p, g2_p, g3_p, g4_p)
  )
ggplot(mean_seperated_df, aes(x="", y=porportion, fill=group)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + geom_text(aes(label=paste0(round(porportion*100), "%")), position=position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#55DDE0", "#2F4858", "#F26419", "#F6AE2D")) +
  labs(x=NULL, y=NULL, fill=NULL, title="Categorized base on means") +
  theme_classic() + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
    
# --------- part H
ggplot(students, aes(y=G3)) + geom_boxplot(fill='#6c8c7e')
quartiles = quantile(students$G3)
IQR = quartiles[4] - quartiles[2]
upper_whisker = min(max(students$G3), quartiles[4] + 1.5*IQR)
lower_whisker = max(min(students$G3), quartiles[2] - 1.5*IQR)

################################### Question 2 ###################################

# --------- part A
library(janitor)
library(dplyr)
tabyl(students$Mjob, sort = TRUE)

# --------- part B
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
ggplot(data=students, aes(x=Mjob)) +
  geom_bar(aes(y=..count..), fill=coul, stat='count') + 
  geom_text(aes(label=round(..count../nrow(students)*100, digits=2), y=..count..), stat="count", vjust=-0.5)

# --------- part C
library(forcats)
ggplot(data=students, aes(x=fct_infreq(Mjob))) +
  geom_bar(aes(y=..count..), fill=coul, stat='count') + 
  geom_text(aes(label=round(..count../nrow(students)*100, digits=2), y=..count..), stat="count", hjust=-0.2) +
  coord_flip()

# --------- part D
ggplot(students, aes(x=Mjob, y=G3, color=Mjob)) + 
  geom_violin() + geom_boxplot(width=0.1) +
  theme(legend.position="none")

################################### Question 3 ###################################

# --------- part B
ggplot(students, aes(x=G2, y=G3)) + geom_point(color='#6c8c7e')

# --------- part C
corr = cor(students$G2, students$G3)

# --------- part E
cor.test(students$G2, students$G3) 

# --------- part F
ggplot(students, aes(x=G2, y=G3)) + 
  geom_point(aes(shape=sex, color=sex))

# --------- part G
library(ggExtra)
library(RColorBrewer)

p = ggplot(students, aes(G2, G3)) + geom_point(col="transparent")
ggMarginal(
  p+geom_hex(bins=20) + theme(legend.position='left') + 
    geom_smooth(method = "loess", color='red') , type="histogram",
  bins=20
  )

# --------- part H
ggplot(students, aes(x=G2, y=G3) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

################################### Question 4 ###################################

# --------- part A
library(GGally)
cols = 2:16
cols = cols[-10]
ggpairs(
  students, columns=cols, 
  upper=list(combo='blankDiag', continuous = wrap('density', color='blue')),
  lower=list(continuous=wrap('points', color='red', size=0.2, alpha=0.5), 
             combo=wrap('points', color='red', size=0.2, alpha=0.5))
  )

# --------- part B
library(ggcorrplot)
nomeric_feature = students[, sapply(students, is.numeric)]
nomeric_feature = nomeric_feature[-1]
p.mat <- cor_pmat(nomeric_feature)
corr <- round(cor(nomeric_feature), 1)
ggcorrplot(
  corr, hc.order = TRUE, type = "lower",
  lab = TRUE, p.mat=p.mat
  )

# --------- part C
library(scatterplot3d)
scatterplot3d(students[, c('G1', 'G2', 'G3')], color=students$failures, pch=16)


################################### Question 5 ###################################

# --------- part A
ft = ftable(students$sex ~ students$romantic)

# --------- part B
ggplot(students, aes(x=romantic, fill=sex)) +
  geom_bar(position="dodge") +
  geom_text(aes(label=..count..), stat='count', position=position_dodge(0.9),vjust=-0.5)

# --------- part C
ggplot(students, aes(x=romantic,fill=sex)) +
  geom_bar() +
  geom_text(aes(label=..count..), stat="count", position=position_stack(0.5))

# --------- part D
ggplot(students,aes(x=romantic,fill=sex)) +
  geom_bar(position="fill") +
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_fill(vjust=0.5))

################################### Question 6 ###################################

# --------- part A
sampled = students[sample(nrow(students), size=35), ]
mean_ = mean(sampled$G1)
sd_ = sd(sampled$G1)
se_ = sd_/sqrt(nrow(sampled))
z = qnorm(p=0.025, lower.tail=FALSE)
ci = c(mean_ - z*se_, mean_ + z*se_)

# --------- part C
ggplot(sampled, aes(x=G1)) + 
  geom_histogram(fill='#6c8c7e') +
  geom_vline(xintercept = mean_, color='red') + 
  geom_vline(xintercept=ci[1], linetype="dotted") +
  geom_vline(xintercept=ci[2], linetype="dotted") +
  geom_errorbarh(aes(y=6, x=mean_, xmin=ci[1], xmax=ci[2]), col="#0094EA", size=0.5)

# --------- part D
d = abs((mean_ - 10) / se_)
p_value = 2 * pnorm(d, lower.tail=FALSE)

# --------- part F
a_mean = mean(students$G1)
test_z = qnorm(p=0.05, lower.tail=FALSE)
type_II = 1 - pnorm(mean_-test_z*se_, mean=a_mean, sd=se_) -
  pnorm(mean_+test_z*se_, mean=a_mean, sd=se_, lower.tail=FALSE)

# --------- part G
a_mean = mean(students$G1)
test_z = qnorm(p=0.05, lower.tail=FALSE)
power = pnorm(mean_-test_z*se_, mean=a_mean, sd=se_) + 
  pnorm(mean_+test_z*se_, mean=a_mean, sd=se_, lower.tail=FALSE)

################################### Question 7 ###################################

# --------- part A
sampled = students[sample(nrow(students), size=25), ]
sampled = sampled[, c('G2', 'G3')]
paired = sampled$G2 - sampled$G3
mean_0 = 0
sample_mean = mean(paired)
sample_sd = sd(paired)
mean_se = sample_sd / nrow(sampled)
d = abs((sample_mean - mean_0) / mean_se)
p_value = 2*pt(d, df=nrow(sampled)-1, lower.tail=FALSE)

# --------- part B
G2_sample = students[sample(nrow(students), size=100), ]$G2
G3_sample = students[sample(nrow(students), size=100), ]$G3

mean_G2 = mean(G2_sample)
mean_G3 = mean(G3_sample)

df = 99
se = sqrt(sd(G2_sample)**2/99 + sd(G3_sample)**2/99)

p_value = 2 * pt(abs(mean_G2-mean_G3), df=df, lower.tail=FALSE)
ci_t = qt(0.025, df=df, lower.tail=FALSE)
ci = c(mean_G2-mean_G3 - ci_t*se, mean_G2-mean_G3 + ci_t*se)

################################### Question 8 ###################################

# --------- part A
sampled = students[sample(nrow(students), size=20), ]
bootstrap_sample = c()
for (i in 1:1000){
  resample = sample(sampled$failures, size=20, replace=TRUE)
  bootstrap_sample = c(bootstrap_sample, resample)
}

bootstrap_median = median(bootstrap_sample)
ci = c(quantile(bootstrap_sample, probs=0.025), quantile(bootstrap_sample, probs=0.975))

# --------- part B
bootstrap_sd = sd(bootstrap_sample)
ci_t = qt(0.025, df=nrow(sampled)-1, lower.tail=FALSE)
ci = c(bootstrap_median-ci_t*bootstrap_sd, bootstrap_median+ci_t*bootstrap_sd)

################################### Question 9 ###################################

students$total = rowMeans(students[, c('G1', 'G2', 'G3')])
one.way = aov(total ~ failures, data=students)
print(summary(one.way))
