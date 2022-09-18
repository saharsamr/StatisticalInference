library(ggplot2)

students <- read.csv('StudentsPerformance.csv')

################################### Functions ###################################

se_two_prop <- function(p1, p2, n1, n2){
  return (((p1*(1-p1)/n1) + (p2*(1-p2)/n2))**0.5)
}

conf_interval <- function(pe, s, percent, distr="Normal", df=0) {
  if (distr == "Normal"){
    z = qnorm((1-percent)/2, lower.tail=FALSE)
    return (c(pe - z*s, pe + z*s))
  }
  else if (distr == 't') {
    t = qt((1-percent)/2, df=df, lower.tail=FALSE)
    return (c(pe - t*s, pe + t*s))
  }
}

tabel_from_vector <- function(vec, cols){
  new_vec = matrix(vec, byrow=TRUE, ncol=length(vec))
  colnames(new_vec) = cols
  new_vec = as.table(new_vec)
  return (new_vec)
}

################################### Question 1 ################################### 

# --------- part A
data = students[c('romantic', 'Mjob')]
romantics = data[data['romantic'] == 'yes', ]
non_romantics = data[data['romantic'] == 'no', ]

for (job in unique(data$Mjob)){
  
  print(job)
  
  rpos_count = nrow(romantics[romantics$Mjob == job, ])
  rtotal_count = nrow(romantics)
  
  nrpos_count = nrow(non_romantics[non_romantics$Mjob == job, ])
  nrtotal_count = nrow(non_romantics)
  
  r_prop = rpos_count / rtotal_count
  nr_prop = nrpos_count / nrtotal_count
  
  d = r_prop - nr_prop
  print("proportion difference:")
  print(d)
  s = se_two_prop(r_prop, nr_prop, rtotal_count, nrtotal_count)
  
  ci = conf_interval(d, s, 0.95)
  print('95% confidence interval for proportion(romantics) - proportion(non-romantics) people for this job:')
  print(ci)
}

# --------- part B
t = ftable(students$romantic ~ students$Mjob)
test_result = chisq.test(t)


################################### Question 2 ################################### 

sampled = students[sample(nrow(students), size=15), ]
sample_prop = length(which(sampled$internet == "yes")) / nrow(sampled)

p = 0.7
props = c()
for (i in 1:100){
  sim_sample = sample(c(0, 1), 15, prob=c(0.3, 0.7), replace=TRUE)
  pos_prop = length(which(sim_sample == 1)) / length(sim_sample)
  props = append(props, pos_prop)
}

p_value = length(which(props >= sample_prop)) / length(props)

ggplot(data.frame(props), aes(x=props)) + geom_histogram(color='#6c8c7e', fill='#6c8c7e')


################################### Question 3 ################################### 

# --------- part A
probs = c()
for (job in unique(students$Mjob)){
  probs = append(probs, nrow(students[students$Mjob == job, ]) / nrow(students))
}
probs_t = tabel_from_vector(probs, unique(students$Mjob))

sampled = students[sample(nrow(students), size=100), ]
fs_probs = c()
for (job in unique(students$Mjob)){
  fs_probs = append(fs_probs, nrow(sampled[sampled$Mjob == job, ]) / nrow(sampled))
}
fs_probs_t = tabel_from_vector(fs_probs, unique(students$Mjob))
comparision = chisq.test(fs_probs, p=probs)

filtered_data = students[students$Fjob == 'services', ]
sampled = filtered_data[sample(nrow(filtered_data), size=100), ]
ps_probs = c()
for (job in unique(students$Mjob)){
  ps_probs = append(ps_probs, nrow(sampled[sampled$Mjob == job, ]) / nrow(sampled))
}
ps_probs_t = tabel_from_vector(ps_probs, unique(students$Mjob))
comparision = chisq.test(ps_probs, p=probs)

selection_prob = transform(students, selection_prob=ifelse(Mjob == 'at_home', 20, 1))$selection_prob
selection_prob = selection_prob / sum(selection_prob)
sampled = students[sample(nrow(students), size=100, prob=selection_prob), ]
ps_probs = c()
for (job in unique(students$Mjob)){
  ps_probs = append(ps_probs, nrow(sampled[sampled$Mjob == job, ]) / nrow(sampled))
}
ps_probs_t = tabel_from_vector(ps_probs, unique(students$Mjob))
comparision = chisq.test(ps_probs, p=probs)

# --------- part B
t = ftable(students$internet ~ students$Mjob)
test_result = chisq.test(t)


################################### Question 4 ################################### 

# --------- part B-a
fit_g1 = lm(students$G2 ~ students$G1)
summary(fit_g1)
fit_g3 = lm(students$G2 ~ students$G3)
summary(fit_g3)
fit_failures = lm(students$G2 ~ students$failures)
summary(fit_failures)

# --------- part B-c
ggplot(students, aes(x=G1, y=G2)) + 
  geom_point(color='#6c8c7e') +
  geom_smooth(method='lm', formula= y~x, linetype = "dashed")

ggplot(students, aes(x=G3, y=G2)) + 
  geom_point(color='#6c8c7e') +
  geom_smooth(method='lm', formula= y~x, linetype = "dashed")

ggplot(students, aes(x=failures, y=G2)) + 
  geom_point(color='#6c8c7e') +
  geom_smooth(method='lm', formula= y~x, linetype = "dashed")

# --------- part D
anova(fit_g1)
anova(fit_g3)
anova(fit_failures)

# --------- part F-a
sampled = students[sample(nrow(students), size=100), ]
train_idx = sample(nrow(sampled), size=90)
train = sampled[train_idx, ]
test = sampled[-train_idx, ]

fit_g1 = lm(train$G2 ~ train$G1)
summary(fit_g1)
fit_g3 = lm(train$G2 ~ train$G3)
summary(fit_g3)
fit_failures = lm(train$G2 ~ train$failures)
summary(fit_failures)

# --------- part F-b
g1_ci = conf_interval(coef(summary(fit_g1))["train$G1", "Estimate"], 
                      coef(summary(fit_g1))["train$G1", "Std. Error"], 
                      0.95, distr='t', df=88)
g3_ci = conf_interval(coef(summary(fit_g3))["train$G3", "Estimate"], 
                      coef(summary(fit_g3))["train$G3", "Std. Error"], 
                      0.95, distr='t', df=88)
failures_ci = conf_interval(coef(summary(fit_failures))["train$failures", "Estimate"], 
                            coef(summary(fit_failures))["train$failures", "Std. Error"], 
                            0.95, distr='t', df=88)

# --------- part F-c
train = sampled[-train_idx, ]
predicted_g1 = predict(fit_g1, newdata=test)
predicted_g3 = predict(fit_g3, newdata=test)
predicted_failures = predict(fit_failures, newdata=test)

# --------- part F-d
df1 = data.frame(predicted_g1, test$G2)
df1 = transform(df1, true_pred=ifelse(abs(predicted_g1-test$G2) < 1, 1, 0))
df3 = data.frame(predicted_g3, test$G2)
df3 = transform(df3, true_pred=ifelse(abs(predicted_g3-test$G2) < 1, 1, 0))
dff = data.frame(predicted_failures, test$G2)
dff = transform(dff, true_pred=ifelse(abs(predicted_failures-test$G2) < 1, 1, 0))

success_rate1 = nrow(df1[df1$true_pred == 1, ])/10
success_rate3 = nrow(df3[df3$true_pred == 1, ])/10
success_ratef = nrow(dff[dff$true_pred == 1, ])/10


################################### Question 5 ###################################

# --------- part A
library(ggcorrplot)

nomeric_feature = students[, sapply(students, is.numeric)]
nomeric_feature = nomeric_feature[-1]
p.mat <- cor_pmat(nomeric_feature)
corr <- round(cor(nomeric_feature), 1)
ggcorrplot(
  corr, hc.order = TRUE, type = "lower",
  lab = TRUE, p.mat=p.mat
)

# --------- part B
mlm = lm(G2 ~ G1 + G3 + failures, data=students)
summary(mlm)

# --------- part E

col_names = c('age', 'goout', 'studytime', 'failures', 'health', 'absences', 'G1', 'G3')

# adj-R2, forward selection
selected_cols = c()
best_model = NULL
max_adjR2 = 0
formula_ = 'G2 ~'
available_cols = setdiff(col_names, selected_cols)
while (length(available_cols) > 0){
  pre_adjR2 = max_adjR2
  selected_var = ''
  selected_formula = ''
  for (col in available_cols){
    if (length(selected_cols) > 0){
      added_str = paste('+', col)
      new_formula = paste(formula_, added_str)
    }
    else{
      new_formula = paste(formula_, col)
    }
    model = lm(new_formula, data=students)
    print(paste('by adding', col, 'adj-R2 is', summary(model)$adj.r.squared))
    if (summary(model)$adj.r.squared > max_adjR2){
      max_adjR2 = summary(model)$adj.r.squared
      selected_var = col
      selected_formula = new_formula
    }
  }
  if (max_adjR2 > pre_adjR2){
    selected_cols = append(selected_cols, selected_var)
    formula_ = selected_formula
    best_model = lm(formula_, data=students)
    print(paste('selected variable in this step is', selected_var))
    print(paste('the new formula is', formula_))
    print('-----------------------------------------------------------------------------------------')
  }
  else{
    print(paste('******** no increase in adj-R2, the final formula is', formula_, '********'))
    break
  }
  available_cols = setdiff(col_names, selected_cols)
}
fs_adjR2_model = best_model
summary(fs_adjR2_model)

# p-value, forward selection
selected_cols = c()
best_model = NULL
formula_ = 'G2 ~'
available_cols = setdiff(col_names, selected_cols)
while (length(available_cols) > 0){
  min_pvalue = 1
  selected_var = ''
  selected_formula = ''
  for (col in available_cols){
    if (length(selected_cols) > 0){
      added_str = paste('+', col)
      new_formula = paste(formula_, added_str)
    }
    else{
      new_formula = paste(formula_, col)
    }
    model = lm(new_formula, data=students)
    print(paste('p-value of variable', col, 'is', coef(summary(model))[col, 'Pr(>|t|)']))
    if (coef(summary(model))[col, 'Pr(>|t|)'] < min_pvalue){
      min_pvalue = coef(summary(model))[col, 'Pr(>|t|)']
      selected_var = col
      selected_formula = new_formula
    }
  }
  if (min_pvalue < 0.05){
    selected_cols = append(selected_cols, selected_var)
    formula_ = selected_formula
    best_model = lm(formula_, data=students)
    print(paste('selected variable in this step is', selected_var))
    print(paste('the new formula is', formula_))
    print('-----------------------------------------------------------------------------------------')
  }
  else{
    print(paste('******** no more significant p-value, the final formula is', formula_, '********'))
    break
  }
  available_cols = setdiff(col_names, selected_cols)
}
fs_pvalue_model = best_model
summary(fs_pvalue_model)

# adj-R2, back elimination
make_formula = function(columns){
  base = 'G2 ~'
  for (i in 1:length(columns)){
    if (i == length(columns)){
      base = paste(base, columns[i])
    }
    else{
      base = paste(base, columns[i], '+')
    }
  }
  return (base)
}
selected_cols = c()
best_model = NULL
formula_ = 'G2 ~ age + goout + studytime + failures + health + absences + G1 + G3'
max_adjR2 = summary(lm(formula_, data=students))$adj.r.squared
print(paste('full model adj-R2 is', max_adjR2))
available_cols = col_names
while (length(available_cols) > 0){
  pre_adjR2 = max_adjR2
  selected_var = ''
  selected_formula = ''
  for (col in available_cols){
    new_formula = make_formula(setdiff(available_cols, c(col)))
    model = lm(new_formula, data=students)
    print(paste('by removing', col, 'adj-R2 is', summary(model)$adj.r.squared))
    if (summary(model)$adj.r.squared > max_adjR2){
      max_adjR2 = summary(model)$adj.r.squared
      selected_var = col
      selected_formula = new_formula
    }
  }
  if (max_adjR2 > pre_adjR2){
    available_cols = setdiff(available_cols, c(col))
    formula_ = selected_formula
    best_model = lm(formula_, data=students)
    print(paste('removed variable in this step is', selected_var))
    print(paste('the new formula is', formula_))
    print('-----------------------------------------------------------------------------------------')
  }
  else{
    print(paste('******** no increase in adj-R2, the final formula is', formula_, '********'))
    break
  }
}
be_adjR2_model = best_model
summary(be_adjR2_model)

# p-value, back elimination
selected_cols = c()
best_model = NULL
formula_ = 'G2 ~ age + goout + studytime + failures + health + absences + G1 + G3'
available_cols = col_names
while (length(available_cols) > 0){
  max_pvalue = 0
  selected_var = ''
  model = lm(formula_, data=students)
  for (col in available_cols){
    print(paste('variable', col, 'has p-value', coef(summary(model))[col, 'Pr(>|t|)']))
    if (coef(summary(model))[col, 'Pr(>|t|)'] > max_pvalue){
      max_pvalue = coef(summary(model))[col, 'Pr(>|t|)']
      selected_var = col
    }
  }
  if (max_pvalue > 0.05){
    available_cols = setdiff(available_cols, c(selected_var))
    formula_ = make_formula(available_cols)
    best_model = lm(formula_, data=students)
    print(paste('removed variable in this step is', selected_var))
    print(paste('the new formula is', formula_))
    print('-----------------------------------------------------------------------------------------')
  }
  else{
    print(paste('******** no more non-significant variables, the final formula is', formula_, '********'))
    break
  }
}
be_pvalue_model = best_model
summary(be_pvalue_model)

# --------- part F
#model in part B
ggplot(students, aes(x=G3, y=G2)) + 
  geom_point(color='#6c8c7e')

ggplot(students, aes(x=G1, y=G2)) + 
  geom_point(color='#6c8c7e')

ggplot(students, aes(x=failures, y=G2)) + 
  geom_point(color='#6c8c7e')

plot(mlm)

#model in part E
ggplot(students, aes(x=health, y=G2)) + 
  geom_point(color='#6c8c7e')

ggplot(students, aes(x=absences, y=G2)) + 
  geom_point(color='#6c8c7e')

plot(be_pvalue_model)

# --------- part G
library(caret)
train.control <- trainControl(method = "cv", number=5)
fit = train(G2 ~ G3 + G1 + failures, data=students, method="lm", trControl=train.control)
print(fit)

fit2 = train(G2 ~ G3 + G1 + failures + health + absences, data=students, method='lm', trControl=train.control)
print(fit2)

################################### Question 6 ###################################

# --------- part A
data = students

data$romantic[data$romantic == 'yes'] = 1
data$romantic[data$romantic == 'no'] = 0
data$romantic = as.numeric(data$romantic)

data$sex[data$sex == 'F'] = 1
data$sex[data$sex == 'M'] = 0
data$sex = as.numeric(data$sex)

data$internet[data$internet == 'yes'] = 1
data$internet[data$internet == 'no'] = 0
data$internet = as.numeric(data$internet)

lr = glm(romantic ~ sex + internet + studytime + age + health, binomial, data=data)
summary(lr)

# --------- part B
internet_OR = exp(coef(summary(lr))['internet', 'Estimate'])
pps = c()
ps = seq(from=0.01, to=0.99, by=0.01)
for (p in ps){
  odd = (p/(1-p))/internet_OR
  pp = odd/(1+odd)
  pps = append(pps, pp)
}

plot(pps, ps, col="blue", pch=".", xlab='P(romantic|no internet)', ylab='P(romantic|internet)')
lines(pps, ps, col="blue")
lines(ps, ps, col='red', type="l", lty=2)


# --------- part C
library(pROC)
preds =predict(lr, data, type="response")
curve = roc(data$romantic, preds, print.auc=TRUE, show.thres=TRUE)
plot(curve)
auc(curve)

# --------- part E
lr = glm(romantic ~ sex + internet + age, binomial, data=data)
summary(lr)

# --------- part F
utilities = c()
ps = seq(from=0.1, to=0.99, by=0.01)
for (p in ps){
  result = coords(curve, x=p, ret=c('tp', 'fp', 'tn', 'fn'))
  utilities = append(utilities, result$tp + result$tn - 2*result$fp - result$fn)
}

plot(ps, utilities, col="red", pch=".", xlab='thresh')
lines(ps, utilities, col="red")


################################### Question 7 ###################################

data = students
data = transform(data, academic_probaition=ifelse(G1+G2+G3 < 25, 1, 0))

data$school[data$school == 'GP'] = 1
data$school[data$school == 'MS'] = 0
data$school = as.numeric(data$school)

data$sex[data$sex == 'F'] = 1
data$sex[data$sex == 'M'] = 0
data$sex = as.numeric(data$sex)

data$internet[data$internet == 'yes'] = 1
data$internet[data$internet == 'no'] = 0
data$internet = as.numeric(data$internet)

data$romantic[data$romantic == 'yes'] = 1
data$romantic[data$romantic == 'no'] = 0
data$romantic = as.numeric(data$romantic)

data = transform(data, Fjob_teacher=ifelse(Fjob == 'teacher', 1, 0))
data = transform(data, Fjob_services=ifelse(Fjob == 'services', 1, 0))
data = transform(data, Fjob_health=ifelse(Fjob == 'health', 1, 0))
data = transform(data, Fjob_at_home=ifelse(Fjob == 'at_home', 1, 0))

data = transform(data, Mjob_teacher=ifelse(Mjob == 'teacher', 1, 0))
data = transform(data, Mjob_services=ifelse(Mjob == 'services', 1, 0))
data = transform(data, Mjob_health=ifelse(Mjob == 'health', 1, 0))
data = transform(data, Mjob_at_home=ifelse(Mjob == 'at_home', 1, 0))

model = glm(academic_probaition ~ school + sex + age + goout + internet + romantic + studytime + failures + health + absences +
              Fjob_teacher + Fjob_services + Fjob_health + Fjob_at_home + Mjob_teacher + Mjob_services + Mjob_health + Mjob_at_home, data=data)
summary(model)
