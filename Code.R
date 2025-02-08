data = read.csv(file.choose())
attach(data)
View(data)

data$group = as.factor(data$group)
data$Area.of.residency = as.factor(data$Area.of.residency)
data$Education = as.factor(data$Education)
data$Occupation = as.factor(data$Occupation)
data$Family.type = as.factor(data$Family.type)
data$Uternine.contraction = as.factor(data$Uternine.contraction)
data$Method.of.conception = as.factor(data$Method.of.conception)
data$oxytocin.augmentation = as.factor(data$Method.of.conception)
data$type.of.labor = as.factor(data$type.of.labor)

attach(data)
summary(data)

#pre test score analysis

shapiro.test(Pre.test.pain.score)
shapiro.test(Post.test.pain.score)

library(dplyr)

control = data %>%
  filter(group==0)

treatment = data %>%
  filter(group==1)

shapiro.test(control$Pre.test.pain.score)
shapiro.test(control$Post.test.pain.score)

shapiro.test(treatment$Pre.test.pain.score)
shapiro.test(treatment$Post.test.pain.score)

t.test(control$Pre.test.pain.score,treatment$Pre.test.pain.score)
wilcox.test(control$Post.test.pain.score,treatment$Post.test.pain.score,paired = T)
median(control$Post.test.pain.score)
median(treatment$Post.test.pain.score)

control['Change'] = control['Post.test.pain.score'] - control['Pre.test.pain.score']
treatment['Change'] = treatment['Post.test.pain.score'] - treatment['Pre.test.pain.score']

shapiro.test(control$Change)
shapiro.test(treatment$Change)

wilcox.test(control$Change,treatment$Change)
median(control$Change)
median(treatment$Change)

summary(aov(Change~Education,data = treatment))
summary(aov(Change~Occupation, data = treatment))
summary(aov(Change~Area.of.residency, data = treatment))
summary(aov(Change~AGE, data = treatment))
summary(aov(Change~Gestational.weeks, data = treatment))
summary(aov(Change~Family.type, data = treatment))
data['Change'] = data['Post.test.pain.score'] - data['Pre.test.pain.score']

summary(aov(Change~group, data = data))
