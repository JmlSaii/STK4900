#Problem 2.
rm(list = ls())
# Reading the data.
blood.data <- read.table("https://www.uio.no/studier/emner/matnat/math/STK4900/data/blood.txt",sep=" ",header=TRUE)

#A.
#summary of the different age groups.
summary(subset(blood.data,age==1))
summary(subset(blood.data,age==2))
summary(subset(blood.data,age==3))

#boxplot of the different age groups.
boxplot(blood.data$Bloodpr  ~ blood.data$age, xlab = "Age Group", ylab = "Blood Pressure")

#B.
#One-way ANOVA.
blood.data$age=factor(blood.data$age)
blood.anova = aov(Bloodpr~age,data = blood.data)
summary(blood.anova)

#C.
#Linear fit.
blood.fit = lm(Bloodpr~age,data = blood.data)
summary(blood.fit)