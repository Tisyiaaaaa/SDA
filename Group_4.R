# Group ID: 4
setwd("C:/Users/hp/Documents/Statistical Data Analytics/Assignment 2")
df1 = read.csv("4.csv", fill=T, header=T)
df1 = df1[1:100, 1:6]
names(df1)[names(df1)=="Phys..Act."]="Physical"
names(df1)[names(df1)=="Reac..Time"]="Reaction"
View(df1)

# Q1
#97% CI
summary(df1$Reaction)
s = sd(df1$Reaction)
n = length(df1$Reaction)
a = mean(df1$Reaction)
alpha = 0.03
LB = a-qnorm(alpha/2, lower.tail=F)*s/sqrt(n)
LB
UB = a+qnorm(alpha/2, lower.tail=F)*s/sqrt(n)
UB

# Q2
#a)
#since comparing a categorical data and numerical data, we will used comparative box plot and scatterplot
dataL = subset(df1, df1$Handedness=='LH')
dataR = subset(df1, df1$Handedness=='RH')
summary(dataL$Reaction)
summary(dataR$Reaction)

#boxplot reaction and handness
boxplot(Reaction~Handedness, data=df1, xlab="Handedness", ylab="Reaction Time", main="Comparative Boxplot of Handedness and Reaction Time")

#plot reaction and age
plot(Reaction~Age, data=df1, xlab="Age", ylab="Reaction Time", main="Reaction time vs Age")

#plot physical and age
plot(Physical~Age, data=df1, xlab="Age", ylab="Physical Activity per Week", main="Physical Activity per Week against Age")


#b)
#LH
L= subset(df1, Handedness=="LH")
meanL=mean(L$Reaction)
sdL=sd(L$Reaction)
lenL=length(L$Reaction)


#RH
R= subset(df1, Handedness=="RH")
meanR=mean(R$Reaction)
sdR=sd(R$Reaction)
lenR=length(RH$Reaction)

RV=(sL^2)/(sR^2)


PV=pf(1/RV, lenR-1, lenL-1, lower.tail=F)+ pf(RV, lenL-1,lenR-1, lower.tail=T)

#sigma L^2 not equal to sigma R^2
var.test(L$Reaction, R$Reaction, alternative="two.sided")


# Q3
#a)
df3 = subset(df1, select = -c(ID) )
View(df3)


plot(df3$Reaction~df3$Age, xlab="Age", 
     ylab="Reaction Time", 
     main="Age VS Reaction Time")

#Model for Reaction Time and Age
M3age = lm(df3$Reaction~df3$Age)
abline(M3age)


plot(df3$Reaction~df3$Physical, xlab="Physical", 
     ylab="Reaction Time", 
     main="Physical VS Reaction Time")

#Model for Reaction Time and Physical
M3phy = lm(df3$Reaction~df3$Physical)
abline(M3phy)


xgen<-c(factor(df3$Gender))
View(df3)

plot(df3$Reaction~xgen, xlab="Gender", 
     ylab="Reaction Time", 
     main="Gender VS Reaction Time")

#Model for Reaction Time and Gender
M3gen = lm(df3$Reaction~xgen)
abline(M3gen)


xhand<-c(factor(df3$Handedness))

plot(df3$Reaction~xhand, xlab="Handedness", 
     ylab="Reaction Time", 
     main="Handedness VS Reaction Time")

#Model for Reaction Time and Handedness
M3hand = lm(df3$Reaction~xhand)
abline(M3hand)


factorG <- factor(df1$Gender)
factorH <- factor(df1$Handedness)

#Model for Reaction Time and all four variables
M3 = lm(df1$Reaction~factorG+df1$Age+factorH+df1$Physical)
abline(M3)
plot(M3)

# (b)
# age=df1$Age+10
new_reaction=predict(M3,newdata=data.frame(df1$Age+10))
boxplot(new_reaction~Handedness, data=df1, xlab="Handedness", ylab="Reaction Time", main="Comparative Boxplot of Handedness and Reaction Time")

# --Calculation--
new_df=df1
new_df$newReaction = new_reaction
View(new_df)

newDataL=subset(new_df, new_df$Handedness=='LH')
newDataR=subset(new_df, new_df$Handedness=='RH')

summary(newDataL$Reaction)
summary(newDataL$newReaction)
summary(newDataR$Reaction)
summary(newDataR$newReaction)

#c)
anova(M3)


#d) 
plot(M3)



