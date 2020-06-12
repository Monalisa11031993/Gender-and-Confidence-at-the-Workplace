#Importing the survey csv file into the Project dataframe
Project<-read.csv(file.choose())

#Creating separate subsets for Males and Females

Males_Sample<-subset(Project, What.is.your.gender..ÿ=="Male")
Males_Sample

Females<-subset(Project,What.is.your.gender..ÿ=="Female")
Females

#Random Sampling of the Females subset to create three samples with 93 observations each
install.packages("data.table")
library(data.table)

Females_sample<-data.table(Females)

#First random sample of Females subset with 93 observations
Females_sample1<-Females_sample[sample(.N, 93)]
Females_sample1

#Second random sample of Females subset with 93 observations
Females_sample2<-Females_sample[sample(.N, 93)]
Females_sample2

#Third random sample of Females subset with 93 observations
Females_sample3<-Females_sample[sample(.N, 93)]
Females_sample3

#Performing the 2-sample t-test between Males_sample and Females_sample1
t.test(Females_sample1$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., Males_Sample$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., alternative = "greater", var.equal = FALSE)

#Performing the 2-sample t-test between Males_sample and Females_sample2
t.test(Females_sample2$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., Males_Sample$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., alternative = "greater", var.equal = FALSE)

#Performing the 2-sample t-test between Males_sample and Females_sample3
t.test(Females_sample3$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., Males_Sample$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., alternative = "greater", var.equal = FALSE)

barplot(table(Project$What.is.your.gender..ÿ))

#Descriptive statistics of the variable related to confidence levels
#For the complete survey dataset
summary(Project$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work.)
sd(Project$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., na.rm = TRUE)

#For the Males subset
summary(Males_Sample$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work.)
sd(Males_Sample$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., na.rm = TRUE)

#For the complete Females subset
summary(Females_sample$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work.)
sd(Females_sample$In.the.past.year..how.frequently.would.you.say.you.lacked.confidence.at.work., na.rm = TRUE)

#calculating power
install.packages("pwr")
library(pwr)
pwr.norm.test(n=93, d=0.2, sig.level = 0.05)
pwr.norm.test(n=93, d=0.4, sig.level = 0.05)
pwr.norm.test(n=93, d=0.8, sig.level = 0.05)

