#The dataset contained in the file timss.csv is a subset of data from the 2015 Trends
#in International Mathematics and Science Study (Mullis et al., 2016).

#It contains results of a study conducted on year 5 (9-10 year old) schoolchildren in England. As
#part of this study, children were asked to complete a mathematics test and answer
#survey questions about their background and their perceptions of mathematics. Each
#row of the dataset corresponds to an individual student, and the columns are:
#• sex: The sex of the child.
#• score: The child’s percentage score in the mathematics test.
#• books: The number of books in the household where the child lives.
#• place of birth: Whether or not the child was born in the UK.
#• good at maths: Did the child agree with the statement “I am good at working
#  out difficult mathematics problems”?


#Goal: How 'good_at_maths' depends on 'score' and other variables. 

#importing libraries and data
install.packages('ggplot2')
library(ggplot2)

df = read.csv('timss.csv')

#analysis

head(df)

str(df)
dim(df)
summary(df)

df$sex = as.factor(as.numeric(df$sex)-1)                       #male = 1
df$good_at_maths = as.factor(as.numeric(df$good_at_maths)-1)   #disagree = 1
df$place_of_birth = as.factor(as.numeric(df$place_of_birth)-1) # uk = 1

#plotting

#score+sex agains the class
#score agains the class
# it probably does influence general iq etc.

ggplot(df, aes(x=score, fill=books)) +
  geom_histogram(binwidth=5, position="dodge")

ggplot(df, aes(books, score))+
  geom_violin(aes(fill = books), trim=FALSE)+
  xlim("0-10", "11-25", "26-100", "101-200", "More than 200")+
  xlab('Number of books in home')+
  ylab('Percentage scored in maths test')+
  ggtitle('Perfomance in mathematics across differet number of book in the household')

#we see nicely that they correlate highly, more books, smarter kid


ggplot(df, aes(sex, score))+
  geom_violin(aes(fill = sex), trim=FALSE)+
  xlab('Gender')+
  ylab('Percentage scored in maths test')+
  ggtitle('Perfomance in mathematics across sex')

summary(df[df$sex == 0,]$score)
summary(df[df$sex == 1,]$score)

#we see clearly that boys tend to score negligibly better, the distribution seem to be shifted by 2%

ggplot(df, aes(good_at_maths, score))+
  geom_violin(aes(fill = good_at_maths), trim=FALSE)+
  ylab('Percentage scored in maths test')+
  ggtitle('Perfomance in mathematics across the class variable')+
  scale_x_discrete("Answer to the question (class variable)", 
                   labels = c("0" = "Agree","1" = "Disagree"))+
  theme(legend.position = 'none')

summary(df[df$good_at_maths == 0,]$score)
summary(df[df$good_at_maths == 1,]$score)

#'generally, we see that kids who Agree are generally better by around 9% on the mean, so we can say that
#'these answers are appropiate for most of the cases
#'distribution of 'disagree' is less skewed i.e students who 

ggplot(df, aes(score, fill = good_at_maths))+
  geom_density(position = "stack")+
  xlab('Test score')+
  ylab('')+
  ggtitle('Distribution of test score across class variable')+
  theme(legend.position = 'none')+
  ylim(0,0.04)

#'the two distributions are different, distribution of those that disagreed is less skewed, 
#'better students tend to agree with the statement

ggplot(df, aes(score))+
  geom_density(fill = "#ff4d5d", alpha = 0.5)+
  xlab('Test score')+
  ylab('')+
  ggtitle('Distribution of test score')+
  theme(legend.position = 'none')+
  ylim(0,0.02)

#people generally score high (not normal

#modelling

logr_1 <- glm(good_at_maths ~ score, family = 'binomial', data = df)
summary(logr_1) #conclusion, 1% on the test gives 0.018 to Agree, statistically significant

logr_2 <- glm(good_at_maths ~ ., family = 'binomial', data = df)
summary(logr_2) #most of parameters apart from score and sex are not significant as have large p-values

logr_3 <- glm(good_at_maths ~ sex + score, family = 'binomial', data = df)
summary(logr_3) #looks like males have higher probability of saying Agree

logr_4 <- glm(good_at_maths ~ books, family = 'binomial', data = df)
summary(logr_4) #books variables are not statistically significant

lm_1 <- lm(score ~ sex, df)
summary(lm_1) #significantly statistic: being a male increases score by about 1.8%, s.e quite large
