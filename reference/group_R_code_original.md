**R code for loading data/ R codes  – Cleaned up, step by step**

&nbsp;

&nbsp;

&nbsp;

**Math dataset**

&nbsp;

\#installing packages and loading libraries

library(dplyr)

install.packages("ggplot2")

library(ggplot2)

library(readr)

install.packages("mice")

library(mice)

&nbsp;

\#Loading dataset

Math \<- read.csv("student\_mat.csv")&nbsp;

&nbsp;

\#Searching for duplicates

sum(duplicated(Math))

&nbsp;

\#data structure

str(Math)

&nbsp;

\#missing values count and percentages

sum(is.na(Math))

missing \<- colSums(is.na(Math))

colMeans(is.na(Math))\*100

View(Math\[\!complete.cases(Math), \])

&nbsp;

\#summary

summary(Math)

&nbsp;

\#Deleting rows with NAs in Walc and Dalc

Math \<- Math %\>%

&nbsp;&nbsp;filter(\!is.na(Dalc) & \!is.na(Walc))

&nbsp;

&nbsp;

\#Mice imputations

cat\_vars \<- c(

&nbsp;&nbsp;"school", "sex", "address", "famsize", "Pstatus",

&nbsp;&nbsp;"Mjob", "Fjob", "reason", "guardian",

&nbsp;&nbsp;"schoolsup", "famsup", "paid", "activities",

&nbsp;&nbsp;"nursery", "higher", "internet", "romantic"

)

&nbsp;

Math \<- Math %\>%

&nbsp;&nbsp;mutate(across(all\_of(cat\_vars), as.factor))

init \<- mice(Math, maxit \= 0\)

&nbsp;

meth \<- init$method

&nbsp;

\# Do not impute the target variables

meth\[c("Dalc", "Walc")\] \<- ""

&nbsp;

imp \<- mice(

&nbsp;&nbsp;Math,

&nbsp;&nbsp;method \= meth,

&nbsp;&nbsp;m \= 5,

&nbsp;&nbsp;maxit \= 10,

&nbsp;&nbsp;seed \= 123

)

&nbsp;

imp$method&nbsp;

summary(Math$age)&nbsp;

imp$imp$age

summary(Math$absences)&nbsp;

imp$imp$absences&nbsp;

summary(Math$G1)&nbsp;

imp$imp$G1&nbsp;

&nbsp;

Math \<- complete(imp, 4\)

&nbsp;

colSums(is.na(Math))

&nbsp;

\#Label encoding the written character variables into numerics

mjob\_map \<- c(

&nbsp;&nbsp;teacher \= 1,

&nbsp;&nbsp;health \= 2,

&nbsp;&nbsp;services \= 3,

&nbsp;&nbsp;at\_home \= 4,

&nbsp;&nbsp;other \= 5

)

&nbsp;

fjob\_map \<- c(

&nbsp;&nbsp;teacher \= 1,

&nbsp;&nbsp;health \= 2,

&nbsp;&nbsp;services \= 3,

&nbsp;&nbsp;at\_home \= 4,

&nbsp;&nbsp;other \= 5

)

&nbsp;

reason\_map \<- c(

&nbsp;&nbsp;home \= 1,

&nbsp;&nbsp;reputation \= 2,

&nbsp;&nbsp;course \= 3,

&nbsp;&nbsp;other \= 4

)

&nbsp;

guardian\_map \<- c(

&nbsp;&nbsp;father \= 1,

&nbsp;&nbsp;mother \= 2,

&nbsp;&nbsp;other \= 3

)

Math \<- Math %\>%

&nbsp;&nbsp;mutate(

&nbsp;&nbsp;&nbsp;&nbsp;\# Binary categorical variables

&nbsp;&nbsp;&nbsp;&nbsp;school \= ifelse(school \== "GP", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;sex \= ifelse(sex \== "F", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;address \= ifelse(address \== "U", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;famsize \= ifelse(famsize \== "LE3", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;Pstatus \= ifelse(Pstatus \== "T", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;schoolsup \= ifelse(schoolsup \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;famsup \= ifelse(famsup \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;paid \= ifelse(paid \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;activities \= ifelse(activities \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;nursery \= ifelse(nursery \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;higher \= ifelse(higher \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;internet \= ifelse(internet \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;romantic \= ifelse(romantic \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;\#variables with multiple categories

&nbsp;&nbsp;&nbsp;&nbsp;Mjob \= mjob\_map\[Mjob\],

&nbsp;&nbsp;&nbsp;&nbsp;Fjob \= fjob\_map\[Fjob\],

&nbsp;&nbsp;&nbsp;&nbsp;reason \= reason\_map\[reason\],

&nbsp;&nbsp;&nbsp;&nbsp;guardian \= guardian\_map\[guardian\]

&nbsp;&nbsp;)

&nbsp;

\#confirming every column is now numeric

sapply(Math, is.numeric)

sum(\!sapply(Math, is.numeric))   \# should be 0

&nbsp;

\#sanity check on the recoding

table(Math$sex, useNA \= "always")

&nbsp;

\#Visualizations

&nbsp;

\#histograms

\#age

ggplot(Math, aes(x \= age)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$age), sd \= sd(Math$age)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of age",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Age",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

\#medu

ggplot(Math, aes(x \= Medu)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$Medu), sd \= sd(Math$Medu)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of education",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Mother's education",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

\#fedu

ggplot(Math, aes(x \= Fedu)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$Fedu), sd \= sd(Math$Fedu)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of education",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Father's education",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#travel time

ggplot(Math, aes(x \= traveltime)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$traveltime), sd \= sd(Math$traveltime)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of travel time",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Travel time",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#study time

ggplot(Math, aes(x \= studytime)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$studytime), sd \= sd(Math$studytime)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of study time",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Study time",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#family relationship

ggplot(Math, aes(x \= famrel)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$famrel), sd \= sd(Math$famrel)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of family relationship quality",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Family relationship",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#free time

ggplot(Math, aes(x \= freetime)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$freetime), sd \= sd(Math$freetime)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of free time",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Free time",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#going out

ggplot(Math, aes(x \= goout)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$goout), sd \= sd(Math$goout)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of going out",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Going out",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#absences

ggplot(Math, aes(x \= absences)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$absences), sd \= sd(Math$absences)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of school absences",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Absences",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#health

ggplot(Math, aes(x \= health)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$health), sd \= sd(Math$health)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of health status",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Health",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#workday drinking

ggplot(Math, aes(x \= Dalc)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$Dalc), sd \= sd(Math$Dalc)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of alcohol consumption",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Workday alcohol consumption",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#weekend drinking

ggplot(Math, aes(x \= Walc)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$Walc), sd \= sd(Math$Walc)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of age",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Weekend alcohol consumption",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#first term grades

ggplot(Math, aes(x \= G1)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$G1), sd \= sd(Math$G1)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of first period grades",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "First period grades",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#second term grades

ggplot(Math, aes(x \= G2)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$G2), sd \= sd(Math$G2)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of second period grades",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Second period grades",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#end grades

ggplot(Math, aes(x \= G3)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Math$G3), sd \= sd(Math$G3)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Math) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of final grades",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Final grade",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

\#Barcharts

&nbsp;

\#Address

ggplot(Math, aes(x \= address)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(title \= "Distribution of address", x \= "Address", y \= "Frequency")

&nbsp;

\#Family size

ggplot(Math, aes(x \= famsize)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(title \= "Distribution of family size", x \= "Family size", y \= "Frequency")

&nbsp;

\#Mother's job

ggplot(Math, aes(x \= Mjob)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(title \= "Distribution of mother's job", x \= "Mother's job", y \= "Frequency")

&nbsp;

\#Father's job

ggplot(Math, aes(x \= Fjob)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(title \= "Distribution of father's job", x \= "Father's job", y \= "Frequency")

&nbsp;

\#Reason for choosing school

ggplot(Math, aes(x \= reason)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(title \= "Reason for choosing school", x \= "Reason", y \= "Frequency")

&nbsp;

\#Guardian

ggplot(Math, aes(x \= guardian)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(title \= "Distribution of guardian", x \= "Guardian", y \= "Frequency")

&nbsp;

\#Sex

ggplot(Math, aes(x \= factor(sex, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("Female", "Male")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs

ggplot(Math, aes(x \= factor(sex, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("Female", "Male")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of sex",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Sex",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#School

ggplot(Math, aes(x \= factor(school, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("GP", "MS")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of schools",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Schools",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Parental status

ggplot(Math, aes(x \= factor(Pstatus, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("Together", "Apart")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of parental status",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Parental status",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Failures

ggplot(Math, aes(x \= failures)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(title \= "Distribution of past failures", x \= "Failures", y \= "Frequency")

\#School support

ggplot(Math, aes(x \= factor(schoolsup, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of school support",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "School support",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Family support

ggplot(Math, aes(x \= factor(famsup, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of family support",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "family support",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Paid courses

ggplot(Math, aes(x \= factor(paid, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of paid classes",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Paid classes",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Extra activities

ggplot(Math, aes(x \= factor(activities, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of extra activities",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Extra activities",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Nursery

ggplot(Math, aes(x \= factor(nursery, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of nursery attendance",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Nursery",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Higher education plans

ggplot(Math, aes(x \= factor(higher, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of Higher education plans",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Higher education plans",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Internet access

ggplot(Math, aes(x \= factor(internet, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of internet access",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Internet access",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

\#Romantic rel

ggplot(Math, aes(x \= factor(romantic, levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")))) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of romantic status",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Romantic rel status",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

\#boxplots

&nbsp;

\#Age

ggplot(Math, aes(y \= age)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of age", y \= "Age", x \= "")

&nbsp;

\#Mother's education

ggplot(Math, aes(y \= Medu)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of mother's education", y \= "Medu", x \= "")

&nbsp;

\#Father's education

ggplot(Math, aes(y \= Fedu)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of father's education", y \= "Fedu", x \= "")

&nbsp;

\#Travel time

ggplot(Math, aes(y \= traveltime)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of travel time", y \= "Traveltime", x \= "")

&nbsp;

\#Study time

ggplot(Math, aes(y \= studytime)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of study time", y \= "Studytime", x \= "")

&nbsp;

\#Family relationship

ggplot(Math, aes(y \= famrel)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of family relationship", y \= "Famrel", x \= "")

&nbsp;

\#Going out

ggplot(Math, aes(y \= goout)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of going out", y \= "Goout", x \= "")

&nbsp;

\#Workday alcohol consumption

ggplot(Math, aes(y \= Dalc)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of workday alcohol consumption", y \= "Dalc", x \= "")

&nbsp;

\#Weekend alcohol consumption

ggplot(Math, aes(y \= Walc)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of weekend alcohol consumption", y \= "Walc", x \= "")

&nbsp;

\#Health

ggplot(Math, aes(y \= health)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of health", y \= "Health", x \= "")

&nbsp;

\#Absences

ggplot(Math, aes(y \= absences)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of absences", y \= "Absences", x \= "")

&nbsp;

\#G1

ggplot(Math, aes(y \= G1)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of G1", y \= "G1", x \= "")

&nbsp;

\#G2

ggplot(Math, aes(y \= G2)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of G2", y \= "G2", x \= "")

&nbsp;

\#G3

ggplot(Math, aes(y \= G3)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of G3", y \= "G3", x \= "")

&nbsp;

\#Correlation matrix&nbsp;

\#Correlation matrix&nbsp;

&nbsp;

Math\_numeric \<- Math %\>% select(where(is.numeric))&nbsp;

cor\_matrix \<- cor(Math\_numeric, use \= "complete.obs", method \= "pearson")

corrplot( cor\_matrix,&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;method \= "color", type \= "upper",

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;tl.col \= "black", tl.srt \= 45,&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;number.cex \= 0.6 )

&nbsp;

&nbsp;

&nbsp;

\#copy of the processed dataset

write.csv(Math, "Math.csv", row.names \= FALSE)

file.exists("Math.csv")

&nbsp;

**Language dataset**

&nbsp;

Lang \<- read.csv("student\_por.csv")&nbsp;

&nbsp;

\#Searching for duplicates

sum(duplicated(Lang))

&nbsp;

\#Data structure

str(Lang)

&nbsp;

\#Missing values count and percentages

sum(is.na(Lang))

missing \<- colSums(is.na(Lang))

colMeans(is.na(Lang)) \* 100

View(Lang\[\!complete.cases(Lang), \])

&nbsp;

\#Summary

summary(Lang)

&nbsp;

&nbsp;

\#Deleting rows with NAs in Walc and Dalc

Lang \<- Lang %\>%

&nbsp;&nbsp;filter(\!is.na(Dalc) & \!is.na(Walc))

&nbsp;

&nbsp;

\#MICE imputations

library(mice)

library(dplyr)

library(ggplot2)

&nbsp;

cat\_vars \<- c(

&nbsp;&nbsp;"school", "sex", "address", "famsize", "Pstatus",

&nbsp;&nbsp;"Mjob", "Fjob", "reason", "guardian",

&nbsp;&nbsp;"schoolsup", "famsup", "paid", "activities",

&nbsp;&nbsp;"nursery", "higher", "internet", "romantic"

)

&nbsp;

Lang \<- Lang %\>%

&nbsp;&nbsp;mutate(across(all\_of(cat\_vars), as.factor))

&nbsp;

init \<- mice(Lang, maxit \= 0\)

&nbsp;

meth \<- init$method

&nbsp;

\# Do not impute the target variables

meth\[c("Dalc", "Walc")\] \<- ""

&nbsp;

imp \<- mice(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;method \= meth,

&nbsp;&nbsp;m \= 5,

&nbsp;&nbsp;maxit \= 10,

&nbsp;&nbsp;seed \= 123

)

&nbsp;

&nbsp;&nbsp;imp$method

&nbsp;&nbsp;

&nbsp;&nbsp;summary(Lang$age)

&nbsp;&nbsp;imp$imp$age

&nbsp;

summary(Lang$absences)

imp$imp$absences

&nbsp;

summary(Lang$G1)

imp$imp$G1

&nbsp;

Lang \<- complete(imp, 5\)

&nbsp;

colSums(is.na(Lang))

&nbsp;

&nbsp;

\#Label encoding the written character variables into numerics

&nbsp;

mjob\_map \<- c(

&nbsp;&nbsp;teacher \= 1,

&nbsp;&nbsp;health \= 2,

&nbsp;&nbsp;services \= 3,

&nbsp;&nbsp;at\_home \= 4,

&nbsp;&nbsp;other \= 5

)

&nbsp;

fjob\_map \<- c(

&nbsp;&nbsp;teacher \= 1,

&nbsp;&nbsp;health \= 2,

&nbsp;&nbsp;services \= 3,

&nbsp;&nbsp;at\_home \= 4,

&nbsp;&nbsp;other \= 5

)

&nbsp;

reason\_map \<- c(

&nbsp;&nbsp;home \= 1,

&nbsp;&nbsp;reputation \= 2,

&nbsp;&nbsp;course \= 3,

&nbsp;&nbsp;other \= 4

)

&nbsp;

guardian\_map \<- c(

&nbsp;&nbsp;father \= 1,

&nbsp;&nbsp;mother \= 2,

&nbsp;&nbsp;other \= 3

)

&nbsp;

Lang \<- Lang %\>%

&nbsp;&nbsp;mutate(

&nbsp;&nbsp;&nbsp;&nbsp;\# Binary categorical variables

&nbsp;&nbsp;&nbsp;&nbsp;school \= ifelse(school \== "GP", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;sex \= ifelse(sex \== "F", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;address \= ifelse(address \== "U", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;famsize \= ifelse(famsize \== "LE3", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;Pstatus \= ifelse(Pstatus \== "T", 0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;schoolsup \= ifelse(schoolsup \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;famsup \= ifelse(famsup \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;paid \= ifelse(paid \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;activities \= ifelse(activities \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;nursery \= ifelse(nursery \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;higher \= ifelse(higher \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;internet \= ifelse(internet \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;romantic \= ifelse(romantic \== "yes", 1, 0),

&nbsp;&nbsp;&nbsp;&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;\# Variables with multiple categories

&nbsp;&nbsp;&nbsp;&nbsp;Mjob \= mjob\_map\[Mjob\],

&nbsp;&nbsp;&nbsp;&nbsp;Fjob \= fjob\_map\[Fjob\],

&nbsp;&nbsp;&nbsp;&nbsp;reason \= reason\_map\[reason\],

&nbsp;&nbsp;&nbsp;&nbsp;guardian \= guardian\_map\[guardian\]

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Confirming every column is now numeric

sapply(Lang, is.numeric)

sum(\!sapply(Lang, is.numeric))   \# should be 0

&nbsp;

\#Sanity check on the recoding

table(Lang$sex, useNA \= "always")

&nbsp;

&nbsp;

\#Visualizations

&nbsp;

\#Histograms

&nbsp;

\#Age

ggplot(Lang, aes(x \= age)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$age), sd \= sd(Lang$age)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of age",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Age",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Medu

ggplot(Lang, aes(x \= Medu)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$Medu), sd \= sd(Lang$Medu)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of education",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Mother's education",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Fedu

ggplot(Lang, aes(x \= Fedu)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$Fedu), sd \= sd(Lang$Fedu)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of education",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Father's education",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Travel time

ggplot(Lang, aes(x \= traveltime)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$traveltime), sd \= sd(Lang$traveltime)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of travel time",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Travel time",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Study time

ggplot(Lang, aes(x \= studytime)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$studytime), sd \= sd(Lang$studytime)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of study time",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Study time",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Family relationship

ggplot(Lang, aes(x \= famrel)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$famrel), sd \= sd(Lang$famrel)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of family relationship quality",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Family relationship",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Free time

ggplot(Lang, aes(x \= freetime)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$freetime), sd \= sd(Lang$freetime)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of free time",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Free time",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Going out

ggplot(Lang, aes(x \= goout)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$goout), sd \= sd(Lang$goout)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of going out",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Going out",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Absences

ggplot(Lang, aes(x \= absences)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$absences), sd \= sd(Lang$absences)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of school absences",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Absences",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Health

ggplot(Lang, aes(x \= health)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$health), sd \= sd(Lang$health)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of health status",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Health",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Workday drinking

ggplot(Lang, aes(x \= Dalc)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$Dalc), sd \= sd(Lang$Dalc)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of alcohol consumption",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Workday alcohol consumption",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Weekend drinking

ggplot(Lang, aes(x \= Walc)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$Walc), sd \= sd(Lang$Walc)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of weekend alcohol consumption",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Weekend alcohol consumption",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#First term grades

ggplot(Lang, aes(x \= G1)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$G1), sd \= sd(Lang$G1)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of first period grades",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "First period grades",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Second term grades

ggplot(Lang, aes(x \= G2)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$G2), sd \= sd(Lang$G2)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of second period grades",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Second period grades",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Final grades

ggplot(Lang, aes(x \= G3)) \+

&nbsp;&nbsp;geom\_histogram(

&nbsp;&nbsp;&nbsp;&nbsp;binwidth \= 1,

&nbsp;&nbsp;&nbsp;&nbsp;boundary \= 0.5,

&nbsp;&nbsp;&nbsp;&nbsp;fill \= "steelblue",

&nbsp;&nbsp;&nbsp;&nbsp;color \= "white"

&nbsp;&nbsp;) \+

&nbsp;&nbsp;stat\_function(

&nbsp;&nbsp;&nbsp;&nbsp;fun \= function(x) {

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;dnorm(x, mean \= mean(Lang$G3), sd \= sd(Lang$G3)) \*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nrow(Lang) \* 1

&nbsp;&nbsp;&nbsp;&nbsp;},

&nbsp;&nbsp;&nbsp;&nbsp;linewidth \= 1

&nbsp;&nbsp;) \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of final grades",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Final grade",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Barcharts

&nbsp;

&nbsp;

\#Address

ggplot(Lang, aes(x \= address)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of address",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Address",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Family size

ggplot(Lang, aes(x \= famsize)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of family size",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Family size",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Mother's job

ggplot(Lang, aes(x \= Mjob)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of mother's job",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Mother's job",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Father's job

ggplot(Lang, aes(x \= Fjob)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of father's job",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Father's job",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Reason for choosing school

ggplot(Lang, aes(x \= reason)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Reason for choosing school",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Reason",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Guardian

ggplot(Lang, aes(x \= guardian)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of guardian",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Guardian",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Sex

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;sex,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("Female", "Male")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of sex",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Sex",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#School

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;school,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("GP", "MS")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of schools",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Schools",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Parental status

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Pstatus,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("Together", "Apart")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of parental status",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Parental status",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Failures

ggplot(Lang, aes(x \= failures)) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of past failures",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Failures",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#School support

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;schoolsup,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of school support",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "School support",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Family support

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;famsup,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of family support",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Family support",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Paid courses

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;paid,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of paid classes",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Paid classes",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Extra activities

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;activities,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of extra activities",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Extra activities",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Nursery

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;nursery,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of nursery attendance",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Nursery",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Higher education plans

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;higher,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of higher education plans",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Higher education plans",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Internet access

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;internet,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of internet access",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Internet access",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#Romantic relationship

ggplot(

&nbsp;&nbsp;Lang,

&nbsp;&nbsp;aes(

&nbsp;&nbsp;&nbsp;&nbsp;x \= factor(

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;romantic,

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;levels \= c(0, 1),

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;labels \= c("No", "Yes")

&nbsp;&nbsp;&nbsp;&nbsp;)

&nbsp;&nbsp;)

) \+

&nbsp;&nbsp;geom\_bar() \+

&nbsp;&nbsp;labs(

&nbsp;&nbsp;&nbsp;&nbsp;title \= "Distribution of romantic relationship status",

&nbsp;&nbsp;&nbsp;&nbsp;x \= "Romantic relationship status",

&nbsp;&nbsp;&nbsp;&nbsp;y \= "Frequency"

&nbsp;&nbsp;)

&nbsp;

&nbsp;

\#boxplots

&nbsp;

&nbsp;

\#Age

ggplot(Lang, aes(y \= age)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of age", y \= "Age", x \= "")

&nbsp;

&nbsp;

\#Mother's education

ggplot(Lang, aes(y \= Medu)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of mother's education", y \= "Medu", x \= "")

&nbsp;

&nbsp;

\#Father's education

ggplot(Lang, aes(y \= Fedu)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of father's education", y \= "Fedu", x \= "")

&nbsp;

&nbsp;

\#Travel time

ggplot(Lang, aes(y \= traveltime)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of travel time", y \= "Traveltime", x \= "")

&nbsp;

&nbsp;

\#Study time

ggplot(Lang, aes(y \= studytime)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of study time", y \= "Studytime", x \= "")

&nbsp;

&nbsp;

\#Family relationship

ggplot(Lang, aes(y \= famrel)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of family relationship", y \= "Famrel", x \= "")

&nbsp;

&nbsp;

\#Going out

ggplot(Lang, aes(y \= goout)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of going out", y \= "Goout", x \= "")

&nbsp;

&nbsp;

\#Workday alcohol consumption

ggplot(Lang, aes(y \= Dalc)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of workday alcohol consumption", y \= "Dalc", x \= "")

&nbsp;

&nbsp;

\#Weekend alcohol consumption

ggplot(Lang, aes(y \= Walc)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of weekend alcohol consumption", y \= "Walc", x \= "")

&nbsp;

&nbsp;

\#Health

ggplot(Lang, aes(y \= health)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of health", y \= "Health", x \= "")

&nbsp;

&nbsp;

\#Absences

ggplot(Lang, aes(y \= absences)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of absences", y \= "Absences", x \= "")

&nbsp;

&nbsp;

\#G1

ggplot(Lang, aes(y \= G1)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of G1", y \= "G1", x \= "")

&nbsp;

&nbsp;

\#G2

ggplot(Lang, aes(y \= G2)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of G2", y \= "G2", x \= "")

&nbsp;

&nbsp;

\#G3

ggplot(Lang, aes(y \= G3)) \+

&nbsp;&nbsp;geom\_boxplot() \+

&nbsp;&nbsp;labs(title \= "Boxplot of G3", y \= "G3", x \= "")

&nbsp;

\#Correlation matrix&nbsp;

Lang\_numeric \<- Lang %\>% select(where(is.numeric))&nbsp;

cor\_matrix \<- cor(Lang\_numeric, use \= "complete.obs", method \= "pearson")

corrplot( cor\_matrix,&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;method \= "color", type \= "upper",

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;tl.col \= "black", tl.srt \= 45,&nbsp;

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;number.cex \= 0.6 )

&nbsp;

&nbsp;

write.csv(Lang, "Lang.csv", row.names \= FALSE)

&nbsp;

file.exists("Lang.csv")

&nbsp;