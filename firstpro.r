
# 2 (a)

# Read csv file and put into variable x
x <- read.csv("D:/Study Matrial/assigment/aom/at1/AT 1 Employee Data.csv")

# To get the overview of first 6 rows
head(x)

# TO check number of rows
nrow(x)

# To check is there any NA values
table(complete.cases(x))

# As there are very few rows which contain NA values so, we will simply revomes that rows
# To remove rows which contain NA values
x = x[complete.cases(x),]

# To check the structure of data frame x
str(x)

# CHangeing few feature's data type into factors
x$LeaverStatus = factor(x$LeaverStatus)
x$Country = factor(x$Country)
x$Gender = factor(x$Gender)

# Creating the model
model = glm(formula = LeaverStatus ~., data = x, family = 'binomial')

# Results of our model
summary(model)
coeff = model$coefficients
exp.coeff = exp(cofii)


#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#
# 2(b)


#                *** Logistic Regrission ***

# calling dataset and puting it into variable a
data("Orange")
a = Orange

# Check the struture and clean the data if needed
str(a)

#applying the logistic regression
lr = glm(a$Tree~., a, family = 'binomial')

#output of our model
summary(lr)

#prediction
pred = predict(lr,newdata = a,type = 'response')
pred = round(pred)

# confusion matrix
table(pred,a$Tree)

# Calculating error
tab = table(pred,a$Tree)
1-sum(diag(tab))/sum(tab)


#                   **** NOTE ****

# Logistic regression clasify the data into binary digits (0 and 1)
# In this case the are more than two types of tress to be predicted
# so using logistic regression is useless


#---------------------------------------------------------------------------------#

#                    *** Ordinal Logistic Regression ***

#installing the package
install.packages('MASS')
library(MASS)

# making the model
olg = polr(a$Tree~.,a,Hess = T)

# Details of model 
summary(olg)

# Calculating p values
c = coef(summary(olg))
p = pnorm(abs(c[,'t value']),lower.tail = F) * 2
c = cbind(c,'p value'= p)

# Predicting 
predd = predict(olg,newdata = a)
predd

# Calculating errors
tab1 = table(predd,a$Tree)
1-sum(diag(tab1))/sum(tab1)


#                      *** NOTES ***
# Ordinal Logistic Regression is better than logistic regression algoritham
# But error is too high

#------------------------------------------------------------#

#              ****  Random Forest  ****

install.packages("randomForest")
library(randomForest)
rf = randomForest(a$Tree~.a)
pred3 = predict(rf,newdata = a)
tab2 = table(pred3,a$Tree)
1-sum(diag(tab2))/sum(tab2)

#                   ***  NOTES ***
# This algoritham give the best result
# The errors are very minimum [0.05714286]



# Best feature to predict the depandent variable is circumference
# as it has less p value than age and has better corelation with tree                         

#-------------------------------------------------------------------------------#


















