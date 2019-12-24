setwd("D:/Dublin Business School/Semester 1/Statistics and Mathematics for Data Analytics/Assignments/CA2")
#Loading the dataset
data<-read.csv("Bank_Data.csv")
head(data)

#Defining output and input variables

y <- data$Exited #dependent variable
x1 <- data$CreditScore #independent variable
x2 <- data$Geography #independent variable
x3 <- data$Gender #independent variable
x4 <- data$Age #independent variable
x5 <- data$Tenure #independent variable
x6 <- data$Balance #independent variable
x7 <- data$NumOfProducts #independent variable
x8 <- data$HasCrCard #independent variable
x9 <- data$IsActiveMember #independent variable
x10 <- data$EstimatedSalary #independent variable

#Cleaning the dataset and loading it into a data frame
df=data.frame( x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,y)
dataset=na.omit(df)

#Splitting the dataset into 80% trainset and 20% testset
set.seed(1390) #for verifying answers
n=nrow(dataset) #storing number of rows of dataset in a variable
indexes = sample(n,n*(80/100)) #splitting it 80-20
trainset = dataset[indexes,] #defining the training set
testset = dataset[-indexes,] #defining the test set

#Fitting the model using trainset

trainset.glm <- glm(trainset$y ~.,trainset, family="binomial") #using glm to fit the model
summary(trainset.glm) #displaying the summary of the fitted model to check what variables are affecting the output

#Prediction
phat=predict(trainset.glm , testset, type='response')
pred=rep(0, length(phat))
pred[phat>=0.5]=1

#Confusion Matrix to calculate the accuracy of the model
actual=testset$y
conf_mat=table(pred,actual)
accuracy=mean(pred==actual)
