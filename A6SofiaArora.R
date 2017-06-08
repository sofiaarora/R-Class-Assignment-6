#This assignment helps in writing functions,loops and conditional statements.

#Assignment 6

#Sofia Arora

#1. Create a function that takes in a numeric vector. 
#The output should be a vector with running mean values. 
#The ith element of the output vector should be the mean of the values in the input vector from 1 to i. 


runningmean=function(a) #defined a function runningmean
{
  x<-length(a)
  d<-vector("numeric",length=0)#created an empty vector
  for (i in 1:x) #outer loop will run the itertations equal to length of vector passed in function
  {
    avg=0
    count=0
    for (j in 1:i) #inner loop will run the iterations equal to the number of elements
    {
      avg=avg+a[j] #each element in vector is added and sum is computed
      count=count+1 #increment counter till the inner loop statisfies the condition
    }
    d[i] = avg/count #avg is computed and stored in a new vector
   }
  return (d)
}
runningmean(c(2,7,8,4))#test case 1
runningmean(c(2.2,6,8,9.7,4,5))#test case 2
#2. Create a function that takes in a numeric vector. 
#The objective is to forecast using exponential smoothing. 
#The formula is (𝑌 is the actual value and 𝑌̂ is the forecasted value
#𝑌̂ = 𝑌̂ + 𝛼 ( 𝑌 − 𝑌̂ )

#The output of the function should be a dataframe with two columns – actual and predicted values. 
#Set a default value of 0.8 for 𝛼.

expsmooth=function(a) #defined a function expsmooth
{
  alpha=0.8 #alpha defined as per the question
  exresult=data.frame() #created an empty data frame
  c<-c(a[1],a[1]) #first value of actual and predicted will be same
  exresult=rbind(exresult,c) #bind the first values to dataframe
  colnames(exresult)=c("Actual","Predicted") #rename columns to "Actual" and "Predicted"
  for (i in 1:length(a)) #for loop computes new predicted value for every number in the vector 
  {
    nextforecasted=exresult$Predicted[i]+alpha*(exresult$Actual[i]-exresult$Predicted[i]) #exponential smoothing formula computes new predicted value
    if(i<=length(a)) #if condition checks the ith element to be less than or equal to length of vector
      {
        newvec<-c(a[i+1],nextforecasted) #vector stores the actual value and new forecasted value
        exresult=rbind(exresult,newvec)#newvec is bind to the dataframe
      }
  }
  return(exresult)
}
expsmooth(c(2,7,8,4))
expsmoo #test case 1th(c(2.2,6,8,9.7,4,5))


#3. T #test case 2here is a package with a function that enables you to check if a number is prime.
#Create a function that takes in two integers (set default values of 1 to both). 
#The function should calculate the number of prime numbers between the two values.

install.packages("schoolmath") #install schoolmath
library(schoolmath) #use schoolmath in R
 
countprime=function(a=1,b=1) #defined a function countprime
{
  count=0
  for (i in a:b) #for loop runs from first function argument till the second function argument 
  {
    if(is.prim(i)){count=count+1} #'if' condition checks if number is prime and if so, count is incremented
  }
  return(count)
}
countprime(2,100)
countpr #test case 1ime(2,200)

#4. Si #test case 2mulate a function to roll a dice. 
#Note that a dice turns up with numbers 1, 2, 3, 4, 5 or 6. 
#The function should do the following: you roll the dice twice, and if both 
#the numbers are the same then return ‘You Win’ otherwise return ‘You Lose’

rollingdice=function() #defined a function rollingdice
{
  dice<-c(1,2,3,4,5,6) #dice is defined from 1 to 6
  diceresult<-sample(dice,size=2,replace=T)#dice is rolled with replacement and it throws two results
  ifelse(diceresult[1]==diceresult[2],"You Win!","You lose!")#if result on 1st dice is equal to 2nd dice , you win or else you lose
}
rollingdice()

#5.Cre #calling functionate a function called Missing that takes in a data frame as the input and 
#outputs another data frame with column names, number of missing values in each column,
#percentage of missing values in each column, and the number of unique values in each column. 
library(dplyr) #using dplyr package
colname=c()
Missing_Values=c()
Percnt_Missing=c()
Unique_Values=c()
Missing=function(b) #defind the function "Missing"
{
  for (i in 1:ncol(b)) #for loop runs till the number of columns in dataframe that is passed as argument to the function
  {
    colname[i]<- colnames(b[i]) #column names assigned to the a new vector
    Missing_Values[i]<- sum(is.na(b[,i]))#number of missing values counted for every column in dataframe
    Percnt_Missing[i]<- mean(is.na(b[,i])) #percentage of missing values for every column in dataframe
    Unique_Values[i]<- count(unique(b[,i]))#number of unique values in every column
  }
  df1<- cbind(colname,Missing_Values,Percnt_Missing,Unique_Values)#all the vectors above are bind to a new dataframe df1
  colnames(df1)<-c("Column Name","# of Missing values","%age of Missing values","# of Unique values")#new column names given to df1
  return(df1)
}
library(ggplot2) #using ggplot2 for using the dataset msleep
Missing(msleep)#test case 1
Missing(mpg)#test case 2
library(MASS)
Missing(survey)

  


