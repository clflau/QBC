#Exercise 
#1) Write a for loop statements so that it runs from 1:9 and prints the following output to your screen

for(ii in 1:9){
  if(ii < 9){
    cat("\n")
  }
  else cat("*")
}

#2) Modify your for loop so that it prints 10 asterisks, with each asterisk separated by exactly one ampersand sign (&), with no spaces or new line characters.

for(ii in 1:10){
  cat("*", "&", sep = "")
}

#4) modify this code so that it will print out a message during presidential as well as congressional election years

###you can use the if statement with the modulus operator to conditionally perform operations
years <- c( 2015, 2016, 2018, 2020, 2021)
for(ii in 1:length(years)){
  if(years[ii] %% 2 == 0){
    cat(years[ii], 'Hooray, congressional elections!', sep = '\t', fill = T)
  }
  if(years[ii] %% 4  == 0){
    cat(years[ii], 'Hooray, presidential elections!', sep = '\t', fill = T)
  }
}


#5) More fun with loops. Here are the bank accounts from seven randomly selected UCLA grad students

bankAccounts <- c(10, 9.2, 5.6, 3.7, 8.8, 0.5);

#Now look at the error message the following lines of code produce. Can you think of a way to modify this loop so that the loop will compund the interest?

interestRate <- 0.0125;
compounded <- rep("NA", length(bankAccounts))
for (i in 1:length(bankAccounts)) {
  compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; 
  }
#HINT: variables must be initialized before you can perform operations on them
#HINT 2: look at the rep() function and see if you can use that to initialize a variable that will help you.


#6) Go back to the compounded interest example. Suppose we now want to compound the interest annually, but 
#across a period of 5 years. The for loop we discussed earlier only compounds for a single year. Try this:

bankAccounts <- c(10, 9.2, 5.6); #define bank accounts here
interestRate <- 0.0525;   
house <- c(4.8, 3.8, 5.7); #deduct
food<- c(3.5, 4.3, 5.0);    #deduct
fun <- c(7.8, 2.1, 10.5);  #deduct
#and incomes (through TAships) of 
income <- c(21, 21, 21); #add this

compounded <- rep("NA", length(bankAccounts))
for (j in 1:5){
  for (i in 1:length(bankAccounts)) {
    bankAccounts <- bankAccounts - house - food - fun + income
    compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; 
  }
}
    #step 1 modify bankAccounts so that amounts reflect income and expenses
    #step 2 get calculate interest and add to accounts from step 1
    #you can actually use the line you have already written if you
    #modify amounts in bankAccounts directly in step 1


#7) Three students have estimated annual expenditures for food, housing, and fun of: (in thousands of dollars)

house <- c(4.8, 3.8, 5.7); 
food<- c(3.5, 4.3, 5.0); 
fun <- c(7.8, 2.1, 10.5); 

#and incomes (through TAships) of 

income <- c(21, 21, 21);

#Modify the 5-year interest-compounding code from #5 and #6 so that it runs from 2015-2020 and so that in odd 
#numbered years students 1 and 3 get trust fund disbursements of $5000. (hint the modulus function %% will be 
#helpful)

trust <- c(5, 0, 5)
interestRate <- 0.0525;  
bankAccounts <- c(10, 9.2, 5.6)
years <- seq(from = 2015, to = 2020, by = 1)
compounded <- rep("NA", length(bankAccounts))
#for loop over 5 years
for (j in 1:5){
#for loop over the student accounts
  for (i in 1:length(bankAccounts)) {
#if statement for odd years
    if(years[i] %% 2 != 0){
      bankAccounts <- bankAccounts - house - food - fun + income + trust
      compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; 
    }
#if statement for even years      
    if(years[i] %% 2 == 0){
      bankAccounts <- bankAccounts - house - food - fun + income
      compounded[i] <- interestRate*bankAccounts[i] + bankAccounts[i]; 
    }
  }
}

#8) use a while loop to sum all numbers from 1:17. You will need to use a counter variable (like index seen in class).

seq17 <- seq(1:17)
length(seq17)
xx <- 0
ii <- 0
while(ii < 17){
  xx <- xx + seq17[ii + 1];
  ii <- ii + 1
}
xx

#9) write a function takes a number, and prints 'small' if number less than or equal to -1; 'medium' if between -1 and + 1'big' if greater than or equal to + 1

smmdbg <- function(xx){
  if(xx <= -1){
    cat("small");
  }
  if(xx > -1 & xx < 1){
    cat("medium");
  }
  if(xx >= 1){
    cat("big")
  }
}





