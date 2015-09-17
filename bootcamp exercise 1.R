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

