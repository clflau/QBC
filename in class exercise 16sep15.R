

#trying in class exercise

cat("hello world")
plot(sin, -pi, 2*pi) #see ?plot.function
tips <- c("learn R", "love R")
tips
cat(tips, sep = "\n") #"\n" means new line
ls() 
u <- "remove this"
rm(u)
rm(list = ls())

source("~/GitHub/QBC/source.example.R")
source("source.example.R")
all.I.know.about.life.I.learned.in.grad.school() #function that Mike made up in his script

#read in tree
tt <- read.tree("tree.tre")
str(tt)
tt$tip.label
head(tt$tip.label)
attributes(tt)

# d contains length data, gamily, species, order, etc
dd <- read.table("data.txt", header = T, as.is = T, sep = "\t")  # "\t" means tabs
attributes(dd)
head(dd)
dim(dd)
dim(dd)[1]
dim(dd)[2]
dflength <- dim(dd)[1]
# generate some random size data
size <- runif(dflength)
#use cbind to add this column to data
newdd <- cbind(dd, size)
head(newdd)
head(newdd$species)
head(newdd$size)
newdd[1, 1] #entry in row 1, column 1
newdd[, 1]
newdd[1:10, ]
which(newdd$mode == "MPF")
a1 <- 1 #variables cannot start with a number
newdd$mode == "MPF"
newdd[which(newdd$mode == "MPF"), ] #outputs all the rows with MPF in column of mode
just_mpf <- newdd[which(newdd$mode == "MPF"), ]
head(just_mpf)
length(tt$tip.label)




#Control statements

## for loops
for (ii in 1:5){
  cat("\nthe number is ", ii)
}

notfish <- c("bat", "dolphin", "toad", "soldier")
for(animal in notfish){
  cat(animal, "fish\n", sep = "")        #cat() defaults to putting spaces between elements, sep= "" eliminates the spaces
}

for(animal in notfish){
  cat(animal, "fish\n", sep = "<>")                
}

##while loops
xx <- 1
while(xx < 5){
  xx <- xx + 1;
  cat("value of xx", xx, "\n")
  if (xx == 3){
    break;}                              #break stops the while loop
}
print(xx)

##if statements
xx <- 1
while(xx < 10){
    cat("value of xx", xx, "\n")
  if(xx == 7){
    cat("lucky number", xx, "\n");}
  else if(xx ==2){
    cat("the number ", xx, "\n")
  }
  else
  {
    cat("not excited about the number", xx, "\n")
  };
  xx <- xx + 1;
  }
print(xx)


#doing an operation 10 times

nn <- 10
RR <-1.1

for (tt in 1:10){
  nn[tt+1] <- RR*nn[tt]
}
