#Plotting and genetic variation data analysis exercises

snpsDataFrame=read.table('hapmap-data.txt',header=TRUE)

# What are the dimensions of the data?
dim(snpsDataFrame)


# Let's look at the top (or head) of the data set:
head(snpsDataFrame)

# Note:  If A is the more common allele and B is the more rare allele (i.e. major and minor alleles, repsectively)
#        Then, AA= 0,  AB= 1, BB = 2


# What are the column names? 
names(snpsDataFrame)

# What are the row names? 
row.names(snpsDataFrame)

# Because the data are really just a large numeric matrix, we convert the dataframe to a matrix:
snps=as.matrix(snpsDataFrame)

################### LOOKING CLOSELY AT ONE SNP #######################

# With row names we can easily extract certain SNPs using the id's
testSNP=snps["rs218206_G",]

table(testSNP) #counts of different value

# What is proportion of heterozygotes at this locus?
het=sum(testSNP==1)/length(testSNP)

# What if there is missing data?
testSNP=snps["rs6717613_A",]

# Try these commands
table(testSNP) #table() ignores NA
testSNP==1
length(testSNP) #length() counts NA
is.na(testSNP)

# Now let's compute the observed heterozygosity
het=sum(testSNP==1)/length(testSNP)  # Note how this fails
het=sum(testSNP==1,na.rm=TRUE)/sum(!is.na(testSNP))  # but this doesn't 


###### EXPLORATORY PLOT OF SNP ALLELE FREQUENCY VS. OBSERVED HETEROZYGOSITY #####

# To inspect the data, let's compute the frequency of each SNP and compare it 
# to the observed heterozygosity (i.e. the proportion of individuals who are heterozygotes)

# What is the frequency of the minor allele?
freq=sum(testSNP,na.rm=TRUE)/(2.0*sum(!is.na(testSNP)))

# Now, let's define functions that do this for a generic set of SNP data
calc_freq=function(x){
  return(sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x))))
}

calc_het=function(x){
  return(sum(x==1,na.rm=TRUE)/(sum(!is.na(x))))
}

# And now let's apply the functions to each and every SNP
freq=apply(snps,1,calc_freq)
het=apply(snps,1,calc_het)

# And now we can make exploratory plots
plot(freq,het,xlab="Frequency",ylab="Heterozygosity", pch=19, col="green")  # Scatter plot

# Let's add a line to show what relationship we'd expect under Hardy-Weinberg expectations
p=seq(0,0.5,by=0.05)   # Set-up a vector with a sequence of allele frequencies
points(p,2*p*(1-p),type="l", col=2) # Plot the HW expectation as a line in red


## APPYLING A CHI-SQUARE TEST TO EACH SNP TO FORMALLY LOOK FOR DEPARTURES FROM HARDY-WEINBERG EXPECTATIONS ###

compute_chisquare=function(x){
  freq=sum(x,na.rm=TRUE)/(2.0*sum(!is.na(x)))
  cnt0=sum(x==0,na.rm=TRUE)
  cnt1=sum(x==1,na.rm=TRUE)
  cnt2=sum(x==2,na.rm=TRUE)
  obscnts=c(cnt0,cnt1,cnt2)
  #print(obscnts)
  n=sum(obscnts)
  expcnts=c((1-freq)^2,2*freq*(1-freq),freq^2)*n
  chisq=sum((obscnts-expcnts)^2/expcnts)
  return(chisq)
}

# Apply the compute_chi_square function to each snp
chisqs=apply(snps,1,compute_chisquare)

# Compute p-values for each chi-square value using the pchisq function
pvals=pchisq(chisqs,1,lower.tail=FALSE)



# Count the number of pvals smaller than the significance threshold
signifthres<-0.05
sum(pvals<signifthres) 

signifthres <- 0.01
sum(pvals < signifthres)

signifthres <- 0.001
sum(pvals < signifthres)

num_pval <- length(pvals)
exp_pvals <- vector(,length(pvals))

for(ii in 1:length(pvals)){
  exp_pvals[ii] <- ii/num_pval
}

sort_pvals <- sort(pvals)

log_sort_pvals <- -log10(sort_pvals)
log_exp_pvals <- -log10(exp_pvals)

plot(log_exp_pvals, log_sort_pvals, col = 1, pch = 19, xlab = "expected p-val", ylab = "observed p-val")
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)


#################################################################################################
##Type 2 diabetes study

zz <- read.table("pheno.sim.2014-1.txt.txt", header = T)
head(zz)

sum(is.na(zz[,2]))

summary(zz)

controls <- which(zz[,2] < quantile(zz[,2], 0.25))

cases <- which(zz[,2] > quantile(zz[,2], 0.75))

plot(density(zz[,2]), main = "Density plot")
abline(v = quantile(zz[,2], 0.25), col = 2)
abline(v = quantile(zz[,2], 0.75), col = 2)

cases_genotypes <- snpsDataFrame["rs7584086_T",cases]

cases_genotypes <- snpsDataFrame["rs7584086_T",cases]

matrix.snps <- as.matrix(snpsDataFrame)

table(matrix.snps["rs7584086_T",cases])

table(matrix.snps["rs7584086_T",controls])


