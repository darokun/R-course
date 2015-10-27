#########################
### Preliminary stuff ###
#########################
#----

# When working from home
data <- read.table("nhanesdataset_d.tsv", sep="\t", header=T)

# When working at KR5
setwd("/usr281/ben/msc1426/Desktop/dataproject_datasets-1")
data <- read.table("nhanesdataset_d.tsv", sep="\t", header=T)


###################################################
###                  EXERCISE 1                 ###
###################################################



#----
# 1. Getting familiar with the dataset
#----

# what's the dimension of the dataset?
dim(data)
str(data)

# How many rows and columns?
nrow(data)
ncol(data)

# What are the variable names of the dataset?
names(data)

# Change class of integers or booleans to factors, as needed:
srhgnrl.labels <- c('Excellent', 'Very Good', 'Good', 'Fair', 'Poor') # Labels for srhgnrl
data$srhgnrl <- factor(data$srhgnrl, labels = srhgnrl.labels)
data$srphbad_prv30d <- as.factor(data$srphbad_prv30d)
data$srmhbad_prv30d <- as.factor(data$srmhbad_prv30d)
data$adlimp_prv30d <- as.factor(data$adlimp_prv30d)
data$educ <- as.factor(data$educ)
data$martlst <- as.factor(data$martlst)
data$ethnic <- as.factor(data$ethnic)
data$increl <- as.factor(data$increl)
data$diab_lft <- as.factor(data$diab_lft)
data$jobstat <- as.factor(data$jobstat)
data$alc_lft <- as.factor(data$alc_lft)
data$smokstat <- as.factor(data$smokstat)

# save the new dataset as an .RData file
#a. create data.RData file if it doesn't exist
if (!file.exists("data.RData")) {
  file.create("data.RData")
}
#b. save the data
save(data,file="data.RData")

# attach the dataframe
attach(data)

# how many women and men are there in the dataset?
table(data$male)

# what's the mean BMI in the overall population? 
mean(data$bmi, na.rm=TRUE)

# mean bmi for men and women?
mean(data$bmi[data$male==TRUE], na.rm=TRUE) # men
mean(data$bmi[data$male==FALSE], na.rm=TRUE) # women <-

# who has highest Hg level? men or women?
mean(data$hg[data$male==TRUE], na.rm=TRUE) # men <-
mean(data$hg[data$male==FALSE], na.rm=TRUE) # women

# who has highest Hg level? people with or without chronic bronchitis?
mean(data$hg[data$cbronch_now==TRUE], na.rm=TRUE) # with chronic bronchitis
mean(data$hg[data$cbronch_now==FALSE], na.rm=TRUE) # without chronic bronchitis <-

# who has highest Hg level? according to ethnic group? 
mean(data$hg[data$ethnic==1], na.rm=TRUE) # Hispanic
mean(data$hg[data$ethnic==2], na.rm=TRUE) # White
mean(data$hg[data$ethnic==3], na.rm=TRUE) # Black
mean(data$hg[data$ethnic==4], na.rm=TRUE) # Other/Mixed <-

# use summary() to get summarized info on all variables of the data set
summary(data) 

#----
# 2. Plots
#----

# plot rr_sys as a function of bmi. Try different pch and col and choose the prettiest ones (:
# label x and y axes with corresponding variable names.
# add main title
plot(data$bmi, data$rr_sys,
     pch=20, cex = 0.1,
     col="dark turquoise",
     xlab = "Body Mass Index (Kg/m*m)",
     ylab = "Systolic Blood Pressure (mmHg)",
     main = "BMI against SBP")

# plot rr_sys against diab_lft
# what plot should be used? #boxplot
# choose different colors for boxes
# give each box a suitable name
# add label for y-axis
# add main title
# is there a difference in SBP between diabetics and non-diabetics? <- sort of

plot(data$diab_lft, data$rr_sys) # one way

boxplot(rr_sys~diab_lft, data=data, # best way
        col=c("light green", "dark turquoise", "coral"),
        names=c("No", "Pre-Diabetes", "Diabetes"),
        xlab="Diabetes Status",
        ylab="Systolic Blood Pressure",
        main="SBP and Diabetes Status",
        pch=20, cex=0.3) 

# plot bmi agaist educ. Interpret
boxplot(bmi~educ,data=data)

# histogram of hdl-c
# how does the distribution look like?
hist(data$hdl, breaks = 25, main = "Histogram and density of HDLc", xlab="", ylab="")

hist(data$hdl, breaks = 25, prob = T, main = "Histogram and density of HDLc", xlab="", ylab="")
lines(density(data$hdl, na.rm=T))

# convert hdl to make it look normal
# add new variable to your dataset
data$log.hdl <- log(data$hdl) 

#save new dataset in a new file
#a. create data_ex1.RData file if it doesn't exist
if (!file.exists("data_ex1.RData")) {
  file.create("data_ex1.RData")
}
#b. save the data
save(data,file="data_ex1.RData")

#####################
### END OF SCRIPT ###
#####################