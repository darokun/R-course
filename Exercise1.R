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
data$phq1 <- as.factor(data$phq1)
data$phq2 <- as.factor(data$phq2)
data$phq3 <- as.factor(data$phq3)
data$phq4 <- as.factor(data$phq4)
data$phq5 <- as.factor(data$phq5)
data$phq6 <- as.factor(data$phq6)
data$phq7 <- as.factor(data$phq7)
data$phq8 <- as.factor(data$phq8)
data$phq9 <- as.factor(data$phq9)

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
# to check NA's
table(data$male, useNA="always")
# another way of doing this:
summary(data$male)
 
# what's the mean BMI in the overall population? 
mean(data$bmi, na.rm=TRUE)
# another way of doing this:
summary(data$bmi)

# mean bmi for men and women?
mean(data$bmi[data$male==TRUE], na.rm=TRUE) # men
mean(data$bmi[data$male==FALSE], na.rm=TRUE) # women <-
# another way of doing this:
summary(data$bmi[data$male==TRUE]) # men
summary(data$bmi[data$male==FALSE]) # women <-

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
     pch=".",
     col="dark turquoise",
     xlab = "Body Mass Index (Kg/m*m)",
     ylab = "Systolic Blood Pressure (mmHg)",
     main = "BMI against SBP")

# to get the same plot, but with different colors according to gender:
# convert data$male to numeric, and add 1 to get values 1:2 instead of 0:1
data$male <- as.numeric(data$male + 1)
# create your plot, including col=col as argument (we'll get col=1 for values=1 and col=2 for values=2)
col = sort(unique(data$male))
plot(data$bmi, data$rr_sys,
     pch=20, cex=0.1,
     col=col,
     xlab = "Body Mass Index (Kg/m*m)",
     ylab = "Systolic Blood Pressure (mmHg)",
     main = "BMI against SBP")
# convert data$male back into a logical vector
data$male <- as.logical(data$male)

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
        names=c("None", "Pre-Diabetes", "Diabetes"),
        xlab="Diabetes Status",
        ylab="Systolic Blood Pressure",
        main="SBP and Diabetes Status",
        pch=20, cex=0.3) 

# plot bmi agaist educ. Interpret
boxplot(bmi~educ,data=data,
        xlab="Educational Level",
        ylab="BMI (Kg/m*m)",
        main="Boxplots of Educational Level vs. BMI",
        col=c("gray40", "gray50", "gray65", "gray80", "gray90"))

# histogram of hdl-c
# how does the distribution look like?
hist(data$hdl, breaks = 30, main = "Histogram and density of HDLc", xlab="", ylab="")

hist(data$hdl, breaks = 30, prob = T, main = "Histogram and density of HDLc", xlab="", ylab="")
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