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
###                  EXERCISE 2                 ###
###################################################



#----
# 2. Describe the US population with regards to:
#----

# a) demographic characteristics.
# Recode the age variable into the following categories: 20-34, 35-49, 50-64, 65-79, 80 or higher. Add this new variable (a factor!) to your data set.
age.labels <- c("20-34", "35-49", "50-64", "65-79", ">80")
data$age.groups <- cut(data$age, c(19, 34, 49, 64, 79, Inf ), 
                      labels = age.labels)

# the way they do it
data$age.groups <- data$age
data$age.groups[data$age.groups >19 & data$age.groups <35] <- 1
data$age.groups[data$age.groups >34 & data$age.groups <50] <- 2
data$age.groups[data$age.groups >49 & data$age.groups <65] <- 3
data$age.groups[data$age.groups >64 & data$age.groups <80] <- 4
data$age.groups[data$age.groups >80] <- 5
data$age.groups <- as.factor(data$age.groups)

# b) self-rated health
summary(data$srhgnrl)
# or
plot(data$srhgnrl)


#----
# 3. Lifetime prevalence of cancer in the population
#----

# a) Estimate the lifetime prevalence of cancer. Can you also give an interval estimate? 
Ever.had.cancer <- table(data$cancer_ever)
prev.cancer <- round(table(data$cancer_ever)/sum(table(data$cancer_ever)), digits=4)
rbind(Ever.had.cancer, prev.cancer)
  #                     FALSE     TRUE
  # Ever.had.cancer 2787.0000 318.0000
  # prev.cancer        0.8976   0.1024

round(rbind(prev.cancer)[1,2]*100, digits=2)
# [1] 10.24

ca.test <- prop.test(table(!data$cancer_ever))
round(ca.test$conf.int, digits=2)*100
# [1]  9 11
# attr(,"conf.level")
# [1] 0.95

# b) What are the prevalences estimates in those who were exposed to pollutants at work for a longer time period, and in those who weren't?
wp.ca <- table(data$workpollut, data$cancer_ever)
x1 <- table(data$workpollut, data$cancer_ever)[1,2]
n1 <- (table(data$workpollut, data$cancer_ever)[1,1] + 
         table(data$workpollut, data$cancer_ever)[1,2])
x2 <- table(data$workpollut, data$cancer_ever)[2,2]
n2 <- (table(data$workpollut, data$cancer_ever)[2,1] + 
         table(data$workpollut, data$cancer_ever)[2,2])
Prevalence <- round(c(x1/n1, x2/n2), digits=3)
cbind(wp.ca, Prevalence)
#       FALSE TRUE Prevalence
# FALSE  1982  156      0.073
# TRUE    541   68      0.112

# The estimated prevalence of cancer in the individuals who were exposed to work pollutants is
cbind(wp.ca, Prevalence)[2 , 3]*100
# [1] 11.2

# and the estimated prevalence of cancer in the individuals who were not exposed to work pollutants is
cbind(wp.ca, Prevalence)[1,3]*100
# [1] 7.3

# Is there a significant difference in prevalence between these two subgroups?
wp.ca.test <- prop.test(table(!data$workpollut, !data$cancer_ever))
# there are significant differences between them, with a chi-square statistic of
wp.ca.test$statistic
# X-squared 
# 8.965526
wp.ca.test$p.value
# p-value: [1] 0.002751215

#----
# 4. HDL cholesterol and gender
#----

# a) Look at the distribution of high-density lipoprotein (HDL) cholesterol levels. What shape does it have?
hist(data$hdl, breaks = 25, prob = T, main = "Histogram and density of HDLc", xlab="", ylab="")
lines(density(data$hdl, na.rm=T))

# Apply an appropriate transformation to normalize HDL and save it as a new variable. (We already did this last week.) 
data$log.hdl <- log(data$hdl) 

# b) Is there a significant difference between men and women in HDL cholesterol levels (using normalized variable)?
t.hdl.sex <- t.test(data$log.hdl~data$male)
mean.hdl.women <- exp(t.hdl.sex$estimate[1])
mean.hdl.men <- exp(t.hdl.sex$estimate[2])
p.value.hdl.sex <- if(t.hdl.sex$p.value < 2.2e-16) print("p-value < 2.2e-16")

# The mean of HDL-c in women is 
mean.hdl.women
# while in men is 
mean.hdl.men
# there are, in fact, statistically significant differences between these values 
p.value.hdl.sex

#####################
### END OF SCRIPT ###
#####################

