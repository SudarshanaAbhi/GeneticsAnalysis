for i in bme_data2$AGE
  if i == max(bme_data2) returns TRUE 
  print 
  
#ICV data
match(c(NA),bme_data2$ICV) #gives index of a specific value 

#PROBLEM 2
MZ_pair <- grepl("MZ",bme_data$PAIR,fixed = TRUE)
# match("TRUE",MZ_pair) #only gives first instance 
which(MZ_pair %in% "TRUE")

#script for prob 3 part b-height
#dz group
dz_b <- bme_data_sorted[1:156,4]
summary(dz_b)
hist(dz_b,main = "DZ histogram-height", xlab = "DZ")
boxplot(dz_b, main = "DZ", ylab = "height")
shapiro.test(dz_b)
sd(dz_b)
var(dz_b)

#mz group
mz_b <- bme_data_sorted[157:432,4]
summary(mz_b)
hist(mz_b,main = "MZ histogram-height", xlab = "MZ")
boxplot(mz_b, main = "MZ", ylab = "height")
shapiro.test(mz_b)
sd(mz_b)
var(mz_b)

#sb group
sb_b <- bme_data_sorted[554:909,4]
summary(sb_b)
hist(sb_b,main = "SB histogram-height", xlab = "SB")
boxplot(sb_b, main = "SB", ylab = "height")
shapiro.test(sb_b)
sd(sb_b)
var(sb_b)

#un group
un_b <- bme_data_sorted[910:1113,4]
summary(un_b)
hist(un_b,main = "UN histogram-height", xlab = "UN")
boxplot(un_b, main = "UN", ylab = "height")
shapiro.test(un_b)
sd(un_b)
var(un_b)

