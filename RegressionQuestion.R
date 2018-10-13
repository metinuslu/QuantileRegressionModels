
# Clean the System & Console Variable and Set the Path --------------------
rm(list = ls())
cat("\014")
options(warn = -1)
# set.seed(10147153) # Just to make it reproducible

# Set the System Path & Variable & Output Directory Name
if (require('here') == FALSE){
  install.packages('here')
  library(here)
}

#Install & Use Library
if (require('quantreg') == FALSE){
  install.packages('quantreg')
  library(quantreg)
}

Path <- here()

setwd(Path)

Sys_Date <- format(Sys.Date(), "%Y%m%d")
# Sys_Time <- format(Sys.time(), "%H:%M:%S")

n <- 30

RandomNumber <- data.frame()

RandomNumberList <- data.frame()

RegModelList <- data.frame()

QuantileRegModelList1 <- data.frame()

QuantileRegModelList2 <- data.frame()

QuantileRegModelList3 <- data.frame()

OtherList <- data.frame()

for(i in 1:1000){
  
  x <- rnorm(n, mean=0, sd=1)
  
  y <- rnorm(n, mean=0.5, sd=1)
  
  RandomNumber <- data.frame(X_Var=x, Y_Var=y)
  
  n_percent5 <- round(n * 0.05)
  
  s_percent5 <- sample(x = 1:n, size = n_percent5)
  
  OtherList <- rbind(OtherList, data.frame(No=i, SampleRank1=s_percent5[1], SampleRank2=s_percent5[2]))
  
  RandomNumber[s_percent5, ] <- 4
  
  RegModel <- lm (Y_Var ~ X_Var, data = RandomNumber)
  
  RegModelList <- rbind(RegModelList, data.frame(ModelNo=i, 
                                                 Coef1=RegModel$coefficients[1], 
                                                 Coef2=RegModel$coefficients[2]))
  
  fit1 <- rq(Y_Var ~ X_Var, tau=0.25, data=RandomNumber)
  
  fit2 <- rq(Y_Var ~ X_Var, tau=0.50, data=RandomNumber)
  
  fit3 <- rq(Y_Var ~ X_Var, tau=0.75, data=RandomNumber)
  
  QuantileRegModelList1 <- rbind(QuantileRegModelList1, data.frame(ModelNo=i,
                                                Coef1=fit1$coefficients[1], 
                                                Coef2=fit1$coefficients[2]))
  
  QuantileRegModelList2 <- rbind(QuantileRegModelList2, data.frame(ModelNo=i,
                                                Coef1=fit2$coefficients[1], 
                                                Coef2=fit2$coefficients[2]))
  
  QuantileRegModelList3 <- rbind(QuantileRegModelList3, data.frame(ModelNo=i, 
                                                Coef1=fit3$coefficients[1], 
                                                Coef2=fit3$coefficients[2]))
  
  RandomNumber <- cbind(No=i, RandomNumber)
  
  RandomNumberList <- rbind(RandomNumberList, RandomNumber)
  
}

write.csv2(RandomNumberList, file = paste0(Path, "/RandomNumberList_", Sys_Date, ".csv") , row.names = FALSE)

write.csv2(RegModelList, file = paste0(Path, "/RegModelList_", Sys_Date, ".csv") , row.names = FALSE)

write.csv2(QuantileRegModelList1, file = paste0(Path, "/QuantileRegModelList1_", Sys_Date, ".csv") , row.names = FALSE)

write.csv2(QuantileRegModelList2, file = paste0(Path, "/QuantileRegModelList2_", Sys_Date, ".csv") , row.names = FALSE)

write.csv2(QuantileRegModelList3, file = paste0(Path, "/QuantileRegModelList3_", Sys_Date, ".csv") , row.names = FALSE)

write.csv2(OtherList, file = paste0(Path, "/OtherList_", Sys_Date, ".csv") , row.names = FALSE)


# fix(newdata1)

# Kontrol
# newdata1 <- subset(RandomNumberList, RandomNumberList$No == "75", select=c(X_Var, Y_Var))
# RegModel1 <- lm (Y_Var ~ X_Var, data = newdata1)
# RegModel1$coefficients
# 
# newdata2 <- subset(RandomNumberList, RandomNumberList$No == "250", select=c(X_Var, Y_Var))
# RegModel2 <- lm (Y_Var ~ X_Var, data = newdata2)
# RegModel2$coefficients
# 
# newdata3 <- subset(RandomNumberList, RandomNumberList$No == "600", select=c(X_Var, Y_Var))
# RegModel3 <- lm (Y_Var ~ X_Var, data = newdata3)
# RegModel3$coefficients
# 
# 
# write(RegModel1$coefficients, file = "Kontrol.txt", append = TRUE)
# write(RegModel2$coefficients, file = "Kontrol.txt", append = TRUE)
# write(RegModel3$coefficients, file = "Kontrol.txt", append = TRUE)
# 
# rm(newdata1,newdata2,newdata3)
# rm(RegModel1,RegModel2, RegModel3)