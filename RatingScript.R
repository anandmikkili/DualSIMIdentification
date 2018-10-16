options(scipen=999999)
options(java.parameters = "-Xmx5g")
library(data.table)
pred_file<-fread("{PredictionH}")
pred_file[is.na(pred_file)] <- 0
sum<-ifelse(pred_file$result1 > 0.5,1,0)+ifelse(pred_file$result2 > 0.5,1,0)+ifelse(pred_file$result3 > 0.5,1,0)+ifelse(pred_file$result4 > 0.5,1,0)+ifelse(pred_file$result5 > 0.5,1,0)+ifelse(pred_file$result6 > 0.5,1,0)
pred_file$DualSimUsers = ifelse(sum >= 3,1,0)
fwrite(pred_file,"{Rating}",quote=FALSE,na="")
rm(list = ls(all = TRUE))
