options(scipen=999999)
options(java.parameters = "-Xmx5g")
library(data.table)
library(properties)
train<-fread("{RenameH}")
dualsim1<-train[which((train$D_M1_REGION == "Gauteng")),]
dualsim2<-train[which((train$D_M1_REGION == "KZN")),]
f_path<-"{CaseHandler}"
case_filename<-paste0(unlist(strsplit(f_path, ".", fixed=TRUE))[1],"_case1.txt")
fwrite(dualsim1,case_filename,quote=FALSE,na="")
case_filename<-paste0(unlist(strsplit(f_path, ".", fixed=TRUE))[1],"_case2.txt")
fwrite(dualsim2,case_filename,quote=FALSE,na="")
rm(list = ls(all = TRUE))
