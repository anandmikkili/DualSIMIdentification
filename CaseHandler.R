options(scipen=999999)
library(data.table)
library(properties)
props<-read.properties("/home/admin/RE/RuleEngine/CHURN_XML/dualsim.property")
mdn<-props$MSISDN
mdn_index<-which(names(train) %in% mdn)
train<-fread("{RenameH}")
f_path<-"{CaseHandler}"
dualsim1<-train[which((train$D_M1_REGION == "Gauteng")),mdn_index,with=FALSE]
dualsim2<-train[which((train$D_M1_REGION == "KZN")),mdn_index,with=FALSE]
dualsim3<-train[which((train$D_M1_REGION == "Gauteng")),mdn_index,with=FALSE]
dualsim4<-train[which((train$D_M1_REGION == "KZN")),mdn_index,with=FALSE]
dualsim5<-train[which((train$D_M1_REGION == "Gauteng")),mdn_index,with=FALSE]
dualsim6<-train[which((train$D_M1_REGION == "KZN")),mdn_index,with=FALSE]
dualsim<-c(dualsim1,dualsim2,dualsim3,dualsim4,dualsim5,dualsim6)
for(i in 1:length(dualsim))
{
  case_filename<-paste0(unlist(strsplit(f_path, ".", fixed=TRUE))[1],paste0("_case",i,".txt"))
  print(case_filename)
  print(dualsim[i])
  fwrite(dualsim[i],case_filename,quote=FALSE,na="")
}
