rm(list = ls(all = TRUE))
options(java.parameters = "-Xmx15g")
options(scipen = 9999)
pkgLoad <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dep=TRUE, repos = "http://cran.csie.ntu.edu.tw/")
    if(!require(package, character.only = TRUE)) stop("Package not found")
  }
  suppressMessages(library(package, character.only=TRUE))
}
pkgLoad("data.table")
pkgLoad("h2o")
pkgLoad("xlsx")
pkgLoad("properties")
pkgLoad("dplyr")

h2o.init()
props<-read.properties("dualsim.property")
msisdn<-props$MSISDN
f_path<-"{RenameH}"
model_path<-props$MODEL_PATH
kpi_list<-c(props$KPIS_1,props$KPIS_2,props$KPIS_3,props$KPIS_4,props$KPIS_5,props$KPIS_6)

selectrecords<-function(file,col_order){
  file <- file[, col_order]
  return(file)
}

AssumedDualSim_files <- function(f_path) {
  files<-c()
  for ( i in 1:6) {
    casefile<-paste0(unlist(strsplit(f_path, ".", fixed=TRUE))[1],paste0("_case",i,".txt"))
    if(file.exists(casefile))
    {
      #cat("File exists....processing",casefile,i)
      files <- append(files,casefile)
    } else{
      cat("File does not exists....So unable to process",casefile,i)
    }
  }
  return(files)
}

prediction <- function(modelfiles,test) {
  for(k in 1:length(modelfiles)){
    saved_model <- h2o.loadModel(modelfiles[k])
    pred_nn <-as.data.frame(h2o.predict(saved_model,test))
    if(k==1){
      data<-cbind(as.data.frame(test[,"D_R_MSISDN"]),pred_nn$p1)
      colnames(data) <- c("R_D_MSISDN", "result1")
    } else {
      pred_nn<-pred_nn %>% select(p1)
      colnames(pred_nn) <-paste0("result",k)
      data <- cbind(data,pred_nn)
    }
  }
  fwrite(data,"{MultiplePredictions}",na="",quote=FALSE)
}


infoExtraction <- function(my_deeplearn,i) {
  confusionMatrix<-h2o.confusionMatrix(my_deeplearn)
  write.xlsx(confusionMatrix,file=paste(model_path,"ModelFile_",i,".xlsx",sep=""),sheetName="Confusion_Matrix",row.names = FALSE)
  variable_imp<-h2o.varimp(my_deeplearn)
  write.xlsx(variable_imp,file=paste(model_path,"ModelFile_",i,".xlsx",sep=""),sheetName="variable_imp",append=TRUE,row.names = FALSE)
  metrics<-my_deeplearn@model$cross_validation_metrics_summary
  write.xlsx(metrics,file=paste(model_path,"ModelFile_",i,".xlsx",sep=""),sheetName="metrics",append=TRUE,row.names = FALSE)
}

train_file<-h2o.importFile("{RenameH}")
test <- h2o.importFile("{TestFileH}")
files<-AssumedDualSim_files(f_path)
modelfiles<-c()

for(i in 1:length(files))
{
  AssumedDualSim_file<-h2o.importFile(files[i])
  train_file<-selectrecords(train_file,col_order)
  DualSim_file<-merge(train_file,AssumedDualSim_file,by=props$MSISDN)
  DualSim_file<-selectrecords(DualSim_file,col_order)
  DualSim_file$status<-1
  train_file$status<-0
  train<-h2o.rbind(as.h2o(DualSim_file),train_file)
  train<-as.data.frame(train)
  train<-train[!duplicated(train$D_R_MSISDN),]
  status<-"status"
  train[,status] <- as.factor(train[,status])
  y<-"status"
  train <- as.h2o(train)
  h2o.table(train$status)
  neural_n <- h2o.deeplearning(y=y,training_frame = train,
                                   nfolds =3,
                                   fold_assignment = "Modulo",
                                   keep_cross_validation_predictions = TRUE,
                                   overwrite_with_best_model=FALSE,
                                   epochs=10,                      
                                   score_validation_samples=10000, 
                                   score_duty_cycle=0.025,         
                                   adaptive_rate=FALSE,               
                                   rate=0.01, 
                                   activation = "Tanh",
                                   rate_annealing=2e-6,            
                                   momentum_start=0.2,            
                                   momentum_stable=0.4, 
                                   momentum_ramp=1e7, 
                                   l1=1e-5,                        
                                   l2=1e-5,
                                   max_w2=10,   
                                   seed = 1
  )
  model<-h2o.saveModel(object=neural_n,path=model_path,force=FALSE)
  modelfiles[i]<-model
  infoExtraction(my_deeplearn,i)
}
prediction(modelfiles,test)

h2o.removeAll()
try(h2o.shutdown(prompt=TRUE), silent = TRUE)
detach("package:data.table",unload=TRUE)
detach("package:h2o",unload=TRUE)
detach("package:properties",unload=TRUE)
detach("package:xlsx",unload=TRUE)
rm(list = ls(all = TRUE))

