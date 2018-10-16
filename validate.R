setwd("E://DUAL_SIM")

options(scipen=999999)
library(data.table)
library(properties)
library(xlsx)
library(dplyr)
source(StyleSheet.R)

props<-read.properties("churn.property")
setwd(props$work_dir)

final_data<-fread("data.txt")
KPIs<-read.properties("KPI.properties")

col_usg<-c(KPIs$VOICE_DUR,KPIs$DATA_VOL,KPIs$SMS_CNT)
col_rev<-c(KPIs$VOICE_REV,KPIs$DATA_REV,KPIs$SMS_REV,KPIs$TOT_REV)
#final_data<-fread(data,select = col_names)
#col_index<-seq(1:ncol(final_data))
colNames<-names(final_data)

data_types<-function(frame){
  res<-lapply(frame,class)
  DATATYPE<-unlist(res)
  res_frame<-data.frame(DATATYPE)
}
datatype<-data_types(final_data)
n<-length(colNames)
len<-n-2
#data1<- data.frame(COL_NAME=character(),COUNT_MISSING=integer(),COUNT_NEGATIVE=integer(),COUNT_ZERO=integer(),DATATYPE=character(),stringsAsFactors=FALSE)
data1<- data.frame(COL_NAME=character(),COUNT_MISSING=integer(),COUNT_NEGATIVE=integer(),COUNT_ZERO=integer(),DATATYPE=character(),stringsAsFactors=FALSE)

for(k in seq(1,length(colNames))){
  #count<-length(which(is.na(final_data[[col_index[k]]])))
  #count_n<-length(which(final_data[[col_index[k]]]<0))
  #count_0<-length(which(final_data[[col_index[k]]]==0))
  count<-length(which(is.na(final_data[[k]])))
  count_n<-length(which(final_data[[k]]<0))
  count_0<-length(which(final_data[[k]]==0))
  data1[k,]<-c(colNames[k],count,count_n,count_0,as.character(datatype$DATATYPE[k]))
}

AON<- data.frame(AON_BAND=character(),COUNT=integer(),stringsAsFactors=FALSE)
aon_band<-c("0-1","1-2","2-3","3-6","6-12",">12")
aon_range<-c(0,1,2,3,6,12)
for(i in 1:length(aon_range))
{
  if(i<length(aon_range))
  {
    AON[i,1]<-aon_band[i]
    AON[i,2] <-final_data %>% filter(final_data$AON>=aon_range[i] & final_data$AON<aon_range[i+1]) %>% nrow()
  }else{
    AON[i,1]<-aon_band[i]
    AON[i,2]<-final_data %>% filter(final_data$AON>aon_range[i]) %>% nrow()
  }
}

#func<-final_data %>% group_by(final_data$REGION) %>% summarise(Count=n())
#region<-data.frame(func)
region<-data.frame(final_data %>% group_by(final_data$REGION) %>% summarise(Count=n()))

comp1<-c()
comp2<-c()
for(i in 1:length(col_usg)){
  comp1[i]<-c(paste(col_usg[i],">0 and ",col_rev[i],"=0"))
  #as.vector(comp1_array)
}
n<-nrow(comp_array)
for(i in 1:length(col_usg)){
 
  comp2[i]<-c(paste(col_usg[i],"> ",col_rev[4]))
}
comp_array<-c()
comp_array<-c(comp1,comp2)
comp_frame<-data.frame(Comparison=character(),Count=integer(),stringsAsFactors = FALSE)


j<-1
n<-length(col_usg)
for(i in 1:length(comp_array))
{
  if(i<=n){
    comp_frame[i,1]<-comp_array[i] #paste()
    comp_frame[i,2]<-nrow(final_data[which(final_data[,col_usg[i],with=FALSE]==0 & final_data[,col_rev[i],with=FALSE]>0),]) 
  } else{
    comp_frame[i,1]<-comp_array[i]
    comp_frame[i,2]<-nrow(final_data[which(final_data[,col_rev[j],with=FALSE]> final_data[,col_rev[length(col_rev)],with=FALSE]),])
    j<-j+1
  }
}


wb <- createWorkbook()
sheet1 <- createSheet(wb, sheetName = "Data distribution")
TABLE_COLNAMES_STYLE <-
  CellStyle(wb) + Fill(foregroundColor = "#d1d1d6") + Font(
    wb,
    heightInPoints = 10,
    isBold = TRUE,
    color = "9",
    name = "Arial"
  ) + Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") + Border(
    color = "#d1d1d6",
    position = c("TOP", "RIGHT", "BOTTOM", "LEFT"),
    pen = c(
      "BORDER_THICK",
      "BORDER_THICK",
      "BORDER_THICK",
      "BORDER_THICK"
    )
  )


Style1 <-
  CellStyle(wb) + Alignment(horizontal = "ALIGN_RIGHT") + Border(
    color = "black",
    position = c("TOP", "RIGHT", "BOTTOM", "LEFT"),
    pen = c("BORDER_THIN", "BORDER_THIN", "BORDER_THIN", "BORDER_THIN")
    
  )

Style2 <-
  CellStyle(wb) + Alignment(horizontal = "ALIGN_CENTER") + Border(
 
    color = "black",
    position = c("TOP", "RIGHT", "BOTTOM", "LEFT"),
    pen = c("BORDER_THIN", "BORDER_THIN", "BORDER_THIN", "BORDER_THIN")

  )
addDataFrame(
  data1,
  sheet1,
  row.names=FALSE,
  startRow = 3,
  startColumn = 3,

  colnamesStyle = TABLE_COLNAMES_STYLE,
  colStyle = list(
    `1` = Style2,
    `2` = Style2,
    `3` = Style1,
    `4` = Style1,
    `5` = Style1
  )
)
setColumnWidth(sheet1, colIndex=c(1:ncol(data1)), colWidth=18)

sheet2 <- createSheet(wb, sheetName = "Comparison")
TABLE_COLNAMES_STYLE <-
  CellStyle(wb) + Fill(foregroundColor = "#d1d1d6") + Font(
    wb,
    heightInPoints = 10,
    isBold = TRUE,
    color = "9",
    name = "Arial"
  ) + Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") + Border(
    color = "#d1d1d6",
    position = c("TOP", "RIGHT", "BOTTOM", "LEFT"),
    pen = c(
      "BORDER_THICK",
      "BORDER_THICK",
      "BORDER_THICK",
      "BORDER_THICK"
    )
  )
sheet3 <- createSheet(wb, sheetName = "AON_Distribution")
TABLE_COLNAMES_STYLE <-
  CellStyle(wb) + Fill(foregroundColor = "#d1d1d6") + Font(
    wb,
    heightInPoints = 10,
    isBold = TRUE,
    color = "9",
    name = "Arial"
  ) + Alignment(wrapText = TRUE, horizontal = "ALIGN_CENTER") + Border(
    color = "#d1d1d6",
    position = c("TOP", "RIGHT", "BOTTOM", "LEFT"),
    pen = c(
      "BORDER_THICK",
      "BORDER_THICK",
      "BORDER_THICK",
      "BORDER_THICK"
    )
  )


Style1 <-
  CellStyle(wb) + Alignment(horizontal = "ALIGN_RIGHT") + Border(
    color = "black",
    position = c("TOP", "RIGHT", "BOTTOM", "LEFT"),
    pen = c("BORDER_THIN", "BORDER_THIN", "BORDER_THIN", "BORDER_THIN")
  )

Style2 <-
  CellStyle(wb) + Alignment(horizontal = "ALIGN_CENTER") + Border(
    color = "black",
    position = c("TOP", "RIGHT", "BOTTOM", "LEFT"),
    pen = c("BORDER_THIN", "BORDER_THIN", "BORDER_THIN", "BORDER_THIN")
  )

addDataFrame(
  comp_frame,
  sheet2,
  row.names=FALSE,
  startRow = 3,
  startColumn = 2,
  colnamesStyle = TABLE_COLNAMES_STYLE,
  colStyle = list(
    `1` = Style2,
    `2` = Style2,
    `3` = Style1,
    `4` = Style1
  )
)
setColumnWidth(sheet2, colIndex=c(1:ncol(comp_frame)), colWidth=35)

addDataFrame(
  AON,
  sheet3,
  row.names=FALSE,
  startRow = 3,
  startColumn = 3,
  colnamesStyle = TABLE_COLNAMES_STYLE,
  colStyle = list(
    `1` = Style2,
    `2` = Style2,
    `3` = Style1,
    `4` = Style1,
    `5` = Style1,
    `6` = Style1
  )
)

addDataFrame(
  region,
  sheet3,
  row.names=FALSE,
  startRow = 3,
  startColumn = 7,
  colnamesStyle = TABLE_COLNAMES_STYLE,
  colStyle = list(
    `1` = Style2,
    `2` = Style2,
    `3` = Style1,
    `4` = Style1,
    `5` = Style1,
    `6` = Style1
  )
)

saveWorkbook(wb, paste("Validation_Report", ".xlsx", sep = ""))


