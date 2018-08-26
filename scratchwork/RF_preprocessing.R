#Rotation tree with non-linear embedding
#Writer: Haedong Kim
#Begin: 2016-07-20
#End: 


##########  data preprocessing
##########  every data is splited into 70% training and 30% test


#data1 abalone
data1_train = read.csv('data1_train.csv')
data1_test  = read.csv('data1_test.csv')
data1 = read.csv('data1.csv')

str(data1)

colnames(data1) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "Y")

write.csv(data1, 'data1.csv')

#data2 adult
data2 = read.csv('data2.csv')
data2$education_num = as.integer(data2$education_num) #from 'factor' to 'integer'
data2 = subset(data2, select = -c(native_country)) #too many attributes (41)
str(data2)

trnIdx = sample(1:nrow(data2), round(nrow(data2)*0.7))

data2_train = data2[trnIdx, ]
data2_test = data2[-trnIdx, ]

write.csv(data2_train, 'data2_train.csv')
write.csv(data2_test, 'data2_test.csv')

#data3 annealing; discarded
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\annealing')
data3 = read.table('annealing.txt', sep = ',', stringsAsFactors = FALSE)

str(data3)

i = 1
for(i in 1:nrow(data3)){
  if('?' %in% data3[i, ]){
    data3 = data3[-i, ]
  }
}

#data3 appendicitis
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data3 = read.csv('data3.csv')
str(data3)

write.csv(data3, 'data3.csv')

#data4 Acute_inflammation
#There are two target variables namely Y, Y2
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data4 = read.csv('data4.csv')
str(data4)

#data5 AU2
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data5 = read.csv('data5.csv')
str(data5)
colnames(data5)[252]

#data6 AU5
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\AU\\Data Sets')
data6 = read.csv('au5_500.csv')
str(data6)
colnames(data6)[126] = "Y"
class(data6[ , 126]) #Y is a factor

trainIdx = sample(1:nrow(data6), round(0.7*nrow(data6)))
data6_train = data6[trainIdx, ]
data6_test = data6[-trainIdx, ]

setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')

write.csv(data6, 'data6.csv')
write.csv(data6_train, 'data6_train.csv')
write.csv(data6_test, 'data6_test.csv')

#data7 australian
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\australian')
data7_train = read.csv('data7_train.csv')
data7_test = read.csv('data7_test.csv')
str(data7_test)
str(data7_train)

#convert of a Y's class into a factor
data7_test$Y = as.factor(data7_test$Y) 
data7_train$Y = as.factor(data7_train$Y)

setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')

write.csv(data7_test, 'data7_test.csv')
write.csv(data7_train, 'data7_train.csv')

data7 = read.csv('data7.csv')
str(data7)
data7$Y = as.factor(data7$Y)
write.csv(data7, 'data7.csv')

#data8 automobile
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')

data8 = read.csv('data8.csv')
data8_train = read.csv('data8_train.csv')
data8_test = read.csv('data8_test.csv')

str(data8)
str(data8_train)
str(data8_test)

data8$Y = as.factor(data8$Y)
data8_train$Y = as.factor(data8_train$Y)
data8_test$Y = as.factor(data8_test$Y)

write.csv(data8, 'data8.csv')

#data9 bach
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\bach_chorals_harmony')
data9 = read.csv('jsbach_chorals_harmony.csv')
data9 = data9[ , -c(1, 16, 17)]
colnames(data9) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V14", "Y")

str(data9)

setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
write.csv(data9, "data9.csv")

#data10 balance
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data10_train = read.csv('data10_train.csv')
data10_test = read.csv('data10_test.csv')

str(data10_train)
str(data10_test)

data10 = rbind(data10_train, data10_test)

write.csv(data10, 'data10.csv')

#data11 banana
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data11_train = read.csv('data11_train.csv')
data11_test = read.csv('data11_test.csv') 

str(data11_train)
str(data11_test)

data11 = rbind(data11_train, data11_test)

write.csv(data11, 'data11.csv')

#data12 band
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data12_train = read.csv('data12_train.csv')
data12_test = read.csv('data12_test.csv')

str(data12_train)
str(data12_test)

data12 = rbind(data12_train, data12_test)

write.csv(data12, 'data12.csv')

#data13 
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\blood-transfusion')
data13 = read.csv('blood-transfusion.csv')
str(data13)
colnames(data13) = c("V1", "V2", "V3", "V4", "Y")

setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')

write.csv(data13, 'data13.csv')

#data14 breast
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data14_train = read.csv('data14_train.csv')
data14_test = read.csv('data14_test.csv')

str(data14_train)
str(data14_test)

data14 = rbind(data14_train, data14_test)
getwd()
write.csv(data14, 'data14.csv')

#data15 breasttissue
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\breasttissue')
data15 = read.csv('breasttissue.csv')
str(data15)
colnames(data15)[1] = "Y"

setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')

write.csv(data15, 'data15.csv')

#data16 bupa
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data16_train = read.csv('data16_train.csv')
data16_test = read.csv('data16_test.csv')

str(data16_train)
str(data16_test)

data16 = rbind(data16_train, data16_test)
getwd()

write.csv(data16, 'data16.csv')

#data17 car
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')

data17_train = read.csv('data17_train.csv')
data17_test = read.csv('data17_test.csv')

str(data17_train)
str(data17_test)

data17 = rbind(data17_train, data17_test)
write.csv(data17, 'data17.csv')

#data18 census
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset')
setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data18_train = read.csv('data18_train.csv')
data18_test = read.csv('data18_test.csv')

str(data18_train)
str(data18_test)

data18 = rbind(data18_train, data18_test)
write.csv(data18, 'data18.csv')

#data19 chess
data19 = read.csv('data19.csv')
str(data19)
write.csv(data19, "data19.csv")

#data20 cleveland
data20 = read.csv('data20.csv')
str(data20)
write.csv(data20, 'data20.csv')

#data21 climate model simulation
data21 = read.table('data21.txt', header = TRUE, stringsAsFactors = FALSE)
str(data21)
colnames(data21)[21] = "Y"
write.csv(data21, "data21.csv")

#data22 coil2000
data22 = read.csv('data22.csv')
str(data22)
write.csv(data22, 'data22.csv')

#data23 congressional voting
data23 = read.csv('data23.csv')
str(data23)
write.csv(data23, 'data23.csv')

#data24 connect-4
data24 = read.csv('data24.csv')
str(data24)
write.csv(data24, 'data24.csv')

#data25 contraceptive
data25 = read.csv('data25.csv')
str(data25)
write.csv(data25, 'data25.csv')

#data26 crx
data26 = read.csv('data26.csv')
str(data26)
write.csv(data26, 'data26.csv')

#data27 freezing of gait
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\daphnet freezing of gait\\dataset')
data27 = read.table('S01R01.txt')
str(data27)
unique(data27$Y)
data27 = data27[ , -1]
colnames(data27) = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9","Y")
data27 = data27[!(data27$Y==0), ]

data27_2 = read.table('S01R02.txt')
str(data27_2)
unique(data27_2$V11)
data27 = data27[ , -1]



setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
write.csv(data27, 'data27.csv')

#data28 default of credit card clients
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\defalutofcreditcard')
install.packages('xlsx', dependencies = TRUE)
library(xlsx)
data28 = read.xlsx('default of credit card clients.xls', sheetName = 'Data')

setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
data28 = read.csv('data28.csv')
str(data28)
write.csv(data28, 'data28.csv')

#data29 dermatology
data29 = read.csv('data29.csv')
str(data29)
write.csv(data29, 'data29.csv')

#data30 diabetic retinopathy
data30 = read.csv('data30.csv')
str(data30)
unique(data30$Y)
write.csv(data30, 'data30.csv')

#data31 echocardiogram
data31 = read.csv('data31.csv', stringsAsFactors = FALSE)
str(data31)
#data.frame(lapply(data31, function(x){gsub(NA, "?", x)}))

data31 = data31[!(data31$Y %in% "?"), ]
data31$Y[grep(2, data31$Y)] = 1
unique(data31$Y)

write.csv(data31, 'data31.csv')

#data32 ecoil
data32 = read.csv('data32.csv')
str(data32)
write.csv(data32, 'data32.csv')

#data33 far 
data33 = read.csv('data33.csv')
str(data33)

data33 = data33[ , c(-1, -26, -27, -28)]

write.csv(data33, 'data33.csv')

#data34 fertility
data35 = read.csv('data35.csv')
str(data35)
write.csv(data35, 'data34.csv')

#data 35 firm_teacher_clave_direction
data35 = read.table('data35.txt')
str(data35)
which(apply(data35[, c('V17', 'V18', 'V19', 'V20')], 1, sum)>=2)
data35[c(641, 1086), c('V17', 'V18', 'V19', 'V20')] #multi-class

data35 = data35[c(-641, -1086), ]

Y = rep(0, nrow(data35))
Y[which(data35[ , 'V17'] == 1)] = 'neutral'
Y[which(data35[ , 'V18'] == 1)] = 'reverseClave'
Y[which(data35[ , 'V19'] == 1)] = 'forwardClave'
Y[which(data35[ , 'V20'] == 1)] = 'incoherent'

data35 = data35[ , c(-17, -18, -19, -20)]
data35 = cbind(data35, Y)

str(data35)

write.csv(data35, 'data35.csv')

#data 36 

#data 38
data38_train = read.csv('data38_train.csv')
data38_test = read.csv('data38_test.csv')

str(data38_train)
str(data38_test)

data38 = rbind(data38_train, data38_test)

write.csv(data38, 'data38.csv')

#data 39; discarded
library(stringr)
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\gas_sensor')
batch1 = read.table('batch1.txt', sep = ':', stringsAsFactors = FALSE)
str(batch1)
as.numeric(batch1$V2)

#data 49
getwd()
data49_train = read.csv('data49_train.csv')
data49_test = read.csv('data49_test.csv')
data49 = rbind(data49_train, data49_test)
write.csv(data49, 'data49.csv')

#data 69
setwd('D:\\DSBA\\Researchworks\\R164RF\\classification_dataset\\primary biliary cirrhosis (PBC)')
data69 = read.table('primary biliary cirrhosis (PBC).txt', sep = "", na.strings = ".",stringsAsFactors = FALSE)
str(data69)
data69 = data69[ , -1] #remove ID variable

colnames(data69)[2] = "Y"
unique(data69$Y)

which(data69$V2 == ".")
as.numeric(data69$V2)
data.frame(lapply(data69, function(x){x = as.numeric(x)}))

setwd('C:\\Users\\haedong\\Google 드라이브\\R16RF\\data')
write.csv(data69, 'data69.csv')

