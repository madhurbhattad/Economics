#Read data 
Philips <- read.csv("Phillips Curve Data.csv", header= TRUE)

Philips <- as.data.frame(Philips)

Philips[] <- lapply(Philips, gsub, pattern = "#N/A", replacement = "-1", fixed = TRUE)
Philips2 <- data.frame(Philips[37:nrow(Philips),])

#Inflation Q1/Q4 - 1
Philips.v <- as.matrix(Philips2)

Inflation <- data.frame(matrix(NA,ncol=4))
colnames(Inflation) <- c("Q1","Q2", "Q3","Q4")

Inflation.1956Q4 <- as.numeric((Philips.v[,2][1]))

Inflation[1,"Q1"] = as.numeric((Philips.v[,2][1]))/ as.numeric(Inflation.1956Q4) - 1
Inflation[1,"Q2"] = as.numeric((Philips.v[,2][2]))/ as.numeric((Philips.v[,2][1])) - 1
Inflation[1,"Q3"] = as.numeric((Philips.v[,2][3]))/ as.numeric((Philips.v[,2][2])) - 1
Inflation[1,"Q4"] = as.numeric((Philips.v[,2][4]))/ as.numeric((Philips.v[,2][3])) - 1

#Q1,Q2,Q3,Q4 inflations
for (i in 1:((floor(length(Philips.v[,1]))/4) - 1)) 
{
  if (Philips.v[,2][i] == - 1) 
  {
    Inflation[i+1,"Q1"] = 1
    Inflation[i+1,"Q2"] = 1
    Inflation[i+1,"Q3"] = 1
    Inflation[i+1,"Q4"] = 1
  } 
  else 
  {
    Inflation[i+1,"Q1"] = as.numeric((Philips.v[,2][4*i + 1]))/
      as.numeric((Philips.v[,2][4*i])) - 1
    Inflation[i+1,"Q2"] = as.numeric((Philips.v[,2][4*i + 2]))/
      as.numeric((Philips.v[,2][4*i + 1])) - 1
    Inflation[i+1,"Q3"] = as.numeric((Philips.v[,2][4*i + 3]))/
      as.numeric((Philips.v[,2][4*i + 2])) - 1
    Inflation[i+1,"Q4"] = as.numeric((Philips.v[,2][4*i + 4]))/
      as.numeric((Philips.v[,2][4*i])) - 1
  }
}

dates <- c()

for (i in 0:(nrow(Inflation)-1)) 
{
  dates = cbind(dates, as.character.Date(1957 + i))
}
rownames(Inflation) <- dates

#Inflation, four quarters
#print(Inflation)

#Unemployment rate, four quarters and average
Unemployment <- data.frame(matrix(NA,ncol=5))
colnames(Unemployment) <- c("Q1","Q2", "Q3","Q4","YEAR")

Philips.v[,5] <- as.numeric(sub("%", "",Philips.v[,5],fixed=TRUE))/100

#Q1,Q2,Q3,Q4 unemployment
for (i in 0:((floor(length(Philips.v[,1])/4)) - 1)) 
{
  Unemployment[i+1,"Q1"] = as.numeric((Philips.v[,5][4*i + 1]))
  Unemployment[i+1,"Q2"] = as.numeric((Philips.v[,5][4*i + 2]))
  Unemployment[i+1,"Q3"] = as.numeric((Philips.v[,5][4*i + 3]))
  Unemployment[i+1,"Q4"] = as.numeric((Philips.v[,5][4*i + 4]))
  Unemployment[i+1,"YEAR"]=(Unemployment[i+1,"Q1"]+Unemployment[i+1,"Q2"]+
                              Unemployment[i+1,"Q3"]+Unemployment[i+1,"Q4"])/4 - 0.06
}
rownames(Unemployment) <- dates

#Unemployment cost index, Core CPI, GDP Deflator
CoreCPI <- data.frame(matrix(NA,ncol=5))
colnames(CoreCPI) <- c("Q1","Q2", "Q3","Q4","YEAR")

TotalCompensation <- data.frame(matrix(NA,ncol=5))
colnames(TotalCompensation) <- c("Q1","Q2", "Q3","Q4","YEAR")

GDPDeflator <- data.frame(matrix(NA,ncol=5))
colnames(GDPDeflator) <- c("Q1","Q2", "Q3","Q4","YEAR")

# Quarters
for (i in 0:((floor(length(Philips.v[,1])/4)) - 1)) 
{
  CoreCPI[i+1,"Q1"] = as.numeric((Philips.v[,2][4*i + 1]))
  CoreCPI[i+1,"Q2"] = as.numeric((Philips.v[,2][4*i + 2]))
  CoreCPI[i+1,"Q3"] = as.numeric((Philips.v[,2][4*i + 3]))
  CoreCPI[i+1,"Q4"] = as.numeric((Philips.v[,2][4*i + 4]))
  
  TotalCompensation[i+1,"Q1"] = as.numeric((Philips.v[,3][4*i + 1]))
  TotalCompensation[i+1,"Q2"] = as.numeric((Philips.v[,3][4*i + 2]))
  TotalCompensation[i+1,"Q3"] = as.numeric((Philips.v[,3][4*i + 3]))
  TotalCompensation[i+1,"Q4"] = as.numeric((Philips.v[,3][4*i + 4]))
  
  GDPDeflator[i+1,"Q1"] = as.numeric((Philips.v[,4][4*i + 1]))
  GDPDeflator[i+1,"Q2"] = as.numeric((Philips.v[,4][4*i + 2]))
  GDPDeflator[i+1,"Q3"] = as.numeric((Philips.v[,4][4*i + 3]))
  GDPDeflator[i+1,"Q4"] = as.numeric((Philips.v[,4][4*i + 4]))
}
rownames(CoreCPI) <- dates
rownames(TotalCompensation) <- dates
rownames(GDPDeflator) <- dates

# Years
for (i in 2:(nrow(CoreCPI)-1))
{
  CoreCPI[i+1,"YEAR"] = 100 *(CoreCPI[i+1,"Q4"]/
                                CoreCPI[i,"Q4"] - CoreCPI[i,"Q4"]/CoreCPI[i-1,"Q4"])
  TotalCompensation[i+1,"YEAR"] = 100 *(TotalCompensation[i+1,"Q4"]/
                                          TotalCompensation[i,"Q4"] - TotalCompensation[i,"Q4"]/
                                          TotalCompensation[i-1,"Q4"])
  GDPDeflator[i+1,"YEAR"] = 100 *(GDPDeflator[i+1,"Q4"]/GDPDeflator[i,"Q4"] -
                                    GDPDeflator[i,"Q4"]/GDPDeflator[i-1,"Q4"])
}


#plots
p1 <- plot(y = Unemployment$YEAR[24:38], x = seq(1980,1994,1), xlab = "Years",
           ylab = "Unemployment in percent", type= "l",main= "Unemployment Gap ")
p2 <- plot(y = CoreCPI$YEAR[24:38], x = seq(1980,1994,1), xlab = "Years",
           ylab = "Core CPI in percent", type= "l",main= "Core CPI")
p3 <- plot(y = TotalCompensation$YEAR[24:38], x = seq(1980,1994,1), xlab = "Years",
           ylab = "Employment cost index in percent", type= "l",main= "Total compensation")
p4 <- plot(y = GDPDeflator$YEAR[24:38], x = seq(1980,1994,1), xlab = "Years",
           ylab = "GDP Deflator in percent", type= "l",main= "GDP Deflator")


p01 <- plot(y = Unemployment$YEAR[39:60], x = seq(1995,2016,1), xlab = "Years",
            ylab = "Unemployment in percent", type= "l",main= "Unemployment Gap ")
p02 <- plot(y = CoreCPI$YEAR[39:60], x = seq(1995,2016,1), xlab = "Years",
            ylab = "Core CPI in percent", type= "l",main= "Core CPI")
p03 <- plot(y = TotalCompensation$YEAR[39:60], x = seq(1995,2016,1), xlab = "Years",
            ylab = "Employment cost index in percent", type= "l",main= "Total compensation")
p04 <- plot(y = GDPDeflator$YEAR[39:60], x = seq(1995,2016,1), xlab = "Years",
            ylab = "GDP Deflator in percent", type= "l",main= "GDP Deflator")


# Getting Individual vectors instead of data frame

Q1Unemployment <- Unemployment$Q1
Q2Unemployment <- Unemployment$Q2
Q3Unemployment <- Unemployment$Q3
Q4Unemployment <- Unemployment$Q4
YEARUnemployment <- Unemployment$YEAR

Q1CoreCPI <- CoreCPI$Q1
Q2CoreCPI <- CoreCPI$Q2
Q3CoreCPI <- CoreCPI$Q3
Q4CoreCPI <- CoreCPI$Q4
YEARCoreCPI <- CoreCPI$YEAR

Q1TotalCompensation <- TotalCompensation$Q1
Q2TotalCompensation <- TotalCompensation$Q2
Q3TotalCompensation <- TotalCompensation$Q3
Q4TotalCompensation <- TotalCompensation$Q4
YEARTotalCompensation <- TotalCompensation$YEAR

Q1GDPDeflator <- GDPDeflator$Q1
Q2GDPDeflator <- GDPDeflator$Q2
Q3GDPDeflator <- GDPDeflator$Q3
Q4GDPDeflator <- GDPDeflator$Q4
YEARGDPDeflator <- GDPDeflator$YEAR

# Net Vectors that would be needed for regression
UnemploymentTimeSeries <- vector(mode="numeric", length = 240)
for(i in 1:60)
{
  UnemploymentTimeSeries[4*i-3] = Q1Unemployment[i]*100
  UnemploymentTimeSeries[4*i-2] = Q2Unemployment[i]*100
  UnemploymentTimeSeries[4*i-1] = Q3Unemployment[i]*100
  UnemploymentTimeSeries[4*i]   = Q4Unemployment[i]*100
}

CoreCPITimeSeries <- vector(mode="numeric", length = 240)
for(i in 1:60)
{
  CoreCPITimeSeries[4*i-3] = Q1CoreCPI[i]
  CoreCPITimeSeries[4*i-2] = Q2CoreCPI[i]
  CoreCPITimeSeries[4*i-1] = Q3CoreCPI[i]
  CoreCPITimeSeries[4*i]   = Q4CoreCPI[i]
}

TotalCompensationTimeSeries <- vector(mode="numeric", length = 240)
for(i in 1:60)
{
  TotalCompensationTimeSeries[4*i-3] = Q1TotalCompensation[i]
  TotalCompensationTimeSeries[4*i-2] = Q2TotalCompensation[i]
  TotalCompensationTimeSeries[4*i-1] = Q3TotalCompensation[i]
  TotalCompensationTimeSeries[4*i]   = Q4TotalCompensation[i]
}

GDPDeflatorTimeSeries <- vector(mode="numeric", length = 240)
for(i in 1:60)
{
  GDPDeflatorTimeSeries[4*i-3] = Q1GDPDeflator[i]
  GDPDeflatorTimeSeries[4*i-2] = Q2GDPDeflator[i]
  GDPDeflatorTimeSeries[4*i-1] = Q3GDPDeflator[i]
  GDPDeflatorTimeSeries[4*i]   = Q4GDPDeflator[i]
}

# Regression on CoreCPI from 1961-94 along with 12 lagged values of CoreCPI and 2 lagged unemployment rates 

Table1 <- lm(
  CoreCPITimeSeries[13:152] ~ 
    CoreCPITimeSeries[1:140] + CoreCPITimeSeries[2:141] + CoreCPITimeSeries[3:142] +
    CoreCPITimeSeries[4:143] + CoreCPITimeSeries[5:144] + CoreCPITimeSeries[6:145] +
    CoreCPITimeSeries[7:146] + CoreCPITimeSeries[8:147] + CoreCPITimeSeries[9:148] +
    CoreCPITimeSeries[10:149] + CoreCPITimeSeries[11:150] + CoreCPITimeSeries[12:151] +
    UnemploymentTimeSeries[11:150] + UnemploymentTimeSeries[12:151]
)
Table1
summary(Table1)

# Regression on CoreCPI from 1995-2016 along with 12 lagged values of CoreCPI and 2 lagged unemployment rates 

Table2 <- lm(
  CoreCPITimeSeries[165:240] ~ 
    CoreCPITimeSeries[153:228] + CoreCPITimeSeries[154:229] + CoreCPITimeSeries[155:230] +
    CoreCPITimeSeries[156:231] + CoreCPITimeSeries[157:232] + CoreCPITimeSeries[158:233] +
    CoreCPITimeSeries[159:234] + CoreCPITimeSeries[160:235] + CoreCPITimeSeries[161:236] +
    CoreCPITimeSeries[162:237] + CoreCPITimeSeries[163:238] + CoreCPITimeSeries[164:239] +
    UnemploymentTimeSeries[163:238] + UnemploymentTimeSeries[164:239]
)
Table2
summary(Table2)

# Regression on CoreCPI from 1961-94 along with 12 lagged values of CoreCPI 

Table3 <- lm(
  CoreCPITimeSeries[13:152] ~ 
    CoreCPITimeSeries[1:140] + CoreCPITimeSeries[2:141] + CoreCPITimeSeries[3:142] +
    CoreCPITimeSeries[4:143] + CoreCPITimeSeries[5:144] + CoreCPITimeSeries[6:145] +
    CoreCPITimeSeries[7:146] + CoreCPITimeSeries[8:147] + CoreCPITimeSeries[9:148] +
    CoreCPITimeSeries[10:149] + CoreCPITimeSeries[11:150] + CoreCPITimeSeries[12:151]
)
Table3
summary(Table3)

# Recreting Core CPI for 1961-94
recCoreCPI <- vector(mode= "numeric", length = 140)
for(i in 1:140)
{
  recCoreCPI[i] = Table1$coefficients[1] 
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[2]*CoreCPITimeSeries[i]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[3]*CoreCPITimeSeries[i+1]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[4]*CoreCPITimeSeries[i+2]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[5]*CoreCPITimeSeries[i+3]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[6]*CoreCPITimeSeries[i+4]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[7]*CoreCPITimeSeries[i+5]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[8]*CoreCPITimeSeries[i+6]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[9]*CoreCPITimeSeries[i+7]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[10]*CoreCPITimeSeries[i+8]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[11]*CoreCPITimeSeries[i+9]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[12]*CoreCPITimeSeries[i+10]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[13]*CoreCPITimeSeries[i+11]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[13]*UnemploymentTimeSeries[i+10]
  recCoreCPI[i] = recCoreCPI[i] + Table1$coefficients[13]*UnemploymentTimeSeries[i+11]
}

plot(y= recCoreCPI, x = 1:140, type ='l', col="red")
points(y=CoreCPITimeSeries[13:152], x=1:140, type ='l', col="blue")

# Recreating CoreCPI for 95-2016 using reg data for 61-94
laggedrecCoreCPI <- vector(mode= "numeric", length = 76)
for(i in 1:76)
{
  j=152+i
  laggedrecCoreCPI[i] = Table1$coefficients[1] 
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[2]*CoreCPITimeSeries[j]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[3]*CoreCPITimeSeries[j+1]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[4]*CoreCPITimeSeries[j+2]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[5]*CoreCPITimeSeries[j+3]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[6]*CoreCPITimeSeries[j+4]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[7]*CoreCPITimeSeries[j+5]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[8]*CoreCPITimeSeries[j+6]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[9]*CoreCPITimeSeries[j+7]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[10]*CoreCPITimeSeries[j+8]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[11]*CoreCPITimeSeries[j+9]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[12]*CoreCPITimeSeries[j+10]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[13]*CoreCPITimeSeries[j+11]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[13]*UnemploymentTimeSeries[j+10]
  laggedrecCoreCPI[i] = laggedrecCoreCPI[i] + Table1$coefficients[13]*UnemploymentTimeSeries[j+11]
}

plot(y= laggedrecCoreCPI, x = 1:76, type ='l', col="red")
points(y=CoreCPITimeSeries[165:240], x=1:76, type ='l', col="blue")


