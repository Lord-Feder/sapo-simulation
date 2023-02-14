Mat <- scan('/home/lord-feder/Desktop/sapo/SIDARTHE_bigVar/final_result')
baseMat <- matrix(Mat, ncol = 16, byrow = TRUE)

Mat <- scan('/home/lord-feder/Desktop/sapo/SIDARTHE_SawBigVar/sawless_result')
sawlessMat <- matrix(Mat, ncol = 16, byrow = TRUE)

Mat <- scan('/home/lord-feder/Desktop/sapo/SIDARTHE_SawBigVarsawmin_result')
minMat <- matrix(Mat, ncol = 16, byrow = TRUE)


Mat <- scan('/home/lord-feder/Desktop/sapo/SIDARTHE_SawBigVar/sawmax_result')
maxMat <- matrix(Mat, ncol = 16, byrow = TRUE)

Mat <- scan('/home/lord-feder/Desktop/sapo/SIDARTHE_SawBigVar/sawc_result')
sawcMat <- matrix(Mat, ncol = 16, byrow = TRUE)

Mat <- scan('/home/lord-feder/Desktop/sapo/SIDARTHE_SawBigVar/sawboth_result')
sawMat <- matrix(Mat, ncol = 16, byrow = TRUE)


#--------------------------------------------
#visualization

if(1){
  bascol<-c("black","blue","cyan","green","magenta","red","orange","grey")
  basLeg <- c("Suscieptible","Infected","Diagnosed","Ailing","Recognized","Threatened","Healed","Extinct")
  #altLeg <- c("Suscieptible","Infected ND AS","Infected D AS","Infected ND S","Infected D S","Infected D IC","Healed","Extinct")
  
  colons <- c(-20)#c(-1,-7,-8,-9,-15,-16)
  
  rows<-350
  
  step<- 0.01
  
  xdata <- seq(0.01, rows, by = step)
  
  data <- (sawlessMat[(xdata/step),colons])
  
  top<- 1
  
  x <- ncol(data)/2
  
  
  colours<- (c(bascol,bascol))[colons]
  
  
  
  matplot(xdata,data,
          type="l",
          col= colours,
          ylim=c(0,top),
          xlim=c(0,rows),
          #main="Evoluzione a lungo termine della componente infetta della popolazione",
          xlab="Tempo (giorni)",
          ylab="Casi (frazione di popolazione)"
  )
  
  polyax<-c(xdata,rev(xdata))
  for(i in 1:x){
    polydata<- c(data[,i], rev(data[,i+x]))
    polygon(polyax,polydata,
            col=(colours[i]),
            density=20,
            angle=((i*360)/x)
    )
  }
  for(i in 1:x){
    lines(xdata, data[,i], col=colours[i],lwd=2)
    lines(xdata, data[,i+x], col=colours[i],lwd=2)
  }
  
  legend(x=0,
         y=top*0.95,
         legend=basLeg[colons],
         col= colours,
         lty=1, cex=0.4)
  
}
#------------------------------------------------
#Grafic actual vs diagnose

if(1){
  bascol<-c("blue","red","green","black","orange")
  altLeg<-c("Cumulative Infected","Current Total Infected","Recovered","Deaths","Diagnosed Current Total Infected")
  
  colons<-c(-20)
  
  if(1){
    mincuminf <- sawMat[, c(2,3,4,5,6,7,8)]
    maxcuminf <- sawMat[, c(10,11,12,13,14,15,16)]
    
    mincurtotinf <- sawMat[, c(2,3,4,5,6)]
    maxcurtotinf <- sawMat[, c(10,11,12,13,14)]
    
    minDiaTotInf<- sawMat[,c(3,5,6)]
    maxDiaTotInf<- sawMat[,c(11,13,14)]
    
    
    cuminf <- cbind(rowSums(mincuminf),rowSums(maxcuminf))
    curtotinf <- cbind(rowSums(mincurtotinf),rowSums(maxcurtotinf))
    rec <- sawMat[,c(7,15)]
    death <- sawMat[,c(8,16)]
    diaTotInf<-  cbind(rowSums(minDiaTotInf),rowSums(maxDiaTotInf))
  }
  
  rows<-46
  
  step<- 0.01
  
  xdata <- seq(0, rows, by = step)
  
  data <- (cbind(cuminf[,1],curtotinf[,1],rec[,1],death[,1],diaTotInf[,1],
                 cuminf[,2],curtotinf[,2],rec[,2],death[,2],diaTotInf[,2]))[(xdata/step+1),colons]
  
  top<- 0.75
  
  x <- ncol(data)/2
  
  colours<- (c(bascol,bascol))[colons]
  
  
  matplot(xdata,data,
          type="l",
          col= colours,
          ylim=c(0,top),
          xlim=c(0,rows),
          #main="Evoluzione a lungo termine della componente infetta della popolazione",
          xlab="Tempo (giorni)",
          ylab="Casi (frazione di popolazione)"
  )
  
  polyax<-c(xdata,rev(xdata))
  for(i in 1:x){
    polydata<- c(data[,i], rev(data[,i+x]))
    polygon(polyax,polydata,
            col=(colours[i]),
            density=20,
            angle=((i*360)/x)
    )
  }
  for(i in 1:x){
    lines(xdata, data[,i], col=colours[i],lwd=2)
    lines(xdata, data[,i+x], col=colours[i],lwd=2)
  }
  
  legend(x=0,
         y=top*0.95,
         legend=altLeg[colons],
         col= colours,
         lty=1:2, cex=0.8)
  
}


#----------------------------------------------------------
#dif/base

if(1){
  
  bascol<-c("black","blue","cyan","green","magenta","red","orange","grey")
  basLeg <- c("Suscieptible","Infected","Diagnosed","Ailing","Recognized","Threatened","Healed","Extinct")
  #altLeg <- c("Suscieptible","Infected ND AS","Infected D AS","Infected ND S","Infected D S","Infected D IC","Healed","Extinct")
  
  colons<-c(1,2,3,4,5,6,7,8)
  
  Mat1 <-sawMat
  Mat2<- sawlessMat
  
  dif <- cbind((Mat1[,c(1:(ncol(Mat1)/2))]-Mat2[,c(1:(ncol(Mat2)/2))]),(Mat2[,c(((ncol(Mat2)/2)+1):ncol(Mat2))]-Mat1[,c(((ncol(Mat1)/2)+1):ncol(Mat1))]))
  
  
  rows<-350
  start<- 0.01
  step<- 0.01
  
  
  xdata <- seq(start, rows, by = step)
  
  data<- (dif/Mat2)[(xdata/step),colons]
  
  
  top<- 0.015
  
  x <- ncol(data)/2
  
  colours<- (c(bascol,bascol))[colons]
  
  
  matplot(xdata,data,
          type="l",
          col= colours,
          #ylim=c(0,top),
          xlim=c(0,rows),
          main="Minimi",
          xlab="Tempo (giorni)",
          ylab="Miglioramento prodotto dal metodo fratto valore originale"
          #ylab="Differenza in percentuale"
  )
  
  # polyax<-c(xdata,rev(xdata))
  # for(i in 1:x){
  #   polydata<- c(data[,i], rev(data[,i+x]))
  #   polygon(polyax,polydata,
  #           col=(colours[i]),
  #           density=20,
  #           angle=((i*360)/x)
  #   )
  # }
  # for(i in 1:x){
  #   lines(xdata, data[,i], col="",lwd=2)
  #   lines(xdata, data[,i+x], col=colours[i],lwd=2)
  # }
  
#   legend(x=250,
#          y=0.95*0.4,
#          legend=basLeg[colons-8],
#          col= colours,
#          lty=1:2, cex=0.6)
#   
}
