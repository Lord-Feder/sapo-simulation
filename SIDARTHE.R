Mat <- scan('/home/lord-feder/Desktop/sapo/SIDARTHE/final_result')
baseMat <- matrix(Mat, ncol = 16, byrow = TRUE)

Mat <- scan('/home/lord-feder/Desktop/sapo/1-IDARTHE/final_result')
Mat <- matrix(Mat, ncol = 14, byrow = TRUE)

lowI <- (1-rowSums(Mat[,c(8,9,10,11,12,13,14)]))
highI <- (1-rowSums(Mat[,c(1,2,3,4,5,6,7)]))

sustMat<- cbind(lowI,Mat[,c(1,2,3,4,5,6,7)],highI,Mat[,c(8,9,10,11,12,13,14)])


#--------------------------------------------
#fit visualization

if(1){
  bascol<-c("black","blue","cyan","green","magenta","red","orange","grey")
  #basLeg <- c("Suscieptible","Infected","Diagnosed","Ailing","Recognized","Threatened","Healed","Extinct")
  altLeg <- c("Suscieptible","Infected ND AS","Infected D AS","Infected ND S","Infected D S","Infected D IC","Healed","Extinct")
  
  colons <-c(-1,-7,-8,-9,-15,-16)
  
  rows<-46
  
  step<- 0.01
  
  xdata <- seq(0, rows, by = step)
  
  data <- (sustMat[(xdata/step+1),colons])
  
  top<- 0.001
  
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
#------------------------------------------------
#Grafic actual vs diagnose

if(1){
  bascol<-c("blue","red","green","black","orange")
  altLeg<-c("Cumulative Infected","Current Total Infected","Recovered","Deaths","Diagnosed Current Total Infected")
  
  colons<-c(-20)
  
  if(1){
    mincuminf <- sustMat[, c(2,3,4,5,6,7,8)]
    maxcuminf <- sustMat[, c(10,11,12,13,14,15,16)]
    
    mincurtotinf <- sustMat[, c(2,3,4,5,6)]
    maxcurtotinf <- sustMat[, c(10,11,12,13,14)]
    
    minDiaTotInf<- sustMat[,c(3,5,6)]
    maxDiaTotInf<- sustMat[,c(11,13,14)]
    
    
    cuminf <- cbind(rowSums(mincuminf),rowSums(maxcuminf))
    curtotinf <- cbind(rowSums(mincurtotinf),rowSums(maxcurtotinf))
    rec <- sustMat[,c(7,15)]
    death <- sustMat[,c(8,16)]
    diaTotInf<-  cbind(rowSums(minDiaTotInf),rowSums(maxDiaTotInf))
  }
  
  rows<-46
  
  step<- 0.01
  
  xdata <- seq(0, rows, by = step)
  
  data <- (cbind(cuminf[,1],curtotinf[,1],rec[,1],death[,1],diaTotInf[,1],
                 cuminf[,2],curtotinf[,2],rec[,2],death[,2],diaTotInf[,2]))[(xdata/step+1),colons]
  
  top<- 0.015
  
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

bascol<-c("black","blue","cyan","green","magenta","red","orange","grey")
basLeg <- c("Suscieptible","Infected","Diagnosed","Ailing","Recognized","Threatened","Healed","Extinct")
#altLeg <- c("Suscieptible","Infected ND AS","Infected D AS","Infected ND S","Infected D S","Infected D IC","Healed","Extinct")

colons<-c(-1,-2,-3,-4,-5,-6,-7,-8)

Mat1 <-sustMat
Mat2< baseMat

dif <- cbind((Mat1[,c(1:(ncol(Mat1)/2))]-Mat2[,c(1:(ncol(Mat2)/2))]),(Mat2[,c(((ncol(Mat2)/2)+1):ncol(Mat2))]-Mat1[,c(((ncol(Mat1)/2)+1):ncol(Mat1))]))


rows<-350

step<- 0.01

xdata <- seq(0, rows, by = step)

data<- (dif/Mat2)[(xdata/step+1),colons]


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
        ylab="Miglioramento prodotto dal metodo fratto il valore originale"
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

legend(x=0.12,
       #y=top*0.95,
       legend=basLeg[colons-8],
       col= colours,
       lty=1:2, cex=0.8)
