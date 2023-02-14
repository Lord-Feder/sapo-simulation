Mat <- scan('/home/lord-feder/Desktop/sapo/SEAIR/final_result')
baseMat <- matrix(Mat, ncol = 10, byrow = TRUE)

Mat <- scan('/home/lord-feder/Desktop/sapo/SEAIR_Saw/sawless_result')
sawlessMat <- matrix(Mat, ncol = 10, byrow = TRUE)

Mat <- scan('/home/lord-feder/Desktop/sapo/SEAIR_Saw/sawboth_result')
sawMat <- matrix(Mat, ncol = 10, byrow = TRUE)

if(1){

bascol<-c("blue","magenta","orange","red","green")
basLeg <- c("Suscieptible","Exposed","Asymptomatic","Infected","Recovered")

colons <-c(-20)

rows<-100

step<- 1

xdata <- seq(1, rows, by = step)

data <- (sawMat[(xdata/step),colons])

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
       lty=1:2, cex=0.8)
}


#----------------------------------------------------------
#dif/base

if(1){
  
  
  bascol<-c("blue","magenta","orange","red","green")
  basLeg <- c("Suscieptible","Exposed","Asymptomatic","Infected","Recovered")
  
  colons<-c(1,2,3,4,5)
  
  Mat1 <-sawlessMat
  Mat2<- baseMat [c(-1),]
  
  dif <- cbind((Mat1[,c(1:(ncol(Mat1)/2))]-Mat2[,c(1:(ncol(Mat2)/2))]),(Mat2[,c(((ncol(Mat2)/2)+1):ncol(Mat2))]-Mat1[,c(((ncol(Mat1)/2)+1):ncol(Mat1))]))
  
  
  rows<- 100
  start<- 1
  step<- 1
  
  
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
          #ylab="Miglioramento prodotto dal metodo fratto il valore originale"
          ylab="Differenza in percentuale"
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
  
  # legend(x=0.12,
  #        #y=top*0.95,
  #        legend=basLeg[colons-8],
  #        col= colours,
  #        lty=1:2, cex=0.8)
  # 
}
