  ##
  ##
  library(tidyverse)
  library(gapminder)
  library(forcats)

library(data.table) 

 dataHB <- read.csv("FourICD10.csv", header = TRUE, sep=";")
     attach(dataHB)

  ICDstr = "C00 - D48" 
  # ICDstr = "D50 - D89"  
  # ICDstr = "E00 - E90" 
  # ICDstr = "R00 - R99" 
   
   dataJA = dataHB[ICD10 == ICDstr,]

  
  dataJA %>%
 #
  ggplot(aes(x=ICD10, y=NO2, fill=factor(LAG))) +
  geom_boxplot() + #geom_boxplot(outlier.colour = "hotpink") +
  labs(fill = "Lag") + 
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme_bw(base_size = 16)


#######################################################################
  dataHA<- dataHB[,"NO2" ]
  dataHE <- dataHB[,"SN" ]
  dataHL <-  dataHA -1.96*dataHE
  dataHU <-  dataHA +1.96*dataHE

  # D=1
  
   D=10
  RR = exp(dataHA*D)
  TL = exp(dataHL*D)
  TU = exp(dataHU*D)
 

########################################################################
#  
library(MASS); library(minpack.lm)
 
 RM=c(0:14); RD= RX=RI=R25=RX= R75=RM
 LM=c(0:14); LD= LX=LI=L25=LX= L75=LM
 UM=c(0:14); UD= UX=UI=U25=UX= U75=UM


# Creat the X-axis; Lag 0-14
xp=seq(0, 14, by=1);
x=xp; xa <- xp
N=14; MIN=2; MAX=-2

#Prepare the data frame by lags 0-14
i=0
for (k in 0:N) { i=i+1
ip=k*18+1; ik=ip+17
ci = c(ip,ik)
print(ci)

if (k==0){
 TRR = RR[ip:ik];  rr=TRR  
 TLR = TL[ip:ik];  rl=TLR 
 TUR = TU[ip:ik];  ru=TUR
 #RM= (mean (rr))
 #RD= (median(rr))
   
  } # for k=0

# The names in dataIN should be as used (TAU, etc.)
  if (k> 0) {
  rr = RR[ip:ik];   
  rl = TL[ip:ik];  
  ru = TU[ip:ik]
  TRR=rbind(TRR,rr); TLR=rbind(TLR,rl); TUR=rbind(TUR,ru)
}
# Find minimum and maximum values for plot
m=mean(rr); RM[i]=m
m=median(rr); RD[i]=m
m=min(rr); RI[i]=m
m=max(rr); RX[i]=m
m=quantile(rr)
R25[i] = m[2]; R75[i]=m[4]

m=mean(rl); LM[i]=m
m=median(rl); LD[i]=m
m=min(rl); LI[i]=m
m=max(rl); LX[i]=m
m=quantile(rl)
L25[i] = m[2]; L75[i]=m[4]

m=mean(ru); UM[i]=m
m=median(ru); UD[i]=m
m=min(ru); UI[i]=m
m=max(ru); UX[i]=m
m=quantile(ru)
U25[i] = m[2]; U75[i]=m[4]

}# # Define frame with X, RR, RRlower, and RRupper (for each lag)
######################################################################
#####################################################################
  Dat=cbind(x,RM,RD,RI,RX,R25,R75,LM,LD,LI,LX,L25,L75,UM,UD,UI,UX,U25,U75)
  DatF = data.frame(Dat)

################Define plot area (white) #############
plot(xa,TRR[,3:3], ylim=c(.95,1.1), col="white"); abline(h=1)
################


 fitM <- nlsLM(
  DatF$RD ~  A*DatF$x^3 + B * DatF$x^2 + C * DatF$x + D,
  start = list( A=2, B = 3, C = 4, D = 1),
  data = DatF)

# summary(fitM)
#

   lines(DatF$x,fitted(fitM), lwd=4,col="black")  #max
   points(DatF$x, DatF$RD)


fitM <- nlsLM(
  DatF$LD ~  A*DatF$x^3 + B * DatF$x^2 + C * DatF$x + D,
  start = list( A=2, B = 3, C = 4, D = 1),
  data = DatF)

# summary(fitM)
#

   lines(DatF$x,fitted(fitM), lwd=4,col="blue")  #max
  # points(DatF$x, DatF$LD)

  fitM <- nlsLM(
  DatF$UD ~  A*DatF$x^3 + B * DatF$x^2 + C * DatF$x + D,
  start = list( A=2, B = 3, C = 4, D = 1),
  data = DatF)

# summary(fitM)
#

   lines(DatF$x,fitted(fitM), lwd=4,col="red")  #max
   #points(DatF$x, DatF$UD)

#############################################################
##########################################################

