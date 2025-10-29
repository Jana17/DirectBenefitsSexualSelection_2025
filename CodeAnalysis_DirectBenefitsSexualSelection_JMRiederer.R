## Script to generate the figures in: Sexual selection driven by direct benefits 
##leads to the erosion of direct benefits
##Jana Riederer, j.m.riederer@rug.nl


###first, let's check if the colour code is colourblind-friendly
install.packages("colorblindcheck")
library(colorblindcheck)
##look up hex codes here: https://rpubs.com/mbh/rgbhex
my_pal<-c("#FF0000","#FFA500", "#40E0D0", "#006400")
palette_check(my_pal,plot=TRUE)
##Red and dark green is a bad combination, 
##but they are never in the same plot together!
##so we are good :)


####Making Figure 2##########
##Here, set the directory containing the data
DataFull<-read.csv(paste("Avg_Baseline_SingleRep.csv",sep = ""))
Data<-subset(DataFull,DataFull$Time<10001)

par(mfrow=c(1,1))
svg(filename="Fig2.svg",width = 5.80, height = 4.80)
plot(NULL,xlim=c(0,10000),ylim=c(0,1.2),xlab="Generation",ylab="Average values")
lines(Data$avg_p~Data$Time,col='red',lwd=2)
lines(Data$avg_tau~Data$Time,col='orange',lwd=2)
lines(Data$avg_gamma~Data$Time,col='turquoise',lwd=3)
text(10000, 1.02, "Female preference", col = "red", cex = 0.9, pos=2)
text(10000, 0.7, "Male investment in ornamentation", col = "orange", cex = 0.9, pos=2)
text(10000, 0.3, "Paternal care", col = "turquoise", cex = 0.9, pos=2)
dev.off()


#######Making Figure 3A###########
##Here, set the directory containing the data
Data<-read.csv(paste("Individual_Baseline.csv",sep = ""))
AvgData<-read.csv(paste("Avg_Baseline_SingleRep.csv",sep = ""))

svg(filename="Fig3A.svg",width=6.30,height = 6.75)
par(mfrow=c(2,2))
DataM <-subset(Data,Data$Sex==0)
t_seq<-seq(0,2,0.001)
chi=10
point_cex=0.6

for(i in 1:4){
  if (i==1){CurrentTime=500}
  if (i==2){CurrentTime=1000}
  if (i==3){CurrentTime=2000}
  if (i==4){CurrentTime=10000}
  DataT<-subset(DataM,DataM$Time==CurrentTime)
  plot(NULL,ylim=c(0,1.5),xlim=c(0.1,1.5),ylab=" ", xlab= "Ornament size T",main=paste("Generation ",CurrentTime,sep = ""))
  points(DataT$Res~DataT$t,pch=20,col="dark green",cex=point_cex)
  points(DataT$gamma~DataT$t,pch=20,col="turquoise",cex=point_cex)
  ##adding female preference function
  AvgDataT<-subset(AvgData,AvgData$Time==CurrentTime)
  p<-AvgDataT$avg_p
  mating_prob<-(exp(-0.5*((p-t_seq)^2)*(chi^2)))
  lines(mating_prob~t_seq,lwd=2,col='red')
  if(CurrentTime==500){
    text(0.4, 1.3, substitute(paste(bold("Resource 
     level"))), col = "dark green", cex = 1.1, pos=4)
    text(0.4, 0.9, substitute(paste(bold("Paternal 
     care"))), col = "turquoise", cex = 1.1, pos=4)
  }
  ##adding avg ornament
  axis(1, at = c(AvgDataT$avg_t), labels = NA,col.ticks = 'orange',lwd.ticks = 5,tcl=0.5)
  axis(1, at = c(AvgDataT$avg_t), labels = NA,col.ticks = 'orange',lwd.ticks = 5,tcl=-0.5)
  ##adding avg female preference
  axis(1, at = c(AvgDataT$avg_p), labels = NA,col.ticks = 'red',lwd.ticks = 5,tcl=0.5)
  axis(1, at = c(AvgDataT$avg_p), labels = NA,col.ticks = 'red',lwd.ticks = 5,tcl=-0.5)
}
dev.off()

###########Making Figure 3B#############
library(yarrr)

##Here, set the directory containing the data
DataFull<-read.csv(paste("MatingEvents_Baseline.csv",sep = ""))
DataSurvival<-data.frame()

for(i in 1:4){
  if (i==1){CurrentTime=500}
  if (i==2){CurrentTime=1000}
  if (i==3){CurrentTime=2000}
  if (i==4){CurrentTime=10000}
  Data<-subset(DataFull,DataFull$Time==CurrentTime)
  nrRows<-nrow(Data)
  DataSurvival1<- data.frame(matrix("", ncol = 1, nrow = nrRows))  
  DataSurvival1$Type<-rep("Chosen male",nrRows)
  DataSurvival1$SurvivalProb<-Data$survivalProbTrue
  DataSurvival1$Time<-CurrentTime
  DataSurvival2<- data.frame(matrix("", ncol = 1, nrow = nrRows))  
  DataSurvival2$Type<-rep("Random male",nrRows)
  DataSurvival2$SurvivalProb<-Data$survivalProbRando
  DataSurvival2$Time<-CurrentTime
  DataSurvivalTime<-rbind(DataSurvival1,DataSurvival2)
  DataSurvival<-rbind(DataSurvival,DataSurvivalTime)
}

svg(filename="Fig3B.svg",width=6.30,height = 6.75)
par(mfrow=c(1,1))
pirateplot(formula = DataSurvival$SurvivalProb~DataSurvival$Type+Time, data = DataSurvival, 
           inf.method="ci", yaxt="n", xaxt="n",bean.f.o=1, avg.line.lwd = 2,avg.line.col = "black",
           pal=c("red","grey"), point.col = "black", point.o = 0.05, point.cex=0.1,width.min=0.0, width.max=0.3,
           ylab="Offspring survival probability", xlab="Generation",
           main=" ",gl.lty = 0)
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8),cex.axis=1)
axis(side = 1,cex.axis=1,at=c(1.5, 4.5, 7.5, 11), labels = c("500","1000","2000","10000"))
text(7.8, 0.7, substitute(paste(bold("Chosen male"))), 
     col = "red", cex = 1.1, pos=4)
text(7.8, 0.65, substitute(paste(bold("Random male"))), 
     col = "grey", cex = 1.1, pos=4)
dev.off()


###########Making Figure 4#############

##Here, set the directory containing the data
##average data (plot top row)
Data1<-read.csv(paste("Avg_Baseline.csv",sep = ""))
Data2<-read.csv(paste("Avg_ShortSeason.csv",sep = ""))
Data3<-read.csv(paste("Avg_NarrowRes.csv",sep = ""))

##individual data (plot bottom row)
DataFull<-read.csv(paste("Individual_Baseline.csv",sep = ""))
DataM_1 <-subset(DataFull,DataFull$Sex==0)
DataM_1 <-subset(DataM_1, DataM_1$Time==10000)

DataFull<-read.csv(paste("Individual_ShortSeason.csv",sep = ""))
DataM_2 <-subset(DataFull,DataFull$Sex==0)

DataFull<-read.csv(paste("Individual_NarrowRes.csv",sep = ""))
DataM_3 <-subset(DataFull,DataFull$Sex==0)


##make the plots
svg(filename="Fig4.svg",width=9,height = 6.75)
par(mfrow=c(2,3))
plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(A) Baseline scenario", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(Data1,Data1$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}
text(50000, 0.97, substitute(paste(bold("Female preference"))), 
     col = "red", cex = 1.1, pos=2)
text(50000, 0.57, substitute(paste(bold("Male investment 
in ornamentation"))), col = "orange", cex = 1.1, pos=2)
text(50000, 0.3, substitute(paste(bold("Paternal care"))), 
     col = "turquoise", cex = 1.1, pos=2)


plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(B) Short mating seasons", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(Data2,Data2$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(C) Low resource variation", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(Data3,Data3$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,ylim=c(0,1.5),xlim=c(0,1.5),ylab="Individual values", xlab= "Ornament size T",main=" ")
points(DataM_1$Res~DataM_1$t,pch=20,col="dark green")
points(DataM_1$gamma~DataM_1$t,pch=20,col="turquoise")
text(0.35, 1.1, substitute(paste(bold("Resource 
     level"))), col = "dark green", cex = 1.1, pos=4)
text(1.0, 0.55, substitute(paste(bold("Paternal 
     care"))), col = "turquoise", cex = 1.1, pos=4)

plot(NULL,ylim=c(0,1.5),xlim=c(0,1.5),ylab="Individual values", xlab= "Ornament size T",main=" ")
points(DataM_2$Res~DataM_2$t,pch=20,col="dark green")
points(DataM_2$gamma~DataM_2$t,pch=20,col="turquoise")

plot(NULL,ylim=c(0,1.5),xlim=c(0,1.5),ylab="Individual values", xlab= "Ornament size T",main=" ")
points(DataM_3$Res~DataM_3$t,pch=20,col="dark green")
points(DataM_3$gamma~DataM_3$t,pch=20,col="turquoise")
points(DataM_3$tau~DataM_3$t,pch=20,col="orange")

dev.off()


##########Making Figure 5################

##Here, set the directory containing the data
DataR<-read.csv(paste("PopDyn_Baseline_SingleRep.csv",sep = ""))
Data1<-read.csv(paste("PopDyn_Baseline.csv",sep = ""))
Data2<-read.csv(paste("PopDyn_ShortSeason.csv",sep = ""))
Data3<-read.csv(paste("PopDyn_NarrowRes.csv",sep = ""))

svg(filename="Fig5.svg",width=9,height = 6.75)
nf <- layout( matrix(c(0,0,0,1,1,1,1,1,1,0,0,0,2,2,2,2,3,3,3,3,4,4,4,4), nrow=2, byrow=TRUE) )

plot((DataR$PopSize/1000)~DataR$Time,type="l",lwd=2,
     xlim=c(0,1500),
     ylim=c(-0.1,1), yaxt="n",
     xlab="Generation",ylab="Average values")
lines(DataR$avg_tau~DataR$Time,col='orange',lwd=2)
lines(DataR$avg_p~DataR$Time,col='red',lwd=2)
lines(DataR$avg_gamma~DataR$Time,col='turquoise',lwd=2)
axis(side = 2, at = c(0,0.2,0.4,0.6,0.8,1))
axis(side = 4, at = c(0,0.2,0.4,0.6,0.8,1),
     labels = c("0","200","400","600","800","1000"))
text(1520, 0.83, "Female preference", col = "red", cex = 0.9, pos=2)
text(1520, 0.59, "Male investment 
        in ornamentation", col = "orange", cex = 0.9, pos=2)
text(1520, 0.4, "Paternal care", col = "turquoise", cex = 0.9, pos=2)
text(1520, 0.01, "Population size", col = "black", cex = 0.9, pos=2)
mtext(side = 4, line = 3, 'Population size',cex=0.65)

ExtinctionCounter=0
data_extinction_points<-data.frame()
plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),main="(A) Baseline scenario", 
     xlab="Generation",ylab="Average values")
for(i in 0:99){
  Data_rep<-subset(Data1,Data1$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
  MaxTime<-max(Data_rep$Time)
  if(MaxTime<50000){
    Data_rep_end<-subset(Data_rep,Data_rep$Time==MaxTime)
    data_extinction_points<-rbind(data_extinction_points,Data_rep_end)
    ExtinctionCounter=ExtinctionCounter+1
  }
}
points(data_extinction_points$avg_tau~data_extinction_points$Time,pch=4,cex=.5)
print(ExtinctionCounter)


ExtinctionCounter=0
data_extinction_points<-data.frame()
plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),main="(B) Short mating seasons", 
     xlab="Generation",ylab="Average values")
for(i in 0:99){
  Data_rep<-subset(Data2,Data2$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
  MaxTime<-max(Data_rep$Time)
  if(MaxTime<50000){
    Data_rep_end<-subset(Data_rep,Data_rep$Time==MaxTime)
    data_extinction_points<-rbind(data_extinction_points,Data_rep_end)
    ExtinctionCounter=ExtinctionCounter+1
  }
}
points(data_extinction_points$avg_tau~data_extinction_points$Time,pch=4,cex=.5)
print(ExtinctionCounter)


ExtinctionCounter=0
data_extinction_points<-data.frame()
plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2), main="(C) Low resource variation", 
     xlab="Generation",ylab="Average values")
for(i in 0:99){
  Data_rep<-subset(Data3,Data3$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
  MaxTime<-max(Data_rep$Time)
  if(MaxTime<50000){
    Data_rep_end<-subset(Data_rep,Data_rep$Time==MaxTime)
    data_extinction_points<-rbind(data_extinction_points,Data_rep_end)
    ExtinctionCounter=ExtinctionCounter+1
  }
}
if(ExtinctionCounter!=0){
  points(data_extinction_points$avg_tau~data_extinction_points$Time,pch=4,cex=.5)
}
print(ExtinctionCounter)
dev.off()


#######Making Figure 6###############

##Here, set the directory containing the data
data_b1<-read.csv(paste("SurvivalWithoutMaleCare02_Baseline.csv",sep = ""))
data_b2<-read.csv(paste("SurvivalWithoutMaleCare05_Baseline.csv",sep = ""))
data_R<-read.csv(paste("SurvivalWithoutMaleCare05_Baseline_SingleRep.csv",sep = ""))
d_uni<-read.csv(paste("Avg_Baseline.csv",sep = ""))

svg(filename="Fig6.svg",width=9,height = 6.75)
nf <- layout( matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow=2, byrow=TRUE) )

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.7),xlab="Generation",ylab="Average values", 
     main="(A)", cex.main=1.5)
for(i in 0:99){
  Data_rep<-subset(d_uni,d_uni$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}
text(50000, 1.3, substitute(paste(bold("Female preference"))), 
     col = "red", cex = 1.1, pos=2)
text(50000, 0.45, substitute(paste(bold("Male investment 
in ornamentation"))), col = "orange", cex = 1.1, pos=2)
text(50000, 0.05, substitute(paste(bold("Paternal care"))), 
     col = "turquoise", cex = 1.1, pos=2)

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.7),xlab="Generation",ylab="Average values", 
     main="(B)", cex.main=1.5)
for(i in 0:99){
  Data_rep<-subset(data_b1,data_b1$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.7),xlab="Generation",ylab="Average values", 
     main="(C)", cex.main=1.5)
for(i in 0:99){
  Data_rep<-subset(data_b2,data_b2$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(0,1.05),
     xlab="Generation",ylab="Average values",
     main="(D)", cex.main=1.5)
lines(data_R$avg_tau~data_R$Time,col='orange',lwd=2)
lines(data_R$avg_gamma~data_R$Time,col='turquoise',lwd=3)
text(10000, 0.15, substitute(paste(bold("Male 
investment in 
ornamentation"))), col = "orange", cex = 1.1, pos=4)
text(10000, 0.83, substitute(paste(bold("Paternal care"))), 
     col = "turquoise", cex = 1.1, pos=4)

plot(NULL,xlim=c(0,1),ylim=c(-1,3),xlab="Mean male ornamentation",ylab="Mean female preference",
     main="(E)", cex.main=1.5)
for(i in 0:99){
  Data_rep<-subset(data_b2,data_b2$Rep==i)
  lines(Data_rep$avg_p~Data_rep$avg_t,col=rgb(0,0,0,0.1))
}
t_start=0
p_start=0
points(p_start~t_start,col='red',pch=20,cex=1.5)

dev.off()


#############Making Figure S1######

##Here, set the directory containing the data
DataAvg<-read.csv(paste("Avg_Baseline.csv",sep = ""))
DataR<-read.csv(paste("Avg_Baseline_SingleRep.csv",sep = ""))

svg(filename="FigS1.svg",width=11,height = 5.65)
par(mfrow=c(1,2))
plot(NULL,xlim=c(0,50000),ylim=c(0,1),xlab="Generation",ylab="Average values", 
     main="(A)", cex.main=1.5)
lines(DataR$avg_survivalprob_true~DataR$Time,col=rgb(0,0.8,0.5,1),lwd=2)
lines(DataR$avg_survivalprob_rando~DataR$Time,col=rgb(0.5,0.0,0.5,1),lwd=2)
text(45000, 0.8, "Survival - random father", col = rgb(0.5,0.0,0.5), cex = 1, pos=2)
text(45000, 0.9, "Survival - chosen father", col = rgb(0,0.8,0.5), cex = 1, pos=2)

plot(NULL,xlim=c(0,50000),ylim=c(0,1),xlab="Generation",ylab="Average values", 
     main="(B)", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(DataAvg,DataAvg$Rep==i)
  lines(Data_rep$avg_survivalprob_true~Data_rep$Time,col=rgb(0,0.8,0.5,0.1))
  lines(Data_rep$avg_survivalprob_rando~Data_rep$Time,col=rgb(0.5,0.0,0.5,0.1))
}
dev.off()


#########Making Figure S2########

##Here, set the directory containing the data
Data1<-read.csv(paste("Avg_Baseline.csv",sep = ""))
Data2<-read.csv(paste("Avg_MediumRes.csv",sep = ""))
Data3<-read.csv(paste("Avg_NarrowRes.csv",sep = ""))

svg(filename="FigS2.svg",width=9,height = 3.5)
par(mfrow=c(1,3))
plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(A) High resource variation", cex.main=1.5)
for(i in 0:99){
  Data_rep<-subset(Data1,Data1$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(B) Intermediate resource variation", cex.main=1.5)
NrSexSelRep=0
for(i in 0:99){
  Data_rep<-subset(Data2,Data2$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
  Data_rep_end<-subset(Data_rep,Data_rep$Time==max(Data_rep$Time))
  if(Data_rep_end$avg_p>0.5){NrSexSelRep=NrSexSelRep+1}
}
print(NrSexSelRep)

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(C) Low resource variation", cex.main=1.5)
for(i in 0:99){
  Data_rep<-subset(Data3,Data3$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}
dev.off()


#########Making Figure S3##########

##Here, set the directory containing the data
d1_d<-read.csv(paste("Diploid_Baseline.csv",sep = ""))
d2_d<-read.csv(paste("Diploid_ShortSeason.csv",sep = ""))
d3_d<-read.csv(paste("Diploid_NarrowRes.csv",sep = ""))
d1_h<-read.csv(paste("Avg_Baseline.csv",sep = ""))
d2_h<-read.csv(paste("Avg_ShortSeason.csv",sep = ""))
d3_h<-read.csv(paste("Avg_NarrowRes.csv",sep = ""))

svg(filename="FigS3.svg",width=9.05,height = 6.75)
par(mfrow=c(2,3))

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(A1) Baseline scenario", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(d1_h,d1_h$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}
text(50000, 0.97, substitute(paste(bold("Female preference"))), 
     col = "red", cex = 1.1, pos=2)
text(50000, 0.57, substitute(paste(bold("Male investment 
in ornamentation"))), col = "orange", cex = 1.1, pos=2)
text(50000, 0.3, substitute(paste(bold("Paternal care"))), 
     col = "turquoise", cex = 1.1, pos=2)

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(B1) Short mating seasons", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(d2_h,d2_h$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(C1) Low resource variation", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(d3_h,d3_h$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(A2) Baseline scenario", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(d1_d,d1_d$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(B2) Short mating seasons", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(d2_d,d2_d$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

plot(NULL,xlim=c(0,50000),ylim=c(-0.5,1.2),xlab="Generation",ylab="Average values", 
     main="(C2) Low resource variation", cex.main=1.5)
for(i in 0:100){
  Data_rep<-subset(d3_d,d3_d$Rep==i)
  lines(Data_rep$avg_tau~Data_rep$Time,col=rgb(1,0.65,0,0.1))
  lines(Data_rep$avg_p~Data_rep$Time,col=rgb(1,0,0,0.1))
  lines(Data_rep$avg_gamma~Data_rep$Time,col=rgb(0.188,0.835,0.784,0.1))
}

dev.off()

