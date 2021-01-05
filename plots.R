library(ggplot2)
library(reshape2)
library(scales)
original<- read.csv(file="D:/Proiect_disertatie/set_date_1/data_originale/ph1.txt", header=FALSE, sep=",")
electrica_cpt <- read.csv(file="D:/Proiect_disertatie/set_date_2/electrica/cpt.txt", header=FALSE, sep=",")
predicted <- read.csv(file="D:/Proiect_disertatie/results/sqrt_750/pv1_sqrt_results_750.csv", header=FALSE, sep=",")

palette <- c("cornflowerblue", "red", "orange", "green", "coral1", "blue", "darkgreen", "blueviolet",
             "chartreuse", "darksalmon", "gold", "deeppink", "hotpink4", "tan3", "mistyrose", "aquamarine","springgreen")


cpt_2014 <- electrica_cpt$V1[1:336]
cpt_2015 <- electrica_cpt$V1[8761:9096]
cpt_2016 <- electrica_cpt$V1[18289:18624]
cpt_2017 <- electrica_cpt$V1[27793:28128]
cpt_2018 <- electrica_cpt$V1[37297:37632]
cpt_2019 <- electrica_cpt$V1[46801:47136]

df <- data.frame(x=c(1:336),cpt_2014=cpt_2014 ,cpt_2015=cpt_2015,cpt_2016=cpt_2016,cpt_2017=cpt_2017,cpt_2018=cpt_2018,cpt_2019=cpt_2019)
meltdf2 <- melt(data = df,id="x",measure.vars = c("cpt_2014","cpt_2015","cpt_2016","cpt_2017","cpt_2018","cpt_2019"))
ggplot(meltdf2,aes(x=x,y=value,colour=variable,group=variable)) + geom_line(size = 0.5)+ xlab("Observare primele doua săptămâni")+ ylab("Energia [Watts]") 

df <- data.frame(x=c(1:42727), original=original$V1[751:43477],predictionat=predicted$V1[1:42727])

meltdf3 <- melt(data = df,id="x",measure.vars = c("original","predictionat"))
ggplot(meltdf3,aes(x=x,y=value,colour=variable,group=variable), cex.lab = 1) + 
  geom_line(size = 0.75)+ xlab("Observare[istoria 750 sezonalitate 288]")+ ylab("Energia [Watts]") 


data <- structure(list(Media= c(34.426,211.07, 198.27, 74.018),
                       Ph1= c(42.39,279.99,215.51,96.48),
                       Ph2 = c(25.18,199.35,157.91,59.40), 
                       Ph3 = c(51.11,357.14,172.99,100.12),
                       Pv1 = c(27.29,111.71,225.91,57.79 ), 
                       Pv2 = c(26.16,107.16,219.01,56.30)), class = "data.frame", row.names = c(NA, -4L))

colours <- c("red", "yellow","green","blue")
par(bg="azure3")
#.Names = c("Ph1", "Ph2", "Ph3", "Pv1", "Pv2")
barplot(as.matrix(data), ylab = "MAE", xlab = "Graficul cu MAE pentru diferite tipuri de modele de predictie",cex.lab = 0.75, beside=TRUE, col=colours, ylim=range(at=seq(0,400,1)), xlim=range(1,50),axes = TRUE) 
legend("topright", 
       legend = c("Markov", "Retea neuronală","ARIMA","TBATS") ,cex=1,y.intersp=0.6,
       fill = colours)

ora1 <- original$V1[1:12]
ora2 <- original$V1[13:24]
ora3 <- original$V1[25:36]
ora4 <- original$V1[37:48]
ora5 <- original$V1[49:60]
ora6 <- original$V1[61:72]
ora7 <- original$V1[73:84]
ora8 <- original$V1[85:96]
ora9 <- original$V1[97:108]
ora10 <- original$V1[109:120]
ora11 <- original$V1[121:132]
ora12 <- original$V1[133:144]
ora13 <- original$V1[145:156]
ora14 <- original$V1[157:168]
ora15 <- original$V1[169:180]
ora15 <- original$V1[181:192]
ora16 <- original$V1[193:204]
ora17 <- original$V1[205:216]
ora18 <- original$V1[217:228]
ora19 <- original$V1[229:240]
ora20 <- original$V1[241:252]
ora21 <- original$V1[253:264]
ora22 <- original$V1[265:276]
ora23 <- original$V1[277:288]

df <- data.frame(x=c(1:12), ora_1=ora1,ora_2=ora2,ora_3=ora3,ora_4=ora4,ora_5=ora5,
                 ora_6=ora6,ora_7=ora7,ora_8=ora8,ora_9=ora9,ora_10=ora10,
                 ora_11=ora11,ora_12=ora12,ora_13=ora13,ora_14=ora14,ora_15=ora15,
                 ora_16=ora16,ora_17=ora17,ora_18=ora18,ora_19=ora19,ora_20=ora20,
                 ora_21=ora21,ora_22=ora22,ora_23=ora23)

meltdf1 <- melt(data = df,id="x",measure.vars = c("ora_1","ora_2","ora_3","ora_4","ora_5","ora_6","ora_7","ora_8","ora_9","ora_10","ora_11","ora_12","ora_13","ora_14","ora_15","ora_16","ora_17","ora_18","ora_19","ora_20","ora_21","ora_22","ora_23"))
ggplot(meltdf1,aes(x=x,y=value,colour=variable,group=variable), cex.lab = 1) + geom_line(size = 0.75)+ xlab("Observare 1 zi")+ ylab("Energia [Watts]") 

