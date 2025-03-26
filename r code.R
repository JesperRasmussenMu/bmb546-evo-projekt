#     Author: Jesper R.
#     date:   25/03-25
######################################

# Read in data

setwd("~/workdesk/bmb546 evo projekt")

x_stat<- read.csv("Howard.csv",header=T,sep="\t")


# Getting all the active names
names<-unique(x_stat$Group)


# Data untangling of 'x_stat' dataset
DataPull<-function(dyr,navn,indsæt)
{
  result<-c(0,0)
  h<-1
  
  for (i in 1:333)
  {
    if (x_stat[i,dyr]==navn)
    { 
      result[h]<-x_stat[i,indsæt]
      h<-h+1
    }
  }
  return(result)
}


bird_eye<-DataPull(3,names[2],5)
bird_weight<-DataPull(3,names[2],4)


fish_eye<-DataPull(3,names[3],5)
fish_weight<-DataPull(3,names[3],4)


mammal_eye<-DataPull(3,names[4],5)
mammal_weight<-DataPull(3,names[4],4)


primate_eye<-DataPull(3,names[5],5)
primate_weight<-DataPull(3,names[5],4)


reptile_eye<-DataPull(3,names[6],5)
reptile_weight<-DataPull(3,names[6],4)


rodent_eye<-DataPull(3,names[7],5)
rodent_weight<-DataPull(3,names[7],4)



#######################################################################
#                         Data visolaztion                            #
#######################################################################
# Y value
bird_div<-bird_eye/bird_weight
fish_div<-fish_eye/fish_weight
mammal_div<-mammal_eye/mammal_weight
primate_div<-primate_eye/primate_weight
reptile_div<-reptile_eye/reptile_weight
rodent_div<-rodent_eye/rodent_weight

# Line 
bird_line<-lm(bird_div~bird_eye)
fish_line<-lm(fish_div~fish_eye)
mammal_line<-lm(mammal_div~mammal_eye)
primate_line<-lm(primate_div~primate_eye)
reptile_line<-lm(reptile_div~reptile_eye)
rodent_line<-lm(rodent_div~rodent_eye)

# making R values
bird_r<-round(cor(bird_eye,bird_div),2)
fish_r<-round(cor(fish_eye,fish_div),2)
mammal_r<-round(cor(mammal_eye,mammal_div),2)
primate_r<-round(cor(primate_eye,primate_div),2)
reptile_r<-round(cor(reptile_eye,reptile_div),2)
rodent_r<-round(cor(rodent_eye,rodent_div),2)

bird_rf<-paste("R²=",bird_r)
fish_rf<-paste("R²=",fish_r)
mammal_rf<-paste("R²=",mammal_r)
primate_rf<-paste("R²=",primate_r)
reptile_rf<-paste("R²=",reptile_r)
rodent_rf<-paste("R²=",rodent_r)


# x and y limitis
x<-c(0,107)
y<-c(0,1108)

# plot
par(mfrow=c(2,3))
plot(bird_eye,bird_div,xlim=x,ylim=y,main="Birds",ylab="Eye axis/weight",xlab="Eye axis",col="#37642A")
abline(bird_line,lty=2,lwd=2,col="#8333a2")
legend('topright', legend =bird_rf , bty = 'n')

plot(fish_eye,fish_div,xlim=x,ylim=y,ylab="Eye axis/weight",xlab="Eye axis",main="Eye axis vs. (eye axis)/(body weight)\n Fish",col="#FDE666",lwd=2)
abline(fish_line,lty=2,lwd=2,col="#8333a2")
legend('topright', legend =fish_rf , bty = 'n')

plot(mammal_eye,mammal_div,xlim=x,ylim=y,main="Mammal",ylab="Eye axis/weight",xlab="Eye axis",col="#FFCB05")
abline(fish_line,lty=2,lwd=2,col="#8333a2")
legend('topright', legend =mammal_rf , bty = 'n')

plot(primate_eye,primate_div,xlim=x,ylim=y,main="Primate",ylab="Eye axis/weight",xlab="Eye axis",pch = "❤",col="#DE656B")
abline(primate_line,lty=2,lwd=2,col="#8333a2")
legend('topright', legend =primate_rf , bty = 'n')

plot(reptile_eye,reptile_div,xlim=x,ylim=y,main="Reptile",ylab="Eye axis/weight",xlab="Eye axis",col="#C83B55",lwd=2)
abline(reptile_line,lty=2,lwd=2,col="#8333a2")
legend('topright', legend =reptile_rf , bty = 'n')

plot(rodent_eye,rodent_div,xlim=x,ylim=y,main="Rodent",ylab="Eye axis/weight",xlab="Eye axis",col="#171411")
abline(rodent_line,lty=2,lwd=2,col="#8333a2")
legend('topright', legend =rodent_rf , bty = 'n')
par(mfrow=c(1,1))