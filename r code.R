#     Author: Jesper R.
#     date:   25/03-25
######################################

# Read in data

setwd("~/workdesk/bmb546 evo projekt")

x_stat<- read.csv("Howard.csv",header=T,sep="\t")
y_stat<- read.csv("Ritland_eyes_raw_FINAL.csv",header=T)



# Getting all the active names
names<-unique(x$Group)


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

amph_eye<-DataPull(3,names[1],5)
amph_weight<-DataPull(3,names[1],4)


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



# Data untangling of 'y' dataset
aquatic_eye<-c(0,0)
h<-1

for (i in 1:4782)
{
  if (y_stat[i,40]=="Aquatic")
  { 
    aquatic_eye[h]<-y_stat[i,38]
    h<-h+1
  }
}


### fjern NA'er HER
aquatic_weight<-c(0,0)
h<-1

for (i in 1:4782)
{
  if (y_stat[i,40]=="Aquatic")
  { 
    aquatic_weight[h]<-y_stat[i,34]
    h<-h+1
  }
}



#######################################################################
#                         Data visolaztion                            #
#######################################################################
# Y value
amph_div<-amph_eye/amph_weight
bird_div<-bird_eye/bird_weight
fish_div<-fish_eye/fish_weight
mammal_div<-mammal_eye/mammal_weight
primate_div<-primate_eye/primate_weight
reptile_div<-reptile_eye/reptile_weight
rodent_div<-rodent_eye/rodent_weight

# Line - not yet used - want to make R values
amph_r<-lm(amph_div~amph_eye)
bird_r<-lm(bird_div~bird_eye)
fish_r<-lm(fish_div~fish_eye)
mammal_r<-lm(mammal_div~mammal_eye)
primate_r<-lm(primate_div~primate_eye)
reptile_r<-lm(reptile_div~reptile_eye)
rodent_r<-lm(rodent_div~rodent_eye)

# x and y limitis
x<-c(0,107)
y<-c(0,1108)

# plot
par(mfrow=c(2,3))
#plot(amph_eye,amph_div,xlim=x,ylim=y,main="Amphibians",col="#588E47",lwd=2)
#legend('topright', legend = "r=42", bty = 'n')
plot(bird_eye,bird_div,xlim=x,ylim=y,main="Birds",col="#37642A")
plot(fish_eye,fish_div,xlim=x,ylim=y,main="Eye axis vs. (eye axis)/(body weight)\n Fish",col="#FDE666",lwd=2)
plot(mammal_eye,mammal_div,xlim=x,ylim=y,main="Mammal",col="#FFCB05")
plot(primate_eye,primate_div,xlim=x,ylim=y,main="Primate",pch = "❤",col="#DE656B")
plot(reptile_eye,reptile_div,xlim=x,ylim=y,main="Reptile",col="#C83B55",lwd=2)
plot(rodent_eye,rodent_div,xlim=x,ylim=y,main="Rodent",col="#171411")
par(mfrow=c(1,1))