#     Author: Jesper R.
#     date:   25/03-25
######################################

# Read in data

setwd("~/workdesk/bmb546 evo projekt")

x<- read.csv("Howard.csv",header=T,sep="\t")
y<- read.csv("Ritland_eyes_raw_FINAL.csv",header=T)



# Getting all the active names
names<-unique(x$Group)


# Data untangling of 'x' dataset
DataPull<-function(dyr,navn,indsæt)
{
  result<-c(0,0)
  h<-1
  
  for (i in 1:333)
  {
    if (x[i,dyr]==navn)
    { 
      result[h]<-x[i,indsæt]
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
  if (y[i,40]=="Aquatic")
  { 
    aquatic_eye[h]<-y[i,38]
    h<-h+1
  }
}


### fjern NA'er HER
aquatic_weight<-c(0,0)
h<-1

for (i in 1:4782)
{
  if (y[i,40]=="Aquatic")
  { 
    aquatic_weight[h]<-y[i,34]
    h<-h+1
  }
}



#######################################################################
#                         Data visolaztion                            #
#######################################################################




par(mfrow=c(3,3))
plot(amph_weight,amph_eye)
plot(bird_weight,bird_eye)
plot(fish_weight,fish_eye)
plot(mammal_weight,mammal_eye)
plot(primate_weight,primate_eye)
plot(reptile_weight,reptile_eye)
plot(rodent_weight,rodent_eye)
par(mfrow=c(1,1))