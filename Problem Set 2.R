#This function finds the first digit of the input
firstdig=function(x){
z<-c()
  for (i in 1:length(x)){
   z<-as.numeric(c(z,substr(x[i],1,1)))
  }
return(z)
}

#This function counts the number of times each digit appears first
countint<-function(x){    
  countvec<-c(0,0,0,0,0,0,0,0,0)
  for (i in 1:length(x)){
    countvec[(x[i])]=countvec[(x[i])]+1
  }
  return(countvec)
}
#This function calculates the Leemis m stat
leemis=function(x){
  hold<-countint(firstdig(x))
  max(hold)/length(x)-(log10(1+1/length(x)))
  
}
#This function calculates the Chogain d stat
 chogain= function(x){
  fred<-countint(firstdig(x))
  daphne=c(0)
  for(i in 1:9){
   daphne=c(daphne,(fred[i]/length(x)-(log10(1+1/length(x)))))
  }
   return(sqrt(sum(daphne^2)))
 }
 #This function calculates whichever stat you want
 statcalc<- function(input,which){
   print(countint(firstdig(input)))
   if (which=="chogain"){
     return(chogain(input))}
   if (which=="leemis"){
     return(leemis(input))}
   if (which=="both"){
     return(c(leemis(input),chogain(input)))}
   else(print("add either leemis, chogain, or both as your second arguement (as a string)"))
    
   
    }
   
 #test
 vector<-sample(1:1000,size=1000,replace=TRUE)
 max(countint(firstdig(vector)))
 statcalc(vector,"leemis")
 statcalc(vector,"chogain")
 statcalc(vector,"both")
 
 
 
 #PROBLEM 2
 
 
 print.benfords<- function(x){
   Statistic<-c("Leemis","Cho-Gain")
   Leem<-(leemis(x))
   CG<-(chogain(x))
   if (leemis(x)>.867 & leemis(x)<.967){
     Leem<-paste(Leem,"*")
   }
   if (leemis(x)>.967 & leemis(x)<1.212){
     Leem<-paste(Leem,"**")
   }
   if (leemis(x)>1.212){
     Leem<-paste(Leem,"***")
   }
   if(chogain(x)>1.212 & chogain(x)<1.330){
     CG<-paste(CG,"*")
   }
   if(chogain(x)>1.330 & chogain(x)<1.569){
     CG<-paste(CG,"**")
   }
   if(chogain(x)>1.569){
     CG<-paste(CG,"***")
   }
   Value<-c(CG,Leem)
   dat<-data.frame(Statistic,Value)
   print(dat)
   print("* <- significant at the 10% level
          ** <- significant at the 5% level
         *** <- significant at the 1% level")
   return(dat)
   }

 sink("benfords.csv")
 print.benfords(vector) 
sink()

