
 
 fun.ex <- function(lf.country,anno_iniziale,anno_finale){
   #yfin <- max(lf.country$Year)
   yfin = anno_finale
   indx<-c(anno_iniziale:yfin)
   nfin <- length(unique(lf.country$Year[lf.country$Year>=anno_iniziale]))
   if(nfin>anno_finale-anno_iniziale+1) nfin=anno_finale-anno_iniziale+1
   
   result=matrix(NA,1,nfin)
   for(i in 1:nfin){
     result[i]<-lf.country[lf.country$Year==indx[i],]$ex[1]
   }
   colnames(result)<- c(anno_iniziale:yfin)
   row.names(result)<-c(sprintf("%s", unique(lf.country$country)))
   return(result)
 }   #funzione che assegnata la life table di un paese produce la matrice degli ex(t) dal 1970
 

 rmse = function (truth, prediction)  {
    sqrt(mean((prediction - truth)^2))
 }
 mae = function(truth, prediction){
    mean(abs(prediction-truth))
 }
 
