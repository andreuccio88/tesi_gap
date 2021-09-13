
Data = ex.countries.f[,select3.f$fdnames$reps] # Seleziono il cluster su cui lavorare, in questo caso il n.3
paesi_cluster=select3.f$fdnames$reps


#anni su cui fittare il modello
n <- 34 
#selezionare righe in relazione alla finestra di forecasting da usare-in quest esempio arriviamo al 2003
D <- (Data[1:n,]) 

# Cointegrazione
H1.trace<- ca.jo(data.frame(D), type="trace",spec="longrun",dumvar = NULL,ecdet = "trend")
summary(H1.trace)

# VECM
var_form<-vec2var(H1.trace)
vecm.pred <- predict(var_form, n.ahead = 10, ci = 0.95, dumvar=H1.trace@dumvar)
plot(vecm.pred)

#Creo marice
predict_vecm= vecm.pred$fcst$CZE[,1]
predict_vecm= cbind(predict_vecm,vecm.pred$fcst$FIN[,1])
predict_vecm= cbind(predict_vecm,vecm.pred$fcst$FRACNP[,1])
predict_vecm= cbind(predict_vecm,vecm.pred$fcst$HUN[,1])
predict_vecm= cbind(predict_vecm,vecm.pred$fcst$POL[,1])
predict_vecm= cbind(predict_vecm,vecm.pred$fcst$PRT[,1])
predict_vecm= cbind(predict_vecm,vecm.pred$fcst$SVK[,1])

# Grandezze per ottenere errori di previsione nella finestra di forecasting year >2003 nel nostro esempio
nvar=ncol(Data)
L <- dim(Data)[1]
nvecm <- dim(predict_vecm)[2]

# dati out-of-sample
D_test <- (Data[(n+1):L,])

#Costruiamo le altre matrici degli errori e plottiamo le due proiezioni
performance_vecm_RMSE=array(0,nvecm)
performance_vecm_MAE=array(0,nvecm)
for (j in 1:nvecm){
  performance_vecm_RMSE[j] = rmse(predict_vecm[,j],D_test[,j])
  performance_vecm_MAE[j] = mae(predict_vecm[,j],D_test[,j])
}
# Errori medi nel cluster
mean(performance_vecm_RMSE)
mean(performance_vecm_MAE)



# Plot

#Inizializziamo le variabili
 y <- 1970:2013

for (j in 1:nvar){
  plot(y, Data[,j], type='l', col= 1, ylim = c(min(Data),max(Data)))
    lines(y,c(rep(NA,n),predict_vecm[,j]), col='blue')
  legend(y[1],max(Data-1.5), legend=c(colnames(Data)[j],"Previsione LSTM","Previsione VAR"),
         fill = c("black","red","blue"))
  abline(v=anno_iniziale+n)
}
