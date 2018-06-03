rm(list=ls())

setwd('/Users/pepe_opi/Documents/GIT/brewingdatacup-comercial/data')
data<-read_xlsx("DemandForecast_Challenge (1).xlsx", sheet = 2)



data<-split(data, list(data$SKU, data$Subagencia))

data<-data[unlist(lapply(data, function(x) nrow(x)))>0]
data<-data[unlist(lapply(data, function(x) sum(x$Hectolitros)>0))>0]
control<-lapply(data, function(x) x[sample(nrow(x), 3),])

train<-data
for(i in 1:length(data)){
  train[[i]][as.data.frame(train[[i]])$Mes %in%  control[[i]]$Mes,'Hectolitros']<-NA
}
 
naive<-do.call(rbind,
        lapply(train, function(x) aggregate(data=x, Hectolitros~Subagencia+SKU, mean, rm=T)))

compara_naive<-do.call(rbind, control)

names(compara_naive)[names(compara_naive)=='Hectolitros']<-'Hectolitros_control'
#compara_naive$SKU<-as.numeric(compara_naive$SKU)

compara_naive<-merge(naive, compara_naive, by=c('Subagencia', 'SKU'))


bechmark_naive<-with(compara_naive[compara_naive$Hectolitros>0,],
     mean(abs(Hectolitros_control-Hectolitros)/Hectolitros))

data<-data[unlist(lapply(data, function(x) sum(x$Hectolitros)>0))]




library(prophet)
library(dplyr)
prueba_1<-lapply(data, function(x) x[as.numeric(gsub('-', '', x$Mes))<=20171101,])



data[[1]]

future<-NULL
forecast<-list()
compara<-list()
m<-list()
m$changepoints
dif<-data.frame()
#prueba_1<-prueba_1[sample(length(prueba_1), 500)]
for(i in 1:length(prueba_1)){
inicio<-Sys.time()
  print(i)
names(prueba_1[[i]])[names(prueba_1[[i]]) %in% c('Mes', 'Hectolitros')]<-c('ds', 'y')
m<-prophet(df = prueba_1[[i]][, c('ds', 'y')])
future <- make_future_dataframe(m[[i]], periods = 6, freq = 'month', include_history = F)
forecast[[i]] <- predict(m, future)
forecast[[i]]$yhat<-ifelse(forecast[[i]]$yhat<0, 0,  forecast[[i]]$yhat)
compara[[i]]<-merge(forecast[[i]][c('ds', 'yhat')], data[[i]], by.x='ds', by.y='Mes', all=T )
dif<-rbind(dif, abs(compara[[i]]$y-compara[[i]]$yhat))
print(inicio-Sys.time())
}

plot(compara[[1]]$ds, compara[[1]]$Hectolitros, type='l')
lines(compara[[200]]$ds, compara[[200]]$yhat, col='red')

ciega_dic<-sum(unlist(lapply(compara,
       function(x) with(x[x$ds=="2017-12-01 GMT",], abs(Hectolitros-yhat)))))
ciega_dic<-ciega_dic/sum(unlist(lapply(compara,
                  function(x) with(x[x$ds=="2017-12-01 GMT",], Hectolitros))))
ciega_dic
ciega_enero<-sum(unlist(lapply(compara,
                               function(x) with(x[x$ds=="2018-01-01 GMT",], abs(Hectolitros-yhat)))))
ciega_enero<-ciega_enero/sum(unlist(lapply(compara,
                                           function(x) with(x[x$ds=="2018-01-01 GMT",], Hectolitros))))

ciega_feb<-sum(unlist(lapply(compara,
                               function(x) with(x[x$ds=="2018-02-01 GMT",], abs(Hectolitros-yhat)))))
ciega_feb<-ciega_feb/sum(unlist(lapply(compara,
                                           function(x) with(x[x$ds=="2018-02-01 GMT",], Hectolitros))))

compara_cons<-do.call(rbind, compara)
write.csv(compara_cons, 'pred.csv', row.names = F)

error_promedio<-unlist(lapply(compara,
       function(x) with(x, sum(abs(Hectolitros-yhat), na.rm = T))))


mejor_pred<-compara[which(error_promedio==min(error_promedio[error_promedio>0], na.rm = T))]

plot(compara[[6]]$ds, compara[[6]]$Hectolitros,type='l',
     main='Subagencia 10003 \n SKU 00000036', xlab = 'Mes',
     ylab='Hectolitros', sub = 'Negro: Puntos observados, verde: predicción, gris:  IC')
points(compara[[6]]$ds, compara[[6]]$Hectolitros )
lines(compara[[6]]$ds, compara[[6]]$yhat,col='blue' )
lines(forecast[[6]]$ds, forecast[[6]]$yhat_lower,col='gray' )
lines(forecast[[6]]$ds, forecast[[6]]$yhat_upper,col='gray' )
compara[[1000]][compara[[1000]]$Hectolitros>0,]



compara_cons<-compara_cons[complete.cases(compara_cons),]



head(compara_cons)
tiedas_peor_predichas<-read.csv('pred.csv')
tiedas_peor_predichas$dif<-abs(tiedas_peor_predichas$Hectolitros-tiedas_peor_predichas$yhat)
tiedas_peor_predichas<-aggregate(data=tiedas_peor_predichas, cbind(dif, Hectolitros)~Subagencia, sum)

tiedas_peor_predichas$error<-tiedas_peor_predichas$dif/tiedas_peor_predichas$Hectolitros
clusters<-read.csv('catalogo_cluster.csv')

dnue<-read.csv('conteo_categorias.csv')




tiedas_peor_predichas<-merge(clusters, tiedas_peor_predichas, by='Subagencia')
tiedas_peor_predichas<-merge(dnue, tiedas_peor_predichas, by='Subagencia')

tiedas_peor_predichas$conteo<-rowSums(tiedas_peor_predichas[, c('X431110', 'X431213', 'X461212', 'X462111', 'X462112', 'X7224', 'X722511')])

cor(tiedas_peor_predichas[, c('error', 'X431110', 'X431213', 'X461212', 'X462111', 'X462112', 'X7224', 'X722511')])[,1]
plot(tiedas_peor_predichas$error, tiedas_peor_predichas$conteo)
plot(tiedas_peor_predichas$error, tiedas_peor_predichas$X431110)
plot(tiedas_peor_predichas$error, tiedas_peor_predichas$X431213)
plot(tiedas_peor_predichas$error, tiedas_peor_predichas$X462112)
plot(tiedas_peor_predichas$error, tiedas_peor_predichas$conteo)
plot(tiedas_peor_predichas$error, tiedas_peor_predichas$conteo)


library(ggmap)
mex<-get_map('mexico', zoom = 4)
ggmap(mex)+
  geom_point(data=tiedas_peor_predichas,
             aes(x=Longitud, y=Latitud, size=error, color=factor(cluster)), alpha=0.5)+
  theme_void()+
  theme(legend.position  = 'none')


dnue_venta<-merge(aggregate(data=data, Hectolitros~Subagencia, mean), dnue)
dnue_venta[, -c(1, 2)]<-dnue_venta[, -c(1, 2)]/rowSums(dnue_venta[, -c(1, 2)])
cor(dnue_venta[, -1])

plot(dnue_venta$X462111, dnue_venta$Hectolitros)


dnue_venta_1<-aggregate(data=data, Hectolitros~Subagencia+SKU, mean)
dnue_venta_1<-aggregate(data=data, Hectolitros~Subagencia, sum)
dnue_venta_1<-merge(dnue_venta_1, dnue)
names(dnue_venta_1)
rbind(summary(lm(data=dnue_venta_1, Hectolitros~X431110))$coefficients[2, c('Estimate', 'Pr(>|t|)')],
summary(lm(data=dnue_venta_1, Hectolitros~X431213))$coefficients[2, c('Estimate', 'Pr(>|t|)')],
summary(lm(data=dnue_venta_1, Hectolitros~X461212))$coefficients[2, c('Estimate', 'Pr(>|t|)')],
summary(lm(data=dnue_venta_1, Hectolitros~X462111))$coefficients[2, c('Estimate', 'Pr(>|t|)')],
summary(lm(data=dnue_venta_1, Hectolitros~X7224))$coefficients[2, c('Estimate', 'Pr(>|t|)')],
summary(lm(data=dnue_venta_1, Hectolitros~X722511))$coefficients[2, c('Estimate', 'Pr(>|t|)')])

dnue_venta[, -c(1, 2)]<-dnue_venta[, -c(1, 2)]/rowSums(dnue_venta[, -c(1, 2)])
cor(dnue_venta[, -1])

plot(dnue_venta$X462111, dnue_venta$Hectolitros)

skus_mas_grandes<-aggregate(data=data, Hectolitros~SKU, sum)
skus_mas_grandes<-skus_mas_grandes[skus_mas_grandes$Hectolitros>quantile(skus_mas_grandes$Hectolitros, 0.95),'SKU']


cors_sku<-data[data$SKU %in% skus_mas_grandes,]

cors_sku<-dcast(cors_sku, Mes~SKU, fun.aggregate=mean)

cors_sku<-cor(cors_sku[, -1])
library(reshape2)
melted_cormat <- melt(cors_sku)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=factor(Var1), y=factor(Var2), fill=value)) + 
  geom_tile()

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}


#lower<-get_lower_tri(melted_cormat)
#melted_cormat <- melt(lower, na.rm = TRUE)

ggplot(data = melted_cormat, aes(factor(Var2), factor(Var1), fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

summary(melted_cormat)
  