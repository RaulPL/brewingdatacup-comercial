rm(list=ls())
library(readxl)
library(tidytext)
library(stringr)
library(tidyr)
library("ldatuning")

setwd('/Users/pepe_opi/Documents/GIT/brewingdatacup-comercial/data')

data<-read_xlsx("DemandForecast_Challenge (1).xlsx", sheet = 2)

sub<-read_xlsx("DemandForecast_Challenge (1).xlsx", sheet = 3)




data<-data[data$Hectolitros>0,]
data[data$SKU=="00000003",]$Hectolitros
ceros<-unlist(lapply(split(data, data$SKU), function(x) sum(x$Hectolitros)==0))
names(ceros)[ceros==T]

seg<-aggregate(data=data, Hectolitros~Subagencia+SKU, sum)

merge(data, seg, by='SKU', all = T)

#seg$Hectolitros<-round(seg$Hectolitros)
seg<-do.call(rbind, split(seg, seg$Subagencia, function(x) (x-mean(x)/sd(x))+100))
summary(round(seg$Hectolitros))
seg$Hectolitros<-round(seg$Hectolitros)

dtm<-cast_dtm(data = seg,document = 'Subagencia', term = 'SKU',value = Hectolitros)


#result <- FindTopicsNumber(
#  dtm,
#  topics = seq(from = 2, to = 5, by = 1),
#  metrics = c("Griffiths2004", "CaoJuan2009"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  mc.cores = 3L,
#  verbose = TRUE
#)
#

library(topicmodels)
library(ineq)
lda <- LDA(dtm, k = 10, control = list(seed = 12345))


skus <- tidy(lda)


library(ggplot2)
ggplot(data=skus, aes(x=factor(topic), y=term))+
  geom_tile(data=skus, aes(x=factor(topic), y=term, fill=beta))

sum_seg<-do.call(rbind, lapply(split(skus, skus$topic),
       function(x) data.frame(unique(x$topic),  ineq(x$beta, type = 'Gini'))))


maximos<-sum_seg[sum_seg$ineq.x.beta..type....Gini..>=0.94,][, 1]



tiendas <- tidy(lda, matrix = "gamma")

tiendas<-tiendas[tiendas$topic %in% maximos,]

ggplot(data=skus[skus$topic %in% maximos,], aes(x=factor(topic), y=term))+
  geom_tile(data=skus[skus$topic %in% maximos,], aes(x=factor(topic), y=term, fill=beta))


tiendas<-do.call(rbind, lapply(split(tiendas, tiendas$document), function(x) x[x$gamma==max(x$gamma),]))

check<-merge(tiendas, sub, by.x='document', by.y='Subagencia')

plot(check$Longitud, check$Latitud, col=factor(check$topic))
data<-merge(tiendas, data, by.x='document', by.y = 'Subagencia')

check_sku<-aggregate(data=data, Hectolitros~SKU+Mes+topic,mean)

ggplot(data=check_sku, aes(x=Mes,y=Hectolitros))+
  geom_line(aes(x=Mes,y=Hectolitros, group=SKU, color=factor(SKU)))+
  facet_wrap(~topic)

table(tiendas$topic)
table(data$topic)


library(ggmap)
library(ggrepel)
mex<-get_map('Mexico', zoom=3)

ggmap(mex)+
  geom_point(data=check[check$topic==1,], aes(x=Longitud,y=Latitud, size=gamma, color=factor(topic)), alpha=0.3)+
  geom_text_repel(data=check[check$topic==1,], aes(x=Longitud,y=Latitud, label=Desc_Subagencia))


write.csv(check, 'cat_cluster.csv', row.names = F)





















