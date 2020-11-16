library(EpiDynamics)
library(readxl)
library(rapportools)

dirP = "C:\\Users\\Pedro Vitor\\Documents\\ESTUDOS\\covid_19"
setwd(dirP)

t_inc = c(4.1,5.2,7) #fonte: https://www.worldometers.info/coronavirus/coronavirus-incubation-period/
death_rate = (2.8+1.7)/2 #fonte: https://www.worldometers.info/coronavirus/coronavirus-age-sex-demographics/

testes_dia = 30*10^3

#arqGeral = read.csv("arquivo_geral.csv",sep = ";")
ibge = read.csv("ibgeBR.csv")
arqGeral = as.matrix(read_excel("DT1_PAINEL_COVIDBR_20200512.xlsx",sheet = "Sheet 1"))

# estados = levels(arqGeral[,2])
# dias = levels(arqGeral[,3])

estados = arqGeral[-which(duplicated(arqGeral[,2])),2]
municipios = arqGeral[-which(duplicated(arqGeral[,3])),3]

ride = c( "Abadiânia","Água Fria de Goiás","Águas Lindas de Goiás",
          "Alexânia","Alto Paraíso de Goiás","Alvorada do Norte",
          "Barro Alto","Cabeceiras","Cavalcante","Cidade Ocidental",
          "Cocalzinho de Goiás","Corumbá de Goiás","Cristalina",
          "Flores de Goiás","Formosa","Goianésia","Luziânia","Mimoso de Goiás",
          "Niquelândia","Novo Gama","Padre Bernardo","Pirenópolis","Planaltina",
          "Santo Antônio do Descoberto","São João d'Aliança","Simolândia",
          "Valparaíso de Goiás","Vila Boa","Vila Propício","Arinos","Buritis","Cabeceira Grande",
          "Unaí")

ride=intersect(ride,municipios)
arqRide=NULL

for(i in 1:length(ride))
{
  arqRide=rbind(arqRide,arqGeral[which(arqGeral[,3]==ride[i]),])
}

regioes = arqRide[-which(duplicated(arqRide[,7])),7]
tDupli=NULL

for(i in 1:length(regioes))
{
  aux=arqRide[which(arqRide[,7]==regioes[i]),]
  municipiosR = aux[-which(duplicated(aux[,3])),3]
  diasR=aux[-which(duplicated(aux[,8])),8]
  if(sum(duplicated(aux[,8]))==0){diasR=aux[,8]}
  casosR=mortesR=rep(0,length(diasR))
  suscR = sum(as.numeric(aux[-which(duplicated(aux[,10])),10]))
  tabR = cbind(diasR,casosR,mortesR,rep(suscR,length(diasR)))
  
  for(ii in 1:length(municipiosR))
  {
    aux2=aux[which(aux[,3]==municipiosR[ii]),]
    if(is.empty(nrow(aux2))==F)
    {
      for(k in 1:nrow(aux2))
      {
        tabR[which(tabR[,1]==aux2[k,8]),2] = as.numeric(tabR[which(tabR[,1]==aux2[k,8]),2])+as.numeric(aux2[k,11])
        tabR[which(tabR[,1]==aux2[k,8]),3] = as.numeric(tabR[which(tabR[,1]==aux2[k,8]),3])+as.numeric(aux2[k,12])
      }
    }
  }
  
  data_plot = seq.Date(as.Date(tabR[1,1]), by = "day",length.out = nrow(tabR))
  plot(data_plot,log(as.numeric(tabR[,2])),main = regioes[i])
  xR=(1:nrow(tabR))
  if(sum(as.numeric(tabR[,2])==0)!=0)
  {
    coef=lm(log(as.numeric(tabR[,2])[-which(as.numeric(tabR[,2])==0)])~xR[-which(as.numeric(tabR[,2])==0)])
  }
  else{coef=lm(log(as.numeric(tabR[,2]))~xR)}
  yR=coef[[1]][1]+coef[[1]][2]*(1:nrow(tabR))
  lines(data_plot,yR)
  tDupli = c(tDupli,log(2)/coef[[1]][2])
}

panorama = cbind(regioes,tDupli)
rownames(panorama)=1:nrow(panorama)

for(i in 1:length(ride))
{
  aux=arqRide[which(arqRide[,3]==ride[i]),]
  if(is.empty(nrow(aux))==F)
  {
    plot(as.numeric(aux[,11]),main = ride[i])
    t=NULL
    t=c(t,min(which(as.numeric(aux[,11])>=as.numeric(aux[,11])[1]*2)))
    while (as.numeric(aux[,11])[t[length(t)]]*2<max(as.numeric(aux[,11]))) {
      t=c(t,min(which(as.numeric(aux[,11])>=as.numeric(aux[,11])[t[length(t)]]*2)))
    }
    t_dupli=mean(t)
    
  }
}


####################################################################################

casosBR=NULL
mortesBR=NULL
for(i in 1:length(dias))
{
  casosBR[i] = sum(arqGeral[which(arqGeral[,3]==dias[i]),5])
  mortesBR[i] = sum(arqGeral[which(arqGeral[,3]==dias[i]),7])
}

aa = which(casosBR==0)
casosBR = casosBR[-aa]
mortesBR = mortesBR[-aa]
diass = dias[-aa]

suscBR = sum(ibge[1:27,7])

plot(casosBR)
plot(mortesBR)
plot(diff(casosBR))
plot(diff(mortesBR))

###############################################################################################
recuperadosBR = NULL
for(i in 22:length(casosBR))
{
  recuperadosBR[i] = casosBR[(i-21)]
}

recuperadosBR[1:21] = 0
recuBR = recuperadosBR+mortesBR

plot(recuBR)

for(i in 2:length(casosBR))
{
  suscBR[i] = suscBR[i-1]-casosBR[i-1]-recuBR[i-1]
}

# plot(casos,col="white")
# lines(casos,col="orange")
# plot(mortes,col="white")
# lines(mortes,col="red")

beta = -c(0,diff(suscBR))/(casosBR*suscBR)

gama = matrix(nrow = length(beta), ncol = length(beta))
for(i in 1:length(beta))
{
  gama[i,] = beta[i]*suscBR-c(0,diff(casosBR))/casosBR
}

beta1=mean(beta)
gama=mean(gama)

parameters <- c(mu = 0, beta = 5*beta1/3,
                sigma = .23, gamma = gama)
initials <- c(S = sum(ibge[1:27,7]), E = 16, I = 1, R = 0)
# Solve and plot.
seir <- SEIR(pars = parameters, init = initials, time = 0:200)
PlotMods(seir)

results = seir$results
results[74,]


####################################################################################

casosBR
mortesBR
suscBR
recuBR


####################################################################################
#SIR
acomp = NULL

for(cont in 20:length(casosBR))
{
  casoss = casosBR[1:cont]
  
  compr = length(casoss)
  
  gama = matrix(nrow = cont, ncol = cont)
  
  for(i in 1:cont)
  {
    gama[i,] = beta[i]*suscBR[1:cont]-c(0,diff(casosBR[1:cont]))/casosBR[1:cont]
  }
  
  mean(beta)
  quantile(beta, probs = .5)
  
  gama = apply(gama, 1, mean)
  gama = mean(gama)
  
  
  #quantile(gama, probs = .5)
  # plot(gama[1:cont],beta[1:cont])
  # #points(gama[30:35],beta[30:35],col = "blue",pch=16)
  # points(quantile(gama, probs = .5),quantile(beta, probs = .5),col="red",pch=16)
  # points(mean(gama),mean(beta),col="green",pch=16)
  # 
  vuln = matrix(nrow = 200, ncol = compr)
  infec = matrix(nrow = 200, ncol = compr)
  imune = matrix(nrow = 200, ncol = compr)
  infec_new = matrix(nrow = 200, ncol = compr)
  vuln[1:compr,] = suscBR[1:cont]
  infec[1:compr,] = casoss
  imune[1:compr,] = recuBR[1:cont]
  infec_new[1:compr,] = c(0,diff(casoss))
  
  for(i in (compr+1):200)
  {
    for(j in 2:compr)
    {
      infectados = beta[j]*vuln[(i-1),j]*infec[(i-1),j]
      rec = gama[j]*infec[(i-1),j]
      infec_new[i,j] = infectados
      infec[i,j] = infec[(i-1),j]+infectados-rec
      imune[i,j] = imune[(i-1),j]+rec
      vuln[i,j] = vuln[(i-1),j]-infectados-rec
      
      if(is.nan(infec[i,j])||is.na(infec[i,j])||is.infinite(infec[i,j]))
      {
        infec[i,j]=0
      }
    }
  }
  
  vuln = vuln[,-1]
  infec = infec[,-1]
  imune = imune[,-1]
  infec_new = infec_new[,-1]
  
  casos0 = apply(infec,1,mean)
  casos1 = apply(infec,1,median)
  casos2 = apply(infec,1,quantile)
  casos2 = casos2[1,]
  #casos_new = apply(infec_new,1,quantile)
  #casos_new = casos_new[1,]
  recu0 = apply(imune,1,mean)
  susc0 = apply(vuln,1,mean)
  dataplot = seq.Date(as.Date(diass[1]), by = "day",length.out = length(casos0))
  cc = cbind(as.character(dataplot),round(casos0),round(casos1),round(casos2))
  
  # pdf(file = "covid19.pdf")
  # plot(dataplot,casos0/1000, col = "white", ylab = "Casos (em milhares)", xlab = "Tempo", main = "Evolução dos casos de CoViD-19")
  # lines(dataplot,casos0/1000, col = "red")
  # lines(dataplot,casos1/1000, col = "gold")
  # lines(dataplot,casos2/1000, col = "green")
  # lines(dataplot,c(0,diff(recu0/1000)), col = "blue")
  # legend("topleft",legend = c("Média","Mediana","Limite inferior","recu0perados e mortos"), col = c("red","gold","green","blue"), lty = "solid")
  # grid()
  # 
  # plot(dataplot,susc0/1000,col = "white", ylab = "susc0etíveis (em milhares)", xlab = "Tempo", main = "Evolução dos casos de CoViD-19")
  # lines(dataplot,susc0/1000, col = "green")
  # dev.off()
  # 
  a = cbind(as.character(dataplot),c(0,round(diff(casos0),digits = 2)),round(casos0,digits = 2),round((0.07*casos0),digits = 2),round(recu0,digits = 2))
  a = cbind(a,c(0,diff(as.numeric(a[,ncol(a)]))))
  a = a[,-(ncol(a)-1)]
  
  colnames(a) = c("Data","Casos novos","Casos totais", "Casos graves","recu0perados")
  a[which(a[,3]==max(as.numeric(a[,3]))),]
  
  aa = cbind(as.character(dataplot),c(0,round(diff(casos1),digits = 2)),round(casos1,digits = 2),round((0.07*casos1),digits = 2),round(recu0,digits = 2))
  aa = cbind(aa,c(0,diff(as.numeric(aa[,ncol(aa)]))))
  aa = aa[,-(ncol(aa)-1)]
  
  colnames(aa) = c("Data","Casos novos","Casos totais", "Casos graves","recu0perados")
  aa[which(aa[,3]==max(as.numeric(aa[,3]))),]
  
  # bb = cbind(as.character(dataplot),round(casos_new),round(casos2,digits = 2),round((0.07*casos2),digits = 2),round(recu0,digits = 2))
  # bb = cbind(bb,c(0,diff(as.numeric(bb[,ncol(bb)]))))
  # bb = bb[,-(ncol(bb)-1)]
  # 
  # colnames(bb) = c("Data","Casos novos","Casos totais", "Casos graves","recu0perados")
  # bb[which(bb[,3]==max(as.numeric(bb[,3]))),]
  # 
  acomp = cbind(acomp,round(casos2))
}

acomp = cbind(as.character(dataplot),acomp)

plot(dataplot,as.numeric(acomp[,2]),col = "white", ylim = c(0,max(as.numeric(acomp)[-which(is.na(as.numeric(acomp)))])),
     ylab = "Casos", xlab = "Tempo", main = paste("Predição da evolução dos casos de CoViD-19 - ","Brasil",sep = ""))
for(i in 2:ncol(acomp))
{
  lines(dataplot,as.numeric(acomp[,i]))
}
lines(dataplot,as.numeric(acomp[,ncol(acomp)]),col="green",lwd=3.2)
legend("topright",c(paste("Trajetória do dia: ",dias[length(dias)],sep = ""),"Demais trajetórias"), col = c("green","black"), pch = 16, cex = .5)
grid()
