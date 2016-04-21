#Adatok betöltése
betolto2014<-function(){
  require(plyr)
  egyeni<-read.csv("feltolt/egyeni_ag.csv", encoding="UTF-8")
  egyeni<<-aggregate(szavazat~megye+oevk+jelolt+szervezet, data=egyeni, FUN=sum)
  level<<-read.csv("feltolt/level.csv", encoding="UTF-8")
  listas<-read.csv("feltolt/listas_ag_1.csv", encoding="UTF-8")
  listas<<-aggregate(szavazat~megye+oevk+szervezet, data=listas, FUN=sum)
}

egyeni_eredmeny<-function(egyeni){
  require(plyr)
  egyeni_szavazatok<-aggregate(szavazat~megye+oevk+jelolt+szervezet, data=egyeni, FUN=sum)
  egyeni_gyoztesek<-ddply(egyeni_szavazatok,~megye+oevk,function(x){x[which.max(x$szavazat),]})
  egyeni_partok<-aggregate(szavazat~szervezet,data=egyeni_gyoztesek, FUN=length)
  colnames(egyeni_partok)<-c("szervezet","mandatum")
  return (egyeni_partok)
}


egyeni_toredek<-function(egyeni,elso=1){
  require(plyr)
  egyeni_szavazatok<-aggregate(szavazat~megye+oevk+jelolt+szervezet, data=egyeni, FUN=sum)
  egyeni_tobbiek<-ddply(egyeni_szavazatok,~megye+oevk,function(x){x[order(x$szavazat,decreasing=TRUE),][2:length(x),]})
  if (elso==1) 
  {egyeni_gyoztesek<-ddply(egyeni_szavazatok,~megye+oevk,function(x){x[which.max(x$szavazat),]})
   egyeni_masodikak<-ddply(egyeni_szavazatok,~megye+oevk,function(x){x[order(x$szavazat,decreasing=TRUE),][2,]})
   kozos<-merge(egyeni_gyoztesek,egyeni_masodikak,by=c("megye","oevk"))
   kozos_eredmeny<-cbind(kozos[,c("megye","oevk","szervezet.x")],(kozos[,"szavazat.x"]-kozos[,"szavazat.y"]-1))
   colnames(kozos_eredmeny)<-c("megye","oevk","szervezet","szavazat")
   egyeni_toredekek<-rbind(egyeni_tobbiek[,c("megye","oevk","szervezet","szavazat")],kozos_eredmeny)
   egyeni_toredek_eredmenyek<-aggregate(szavazat~szervezet,data=egyeni_toredekek, FUN=sum)
   return (egyeni_toredek_eredmenyek)}
  egyeni_toredek_eredmenyek<-aggregate(szavazat~szervezet,data=egyeni_tobbiek, FUN=sum)
  return (egyeni_toredek_eredmenyek)
}

listas_szavazatok<-function(listas,kuszob=0.02){
  require(plyr)
  lista_level<-rbind(listas[,c("szervezet","szavazat")],level[,c("szervezet","szavazat")])
  listas_szavazatok<-aggregate(szavazat~szervezet, data=lista_level, FUN=sum)
  listas_szavazatok$eredmeny<-listas_szavazatok$szavazat/sum(listas_szavazatok$szavazat)
  listas_szavazatok<-listas_szavazatok[listas_szavazatok$eredmeny>=kuszob,]
  return(listas_szavazatok[,c("szervezet","szavazat")])
}


listas_toredek_szavazatok<-function(listas,toredek){
  require(plyr)
  szavazatok<-merge(listas,toredek, by="szervezet",all.x=TRUE)
  szavazatok$szavazat<-szavazatok$szavazat.x+szavazatok$szavazat.y
  szavazatok<-szavazatok[,c("szervezet","szavazat")]
  return(droplevels(szavazatok))
}


dhondt <- function( szervezet, szavazat, seats=93 ){ 
  require(plyr)
  if(seats>0)
  {tmp <- data.frame( 
    szervezet = rep( szervezet, each = seats ), 
    mandatum     = as.vector(sapply( szavazat, function(x) x / 
                                       1:seats )) 
  ) 
  tmp <-tmp[order( - tmp$mandatum ),] [1:seats,] 
  tmp<-aggregate(mandatum~szervezet,data=tmp,FUN=length)
  return(tmp)}
  return(data.frame(szervezet=character(0),mandatum=integer(0)))
} 

saintelague <- function( szervezet, szavazat, seats=93 ){ 
  require(plyr)
  if(seats>0)
  {tmp <- data.frame( 
    szervezet = rep( szervezet, each = seats ), 
    mandatum     = as.vector(sapply( szavazat, function(x) x/((1:seats)*2-1) )) 
  ) 
  tmp <-tmp[order( - tmp$mandatum ),] [1:seats,] 
  tmp<-aggregate(mandatum~szervezet,data=tmp,FUN=length)
  return(tmp)}
  return(data.frame(szervezet=character(0),mandatum=integer(0)))
} 

modositottsaintelague <- function( szervezet, szavazat, seats=93 ){ 
  require(plyr)
  if(seats>0)
  {tmp <- data.frame( 
    szervezet = rep( szervezet, each = seats ), 
    mandatum     = as.vector(sapply( szavazat, function(x) x/c(1.4,((2:seats)*2-1)) )) 
  ) 
  tmp <-tmp[order( - tmp$mandatum ),] [1:seats,] 
  tmp<-aggregate(mandatum~szervezet,data=tmp,FUN=length)
  return(tmp)}
  return(data.frame(szervezet=character(0),mandatum=integer(0)))
} 

bonuszkiosztas<- function(lista,bonuszdb){
  if(length(lista)>0){
    lista<-lista[order(lista$szavazat,decreasing=TRUE),]
    return(data.frame( 
      szervezet = lista[1,1], 
      mandatum     = bonuszdb) 
    ) 
  }
  return (lista)
}

billegokorzet<-function(lista,kulonbseg=0.05){
  lista<-ddply(lista,~megye+oevk,function(x){x$arany<-x$szavazat/sum(x$szavazat)
                                                        x<-x[order(x$szavazat,decreasing=TRUE),][1:2,]
                                                        x<-data.frame(megye=x[1,1],
                                                                         oevk=x[1,2],
                                                                         gyoztes_jelolt=x[1,3],
                                                                         gyoztes_szervezet=x[1,4],
                                                                         gyoztes_arany=x[1,6],
                                                                         masodik_jelolt=x[2,3],
                                                                         masodik_szervezet=x[2,4],
                                                                         masodik_arany=x[2,6],
                                                                         kulonbseg=(x[1,6]-x[2,6])
                                                                         )
                                                        })
  lista<-lista[lista$kulonbseg<=kulonbseg,]
  lista<-lista[order(lista$kulonbseg),]
  colnames(lista)<-c("Megye","OEVK","Győztes jelölt","Győztes szervezet","Győztes szavazat","Második jelölt","Második szervezet","Második szavazat","Különbség")
  return(lista)
}

szavazat_modosito<-function(lista,szervezet='mind',megye='mind', oevk='mind',szavazat=1){
  feltetel<-''
  if (szervezet!='mind'){
    if (feltetel !='') 
      {feltetel<-paste0(feltetel,' & ')}
    feltetel<-paste0(feltetel,"lista$szervezet=='",szervezet,"' ")}
  if (megye!='mind'){
    if (feltetel !='') 
      {feltetel<-paste0(feltetel,' & ')}
    feltetel<-paste0(feltetel,"lista$megye=='",megye,"' ")}
  if (oevk!='mind'){
    if (feltetel !='') 
      {feltetel<-paste0(feltetel,' & ')}
    feltetel<-paste0(feltetel,"lista$oevk=='",oevk,"' ")}
  lista[eval(parse(text=feltetel)),'szavazat']<-lista[eval(parse(text=feltetel)),'szavazat']*szavazat
  return (lista)
}


valasztasi_eredmeny<-function(egyeni,listas,level,kuszob=0.05, listashely=93, 
                              elsotoredek=1, elosztas="dhondt", bonusz=0){
  require(plyr)
  eredmeny_egyeni<-egyeni_eredmeny(egyeni)
  toredek<-egyeni_toredek(egyeni,elsotoredek)
  lista_eredmeny<-listas_szavazatok(listas,kuszob)
  ifelse (elsotoredek!=-1,listas_mandatum<-listas_toredek_szavazatok(lista_eredmeny,toredek),listas_mandatum<-lista_eredmeny)
  listashely_szetoszt<-listashely-round(listashely*bonusz)
  listas_eredmeny<-switch(elosztas, 
                          "dhondt"=dhondt(listas_mandatum$szervezet,listas_mandatum$szavazat,listashely_szetoszt),
                          "saintelague"=saintelague(listas_mandatum$szervezet,listas_mandatum$szavazat,listashely_szetoszt),
                          "modositottsaintelague"=modositottsaintelague(listas_mandatum$szervezet,listas_mandatum$szavazat,listashely_szetoszt))
  listas_eredmeny<-rbind(listas_eredmeny,bonuszkiosztas(lista_eredmeny,round(listashely*bonusz)))
  listas_eredmeny<-aggregate(mandatum~szervezet,data=listas_eredmeny,FUN=sum)
  mandatumok<-rbind(eredmeny_egyeni,listas_eredmeny)
  osszesitett_mandatum<-aggregate(mandatum~szervezet,data=mandatumok,FUN=sum)
  osszesitett_mandatum$arany<-osszesitett_mandatum$mandatum/sum(osszesitett_mandatum$mandatum)
  osszesitett_mandatum$szervezet<-droplevels(osszesitett_mandatum$szervezet)
  return(osszesitett_mandatum)
}