# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

#CONEXÃO COM A DATABASE

con <- dbConnect(drv, dbname = "collins",
                 host = "173.249.21.143",
                 user = "covid", password = "StopCovid2019!")

#DADOS DE HORAS DE BOMBAGEM

horas_tbl <- dbReadTable(con,"horas_bombagem")

horas_tbl[,15] <- gsub("Combumune", "Combomune" ,horas_tbl[,15], perl=TRUE)

horas_tbl$local<- as.factor(horas_tbl$local)


#DADOS DAS AVARIAS E RESTRIÇÕES DE SERVIÇOS

avarias <- dbReadTable(con,"avarias")

avarias$local <- as.factor(avarias$local)

#DADOS DE VOLUMES DE AGUA

dbVolume <- dbReadTable(con,"volume")

dbVolume$local <- as.factor(dbVolume$local)

dbVolume[is.na(dbVolume)] <- 0

locais <- dbVolume$local

#DADOS DE CONSUMO DE QUIMICOS E REAGENTES
consumo <- dbReadTable(con,"consumo")

consumo[is.na(consumo)]<-0

consumo$local <- as.factor(consumo$local)

consumo[,10] <- gsub("Combumune", "Combomune" ,consumo[,10], perl=TRUE)

consumo[2:9] <- round(consumo[2:9],2)

#DADOS DE QUALIDADE
qualidade <- dbReadTable(con,"qualidade")

#qualidade <- dbGetQuery(con,"SELECT * FROM qualidade")
qualidade[,16] <- gsub("UlÃ³nguÃ¨", "Ulongue" ,qualidade[,16], perl=TRUE)

qualidade$local <- as.factor(qualidade$local)

qualidade[is.na(qualidade)] <- 0

colnames(qualidade) <-
  c(
    "date",
    "pH_Captacao",
    "Turvacao_Captacao",
    "pH_CD",
    "Turvacao_CD",
    "Cloro residual_CD",
    "phDeposito_elevado",
    "turvacao_deposito_elevado",
    "cloro residual_DE",
    "pH_RD1",
    "Turvacao_RD1",
    "Cloro residual_RD1",
    "pH_RD2",
    "Turvacao_RD2",
    "Cloro residual_RD2",
    "local",
    "phETA",
    "turvacaoETA",
    "Cloro residual_ETA",
    "turvacao_CD",
    "pH_RD3",
    "Turvacao_RD3",
    "Cloro residual_RD3",
    "pCaptacaoF1",
    "phCaptacaoF2",
    "phCaptacaoF3",
    "phCaptacaoF4",
    "ceCaptacaoF1",
    "ceCaptacaoF2",
    "ceCaptacaoF3",
    "ceCaptacaoF4",
    "CECD",
    "Cloro residual_CE",
    "CE_RD1",
    "CE_RD2",
    "CE_RD3",
    "Cloro residual_DE3",
    "phCaptacaoCE1",
    "phCaptacaoCE2",
    "phCaptacaoCE3",
    "CDRD1",
    "CDRD2",
    "CDRD3",
    "pH_Captacao_CE4",
    "pH_CF",
    "pH_CB",
    "cloro residual_CB",
    "Conformes CD",
    "Conformes DE",
    "Conformes RD1"
  )

qualidade <-qualidade[,1:50]

fact <- dbReadTable(con,"facturado")
fact[is.na(fact)]<- 0
fact$local <- as.factor(fact$local)

#CREDENCIAIS

#credentials <- dbReadTable(con,"credentials")


dbDisconnect(con)