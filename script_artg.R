library(tidyr)
library(car)
library(lmtest)
library(sandwich)
library(arm)
library(lme4)

## Códigos IBGE / TSE
cod <- read.csv('artg_seminario/codigos_ibge_tse.csv', 
                sep = ',',encoding = 'utf-8')
cod$cod_mun <- cod$cod_municipio_tse

## Atlas dos Municípios
atlas <- read.table('new_atlas.txt', sep = ';')
View(atlas)

getwd()
setwd("C:/Users/test/Desktop/artigoseminario/seminariocp_dados")

## Censo
censo <- read.csv("censo_municipios_cor_sexo_religiao.csv",
                  sep = ',')
View(censo)

levels(censo$uf) <- levels(cod$uf)

censo <- merge(censo,cod, by = 'uf')
censo$cod_mun <- as.numeric(substr(censo$cod_municipio_ibge,1,6))

censo$pop_tot <- censo$pop_homem + censo$pop_mulher
censo$pop_branca <- censo$pop_branca / censo$pop_tot
censo$pop_homem <- censo$pop_homem / censo$pop_tot
censo$pop_mulher <- censo$pop_mulher / censo$pop_tot
censo$pop_evangelica <- censo$pop_evangelica / censo$pop_tot

## População Municipios
pop <- read.csv('artg_seminario/populacao_residente_91_17.csv', 
                sep = ',',encoding = 'UTF-8')
pop <- gather(pop,ano,populacao,a1992:a2017,factor_key = T)
pop$ano <- as.numeric(substr(pop$ano,2,5))
pop$cod_mun <- as.numeric(substr(pop$cod_mun,1,6))

## PIB municiípios
pib1 <- read.csv('artg_seminario/pibmun_02_09.csv', sep = ';')
pib1 <- pib1[c('Ano','Código.do.Município',
               'Produto.Interno.Bruto..a.preços.correntes..R..1.000.')]
pib2 <- read.csv('artg_seminario/pibmun_10_15.csv', sep = ';')
pib2 <- pib2[c('Ano','Código.do.Município',
               'Produto.Interno.Bruto..a.preços.correntes..R..1.000.')]
colnames(pib1) = colnames(pib2) = c('ano','cod_mun','pib')
pib <- rbind(pib1,pib2)
pib$pib <- as.numeric(gsub(',','.',pib$pib))
pib$cod_mun <- as.numeric(substr(pib$cod_mun,1,6))

pib_att <- data.frame(ano = 2016,
                      cod_mun = pib$cod_mun[pib$ano == 2015],
                      pib = pib$pib[pib$ano == 2015])

pib <- rbind(pib,pib_att)

## Bolsa Família
bolsa_n <- read.table('artg_seminario/numero_beneficiarios_04_16.csv', 
                      sep = ',',encoding = 'UTF-8',header = T)
bolsa_n <- gather(bolsa_n,ano,num_bolsa,a2004:a2016,factor_key = T)
bolsa_n$ano <- as.numeric(substr(bolsa_n$ano,2,5))

bolsa_v <- read.table('artg_seminario/valor_beneficiarios_04_16.csv', 
                      sep = ',',encoding = 'UTF-8',header = T)
bolsa_v <- gather(bolsa_v,ano,valor_bolsa,a2004:a2016,factor_key = T)
bolsa_v$ano <- as.numeric(substr(bolsa_v$ano,2,5))

no_data <- cod$cod_municipio_ibge[!(cod$cod_municipio_ibge %in% levels(as.factor(bolsa_n$cod_mun)))]
yk = c()
for(k in c(2004:2013,2016)){
  yk = c(yk,rep(k,length(no_data)))
}
no_data <- data.frame(uf = cod$uf[cod$cod_municipio_ibge %in% no_data],
                      cod_mun = no_data,
                      municipio = cod$nome_municipio_ibge[cod$cod_municipio_ibge %in% no_data],
                      ano = yk,
                      num_bolsa = NA)

bolsa_n <- rbind(bolsa_n,no_data)
colnames(no_data) <- c("uf","cod_mun","municipio","ano","valor_bolsa")
bolsa_v <- rbind(bolsa_v,no_data)

bolsa_n$cod_mun <- as.numeric(substr(bolsa_n$cod_mun,1,6))
bolsa_v$cod_mun <- as.numeric(substr(bolsa_v$cod_mun,1,6))

## Dados Data SUS
sus <- read.table('artg_seminario/join_datasus_00_15.txt', sep = ';')
sus$cod_mun <- as.numeric(substr(sus$cod_mun,1,6))
sus_att <- data.frame(ano = 2016,
                      cod_mun = sus$cod_mun[sus$ano == 2015],
                      homicidio = sus$homicidio[sus$ano == 2015],
                      suicidio = sus$suicidio[sus$ano == 2015],
                      armas = sus$armas[sus$ano == 2015],
                      drogas = sus$drogas[sus$ano == 2015],
                      alcool = sus$alcool[sus$ano == 2015])

sus <- rbind(sus,sus_att)

## Dados TSE
tse1 <- read.table('artg_seminario/eleicao_local_2004.txt', sep = ';')
tse2 <- read.table('artg_seminario/eleicao_local_2008.txt', sep = ';')
tse3 <- read.table('artg_seminario/eleicao_local_2012.txt', sep = ';')
tse4 <- read.table('artg_seminario/eleicao_local_2016.txt', sep = ';')

tse <- rbind(tse1,tse2)
tse <- rbind(tse,tse3)
tse <- rbind(tse,tse4)

## Dados Taxa de Reeleição
relec <- read.csv('artg_seminario/taxa_reeleicao_uf.csv')
relec <- merge(relec,cod, by = 'uf')
relec$cod_mun <- as.numeric(substr(relec$cod_municipio_ibge,1,6))

## Agregando Dados
aa <- merge(tse,cod[c('cod_mun','cod_municipio_ibge','uf')],by = 'cod_mun')
aa$cod_mun <- as.numeric(substr(aa$cod_municipio_ibge,1,6))
aa <- merge(aa,bolsa_n, by = c('cod_mun','ano'))
aa <- merge(aa,bolsa_v, by = c('cod_mun','ano'))
aa <- merge(aa,pib,by = c('cod_mun','ano'))
aa <- merge(aa,pop, by = c('cod_mun','ano'))
aa <- merge(aa,sus, by = c('cod_mun','ano'))
aa <- merge(aa,atlas, by = c('cod_mun','ano'))
aa <- merge(aa,censo, by = c('cod_mun','ano'))
aa <- merge(aa,relec, by = c('cod_mun','ano'))

aa$uf <- aa$uf.x

## Normalizando
Normal <- function(x){
  y = (x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T))
  return(y)
}

## Variáveis
aa$votos_seg[is.na(aa$votos_seg) == T] <- 0
aa$despesa_seg[is.na(aa$despesa_seg) == T] <- 0

aa$voto_med <- aa$total_votos / aa$total_pol
aa$votos_seg <- (aa$votos_seg - aa$voto_med) / aa$sd_votos
aa$votos_seg <- Normal(aa$votos_seg)

aa$despesa_med <- aa$total_despesa / (aa$total_pol * aa$populacao)
aa$despesa_seg <- aa$despesa_seg / aa$populacao
aa$despesa_seg <- (aa$despesa_seg - aa$despesa_med) / aa$sd_despesa

aa$despesa_seg[aa$despesa_seg == min(aa$despesa_seg,na.rm = T)] <- 
  sort(aa$despesa_seg[aa$despesa_seg > - Inf])[1]
aa$despesa_seg[aa$despesa_seg == max(aa$despesa_seg,na.rm = T)] <- 
  sort(aa$despesa_seg[aa$despesa_seg < Inf],decreasing = T)[1]
aa$despesa_seg <- Normal(aa$despesa_seg)

aa$n_part_seg <- Normal(aa$n_part_seg / aa$total_pol)

aa$politico_pop <- Normal(aa$total_pol * 100000 / aa$populacao)

aa$legenda <- Normal(aa$legenda / aa$comparecimento)
aa$brancos <- Normal(aa$brancos / aa$comparecimento)
aa$nulos <- Normal(aa$nulos / aa$comparecimento)

aa$tx_reeleicao <- Normal(aa$tx_reeleicao)
aa$margem_vit <- Normal(aa$margem_vit)

aa$seg_turno <- ifelse(aa$aptos > 200000,1,0)

aa$jovem <- Normal(aa$jovem)
aa$idosa <- Normal(aa$idosa)
aa$homem <- Normal(aa$pop_homem)
aa$branca <- Normal(aa$pop_branca)
aa$evangelica <- Normal(aa$pop_evangelica)

aa$mort_infantil <- Normal(aa$mort_infantil)
aa$anos_estudo <- Normal(aa$anos_estudo)
aa$renda_dif <- Normal(aa$renda_rico - aa$renda_pobre)
aa$renda_media <- Normal(aa$renda_media)
aa$gini <- Normal(aa$gini)
aa$idhm <- Normal(aa$idhm)

aa$homicidio <- Normal(aa$homicidio * 100000 / aa$populacao)
aa$suicidio <- Normal(aa$suicidio * 100000 / aa$populacao)
aa$armas <- Normal(aa$armas * 100000 / aa$populacao)
aa$alcool <- Normal(aa$alcool * 100000 / aa$populacao)
aa$drogas <- Normal(aa$drogas * 100000 / aa$populacao)

bb <- aa[c('votos_seg','homicidio','armas','alcool','drogas',
           'despesa_seg','partido_seg','n_part_seg','politico_pop',
           'seg_turno','brancos','nulos','jovem','homem','branca',
           'evangelica','anos_estudo','gini','renda_media',
           'mort_infantil','margem_vit','ano','uf')]
bb <- na.omit(bb)

bb$reg <- ifelse(bb$uf %in% c('TO','AC','PA','RO','RR','AP','AM'), 'Norte',
                 ifelse(bb$uf %in% c('MT','MS','GO','DF'),'Centro',
                        ifelse(bb$uf %in% c('BA','SE','AL','PB','PE','RN','CE','PI','MA'),'Nordeste',
                               ifelse(bb$uf %in% c('ES','RJ','MG','SP'),'Sudeste','Sul'))))

## Evolução do Desempenho de Candidaturas Militaristas
plot(tapply(bb$votos_seg, bb$ano, mean) ~ c(2004,2008,2012,2016),
     type = 'l', ylab = 'Desempenho Eleitoral',xlab = 'Ano',
     lwd = 2, cex.lab = 1.5,cex.axis = 1.3,family = 'serif',
     main = 'Brasil')

par(mar = c(5, 4, 4, 2) + 0.1,mfrow = c(3,2))
for(k in levels(as.factor(bb$reg))){
  with(bb[bb$reg == k,],{
    plot(tapply(votos_seg, ano, mean) ~ c(2004,2008,2012,2016),
         type = 'l', ylab = 'Desempenho Eleitoral',xlab = 'Ano',
         lwd = 2, cex.lab = 1.5,cex.axis = 1.3,family = 'serif',
         main = k)
  })
}

## Evolução do Financiamento de Candidaturas Militaristas
plot(tapply(bb$despesa_seg, bb$ano, mean) ~ c(2004,2008,2012,2016),
     type = 'l', ylab = 'Despesa de Campanha',xlab = 'Ano',
     lwd = 2, cex.lab = 1.5,cex.axis = 1.3,family = 'serif',
     main = 'Brasil')

for(k in levels(as.factor(bb$reg))){
  with(bb[bb$reg == k,],{
    plot(tapply(despesa_seg, ano, mean) ~ c(2004,2008,2012,2016),
         type = 'l', ylab = 'Despesa de Campanha',xlab = 'Ano',
         lwd = 2, cex.lab = 1.5,cex.axis = 1.3,family = 'serif',
         main = k)
  })
}

## Moldelos Multiníveis
m1 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
             despesa_seg + partido_seg + n_part_seg + politico_pop + 
             seg_turno + margem_vit + I(brancos + nulos) + jovem + 
             branca + evangelica + anos_estudo + gini + renda_media + 
             mort_infantil + (1|reg) + (1|ano), data = bb)
summary(m1)
vif(m1)

# Calculando hatvalues para extrair outliers
res <- hatvalues(m1)
res <- ifelse(res > 3 * 18 / nrow(bb),1,0)

m1n <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + margem_vit + I(brancos + nulos) + jovem + 
              branca + evangelica + anos_estudo + gini + renda_media + 
              mort_infantil + (1|reg) + (1|ano), data = bb[res == 0,])
summary(m1n)

var_crim = c('homicidio','armas','alcool','drogas')
var_inst = c('despesa_seg','partido_seg','n_part_seg','politico_pop','seg_turno')
var_demg = c('jovem','branca','evangelica')
var_insf = c('I(brancos + nulos)','margem_vit')
var_soce = c('anos_estudo','gini','renda_media','mort_infantil')

## Plotando Coeficientes
par(mar = c(1,8.5,5.1,2),family = 'serif')
coefplot(summary(m1n)$coef[var_crim,1],summary(m1n)$coef[var_crim,2],
         varnames = c('Tx. Homicídio','Qtd. Armas','Cons. Alcool','Cons. Drogas'),
         main = 'Variáveis Criminológicas',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

coefplot(c(0.088094508,0.01235989,0.064593331,0.03797164,0.000004196),
         c(0.003501805,0.00198851,0.003319707,0.00366203,0.000206740),
         varnames = c('Desp. Camp.','Part. Pref. * 10','Num. Col. Parl.',
                      'Pol. 100milhab.*10','Seg. Turno'),
         main = 'Variáveis Institucionais',family = 'serif',
         xlim = c(0,0.1),cex.var = 1.2,cex.pts = 1.5)

coefplot(summary(m1n)$coef[var_demg,1],summary(m1n)$coef[var_demg,2],
         varnames = c('Prop. Pop. Jovem','Prop. Pop. Brancos','Prop. Pop. Evangel.'),
         main = 'Variáveis Demográficas',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

coefplot(summary(m1n)$coef[var_insf,1],summary(m1n)$coef[var_insf,2],
         varnames = c('Brancos + Nulos','Pref. Margem Vit.'),
         main = 'Variáveis Insatisfação Política',family = 'serif',
         xlim = c(0,0.003),cex.var = 1.2,cex.pts = 1.5)

coefplot(summary(m1n)$coef[var_soce,1],summary(m1n)$coef[var_soce,2],
         varnames = c('Anos de Estudo','Gini','Renda Média','Mort. Infantil'),
         main = 'Variáveis Socioeconômicas',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

par(mar = c(5, 4, 4, 2) + 0.1,family = 'serif')

## Moldelos Multiníveis desagregados por ano
mp1 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + margem_vit + I(brancos + nulos) + jovem + 
              branca + evangelica + anos_estudo + gini + renda_media + 
              mort_infantil + (1|reg), 
            data = bb[bb$ano == 2004 & res == 0,])
summary(mp1)

mp2 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + margem_vit + I(brancos + nulos) + jovem + 
              branca + evangelica + anos_estudo + gini + renda_media + 
              mort_infantil + (1|reg), 
            data = bb[bb$ano == 2008 & res == 0,])
summary(mp2)

mp3 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + margem_vit + I(brancos + nulos) + jovem + 
              branca + evangelica + anos_estudo + gini + renda_media + 
              mort_infantil + (1|reg), 
            data = bb[bb$ano == 2012 & res == 0,])
summary(mp3)

mp4 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + margem_vit + I(brancos + nulos) + jovem + 
              branca + evangelica + anos_estudo + gini + renda_media + 
              mort_infantil + (1|reg), 
            data = bb[bb$ano == 2016 & res == 0,])
summary(mp4)

## Evolução Efeito Variáveis Criminológicas
crime = c(summary(mp1)$coef[2,1],summary(mp2)$coef[2,1],
          summary(mp3)$coef[2,1],summary(mp4)$coef[2,1])
crime = crime + c(summary(mp1)$coef[3,1],summary(mp2)$coef[3,1],
                  summary(mp3)$coef[3,1],summary(mp4)$coef[3,1])
crime = crime + c(summary(mp1)$coef[4,1],summary(mp2)$coef[4,1],
                  summary(mp3)$coef[4,1],summary(mp4)$coef[4,1])
crime = crime + c(summary(mp1)$coef[5,1],summary(mp2)$coef[5,1],
                  summary(mp3)$coef[5,1],summary(mp4)$coef[5,1])

plot(crime ~ c(2004,2008,2012,2016),col = 'black',
     lwd = 2,pch = 16,type = 'l')

## Evolução Efeito Variáveis Institucionais
inst = c(summary(mp1)$coef[6,1],summary(mp2)$coef[6,1],
         summary(mp3)$coef[6,1],summary(mp4)$coef[6,1])
inst = inst + c(summary(mp1)$coef[7,1],summary(mp2)$coef[7,1],
                summary(mp3)$coef[7,1],summary(mp4)$coef[7,1])
inst = inst + c(summary(mp1)$coef[8,1],summary(mp2)$coef[8,1],
                summary(mp3)$coef[8,1],summary(mp4)$coef[8,1])
inst = inst + c(summary(mp1)$coef[9,1],summary(mp2)$coef[9,1],
                summary(mp3)$coef[9,1],summary(mp4)$coef[9,1])
inst = inst + c(summary(mp1)$coef[10,1],summary(mp2)$coef[10,1],
                summary(mp3)$coef[10,1],summary(mp4)$coef[10,1])

plot(inst ~ c(2004,2008,2012,2016),col = 'black',
     lwd = 2,pch = 16,type = 'l')

## Evolução Efeito Variáveis Insatisfação com a Política
insa = c(summary(mp1)$coef[11,1],summary(mp2)$coef[11,1],
         summary(mp3)$coef[11,1],summary(mp4)$coef[11,1])
insa = insa + c(summary(mp1)$coef[12,1],summary(mp2)$coef[12,1],
                summary(mp3)$coef[12,1],summary(mp4)$coef[12,1])

plot(insa ~ c(2004,2008,2012,2016),col = 'black',
     lwd = 2,pch = 16,type = 'l')

## Evolução Efeito Variáveis Demográficas
demo = c(summary(mp1)$coef[13,1],summary(mp2)$coef[13,1],
         summary(mp3)$coef[13,1],summary(mp4)$coef[13,1])
demo = demo - c(summary(mp1)$coef[14,1],summary(mp2)$coef[14,1],
                summary(mp3)$coef[14,1],summary(mp4)$coef[14,1])
demo = demo + c(summary(mp1)$coef[15,1],summary(mp2)$coef[15,1],
                summary(mp3)$coef[15,1],summary(mp4)$coef[15,1])

plot(demo ~ c(2004,2008,2012,2016),col = 'black',
     lwd = 2,pch = 16,type = 'l')

## Incluindo Variável Taxa de Reeleição ( ainda em construção )
cc <- aa[c('votos_seg','homicidio','armas','alcool','drogas',
           'despesa_seg','partido_seg','n_part_seg','politico_pop',
           'seg_turno','brancos','nulos','jovem','homem','branca',
           'evangelica','anos_estudo','gini','renda_media',
           'mort_infantil','ano','uf','tx_reeleicao','margem_vit')]
cc <- na.omit(cc)

cc$reg <- ifelse(cc$uf %in% c('TO','AC','PA','RO','RR','AP','AM'), 'Norte',
                 ifelse(cc$uf %in% c('MT','MS','GO','DF'),'Centro',
                        ifelse(cc$uf %in% c('BA','SE','AL','PB','PE','RN','CE','PI','MA'),'Nordeste',
                               ifelse(cc$uf %in% c('ES','RJ','MG','SP'),'Sudeste','Sul'))))

nm1 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + I(brancos + nulos) + margem_vit + tx_reeleicao + 
              jovem + branca + evangelica + anos_estudo + gini + 
              renda_media + mort_infantil + (1|reg) + (1|ano), data = cc)
summary(nm1)
vif(nm1)

ress <- hatvalues(nm1)
ress <- ifelse(ress > 3 * 19 / nrow(cc),1,0)

nm1n <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
               despesa_seg + partido_seg + n_part_seg + politico_pop + 
               seg_turno + I(brancos + nulos) + margem_vit + tx_reeleicao + 
               jovem + branca + evangelica + anos_estudo + gini + 
               renda_media + mort_infantil + (1|reg) + (1|ano), data = cc[ress == 0,])
summary(nm1n)

np1 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + I(brancos + nulos) + margem_vit + tx_reeleicao + 
              jovem + branca + evangelica + anos_estudo + gini + 
              renda_media + mort_infantil + (1|reg), 
            data = cc[cc$ano == 2004 & ress == 0,])
summary(np1)

np2 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + I(brancos + nulos) + margem_vit + tx_reeleicao + 
              jovem + branca + evangelica + anos_estudo + gini + 
              renda_media + mort_infantil + (1|reg), 
            data = cc[cc$ano == 2008 & ress == 0,])
summary(np2)

np3 <- lmer(votos_seg ~ homicidio + armas + alcool + drogas +
              despesa_seg + partido_seg + n_part_seg + politico_pop + 
              seg_turno + I(brancos + nulos) + margem_vit + tx_reeleicao + 
              jovem + branca + evangelica + anos_estudo + gini + 
              renda_media + mort_infantil + (1|reg), 
            data = cc[cc$ano == 2012 & res == 0,])
summary(np3)



