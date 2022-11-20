################################################################################
#################### Script Biomassa - Alunos ##################################
################################################################################
#------------------------------------------------------------------------------#
# Baixar e ativar os pacotes
pacotes <- c("tidyverse","dplyr", "tidyr", "BIOMASS", "xlsx", "flora")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Digitar o Banco de dados 
dados <- data.frame(parcelas = c("P01"), # parcela,  
                    UTM_leste = c(541906), # coordenada x da árvore
                    UTM_norte = c(7192495), # coordenada y da árvore
                    Familia = c("Myrtaceae"), # altura total da árvore
                    nome_comum = c("Guabiroba"), # diâmetro da copa de cada árvore
                    Nome_cientifico = c("Campomanesia xanthocarpa"),# altura da copa de cada árvore
                    DC = c(5.22), # diâmetro de copa
                    DAP = c(16.01), # diâmetro à altura do peito (1,30 m)
                    Ht = c(10.8), # altura total
                    Hc = c(6.5), # altura comercial
                    C.copa = c(4.3)) # comprimento de copa

#------------SEPARAR A COLUNA NOME CIENTIFICO EM GÊNERO E ESPÉCIE--------------#
dados<-dados %>%
  separate(Nome_cientifico, into=c("genero","especie"), sep=c(" "))
dados<-mutate(dados, nome_cientifico=paste(genero, especie))

#--------------------CHEGAR O NOME CIENTIFICO----------------------------#
Taxo <- correctTaxo(genus = dados$genero, # precisa de internet!
                    species = dados$especie)
dados$generoCorreto <- Taxo$genusCorrected
dados$especieCorrigido <- Taxo$speciesCorrected
dados$modificado <- Taxo$nameModified

#----------------Obter familia/ordem botanica (se necessário)-------------#
APG <- getTaxonomy(dados$genero, findOrder = T)
dados$Familia <- APG$family

# 1) Densidade da madeira ----
dataWD <- getWoodDensity(
  genus = dados$genero,
  species = dados$especie,
  stand = dados$parcelas)

dados$DB <- dataWD$meanWD
dados$levelDB <- dataWD$levelWD
DB<-round(dados$DB, 2)
dados$DB <- DB

# Calcular AGB (Biomassa), Estoque de Carbono, CRE e GJ  ------

# Para cada arvore:
AGBtree <- computeAGB( # in Mg (ton)
  D = dados$DAP,
  WD = dados$DB,
  H = dados$Ht)
dados$AGB <- AGBtree

# Arredondar valores alguns dados
AGB<-round(dados$AGB, 2)
dados$AGB <- AGB

# Estoque de Carbono - (Calculado em toneladas)
attach(dados)
dados<-mutate(dados, Estoque_carbono=AGB*(44*50/1200))
Estoque_carbono<-round(dados$Estoque_carbono, 2)
dados$Estoque_carbono <- Estoque_carbono

# CRE - Certificado de Redução de Emissão = Estoque de carbono em tonelada
dados<-mutate(dados, CRE= Estoque_carbono)
CRE<-round(dados$CRE, 2)
dados$CRE <- CRE

# Emissão de CO2 na queima completa da biomassa (Nm³)
# Densidade de CO2 = 1,9632 kg/ Nm³
dados<-mutate(dados, ECO2=((Estoque_carbono/1.9632)*1000))
ECO2<-round(dados$ECO2, 2)
dados$ECO2 <- ECO2

# GJ - Energia Liberada na forma de calor no Incendio Florestal
# PCI = Poder Calorífico Inferior é de 18.5281 Mj/kg
dados <- mutate (dados, GJ= AGB*1000*18.5281)
GJ<-round(dados$GJ, 2)
dados$GJ <- GJ

names(dados)
dados<-dplyr::select(dados, "parcelas", "UTM_leste", "UTM_norte","Familia",
                     "nome_comum", "nome_cientifico", "genero", "especie",
                     "DAP", "Ht", "Hc", "C.copa", "DC", "DB", "AGB",
                     "Estoque_carbono", "CRE", "ECO2", "GJ")

# Transformar número em caracteres
str(dados)
dados$UTM_leste<-as.character(dados$UTM_leste)
dados$UTM_norte<-as.character(dados$UTM_norte)
dados$DC<-as.character(dados$DC)
dados$DAP<-as.character(dados$DAP)
dados$Ht<-as.character(dados$Ht)
dados$Hc<-as.character(dados$Hc)
dados$C.copa<-as.character(dados$C.copa)
dados$DB<-as.character(dados$DB)
dados$AGB<-as.character(dados$AGB)
dados$Estoque_carbono<-as.character(dados$Estoque_carbono)
dados$CRE<-as.character(dados$CRE)
dados$ECO2<-as.character(dados$ECO2)
dados$GJ<-as.character(dados$GJ)


# Converter linha em coluna
dados<- dados%>%
  tidyr::pivot_longer(everything(),
                      names_to ="Caracteristicas",
                      values_to = "Informacoes")

dados<-mutate(dados, unidade=c(" "," ", " ", " ", " ", " ", " ", " ",
                               "cm", "m", "m", "m","m", "g/cm³", "Mg",
                               "t", "t", "kg/Nm³", "Mj/kg"))

# Baixar o banco de dados
write.xlsx(dados, file="dados_completo.xlsx")

# Informações taxonômicas ----

################################################################################ 
################# Informações taxonômicas ######################################
################################################################################

# Função get.taxa
help("get.taxa")

# A função gera informações taxonômicas a 
# partir de uma lista de espécies científicas. 

lista = c("Campomanesia xanthocarpa")
lista

info = get.taxa(lista, 
                life.form = TRUE,
                suggest.names = TRUE,
                habitat = TRUE,
                states = TRUE,
                establishment = TRUE,
                domain = TRUE,
                endemism = TRUE,
                vegetation.type = TRUE,
                replace.synonyms = TRUE)
str(info)
info$id<-as.character(info$id)

info<- info%>%
  tidyr::pivot_longer(everything(),
                      names_to ="Caracteristicas",
                      values_to = "Resultados")

# Nome científico completo com o nome dos autores na frente. Tem informações 
# de família, forma de vida informa se é árvore, arbusto, no establishment 
# informa de é nativa do Brasil ou se não é nativa (espécie naturalizada)

# Baixar o banco de dados
write.xlsx(info, file="dados_especie.xlsx")
