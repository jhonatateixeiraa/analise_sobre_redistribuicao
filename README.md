# analise_sobre_redistribuicao
Tratamento de dados e plotagem de modelo logit para analisar quais variáveis são significativas no apoio a redistribuição de renda no Brasil, utilizando dummys.

# Bibliotecas 
library(broom)
library(tibble)
library(readxl)
library(ggplot2)

# Importar o banco de dados
banco <- read_excel("DADOS_SURVEY_REG (1).xlsx")
# Transformando em data frame
dados <- as.data.frame(banco)

# ou transformando em tibble
dados <- as_tibble(banco)

# Substitui os valores negativos por NA
dados <- data.frame(lapply(dados, function(x) ifelse(x < 0, NA, x)))

# Etapa 2: Criar nova coluna "apoio a redistribuição"
dados$APOIO_REDISTRIBUICAO[!is.na(dados$REDISTRIBUICAO)]<- ifelse(dados$REDISTRIBUICAO[!is.na(dados$REDISTRIBUICAO)] %in% c(1, 2), 1, 0)

# Etapa 3: Criar nova coluna "renda mais baixa" e "renda mais alta"
dados$RENDA_MAIS_BAIXA[!is.na(dados$RENDA)]<- ifelse(dados$RENDA[!is.na(dados$RENDA)] %in% c(1, 2), 1, 0)
dados$RENDA_MAIS_ALTA[!is.na(dados$RENDA)]<- ifelse(dados$RENDA[!is.na(dados$RENDA)] %in% c(9, 10), 1, 0)

# Etapa 4: Criar nova coluna "apoio a igualdade" e "apoio a desigualdade"
dados$APOIO_IGUALDADE[!is.na(dados$IGUALDADE)]<- ifelse(dados$IGUALDADE[!is.na(dados$IGUALDADE)] %in% c(1, 2), 1, 0)
dados$APOIO_DESIGUALDADE[!is.na(dados$IGUALDADE)]<- ifelse(dados$IGUALDADE[!is.na(dados$IGUALDADE)] %in% c(9, 10), 1, 0)

# Etapa 5: Criar nova coluna "visão_trabalho1" e "visão_trabalho2"
dados$VISAO_TRABALHO1[!is.na(dados$VISAO_TRABALHO)]<- ifelse(dados$VISAO_TRABALHO[!is.na(dados$VISAO_TRABALHO)] %in% c(1, 2), 1, 0)
dados$VISAO_TRABALHO2[!is.na(dados$VISAO_TRABALHO)]<- ifelse(dados$VISAO_TRABALHO[!is.na(dados$VISAO_TRABALHO)] %in% c(9, 10), 1, 0)

# Etapa 6: Criar novas colunas para a variável "crime"
dados$CRIMINALIDADE1[!is.na(dados$CRIME)]<- ifelse(dados$CRIME[!is.na(dados$CRIME)] == 1, 1, 0)
dados$CRIMINALIDADE2[!is.na(dados$CRIME)]<- ifelse(dados$CRIME[!is.na(dados$CRIME)] == 2, 1, 0)
dados$CRIMINALIDADE3[!is.na(dados$CRIME)]<- ifelse(dados$CRIME[!is.na(dados$CRIME)] == 3, 1, 0)

# Etapa 7: Criar novas colunas para a variável "política"
dados$MAIS_ESQUERDA[!is.na(dados$POLITICA)]<- ifelse(dados$POLITICA[!is.na(dados$POLITICA)] %in% c(1, 2), 1, 0)
dados$MAIS_DIREITA[!is.na(dados$POLITICA)]<- ifelse(dados$POLITICA[!is.na(dados$POLITICA)] %in% c(9, 10), 1, 0)

# Etapa 8: Criar novas colunas para a variável "social"
dados$SOCIAL1[!is.na(dados$SOCIAL)] <- ifelse(dados$SOCIAL[!is.na(dados$SOCIAL)] %in% c(1, 2), 1, 0)
dados$SOCIAL2[!is.na(dados$SOCIAL)] <- ifelse(dados$SOCIAL[!is.na(dados$SOCIAL)] == 3, 1, 0)
dados$SOCIAL3[!is.na(dados$SOCIAL)] <- ifelse(dados$SOCIAL[!is.na(dados$SOCIAL)] == 4, 1, 0)

# Etapa 9: Criar nova coluna para a variável "sexo"
dados$SEXO1[!is.na(dados$SEXO)] <- ifelse(dados$SEXO[!is.na(dados$SEXO)] == 0, 1, 0)

# Etapa 10: Criar novas colunas para a variável "escolaridade"
dados$ESCOLARIDADE1[!is.na(dados$ESCOLARIDADE)]<- ifelse(dados$ESCOLARIDADE == 0, 1, 0)
dados$ESCOLARIDADE2[!is.na(dados$ESCOLARIDADE)]<- ifelse(dados$ESCOLARIDADE %in% c(1, 2), 1, 0)
dados$ESCOLARIDADE3[!is.na(dados$ESCOLARIDADE)]<- ifelse(dados$ESCOLARIDADE == 3, 1, 0)
dados$SEXO1[is.na(dados$SEXO)] <- NA

# Etapa 11: Criar nova coluna para a variável "emprego"
dados$EMPREGO1[!is.na(dados$EMPREGO)]< ifelse(dados$EMPREGO[!is.na(dados$EMPREGO)] %in% c(1, 2), 1, 0)
dados$EMPREGO1[!is.na(dados$EMPREGO)]<- ifelse(dados$EMPREGO[!is.na(dados$EMPREGO)] %in% c(3, 4, 5, 6, 7), 0, dados$EMPREGO1[!is.na(dados$EMPREGO)])
dados$EMPREGO1[is.na(dados$EMPREGO)] <- NA

# Etapa 12: Criar nova coluna na coluna "ESTADO_CIVIL"
dados$ESTADO_CIVIL1[!is.na(dados$ESTADO_CIVIL)]<- ifelse(dados$ESTADO_CIVIL[!is.na(dados$ESTADO_CIVIL)] %in% c(1,2), 1, 0)

# Ajustar o modelo logístico 1
modelo_logit1 <- glm(APOIO_REDISTRIBUICAO ~ RENDA_MAIS_BAIXA + RENDA_MAIS_ALTA + APOIO_IGUALDADE + APOIO_DESIGUALDADE + VISAO_TRABALHO1 + VISAO_TRABALHO2 + CRIMINALIDADE1 + CRIMINALIDADE2 + CRIMINALIDADE3 + MAIS_ESQUERDA + MAIS_DIREITA + SOCIAL1 + SOCIAL2 + SOCIAL3 + SEXO1 + ESTADO_CIVIL1 + EMPREGO1 + ESCOLARIDADE1 + ESCOLARIDADE2 + ESCOLARIDADE3 + IDADE, data = dados, family = binomial(link = "logit"))

# Visualizar os resultados do modelo
summary(modelo_logit1)

# Ajustar o modelo logístico 2
modelo_logit2 <- glm(APOIO_REDISTRIBUICAO ~ RENDA_MAIS_BAIXA + RENDA_MAIS_ALTA  + VISAO_TRABALHO1 + VISAO_TRABALHO2 + CRIMINALIDADE1 + CRIMINALIDADE2 + CRIMINALIDADE3 + MAIS_ESQUERDA + MAIS_DIREITA + SOCIAL1 + SOCIAL2 + SOCIAL3 + SEXO1 + ESTADO_CIVIL1 + EMPREGO1 + ESCOLARIDADE1 + ESCOLARIDADE2 + ESCOLARIDADE3 + IDADE, data = dados, family = binomial(link = "logit"))

# Visualizar os resultados do modelo
summary(modelo_logit2) 

