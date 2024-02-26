#Instala tidyverse
install.packages("tidyverse")

#Carrega bibliotecas
library(tidyverse)
library(readr)

#Importa questionário
df <- read_csv("bases/questionario-catalano-07122023.csv")

#Remove linha vazia do questionário
df <- df %>% 
  filter(!is.na(`Post ID`))


#Alterar Q. 2  para númerico versão com pipe %>% 
df2 <- df %>% 
  mutate(`12. Quantas irmãs ou irmãos moram com você?` = ifelse(
    is.na(`12. Quantas irmãs ou irmãos moram com você?`), 0, `12. Quantas irmãs ou irmãos moram com você?`))

#Criar novas colunas baseadas nas opções marcadas em Q. 28
df2 <- df2 %>% 
  mutate("Q28_SOZINHA" = ifelse(
    grepl("sozinha", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
    T, F),
    "Q28_IRMA" = ifelse(
      grepl("irmã", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    "Q28_AMIGA" = ifelse(
      grepl("amiga", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    "Q28_NAMORADA" = ifelse(
      grepl("namorada", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    "Q28_MAE" = ifelse(
      grepl("mãe", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    "Q28_FAMILIA" = ifelse(
      grepl("familiares", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    "Q28_VIZINHA" = ifelse(
      grepl("vizinha", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    "Q28_PAMIGOS" = ifelse(
      grepl("pais de amigos", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    "Q28_OUTROS" = ifelse(
      grepl("outras pessoas", `28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`),
      T, F),
    )

#Verificar e comparar as novas colunas com a coluna original
df2 %>% 
  select(`28. Com quem você geralmente vai para a escola? (marcar uma ou mais opções)`,
         Q28_SOZINHA, Q28_IRMA, Q28_AMIGA, Q28_NAMORADA,
         Q28_MAE, Q28_FAMILIA, Q28_VIZINHA, Q28_PAMIGOS, Q28_OUTROS) %>% 
  View()

#Conta quantas pessoas por gênero
df2 %>% 
  group_by(`4. Qual o gênero com o qual você se identifica?`) %>% 
  summarise("count" = n())

#Conta quantos responderam que vão sozinhas e/ou com amigas por gênero
df2 %>% 
  group_by(Q28_SOZINHA, `4. Qual o gênero com o qual você se identifica?`) %>% 
  summarise("count" = n()) %>% 
  mutate(freq = count / sum(count))
summarise("count" = n())

#Conta quantos responderam que vão sozinhas e/ou com amigas por gênero
df2 %>% 
  group_by(`4. Qual o gênero com o qual você se identifica?`, Q28_SOZINHA) %>% 
  summarise("count" = n()) %>% 
  mutate(freq = count / sum(count))
