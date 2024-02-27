#Criar novas colunas baseadas nas opções marcadas em Q. 43
df2 <- df2 %>% 
  mutate(
    "Q43_NENHUM" = ifelse(
      grepl("nenhum", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_MUITOS_ALUNOS_POR_CLASSE" = ifelse(
      grepl("muitos alunos por classe", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_PROFESSORES_RUINS" = ifelse(
      grepl("professores ruins", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_INSEGURANÇA_DENTRO_DA_ESCOLA" = ifelse(
      grepl("insegurança dentro da escola", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_BULLYING" = ifelse(
      grepl("bullying", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_PRECONCEITO" = ifelse(
      grepl("preconceito", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_POUCO_TEMPO_NA_ESCOLA" = ifelse(
      grepl("pouco tempo na escola", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_FALTA_MATERIAL_ESCOLAR" = ifelse(
      grepl("falta material escolar", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_FALTA_DE_EQUIPAMENTOS" = ifelse(
      grepl("falta de equipamentos", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_ESCOLA_PEQUENA_/_APERTADA" = ifelse(
      grepl("escola pequena / apertada", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_INFRAESTRUTURA_RUIM" = ifelse(
      grepl("infraestrutura ruim", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_SUJEIRA" = ifelse(
      grepl("sujeira", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_MERENDA_RUIM_OU_INSUFICIENTE" = ifelse(
      grepl("merenda ruim ou insuficiente", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_INDISCIPLINA_DOS_ALUNOS" = ifelse(
      grepl("indisciplina dos alunos", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_DEMORA_PARA_ABRIR_O_PORTAO" = ifelse(
      grepl("demora para abrir o portão", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_FALTA_DE_PROFESSORES" = ifelse(
      grepl("falta de professores", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_FALTA_TRANSPORTE_ESCOLAR" = ifelse(
      grepl("falta transporte escolar", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_A_DISTANCIA_DE_CASA_ATE_A_ESCOLA_E_MUITO_GRANDE" = ifelse(
      grepl("a distância de casa até a escola é muito grande", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_FALTA_DE_EVENTOS_OU_ATIVIDADES_CULTURAIS" = ifelse(
      grepl("falta de eventos ou atividades culturais", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_ENSINO_RUIM" = ifelse(
      grepl("ensino ruim", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
    "Q43_OUTRO" = ifelse(
      grepl("outro", `43. Na sua opinião, quais são os maiores problemas da sua escola? (marcar até 3 opções)`),
      T, F),
  )

#Conta quantas vezes cada problema foi marcado
tbl3 <- df2 %>% 
  summarise(across(c(Q43_NENHUM:Q43_OUTRO), ~ sum(.x))) %>% 
  pivot_longer(Q43_NENHUM:Q43_OUTRO, names_to = "problema")

#Ordena a tabela de acordo com os problemas mais relatados
arrange(tbl3, desc(value))

#Cria gráfico dos problemas mais relatados
tbl3 %>% 
  ggplot(aes(y = value, x = reorder(problema, value))) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
