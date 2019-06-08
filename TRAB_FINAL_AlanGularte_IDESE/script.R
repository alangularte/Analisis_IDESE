{
  install.packages('readr')
  install.packages('skimr')
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(skimr)
  
  idese <- readRDS('data/Idese Municipios 2007-2014.rds')
  
  # Sumário rápido do dataset
  idese %>% 
    skim()
  
  # Exibir o nome das colunas
  colnames(idese)
  
  # Remoção de caracteres especiais das colunas
  names(idese) <- make.names(names(idese))
  
  # Deixar as colunas em lowcase
  names(idese) <- tolower(names(idese))
  
  # Renomear algumas colunas
  idese <- idese %>% 
    rename(
      bl_edu = bloco.educação,
      bl_edu_ef = bloco.educação.ensino.fundamental,           
      bl_edu_ef_af = bloco.educação.ensino.fundamental.anos.finais,
      bl_edu_ef_ai = bloco.educação.ensino.fundamental.anos.iniciais,
      bl_edu_em = bloco.educação.ensino.médio,
      bl_edu_ea = bloco.educação.escolaridade.adulta,
      bl_edu_pre = bloco.educação.pré.escola,
      bl_rd = bloco.renda,
      bl_rd_aprop = bloco.renda.apropriação.da.renda,
      bl_rd_ger = bloco.renda.geração.da.renda,
      bl_sd = bloco.saúde,
      bl_sd_cond_gerais = bloco.saúde.condições.gerais.de.saúde,
      bl_sd_obitos_evitaveis = bloco.saúde.condições.gerais.de.saúde.óbitos.por.causas.evitáveis,
      bl_sd_obitos_mal_definido = bloco.saúde.condições.gerais.de.saúde.óbitos.por.causas.mal.definidas,
      bl_sd_long = bloco.saúde.longevidade,
      bl_saude_materno_infantil = bloco.saúde.saúde.materno.infantil,                                  
      bl_sd_consult_pre_natal = bloco.saúde.saúde.materno.infantil.consultas.pré.natal,
      bl_sd_morte_menor_5anos = bloco.saúde.saúde.materno.infantil.mortalidade.de.menores.de.5.anos
    )
  
  # Desempenho Nota da Prova Brasil dos alunos de anos iniciais (1º ao 5º ano)
  idese %>% 
    group_by(ano) %>% 
    summarise(media = mean(bl_edu_ef_ai, na.rm = TRUE)) %>%
    ggplot(aes(x = ano, y = media)) +
    geom_line() +
    ylim(0,1)
  
  # Nota da Prova Brasil dos alunos de anos finais (6º ao 9º ano)
  idese %>% 
    group_by(ano) %>% 
    summarise(media = mean(bl_edu_em, na.rm = TRUE)) %>%
    ggplot(aes(x = ano, y = media)) +
    geom_line() +
    ylim(0,1)
  
  # Taxa de matrícula no Ensino Médio
  idese %>% 
    group_by(ano) %>% 
    summarise(media = mean(bl_edu_ef_af, na.rm = TRUE)) %>%
    ggplot(aes(x = ano, y = media)) +
    geom_line() +
    ylim(0,1)
  
  # Percentual da população adulta com pelo menos o Ensino Fundamental completo
  idese %>% 
    group_by(ano) %>% 
    summarise(media = mean(bl_edu_ea, na.rm = TRUE)) %>%
    ggplot(aes(x = ano, y = media)) +
    geom_line() +
    ylim(0,1)
  
  # Renda Domiciliar per capita média
  idese %>% 
    group_by(ano) %>% 
    summarise(media = mean(bl_rd, na.rm = TRUE)) %>%
    ggplot(aes(x = ano, y = media)) +
    geom_line() +
    ylim(0,1)
  
  # PIB per capita
  idese %>% 
    group_by(ano) %>% 
    summarise(media = mean(bl_rd_ger, na.rm = TRUE)) %>%
    ggplot(aes(x = ano, y = media)) +
    geom_line() +
    ylim(0,1)
  
}
