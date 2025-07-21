#-----------------IMPORTANDO BASE DE DADOS----------
# ATIVANDO PACOTE
library(readr)
library(dplyr)
library(tidyr)

#Importando dados com enconding apropriado
base = read_csv2(file = "microdados_ed_basica_2024.csv",
                 locale=locale(encoding = "ISO-8859-1"))

#Selecionando váriaveis específicas da base para criação dos indicadores
base_ind <- base |>
  dplyr::filter(SG_UF == "RJ",
                QT_MAT_BAS>0,
                TP_SITUACAO_FUNCIONAMENTO==1) |>
  dplyr::select(CO_MUNICIPIO,NO_MUNICIPIO,TP_DEPENDENCIA, 
                 IN_AGUA_POTAVEL, IN_PROF_SAUDE,
                IN_PROF_NUTRICIONISTA, IN_PROF_PSICOLOGO)

#TRATAMENTO DE VARIÁVEIS QUALITATIVAS
base_ind <- base_ind |>

  #recodificando  a variável TP_DEPENDENCIA
  dplyr::mutate(TP_DEPENDENCIA = case_when(
    TP_DEPENDENCIA == 1 ~"Federal",
    TP_DEPENDENCIA == 2 ~"Estadual",
    TP_DEPENDENCIA == 3 ~"Municipal",
    TP_DEPENDENCIA == 4 ~"Privada"
  )) |>
  

  #recodificando a variável IN_AGUA_POTAVEL
  mutate(IN_AGUA_POTAVEL= case_when(
    IN_AGUA_POTAVEL == 0 ~ "Não",
    IN_AGUA_POTAVEL== 1 ~ "Sim"
  ))  |>
  #recodificando a variável IN_PROF_SAUDE
  mutate(IN_PROF_SAUDE= case_when(
    IN_PROF_SAUDE == 0 ~ "Não",
    IN_PROF_SAUDE== 1 ~ "Sim"
  ))  |>
  #recodificando a variável IN_PROF_NUTRICIONISTA
  mutate(IN_PROF_NUTRICIONISTA= case_when(
    IN_PROF_NUTRICIONISTA == 0 ~ "Não",
    IN_PROF_NUTRICIONISTA == 1 ~ "Sim"
  ))  |>
  #recodificando a variável IN_PROF_PSICOLOGO
  mutate(IN_PROF_PSICOLOGO= case_when(
    IN_PROF_PSICOLOGO == 0 ~ "Não",
    IN_PROF_PSICOLOGO == 1 ~ "Sim"
  ))

base_ind <- base_ind |>
  mutate(
    TIPO_DEPENDENCIA = case_when(
      TP_DEPENDENCIA %in% c("Federal", "Estadual", "Municipal") ~ "Publica",
      TP_DEPENDENCIA == "Privada" ~ "Privada",
      TRUE ~ NA_character_
    )) |>
  mutate(PSICO_OU_NUTRI = IN_PROF_NUTRICIONISTA == 'Sim' | IN_PROF_PSICOLOGO == 'Sim') |>
  mutate(PSICO_OU_NUTRI = case_when(
    PSICO_OU_NUTRI == TRUE ~ 'Sim',
    PSICO_OU_NUTRI == FALSE ~ 'Não'
  ))

#---------------------Criação dos Indicadores-----------------------------------
## Função que auxilia  a criação de alguns indicadores----
calcular_percentual <- function(base, var, dependencia, nome_perc){
  base |>
    filter(TIPO_DEPENDENCIA == dependencia) |>
    group_by(CO_MUNICIPIO, NO_MUNICIPIO) |>
    summarise(
      quant = sum({{var}} == 'Sim', na.rm = TRUE),
      tem_dado = any(!is.na({{var}})),
      .groups = 'drop'
    ) |>
    right_join(base_escolas, by = 'NO_MUNICIPIO') |>
    mutate(
      !!nome_perc := case_when(
        is.na(tem_dado) | !tem_dado ~ NA_real_,
        .data[[dependencia]] == 0 ~ 0,
        TRUE ~ round(quant / .data[[dependencia]], 4) * 100
      )
    )
}

baseteste <- base_ind %>% 
  filter(TIPO_DEPENDENCIA == 'Publica') %>% 
  filter(IN_PROF_SAUDE == 'Sim')

## Criando a base group----
base_group <- base_ind |>
  group_by(CO_MUNICIPIO, NO_MUNICIPIO, TIPO_DEPENDENCIA) |>
  summarise(
    n = n(),
    .groups = 'drop'
  )

# Transformando para o formato wide as observações de escolas por município
base_escolas <- base_group |>
  tidyr::pivot_wider(
    names_from = 'TIPO_DEPENDENCIA',
    values_from = n,
    values_fill = 0
  )

# Criando uma variável pública (Estadual, Federal e Municipal) e total
base_escolas <- base_escolas |>
  mutate(Total = Privada+Publica)


# -------------------------------Convênios--------------------------------------
## ind_16----Percentual de Escolas Públicas dentro do Município
ind_16 <- base_escolas |>
  mutate(ind_16 = round(Publica/Total, 4)*100)


## ind-17----Percentual de Escolas Privadas dentro do Município
ind_17<- base_escolas |>
  mutate(ind_17 = round(Privada/Total, 4)*100)

#-------- (percentual de escolas públicas que possuem profissionais da saúde)----------
## ind_18----
ind_18 <- calcular_percentual(
  base = base_ind,
  var = IN_PROF_SAUDE,
  dependencia = 'Publica',
  nome_perc = 'ind_18'
)
#-------- (percentual de escolas privadas que possuem profissionais da saúde)----------
## ind_19----
ind_19 <- calcular_percentual(
  base = base_ind,
  var = IN_PROF_SAUDE,
  dependencia = 'Privada',
  nome_perc = 'ind_19'
)

#------- (percentual de escolas públicas que possuem água potável)---------------------
## ind_20----
ind_20 <- calcular_percentual(
  base = base_ind,
  var = IN_AGUA_POTAVEL,
  dependencia = 'Publica',
  nome_perc = 'ind_20'
)
#------- (percentual de escolas privadas que possuem água potável)---------------------
## IND_21----
ind_21<- calcular_percentual(
  base = base_ind,
  var = IN_AGUA_POTAVEL,
  dependencia = 'Privada',
  nome_perc = 'ind_21'
)

#---- (percentual de escolas públicas que possuem nutricionistas ou psicólogos)--------

ind_22 <- calcular_percentual(
  base = base_ind,
  var = PSICO_OU_NUTRI,
  dependencia = 'Publica',
  nome_perc = 'ind_22'
)
#---- (percentual de escolas privadas que possuem nutricionistas ou psicólogos)--------

ind_23 <- calcular_percentual(
  base = base_ind,
  var = PSICO_OU_NUTRI,
  dependencia = 'Privada',
  nome_perc = 'ind_23'
)

#------------------------------Tabela Final-------------------------------------
## Tratando das bases antes de juntar à uma base final----
base_escolas <- base_escolas |>
  select(CO_MUNICIPIO, NO_MUNICIPIO)

ind_16 <- ind_16 |>
  select(NO_MUNICIPIO, ind_16)

ind_17 <- ind_17 |>
  select(NO_MUNICIPIO, ind_17)

ind_18 <- ind_18 |>
  select(NO_MUNICIPIO, ind_18)

ind_19 <- ind_19 |>
  select(NO_MUNICIPIO, ind_19)

ind_20 <- ind_20 |>
  select(NO_MUNICIPIO, ind_20)

ind_21 <- ind_21 |>
  select(NO_MUNICIPIO, ind_21)

ind_22<- ind_22 |>
  select(NO_MUNICIPIO, ind_22)

ind_23 <- ind_23 |>
  select(NO_MUNICIPIO, ind_23)

## Fazendo join e criando a base final----

base_final <- base_escolas |>
  left_join(ind_16, by = 'NO_MUNICIPIO') |>
  left_join(ind_17, by = 'NO_MUNICIPIO') |>
  left_join(ind_18, by = 'NO_MUNICIPIO') |>
  left_join(ind_19, by = 'NO_MUNICIPIO') |>
  left_join(ind_20, by = 'NO_MUNICIPIO') |>
  left_join(ind_21, by = 'NO_MUNICIPIO') |>
  left_join(ind_22, by = 'NO_MUNICIPIO') |>
  left_join(ind_23, by = 'NO_MUNICIPIO')


