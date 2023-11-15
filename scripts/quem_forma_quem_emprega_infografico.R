####
# Bibliotecas e banco ----

library(tidyverse)
library(readxl)
library(gt)

# .----
####
# INEP ----

inep_limpo <- read_excel("inep/banco_de_dados/EDUCACAO_PROFISSIONAL_2015_2022_limpo.xlsx") # Banco Uniep

colnames(inep_limpo) <- tolower(colnames(inep_limpo)) # Por os nomes das varieis em letras minusculas

####
## Identificacao cursos saude ----

# Abrindo o banco

bd_tecnicos_saude <- read_excel("inep/banco_de_dados/cursos_tecnicos_saude.xlsx") |> 
  mutate(tecnico_saude = 1)

# Juntando os bancos

inep_limpo <- left_join(
  inep_limpo,
  bd_tecnicos_saude,
  by = join_by(no_curso_educacao_profissional)) 

# Fazendo variavel

inep_limpo <- inep_limpo |> 
  mutate(tecnico_saude = ifelse(is.na(tecnico_saude), 0, 1))

# Pondo alguns tecnicos que nao estavam vindo

inep_limpo$tecnico_saude <- ifelse(inep_limpo$no_curso_educacao_profissional %in%
                                     c("Análises Clínicas", "Outros - Eixo Ambiente e Saúde", "Dependência química"), 1, 
                                   inep_limpo$tecnico_saude)
# Salvando o banco

# save(inep_limpo,
#       file = "inep/banco_de_dados/inep_limpo.Rda") # Salvar em Rda


####
## Nome dos cursos ----

nome_cursos_tecnicos_inep <- as.data.frame(table(inep_limpo$no_curso_educacao_profissional)) |> 
  rename(nome_curso_tecnicos_15_22= Var1) |> 
  mutate(Freq = NULL)

# write.csv(
#   nome_cursos_tecnicos_inep,
#   "~/trabalho_profissionais/inep/resultados/nome_cursos_tecnicos_inep.csv",
#   row.names = FALSE,  # Evita a inclusão das linhas de contagem
#   fileEncoding = "latin1")


####
## Controle Outros - Eixo Ambiente e Saúde ----

eixo_saude_ambiente <- inep_limpo |> 
  filter(no_curso_educacao_profissional == "Outros - Eixo Ambiente e Saúde") |> 
  select (nu_ano_censo,
          nome_escola,
          no_regiao, 
          no_uf,
          rede,
          dependencia_administrativa, 
          no_area_curso_profissional,
          no_curso_educacao_profissional)

# write.csv(
#   eixo_saude_ambiente,
#   "~/trabalho_profissionais/inep/resultados/eixo_saude_ambiente.csv",
#   row.names = FALSE,  # Evita a inclusão das linhas de contagem
#   fileEncoding = "latin1")


## Pergunta 1: Quantas matriculas existem em cursos técnicos em 2022? ----

inep_limpo |> 
  filter (nu_ano_censo ==  2022) |> 
  summarise(soma_matriculas = sum(mat))

# Resposta: 2.152.506

## Pergunta 2: Dessas matriculas, quantas são em cursos técnicos em saúde em 2022? ----

inep_limpo |> 
  filter (nu_ano_censo ==  2022) |> 
  filter (tecnico_saude ==  1) |> 
  summarise(soma_matriculas = sum(mat))

518874/2152506*100

# Verificar os cursos que possuem matricula 

# a <- inep_limpo |> 
#   filter(tecnico_saude == 1) |> 
#   group_by(no_curso_educacao_profissional) |> 
#   summarise(n = n(),
#             # n = NULL,
#             presente = 1)
# 
# 
# aa <- left_join(
#   bd_tecnicos_saude,
#   a) 

# Resposta: 518.874 matriculas (24.1%)

## Pergunta 3: Das matriculas dos cursos técnicos em saúde, como se dispõe por natureza jurídica? ----

# Tabela

tabela_inep_1 <- inep_limpo |> 
  filter (nu_ano_censo ==  2022) |> 
  filter (tecnico_saude ==  1) |> 
  group_by(rede) |> 
  summarise(soma_matriculas = sum(mat)) |> 
  mutate(soma_total_natriculas = sum(soma_matriculas),
         perc = soma_matriculas/soma_total_natriculas * 100) 

# tabela_inep_1 <- inep_limpo |>
#   filter (nu_ano_censo ==  2022) |>
#   filter (tecnico_saude ==  1) |>
#   group_by(rede) |>
#   summarise(soma_matriculas = sum(mat)) |>
#   mutate(soma_total_natriculas = sum(soma_matriculas),
#          perc = soma_matriculas/soma_total_natriculas * 100) |>
#   gt()

# salvar tabela 

#gtsave(tabela_inep_1, "tabela_inep_1.png")

# Grafico 

tabela_inep_1  |> 
  ggplot(aes(x = rede , y= perc))+
  geom_col(position = "dodge", alpha= .5)+
  geom_text(aes(label = round(perc)), 
            position = position_dodge(width = 1), hjust = 0,vjust = 0,
            size = 3.2)+
  theme_classic()+
  labs(subtitle = "Matriculas dos cursos técnicos do eixo ambiente e saúde, por tipo de rede",
       caption = "Fonte: INEP. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
       x = NULL,
       y = "Percentual")

# ggsave("~/trabalho_profissionais/inep/resultados/matriculas_natureza_juridica.png", # Salvar
#          width = 20, height = 10, units = "cm")

## Pergunta 4: Como as matriculas dos cursos técnicos em saúde se dispõem por região? ----

tabela_inep_2 <- inep_limpo |> 
  filter (nu_ano_censo ==  2022) |> 
  filter (tecnico_saude ==  1) |> 
  group_by(no_regiao) |> 
  summarise(soma_matriculas = sum(mat)) |> 
  mutate(soma_total_natriculas = sum(soma_matriculas),
         perc = soma_matriculas/soma_total_natriculas * 100) 

# tabela_inep_2 <- inep_limpo |>
#   filter (nu_ano_censo ==  2022) |>
#   filter (tecnico_saude ==  1) |>
#   group_by(no_regiao) |>
#   summarise(soma_matriculas = sum(mat)) |>
#   mutate(soma_total_natriculas = sum(soma_matriculas),
#          perc = soma_matriculas/soma_total_natriculas * 100)  |>
#   gt()

# salvar tabela 

#gtsave(tabela_inep_2, "tabela_inep_2.png")

# Grafico 

tabela_inep_2  |> 
  
  ggplot(aes(x = fct_reorder(no_regiao, - perc) , y= perc))+
  geom_col(position = "dodge", alpha= .5)+
  geom_text(aes(label = round(perc)), 
            position = position_dodge(width = 1), hjust = 0,vjust = 0,
            size = 3.2)+
  theme_classic()+
  labs(subtitle = "Matriculas dos cursos técnicos do eixo ambiente e saúde, por região",
       caption = "Fonte: INEP. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
       x = NULL,
       y = "Percentual")

# ggsave("~/trabalho_profissionais/inep/resultados/matriculas_regiao.png", # Salvar
#         width = 20, height = 10, units = "cm")

## Pergunta 5: Como as matriculas dos cursos técnicos em saúde se dispõem por região e tipo de rede? ----

tabela_inep_3 <- inep_limpo |> 
  filter (nu_ano_censo ==  2022) |> 
  filter (tecnico_saude ==  1) |> 
  group_by(no_regiao, rede) |> 
  summarise(soma_matriculas = sum(mat)) |> 
  mutate(soma_total_natriculas = sum(soma_matriculas),
         perc = soma_matriculas/soma_total_natriculas * 100) 

# tabela_inep_3 <- inep_limpo |>
#   filter (nu_ano_censo ==  2022) |>
#   filter (tecnico_saude ==  1) |>
#   group_by(no_regiao, rede) |>
#   summarise(soma_matriculas = sum(mat)) |>
#   mutate(soma_total_natriculas = sum(soma_matriculas),
#          perc = soma_matriculas/soma_total_natriculas * 100) |>
#    gt()

# salvar tabela 

#gtsave(tabela_inep_3, "tabela_inep_3.png")

tabela_inep_3  |> 
  ggplot(aes(x = rede, y= perc)+
           geom_col(position = "dodge", alpha= .5)+
           facet_wrap(vars(no_regiao))+ 
           geom_text(aes(label = round(perc)), 
                     position = position_dodge(width = 1), hjust = 0,vjust = 0,
                     size = 3.2)+
           theme_classic()+
           labs(subtitle = "Matriculas dos cursos técnicos do eixo ambiente e saúde, por região e por tipo de rede",
                caption = "Fonte: INEP. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
                x = NULL,
                y = "Percentual")
         
         # ggsave("~/trabalho_profissionais/inep/resultados/matriculas_rede_regiao.png", # Salvar
         #        width = 24, height = 14, units = "cm")
         
         
         
         



# Carregar o banco de dados

load("~/trabalho_profissionais/2023/br_2023.Rda")

#. ----

####
##
# CNES ----

## Pergunta 1: Quantos vínculos de trabalho existem nos estabelecimentos de saúde em 2023? ----

# Vínculos totais: 5.360.547


# Vínculos de profissionais de saúde: 4.495.343

## Pergunta 2: Dos vinculos de profissionais de saúde existentes, quantos são de técnicos em saúde? ----

tecnicos <- br_2023_vari |> 
  group_by(tenico_em_saude_n) |> 
  summarise(quant = n()) |> 
  mutate(soma_quant = sum(quant),
         per = quant/soma_quant * 100,
         descricao = case_when(tenico_em_saude_n == 1 ~ "Técnicos eixo ambiente e sáude",
                               tenico_em_saude_n == 0 ~ "Demais profissionais"),
         tenico_em_saude_n = NULL)

# tabela

# tabela_1 <- tecnicos |>
#   rename(
#     Quantidade = quant,
#     Quantidade_total = soma_quant,
#     Percentual = per,
#     Descricao = descricao) |>
#   gt()

# 
# # Salvar tabela 
# 
#gtsave(tabela_1, "tabela_1.png")


# Grafico

tecnicos |> 
  ggplot(aes(x = descricao, y= per))+
  geom_col(position = "dodge", alpha= .5)+
  geom_text(aes(label = round(per)), 
            position = position_dodge(width = 1), hjust = 0,vjust = 0,
            size = 3.2)+
  theme_classic()  +
  labs(subtitle = "% correspondente aos técnicos do eixo ambiente e saúde no total de vínculos de trabalho",
       caption = "Fonte: CNES. Elaboração: Elaboração: Observatório dos Técnicos em Saúde (OTS).",
       x = NULL,
       y = "Percentual")

# Salvar

# ggsave("~/trabalho_profissionais/resultados/oficina_tecnicos/figura_1.png", # Salvar
#        width = 20, height = 10, units = "cm")

## Pergunta 3: Desses vínculos quantos estão distribuídos por SUS e não SUS? ----  

tabela_2 <- br_2023_vari |> # para saber o total de profissionais de saude
  filter(tenico_em_saude_n == 1) |>
  group_by(sus) |> 
  summarise(quant = n()) |> 
  mutate(soma_quant = sum(quant),
         per = quant/soma_quant * 100,
         descricao = case_when(sus == "S" ~ "SUS",
                               sus == "N" ~ "Não SUS"),
         sus = NULL)

# Tabela

# tabela_2 <- tabela_2 |>
#   rename(
#     Quantidade = quant,
#     Quantidade_total = soma_quant,
#     Percentual = per,
#     Descricao = descricao) |>
#   gt()
# 
# # Salvar tabela 
# 
# gtsave(tabela_2, "tabela_2.png")

# Graficando

tabela_2   |> 
  ggplot(aes(x = descricao, y= per))+
  geom_col(position = "dodge", alpha= .5)+
  geom_text(aes(label = round(per)), 
            position = position_dodge(width = 1), hjust = 0,vjust = 0,
            size = 3.2)+
  theme_classic()  +
  labs(subtitle = "% correspondente aos técnicos do eixo ambiente e saúde, por local de atuação",
       caption = "Fonte: CNES. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
       x = NULL,
       y = "Percentual")

# Salvar

# ggsave("~/trabalho_profissionais/resultados/oficina_tecnicos/figura_2.png", # Salvar
#        width = 20, height = 10, units = "cm")

## Pergunta 4: Desses vínculos quantos estão distribuídos por regiao? ---- 

tabela_3 <- br_2023_vari |> 
  filter(tenico_em_saude_n == 1) |>
  group_by(regiao) %>%
  summarise(quant=n()) |> 
  mutate(soma_quant = sum(quant),
         per = quant/soma_quant * 100) |> 
  rename(
    Quantidade = quant,
    Quantidade_total = soma_quant,
    Percentual = per,
    Região = regiao) 

# Tabela

tabela_3 <- br_2023 |>
  filter(tenico_em_saude_n == 1) |>
  group_by(regiao) %>%
  summarise(quant=n()) |>
  mutate(soma_quant = sum(quant),
         per = quant/soma_quant * 100) |>
  rename(
    Quantidade = quant,
    Quantidade_total = soma_quant,
    Percentual = per,
    Região = regiao) |>
  gt()

# Salvar tabela 

# gtsave(tabela_3, "tabela_3.png")

# Grafico

tabela_3 |> 
  filter(Região > 1) |> # Tirar os quatra casos
  ggplot(aes(x = fct_reorder(Região, - Percentual), y= round(Percentual)))+
  geom_col(position = "dodge", alpha= .5)+
  geom_text(aes(label = round(Percentual)), 
            position = position_dodge(width = 1), hjust = 0,vjust = 0,
            size = 3.2)+
  theme_classic()+
  labs(subtitle = "% correspondente aos técnicos do eixo ambiente e saúde, por região",
       caption = "Fonte: CNES. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
       x = NULL,
       y = "Percentual")

# ggsave("~/trabalho_profissionais/resultados/oficina_tecnicos/posto_trabalho_regiao.png", # Salvar
#        width = 20, height = 10, units = "cm")

## Pergunta 5: Desses vínculos quantos estão distribuídos por SUS NÃO SUS e por região e ? ----

# tabela 

tabela_5 <- br_2023 |> 
  filter(tenico_em_saude_n == 1) |>
  group_by(sus, regiao) %>%
  summarise(quant=n()) |> 
  mutate(soma = sum(quant),
         perc = round((quant/soma)*100),
         sus = case_when(sus == "S" ~ "SUS",
                         sus == "N" ~ "Não SUS")) |> 
  rename(
    Quantidade = quant,
    Quantidade_total_rede = soma,
    Percentual = perc,
    Região = regiao,
  ) |> 
  ungroup() |> 
  mutate(Quantidade_total_Brasil = sum(Quantidade))# Para sabe o valor total


# tabela_5 <- tabela_5  |>
#   gt()
# 
# # salvar tabela 
# 
# gtsave(tabela_5, "tabela_5.png")

# Grafico

tabela_5 |> 
  filter(Percentual > 1) |> # Tirar os quarenta casos
  
  ggplot(aes(x = Região , y= Percentual))+
  geom_col(position = "dodge", alpha= .5)+
  facet_wrap(vars(sus))+ 
  geom_text(aes(label = Percentual), 
            position = position_dodge(width = 1), hjust = 0,vjust = 0,
            size = 3.2)+
  theme_classic()+
  labs(subtitle = "% correspondente aos técnicos do eixo ambiente e saúde, por tipo de atuação e região",
       caption = "Fonte: CNES. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
       x = NULL,
       y = "Percentual")

# ggsave("~/trabalho_profissionais/resultados/oficina_tecnicos/posto_trabalho_regiao_sus.png", # Salvar
#        width = 20, height = 10, units = "cm")

## Pergunta 5.1 : Desses vínculos quantos estão distribuídos por região e por SUS NÃO SUS? ----

# tabela 

tabela_5_1 <- br_2023 |> 
  filter(tenico_em_saude_n == 1) |>
  group_by(regiao, sus) %>%
  summarise(quant=n()) |> 
  mutate(soma = sum(quant),
         perc = round((quant/soma)*100),
         sus = case_when(sus == "S" ~ "SUS",
                         sus == "N" ~ "Não SUS")) |> 
  rename(
    Quantidade = quant,
    Quantidade_total_regiao = soma,
    Percentual = perc,
    Região = regiao,
  ) |> 
  ungroup() |> 
  mutate(Quantidade_total_Brasil = sum(Quantidade))# Para sabe o valor total


# tabela_5_1 <- tabela_5_1 |>
#    gt()

# salvar tabela 

# gtsave(tabela_5_1, "tabela_5_1.png")

# Grafico

tabela_5_1 |>
  filter(Percentual > 1,
         Região != "") |> # Tirar os quarenta casos
  
  ggplot(aes(x = sus , y= Percentual))+
  geom_col(position = "dodge", alpha= .5)+
  facet_wrap(vars(Região))+
  geom_text(aes(label = Percentual),
            position = position_dodge(width = 1), hjust = 0,vjust = 0,
            size = 3.2)+
  theme_classic()+
  labs(subtitle = "% correspondente aos técnicos do eixo ambiente e saúde, por região e tipo de atuação",
       caption = "Fonte: CNES. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
       x = NULL,
       y = "Percentual")
# 
# ggsave("~/trabalho_profissionais/resultados/oficina_tecnicos/posto_trabalho_regiao_sus_certo.png", # Salvar
#        width = 25, height = 15, units = "cm")


## Pergunta 6: Desses vínculos de trabalho de técnicos em saúde quantas são ocupados por mulheres e quanto são ocupados por homens? ----  

tabela_6 <- br_2023 |> # para saber o total de profissionais de saude
  filter(tenico_em_saude_n == 1) |>
  group_by(sexo_probabilidade) |> 
  summarise(quant = n()) |>
  filter(sexo_probabilidade != "NA") |> 
  mutate(soma_quant = sum(quant),
         per = quant/soma_quant * 100) |> 
  rename(
    Quantidade = quant,
    Quantidade_total = soma_quant,
    Percentual = per,
    Sexo = sexo_probabilidade) 


# Para saber os nao classificados 

tabela_6_1 <- br_2023 |> # para saber o total de profissionais de saude
  filter(tenico_em_saude_n == 1) |>
  group_by(sexo_probabilidade) |>
  summarise(quant = n()) |>
  #filter(sexo_probabilidade != "NA") |>
  mutate(soma_quant = sum(quant),
         per = quant/soma_quant * 100) |>
  rename(
    Quantidade = quant,
    Quantidade_total = soma_quant,
    Percentual = per,
    Sexo = sexo_probabilidade) |>
  mutate(Sexo = case_when(
    Sexo == "Feminino" ~ "Feminino",
    Sexo == "Masculino" ~ "Masculino",
    is.na(Sexo) ~ "Não Classificado")) |>
  gt()


# Tabela

# tabela_6 <- tabela_6 |>
#   gt()

# Salvar tabela 

# gtsave(tabela_6, "tabela_6.png")

# Graficando

# tabela_6 |> 
#   ggplot(aes(x = Sexo, y= Percentual))+
#   geom_col(position = "dodge", alpha= .5)+
#   geom_text(aes(label = round(Percentual)), 
#             position = position_dodge(width = 1), hjust = 0,vjust = 0,
#             size = 3.2)+
#   theme_classic()  +
#   labs(subtitle = "% correspondente aos técnicos do eixo ambiente e saúde, por sexo",
#        caption = "Fonte: CNES. Elaboração: Observatório dos Técnicos em Saúde (OTS).",
#        x = NULL,
#        y = "Percentual")
# 
# ggsave("~/trabalho_profissionais/resultados/oficina_tecnicos/tecnicos_sexo.png", # Salvar
#        width = 20, height = 10, units = "cm")
