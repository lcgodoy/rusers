library(transpbrr)
library(dplyr)
library(ggplot2)
library(highcharter)

db_cpgf <-
  download_cp(year = 2014:2018,
              month = 1:12,
              type = "cpgf") %>%
  janitor::clean_names()

glimpse(db_cpgf)

# db_cpgf <- readRDS('data/example.rds')
# 
# glimpse(db_cpgf)

db_ministerio <- db_cpgf %>%
  group_by(nome_orgao_superior) %>%
  summarise(n = n(),
            vl_transacao = sum(valor_transacao)) %>%
  ungroup() %>%
  mutate(pc_transacao = vl_transacao / sum(vl_transacao)) %>%
  arrange(desc(vl_transacao))

tooltip <- 'Orgão: <b>{point.nome_orgao_superior}</b><br>
Valor:{point.vl_transacao}<br>
Percentual:{point.pc_transacao}<br>'

hctreemap2(
  db_ministerio,
  group_vars = c('nome_orgao_superior'),
  size_var = 'vl_transacao',
  color_var = 'pc_transacao',
  layoutAlgorithm = "squarified",
  levelIsConstant = F,
  allowDrillToNode = T,
  animationLimiti = 1000,
  dataLabels = list(
    enabled = F,
    color = 'black',
    shadow = F
  ),
  levels = list(list(
    level = 1,
    dataLabels = list(enabled = T)
  ))
) %>%
  hc_colorAxis(
    minColor = RColorBrewer::brewer.pal(9, "Greens")[1],
    maxColor = RColorBrewer::brewer.pal(9, "Greens")[9]
  ) %>%
  hc_tooltip(
    backgroundColor = "#e6ffe6",
    shared = TRUE,
    useHTML = T,
    borderWidth = 5,
    valueDecimals = 2,
    pointFormat = tooltip
  ) %>%
  widgetframe::frameWidget()


tooltip <- 'Orgão: <b>{point.nome_orgao_superior}</b><br>
Valor:{point.vl_transacao}<br>
Percentual:{point.pc_transacao}<br>'

hctreemap2(
  db_ministerio,
  group_vars = c('nome_orgao_superior'),
  size_var = 'vl_transacao',
  color_var = 'pc_transacao',
  layoutAlgorithm = "squarified",
  levelIsConstant = F,
  allowDrillToNode = T,
  animationLimiti = 1000,
  dataLabels = list(
    enabled = F,
    color = 'black',
    shadow = F
  ),
  levels = list(list(
    level = 1,
    dataLabels = list(enabled = T)
  ))
) %>%
  hc_colorAxis(
    minColor = RColorBrewer::brewer.pal(9, "Greens")[1],
    maxColor = RColorBrewer::brewer.pal(9, "Greens")[9]
  ) %>%
  hc_tooltip(
    backgroundColor = "#e6ffe6",
    shared = TRUE,
    useHTML = T,
    borderWidth = 5,
    valueDecimals = 2,
    pointFormat = tooltip
  ) %>%
  widgetframe::frameWidget()

#---- Gastos em Restaurantes ----

dados_restaurante <- db_cpgf %>%
  filter(stringr::str_detect(nome_favorecido, "RESTAURANTE|CHURRASCARIA|COCO BAMBU")) %>%
  mutate(data = lubridate::ymd(paste(ano_extrato, mes_extrato, "01", sep = '-'))) %>%
  mutate_if(is.character, stringr::str_to_title)

dados_restaurante %>%
  group_by(nome_orgao_superior) %>%
  summarise(vl_total = sum(valor_transacao)) %>%
  mutate(pc_total = vl_total / sum(vl_total)) %>%
  mutate(defesa = ifelse(nome_orgao_superior == "Ministerio Da Defesa", "Defesa", "Outro")) %>%
  ggplot(aes(x = defesa, y = pc_total)) +
  geom_bar(stat = 'identity', fill = '#FFDD34') +
  labs(x = 'Ministério', y = "Percentual") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_bw()

dados_restaurante %>%  
  arrange(desc(valor_transacao)) %>% 
  select(nome_orgao, nome_portador, 
         valor_transacao, nome_favorecido) %>% 
  as_tibble()
