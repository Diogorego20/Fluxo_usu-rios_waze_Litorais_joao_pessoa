# ==============================================================================
# SISTEMA AVANÇADO DE ANÁLISE DE DADOS WAZE - MOBILIDADE URBANA
# ==============================================================================
# Projeto: Análise de Tráfego e Mobilidade Urbana no Litoral de João Pessoa
# Autor: Diogo Rego - Estudante de Estatística
# Instituição: Universidade Federal da Paraíba (UFPB)
# Aplicação: DETRAN-PB - Políticas Públicas de Mobilidade Urbana
# Data: Dezembro 2024
# ==============================================================================

# REFERÊNCIAS BIBLIOGRÁFICAS
# [1] Box, G. E. P., & Jenkins, G. M. (2015). Time Series Analysis: Forecasting and Control
# [2] Hyndman, R. J., & Athanasopoulos, G. (2021). Forecasting: Principles and Practice
# [3] Anselin, L. (1995). Local Indicators of Spatial Association - LISA
# [4] Getis, A., & Ord, J. K. (1992). The Analysis of Spatial Association

# ==============================================================================
# 1. CONFIGURAÇÃO DO AMBIENTE
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════════════════╗\n")
cat("║  SISTEMA AVANÇADO DE ANÁLISE DE DADOS WAZE - MOBILIDADE URBANA           ║\n")
cat("║  Análise Estatística para Políticas Públicas de Trânsito                 ║\n")
cat("╚════════════════════════════════════════════════════════════════════════════╝\n")
cat("\n")

# Limpar ambiente
rm(list = ls())
gc()

# Configurar locale para português brasileiro
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")
Sys.setlocale("LC_NUMERIC", "C")

# Definir diretório de trabalho
if (!dir.exists("results")) dir.create("results", recursive = TRUE)
if (!dir.exists("img/plots")) dir.create("img/plots", recursive = TRUE)
if (!dir.exists("img/maps")) dir.create("img/maps", recursive = TRUE)
if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# ==============================================================================
# 2. INSTALAÇÃO E CARREGAMENTO DE PACOTES
# ==============================================================================

cat("📦 Verificando e instalando pacotes necessários...\n\n")

# Lista completa de pacotes
pacotes_necessarios <- c(
  # Manipulação de dados
  "dplyr", "tidyr", "data.table", "lubridate",
  
  # Análise estatística
  "forecast", "tseries", "zoo", "changepoint", "trend",
  "nortest", "car", "lmtest", "strucchange",
  
  # Visualização
  "ggplot2", "plotly", "viridis", "RColorBrewer", "scales",
  "gridExtra", "patchwork", "ggthemes", "gganimate",
  
  # Análise espacial
  "sf", "leaflet", "leaflet.extras", "sp", "spdep",
  "spatstat", "mapview",
  
  # Mapas
  "ggmap", "maps", "mapdata", "raster",
  
  # Relatórios
  "htmlwidgets", "htmltools", "knitr", "rmarkdown",
  
  # APIs e dados
  "httr", "jsonlite", "curl"
)

# Instalar pacotes faltantes
pacotes_faltantes <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[,"Package"])]

if(length(pacotes_faltantes) > 0) {
  cat("📥 Instalando pacotes:", paste(pacotes_faltantes, collapse = ", "), "\n\n")
  install.packages(pacotes_faltantes, dependencies = TRUE, repos = "https://cloud.r-project.org/")
}

# Carregar pacotes silenciosamente
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(lubridate)
  library(forecast)
  library(tseries)
  library(zoo)
  library(changepoint)
  library(trend)
  library(nortest)
  library(car)
  library(lmtest)
  library(strucchange)
  library(ggplot2)
  library(plotly)
  library(viridis)
  library(RColorBrewer)
  library(scales)
  library(gridExtra)
  library(patchwork)
  library(ggthemes)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(sp)
  library(spdep)
  library(ggmap)
  library(maps)
  library(mapdata)
  library(htmlwidgets)
  library(htmltools)
  library(httr)
  library(jsonlite)
})

cat("✅ Todos os pacotes carregados com sucesso!\n\n")

# ==============================================================================
# 3. DEFINIÇÃO DAS REGIÕES E PARÂMETROS
# ==============================================================================

# Regiões do Litoral Norte de João Pessoa
regioes_litoral_norte <- list(
  "Bessa" = list(
    lat_min = -7.10, lat_max = -7.08,
    lon_min = -34.85, lon_max = -34.83,
    cor = "#1f77b4",
    descricao = "Área residencial com praias, restaurantes e comércio local",
    tipo_via_principal = "Avenida",
    populacao_estimada = 25000
  ),
  "Intermares" = list(
    lat_min = -7.12, lat_max = -7.10,
    lon_min = -34.86, lon_max = -34.84,
    cor = "#ff7f0e",
    descricao = "Praia popular com grande fluxo turístico e eventos",
    tipo_via_principal = "Orla",
    populacao_estimada = 18000
  ),
  "Camboinha" = list(
    lat_min = -7.14, lat_max = -7.12,
    lon_min = -34.87, lon_max = -34.85,
    cor = "#2ca02c",
    descricao = "Área residencial com praias mais tranquilas",
    tipo_via_principal = "Rua",
    populacao_estimada = 15000
  ),
  "Cabo Branco" = list(
    lat_min = -7.16, lat_max = -7.14,
    lon_min = -34.88, lon_max = -34.86,
    cor = "#d62728",
    descricao = "Ponto turístico famoso - Ponta do Seixas",
    tipo_via_principal = "Orla",
    populacao_estimada = 22000
  ),
  "Tambaú" = list(
    lat_min = -7.13, lat_max = -7.11,
    lon_min = -34.83, lon_max = -34.81,
    cor = "#9467bd",
    descricao = "Centro turístico com hotéis, restaurantes e vida noturna",
    tipo_via_principal = "Avenida",
    populacao_estimada = 30000
  )
)

# Regiões do Litoral Sul
regioes_litoral_sul <- list(
  "Penha" = list(
    lat_min = -7.17, lat_max = -7.15,
    lon_min = -34.82, lon_max = -34.80,
    cor = "#8c564b",
    descricao = "Bairro residencial próximo ao centro",
    tipo_via_principal = "Rua",
    populacao_estimada = 20000
  ),
  "Manaíra" = list(
    lat_min = -7.15, lat_max = -7.13,
    lon_min = -34.84, lon_max = -34.82,
    cor = "#e377c2",
    descricao = "Área nobre com shopping e comércio sofisticado",
    tipo_via_principal = "Avenida",
    populacao_estimada = 28000
  ),
  "Barra de Gramame" = list(
    lat_min = -7.23, lat_max = -7.21,
    lon_min = -34.82, lon_max = -34.80,
    cor = "#7f7f7f",
    descricao = "Praia natural com acesso por estrada",
    tipo_via_principal = "Rodovia",
    populacao_estimada = 5000
  )
)

# Combinar todas as regiões
regioes_completas <- c(regioes_litoral_norte, regioes_litoral_sul)

cat("📍 Regiões configuradas:\n")
cat("   - Litoral Norte:", length(regioes_litoral_norte), "regiões\n")
cat("   - Litoral Sul:", length(regioes_litoral_sul), "regiões\n")
cat("   - Total:", length(regioes_completas), "regiões\n\n")

# ==============================================================================
# 4. FUNÇÕES AUXILIARES
# ==============================================================================

# Função para mensagens formatadas
msg <- function(texto, tipo = "info") {
  icones <- list(
    info = "ℹ️",
    sucesso = "✅",
    erro = "❌",
    aviso = "⚠️",
    execucao = "🚀",
    mapa = "🗺️",
    grafico = "📊",
    relatorio = "📄",
    dados = "📈",
    estatistica = "📐"
  )
  
  icone <- ifelse(!is.null(icones[[tipo]]), icones[[tipo]], "•")
  cat(icone, " ", texto, "\n", sep = "")
}

# Função para formatar números
fmt_num <- function(x, decimais = 0) {
  format(round(x, decimais), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

# Função para criar linha divisória
linha <- function(tamanho = 80, char = "=") {
  cat(paste0(rep(char, tamanho), collapse = ""), "\n")
}

# Função para criar seção
secao <- function(titulo, tamanho = 80) {
  cat("\n")
  linha(tamanho)
  cat(titulo, "\n")
  linha(tamanho)
  cat("\n")
}

# ==============================================================================
# 5. GERAÇÃO DE DADOS SIMULADOS AVANÇADOS
# ==============================================================================

gerar_dados_waze_completo <- function(anos = 2020:2024, 
                                      meses = c(12, 1, 2),
                                      regioes = regioes_completas) {
  
  msg("Gerando dados simulados avançados...", "dados")
  
  set.seed(42)  # Reprodutibilidade
  dados_lista <- list()
  contador <- 1
  
  for(ano in anos) {
    cat("  📅 Processando ano:", ano, "\n")
    
    for(mes in meses) {
      # Ajuste de ano para janeiro e fevereiro
      ano_mes <- ifelse(mes == 12, ano, ano + 1)
      
      # Define período do mês
      data_inicio <- as.Date(paste(ano_mes, sprintf("%02d", mes), "01", sep = "-"))
      dias_mes <- days_in_month(data_inicio)
      data_fim <- as.Date(paste(ano_mes, sprintf("%02d", mes), dias_mes, sep = "-"))
      
      n_dias <- as.numeric(data_fim - data_inicio) + 1
      
      # Número de registros por dia (variável)
      n_registros_dia <- sample(80:200, n_dias, replace = TRUE)
      n_total <- sum(n_registros_dia)
      
      # Sequência de datas e horas
      datas_horas <- seq(
        from = as.POSIXct(paste(data_inicio, "05:00:00")),
        to = as.POSIXct(paste(data_fim, "23:59:59")),
        length.out = n_total
      )
      
      # Fatores sazonais (alta temporada)
      fator_sazonal <- case_when(
        mes == 12 ~ runif(n_total, 1.8, 2.5),  # Dezembro - Natal
        mes == 1 ~ runif(n_total, 2.0, 3.0),   # Janeiro - Férias
        mes == 2 ~ runif(n_total, 1.5, 2.2),   # Fevereiro - Carnaval
        TRUE ~ 1.0
      )
      
      # Crescimento anual (4% ao ano)
      crescimento_anual <- 1 + (ano - 2020) * 0.04
      
      # Criar dataframe base
      dados_mes <- data.frame(
        timestamp = as.numeric(datas_horas) * 1000,
        data_hora = datas_horas,
        data = as.Date(datas_horas),
        ano = ano_mes,
        mes = mes,
        dia = day(datas_horas),
        hora = hour(datas_horas),
        minuto = minute(datas_horas),
        dia_semana = wday(datas_horas, label = TRUE, abbr = FALSE, locale = "pt_BR.UTF-8"),
        dia_semana_num = wday(datas_horas),
        semana_ano = week(datas_horas),
        stringsAsFactors = FALSE
      )
      
      # Adicionar período do dia
      dados_mes <- dados_mes %>%
        mutate(
          periodo_dia = case_when(
            hora >= 5 & hora < 12 ~ "Manhã",
            hora >= 12 & hora < 18 ~ "Tarde",
            hora >= 18 & hora <= 23 ~ "Noite",
            TRUE ~ "Madrugada"
          ),
          periodo_dia = factor(periodo_dia, levels = c("Madrugada", "Manhã", "Tarde", "Noite"))
        )
      
      # Adicionar região com probabilidades
      nomes_regioes <- names(regioes)
      n_regioes <- length(nomes_regioes)
      
      # Probabilidades baseadas na população
      pop_total <- sum(sapply(regioes, function(x) x$populacao_estimada))
      prob_regioes <- sapply(regioes, function(x) x$populacao_estimada / pop_total)
      
      dados_mes$regiao <- sample(nomes_regioes, n_total, replace = TRUE, prob = prob_regioes)
      
      # Adicionar coordenadas geográficas
      dados_mes <- dados_mes %>%
        rowwise() %>%
        mutate(
          lat = runif(1, regioes[[regiao]]$lat_min, regioes[[regiao]]$lat_max),
          lon = runif(1, regioes[[regiao]]$lon_min, regioes[[regiao]]$lon_max)
        ) %>%
        ungroup()
      
      # Adicionar métricas de tráfego
      dados_mes <- dados_mes %>%
        mutate(
          # Número de usuários (Poisson com sazonalidade)
          usuarios = rpois(n_total, lambda = 20 * fator_sazonal * crescimento_anual),
          
          # Velocidade média (km/h) - depende do período e dia da semana
          velocidade_base = case_when(
            periodo_dia == "Manhã" & dia_semana_num %in% 2:6 ~ runif(n_total, 25, 40),  # Rush manhã
            periodo_dia == "Tarde" & dia_semana_num %in% 2:6 ~ runif(n_total, 20, 35),  # Rush tarde
            periodo_dia == "Noite" ~ runif(n_total, 45, 65),
            dia_semana_num %in% c(1, 7) ~ runif(n_total, 30, 50),  # Fim de semana
            TRUE ~ runif(n_total, 35, 55)
          ),
          
          velocidade_media = pmax(10, velocidade_base + rnorm(n_total, 0, 5)),
          
          # Nível de congestionamento (1-5)
          nivel_congestionamento = case_when(
            velocidade_media < 20 ~ 5L,
            velocidade_media < 30 ~ 4L,
            velocidade_media < 40 ~ 3L,
            velocidade_media < 50 ~ 2L,
            TRUE ~ 1L
          ),
          
          # Tempo de viagem (minutos) - correlacionado com congestionamento
          tempo_viagem = round(10 + (6 - nivel_congestionamento) * 5 + rnorm(n_total, 0, 2)),
          tempo_viagem = pmax(5, tempo_viagem),
          
          # Tipo de via
          tipo_via = sapply(regiao, function(r) {
            via_principal <- regioes[[r]]$tipo_via_principal
            sample(c(via_principal, "Avenida", "Rua", "Orla", "Rodovia"), 1,
                   prob = c(0.5, 0.2, 0.15, 0.1, 0.05))
          }),
          
          # Eventos de trânsito
          tem_acidente = rbinom(n_total, 1, 0.02),  # 2% de chance
          tem_obra = rbinom(n_total, 1, 0.05),      # 5% de chance
          tem_evento = rbinom(n_total, 1, 0.03)     # 3% de chance
        )
      
      # Adicionar eventos especiais
      dados_mes <- dados_mes %>%
        mutate(
          evento_especial = case_when(
            mes == 12 & dia >= 20 & dia <= 25 ~ "Natal",
            mes == 12 & dia >= 26 & dia <= 31 ~ "Réveillon",
            mes == 1 & dia <= 15 ~ "Férias de Verão",
            mes == 2 & dia >= 10 & dia <= 18 ~ "Carnaval",
            dia_semana_num %in% c(1, 7) ~ "Fim de Semana",
            TRUE ~ "Normal"
          )
        )
      
      # Ajustar métricas para eventos especiais
      dados_mes <- dados_mes %>%
        mutate(
          usuarios = case_when(
            evento_especial %in% c("Natal", "Réveillon", "Carnaval") ~ as.integer(usuarios * 1.5),
            evento_especial == "Férias de Verão" ~ as.integer(usuarios * 1.3),
            evento_especial == "Fim de Semana" ~ as.integer(usuarios * 1.2),
            TRUE ~ usuarios
          ),
          nivel_congestionamento = pmin(5L, nivel_congestionamento + 
                                          as.integer(evento_especial %in% c("Réveillon", "Carnaval")))
        )
      
      # Adicionar outliers (5% dos dados)
      n_outliers <- round(n_total * 0.05)
      if(n_outliers > 0) {
        idx_outliers <- sample(1:n_total, n_outliers)
        dados_mes$usuarios[idx_outliers] <- dados_mes$usuarios[idx_outliers] * sample(2:4, n_outliers, replace = TRUE)
        dados_mes$nivel_congestionamento[idx_outliers] <- 5L
        dados_mes$velocidade_media[idx_outliers] <- dados_mes$velocidade_media[idx_outliers] * 0.5
      }
      
      dados_lista[[contador]] <- dados_mes
      contador <- contador + 1
    }
  }
  
  # Combinar todos os dados
  dados_final <- bind_rows(dados_lista)
  
  # Adicionar identificador único
  dados_final$id <- 1:nrow(dados_final)
  
  # Reordenar colunas
  dados_final <- dados_final %>%
    select(id, timestamp, data_hora, data, ano, mes, dia, hora, minuto,
           dia_semana, dia_semana_num, semana_ano, periodo_dia,
           regiao, lat, lon, usuarios, velocidade_media, nivel_congestionamento,
           tempo_viagem, tipo_via, tem_acidente, tem_obra, tem_evento,
           evento_especial, everything())
  
  msg(paste("Dados gerados:", fmt_num(nrow(dados_final)), "registros"), "sucesso")
  cat("  📅 Período:", format(min(dados_final$data), "%d/%m/%Y"), 
      "a", format(max(dados_final$data), "%d/%m/%Y"), "\n")
  cat("  📍 Regiões:", length(unique(dados_final$regiao)), "\n")
  cat("  👥 Total de usuários:", fmt_num(sum(dados_final$usuarios)), "\n\n")
  
  return(dados_final)
}

# ==============================================================================
# 6. ANÁLISES ESTATÍSTICAS AVANÇADAS
# ==============================================================================

analise_estatistica_completa <- function(dados) {
  
  secao("📐 ANÁLISES ESTATÍSTICAS AVANÇADAS")
  
  resultados <- list()
  
  # ============================================================================
  # 6.1 Estatísticas Descritivas
  # ============================================================================
  
  msg("Calculando estatísticas descritivas...", "estatistica")
  
  estat_desc <- dados %>%
    summarise(
      n_registros = n(),
      n_dias = n_distinct(data),
      periodo_inicio = min(data),
      periodo_fim = max(data),
      
      # Usuários
      total_usuarios = sum(usuarios),
      media_usuarios = mean(usuarios),
      mediana_usuarios = median(usuarios),
      dp_usuarios = sd(usuarios),
      cv_usuarios = sd(usuarios) / mean(usuarios),
      min_usuarios = min(usuarios),
      max_usuarios = max(usuarios),
      q1_usuarios = quantile(usuarios, 0.25),
      q3_usuarios = quantile(usuarios, 0.75),
      iqr_usuarios = IQR(usuarios),
      
      # Velocidade
      media_velocidade = mean(velocidade_media, na.rm = TRUE),
      mediana_velocidade = median(velocidade_media, na.rm = TRUE),
      dp_velocidade = sd(velocidade_media, na.rm = TRUE),
      min_velocidade = min(velocidade_media, na.rm = TRUE),
      max_velocidade = max(velocidade_media, na.rm = TRUE),
      
      # Congestionamento
      media_congestionamento = mean(nivel_congestionamento),
      moda_congestionamento = as.numeric(names(sort(table(nivel_congestionamento), decreasing = TRUE)[1])),
      
      # Tempo de viagem
      media_tempo_viagem = mean(tempo_viagem, na.rm = TRUE),
      mediana_tempo_viagem = median(tempo_viagem, na.rm = TRUE),
      
      # Eventos
      taxa_acidentes = sum(tem_acidente) / n() * 100,
      taxa_obras = sum(tem_obra) / n() * 100,
      taxa_eventos = sum(tem_evento) / n() * 100
    )
  
  resultados$estatisticas_descritivas <- estat_desc
  
  # Estatísticas por região
  estat_regiao <- dados %>%
    group_by(regiao) %>%
    summarise(
      n_registros = n(),
      total_usuarios = sum(usuarios),
      media_usuarios = mean(usuarios),
      media_velocidade = mean(velocidade_media, na.rm = TRUE),
      media_congestionamento = mean(nivel_congestionamento),
      taxa_acidentes = sum(tem_acidente) / n() * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(total_usuarios))
  
  resultados$estatisticas_regiao <- estat_regiao
  
  cat("  ✓ Estatísticas descritivas calculadas\n")
  cat("  ✓ Estatísticas por região calculadas\n\n")
  
  # ============================================================================
  # 6.2 Testes de Normalidade
  # ============================================================================
  
  msg("Realizando testes de normalidade...", "estatistica")
  
  # Teste de Shapiro-Wilk (amostra de 5000 observações)
  amostra_usuarios <- sample(dados$usuarios, min(5000, nrow(dados)))
  amostra_velocidade <- sample(dados$velocidade_media, min(5000, nrow(dados)))
  
  teste_shapiro_usuarios <- shapiro.test(amostra_usuarios)
  teste_shapiro_velocidade <- shapiro.test(amostra_velocidade)
  
  # Teste de Anderson-Darling
  teste_ad_usuarios <- ad.test(dados$usuarios)
  teste_ad_velocidade <- ad.test(dados$velocidade_media)
  
  resultados$testes_normalidade <- list(
    shapiro_usuarios = teste_shapiro_usuarios,
    shapiro_velocidade = teste_shapiro_velocidade,
    anderson_darling_usuarios = teste_ad_usuarios,
    anderson_darling_velocidade = teste_ad_velocidade
  )
  
  cat("  ✓ Testes de normalidade concluídos\n\n")
  
  # ============================================================================
  # 6.3 Análise de Correlação
  # ============================================================================
  
  msg("Calculando correlações entre variáveis...", "estatistica")
  
  dados_cor <- dados %>%
    select(usuarios, velocidade_media, nivel_congestionamento, 
           tempo_viagem, hora, dia_semana_num) %>%
    na.omit()
  
  matriz_correlacao <- cor(dados_cor, method = "spearman")
  
  resultados$matriz_correlacao <- matriz_correlacao
  
  cat("  ✓ Matriz de correlação calculada\n\n")
  
  # ============================================================================
  # 6.4 Testes de Hipóteses
  # ============================================================================
  
  msg("Realizando testes de hipóteses...", "estatistica")
  
  # H0: Não há diferença na velocidade média entre dias úteis e fins de semana
  dados_teste <- dados %>%
    mutate(tipo_dia = ifelse(dia_semana_num %in% c(1, 7), "Fim de Semana", "Dia Útil"))
  
  teste_wilcox_velocidade <- wilcox.test(
    velocidade_media ~ tipo_dia,
    data = dados_teste,
    alternative = "two.sided"
  )
  
  # H0: Não há diferença no número de usuários entre regiões
  teste_kruskal_usuarios <- kruskal.test(usuarios ~ regiao, data = dados)
  
  # H0: Não há diferença no congestionamento entre períodos do dia
  teste_kruskal_congestionamento <- kruskal.test(nivel_congestionamento ~ periodo_dia, data = dados)
  
  resultados$testes_hipoteses <- list(
    wilcox_velocidade = teste_wilcox_velocidade,
    kruskal_usuarios_regiao = teste_kruskal_usuarios,
    kruskal_congestionamento_periodo = teste_kruskal_congestionamento
  )
  
  cat("  ✓ Testes de hipóteses concluídos\n\n")
  
  return(resultados)
}

# Continua na próxima parte...
