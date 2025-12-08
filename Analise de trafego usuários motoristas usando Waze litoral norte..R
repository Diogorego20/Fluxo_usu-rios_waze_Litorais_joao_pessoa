# ============================================================================
# SISTEMA DE ANÁLISE DE DADOS WAZE - LITORAL NORTE JOÃO PESSOA
# Versão corrigida com todas as funções atualizadas
# ============================================================================

# 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES
# ============================================================================
cat("🔄 Iniciando sistema de análise Waze...\n")
cat("📦 Verificando e instalando pacotes necessários...\n\n")

# Lista de pacotes necessários
pacotes_necessarios <- c("httr", "jsonlite", "dplyr", "lubridate", "tidyr", 
                         "ggplot2", "sf", "ggmap", "leaflet", "plotly",
                         "viridis", "gridExtra", "zoo", "forecast", "tseries",
                         "maps", "mapdata", "RColorBrewer", "scales", "htmlwidgets",
                         "leaflet.extras")  # Adicionado para heatmap

# Instalar pacotes faltantes
pacotes_instalados <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[,"Package"])]
if(length(pacotes_instalados) > 0) {
  cat("📥 Instalando pacotes:", paste(pacotes_instalados, collapse = ", "), "\n")
  install.packages(pacotes_instalados, dependencies = TRUE)
}

# Carregar pacotes
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(sf)
library(ggmap)
library(leaflet)
library(plotly)
library(viridis)
library(gridExtra)
library(zoo)
library(forecast)
library(tseries)
library(maps)
library(mapdata)
library(RColorBrewer)
library(scales)
library(htmlwidgets)
library(leaflet.extras)  # Para heatmap

cat("✅ Todos os pacotes foram carregados com sucesso!\n\n")

# Configurar locale para formatação de números
tryCatch({
  Sys.setlocale("LC_NUMERIC", "C")  # Usar locale padrão para evitar problemas
}, error = function(e) {
  cat("⚠️  Não foi possível configurar o locale. Continuando...\n")
})

# 2. DEFINIÇÃO DAS REGIÕES
# ============================================================================
regioes_litoral_norte <- list(
  "Bessa" = list(
    lat_min = -7.10, lat_max = -7.08,
    lon_min = -34.85, lon_max = -34.83,
    cor = "#1f77b4",
    descricao = "Área com praias, restaurantes e comércio local"
  ),
  "Intermares" = list(
    lat_min = -7.12, lat_max = -7.10,
    lon_min = -34.86, lon_max = -34.84,
    cor = "#ff7f0e",
    descricao = "Praia popular com grande fluxo turístico"
  ),
  "Camboinha" = list(
    lat_min = -7.14, lat_max = -7.12,
    lon_min = -34.87, lon_max = -34.85,
    cor = "#2ca02c",
    descricao = "Área residencial e praias mais tranquilas"
  ),
  "Cabo Branco" = list(
    lat_min = -7.16, lat_max = -7.14,
    lon_min = -34.88, lon_max = -34.86,
    cor = "#d62728",
    descricao = "Famoso por sua orla e ponta do Cabo Branco"
  ),
  "Tambaú" = list(
    lat_min = -7.13, lat_max = -7.11,
    lon_min = -34.83, lon_max = -34.81,
    cor = "#9467bd",
    descricao = "Centro turístico com hotéis e restaurantes"
  )
)

# 3. FUNÇÕES AUXILIARES
# ============================================================================
linha_divisoria <- function(tamanho = 60, caractere = "=") {
  cat(paste0(rep(caractere, tamanho), collapse = ""), "\n")
}

titulo_secao <- function(titulo, tamanho = 60, caractere = "=") {
  cat("\n")
  linha_divisoria(tamanho, caractere)
  cat(titulo, "\n")
  linha_divisoria(tamanho, caractere)
  cat("\n")
}

mensagem_status <- function(mensagem, tipo = "info") {
  icones <- list(
    info = "ℹ️",
    sucesso = "✅",
    erro = "❌",
    aviso = "⚠️",
    execucao = "🚀",
    mapa = "🗺️",
    grafico = "📊",
    relatorio = "📄",
    dados = "📈"
  )
  
  icone <- icones[[tipo]]
  if (is.null(icone)) icone <- "•"
  
  cat(icone, " ", mensagem, "\n")
}

# Função para formatar números (resolver conflito de locale)
formatar_numero <- function(x) {
  if (is.numeric(x)) {
    return(format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE))
  }
  return(x)
}

# 4. FUNÇÃO PARA GERAR DADOS SIMULADOS (CORRIGIDA)
# ============================================================================
gerar_dados_waze_5anos <- function(anos = 2020:2024, meses = c(12, 1, 2)) {
  mensagem_status(paste("Gerando dados simulados para", length(anos), "anos..."), "dados")
  
  set.seed(123)
  dados_completos <- list()
  
  for(ano in anos) {
    cat("  Processando ano:", ano, "\n")
    
    for(mes in meses) {
      # Ajuste para janeiro e fevereiro do ano seguinte
      ano_mes <- ifelse(mes == 12, ano, ano + 1)
      
      # Define período do mês
      data_inicio <- as.Date(paste(ano_mes, sprintf("%02d", mes), "01", sep = "-"))
      if(mes == 12) {
        data_fim <- as.Date(paste(ano_mes, "12", "31", sep = "-"))
      } else {
        data_fim <- as.Date(paste(ano_mes, sprintf("%02d", mes), 
                                  days_in_month(data_inicio), sep = "-"))
      }
      
      n_dias <- as.numeric(data_fim - data_inicio) + 1
      n_registros_dia <- sample(50:150, n_dias, replace = TRUE)
      n_total <- sum(n_registros_dia)
      
      # Cria sequência de datas e horas
      datas_horas <- seq(
        from = as.POSIXct(paste(data_inicio, "06:00:00")),
        to = as.POSIXct(paste(data_fim, "22:00:00")),
        length.out = n_total
      )
      
      # Fatores sazonais
      if(mes %in% c(12, 1)) {
        fator_sazonal <- runif(n_total, 1.5, 2.5)
      } else if(mes == 2) {
        fator_sazonal <- runif(n_total, 1.2, 1.8)
      } else {
        fator_sazonal <- 1
      }
      
      # Crescimento anual
      crescimento_anual <- 1 + (ano - 2020) * 0.04
      
      # Gera dados básicos - FASE 1
      dados_mes <- data.frame(
        timestamp = as.numeric(datas_horas) * 1000,
        data_hora = datas_horas,
        ano = ano_mes,
        mes = mes,
        dia_semana = wday(datas_horas, label = TRUE, abbr = FALSE, locale = "pt_BR.UTF-8")
      )
      
      # FASE 2: Adicionar hora e periodo_dia (CORREÇÃO APLICADA)
      dados_mes <- dados_mes %>%
        mutate(
          hora = hour(data_hora),
          periodo_dia = case_when(
            hora >= 6 & hora < 12 ~ "Manhã",
            hora >= 12 & hora < 18 ~ "Tarde",
            hora >= 18 & hora < 24 ~ "Noite",
            TRUE ~ "Madrugada"
          )
        )
      
      # Adiciona região aleatória
      regioes <- names(regioes_litoral_norte)
      prob_regioes <- c(0.25, 0.20, 0.15, 0.25, 0.15)
      dados_mes$regiao <- sample(regioes, n_total, replace = TRUE, prob = prob_regioes)
      
      # FASE 3: Adicionar coordenadas e métricas
      dados_mes <- dados_mes %>%
        rowwise() %>%
        mutate(
          lat = runif(1, 
                      regioes_litoral_norte[[regiao]]$lat_min,
                      regioes_litoral_norte[[regiao]]$lat_max),
          lon = runif(1,
                      regioes_litoral_norte[[regiao]]$lon_min,
                      regioes_litoral_norte[[regiao]]$lon_max),
          usuarios = round(rpois(1, lambda = 15 * fator_sazonal * crescimento_anual)),
          velocidade_media = case_when(
            periodo_dia == "Manhã" ~ runif(1, 30, 50),
            periodo_dia == "Tarde" ~ runif(1, 20, 40),
            periodo_dia == "Noite" ~ runif(1, 40, 60),
            TRUE ~ runif(1, 50, 70)
          ),
          nivel_congestionamento = case_when(
            usuarios > 20 ~ sample(4:5, 1, prob = c(0.3, 0.7)),
            usuarios > 10 ~ sample(2:3, 1, prob = c(0.4, 0.6)),
            TRUE ~ 1
          ),
          tipo_via = sample(c("Avenida", "Rua", "Orla", "Rodovia"), 1, 
                            prob = c(0.4, 0.3, 0.2, 0.1))
        ) %>%
        ungroup()
      
      # Adicionar eventos especiais
      dados_mes <- dados_mes %>%
        mutate(
          evento_especial = case_when(
            month(data_hora) == 12 & day(data_hora) >= 20 & day(data_hora) <= 31 ~
              sample(c("Natal", "Réveillon", "Férias"), 1, prob = c(0.4, 0.4, 0.2)),
            month(data_hora) == 1 & day(data_hora) <= 6 ~ "Férias",
            month(data_hora) == 2 & day(data_hora) >= 10 & day(data_hora) <= 18 ~ "Carnaval",
            TRUE ~ NA_character_
          )
        )
      
      # Adiciona outliers (5%)
      n_outliers <- round(n_total * 0.05)
      if(n_outliers > 0) {
        outliers_idx <- sample(1:n_total, n_outliers)
        dados_mes$usuarios[outliers_idx] <- dados_mes$usuarios[outliers_idx] * 
          sample(2:3, n_outliers, replace = TRUE)
        dados_mes$nivel_congestionamento[outliers_idx] <- 5
      }
      
      dados_completos[[paste(ano_mes, mes, sep = "-")]] <- dados_mes
    }
  }
  
  # Combina todos os dados
  dados_finais <- bind_rows(dados_completos)
  
  mensagem_status(paste("Dados gerados:", formatar_numero(nrow(dados_finais)), "registros"), "sucesso")
  cat("  Período:", format(min(dados_finais$data_hora), "%d/%m/%Y"), 
      "a", format(max(dados_finais$data_hora), "%d/%m/%Y"), "\n")
  cat("  Regiões:", paste(unique(dados_finais$regiao), collapse = ", "), "\n")
  
  return(dados_finais)
}

# 5. FUNÇÃO PARA GERAR MAPA INTERATIVO (CORRIGIDA)
# ============================================================================
gerar_mapa_interativo <- function(dados) {
  mensagem_status("Gerando mapa interativo...", "mapa")
  
  # Agrega dados por região
  dados_agregados <- dados %>%
    group_by(regiao) %>%
    summarise(
      lat_media = mean(lat, na.rm = TRUE),
      lon_media = mean(lon, na.rm = TRUE),
      total_usuarios = sum(usuarios, na.rm = TRUE),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      media_congestionamento = mean(nivel_congestionamento, na.rm = TRUE),
      n_registros = n(),
      .groups = "drop"
    ) %>%
    left_join(
      data.frame(
        regiao = names(regioes_litoral_norte),
        cor = sapply(regioes_litoral_norte, function(x) x$cor),
        descricao = sapply(regioes_litoral_norte, function(x) x$descricao)
      ),
      by = "regiao"
    )
  
  # Cria mapa leaflet
  mapa <- leaflet(dados_agregados) %>%
    addTiles() %>%
    setView(lng = -34.85, lat = -7.12, zoom = 12) %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # Adiciona marcadores para cada região
  for(i in 1:nrow(dados_agregados)) {
    mapa <- mapa %>%
      addCircleMarkers(
        lng = dados_agregados$lon_media[i],
        lat = dados_agregados$lat_media[i],
        radius = sqrt(dados_agregados$total_usuarios[i]) / 20,
        color = dados_agregados$cor[i],
        fillColor = dados_agregados$cor[i],
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 2,
        popup = paste(
          "<strong>", dados_agregados$regiao[i], "</strong><br>",
          "<hr>",
          "Total de Usuários: <b>", formatar_numero(dados_agregados$total_usuarios[i]), "</b><br>",
          "Velocidade Média: <b>", round(dados_agregados$velocidade_media[i], 1), "km/h</b><br>",
          "Nível de Congestionamento: <b>", round(dados_agregados$media_congestionamento[i], 1), "/5</b><br>",
          "Registros: <b>", formatar_numero(dados_agregados$n_registros[i]), "</b><br>",
          "<hr>",
          "<em>", dados_agregados$descricao[i], "</em>"
        )
      )
  }
  
  # Adiciona heatmap de densidade (CORREÇÃO: usando leaflet.extras)
  mapa <- mapa %>%
    addHeatmap(
      lng = dados$lon,
      lat = dados$lat,
      intensity = dados$usuarios,
      blur = 15,
      max = 0.05,
      radius = 12
    )
  
  # Adiciona legenda
  mapa <- mapa %>%
    addLegend(
      position = "bottomright",
      colors = dados_agregados$cor,
      labels = paste0(dados_agregados$regiao, " (", 
                      formatar_numero(dados_agregados$total_usuarios), ")"),
      title = "Regiões - Total de Usuários",
      opacity = 0.8
    )
  
  mensagem_status("Mapa interativo gerado com sucesso", "sucesso")
  return(mapa)
}

# 6. FUNÇÃO PARA ANÁLISE TEMPORAL DETALHADA (CORRIGIDA)
# ============================================================================
analise_temporal_detalhada <- function(dados) {
  mensagem_status("Realizando análise temporal detalhada...", "dados")
  
  # Prepara dados para análise temporal
  dados_temporais <- dados %>%
    mutate(
      data = as.Date(data_hora),
      mes_ano = format(data_hora, "%Y-%m"),
      semana_ano = week(data_hora),
      trimestre = quarter(data_hora)
    )
  
  # 1. Agregação diária
  dados_diarios <- dados_temporais %>%
    group_by(data, regiao) %>%
    summarise(
      usuarios_total = sum(usuarios, na.rm = TRUE),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento, na.rm = TRUE),
      n_registros = n(),
      .groups = "drop"
    )
  
  # 2. Agregação mensal
  dados_mensais <- dados_temporais %>%
    mutate(mes_nome = month(data_hora, label = TRUE, abbr = FALSE, locale = "pt_BR.UTF-8")) %>%
    group_by(ano, mes, mes_nome, regiao) %>%
    summarise(
      usuarios_total = sum(usuarios, na.rm = TRUE),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento, na.rm = TRUE),
      n_dias = n_distinct(data),
      n_registros = n(),
      .groups = "drop"
    ) %>%
    mutate(
      usuarios_por_dia = usuarios_total / n_dias,
      periodo = paste(ano, mes_nome, sep = " - ")
    )
  
  # 3. Série temporal para previsão (Bessa como exemplo) - CORREÇÃO APLICADA
  if(any(dados_diarios$regiao == "Bessa")) {
    serie_temporal_bessa <- dados_diarios %>%
      filter(regiao == "Bessa") %>%
      arrange(data) %>%
      select(data, usuarios_total)
    
    # CORREÇÃO: Extrair min e max antes de usar no complete()
    min_data <- min(serie_temporal_bessa$data, na.rm = TRUE)
    max_data <- max(serie_temporal_bessa$data, na.rm = TRUE)
    
    # Criar sequência completa de datas
    todas_datas <- data.frame(
      data = seq.Date(from = min_data, to = max_data, by = "day")
    )
    
    # Juntar com os dados existentes
    serie_temporal <- todas_datas %>%
      left_join(serie_temporal_bessa, by = "data") %>%
      mutate(
        usuarios_total = ifelse(is.na(usuarios_total), 0, usuarios_total),
        usuarios_media_movel = rollmean(usuarios_total, k = 7, fill = NA, align = "right")
      )
  } else {
    # Se não houver dados para Bessa, criar uma série vazia
    serie_temporal <- data.frame(
      data = seq.Date(from = as.Date("2020-12-01"), to = as.Date("2024-02-28"), by = "day"),
      usuarios_total = 0,
      usuarios_media_movel = NA
    )
  }
  
  # 4. Análise de tendência por região
  tendencia_regioes <- dados_mensais %>%
    group_by(regiao) %>%
    mutate(
      indice = row_number(),
      linha_tendencia = predict(lm(usuarios_por_dia ~ indice)),
      crescimento_percentual = (usuarios_por_dia / lag(usuarios_por_dia, 1) - 1) * 100
    ) %>%
    select(-indice) %>%
    ungroup()
  
  # 5. Estatísticas gerais
  estatisticas_gerais <- list(
    periodo_total = paste(format(range(dados_temporais$data), "%d/%m/%Y"), collapse = " a "),
    total_usuarios = sum(dados_temporais$usuarios, na.rm = TRUE),
    media_usuarios_dia = mean(dados_diarios$usuarios_total, na.rm = TRUE),
    pico_usuarios_dia = max(dados_diarios$usuarios_total, na.rm = TRUE),
    data_pico = dados_diarios$data[which.max(dados_diarios$usuarios_total)],
    velocidade_media_geral = mean(dados_temporais$velocidade_media, na.rm = TRUE),
    congestionamento_medio_geral = mean(dados_temporais$nivel_congestionamento, na.rm = TRUE),
    regiao_mais_movimentada = dados_mensais %>%
      group_by(regiao) %>%
      summarise(total = sum(usuarios_total), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice(1) %>%
      pull(regiao),
    mes_mais_movimentado = dados_mensais %>%
      group_by(mes_nome) %>%
      summarise(media = mean(usuarios_por_dia), .groups = "drop") %>%
      arrange(desc(media)) %>%
      slice(1) %>%
      pull(mes_nome)
  )
  
  mensagem_status("Análise temporal concluída", "sucesso")
  
  return(list(
    dados_diarios = dados_diarios,
    dados_mensais = dados_mensais,
    serie_temporal = serie_temporal,
    tendencia_regioes = tendencia_regioes,
    estatisticas = estatisticas_gerais
  ))
}

# 7. FUNÇÃO PARA GERAR VISUALIZAÇÕES
# ============================================================================
gerar_visualizacoes_completas <- function(dados, analise_temporal) {
  mensagem_status("Gerando visualizações e gráficos...", "grafico")
  
  # 1. Gráfico de evolução mensal
  p1 <- ggplot(analise_temporal$dados_mensais, 
               aes(x = periodo, y = usuarios_por_dia, group = regiao, color = regiao)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = "Evolução Mensal do Tráfego por Região",
      subtitle = "Litoral Norte de João Pessoa (Dez-Jan-Fev 2020-2024)",
      x = "Período (Mês - Ano)",
      y = "Média de Usuários por Dia",
      color = "Região"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    ) +
    scale_color_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::comma)
  
  # 2. Heatmap de tráfego por hora e dia
  dados_hora_dia <- dados %>%
    mutate(
      hora_fator = factor(hora),
      dia_semana = factor(dia_semana, 
                          levels = c("domingo", "sábado", "sexta-feira", 
                                     "quinta-feira", "quarta-feira", 
                                     "terça-feira", "segunda-feira"))
    ) %>%
    group_by(hora_fator, dia_semana) %>%
    summarise(
      usuarios_medio = mean(usuarios, na.rm = TRUE),
      .groups = "drop"
    )
  
  p2 <- ggplot(dados_hora_dia, aes(x = hora_fator, y = dia_semana, fill = usuarios_medio)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_viridis(option = "C", name = "Usuários\nMédios") +
    labs(
      title = "Padrão de Tráfego por Hora e Dia da Semana",
      subtitle = "Intensidade de uso do Waze",
      x = "Hora do Dia",
      y = "Dia da Semana"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 0),
      panel.grid = element_blank()
    )
  
  # 3. Boxplot de velocidade por região
  p3 <- ggplot(dados, aes(x = regiao, y = velocidade_media, fill = regiao)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    labs(
      title = "Distribuição de Velocidade Média por Região",
      subtitle = "Linha vermelha indica a média",
      x = "Região",
      y = "Velocidade Média (km/h)",
      fill = "Região"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Set2")
  
  # 4. Tendências de crescimento
  p4 <- ggplot(analise_temporal$tendencia_regioes, 
               aes(x = periodo, y = usuarios_por_dia, color = regiao)) +
    geom_line(aes(group = regiao), size = 1) +
    geom_line(aes(y = linha_tendencia), linetype = "dashed", size = 0.8) +
    geom_point(size = 2) +
    facet_wrap(~ regiao, scales = "free_y", ncol = 3) +
    labs(
      title = "Tendência de Crescimento do Tráfego por Região",
      subtitle = "Linha sólida: dados reais | Linha tracejada: tendência",
      x = "Período",
      y = "Usuários por Dia",
      color = "Região"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    ) +
    scale_color_brewer(palette = "Set2")
  
  # 5. Série temporal com média móvel
  p5 <- ggplot(analise_temporal$serie_temporal, aes(x = data)) +
    geom_line(aes(y = usuarios_total), color = "gray70", alpha = 0.5) +
    geom_line(aes(y = usuarios_media_movel), color = "#d62728", size = 1) +
    labs(
      title = "Série Temporal do Tráfego (Bessa) com Média Móvel de 7 Dias",
      subtitle = paste("Período:", analise_temporal$estatisticas$periodo_total),
      x = "Data",
      y = "Total de Usuários por Dia"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    scale_y_continuous(labels = scales::comma)
  
  mensagem_status("Visualizações geradas com sucesso", "sucesso")
  
  return(list(
    grafico_evolucao = p1,
    heatmap_hora_dia = p2,
    boxplot_velocidade = p3,
    grafico_tendencias = p4,
    serie_temporal = p5
  ))
}

# 8. FUNÇÃO PARA GERAR RELATÓRIO HTML (ATUALIZADA)
# ============================================================================
gerar_relatorio_html <- function(dados, mapa, analise_temporal, visualizacoes, 
                                 arquivo = "relatorio_waze_completo.html") {
  mensagem_status("Criando relatório HTML completo...", "relatorio")
  
  # Cria diretório se não existir
  dir.create("relatorios", showWarnings = FALSE)
  dir.create("relatorios/graficos", showWarnings = FALSE)
  
  caminho_arquivo <- file.path("relatorios", arquivo)
  
  # Salva o mapa como HTML temporário
  mapa_html <- tempfile(fileext = ".html")
  saveWidget(mapa, mapa_html, selfcontained = FALSE)
  
  # Salva os gráficos como imagens
  ggsave("relatorios/graficos/evolucao_mensal.png", visualizacoes$grafico_evolucao, 
         width = 12, height = 6, dpi = 150)
  ggsave("relatorios/graficos/heatmap_hora_dia.png", visualizacoes$heatmap_hora_dia, 
         width = 10, height = 6, dpi = 150)
  ggsave("relatorios/graficos/boxplot_velocidade.png", visualizacoes$boxplot_velocidade, 
         width = 10, height = 6, dpi = 150)
  ggsave("relatorios/graficos/tendencias_regioes.png", visualizacoes$grafico_tendencias, 
         width = 12, height = 8, dpi = 150)
  ggsave("relatorios/graficos/serie_temporal.png", visualizacoes$serie_temporal, 
         width = 12, height = 6, dpi = 150)
  
  # Gera conteúdo HTML
  html_conteudo <- paste0('
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Relatório Completo - Análise Waze Litoral Norte João Pessoa</title>
    <style>
        body { font-family: Arial, sans-serif; line-height: 1.6; color: #333; max-width: 1200px; margin: 0 auto; padding: 20px; }
        .header { background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%); color: white; padding: 30px; border-radius: 10px; margin-bottom: 30px; text-align: center; }
        .section { background: white; padding: 25px; margin-bottom: 25px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); }
        h1 { margin: 0; font-size: 2.2em; }
        h2 { color: #2a5298; border-bottom: 2px solid #2a5298; padding-bottom: 10px; margin-top: 30px; }
        .stat-card { background: #e8f4fd; padding: 15px; border-radius: 5px; margin: 10px 0; border-left: 4px solid #2a5298; }
        .stat-value { font-size: 1.8em; font-weight: bold; color: #2a5298; }
        .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 20px; margin: 20px 0; }
        .region-card { border: 1px solid #ddd; padding: 15px; border-radius: 5px; }
        .conclusion { background-color: #e8f5e9; padding: 20px; border-radius: 5px; margin: 20px 0; border-left: 4px solid #4caf50; }
        .footer { text-align: center; margin-top: 40px; padding: 20px; color: #666; font-size: 0.9em; border-top: 1px solid #ddd; }
        img { max-width: 100%; height: auto; border-radius: 5px; margin: 10px 0; }
    </style>
</head>
<body>
    <div class="header">
        <h1>📊 Relatório Completo - Análise Waze</h1>
        <p>Litoral Norte de João Pessoa | Dezembro-Janeiro-Fevereiro 2020-2024</p>
        <p><em>Análise de tráfego, velocidade e padrões de mobilidade</em></p>
    </div>
    
    <div class="section">
        <h2>📈 Estatísticas Gerais</h2>
        <div class="grid">
            <div class="stat-card">
                <div class="stat-value">', formatar_numero(analise_temporal$estatisticas$total_usuarios), '</div>
                <div>Total de Usuários Registrados</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">', round(analise_temporal$estatisticas$velocidade_media_geral, 1), ' km/h</div>
                <div>Velocidade Média Geral</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">', round(analise_temporal$estatisticas$congestionamento_medio_geral, 1), '/5</div>
                <div>Nível Médio de Congestionamento</div>
            </div>
        </div>
    </div>
    
    <div class="section">
        <h2>🗺️ Mapa Interativo das Regiões</h2>
        <iframe src="', mapa_html, '" width="100%" height="500px" style="border: none; border-radius: 8px;"></iframe>
    </div>
    
    <div class="section">
        <h2>🏙️ Regiões Analisadas</h2>
        <div class="grid">
')
  
  # Adiciona cards para cada região
  for(regiao_nome in names(regioes_litoral_norte)) {
    dados_regiao <- dados %>% filter(regiao == regiao_nome)
    estat_regiao <- dados_regiao %>%
      summarise(
        total_usuarios = sum(usuarios),
        velocidade_media = mean(velocidade_media),
        registros = n()
      )
    
    html_conteudo <- paste0(html_conteudo, '
            <div class="region-card">
                <h3>', regiao_nome, '</h3>
                <p><strong>Característica:</strong> ', regioes_litoral_norte[[regiao_nome]]$descricao, '</p>
                <p><strong>Total de Usuários:</strong> ', formatar_numero(estat_regiao$total_usuarios), '</p>
                <p><strong>Velocidade Média:</strong> ', round(estat_regiao$velocidade_media, 1), ' km/h</p>
            </div>
')
  }
  
  html_conteudo <- paste0(html_conteudo, '
        </div>
    </div>
    
    <div class="section">
        <h2>📊 Principais Achados</h2>
        <p><strong>Região Mais Movimentada:</strong> ', analise_temporal$estatisticas$regiao_mais_movimentada, '</p>
        <p><strong>Mês de Pico:</strong> ', analise_temporal$estatisticas$mes_mais_movimentado, '</p>
        <p><strong>Dia Recorde:</strong> ', format(analise_temporal$estatisticas$data_pico, "%d/%m/%Y"), 
                          ' (', formatar_numero(analise_temporal$estatisticas$pico_usuarios_dia), ' usuários)</p>
    </div>
    
    <div class="section">
        <h2>📈 Visualizações</h2>
        <h3>Evolução Mensal do Tráfego</h3>
        <img src="relatorios/graficos/evolucao_mensal.png" alt="Evolução Mensal">
        
        <h3>Padrão de Tráfego por Hora e Dia</h3>
        <img src="relatorios/graficos/heatmap_hora_dia.png" alt="Heatmap Hora-Dia">
        
        <h3>Distribuição de Velocidade por Região</h3>
        <img src="relatorios/graficos/boxplot_velocidade.png" alt="Boxplot Velocidade">
    </div>
    
    <div class="conclusion">
        <h2>✅ Conclusões e Recomendações</h2>
        <h3>Para Gestão de Tráfego:</h3>
        <ol>
            <li>Focar ações de controle em <strong>', analise_temporal$estatisticas$regiao_mais_movimentada, '</strong></li>
            <li>Implementar ajustes de semáforos nos horários de pico (tarde/noite)</li>
            <li>Reforçar sinalização turística na alta temporada</li>
        </ol>
        
        <h3>Para Planejamento Urbano:</h3>
        <ol>
            <li>Expandir infraestrutura viária nas regiões de maior crescimento</li>
            <li>Desenvolver alternativas de mobilidade para áreas críticas</li>
            <li>Criar sistema de monitoramento em tempo real</li>
        </ol>
    </div>
    
    <div class="footer">
        <p>Relatório gerado automaticamente em ', format(Sys.time(), "%d/%m/%Y às %H:%M"), '</p>
        <p>Sistema de Análise de Dados Waze - Litoral Norte de João Pessoa</p>
        <p><em>Dados simulados para fins de demonstração e análise</em></p>
    </div>
</body>
</html>
')
  
  # Salva o arquivo HTML
  writeLines(html_conteudo, caminho_arquivo)
  
  mensagem_status(paste("Relatório HTML gerado:", caminho_arquivo), "sucesso")
  mensagem_status("Gráficos salvos em: relatorios/graficos/", "sucesso")
  
  return(caminho_arquivo)
}

# 9. FUNÇÃO PRINCIPAL DE ANÁLISE COMPLETA
# ============================================================================
executar_analise_completa <- function() {
  titulo_secao("🚀 SISTEMA DE ANÁLISE WAZE - ANÁLISE COMPLETA", 70)
  cat("📅 Período: Dezembro, Janeiro, Fevereiro de 2020 a 2024\n")
  cat("📍 Área: Litoral Norte de João Pessoa (5 regiões)\n")
  linha_divisoria(70)
  cat("\n")
  
  inicio <- Sys.time()
  
  # 1. Gerar dados simulados para 5 anos
  mensagem_status("[1/5] Gerando dados simulados para 5 anos...", "execucao")
  dados_waze <- gerar_dados_waze_5anos(anos = 2020:2024, meses = c(12, 1, 2))
  cat("\n")
  
  # 2. Gerar mapa interativo
  mensagem_status("[2/5] Criando mapa interativo das regiões...", "execucao")
  mapa_waze <- gerar_mapa_interativo(dados_waze)
  cat("\n")
  
  # 3. Análise temporal detalhada
  mensagem_status("[3/5] Realizando análise temporal detalhada...", "execucao")
  analise_temporal <- analise_temporal_detalhada(dados_waze)
  cat("\n")
  
  # 4. Gerar visualizações
  mensagem_status("[4/5] Gerando visualizações e gráficos...", "execucao")
  visualizacoes <- gerar_visualizacoes_completas(dados_waze, analise_temporal)
  cat("\n")
  
  # 5. Gerar relatório HTML
  mensagem_status("[5/5] Criando relatório HTML completo...", "execucao")
  relatorio_path <- gerar_relatorio_html(dados_waze, mapa_waze, analise_temporal, visualizacoes)
  cat("\n")
  
  # Tempo de execução
  fim <- Sys.time()
  tempo_execucao <- round(as.numeric(difftime(fim, inicio, units = "secs")), 1)
  
  # Resultados finais
  titulo_secao("✅ ANÁLISE COMPLETA CONCLUÍDA", 70)
  cat("⏱️  Tempo de execução:", tempo_execucao, "segundos\n")
  cat("📁 Arquivos gerados:\n")
  cat("   -", relatorio_path, "\n")
  cat("   - relatorios/graficos/ (5 imagens PNG)\n")
  cat("📊 Dados gerados:\n")
  cat("   -", formatar_numero(nrow(dados_waze)), "registros simulados\n")
  cat("   -", length(unique(dados_waze$regiao)), "regiões analisadas\n")
  cat("   -", length(unique(dados_waze$ano)), "anos de dados\n\n")
  
  # Estatísticas principais
  cat("📋 RESULTADOS PRINCIPAIS:\n")
  cat("   1. Total de usuários:", formatar_numero(analise_temporal$estatisticas$total_usuarios), "\n")
  cat("   2. Velocidade média:", round(analise_temporal$estatisticas$velocidade_media_geral, 1), "km/h\n")
  cat("   3. Região mais movimentada:", analise_temporal$estatisticas$regiao_mais_movimentada, "\n")
  cat("   4. Mês de pico:", analise_temporal$estatisticas$mes_mais_movimentado, "\n")
  cat("   5. Dia recorde:", format(analise_temporal$estatisticas$data_pico, "%d/%m/%Y"), "\n\n")
  
  # Próximos passos
  cat("🎯 PRÓXIMOS PASSOS:\n")
  cat("   1. Abra o relatório HTML no navegador\n")
  cat("   2. Explore o mapa interativo clicando nas regiões\n")
  cat("   3. Analise os gráficos no diretório 'relatorios/graficos/'\n")
  cat("   4. Use os dados para planejamento de tráfego e turismo\n\n")
  
  # Retorna todos os resultados
  return(list(
    dados = dados_waze,
    mapa = mapa_waze,
    analise_temporal = analise_temporal,
    visualizacoes = visualizacoes,
    relatorio = relatorio_path,
    tempo_execucao = tempo_execucao
  ))
}

# 10. FUNÇÃO DE ANÁLISE RÁPIDA
# ============================================================================
analise_rapida_waze <- function() {
  titulo_secao("🔍 ANÁLISE WAZE - MODO RÁPIDO", 60)
  
  mensagem_status("Gerando dados para 1 ano (2024)...", "info")
  dados_waze <- gerar_dados_waze_5anos(anos = 2024, meses = c(12, 1, 2))
  
  # Análise básica
  analise_basica <- dados_waze %>%
    group_by(regiao) %>%
    summarise(
      total_usuarios = sum(usuarios),
      velocidade_media = mean(velocidade_media),
      congestionamento_medio = mean(nivel_congestionamento),
      registros = n(),
      .groups = "drop"
    )
  
  # Gráfico básico
  p_basico <- ggplot(analise_basica, aes(x = reorder(regiao, total_usuarios), y = total_usuarios, fill = regiao)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = formatar_numero(total_usuarios)), vjust = -0.5) +
    labs(
      title = "Total de Usuários Waze por Região (Verão 2024)",
      subtitle = "Litoral Norte de João Pessoa",
      x = "Região",
      y = "Total de Usuários"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    scale_fill_brewer(palette = "Set2")
  
  # Exibe resultados
  print(p_basico)
  cat("\n")
  titulo_secao("📊 RESUMO POR REGIÃO", 60)
  print(analise_basica)
  
  return(list(dados = dados_waze, grafico = p_basico))
}

# 11. FUNÇÃO PARA EXPORTAR DADOS
# ============================================================================
exportar_dados_waze <- function(dados, tipo = "csv") {
  mensagem_status("Exportando dados...", "dados")
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if(tipo == "csv") {
    arquivo <- paste0("dados_waze_", timestamp, ".csv")
    write.csv(dados, arquivo, row.names = FALSE)
    mensagem_status(paste("Dados exportados para:", arquivo), "sucesso")
    return(arquivo)
  } else if(tipo == "excel") {
    arquivo <- paste0("dados_waze_", timestamp, ".xlsx")
    mensagem_status("Exportação Excel requer pacote adicional (writexl)", "aviso")
    return(NULL)
  } else {
    mensagem_status("Tipo de exportação não suportado", "erro")
    return(NULL)
  }
}

# 12. INTERFACE DO SISTEMA
# ============================================================================
titulo_secao("🌊 SISTEMA DE ANÁLISE DE DADOS WAZE - LITORAL NORTE JOÃO PESSOA", 80)

cat("\nOPÇÕES DISPONÍVEIS:\n")
cat("  1. executar_analise_completa()   - Análise completa 5 anos com mapa e relatório\n")
cat("  2. analise_rapida_waze()         - Análise rápida de 1 ano\n")
cat("  3. gerar_dados_waze_5anos()      - Apenas gerar dados (2020-2024)\n")
cat("  4. exportar_dados_waze()         - Exportar dados para CSV\n")

cat("\nEXEMPLOS DE USO:\n")
cat("  # Para análise completa (recomendado):\n")
cat("  resultado <- executar_analise_completa()\n")
cat("\n  # Para ver o mapa interativo:\n")
cat("  print(resultado$mapa)\n")
cat("\n  # Para exportar dados:\n")
cat("  write.csv(resultado$dados, 'dados_waze.csv', row.names = FALSE)\n")
cat("\n  # Para abrir o relatório no navegador:\n")
cat("  browseURL(resultado$relatorio)\n")

titulo_secao("", 80)
cat("\n")

mensagem_status("Para iniciar a análise completa, execute: resultado <- executar_analise_completa()", "info")
cat("\n")
resultado <- executar_analise_completa()
# Visualizar o mapa com as regiões e heatmap
print(resultado$mapa)
# Abrir o relatório no navegador
browseURL(resultado$relatorio)
# Visualizar os gráficos individualmente
print(resultado$visualizacoes$grafico_evolucao)
print(resultado$visualizacoes$heatmap_hora_dia)
print(resultado$visualizacoes$boxplot_velocidade)
print(resultado$visualizacoes$grafico_tendencias)
print(resultado$visualizacoes$serie_temporal)

# Ver estrutura dos dados
str(resultado$dados)

# Ver resumo estatístico
summary(resultado$dados)

# Ver as estatísticas principais da análise
print(resultado$analise_temporal$estatisticas)
# Exportar para CSV
exportar_dados_waze(resultado$dados, tipo = "csv")

# Ou manualmente:
write.csv(resultado$dados, "dados_waze_completos.csv", row.names = FALSE)
# Para uma análise mais simples de 1 ano
analise_rapida <- analise_rapida_waze()
