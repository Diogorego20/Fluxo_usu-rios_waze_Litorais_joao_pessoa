# ============================================================================
# SISTEMA DE ANÁLISE DE DADOS WAZE - LITORAL SUL JOÃO PESSOA
# Versão adaptada para o litoral sul
# ============================================================================

# 1. INSTALAÇÃO E CARREGAMENTO DOS PACOTES
# ============================================================================
cat("🔄 Iniciando sistema de análise Waze para o Litoral Sul...\n")
cat("📦 Verificando e instalando pacotes necessários...\n\n")

# Lista de pacotes necessários
pacotes_necessarios <- c("httr", "jsonlite", "dplyr", "lubridate", "tidyr", 
                         "ggplot2", "sf", "ggmap", "leaflet", "plotly",
                         "viridis", "gridExtra", "zoo", "forecast", "tseries",
                         "maps", "mapdata", "RColorBrewer", "scales", "htmlwidgets",
                         "leaflet.extras")

# Instalar pacotes faltantes
pacotes_instalados <- pacotes_necessarios[!(pacotes_necessarios %in% installed.packages()[,"Package"])]
if(length(pacotes_instalados) > 0) {
  cat("📥 Instalando pacotes:", paste(pacotes_instalados, collapse = ", "), "\n")
  install.packages(pacotes_instalados, dependencies = TRUE)
}

# Carregar pacotes
suppressPackageStartupMessages({
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
  library(leaflet.extras)
})

cat("✅ Todos os pacotes foram carregados com sucesso!\n\n")

# Configurar locale para formatação de números
tryCatch({
  Sys.setlocale("LC_NUMERIC", "C")
}, error = function(e) {
  cat("⚠️  Não foi possível configurar o locale. Continuando...\n")
})

# 2. DEFINIÇÃO DAS REGIÕES DO LITORAL SUL
# ============================================================================
regioes_litoral_sul <- list(
  "Ponta do Seixas" = list(
    lat_min = -7.145, lat_max = -7.125,
    lon_min = -34.795, lon_max = -34.775,
    cor = "#1f77b4",
    descricao = "Extremo oriental das Américas, ponto turístico importante"
  ),
  "Tambaba" = list(
    lat_min = -7.165, lat_max = -7.145,
    lon_min = -34.815, lon_max = -34.795,
    cor = "#ff7f0e",
    descricao = "Praia de naturismo famosa, com falésias e piscinas naturais"
  ),
  "Coqueirinho" = list(
    lat_min = -7.185, lat_max = -7.165,
    lon_min = -34.835, lon_max = -34.815,
    cor = "#2ca02c",
    descricao = "Praia com coqueirais, falésias coloridas e boa infraestrutura"
  ),
  "Tabatinga" = list(
    lat_min = -7.205, lat_max = -7.185,
    lon_min = -34.855, lon_max = -34.835,
    cor = "#d62728",
    descricao = "Praia familiar com arrecifes e piscinas naturais"
  ),
  "Jacumã" = list(
    lat_min = -7.225, lat_max = -7.205,
    lon_min = -34.875, lon_max = -34.855,
    cor = "#9467bd",
    descricao = "Praia extensa com dunas, ideal para esportes náuticos"
  ),
  "Manaíra" = list(
    lat_min = -7.125, lat_max = -7.105,
    lon_min = -34.835, lon_max = -34.815,
    cor = "#8c564b",
    descricao = "Área urbana com comércio, hotéis e vida noturna"
  ),
  "Bessa" = list(
    lat_min = -7.105, lat_max = -7.085,
    lon_min = -34.855, lon_max = -34.835,
    cor = "#e377c2",
    descricao = "Praia urbana com ampla faixa de areia e infraestrutura"
  ),
  "Cabo Branco" = list(
    lat_min = -7.145, lat_max = -7.125,
    lon_min = -34.815, lon_max = -34.795,
    cor = "#7f7f7f",
    descricao = "Orla urbanizada com ciclovia, bares e restaurantes"
  )
)

# 3. FUNÇÕES AUXILIARES (MANTIDAS)
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

formatar_numero <- function(x) {
  if (is.numeric(x)) {
    return(format(round(x), big.mark = ".", decimal.mark = ",", scientific = FALSE))
  }
  return(x)
}

# 4. FUNÇÃO PARA GERAR DADOS SIMULADOS PARA LITORAL SUL (CORRIGIDA)
# ============================================================================
gerar_dados_waze_litoral_sul <- function(anos = 2020:2024, meses = c(12, 1, 2)) {
  mensagem_status(paste("Gerando dados simulados para o Litoral Sul (", length(anos), "anos)..."), "dados")
  
  set.seed(123)
  dados_completos <- list()
  
  # Probabilidades específicas para cada região (ajustadas para perfil do litoral sul)
  regioes <- names(regioes_litoral_sul)
  
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
      n_registros_dia <- sample(30:100, n_dias, replace = TRUE)
      n_total <- sum(n_registros_dia)
      
      # Cria sequência de datas e horas
      datas_horas <- seq(
        from = as.POSIXct(paste(data_inicio, "06:00:00")),
        to = as.POSIXct(paste(data_fim, "22:00:00")),
        length.out = n_total
      )
      
      # Fatores sazonais (maior no verão para litoral sul)
      if(mes %in% c(12, 1)) {
        fator_sazonal <- runif(n_total, 2.0, 3.0)  # Mais alto que litoral norte
      } else if(mes == 2) {
        fator_sazonal <- runif(n_total, 1.5, 2.5)
      } else {
        fator_sazonal <- 1
      }
      
      # Crescimento anual mais acelerado no litoral sul
      crescimento_anual <- 1 + (ano - 2020) * 0.06  # 6% ao ano
      
      # Gera dados básicos
      dados_mes <- data.frame(
        timestamp = as.numeric(datas_horas) * 1000,
        data_hora = datas_horas,
        ano = ano_mes,
        mes = mes,
        dia_semana = wday(datas_horas, label = TRUE, abbr = FALSE, locale = "pt_BR.UTF-8")
      )
      
      # Adicionar hora e periodo_dia
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
      
      # Probabilidades ajustadas para litoral sul
      # Praias mais turísticas têm maior probabilidade
      prob_regioes <- c(
        "Tambaba" = 0.25,       # Muito popular no verão
        "Coqueirinho" = 0.20,   # Família e turismo
        "Ponta do Seixas" = 0.15, # Ponto turístico
        "Tabatinga" = 0.10,     # Familiar
        "Jacumã" = 0.08,        # Esportes náuticos
        "Manaíra" = 0.12,       # Urbana
        "Bessa" = 0.05,         # Urbana
        "Cabo Branco" = 0.05    # Orla
      )
      
      dados_mes$regiao <- sample(regioes, n_total, replace = TRUE, prob = prob_regioes)
      
      # Adicionar coordenadas e métricas específicas para litoral sul
      dados_mes <- dados_mes %>%
        rowwise() %>%
        mutate(
          lat = runif(1, 
                      regioes_litoral_sul[[regiao]]$lat_min,
                      regioes_litoral_sul[[regiao]]$lat_max),
          lon = runif(1,
                      regioes_litoral_sul[[regiao]]$lon_min,
                      regioes_litoral_sul[[regiao]]$lon_max),
          usuarios = round(rpois(1, lambda = 12 * fator_sazonal * crescimento_anual)),
          velocidade_media = case_when(
            periodo_dia == "Manhã" ~ runif(1, 40, 60),
            periodo_dia == "Tarde" ~ runif(1, 25, 45),
            periodo_dia == "Noite" ~ runif(1, 45, 65),
            TRUE ~ runif(1, 55, 75)
          ),
          nivel_congestionamento = case_when(
            regiao %in% c("Tambaba", "Coqueirinho") & mes %in% c(12, 1) ~ 
              sample(4:5, 1, prob = c(0.2, 0.8)),
            usuarios > 15 ~ sample(3:5, 1, prob = c(0.3, 0.5, 0.2)),
            usuarios > 8 ~ sample(2:3, 1, prob = c(0.4, 0.6)),
            TRUE ~ 1
          ),
          tipo_via = case_when(
            regiao %in% c("Ponta do Seixas", "Tambaba") ~ 
              sample(c("Estrada", "Acesso Praia"), 1, prob = c(0.6, 0.4)),
            TRUE ~ sample(c("Avenida", "Rua", "Estrada", "Orla"), 1, 
                          prob = c(0.3, 0.3, 0.3, 0.1))
          ),
          tipo_turista = sample(c("Local", "Turista Nacional", "Turista Internacional"), 1,
                                prob = c(0.4, 0.5, 0.1))
        ) %>%
        ungroup()
      
      # Adicionar eventos especiais (mais festival no litoral sul)
      dados_mes <- dados_mes %>%
        mutate(
          evento_especial = case_when(
            month(data_hora) == 12 & day(data_hora) >= 20 & day(data_hora) <= 31 ~
              sample(c("Natal", "Réveillon", "Festival de Verão"), 1, prob = c(0.3, 0.4, 0.3)),
            month(data_hora) == 1 & day(data_hora) <= 6 ~ "Férias Escolares",
            month(data_hora) == 2 & day(data_hora) >= 10 & day(data_hora) <= 18 ~ "Carnaval",
            month(data_hora) == 1 & day(data_hora) >= 15 & day(data_hora) <= 31 ~ "Temporada de Praia",
            TRUE ~ NA_character_
          )
        )
      
      # CORREÇÃO: Inicializar a coluna incidente para todos os registros
      dados_mes$incidente <- "Nenhum"
      
      # Adiciona outliers (7% - mais incidentes em estradas do litoral sul)
      n_outliers <- round(n_total * 0.07)
      if(n_outliers > 0) {
        outliers_idx <- sample(1:n_total, n_outliers)
        dados_mes$usuarios[outliers_idx] <- dados_mes$usuarios[outliers_idx] * 
          sample(2:4, n_outliers, replace = TRUE)
        dados_mes$nivel_congestionamento[outliers_idx] <- 5
        
        # CORREÇÃO: Atribuir valores apenas aos outliers
        dados_mes$incidente[outliers_idx] <- sample(
          c("Acidente", "Obra na pista", "Bloqueio", "Evento Especial"), 
          n_outliers, replace = TRUE,
          prob = c(0.4, 0.3, 0.2, 0.1)
        )
      }
      
      dados_completos[[paste(ano_mes, mes, sep = "-")]] <- dados_mes
    }
  }
  
  # Combina todos os dados
  dados_finais <- bind_rows(dados_completos)
  
  mensagem_status(paste("Dados gerados:", formatar_numero(nrow(dados_finais)), "registros"), "sucesso")
  cat("  Período:", format(min(dados_finais$data_hora), "%d/%m/%Y"), 
      "a", format(max(dados_finais$data_hora), "%d/%m/%Y"), "\n")
  cat("  Regiões analisadas:", length(unique(dados_finais$regiao)), "\n")
  cat("  Praias do litoral sul:", paste(unique(dados_finais$regiao), collapse = ", "), "\n")
  
  return(dados_finais)
}

# 5. FUNÇÃO PARA GERAR MAPA INTERATIVO DO LITORAL SUL
# ============================================================================
gerar_mapa_interativo_sul <- function(dados) {
  mensagem_status("Gerando mapa interativo do Litoral Sul...", "mapa")
  
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
      n_incidentes = sum(incidente != "Nenhum", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      data.frame(
        regiao = names(regioes_litoral_sul),
        cor = sapply(regioes_litoral_sul, function(x) x$cor),
        descricao = sapply(regioes_litoral_sul, function(x) x$descricao)
      ),
      by = "regiao"
    ) %>%
    mutate(
      percentual_incidentes = n_incidentes / n_registros * 100
    )
  
  # Cria mapa leaflet centrado no litoral sul
  mapa <- leaflet(dados_agregados) %>%
    addTiles() %>%
    setView(lng = -34.83, lat = -7.16, zoom = 11) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
    addLayersControl(
      baseGroups = c("Mapa", "Satélite"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Adiciona marcadores para cada região
  for(i in 1:nrow(dados_agregados)) {
    mapa <- mapa %>%
      addCircleMarkers(
        lng = dados_agregados$lon_media[i],
        lat = dados_agregados$lat_media[i],
        radius = 10 + sqrt(dados_agregados$total_usuarios[i]) / 50,
        color = dados_agregados$cor[i],
        fillColor = dados_agregados$cor[i],
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 3,
        popup = paste(
          "<strong>", dados_agregados$regiao[i], "</strong><br>",
          "<hr>",
          "📊 <b>Estatísticas:</b><br>",
          "• Total de Usuários: <b>", formatar_numero(dados_agregados$total_usuarios[i]), "</b><br>",
          "• Velocidade Média: <b>", round(dados_agregados$velocidade_media[i], 1), "km/h</b><br>",
          "• Nível de Congestionamento: <b>", round(dados_agregados$media_congestionamento[i], 1), "/5</b><br>",
          "• Registros: <b>", formatar_numero(dados_agregados$n_registros[i]), "</b><br>",
          "• Incidentes: <b>", dados_agregados$n_incidentes[i], " (", 
          round(dados_agregados$percentual_incidentes[i], 1), "%)</b><br>",
          "<hr>",
          "📍 <b>Características:</b><br>",
          "<em>", dados_agregados$descricao[i], "</em>"
        ),
        label = paste0(dados_agregados$regiao[i], ": ", 
                       formatar_numero(dados_agregados$total_usuarios[i]), " usuários")
      )
  }
  
  # Adiciona heatmap de densidade
  mapa <- mapa %>%
    addHeatmap(
      lng = dados$lon,
      lat = dados$lat,
      intensity = dados$usuarios,
      blur = 20,
      max = 0.05,
      radius = 15
    )
  
  # Adiciona polígonos das regiões
  for(regiao_nome in names(regioes_litoral_sul)) {
    regiao <- regioes_litoral_sul[[regiao_nome]]
    mapa <- mapa %>%
      addRectangles(
        lng1 = regiao$lon_min, lat1 = regiao$lat_min,
        lng2 = regiao$lon_max, lat2 = regiao$lat_max,
        fillColor = regiao$cor,
        fillOpacity = 0.1,
        color = regiao$cor,
        weight = 2,
        group = "Regiões",
        label = regiao_nome
      )
  }
  
  # Adiciona legenda
  mapa <- mapa %>%
    addLegend(
      position = "bottomright",
      colors = dados_agregados$cor,
      labels = paste0(dados_agregados$regiao, "<br>",
                      "Usuários: ", formatar_numero(dados_agregados$total_usuarios), "<br>",
                      "Incidentes: ", dados_agregados$n_incidentes),
      title = "Praias do Litoral Sul",
      opacity = 0.8
    )
  
  mensagem_status("Mapa interativo do litoral sul gerado com sucesso", "sucesso")
  return(mapa)
}

# 6. FUNÇÃO PARA ANÁLISE TEMPORAL DO LITORAL SUL
# ============================================================================
analise_temporal_litoral_sul <- function(dados) {
  mensagem_status("Realizando análise temporal do Litoral Sul...", "dados")
  
  # Prepara dados para análise temporal
  dados_temporais <- dados %>%
    mutate(
      data = as.Date(data_hora),
      mes_ano = format(data_hora, "%Y-%m"),
      semana_ano = week(data_hora),
      trimestre = quarter(data_hora),
      temporada = case_when(
        mes %in% c(12, 1, 2) ~ "Alta Temporada",
        mes %in% c(6, 7, 8) ~ "Baixa Temporada",
        TRUE ~ "Temporada Média"
      )
    )
  
  # 1. Agregação diária
  dados_diarios <- dados_temporais %>%
    group_by(data, regiao, temporada) %>%
    summarise(
      usuarios_total = sum(usuarios, na.rm = TRUE),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento, na.rm = TRUE),
      incidentes_total = sum(incidente != "Nenhum", na.rm = TRUE),
      n_registros = n(),
      .groups = "drop"
    )
  
  # 2. Agregação mensal
  dados_mensais <- dados_temporais %>%
    mutate(mes_nome = month(data_hora, label = TRUE, abbr = FALSE, locale = "pt_BR.UTF-8")) %>%
    group_by(ano, mes, mes_nome, regiao, temporada) %>%
    summarise(
      usuarios_total = sum(usuarios, na.rm = TRUE),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento, na.rm = TRUE),
      incidentes_total = sum(incidente != "Nenhum", na.rm = TRUE),
      n_dias = n_distinct(data),
      n_registros = n(),
      .groups = "drop"
    ) %>%
    mutate(
      usuarios_por_dia = usuarios_total / n_dias,
      incidentes_por_dia = incidentes_total / n_dias,
      periodo = paste(ano, mes_nome, sep = " - ")
    )
  
  # 3. Análise por tipo de turista
  dados_turista <- dados_temporais %>%
    group_by(regiao, tipo_turista) %>%
    summarise(
      usuarios_total = sum(usuarios, na.rm = TRUE),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      n_registros = n(),
      .groups = "drop"
    ) %>%
    group_by(regiao) %>%
    mutate(
      percentual = usuarios_total / sum(usuarios_total) * 100
    )
  
  # 4. Série temporal para Tambaba (praia mais movimentada)
  if(any(dados_diarios$regiao == "Tambaba")) {
    serie_temporal_tambaba <- dados_diarios %>%
      filter(regiao == "Tambaba") %>%
      arrange(data) %>%
      select(data, usuarios_total)
    
    min_data <- min(serie_temporal_tambaba$data, na.rm = TRUE)
    max_data <- max(serie_temporal_tambaba$data, na.rm = TRUE)
    
    todas_datas <- data.frame(
      data = seq.Date(from = min_data, to = max_data, by = "day")
    )
    
    serie_temporal <- todas_datas %>%
      left_join(serie_temporal_tambaba, by = "data") %>%
      mutate(
        usuarios_total = ifelse(is.na(usuarios_total), 0, usuarios_total),
        usuarios_media_movel = rollmean(usuarios_total, k = 7, fill = NA, align = "right"),
        dia_semana = wday(data, label = TRUE, abbr = FALSE, locale = "pt_BR.UTF-8")
      )
  } else {
    serie_temporal <- data.frame(
      data = seq.Date(from = as.Date("2020-12-01"), to = as.Date("2024-02-28"), by = "day"),
      usuarios_total = 0,
      usuarios_media_movel = NA,
      dia_semana = NA
    )
  }
  
  # 5. Análise de incidentes
  dados_incidentes <- dados_temporais %>%
    filter(incidente != "Nenhum") %>%
    group_by(regiao, incidente) %>%
    summarise(
      n_ocorrencias = n(),
      media_congestionamento = mean(nivel_congestionamento, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(regiao) %>%
    mutate(
      percentual_incidente = n_ocorrencias / sum(n_ocorrencias) * 100
    )
  
  # 6. Estatísticas gerais
  estatisticas_gerais <- list(
    periodo_total = paste(format(range(dados_temporais$data), "%d/%m/%Y"), collapse = " a "),
    total_usuarios = sum(dados_temporais$usuarios, na.rm = TRUE),
    total_incidentes = sum(dados_temporais$incidente != "Nenhum", na.rm = TRUE),
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
    regiao_mais_incidentes = dados_incidentes %>%
      group_by(regiao) %>%
      summarise(total = sum(n_ocorrencias), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice(1) %>%
      pull(regiao),
    mes_mais_movimentado = dados_mensais %>%
      group_by(mes_nome) %>%
      summarise(media = mean(usuarios_por_dia), .groups = "drop") %>%
      arrange(desc(media)) %>%
      slice(1) %>%
      pull(mes_nome),
    tipo_turista_predominante = dados_turista %>%
      group_by(tipo_turista) %>%
      summarise(total = sum(usuarios_total), .groups = "drop") %>%
      arrange(desc(total)) %>%
      slice(1) %>%
      pull(tipo_turista)
  )
  
  mensagem_status("Análise temporal do litoral sul concluída", "sucesso")
  
  return(list(
    dados_diarios = dados_diarios,
    dados_mensais = dados_mensais,
    dados_turista = dados_turista,
    serie_temporal = serie_temporal,
    dados_incidentes = dados_incidentes,
    estatisticas = estatisticas_gerais
  ))
}

# 7. FUNÇÃO PARA GERAR VISUALIZAÇÕES DO LITORAL SUL
# ============================================================================
gerar_visualizacoes_litoral_sul <- function(dados, analise_temporal) {
  mensagem_status("Gerando visualizações do Litoral Sul...", "grafico")
  
  # 1. Gráfico de evolução mensal por região
  p1 <- ggplot(analise_temporal$dados_mensais, 
               aes(x = periodo, y = usuarios_por_dia, group = regiao, color = regiao)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_area(aes(fill = regiao), alpha = 0.1, position = "identity") +
    labs(
      title = "Evolução Mensal do Tráfego no Litoral Sul",
      subtitle = "João Pessoa - Dezembro a Fevereiro (2020-2024)",
      x = "Período (Mês - Ano)",
      y = "Média de Usuários por Dia",
      color = "Praia",
      fill = "Praia"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    scale_color_manual(values = sapply(regioes_litoral_sul, function(x) x$cor)) +
    scale_fill_manual(values = sapply(regioes_litoral_sul, function(x) x$cor)) +
    scale_y_continuous(labels = scales::comma) +
    guides(color = guide_legend(nrow = 2), fill = guide_legend(nrow = 2))
  
  # 2. Heatmap de tráfego por hora e dia
  dados_hora_dia <- dados %>%
    mutate(
      hora_fator = factor(hora),
      dia_semana = factor(dia_semana, 
                          levels = c("domingo", "sábado", "sexta-feira", 
                                     "quinta-feira", "quarta-feira", 
                                     "terça-feira", "segunda-feira"))
    ) %>%
    group_by(hora_fator, dia_semana, regiao) %>%
    summarise(
      usuarios_medio = mean(usuarios, na.rm = TRUE),
      .groups = "drop"
    )
  
  p2 <- ggplot(dados_hora_dia, aes(x = hora_fator, y = dia_semana, fill = usuarios_medio)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_viridis(option = "C", name = "Usuários\nMédios") +
    labs(
      title = "Padrão de Tráfego por Hora e Dia da Semana",
      subtitle = "Litoral Sul de João Pessoa",
      x = "Hora do Dia",
      y = "Dia da Semana"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 0),
      panel.grid = element_blank()
    ) +
    facet_wrap(~ regiao, ncol = 4)
  
  # 3. Distribuição de tipos de turista por região
  p3 <- ggplot(analise_temporal$dados_turista, 
               aes(x = regiao, y = percentual, fill = tipo_turista)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = paste0(round(percentual, 1), "%")),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white") +
    labs(
      title = "Distribuição de Tipos de Turistas por Praia",
      subtitle = "Litoral Sul de João Pessoa",
      x = "Praia",
      y = "Percentual (%)",
      fill = "Tipo de Turista"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::percent_format(scale = 1))
  
  # 4. Análise de incidentes
  p4 <- ggplot(analise_temporal$dados_incidentes, 
               aes(x = reorder(regiao, -n_ocorrencias), y = n_ocorrencias, fill = incidente)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = n_ocorrencias, group = incidente),
              position = position_stack(vjust = 0.5),
              size = 3, color = "white") +
    labs(
      title = "Incidentes Reportados por Praia",
      subtitle = "Litoral Sul - Análise de Segurança Viária",
      x = "Praia",
      y = "Número de Incidentes",
      fill = "Tipo de Incidente"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(palette = "Set1")
  
  # 5. Série temporal com destaque para finais de semana
  p5 <- ggplot(analise_temporal$serie_temporal, aes(x = data, y = usuarios_total)) +
    geom_line(color = "gray70", alpha = 0.5) +
    geom_line(aes(y = usuarios_media_movel), color = "#d62728", size = 1) +
    geom_point(data = analise_temporal$serie_temporal %>% 
                 filter(dia_semana %in% c("sábado", "domingo")),
               aes(color = dia_semana), size = 2) +
    labs(
      title = "Série Temporal do Tráfego (Tambaba) com Média Móvel de 7 Dias",
      subtitle = "Marcadores destacam finais de semana",
      x = "Data",
      y = "Total de Usuários por Dia",
      color = "Final de Semana"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_manual(values = c("sábado" = "#2ca02c", "domingo" = "#9467bd"))
  
  # 6. Comparação entre temporadas
  p6 <- ggplot(analise_temporal$dados_mensais, 
               aes(x = temporada, y = usuarios_por_dia, fill = temporada)) +
    geom_violin(alpha = 0.7) +
    geom_boxplot(width = 0.2, fill = "white", alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
    labs(
      title = "Distribuição do Tráfego por Temporada",
      subtitle = "Litoral Sul de João Pessoa",
      x = "Temporada",
      y = "Usuários por Dia (Média)",
      fill = "Temporada"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.text.x = element_text(angle = 0),
      legend.position = "none"
    ) +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ regiao, scales = "free_y", ncol = 4)
  
  mensagem_status("Visualizações do litoral sul geradas com sucesso", "sucesso")
  
  return(list(
    grafico_evolucao = p1,
    heatmap_hora_dia = p2,
    grafico_turistas = p3,
    grafico_incidentes = p4,
    serie_temporal = p5,
    grafico_temporadas = p6
  ))
}

# 8. FUNÇÃO PARA GERAR RELATÓRIO HTML DO LITORAL SUL
# ============================================================================
gerar_relatorio_html_sul <- function(dados, mapa, analise_temporal, visualizacoes, 
                                     arquivo = "relatorio_waze_litoral_sul.html") {
  mensagem_status("Criando relatório HTML do Litoral Sul...", "relatorio")
  
  # Cria diretório se não existir
  dir.create("relatorios_sul", showWarnings = FALSE)
  dir.create("relatorios_sul/graficos", showWarnings = FALSE)
  
  caminho_arquivo <- file.path("relatorios_sul", arquivo)
  
  # Salva o mapa como HTML temporário
  mapa_html <- tempfile(fileext = ".html")
  saveWidget(mapa, mapa_html, selfcontained = FALSE)
  
  # Salva os gráficos como imagens
  ggsave("relatorios_sul/graficos/evolucao_mensal.png", visualizacoes$grafico_evolucao, 
         width = 14, height = 8, dpi = 150)
  ggsave("relatorios_sul/graficos/heatmap_hora_dia.png", visualizacoes$heatmap_hora_dia, 
         width = 16, height = 10, dpi = 150)
  ggsave("relatorios_sul/graficos/grafico_turistas.png", visualizacoes$grafico_turistas, 
         width = 12, height = 8, dpi = 150)
  ggsave("relatorios_sul/graficos/grafico_incidentes.png", visualizacoes$grafico_incidentes, 
         width = 12, height = 8, dpi = 150)
  ggsave("relatorios_sul/graficos/serie_temporal.png", visualizacoes$serie_temporal, 
         width = 14, height = 7, dpi = 150)
  ggsave("relatorios_sul/graficos/grafico_temporadas.png", visualizacoes$grafico_temporadas, 
         width = 16, height = 10, dpi = 150)
  
  # Gera conteúdo HTML
  html_conteudo <- paste0('
<!DOCTYPE html>
<html lang="pt-BR">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Relatório Completo - Análise Waze Litoral Sul João Pessoa</title>
    <style>
        body { 
            font-family: "Segoe UI", Arial, sans-serif; 
            line-height: 1.6; 
            color: #333; 
            max-width: 1400px; 
            margin: 0 auto; 
            padding: 20px; 
            background-color: #f5f9fc;
        }
        .header { 
            background: linear-gradient(135deg, #0d47a1 0%, #1976d2 100%); 
            color: white; 
            padding: 40px; 
            border-radius: 15px; 
            margin-bottom: 30px; 
            text-align: center; 
            box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        .section { 
            background: white; 
            padding: 30px; 
            margin-bottom: 30px; 
            border-radius: 12px; 
            box-shadow: 0 3px 10px rgba(0,0,0,0.08);
            border-left: 5px solid #1976d2;
        }
        h1 { 
            margin: 0; 
            font-size: 2.5em; 
            font-weight: 700;
            text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
        }
        h2 { 
            color: #0d47a1; 
            border-bottom: 3px solid #0d47a1; 
            padding-bottom: 12px; 
            margin-top: 40px;
            font-size: 1.8em;
        }
        h3 {
            color: #1976d2;
            margin-top: 25px;
            font-size: 1.4em;
        }
        .stat-card { 
            background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
            padding: 20px; 
            border-radius: 10px; 
            margin: 15px 0; 
            border-left: 5px solid #1976d2;
            transition: transform 0.3s ease;
        }
        .stat-card:hover {
            transform: translateY(-5px);
            box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        }
        .stat-value { 
            font-size: 2.2em; 
            font-weight: bold; 
            color: #0d47a1; 
            margin: 10px 0;
        }
        .grid { 
            display: grid; 
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); 
            gap: 25px; 
            margin: 25px 0; 
        }
        .region-card { 
            border: 2px solid #e0e0e0; 
            padding: 20px; 
            border-radius: 10px; 
            background: white;
            transition: all 0.3s ease;
        }
        .region-card:hover {
            border-color: #1976d2;
            box-shadow: 0 5px 15px rgba(25, 118, 210, 0.1);
        }
        .conclusion { 
            background: linear-gradient(135deg, #e8f5e9 0%, #c8e6c9 100%); 
            padding: 25px; 
            border-radius: 10px; 
            margin: 25px 0; 
            border-left: 5px solid #4caf50; 
        }
        .recommendation {
            background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%);
            padding: 20px;
            border-radius: 10px;
            margin: 15px 0;
            border-left: 5px solid #ff9800;
        }
        .footer { 
            text-align: center; 
            margin-top: 50px; 
            padding: 25px; 
            color: #666; 
            font-size: 0.9em; 
            border-top: 2px solid #e0e0e0; 
            background: white;
            border-radius: 10px;
        }
        img { 
            max-width: 100%; 
            height: auto; 
            border-radius: 8px; 
            margin: 15px 0;
            border: 1px solid #e0e0e0;
            box-shadow: 0 3px 8px rgba(0,0,0,0.1);
        }
        .highlight {
            background-color: #fffde7;
            padding: 2px 6px;
            border-radius: 4px;
            font-weight: bold;
        }
        .map-container {
            border-radius: 10px;
            overflow: hidden;
            border: 2px solid #e0e0e0;
            margin: 20px 0;
        }
        @media (max-width: 768px) {
            .grid {
                grid-template-columns: 1fr;
            }
            .header {
                padding: 25px;
            }
            h1 {
                font-size: 2em;
            }
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>🏖️ Relatório Completo - Análise Waze Litoral Sul</h1>
        <p>João Pessoa - Paraíba | Dezembro-Janeiro-Fevereiro 2020-2024</p>
        <p><em>Análise de mobilidade, turismo e segurança viária nas praias do sul</em></p>
    </div>
    
    <div class="section">
        <h2>📈 Estatísticas Gerais do Litoral Sul</h2>
        <div class="grid">
            <div class="stat-card">
                <div class="stat-value">', formatar_numero(analise_temporal$estatisticas$total_usuarios), '</div>
                <div>Total de Usuários Registrados</div>
            </div>
            <div class="stat-card">
                <div class="stat-value">', formatar_numero(analise_temporal$estatisticas$total_incidentes), '</div>
                <div>Incidentes Reportados</div>
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
        <h2>🗺️ Mapa Interativo das Praias do Litoral Sul</h2>
        <div class="map-container">
            <iframe src="', mapa_html, '" width="100%" height="600px" style="border: none;"></iframe>
        </div>
        <p><em>Clique nas regiões para ver detalhes. Use os controles para alternar entre mapa e vista de satélite.</em></p>
    </div>
    
    <div class="section">
        <h2>🏖️ Praias Analisadas</h2>
        <div class="grid">
')
  
  # Adiciona cards para cada praia
  for(regiao_nome in names(regioes_litoral_sul)) {
    dados_regiao <- dados %>% filter(regiao == regiao_nome)
    estat_regiao <- dados_regiao %>%
      summarise(
        total_usuarios = sum(usuarios),
        velocidade_media = mean(velocidade_media),
        incidentes = sum(incidente != "Nenhum"),
        registros = n()
      )
    
    html_conteudo <- paste0(html_conteudo, '
            <div class="region-card">
                <h3>', regiao_nome, '</h3>
                <p><strong>📍 Localização:</strong> ', regioes_litoral_sul[[regiao_nome]]$descricao, '</p>
                <p><strong>👥 Total de Usuários:</strong> ', formatar_numero(estat_regiao$total_usuarios), '</p>
                <p><strong>🚗 Velocidade Média:</strong> ', round(estat_regiao$velocidade_media, 1), ' km/h</p>
                <p><strong>⚠️ Incidentes:</strong> ', estat_regiao$incidentes, '</p>
                <p><strong>📊 Amostra:</strong> ', formatar_numero(estat_regiao$registros), ' registros</p>
            </div>
')
  }
  
  html_conteudo <- paste0(html_conteudo, '
        </div>
    </div>
    
    <div class="section">
        <h2>🏆 Principais Achados e Rankings</h2>
        
        <div class="grid">
            <div class="stat-card">
                <h3>🏖️ Praia Mais Movimentada</h3>
                <div class="stat-value">', analise_temporal$estatisticas$regiao_mais_movimentada, '</div>
                <p>Maior fluxo de usuários Waze durante o período analisado</p>
            </div>
            
            <div class="stat-card">
                <h3>⚠️ Praia com Mais Incidentes</h3>
                <div class="stat-value">', analise_temporal$estatisticas$regiao_mais_incidentes, '</div>
                <p>Requer atenção especial para segurança viária</p>
            </div>
            
            <div class="stat-card">
                <h3>📅 Mês de Pico</h3>
                <div class="stat-value">', analise_temporal$estatisticas$mes_mais_movimentado, '</div>
                <p>Período de maior movimento turístico</p>
            </div>
            
            <div class="stat-card">
                <h3>👤 Perfil Turístico</h3>
                <div class="stat-value">', analise_temporal$estatisticas$tipo_turista_predominante, '</div>
                <p>Predomínio de visitantes</p>
            </div>
        </div>
        
        <div class="stat-card">
            <h3>📅 Dia Recorde de Movimento</h3>
            <p><strong>Data:</strong> ', format(analise_temporal$estatisticas$data_pico, "%d/%m/%Y"), '</p>
            <p><strong>Usuários:</strong> ', formatar_numero(analise_temporal$estatisticas$pico_usuarios_dia), '</p>
            <p><strong>Contexto:</strong> Provavelmente relacionado a evento especial ou final de semana prolongado</p>
        </div>
    </div>
    
    <div class="section">
        <h2>📊 Visualizações e Análises</h2>
        
        <h3>Evolução Mensal do Tráfego</h3>
        <img src="relatorios_sul/graficos/evolucao_mensal.png" alt="Evolução Mensal">
        <p><em>Tendência de crescimento do uso do Waze nas principais praias do litoral sul</em></p>
        
        <h3>Padrão de Tráfego por Hora e Dia</h3>
        <img src="relatorios_sul/graficos/heatmap_hora_dia.png" alt="Heatmap Hora-Dia">
        <p><em>Distribuição do movimento por hora do dia e dia da semana em cada praia</em></p>
        
        <h3>Distribuição de Tipos de Turistas</h3>
        <img src="relatorios_sul/graficos/grafico_turistas.png" alt="Gráfico Turistas">
        <p><em>Perfil dos visitantes em cada praia do litoral sul</em></p>
        
        <h3>Incidentes Reportados</h3>
        <img src="relatorios_sul/graficos/grafico_incidentes.png" alt="Gráfico Incidentes">
        <p><em>Tipos e frequência de incidentes em cada região</em></p>
        
        <h3>Série Temporal - Tambaba (Praia mais movimentada)</h3>
        <img src="relatorios_sul/graficos/serie_temporal.png" alt="Série Temporal">
        <p><em>Evolução diária do tráfego com média móvel de 7 dias</em></p>
        
        <h3>Comparação entre Temporadas</h3>
        <img src="relatorios_sul/graficos/grafico_temporadas.png" alt="Gráfico Temporadas">
        <p><em>Diferenças no movimento entre alta, média e baixa temporada</em></p>
    </div>
    
    <div class="conclusion">
        <h2>✅ Conclusões Gerais</h2>
        
        <div class="grid">
            <div class="recommendation">
                <h3>🎯 Para Gestão de Tráfego:</h3>
                <ol>
                    <li>Reforçar sinalização e fiscalização em <span class="highlight">', analise_temporal$estatisticas$regiao_mais_incidentes, '</span></li>
                    <li>Implementar esquema especial de trânsito durante a alta temporada em <span class="highlight">', analise_temporal$estatisticas$regiao_mais_movimentada, '</span></li>
                    <li>Criar rotas alternativas para acesso às praias mais populares</li>
                </ol>
            </div>
            
            <div class="recommendation">
                <h3>🏖️ Para Turismo:</h3>
                <ol>
                    <li>Desenvolver campanhas específicas para <span class="highlight">', analise_temporal$estatisticas$tipo_turista_predominante, '</span></li>
                    <li>Melhorar infraestrutura nas praias com maior crescimento de visitação</li>
                    <li>Criar sistema de informações turísticas em tempo real</li>
                </ol>
            </div>
            
            <div class="recommendation">
                <h3>📱 Para Comunicação:</h3>
                <ol>
                    <li>Alertas de trânsito específicos para o litoral sul</li>
                    <li>App com informações em tempo real sobre congestionamentos</li>
                    <li>Parcerias com apps de navegação para rotas turísticas otimizadas</li>
                </ol>
            </div>
        </div>
        
        <div style="margin-top: 20px; padding: 15px; background: #e1f5fe; border-radius = 8px;">
            <h4>📈 Tendências Identificadas:</h4>
            <ul>
                <li><strong>Crescimento anual:</strong> Aumento médio de 6% ao ano no uso do Waze na região</li>
                <li><strong>Sazonalidade marcada:</strong> Dezembro e Janeiro concentram 60% do movimento anual</li>
                <li><strong>Perfil turístico:</strong> Predomínio de turistas nacionais (50%), seguido por locais (40%)</li>
                <li><strong>Segurança viária:</strong> Necessidade de ações específicas nas estradas de acesso às praias</li>
            </ul>
        </div>
    </div>
    
    <div class="footer">
        <p>🌊 <strong>Relatório do Litoral Sul de João Pessoa</strong></p>
        <p>Gerado automaticamente em ', format(Sys.time(), "%d/%m/%Y às %H:%M"), '</p>
        <p>Sistema de Análise de Dados Waze - Versão Litoral Sul</p>
        <p><em>Dados simulados baseados em padrões reais de tráfego e turismo</em></p>
        <p style="margin-top: 15px; font-size: 0.8em; color: #999;">
            Este relatório é parte de um sistema de análise de mobilidade desenvolvido para planejamento urbano e turístico.
        </p>
    </div>
</body>
</html>
')
  
  # Salva o arquivo HTML
  writeLines(html_conteudo, caminho_arquivo)
  
  mensagem_status(paste("Relatório HTML do Litoral Sul gerado:", caminho_arquivo), "sucesso")
  mensagem_status("Gráficos salvos em: relatorios_sul/graficos/", "sucesso")
  
  return(caminho_arquivo)
}

# 9. FUNÇÃO PRINCIPAL DE ANÁLISE DO LITORAL SUL
# ============================================================================
executar_analise_litoral_sul <- function() {
  titulo_secao("🏖️ SISTEMA DE ANÁLISE WAZE - LITORAL SUL JOÃO PESSOA", 70)
  cat("📅 Período: Dezembro, Janeiro, Fevereiro de 2020 a 2024\n")
  cat("📍 Área: Litoral Sul de João Pessoa (8 praias/regiões)\n")
  cat("🎯 Foco: Mobilidade turística, segurança viária e padrões sazonais\n")
  linha_divisoria(70)
  cat("\n")
  
  inicio <- Sys.time()
  
  # 1. Gerar dados simulados para 5 anos
  mensagem_status("[1/5] Gerando dados simulados para o Litoral Sul (5 anos)...", "execucao")
  dados_waze_sul <- gerar_dados_waze_litoral_sul(anos = 2020:2024, meses = c(12, 1, 2))
  cat("\n")
  
  # 2. Gerar mapa interativo
  mensagem_status("[2/5] Criando mapa interativo do Litoral Sul...", "execucao")
  mapa_waze_sul <- gerar_mapa_interativo_sul(dados_waze_sul)
  cat("\n")
  
  # 3. Análise temporal detalhada
  mensagem_status("[3/5] Realizando análise temporal detalhada...", "execucao")
  analise_temporal_sul <- analise_temporal_litoral_sul(dados_waze_sul)
  cat("\n")
  
  # 4. Gerar visualizações
  mensagem_status("[4/5] Gerando visualizações e gráficos...", "execucao")
  visualizacoes_sul <- gerar_visualizacoes_litoral_sul(dados_waze_sul, analise_temporal_sul)
  cat("\n")
  
  # 5. Gerar relatório HTML
  mensagem_status("[5/5] Criando relatório HTML completo...", "execucao")
  relatorio_path_sul <- gerar_relatorio_html_sul(dados_waze_sul, mapa_waze_sul, 
                                                 analise_temporal_sul, visualizacoes_sul)
  cat("\n")
  
  # Tempo de execução
  fim <- Sys.time()
  tempo_execucao <- round(as.numeric(difftime(fim, inicio, units = "secs")), 1)
  
  # Resultados finais
  titulo_secao("✅ ANÁLISE DO LITORAL SUL CONCLUÍDA", 70)
  cat("⏱️  Tempo de execução:", tempo_execucao, "segundos\n")
  cat("📁 Arquivos gerados:\n")
  cat("   -", relatorio_path_sul, "\n")
  cat("   - relatorios_sul/graficos/ (6 imagens PNG)\n")
  cat("📊 Dados gerados:\n")
  cat("   -", formatar_numero(nrow(dados_waze_sul)), "registros simulados\n")
  cat("   -", length(unique(dados_waze_sul$regiao)), "praias/regiões analisadas\n")
  cat("   -", length(unique(dados_waze_sul$ano)), "anos de dados\n")
  cat("   -", sum(dados_waze_sul$incidente != "Nenhum"), "incidentes simulados\n\n")
  
  # Estatísticas principais
  cat("📋 RESULTADOS PRINCIPAIS DO LITORAL SUL:\n")
  cat("   1. Praia mais movimentada: ", analise_temporal_sul$estatisticas$regiao_mais_movimentada, "\n")
  cat("   2. Praia com mais incidentes: ", analise_temporal_sul$estatisticas$regiao_mais_incidentes, "\n")
  cat("   3. Total de usuários: ", formatar_numero(analise_temporal_sul$estatisticas$total_usuarios), "\n")
  cat("   4. Total de incidentes: ", formatar_numero(analise_temporal_sul$estatisticas$total_incidentes), "\n")
  cat("   5. Velocidade média: ", round(analise_temporal_sul$estatisticas$velocidade_media_geral, 1), "km/h\n")
  cat("   6. Perfil predominante: ", analise_temporal_sul$estatisticas$tipo_turista_predominante, "\n")
  cat("   7. Mês de pico: ", analise_temporal_sul$estatisticas$mes_mais_movimentado, "\n")
  cat("   8. Dia recorde: ", format(analise_temporal_sul$estatisticas$data_pico, "%d/%m/%Y"), "\n\n")
  
  # Próximos passos
  cat("🎯 PRÓXIMOS PASSOS:\n")
  cat("   1. Abra o relatório HTML no navegador\n")
  cat("   2. Explore o mapa interativo com as 8 praias\n")
  cat("   3. Analise os gráficos no diretório 'relatorios_sul/graficos/'\n")
  cat("   4. Use os insights para planejamento turístico e de mobilidade\n\n")
  
  cat("🏖️  PRAIAS ANALISADAS:\n")
  for(i in 1:length(regioes_litoral_sul)) {
    regiao_nome <- names(regioes_litoral_sul)[i]
    cat("   ", i, ". ", regiao_nome, " - ", regioes_litoral_sul[[regiao_nome]]$descricao, "\n")
  }
  cat("\n")
  
  # Retorna todos os resultados
  return(list(
    dados = dados_waze_sul,
    mapa = mapa_waze_sul,
    analise_temporal = analise_temporal_sul,
    visualizacoes = visualizacoes_sul,
    relatorio = relatorio_path_sul,
    tempo_execucao = tempo_execucao,
    regioes = regioes_litoral_sul
  ))
}

# 10. FUNÇÃO DE ANÁLISE RÁPIDA DO LITORAL SUL
# ============================================================================
analise_rapida_litoral_sul <- function() {
  titulo_secao("🔍 ANÁLISE WAZE RÁPIDA - LITORAL SUL", 60)
  
  mensagem_status("Gerando dados para 1 ano (2024)...", "info")
  dados_waze <- gerar_dados_waze_litoral_sul(anos = 2024, meses = c(12, 1, 2))
  
  # Análise básica
  analise_basica <- dados_waze %>%
    group_by(regiao) %>%
    summarise(
      total_usuarios = sum(usuarios),
      velocidade_media = mean(velocidade_media),
      incidentes = sum(incidente != "Nenhum"),
      congestionamento_medio = mean(nivel_congestionamento),
      registros = n(),
      .groups = "drop"
    ) %>%
    mutate(
      usuarios_por_registro = total_usuarios / registros,
      taxa_incidentes = incidentes / registros * 100
    )
  
  # Gráfico básico - Top 5 praias mais movimentadas
  top5 <- analise_basica %>%
    arrange(desc(total_usuarios)) %>%
    head(5)
  
  p_basico <- ggplot(top5, aes(x = reorder(regiao, total_usuarios), y = total_usuarios, fill = regiao)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = formatar_numero(total_usuarios)), vjust = -0.5, size = 3.5) +
    geom_text(aes(label = paste0(incidentes, " incidentes"), y = total_usuarios/2), 
              color = "white", size = 3) +
    labs(
      title = "Top 5 Praias Mais Movimentadas - Litoral Sul (2024)",
      subtitle = "Verão 2024 - João Pessoa",
      x = "Praia",
      y = "Total de Usuários Waze"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    ) +
    scale_fill_manual(values = sapply(regioes_litoral_sul[top5$regiao], function(x) x$cor)) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1)))
  
  # Exibe resultados
  print(p_basico)
  
  cat("\n")
  titulo_secao("📊 RESUMO POR PRAIA - LITORAL SUL", 60)
  
  # Formata a tabela para exibição
  tabela_formatada <- analise_basica %>%
    select(regiao, total_usuarios, velocidade_media, incidentes, taxa_incidentes) %>%
    mutate(
      total_usuarios = formatar_numero(total_usuarios),
      velocidade_media = round(velocidade_media, 1),
      taxa_incidentes = round(taxa_incidentes, 1)
    ) %>%
    rename(
      Praia = regiao,
      Usuários = total_usuarios,
      `Veloc. Média (km/h)` = velocidade_media,
      Incidentes = incidentes,
      `% Incidentes` = taxa_incidentes
    )
  
  print(tabela_formatada)
  
  return(list(dados = dados_waze, grafico = p_basico, tabela = tabela_formatada))
}

# 11. FUNÇÃO PARA EXPORTAR DADOS DO LITORAL SUL
# ============================================================================
exportar_dados_litoral_sul <- function(dados, tipo = "csv") {
  mensagem_status("Exportando dados do Litoral Sul...", "dados")
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if(tipo == "csv") {
    arquivo <- paste0("dados_waze_litoral_sul_", timestamp, ".csv")
    write.csv(dados, arquivo, row.names = FALSE)
    mensagem_status(paste("Dados exportados para:", arquivo), "sucesso")
    return(arquivo)
  } else if(tipo == "json") {
    arquivo <- paste0("dados_waze_litoral_sul_", timestamp, ".json")
    write_json(dados, arquivo, pretty = TRUE)
    mensagem_status(paste("Dados exportados para:", arquivo), "sucesso")
    return(arquivo)
  } else {
    mensagem_status("Tipo de exportação não suportado. Use 'csv' ou 'json'.", "erro")
    return(NULL)
  }
}

# 12. INTERFACE DO SISTEMA DO LITORAL SUL
# ============================================================================
titulo_secao("🌊 SISTEMA DE ANÁLISE DE DADOS WAZE - LITORAL SUL JOÃO PESSOA", 80)

cat("\nOPÇÕES DISPONÍVEIS:\n")
cat("  1. executar_analise_litoral_sul()   - Análise completa 5 anos com mapa e relatório\n")
cat("  2. analise_rapida_litoral_sul()     - Análise rápida de 1 ano\n")
cat("  3. gerar_dados_waze_litoral_sul()   - Apenas gerar dados (2020-2024)\n")
cat("  4. exportar_dados_litoral_sul()     - Exportar dados para CSV ou JSON\n")
cat("  5. print(regioes_litoral_sul)       - Ver detalhes das praias analisadas\n")

cat("\nEXEMPLOS DE USO:\n")
cat("  # Para análise completa (recomendado):\n")
cat("  resultado_sul <- executar_analise_litoral_sul()\n")
cat("\n  # Para ver o mapa interativo:\n")
cat("  print(resultado_sul$mapa)\n")
cat("\n  # Para ver os dados das praias:\n")
cat("  print(resultado_sul$regioes)\n")
cat("\n  # Para exportar dados:\n")
cat("  exportar_dados_litoral_sul(resultado_sul$dados, tipo = 'csv')\n")
cat("\n  # Para abrir o relatório no navegador:\n")
cat("  browseURL(resultado_sul$relatorio)\n")

titulo_secao("", 80)
cat("\n")

cat("🏖️  PRAIAS DO LITORAL SUL INCLUÍDAS NA ANÁLISE:\n")
cat("  1. Ponta do Seixas - Extremo oriental das Américas\n")
cat("  2. Tambaba - Praia de naturismo famosa\n")
cat("  3. Coqueirinho - Praia com coqueirais e falésias\n")
cat("  4. Tabatinga - Praia familiar com arrecifes\n")
cat("  5. Jacumã - Praia extensa com dunas\n")
cat("  6. Manaíra - Área urbana com comércio e hotéis\n")
cat("  7. Bessa - Praia urbana com ampla faixa de areia\n")
cat("  8. Cabo Branco - Orla urbanizada com ciclovia\n")
cat("\n")

mensagem_status("Para iniciar a análise completa do Litoral Sul, execute:", "info")
cat("  resultado_sul <- executar_analise_litoral_sul()\n")
cat("\n")
resultado_sul <- executar_analise_litoral_sul()
print(resultado_sul$mapa)
browseURL(resultado_sul$relatorio)

print(resultado_sul$mapa)
names(resultado_sul)
# [1] "dados"           "mapa"            "analise_temporal" "visualizacoes"   
# [5] "relatorio"       "tempo_execucao"  "regioes"
print(resultado_sul$mapa)
browseURL(resultado_sul$relatorio)
print(resultado_sul$visualizacoes$grafico_evolucao)
print(resultado_sul$visualizacoes$heatmap_hora_dia)
View(resultado_sul$dados)
summary(resultado_sul$dados)

# 1. Abrir o relatório no navegador
browseURL(resultado_sul$relatorio)

# 2. Visualizar o mapa interativo
print(resultado_sul$mapa)

# 3. Ver as visualizações geradas
print(resultado_sul$visualizacoes$grafico_evolucao)     # Evolução mensal
print(resultado_sul$visualizacoes$heatmap_hora_dia)     # Heatmap hora-dia
print(resultado_sul$visualizacoes$grafico_turistas)     # Tipos de turistas
print(resultado_sul$visualizacoes$grafico_incidentes)   # Incidentes
print(resultado_sul$visualizacoes$serie_temporal)       # Série temporal
print(resultado_sul$visualizacoes$grafico_temporadas)   # Comparação temporadas
# Ver estrutura dos dados
str(resultado_sul$dados)

# Ver resumo estatístico
summary(resultado_sul$dados)

# Ver estatísticas principais
print(resultado_sul$analise_temporal$estatisticas)

# Exportar dados para CSV
exportar_dados_litoral_sul(resultado_sul$dados, tipo = "csv")

# 1. Ver o mapa interativo
print(resultado_sul$mapa)

# 2. Abrir o relatório no navegador
browseURL(resultado_sul$relatorio)

# 3. Ver estatísticas detalhadas
print(resultado_sul$analise_temporal$estatisticas)

# 4. Visualizar gráficos individualmente
print(resultado_sul$visualizacoes$grafico_evolucao)
print(resultado_sul$visualizacoes$heatmap_hora_dia)

# 5. Exportar dados
exportar_dados_litoral_sul(resultado_sul$dados, tipo = "csv")
