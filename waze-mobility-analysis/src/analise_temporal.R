# ==============================================================================
# ANÁLISE DE SÉRIES TEMPORAIS - WAZE MOBILITY
# ==============================================================================

# ==============================================================================
# 7. ANÁLISE DE SÉRIES TEMPORAIS
# ==============================================================================

analise_series_temporais <- function(dados) {
  
  secao("📈 ANÁLISE DE SÉRIES TEMPORAIS")
  
  resultados <- list()
  
  # ============================================================================
  # 7.1 Preparação dos dados temporais
  # ============================================================================
  
  msg("Preparando séries temporais...", "dados")
  
  # Agregação diária
  serie_diaria <- dados %>%
    group_by(data) %>%
    summarise(
      usuarios_total = sum(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento),
      n_acidentes = sum(tem_acidente),
      .groups = "drop"
    ) %>%
    arrange(data)
  
  # Criar objeto ts (série temporal)
  ts_usuarios <- ts(serie_diaria$usuarios_total, 
                    start = c(year(min(serie_diaria$data)), yday(min(serie_diaria$data))),
                    frequency = 365)
  
  resultados$serie_diaria <- serie_diaria
  resultados$ts_usuarios <- ts_usuarios
  
  # ============================================================================
  # 7.2 Decomposição da Série Temporal
  # ============================================================================
  
  msg("Decompondo série temporal (tendência, sazonalidade, resíduos)...", "estatistica")
  
  # Decomposição STL (Seasonal and Trend decomposition using Loess)
  decomp_stl <- stl(ts_usuarios, s.window = "periodic")
  
  resultados$decomposicao <- decomp_stl
  
  # Extrair componentes
  tendencia <- as.numeric(decomp_stl$time.series[, "trend"])
  sazonalidade <- as.numeric(decomp_stl$time.series[, "seasonal"])
  residuos <- as.numeric(decomp_stl$time.series[, "remainder"])
  
  serie_diaria$tendencia <- tendencia
  serie_diaria$sazonalidade <- sazonalidade
  serie_diaria$residuos <- residuos
  
  # Força da tendência e sazonalidade
  forca_tendencia <- var(tendencia) / var(tendencia + residuos)
  forca_sazonalidade <- var(sazonalidade) / var(sazonalidade + residuos)
  
  resultados$forca_componentes <- list(
    tendencia = forca_tendencia,
    sazonalidade = forca_sazonalidade
  )
  
  cat("  ✓ Força da tendência:", round(forca_tendencia, 4), "\n")
  cat("  ✓ Força da sazonalidade:", round(forca_sazonalidade, 4), "\n\n")
  
  # ============================================================================
  # 7.3 Testes de Estacionariedade
  # ============================================================================
  
  msg("Testando estacionariedade da série...", "estatistica")
  
  # Teste de Dickey-Fuller Aumentado (ADF)
  teste_adf <- adf.test(ts_usuarios, alternative = "stationary")
  
  # Teste KPSS
  teste_kpss <- kpss.test(ts_usuarios, null = "Trend")
  
  # Teste de Phillips-Perron
  teste_pp <- pp.test(ts_usuarios, alternative = "stationary")
  
  resultados$testes_estacionariedade <- list(
    adf = teste_adf,
    kpss = teste_kpss,
    phillips_perron = teste_pp
  )
  
  cat("  ✓ Teste ADF p-value:", round(teste_adf$p.value, 4), "\n")
  cat("  ✓ Teste KPSS p-value:", round(teste_kpss$p.value, 4), "\n")
  cat("  ✓ Teste PP p-value:", round(teste_pp$p.value, 4), "\n\n")
  
  # ============================================================================
  # 7.4 Detecção de Mudanças Estruturais (Change Points)
  # ============================================================================
  
  msg("Detectando mudanças estruturais na série...", "estatistica")
  
  # Análise de change points usando PELT
  cpt_mean <- cpt.mean(ts_usuarios, method = "PELT", penalty = "BIC")
  cpt_var <- cpt.var(ts_usuarios, method = "PELT", penalty = "BIC")
  cpt_meanvar <- cpt.meanvar(ts_usuarios, method = "PELT", penalty = "BIC")
  
  # Datas dos change points
  if(length(cpts(cpt_meanvar)) > 0) {
    datas_changepoints <- serie_diaria$data[cpts(cpt_meanvar)]
    cat("  ✓ Mudanças estruturais detectadas em:\n")
    for(d in datas_changepoints) {
      cat("    -", format(d, "%d/%m/%Y"), "\n")
    }
  } else {
    cat("  ✓ Nenhuma mudança estrutural significativa detectada\n")
  }
  cat("\n")
  
  resultados$change_points <- list(
    media = cpt_mean,
    variancia = cpt_var,
    media_variancia = cpt_meanvar
  )
  
  # ============================================================================
  # 7.5 Análise de Tendência (Mann-Kendall)
  # ============================================================================
  
  msg("Testando tendência monotônica (Mann-Kendall)...", "estatistica")
  
  # Teste de Mann-Kendall
  mk_test <- mk.test(ts_usuarios)
  
  # Teste de Sen's Slope
  sens_slope <- sens.slope(ts_usuarios)
  
  resultados$teste_tendencia <- list(
    mann_kendall = mk_test,
    sens_slope = sens_slope
  )
  
  cat("  ✓ Estatística de Mann-Kendall:", round(mk_test$statistic, 2), "\n")
  cat("  ✓ p-value:", round(mk_test$p.value, 4), "\n")
  cat("  ✓ Sen's Slope:", round(sens_slope$estimates, 2), "usuários/dia\n\n")
  
  # ============================================================================
  # 7.6 Modelagem ARIMA e Previsão
  # ============================================================================
  
  msg("Ajustando modelo ARIMA e gerando previsões...", "estatistica")
  
  # Auto ARIMA (seleção automática de parâmetros)
  modelo_arima <- auto.arima(ts_usuarios, 
                             seasonal = TRUE,
                             stepwise = FALSE,
                             approximation = FALSE,
                             trace = FALSE)
  
  # Previsão para os próximos 30 dias
  previsao_arima <- forecast(modelo_arima, h = 30)
  
  # Acurácia do modelo
  acuracia_arima <- accuracy(modelo_arima)
  
  resultados$modelo_arima <- modelo_arima
  resultados$previsao_arima <- previsao_arima
  resultados$acuracia_arima <- acuracia_arima
  
  cat("  ✓ Modelo ARIMA ajustado:", arima_string(modelo_arima), "\n")
  cat("  ✓ AIC:", round(modelo_arima$aic, 2), "\n")
  cat("  ✓ BIC:", round(modelo_arima$bic, 2), "\n")
  cat("  ✓ RMSE:", round(acuracia_arima[1, "RMSE"], 2), "\n")
  cat("  ✓ MAPE:", round(acuracia_arima[1, "MAPE"], 2), "%\n\n")
  
  # ============================================================================
  # 7.7 Suavização Exponencial (ETS)
  # ============================================================================
  
  msg("Ajustando modelo de suavização exponencial (ETS)...", "estatistica")
  
  modelo_ets <- ets(ts_usuarios)
  previsao_ets <- forecast(modelo_ets, h = 30)
  acuracia_ets <- accuracy(modelo_ets)
  
  resultados$modelo_ets <- modelo_ets
  resultados$previsao_ets <- previsao_ets
  resultados$acuracia_ets <- acuracia_ets
  
  cat("  ✓ Modelo ETS ajustado:", modelo_ets$method, "\n")
  cat("  ✓ AIC:", round(modelo_ets$aic, 2), "\n")
  cat("  ✓ BIC:", round(modelo_ets$bic, 2), "\n")
  cat("  ✓ RMSE:", round(acuracia_ets[1, "RMSE"], 2), "\n\n")
  
  # ============================================================================
  # 7.8 Médias Móveis
  # ============================================================================
  
  msg("Calculando médias móveis...", "dados")
  
  serie_diaria <- serie_diaria %>%
    mutate(
      ma_7 = rollmean(usuarios_total, k = 7, fill = NA, align = "right"),
      ma_14 = rollmean(usuarios_total, k = 14, fill = NA, align = "right"),
      ma_30 = rollmean(usuarios_total, k = 30, fill = NA, align = "right")
    )
  
  resultados$serie_diaria <- serie_diaria
  
  cat("  ✓ Médias móveis calculadas (7, 14, 30 dias)\n\n")
  
  # ============================================================================
  # 7.9 Análise de Autocorrelação
  # ============================================================================
  
  msg("Calculando autocorrelação e autocorrelação parcial...", "estatistica")
  
  acf_result <- acf(ts_usuarios, lag.max = 40, plot = FALSE)
  pacf_result <- pacf(ts_usuarios, lag.max = 40, plot = FALSE)
  
  resultados$acf <- acf_result
  resultados$pacf <- pacf_result
  
  cat("  ✓ ACF e PACF calculados\n\n")
  
  # ============================================================================
  # 7.10 Análise por Região
  # ============================================================================
  
  msg("Analisando séries temporais por região...", "dados")
  
  series_regioes <- dados %>%
    group_by(data, regiao) %>%
    summarise(
      usuarios_total = sum(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(regiao, data)
  
  # Calcular crescimento percentual por região
  crescimento_regioes <- series_regioes %>%
    group_by(regiao) %>%
    summarise(
      primeiro_mes = sum(usuarios_total[data <= min(data) + 30]),
      ultimo_mes = sum(usuarios_total[data >= max(data) - 30]),
      crescimento_absoluto = ultimo_mes - primeiro_mes,
      crescimento_percentual = (ultimo_mes - primeiro_mes) / primeiro_mes * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(crescimento_percentual))
  
  resultados$series_regioes <- series_regioes
  resultados$crescimento_regioes <- crescimento_regioes
  
  cat("  ✓ Séries temporais por região calculadas\n")
  cat("  ✓ Crescimento por região calculado\n\n")
  
  return(resultados)
}

# ==============================================================================
# 8. ANÁLISE ESPACIAL
# ==============================================================================

analise_espacial <- function(dados) {
  
  secao("🗺️ ANÁLISE ESPACIAL")
  
  resultados <- list()
  
  msg("Realizando análise espacial dos dados...", "mapa")
  
  # ============================================================================
  # 8.1 Agregação espacial por região
  # ============================================================================
  
  agregacao_espacial <- dados %>%
    group_by(regiao, lat, lon) %>%
    summarise(
      n_registros = n(),
      usuarios_total = sum(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento),
      .groups = "drop"
    )
  
  resultados$agregacao_espacial <- agregacao_espacial
  
  # ============================================================================
  # 8.2 Identificação de Hotspots (pontos críticos)
  # ============================================================================
  
  msg("Identificando hotspots de congestionamento...", "mapa")
  
  # Definir hotspots como locais com congestionamento médio > 3.5
  hotspots <- agregacao_espacial %>%
    filter(congestionamento_medio > 3.5) %>%
    arrange(desc(congestionamento_medio)) %>%
    mutate(
      criticidade = case_when(
        congestionamento_medio >= 4.5 ~ "Crítico",
        congestionamento_medio >= 4.0 ~ "Alto",
        TRUE ~ "Moderado"
      )
    )
  
  resultados$hotspots <- hotspots
  
  cat("  ✓", nrow(hotspots), "hotspots identificados\n")
  if(nrow(hotspots) > 0) {
    cat("    - Críticos:", sum(hotspots$criticidade == "Crítico"), "\n")
    cat("    - Altos:", sum(hotspots$criticidade == "Alto"), "\n")
    cat("    - Moderados:", sum(hotspots$criticidade == "Moderado"), "\n")
  }
  cat("\n")
  
  # ============================================================================
  # 8.3 Densidade espacial
  # ============================================================================
  
  msg("Calculando densidade espacial...", "mapa")
  
  densidade_regiao <- dados %>%
    group_by(regiao) %>%
    summarise(
      n_pontos = n(),
      usuarios_total = sum(usuarios),
      area_aprox = (max(lat) - min(lat)) * (max(lon) - min(lon)) * 111 * 111,  # km²
      densidade_pontos = n_pontos / area_aprox,
      densidade_usuarios = usuarios_total / area_aprox,
      .groups = "drop"
    ) %>%
    arrange(desc(densidade_usuarios))
  
  resultados$densidade_regiao <- densidade_regiao
  
  cat("  ✓ Densidade espacial calculada por região\n\n")
  
  # ============================================================================
  # 8.4 Análise de proximidade
  # ============================================================================
  
  msg("Analisando padrões de proximidade...", "mapa")
  
  # Calcular centróides das regiões
  centroides <- dados %>%
    group_by(regiao) %>%
    summarise(
      lat_centro = mean(lat),
      lon_centro = mean(lon),
      .groups = "drop"
    )
  
  resultados$centroides <- centroides
  
  cat("  ✓ Centróides das regiões calculados\n\n")
  
  return(resultados)
}

# ==============================================================================
# 9. ANÁLISE DE PADRÕES TEMPORAIS
# ==============================================================================

analise_padroes_temporais <- function(dados) {
  
  secao("⏰ ANÁLISE DE PADRÕES TEMPORAIS")
  
  resultados <- list()
  
  # ============================================================================
  # 9.1 Padrões por hora do dia
  # ============================================================================
  
  msg("Analisando padrões por hora do dia...", "dados")
  
  padroes_hora <- dados %>%
    group_by(hora) %>%
    summarise(
      usuarios_medio = mean(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento),
      n_registros = n(),
      .groups = "drop"
    ) %>%
    arrange(hora)
  
  # Identificar horários de pico
  hora_pico_usuarios <- padroes_hora$hora[which.max(padroes_hora$usuarios_medio)]
  hora_pico_congestionamento <- padroes_hora$hora[which.max(padroes_hora$congestionamento_medio)]
  hora_menor_velocidade <- padroes_hora$hora[which.min(padroes_hora$velocidade_media)]
  
  resultados$padroes_hora <- padroes_hora
  resultados$horarios_pico <- list(
    usuarios = hora_pico_usuarios,
    congestionamento = hora_pico_congestionamento,
    menor_velocidade = hora_menor_velocidade
  )
  
  cat("  ✓ Hora de pico (usuários):", hora_pico_usuarios, "h\n")
  cat("  ✓ Hora de pico (congestionamento):", hora_pico_congestionamento, "h\n")
  cat("  ✓ Hora de menor velocidade:", hora_menor_velocidade, "h\n\n")
  
  # ============================================================================
  # 9.2 Padrões por dia da semana
  # ============================================================================
  
  msg("Analisando padrões por dia da semana...", "dados")
  
  padroes_dia_semana <- dados %>%
    group_by(dia_semana) %>%
    summarise(
      usuarios_medio = mean(usuarios),
      usuarios_total = sum(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento),
      n_registros = n(),
      .groups = "drop"
    )
  
  resultados$padroes_dia_semana <- padroes_dia_semana
  
  cat("  ✓ Padrões por dia da semana calculados\n\n")
  
  # ============================================================================
  # 9.3 Padrões por período do dia
  # ============================================================================
  
  msg("Analisando padrões por período do dia...", "dados")
  
  padroes_periodo <- dados %>%
    group_by(periodo_dia) %>%
    summarise(
      usuarios_medio = mean(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento),
      n_registros = n(),
      .groups = "drop"
    )
  
  resultados$padroes_periodo <- padroes_periodo
  
  cat("  ✓ Padrões por período do dia calculados\n\n")
  
  # ============================================================================
  # 9.4 Matriz hora x dia da semana
  # ============================================================================
  
  msg("Criando matriz de padrões hora x dia da semana...", "dados")
  
  matriz_hora_dia <- dados %>%
    group_by(hora, dia_semana) %>%
    summarise(
      usuarios_medio = mean(usuarios),
      congestionamento_medio = mean(nivel_congestionamento),
      .groups = "drop"
    )
  
  resultados$matriz_hora_dia <- matriz_hora_dia
  
  cat("  ✓ Matriz hora x dia da semana criada\n\n")
  
  # ============================================================================
  # 9.5 Análise de eventos especiais
  # ============================================================================
  
  msg("Analisando impacto de eventos especiais...", "dados")
  
  impacto_eventos <- dados %>%
    group_by(evento_especial) %>%
    summarise(
      n_registros = n(),
      usuarios_medio = mean(usuarios),
      usuarios_total = sum(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento),
      .groups = "drop"
    ) %>%
    arrange(desc(usuarios_medio))
  
  resultados$impacto_eventos <- impacto_eventos
  
  cat("  ✓ Impacto de eventos especiais analisado\n\n")
  
  return(resultados)
}

# ==============================================================================
# 10. FUNÇÃO AUXILIAR PARA ARIMA STRING
# ==============================================================================

arima_string <- function(modelo) {
  order <- modelo$arma[c(1, 6, 2)]
  seasonal <- modelo$arma[c(3, 7, 4)]
  freq <- modelo$arma[5]
  
  if(seasonal[1] == 0 && seasonal[2] == 0 && seasonal[3] == 0) {
    return(paste0("ARIMA(", paste(order, collapse = ","), ")"))
  } else {
    return(paste0("ARIMA(", paste(order, collapse = ","), 
                  ")(", paste(seasonal, collapse = ","), ")[", freq, "]"))
  }
}
