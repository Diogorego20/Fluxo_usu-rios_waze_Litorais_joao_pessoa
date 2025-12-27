# ==============================================================================
# VISUALIZAÇÕES PROFISSIONAIS - WAZE MOBILITY ANALYSIS
# ==============================================================================

# ==============================================================================
# 11. VISUALIZAÇÕES ESTATÍSTICAS AVANÇADAS
# ==============================================================================

gerar_visualizacoes_completas <- function(dados, analise_estat, analise_temporal, 
                                          analise_espacial, analise_padroes) {
  
  secao("📊 GERANDO VISUALIZAÇÕES PROFISSIONAIS")
  
  # Criar diretórios se não existirem
  dir.create("img/plots", recursive = TRUE, showWarnings = FALSE)
  dir.create("img/maps", recursive = TRUE, showWarnings = FALSE)
  
  visualizacoes <- list()
  
  # Tema personalizado para os gráficos
  tema_custom <- theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, hjust = 1, color = "gray50"),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
  
  # ============================================================================
  # 11.1 Série Temporal com Decomposição
  # ============================================================================
  
  msg("Criando gráfico de série temporal com decomposição...", "grafico")
  
  serie_df <- analise_temporal$serie_diaria
  
  p1 <- ggplot(serie_df, aes(x = data)) +
    geom_line(aes(y = usuarios_total), color = "steelblue", size = 0.8, alpha = 0.7) +
    geom_line(aes(y = ma_7), color = "red", size = 1.2) +
    geom_line(aes(y = tendencia), color = "darkgreen", size = 1, linetype = "dashed") +
    labs(
      title = "Série Temporal de Usuários Waze",
      subtitle = "Linha azul: dados diários | Linha vermelha: média móvel 7 dias | Linha verde: tendência",
      x = "Data",
      y = "Número de Usuários",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_x_date(date_breaks = "2 months", date_labels = "%b/%Y") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("img/plots/01_serie_temporal_completa.png", p1, width = 14, height = 6, dpi = 300)
  visualizacoes$serie_temporal <- p1
  
  # ============================================================================
  # 11.2 Decomposição STL
  # ============================================================================
  
  msg("Criando gráfico de decomposição STL...", "grafico")
  
  decomp_df <- data.frame(
    data = serie_df$data,
    observado = serie_df$usuarios_total,
    tendencia = serie_df$tendencia,
    sazonalidade = serie_df$sazonalidade,
    residuos = serie_df$residuos
  )
  
  p2_1 <- ggplot(decomp_df, aes(x = data, y = observado)) +
    geom_line(color = "steelblue", size = 0.8) +
    labs(title = "Dados Observados", y = "Usuários") +
    tema_custom + theme(axis.title.x = element_blank())
  
  p2_2 <- ggplot(decomp_df, aes(x = data, y = tendencia)) +
    geom_line(color = "darkgreen", size = 1) +
    labs(title = "Tendência", y = "Usuários") +
    tema_custom + theme(axis.title.x = element_blank())
  
  p2_3 <- ggplot(decomp_df, aes(x = data, y = sazonalidade)) +
    geom_line(color = "orange", size = 0.8) +
    labs(title = "Sazonalidade", y = "Efeito") +
    tema_custom + theme(axis.title.x = element_blank())
  
  p2_4 <- ggplot(decomp_df, aes(x = data, y = residuos)) +
    geom_line(color = "gray40", size = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Resíduos", y = "Resíduos", x = "Data") +
    tema_custom
  
  p2 <- (p2_1 / p2_2 / p2_3 / p2_4) +
    plot_annotation(
      title = "Decomposição STL da Série Temporal",
      subtitle = "Separação em Tendência, Sazonalidade e Resíduos",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    )
  
  ggsave("img/plots/02_decomposicao_stl.png", p2, width = 14, height = 10, dpi = 300)
  visualizacoes$decomposicao_stl <- p2
  
  # ============================================================================
  # 11.3 Previsão ARIMA
  # ============================================================================
  
  msg("Criando gráfico de previsão ARIMA...", "grafico")
  
  previsao <- analise_temporal$previsao_arima
  
  # Criar dataframe para previsão
  ultima_data <- max(serie_df$data)
  datas_futuras <- seq(ultima_data + 1, by = "day", length.out = 30)
  
  prev_df <- data.frame(
    data = datas_futuras,
    previsao = as.numeric(previsao$mean),
    lower_80 = as.numeric(previsao$lower[, 1]),
    upper_80 = as.numeric(previsao$upper[, 1]),
    lower_95 = as.numeric(previsao$lower[, 2]),
    upper_95 = as.numeric(previsao$upper[, 2])
  )
  
  # Últimos 60 dias de dados históricos
  hist_df <- serie_df %>%
    filter(data >= max(data) - 60) %>%
    select(data, usuarios_total)
  
  p3 <- ggplot() +
    # Intervalo de confiança 95%
    geom_ribbon(data = prev_df, aes(x = data, ymin = lower_95, ymax = upper_95),
                fill = "lightblue", alpha = 0.3) +
    # Intervalo de confiança 80%
    geom_ribbon(data = prev_df, aes(x = data, ymin = lower_80, ymax = upper_80),
                fill = "lightblue", alpha = 0.5) +
    # Dados históricos
    geom_line(data = hist_df, aes(x = data, y = usuarios_total),
              color = "steelblue", size = 1) +
    # Previsão
    geom_line(data = prev_df, aes(x = data, y = previsao),
              color = "red", size = 1.2) +
    geom_point(data = prev_df, aes(x = data, y = previsao),
               color = "red", size = 2) +
    # Linha divisória
    geom_vline(xintercept = as.numeric(ultima_data), linetype = "dashed", color = "gray50") +
    annotate("text", x = ultima_data, y = max(hist_df$usuarios_total) * 0.9,
             label = "Previsão →", hjust = 1.1, size = 4, color = "gray30") +
    labs(
      title = "Previsão de Tráfego para os Próximos 30 Dias",
      subtitle = paste("Modelo:", arima_string(analise_temporal$modelo_arima)),
      x = "Data",
      y = "Número de Usuários",
      caption = "Áreas sombreadas representam intervalos de confiança de 80% e 95%"
    ) +
    tema_custom +
    scale_x_date(date_breaks = "1 week", date_labels = "%d/%m") +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("img/plots/03_previsao_arima.png", p3, width = 14, height = 6, dpi = 300)
  visualizacoes$previsao_arima <- p3
  
  # ============================================================================
  # 11.4 Heatmap Hora x Dia da Semana
  # ============================================================================
  
  msg("Criando heatmap hora x dia da semana...", "grafico")
  
  matriz_hd <- analise_padroes$matriz_hora_dia %>%
    mutate(
      dia_semana = factor(dia_semana, 
                          levels = c("segunda-feira", "terça-feira", "quarta-feira",
                                     "quinta-feira", "sexta-feira", "sábado", "domingo"))
    )
  
  p4 <- ggplot(matriz_hd, aes(x = hora, y = dia_semana, fill = usuarios_medio)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_viridis_c(option = "plasma", name = "Usuários\nMédios",
                         labels = scales::comma) +
    labs(
      title = "Padrão de Tráfego por Hora e Dia da Semana",
      subtitle = "Intensidade de uso do Waze ao longo da semana",
      x = "Hora do Dia",
      y = "Dia da Semana",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_x_continuous(breaks = seq(0, 23, 2)) +
    theme(
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 11)
    )
  
  ggsave("img/plots/04_heatmap_hora_dia.png", p4, width = 14, height = 6, dpi = 300)
  visualizacoes$heatmap_hora_dia <- p4
  
  # ============================================================================
  # 11.5 Boxplot de Velocidade por Região
  # ============================================================================
  
  msg("Criando boxplot de velocidade por região...", "grafico")
  
  p5 <- ggplot(dados, aes(x = reorder(regiao, velocidade_media, FUN = median), 
                          y = velocidade_media, fill = regiao)) +
    geom_boxplot(alpha = 0.7, outlier.alpha = 0.3, outlier.size = 1) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, 
                 color = "red", show.legend = FALSE) +
    geom_hline(yintercept = mean(dados$velocidade_media, na.rm = TRUE),
               linetype = "dashed", color = "blue", size = 1) +
    labs(
      title = "Distribuição de Velocidade Média por Região",
      subtitle = "Losango vermelho = média | Linha azul = média geral",
      x = "Região",
      y = "Velocidade Média (km/h)",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_fill_brewer(palette = "Set2", name = "Região") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  
  ggsave("img/plots/05_boxplot_velocidade_regiao.png", p5, width = 12, height = 6, dpi = 300)
  visualizacoes$boxplot_velocidade <- p5
  
  # ============================================================================
  # 11.6 Violin Plot de Congestionamento por Período
  # ============================================================================
  
  msg("Criando violin plot de congestionamento...", "grafico")
  
  p6 <- ggplot(dados, aes(x = periodo_dia, y = nivel_congestionamento, 
                          fill = periodo_dia)) +
    geom_violin(alpha = 0.7, trim = FALSE) +
    geom_boxplot(width = 0.2, alpha = 0.5, outlier.alpha = 0) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
    labs(
      title = "Distribuição do Nível de Congestionamento por Período do Dia",
      subtitle = "Violin plot mostra a densidade da distribuição",
      x = "Período do Dia",
      y = "Nível de Congestionamento (1-5)",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_fill_brewer(palette = "Set1", name = "Período") +
    scale_y_continuous(breaks = 1:5) +
    theme(legend.position = "none")
  
  ggsave("img/plots/06_violin_congestionamento_periodo.png", p6, width = 12, height = 6, dpi = 300)
  visualizacoes$violin_congestionamento <- p6
  
  # ============================================================================
  # 11.7 Gráfico de Barras - Usuários por Região
  # ============================================================================
  
  msg("Criando gráfico de barras de usuários por região...", "grafico")
  
  usuarios_regiao <- dados %>%
    group_by(regiao) %>%
    summarise(total_usuarios = sum(usuarios), .groups = "drop") %>%
    arrange(desc(total_usuarios))
  
  p7 <- ggplot(usuarios_regiao, aes(x = reorder(regiao, total_usuarios), 
                                    y = total_usuarios, fill = regiao)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = fmt_num(total_usuarios)), hjust = -0.2, size = 4) +
    coord_flip() +
    labs(
      title = "Total de Usuários Waze por Região",
      subtitle = "Período: 2020-2024 (Dezembro, Janeiro, Fevereiro)",
      x = "Região",
      y = "Total de Usuários",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_fill_brewer(palette = "Set2") +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
    theme(legend.position = "none")
  
  ggsave("img/plots/07_barras_usuarios_regiao.png", p7, width = 12, height = 6, dpi = 300)
  visualizacoes$barras_usuarios_regiao <- p7
  
  # ============================================================================
  # 11.8 Gráfico de Linhas - Evolução por Região
  # ============================================================================
  
  msg("Criando gráfico de evolução temporal por região...", "grafico")
  
  evolucao_regiao <- dados %>%
    mutate(mes_ano = floor_date(data, "month")) %>%
    group_by(mes_ano, regiao) %>%
    summarise(usuarios_total = sum(usuarios), .groups = "drop")
  
  p8 <- ggplot(evolucao_regiao, aes(x = mes_ano, y = usuarios_total, 
                                    color = regiao, group = regiao)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 2, alpha = 0.6) +
    labs(
      title = "Evolução Temporal do Tráfego por Região",
      subtitle = "Agregação mensal de usuários Waze",
      x = "Mês/Ano",
      y = "Total de Usuários",
      color = "Região",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_color_brewer(palette = "Set2") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b/%Y") +
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave("img/plots/08_evolucao_temporal_regiao.png", p8, width = 14, height = 6, dpi = 300)
  visualizacoes$evolucao_temporal <- p8
  
  # ============================================================================
  # 11.9 Facet Grid - Padrões por Região e Período
  # ============================================================================
  
  msg("Criando gráfico facetado por região...", "grafico")
  
  padroes_regiao_periodo <- dados %>%
    group_by(regiao, periodo_dia) %>%
    summarise(
      usuarios_medio = mean(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      .groups = "drop"
    )
  
  p9 <- ggplot(padroes_regiao_periodo, aes(x = periodo_dia, y = usuarios_medio, 
                                           fill = periodo_dia)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    facet_wrap(~ regiao, ncol = 3, scales = "free_y") +
    labs(
      title = "Padrão de Usuários por Região e Período do Dia",
      subtitle = "Comparação entre diferentes regiões do litoral",
      x = "Período do Dia",
      y = "Número Médio de Usuários",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_fill_brewer(palette = "Set1") +
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 11),
      legend.position = "none"
    )
  
  ggsave("img/plots/09_facet_regiao_periodo.png", p9, width = 14, height = 8, dpi = 300)
  visualizacoes$facet_regiao_periodo <- p9
  
  # ============================================================================
  # 11.10 Matriz de Correlação
  # ============================================================================
  
  msg("Criando matriz de correlação...", "grafico")
  
  matriz_cor <- analise_estat$matriz_correlacao
  
  # Transformar em formato longo
  cor_df <- as.data.frame(as.table(matriz_cor))
  names(cor_df) <- c("Var1", "Var2", "Correlacao")
  
  # Renomear variáveis para português
  nomes_vars <- c(
    "usuarios" = "Usuários",
    "velocidade_media" = "Velocidade",
    "nivel_congestionamento" = "Congestionamento",
    "tempo_viagem" = "Tempo de Viagem",
    "hora" = "Hora",
    "dia_semana_num" = "Dia da Semana"
  )
  
  cor_df <- cor_df %>%
    mutate(
      Var1 = recode(as.character(Var1), !!!nomes_vars),
      Var2 = recode(as.character(Var2), !!!nomes_vars)
    )
  
  p10 <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlacao)) +
    geom_tile(color = "white", size = 1) +
    geom_text(aes(label = sprintf("%.2f", Correlacao)), size = 4, fontface = "bold") +
    scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4",
                         midpoint = 0, limit = c(-1, 1), name = "Correlação\n(Spearman)") +
    labs(
      title = "Matriz de Correlação entre Variáveis de Tráfego",
      subtitle = "Correlação de Spearman (não-paramétrica)",
      x = NULL,
      y = NULL,
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
      axis.text.y = element_text(size = 11),
      panel.grid = element_blank()
    ) +
    coord_fixed()
  
  ggsave("img/plots/10_matriz_correlacao.png", p10, width = 10, height = 8, dpi = 300)
  visualizacoes$matriz_correlacao <- p10
  
  # ============================================================================
  # 11.11 Gráfico de Eventos Especiais
  # ============================================================================
  
  msg("Criando gráfico de impacto de eventos especiais...", "grafico")
  
  eventos_df <- analise_padroes$impacto_eventos %>%
    filter(evento_especial != "Normal") %>%
    arrange(desc(usuarios_medio))
  
  p11 <- ggplot(eventos_df, aes(x = reorder(evento_especial, usuarios_medio), 
                                y = usuarios_medio, fill = evento_especial)) +
    geom_bar(stat = "identity", alpha = 0.8) +
    geom_text(aes(label = round(usuarios_medio, 1)), hjust = -0.2, size = 4) +
    coord_flip() +
    labs(
      title = "Impacto de Eventos Especiais no Tráfego",
      subtitle = "Número médio de usuários por tipo de evento",
      x = "Tipo de Evento",
      y = "Usuários Médios",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    ) +
    tema_custom +
    scale_fill_brewer(palette = "Set3") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme(legend.position = "none")
  
  ggsave("img/plots/11_impacto_eventos.png", p11, width = 12, height = 6, dpi = 300)
  visualizacoes$impacto_eventos <- p11
  
  # ============================================================================
  # 11.12 ACF e PACF
  # ============================================================================
  
  msg("Criando gráficos ACF e PACF...", "grafico")
  
  acf_data <- with(analise_temporal$acf, data.frame(lag = lag, acf = acf))
  pacf_data <- with(analise_temporal$pacf, data.frame(lag = lag, pacf = acf))
  
  p12_1 <- ggplot(acf_data[-1, ], aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-1.96/sqrt(length(analise_temporal$ts_usuarios)),
                               1.96/sqrt(length(analise_temporal$ts_usuarios))),
               linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0), color = "steelblue", size = 1) +
    labs(title = "Função de Autocorrelação (ACF)", x = "Lag", y = "ACF") +
    tema_custom
  
  p12_2 <- ggplot(pacf_data, aes(x = lag, y = pacf)) +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(-1.96/sqrt(length(analise_temporal$ts_usuarios)),
                               1.96/sqrt(length(analise_temporal$ts_usuarios))),
               linetype = "dashed", color = "blue") +
    geom_segment(aes(xend = lag, yend = 0), color = "darkorange", size = 1) +
    labs(title = "Função de Autocorrelação Parcial (PACF)", x = "Lag", y = "PACF") +
    tema_custom
  
  p12 <- (p12_1 | p12_2) +
    plot_annotation(
      title = "Análise de Autocorrelação da Série Temporal",
      subtitle = "Linhas azuis tracejadas indicam limites de significância (95%)",
      caption = "Fonte: Dados Waze - Análise DETRAN-PB"
    )
  
  ggsave("img/plots/12_acf_pacf.png", p12, width = 14, height = 6, dpi = 300)
  visualizacoes$acf_pacf <- p12
  
  msg("Todas as visualizações foram geradas com sucesso!", "sucesso")
  cat("  📁 Arquivos salvos em: img/plots/\n\n")
  
  return(visualizacoes)
}

# ==============================================================================
# 12. MAPAS INTERATIVOS
# ==============================================================================

gerar_mapas_interativos <- function(dados, analise_espacial) {
  
  secao("🗺️ GERANDO MAPAS INTERATIVOS")
  
  mapas <- list()
  
  # ============================================================================
  # 12.1 Mapa base com regiões
  # ============================================================================
  
  msg("Criando mapa interativo das regiões...", "mapa")
  
  # Agregar dados por região
  dados_agregados <- dados %>%
    group_by(regiao) %>%
    summarise(
      lat_media = mean(lat, na.rm = TRUE),
      lon_media = mean(lon, na.rm = TRUE),
      total_usuarios = sum(usuarios),
      velocidade_media = mean(velocidade_media, na.rm = TRUE),
      congestionamento_medio = mean(nivel_congestionamento),
      n_registros = n(),
      .groups = "drop"
    )
  
  # Adicionar informações das regiões
  dados_agregados <- dados_agregados %>%
    rowwise() %>%
    mutate(
      cor = regioes_completas[[regiao]]$cor,
      descricao = regioes_completas[[regiao]]$descricao
    ) %>%
    ungroup()
  
  # Criar mapa leaflet
  mapa_base <- leaflet(dados_agregados) %>%
    addTiles() %>%
    setView(lng = -34.85, lat = -7.12, zoom = 11) %>%
    addProviderTiles(providers$CartoDB.Positron)
  
  # Adicionar círculos para cada região
  for(i in 1:nrow(dados_agregados)) {
    regiao_info <- dados_agregados[i, ]
    
    popup_html <- paste0(
      "<div style='font-family: Arial; font-size: 12px;'>",
      "<h4 style='margin: 5px 0; color: ", regiao_info$cor, ";'>", regiao_info$regiao, "</h4>",
      "<p style='margin: 3px 0;'><b>Descrição:</b> ", regiao_info$descricao, "</p>",
      "<p style='margin: 3px 0;'><b>Total de Usuários:</b> ", fmt_num(regiao_info$total_usuarios), "</p>",
      "<p style='margin: 3px 0;'><b>Velocidade Média:</b> ", round(regiao_info$velocidade_media, 1), " km/h</p>",
      "<p style='margin: 3px 0;'><b>Congestionamento Médio:</b> ", round(regiao_info$congestionamento_medio, 2), "/5</p>",
      "<p style='margin: 3px 0;'><b>Registros:</b> ", fmt_num(regiao_info$n_registros), "</p>",
      "</div>"
    )
    
    mapa_base <- mapa_base %>%
      addCircleMarkers(
        lng = regiao_info$lon_media,
        lat = regiao_info$lat_media,
        radius = sqrt(regiao_info$total_usuarios) / 50,
        color = regiao_info$cor,
        fillColor = regiao_info$cor,
        fillOpacity = 0.6,
        weight = 2,
        popup = popup_html,
        label = regiao_info$regiao
      )
  }
  
  # Adicionar legenda
  mapa_base <- mapa_base %>%
    addLegend(
      position = "bottomright",
      colors = dados_agregados$cor,
      labels = dados_agregados$regiao,
      title = "Regiões",
      opacity = 0.8
    )
  
  # Salvar mapa
  saveWidget(mapa_base, "img/maps/mapa_regioes.html", selfcontained = TRUE)
  mapas$mapa_regioes <- mapa_base
  
  msg("Mapa de regiões criado com sucesso!", "sucesso")
  
  # ============================================================================
  # 12.2 Mapa de calor (Heatmap)
  # ============================================================================
  
  msg("Criando mapa de calor de densidade de tráfego...", "mapa")
  
  # Amostrar dados para o heatmap (máximo 10000 pontos)
  if(nrow(dados) > 10000) {
    dados_heatmap <- dados %>% sample_n(10000)
  } else {
    dados_heatmap <- dados
  }
  
  mapa_calor <- leaflet(dados_heatmap) %>%
    addTiles() %>%
    setView(lng = -34.85, lat = -7.12, zoom = 11) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    addHeatmap(
      lng = ~lon,
      lat = ~lat,
      intensity = ~usuarios,
      blur = 20,
      max = 0.5,
      radius = 15
    ) %>%
    addLegend(
      position = "bottomright",
      colors = c("#313695", "#4575b4", "#74add1", "#abd9e9", "#fee090", "#fdae61", "#f46d43", "#d73027"),
      labels = c("Muito Baixo", "", "", "", "", "", "", "Muito Alto"),
      title = "Densidade de Tráfego",
      opacity = 0.8
    )
  
  saveWidget(mapa_calor, "img/maps/mapa_calor.html", selfcontained = TRUE)
  mapas$mapa_calor <- mapa_calor
  
  msg("Mapa de calor criado com sucesso!", "sucesso")
  
  # ============================================================================
  # 12.3 Mapa de Hotspots
  # ============================================================================
  
  if(nrow(analise_espacial$hotspots) > 0) {
    msg("Criando mapa de hotspots de congestionamento...", "mapa")
    
    hotspots <- analise_espacial$hotspots
    
    # Paleta de cores por criticidade
    pal <- colorFactor(
      palette = c("Crítico" = "#d73027", "Alto" = "#fc8d59", "Moderado" = "#fee08b"),
      domain = hotspots$criticidade
    )
    
    mapa_hotspots <- leaflet(hotspots) %>%
      addTiles() %>%
      setView(lng = -34.85, lat = -7.12, zoom = 11) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        radius = ~congestionamento_medio * 2,
        color = ~pal(criticidade),
        fillColor = ~pal(criticidade),
        fillOpacity = 0.7,
        weight = 2,
        popup = ~paste0(
          "<b>Região:</b> ", regiao, "<br>",
          "<b>Congestionamento:</b> ", round(congestionamento_medio, 2), "/5<br>",
          "<b>Criticidade:</b> ", criticidade, "<br>",
          "<b>Usuários:</b> ", fmt_num(usuarios_total)
        ),
        label = ~paste(regiao, "-", criticidade)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~criticidade,
        title = "Criticidade",
        opacity = 0.8
      )
    
    saveWidget(mapa_hotspots, "img/maps/mapa_hotspots.html", selfcontained = TRUE)
    mapas$mapa_hotspots <- mapa_hotspots
    
    msg("Mapa de hotspots criado com sucesso!", "sucesso")
  }
  
  msg("Todos os mapas foram gerados com sucesso!", "sucesso")
  cat("  📁 Arquivos salvos em: img/maps/\n\n")
  
  return(mapas)
}
