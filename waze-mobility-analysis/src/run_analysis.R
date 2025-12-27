# ==============================================================================
# SCRIPT PRINCIPAL DE EXECUÇÃO - ANÁLISE DE MOBILIDADE URBANA
# ==============================================================================
# Este script executa o pipeline completo de análise de dados Waze.
# ==============================================================================

# 1. Carregar todos os scripts de funções
secao("CARREGANDO SCRIPTS")

# Definir diretório de trabalho (se necessário)
# setwd("/path/to/your/project/waze-mobility-analysis")

source("src/analise_waze_avancada.R")
source("src/analise_temporal.R")
source("src/visualizacoes.R")

# 2. Executar o pipeline completo
executar_pipeline_completo <- function() {
  
  # Iniciar cronômetro
  inicio_global <- Sys.time()
  
  secao("INICIANDO PIPELINE DE ANÁLISE COMPLETA", 80)
  
  # Passo 1: Geração de Dados
  dados_waze <- gerar_dados_waze_completo(anos = 2020:2024, meses = c(12, 1, 2))
  
  # Salvar dados gerados
  saveRDS(dados_waze, "data/dados_waze_simulados.rds")
  fwrite(dados_waze, "data/dados_waze_simulados.csv")
  msg("Dados simulados salvos em 'data/dados_waze_simulados.rds' e '.csv'", "sucesso")
  
  # Passo 2: Análises Estatísticas
  analise_estat <- analise_estatistica_completa(dados_waze)
  
  # Passo 3: Análise de Séries Temporais
  analise_temporal <- analise_series_temporais(dados_waze)
  
  # Passo 4: Análise Espacial
  analise_espacial <- analise_espacial(dados_waze)
  
  # Passo 5: Análise de Padrões Temporais
  analise_padroes <- analise_padroes_temporais(dados_waze)
  
  # Passo 6: Geração de Visualizações
  visualizacoes <- gerar_visualizacoes_completas(dados_waze, analise_estat, 
                                                 analise_temporal, analise_espacial, 
                                                 analise_padroes)
  
  # Passo 7: Geração de Mapas Interativos
  mapas <- gerar_mapas_interativos(dados_waze, analise_espacial)
  
  # Passo 8: Geração do Relatório HTML (simplificado, para exemplo)
  # Para um relatório completo, recomenda-se usar R Markdown
  gerar_relatorio_final(visualizacoes, mapas)
  
  # Finalizar cronômetro
  fim_global <- Sys.time()
  tempo_total <- difftime(fim_global, inicio_global, units = "secs")
  
  secao("PIPELINE CONCLUÍDO COM SUCESSO", 80)
  msg(paste("Tempo total de execução:", round(tempo_total, 2), "segundos"), "sucesso")
  msg("Todos os resultados foram gerados nas pastas 'img', 'data' e 'results'.", "info")
  
  # Retornar todos os resultados
  return(invisible(list(
    dados = dados_waze,
    analise_estat = analise_estat,
    analise_temporal = analise_temporal,
    analise_espacial = analise_espacial,
    analise_padroes = analise_padroes,
    visualizacoes = visualizacoes,
    mapas = mapas
  )))
}

# Função para gerar um relatório HTML simples
gerar_relatorio_final <- function(visualizacoes, mapas) {
  
  secao("GERANDO RELATÓRIO FINAL")
  
  html_content <- tagList(
    tags$h1("Relatório de Análise de Mobilidade Urbana - Waze"),
    tags$h2("Mapa de Calor de Densidade de Tráfego"),
    mapas$mapa_calor,
    tags$h2("Previsão de Tráfego para os Próximos 30 Dias"),
    tags$img(src = "../img/plots/03_previsao_arima.png", width = "100%"),
    tags$h2("Padrão de Tráfego por Hora e Dia da Semana"),
    tags$img(src = "../img/plots/04_heatmap_hora_dia.png", width = "100%")
  )
  
  save_html(html_content, file = "results/Relatorio_Mobilidade_Waze.html")
  msg("Relatório HTML salvo em 'results/Relatorio_Mobilidade_Waze.html'", "sucesso")
}

# Executar o pipeline
resultados_finais <- executar_pipeline_completo()


