🚦 Análise Avançada de Mobilidade Urbana com Dados Waze

Análise de Tráfego e Mobilidade Urbana no Litoral de João Pessoa para Apoio a Políticas Públicas

Autor: Diogo Rego
Instituição: Estudante de Estatística, Universidade Federal da Paraíba (UFPB)
Aplicação: Análise de dados para o Departamento Estadual de Trânsito da Paraíba (DETRAN-PB)
Data: Dezembro de 2024
1. Visão Geral do Projeto

Este projeto apresenta um sistema completo para a análise estatística avançada de dados de mobilidade urbana, utilizando dados simulados baseados nos padrões de tráfego do aplicativo Waze. O foco da análise é o litoral da cidade de João Pessoa, Paraíba, abrangendo as principais praias e corredores turísticos dos litorais Norte e Sul.

O objetivo principal é transformar dados brutos de tráfego em insights acionáveis que possam subsidiar a tomada de decisão do DETRAN-PB e de outros órgãos de gestão urbana. Através de técnicas estatísticas robustas, o sistema permite identificar padrões, prever congestionamentos, localizar pontos críticos e compreender a dinâmica do fluxo de veículos na região.

2. Objetivos

Analisar o Perfil de Tráfego: Compreender os padrões de fluxo de veículos, incluindo variações sazonais, diárias e horárias.
Identificar Pontos Críticos (Hotspots): Mapear geograficamente as áreas com maiores níveis de congestionamento e menor velocidade média.
Modelagem Preditiva: Desenvolver modelos de séries temporais (ARIMA) para prever a demanda de tráfego em curto prazo.
Análise de Correlação: Investigar a relação entre variáveis como número de usuários, velocidade, período do dia e eventos especiais.
Gerar Recomendações: Fornecer subsídios técnicos para a implementação de políticas públicas de mobilidade, como otimização de semáforos, planejamento de rotas alternativas e alocação de agentes de trânsito.
Criar Visualizações Interativas: Apresentar os resultados de forma clara e intuitiva através de gráficos e mapas interativos.
3. Metodologia e Análises Realizadas

O projeto emprega um pipeline de análise de dados completo, desde a geração de dados sintéticos realistas até a criação de um relatório final. As principais técnicas estatísticas aplicadas incluem:

Análise Realizada	Descrição	Técnicas e Testes Aplicados
Análise Descritiva	Resumo e caracterização das principais variáveis de tráfego.	Medidas de tendência central, dispersão e distribuição.
Análise de Séries Temporais	Estudo da evolução do tráfego ao longo do tempo para identificar padrões e tendências.	Decomposição STL, Testes de Estacionariedade (ADF, KPSS), Teste de Mann-Kendall.
Modelagem Preditiva	Criação de modelos para prever o volume de tráfego futuro.	auto.arima para ajuste de modelos ARIMA sazonais.
Análise Espacial	Identificação de padrões geográficos, incluindo a localização de aglomerações e pontos críticos.	Análise de densidade, identificação de hotspots e centróides.
Testes de Hipóteses	Validação estatística de diferenças observadas entre grupos (ex: dias úteis vs. fins de semana).	Testes não-paramétricos (Wilcoxon, Kruskal-Wallis).
Análise de Correlação	Quantificação da associação entre diferentes variáveis de tráfego.	Correlação de Spearman.
4. Vitrine de Visualizações

As visualizações são fundamentais para a interpretação dos resultados. Abaixo estão alguns exemplos dos gráficos e mapas gerados pelo sistema.

Mapa de Calor de Densidade de Tráfego	Previsão de Tráfego com Modelo ARIMA
Heatmap de Tráfego	Previsão ARIMA
Identifica as áreas de maior concentração de usuários do Waze.	Projeta o volume de tráfego para os próximos 30 dias.
Padrão de Tráfego por Hora e Dia da Semana	Distribuição de Velocidade por Região
Heatmap Hora x Dia	Boxplot de Velocidade
Mostra os horários de pico de tráfego ao longo da semana.	Compara a performance do fluxo de veículos entre as diferentes regiões.
5. Estrutura do Repositório

O projeto está organizado na seguinte estrutura de diretórios para garantir modularidade e reprodutibilidade:

/waze-mobility-analysis
│
├── 📂 data/              # Armazena os dados brutos e processados (.csv, .rds)
├── 📂 docs/              # Documentação adicional e relatórios em Markdown
├── 📂 img/
│   ├── 📂 maps/          # Arquivos HTML dos mapas interativos
│   └── 📂 plots/         # Imagens PNG dos gráficos estáticos
├── 📂 results/           # Relatórios finais (HTML) e outros artefatos
├── 📂 src/               # Scripts R com o código-fonte da análise
│   ├── analise_waze_avancada.R
│   ├── analise_temporal.R
│   ├── visualizacoes.R
│   └── run_analysis.R    # Script principal para executar todo o pipeline
├── 📂 tests/             # Testes unitários para garantir a qualidade do código
│
├── .gitignore            # Arquivos e pastas a serem ignorados pelo Git
└── README.md             # Este arquivo
6. Como Utilizar o Sistema

Para executar a análise completa e gerar todos os resultados, siga os passos abaixo.

Pré-requisitos

R: Versão 4.0 ou superior.
RStudio: Versão 2022.07 ou superior (recomendado).
Passos para Execução

Clone o repositório:

git clone https://github.com/Diogorego20/Fluxo_usu-rios_waze_Litorais_joao_pessoa.git
cd Fluxo_usu-rios_waze_Litorais_joao_pessoa
Abra o projeto no RStudio: Abra o arquivo .Rproj ou navegue até a pasta do projeto.

Execute o script principal: No console do R, execute o comando abaixo. O script cuidará da instalação de pacotes, geração de dados, realização das análises e criação dos resultados.

source("src/run_analysis.R")
Explore os resultados:

O relatório HTML interativo será gerado em results/Relatorio_Mobilidade_Waze.html.
Os gráficos estáticos estarão disponíveis na pasta img/plots/.
Os mapas interativos estarão na pasta img/maps/.
7. Recomendações para o DETRAN-PB

Com base nos resultados da análise (simulada), o sistema pode gerar recomendações estratégicas, tais como:

Gestão de Tráfego em Tempo Real: Utilizar as previsões de curto prazo para antecipar congestionamentos e ajustar dinamicamente os tempos de semáforos em cruzamentos críticos, especialmente nas avenidas de Tambaú e Manaíra durante os horários de pico da tarde.
Planejamento de Operações Sazonais: Alocar um efetivo maior de agentes de trânsito e planejar desvios durante os meses de alta temporada (Dezembro a Fevereiro) e eventos como o Réveillon e Carnaval, focando nas áreas de orla como Cabo Branco e Intermares.
Infraestrutura Viária: Usar os mapas de hotspots para identificar locais que necessitam de estudos de engenharia de tráfego, como a implementação de rotatórias, faixas adicionais ou a melhoria da sinalização.
Comunicação com o Cidadão: Divulgar, através de canais oficiais, os horários de menor fluxo para deslocamentos, com base nos padrões identificados no heatmap hora-dia, incentivando o uso da infraestrutura viária fora dos horários de pico.
8. Referências

[1] Box, G. E. P., & Jenkins, G. M. (2015). Time Series Analysis: Forecasting and Control.

[2] Hyndman, R. J., & Athanasopoulos, G. (2021). Forecasting: Principles and Practice.

[3] Anselin, L. (1995). Local Indicators of Spatial Association - LISA.

9. Licença

Este projeto está licenciado sob a Licença MIT. Veja o arquivo LICENSE para mais detalhes.
