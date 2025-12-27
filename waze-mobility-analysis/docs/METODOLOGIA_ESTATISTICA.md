# Metodologia Estatística Aplicada

## Análise de Mobilidade Urbana com Dados Waze

---

Este documento detalha as técnicas estatísticas e metodologias aplicadas no projeto de análise de mobilidade urbana do litoral de João Pessoa.

## 1. Estatística Descritiva

A análise descritiva fornece um resumo quantitativo das principais características dos dados de tráfego. As medidas calculadas incluem:

### 1.1 Medidas de Tendência Central

*   **Média Aritmética:** Representa o valor médio de usuários, velocidade e congestionamento.
*   **Mediana:** Valor central da distribuição, menos sensível a outliers que a média.
*   **Moda:** Valor mais frequente, especialmente relevante para o nível de congestionamento.

### 1.2 Medidas de Dispersão

*   **Desvio Padrão:** Quantifica a variabilidade dos dados em torno da média.
*   **Coeficiente de Variação (CV):** Medida relativa de dispersão, calculada como `CV = σ/μ × 100%`.
*   **Intervalo Interquartil (IQR):** Diferença entre o terceiro e primeiro quartil, robusta a outliers.

### 1.3 Medidas de Posição

*   **Quartis (Q1, Q2, Q3):** Dividem a distribuição em quatro partes iguais.
*   **Percentis:** Permitem identificar valores extremos e padrões de distribuição.

## 2. Análise de Séries Temporais

A análise temporal é fundamental para compreender a evolução do tráfego ao longo do tempo e realizar previsões.

### 2.1 Decomposição STL (Seasonal and Trend decomposition using Loess)

A decomposição STL separa a série temporal em três componentes:

*   **Tendência (T):** Movimento de longo prazo da série.
*   **Sazonalidade (S):** Padrão repetitivo em intervalos fixos (diário, semanal, mensal).
*   **Resíduos (R):** Variação aleatória não explicada pelos componentes anteriores.

A série observada pode ser expressa como: `Y(t) = T(t) + S(t) + R(t)` (modelo aditivo).

### 2.2 Testes de Estacionariedade

A estacionariedade é uma propriedade fundamental para a modelagem ARIMA. Uma série é estacionária quando suas propriedades estatísticas (média, variância) não variam ao longo do tempo.

*   **Teste de Dickey-Fuller Aumentado (ADF):** Testa a hipótese nula de que a série possui raiz unitária (não estacionária).
    *   H₀: A série possui raiz unitária (não estacionária)
    *   H₁: A série é estacionária
    *   Decisão: Se p-valor < 0.05, rejeitamos H₀ e concluímos que a série é estacionária.

*   **Teste KPSS (Kwiatkowski-Phillips-Schmidt-Shin):** Testa a hipótese nula de estacionariedade.
    *   H₀: A série é estacionária
    *   H₁: A série não é estacionária
    *   Decisão: Se p-valor > 0.05, não rejeitamos H₀ e concluímos que a série é estacionária.

*   **Teste de Phillips-Perron (PP):** Similar ao ADF, mas mais robusto a autocorrelação e heterocedasticidade.

### 2.3 Detecção de Mudanças Estruturais (Change Points)

Identifica pontos no tempo onde as propriedades estatísticas da série mudam abruptamente. Utilizamos o algoritmo **PELT (Pruned Exact Linear Time)** com critério BIC (Bayesian Information Criterion) para detectar:

*   Mudanças na média
*   Mudanças na variância
*   Mudanças simultâneas na média e variância

### 2.4 Teste de Tendência de Mann-Kendall

Teste não-paramétrico que detecta a presença de uma tendência monotônica na série temporal.

*   H₀: Não há tendência monotônica
*   H₁: Existe uma tendência monotônica (crescente ou decrescente)
*   Estatística S: Soma dos sinais das diferenças entre pares de observações.
*   **Sen's Slope:** Estima a magnitude da tendência (usuários/dia).

### 2.5 Modelagem ARIMA

ARIMA (AutoRegressive Integrated Moving Average) é uma classe de modelos para séries temporais que combina:

*   **AR(p):** Componente autorregressivo de ordem p.
*   **I(d):** Diferenciação de ordem d para tornar a série estacionária.
*   **MA(q):** Componente de média móvel de ordem q.

O modelo é representado como **ARIMA(p,d,q)** ou **ARIMA(p,d,q)(P,D,Q)[s]** para séries sazonais.

Utilizamos a função `auto.arima()` do pacote `forecast` que seleciona automaticamente os parâmetros ótimos com base nos critérios AIC (Akaike Information Criterion) e BIC.

### 2.6 Suavização Exponencial (ETS)

Modelos ETS (Error, Trend, Seasonal) são uma alternativa ao ARIMA, especialmente eficazes para séries com padrões sazonais fortes. O modelo é especificado por três componentes:

*   **E:** Tipo de erro (aditivo ou multiplicativo)
*   **T:** Tipo de tendência (nenhuma, aditiva, multiplicativa, amortecida)
*   **S:** Tipo de sazonalidade (nenhuma, aditiva, multiplicativa)

### 2.7 Análise de Autocorrelação

*   **ACF (Autocorrelation Function):** Mede a correlação entre a série e suas defasagens (lags).
*   **PACF (Partial Autocorrelation Function):** Mede a correlação entre a série e suas defasagens, removendo o efeito das defasagens intermediárias.

Essas funções são essenciais para identificar os parâmetros p e q do modelo ARIMA.

## 3. Análise Espacial

### 3.1 Agregação Espacial

Os dados são agregados por região geográfica para calcular métricas como:

*   Total de usuários por região
*   Velocidade média por região
*   Nível de congestionamento médio por região

### 3.2 Identificação de Hotspots

Hotspots são áreas com congestionamento significativamente acima da média. Utilizamos o seguinte critério:

*   **Crítico:** Congestionamento médio ≥ 4.5
*   **Alto:** Congestionamento médio ≥ 4.0
*   **Moderado:** Congestionamento médio ≥ 3.5

### 3.3 Densidade Espacial

Calculamos a densidade de pontos e de usuários por unidade de área (km²) para cada região, permitindo identificar áreas de maior pressão sobre a infraestrutura viária.

### 3.4 Centróides

O centróide de cada região é calculado como a média das coordenadas geográficas (latitude e longitude) de todos os pontos de dados naquela região.

## 4. Testes de Hipóteses

### 4.1 Teste de Wilcoxon-Mann-Whitney

Teste não-paramétrico para comparar duas amostras independentes (ex: velocidade média em dias úteis vs. fins de semana).

*   H₀: As duas amostras provêm de populações com a mesma distribuição.
*   H₁: As distribuições são diferentes.

### 4.2 Teste de Kruskal-Wallis

Teste não-paramétrico para comparar três ou mais amostras independentes (ex: número de usuários entre diferentes regiões).

*   H₀: Todas as amostras provêm de populações com a mesma distribuição.
*   H₁: Pelo menos uma das distribuições é diferente.

### 4.3 Testes de Normalidade

*   **Teste de Shapiro-Wilk:** Testa se uma amostra provém de uma distribuição normal.
*   **Teste de Anderson-Darling:** Teste mais sensível a desvios nas caudas da distribuição.

## 5. Análise de Correlação

### 5.1 Correlação de Spearman

Medida não-paramétrica de correlação que avalia a relação monotônica entre duas variáveis. Utilizada quando os dados não seguem uma distribuição normal ou quando a relação não é linear.

*   Varia de -1 (correlação negativa perfeita) a +1 (correlação positiva perfeita).
*   Valor próximo de 0 indica ausência de correlação.

### 5.2 Matriz de Correlação

Apresenta as correlações de Spearman entre todas as variáveis numéricas do estudo, facilitando a identificação de relações importantes.

## 6. Métricas de Avaliação de Modelos

### 6.1 Métricas de Erro

*   **RMSE (Root Mean Squared Error):** Raiz quadrada da média dos erros quadráticos. Penaliza erros grandes.
*   **MAE (Mean Absolute Error):** Média dos erros absolutos. Menos sensível a outliers que o RMSE.
*   **MAPE (Mean Absolute Percentage Error):** Erro percentual médio absoluto. Útil para comparar modelos em diferentes escalas.

### 6.2 Critérios de Informação

*   **AIC (Akaike Information Criterion):** Penaliza a complexidade do modelo. Menor AIC indica melhor ajuste.
*   **BIC (Bayesian Information Criterion):** Similar ao AIC, mas com penalização mais forte para modelos complexos.

## 7. Referências Bibliográficas

[1] Box, G. E. P., & Jenkins, G. M. (2015). *Time Series Analysis: Forecasting and Control*. John Wiley & Sons.

[2] Hyndman, R. J., & Athanasopoulos, G. (2021). *Forecasting: Principles and Practice* (3rd ed.). OTexts.

[3] Anselin, L. (1995). Local Indicators of Spatial Association—LISA. *Geographical Analysis*, 27(2), 93-115.

[4] Getis, A., & Ord, J. K. (1992). The Analysis of Spatial Association by Use of Distance Statistics. *Geographical Analysis*, 24(3), 189-206.

[5] Kwiatkowski, D., Phillips, P. C., Schmidt, P., & Shin, Y. (1992). Testing the null hypothesis of stationarity against the alternative of a unit root. *Journal of Econometrics*, 54(1-3), 159-178.

[6] Mann, H. B. (1945). Nonparametric tests against trend. *Econometrica*, 13(3), 245-259.

[7] Kendall, M. G. (1975). *Rank Correlation Methods* (4th ed.). Charles Griffin.
