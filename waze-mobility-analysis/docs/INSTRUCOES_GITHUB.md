# Instruções para Atualizar o Repositório GitHub

Este documento fornece um guia passo a passo para atualizar seu repositório GitHub com os novos arquivos do projeto.

## Passo 1: Preparar o Ambiente Local

Primeiro, faça o download do arquivo `waze-mobility-analysis.zip` e extraia seu conteúdo em uma pasta local.

## Passo 2: Clonar o Repositório Existente

Se você ainda não clonou o repositório, execute:

```bash
git clone https://github.com/Diogorego20/Fluxo_usu-rios_waze_Litorais_joao_pessoa.git
cd Fluxo_usu-rios_waze_Litorais_joao_pessoa
```

## Passo 3: Copiar os Novos Arquivos

Copie todos os arquivos da pasta `waze-mobility-analysis` para o diretório do repositório clonado, substituindo os arquivos existentes.

## Passo 4: Adicionar os Arquivos ao Git

```bash
# Adicionar todos os novos arquivos
git add .

# Verificar o status
git status
```

## Passo 5: Fazer o Commit

```bash
git commit -m "Atualização completa: README profissional, código aprimorado e documentação"
```

## Passo 6: Enviar para o GitHub

```bash
git push origin main
```

Se você estiver usando a branch `master` em vez de `main`, substitua `main` por `master` no comando acima.

## Passo 7: Verificar no GitHub

Acesse seu repositório no GitHub e verifique se todos os arquivos foram atualizados corretamente.

## Estrutura Final do Repositório

Após a atualização, seu repositório deverá ter a seguinte estrutura:

```
/Fluxo_usu-rios_waze_Litorais_joao_pessoa
│
├── 📂 data/
├── 📂 docs/
│   ├── METODOLOGIA_ESTATISTICA.md
│   ├── GUIA_CONTRIBUICAO.md
│   ├── RECOMENDACOES_DETRAN_PB.md
│   └── INSTRUCOES_GITHUB.md
├── 📂 img/
│   ├── 📂 maps/
│   └── 📂 plots/
├── 📂 results/
├── 📂 src/
│   ├── analise_waze_avancada.R
│   ├── analise_temporal.R
│   ├── visualizacoes.R
│   └── run_analysis.R
├── 📂 tests/
│
├── .gitignore
├── LICENSE
└── README.md
```

## Dicas Adicionais

### Atualizar a Descrição do Repositório

No GitHub, vá até a página do seu repositório e clique em "About" (no canto superior direito). Atualize a descrição para:

```
Análise Avançada de Mobilidade Urbana com Dados Waze - Litoral de João Pessoa/PB. Projeto de análise estatística para apoio a políticas públicas do DETRAN-PB.
```

### Adicionar Topics (Tags)

Adicione as seguintes tags ao seu repositório para facilitar a descoberta:

*   `r`
*   `data-analysis`
*   `statistics`
*   `time-series`
*   `urban-mobility`
*   `traffic-analysis`
*   `waze`
*   `detran`
*   `paraiba`

### Criar um GitHub Pages (Opcional)

Se desejar hospedar o relatório HTML online:

1.  Vá em "Settings" > "Pages"
2.  Selecione a branch `main` e a pasta `/docs`
3.  Clique em "Save"

## Solução de Problemas

### Erro: "Permission denied"

Se você receber um erro de permissão, configure suas credenciais do GitHub:

```bash
git config --global user.name "Seu Nome"
git config --global user.email "seu-email@example.com"
```

### Erro: "Merge conflict"

Se houver conflitos de merge, resolva-os manualmente editando os arquivos conflitantes e depois:

```bash
git add .
git commit -m "Resolve merge conflicts"
git push origin main
```

---

**Pronto!** Seu repositório GitHub agora está atualizado com um README profissional e código aprimorado.
