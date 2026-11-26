# Not all paper dissolves in water: social wasps (Vespidae: Polistinae) in protected areas of Mato Grosso do Sul

**Autores:** João Marcos Gomes Leite¹\* · Alexandre Somavilla² · Gubio Gomes de Lima³ · Danielle C. de A. Amorim⁴ · Orlando T. Silveira⁴ · William Fernando Antonialli Junior¹˒⁵

1. Programa de Pós-Graduação em Entomologia e Conservação da Biodiversidade, Faculdade de Ciências Biológicas e Ambientais, Universidade Federal da Grande Dourados (UFGD), Dourados, MS, Brasil
2. Coordenação de Biodiversidade, Instituto Nacional de Pesquisas da Amazônia (INPA), Manaus, AM, Brasil
3. Centro Brasileiro de Pesquisas Físicas (CBPF), Rio de Janeiro, RJ, Brasil
4. Museu Paraense Emílio Goeldi, Coordenação de Zoologia, Laboratório de Entomologia, Belém, PA, Brasil
5. Laboratório de Ecologia Comportamental, Universidade Estadual de Mato Grosso do Sul (UEMS), Dourados, MS, Brasil

\* Autor correspondente: João Marcos Gomes Leite (jgomes.ifto@gmail.com)

| Autor | ORCID |
|---|---|
| João Marcos Gomes Leite | [0000-0001-5702-5696](https://orcid.org/0000-0001-5702-5696) |
| Alexandre Somavilla | [0000-0002-8174-7418](https://orcid.org/0000-0002-8174-7418) |
| Gubio Gomes de Lima | [0000-0002-4551-3251](https://orcid.org/0000-0002-4551-3251) |
| Danielle C. de A. Amorim | [0000-0002-0570-2728](https://orcid.org/0000-0002-0570-2728) |
| Orlando T. Silveira | [0000-0002-5899-199X](https://orcid.org/0000-0002-5899-199X) |
| William Fernando Antonialli Junior | [0000-0001-7977-9827](https://orcid.org/0000-0001-7977-9827) |

**Link do artigo:** [inserir link/DOI aqui]

**Palavras-chave:** Beta diversity · Environmental heterogeneity · Indicator species · Protected areas · Species turnover · Zeta diversity

---

## Sobre este repositório

Material suplementar (dados e scripts em R) do artigo acima. O estudo compara assembleias de vespas sociais (Polistinae) em três unidades de conservação de Mato Grosso do Sul com regimes hidrológicos contrastantes:

| Paisagem (`landscape`) | Unidade de conservação | Caracterização |
|---|---|---|
| Cerrado | Parque Nacional da Serra da Bodoquena | Cerrado–floresta estacional, não inundável |
| Pantanal | Área de Proteção Ambiental (APA) Baía Negra | Pantanal, unidade de uso sustentável |
| Floodplain | Parque Estadual das Várzeas do Rio Ivinhema | Planície de inundação da Mata Atlântica |

A amostragem padronizada usou armadilhas suspensas com isca (`trap = suspension`), complementada por coletas ocasionais. Foram analisadas riqueza, diversidade de Shannon, frequência de registros, composição, espécies indicadoras e compartilhamento de espécies entre múltiplos pontos (diversidade zeta).

**Resumo dos resultados:** 653 indivíduos de 23 espécies no total. Nas armadilhas padronizadas (99 registros independentes, 17 espécies, 46 pontos), a Floodplain teve maior riqueza e diversidade de Shannon que o Cerrado; a frequência de registros não diferiu. A composição da Floodplain diferiu da do Cerrado e da do Pantanal, que não diferiram entre si. A análise de espécies indicadoras apontou três espécies da Floodplain, uma do Pantanal e uma compartilhada Cerrado–Pantanal; a diversidade zeta decaiu com o número de pontos.

## Estrutura do repositório

Cada pasta de análise é autocontida: traz sua própria cópia da base de dados em `data/` (as cópias são idênticas), o script R e a pasta de saída.

```
.
├── README.md
│
├── localizationmap/                                  # Mapa de localização das áreas de estudo
│   ├── location map.R
│   ├── data/dados_utf8.csv.csv
│   ├── shapes_ucs_ms/                                # Limites das 3 UCs (shp, gpkg, geojson) + metadados (MMA/CNUC)
│   └── output/                                       # Figure_StudyArea_Location_v2_redesign (.png, .pdf, legenda .txt)
│
├── SOCIAL WASPS - ANALYSES ok/                       # Análises principais (nível de ponto, armadilhas suspensas)
│   ├── SOCIAL WASPS - ANALYSES ok.R                  # Riqueza, Shannon, frequência, PERMANOVA/betadisper (Jaccard), IndVal
│   ├── data/dados_utf8.csv.csv
│   └── results_pointlevel_valid_v2/                  # Painel de diversidade alfa e lollipop de espécies indicadoras
│
├── ZETA/                                             # Diversidade zeta (pacote zetadiv)
│   ├── ZETA.R
│   ├── data/dados_utf8.csv.csv
│   └── output/                                       # Figure_5_zeta_diversity_v2 (.png, .pdf)
│
├── Relative occurrence of species across landscapes data/   # Ocorrência relativa das espécies por paisagem
│   ├── Relative occurrence of species across landscapes.R
│   ├── data/dados_utf8.csv.csv
│   ├── Relative occurrence of species across landscapes/    # Ícones das paisagens (cerrado, pantanal, Floodplain)
│   └── output/                                       # fig_relative_occurrence_species_landscapes (.png, .pdf)
│
├── Species-landscape bipartite network/              # Rede bipartida quantitativa espécie–paisagem (todos os métodos)
│   ├── Species-landscape bipartite network_v2_modular.R
│   ├── data/dados_utf8.csv.csv
│   ├── cerrado.png, pantanal.png, floodplain.png     # Ícones das paisagens
│   └── output_network_quantitative_v2_modular/       # Figure_bipartite_quantitative_all_methods_v2 (.png, .pdf)
│
└── material suplementar prancha/                     # Pranchas fotográficas suplementares
    ├── prancha.R                                     # Figura S1 – paisagens
    ├── cerrado.png, pantanal.png, floodplain.png
    ├── Supplementary_Figure_S1_Landscapes (.png, .pdf)
    └── prancha 2/
        ├── prancha 2.R                               # Figura S2 – armadilhas e ninhos por paisagem
        ├── armadilha *.JPG, ninho *.jpg
        └── Supplementary_Figure_S2_Tribes_Traps_V2 (.png, .pdf)
```

## Descrição dos dados (`data/dados_utf8.csv.csv`)

Arquivo CSV em UTF-8 (com BOM), separador `;`, 653 registros (um por indivíduo).

| Coluna           | Descrição |
|------------------|-----------|
| `tribe`          | Tribo taxonômica |
| `gender`         | Gênero taxonômico |
| `species`        | Espécie |
| `point`          | Ponto/local de coleta |
| `date`           | Data da coleta (dd/mm/aaaa) |
| `year`           | Ano da coleta |
| `month`          | Mês da coleta |
| `trap`           | Método: `suspension` (armadilha suspensa com isca, padronizada), `malaise` ou `puca` (rede entomológica) — os dois últimos são coletas complementares |
| `baits`          | Isca utilizada (ex.: `guava`) |
| `Collection`     | Tipo de coleta: `passive` ou `active` |
| `lati_s`         | Latitude Sul (graus, minutos, segundos) |
| `long_w`         | Longitude Oeste (graus, minutos, segundos) |
| `classification` | Classificação da área (ex.: `natural reserve`) |
| `landscape`      | Paisagem: `cerrado`, `pantanal` ou `floodplain` |

## Como reproduzir as análises

1. Abra o script `.R` de uma pasta no RStudio e defina essa pasta como diretório de trabalho (cada script lê `data/dados_utf8.csv.csv` relativo a ela; se não encontrar, abre uma janela para selecionar o arquivo).
2. Instale os pacotes carregados no início de cada script (ex.: `dplyr`, `tidyr`, `ggplot2`, `vegan`, `indicspecies`, `zetadiv`, `ragg`, `patchwork`, `sf`, `terra`, `elevatr`, `geobr`).
3. Execute o script; figuras e tabelas são salvas na pasta de saída correspondente.

> **Atenção:** `ZETA/ZETA.R` e `SOCIAL WASPS - ANALYSES ok/SOCIAL WASPS - ANALYSES ok.R` definem a pasta de saída com um caminho absoluto do Windows (`output_directory <- ...`). Ajuste essa linha para o seu computador antes de executar.

## Citação

Se utilizar estes dados ou scripts, favor citar o artigo original:

> Leite, J. M. G., Somavilla, A., Lima, G. G. de, Amorim, D. C. de A., Silveira, O. T., & Antonialli Junior, W. F. Not all paper dissolves in water: social wasps (Vespidae: Polistinae) in protected areas of Mato Grosso do Sul. [periódico, ano, DOI]
