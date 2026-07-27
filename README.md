# Not all paper dissolves in water: social wasps (Vespidae: Polistinae) in protected areas of Mato Grosso do Sul

**Autores:** [Nome 1, Nome 2, Nome 3, ...]

**Link do artigo:** [inserir link/DOI aqui]

---

## Sobre este repositório

Este repositório contém o material suplementar (dados e scripts de análise em R) referente ao artigo científico acima. Os dados tratam da ocorrência de vespas sociais (Epiponini) em diferentes paisagens (Cerrado, Pantanal e planície de inundação) no estado de Mato Grosso do Sul, Brasil.

## Estrutura do repositório

```
Joao/
├── README.md
└── material_suplementar/
    ├── data/
    │   ├── dados.csv                  # Base de dados bruta.
    │   ├── cerrado.png                 # Imagem/foto da paisagem de Cerrado
    │   ├── pantanal.png                 # Imagem/foto da paisagem de Pantanal
    │   └── Floodplain.png                # Imagem/foto da paisagem de planície de inundação
    │
    ├── localizationmap/
    │   ├── location map.R              # Script para geração do mapa de localização das áreas de estudo
    │   ├── shapes_ucs_ms/               # Shapefiles/geojson das unidades de conservação usadas no mapa
    │   ├── .Rhistory
    │   └── output/                      # Saída gerada pelo script (mapas)
    │
    ├── heatmap/
    │   ├── heat map.R                  # Script para geração de mapa de calor (heatmap) de ocorrência das espécies
    │   └── output/                      # Saída gerada pelo script
    │
    ├── nmds/
    │   ├── NMDS.R                       # Script de escalonamento multidimensional não métrico (NMDS)
    │   └── output/                      # Saída gerada pelo script
    │
    ├── ZETA/
    │   ├── ZETA.R                       # Script de análise de diversidade zeta (Zeta diversity)
    │   └── output/                      # Saída gerada pelo script
    │
    ├── beta_complementarity_analysis/
    │   ├── BETA DIVERSIDADE (TURNOVER + NESTEDNESS) +.R   # Script de partição da diversidade beta (turnover e aninhamento)
    │   └── output/                      # Saída gerada pelo script
    │
    ├── Relative occurrence of species across landscapes data/
    │   ├── Relative occurrence of species across landscapes.R   # Script de análise da ocorrência relativa das espécies entre paisagens
    │   └── output/                      # Saída gerada pelo script
    │
    ├── Species-landscape bipartite network/
    │   ├── Species-landscape bipartite network.R   # Script de construção da rede bipartida espécie-paisagem
    │   └── output/                      # Saída gerada pelo script
    │
    ├── SOCIAL WASPS - ANALYSES ok/
    │   ├── SOCIAL WASPS - ANALYSES ok.R   # Script principal com as análises estatísticas do estudo
    │   ├── results_pointlevel_valid/     # Resultados das análises em nível de ponto amostral
    │   └── output/                      # Saída gerada pelo script
    │
    └── output/                          # Diretório geral de saídas do projeto
```

## Descrição dos dados (`data/dados.csv`)

| Coluna           | Descrição                                              |
|------------------|---------------------------------------------------------|
| `tribe`          | Tribo taxonômica                                         |
| `gender`         | Gênero taxonômico                                        |
| `species`        | Espécie                                                   |
| `point`          | Ponto/local de coleta                                     |
| `date`           | Data da coleta                                            |
| `year`           | Ano da coleta                                             |
| `month`          | Mês da coleta                                             |
| `trap`           | Tipo de armadilha utilizada                                |
| `baits`          | Isca utilizada                                             |
| `Collection`     | Tipo de coleta (ativa/passiva)                             |
| `lati_s`         | Latitude (Sul)                                             |
| `long_w`         | Longitude (Oeste)                                          |
| `classification` | Classificação da área (ex.: reserva natural)                |
| `landscape`      | Tipo de paisagem (cerrado, pantanal, planície de inundação) |

## Como reproduzir as análises

1. Abra os scripts `.R` em cada subpasta com o RStudio (ou outro ambiente R).
2. Ajuste o caminho de importação para apontar para `material_suplementar/data/dados.csv`.
3. Execute o script — os resultados (gráficos, tabelas) serão salvos na respectiva pasta `output/`.

## Citação

Se utilizar estes dados ou scripts, favor citar o artigo original:

> [Inserir citação completa do artigo aqui]
