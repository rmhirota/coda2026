# =============================================================================
# Geojornalismo: explorando dados climáticos com R
# Minicurso — Renata Hirota
#
# Roteiro do live coding (~60 min)
#   Bloco 0 · Preparação ............................ (antes da aula)
#   Bloco 1 · Do CSV ao mapa: focos de queimadas ..... 15 min
#   Bloco 2 · CRS: graus ou metros? .................. 10 min
#   Bloco 3 · Junção espacial: fogo em Terras Indígenas 15 min
#   Bloco 4 · Desmatamento: interseção, área e buffer . 15 min
#   Bloco 5 · Exportar e levar para a redação .......... 5 min
#
# Pacotes obrigatórios: sf, dplyr
# Opcionais: ggplot2 (mapas mais bonitos), geobr (baixar limites oficiais)
# =============================================================================

library(sf)
library(dplyr)


# -----------------------------------------------------------------------------
# Bloco 0 · Preparação (rodar ANTES da aula — precisa de internet)
# -----------------------------------------------------------------------------
# Os dados abaixo são públicos. Baixe, rode as funções de preparo e deixe
# tudo na pasta dados/ para não depender da conexão durante o minicurso.
#
# 1) Focos de calor — INPE / Programa Queimadas (BDQueimadas)
#    https://terrabrasilis.dpi.inpe.br/queimadas/portal/
#    Dados abertos (CSV anual de 2025, satélite de referência):
#    https://dataserver-coids.inpe.br/queimadas/queimadas/focos/csv/anual/Brasil_sat_ref/
#    -> o arquivo vem zipado: descompacte e salve o CSV como dados/focos_2025.csv
#
# 2) Alertas de desmatamento — INPE / DETER Amazônia e Cerrado (TerraBrasilis)
#    https://terrabrasilis.dpi.inpe.br/downloads/
#    -> descompacte em dados/deter_amz e dados/deter_cer e rode prepara_deter()
#    (a lista de municípios da Amazônia Legal fica em dados/amazonia_legal.xls)
#
# 3) Limites territoriais
#    - Amazônia Legal: pacote {geobr} (Ipea)
#    - Terras Indígenas: o padrão do geobr é a versão de JULHO DE 2019.
#      Para dados atuais, baixe o shapefile de TIs no site da Funai e passe
#      o caminho para prepara_limites():
#      https://www.gov.br/funai/pt-br/atuacao/terras-indigenas/geoprocessamento-e-mapas

prepara_limites <- function(arquivo_tis_funai = NULL) {
  # install.packages("geobr")
  dir.create("dados", showWarnings = FALSE)

  amazonia <- geobr::read_amazon()

  tis <- if (is.null(arquivo_tis_funai)) {
    geobr::read_indigenous_land()                  # versão 2019
  } else {
    st_read(arquivo_tis_funai) |> st_transform(4674)
  }

  # Só as TIs da Amazônia Legal: dados menores
  tis <- tis |>
    st_make_valid() |>
    st_filter(amazonia)

  st_write(amazonia, "dados/limites.gpkg", layer = "amazonia_legal", delete_layer = TRUE)
  st_write(tis,      "dados/limites.gpkg", layer = "terras_indigenas", delete_layer = TRUE)
}
# prepara_limites()                                 # TIs do geobr (2019)
# prepara_limites("caminho/tis_funai.shp")          # TIs atuais da Funai

# Junta DETER Amazônia + DETER Cerrado, mantém só os municípios da
# Amazônia Legal e salva com um nome fixo, o mesmo usado no Bloco 4
prepara_deter <- function(pasta_amz = "dados/deter_amz",
                          pasta_cer = "dados/deter_cer",
                          arquivo_amz_legal = "dados/amazonia_legal.xls") {
  dir.create("dados/deter", recursive = TRUE, showWarnings = FALSE)

  amz_legal <- readxl::read_excel(arquivo_amz_legal)

  deter_amz <- sf::st_read(pasta_amz) |>
    dplyr::filter(GEOCODIBGE %in% amz_legal$CD_MUN)
  deter_cer <- sf::st_read(pasta_cer) |>
    dplyr::filter(GEOCODIBGE %in% amz_legal$CD_MUN)

  dplyr::bind_rows(deter_amz, deter_cer) |>
    dplyr::filter(lubridate::year(VIEW_DATE) == 2025) |>
    sf::st_write("dados/deter/deter.shp", delete_dsn = TRUE)
}
# prepara_deter()


# -----------------------------------------------------------------------------
# Bloco 1 · Do CSV ao mapa: focos de queimadas
# -----------------------------------------------------------------------------
# Um CSV com colunas de latitude e longitude ainda NÃO é um dado espacial.
# st_as_sf() transforma essas colunas numa coluna de geometria.

focos_csv <- read.csv("dados/focos_2025.csv", encoding = "UTF-8")  # acentos no Windows
head(focos_csv)

focos <- focos_csv |>
  st_as_sf(
    coords = c("lon", "lat"),   # atenção: x (longitude) vem primeiro!
    crs = 4326                  # WGS 84, o padrão do GPS
  )

focos                           # um data.frame com uma coluna "geometry"
class(focos)                    # "sf" "data.frame"

# Tudo do dplyr continua funcionando — e a geometria vem junto ("sticky")
focos_amazonia <- focos |>
  filter(bioma == "Amazônia")

plot(st_geometry(focos_amazonia), pch = ".", col = "firebrick")

# Camadas de polígonos: st_read() lê shapefile, GeoPackage, GeoJSON, KML...
st_layers("dados/limites.gpkg")
amazonia <- st_read("dados/limites.gpkg", layer = "amazonia_legal")
tis      <- st_read("dados/limites.gpkg", layer = "terras_indigenas")

# A coluna de geometria pode se chamar geom, geometry, SHAPE...
# Padronizar o nome facilita o resto do código
tis <- rename(tis, geometry = geom)


# -----------------------------------------------------------------------------
# Bloco 2 · CRS: o sistema de referência de coordenadas
# -----------------------------------------------------------------------------
st_crs(focos)$epsg      # 4326  (WGS 84)
st_crs(tis)$epsg        # 4674  (SIRGAS 2000, padrão oficial do Brasil)

# Camadas precisam estar no MESMO CRS para conversar entre si
focos_amazonia <- st_transform(focos_amazonia, st_crs(tis))

# Para buffer e distância, use uma projeção em metros.
# EPSG:5880 = SIRGAS 2000 / Brazil Polyconic
tis_m <- st_transform(tis, 5880)
st_area(tis_m[1, ])     # em m²

# Mas a Polyconic NÃO preserva área: longe do meridiano -54 ela infla
# (≈ +2% no oeste do AM, ≈ +6% no Acre). Para ÁREA, use uma projeção
# de área igual — aqui, a Albers do IBGE (EPSG:10857, SIRGAS 2000 /
# Brazil Albers). Com PROJ recente, st_crs(10857) basta; a string abaixo
# é equivalente e funciona também em instalações mais antigas.
albers <- paste(
  "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22",
  "+x_0=5000000 +y_0=10000000 +ellps=GRS80 +units=m +no_defs"
)

# Uma funçãozinha que projeta em Albers e converte m² -> km²
# (funciona com geometria em qualquer CRS)
km2 <- function(geom) as.numeric(st_area(st_transform(geom, albers))) / 1e6
km2(tis_m[1, ])

# Curiosidade: com lat/lon, o sf também calcula área em m² (via s2),
# então área em "graus" não é o problema. O perigo é st_buffer() com
# o s2 desligado: aí dist = 10000 vira 10 mil GRAUS.


# -----------------------------------------------------------------------------
# Bloco 3 · Junção espacial: quantos focos caíram em cada Terra Indígena?
# -----------------------------------------------------------------------------
# st_join() é o left_join() do mundo geográfico: em vez de uma coluna-chave,
# o critério é uma relação espacial (por padrão, st_intersects).

focos_tis <- st_join(focos_amazonia, tis, join = st_within)

# TIs sobrepostas fazem o mesmo foco aparecer em duas linhas.
# Se der TRUE, há focos contados em mais de uma TI: investigue antes de ranquear.
nrow(focos_tis) > nrow(focos_amazonia)

focos_por_ti <- focos_tis |>
  st_drop_geometry() |>                 # tira a geometria para virar tabela
  filter(!is.na(terrai_nom)) |>
  count(terrai_nom, sort = TRUE)

focos_por_ti                            # <- a pauta está aqui

# Normalizando pelo tamanho do território: focos por 1.000 km²
ranking_tis <- tis_m |>
  mutate(area_km2 = km2(geometry)) |>
  st_drop_geometry() |>
  select(terrai_nom, area_km2) |>
  inner_join(focos_por_ti, by = "terrai_nom") |>
  mutate(focos_por_mil_km2 = n / area_km2 * 1000) |>
  arrange(desc(focos_por_mil_km2))

ranking_tis

# Proporção de focos da Amazônia que caiu dentro de TIs
mean(!is.na(focos_tis$terrai_nom))


# -----------------------------------------------------------------------------
# Bloco 4 · Desmatamento: interseção, área e zona de amortecimento
# -----------------------------------------------------------------------------
deter <- st_read("dados/deter/deter.shp") |>
  st_make_valid() |>                    # polígonos inválidos quebram as operações
  st_transform(5880)

# O DETER traz várias classes: veja todas antes de filtrar
count(st_drop_geometry(deter), CLASSNAME)

# Cicatriz de queimada, degradação e corte seletivo NÃO são desmatamento.
# Mineração entra como desmatamento (remove a vegetação por completo).
classes_desmat <- c("DESMATAMENTO_CR",
  "DESMATAMENTO_VEG", "MINERACAO")

# Recorte: ano civil de 2025 (jan–dez). O PRODES usa o ano de ago a jul,
# então os números não são diretamente comparáveis com a taxa oficial.
deter_2025 <- deter |>
  filter(CLASSNAME %in% classes_desmat) |>
  filter(format(as.Date(VIEW_DATE), "%Y") == "2025")

# 4a) Quanto foi desmatado DENTRO de cada TI?
# st_intersection() recorta geometrias (st_join só cruza atributos)
desmat_em_tis <- st_intersection(deter_2025, tis_m) |>
  mutate(area_km2 = km2(geometry)) |>
  st_drop_geometry() |>
  group_by(terrai_nom) |>
  summarise(desmatado_km2 = sum(area_km2)) |>
  arrange(desc(desmatado_km2))

desmat_em_tis

# 4b) E no entorno? Um buffer de 10 km ao redor de cada TI
entorno <- tis_m |>
  st_buffer(dist = 10000) |>            # unidade = a do CRS (metros)
  st_difference(st_union(tis_m))        # só o anel, sem a TI em si

desmat_entorno <- st_intersection(deter_2025, entorno) |>
  mutate(area_km2 = km2(geometry)) |>
  st_drop_geometry() |>
  group_by(terrai_nom) |>
  summarise(entorno_km2 = sum(area_km2))

# A pressão está dentro ou na borda?
left_join(desmat_em_tis, desmat_entorno, by = "terrai_nom")

# 4c) summarise() em um objeto sf UNE as geometrias de cada grupo
#     (aqui: área desmatada por classe — corte raso, com vegetação, mineração)
deter_por_classe <- deter_2025 |>
  group_by(CLASSNAME) |>
  summarise(area_km2 = sum(km2(geometry)))

plot(deter_por_classe["CLASSNAME"])


# -----------------------------------------------------------------------------
# Bloco 5 · Mapa final e exportação
# -----------------------------------------------------------------------------
library(ggplot2)                        # opcional: quem não instalou só acompanha

ggplot() +
  geom_sf(data = amazonia, fill = "grey95", color = "grey60") +
  geom_sf(data = tis, fill = "#d9ead3", color = NA) +
  geom_sf(data = focos_amazonia, color = "firebrick", size = 0.1, alpha = 0.3) +
  labs(
    title = "Focos de calor na Amazônia Legal em 2025",
    caption = "Fonte: INPE/Programa Queimadas; Funai via geobr"
  ) +
  theme_void()

# Exportar para quem vai fazer o mapa interativo / gráfico
dir.create("saida", showWarnings = FALSE)
# GeoJSON pede WGS 84 (4326): ferramentas como Datawrapper e Flourish esperam isso
st_write(st_transform(focos_tis, 4326), "saida/focos_tis_2025.geojson", delete_dsn = TRUE)
write.csv(ranking_tis, "saida/ranking_tis_2025.csv", row.names = FALSE)

# GeoJSON abre em Datawrapper, Flourish, QGIS, Mapbox, kepler.gl...

# =============================================================================
# Cola das funções
#   st_as_sf()         CSV com lat/lon  -> sf
#   st_read()          arquivo espacial -> sf
#   st_crs()           qual é o CRS?
#   st_transform()     muda o CRS
#   st_make_valid()    conserta polígonos inválidos
#   st_join()          junta atributos por relação espacial
#   st_intersection()  recorta geometrias
#   st_area()          área (para km² confiáveis, projete na Albers: km2())
#   st_buffer()        área ao redor (dist na unidade do CRS: use metros)
#   st_union()         une geometrias
#   st_drop_geometry() volta a ser um data.frame comum
#   st_write()         salva em arquivo
# =============================================================================