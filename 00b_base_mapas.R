# =============================================================
# 00b - Base e funções de mapa usadas por 02_mapas.R e 03_renda.R
# =============================================================
source("00_config.R")

base <- readRDS("dados/base_municipios.rds")
uf   <- readRDS("dados/malha_uf_final.rds")
rgi  <- readRDS("dados/malha_rgint_final.rds")
pam  <- readRDS("dados/pam_limpa.rds")

ano     <- unique(na.omit(base$ano_pib))[1]
ano_pam <- unique(na.omit(base$ano_pam))[1]

fonte_pib <- paste0("Fonte: elaborado pelo autor com dados do IBGE, PIB dos Municípios (",
                    ano, ").")
fonte_pam <- paste0("Fonte: elaborado pelo autor com dados do IBGE, Produção Agrícola Municipal (",
                    ano_pam, ").")

# Municípios com VAB total <= 0 (em 2021, só Cachoeira Dourada/GO, por causa
# da usina hidrelétrica) não têm participação setorial interpretável
base <- base %>% mutate(across(c(agro_pct, ind_pct, serv_pct),
                               ~ if_else(vab_total > 0, .x, NA_real_)))

# Moldura do Brasil continental (exclui Fernando de Noronha e Trindade,
# que encolhiam o mapa)
moldura_br <- coord_sf(xlim = c(-74.2, -34.6), ylim = c(-33.9, 5.4), expand = FALSE)

pr     <- base %>% filter(abbrev_state == "PR")
uf_pr  <- uf   %>% filter(abbrev_state == "PR")
reg_pg <- rgi  %>% filter(name_intermediate == "Ponta Grossa")
cg     <- pr   %>% filter(name_intermediate == "Ponta Grossa")

# =============================================================
# Escalas e legendas
# =============================================================
classes_agro <- function(x) cut(x, c(0, 5, 10, 20, 30, 40, 100), right = FALSE,
  include.lowest = TRUE,
  labels = c("Menos de 5%", "5% a 10%", "10% a 20%", "20% a 30%", "30% a 40%", "40% ou mais"))

base <- base %>% mutate(agro_cl = classes_agro(agro_pct))
pr   <- pr   %>% mutate(agro_cl = classes_agro(agro_pct))
cg   <- cg   %>% mutate(agro_cl = classes_agro(agro_pct))

legenda_faixa <- function(nrow = 1)
  guide_legend(title.position = "top", nrow = nrow, byrow = TRUE,
               label.position = "bottom",
               keywidth = unit(1.7, "cm"), keyheight = unit(0.32, "cm"))

escala_agro <- scale_fill_brewer(
  palette = "YlGn", na.value = "grey85", drop = FALSE,
  breaks = c("Menos de 5%", "5% a 10%", "10% a 20%", "20% a 30%", "30% a 40%", "40% ou mais"),
  name = "Participação da agropecuária no valor adicionado bruto do município",
  guide = legenda_faixa())

# Culturas: cada lavoura tem sempre a mesma cor em todas as figuras
cores_fixas <- c("Soja" = "#d9a400", "Milho" = "#7aa63a", "Trigo" = "#b5835a",
                 "Cana-de-açúcar" = "#2a9d8f", "Fumo" = "#8e6bbf", "Feijão" = "#d95f02",
                 "Café" = "#6b3d1e", "Mandioca" = "#e377a8", "Erva-mate" = "#3d6b4f",
                 "Batata-inglesa" = "#c9b98a", "Laranja" = "#f28e2b", "Arroz" = "#8fb9d9",
                 "Algodão herbáceo" = "#b8b3dd", "Cacau" = "#8c510a", "Açaí" = "#54278f",
                 "Banana" = "#f2c94c", "Uva" = "#7b3294", "Cebola" = "#c2410c",
                 "Tomate" = "#cb181d", "Maçã" = "#a50f15",
                 "Outras lavouras" = "grey78", "Sem registro" = "grey95")

cores_para <- function(niveis) {
  extra <- c("#4e79a7", "#76b7b2", "#edc948", "#ff9da7", "#9c755f", "#59a14f", "#e15759", "#bab0ac")
  out <- unname(cores_fixas[niveis])
  faltam <- which(is.na(out))
  out[faltam] <- extra[seq_along(faltam)]
  setNames(out, niveis)
}

agrupa_cultura <- function(x, n) {
  f <- fct_lump_n(factor(x), n = n, other_level = "Outras lavouras", ties.method = "first")
  f <- fct_infreq(f)
  f <- fct_expand(f, "Sem registro")
  f[is.na(f)] <- "Sem registro"
  niv <- c(setdiff(levels(f), c("Outras lavouras", "Sem registro")),
           intersect(c("Outras lavouras", "Sem registro"), levels(f)))
  fct_drop(factor(f, levels = niv))
}

# =============================================================
# Cidades nomeadas nos mapas do Paraná (edite à vontade)
# =============================================================
cidades_pr <- c("Ponta Grossa", "Castro", "Carambeí", "Palmeira", "Tibagi",
                "Telêmaco Borba", "Piraí do Sul", "Arapoti", "Jaguariaíva",
                "Imbituva", "Ipiranga", "Teixeira Soares", "Porto Amazonas")

pontos_cidades <- function(dados, nomes) {
  dados %>%
    filter(tolower(name_muni) %in% tolower(nomes)) %>%
    st_point_on_surface() %>%
    mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
    st_drop_geometry() %>%
    select(name_muni, x, y)
}

pts_viz <- pontos_cidades(pr, cidades_pr) %>%
  mutate(destaque = tolower(name_muni) == "ponta grossa")
pts_cwb <- pontos_cidades(pr, "Curitiba") %>% mutate(destaque = TRUE)

rotulos_pr <- function(tam = 2.8) {
  list(
    geom_point(data = pts_viz, aes(x, y), inherit.aes = FALSE,
               shape = 21, size = 1.5, fill = "grey15", color = "white", stroke = 0.4),
    geom_point(data = pts_viz %>% filter(destaque), aes(x, y), inherit.aes = FALSE,
               shape = 21, size = 2.8, fill = "#c0392b", color = "white", stroke = 0.5),
    geom_point(data = pts_cwb, aes(x, y), inherit.aes = FALSE,
               shape = 23, size = 2.8, fill = "#1f4e79", color = "white", stroke = 0.5),
    geom_text_repel(data = bind_rows(pts_viz, pts_cwb),
                    aes(x, y, label = name_muni,
                        fontface = ifelse(destaque, "bold", "plain")),
                    inherit.aes = FALSE, family = fonte, size = tam, color = "grey10",
                    bg.color = "white", bg.r = 0.15,
                    min.segment.length = 0, segment.color = "grey30", segment.size = 0.25,
                    box.padding = 0.35, max.overlaps = Inf, seed = 2),
    labs(x = NULL, y = NULL)
  )
}

# Mapas regionais: entorno em cinza e moldura estendida até Curitiba
caixa_reg <- st_bbox(st_union(st_geometry(cg),
                              st_geometry(pr %>% filter(name_muni == "Curitiba"))))
entorno <- pr %>% filter(!code_muni %in% cg$code_muni)

camada_entorno <- function()
  geom_sf(data = entorno, fill = "grey93", color = "white", linewidth = 0.2,
          inherit.aes = FALSE)

contexto_regional <- function() {
  list(
    geom_sf(data = reg_pg, fill = NA, color = "grey10", linewidth = 0.5, inherit.aes = FALSE),
    geom_point(data = pts_cwb, aes(x, y), inherit.aes = FALSE,
               shape = 23, size = 3.2, fill = "#1f4e79", color = "white", stroke = 0.5),
    geom_text_repel(data = pts_cwb, aes(x, y, label = name_muni), inherit.aes = FALSE,
                    family = fonte, size = 3.4, fontface = "bold",
                    bg.color = "white", bg.r = 0.15, nudge_y = 0.1, seed = 3),
    coord_sf(xlim = caixa_reg[c("xmin", "xmax")] + c(-0.05, 0.15),
             ylim = caixa_reg[c("ymin", "ymax")] + c(-0.05, 0.05), expand = FALSE),
    labs(x = NULL, y = NULL)
  )
}

rot_cg <- pontos_cidades(cg, cg$name_muni) %>%
  mutate(destaque = tolower(name_muni) == "ponta grossa")

rotulos_regiao <- function()
  geom_text_repel(data = rot_cg,
                  aes(x, y, label = name_muni, fontface = ifelse(destaque, "bold", "plain")),
                  inherit.aes = FALSE, family = fonte, size = 2.8, color = "grey10",
                  bg.color = "white", bg.r = 0.15,
                  min.segment.length = 0, segment.color = "grey35", segment.size = 0.25,
                  max.overlaps = Inf, seed = 1)

nota_contorno <- "Contorno preto: Região Geográfica Intermediária de Ponta Grossa (IBGE)."

