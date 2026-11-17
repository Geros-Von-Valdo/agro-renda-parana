# =============================================================
# 00 - Configuração comum (pacotes, pastas, tema, funções)
# Os outros scripts chamam este com source("00_config.R")
# =============================================================

# ---- Pacotes --------------------------------------------------
lib_usuario <- Sys.getenv("R_LIBS_USER")
dir.create(lib_usuario, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_usuario, .libPaths()))

pacotes <- c("geobr", "sidrar", "sf", "ggplot2", "dplyr", "tidyr",
             "stringr", "forcats", "scales", "biscale", "cowplot",
             "spdep", "ggrepel", "readr", "patchwork", "showtext")
faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(faltando) > 0) {
  install.packages(faltando, lib = lib_usuario,
                   repos = "https://cloud.r-project.org")
}
invisible(lapply(pacotes, library, character.only = TRUE))

sf_use_s2(FALSE)   # evita erros de geometria em malhas simplificadas

# ---- Pasta do projeto ------------------------------------------
# Garante que dados/, figuras/ e tabelas/ fiquem sempre dentro do projeto,
# mesmo rodando com source() de outra pasta
pasta_projeto <- "C:/Users/W11/Downloads/@CODING/artigoComputacaoAplicada"
if (dir.exists(pasta_projeto)) setwd(pasta_projeto)

# ---- SIDRA ------------------------------------------------------
# Versões novas do sidrar rejeitam espaços na URL; esta função troca por %20
sidra <- function(api) get_sidra(api = gsub(" ", "%20", api, fixed = TRUE))

# ---- Pastas ---------------------------------------------------
dir.create("dados",   showWarnings = FALSE)   # downloads em cache (.rds)
dir.create("figuras", showWarnings = FALSE)   # PNGs para o artigo
dir.create("tabelas", showWarnings = FALSE)   # CSVs de apoio

# ---- Cache: baixa só na primeira vez -------------------------
# Uso: x <- cache("nome", codigo_que_baixa())
# Para forçar novo download, apague o arquivo em dados/nome.rds
cache <- function(nome, expr) {
  arq <- file.path("dados", paste0(nome, ".rds"))
  if (file.exists(arq)) return(readRDS(arq))
  obj <- expr
  if (!is.null(obj) && NROW(obj) > 0) saveRDS(obj, arq)   # não guarda download vazio
  obj
}

# ---- Fonte tipográfica -----------------------------------------
# Source Sans 3 (Google Fonts): sóbria e comum em publicações científicas.
# Sem internet, cai para Arial.
fonte <- tryCatch({
  sysfonts::font_add_google("Source Sans 3", "fonte_artigo")
  "fonte_artigo"
}, error = function(e) { message("Fonte do Google indisponível; usando Arial."); "sans" })
showtext::showtext_auto()
update_geom_defaults("text",  list(family = fonte))
update_geom_defaults("label", list(family = fonte))

# ---- Temas ------------------------------------------------------
tema_mapa <- theme_void(base_size = 10, base_family = fonte) +
  theme(plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 3)),
        plot.subtitle = element_text(size = 10.5, color = "grey25", margin = margin(b = 10)),
        plot.caption  = element_text(size = 8, color = "grey35", hjust = 0,
                                     lineheight = 1.1, margin = margin(t = 10)),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        legend.position = "bottom",
        legend.title  = element_text(size = 9.5, face = "bold", margin = margin(b = 4)),
        legend.text   = element_text(size = 9, color = "grey15"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin   = margin(14, 14, 10, 14))

tema_graf <- theme_minimal(base_size = 10.5, base_family = fonte) +
  theme(plot.title    = element_text(face = "bold", size = 14, margin = margin(b = 3)),
        plot.subtitle = element_text(size = 10.5, color = "grey25", margin = margin(b = 10)),
        plot.caption  = element_text(size = 8, color = "grey35", hjust = 0,
                                     lineheight = 1.1, margin = margin(t = 10)),
        plot.title.position   = "plot",
        plot.caption.position = "plot",
        axis.text     = element_text(color = "grey15"),
        axis.title    = element_text(size = 9.5, color = "grey25"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        legend.position = "top",
        legend.justification = "left",
        legend.text   = element_text(size = 9.5),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin   = margin(14, 14, 10, 14))

# ---- Salvar em PNG 300 dpi (com a fonte embutida corretamente) --
salvar <- function(g, nome, w = 7, h = 7) {
  arq <- file.path("figuras", paste0(nome, ".png"))
  tipo <- if (isTRUE(capabilities("cairo"))) "cairo" else NULL
  tryCatch({
    showtext::showtext_opts(dpi = 300)
    do.call(png, c(list(filename = arq, width = w, height = h, units = "in",
                        res = 300, bg = "white"),
                   if (!is.null(tipo)) list(type = tipo)))
    print(g)
    dev.off()
    message("Salvo: ", arq)
  }, error = function(e) {
    try(dev.off(), silent = TRUE)
    if (file.exists(arq)) file.remove(arq)
    message("ERRO em ", nome, ": ", conditionMessage(e))
  }, finally = showtext::showtext_opts(dpi = 96))
}

fonte_pib <- "Fonte: IBGE, PIB dos Municípios (SIDRA 5938). Malha: geobr/Ipea."
fonte_pam <- "Fonte: IBGE, Produção Agrícola Municipal (SIDRA 5457). Malha: geobr/Ipea."
