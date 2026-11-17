# =============================================================
# 01 - Mapa do Brasil com dados do IBGE
# geobr  -> malhas (mapas) oficiais, pacote do Ipea
# sidrar -> dados do IBGE direto do SIDRA (ex.: PAM - Produção Agrícola Municipal)
# =============================================================

# ---- 1. Instalar pacotes (só instala o que estiver faltando) --
# Cria a biblioteca pessoal do usuário (evita o erro "lib is not writable")
lib_usuario <- Sys.getenv("R_LIBS_USER")
dir.create(lib_usuario, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib_usuario, .libPaths()))

pacotes <- c("geobr", "sidrar", "sf", "ggplot2", "dplyr")
faltando <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(faltando) > 0) {
  install.packages(faltando, lib = lib_usuario,
                   repos = "https://cloud.r-project.org")
}

# ---- 2. Carregar pacotes ------------------------------------
library(geobr)
library(sidrar)
library(sf)
library(ggplot2)
library(dplyr)

# ---- 3. Baixar o mapa dos estados (geobr / Ipea) ------------
estados <- read_state(code_state = "all", year = 2020, showProgress = FALSE)

# Teste rápido: mapa em branco do Brasil
print(
  ggplot(estados) +
    geom_sf(fill = "grey90", color = "white") +
    theme_void()
)

# ---- 4. Baixar dados do IBGE (SIDRA) ------------------------
# Tabela 5457 = PAM (lavouras temporárias e permanentes)
# v/214 = quantidade produzida | n3 = por estado | p/last 1 = último ano disponível
pam <- get_sidra(api = "/t/5457/n3/all/v/214/p/last%201/c782/allxt")
ano <- unique(pam$Ano)
cat("Ano dos dados:", ano, "\n")

# Veja os nomes das colunas e dos produtos disponíveis
print(names(pam))
print(unique(pam[["Produto das lavouras temporárias e permanentes"]]))

# Filtra só a soja (troque por "Milho", "Café", "Cana" etc.)
soja <- pam %>%
  filter(grepl("^Soja", `Produto das lavouras temporárias e permanentes`)) %>%
  transmute(code_state = as.numeric(`Unidade da Federação (Código)`),
            producao_t = Valor)

# ---- 5. Juntar dados + mapa e plotar ------------------------
mapa <- estados %>% left_join(soja, by = "code_state")

g <- ggplot(mapa) +
  geom_sf(aes(fill = producao_t / 1e6), color = "white", linewidth = 0.2) +
  scale_fill_distiller(palette = "Greens", direction = 1,
                       name = "Produção\n(milhões t)", na.value = "grey90") +
  labs(title = paste("Produção de soja por estado -", ano),
       caption = "Fonte: IBGE - PAM (SIDRA, tabela 5457). Malha: geobr/Ipea.") +
  theme_void()

print(g)

# ---- 6. Salvar a figura em alta resolução (para o artigo) ---
ggsave(paste0("mapa_soja_", ano, ".png"), g, width = 7, height = 7, dpi = 300)
