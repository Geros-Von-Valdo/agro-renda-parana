# =============================================================
# 02 - Mapas e gráficos do artigo (Brasil -> Paraná -> Campos Gerais)
# Pré-requisito: ter rodado 01_dados.R
# =============================================================
source("00b_base_mapas.R")

# =============================================================
# FIGURA 1 - Brasil: peso da agropecuária no valor adicionado
# =============================================================
g1 <- ggplot(base) +
  geom_sf(aes(fill = agro_cl), color = NA) +
  geom_sf(data = uf, fill = NA, color = "grey30", linewidth = 0.25) +
  moldura_br +
  escala_agro +
  labs(title = "O peso da agropecuária na economia dos municípios",
       subtitle = paste0("Participação da agropecuária no valor adicionado bruto municipal, Brasil, ", ano),
       caption = fonte_pib) +
  tema_mapa
salvar(g1, "fig01_brasil_agro_vab", w = 8, h = 8.5)

# =============================================================
# FIGURA 2 - Brasil: lavoura de maior valor em cada município
# =============================================================
base_cult <- base %>% mutate(cultura = agrupa_cultura(cultura_dom, 8))

g2 <- ggplot(base_cult) +
  geom_sf(aes(fill = cultura), color = NA) +
  geom_sf(data = uf, fill = NA, color = "grey30", linewidth = 0.25) +
  moldura_br +
  scale_fill_manual(values = cores_para(levels(base_cult$cultura)),
                    name = "Lavoura com maior valor da produção no município",
                    guide = legenda_faixa(nrow = 2)) +
  labs(title = "Qual lavoura gera mais valor em cada município",
       subtitle = paste0("Lavoura temporária ou permanente de maior valor da produção, Brasil, ", ano_pam),
       caption = fonte_pam) +
  tema_mapa
salvar(g2, "fig02_brasil_cultura_dominante", w = 8, h = 8.8)

# =============================================================
# FIGURA 3 - Paraná: peso da agropecuária
# =============================================================
g3 <- ggplot(pr) +
  geom_sf(aes(fill = agro_cl), color = "white", linewidth = 0.08) +
  geom_sf(data = reg_pg, fill = NA, color = "grey10", linewidth = 0.7) +
  rotulos_pr() +
  escala_agro +
  labs(title = "O peso da agropecuária na economia dos municípios paranaenses",
       subtitle = paste0("Participação da agropecuária no valor adicionado bruto municipal, Paraná, ", ano),
       caption = paste(fonte_pib, nota_contorno, sep = "\n")) +
  tema_mapa
salvar(g3, "fig03_parana_agro_vab", w = 9, h = 7)

# =============================================================
# FIGURA 4 - Paraná: bivariado agropecuária x indústria
# Legenda ao lado do mapa (não sobrepõe o território)
# =============================================================
pr_bi <- bi_class(pr %>% filter(!is.na(agro_pct)),
                  x = agro_pct, y = ind_pct, style = "quantile", dim = 3)

mapa_bi <- ggplot(pr_bi) +
  geom_sf(aes(fill = bi_class), color = "white", linewidth = 0.08, show.legend = FALSE) +
  geom_sf(data = reg_pg, fill = NA, color = "grey10", linewidth = 0.7) +
  rotulos_pr() +
  bi_scale_fill(pal = "DkViolet2", dim = 3) +
  labs(title = "Onde o campo produz e onde a indústria pesa",
       subtitle = paste0("Participação da agropecuária e da indústria no valor adicionado bruto municipal, Paraná, ", ano),
       caption = paste(fonte_pib, nota_contorno,
                       "Classes definidas pelos tercis de cada variável entre os municípios do estado.",
                       sep = "\n")) +
  tema_mapa

leg_bi <- bi_legend(pal = "DkViolet2", dim = 3,
                    xlab = "Mais agropecuária", ylab = "Mais indústria", size = 9) +
  theme(text = element_text(family = fonte),
        plot.background = element_rect(fill = NA, color = NA),
        panel.background = element_rect(fill = NA, color = NA))

texto_bi <- paste(
  "Como ler o mapa",
  "",
  "Canto superior direito: agropecuária",
  "e indústria fortes (agroindústria).",
  "",
  "Canto inferior direito: predomínio",
  "da agropecuária.",
  "",
  "Canto superior esquerdo: predomínio",
  "da indústria.",
  "",
  "Canto inferior esquerdo: ambas fracas",
  "(economia de serviços).", sep = "\n")

g4 <- ggdraw() +
  draw_plot(mapa_bi, x = 0, y = 0, width = 0.76, height = 1) +
  draw_plot(leg_bi, x = 0.775, y = 0.55, width = 0.2, height = 0.25) +
  draw_label(texto_bi, x = 0.785, y = 0.52, hjust = 0, vjust = 1, size = 9,
             fontfamily = fonte, color = "grey20", lineheight = 1.05)
salvar(g4, "fig04_parana_bivariado_agro_industria", w = 11.5, h = 7)

# =============================================================
# FIGURA 5 - Região de Ponta Grossa: peso da agropecuária
# =============================================================
g5 <- ggplot(cg) +
  camada_entorno() +
  geom_sf(aes(fill = agro_cl), color = "white", linewidth = 0.25) +
  rotulos_regiao() +
  contexto_regional() +
  escala_agro +
  labs(title = "O peso da agropecuária na região de Ponta Grossa",
       subtitle = paste0("Participação da agropecuária no valor adicionado bruto municipal, ", ano),
       caption = paste(fonte_pib, nota_contorno, "Municípios fora da região em cinza.", sep = "\n")) +
  tema_mapa
salvar(g5, "fig05_campos_gerais_agro_vab", w = 8.5, h = 8)

# =============================================================
# FIGURA 6 - Região de Ponta Grossa: composição do valor adicionado
# =============================================================
comp <- cg %>%
  st_drop_geometry() %>%
  select(name_muni, agro_pct, ind_pct, serv_pct) %>%
  pivot_longer(-name_muni, names_to = "setor", values_to = "pct") %>%
  mutate(setor = factor(setor, levels = c("serv_pct", "ind_pct", "agro_pct"),
                        labels = c("Serviços e administração pública", "Indústria", "Agropecuária")))

ordem <- cg %>% st_drop_geometry() %>% arrange(agro_pct) %>% pull(name_muni)

g6 <- ggplot(comp, aes(x = pct, y = factor(name_muni, levels = ordem), fill = setor)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c("Agropecuária" = "#4a9a3f", "Indústria" = "#5b4a9b",
                               "Serviços e administração pública" = "grey82"),
                    breaks = c("Agropecuária", "Indústria", "Serviços e administração pública"),
                    name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1), expand = c(0, 0)) +
  labs(title = "Do que vive cada município da região de Ponta Grossa",
       subtitle = paste0("Composição do valor adicionado bruto por setor, ", ano,
                         ". Municípios ordenados pelo peso da agropecuária"),
       x = "Participação no valor adicionado bruto", y = NULL, caption = fonte_pib) +
  tema_graf
salvar(g6, "fig06_campos_gerais_composicao_vab", w = 8, h = 8.5)

# =============================================================
# FIGURA 7 - Paraná: 20 maiores valores adicionados da agropecuária
# =============================================================
top_pr <- pr %>%
  st_drop_geometry() %>%
  slice_max(vab_agro, n = 20) %>%
  mutate(regiao = if_else(name_intermediate == "Ponta Grossa",
                          "Região de Ponta Grossa", "Demais regiões do Paraná"))

g7 <- ggplot(top_pr, aes(x = vab_agro / 1e6, y = reorder(name_muni, vab_agro), fill = regiao)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = format(round(vab_agro / 1e6, 2), decimal.mark = ",", nsmall = 2)),
            hjust = -0.15, size = 3, family = fonte, color = "grey20") +
  scale_fill_manual(values = c("Região de Ponta Grossa" = "#c0392b",
                               "Demais regiões do Paraná" = "grey65"), name = NULL) +
  scale_x_continuous(labels = label_number(decimal.mark = ",", accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Os maiores produtores agropecuários do Paraná",
       subtitle = paste0("Valor adicionado bruto da agropecuária, em R$ bilhões correntes, ", ano),
       x = "R$ bilhões", y = NULL, caption = fonte_pib) +
  tema_graf
salvar(g7, "fig07_parana_top20_vab_agro", w = 8, h = 7)

# =============================================================
# FIGURA 10 - Paraná: lavoura de maior valor por município
# =============================================================
pr_cult <- pr %>% mutate(cultura = agrupa_cultura(cultura_dom, 7))
cores_pr <- cores_para(levels(pr_cult$cultura))

g10 <- ggplot(pr_cult) +
  geom_sf(aes(fill = cultura), color = "white", linewidth = 0.08) +
  geom_sf(data = reg_pg, fill = NA, color = "grey10", linewidth = 0.7) +
  rotulos_pr() +
  scale_fill_manual(values = cores_pr,
                    name = "Lavoura com maior valor da produção no município",
                    guide = legenda_faixa(nrow = 1)) +
  labs(title = "Qual lavoura gera mais valor em cada município paranaense",
       subtitle = paste0("Lavoura temporária ou permanente de maior valor da produção, Paraná, ", ano_pam),
       caption = paste(fonte_pam, nota_contorno, sep = "\n")) +
  tema_mapa
salvar(g10, "fig10_parana_cultura_dominante", w = 10, h = 7)

# =============================================================
# FIGURA 11 - Região de Ponta Grossa: lavoura de maior valor
# =============================================================
cg_cult <- pr_cult %>% filter(name_intermediate == "Ponta Grossa") %>%
  mutate(cultura = fct_drop(cultura))

g11 <- ggplot(cg_cult) +
  camada_entorno() +
  geom_sf(aes(fill = cultura), color = "white", linewidth = 0.25) +
  rotulos_regiao() +
  contexto_regional() +
  scale_fill_manual(values = cores_pr,
                    name = "Lavoura com maior valor da produção no município",
                    guide = legenda_faixa(nrow = 1)) +
  labs(title = "Qual lavoura gera mais valor na região de Ponta Grossa",
       subtitle = paste0("Lavoura temporária ou permanente de maior valor da produção, ", ano_pam),
       caption = paste(fonte_pam, nota_contorno, "Municípios fora da região em cinza.", sep = "\n")) +
  tema_mapa
salvar(g11, "fig11_campos_gerais_cultura_dominante", w = 8.5, h = 8)

# =============================================================
# FIGURA 12 - Composição do valor das lavouras: região x Paraná
# =============================================================
comp_cult <- bind_rows(
  pam %>% filter(code_muni %in% cg$code_muni) %>% mutate(recorte = "Região de Ponta Grossa"),
  pam %>% filter(code_muni %in% pr$code_muni) %>% mutate(recorte = "Paraná")) %>%
  group_by(recorte, produto) %>%
  summarise(valor = sum(valor), .groups = "drop_last") %>%
  mutate(pct = 100 * valor / sum(valor)) %>%
  ungroup()

top_cult <- comp_cult %>% group_by(produto) %>% summarise(m = max(pct)) %>%
  slice_max(m, n = 8) %>% pull(produto)

comp_cult <- comp_cult %>%
  mutate(produto = if_else(produto %in% top_cult, produto, "Outras lavouras")) %>%
  group_by(recorte, produto) %>% summarise(pct = sum(pct), .groups = "drop") %>%
  mutate(produto = fct_relevel(fct_reorder(produto, pct, .fun = max), "Outras lavouras"),
         recorte = factor(recorte, levels = c("Região de Ponta Grossa", "Paraná")))

g12 <- ggplot(comp_cult, aes(x = pct, y = produto, fill = recorte)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.74) +
  geom_text(aes(label = paste0(format(round(pct, 1), decimal.mark = ",", nsmall = 1), "%")),
            position = position_dodge(width = 0.8), hjust = -0.15, size = 3,
            family = fonte, color = "grey20") +
  scale_fill_manual(values = c("Região de Ponta Grossa" = "#c0392b", "Paraná" = "grey65"),
                    name = NULL) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Quanto cada lavoura representa no valor da produção agrícola",
       subtitle = paste0("Participação no valor total das lavouras, região de Ponta Grossa e Paraná, ", ano_pam),
       x = "Participação no valor da produção das lavouras", y = NULL, caption = fonte_pam) +
  tema_graf
salvar(g12, "fig12_composicao_lavouras_pg_vs_pr", w = 8, h = 6.5)
comp_cult %>% write_csv2("tabelas/composicao_lavouras_pg_vs_pr.csv")

# =============================================================
# FIGURAS 8 e 9 - Autocorrelação espacial: mapa LISA + diagrama de Moran
# =============================================================
rot_lisa <- c("Alto-Alto: agro forte cercado por vizinhos com agro forte",
              "Baixo-Baixo: agro fraco cercado por vizinhos com agro fraco",
              "Alto-Baixo: agro forte isolado em vizinhança de agro fraco",
              "Baixo-Alto: agro fraco isolado em vizinhança de agro forte",
              "Sem agrupamento estatisticamente significativo")
cores_lisa <- setNames(c("#c0392b", "#2c6fad", "#f0a35e", "#9ecae1", "grey92"), rot_lisa)

lisa_fig <- function(dados, var, titulo, nome, w, h,
                     borda = "grey70", esp_borda = 0.1,
                     camadas_mapa = NULL, destacar = NULL) {
  d  <- dados %>% filter(!is.na(.data[[var]]))
  nb <- poly2nb(d, queen = TRUE)
  lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

  z     <- as.numeric(scale(d[[var]]))
  defas <- lag.listw(lw, z, zero.policy = TRUE)
  moran <- moran.test(d[[var]], lw, zero.policy = TRUE)
  # significância local por 999 permutações condicionais (padrão GeoDa);
  # o p-valor analítico não é confiável para variáveis assimétricas como esta
  set.seed(42)
  lm    <- localmoran_perm(d[[var]], lw, nsim = 999, zero.policy = TRUE)
  quad  <- as.character(attr(lm, "quadr")$mean)
  col_p <- grep("Sim", colnames(lm))[1]
  if (is.na(col_p)) col_p <- grep("^Pr", colnames(lm))[1]
  pval  <- lm[, col_p]

  # municípios sem vizinhos (ilhas) têm p-valor NA: entram como não significativos
  cod <- c("High-High", "Low-Low", "High-Low", "Low-High")
  d$lisa <- factor(ifelse(!is.na(pval) & pval < 0.05 & quad %in% cod, quad, "ns"),
                   levels = c(cod, "ns"), labels = rot_lisa)

  p_txt <- ifelse(moran$p.value < 0.001, "p < 0,001",
                  paste0("p = ", format(round(moran$p.value, 3), decimal.mark = ",")))

  mapa <- ggplot(d) +
    geom_sf(aes(fill = lisa), color = borda, linewidth = esp_borda) +
    camadas_mapa +
    scale_fill_manual(values = cores_lisa, drop = FALSE,
                      name = "Agrupamento espacial do peso da agropecuária (LISA, p < 0,05)",
                      guide = guide_legend(title.position = "top", ncol = 2,
                                           keywidth = unit(0.5, "cm"),
                                           keyheight = unit(0.42, "cm"))) +
    tema_mapa +
    theme(legend.position = "bottom", legend.justification = "left")

  df <- tibble(nome = d$name_muni, z = z, defas = defas, lisa = d$lisa)
  r  <- max(abs(c(df$z, df$defas)), na.rm = TRUE)

  disp <- ggplot(df, aes(z, defas)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "grey55", linewidth = 0.3) +
    geom_point(aes(fill = lisa), shape = 21, color = "white", stroke = 0.15,
               size = ifelse(nrow(df) > 1000, 1.1, 2)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                color = "grey10", linewidth = 0.6) +
    annotate("text", x =  Inf, y =  Inf, label = "Alto-Alto",   hjust = 1.1, vjust = 1.6,
             family = fonte, size = 3.2, color = "grey35", fontface = "bold") +
    annotate("text", x = -Inf, y = -Inf, label = "Baixo-Baixo", hjust = -0.1, vjust = -1,
             family = fonte, size = 3.2, color = "grey35", fontface = "bold") +
    annotate("text", x =  Inf, y = -Inf, label = "Alto-Baixo",  hjust = 1.1, vjust = -1,
             family = fonte, size = 3.2, color = "grey35", fontface = "bold") +
    annotate("text", x = -Inf, y =  Inf, label = "Baixo-Alto",  hjust = -0.1, vjust = 1.6,
             family = fonte, size = 3.2, color = "grey35", fontface = "bold") +
    scale_fill_manual(values = cores_lisa, drop = FALSE, guide = "none") +
    labs(title = "Diagrama de dispersão de Moran",
         subtitle = sprintf("Inclinação da reta = I de Moran = %s",
                            format(round(moran$estimate[1], 3), decimal.mark = ",")),
         x = "Peso da agropecuária no município (padronizado)",
         y = "Média dos municípios vizinhos (padronizada)") +
    tema_graf +
    theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
          plot.title = element_text(size = 11.5),
          plot.subtitle = element_text(size = 9.5))

  if (!is.null(destacar)) {
    disp <- disp +
      geom_point(data = df %>% filter(tolower(nome) %in% tolower(destacar)),
                 shape = 21, size = 2.8, fill = NA, color = "grey5", stroke = 0.7) +
      geom_text_repel(data = df %>% filter(tolower(nome) %in% tolower(destacar)),
                      aes(label = nome), family = fonte, size = 3, fontface = "bold",
                      bg.color = "white", bg.r = 0.15, min.segment.length = 0,
                      box.padding = 0.5, seed = 4)
  }

  fig <- (mapa | disp) +
    plot_layout(widths = c(1.9, 1)) +
    plot_annotation(
      title = titulo,
      subtitle = sprintf("I de Moran global = %s (%s). Vizinhança por contiguidade (critério rainha).",
                         format(round(moran$estimate[1], 3), decimal.mark = ","), p_txt),
      caption = paste(fonte_pib,
                      "Significância local estimada por 999 permutações condicionais (p < 0,05).",
                      sep = "\n"),
      theme = tema_mapa)
  salvar(fig, nome, w, h)

  invisible(list(moran = moran, dados = d))
}

lisa_br <- lisa_fig(
  base, "agro_pct",
  "Agrupamentos espaciais do peso da agropecuária nos municípios brasileiros",
  "fig08_brasil_lisa_agro", w = 13, h = 8,
  borda = NA,
  camadas_mapa = list(geom_sf(data = uf, fill = NA, color = "grey25", linewidth = 0.3),
                      moldura_br))

lisa_pr <- lisa_fig(
  pr, "agro_pct",
  "Agrupamentos espaciais do peso da agropecuária nos municípios paranaenses",
  "fig09_parana_lisa_agro", w = 14, h = 8,
  borda = "grey60", esp_borda = 0.12,
  camadas_mapa = c(list(geom_sf(data = uf_pr, fill = NA, color = "grey15", linewidth = 0.4),
                        geom_sf(data = reg_pg, fill = NA, color = "black", linewidth = 0.8)),
                   rotulos_pr(tam = 2.6)),
  destacar = c("Ponta Grossa", "Curitiba", "Castro", "Tibagi"))

# ---- Tabelas de apoio ----------------------------------------
tibble(recorte = c("Brasil", "Paraná"),
       moran_I = c(lisa_br$moran$estimate[1], lisa_pr$moran$estimate[1]),
       p_valor = c(lisa_br$moran$p.value,     lisa_pr$moran$p.value)) %>%
  write_csv2("tabelas/moran_global.csv")

cg %>%
  st_drop_geometry() %>%
  select(name_muni, pib, vab_agro, vab_ind, agro_pct, ind_pct,
         cultura_dom, cultura_dom_pct, any_of(c("pop", "pib_pc"))) %>%
  arrange(desc(vab_agro)) %>%
  write_csv2("tabelas/campos_gerais_indicadores.csv")

lisa_pr$dados %>%
  st_drop_geometry() %>%
  count(lisa, name = "municipios") %>%
  write_csv2("tabelas/lisa_parana_resumo.csv")

message("Pronto! Figuras em figuras/ e tabelas em tabelas/")
