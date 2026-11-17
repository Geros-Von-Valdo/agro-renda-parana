# =============================================================
# 03 - Renda: onde a riqueza produzida fica?
# Cruza o PIB municipal (onde se produz) com a renda dos moradores
# (Censo 2022) e o salário formal (Cadastro Central de Empresas).
# Pré-requisito: 01_dados.R já rodado.
# =============================================================
source("00b_base_mapas.R")
if (!"jsonlite" %in% rownames(installed.packages()))
  install.packages("jsonlite", lib = lib_usuario, repos = "https://cloud.r-project.org")
library(jsonlite)

# =============================================================
# 1. Localiza as tabelas certas no SIDRA pelo catálogo oficial
#    (evita depender de códigos de tabela que o IBGE muda)
# =============================================================
api_ibge <- "https://servicodados.ibge.gov.br/api/v3/agregados"

catalogo <- cache("catalogo_sidra", {
  x <- fromJSON(api_ibge, simplifyVector = FALSE)
  bind_rows(lapply(x, function(p) bind_rows(lapply(p$agregados, function(a)
    tibble(pesquisa = p$nome, tabela = as.integer(a$id), nome = a$nome)))))
})

acha_tabela <- function(rotulo, padrao_pesq, padrao_tab, padrao_var, ano,
                        evitar_var = "^$", evitar_tab = "^$", max_cand = 40) {
  cand <- catalogo %>%
    filter(str_detect(pesquisa, regex(padrao_pesq, ignore_case = TRUE)),
           str_detect(nome,     regex(padrao_tab,  ignore_case = TRUE)),
          !str_detect(nome,     regex(evitar_tab,  ignore_case = TRUE))) %>%
    arrange(desc(tabela)) %>%          # tabelas mais novas primeiro
    head(max_cand)
  achados <- list()
  for (i in seq_len(nrow(cand))) {
    m <- tryCatch(fromJSON(paste0(api_ibge, "/", cand$tabela[i], "/metadados")),
                  error = function(e) NULL)
    Sys.sleep(0.2)
    if (is.null(m) || !"N6" %in% unlist(m$nivelTerritorial$Administrativo)) next
    ini <- as.numeric(m$periodicidade$inicio); fim <- as.numeric(m$periodicidade$fim)
    if (length(ini) && length(fim) && (ano < ini || ano > fim)) next
    v  <- m$variaveis
    ok <- str_detect(v$nome, regex(padrao_var, ignore_case = TRUE)) &
         !str_detect(v$nome, regex(evitar_var, ignore_case = TRUE))
    if (!any(ok)) next
    achados[[length(achados) + 1]] <- tibble(
      tabela = cand$tabela[i], nome_tab = cand$nome[i],
      variavel = v$id[ok][1], nome_var = v$nome[ok][1], unidade = v$unidade[ok][1],
      n_class = NROW(m$classificacoes))
  }
  if (!length(achados)) { message("Nenhuma tabela encontrada para: ", rotulo); return(NULL) }
  esc <- bind_rows(achados) %>% arrange(n_class) %>% slice(1)
  message(sprintf("\n[%s]\n  Tabela %s: %s\n  Variável %s: %s (%s)",
                  rotulo, esc$tabela, esc$nome_tab, esc$variavel, esc$nome_var, esc$unidade))
  esc
}

# Baixa uma variável por município; se houver classificações, fica com "Total"
baixa_mun <- function(info, ano, nome_col) {
  if (is.null(info)) return(NULL)
  df <- sidra(paste0("/t/", info$tabela, "/n6/all/v/", info$variavel, "/p/", ano))
  fixas <- c("Nível Territorial", "Unidade de Medida", "Valor", "Município", "Variável", "Ano")
  cls <- setdiff(names(df)[!str_detect(names(df), "Código")], fixas)
  for (cl in cls) if ("Total" %in% df[[cl]]) df <- df[df[[cl]] == "Total", ]
  df %>%
    transmute(code_muni = as.numeric(`Município (Código)`), valor = Valor) %>%
    distinct(code_muni, .keep_all = TRUE) %>%
    rename(!!nome_col := valor)
}

# Censo 2022, tabela 10295, variável 13431 = rendimento nominal médio mensal
# domiciliar per capita (testada: 5.570 municípios). Sem classificação na
# consulta, o SIDRA devolve o total (todos os sexos, idades e cores).
# Obs.: o Gini do Censo 2022 (tabela 10301) só existe até o nível estadual.
renda <- cache("renda_censo2022",
  sidra("/t/10295/n6/all/v/13431/p/2022") %>%
    transmute(code_muni = as.numeric(`Município (Código)`), renda_pc = Valor))
gini <- NULL

# Salário médio do emprego formal (CEMPRE, tabela 1685, variável 1606), já testado
sal <- cache(paste0("salario_cempre_", ano),
  sidra(paste0("/t/1685/n6/all/v/1606/p/", ano)) %>%
    transmute(code_muni = as.numeric(`Município (Código)`), sal_medio = Valor) %>%
    distinct(code_muni, .keep_all = TRUE))

# População estimada no mesmo ano do PIB (para o PIB per capita ficar coerente)
pop_ano <- tryCatch(
  cache(paste0("pop_estimada_", ano),
        sidra(paste0("/t/6579/n6/all/v/9324/p/", ano)) %>%
          transmute(code_muni = as.numeric(`Município (Código)`), pop_ano = Valor)),
  error = function(e) { message("Estimativa de população não baixada; usando Censo 2022."); NULL })

# =============================================================
# 2. Indicadores
# =============================================================
junta <- function(d) {
  for (x in list(renda, gini, sal, pop_ano)) if (!is.null(x)) d <- left_join(d, x, by = "code_muni")
  if (!"pop_ano" %in% names(d)) d$pop_ano <- d$pop
  d %>% mutate(
    pib_pc_ano = 1000 * pib / pop_ano,               # R$ por habitante no ano do PIB
    pib_pc_mes = pib_pc_ano / 12,                     # equivalente mensal
    retencao   = 100 * renda_pc / pib_pc_mes,         # renda dos moradores / PIB per capita
    setor = case_when(agro_pct >= pmax(ind_pct, serv_pct) ~ "Agropecuária",
                      ind_pct  >= pmax(agro_pct, serv_pct) ~ "Indústria",
                      TRUE ~ "Serviços e adm. pública"),
    setor = factor(setor, levels = c("Agropecuária", "Indústria", "Serviços e adm. pública")))
}
base_r <- junta(base)
pr_r   <- base_r %>% filter(abbrev_state == "PR")
cg_r   <- pr_r   %>% filter(name_intermediate == "Ponta Grossa")

stopifnot("Renda per capita não foi baixada; veja as mensagens acima" =
            sum(!is.na(base_r$renda_pc)) > 5000)

nota_anos <- paste0("PIB per capita de ", ano, " (população estimada do mesmo ano); ",
                    "renda domiciliar per capita do Censo 2022, em R$ correntes.")
fonte_renda <- paste0("Fonte: elaborado pelo autor com dados do IBGE: PIB dos Municípios (", ano,
                      ") e Censo Demográfico 2022. Malha: IBGE, via geobr (Ipea).")
reais <- label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",", accuracy = 1)
cores_setor <- c("Agropecuária" = "#4a9a3f", "Indústria" = "#5b4a9b",
                 "Serviços e adm. pública" = "grey60")

# =============================================================
# FIGURA 13 - Paraná: renda domiciliar per capita
# =============================================================
g13 <- ggplot(pr_r) +
  geom_sf(aes(fill = renda_pc), color = "white", linewidth = 0.08) +
  geom_sf(data = reg_pg, fill = NA, color = "grey10", linewidth = 0.7) +
  rotulos_pr() +
  scale_fill_fermenter(palette = "YlGnBu", direction = 1, n.breaks = 7,
                       labels = reais, na.value = "grey85",
                       name = "Rendimento domiciliar mensal por pessoa (Censo 2022)",
                       guide = guide_colorsteps(title.position = "top",
                                                barwidth = unit(14, "cm"),
                                                barheight = unit(0.35, "cm"))) +
  labs(title = "Quanto ganham, em média, os moradores de cada município",
       subtitle = "Rendimento nominal mensal domiciliar per capita, Paraná, 2022",
       caption = paste(fonte_renda, nota_contorno, sep = "\n")) +
  tema_mapa
salvar(g13, "fig13_parana_renda_pc", w = 9, h = 7)

# =============================================================
# FIGURA 14 - Região de PG: riqueza produzida x renda dos moradores
# =============================================================
cg_long <- cg_r %>%
  st_drop_geometry() %>%
  select(name_muni, pib_pc_mes, renda_pc, setor) %>%
  mutate(name_muni = fct_reorder(name_muni, pib_pc_mes))

g14 <- ggplot(cg_long, aes(y = name_muni)) +
  geom_segment(aes(x = renda_pc, xend = pib_pc_mes, yend = name_muni),
               color = "grey75", linewidth = 1.1) +
  geom_point(aes(x = pib_pc_mes, color = "PIB por habitante (equivalente mensal)"), size = 3) +
  geom_point(aes(x = renda_pc, color = "Renda domiciliar por pessoa (mensal)"), size = 3) +
  scale_color_manual(values = c("PIB por habitante (equivalente mensal)" = "#c0392b",
                                "Renda domiciliar por pessoa (mensal)" = "#1f4e79"),
                     name = NULL) +
  scale_x_continuous(labels = reais, expand = expansion(mult = c(0.02, 0.05))) +
  labs(title = "A riqueza que o município produz e a renda que fica com os moradores",
       subtitle = "Região Geográfica Intermediária de Ponta Grossa. Quanto maior a barra, maior a distância entre produção e renda",
       x = NULL, y = NULL,
       caption = paste(fonte_renda, nota_anos, sep = "\n")) +
  tema_graf
salvar(g14, "fig14_campos_gerais_pib_vs_renda", w = 9, h = 8.5)

# =============================================================
# FIGURA 15 - Paraná: PIB per capita x renda per capita
# =============================================================
pr_df <- pr_r %>% st_drop_geometry() %>% filter(!is.na(renda_pc), pib_pc_mes > 0)
destaque_nomes <- c("Ponta Grossa", "Curitiba", "Tibagi", "Castro", "Carambeí",
                    "Ortigueira", "Telêmaco Borba", "Imbaú", "Curiúva", "Teixeira Soares")
pr_df <- pr_df %>% mutate(rot = if_else(name_muni %in% destaque_nomes, name_muni, NA_character_),
                          regiao_pg = name_intermediate == "Ponta Grossa")

cor_sp <- cor(log(pr_df$pib_pc_mes), pr_df$renda_pc, method = "spearman")

g15 <- ggplot(pr_df, aes(pib_pc_mes, renda_pc)) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "grey20",
              fill = "grey85", linewidth = 0.6) +
  geom_point(aes(fill = setor), shape = 21, color = "white", stroke = 0.2, size = 2.2, alpha = 0.9) +
  geom_point(data = filter(pr_df, regiao_pg), shape = 21, fill = NA, color = "grey5",
             stroke = 0.5, size = 2.6) +
  geom_text_repel(aes(label = rot), family = fonte, size = 3, fontface = "bold",
                  bg.color = "white", bg.r = 0.15, min.segment.length = 0,
                  box.padding = 0.4, max.overlaps = Inf, seed = 5, na.rm = TRUE) +
  scale_x_log10(labels = reais) +
  scale_y_continuous(labels = reais) +
  scale_fill_manual(values = cores_setor, name = "Setor com maior valor adicionado") +
  labs(title = "Produzir mais riqueza nem sempre significa moradores com renda maior",
       subtitle = sprintf("Municípios do Paraná. Contorno preto: região de Ponta Grossa. Correlação de Spearman = %s",
                          format(round(cor_sp, 2), decimal.mark = ",")),
       x = "PIB por habitante, equivalente mensal (escala logarítmica)",
       y = "Renda domiciliar mensal por pessoa",
       caption = paste(fonte_renda, nota_anos, sep = "\n")) +
  tema_graf +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3))
salvar(g15, "fig15_parana_pib_vs_renda", w = 9, h = 7)

# =============================================================
# FIGURA 16 - Paraná: quanto da riqueza produzida vira renda local
# =============================================================
pr_r <- pr_r %>% mutate(ret_cl = cut(retencao, c(0, 25, 35, 45, 55, 70, Inf), right = FALSE,
  labels = c("Menos de 25%", "25% a 35%", "35% a 45%", "45% a 55%", "55% a 70%", "70% ou mais")))

g16 <- ggplot(pr_r) +
  geom_sf(aes(fill = ret_cl), color = "white", linewidth = 0.08) +
  geom_sf(data = reg_pg, fill = NA, color = "grey10", linewidth = 0.7) +
  rotulos_pr() +
  scale_fill_brewer(palette = "RdYlBu", direction = 1, na.value = "grey85", drop = FALSE,
                    name = "Renda domiciliar per capita como proporção do PIB per capita",
                    guide = legenda_faixa()) +
  labs(title = "Quanto da riqueza produzida no município aparece na renda dos moradores",
       subtitle = "Tons vermelhos: o município produz muito mais do que seus moradores recebem",
       caption = paste(fonte_renda, nota_anos,
                       "Indicador exploratório: os anos diferem e parte do PIB remunera capital e impostos, não famílias.",
                       nota_contorno, sep = "\n")) +
  tema_mapa
salvar(g16, "fig16_parana_retencao_renda", w = 9, h = 7.5)

# =============================================================
# FIGURA 17 - Brasil e Paraná: peso do agro x renda dos moradores
# =============================================================
dupla <- bind_rows(
  base_r %>% st_drop_geometry() %>% mutate(recorte = "Brasil"),
  pr_r   %>% st_drop_geometry() %>% mutate(recorte = "Paraná")) %>%
  filter(!is.na(renda_pc), !is.na(agro_pct)) %>%
  mutate(recorte = factor(recorte, levels = c("Brasil", "Paraná")))

cors <- dupla %>% group_by(recorte) %>%
  summarise(rho = cor(agro_pct, renda_pc, method = "spearman"), .groups = "drop") %>%
  mutate(rot = paste0("Spearman = ", format(round(rho, 2), decimal.mark = ",")))

g17 <- ggplot(dupla, aes(agro_pct, renda_pc)) +
  geom_point(aes(fill = setor), shape = 21, color = "white", stroke = 0.1,
             size = 1.3, alpha = 0.7) +
  geom_smooth(method = "loess", formula = y ~ x, color = "grey10", fill = "grey80",
              linewidth = 0.7) +
  geom_text(data = cors, aes(x = Inf, y = Inf, label = rot), inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.5, family = fonte, size = 3.4, color = "grey20") +
  facet_wrap(~recorte, scales = "free_y") +
  scale_x_continuous(labels = label_percent(scale = 1), limits = c(0, 100)) +
  scale_y_continuous(labels = reais) +
  scale_fill_manual(values = cores_setor, name = "Setor com maior valor adicionado",
                    guide = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  labs(title = "Municípios mais dependentes do agro têm moradores mais ricos ou mais pobres?",
       subtitle = "Participação da agropecuária no valor adicionado x renda domiciliar per capita",
       x = "Participação da agropecuária no valor adicionado bruto",
       y = "Renda domiciliar mensal por pessoa",
       caption = paste(fonte_renda, nota_anos, sep = "\n")) +
  tema_graf +
  theme(panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        strip.text = element_text(face = "bold", size = 11, hjust = 0))
salvar(g17, "fig17_agro_vs_renda_brasil_parana", w = 11, h = 6)

# =============================================================
# Tabelas
# =============================================================
cg_r %>%
  st_drop_geometry() %>%
  transmute(municipio = name_muni, setor_predominante = setor,
            agro_pct, ind_pct, pib_pc_mes, renda_pc, retencao,
            across(any_of(c("gini", "sal_medio")))) %>%
  arrange(desc(pib_pc_mes)) %>%
  write_csv2("tabelas/renda_campos_gerais.csv")

correl <- function(d, recorte) {
  d <- st_drop_geometry(d)
  vars <- intersect(c("agro_pct", "ind_pct", "pib_pc_mes", "sal_medio", "gini"), names(d))
  tibble(recorte = recorte, variavel = vars,
         spearman_com_renda_pc = sapply(vars, function(v)
           cor(d[[v]], d$renda_pc, method = "spearman", use = "complete.obs")))
}
bind_rows(correl(base_r, "Brasil"), correl(pr_r, "Paraná"), correl(cg_r, "Região de PG")) %>%
  write_csv2("tabelas/correlacoes_renda.csv")

saveRDS(base_r, "dados/base_municipios_renda.rds")
message("Pronto! Figuras 13 a 17 e tabelas de renda geradas.")
