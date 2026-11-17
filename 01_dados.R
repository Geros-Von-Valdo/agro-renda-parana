# =============================================================
# 01 - Coleta e preparo dos dados (rode uma vez; fica em cache)
# Saída: dados/base_municipios.rds  (sf com todos os indicadores)
# =============================================================
source("00_config.R")

# ---- 1. Malhas (geobr / Ipea) --------------------------------
mun <- cache("malha_mun", read_municipality(year = 2020, showProgress = FALSE))
uf  <- cache("malha_uf",  read_state(year = 2020, showProgress = FALSE))
rgi <- cache("malha_rgint", tryCatch(
  read_intermediate_region(year = 2020, showProgress = FALSE),
  error = function(e) read_intermediate_region(year = 2019, showProgress = FALSE)))

# remove os polígonos de lagoas (Lagoa dos Patos / Mirim), que não são municípios
mun <- mun %>% filter(!code_muni %in% c(4300001, 4300002))

# Região geográfica intermediária de cada município (para o recorte Ponta Grossa)
mun_rgi <- suppressWarnings(
  st_join(st_point_on_surface(mun),
          rgi %>% select(code_intermediate, name_intermediate))
) %>%
  st_drop_geometry() %>%
  select(code_muni, code_intermediate, name_intermediate)

# ---- 2. PIB dos Municípios (SIDRA 5938, último ano) ----------
# v/37  = PIB a preços correntes      v/498 = VAB total
# v/513 = VAB da agropecuária         v/517 = VAB da indústria
# (valores em mil R$). Se der erro, confira com info_sidra(5938)
# Baixa cada variável separadamente (últimos 3 anos) e usa o ano mais
# recente em que o VAB está publicado
pib_raw <- cache("pib_mun_raw_v2", {
  bind_rows(lapply(c(37, 498, 513, 517), function(v) {
    message("PIB municipal - variável ", v)
    Sys.sleep(1)
    sidra(paste0("/t/5938/n6/all/v/", v, "/p/last 3"))
  }))
})

pib_long <- pib_raw %>%
  transmute(
    code_muni = as.numeric(`Município (Código)`),
    ano_pib   = as.numeric(Ano),
    var = case_when(
      str_detect(Variável, regex("^Produto Interno Bruto", ignore_case = TRUE)) ~ "pib",
      str_detect(Variável, regex("total", ignore_case = TRUE))                 ~ "vab_total",
      str_detect(Variável, regex("agropecu", ignore_case = TRUE))              ~ "vab_agro",
      str_detect(Variável, regex("ind[uú]stria", ignore_case = TRUE))          ~ "vab_ind"),
    valor = Valor) %>%
  filter(!is.na(var))

# anos em que o VAB da agropecuária veio preenchido
cobertura <- pib_long %>%
  filter(var == "vab_agro") %>%
  group_by(ano_pib) %>%
  summarise(preenchidos = sum(!is.na(valor)))
print(cobertura)
ano_ok <- max(cobertura$ano_pib[cobertura$preenchidos > 5000])
cat("Usando o ano do PIB municipal:", ano_ok, "\n")

pib <- pib_long %>%
  filter(ano_pib == ano_ok) %>%
  pivot_wider(names_from = var, values_from = valor)

stopifnot("Variáveis do PIB não encontradas; rode info_sidra(5938)" =
            all(c("pib", "vab_total", "vab_agro", "vab_ind") %in% names(pib)),
          "VAB veio vazio; veja a tabela 'cobertura' acima" =
            sum(!is.na(pib$vab_agro)) > 5000)

pib <- pib %>%
  mutate(agro_pct = 100 * vab_agro / vab_total,
         ind_pct  = 100 * vab_ind  / vab_total,
         serv_pct = 100 - agro_pct - ind_pct)   # serviços + adm. pública

cat("Ano do PIB municipal:", unique(pib$ano_pib), "\n")

# ---- 3. População (Censo 2022, SIDRA 4709) -------------------
pop <- tryCatch(
  cache("pop_censo2022",
        sidra("/t/4709/n6/all/v/93/p/all")) %>%
    transmute(code_muni = as.numeric(`Município (Código)`), pop = Valor),
  error = function(e) { message("População não baixada: ", e$message); NULL })

# ---- 4. PAM: valor da produção por cultura (SIDRA 5457) ------
# Baixa estado por estado (a API do SIDRA limita o tamanho da consulta)
# v/215 = valor da produção (mil R$)
pam_raw <- cache("pam_valor_raw", {
  lista <- lapply(sort(unique(uf$code_state)), function(u) {
    message("PAM - UF ", u)
    Sys.sleep(1)
    sidra(paste0("/t/5457/n6/in n3 ", u,
                           "/v/215/p/last 1/c782/allxt"))
  })
  bind_rows(lista)
})

pam <- pam_raw %>%
  transmute(code_muni = as.numeric(`Município (Código)`),
            ano_pam   = as.numeric(Ano),
            produto   = `Produto das lavouras temporárias e permanentes`,
            valor     = Valor) %>%
  filter(!is.na(valor), valor > 0,
         !str_detect(produto, "Arábica|Canephora")) %>%   # evita contar café 2x
  mutate(produto = produto %>%
           str_remove_all("\\s*\\(.*?\\)") %>%
           str_remove("\\s+Total$") %>%
           str_squish())

dominante <- pam %>%
  group_by(code_muni) %>%
  mutate(valor_lavouras = sum(valor)) %>%
  slice_max(valor, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(code_muni, ano_pam,
            cultura_dom   = produto,
            cultura_dom_pct = 100 * valor / valor_lavouras,
            valor_lavouras)

saveRDS(pam, "dados/pam_limpa.rds")   # usada nos gráficos por cultura

# ---- 5. Base final -------------------------------------------
base <- mun %>%
  left_join(mun_rgi,   by = "code_muni") %>%
  left_join(pib,       by = "code_muni") %>%
  left_join(dominante, by = "code_muni")

if (!is.null(pop)) {
  base <- base %>%
    left_join(pop, by = "code_muni") %>%
    mutate(pib_pc      = 1000 * pib / pop,        # R$ por habitante
           vab_agro_pc = 1000 * vab_agro / pop)
}

saveRDS(base, "dados/base_municipios.rds")
saveRDS(uf,   "dados/malha_uf_final.rds")
saveRDS(rgi,  "dados/malha_rgint_final.rds")

cat("Municípios na base:", nrow(base),
    "| sem PIB:", sum(is.na(base$pib)),
    "| sem PAM:", sum(is.na(base$cultura_dom)), "\n")
cat("Região intermediária de Ponta Grossa:",
    sum(base$name_intermediate == "Ponta Grossa", na.rm = TRUE), "municípios\n")
