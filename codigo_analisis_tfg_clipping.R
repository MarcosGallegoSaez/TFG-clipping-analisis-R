############################################################
# Trabajo de Fin de Grado
# Análisis estadístico de la encuesta sobre content clipping
# Autor: Marcos Gallego Sáez
# Universidad: CUNEF Universidad
# Curso: 2025-2026
#
# Este script reproduce el análisis estadístico realizado
# sobre la encuesta utilizada en el TFG.
############################################################

# 1. Paquetes ---------------------------------------------------------------

paquetes <- c(
  "tidyverse",
  "janitor",
  "psych",
  "GPArotation",
  "corrplot",
  "broom",
  "knitr",
  "readr",
  "stringr"
)

paquetes_no_instalados <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

if (length(paquetes_no_instalados) > 0) {
  install.packages(paquetes_no_instalados)
}

lapply(paquetes, library, character.only = TRUE)


# 2. Carga de datos ---------------------------------------------------------

# El archivo debe estar en la misma carpeta que este script.
# Nombre recomendado: datos_encuesta_anonimizados.csv

datos_raw <- read_csv("datos_encuesta_anonimizados.csv", show_col_types = FALSE)

# Limpieza de nombres de columnas para facilitar el análisis
datos <- datos_raw %>%
  clean_names()

# Comprobación básica
glimpse(datos)
summary(datos)


# 3. Identificación de variables ------------------------------------------

# Nota:
# Los nombres exactos de columnas pueden variar según la exportación de Google Forms.
# Por ello, se renombran manualmente las variables principales.
# Si algún nombre no coincide, revisar names(datos) y ajustar el nombre de origen.

names(datos)

# Renombrado recomendado.
# IMPORTANTE: ajustar los nombres de la izquierda a los nombres reales
# que aparezcan tras ejecutar names(datos).

datos <- datos %>%
  rename(
    actividad_profesional = matches("actividad_profesional|describe_mejor_tu_actividad"),
    usa_redes = matches("utilizas_redes_sociales"),
    seguidores = matches("seguidores"),
    ingresos = matches("ingresos_mensuales"),
    redes = matches("redes_sociales_tienes_mayor_presencia"),
    frecuencia_publicacion = matches("frecuencia_publicas"),
    monetizacion = matches("modelo_de_monetizacion"),
    
    dificultad_publicar = matches("dificil_mantener_una_publicacion"),
    dependencia_ads = matches("dependo_o_dependeria_de_publicidad"),
    interes_organico = matches("estrategias_organicas"),
    
    conocimiento_clipping = matches("conocias_el_concepto"),
    experiencia_clipping = matches("has_contratado_o_utilizado"),
    
    visibilidad = matches("aumentar_su_visibilidad"),
    autoridad_confianza = matches("autoridad_y_confianza"),
    captar_clientes = matches("captar_potenciales_clientes"),
    ahorro_tiempo = matches("ahorrar_tiempo"),
    confianza_proveedor = matches("confianza_en_la_agencia"),
    riesgo_imagen = matches("danaran_mi_imagen|dañaran_mi_imagen"),
    medicion_retorno = matches("medir_claramente_el_retorno"),
    
    modelo_preferido = matches("modelo_de_clipping.*preferirias"),
    pago_visibilidad = matches("pagarias_por_clipping.*alcance"),
    intencion_adopcion = matches("dispuesto_a_contratar_o_probar"),
    recomendacion = matches("recomendaria_el_clipping"),
    pago_visibilidad_masiva = matches("visibilidad_masiva"),
    pago_clientes_cualificados = matches("clientes_potenciales_cualificados"),
    fee_inicial = matches("fee_inicial")
  )


# 4. Conversión de variables Likert ----------------------------------------

# Las variables Likert proceden de escalas 1-5.
# Se convierten a formato numérico para el análisis descriptivo,
# correlaciones, fiabilidad y regresión.

vars_likert <- c(
  "dificultad_publicar",
  "dependencia_ads",
  "interes_organico",
  "visibilidad",
  "autoridad_confianza",
  "captar_clientes",
  "ahorro_tiempo",
  "confianza_proveedor",
  "riesgo_imagen",
  "medicion_retorno",
  "intencion_adopcion",
  "recomendacion"
)

datos <- datos %>%
  mutate(across(all_of(vars_likert), ~ as.numeric(as.character(.))))


# 5. Análisis descriptivo de la muestra ------------------------------------

tabla_actividad <- datos %>%
  count(actividad_profesional) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

tabla_seguidores <- datos %>%
  count(seguidores) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

tabla_ingresos <- datos %>%
  count(ingresos) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

tabla_actividad
tabla_seguidores
tabla_ingresos


# 6. Descriptivos de variables Likert --------------------------------------

descriptivos_likert <- datos %>%
  select(all_of(vars_likert)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
  group_by(variable) %>%
  summarise(
    n = sum(!is.na(valor)),
    media = round(mean(valor, na.rm = TRUE), 2),
    mediana = round(median(valor, na.rm = TRUE), 2),
    desviacion_tipica = round(sd(valor, na.rm = TRUE), 2),
    minimo = min(valor, na.rm = TRUE),
    maximo = max(valor, na.rm = TRUE),
    porcentaje_acuerdo = round(mean(valor >= 4, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

descriptivos_likert


# 7. Recodificación de conocimiento y experiencia --------------------------

# Recodificación aplicada para evitar solapamientos entre categorías originales.

datos <- datos %>%
  mutate(
    conocimiento_recod = case_when(
      str_detect(str_to_lower(conocimiento_clipping), "no lo conoc") ~ "No lo conocía",
      str_detect(str_to_lower(conocimiento_clipping), "escuchado") ~ "Conocimiento superficial",
      str_detect(str_to_lower(conocimiento_clipping), "parcial") ~ "Conocimiento superficial",
      str_detect(str_to_lower(conocimiento_clipping), "conozco bien") ~ "Lo conoce bien",
      TRUE ~ NA_character_
    ),
    experiencia_recod = case_when(
      experiencia_clipping == "No" ~ "No le interesa",
      str_detect(str_to_lower(experiencia_clipping), "planteado") ~ "Interés potencial",
      str_detect(str_to_lower(experiencia_clipping), "interesarme") ~ "Interés potencial",
      experiencia_clipping == "Sí" ~ "Sí lo ha usado/contratado",
      TRUE ~ NA_character_
    )
  )

tabla_conocimiento <- datos %>%
  count(conocimiento_recod) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))

tabla_experiencia <- datos %>%
  count(experiencia_recod) %>%
  mutate(porcentaje = round(n / sum(n) * 100, 1))

tabla_conocimiento
tabla_experiencia


# 8. Gráfico conocimiento e interés ----------------------------------------

dir.create("figuras_resultados", showWarnings = FALSE)

g_conocimiento <- ggplot(tabla_conocimiento, aes(x = reorder(conocimiento_recod, n), y = n)) +
  geom_col(fill = "#356899") +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")), hjust = -0.1, size = 3.2) +
  coord_flip() +
  labs(
    title = "Conocimiento previo del clipping",
    x = NULL,
    y = "Frecuencia"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank()
  ) +
  ylim(0, max(tabla_conocimiento$n, na.rm = TRUE) + 15)

g_experiencia <- ggplot(tabla_experiencia, aes(x = reorder(experiencia_recod, n), y = n)) +
  geom_col(fill = "#356899") +
  geom_text(aes(label = paste0(n, " (", porcentaje, "%)")), hjust = -0.1, size = 3.2) +
  coord_flip() +
  labs(
    title = "Experiencia e interés hacia servicios de clipping",
    x = NULL,
    y = "Frecuencia"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_blank()
  ) +
  ylim(0, max(tabla_experiencia$n, na.rm = TRUE) + 15)

ggsave(
  filename = "figuras_resultados/conocimiento_experiencia_clipping.png",
  plot = g_conocimiento + g_experiencia,
  width = 11,
  height = 5.5,
  dpi = 300
)


# 9. Adecuación para análisis factorial ------------------------------------

items_percepcion <- datos %>%
  select(
    visibilidad,
    autoridad_confianza,
    captar_clientes,
    ahorro_tiempo,
    confianza_proveedor,
    riesgo_imagen,
    medicion_retorno,
    intencion_adopcion,
    recomendacion
  ) %>%
  drop_na()

# Matriz de correlaciones
matriz_cor <- cor(items_percepcion, use = "pairwise.complete.obs")

# KMO y Bartlett
kmo_result <- KMO(matriz_cor)
bartlett_result <- cortest.bartlett(matriz_cor, n = nrow(items_percepcion))

kmo_result
bartlett_result


# 10. Análisis factorial exploratorio --------------------------------------

afe_result <- fa(
  r = matriz_cor,
  nfactors = 4,
  rotate = "varimax",
  fm = "ml"
)

afe_result
print(afe_result$loadings, cutoff = 0.30)


# 11. Fiabilidad interna: alfa de Cronbach ---------------------------------

# Escala de utilidad percibida
escala_utilidad <- datos %>%
  select(visibilidad, autoridad_confianza, captar_clientes, ahorro_tiempo) %>%
  drop_na()

alpha_utilidad <- psych::alpha(escala_utilidad)

# Escala de riesgo percibido
escala_riesgo <- datos %>%
  select(riesgo_imagen, medicion_retorno) %>%
  drop_na()

alpha_riesgo <- psych::alpha(escala_riesgo)

# Escala de adopción/recomendación
escala_adopcion <- datos %>%
  select(intencion_adopcion, recomendacion) %>%
  drop_na()

alpha_adopcion <- psych::alpha(escala_adopcion)

alpha_utilidad$total
alpha_riesgo$total
alpha_adopcion$total


# 12. Construcción de índices exploratorios --------------------------------

# Debido a que algunas escalas presentan fiabilidad baja,
# estos índices se interpretan con cautela y como aproximaciones exploratorias.

datos <- datos %>%
  mutate(
    indice_utilidad = rowMeans(select(., visibilidad, autoridad_confianza, captar_clientes, ahorro_tiempo), na.rm = TRUE),
    indice_riesgo = rowMeans(select(., riesgo_imagen, medicion_retorno), na.rm = TRUE),
    indice_adopcion = rowMeans(select(., intencion_adopcion, recomendacion), na.rm = TRUE)
  )


# 13. Correlaciones de Spearman --------------------------------------------

variables_cor <- datos %>%
  select(
    intencion_adopcion,
    indice_utilidad,
    confianza_proveedor,
    dificultad_publicar,
    indice_riesgo,
    dependencia_ads
  ) %>%
  drop_na()

cor_spearman <- cor(
  variables_cor,
  method = "spearman",
  use = "pairwise.complete.obs"
)

cor_spearman

# P-valores de Spearman
cor_test_spearman <- function(x, y) {
  test <- cor.test(x, y, method = "spearman", exact = FALSE)
  data.frame(
    rho = unname(test$estimate),
    p_value = test$p.value
  )
}

resultados_spearman <- map_dfr(
  names(variables_cor)[-1],
  ~ cor_test_spearman(variables_cor$intencion_adopcion, variables_cor[[.x]]) %>%
    mutate(variable = .x)
) %>%
  select(variable, rho, p_value)

resultados_spearman


# 14. Contraste binomial para H5 -------------------------------------------

# Se contrasta si la preferencia por pocos clippers de alta calidad
# supera una distribución equilibrada frente a la alternativa de mayor escala.

preferencia_calidad <- datos %>%
  filter(!is.na(modelo_preferido)) %>%
  mutate(
    prefiere_calidad = if_else(
      str_detect(str_to_lower(modelo_preferido), "pocos"),
      1,
      0
    )
  )

n_calidad <- sum(preferencia_calidad$prefiere_calidad == 1, na.rm = TRUE)
n_total_preferencia <- nrow(preferencia_calidad)

binom_result <- binom.test(
  x = n_calidad,
  n = n_total_preferencia,
  p = 0.5,
  alternative = "greater"
)

binom_result


# 15. Regresión lineal múltiple --------------------------------------------

modelo_regresion <- lm(
  intencion_adopcion ~ indice_utilidad +
    confianza_proveedor +
    dificultad_publicar +
    indice_riesgo +
    dependencia_ads,
  data = datos
)

summary(modelo_regresion)

tabla_regresion <- broom::tidy(modelo_regresion)
resumen_modelo <- broom::glance(modelo_regresion)

tabla_regresion
resumen_modelo


# 16. Exportación de tablas -------------------------------------------------

dir.create("tablas_resultados", showWarnings = FALSE)

write_csv(descriptivos_likert, "tablas_resultados/descriptivos_likert.csv")
write_csv(resultados_spearman, "tablas_resultados/correlaciones_spearman.csv")
write_csv(tabla_regresion, "tablas_resultados/regresion_lineal_multiple.csv")
write_csv(resumen_modelo, "tablas_resultados/resumen_modelo.csv")


# 17. Fin del script --------------------------------------------------------

cat("Análisis completado correctamente.\n")
