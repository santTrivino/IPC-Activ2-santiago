setwd("C:/Users/Usuario/OneDrive/UNIR/MAESTRIA UNIR/Primer cuatrimestre/Estadística y R para Ciencias de la Salud/Actividad 2")
dataset <- read.csv("Dataset expresión genes.csv",header = T, sep = ",", dec = ".")
install.packages("nortest")
install.packages("gtsummary")
install.packages("rstatix")
install.packages("gt")
install.packages("flextable")
install.packages("officer")

library(nortest) # Para el test de Anderson-Darling
library(tidyverse)
library(gtsummary)
library(purrr)
library(gt)

####### A PARTIR DE LA LINEA 122 VALE ########
# Aplicar test de Anderson-Darling para normalidad
prueba_normalidad <- dataset %>%
  select(starts_with("AQ_")) %>%
  summarise_all(~ ad.test(.)$p.value) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor_p") %>%
  mutate(Interpretación = ifelse(Valor_p > 0.05, "Normal", "No normal"))
print(prueba_normalidad)

ordenado <- sort(as.matrix(prueba_normalidad))  #ningún gen sigue distribución normal



# 1. Tabla en función del tratamiento y tipo de tumor (anidada - alternativa 3)
tabla_trat_tumor_lista <- dataset %>%
  select(starts_with("AQ_"), trat, tumor) %>%
  group_split(trat) %>%
  map(~ .x %>%
        tbl_summary(by = "tumor", include = starts_with("AQ_")) %>%
        modify_header(label ~ "**{level}**")
  )

tabla_trat_tumor_con_total <- list()
for (i in seq_along(tabla_trat_tumor_lista)) {
  tabla_overall <- dataset %>%
    filter(trat == unique(dataset$trat)[i]) %>%
    select(starts_with("AQ_")) %>%
    tbl_summary(include = starts_with("AQ_")) %>%
    modify_header(label ~ "**Total**")
  
  tabla_trat_tumor_con_total[[i]] <- tbl_stack(list(tabla_trat_tumor_lista[[i]], tabla_overall))
}

tabla_trat_tumor <- tbl_merge(tabla_trat_tumor_con_total, tab_spanner = names(tabla_trat_tumor_lista)) %>%
  modify_header(label ~ "**Gen**") %>%
  modify_caption("Expresión génica por tratamiento y tipo de tumor") %>%
  as_gt()

# Para visualizar la tabla
tabla_trat_tumor

# 2. Tabla en función de la edad categorizada por la mediana
mediana_edad <- median(dataset$edad, na.rm = TRUE)

dataset <- dataset %>%
  mutate(
    edad_categoria = ifelse(edad < mediana_edad,
                            paste0("< ", round(mediana_edad, 2)),
                            paste0(">= ", round(mediana_edad, 2))
    )
  )

tabla_edad <- dataset %>%
  select(starts_with("AQ_"), edad_categoria) %>%
  tbl_summary(
    by = "edad_categoria",
    include = starts_with("AQ_")
  ) %>%
  modify_header(label ~ "**Gen**") %>%
  modify_caption(paste0("Expresión génica por categoría de edad (Mediana = ", round(mediana_edad, 2), ")")) %>%
  as_gt()

 
  
#####################################################
#AQUÍ DE OTRA MANERA
####################################################

tabla_final <- dataset %>%
  select(starts_with("AQ_"), trat, tumor) %>%
  group_by(trat) %>%
  nest() %>%
  mutate(
    tabla = map(data, ~ .x %>%
                  tbl_cross(
                    row = starts_with("AQ_"),
                    col = tumor,
                    statistic = list(
                      all_continuous() ~ "{mean} ({sd})",
                      all_categorical() ~ "{n} ({p}%)"
                    ),
                    missing = "no"
                  ) %>%
                  add_p() %>%
                  modify_header(p.value ~ "**p-value**")
    )
  ) %>%
  unnest(tabla) %>%
  ungroup() %>%
  select(-data) %>%
  modify_header(label ~ "**Gen**") %>%
  modify_spanning_header(c(starts_with("CCR"), starts_with("CM"), starts_with("CP")) ~ "**{level}**") %>%
  modify_spanning_header(match = "CCR", columns = contains("tratA")) %>%
  modify_spanning_header(match = "CM", columns = contains("tratA")) %>%
  modify_spanning_header(match = "CP", columns = contains("tratA")) %>%
  modify_spanning_header(match = "CCR", columns = contains("tratB")) %>%
  modify_spanning_header(match = "CM", columns = contains("tratB")) %>%
  modify_spanning_header(match = "CP", columns = contains("tratB")) %>%
  modify_caption("Expresión génica por tratamiento y tipo de tumor") %>%
  as_gt()

# Para visualizar la tabla en la consola o en un visor
tabla_final



#####################################################
#AQUÍ LA MEJOR OPCION
####################################################


setwd("C:/Users/Usuario/OneDrive/UNIR/MAESTRIA UNIR/Primer cuatrimestre/Estadística y R para Ciencias de la Salud/Actividad 2")
dataset <- read.csv("Dataset expresión genes.csv",header = T, sep = ",", dec = ".")
# install.packages("nortest")
# install.packages("gtsummary")
# install.packages("rstatix")
# install.packages("gt")

library(tidyverse)
library(gtsummary)
# library(gt)

# Comprobar la normalidad de las variables (genes)

library(nortest) # Para el test de Anderson-Darling
# Aplicar el test de normalidad a cada gen
normalidad <- dataset %>% 
  select(starts_with("AQ_")) %>% 
  summarise_all(~ ad.test(.)$p.value) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "Valor_p") %>%
  mutate(Interpretación = ifelse(Valor_p > 0.05, "Normal", "No normal"))

# Crear la tabla de normalidad
tabla_1 <- tibble(
  Variable = normalidad$Variable,
  `Test utilizado` = "Anderson-Darling",
  `Valor p` = normalidad$Valor_p,
  Interpretación = normalidad$Interpretación
)

tabla_1
## Hasta aquí excelente en la generación de la Tabla


# Ahora generándola con gtsummary
# Convertir el tibble en una tabla de flextable
tabla_1_word <- tabla_1 %>%
  flextable() %>%
  autofit()  # Ajustar automáticamente el tamaño de las columnas

# Exportar la tabla a Word
save_as_docx(tabla_1_word, path = "Tabla_1.docx")


#MODIFICANDO PARA QUE CAMBIE EL FORMATO DE P CON 3 DIGITOS Y ENCABEZADOS EN NEGRITAS 

# Convertir el valor p
tabla_1 <- tabla_1 %>%
  mutate(`Valor p` = as.numeric(`Valor p`)) %>%  # Convertir a numérico
  mutate(`Valor p` = ifelse(`Valor p` < 0.001, "< 0.001", formatC(`Valor p`, format = "f", digits = 3)))  # Aplicar formato

# Crear la tabla en flextable con encabezados en negrita
tabla_1_word <- tabla_1 %>%
  flextable() %>%
  autofit() %>%
  bold(part = "header")  # Encabezados en negrita

# Exportar la tabla a Word
save_as_docx(tabla_1_word, path = "Tabla_1.docx")



# 3. Generar descriptivos
#==================================================
# PRIMER INTENTO, CASI PERFECTO. FALTA RANGO INTERCUARTILICO, NOTAC. CIENTIFICA
# Cargar librerías necesarias
library(gtsummary)
library(dplyr)
library(flextable)
library(officer)

# Tabla A: Variables sociodemográficas
tabla_A <- dataset %>%
  select(edad, sexo, exfumador) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_categorical() ~ "categorical",
    percent = "column"
  ) %>%
  modify_header(label ~ "**Variable Sociodemográfica**")

# Tabla B: Estado de salud
tabla_B <- dataset %>%
  select(hta, dm, alergia, cardiopatia, ETE, neumopatia, hepatopatia, colelitiasis, 
         utolitiasis, ITU, renal, neuropatia, corticoides, tos, disnea, expect, secrecion, 
         dolor_garg, escalofrios, fiebre, diarrea, nauseas, vomitos, cefalea, mareo, cansancio, 
         anosmia, disgueusia, dolor_hueso, dolor_abdo, perd_ape, score_dieta, calidad_fisica, 
         calidad_mental, tumor, extension) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_categorical() ~ "categorical",
    percent = "column"
  ) %>%
  modify_header(label ~ "**Estado de Salud**")

# Tabla C: Variables bioquímicas
tabla_C <- dataset %>%
  select(glucosa, leucocitos, linfocitos, neutrofilos, chol, hdl, hierro, igA, igE, igG, 
         igN, ldl, pcr, transferrina, trigliceridos, cpk) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_continuous() ~ "continuous"
  ) %>%
  modify_header(label ~ "**Variables Bioquímicas**")

# Tabla D: Expresión de los 46 genes
tabla_D <- dataset %>%
  select(starts_with("AQ_")) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_continuous() ~ "continuous"
  ) %>%
  modify_header(label ~ "**Expresión Génica**")

#  Exportar todas las tablas a Word
save_as_docx(
  flextable(tabla_A), path = "Tabla_A.docx"
)

save_as_docx(
  flextable(tabla_B), path = "Tabla_B.docx"
)

save_as_docx(
  flextable(tabla_C), path = "Tabla_C.docx"
)

save_as_docx(
  flextable(tabla_D), path = "Tabla_D.docx"
)

#==================================================================
#HASTA AQUÍ PRIMER INTENTO
#=================================================================

#SEGUNDO INTENTO

# Cargar librerías necesarias
library(gtsummary)
library(dplyr)
library(flextable)
library(officer)

# Tabla A: Variables sociodemográficas (mediana y rango intercuartílico)
tabla_A <- dataset %>%
  select(edad, sexo, exfumador) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    type = all_categorical() ~ "categorical",
    percent = "column"
  ) %>%
  modify_header(label ~ "**Variable Sociodemográfica**")
tabla_A


# Tabla B: Estado de salud (mediana y rango intercuartílico)
tabla_B <- dataset %>%
  select(hta, dm, alergia, cardiopatia, ETE, neumopatia, hepatopatia, colelitiasis, 
         utolitiasis, ITU, renal, neuropatia, corticoides, tos, disnea, expect, secrecion, 
         dolor_garg, escalofrios, fiebre, diarrea, nauseas, vomitos, cefalea, mareo, cansancio, 
         anosmia, disgueusia, dolor_hueso, dolor_abdo, perd_ape, score_dieta, calidad_fisica, 
         calidad_mental, tumor, extension) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    type = all_categorical() ~ "categorical",
    percent = "column"
  ) %>%
  modify_header(label ~ "**Estado de Salud**")
tabla_B



# ESTA TABLA NO SE VA A UTILIZAR Tabla C: Variables bioquímicas (modificación de nombres)
tabla_C <- dataset %>%
  select(glucosa, leucocitos, linfocitos, neutrofilos, chol, hdl, hierro, igA, igE, igG, 
         igN, ldl, pcr, transferrina, trigliceridos, cpk) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_continuous() ~ "continuous"
  ) %>%
  modify_header(
    glucosa ~ "**Glucosa (mg/dL)**",
    leucocitos ~ "**Leucocitos (10^3/µL)**",
    linfocitos ~ "**Linfocitos (%)**",
    neutrofilos ~ "**Neutrófilos (%)**",
    chol ~ "**Colesterol Total (mg/dL)**",
    hdl ~ "**HDL (mg/dL)**",
    hierro ~ "**Hierro (µg/dL)**",
    igA ~ "**IgA (mg/dL)**",
    igE ~ "**IgE (UI/mL)**",
    igG ~ "**IgG (mg/dL)**",
    igN ~ "**IgM (mg/dL)**",
    ldl ~ "**LDL (mg/dL)**",
    pcr ~ "**Proteína C Reactiva (mg/L)**",
    transferrina ~ "**Transferrina (mg/dL)**",
    trigliceridos ~ "**Triglicéridos (mg/dL)**",
    cpk ~ "**CPK (U/L)**"
  )


#=== ESTA QUEDA Y REEMPLAZA A LA ANTERIOR (MENSAJE DE ERROR
#Error in modify_header(): ! Error processing dots argument. ! Can't select columns that don't exist. ✖ Column glucosa doesn't exist. ℹ Select among columns "variable", "var_type", "row_type", "var_label", "label", and "stat_0")

tabla_C <- dataset %>%
  select(glucosa, leucocitos, linfocitos, neutrofilos, chol, hdl, hierro, igA, igE, igG, 
         igN, ldl, pcr, transferrina, trigliceridos, cpk) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_continuous() ~ "continuous"
  ) %>%
  modify_header(label ~ "**Parámetros Bioquímicos**") %>%
  modify_header(
    label ~ "**Variable**",
    stat_0 ~ "**Valores Bioquímicos**"
  ) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        label = case_when(
          label == "glucosa" ~ "Glucosa (mg/dL)",
          label == "leucocitos" ~ "Leucocitos (10³/µL)",
          label == "linfocitos" ~ "Linfocitos (%)",
          label == "neutrofilos" ~ "Neutrófilos (%)",
          label == "chol" ~ "Colesterol Total (mg/dL)",
          label == "hdl" ~ "HDL (mg/dL)",
          label == "hierro" ~ "Hierro (µg/dL)",
          label == "igA" ~ "IgA (mg/dL)",
          label == "igE" ~ "IgE (UI/mL)",
          label == "igG" ~ "IgG (mg/dL)",
          label == "igN" ~ "IgM (mg/dL)",
          label == "ldl" ~ "LDL (mg/dL)",
          label == "pcr" ~ "Proteína C Reactiva (mg/L)",
          label == "transferrina" ~ "Transferrina (mg/dL)",
          label == "trigliceridos" ~ "Triglicéridos (mg/dL)",
          label == "cpk" ~ "CPK (U/L)",
          TRUE ~ label
        )
      )
  )

#===



# Tabla D: Expresión de los 46 genes (notación científica) ERRORES PORQUE NO APARECE NOTACION CIENTIFICA

tabla_D <- dataset %>%
  select(starts_with("AQ_")) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    type = all_continuous() ~ "continuous",
    digits = all_continuous() ~ function(x) format(x, digits = 2, scientific = TRUE)
    ) %>%
    modify_header(label ~ "**Expresión Génica**")

# Notación científica

# Exportar todas las tablas a Word
save_as_docx(
  flextable(tabla_A), path = "Tabla_A.docx"
)

save_as_docx(
  flextable(tabla_B), path = "Tabla_B.docx"
)

save_as_docx(
  flextable(tabla_C), path = "Tabla_C.docx"
)

save_as_docx(
  flextable(tabla_D), path = "Tabla_D.docx"
)



# 3a. Generar descriptivos en función de tratamiento y tipo de tumor (Tabla 2)
library(gtsummary)
library(dplyr)


# Generar la tabla descriptiva solo para los genes en función del tratamiento y tipo de tumor
tabla_2 <- dataset %>%
  select(trat, tumor, starts_with("AQ_")) %>%
  tbl_strata(
    strata = trat,  # Estratificación por tipo de tumor
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = tumor,  # Comparación entre tratA y tratB dentro de cada tipo de tumor
        statistic = all_continuous() ~ "{mean} ({sd})",  
        type = all_continuous() ~ "continuous",
        digits = all_continuous() ~ 2
      ) %>%
      add_p(test = all_continuous() ~ "kruskal.test", 
            pvalue_fun = ~ style_pvalue(.x))
  )

# Mostrar la tabla
tabla_2

#PARA NOTACIÓN CIENTÍFICA. ESTA QUEDARÁ
tabla_2 <- dataset %>%
  select(trat, tumor, starts_with("AQ_")) %>%
  tbl_strata(
    strata = trat,  # Estratificación por tipo de tumor
    .tbl_fun = ~ .x %>%
      tbl_summary(
        by = tumor,  # Comparación entre tratA y tratB dentro de cada tipo de tumor
        statistic = all_continuous() ~ "{mean} ({sd})",  
        type = all_continuous() ~ "continuous",
        digits = all_continuous() ~ function(x) format(x, digits = 2, scientific = TRUE)
      ) %>%
      add_p(test = all_continuous() ~ "kruskal.test", 
            pvalue_fun = ~ style_pvalue(.x)) %>%
      modify_header(
        list(
          label ~ "**Variable**",  # Cambia "Characteristic" por "Gen"
                    p.value ~ "**Valor p**"  # Cambia "p-value" por "Valor p"
        )
      )
  )
tabla_2
#EXPORTAR A WORD (excede los márgenes de la hoja en word)
tabla_2 %>%
  as_flex_table() %>%
  #autofit() %>%
  #set_table_properties(width = 1, layout = "autofit")  # Ajusta la tabla al ancho
  save_as_docx(path = "Tabla_2.docx")


#a ver ahora...estaba saliendo error de duplicados **valor p**

# Convertir tabla_2 a tibble antes de exportarla
tabla_2_tibble <- as_tibble(tabla_2) %>%   # crea a formato compatible
  rename_with(~ make.names(.), everything())  # Asegura nombres únicos


# Crear la tabla en flextable con ajuste automático
tabla_2_word <- tabla_2_tibble %>%
  flextable() %>%
  autofit() %>%
  set_table_properties(width = 1, layout = "autofit")  # Ajustar al ancho de la página

# Exportar la tabla a Word
save_as_docx(tabla_2_word, path = "Tabla_2.docx")


# 4. Generar descriptivos de genes en función de la edad categorizada (Tabla 3)

dataset_plus <- dataset %>%
  mutate(Edad_cat = cut(
    edad,  # Solo pasamos la columna numérica 'edad'
    breaks = quantile(edad, probs = c(0, 0.5, 1), na.rm = TRUE),  # Calculamos los cuantiles
    labels = c("Categoría 1", "Categoría 2"),  # Etiquetas para las categorías
    include.lowest = TRUE  # Asegura que el primer intervalo incluya el valor mínimo
  ))


tabla_3 <- dataset_plus %>%
  select(Edad_cat, starts_with("AQ_")) %>%
  tbl_summary(by = Edad_cat,
              statistic = all_continuous() ~ "{mean} ({sd})",
              type = all_continuous() ~ "continuous",
              digits = all_continuous() ~ function(x) format(x, digits = 2, scientific = TRUE)) %>%
  add_p(test = all_continuous() ~ "kruskal.test", pvalue_fun = ~ style_pvalue(.x))  %>%
    modify_header(
      list(
        label ~ "**Variable**",  # Cambia "Characteristic" por "Gen"
        p.value ~ "**Valor p**"  # Cambia "p-value" por "Valor p"
      )
    )
  
tabla_3
