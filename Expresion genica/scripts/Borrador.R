##Se puede realizar un análisis simple de las diferencias de los parámetros bioquímicos y algunos genes importantes para rutas metabolicas y de crecimiento celular
#se puede sugerir borrar el script para que este sea definitivo

library("ggplot2") #Graficos 
library("patchwork") #Multiplots
library("grid") #multiplots 
library("ComplexHeatmap") #heatmaps
library("pheatmap") #heatmaps

data<-read.csv(file.choose(TRUE))  #Podría tambien añadirse el path, pero file deja elegir por ventana

#Primer ejercicio 
#Genes solicitados 
genes <- c("AQ_ALOX5", "AQ_CD274", "AQ_CHKA", "AQ_CSF2", "AQ_FOXO3", "AQ_IL6", "AQ_LDHA", "AQ_LIF", "AQ_MAPK1", "AQ_NOS2", "AQ_IFNG", "AQ_PDCD1", "AQ_PPARG", "AQ_TGFB1", "AQ_TNF")

#Se crea una lista para guadar los multiples plots que se generan en el loop
lista_plots <- list()

#Loop para generar un plot por gen
for (gen in genes) {
  
  p <- ggplot(data, aes(x = trat, y = .data[[gen]])) +
    geom_boxplot(fill = c("lightblue", "lightgreen"), color = "black") +
    labs(title= gen, x = "", y = "") +
    theme_minimal()+
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          plot.title = element_text(size = 8, hjust = 0.5))
  
  lista_plots[[gen]] <- p #cada plot se ingresa a la lista 
  
}


plot_combinado <- wrap_plots(lista_plots, ncol = 5, scales = "free_y") + 
  plot_annotation(title = "Niveles de expresión génica por tratamiento",
                  subtitle =  "Comparación de la expresión de genes relacionados con la respuesta inmune, metabolismo y ciclo celular
acorde al tratameinto empleado",
                  theme = theme(plot.title = element_text(size = 14, hjust = 0.5),
                                plot.subtitle = element_text(size = 10, hjust = 0.5)))

plot_combinado #se debe añadir titulos de ejes
grid.text("Tratamiento", y = unit(0.02, "npc"), gp = gpar(fontsize = 12))
grid.text("Expresión génica", x = unit(0.02, "npc"), rot = 90, gp = gpar(fontsize =12))  


bioquimicos <- c("glucosa", "leucocitos", "linfocitos", "neutrofilos", "chol", "hdl", "hierro", "igA", "igE", "igG", "igN", "ldl", "pcr", "transferrina", "trigliceridos", "cpk")

lista_histos <- list()

# Loop sobre cada variable bioquímica
for (var in bioquimicos) {
  
  p <- ggplot(data, aes(x = .data[[var]])) +
    geom_histogram( aes(y=after_stat(density)), bins = 35,
                    fill = "cadetblue1", color = "cadetblue4") +
    theme_minimal()+
    
    geom_density(color = "red", linewidth = 1) +  #linea para mostrar la distribución
    
    labs(title= var, x = "", y = "") +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          axis.text = element_text(size = 8)) 
  
  lista_histos[[var]] <- p
}

# Combinar en una grilla. Lo Haré dos para que no queden taaaaan apretadas 
histogramas1 <- wrap_plots(lista_histos[1:8], ncol = 4) + 
  plot_annotation(title = "Distribución de variables bioquímicas 1",
                  subtitle = "Histogramas de distribución de las variables bioquimicas analizadas, comparados con el modelo de distribución normal",
                  theme = theme(plot.title = element_text(size = 14, hjust = 0.5),
                                plot.subtitle = element_text(size = 10, hjust = 0.5)))

histogramas2 <- wrap_plots(lista_histos[9:16], ncol = 4) + 
  plot_annotation(title = "Distribución de variables bioquímicas 2",
                  subtitle = "Histogramas de distribución de las variables bioquimicas analizadas, comparados con el modelo de distribución normal",
                  theme = theme(plot.title = element_text(size = 14, hjust = 0.5),
                                plot.subtitle = element_text(size = 10, hjust = 0.5)))
# Mostrar los histogramas
print(histogramas1)
print(histogramas2)

genes <- grep("^AQ_", colnames(data), value = TRUE)  #seleccionar las columnas de los genes (empiezan con AQ_)
expression_matrix <- as.matrix(data[, genes])  
rownames(expression_matrix) <- data$id  

expresion_matrix_sc <- scale(expression_matrix)  #SCALE PARA PODER COMPARAR

set.seed(1995)  #Semilla del enunciado 

heat <- Heatmap(
  t(expresion_matrix_sc),  
  name = "Nivel de Expresión",  
  show_row_names = TRUE, 
  show_column_names = TRUE, 
  row_km = 3,               
  column_km = 3, 
  cluster_rows = TRUE,  
  cluster_columns = TRUE, 
  row_names_gp = gpar(fontsize = 6),  
  column_names_gp = gpar(fontsize = 7), 
  col = circlize::colorRamp2(c(-4, 0, 4), c("yellow", "white", "red"))
  
)

draw(heat,
    column_title = "Heatmap de la expresión génica normalizada en los pacientes", column_title_gp = gpar(fontsize = 16))
