# cargar librerías
library(readxl)
library(tidyverse)
library(datos)
library(patchwork)
library(cowplot)


# ruta
ruta_excel <- "C:\\Users\\XMF1388_1\\Downloads\\rentas.xlsx"


# visualizar hojas
excel_sheets(ruta_excel)


# encabezados
meses <- c(NA,'enero','febrero','marzo','abril','mayo','junio','julio',
           'agosto','septiembre','octubre','noviembre','diciembre')

# registros
regPob <- c('Total Gral','Nacionales','Extranjeros','Estadounidenses')


# cargar hojas como DF
df18 <- read_xlsx(ruta_excel,sheet = '2018',col_names = meses,skip = 1)
df19 <- read_xlsx(ruta_excel,sheet = '2019',col_names = meses,skip = 1)
df20 <- read_xlsx(ruta_excel,sheet = '2020',col_names = meses,skip = 1)
df21 <- read_xlsx(ruta_excel,sheet = '2021',col_names = meses,skip = 1)
df22 <- read_xlsx(ruta_excel,sheet = '2022',col_names = meses,skip = 1)
df23 <- read_xlsx(ruta_excel,sheet = '2023',col_names = c(NA,'enero','febrero',
        'marzo','abril','mayo','junio','julio','agosto','septiembre'),skip = 1)


# eliminar columna de nombres
df18 <- df18%>% select(-1)
df19 <- df19%>% select(-1)
df20 <- df20%>% select(-1)
df21 <- df21%>% select(-1)
df22 <- df22%>% select(-1)
df23 <- df23%>% select(-1)


# nombrar registros
row.names(df18) <- regPob
row.names(df19) <- regPob
row.names(df20) <- regPob
row.names(df21) <- regPob
row.names(df22) <- regPob
row.names(df23) <- regPob


# Crea Dataframes
df18 <- data.frame(t(df18))
df19 <- data.frame(t(df19))
df20 <- data.frame(t(df20))
df21 <- data.frame(t(df21))
df22 <- data.frame(t(df22))
df23 <- data.frame(t(df23))


# porcentaje de extranjeros frente a locales
df18 <- cbind(df18,extVSloc = df18$Nacionales/df18$Extranjeros)
df19 <- cbind(df19,extVSloc = df19$Nacionales/df19$Extranjeros)
df20 <- cbind(df20,extVSloc = df20$Nacionales/df20$Extranjeros)
df21 <- cbind(df21,extVSloc = df21$Nacionales/df21$Extranjeros)
df22 <- cbind(df22,extVSloc = df22$Nacionales/df22$Extranjeros)
df23 <- cbind(df23,extVSloc = df23$Nacionales/df23$Extranjeros)


# visualizaciones
mesViz <- c('ene','feb','mzo','abr','may','jun','jul',
           'ago','sep','oct','nov','dic')

# g1 <- ggplot(df18,mapping=aes(x=mesViz,y=df18$extVSloc)) +
#   scale_x_discrete(limits = mesViz) + geom_point() + theme_cowplot(8) +
#   labs(title = '2018') + ylab('Locales VS Extranjeros') +
#   theme(axis.title.x = element_blank())
#
# g2 <- ggplot(df19,mapping=aes(x=mesViz,y=df19$extVSloc)) +
#   scale_x_discrete(limits = mesViz) + geom_point() + theme_cowplot(8) +
#   labs(title = '2019') + theme(axis.title = element_blank())
#
# g3 <- ggplot(df20,mapping=aes(x=mesViz,y=df20$extVSloc)) +
#   scale_x_discrete(limits = mesViz) + geom_point() + theme_cowplot(8) +
#   labs(title = '2020') + ylab('Locales VS Extranjeros') +
#   theme(axis.title.x = element_blank())
#
# g4 <- ggplot(df21,mapping=aes(x=mesViz,y=df21$extVSloc)) +
#   scale_x_discrete(limits = mesViz) + geom_point() + theme_cowplot(8) +
#   labs(title = '2021') + theme(axis.title = element_blank())
#
# g5 <- ggplot(df22,mapping=aes(x=mesViz,y=df22$extVSloc)) +
#   scale_x_discrete(limits = mesViz) + geom_point() + theme_cowplot(8) +
#   labs(title = '2022') + ylab('Locales VS Extranjeros') +
#   theme(axis.title.x = element_blank())
#
# g6 <- ggplot(df23,mapping=aes(x=mesViz[1:9],y=df23$extVSloc)) +
#   scale_x_discrete(limits = mesViz[1:9]) + geom_point() + theme_cowplot(8) +
#   labs(title = '2023') + theme(axis.title = element_blank())

 # g1 + g2 + g3 + g4 + g5 + g6 + plot_layout(ncol = 2, nrow = 3)


# Crear dataset por mes
ulMes <- c(df23$Extranjeros, 0, 0, 0)
ulMes
extMes <- data.frame('2018' = df18$Extranjeros,'2019' = df19$Extranjeros,
                     '2020' = df20$Extranjeros,'2021' = df21$Extranjeros,
                     '2022' = df22$Extranjeros,'2023' = ulMes, row.names = mesViz)

prom <- list()

for (i in 1:9){
  prom[i] <- as.integer(mean(as.numeric(extMes[i,])))
}
for (j in 10:12){
  prom[j] <- as.integer(mean(as.numeric(extMes[j,1:5])))
}
prom <- matrix(prom)

extMes <- cbind(extMes,Promedio=prom)


# DISTRIBUCIÓN DE POISSON
pP <- list()
for (i in 1:12){
  l <- as.integer(extMes[i,7])
  pP[i] <- ppois(l + 1,lambda = l, lower.tail = FALSE)
}

pP <- matrix(pP)

plot(x = pP, type = 'h', main = 'Probabilidad de que ingresen más extranjeros en los meses del 2024',xlab = 'mes', ylab = 'P(X = x)',las = 1,xaxt='n')
axis(1, at = 1:12,labels = mesViz)
