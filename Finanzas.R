#https://finance-r.netlify.app/intro.html

install.packages("gapminder")

library("gapminder")
library("tidyverse")
library(ggplot2)

#obtenemos los datos

world.data <- gapminder

#Organizamos los datos por año, los agrupamos por continentes  y sacamos el promedio
mean.lifeExp <- world.data %>% 
  filter(year == 2007) %>% 
  group_by(continent) %>%
  summarize(mean(lifeExp))



#Filtramos la data por pais
world.data <- world.data %>% 
  filter(country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Paraguay", "Peru", "Uruguay", "Venezuela"))

g1 <- ggplot(world.data) + geom_line(mapping = aes(year,lifeExp, colour= country), size = 1.2)
g1 <- g1 + theme_bw() + labs(title = "Expectativa de vida para America del Sur", subtitle = "Desde 1952 a 2007", colour = "") 
g1 <- g1 + xlab("Años") + ylab("Expectativa de vida")
g1 <- g1 + theme(legend.position="bottom") 
g1

#Descargas de datos de yahoo finance

library(quantmod)

getSymbols("^GSPC", src = "yahoo", from = "2010-01-01", to = "2010-07-30", periodicity = "daily")


#Grafico
chartSeries(GSPC) #, TA=NULL      paraver el volumen


#chartSeries(GSPC, subset = "last 3 months")      subseccion


ggplot(mpg, aes(x = displ, y = hwy)) + geom_point()


ggplot(mpg, aes(displ, cty, colour = class)) +
  geom_point() 




ggplot(mpg, aes(displ, hwy)) + geom_point(aes(colour = "blue"))

ggplot(mpg, aes(displ, hwy)) + geom_point(aes(displ, hwy, colour = class))



#aes(displ, hwy, colour = class)
#aes(displ, hwy, shape = drv)
#aes(displ, hwy, size = cyl)

#transformamos en dataframe
gspc <- as.data.frame(GSPC)
#?as.data.frame


g1 <- ggplot(gspc) + geom_line(mapping = aes(index(gspc),GSPC.Adjusted))
g1 <- g1 + labs(title = "S&P 500", subtitle = "Desde Enero 2010 a 2018") + xlab("Fecha") + ylab("")
g1 <- g1 + theme_bw()
g1



#descarga de multiples tickets
tickers <- c("ORCL","AMD","IBM","NVDA")


getSymbols(tickers, src = "yahoo", from = "2010-01-01", to = "2018-07-30", periodicity = "daily")

list <- lapply(tickers, function(x) Cl(get(x)))
precio.cierre <- do.call(merge,list)






#construccion de retornos logaritmicos
#renombramos variables
#eliminamos NA


retornos <- data.frame(apply(precio.cierre, 2, function(x) Delt(x, type = "log")),
                       fecha = index(precio.cierre)) %>%
  rename(orcl = ORCL.Close, amd = AMD.Close, ibm = IBM.Close, nvda = NVDA.Close) %>% 
  na.omit() 



#RENDIMIENTOS ACUMULADOS
#ALIMINAMOS PRIMER REGLON
acumulados <- data.frame(apply(retornos[1:4], 2, function(x) cumsum(x)), fecha = index(precio.cierre[-1]))

#graficos de retornos acumulados
library("reshape2")


reshape <- melt(acumulados, id.vars = "fecha")

g2 <- ggplot(reshape) + geom_line(mapping = aes(fecha,value, color = variable))
g2 <- g2 + labs(title = "Retornos Acumulados", subtitle = "Oracle, AMD, IBM y Nvidia")
g2 <- g2 + theme_bw() + xlab("Fecha") + ylab("Retornos Acumulados")
g2 <- g2 + scale_color_manual("Tickers", values = c("red","blue","green","orange"))
g2 <- g2 + theme(legend.position = "bottom")
g2



#ESTADISTICA DESCRIPTIVA
library("fBasics")


summary <- basicStats(retornos[1:4])[c("Mean", "Stdev", "Median", "Minimum", "Variance",
                                       "Maximum", "nobs","Skewness","Kurtosis"),]


#portafolio de oracle
SR_orcl <- (mean(retornos$orcl) - 0.0000 )/sd(retornos$orcl)
#normalidad
jarqueberaTest(retornos$orcl)


#IMPORTACION DE DATOS DESDE LA RED
FAANG <- read_csv("https://www.dropbox.com/s/tjqvs9w16al1jl2/FAANG.csv?dl=1")

# retornos 


###NO ME FUNCIONAAAA

FAANG_returns <- FAANG %>% 
  gather(symbol, prices, -date) %>% # en la tarea es -Date y no -date
  group_by(symbol) %>% 
  mutate(returns = log(prices/lag(prices))) %>% 
  filter(!is.na(returns)) 


