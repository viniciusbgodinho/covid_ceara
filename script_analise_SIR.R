con <- gzcon(url(paste("https://data.brasil.io/dataset/covid19/caso_full.csv.gz", sep=",")))
txt <- readLines(con)
dados <- read.csv(textConnection(txt))


library("dplyr")
library("tidyr")
library("lubridate")
library("ggplot2")
require('tikzDevice')
library("devtools")
library(plotly)
library("flexdashboard")
library("sf")  
library("tmap")
library("tmaptools")
library("rgdal")
library("leaflet")
library('easyGgplot2')


colnames(dados)[8] <-'confirmed'
colnames(dados)[12] <-'deaths'
colnames(dados)[9] <- 'confirmed_per_100k'




###Brasil


dados_ce <- dados %>%
  filter(place_type=="city", state =="CE") %>%
  arrange(date) %>%
  mutate(round(deaths/confirmed*100,digits = 2)) 

names(dados_ce)

colnames(dados_ce)[18] <- "tx_letalidade"

dados_ce$date <- as.Date(dados_ce$date)


##Estado


df_ce <- dados%>%
  filter(place_type=="state", state =="CE") %>%
               arrange(date) 
  

df_ce <- df_ce%>%
  mutate(ma7_confirmed = stats::filter(df_ce$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(df_ce$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  mutate(tx_letalidade = deaths/confirmed*100)



##Fortaleza 


dados_fortal <- dados %>% 
  filter( city_ibge_code == "2304400") %>%
  arrange(date)



dados_fortal <- dados_fortal%>%
  mutate(ma7_confirmed = stats::filter(dados_fortal$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_fortal$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  select(date,confirmed,deaths,new_confirmed,new_deaths,ma7_confirmed,ma7_deaths,city)



df_fortal <- dados_fortal %>%
            filter(deaths>="5")

##Caucaia

dados_caucaia <- dados %>% 
  filter( city_ibge_code == "2303709") %>%
  arrange(date)



dados_caucaia <- dados_caucaia%>%
  mutate(ma7_confirmed = stats::filter(dados_caucaia$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_caucaia$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  select(date,confirmed,deaths,new_confirmed,new_deaths,ma7_confirmed,ma7_deaths,city)



df_caucaia <- dados_caucaia %>%
  filter(deaths>="5")


##Maracanau

dados_maracanau <- dados %>% 
  filter( city_ibge_code == "2307650") %>%
  arrange(date)



dados_maracanau <- dados_maracanau%>%
  mutate(ma7_confirmed = stats::filter(dados_maracanau$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_maracanau$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  select(date,confirmed,deaths,new_confirmed,new_deaths,ma7_confirmed,ma7_deaths,city)



df_maracanau <- dados_maracanau %>%
  filter(deaths>="5")

##Maranguape

dados_maranguape <- dados %>% 
  filter( city_ibge_code == "2307700") %>%
  arrange(date)



dados_maranguape <- dados_maranguape%>%
  mutate(ma7_confirmed = stats::filter(dados_maranguape$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_maranguape$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  select(date,confirmed,deaths,new_confirmed,new_deaths,ma7_confirmed,ma7_deaths,city)



df_maranguape <- dados_maranguape %>%
  filter(deaths>="5")


##Sobral

dados_sobral <- dados %>% 
  filter( city_ibge_code == "2312908") %>%
  arrange(date)



dados_sobral <- dados_sobral%>%
  mutate(ma7_confirmed = stats::filter(dados_sobral$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_sobral$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  select(date,confirmed,deaths,new_confirmed,new_deaths,ma7_confirmed,ma7_deaths,city)



df_sobral <- dados_sobral %>%
  filter(deaths>="5")

##Cariri Barbalha, Crato e Juazeiro do Norte

dados_cariri <- dados %>% 
  filter( city_ibge_code == "2301901" |city_ibge_code == "2304202"|city_ibge_code == "2307304") %>%
  arrange(date)


dados_cariri <- group_by(dados_cariri,date) %>%
  summarise(confirmed=sum(confirmed), deaths=sum(deaths),new_confirmed=sum(new_confirmed), 
            new_deaths=sum(new_deaths))


dados_cariri <- dados_cariri%>%
  mutate(ma7_confirmed = stats::filter(dados_cariri$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_cariri$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  mutate(city="Cariri")


df_cariri <- dados_cariri %>%
  filter(deaths>="5")

##Juazeiro

dados_juazeiro <- dados %>% 
  filter( city_ibge_code == "2307304") %>%
  arrange(date)




dados_juazeiro <- dados_juazeiro%>%
  mutate(ma7_confirmed = stats::filter(dados_juazeiro$new_confirmed, filter=rep(1/7, 7), 
                                       method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_deaths = stats::filter(dados_juazeiro$new_deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  select(date,confirmed,deaths,new_confirmed,new_deaths,ma7_confirmed,ma7_deaths,city)





dados_maracanau$city <- "Maracanau"


df_total <-rbind.data.frame(dados_fortal,dados_cariri,dados_caucaia,dados_maracanau,dados_sobral)

df_total$date <- as.Date(df_total$date)



df_cidades <- rbind.data.frame(dados_cariri,dados_caucaia,dados_maracanau,dados_sobral)



df_cidades$date <- as.Date(df_cidades$date)





g1 <- ggplot(df_total, aes(date, confirmed, colour = city)) +geom_line(size=.8)  +
  xlab("Data") +
  ylab("") +
  scale_colour_brewer(palette="Dark2")+
  scale_x_date(date_labels = "%d/%m")+
  ggtitle ("", subtitle = "Casos Acumulados") +
  theme(text = element_text(size=8) ,axis.text = element_text(size=5) ,
        axis.text.x = element_text(size=5, angle = 45), 
        axis.title.x = element_text(size=8), legend.text = element_text(size=5,angle=45) , 
        legend.position = "bottom",legend.title = element_blank(),legend.key =element_blank(),
        panel.background = element_blank(), panel.grid.minor = element_line(colour = "gray90"), 
        panel.grid.major = element_line(colour = "gray90"))



g2 <- ggplot(df_total, aes(date, deaths, colour = city)) +geom_line(size=.8)  +
  xlab("Data") +
  ylab("") +
  scale_colour_brewer(palette="Dark2")+
  scale_x_date(date_labels = "%d/%m")+
  ggtitle ("", subtitle = "{\\'O}bitos Acumulados") +
  theme(text = element_text(size=8) ,axis.text = element_text(size=5) ,axis.text.x = element_text(size=5, angle = 45), axis.title.x = element_text(size=8), legend.text = element_text(size=5,angle=45) , legend.position = "bottom",legend.title = element_blank(), legend.key =element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "gray90"), panel.grid.major = element_line(colour = "gray90"))


setwd("E:\\Covid-19\\rede analise")

tikz(file = "tikz_cidades_ce_total.tex", width = 7, height = 2.5) 

ggplot2.multiplot(g1,g2, cols = 2)

dev.off()





###Fortaleza


dados_fortal$date <- as.Date(dados_fortal$date)

g1_fortal <- ggplot(dados_fortal, aes(date, ma7_confirmed)) +geom_line(size=.8, color="darkblue")  +
  xlab("Data") +
  ylab("") +
  scale_colour_brewer(palette="Dark2")+
  scale_x_date(date_labels = "%d/%m")+
  ggtitle ("", subtitle = "M{\\'e}dia M{\\'o}vel 7 dias: Novos Casos") +
  theme(text = element_text(size=8) ,axis.text = element_text(size=5) ,axis.text.x = element_text(size=5, angle = 45), axis.title.x = element_text(size=8), legend.text = element_text(size=5,angle=45) , legend.position = "bottom",legend.title = element_blank(), legend.key =element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "gray90"), panel.grid.major = element_line(colour = "gray90"))



g2_fortal <- ggplot(dados_fortal, aes(date, ma7_deaths)) +geom_line(size=.8, color="darkred")  +
  xlab("Data") +
  ylab("") +
  scale_colour_brewer(palette="Dark2")+
  scale_x_date(date_labels = "%d/%m")+
  ggtitle ("", subtitle = "M{\\'e}dia M{\\'o}vel 7 dias: Novos {\\'O}bitos") +
  theme(text = element_text(size=8) ,axis.text = element_text(size=5) ,
        axis.text.x = element_text(size=5, angle = 45), axis.title.x = element_text(size=8), 
        legend.text = element_text(size=5,angle=45) , legend.position = "bottom",
        legend.title = element_blank(), legend.key =element_blank(), 
        panel.background = element_blank(), panel.grid.minor = element_line(colour = "gray90"), 
        panel.grid.major = element_line(colour = "gray90"))

tikz(file = "tikz_fortal.tex", width = 6.5, height = 2.5) 

g2_fortal + geom_vline(xintercept = as.Date(c("2020-05-08","2020-06-01")) , linetype=4) +
  annotate(geom="text", label= "Isolamento Rigido",x=as.Date("2020-05-20"),y=10, size=2)


dev.off()

dados_fortal$date[54]
dados_fortal$date[78]
dados_fortal$date[85]
dados_fortal$date[99]





g3 <- ggplot(df_cidades, aes(date, ma7_confirmed, colour = city)) +geom_line(size=.8)  +
  xlab("Data") +
  ylab("") +
  scale_colour_brewer(palette="Dark2")+
  scale_x_date(date_labels = "%d/%m")+
  ggtitle ("", subtitle = "M{\\'e}dia M{\\'o}vel 7 dias: Novos Casos") +
  theme(text = element_text(size=8) ,axis.text = element_text(size=5) ,axis.text.x = element_text(size=5, angle = 45), axis.title.x = element_text(size=8), legend.text = element_text(size=5,angle=45) , legend.position = "bottom",legend.title = element_blank(), legend.key =element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "gray90"), panel.grid.major = element_line(colour = "gray90"))



g4 <- ggplot(df_cidades, aes(date, ma7_deaths, colour = city)) +geom_line(size=.8)  +
  xlab("Data") +
  ylab("") +
  scale_colour_brewer(palette="Dark2")+
  scale_x_date(date_labels = "%d/%m")+
  ggtitle ("", subtitle = "M{\\'e}dia M{\\'o}vel 7 dias: Novos {\\'O}bitos") +
  theme(text = element_text(size=8) ,axis.text = element_text(size=5) ,axis.text.x = element_text(size=5, angle = 45), axis.title.x = element_text(size=8), legend.text = element_text(size=5,angle=45) , legend.position = "bottom",legend.title = element_blank(), legend.key =element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "gray90"), panel.grid.major = element_line(colour = "gray90"))


tikz(file = "tikz_cidades_ce.tex", width = 7, height = 2.5) 

ggplot2.multiplot(g3,g4, cols = 2)

dev.off()




####Modelo SIR

library('deSolve')

B <- 0.88*(1/5.2)

N_fortal <- 2645000

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -(beta * I * S)/N_fortal
    dI <-  (beta * I * S)/N_fortal - (gamma * I)
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}




parameters_values <- c(
  beta  = 0.81*(1/5.2), # infectious contact rate (/person/day)
  gamma = 1/5.2    # recovery rate (/day)
)



initial_values <- c(
  S = 2645000 - last(dados_fortal$confirmed),  # number of susceptibles at time = 0
  I =   last(dados_fortal$confirmed) - 28614,  # number of infectious at time = 0
  R =   28614   # number of recovered (and immune) at time = 0
)


time_values <- seq(0, 10) # days

sir_values_1 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)


sir_values_1$time <- round(sir_values_1$time)
sir_values_1$S <- round(sir_values_1$S)
sir_values_1$I <- round(sir_values_1$I)
sir_values_1$R <- round(sir_values_1$R)


sir_values_1 <- as.data.frame(sir_values_1)
sir_values_1



require('xtable')
xtable(sir_values_1)

tikz(file = "tikz_fortal_sir.tex", width = 6.5, height = 2.5) 
ts.plot(sir_values_1$I)
dev.off()




plot(sir_values_1$I)

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")





##cariri

B <- 0.88*(1/5.2)

N_cariri <- 255648+93469

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -(beta * I * S)/N_cariri
    dI <-  (beta * I * S)/N_cariri - (gamma * I)
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}



parameters_values <- c(
  beta  = 0.95*(1/5.2), # infectious contact rate (/person/day)
  gamma = 1/5.2    # recovery rate (/day)
)



initial_values <- c(
  S = N_cariri - last(dados_cariri$confirmed),  # number of susceptibles at time = 0
  I =   last(dados_cariri$confirmed) - (3601+1140),  # number of infectious at time = 0
  R =   (3601+1140)   # number of recovered (and immune) at time = 0
)


time_values <- seq(0, 10) # days

sir_values_2 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)


sir_values_2 <- as.data.frame(sir_values_2)
sir_values_1


xtable(sir_values_1)

tikz(file = "tikz_cariri_sir.tex", width = 6.5, height = 2.5) 
ts.plot(sir_values_1$I)
dev.off()




ts.plot(sir_values_1$I)



##Sobral

B <- 0.98*(1/5.2)

N_sobral <- 147353

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -(beta * I * S)/N_sobral
    dI <-  (beta * I * S)/N_sobral - (gamma * I)
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}



parameters_values <- c(
  beta  = 0.98*(1/5.2), # infectious contact rate (/person/day)
  gamma = 1/5.2    # recovery rate (/day)
)



initial_values <- c(
  S = N_sobral - last(dados_sobral$confirmed),  # number of susceptibles at time = 0
  I =   last(dados_sobral$confirmed) - 7412,  # number of infectious at time = 0
  R =   7412   # number of recovered (and immune) at time = 0
)


time_values <- seq(0, 10) # days

sir_values_3 <- ode(
  y = initial_values,
  times = time_values,
  func = sir_equations,
  parms = parameters_values 
)



sir_values_3 <- as.data.frame(sir_values_3)
sir_values_1

str(sir_values_1)


sir_values_1$time
sir_values_1$I
sir_values_2$I
sir_values_3$I


sir_values_1$time <- as.Date(sir_values_1$time, origin=("2020-07-14"))


prev_sir <- data.frame(casos=c(rep("Fortaleza",11),rep("Cariri",11),rep("Sobral",11)), 
                       data = rep(sir_values_1$time,3), 
                       total = c(sir_values_1$I,sir_values_2$I,sir_values_3$I))



str(prev_sir)



tikz(file = "tikz_sir.tex", width = 6.5, height = 2.5) 

ggplot(prev_sir, aes(data, total, colour = casos)) +geom_line(size=.8)  +
  xlab("Data") +
  ylab("") +
  scale_colour_brewer(palette="Dark2")+
  scale_x_date(date_labels = "%d/%m")+
  theme(text = element_text(size=8) ,axis.text = element_text(size=5) ,axis.text.x = element_text(size=5, angle = 45), axis.title.x = element_text(size=8), legend.text = element_text(size=5,angle=45) , legend.position = "bottom",legend.title = element_blank(), legend.key =element_blank(), panel.background = element_blank(), panel.grid.minor = element_line(colour = "gray90"), panel.grid.major = element_line(colour = "gray90"))

dev.off()




ts.plot(sir_values_1$I)

with(sir_values_1, {
  # plotting the time series of susceptibles:
  plot(time, S, type = "l", col = "blue",
       xlab = "time (days)", ylab = "number of people")
  # adding the time series of infectious:
  lines(time, I, col = "red")
  # adding the time series of recovered:
  lines(time, R, col = "green")
})

# adding a legend:
legend("right", c("susceptibles", "infectious", "recovered"),
       col = c("blue", "red", "green"), lty = 1, bty = "n")


























#############



dados_mg <- dados %>%
  filter(place_type=="state", state=="MG") 


dados_mg <- dados_mg %>% 
  mutate(ma7_deaths = stats::filter(dados_mg$deaths, filter=rep(1/7, 7), 
                                    method="convolution", sides=2, circular=F)) %>%
  mutate(ma7_new_deaths = stats::filter(dados_mg$new_deaths, filter=rep(1/7, 7), 
                                        method="convolution", sides=2, circular=F))



dados_mg <- dados_mg %>%
  filter(date>="2020-06-18" & date <= "2020-08-07")



x <- c(1:23)
y <- dados_mg$ma7_deaths[1:23]
y <- as.numeric(x)





teste <- cbind.data.frame(y,x)


Model<-lm(y ~ x + I(x^2), data = teste)
print(Model)
