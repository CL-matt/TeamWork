cfw
library(clickR)
#Arreglar los datos
datos <- cfw
datos <- fix_factors(datos, k=15)
datos <- fix_numerics(datos)
datos <- nice_names(datos)
table(datos$region)
descriptive(datos)
mean(datos$population[datos$region %in% "oceania"])


datos$region_agrupada <- factor(ifelse(datos$region %in% c("asia (ex. near east)", "near east", "c.w. of ind. states"), "asia",
                                ifelse(datos$region %in% c("western europe", "baltics", "eastern europe"), "europe",
                                       ifelse(datos$region %in% c("latin amer. & carib", "northern america"), "america",
                                              ifelse(datos$region %in% c("northern africa", "sub-saharan africa"), "africa",
                                                     ifelse(datos$region %in% "oceania", "oceania", NA))))))

anova <- aov(population ~ area_sq_mi, data = datos)
summary(anova)

DAdrica <- (datos$arable_percent[datos$region_agrupada %in% "africa"])
Dafrica <- ifelse(is.na(DAdrica),0,DAdrica)
Dafrica
#comparison normalidad
by(datos$arable_percent, datos$region_agrupada, function(x){
  qqnorm(x, pch=16)
  qqline(x)
})

by(datos$crops_percent, datos$region_agrupada, function(x){
  qqnorm(x, pch=16)
  qqline(x)
})

#comparison for crops in continent
names(datos)
#anova2 <- oneway.test(arable_percent ~ region_agrupada, data = datos)
#anova2
anova2 <- kruskal.test(arable_percent ~ region_agrupada, data = datos)
anova2
#anova3 <- oneway.test(crops_percent ~ region_agrupada, data = datos)
anova3 <- kruskal.test(crops_percent ~ region_agrupada, data = datos)
anova3

#datos$country[datos$region_agrupada %in% "c.w. of ind. states"]


mpoceania <- mean(datos$population[datos$region %in% "oceania"])
mpasia <- mean(datos$population[datos$region_agrupada %in% "asia"])
mpafrica <- mean(datos$population[datos$region_agrupada %in% "africa"])
mpeurope <- mean(datos$population[datos$region_agrupada %in% "europe"])
mpamerica <- mean(datos$population[datos$region_agrupada %in% "america"])

