df <- read.csv("./Desktop/Master - Data Science/UOC/2 Semestre/Visualización de Datos/PECs/data-visualization/PEC4/breaches.csv", header=TRUE, encoding = "UTF-8", )
df <- df[-c(1), ] 
#Mirando los nombres de columnas del dataframe y despues los tipos de variables estadisticas
colnames(df)

#Mantener simplemente el dataset que nos interesa
keeps <- c("Entity", "records.lost", "YEAR", "SECTOR", "METHOD", "DATA.SENSITIVITY")
df <- df[keeps] 

colnames(df)


unlist(lapply(df, function(x) any(is.na(x))))
summary(df)

df$sensitivity <- as.factor(
  ifelse(df$DATA.SENSITIVITY == 1, "Just email address/Online information", 
  ifelse(df$DATA.SENSITIVITY == 2, "SSN/Personal details",
  ifelse(df$DATA.SENSITIVITY == 3, "Credit card information", 
  ifelse(df$DATA.SENSITIVITY == 4, "Health & other personal records", 
  ifelse(df$DATA.SENSITIVITY == 5, "Full details", "None"))))))


write.csv(df, file = "./Desktop/Master - Data Science/UOC/2 Semestre/Visualización de Datos/PECs/data-visualization/PEC4/prepared-data-breaches.csv",row.names=FALSE)


library(rjson)
x <- toJSON(unname(split(df, 1:nrow(df))))
cat(x)
library(jsonlite)
write_json(x, path="./Desktop/Master - Data Science/UOC/2 Semestre/Visualización de Datos/PECs/data-visualization/PEC4/prepared-data-breaches.json")


df_filtered <- df[c("METHOD", "sensitivity")]
df_filtered
df_filtered$METHOD <- as.factor(trimws(df_filtered$METHOD))
library(plyr)
df_final <- count(df_filtered, c('METHOD','sensitivity'))
x <- toJSON(unname(split(df_final, 1:nrow(df_final))))
write_json(x, path="./Desktop/Master - Data Science/UOC/2 Semestre/Visualización de Datos/PECs/data-visualization/PEC4/prepared-data-breaches.json")

summary(df_final)

df_final$METHOD <- trimws(df_final$METHOD)
df_final

df_export <- ddply(df_filtered, c("METHOD"), summarise, 
                 
                  "Just email address/Online information"=sum(sensitivity!="Just email address/Online information")
                 , "SSN/Personal details"=sum(sensitivity=="SSN/Personal details")
                 , "Credit card information"=sum(sensitivity=="Credit card information")
                 , "Health & other personal records"=sum(sensitivity=="Health & other personal records")
                 , "Full details"=sum(sensitivity=="Full details")
                 )

colnames(df_final) <- c("method", "sensitivity", "value")



df_final$sensitivity <- as.factor(
  ifelse(df_final$sensitivity == "Just email address/Online information", 1, 
  ifelse(df_final$sensitivity == "SSN/Personal details", 2,
  ifelse(df_final$sensitivity == "Credit card information", 3, 
  ifelse(df_final$sensitivity == "Health & other personal records", 4, 
  ifelse(df_final$sensitivity == "Full details", 5, 0))))))


df_final$method <- as.factor(
  ifelse(df_final$method == "hacked", 1, 
  ifelse(df_final$method == "inside job", 2,
  ifelse(df_final$method == "lost device", 3, 
  ifelse(df_final$method == "oops!", 4, 
  ifelse(df_final$method == "poor security", 5, 0))))))





write.csv(df_final, file = "./Desktop/Master - Data Science/UOC/2 Semestre/Visualización de Datos/PECs/data-visualization/PEC4/prepared-data-breaches.csv",row.names=FALSE)


df_lixo <- t(df_final[-1])

reshape(data.frame(ID,Score,Attribute),idvar='METHOD',timevar='Sensi',dir='w')

