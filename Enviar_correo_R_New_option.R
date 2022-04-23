#Automatizacion del informe del registro de DH por Visitaduria
#Cordinacion del SIIGESI
#20-Septiembre-2021

rm(list=ls())
setwd("Z:/Reportes Automatizados/Registro_DERECHOS")
#install.packages("keyring")
#install.packages("blastula")

library(blastula)
library(ggplot2)
library(glue)
library(keyring)
library(tidyverse)
library(dplyr)
library(RODBC)
library(tidyr)
library(dplyr)

Conexion <- odbcConnect("data_source",
                        uid="ciadh",
                        pwd="sBp60000")


df <- sqlQuery(Conexion, 
"DECLARE @Fecha_Ini CHAR(10)			
DECLARE @Fecha_Fin CHAR(10)			
SET  @Fecha_Ini = '2019/07/13'			
SET  @Fecha_Fin = CAST(DATEADD(DAY,-1,GETDATE()) AS DATE)

SELECT DISTINCT v_menciones_derecho.Id_gestion
      ,v_menciones_derecho.Fecha_creacion
      ,v_menciones_derecho.Anio
      ,v_menciones_derecho.Mes
      ,v_menciones_derecho.Id_expediente
      ,v_menciones_derecho.Expediente
      ,v_menciones_derecho.Numero_expediente
      ,v_menciones_derecho.Id_adscripcion
      ,v_menciones_derecho.Adscripcion
      ,v_menciones_derecho.Id_calificacion
      ,v_menciones_derecho.Calificacion
      ,v_menciones_derecho.Id_estatus
      ,v_menciones_derecho.Estatus
      ,v_menciones_derecho.Id_tipo_conclusion
      ,v_menciones_derecho.Tipo_conclusion
      ,v_menciones_derecho.Fecha_conclusion
      ,v_menciones_derecho.Id_Investigador
      ,v_menciones_derecho.Investigador
      ,v_menciones_derecho.Penitenciarias
      ,IIF(v_menciones_derecho.Id_Derecho IS NULL,'NO','SI') AS Con_Derecho		
FROM dbo.v_menciones_derecho
WHERE dbo.v_menciones_derecho.Fecha_creacion >= @Fecha_Ini AND dbo.v_menciones_derecho.Fecha_creacion < @Fecha_Fin			
	AND dbo.v_menciones_derecho.Id_calificacion IN (1,9)" 
)

odbcClose(Conexion)

Registros_Derechos_PV <- df[df$Id_calificacion==1,]

Registros_Derechos <- df[df$Id_calificacion==9,]

Registros_Derechos <- select(Registros_Derechos, c(Id_gestion,
                                                   Fecha_creacion, 
                                                   Anio, 
                                                   Mes, 
                                                   Id_adscripcion, 
                                                   Adscripcion, 
                                                   Id_calificacion, 
                                                   Calificacion, 
                                                   Id_estatus, 
                                                   Estatus, 
                                                   Con_Derecho))



##Grafica- En esta parte creamos los compornentes de la grafica##


Registros_Derechos_graf <- Registros_Derechos %>%
  mutate(Id_Mes=case_when(
    Mes=="Ene"~1,
    Mes=="Feb"~2,
    Mes=="Mar"~3,
    Mes=="Abr"~4,
    Mes=="May"~5,
    Mes=="Jun"~6,
    Mes=="Jul"~7,
    Mes=="Ago"~8,
    Mes=="Sep"~9,
    Mes=="Oct"~10,
    Mes=="Nov"~11,
    Mes=="Dic"~12))



Registros_Derechos_graf <- Registros_Derechos_graf %>% 
  mutate (Visitadurias=factor(Id_adscripcion,
                              levels = c("1", "2", "3", "4", "5"),
                              labels = c("Primera", "Segunda", "Tercera", "Cuarta", "Quinta")))

Registros_Derechos_graf <- Registros_Derechos_graf %>% 
  mutate(Expedientes=recode(Con_Derecho,"NO"="Sin Derecho", "SI"="Con Derecho"))

Registros_Derechos_graf <- Registros_Derechos_graf %>% 
  mutate(Estatus_Expediente=recode(Estatus,"Tramite Indagación"="Tramite","Tramite Investigaci?n"="Tramite", "Acumulado"="Tramite"))


d <- Registros_Derechos_graf %>%
  count(Visitadurias, Anio, Id_Mes, Mes, Expedientes, .drop=FALSE) #Aqui se cuenta el numero de expedientes para cada categoria del registro para cada visitaduria #


f <- d %>% 
  group_by(Visitadurias, Anio, Id_Mes, Mes) %>% #Aqui añadimos el total de expedientes por cada mes para cada visitaduria#
  summarise(Frequency = sum(n)) 

g <- merge(x = d, y = f, all.x = TRUE) #Aqui unimos las dos bases para tener la frecuencia en cada categoria y el total de expedientes#


g  <- g  %>% 
  group_by(Visitadurias, Anio, Id_Mes, Mes, Expedientes) %>%  #Se sacan las frecuencias#
  mutate(prop = round((n/Frequency),3),
         prop=(prop)*100)

g$Anio_mes <- paste0(g$Mes,"-",g$Anio) #Se cambia el nombre de algunas variables#


A <- table(Registros_Derechos_graf[,"Visitadurias"], Registros_Derechos_graf[,"Expedientes"]) #Numero de expedientes por visitaduria#
Sin_Derecho <- Registros_Derechos_graf[Registros_Derechos_graf$Expedientes == "Sin Derecho", ] #Numero de expedientes sin registro de DH#
B <- table(Sin_Derecho$Visitadurias, Sin_Derecho$Estatus) #Frecuencia de los expedientes que faltan sin registro# 
C <- cbind(Trámite = B[,1]+B[,3], Concluido = B[,2]) #Estatus de los expedientes#

#Leer la lista de correos a quienes se les envia las inconsistencias.
correos <- read.csv("Lista_Correos_Prueba.csv")

#Fecha para el próximo envío
mes <- as.numeric(format(Sys.Date(),'%m'))
if(mes == 12){
  fecha_envio <- paste0("01-01-",as.numeric(format(Sys.Date(),'%Y'))+1)
} else{
  fecha_envio <- paste0("01-",as.numeric(format(Sys.Date(),'%m'))+1,"-",as.numeric(format(Sys.Date(),'%Y')))
}



Visitas <- c("Primera", "Segunda", "Tercera", "Cuarta", "Quinta") #Vector para los labels de las visitadurias#

Meses <- c('Jul-2019', 'Ago-2019', 'Sep-2019', 'Oct-2019', 'Nov-2019', 'Dic-2019', #Vector para los labels de los meses en la grafica#
           'Ene-2020', 'Feb-2020', 'Mar-2020', 'Abr-2020', 'May-2020', 'Jun-2020',   
           'Jul-2020', 'Ago-2020', 'Sep-2020', 'Oct-2020', 'Nov-2020', 'Dic-2020',       
           'Ene-2021','Feb-2021', 'Mar-2021', 'Abr-2021', 'May-2021',  'Jun-2021',
           'Jul-2021', 'Ago-2021','Sep-2021')

H <- table(Registros_Derechos_PV$Id_adscripcion) #Tabla de expedientes en presunta violación#

Sys.setenv("SMTP_PASSWORD"="password") #Este proceso es para registrar la contraseña en el proceso por fuera del loops#
#Nota: es importante tener en cuenta que este codigo se debe correr cada vez que se ejecute el codigo porque no se guarda por siempre en la memoria#


envio <- function(){
  for (i in 1:5){
    
#Grafica#    
plot <-  ggplot(data=g[g$Visitadurias==Visitas[i],], aes(x=factor(Anio_mes, levels = Meses), y=prop, fill=Expedientes)) +
  geom_bar(stat = "identity")+
  labs(title= paste0("Registro de Derechos Humanos vinculados a los expedientes de quejas \n correspondiente a la ",rownames(A)[i]," visitaduría"),
       x="Meses",
       y="Proporción (%)",
       caption = "Fuente: Coordinación del SIIGESI") +
  scale_fill_manual(values=c('darkolivegreen3','darkgreen')) +
  theme_minimal() +
  theme(plot.title = element_text(size=8,lineheight=.8,hjust=0,face = "bold"),
        plot.subtitle = element_text(size=9,lineheight=.8,vjust=1),
        plot.caption = element_text(size = 7,lineheight=.8, hjust = 0),
        axis.title.y = element_text( size = 9),
        axis.title.x = element_text( size = 9),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"),
        axis.text.x = element_text(angle = 90, color = "grey20", size = 4, hjust = .5, vjust = .5, face = "plain"))
plot + scale_size("Expedientes")
plot + scale_size(range = c(6, 6))

plot_email <- add_ggplot(plot) #Comando para integrar el plot en el cuerpo del correo#

Sin_Derechos <- read.csv("Expedientes_sin_Registro_DH.csv", check.names = FALSE) #Leer la plantilla de excel para asignar la lista de expedientes sin registro de DH# 
Exp_Sin_DH <- Sin_Derecho[,1:10] 
Exp_Sin_DH <- filter(Exp_Sin_DH, Id_adscripcion==i)
write.csv(Exp_Sin_DH, "Expedientes_sin_Registro_DH.csv", row.names = FALSE) #Reescribir en la plantilla los expedientes sin regristros correspondientes a cada area#


email_footer <- #Pie de pagina del correo#
 "
Adjunto a este correo electronico se encuentra el listado de expedientes \n
que no cuentan a la fecha con el registro de o los Derechos Humanos correspondientes. En caso de alguna inquietud comunicarse con Jorge Ruiz-SIIGESI (enrique.ruiz@cdhcm.org.mx).
 "

#Cuerpo del correo#
email <- compose_email(
  body = md(c( "Buen día", correos[correos$Id_Correo == i,3],":
                            <br />
                            <br />
                           En el marco del proceso de consolidación de la información sobre derechos humanos y autoridades en expedientes de quejas en etapa de indagación preliminar, me permito remitir el estatus en el que se encuentra la ",rownames(A)[i]," Visitaduría:
                            <br />
                            <br />
                           De los ",sum(A[i,])," expedientes en etapa de indagación preliminar registrados en la ",rownames(A)[i]," Visitaduría desde el 13 de julio de 2019, un total de ",A[i,2]," aún no tiene calificado el (o los) derecho(s) humano(s), lo que representa el ", round(A[i,2]/sum(A[i,])*100,1),"% de los expedientes de queja en esa etapa. De ese total (",A[i,2],"), ",C[i,1]," se encuentran en trámite (",round(C[i,1]/sum(C[i,])*100, 1),"%) y ",C[i,2]," concluidos (",round(C[i,2]/sum(C[i,])*100, 1),"%).
                            <br />
                            <br />
                           Adicionalmente, se les informa que desde el 13 de Julio de 2019, en esta visitaduría han transitado a etapa de investigación (presunta violación) un total de  ",H[i]," expedientes.
                            <br /> 
                            <br />
                           Por lo anterior, solicitamos atentamente el apoyo de la Visitaduría General para capturar la informacióin pendiente, a fin de consolidar la base de datos y estar en condición de reportar la información institucional de manera más robusta y actualizada.
                            <br />
                            <br />
                            Este reporte se actualizará y enviará el próximo ",fecha_envio," con la finalidad de dar seguimiento e informar sobre el avance que la Visitaduría está realizando.
                            <br />
                            <br />
                           Saludos.",
    plot_email
  )),
  
  footer = email_footer
)


# create_smtp_creds_key(
#   id = "gmail",
#   user = "ali.arrieta@cdhdf.org.mx",
#   host = "smtp.gmail.com",
#   port = 465,
#   use_ssl = TRUE,
#   overwrite = TRUE
#    )

#Envio del correo#
email %>%
  add_attachment(file = "Expedientes_sin_Registro_DH.csv", filename = "Lista de Expedientes sin registro.csv") %>%
  smtp_send(
    from = "ali.arrieta@cdhdf.org.mx",
    to = correos[correos$Id_Correo == i,2],
    cc = c(correos[correos$Id_Correo == 6, 2],correos[correos$Id_Correo == 7, 2],correos[correos$Id_Correo == i+7, 2]),
    subject = "Reporte para la Consolidación de la información sobre derechos humanos y autoridades",
    #credentials = creds_key(id = "gmail")
    credentials = blastula:: creds_envvar(
      user = "ali.arrieta@cdhdf.org.mx",
      pass_envvar="SMTP_PASSWORD",
      provider = "gmail",
      host = "smtp.gmail.com",
      port = 465,
      use_ssl = TRUE) )

  } #Fin del bucle

  }#Fin del bucle
  
#Ejecución del envío
envio()


##THE END##





