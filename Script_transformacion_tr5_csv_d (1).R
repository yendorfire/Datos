#################################################################
#   Script para convertir archivos tr5 en archivos csv          # 
#   ==========================================================  #
# Elaborado por: Sandra Milena Sorza Gonzalez                   #
# Contrato PS0580 de 2015 celebrado con CORPOICA                #
# Fecha: Agosto de 2015                                         #
# Funcion para realizar la conversion de los archivos           #
#################################################################



######################################################################
# FUNCION VERIFICA_TR5                                               #
# Objetivo: A partir de la revision del ultimo dato en los           #
#           registros del tr5 se identifica los archivo que          #
#           contienen caracteres extraño que no permiten la          # 
#           correcta conversion de los archivos a formato csv        #
# Parametros:                                                        #
#       Ruta_tr5: Ruta donde se encuentra el archivo .tr5, debe      #
#                 terminar con el simbolo /                          #    
#       Archivo_tr5: Nombre del archivo (sin incluir la extension)   # 
# Resultado: La funcion imprime en la consola los diferentes valores #
#          que hay al final de cada registro del tr5, cuando aparece #
#          un solo valor los datos se leyeron correctamente, cuando  #
#          aparece un NA hay un caracter extraño que debe ser        #
#          eliminado manualmente                                     #
#          Cuando se identifica este error se utilizan los objetos   #
#          f0 y Archivo creados en el WorkSpace para identificar el  #
#          registro donde se encuentra el error                      #
######################################################################



VERIFICA_TR5 = function(Ruta_tr5,Archivo_tr5){
  Archivo <<- read.delim(paste(Ruta_tr5,Archivo_tr5,".tr5",sep=""), header=FALSE)
  f0 <<- as.numeric(substr(Archivo$V1,90,140))
  print(paste("Valores en la última columna del tr5:",unique(f0)))
  print("Nota: Si aparece NA hay un error el archivo tr5 que se debe corregir")  

}




#####################################################################
# FUNCION TR5_A_CSV                                                 #
# Objetivo: Convertir la informacion que hay en un archivo tr5      #
#           a archivos en formato csv, donde cada archivo es        #
#           corresponde a una estacion y cada registro la           #
#           la es la informacion de un dia                          #
# Parametros:                                                       #
#       Ruta_tr5: Ruta donde se encuentra el archivo .tr5, debe     #
#                 terminar con el simbolo /                         #
#       Archivo_tr5: Nombre del archivo (sin incluir la extension)  #
#       Ruta_csv: Ruta donde se ubicaran los archivos csv, debe     #
#                 terminar con el simbolo /                         #
#       Variable: Nombre que identificara la variable en los        #
#                 archivos csv, (ppt,tmin,tmax o tmed)              #          
# Resultado: Imprime en la consola los valores de la variable origen#
#            de dato antes y despues de reemplazar los espacios     #
#            Crea un archivo por estacion en formato csv            #
#####################################################################




TR5_A_CSV = function(Ruta_tr5,Archivo_tr5,Ruta_csv,Variable,area){
Archivo <- read.delim(paste(Ruta_tr5,Archivo_tr5,".tr5",sep=""), header=FALSE)
print(paste("Total de registros identificados en el tr5 = ",nrow(Archivo)))

#Separa los datos por caracteres
CODIGO <- as.numeric(substr(Archivo$V1,2,9))
ANIO <- as.numeric(substr(Archivo$V1,12,15))
DIA <- as.numeric(substr(Archivo$V1,16,17))
m1 <-   as.numeric(substr(Archivo$V1,18,22))
f1 <-  substr(Archivo$V1,23,23)
m2 <-   as.numeric(substr(Archivo$V1,24,28))
f2 <-  substr(Archivo$V1,29,29)
m3 <-   as.numeric(substr(Archivo$V1,30,34))
f3 <-  substr(Archivo$V1,35,35)
m4 <-   as.numeric(substr(Archivo$V1,36,40))
f4 <-  substr(Archivo$V1,41,41)
m5 <-   as.numeric(substr(Archivo$V1,42,46))
f5 <-  substr(Archivo$V1,47,47)
m6 <-   as.numeric(substr(Archivo$V1,48,52))
f6 <-  substr(Archivo$V1,53,53)
m7 <-   as.numeric(substr(Archivo$V1,54,58))
f7 <-  substr(Archivo$V1,59,59)
m8 <-   as.numeric(substr(Archivo$V1,60,64))
f8 <-  substr(Archivo$V1,65,65)
m9 <-   as.numeric(substr(Archivo$V1,66,70))
f9 <-  substr(Archivo$V1,71,71)
m10 <-   as.numeric(substr(Archivo$V1,72,76))
f10 <-  substr(Archivo$V1,77,77)
m11 <-   as.numeric(substr(Archivo$V1,78,82))
f11 <-  substr(Archivo$V1,83,83)
m12 <-   as.numeric(substr(Archivo$V1,84,88))
f12 <-  substr(Archivo$V1,89,89)
f0 <- as.numeric(substr(Archivo$V1,90,140))

print(paste("Valores en la última columna del tr5:",unique(f0)))

# Crea para cada mes el archivo donde queda cada día como un registro
Datos1 = data.frame(CODIGO,ANIO,MES=1,DIA,VALOR = m1,ORIGEN = f1)
Datos2 = data.frame(CODIGO,ANIO,MES=2,DIA,VALOR = m2,ORIGEN = f2)
Datos3 = data.frame(CODIGO,ANIO,MES=3,DIA,VALOR = m3,ORIGEN = f3)
Datos4 = data.frame(CODIGO,ANIO,MES=4,DIA,VALOR = m4,ORIGEN = f4)
Datos5 = data.frame(CODIGO,ANIO,MES=5,DIA,VALOR = m5,ORIGEN = f5)
Datos6 = data.frame(CODIGO,ANIO,MES=6,DIA,VALOR = m6,ORIGEN = f6)
Datos7 = data.frame(CODIGO,ANIO,MES=7,DIA,VALOR = m7,ORIGEN = f7)
Datos8 = data.frame(CODIGO,ANIO,MES=8,DIA,VALOR = m8,ORIGEN = f8)
Datos9 = data.frame(CODIGO,ANIO,MES=9,DIA,VALOR = m9,ORIGEN = f9)
Datos10 = data.frame(CODIGO,ANIO,MES=10,DIA,VALOR = m10,ORIGEN = f10)
Datos11 = data.frame(CODIGO,ANIO,MES=11,DIA,VALOR = m11,ORIGEN = f11)
Datos12 = data.frame(CODIGO,ANIO,MES=12,DIA,VALOR = m12,ORIGEN = f12)

#Se hace la union de los 12 archivos
DATOS <- data.frame(rbind(Datos1,Datos2,Datos3,Datos4,Datos5,Datos6,Datos7,Datos8,Datos9,Datos10,Datos11,Datos12))
#Se da formato fecha para eliminar las fechas no existentes
DATOS$fechas_datos = as.Date(paste(DATOS$DIA,"/",DATOS$MES,"/",DATOS$ANIO,sep=""),"%d/%m/%Y")
#fechas_erradas <<- subset(DATOS,is.na(fechas_datos))
DATOS = DATOS[!is.na(DATOS$fechas_datos),]

# Se reemplaza el origen de Dato que es un espacio por SF
print("Valores en origen antes de reemplazar espacios:")
print(table(DATOS$ORIGEN))
DATOS$ORIGEN = sub(" ","SF",DATOS$ORIGEN)
eval(parse(text=paste("DATOS_",area,"_",Variable, "<<-DATOS",sep="")))
print("Valores en origen despues de reemplazar espacios:")
      print(table(DATOS$ORIGEN))
      
# Se hace la creacion de un archivo por estacion eliminando las fechas que no existen y colocando SF donde no hay origen de dato
codigos = unique(CODIGO)
control = c()
for (i in 1:length(codigos)){
if (i == round(length(codigos)/2)) {print(paste(" ... Conversion de la estacion",i,"de",length(codigos)))}
  
  D = subset(DATOS,CODIGO==codigos[i])
  # ordena por anio, mes, dia
  ordena = with(D, order(ANIO,MES,DIA))
  D = D[ordena,]
  D = D[c("CODIGO","ANIO","MES","DIA","VALOR","ORIGEN")]
  write.table(D, file = paste(Ruta_csv,codigos[i],"_",Variable,".csv",sep=""), sep=",",row.names=FALSE,col.names=FALSE)
  
  }
  
}

# ####################################################################
# #         INFORMACION A INGRESAR POR EL USUARIO                    #   
# ####################################################################
# 
# # NOTA: PARA EJECUTAR DESCOMENTAR LAS SIGUIENTES INSTRUCCIONES
# #       LOS VALORES QUE SE INTRODUCEN SON DONDE HAY <<>>
# #       "<<>>" INDICA PARAMETRO DE TIPO TEXTO Y SE DEBE DEJAR LAS COMILLAS
# #        <<#>>  INDICA PARAMETRO DE TIPO NUMERICO 
# #
# # PASO 1. Verificar la existencia de caracteres extranos en los archivos tr5
# 
# 
# VERIFICA_TR5("<<Ruta_tr5>>/","<<Archivo_tr5>>")
# 
# # PASO 2. Si el resultado de correr la funcion VERITIFA_TR5 indica que hay error
# #          se ejecutan las siguientes lineas, caso contrario se ejecuta el 
# #          Paso 3.
# 
# revi = cbind(f0,c(1:length(f0)))
# subset(revi,is.na(f0))
# # con los valores que aparezcan de la anterior instruccion se busca un registro 
# Archivo[<<#registro>>,]
# # con la informacion se busca en el tr5 el registro y se identifica el simbolo que ocasiona el error
# # es mas facil identificar con gedit bajo linux
# 
# rm(revi,f0,Archivo)
#         
# 
# # PASO OPCIONAL. SE CONSTRUYEN LAS RUTAS DONDE SE GUARDARAN LOS ARCHIVOS EN FORMATO CSV
# #  Si se quiere conservar la estructura de organizacion por areas operativas se puede contruir
# # para cada variable 11 carpetas con el nombre aop#, donde # identifica el numero del area  
# # En Ruta se coloca la ruta donde se crearan las 11 carpetas (1 por area operativa). La ruta finaliza con 
# # un / 
# # Variable se coloca tmed, tmin, tmax, ppt segun sea el caso
# 
# Ruta = "<<>>/"
# Variable = "<<>>"
# for (i in 1:11){
#   if (!file.exists(paste(Ruta,Variable,sep=""))) dir.create(paste(Ruta,Variable,sep=""))
#   if (!file.exists(paste(Ruta,Variable,"/aop",i,sep=""))) dir.create(paste(Ruta,Variable,"/aop",i,sep=""))
# 
# }
# 
# rm(i,Variable,Ruta)
# 
# #PASO 3. TRANSFORMACION DE LOS ARCHIVOS A CSV
# TR5_A_CSV("<<Ruta_tr5>>/","<<Archivo_tr5>>","<<Ruta_csv>>/","<<Variable>>",<<#area>>)

