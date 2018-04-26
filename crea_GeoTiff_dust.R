#Rivisto 26 aprile 2018
#
#Il programma non usa più lo shapefile GADM per acquisire l'informazione sulle regioni.
#Questa nuova versione utilizza direttamente un raster delle regioni prodotto a partire dallo shapefile dell'ISTAT. 
#
#Il programma non crea più un netCDF ma un GeoTiff con gli eventi dust partendo dal file di Stafoggia che elenca giorno per giorno del 2015
#la presenza (1) o meno (0) di un evento dust in Nord/Centro/sud/Sicilia/Sardegna
rm(list=objects())
library("raster")
library("rgdal")
library("dplyr")
library("readr")
library("tidyr")
library("stringr")
library("rasterVis")
options(warn=2,error=recover)

#GENERARE UN PDF CON LA VISUALIZZAZIONE DEL DUST?
GRAFICO<-TRUE

#Anno di elaborazione, inizio e fine
ANNOI<-2015
ANNOF<-2015
#creazione di un calendario da ANNOI a ANNOF
calendario<-as.character(seq.Date(from=as.Date(paste0(ANNOI,"-01-01")),to=as.Date(paste0(ANNOF,"-12-31")),by="day",format="%Y-%m-%d"))

#FILE DUST con i dati di input riguardo alla presenza (1) o assenza (0) di dust in nord/centro/sud(sardegna/sicilia
#Sono dati passati da Stafoggia e rielaborat da Guido (semplicemente eliminato anni e colonne inutili)
DUST.FILE<-"dust.csv"

#il file dust ha le colonne giorno mese e anno e le colonne Nord, Centro, Sud, Sardegna e Sicilia.
#Nelle colonne Nord, Centro... viene riportato 1 se nel giorno X nell'area Y si è verificato un evento di dust altrimenti riporta 0
#Mediante tidyr costruiamo una colonna area che riporta l'informazione sull'area e una colonna dust che riporterà 0/1
tryCatch({
  read_delim(DUST.FILE,delim=";",col_names = TRUE)
},error=function(e){
  stop(sprintf("Non trovo il file di testo %s",DUST.FILE))
})->dust  
  
names(dust)<-tolower(names(dust))
#le colonne debbono riportare i seguenti nomi altrimenti mi blocco
stopifnot(c("yy","mm","dd","nord","centro","sud","sardegna","sicilia") %in% names(dust))

dust %>% 
  gather(key=area,value=dust,-yy,-mm,-dd) %>% 
  mutate(mm=str_pad(mm,width=2,pad=0,side="left"),dd=str_pad(dd,width=2,pad=0,side="left")) %>%  
  mutate(data_record_start_time=paste0(yy,"-",mm,"-",dd)) %>% 
  filter(data_record_start_time %in% calendario) %>% #solo eventi dust in calendario
  dplyr::select(-yy,-mm,-dd)->mydust

rm(dust)
stopifnot(nrow(mydust)!=0)

########################## Suddivione dell'ITALIA #####################################
c("Valle d'Aosta","Piemonte","Lombardia","Trentino-Alto Adige","Veneto","Friuli-Venezia Giulia","Liguria","Emilia-Romagna")->Nord
c("Marche","Toscana","Umbria","Lazio")->Centro
c("Campania","Abruzzo","Calabria","Basilicata","Molise","Apulia")->Sud #no Sardegna e Sicilia, che vanno a se
#######################################################################################


########################## Suddivione dell'ITALIA #####################################
c(2,1,3,4,5,6,7,8)->Nord
c(11,9,10,12)->Centro
c(15,13,18,17,14,16)->Sud #no Sardegna e Sicilia, che vanno a se
c(19)->Sicilia
c(20)->Sardegna
#######################################################################################

#Lettura del file raster griglia.tif che rappresenta la proiezione di arrivo e che utilizziamo come raster per via via
#rasterizzare lo shapefile contenente le info sull'area geografica e la presenza o meno di dust
FILE.GRIGLIA<-"../griglia_tif/griglia.tif"
tryCatch({
  raster(FILE.GRIGLIA)
},error=function(e){
  stop(sprintf("Non trovo il file raster %s",FILE.GRIGLIA))  
})->grigliaTemplate
  
#Shapefile: ITA_adm1 contiene i nomi delle regioni
tryCatch({
  raster("../istat_regioni_raster/istat_raster_regioni.tif")
},error=function(e){
  stop("Errore lettura shapefile GADM per l'Italia")
})->italia

raster_no_dust<-grigliaTemplate
raster_no_dust[!is.na(raster_no_dust)]<-0

#raster_dust: qui manteniamo i codici regione
raster_dust<-italia

#Ciclo sui vari giorni
purrr::map(1:length(calendario),.f=function(ii){

  yymmdd<-calendario[ii]
  
  #qualiAree nel giorno yymmdd hanno un evento di dust (1)?
  which(mydust$dust==1 & mydust$data_record_start_time==yymmdd)->qualiAree

  #nessuna area, nessun evento per quel giorno
  if(!length(qualiAree)){
    metadata(raster_no_dust)<-list("data_record_start_time",yymmdd)
    return(raster_no_dust)    
  }

  #Altrimenti: aggiorna il raster italia
  mydust[qualiAree,]$area->areeConDust
  
  #troviamo i codici numerici corrispondenti
  purrr::map(areeConDust,~(get(Hmisc::capitalize(.)))) %>% purrr::reduce(c)->codiciRegioni #get utilizza le variabili Nord Centro Sud Sicilia Sardegna per ricavare i codici numerici
  
  raster_dust[raster_dust %in% codiciRegioni]<-999
  raster_dust[raster_dust < 999]<-0
  raster_dust[raster_dust >0]<-1
  
  #memorizzo la data (giorno) dell'evento dust
  metadata(raster_dust)<-list("data_record_start_time",yymmdd)

  raster_dust

}) ->listaOut

#listaOut è una lista di raster
purrr::map_chr(listaOut,~(metadata(.)[[2]]))->data_record_start_time

#verifico la posizione dei raster rispetto ai giorni. 
if(!identical(data_record_start_time,calendario)) stop("Metadata data_record_start_time e calendario non coincidono!")

if(GRAFICO){
    #riordino i dati e faccio un grafico 
    pdf("dust.pdf",768,1024,onefile=TRUE)
      purrr::walk(1:length(calendario),.f=function(ii){
        
        print(levelplot(listaOut[[ii]],main=metadata(listaOut[[ii]])[[2]],margin=FALSE))
  
      })
    dev.off()
}#fine GRAFICO
    
brick(listaOut)->mybrick
writeRaster(mybrick,filename = "dust_daily_utm.tif",format="GTiff",overwrite=TRUE)


