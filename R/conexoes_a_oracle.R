library(rjson)
library(RJDBC)



jdbcDriver = JDBC("oracle.jdbc.OracleDriver", classPath = "C:/app/client/rodrigo.emmanuel/product/12.2.0/client_1/jdbc/lib/ojdbc8.jar")

###Exemplo para RJPO1DR
esquemapd <- "RJPO1DR.saude.gov"
servidorpd <- "exaccdfdr-scan.saude.gov"
#CNES.TB_ESTABELECIMENTO



baseodbc <- "jdbc:oracle:thin:@//"



pd_ender <- paste0(baseodbc,servidorpd,":1521/",esquemapd)
con_pd  <-  dbConnect(jdbcDriver, pd_ender,Sys.getenv("usrjp"),Sys.getenv("senharjp"))














###Exemplo para DFPO1
#esquemaodbc <- "DFPO1.saude.gov"
#servidorodbc <- "exaccdfprd-scan.saude.gov"
#odbcender <- paste0(baseodbc,servidorodbc,":1521/",esquemaodbc)

#con_cnes  <-  dbConnect(jdbcDriver, odbcender,Sys.getenv("usuarioodbc"),Sys.getenv("senhaodbc"))


#testecnes <- dbGetQuery(con_cnes,'select * from DBCNESRJ.TB_ESTABELECIMENTO')


###Soltos outras consultas
#tabs_rjp <- dbGetTables(con_pd)


#http://landpage-h.cgu.gov.br/dadosabertos/index.php?url=https://s3.sa-east-1.amazonaws.com/ckan.saude.gov.br/CNES/cnes_estabelecimentos.zip
#ftp://ftp.datasus.gov.br/cnes/BASE_DE_DADOS_CNES_202208.ZIP

# apisaude <- "https://apidadosabertos.saude.gov.br/cnes/estabelecimentos?limit=20&offset="


# lepgapi <- function(pg){  t <- rjson::fromJSON(file=paste0(apisaude,pg)) }

# cnes_api <- rbindlist(lapply(0:25000,lepgapi))

#CNES_SNP
# cnes_val_t <- dbGetQuery(con_pd,'select * from CNES_SNP.TB_CNES_VALIDOS')


#leish <- dbGetQuery(con_pd, "select * from AIH.TB_AIH WHERE (CO_DIAG_PRI='B550' OR CO_DIAG_SEC='B550') and NU_MUN_HOSP = '530010' ")

# internacoes_leish_DF <- leish%>%group_by(DT_CMPT)%>%summarize(local="DF",cid10 = "B55.0",internacoes=n())
#
#
# habs_estabs <-  dbGetQuery(con_pd,"select * from CNES.RL_COMP_ESTAB_SIPAC a LEFT JOIN cnes_snp.TB_GRUPOS_HABILITACAO b ON a.COD_GRUPO_HABILITACAO = b.CO_HABILITACAO WHERE (a.NU_COMP = 201812 OR a.NU_COMP = 202210)")
#
#
# sumario_habs_estabs <- habs_estabs[-24]%>%dplyr::filter(COD_SUB_GRUPO_HABILITACAO %in% anx$CÓDIGO)%>%group_by(NU_COMP,NO_DESCRICAO_HABILITACAO)%>%summarize(estabelecimentos_habilitados = n_distinct(CO_CNES))
#
#
# sumario_habs_estabs%<>%pivot_wider(names_from=1, values_from=estabelecimentos_habilitados)
#
# names(sumario_habs_estabs) <- c("Grupo de Habilitação","Situação em Dez/2018","Situação em Out/2022")
#
# sumario_habs_estabs[is.na(sumario_habs_estabs)] <- 0

#sumario_habs_vigentes_em_2022 <- sumario_habs_estabs


