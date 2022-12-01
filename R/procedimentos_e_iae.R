###Procedimentos

anx <- readRDS("dados/2022-09-15anexo-novo.rds")


linksigtap <- "ftp://ftp2.datasus.gov.br/public/sistemas/tup/downloads/TabelaUnificada_202211_v2211011719.zip"

nomesigtap <- "2022-11-011719-sigtap-tabela-unificada.zip"
download.file(linksigtap,paste0("dados/",nomesigtap) , method = "auto")


system(paste('cd dados && "C:/Program Files/7-Zip/7z.exe" e -osigtap',nomesigtap))


##função para ler arquivo do sigtap
lesigtap <- function(nometab, pasta = "dados/sigtap", tipo = "tb") {
  basenome <- paste0(pasta,"/",tipo,"_",nometab)
  tamanhos <- read_csv(paste0(basenome,"_layout.txt"))
  tabela <- read.fwf(paste0(basenome,".txt"),tamanhos[2], fileEncoding = 'iso-8859-1')
  names(tabela) <- tamanhos[[1]]
  tabela

}

proc_grupo_tus <- c("grupo","sub_grupo",)

subgrupost <- lesigtap("sub_grupo")

grupost <- lesigtap("grupo")


consulta <- paste("SELECT SUM(NU_PA_QTDAPR), SUM(x.NU_PA_TOT),x.CO_PA_TPUPS , x.CO_PA_NIVCPL, x.CO_PA_GESTAO, x.CO_PA_UFMUN,  b.CO_CODIGO_GRUPO FROM
                  SIA.TB_PA x LEFT JOIN CNRAC.RL_PROCEDIMENTO_HABILITACAO a
                  ON x.CO_PA_PROC_ID = a.CO_PROCEDIMENTO
                  LEFT JOIN CNES_SNP.TB_SUB_GRUPOS_HABILITACAO b
                  ON a.CO_HABILITACAO = b.CO_CODIGO_GRUPO
                  WHERE ((x.CO_PA_CMP  BETWEEN  '201901' AND '202212')
                  AND a.CO_HABILITACAO IN (",paste(anx$`CÓDIGO`,collapse=","),"))
                  GROUP BY x.CO_PA_TPUPS , x.CO_PA_NIVCPL, x.CO_PA_GESTAO, x.CO_PA_UFMUN, b.CO_CODIGO_GRUPO")



procedimentos <- dbGetQuery(con_pd,consulta)



grupos_hab <- dbGetQuery(con_pd,"select * from cnes_snp.TB_GRUPOS_HABILITACAO")



proc_grupos <- procedimentos%>%mutate(grupo = substr(CO_CODIGO_GRUPO,1,2))%>%left_join(grupos_hab%>%mutate(grupo=substr(CO_HABILITACAO,1,2)))



proc_grupos%<>%group_by(NO_DESCRICAO_HABILITACAO,CO_PA_TPUPS,CO_PA_UFMUN,CO_PA_NIVCPL,CO_PA_GESTAO)%>%
  summarise(across(contains("SUM"),sum,na.rm=T))



resumao <- proc_grupos%>%ungroup()%>%group_by(NO_DESCRICAO_HABILITACAO)%>%summarize(across(contains("SUM"),sum, na.rm=T))
