##Obter leitos
#https://cnes2.datasus.gov.br/Mod_Ind_Leitos_Listar.asp?VCod_Leito=01&VTipo_Leito=1&VListar=1&VEstado=00&VMun=&VComp=202112


library(DBI)
demas <- dbConnect(odbc::odbc(), "DEMAS", timeout = 10)
nodebe <- "DBCNES_LEITOS_COMP_WEB"

grupos_leitos <- dbGetQuery(demas,"SELECT * from dbacoes_saude.tb_cnes_grupo_leito")

codigos_leitos <- dbGetQuery(demas,"SELECT * from dbacoes_saude.tb_cnes_leito")

codigos_leitos %<>% group_by(co_leito,co_grupo_leito)%>%summarize(co_leito=first(co_leito),
                                                                  co_grupo_leito=first(co_grupo_leito))

codigos_leitos$co_leito <- sprintf("%02d",codigos_leitos$co_leito)

pega_leitos <- function(comp = 202208) {

  blinkl <- "https://cnes2.datasus.gov.br/"
  linkl <- paste0(blinkl,'Mod_Ind_Tipo_Leito.asp?VEstado=00&VCOMP=',comp)


  siteh <- read_html(linkl)

  leitoslinks <- siteh%>%html_elements(xpath="//table//a")%>%html_attr("href")

  leitoslinks <- leitoslinks[grepl("Mod_Ind",leitoslinks)]

  leitoslinks <- paste0(blinkl,leitoslinks)


  px_tab <- function(vinculo){
    indice <- match(vinculo,ls(pattern="tab_leito*",envir = .GlobalEnv))
    print(paste("Processado link número",indice))
    vinom <- vinculo
    vinculo <- get(vinculo)
    grupo_leito <- vinculo%>%
      html_elements(xpath="//table//font[4]/b")%>%
      gsub(pattern=paste0("[^\\-]*- (.*)- (.*)</.*"),replacement = "\\1- \\2")

    if (length(grupo_leito) == 0) {
      f <- tempfile()
      download.file(leitoslinks[indice],f, mode = "wb")
      assign(paste0("tab_leito",sprintf("%03d",indice)),read_html(f),envir = .GlobalEnv)
      vinculo <- get(vinom)
    }
    grupo_leito <- vinculo%>%
      html_elements(xpath="//table//font[4]/b")%>%
      gsub(pattern=paste0("[^\\-]*- (.*)- (.*)</.*"),replacement = "\\1- \\2")


    print("ref_leito completo")


    a <- try(vinculo%>%html_element(xpath="//table[2]")%>%html_table(header = T))


    a <- cbind(a,grupo_leito)
    print(dim(a))
    a

  }





  links_div <- split(leitoslinks,rep(1:15,each=13)[1:length(leitoslinks)])

  for (i in 1:length(leitoslinks)) {
    f <- tempfile()
    download.file(leitoslinks[i],f,mode = "wb")
    assign(paste0("tab_leito",sprintf("%03d",i)),read_html(f),envir = .GlobalEnv)
    if(i %% 5 == 0) {Sys.sleep(4)}
  }

  tabela_leitos <- rbindlist(lapply(ls(pattern = "tab_leito[0-9]",envir = .GlobalEnv),px_tab))

  tabela_leitos <- tabela_leitos[!(is.na(as.numeric(tabela_leitos$CNES))),]

  tabela_leitos$CNES <- as.numeric(tabela_leitos$CNES)

  tabela_leitos$Existentes <- as.numeric(tabela_leitos$Existentes)

  tabela_leitos$competencia <- comp

  tabela_leitos%<>%separate(grupo_leito,into = c("grupo_leito","leito"),sep = "- ",extra = "merge")

  if(!dbExistsTable(demas,nodebe)) {
    dbCreateTable(demas,nodebe,tabela_leitos)
  }
  dbAppendTable(demas,nodebe,tabela_leitos)
  rm(list=ls(pattern="tab_leito0*"),envir=.GlobalEnv)
  gc()
  tabela_leitos
}


leitos_ago2022 <- pega_leitos()


periodos <- expand.grid(1:12,2008:2021)

periodos <- paste0(periodos$Var2,sprintf("%02d",periodos$Var1))

periodos <- c(periodos,paste0(20220,1:7))

leitos_t <- rbindlist(lapply(periodos[(length(periodos)-21):length(periodos)],pega_leitos))


leitos_cod_nome <- read_csv2("dados/leitos_e_codigos-obtido-site-cnes.csv")

leitos_cod_nome <- leitos_cod_nome%>%mutate(across(contains("leito"),as.numeric))


testeretorno <- dbGetQuery(demas,'SELECT * from "DBCNES_LEITOS_COMP_WEB"')

testeretorno <- testeretorno%>%left_join(leitos_cod_nome[-1],by = c("leito" = "nome"))

names(testeretorno)[8] <- "co_grupo_leito"

dbRemoveTable(demas,nodebe)

dbCreateTable(demas,nodebe,testeretorno)

dbWriteTable(demas,nodebe,testeretorno, overwrite = T)


