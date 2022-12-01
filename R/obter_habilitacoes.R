##Baixa habilitações - caso não haja acesso a BDs internas do MS


pega_hab <- function(tipo = "habilitações") {

blinkhab <- "https://cnes2.datasus.gov.br/"
linkhab <- paste0(blinkhab,'Mod_Ind_Habilitacoes.asp?VEstado=00&VTipo=',toupper(substr(tipo,1,1)))


siteh <- read_html(linkhab)

habsite <- siteh%>%html_elements(xpath="//table[contains(@class,'borda')]")%>%html_table(header =T)

ncerto <- paste0(c(rep(names(habsite[[1]])[3],3),rep(names(habsite[[1]])[6],3)),"\n",
                habsite[[1]][1,])

names(habsite[[1]])[3:8] <- ncerto


links_habs <- siteh%>%html_elements(xpath="//table//tr//td//a")%>%html_attr("href")

links_habs <- paste0(blinkhab,links_habs[-1])

hab_tab <- as_tibble(habsite[[1]][-1,])

px_tab <- function(vinculo){
  print(paste("Processado link número",match(vinculo,ls(pattern="tabh_*",envir = .GlobalEnv))))
  vinculo <- get(vinculo)
  hab_ref <- vinculo%>%
    html_elements(xpath="//table[contains(@colspan,2)]/tr/td/p/font[2]")%>%
    gsub(pattern=paste0("[^\\-]*- (.*)- (.*)</font>"),replacement = "\\1- \\2")
  print("ref_hab completo")

  a <- vinculo%>%html_element(xpath="//table[2]")%>%html_table(header =T)
  a <- a[1:(nrow(a)-2),]

  a <- cbind(a,hab_ref)
  print(dim(a))
  a

}





links_div <- split(links_habs,rep(1:15,each=13)[1:length(links_habs)])

 for (i in 1:length(links_habs)) {
   f <- tempfile()
   download.file(links_habs[i],f,mode = "wb")
   assign(paste0("tabh_",sprintf("%03d",i)),read_html(f),envir = .GlobalEnv)
   if(i %% 9 == 0) {Sys.sleep(5)}
 }

tabela_hab <- rbindlist(lapply(ls(pattern = "tabh_[0-9]"),px_tab))

tabela_hab%<>%separate(hab_ref,into = c("hab_ref","hab_desc"),sep = "- ",extra = "merge")

write.xlsx(tabela_hab,paste0("dados/",Sys.Date()-3,"-habilitações capturadas do site-",tipo,".xlsx"))

tabela_hab

}

tabela_hab <- pega_hab()

rm(list=ls(pattern="tabh_*"))

tabela_inc <- pega_hab("incentivos")

rm(list=ls(pattern="tabh_*"))

tabela_hab$tipo <- "H"

tabela_inc$tipo <- "I"

hab_e_inc <- rbind(tabela_hab,tabela_inc)

#Formatação
hab_e_inc$CNES <- as.numeric(hab_e_inc$CNES)

hab_e_inc$CompetênciaInicial <- as.Date(paste0("15/",hab_e_inc$CompetênciaInicial),"%d/%m/%Y")

hab_e_inc$CompetênciaFinal <- as.Date(paste0("15/",hab_e_inc$CompetênciaInicial),"%d/%m/%Y")

hab_e_inc$hab_ref <- as.numeric(hab_e_inc$hab_ref)


##Harmonização com nomes internos ao BD CNES RL_ESTAB_SIPAC
names(hab_e_inc)[c(4:6,9,11)] <-
  c("CMTP_INICIO","CMTP_FIM","NU_LEITOS","COD_SUB_GRUPO_HABILITACAO","TP_HABILITACAO")

saveRDS(hab_e_inc,paste0("dados/",Sys.Date(),"-hab_e_inc.rds"))
