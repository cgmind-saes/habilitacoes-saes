##Baixa habilitações
library(xlsx)
library(readxl)
library(rvest)
library(xml2)
library(tidyverse)
library(data.table)

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
    gsub(pattern=".*-(.*)-[^-]*",replacement = "\\1")
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

write.xlsx(tabela_hab,paste0("dados/",Sys.Date(),"-habilitações capturadas do site-",tipo,".xlsx"))

tabela_hab

}

tabela_hab <- pega_hab()

tabela_inc <- pega_hab("incentivos")

rm(list=ls(pattern="tabh_*"))

tabela_hab$tipo <- "H"

tabela_inc$tipo <- "I"

hab_e_inc <- rbind(tabela_hab,tabela_inc)

##Tabela anterior

tab2020031a <- read_xlsx("dados/arquivados - histórico/primeira tentativa/Hab_Novo_tot.xlsx", sheet = 1)

#tab_bd_orig <- read_xlsx("dados/banco de dados originais/habiltações_total_2020_03_BD origem.xlsx")

#tba_estab_com_intern <- read_xlsx("dados/estab com internação/BD estabelecimento com leitos de internação_20-11-2020.xlsx")


hab_sep <- list.files(path = "dados/estab com internação/",pattern="habilta.*",full.names = T)

tb_hab_tot <- data.table::rbindlist(lapply(hab_sep,read_xlsx,sheet = "BD origem"))
