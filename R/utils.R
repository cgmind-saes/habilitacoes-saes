### Obter do IBGE lista de regiões, estados e municípios

compdata <- function(valor) {
  as.Date(paste0("15",valor),"%d%m%Y")
}

library(rjson)
filter <- dplyr::filter
municipios <- "https://servicodados.ibge.gov.br/api/v1/localidades/municipios"

br_mun <- fromJSON(file = municipios)

achata <- function(x){as.data.frame(br_mun[[x]])}

br_mun <- data.table::rbindlist(lapply(1:length(br_mun),achata))

br_mun$idt <- substr(br_mun$id,1,6)



##funções úteis
fnx  <- function(x){formatC(as.numeric(x), format="f", big.mark=".", decimal.mark = ",", digits = 1)}
fni <- function(x){formatC(as.numeric(x), format="d", big.mark=".", decimal.mark = ",",digits = 0)}



pop_estadual <- datasus::ibge_projpop_bruf()

pop_estadual <- pop_estadual[c("Unidade da Federação","Total")]

pop_estadual  %<>%filter(`Unidade da Federação` != "TOTAL")
pop_estadual %<>% separate(`Unidade da Federação`,3,into=c("n_UF","UF"))



