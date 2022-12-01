##Pegar recentes


habs_incs_novos <- hab_e_inc%>%filter(CompetênciaInicial>as.Date("2020-03-01","%Y-%m-%d"))


# foracods <- habs_incs_novos%>%mutate(across(hab_ref,as.numeric))%>%
#   filter(!(hab_ref %in% anx$CÓDIGO))


##Consulta a H e I e respectivos códigos constantes da tabela do estudo de 2020
lihain <- lista_habs_inc%>%filter(tipo %in% c("H","I"))

foracods <- habs_incs_novos%>%mutate(across(hab_ref,as.numeric))%>%
  filter(!(hab_ref %in% as.numeric(lihain$hab_ref)))

foracods <- foracods[!is.na(foracods$hab_ref),]



codsnovos <- foracods %>%group_by(hab_ref)%>%summarize(desc_ref=first(hab_desc),habilitados= n(),tipo = first(tipo))


codsnovos$interesse_especial <- 1

#Cf. orientado pela coordenadora Kathleen Machado,
#Serviço de aconselhamento genético e CERESTs não devem ser incluídos
codsnovos[c(3,5,6),"interesse_especial"] <- 0

codsnovos%>%write_csv2("dados/2022-09-novos_habs_refs.csv")



novos_ieae <- codsnovos%>%filter(interesse_especial == 1)%>%
  rename(CÓDIGO = hab_ref, desc = desc_ref)%>%select(-habilitados,-interesse_especial)


anx_novo <- bind_rows(anx,novos_ieae)%>%arrange(tipo,CÓDIGO)


saveRDS(anx_novo,paste0("dados/",Sys.Date(),"anexo-novo.rds"))
