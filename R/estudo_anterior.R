##Do Estudo Anterior


##Tabelão com todas habilitações e incentivos em coluna
hab_sep <- list.files(path = "dados/estab com internação/",pattern="habilta.*",full.names = T)

tb_hab_tot <- data.table::rbindlist(lapply(hab_sep,read_xlsx,sheet = "BD origem"))


lista_habs_inc <- data.frame("misturado" = names(tb_hab_tot)[7:length(names(tb_hab_tot))])

lista_habs_inc <- lista_habs_inc%>%separate(misturado,c("tipo","tp_extenso","hab_ref"),sep = c("[–-]"))


##Lista com os de interesse da saúde especializada

anx_ieae <- "dados/Documentos para o SEI/Anexo_1__habilitacoes_e_incentivos_da_AE_vf.pdf"

pg_0101 <- locate_areas(anx_ieae,1)

pg_0201 <- locate_areas(anx_ieae,2)

pg_0301 <- locate_areas(anx_ieae,3)

pg_0401 <- locate_areas(anx_ieae,4)

pg_0501 <- locate_areas(anx_ieae,5)

pg_0502 <- locate_areas(anx_ieae,5)

pg_0601 <- locate_areas(anx_ieae,6)

area_anx_h <- c(pg_0101,pg_0201,pg_0301,pg_0401,pg_0501)

area_anx_i <- c(pg_0502,pg_0601)

saveRDS(area_anx_h,"dados/areas_habs_anx_antigo.rds")

saveRDS(area_anx_i,"dados/areas_incs_anx_antigo.rds")

anx_h <- extract_tables(anx_ieae,pages = 1:5,area = area_anx_h, guess = F, output="data.frame")

for (i in 2:5) {
  m <- anx_h[[i]]
  ml1 <- names(m)
  names(m) <- names(ml1) <- names(anx_h[[1]])
  m <- rbind(ml1,m)
  m[grepl("X",m$CÓDIGO),1] <- NA
  anx_h[[i]] <- m
}

anx_h <- rbindlist(anx_h)

for (i in 1:nrow(anx_h)) {
  if(is.na(anx_h[i,1])) {
    anx_h[i:(i+2),1] <- anx_h[i+1,1]
  }
}


anx_h <- anx_h%>%group_by(CÓDIGO)%>%
  summarize(HABILITAÇÕES = paste(HABILITAÇÕES, collapse = " "))

anx_h$HABILITAÇÕES <- gsub("\\."," ",anx_h$HABILITAÇÕES)
anx_h$HABILITAÇÕES <- gsub("  "," ",anx_h$HABILITAÇÕES)


anx_i <- extract_tables(anx_ieae,pages = 5:6,area = area_anx_i, guess = F, output="data.frame")

m <- anx_i[[2]]
ml1 <- names(m)
ml1[1] <- as.numeric(gsub("X","",ml1[1]))
m <- rbind(ml1,m)
names(m) <- names(anx_i[[1]])
anx_i[[2]] <- m

anx_i <- rbindlist(anx_i)

anx_h$tipo <- "H"

anx_i$tipo <- "I"

names(anx_h)[2] <- names(anx_i)[2] <- "desc"

anx <- rbind(anx_h,anx_i)

rm(list = ls(pattern = ".*nx_.*"))


#Formatação do anexo
anx$CÓDIGO <- as.numeric(anx$CÓDIGO)

saveRDS(anx, "dados/anexo2020.rds")
