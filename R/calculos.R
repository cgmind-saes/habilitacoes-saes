##tabelas estáticas
cnesv <- paste0("dados/",Sys.Date(),"-cnes_validos.rds")
source("R/conexoes_a_oracle.R")
source("R/utils.R")
source("R/paletas.R")
tp_unidade_cl <- read_csv2("dados/tipos_unidades.csv")[-6,]

anx <- readRDS("dados/2022-09-15anexo-novo.rds")


##Elementos de mapas
brbb <- c(6.545, -75.674,-34.271, -35.354)

canto_se <- brbb[1:2]

canto_id <- brbb[3:4]

brbasemap <- openmap(canto_se,canto_id, zoom = 7,'stamen-watercolor', mergeTiles = T)


mapag <- read_state(year = 2020, simplified = F)



#Conserta Amazonas
mapag$name_state[3] <- "Amazonas"



mapag <- rmapshaper::ms_filter_islands(mapag,min_area = 2e7)
#Tabelas do dbdemas
demas <- dbConnect(odbc::odbc(), "dbdemas", timeout = 10)
dfdg1 <- dbConnect(odbc::odbc(), "DfDG1", timeout = 10)
#cnes_nj <- dbGetQuery(demas,'SELECT co_cnes,co_natureza_jur,tp_unidade from dbcnes_tb_estabelecimento' )

cnesvalidos <- dbGetQuery(con_pd,'select * from CNES.TB_ESTABELECIMENTO WHERE co_motivo_desab is null')

cnesvalidos <- cnesvalidos %>% mutate(across(c(CO_CNES,CO_NATUREZA_JUR,TP_UNIDADE),as.numeric))

saveRDS(cnesvalidos,cnesv)


#cnes_nj <- cnes_nj%>%mutate(across(.cols = everything(), as.numeric))

#cnesvalidos <- readRDS(cnesv)


##tabelas estáticas

#tipos_nj <- readRDS("dados/natureza_juridica_tipos_codigos.rds")

#tipos_nj <- tipos_nj[!is.na(tipos_nj$CO_NATUREZA_JURIDICA_CONCLA),]

#cnesvalidos%<>%inner_join(tipos_nj, by = c("CO_NATUREZA_JUR" = "CO_NATUREZA_JURIDICA_CONCLA"))

######REFAZER
cnesvalidos%<>%mutate(natureza = case_when(
  grepl("^1",CO_NATUREZA_JUR) ~ "Administração Pública",
  grepl("^3",CO_NATUREZA_JUR) ~ "Entidades Sem Fins Lucrativos",
  T ~ "Entidades Empresariais"
))

###Unidades Mistas, Hospitais Gerais, Hosp Esp, PS Geral, PS Especializado
cl_por_nat <- cnesvalidos%>%dplyr::filter(TP_UNIDADE %in% c(5,7,15,20,21))%>%count(natureza)

cl_por_nat$percentual <- 100*prop.table(cl_por_nat$n)



# listas_hi <- list.files(path = "dados",pattern="-hab_e_inc.rds",
#                         full.names = T)
#hab_e_inc <- readRDS(listas_hi[length(listas_hi)])


hab_e_inc <- dbGetQuery(con_pd,"SELECT * FROM CNES.RL_ESTAB_SIPAC")

hab_e_inc%<>%mutate(across(contains("CMTP"),compdata))

hab_e_inc %<>%dplyr::filter( TP_HABILITACAO %in% c("H","I") & (is.na(CMTP_FIM) | CMTP_FIM> Sys.Date()))


hab_e_inc$CO_CNES <- as.numeric(hab_e_inc$CO_CNES)


datacnes <- format.Date(Sys.Date(),"%B de %Y")

total_estabs <- length(unique(cnesvalidos$CO_CNES))

com_hab_inc <- unique(hab_e_inc$CO_CNES)

tot_chi <- length(com_hab_inc)



hab_e_inc$iae <- case_when(hab_e_inc$COD_SUB_GRUPO_HABILITACAO %in% anx$CÓDIGO ~ T,
                           T ~ F)

hab_por_cnes <- hab_e_inc%>%group_by(CO_CNES)%>%summarize(habs_incs = n(), leitos=sum(NU_LEITOS,na.rm=T))

hab_por_cnes <- hab_por_cnes%>%inner_join(cnesvalidos)



hab_por_nat <- hab_por_cnes%>%count(natureza)

hab_por_nat$percentual <- 100*prop.table(hab_por_nat$n)

hab_nat_det <- hab_por_cnes%>%count(CO_NATUREZA_JUR,ST_ADESAO_FILANTROP)

hi_iae <- hab_e_inc[hab_e_inc$iae ==T & hab_e_inc$CO_CNES %in% cnesvalidos$CO_CNES,]

est_iae <- data.frame("CO_CNES" = unique(hi_iae$CO_CNES))
tehab <- nrow(est_iae)



est_iae %<>%left_join(cnesvalidos, by = "CO_CNES")

#CLIMEP desativado em setembro
#est_iae[is.na(est_iae$CO_MUNICIPIO_GESTOR),]$CO_MUNICIPIO_GESTOR <- 330490


prop_iae <- est_iae%>%group_by(CO_CNES)%>%summarize(natureza = first(natureza))%>%
  count(natureza)%>%mutate(percentual = 100*prop.table(n))


est_iae <- est_iae%>%left_join(br_mun, by = c("CO_MUNICIPIO_GESTOR" = "idt"))

names(est_iae)[match("microrregiao.mesorregiao.UF.nome",names(est_iae))] <- "UF_nome"

names(est_iae)[match("microrregiao.mesorregiao.UF.id",names(est_iae))] <- "n_UF"

pop_estadual$n_UF <- as.numeric(pop_estadual$n_UF)
est_iae%<>%left_join(pop_estadual, by = "n_UF")

est_iae%<>%left_join(hab_por_cnes[c("CO_CNES","habs_incs")])



iae_uf <- est_iae%>%
  left_join(hab_e_inc%>%select(CO_CNES,NU_LEITOS))%>%group_by(UF)%>%summarize(
  n_estabs = n(),
  n_leitos = sum(NU_LEITOS,na.rm = T),
  pop = first(Total),
  `Estabelecimentos por 1.000 habitantes` = n_estabs*1e3/pop,
  `Leitos por 1.000 habitantes` = n_leitos*1e3/pop,
  n_habs = sum(habs_incs,na.rm = T),
  UF = first(UF))%>%mutate(
    bordab = 20*n_habs/sum(n_habs),
    `Habilitações por estabelecimento` = n_habs/n_estabs
  )

iae_uf$UF <- str_to_title(iae_uf$UF)

iae_uf <- mapag %>%left_join(iae_uf, by = c("name_state" = "UF"))


iae_uf$fx_cor <- cut(iae_uf$`Estabelecimentos por 1.000 habitantes`,c(0,2,4,6,8,10))
mapa1 <-  autoplot.OpenStreetMap(brbasemap)+
  ggspatial::annotation_map_tile()+
  geom_sf(data = iae_uf,inherit.aes = F , aes(fill = `Leitos por 1.000 habitantes`), alpha = 0.5)+
  scale_fill_gradientn(colours = paleta5, limits = c(0,max(iae_uf$`Leitos por 1.000 habitantes`)))+
  geom_sf(data = sf::st_centroid(iae_uf),
          inherit.aes = F , aes(size = 1.4*bordab),
          color = "black", legend.position ="none", show.legend = F)+
  guides(size=F)+
  geom_sf(data = sf::st_centroid(iae_uf),
          inherit.aes = F , aes(color = `Habilitações por estabelecimento` , size = bordab))+
  scale_color_distiller(palette = "Spectral")+
  #  scale_fill_gradientn(colours = paleta3, limits = c(0,10))+
  theme_minimal()+theme(axis.text = element_blank(),axis.title = element_blank(),
                        legend.position = "bottom")


prop_iae_rg <- est_iae%>%group_by(regiao.imediata.regiao.intermediaria.UF.regiao.nome,CO_CNES)%>%summarize(natureza = first(natureza))%>%
  count(natureza)%>%mutate(percentual = 100*prop.table(n))

names(prop_iae_rg)[1] <- "Região"

piaergraf <- ggplot(prop_iae_rg,aes(x = `Região`, y = n, fill = natureza))+
  geom_bar(aes(y=n),stat = "identity",position= position_dodge2(preserve = "single",width = 2))+scale_fill_manual(values=paleta7[seq(1,13,by = 2)])+
  theme_minimal()+theme(legend.position = "bottom", legend.title = element_blank(),legend.text = element_text(size=8) )+
  xlab("Região e natureza jurídica")+ylab("Número de estabelecimentos")+
  geom_label(aes(x = `Região`, y=n+13*(n^0.5),label=paste0(n)),colour="white",position = position_dodge(width = 1))
#    ggtitle(paste0("Grafico 01 -
# Quantidade de estabelecimento de saúde com leitos de internação e habilitação/incentivo de interesse da atenção especializada, segundo as grandes regiões e por esfera administrativa - Brasil -",datacnes))




hab_cnesiae <- hi_iae%>%
  left_join(est_iae, by = "CO_CNES")%>%
  group_by(CO_CNES)%>%
  summarize(habs_incs = n(),
            natureza = first(natureza),
            TP_UNIDADE = first(TP_UNIDADE))

##IAE = Interesse da atenção especializada
##Tab 1 **COM IAE**
hab_nat_leito <- hab_cnesiae%>%
  dplyr::filter(TP_UNIDADE %in% tp_unidade_cl$co_tipo_unidade)%>%
  left_join(tp_unidade_cl, by = c("TP_UNIDADE" = "co_tipo_unidade"))%>%
  rename(`Tipo de Estabelecimento` = ds_tipo_unidade)

hab_nat_leito$prop_habs <- 100*prop.table(hab_nat_leito$habs_incs)

hab_nat_leito <- hab_nat_leito%>%arrange(habs_incs)

hab_nat_leito$proph_acumulada <- cumsum(hab_nat_leito$prop_habs)


hab_nat_leito$prop_estabs <- cumsum(100*prop.table(1:nrow(hab_nat_leito)))


#https://towardsdatascience.com/abc-analysis-with-k-means-clustering-10d63ffff5b

modelo_estabs <- kmeans(hab_nat_leito$habs_incs,5)


hab_nat_leito$classe <-
  as.factor(case_when(modelo_estabs$cluster %in% c(1,3) ~ "Classe A",
                      modelo_estabs$cluster %in% c(4,5) ~ "Classe B",
                      T ~ "Classe C"))


hab_nat_leito$proph_acumulada <- 100-hab_nat_leito$proph_acumulada

hab_nat_leito$prop_estabs <- 100-hab_nat_leito$prop_estabs


hab_nat_leito%<>%arrange(desc(proph_acumulada))
resumo_hnl <- hab_nat_leito%>%group_by(classe)%>%summarize(proph_acumulada = first(proph_acumulada),prop_estabs = first(prop_estabs),
                                                           n_estabs = n())


resumo_hnl%<>%arrange(levels(classe))


curva_abc <- ggplot(hab_nat_leito%>%arrange(classe,desc(proph_acumulada)),aes(x = prop_estabs,y = proph_acumulada))+
  geom_rect(aes(xmin=resumo_hnl$prop_estabs[1],xmax =  resumo_hnl$prop_estabs[2], ymin=0, ymax= Inf,fill = paleta4[4]))+
  geom_rect(aes(xmin=resumo_hnl$prop_estabs[2],xmax =  100, ymin=0, ymax= Inf,fill = paleta4[7]))+
  geom_rect(aes(xmin=0,xmax =  resumo_hnl$prop_estabs[1], ymin=0, ymax= Inf,fill = paleta4[1]))+
  scale_fill_discrete(type = rev(paleta7))+
  geom_line()+
  geom_label(data = resumo_hnl,aes(y = 50, x = prop_estabs-6),label = paste(resumo_hnl$classe,"\n",resumo_hnl$n_estabs))+
  geom_vline(xintercept = resumo_hnl$prop_estabs[1])+
  geom_vline(xintercept = resumo_hnl$prop_estabs[2])+
  guides(fill = F)+
  xlab("Proporção no total de Estabelecimentos (%)")+
  ylab("Proporção no total de\nHabilitações/Incentivos (%)")+
  theme_minimal()



tab2 <- hab_nat_leito%>%
  group_by(classe,`Tipo de Estabelecimento`)%>%
  summarize(quantidade=n())%>%
  pivot_wider(names_from = classe, values_from = quantidade)

tab2[is.na(tab2)] <- 0



####Tabela 3

tab3 <- hab_nat_leito%>%
  inner_join(cnesvalidos%>%select(CO_CNES,CO_MUNICIPIO_GESTOR),
            by = "CO_CNES")%>%
  left_join(br_mun%>%
              select(idt,
                     UF = microrregiao.mesorregiao.UF.nome ,
                     `Região` = microrregiao.mesorregiao.UF.regiao.nome),
            by = c("CO_MUNICIPIO_GESTOR" = "idt"))%>%
  group_by(Região,UF,classe)%>%
  summarize(quantidade=n())

tab3_tot_uf <- tab3%>%
  summarize(quantidade=sum(quantidade))%>%
  mutate(classe = "Total")

tab3_tot_reg <- tab3%>%
  ungroup()%>%
  group_by(Região,classe)%>%
  summarize(quantidade=sum(quantidade))%>%
  mutate(UF = "Total")

tab3_trt <- tab3_tot_reg %>%summarize(quantidade = sum(quantidade))

tab3_trt %<>%mutate(UF = "Total", classe = "Total")

tab3%<>%bind_rows(tab3_tot_uf,tab3_tot_reg,tab3_trt)


tab3_trt$codigo <- 10*as.numeric(row.names(tab3_trt))


tab3 %<>% pivot_wider(names_from=classe, values_from = quantidade)

tab3%<>%arrange(Região,desc(UF))

tab3[is.na(tab3)] <- 0


####Tabela 4

tab4 <- hab_nat_leito%>%
  left_join(cnesvalidos%>%select(CO_CNES,CO_MUNICIPIO_GESTOR),
            by= "CO_CNES")%>%
  left_join(br_mun%>%
              select(idt,
                     UF = microrregiao.mesorregiao.UF.nome ,
                     `Região` = microrregiao.mesorregiao.UF.regiao.nome),
            by = c("CO_MUNICIPIO_GESTOR" = "idt"))%>%
  group_by(Região,UF,`Tipo de Estabelecimento`)%>%
  summarize(quantidade=n())

tab4_tot_uf <- tab4%>%
  summarize(quantidade=sum(quantidade))%>%
  mutate(`Tipo de Estabelecimento` = "Total")

tab4_tot_reg <- tab4 %>%
  ungroup() %>%
  group_by(Região,`Tipo de Estabelecimento`) %>%
  summarize(quantidade=sum(quantidade)) %>%
  mutate(UF = "Total")

tab4_trt <- tab4_tot_reg %>%summarize(quantidade = sum(quantidade))

tab4_trt %<>%mutate(UF = "Total", `Tipo de Estabelecimento` = "Total")

tab4%<>%bind_rows(tab4_tot_uf,tab4_tot_reg,tab4_trt)


tab4_trt$codigo <- 10*as.numeric(row.names(tab4_trt))


tab4 %<>%
  pivot_wider(names_from=`Tipo de Estabelecimento`, values_from = quantidade)

tab4%<>%arrange(Região,desc(UF))

tab4[is.na(tab4)] <- 0


####Tabela 5

tab5 <- hab_nat_leito%>%
  left_join(cnesvalidos%>%select(CO_CNES,CO_MUNICIPIO_GESTOR),
            by= "CO_CNES")%>%
  left_join(br_mun%>%
              select(idt,UF = microrregiao.mesorregiao.UF.nome ,
                     `Região` = microrregiao.mesorregiao.UF.regiao.nome),
            by = c("CO_MUNICIPIO_GESTOR" = "idt"))%>%
  group_by(Região,UF,`natureza`)%>%
  summarize(quantidade=n())

tab5_tot_uf <- tab5%>%summarize(quantidade=sum(quantidade))%>%
  mutate(`natureza` = "Total")

tab5_tot_reg <- tab5%>%
  ungroup()%>%
  group_by(Região,`natureza`)%>%
  summarize(quantidade=sum(quantidade))%>%
  mutate(UF = "Total")

tab5_trt <- tab5_tot_reg %>%summarize(quantidade = sum(quantidade))

tab5_trt %<>%mutate(UF = "Total", `natureza` = "Total")

tab5%<>%bind_rows(tab5_tot_uf,tab5_tot_reg,tab5_trt)


tab5_trt$codigo <- 10*as.numeric(row.names(tab5_trt))


tab5 %<>% pivot_wider(names_from=`natureza`, values_from = quantidade)

tab5%<>%arrange(Região,desc(UF))

tab5[is.na(tab5)] <- 0



hab_por_nat_com_leito <- hab_nat_leito %>%
  group_by(`Tipo de Estabelecimento`,natureza)%>%
  summarize( `Qtd Estabelecimentos` = n(),
             `Qtd habilitações` = sum(habs_incs,na.rm = T))

totais_sem_nat <- hab_por_nat_com_leito%>%
  summarize(`Qtd Estabelecimentos` = sum(`Qtd Estabelecimentos`),
            `Qtd habilitações` = sum(`Qtd habilitações`,na.rm = T))

totais_sem_nat <- cbind("natureza" = "Total",totais_sem_nat)%>%
  select(names(hab_por_nat_com_leito))

hab_por_nat_com_leito <- bind_rows(hab_por_nat_com_leito,totais_sem_nat)

tab1 <- hab_por_nat_com_leito%>%
  pivot_longer(-1:-2,"qtd",values_to="valor")%>%
  pivot_wider(names_from = c(natureza,qtd),values_from = "valor", names_sep="\n")
tab1[is.na(tab1)] <- 0

totais <- data.frame("Tipo de Estabelecimento"="Total",t(colSums(tab1[-1])))

tab1[nrow(tab1)+1,] <- totais

lin1 <- gsub("\n.*","",names(tab1))
lin1[c(1,3,5,7,9)] <- ""

lin1 <- gsub(" ","\n",lin1)

lin2 <- gsub(".*\n","",names(tab1))

lin2 <- gsub(" ", "\n",lin2)

t1tab <- as_hux(tab1)%>%insert_row(lin2)%>%
  insert_row(lin1)%>%merge_cells(1,2:3)%>%merge_cells(1,4:5)%>%
  merge_cells(1,6:7)%>%merge_cells(1,8:9)%>%
  set_align(row = 1:3,value="center")%>%set_width(value = "12cm")%>%
  set_font_size(1)

t1tab <- t1tab[-3,]




parapizza <- tab3%>%dplyr::filter(UF != "Total")%>%ungroup()%>%select(-1,-6)

nomsclasses <- c("Classe A","Classe B","Classe C")
#names(parapizza)[2:4] <- c("classe_a","classe_b","classe_c")


parapizza$UF <- str_to_title(parapizza$UF)

parapizza <- mapag%>%
  select(name_state,name_region,geom)%>%
  left_join(parapizza, by = c("name_state" = "UF"))

parapizza <- cbind(parapizza,st_coordinates(st_centroid(parapizza$geom)))

parapizza <- as.data.frame(parapizza%>%select(-"geom"))

parapizza <- parapizza[-8]

parapizza[is.na(parapizza)] <- 0

parapizza%<>%mutate(across(where(is.character),as.factor))

sipp <- as.data.frame(parapizza%>%
                        select(contains("classe"),X,Y,name_region))

names(sipp)[1:3] <- nomsclasses

sipp$total = rowSums(sipp[1:3])

sipp$tam =  ((sipp$total-min(sipp$total))/max(sipp$total))+0.5

mapa2base_col <- geobr::read_state()%>%left_join(st_drop_geometry(iae_uf[c("code_state","Leitos por 1.000 habitantes")]))

#mapa2base_col$`Leitos por mil habitantes` <- cut(mapa2base_col$`Leitos por 1.000 habitantes`,c(0,2,4,6,8,10))

mapa2 <- ggplot(mapa2base_col)+
  geom_sf(aes(fill=`Leitos por 1.000 habitantes`))+
  labs(fill = "Leitos*/1.000 hab")+
  scale_fill_gradientn(colors = rev(paleta2[-length(paleta2)]),labels = scales::number_format(big.mark = ".",decimal.mark=",",acurracy = 0.01))+
  # geom_sf(data = iae_uf,inherit.aes = F ,
  #          aes(col = `Estabelecimentos por 100.000 habitantes`), alpha = 0.5)+
  new_scale_fill()+
  geom_scatterpie(aes(x=`X`,y=`Y`, r = tam),
                  data=(sipp),
                  cols=rev(nomsclasses) ,alpha = 0.8,
                  legend_name = "Estabelecimentos")+
  geom_scatterpie_legend(sipp$tam, x=-66, y=-24,labeller = function(x) {round((x-0.5)*max(sipp$total)+min(sipp$total),0)})+
  scale_fill_manual(values = paleta7[c(5,9,3)])+
  theme_minimal()+theme(axis.text = element_blank(),
                        axis.title = element_blank(),
                        legend.position = "bottom")+labs(fill = "Classe")




##Variáveis de texto corrido pendentes
modalidades_h <- hab_e_inc%>%group_by(TP_HABILITACAO,COD_SUB_GRUPO_HABILITACAO)%>%summarize(modalidades=n())%>%summarize(modalidades=n())

mod_iae <- anx%>%count(tipo)

habs_sfl <- 100*(tab1[[7]]/tab1[[9]])[6]

habs_pub <- 100*(tab1[[3]]/tab1[[9]])[6]

abc_prop <- hab_nat_leito%>%group_by(classe)%>%summarize(qtde_hi = sum(habs_incs),prop_hi = 100*qtde_hi/sum(hab_nat_leito$habs_incs))


save.image(file="relatorio/dados.RData")
