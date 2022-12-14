# Estudo de habilitações MS

## Atualização de estudo realizado em 2020

## Complementação do estudo realizado em 2020, em 2022

Estudo de distribuição de estabelecimentos com leitos de internações com habilitações ou incentivos de interesse à atenção especializada à saúde


# INTRODUÇÃO

Em seguimento à proposta de 'identificar a capacidade instalada da média e alta complexidade de saúde', a presente nota técnica aborda a distribuição dos estabelecimentos de saúde com leitos de internação, com foco nos detentores de habilitações ou incentivos do Ministério da Saúde, vigentes em dezembro de 2022, e nos estabelecimentos tipificados como Hospitais Gerais ou Especializados, Prontos-Socorro Gerais ou Especializados e Unidades Mistas.

A capacidade instalada considerada em pauta, em linha ao estudo anterior, foi a do conjunto de instituições que ofertam e produzem serviços de saúde para o Sistema Único de Saúde (SUS).

A compreensão expressa pela Portaria SAS/MS nº 414, de 11 de agosto de 2005 [@brasil2005b], que define habilitação de serviços como **“o ato do gestor municipal, estadual ou federal autorizar um estabelecimento de saúde já credenciado do SUS a realizar procedimentos constantes das tabelas do SUS, vinculados a normalizações específicas”**.

Por questões de escopo, não foram consideradas as características inerentes de cada tipo de estabelecimento bem como a capacidade de resposta em relação a doenças específicas. Logo, o ponto de partida do estudo, das habilitações, oferece um dimensionamento parcial da capacidade de média e alta complexidade vinculada ao SUS.


Em particular, muitos estabelecimentos de saúde são contratualizados pelos gestores locais e não habilitados pelo Ministério da Saúde. _Esse estudo, portanto, restringir-se-á aos aspectos quantitativos da capacidade instalada em uma única dimensão: tipos de estabelecimentos com habilitações vigentes de interesse da atenção especializada à saúde_. Além do quantitativo de habilitações, incidir-se-á sobre a quantidade de leitos vinculados e não vinculados às mesmas pelos estabelecimentos ora analisados.



# Como utilizar o repositório

Após clonar ou baixar o repositório, é possível executar todas as etapas de cálculos em gráficos, tabelas e mapas, além do apresentado ao longo do texto.

Na pasta R constam os scripts. O principal deles é `calculos.R`, que, dada a necessidade de acesso a Bancos de Dados internos ao Ministério da Saúde, utiliza o script `conexões a oracle.R` .

O usuário deve inserir suas credenciais de acesso em arquivo na pasta raiz do repositório clonado ou pasta baixada e descompactada em arquivo com nome `.Renviron` com o seguinte conteúdo:
```
usrjp = "SEUUSUARIO"

senharjp = "SUA SENHA DE ACESSO"
```

Uma vez realizados os cálculos, os objetos ficarão salvos na pasta relatorio, em arquivo de nome `dados.RData` .

Na pasta relatório, consta o script de geração do presente estudo em formato docx:
`relatorio_com_leitos.RMD` . 

Após os cálculos, executar esse script gerará o arquivo 'relatorio_com_leitos.docx' 
