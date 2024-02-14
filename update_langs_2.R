install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)

if (!dir.exists("outputs")) {dir.create("outputs")}

# lê arquivo original de tabs
documents_languages <- read_csv("tabs/documents_languages.csv")
print("Quantidade de linhas de documents_languages.csv")
print(nrow(documents_languages))

# lê arquivo com dados obtidos da base de dados www.scielo.br
documents_languages_complementar <- read_csv("tabs/documents_languages-complementar-2024-02-14.csv")
print("Quantidade de linhas de documents_languages-complementar-2024-02-14.csv")
print(nrow(documents_languages_complementar))

# renomeia as colunas para facilitar o código
dl <- documents_languages %>%
  rename('dl_dlangs'='document languages',
         'dl_pt'='document pt', 
         'dl_es'='document es', 
         'dl_en'='document en', 
         'dl_oth'='document other languages')

# renomeia as colunas para facilitar o código
# seleciona as colunas "pid_v2", 'dlangs', 'pt', 'es', 'en', 'oth'
dc <- documents_languages_complementar %>%
  rename('dlangs'='document languages',
         'pt'='document pt', 
         'es'='document es', 
         'en'='document en', 
         'oth'='document other languages') %>%
  select("pid_v2", 'dlangs', 'pt', 'es', 'en', 'oth') %>% unique()
print("Quantidade de linhas de documents_languages-complementar-2024-02-14.csv sem duplicidade")
print(nrow(dc))

# procura inconsistências em dc
dc_duplicated_pids <- dc %>% select('pid_v2') %>% filter(duplicated(pid_v2))
dc_rows_with_duplicated_pids <- left_join(dc_duplicated_pids, dc, by="pid_v2")
print("Quantidade de linhas inconsistentes")
print(nrow(dc_rows_with_duplicated_pids))
# guarda as inconsistências em arquivo
write.table(dc_rows_with_duplicated_pids, file = "outputs/documents_languages_inconsistencias.csv",
            sep = ",",
            na = "",
            quote = TRUE,
            row.names = F)
# exclui as linhas inconsistentes de dc (complementar)
dc <- dc %>% anti_join(dc_rows_with_duplicated_pids)

# completa com os idiomas dl com dc
joined <- left_join(dl, dc, by = c("document publishing ID (PID SciELO)"="pid_v2"))

# identifca os registros que tem diferença nos idiomas
is_diff <- joined %>%  mutate(
  diff=case_when(
    (! is.na(pt) & dl_pt != pt)  | (! is.na(es) & dl_es != es)  | (! is.na(en) & dl_en != en)  | (! is.na(oth) & dl_oth != oth) ~ "diff"
  )) %>% filter(diff=="diff")
print("Quantidade de linhas com diferenças nos idiomas")
print(nrow(is_diff))
# guarda diferenças em arquivo
write.table(is_diff, file = "outputs/documents_languages_diff.csv",
            sep = ",",
            na = "",
            quote = TRUE,
            row.names = F)

# completa os idiomas das linhas que há diferença
result <- joined %>% mutate(
  dl_dlangs=case_when(
    ! is.na(dlangs) & dl_dlangs != dlangs ~ dlangs,
    TRUE ~ dl_dlangs
  ),
  dl_pt=case_when(
    !is.na(pt) & dl_pt!= pt ~ pt,
    TRUE ~ dl_pt
  ),
  dl_es=case_when(
    !is.na(es) & dl_es!= es ~ es,
    TRUE ~ dl_es
  ),
  dl_en=case_when(
    !is.na(en) & dl_en!= en ~ en,
    TRUE ~ dl_en
  ),
  dl_oth=case_when(
    !is.na(oth) & dl_oth!= oth ~ oth,
    TRUE ~ dl_oth
  )) %>%  
  select(-c("dlangs", "pt", "en", "es", "oth"))   %>% 
  mutate(
    dl_pt=as.character(dl_pt),
    dl_es=as.character(dl_es),
    dl_en=as.character(dl_en),
    dl_oth=as.character(dl_oth),
  ) %>%
  rename('document languages'='dl_dlangs',
         'document pt'='dl_pt', 
         'document es'='dl_es', 
         'document en'='dl_en',
         'document other languages'='dl_oth')

print("result")
print(nrow(result))
# guarda resultado em documents_languages_completado.csv
write.table(result, file = "outputs/documents_languages_completado.csv",
            sep = ",",
            na = "",
            quote = TRUE,
            row.names = F)


