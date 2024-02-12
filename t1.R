# - col 1: nome do periódico
# - col 2: ano 
# - col 3: número de docs
# - col 4: número de docs citáveis

install.packages("tidyverse")
library(tidyverse)
library(readr)

documents_counts <- read_csv("tabs/documents_counts.csv")
spec(documents_counts)
dc <- select(documents_counts, 'title current status', 'title at SciELO', 'document publishing year', 'document is citable');
dc <- dc %>%
  rename("c" = "document is citable") %>%
  rename("title" = "title at SciELO") %>%
  rename("status" = "title current status") %>%
  rename("year" = "document publishing year")

sumarized_dc <- dc %>%
  mutate(not_c=case_when(
    c == 1 ~ 0,
    TRUE ~ 1)) %>%
  group_by(title, year, status) %>%
  summarise(cit=sum(c), not_cit=sum(not_c))

if (!dir.exists("outputs")) {dir.create("outputs")}

write.table(sumarized_dc, file = "outputs/para_tabela1.csv",
            sep = ",",
            row.names = F)


