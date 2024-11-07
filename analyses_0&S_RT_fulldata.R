library(tidyverse)
library(RPostgres)
library(readxl)
library(writexl)
library(stringr)
library(scales)
library(ggplot2) ##for figures
library(showtext) ##for figures
library(patchwork) ##for figures
library(ggpubr) ##for figures
library(ggsci) ##for figures
library(reshape2)
library(tidycomm) ##forintercodercomparison
library(vctrs)
library(svglite)

showtext_auto()
cbPalette <- c("#009E73", "#FAEBD7", "#56B4E9", "#FFFFFF", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbPalette2 <- c("#FFFFFF", "#009E73", "#FAEBD7", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#Connect to QSS database

{
  if(exists("db")) { 
    dbDisconnect(db) 
    rm(db)
  }
  db = dbConnect(Postgres(),
                 user = read.csv("admin/db_connection.csv")$user,
                 password = read.csv("admin/db_connection.csv")$pw,
                 host = read.csv("admin/db_connection.csv")$host,
                 port = 5432,
                 dbname = "postgres",
                 options="-c search_path=projectdb_ofi")
}

##Download data from QSS databases

full_works<-dbReadTable(db, "works") #data for all works
core_works<-dbReadTable(db, "core_works") #data for core works
works_scores<-dbReadTable(db, "works_scores") #data about work scores to see inclusion in report
#authors<-dbReadTable(db, "authors")
#authors_works<-dbReadTable(db, "authors_works")

coreworks_lists <- unique(core_works$openalex_work_id) #get the list of openalex id for core works in report

fullworks_core <- full_works %>%
  mutate(iscore = if_else(openalex_work_id  %in% core_works$openalex_work_id, "core", "noncore")) #create new column adding if document is core or noncore



#Prepare data for source analysis
test_core <- fullworks_core %>% #get source data (n and prop) from core works
  filter(iscore == "core") %>%
  group_by(source_name) %>%
  summarise(n_incore = n()) %>% #nb of core documents by source
  ungroup() %>%
  mutate(n_incore_total = sum(n_incore), #nb of total core documents
         prop_incore = n_incore/n_incore_total*100) #nb of prop
            
source_data <- fullworks_core %>%
  filter(!is.na(source_name)) %>%
  group_by(source_name, iscore) %>%
  summarise(n_papers = n()) %>%
  ungroup() %>%
  group_by(iscore) %>%
  mutate(n_core = sum(n_papers), prop = n_papers/n_core*100) %>%
  ungroup() %>%
  group_by(source_name) %>%
  mutate(n_source = sum(n_papers), n_total = sum(n_core), prop_total = n_source/n_total*100) %>%
  left_join(test_core, by = "source_name") %>%
  mutate(diff_prop = prop_incore-prop_total) %>%
  filter(iscore == "noncore") %>%
  arrange(desc(n_source))

source_data_noncore <- source_data %>%
  filter(is.na(n_incore))

#Get openalex info for core works
core_works <- core_works %>%
  left_join(full_works %>% select(openalex_work_id, source_name), by = "openalex_work_id")

#Set ggplot2 theme
theme_set(theme_light(base_size = 10))



###Top Journals - Figure 2
#create plot for journals
source_data$source_name <- gsub("Proceedings of the National Academy of Sciences of the United States of America", "PNAS", source_data$source_name)
source_data$source_name <- gsub("\\(Cold Spring Harbor Laboratory\\)", "", source_data$source_name)
source_data$source_name <- gsub("Deep-sea research. Part 2. Topical studies in oceanography/Deep sea research. Part II, Topical studies in oceanography", "Deep-sea Research. Part 2", source_data$source_name)
source_data$source_name <- gsub("Frontiers in marine science", "Frontiers in Marine Science", source_data$source_name)
source_data$source_name <- gsub("PloS one", "PLOS ONE", source_data$source_name)
source_data$source_name <- gsub("Scientific reports", "Scientific Reports", source_data$source_name)
source_data$source_name <- gsub("Marine pollution bulletin", "Marine Pollution Bulletin", source_data$source_name)
source_data$source_name <- gsub("Marine policy", "Marine Policy", source_data$source_name)
source_data$source_name <- gsub("Marine ecology. Progress series", "Marine Ecology Progress Series", source_data$source_name)
source_data$source_name <- gsub("Science of the total environment", "Science of the Total Environment", source_data$source_name)
source_data$source_name <- gsub("Geophysical research letters", "Geophysical Research Letters", source_data$source_name)

p1 <- source_data %>%
  arrange(desc(prop_incore)) %>%
  head(10) %>%
  ggplot() + 
  geom_col(aes(x = reorder(source_name, prop_incore), y = prop_incore), width = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  labs(title = "Top journals in core dataset", x = "", y = "") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic"),
    legend.text = element_text(margin = margin(r = 10))
  )

p2 <- source_data %>%
  arrange(desc(prop_total)) %>%
  head(10) %>%
  ggplot() + 
  geom_col(aes(x = reorder(source_name, prop_total), y = prop_total), width = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 4), expand = c(0, 0)) +
  labs(title = "Top journals in full dataset", x = "", y = "", fill = "") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic")
  )


#combine plot
p3 <- ggarrange(p1, p2, nrow = 2, common.legend = TRUE, legend = "none") 
p3


###Journals Weight
source_data %>%
  filter(!is.na(n_incore)) %>%
  ggplot() +
  geom_col(aes(x = reorder(source_name, prop_incore), y = prop_total, color = "Full dataset"), show.legend = FALSE) +
  geom_point(aes(x = reorder(source_name, prop_incore), y = prop_incore, group=source_name, color = "Docs in WOAII report")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 5), expand = c(0, 0)) +
  labs(title = "Relative weight of journals in report dataset", x = "Journals", y = "Relative weight of journals (%)", colour = "")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.position = "bottom",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "gray50", linewidth = .1))

source_data %>%
  filter(!is.na(n_incore)) %>%
  group_by(source_name) %>%
  ggplot() +
  geom_col(aes(x = reorder(source_name, diff_prop), y = diff_prop), show.legend = FALSE) +
#  geom_point(aes(x = reorder(source_name, prop_incore), y = prop_incore, group=source_name, color = "Docs in WOAII report")) +
  coord_flip() +
  scale_y_continuous(limits = c(-2, 3), expand = c(0, 0)) +
  labs(title = "Relative weight of journals in report dataset", x = "Journals", y = "Relative weight of journals (%)", colour = "")+
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.key = element_rect(fill = "white"),
        legend.position = "bottom",
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "gray50", linewidth = .1))

###Preparing tables with papers per dimensions of inclusions
works_scores2 <- works_scores %>%
  filter(core_doc =="noncore") %>%
  group_by(openalex_work_id) %>%
  summarise(cited_count = sum(cited_count),
            citing_count = sum(citing_count),
            coauthor_count = sum(coauthor_count),
            keywords_score = sum(keywords_score),
            journal_score = sum(core_journal))

set.seed(2024)

#####Create sampling tables
scores_5 <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_5, "scores_5.csv", row.names = F)

scores_4cdcgcakw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_4cdcgcakw, "scores_4cdcgcakw.csv", row.names = F)

scores_4cdcgcajo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_4cdcgcajo, "scores_4cdcgcajo.csv", row.names = F)

scores_4cdcgkwjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_4cdcgkwjo, "scores_4cdcgkwjo.csv", row.names = F)

scores_4cdcakwjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_4cdcakwjo, "scores_4cdcakwjo.csv", row.names = F)

scores_4cgcakwjo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_4cgcakwjo, "scores_4cgcakwjo.csv", row.names = F)

scores_3cdcgca <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cdcgca, "scores_3cdcgca.csv", row.names = F)

scores_3cdcgkw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cdcgkw, "scores_3cdcgkw.csv", row.names = F)

scores_3cdcgjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cdcgjo, "scores_3cdcgjo.csv", row.names = F)

scores_3cdcakw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cdcakw, "scores_3cdcakw.csv", row.names = F)

scores_3cdcajo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cdcajo, "scores_3cdcajo.csv", row.names = F)

scores_3cdkwjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cdkwjo, "scores_3cdkwjo.csv", row.names = F)

scores_3cgcakw <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cgcakw, "scores_3cgcakw.csv", row.names = F)

scores_3cgcajo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cgcajo, "scores_3cgcajo.csv", row.names = F)

scores_3cgkwjo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_3cgkwjo, "scores_3cgkwjo.csv", row.names = F)

scores_2cdcg <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_2cdcg, "scores_2cdcg.csv", row.names = F)

scores_2cdca <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_2cdca, "scores_2cdca.csv", row.names = F)

scores_2cdkw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_2cdkw, "scores_2cdkw.csv", row.names = F)

scores_2cdjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_2cdjo, "scores_2cdjo.csv", row.names = F)

scores_2cgca <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_2cgca, "scores_2cgca.csv", row.names = F)

scores_2cgkw <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_2cgkw, "scores_2cgkw.csv", row.names = F)

scores_2cgjo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_2cgkw, "scores_2cgkw.csv", row.names = F)

scores_1cd <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_1cd, "scores_1cd.csv", row.names = F)

scores_1cg <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  sample_n(100)
#write.csv2(scores_1cg, "scores_1cg.csv", row.names = F)

scores_sampling <- bind_rows(scores_5, scores_4cdcgcakw, scores_4cdcgcajo, scores_4cdcgkwjo, scores_4cdcakwjo, scores_4cgcakwjo, scores_3cdcgca, scores_3cdcgkw, scores_3cdcgjo, scores_3cdcakw, scores_3cdcajo, scores_3cdkwjo, scores_3cgcakw, scores_3cgcajo, scores_3cgkwjo, scores_2cdcg, scores_2cdca, scores_2cdkw, scores_2cdjo, scores_2cgca, scores_2cgkw, scores_2cgjo, scores_1cd, scores_1cg)
write.csv2(scores_sampling, "sampling_full_works.csv", row.names = F)

scores_sampling2 <- scores_sampling %>%
  select(openalex_work_id, title, abstract) %>%
  arrange(openalex_work_id)
write.csv2(scores_sampling2, "sampling_works.csv", row.names = F, fileEncoding = "UTF-8")


##Prepare files for intercoder assessment
poppy <- read_excel("data/Papers_sampling/Test/sampling_works_poppy_test.xlsx", sheet="sampling_works", col_names = TRUE) %>%
  rename(code_rev2 = coding)
test_file <- read_excel("data/Papers_sampling/Test/sampling_works_test.xlsx", sheet="sampling_works", col_names = TRUE) %>%
  rename(code_rev = Coding) %>%
  left_join(poppy %>% select(openalex_work_id, code_rev2), by = "openalex_work_id") %>%
  select(openalex_work_id, code_rev, code_rev2) %>%
  pivot_longer(cols = !openalex_work_id, names_to = "coder", values_to = "code") %>%
  filter(!is.na(code))

test800 <- test_file %>%
  test_icr(openalex_work_id, coder, levels = c(code = "ordinal"),  cohens_kappa = T)

test_file_ver <- read_excel("data/Papers_sampling/Test/sampling_works_review_remi.xlsx", sheet="test_file_check_800", col_names = TRUE) %>%
  left_join(poppy %>% select(openalex_work_id, code_rev2), by = "openalex_work_id") %>%
  group_by(openalex_work_id) %>%
  filter(code_rev != code_rev2)

write.csv2(test_file_ver, "test_file_check_final_rev.csv", row.names = F, fileEncoding = "UTF-8")

##Upload data from sample coding
coded_papers <- read_excel("data/Papers_sampling/sampling_works.xlsx", sheet="sampling_works", col_names = TRUE) %>%
  mutate(Coding = if_else(Coding == 0,"No relation", if_else(Coding == 1, "Indirect relation", if_else(Coding == 2, "Direct relation", ""))))

full_sampled_works <- read.csv2("data/Papers_sampling/sampling_full_works.csv", header = T) %>%
  left_join(coded_papers %>% select(openalex_work_id, Coding), by = "openalex_work_id") %>%
  mutate(Dimensions = if_else(
    cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0, "CdCgCaKwJo", (
      if_else(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0, "CdCgCaKw", 
        if_else(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0, "CdCgCaJo", 
          if_else(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0, "CdCgKwJo", 
            if_else(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0, "CdCaKwJo", 
              if_else(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0, "CgCaKwJo", 
                if_else(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0, "CdCgCa", 
                  if_else(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0, "CdCgKw", 
                    if_else(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0, "CdCaKw", 
                      if_else(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0, "CgCaKw",
                        if_else(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0, "CdCgJo",
                          if_else(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0, "CdCaJo",
                            if_else(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0, "CgCaJo",
                              if_else(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0, "CgKwJo",
                                if_else(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0, "CdKwJo",
                                  if_else(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0, "CdCg",
                                    if_else(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0, "CdCa",
                                      if_else(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0, "CdKw",
                                        if_else(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0, "CdJo",
                                          if_else(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0, "CgCa",
                                            if_else(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0, "CgKw",
                                              if_else(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0, "CgJo",
                                                if_else(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0, "Cg",
                                                  if_else(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0, "Cd", "")
                                                                              ))
                                                                      ))
                                                              ))
                                                      ))
                                              ))
                                      ))
                              ))
                      ))
              ))
      ))
    ))
  ))
  ) %>%
  mutate(Dimensions = factor(Dimensions, levels = c("Cg", "Cd", "CgCa", "CdCa", "CdCg", "CgJo", "CdJo", "CgKw", "CdKw", "CdCgCa", "CdCgKw", "CdCgJo",	"CgCaJo", "CgCaKw",	"CgKwJo",	"CdCaJo", "CdCaKw", "CdKwJo", "CdCgCaKw", "CdCgCaJo", "CdCgKwJo", "CgCaKwJo", "CdCaKwJo", "CdCgCaKwJo"))) %>%
  mutate(Coding = factor(Coding, levels = c("No relation", "Indirect relation", "Direct relation"))) %>%
  mutate(CodeScore = if_else(Coding == "Direct relation", "2", if_else(Coding == "Indirect relation", "1", if_else(Coding == "No relation", "0", "")))) %>%
  mutate_at(c("CodeScore"), as.numeric) %>%
  group_by(Dimensions) %>%
  mutate(Score = sum(CodeScore/n()*50)) %>%
  ungroup()

##Calculate performance score of combinations
performance_table <- full_sampled_works %>%
  select(Dimensions, Score) %>%
  distinct(Dimensions, Score)

##Creating Figure 4 - Results of sample coding
plot <- full_sampled_works %>%
  ggplot(aes(x = reorder(Dimensions, Score), fill = Coding), show.legend = T) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=cbPalette) +
  labs(x = "Document-level combinations of criteria", y = "Sample number of documents categorized to ocean research", fill = "") +
#  annotate(geom = "text", x = "", y = 1.02, 
#           label = "", hjust = 0) +
#  annotate(geom = "text", x = "Factors", y = 1.02, 
#           label = "NPapers (Coding Score)", hjust = 0) +
  annotate(geom = "text", x = "CdCgCaKwJo", y = 1.02, 
           label = "5,694 (98.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCgCaKw", y = 1.02, 
           label = "9,160 (91.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCaKwJo", y = 1.02, 
           label = "12,615 (99.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CgCaKwJo", y = 1.02, 
           label = "26,403 (95.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCgKwJo", y = 1.02, 
           label = "1,455 (95.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCgCaJo", y = 1.02, 
           label = "170 (90.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdKwJo", y = 1.02, 
           label = "10,252 (94.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCaKw", y = 1.02, 
           label = "20,040 (90)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCaJo", y = 1.02, 
           label = "973 (86.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CgKwJo", y = 1.02, 
           label = "30,729 (86.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CgCaKw", y = 1.02, 
           label = "55,195 (86.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CgCaJo", y = 1.02, 
           label = "1,115 (84.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCgKw", y = 1.02, 
           label = "2,758 (78.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCgJo", y = 1.02, 
           label = "84 (78.3)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCgCa", y = 1.02, 
           label = "657 (54.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdKw", y = 1.02, 
           label = "23,915 (72.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CgKw", y = 1.02, 
           label = "117,619 (70.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdJo", y = 1.02, 
           label = "1,508 (68.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CgJo", y = 1.02, 
           label = "3,631 (53.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCg", y = 1.02, 
           label = "460 (46.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CdCa", y = 1.02, 
           label = "4,370 (39.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "CgCa", y = 1.02, 
           label = "8,207 (36.5)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "Cd", y = 1.02, 
           label = "16,802  (27.0)", hjust = 0, size = 18/.pt) +
  annotate(geom = "text", x = "Cg", y = 1.02, 
           label = "61,725  (26.5)", hjust = 0, size = 18/.pt) +
  geom_vline(xintercept = c(7.5), linewidth = 1.5, linetype = "dashed") +
  geom_vline(xintercept = c(10.5), linewidth = 1.5, linetype = "dashed", color = "#555555") +
  geom_vline(xintercept = c(16.5), linewidth = 1.5, linetype = "dashed", color = "#999999") +
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linewidth = 0.5, color = "#999999") +
  annotate(geom = "text", x = 7.8, y = .5, label = "60% threshold", size = 14/.pt) +
  annotate(geom = "text", x = 10.8, y = .5, label = "75% threshold", size = 14/.pt) +
  annotate(geom = "text", x = 16.8, y = .5, label = "90% threshold", size = 14/.pt) +
  coord_flip(ylim = c(0,1.12), expand = F, clip = "off") +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 20),
        legend.key.spacing.x = unit(30, "pt"),
        axis.title.x=element_text(size=24,face="bold", margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y=element_text(size=24,face="bold", margin = unit(c(0, 3, 0, 0), "mm")),
        axis.text.y=element_text(size = 18),
        axis.text.x=element_text(size = 18),
        axis.ticks.x=element_blank(),
        axis.ticks.y= element_blank(),
        panel.grid = element_blank())
plot

plot +
  geom_text(aes(x = 0, y = 1.02,
        label = "Total N of docs\nper combination\n(Performance\nscore (out of 100)\nof combination)"),
            family = "Helvetica-Narrow", stat = "unique", size = 18/.pt, hjust = 0, vjust = 1.3, lineheight = 0.7)

ggsave("tables_and_figures/figure1_poster_nwb.svg", plot, width = 52, height = 28, units = "cm")

##Preparing full tables of work per combinations
scores_5 <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_5, "scores_5.csv", row.names = F)

scores_4cdcgcakw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_4cdcgcakw, "scores_4cdcgcakw.csv", row.names = F)

scores_4cdcgcajo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_4cdcgcajo, "scores_4cdcgcajo.csv", row.names = F)

scores_4cdcgkwjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_4cdcgkwjo, "scores_4cdcgkwjo.csv", row.names = F)

scores_4cdcakwjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_4cdcakwjo, "scores_4cdcakwjo.csv", row.names = F)

scores_4cgcakwjo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_4cgcakwjo, "scores_4cgcakwjo.csv", row.names = F)

scores_3cdcgca <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cdcgca, "scores_3cdcgca.csv", row.names = F)

scores_3cdcgkw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cdcgkw, "scores_3cdcgkw.csv", row.names = F)

scores_3cdcgjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cdcgjo, "scores_3cdcgjo.csv", row.names = F)

scores_3cdcakw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cdcakw, "scores_3cdcakw.csv", row.names = F)

scores_3cdcajo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cdcajo, "scores_3cdcajo.csv", row.names = F)

scores_3cdkwjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cdkwjo, "scores_3cdkwjo.csv", row.names = F)

scores_3cgcakw <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cgcakw, "scores_3cgcakw.csv", row.names = F)

scores_3cgcajo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cgcajo, "scores_3cgcajo.csv", row.names = F)

scores_3cgkwjo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_3cgkwjo, "scores_3cgkwjo.csv", row.names = F)

scores_2cdcg <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_2cdcg, "scores_2cdcg.csv", row.names = F)

scores_2cdca <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_2cdca, "scores_2cdca.csv", row.names = F)

scores_2cdkw <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_2cdkw, "scores_2cdkw.csv", row.names = F)

scores_2cdjo <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_2cdjo, "scores_2cdjo.csv", row.names = F)

scores_2cgca <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count > 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_2cgca, "scores_2cgca.csv", row.names = F)

scores_2cgkw <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score > 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_2cgkw, "scores_2cgkw.csv", row.names = F)

scores_2cgjo <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score > 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_2cgkw, "scores_2cgkw.csv", row.names = F)

scores_1cd <- works_scores2 %>%
  filter(cited_count > 0 & citing_count  == 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_1cd, "scores_1cd.csv", row.names = F)

scores_1cg <- works_scores2 %>%
  filter(cited_count == 0 & citing_count  > 0 & coauthor_count == 0 & keywords_score == 0 & journal_score == 0) %>%
  left_join(full_works, by = "openalex_work_id") #%>%
#  sample_n(100)
#write.csv2(scores_1cg, "scores_1cg.csv", row.names = F)

##Inclusion of papers in different threshold

scores_good60 <- bind_rows(scores_5, scores_4cgcakwjo, scores_4cdcgkwjo, scores_4cdcgcakw, scores_4cdcgcajo, scores_4cdcakwjo, scores_3cdcajo, scores_3cdcgjo, scores_3cdkwjo, scores_3cgcajo, scores_3cgkwjo, scores_3cdcgkw, scores_3cdcakw, scores_3cgcakw, scores_2cdjo, scores_2cdkw, scores_2cgkw)
scores_good75 <- bind_rows(scores_5, scores_4cgcakwjo, scores_4cdcgkwjo, scores_4cdcgcakw, scores_4cdcgcajo, scores_4cdcakwjo, scores_3cdcajo, scores_3cdcgjo, scores_3cdkwjo, scores_3cgcajo, scores_3cgkwjo, scores_3cdcgkw, scores_3cdcakw, scores_3cgcakw)
scores_good90 <- bind_rows(scores_5, scores_4cgcakwjo, scores_4cdcgkwjo, scores_4cdcgcakw, scores_4cdcgcajo, scores_4cdcakwjo, scores_3cdkwjo, scores_3cdcakw)

works_include60 <- scores_good60$openalex_work_id
works_include75 <- scores_good75$openalex_work_id
works_include90 <- scores_good90$openalex_work_id

#write.csv2(scores_good90, "filtered_dataset_90.csv", row.names = F)

##Full dataset analysis after segmentation

n_distinct(scores_good90$source_name)
n_distinct(scores_good75$source_name)
n_distinct(scores_good60$source_name)

scores_good90 %>%
  count(source_name) %>%
  arrange(desc(n))

scores_good75 %>%
  count(source_name) %>%
  arrange(desc(n))

scores_good60 %>%
  count(source_name) %>%
  arrange(desc(n))

##Checking for papers by chapter

works_90 <- works_scores %>%
  filter(openalex_work_id %in% works_include90 & woaii_chapter != 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  group_by(woaii_chapter) %>%
  summarise(Ref_90 = n())

works_75 <- works_scores %>%
  filter(openalex_work_id %in% works_include75 & woaii_chapter != 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  group_by(woaii_chapter) %>%
  summarise(Ref_75 = n())

works_60 <- works_scores %>%
  filter(openalex_work_id %in% works_include60 & woaii_chapter != 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  group_by(woaii_chapter) %>%
  summarise(Ref_60 = n())

works_full <- works_scores %>%
  filter(woaii_chapter != 0) %>%
  left_join(full_works, by = "openalex_work_id") %>%
  group_by(woaii_chapter) %>%
  summarise(Ref_Full = n())

works_by_chapter <- works_full %>%
  left_join(works_90, by = "woaii_chapter") %>%
  left_join(works_75, by = "woaii_chapter") %>%
  left_join(works_60, by = "woaii_chapter") %>%
  group_by(woaii_chapter) %>%
  mutate(prop90 = round(Ref_90/Ref_Full, digits = 4)*100) %>%
  mutate(prop75 = round(Ref_75/Ref_Full, digits = 4)*100) %>%
  mutate(prop60 = round(Ref_60/Ref_Full, digits = 4)*100) %>%
  mutate(propout = (1 - round(Ref_60/Ref_Full, digits = 4))*100) %>%
  ungroup() %>%
  mutate(woaii_chapter = factor(woaii_chapter, levels= str_sort(woaii_chapter, numeric=T))) %>%
  arrange(woaii_chapter)

works_by_chapter2 <- works_scores %>%
  filter(woaii_chapter != 0) %>%
  mutate(Threshold = if_else(openalex_work_id %in% works_include90, "90%", if_else(openalex_work_id %in% works_include75, "75%", if_else(openalex_work_id %in% works_include60, "60%", "Out")))) %>%
  left_join(works_by_chapter, by = "woaii_chapter") %>%
  mutate(Threshold = factor(Threshold, levels = c("Out", "60%", "75%", "90%")))

works_by_chapter2 %>%
  ggplot(aes(x = reorder(woaii_chapter, -propout), fill = Threshold), show.legend = T) +
  geom_bar(position = "fill") +
  scale_fill_manual(values=cbPalette2) +
  labs(x = "Chapters", y = "", fill = "") +
  coord_flip(ylim = c(0,1), expand = F, clip = "off") +
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 11),
        legend.key.spacing.x = unit(30, "pt"),
        axis.text.y=element_text(size = 11),
        axis.text.x=element_text(size = 8),
        axis.ticks.x=element_blank(),
        axis.ticks.y= element_blank(),
        panel.grid = element_blank())



write.csv2(works_by_chapter, "works_chapter.csv", row.names = F)

works_by_chapter %>%
  ggplot() +
  geom_col(aes(x = reorder(woaii_chapter, Ref_90), y = Ref_60, fill = "N ref (60%)")) +
  geom_col(aes(x = reorder(woaii_chapter, Ref_90), y = Ref_75, fill = "N ref (75%)")) +
  geom_col(aes(x = reorder(woaii_chapter, Ref_90), y = Ref_90, fill = "N ref (90%)")) +
  labs(title = "", x = "Chapters", y = "", fill = "") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values=cbPalette) +
  coord_flip() +
  theme(#plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.spacing.x = unit(30, "pt"),
    axis.text.y=element_text(face = "bold", size = 10),
    axis.text.x=element_text(size = 8),
    legend.position = "bottom",
    axis.ticks.y=element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "gray50", linewidth = .1))

####Redo Figure 3 (replace at end of results)
full_works_final <- fullworks_core %>%
  left_join(works_by_chapter2 %>% select(openalex_work_id, Threshold), by = "openalex_work_id") %>%
  distinct()

write.csv2(full_works_final, "full_dataset.csv", row.names = F)

full_works_final$source_name <- gsub("Proceedings of the National Academy of Sciences of the United States of America", "PNAS", full_works_final$source_name)
full_works_final$source_name <- gsub("\\(Cold Spring Harbor Laboratory\\)", "", full_works_final$source_name)
full_works_final$source_name <- gsub("Deep-sea research. Part 2. Topical studies in oceanography/Deep sea research. Part II, Topical studies in oceanography", "Deep-Sea Research. Part 2", full_works_final$source_name)
full_works_final$source_name <- gsub("Deep-sea research. Part 1. Oceanographic research papers/Deep sea research. Part I, Oceanographic research papers", "Deep-Sea Research. Part 1", full_works_final$source_name)
full_works_final$source_name <- gsub("Frontiers in marine science", "Frontiers in Marine Science", full_works_final$source_name)
full_works_final$source_name <- gsub("PloS one", "PLOS ONE", full_works_final$source_name)
full_works_final$source_name <- gsub("Scientific reports", "Scientific Reports", full_works_final$source_name)
full_works_final$source_name <- gsub("Marine pollution bulletin", "Marine Pollution Bulletin", full_works_final$source_name)
full_works_final$source_name <- gsub("Marine policy", "Marine Policy", full_works_final$source_name)
full_works_final$source_name <- gsub("Marine ecology. Progress series", "Marine Ecology Progress Series", full_works_final$source_name)
full_works_final$source_name <- gsub("Science of the total environment", "Science of the Total Environment", full_works_final$source_name)
full_works_final$source_name <- gsub("Geophysical research letters", "Geophysical Research Letters", full_works_final$source_name)
full_works_final$source_name <- gsub("Estuarine, coastal and shelf science", "Estuarine, Coastal, and Shelf Science", full_works_final$source_name)
full_works_final$source_name <- gsub("Environmental science & technology", "Environmental Science & Technology", full_works_final$source_name)
full_works_final$source_name <- gsub("Limnology and oceanography/The l & o on cd-rom", "Limnology and Oceanography", full_works_final$source_name)
full_works_final$source_name <- gsub("ICES journal of marine science", "ICES Journal of Marine Science", full_works_final$source_name)
full_works_final$source_name <- gsub("Marine biology", "Marine Biology", full_works_final$source_name)
full_works_final$source_name <- gsub("Journal of experimental marine biology and ecology", "J. of Exp. Marine Biology and Ecology", full_works_final$source_name)
full_works_final$source_name <- gsub("Progress in oceanography/Progress in Oceanography", "Progress in Oceanography", full_works_final$source_name)
full_works_final$source_name <- gsub("Ocean & coastal management", "Ocean & Coastal Management", full_works_final$source_name)


source_data_full <- full_works_final %>%
  filter(!is.na(source_name)) %>%
  group_by(source_name) %>%
  summarise(n_papers = n()) %>%
  ungroup() %>%
  mutate(n_total = sum(n_papers)) %>%
  group_by(source_name) %>%
  mutate(prop = n_papers/n_total) %>%
  arrange(desc(n_papers))

source_data_core <- full_works_final %>%
  filter(iscore == "core") %>%
  filter(!is.na(source_name)) %>%
  group_by(source_name) %>%
  summarise(n_papers = n()) %>%
  ungroup() %>%
  mutate(n_total = sum(n_papers)) %>%
  group_by(source_name) %>%
  mutate(prop = n_papers/n_total) %>%
  arrange(desc(n_papers))

source_data90 <- full_works_final %>%
  filter(openalex_work_id %in% works_include90) %>% 
  filter(!is.na(source_name)) %>%
  group_by(source_name) %>%
  summarise(n_papers = n()) %>%
  ungroup() %>%
  mutate(n_total = sum(n_papers)) %>%
  group_by(source_name) %>%
  mutate(prop = n_papers/n_total) %>%
  arrange(desc(n_papers))

source_data75 <- full_works_final %>%
  filter(openalex_work_id %in% works_include75) %>% 
  filter(!is.na(source_name)) %>%
  group_by(source_name) %>%
  summarise(n_papers = n()) %>%
  ungroup() %>%
  mutate(n_total = sum(n_papers)) %>%
  group_by(source_name) %>%
  mutate(prop = n_papers/n_total) %>%
  arrange(desc(n_papers))

source_data60 <- full_works_final %>%
  filter(openalex_work_id %in% works_include60) %>% 
  filter(!is.na(source_name)) %>%
  group_by(source_name) %>%
  summarise(n_papers = n()) %>%
  ungroup() %>%
  mutate(n_total = sum(n_papers)) %>%
  group_by(source_name) %>%
  mutate(prop = n_papers/n_total) %>%
  arrange(desc(n_papers))

##Prepare Figure 4
#create plot for journals
p1 <- source_data_core %>%
  arrange(desc(prop)) %>%
  head(12) %>%
  ggplot() + 
  geom_col(aes(x = reorder(source_name, prop), y = prop), width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, .06), expand = c(0, 0)) +
  labs(title = "Top journals in core dataset", x = "", y = "") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic"),
    legend.text = element_text(margin = margin(r = 10))
  )

p2 <- source_data_full %>%
  arrange(desc(prop)) %>%
  head(12) %>%
  ggplot() + 
  geom_col(aes(x = reorder(source_name, prop), y = prop), width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, .06), expand = c(0, 0)) +
  labs(title = "Top journals in full dataset", x = "", y = "", fill = "") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic")
  )

p3 <- source_data90 %>%
  arrange(desc(prop)) %>%
  head(12) %>%
  ggplot() + 
  geom_col(aes(x = reorder(source_name, prop), y = prop), width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, .06), expand = c(0, 0)) +
  labs(title = "Top journals in 90% identification performance score dataset", x = "", y = "% of documents in dataset", fill = "") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic"),
    axis.title.x = element_text(size=10,face="bold", margin = unit(c(3, 0, 0, 0), "mm"))
  )

p4 <- source_data75 %>%
  arrange(desc(prop)) %>%
  head(12) %>%
  ggplot() + 
  geom_col(aes(x = reorder(source_name, prop), y = prop), width = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  labs(title = "Top journals at 75% threshold", x = "", y = "", fill = "") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic")
  )

p5 <- source_data60 %>%
  arrange(desc(prop)) %>%
  head(12) %>%
  ggplot() + 
  geom_col(aes(x = reorder(source_name, prop), y = prop), width = 0.6) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 6), expand = c(0, 0)) +
  labs(title = "Top journals at 60% threshold", x = "", y = "", fill = "") +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, family = "Times", face = "italic")
  )


#combine plot
p6 <- ggarrange(p1, p2, p3, nrow = 3, align = "v", common.legend = TRUE, legend = "none") 
p6

ggsave("tables_and_figures/figure4_vector.svg", p6, width = 24, height = 20, units = "cm")

##Additional analyses (revision)
fullworks_core %>%
  count(type) %>%
  arrange(desc(n))

fullworks_core %>%
  count(source_name) %>%
  arrange(desc(n))

scores_good90 %>%
  count(type) %>%
  arrange(desc(n))

scores_good90 %>%
  count(source_name) %>%
  arrange(desc(n))

scores_good75 %>%
  count(type) %>%
  arrange(desc(n))

scores_good75 %>%
  count(source_name) %>%
  arrange(desc(n))

scores_good60 %>%
  count(type) %>%
  arrange(desc(n))

scores_good60 %>%
  count(source_name) %>%
  arrange(desc(n))

scores_good90 %>%
  count(type) %>%
  arrange(desc(n))

scores_good90 %>%
  count(source_name) %>%
  arrange(desc(n))

scores_good90 %>%
  count(type) %>%
  arrange(desc(n))

scores_good90 %>%
  count(publication_year) %>%
  arrange(desc(n))

scores_good75 %>%
  count(publication_year) %>%
  arrange(desc(n))

scores_good60 %>%
  count(publication_year) %>%
  arrange(desc(n))