library(tidyverse)
library(RPostgres)
library(readxl)
library(writexl)
library(stringr)
library(scales)
library(reshape2)
library(gtools)
library(svglite)


## Connect to database
{
  if(exists("db")) { 
    dbDisconnect(db) 
    rm(db)
  }
  db = dbConnect(Postgres(),
                 user = read.csv()$user,
                 password = read.csv()$pw,
                 host = read.csv()$host,
                 port = ,
                 dbname = "",
                 options="")
}

##Retrieve data from database
chap_works<-read_excel("references/References_Cat.xlsx", sheet = "Non-DOI refs", col_names = TRUE) %>%
  rename(woaii_chapter = chapter) %>%
  drop_na(type_pub)
chap_works_doi<-readRDS("data/ch_works_v2.RDS") %>%
  rename(woaii_chapter = chapter) %>%
  distinct(woaii_chapter, doi)

chap<-dbReadTable(db, "chapters") %>%
  filter(woaii_chapter != 0)
all_works<-dbReadTable(db, "works")
core_works<-dbReadTable(db, "core_works") %>%
  left_join(all_works, by = "openalex_work_id")


chapters <- str_sort(chap$woaii_chapter, numeric=TRUE)


##Calculate number of references, DOIs and OAIDs per chapter - n_doi2 and n_core2 are the good numbers; ch_doi and ch_works are built in the openalex_data_collection.qmd file
n_ref <- chap_works %>%
  group_by(woaii_chapter) %>%
  summarise(R = n())

n_doi <- chap_works_doi %>%
  group_by(woaii_chapter) %>%
  summarise(D = n())

n_doi2 <- ch_doi %>%
  rename(woaii_chapter = chapter) %>%
  group_by(woaii_chapter) %>%
  distinct(doi) %>%
  summarise(D = n())

n_core <- core_works %>%
  group_by(woaii_chapter) %>%
  summarise(O = n())

n_core2 <- ch_works %>%
  rename(woaii_chapter = chapter) %>%
  group_by(woaii_chapter) %>%
  distinct(id) %>%
  summarise(O = n())


##Checking for the top journals per chapter
top_journals2 <- core_works %>%
  drop_na(source_name) %>%
#  group_by(woaii_chapter) %>%
  group_by(source_name) %>%
  summarise(total_source = n(),
            distinct_source = n_distinct(openalex_work_id))

top_journals <- core_works %>%
  drop_na(source_name) %>%
  group_by(woaii_chapter) %>%
  count(source_name) %>%
  slice_max(order_by = n, n = 3) %>%
  arrange(desc(n)) %>%
  mutate(n = paste("(", n, ")", sep="")) %>%
  ungroup() %>%
  group_by(woaii_chapter) %>%
  summarise(so=paste(source_name, n, collapse =", ")) %>%
  ungroup()


##Build the summary table (number of references) for chapters
summary_chap <- chap %>%
  select(woaii_chapter, chapter_title) %>%
  left_join(n_ref, by = "woaii_chapter") %>%
  left_join(n_doi2, by = "woaii_chapter") %>%
  left_join(n_core2, by = "woaii_chapter") %>%
  left_join(top_journals, by = "woaii_chapter") %>%
  mutate(Prop = round(O/R, digits = 4)*100) %>%
  mutate(woaii_chapter = factor(woaii_chapter, levels= str_sort(woaii_chapter, numeric=TRUE))) %>%
  arrange(woaii_chapter)

summary_chap_core <- summary_chap %>%
  filter(!row_number() %in% c(1, 2))

summary_desc <- summary_chap_core %>%
  summarise_at(c("R", "D", "O", "Prop"), .funs = list(mean = mean, median = median, min = min, max = max, sd = sd))

##Build the summary table (number of references to references) for chapters
core_works_summary <- core_works %>%
  group_by(woaii_chapter) %>%
  summarise(total_references = sum(reference_count),
            total_citations = sum(cited_by_count),
            max_reference = max(reference_count),
            max_citation = max(cited_by_count),
            mean_references = round(mean(reference_count), digits = 4),
            mean_citations = round(mean(cited_by_count), digits = 4),
            median_references = median(reference_count),
            median_citations = median(cited_by_count),
            sd_references = round(sd(reference_count), digits = 4),
            sd_citations = round(sd(cited_by_count), digits = 4)) %>%
  ungroup() %>%
  mutate(woaii_chapter = factor(woaii_chapter, levels= str_sort(woaii_chapter, numeric=TRUE))) %>%
  arrange(woaii_chapter)

openalex_works2 %>%
  count(type) %>%
  arrange(desc(n))

core_works %>%
  count(type) %>%
  arrange(desc(n))

##Save both summary tables for exportation to word
write.csv2(summary_chap_core, "summary_chap.csv", row.names = F)
write.csv2(core_works_summary, "summary_core.csv", row.names = F)

##Checking some stuff
most_cited <- chap_works %>%
  count(Raw) %>%
  arrange(desc(n))

core_works %>%
  count(publication_year) %>% 
  arrange(desc(n))
  
##Building figure 1 - number of references
theme_set(theme_light(base_size = 12))

fig2 <- summary_chap_core %>%
  ggplot() +
  geom_col(aes(x = reorder(woaii_chapter, R), y = R, fill = "Total number of references")) +
  geom_col(aes(x = reorder(woaii_chapter, R), y = O, fill = "Number of OpenAlex documents")) +
  labs(title = "", x = "Chapters", y = "", fill = "") +
  scale_y_continuous(limits = c(0, 260), expand = c(0, 0)) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        legend.text = element_text(size = 12),
        legend.key.spacing.x = unit(10, "pt"),
        axis.text.y=element_text(face = "bold", size = 10),
        axis.text.x=element_text(size = 10),
        legend.position = "bottom",
        axis.ticks.y=element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "gray50", linewidth = .1))

ggsave("tables_and_figures/figure2_vector.svg", fig2, width = 20, height = 24, units = "cm")
