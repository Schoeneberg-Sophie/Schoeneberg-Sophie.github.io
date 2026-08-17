################################################################################
# Master Dataset: Keywords, Context & Sentiment
################################################################################

library(dplyr)
library(quanteda)
library(sentimentr)
library(officer)
library(readr)


all_keywords <- read_csv("/all-keywords.csv")


blocked_terms <- all_keywords$Translation %>%
  as.character() %>%
  strsplit("\\+") %>%
  unlist() %>%
  na.omit() %>%
  gsub("[()]", "", .) %>%
  trimws()

blocked_terms <- unique(c(
  blocked_terms,
  "xi", "propaganda", "re-education", "surveillance",
  "police state", "secret police", "detention", "june fourth",
  "cultural revolution", "firewall", "whistleblower", "exile",
  "uyghur", "uighur", "china"
))

blocked_terms <- blocked_terms[blocked_terms != ""]

dictionary_blocked_terms <- dictionary(
  list(blocked_terms = blocked_terms)
)


# Note: files are not uploaded due to copyright

files <- c(
  "South China Morning Post" = "Files(scmp).DOCX",
  "China Daily" = "Files(cd).DOCX",
  "China Pictorial" = "Files(cp).DOCX",
  "Beijing Review" = "Files(br).DOCX",
  "People's Daily Online - English" = "Files(pdo).DOCX",
  "BBC Monitoring" = "Files(bbc).DOCX",
  "Agence France Presse" = "Files(afp).DOCX",
  "Associated Press" = "Files(ap).DOCX",
  "New York Times" = "Files(nyt).DOCX",
  "The Times (London)" = "Files(tt).DOCX"
)

pro_beijing <- c(
  "South China Morning Post",
  "China Daily",
  "China Pictorial",
  "Beijing Review",
  "People's Daily Online - English"
)


# Read DOCX files
# =================

master_articles <- lapply(names(files), function(source_name) {
  
  doc <- read_docx(files[[source_name]])
  doc_summary <- docx_summary(doc)
  
  doc_summary <- doc_summary %>%
    filter(
      !is.na(text),
      trimws(text) != ""
    )
  
  # Find a column containing dates
  date_candidates <- names(doc_summary)[
    sapply(doc_summary, function(x) {
      any(grepl(
        "^\\d{4}-\\d{2}-\\d{2}$",
        as.character(x),
        na.rm = TRUE
      ))
    })
  ]
  
  if (length(date_candidates) > 0) {
    date_column <- date_candidates[1]
    article_dates <- as.Date(
      as.character(doc_summary[[date_column]]),
      format = "%Y-%m-%d"
    )
  } else {
    article_dates <- as.Date(rep(NA, nrow(doc_summary)))
  }
  
  data.frame(
    articles = doc_summary$text,
    date = article_dates,
    source = source_name,
    group = ifelse(
      source_name %in% pro_beijing,
      "Pro-Beijing",
      "Independent"
    ),
    stringsAsFactors = FALSE
  )
  
}) %>%
  bind_rows() %>%
  mutate(doc_id = row_number())


# corpus & keywords:
# ==================

master_corpus <- corpus(
  master_articles,
  text_field = "articles",
  docid_field = "doc_id"
)

master_tokens <- tokens(
  master_corpus,
  remove_punct = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)


kwic_results <- kwic(
  master_tokens,
  pattern = dictionary_blocked_terms,
  window = 5,
  case_insensitive = TRUE
)

df_kwic <- as.data.frame(kwic_results) %>%
  mutate(
    doc_id = as.numeric(docname),
    text = paste(pre, keyword, post, sep = " ")
  ) %>%
  select(doc_id, keyword, text)


# sentiment per article:
# ======================

sentences <- get_sentences(master_articles$articles)

article_sentiments <- data.frame(
  doc_id = master_articles$doc_id,
  sentiment = sentiment_by(sentences)$ave_sentiment
)


# final dataset:
# ==============

final_dataset <- df_kwic %>%
  left_join(
    master_articles %>%
      select(doc_id, date, source, group),
    by = "doc_id"
  ) %>%
  left_join(
    article_sentiments,
    by = "doc_id"
  ) %>%
  select(
    doc_id,
    date,
    source,
    group,
    keyword,
    sentiment,
    text
  )

View(final_dataset)