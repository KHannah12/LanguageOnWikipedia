 #### The Availability of Knowledge: Language Bias on Wikipedia for Global Issues - R Code ####
 library(dplyr)
 library(readr)
 library(tidyr)
 library(ggplot2)
 library(scales)
 library(RColorBrewer)
 library(patchwork)
 library(lubridate)
 library(car)
 library(MASS)
 library(forcats)
 library(pscl) 
 library(rvest)
 library(gtranslate)
 library(R.utils)
 library(stringr)
 
 #### Function to extract data from Wikipedia ####
 WikiPages_sorted <- read_csv("https://raw.githubusercontent.com/KHannah12/LanguageOnWikipedia/main/WikiPages_Sorted.csv") 
 
 BiodiversityPages <- WikiPages_sorted[1:125, 2]
 BiodiversityPages$URL <- gsub("https://en.wikipedia.org/wiki/", "", BiodiversityPages$URL)
 Biodiversity_list <- as.list(BiodiversityPages$URL)
 Biodiversity_list <- unlist(Biodiversity_list)
 
 ClimateChangePages <- WikiPages_sorted[126:180, 2]
 ClimateChangePages$URL <- gsub("https://en.wikipedia.org/wiki/", "", ClimateChangePages$URL)
 ClimateChange_list <- as.list(ClimateChangePages$URL)
 ClimateChange_list <- unlist(ClimateChange_list)
 
 EnvironmentalConservationPages <- WikiPages_sorted[181:257, 2]
 EnvironmentalConservationPages$URL <- gsub("https://en.wikipedia.org/wiki/", "", EnvironmentalConservationPages$URL)
 EnvironmentalConservation_list <- as.list(EnvironmentalConservationPages$URL)
 EnvironmentalConservation_list <- unlist(EnvironmentalConservation_list)
 
 PublicHealthPages <- WikiPages_sorted[258:389, 2]
 PublicHealthPages$URL <- gsub("https://en.wikipedia.org/wiki/", "", PublicHealthPages$URL)
 PublicHealth_list <- as.list(PublicHealthPages$URL)
 PublicHealth_list <- unlist(PublicHealth_list)
 
 #  Creating functions #
 
 # Function to get languages and their URLs for given Wikipedia pages
 get_pages_languages <- function(page_titles) {
   all_languages_data <- list()
   for (page_title in page_titles) {
     url <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", page_title))
     page <- read_html(url)
     languages <- page %>%
       html_nodes(".interlanguage-link-target") %>%
       html_text()
     language_urls <- page %>%
       html_nodes(".interlanguage-link-target") %>%
       html_attr("href")
     english_url <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", page_title))
     languages <- c("English", languages)
     language_urls <- c(english_url, language_urls)
     language_data <- data.frame(language = languages, url = language_urls, original_topic = page_title)
     all_languages_data[[page_title]] <- language_data
   }
   return(all_languages_data)
 }
 
 # Extract only the URLs from the language data
 transform_language_data <- function(language_data) {
   urls <- language_data$url
   original_topics <- language_data$original_topic
   return(list(urls = urls, original_topics = original_topics))
 }
 
 # Function to extract info from wiki page
 extract_info <- function(url, language, original_topic) {
   tryCatch({
     # Read the HTML content of the Wikipedia page
     page <- read_html(url)
     
     # Extract the number of words for only the main text of the page
     main_content_node <- page %>%
       html_node("#mw-content-text .mw-parser-output")
     # Extract only the paragraph texts from the main content
     paragraphs <- main_content_node %>%
       html_nodes("p") %>%
       html_text(trim = TRUE)
     # Combine all paragraph texts into a single string
     content <- paste(paragraphs, collapse = " ")
     # Count the number of words in the combined paragraph text
     num_words <- str_count(content, "\\w+")
     
     # Count the number of references (how many are listed in the references section)
     num_sources <- page %>%
       html_nodes("ol.references li") %>% 
       length()
     
     # Count the number of sections by considering heading tags h2 (headings) and h3 (subheadings)
     num_sections <- main_content_node %>%
       html_nodes("h2, h3") %>%
       length()
    
     # Count the number of images (all images on the page)
     num_images <- main_content_node %>%
       html_nodes("img") %>%
       length()
     
     # Extract language code from the URL
     lang_code <- gsub(".*://(.*).wikipedia.*", "\\1", tolower(url))
     
     # Construct the page information URL and read the html
     page_title <- gsub(".*/wiki/", "", url)
     page_info_url <- paste0("https://", lang_code, ".wikipedia.org/w/index.php?title=",
                             URLencode(page_title), "&action=info")
     page_info_page <- read_html(page_info_url)
     
     # Extract page views from the information page
     page_views_node <- page_info_page %>%
       html_node("#mw-pvi-month-count .mw-pvi-month")
     page_views <- if (!is.null(page_views_node)) {
       page_views_text <- page_views_node %>%
         html_text() %>%
         gsub("[,. ]", "", .) 
       as.numeric(page_views_text)
     } else {
       NA
     }
     
     # Extract page creation date
     page_creation_node <- page_info_page %>%
       html_node("#mw-pageinfo-firsttime td:nth-child(2)")
     page_creation_date <- if (!is.null(page_creation_node)) {
       page_creation_node %>%
         html_text() %>%
         trimws() 
     } else {
       NA
     }
     
     # Extracting recent edits (30 days)
     edits_30_days_node <- page_info_page %>%
       html_node("#mw-pageinfo-recent-edits td:nth-child(2)")
     edits_30_days <- if (!is.null(edits_30_days_node)) {
       edits_30_days_text <- edits_30_days_node %>%
         html_text() %>%
         gsub("[,. ]", "", .) # Remove commas, spaces, and full stops
       as.numeric(edits_30_days_text)
     } else {
       NA
     }
     
     # Extracting number of links to other Wikipedia pages
     links <- page %>%
       html_nodes("a") %>%
       html_attr("href")
     wiki_links <- grep("^/wiki/", links, value = TRUE)
     wiki_links <- wiki_links[!grepl(":", wiki_links)]
     wiki_links <- unique(wiki_links)
     num_links_to_wikipedia <- length(wiki_links)
     
     # Return the extracted information along with the language, URL, and original topic
     return(data.frame(Language = language, 
                       Page_Title = page_title,
                       Num_Words = num_words, 
                       Num_Sources = num_sources, 
                       Num_Sections = num_sections, 
                       Num_Images = num_images,
                       Page_Views = page_views,
                       Page_Creation_Date = page_creation_date,
                       Edits_30_Days = edits_30_days,
                       Num_Links_to_Wikipedia = num_links_to_wikipedia,
                       URL = url, 
                       Original_Topic = original_topic,
                       stringsAsFactors = FALSE))
   }, error = function(e) {
     message(paste("Error for URL:", url, ":", e$message))
     return(NULL)
   })
 }
 
 ## Running the code ##
 topics <- PublicHealth_list ## TOPIC INPUT - need to input for each category ##
 
 all_languages_data <- get_pages_languages(topics)
 all_data <- lapply(all_languages_data, transform_language_data)
 urls <- unlist(lapply(all_data, function(x) x$urls))
 original_topics <- unlist(lapply(all_data, function(x) x$original_topics))
 
 # Extract function 
 start <- Sys.time()
 results <- lapply(seq_along(urls), function(i) {
   url <- urls[i]
   language <- urls[i]
   original_topic <- original_topics[i]
   if (!is.na(url) && !is.na(language) && url != "" && language != "") {
     tryCatch({
       result <- withTimeout({
         extract_info(url, language, original_topic)
       }, timeout = 300) # Timeout set to 5 minutes
       message(paste("Finished extracting:", url))
       return(result)
     }, TimeoutException = function(ex) {
       message(paste("Timeout for URL:", url))
       return(NULL)
     }, error = function(e) {
       message(paste("Error for URL:", url, ":", e$message))
       return(NULL)
     })
   } else {
     NULL  # Skip processing if URL or language is empty or NA
   }
 })
 end <- Sys.time()
 Time_elapsed <- end - start
 Time_elapsed
 
 #Processing the output #
 result_df <- do.call(rbind, results)
 
 lang_code <-  read_csv("https://raw.githubusercontent.com/KHannah12/LanguageOnWikipedia/main/iso_639-1.csv") 
 lang_code <- lang_code[,c(2,4)]
 
 # Extract language code from URL
 result_df$language_code <- gsub(".*://(.*).wikipedia.*", "\\1", tolower(result_df$URL))
 
 #### Figure 2 - Number of articles per language ####
 ExtractedDat <- read_csv("https://raw.githubusercontent.com/KHannah12/LanguageOnWikipedia/main/Extraction3_CURRENT.csv") 
 BiodivCons <- ExtractedDat[ExtractedDat$Category == "Biodiversity Conservation", ]
 ClimateCh <- ExtractedDat[ExtractedDat$Category == "Climate Change", ]
 PubHealth <- ExtractedDat[ExtractedDat$Category == "Public Health", ]
 
 B_language_topic_counts <- BiodivCons %>%
   group_by(language) %>%
   summarise(Num_Topics = n_distinct(Original_Topic)) %>%
   arrange(desc(Num_Topics))
 
 B_languages_to_callout <- c("English")
 B_callout_data <- B_language_topic_counts %>%
   filter(language %in% B_languages_to_callout)
 
 total_biodiv_langs <- length(unique(BiodivCons$language))
 
 B_plot_topic <- ggplot(B_language_topic_counts,
                        aes(x = fct_reorder(language, Num_Topics, .desc = TRUE),
                            y = Num_Topics,
                            fill = ifelse(language == "English", "English", "Non-English languages"))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(values = c("English" = "darkorange", "Non-English languages" = "skyblue"),
                     name = "Languages of Wikipedia Pages") +
   theme_minimal() +
   labs(title = "Biodiversity Conservation",
        x = paste0("n = ", total_biodiv_langs, " Languages"),
        y = "Number of Wikipedia Articles") +
   theme(axis.text.x = element_blank(),
         plot.title = element_text(hjust = 0.5),
         legend.position = "bottom")
 
 # Plot 2: Climate Change 
 C_language_topic_counts <- ClimateCh %>%
   group_by(language) %>%
   summarise(Num_Topics = n_distinct(Original_Topic)) %>%
   arrange(desc(Num_Topics))
 
 C_languages_to_callout <- c("English")
 C_callout_data <- C_language_topic_counts %>%
   filter(language %in% C_languages_to_callout)
 
 total_climate_langs <- length(unique(ClimateCh$language))
 
 C_plot_topic <- ggplot(C_language_topic_counts, 
                        aes(x = fct_reorder(language, Num_Topics, .desc = TRUE),
                            y = Num_Topics,
                            fill = ifelse(language == "English", "English", "Non-English languages"))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(values = c("English" = "darkorange", "Non-English languages" = "skyblue"),
                     name = "Languages of Wikipedia Pages") +
   theme_minimal() +
   labs(title = "Climate Change",
        x = paste0("n = ", total_climate_langs, " Languages"),
        y = " ") +
   theme(axis.text.x = element_blank(),
         plot.title = element_text(hjust = 0.5),
         legend.position = "none")
 
 # Plot 3: Public Health
 P_language_topic_counts <- PubHealth %>%
   group_by(language) %>%
   summarise(Num_Topics = n_distinct(Original_Topic)) %>%
   arrange(desc(Num_Topics))
 
 P_languages_to_callout <- c("English")
 P_callout_data <- P_language_topic_counts %>%
   filter(language %in% P_languages_to_callout)
 
 total_health_langs <- length(unique(PubHealth$language))
 
 P_plot_topic <- ggplot(P_language_topic_counts, 
                        aes(x = fct_reorder(language, Num_Topics, .desc = TRUE),
                            y = Num_Topics,
                            fill = ifelse(language == "English", "English", "Non-English languages"))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(values = c("English" = "darkorange", "Non-English languages" = "skyblue"),
                     name = "Languages of Wikipedia Pages") +
   theme_minimal() +
   labs(title = "Public Health",
        x = paste0("n = ", total_health_langs, " Languages"),
        y = " ") +
   theme(axis.text.x = element_blank(),
         plot.title = element_text(hjust = 0.5),
         legend.position = "right")
 
 B_plot_topic + C_plot_topic + P_plot_topic +
   plot_layout(ncol = 3, guides = "collect") &
   theme(legend.position = "bottom",
         legend.title.position = "top")
 
 
 #### Figure 3 - Percentage Comparison to English ####
 english_words <- BiodivCons %>%
   filter(language == "English") %>%
   dplyr::select(Original_Topic, English_Words = Num_Words)
 
 BiodivCons <- BiodivCons %>%
   left_join(english_words, by = "Original_Topic") %>%
   mutate(WordsPercentage = ifelse(language == "English", NA, (Num_Words / English_Words) * 100))
 
 english_words <- ClimateCh %>%
   filter(language == "English") %>%
   dplyr::select(Original_Topic, English_Words = Num_Words)
 
 ClimateCh <- ClimateCh %>%
   left_join(english_words, by = "Original_Topic") %>%
   mutate(WordsPercentage = ifelse(language == "English", NA, (Num_Words / English_Words) * 100))
 
 english_words <- PubHealth %>%
   filter(language == "English") %>%
   dplyr::select(Original_Topic, English_Words = Num_Words)
 
 PubHealth <- PubHealth %>%
   left_join(english_words, by = "Original_Topic") %>%
   mutate(WordsPercentage = ifelse(language == "English", NA, (Num_Words / English_Words) * 100))
 
 # Filter data and set up paramters
 plot_data_BiodivCons <- BiodivCons %>%
   filter(language != "English")
 plot_data_ClimateCh <- ClimateCh %>%
   filter(language != "English")
 plot_data_PubHealth <- PubHealth %>%
   filter(language != "English")
 
 global_min <- min(c(min(BiodivCons$WordsPercentage, na.rm = TRUE),
                     min(ClimateCh$WordsPercentage, na.rm = TRUE),
                     min(PubHealth$WordsPercentage, na.rm = TRUE)))
 global_max <- max(c(max(BiodivCons$WordsPercentage, na.rm = TRUE),
                     max(ClimateCh$WordsPercentage, na.rm = TRUE),
                     max(PubHealth$WordsPercentage, na.rm = TRUE)))
 
 y_limits <- c(global_min, global_max)
 y_breaks <- c(1, 3, 5, 10, 25, 50, 100, 250, 500, 1000, 2500)
 
 # Run plots seperately and then combine
 B_comp_word <- plot_data_BiodivCons %>%
   mutate(language = reorder(language, -WordsPercentage, FUN = mean)) %>%
   ggplot(aes(x = language, y = WordsPercentage)) +
   geom_boxplot(fill = "steelblue") +
   labs(title = "Biodiversity Conservation",
        x = "",
        y = "Percentage of English Word Count") +
   theme_minimal() +
   theme(legend.position = "none",
         axis.text.x = element_blank(),
         axis.text.y = element_text(size = 10),
         axis.title.y = element_text(size = 14),
         plot.title = element_text(size = 16)) +
   scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), limits = y_limits, breaks = y_breaks) +
   geom_hline(yintercept = 100, linetype = "dashed", color = "red", alpha = 0.5)
 
 C_comp_word <- plot_data_ClimateCh %>%
   mutate(language = reorder(language, -WordsPercentage, FUN = mean)) %>%
   ggplot(aes(x = language, y = WordsPercentage)) +
   geom_boxplot(fill = "steelblue") +
   labs(title = "B - Climate Change",
        x = "Language",
        y = " ") +
   theme_minimal() +
   theme(legend.position = "none",
         axis.text.x = element_blank()) +
   scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), limits = y_limits, breaks = y_breaks) +
   geom_hline(yintercept = 100, linetype = "dashed", color = "red", alpha = 0.5)
 
 P_comp_word <- plot_data_PubHealth %>%
   mutate(language = reorder(language, -WordsPercentage, FUN = mean)) %>%
   ggplot(aes(x = language, y = WordsPercentage)) +
   geom_boxplot(fill = "steelblue") +
   labs(title = "C - Public Health",
        x = "",
        y = "") +
   theme_minimal() +
   theme(legend.position = "none",
         axis.text.x = element_blank()) +
   scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), limits = y_limits, breaks = y_breaks) +
   geom_hline(yintercept = 100, linetype = "dashed", color = "red", alpha = 0.5)
 
 B_comp_word + C_comp_word + P_comp_word + 
   plot_layout(ncol = 3) +
   plot_annotation(tag_levels = NULL)
 #### Figure 4 - Shannon's Evenness Index ####
 # Reshape data - BC #
 BiodivCons_Words_wide <- BiodivCons %>%
   dplyr::select(ISO_639, language, Original_Topic, Num_Words) %>%  
   pivot_wider(
     names_from = Original_Topic,
     values_from = Num_Words)
 
 BiodivCons_Words_wide <- BiodivCons_Words_wide %>%
   mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, .)))
 
 BiodivCons_Words_wide <- BiodivCons_Words_wide %>%
   rowwise() %>% 
   mutate(Row_Sum = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
   ungroup()  
 
 ethno_collapsed <- ethnolangdat %>% ## Requires Ethnologue Data
   group_by(ISO_639) %>%
   summarise(Total_L1_Users = sum(L1_Users, na.rm = TRUE))
 
 BiodivCons_Words_wide <- BiodivCons_Words_wide %>% ## Requires Ethnologue Data
   left_join(ethno_collapsed, by = "ISO_639")
 
 # Reshape data - CC #
 ClimateCh_Words_wide <- ClimateCh %>%
   dplyr::select(ISO_639, language, Original_Topic, Num_Words) %>%  
   pivot_wider(
     names_from = Original_Topic,
     values_from = Num_Words)
 
 ClimateCh_Words_wide <- ClimateCh_Words_wide %>%
   mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, .)))
 
 ClimateCh_Words_wide <- ClimateCh_Words_wide %>%
   rowwise() %>% 
   mutate(Row_Sum = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
   ungroup()  
 
 ClimateCh_Words_wide <- ClimateCh_Words_wide %>% ## Requires Ethnologue Data
   left_join(ethno_collapsed, by = "ISO_639")
 
 # Reshape data - PH #
 PubHealth_Words_wide <- PubHealth %>%
   dplyr::select(ISO_639, language, Original_Topic, Num_Words) %>%  
   pivot_wider(
     names_from = Original_Topic,
     values_from = Num_Words)
 
 PubHealth_Words_wide <- PubHealth_Words_wide %>%
   mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, .)))
 
 PubHealth_Words_wide <- PubHealth_Words_wide %>%
   rowwise() %>% 
   mutate(Row_Sum = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
   ungroup()  
 
 PubHealth_Words_wide <- PubHealth_Words_wide %>% ## Requires Ethnologue Data
   left_join(ethno_collapsed, by = "ISO_639")
 
 # Calculate Evennes for each theme #
 calculate_evenness <- function(topic_values) {
   total <- sum(topic_values)
   proportions <- topic_values / total
   H <- -sum(proportions * log(proportions), na.rm = TRUE)
   H_max <- log(length(topic_values))
   evenness <- H / H_max
   return(evenness)
 }
 
 B_evenness_results <- BiodivCons_Words_wide %>%
   dplyr::select(-ISO_639, -language,-Total_L1_Users, -Row_Sum) %>%  
   rowwise() %>%
   mutate(Evenness = calculate_evenness(c_across(everything()))) %>%
   ungroup() %>%
   bind_cols(BiodivCons_Words_wide %>% dplyr::select(language)) %>%  
   dplyr::select(language, Evenness) 
 
 C_evenness_results <- ClimateCh_Words_wide %>%
   dplyr::select(-ISO_639, -language, -Total_L1_Users, -Row_Sum) %>%  
   rowwise() %>%
   mutate(Evenness = calculate_evenness(c_across(everything()))) %>%
   ungroup() %>%
   bind_cols(ClimateCh_Words_wide %>% dplyr::select(language)) %>%  
   dplyr::select(language, Evenness) 
 
 P_evenness_results <- PubHealth_Words_wide %>%
   dplyr::select(-ISO_639, -language, -Total_L1_Users, -Row_Sum) %>%  
   rowwise() %>%
   mutate(Evenness = calculate_evenness(c_across(everything()))) %>%
   ungroup() %>%
   bind_cols(PubHealth_Words_wide %>% dplyr::select(language)) %>%  
   dplyr::select(language, Evenness) 
 
 n_lang_B <- nrow(B_evenness_results)
 n_lang_C <- nrow(C_evenness_results)
 n_lang_P <- nrow(P_evenness_results)
 B_evenness_results_filtered <- B_evenness_results %>%
   filter(Evenness > 0)
 
 C_evenness_results_filtered <- C_evenness_results %>%
   filter(Evenness > 0)
 
 P_evenness_results_filtered <- P_evenness_results %>%
   filter(Evenness > 0)
 
 # Run plots and combine #
 Lang_PlotB <- ggplot(B_evenness_results_filtered, aes(x = reorder(language, -Evenness), y = Evenness,
                                                       fill = ifelse(language == "English", "English", "Non-English languages"))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(values = c("English" = "darkorange", "Non-English languages" = "skyblue"),
                     name = "Languages of Wikipedia Pages") +
   theme_minimal() +
   theme(
     axis.ticks.x = element_blank(),
     axis.text.x = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.background = element_rect(fill = "grey95", color = NA),
     legend.position = "none") +
   labs(title = "A - Biodiversity Conservation", x = paste0("n = ", n_lang_B," Languages"), y = "Shannon's Evenness Index") +
   ylim(0, 1)
 
 Lang_PlotC <- ggplot(C_evenness_results_filtered, aes(x = reorder(language, -Evenness), y = Evenness,
                                                       fill = ifelse(language == "English", "English", "Non-English languages"))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(values = c("English" = "darkorange", "Non-English languages" = "skyblue"),
                     name = "Languages of Wikipedia Pages") +
   theme_minimal() +
   theme(
     axis.ticks.x = element_blank(),
     axis.text.x = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.background = element_rect(fill = "grey95", color = NA),
     legend.position = "none") +
   labs(title = "B - Climate Change", x = paste0("n = ", n_lang_C," Languages"), y = "") +
   ylim(0, 1)
 
 Lang_PlotP <- ggplot(P_evenness_results_filtered, aes(x = reorder(language, -Evenness), y = Evenness,
                                                       fill = ifelse(language == "English", "English", "Non-English languages"))) +
   geom_bar(stat = "identity") +
   scale_fill_manual(values = c("English" = "darkorange", "Non-English languages" = "skyblue"),
                     name = "Languages of Wikipedia Pages") +
   theme_minimal() +
   theme(
     axis.ticks.x = element_blank(),
     axis.text.x = element_blank(),
     panel.grid.major.x = element_blank(),
     panel.grid.minor.x = element_blank(),
     panel.background = element_rect(fill = "grey95", color = NA),
     legend.position = "bottom") +
   labs(title = "C - Public Health", x = paste0("n = ", n_lang_P," Languages"), y = "") +
   ylim(0, 1)
 
 Lang_PlotB + Lang_PlotC + Lang_PlotP +
   plot_layout(ncol = 3, guides = "collect") &
   theme(legend.position = "bottom",
         legend.title.position = "top")
 #### Figure 7 - Cumulative Increases ####
 common_fill_scale <- scale_fill_manual(
   values = c(
     "eng" = "lightgreen",
     "fas" = "blue",
     "arq" = "orange",
     "fra" = "pink",
     "spa" = "red",
     "deu" = "purple",
     "zho" = "yellow",
     "por" = "brown",
     "ukr" = "lightblue",
     "nld" = "darkgreen",
     "ibo" = "darkblue",
     "ces" = "maroon",
     "hau" = "limegreen",
     "pus" = "chocolate",
     "jpn" = "pink4",
     "ind" = "forestgreen",
     "Other" = "darkgrey"
   ),
   labels = c(
     "eng" = "English",
     "fas" = "Farsi",
     "arq" = "Algerian Arabic",
     "fra" = "French",
     "spa" = "Spanish",
     "deu" = "German",
     "zho" = "Chinese",
     "por" = "Portuguese",
     "ukr" = "Ukrainian",
     "nld" = "Dutch",
     "ibo" = "Igbo",
     "ces" = "Czech",
     "hau" = "Hausa",
     "pus" = "Pashto",
     "jpn" = "Japanese",
     "ind" = "Indonesian",
     "Other" = "Other Languages"))
 
 ## Biodiversity Conservation ##
 BiodivCons_datetest <- BiodivCons %>%
   mutate(date = as.Date(date, format = "%d/%m/%Y"),
          year_month = floor_date(date, "month"))
 
 BiodivCons_datetest <- BiodivCons_datetest %>%
   mutate(Language_Group = ifelse(ISO_639 %in% bio_top_languages, ISO_639, "Other"))
 
 cumulative_db_B <- BiodivCons_datetest %>%
   group_by(year_month) %>%
   summarise(
     eng = sum(ISO_639 == "eng"),
     fas = sum(ISO_639 == "fas"),
     arq = sum(ISO_639 == "arq"),
     fra = sum(ISO_639 == "fra"),
     spa = sum(ISO_639 == "spa"),
     deu = sum(ISO_639 == "deu"),
     zho = sum(ISO_639 == "zho"),
     por = sum(ISO_639 == "por"),
     ukr = sum(ISO_639 == "ukr"),
     nld = sum(ISO_639 == "nld"),
     Other = sum(Language_Group == "Other")
   ) %>%
   ungroup()
 
 cumulative_db_B <- cumulative_db_B %>%
   complete(
     year_month = seq(min(year_month), max(year_month), by = "month"),
     fill = list(
       eng = 0, fas = 0, arq = 0, fra = 0, spa = 0, deu = 0,
       zho = 0, por = 0, ukr = 0, nld = 0, Other = 0))
 
 cumulative_db_B_long <- cumulative_db_B %>%
   pivot_longer(
     cols = c("eng", "fas", "arq", "fra", "spa", "deu", "zho", "por", "ukr", "nld", "Other"),
     names_to = "Language",
     values_to = "Monthly_Count"
   ) %>%
   arrange(year_month) %>%
   group_by(Language) %>%
   mutate(Cumulative_Count = cumsum(Monthly_Count)) %>%
   ungroup() %>%
   mutate(Language = factor(Language, levels = c(
     "eng", "fas", "arq", "fra", "spa", "deu", "zho", "por", "ukr", "nld", "Other"
   )))
 
 B_timeplot2 <- ggplot(cumulative_db_B_long, aes(x = year_month, y = Cumulative_Count, fill = Language)) +
   geom_bar(stat = "identity", position = "stack") +
   common_fill_scale +
   labs(title = "Biodiversity Conservation", x = "", y = "", fill = "Language") +
   theme_minimal() +
   scale_x_date(
     breaks = seq(as.Date("2001-01-01"), as.Date("2024-12-01"), by = "1 year"),
     labels = scales::date_format("%Y")) +
   theme(
     panel.grid = element_blank(),
     axis.text.x = element_text(angle = 45, hjust = 1),
     legend.position = "right")
 
 B_timeplot2
 
 ## Climate Change ##
 ClimateCh_datetest <- ClimateCh %>%
   mutate(date = as.Date(date, format = "%d/%m/%Y"),  
          year_month = floor_date(date, "month"))      
 
 clim_top_languages <- c("eng", "arq", "fra", "spa", "deu", "zho", "ibo", "ces", "hau", "pus")
 
 ClimateCh_datetest <- ClimateCh_datetest %>%
   mutate(Language_Group = ifelse(ISO_639 %in% clim_top_languages, ISO_639, "Other"))
 
 
 cumulative_db_C <- ClimateCh_datetest %>%
   group_by(year_month) %>%
   summarise(
     eng = sum(ISO_639 == "eng"),
     arq = sum(ISO_639 == "arq"),
     fra = sum(ISO_639 == "fra"),
     spa = sum(ISO_639 == "spa"),
     deu = sum(ISO_639 == "deu"),
     zho = sum(ISO_639 == "zho"),
     ibo = sum(ISO_639 == "ibo"),
     ces = sum(ISO_639 == "ces"),
     hau = sum(ISO_639 == "hau"),
     pus = sum(ISO_639 == "pus"),
     Other = sum(!ISO_639 %in% c("eng", "arq", "fra", "spa", "deu", "zho", "ibo", "ces", "hau", "pus"))
   ) %>%
   ungroup()
 
 all_months <- seq(min(cumulative_db_C$year_month), max(cumulative_db_C$year_month), by = "month")
 cumulative_db_C <- cumulative_db_C %>%
   complete(year_month = all_months, fill = list(eng = 0, arq = 0, fra = 0, spa = 0, deu = 0, zho = 0, ibo = 0, ces = 0, hau = 0, pus = 0, Other = 0))
 
 cumulative_db_C_long <- cumulative_db_C %>%
   pivot_longer(
     cols = c("eng", "arq", "fra", "spa", "deu", "zho", "ibo", "ces", "hau", "pus", "Other"),
     names_to = "Language",
     values_to = "Monthly_Count"
   ) %>%
   arrange(year_month) %>%
   group_by(Language) %>%
   mutate(Cumulative_Count = cumsum(Monthly_Count)) %>%
   ungroup()
 
 cumulative_db_C_long <- cumulative_db_C_long %>%
   mutate(Language = factor(Language, levels = c("eng", "arq", "fra", "spa", "deu", "zho", "ibo", "ces", "hau", "pus", "Other")))
 
 C_timeplot2 <- ggplot(cumulative_db_C_long, aes(x = year_month, y = Cumulative_Count, fill = Language)) +
   geom_bar(stat = "identity", position = "stack") +
   common_fill_scale +
   labs(title = "Climate Change",
        x = "", y = "Cumulative Count", fill = "Language") +
   theme_minimal() +
   scale_x_date(breaks = seq(as.Date("2001-09-01"), as.Date("2024-08-01"), by = "1 year"),
                labels = scales::date_format("%Y")) +
   theme(
     panel.grid = element_blank(),
     axis.text.x = element_text(angle = 45, hjust = 1),
     legend.position = "right")
 
 C_timeplot2
 
 ## Public Health ##
 PubHealth_datetest <- PubHealth %>%
   mutate(date = as.Date(date, format = "%d/%m/%Y"),  
          year_month = floor_date(date, "month"))      
 
 pub_top_languages <- c("eng", "arq", "deu", "spa", "fra", "jpn", "zho", "ind", "por", "fas")
 
 PubHealth_datetest <- PubHealth_datetest %>%
   mutate(Language_Group = ifelse(ISO_639 %in% pub_top_languages, ISO_639, "Other"))
 
 cumulative_db_P <- PubHealth_datetest %>%
   group_by(year_month) %>%
   summarise(
     eng = sum(ISO_639 == "eng"),
     arq = sum(ISO_639 == "arq"),
     deu = sum(ISO_639 == "deu"),
     spa = sum(ISO_639 == "spa"),
     fra = sum(ISO_639 == "fra"),
     jpn = sum(ISO_639 == "jpn"),
     zho = sum(ISO_639 == "zho"),
     ind = sum(ISO_639 == "ind"),
     por = sum(ISO_639 == "por"),
     fas = sum(ISO_639 == "fas"),
     Other = sum(!ISO_639 %in% c("eng", "arq", "deu", "spa", "fra", "jpn", "zho", "ind", "por", "fas"))
   ) %>%
   ungroup()
 
 # Complete the data with all months
 all_months <- seq(min(cumulative_db_P$year_month), max(cumulative_db_P$year_month), by = "month")
 cumulative_db_P <- cumulative_db_P %>%
   complete(year_month = all_months, fill = list(eng = 0, arq = 0, deu = 0, spa = 0, fra = 0, jpn = 0, zho = 0, ind = 0, por = 0, fas = 0, Other = 0))
 
 # Pivot longer and calculate cumulative sums
 cumulative_db_P_long <- cumulative_db_P %>%
   pivot_longer(
     cols = c("eng", "arq", "deu", "spa", "fra", "jpn", "zho", "ind", "por", "fas", "Other"),
     names_to = "Language",
     values_to = "Monthly_Count"
   ) %>%
   arrange(year_month) %>%
   group_by(Language) %>%
   mutate(Cumulative_Count = cumsum(Monthly_Count)) %>%
   ungroup()
 
 # Ensure Language is a factor with the correct order
 cumulative_db_P_long <- cumulative_db_P_long %>%
   mutate(Language = factor(Language, levels = c("eng", "arq", "deu", "spa", "fra", "jpn", "zho", "ind", "por", "fas", "Other")))
 
 # Plotting the data
 P_timeplot2 <- ggplot(cumulative_db_P_long, aes(x = year_month, y = Cumulative_Count, fill = Language)) +
   geom_bar(stat = "identity", position = "stack") +
   common_fill_scale +
   labs(x = "Date", y = "", fill = "Language", title = "Public Health") +
   theme_minimal() +
   scale_x_date(breaks = seq(as.Date("2001-01-01"), as.Date("2024-12-01"), by = "1 year"),
                labels = scales::date_format("%Y")) +
   theme(
     panel.grid = element_blank(),
     axis.text.x = element_text(angle = 45, hjust = 1),
     legend.position = "right")
 
 #### Display the plot
 P_timeplot2
 
 combined_plot <- B_timeplot2 + C_timeplot2 + P_timeplot2 + 
   plot_layout(ncol = 1, guides = "collect") +
   plot_annotation(title = "",
                   theme = theme(plot.title = element_text(hjust = 0.5),
                                 plot.subtitle = element_text(hjust = 0.5)))
 combined_plot
 #### Analysis Data Set Up ####
 analysis <- read_csv("https://raw.githubusercontent.com/KHannah12/LanguageOnWikipedia/main/new_db2.csv") 
 analysis_long <- analysis %>%
   pivot_longer(cols = 8:ncol(analysis),  
                names_to = "ISO_639", values_to = "Speakers") %>%
   filter(Speakers > 0)
 
 analysis_long <- analysis_long %>%
   group_by(Country_Name) %>%
   mutate(Country_Total_Speakers = sum(Speakers, na.rm = TRUE),
          Threatened_Species_Weight = Speakers / Country_Total_Speakers) %>%
   ungroup()
 
 analysis_long <- analysis_long %>%
   group_by(ISO_639) %>%
   mutate(Global_Total_Language_Speakers = sum(Speakers, na.rm = TRUE),
          Global_Weight = Speakers / Global_Total_Language_Speakers) %>%
   ungroup()
 
 analysis_long <- analysis_long %>%
   mutate(
     Weighted_Threatened_Species = `Threatened Species` * Threatened_Species_Weight,
     Weighted_GHS_Index = `GHS Index` * Global_Weight,
     Weighted_GDP = `GDP 2022` * Global_Weight,
     Weighted_Climate_Vul = `Climate_Vul` * Global_Weight,
     Weighted_HDI = `HDI 2022` * Global_Weight
   )
 
 language_metrics <- analysis_long %>%
   group_by(ISO_639) %>%
   summarise(
     Total_Threatened_Species = sum(Weighted_Threatened_Species, na.rm = TRUE),
     Avg_GHS_Index = sum(Weighted_GHS_Index, na.rm = TRUE),
     Avg_GDP = sum(Weighted_GDP, na.rm = TRUE),
     Avg_Climate_Vul = sum(Weighted_Climate_Vul, na.rm = TRUE),
     Avg_HDI = sum(Weighted_HDI, na.rm = TRUE)
   )
 
 biodiv_summary <- BiodivCons %>%
   group_by(ISO_639) %>%
   summarise(
     n_entries_biodiv = n(),
     Bio_Words = sum(Num_Words, na.rm = TRUE),
     Bio_Sources = sum(Num_Sources, na.rm = TRUE),
     Bio_Sections = sum(Num_Sections, na.rm = TRUE),
     Bio_Images = sum(Num_Images, na.rm = TRUE),
     Bio_Page_Views = sum(Page_Views, na.rm = TRUE),
     Bio_Edits_30_Days = sum(Edits_30_Days, na.rm = TRUE),
     Bio_Links_to_Wikipedia = sum(Num_Links_to_Wikipedia, na.rm = TRUE)
   )
 
 Clim_summary <- ClimateCh %>%
   group_by(ISO_639) %>%
   summarise(
     n_entries_climate = n(),
     clim_Words = sum(Num_Words, na.rm = TRUE),
     clim_Sources = sum(Num_Sources, na.rm = TRUE),
     clim_Sections = sum(Num_Sections, na.rm = TRUE),
     clim_Images = sum(Num_Images, na.rm = TRUE),
     clim_Page_Views = sum(Page_Views, na.rm = TRUE),
     clim_Edits_30_Days = sum(Edits_30_Days, na.rm = TRUE),
     clim_Links_to_Wikipedia = sum(Num_Links_to_Wikipedia, na.rm = TRUE)
   )
 
 phealth_summary <- PubHealth %>%
   group_by(ISO_639) %>%
   summarise(
     n_entries_pubhealth = n(),
     pub_Words = sum(Num_Words, na.rm = TRUE),
     pub_Sources = sum(Num_Sources, na.rm = TRUE),
     pub_Sections = sum(Num_Sections, na.rm = TRUE),
     pub_Images = sum(Num_Images, na.rm = TRUE),
     pub_Page_Views = sum(Page_Views, na.rm = TRUE),
     pub_Edits_30_Days = sum(Edits_30_Days, na.rm = TRUE),
     pub_Links_to_Wikipedia = sum(Num_Links_to_Wikipedia, na.rm = TRUE)
   )
 
 final_data <- language_metrics %>%
   left_join(biodiv_summary, by = "ISO_639") %>%
   left_join(Clim_summary, by = "ISO_639") %>%
   left_join(phealth_summary, by = "ISO_639")
 
 final_data <- final_data %>%
   filter(
     !is.na(n_entries_biodiv) | !is.na(Bio_Words) | !is.na(Bio_Sources) | !is.na(Bio_Sections) | 
       !is.na(Bio_Images) | !is.na(Bio_Page_Views) | !is.na(Bio_Edits_30_Days) | !is.na(Bio_Links_to_Wikipedia) |
       !is.na(n_entries_climate) | !is.na(clim_Words) | !is.na(clim_Sources) | !is.na(clim_Sections) | !is.na(clim_Images) | 
       !is.na(clim_Page_Views) | !is.na(clim_Edits_30_Days) | !is.na(clim_Links_to_Wikipedia) | !is.na(n_entries_pubhealth)|
       !is.na(pub_Words) | !is.na(pub_Sources) | !is.na(pub_Sections) | !is.na(pub_Images) | 
       !is.na(pub_Page_Views) | !is.na(pub_Edits_30_Days) | !is.na(pub_Links_to_Wikipedia))
 
 final_data <- final_data %>%
   filter(
     Avg_GHS_Index != 0,
     Avg_GDP != 0,
     Avg_Climate_Vul != 0,
     Avg_HDI != 0
   )
 
 final_data <- final_data %>%
   mutate(across(
     c(n_entries_biodiv, Bio_Words, Bio_Sources, Bio_Sections, Bio_Images, Bio_Page_Views, 
       Bio_Edits_30_Days, Bio_Links_to_Wikipedia, n_entries_climate, clim_Words, clim_Sources, 
       clim_Sections, clim_Images, clim_Page_Views, clim_Edits_30_Days, 
       clim_Links_to_Wikipedia, n_entries_pubhealth, pub_Words, pub_Sources, pub_Sections, 
       pub_Images, pub_Page_Views, pub_Edits_30_Days, pub_Links_to_Wikipedia), 
     ~ replace_na(., 0)
   ))
 
 final_data <- final_data %>%
   dplyr::inner_join(
     ethno_collapsed %>% dplyr::select(ISO_639, Total_L1_Users),
     by = "ISO_639"
   )
 
 
 #### Analysis 1 - Number of Articles ####
 final_data <- final_data %>%
   mutate(
     log10_n_entries_biodiv = log10(n_entries_biodiv),
     log10_n_entries_climate = log10(n_entries_climate),
     log10_n_entries_pubhealth = log10(n_entries_pubhealth),
     log10_Total_Threatened_Species = log10(Total_Threatened_Species),
     log10_Total_L1_Users = log(Total_L1_Users),
     log10_Avg_GDP = log(Avg_GDP))
 
 ## Biodiversity Conservation ##
 biodiv_data_clean <- final_data %>%
   filter(!is.na(n_entries_biodiv), !is.na(n_entries_biodiv))
 
 nb_b <- glm.nb(n_entries_biodiv ~ Total_Threatened_Species + Avg_GDP + 
                  Avg_HDI + log10_Total_L1_Users,
                data = final_data,
                control = glm.control(maxit = 45))
 summary(nb_b)
 vif(nb_b)
 
 ## Threatened Species ##
 var_seq <- seq(min(biodiv_data_clean$Total_Threatened_Species, na.rm = TRUE), 
                max(biodiv_data_clean$Total_Threatened_Species, na.rm = TRUE), 
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Total_Threatened_Species, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$Total_Threatened_Species <- var_seq  
 
 mm <- model.matrix(~ Total_Threatened_Species + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(nb_b))
 se_link <- sqrt(diag(mm %*% vcov(nb_b) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 b_t <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = Total_Threatened_Species, ymin = lower_ci, ymax = upper_ci), 
               fill = "green", alpha = 0.2) +
   geom_line(data = pred_df, aes(x = Total_Threatened_Species, y = fit), color = "darkgreen") +
   scale_y_log10() +
   scale_x_log10() +
   geom_point(data = final_data, aes(x = Total_Threatened_Species, y = n_entries_biodiv), alpha = 0.5, color = point_color) +
   labs(x = "Total Threatened Species (log10 Transformed)", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = "Biodiversity Conservation") +
   theme_minimal()
 
 ## Avg_HDI ##
 var_seq <- seq(range(biodiv_data_clean$Avg_HDI, na.rm = TRUE)[1],
                range(biodiv_data_clean$Avg_HDI, na.rm = TRUE)[2],
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Total_Threatened_Species, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$Avg_HDI <- var_seq  
 
 mm <- model.matrix(~ Total_Threatened_Species + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(nb_b))
 se_link <- sqrt(diag(mm %*% vcov(nb_b) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 b_HDI <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = Avg_HDI, ymin = lower_ci, ymax = upper_ci), 
               fill = "green", alpha = 0.2) +
   geom_line(data = pred_df, aes(x = Avg_HDI, y = fit), color = "darkgreen") +
   scale_y_log10() +
   geom_point(data = final_data, aes(x = Avg_HDI, y = n_entries_biodiv), alpha = 0.5, color = "black") +
   labs(x = "Average HDI per Language", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = " ") +
   theme_minimal()
 
 ## L1 Speakers
 var_seq <- seq(range(biodiv_data_clean$log10_Total_L1_Users, na.rm = TRUE)[1],
                range(biodiv_data_clean$log10_Total_L1_Users, na.rm = TRUE)[2],
                length.out = 300)
 
 pred_df <- biodiv_data_clean %>%
   summarise(across(c(Total_Threatened_Species, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$log10_Total_L1_Users <- var_seq  # Vary L1 Users
 
 mm <- model.matrix(~ Total_Threatened_Species + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(nb_b))
 se_link <- sqrt(diag(mm %*% vcov(nb_b) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 b_L1 <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = log10_Total_L1_Users, ymin = lower_ci, ymax = upper_ci), 
               fill = "green", alpha = 0.2) +
   geom_line(data = pred_df, aes(x = log10_Total_L1_Users, y = fit), color = "darkgreen") +
   scale_y_log10() +
   geom_point(data = final_data, aes(x = log10_Total_L1_Users, y = n_entries_biodiv), alpha = 0.5, color = "black") +
   labs(x = "log10 Transformed Total L1 Users", y = "", 
        title = " ") +
   theme_minimal()
 
 ## Climate Change ##
 clim_data_clean <- final_data %>%
   filter(!is.na(n_entries_climate), !is.na(n_entries_climate))
 
 nb_c <- glm.nb(n_entries_climate ~ Avg_Climate_Vul + Avg_GDP + 
                  Avg_HDI + log10_Total_L1_Users,
                data = final_data)
 summary(nb_c)
 vif(nb_c)
 
 ## Climate Vulnerability (not significant) ##
 c_t <- ggplot() +
   geom_point(data = final_data, aes(x = Avg_Climate_Vul, y = n_entries_climate), alpha = 0.5, color = "black") +
   scale_y_log10() +
   labs(x = "Average Climate Vulnerability", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = "Climate Change") +
   theme_minimal()
 
 ## Avg_HDI (not significant) ##
 c_HDI <- ggplot() +
   geom_point(data = final_data, aes(x = Avg_HDI, y = n_entries_climate), alpha = 0.5, color = "black") +
   scale_y_log10() +
   labs(x = "Average HDI per Language", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = " ") +
   theme_minimal()
 
 ## L1 Speakers
 var_seq <- seq(range(clim_data_clean$log10_Total_L1_Users, na.rm = TRUE)[1],
                range(clim_data_clean$log10_Total_L1_Users, na.rm = TRUE)[2],
                length.out = 300)
 
 pred_df <- clim_data_clean %>%
   summarise(across(c(Avg_Climate_Vul, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$log10_Total_L1_Users <- var_seq  
 
 mm <- model.matrix(~ Avg_Climate_Vul + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(nb_c))
 se_link <- sqrt(diag(mm %*% vcov(nb_c) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 c_L1 <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = log10_Total_L1_Users, ymin = lower_ci, ymax = upper_ci), 
               fill = "green", alpha = 0.2) +
   geom_line(data = pred_df, aes(x = log10_Total_L1_Users, y = fit), color = "darkgreen") +
   scale_y_log10() +
   geom_point(data = final_data, aes(x = log10_Total_L1_Users, y = n_entries_climate), alpha = 0.5, color = "black") +
   labs(x = "log10 Transformed Total L1 Users", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = " ") +
   theme_minimal()
 
 ## Public Health ##
 PubHealth_clean <- final_data %>%
   filter(!is.na(n_entries_pubhealth), !is.na(n_entries_pubhealth))
 
 nb_p <- glm.nb(n_entries_pubhealth ~ Avg_GHS_Index + Avg_GDP + 
                  Avg_HDI  + log10_Total_L1_Users,
                data = final_data,
                control = glm.control(maxit = 45))
 summary(nb_p)
 vif(nb_p)
 
 ## Average GHS (not significant) ##
 p_t <- ggplot() +
   geom_point(data = final_data, aes(x = Avg_GHS_Index, y = n_entries_pubhealth), alpha = 0.5, color = "black") +
   scale_y_log10() +
   labs(x = "Total Threatened Species", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = "Public Health") +
   theme_minimal()
 
 ## Avg_HDI ##
 var_seq <- seq(range(PubHealth_clean$Avg_HDI, na.rm = TRUE)[1],
                range(PubHealth_clean$Avg_HDI, na.rm = TRUE)[2],
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Avg_GHS_Index, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$Avg_HDI <- var_seq  
 
 mm <- model.matrix(~ Avg_GHS_Index + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(nb_p))
 se_link <- sqrt(diag(mm %*% vcov(nb_p) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 p_HDI <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = Avg_HDI, ymin = lower_ci, ymax = upper_ci), 
               fill = "green", alpha = 0.2) +
   geom_line(data = pred_df, aes(x = Avg_HDI, y = fit), color = "darkgreen") +
   scale_y_log10() +
   geom_point(data = final_data, aes(x = Avg_HDI, y = n_entries_pubhealth), alpha = 0.5, color = "black") +
   labs(x = "Average HDI per Language", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = " ") +
   theme_minimal()
 
 ## L1 Speakers
 var_seq <- seq(range(PubHealth_clean$log10_Total_L1_Users, na.rm = TRUE)[1],
                range(PubHealth_clean$log10_Total_L1_Users, na.rm = TRUE)[2],
                length.out = 300)
 
 pred_df <- PubHealth_clean %>%
   summarise(across(c(Avg_GHS_Index, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$log10_Total_L1_Users <- var_seq  # Vary L1 Users
 
 mm <- model.matrix(~ Avg_GHS_Index + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(nb_p))
 se_link <- sqrt(diag(mm %*% vcov(nb_p) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 p_L1 <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = log10_Total_L1_Users, ymin = lower_ci, ymax = upper_ci), 
               fill = "green", alpha = 0.2) +
   geom_line(data = pred_df, aes(x = log10_Total_L1_Users, y = fit), color = "darkgreen") +
   scale_y_log10() +
   geom_point(data = final_data, aes(x = log10_Total_L1_Users, y = n_entries_pubhealth), alpha = 0.5, color = "black") +
   labs(x = "log10 Transformed Total L1 Users", y = "Number of Wikipedia Articles\n(log10 Transformed)", 
        title = " ") +
   theme_minimal()
 
 (b_t + c_t + p_t) / (b_HDI + c_HDI + p_HDI) / (b_L1 + c_L1 + p_L1)
 #### Analysis 2 - Median content ####
 final_data <- final_data %>%
   mutate(
     Bio_Words_int = round(Bio_Words),
     Bio_Sources_int = round(Bio_Sources),
     Bio_Sections_int = round(Bio_Sections),
     Bio_Images_int = round(Bio_Images),
     Bio_Page_Views_int = round(Bio_Page_Views),
     Bio_Edits_30_Days_int = round(Bio_Edits_30_Days),
     Bio_Links_to_Wikipedia_int = round(Bio_Links_to_Wikipedia),
     clim_Words_int = round(clim_Words),
     clim_Sources_int = round(clim_Sources),
     clim_Sections_int = round(clim_Sections),
     clim_Images_int = round(clim_Images),
     clim_Page_Views_int = round(clim_Page_Views),
     clim_Edits_30_Days_int = round(clim_Edits_30_Days),
     clim_Links_to_Wikipedia_int = round(clim_Links_to_Wikipedia),
     pub_Words_int = round(pub_Words),
     pub_Sources_int = round(pub_Sources),
     pub_Sections_int = round(pub_Sections),
     pub_Images_int = round(pub_Images),
     pub_Page_Views_int = round(pub_Page_Views),
     pub_Edits_30_Days_int = round(pub_Edits_30_Days),
     pub_Links_to_Wikipedia_int = round(pub_Links_to_Wikipedia))
 
 lm_median_b <- glm.nb(Bio_Words_int ~ Total_Threatened_Species + Avg_GDP + 
                         Avg_HDI + log10_Total_L1_Users,
                       data = final_data,
                       control = glm.control(maxit = 25))
 summary(lm_median_b)
 vif(lm_median_b)
 
 lm_median_c <- glm.nb(clim_Words_int ~ Avg_Climate_Vul + Avg_GDP + 
                         Avg_HDI + log10_Total_L1_Users,
                       data = final_data,
                       control = glm.control(maxit = 25))
 summary(lm_median_c)
 vif(lm_median_c)
 
 lm_median_p <- glm.nb(pub_Words_int ~ Avg_GHS_Index + Avg_GDP + 
                         Avg_HDI + log10_Total_L1_Users,
                       data = final_data,
                       control = glm.control(maxit = 25))
 summary(lm_median_p)
 vif(lm_median_p)
 
 ## Biodiversity Conservation ##
 # HDI Prediction
 var_seq <- seq(min(biodiv_data_clean$Avg_HDI, na.rm = TRUE), 
                max(biodiv_data_clean$Avg_HDI, na.rm = TRUE), 
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Total_Threatened_Species, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$Avg_HDI <- var_seq
 
 mm <- model.matrix(~ Total_Threatened_Species + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(lm_median_b))
 se_link <- sqrt(diag(mm %*% vcov(lm_median_b) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 b_HDI <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = Avg_HDI, ymin = lower_ci, ymax = upper_ci), fill = fill_color, alpha = 0.2) +
   geom_line(data = pred_df, aes(x = Avg_HDI, y = fit), color = line_color) +
   geom_point(data = final_data, aes(x = Avg_HDI, y = Bio_Words_int), alpha = 0.5) +
   scale_y_log10() +
   labs(x = "Avg HDI per Language", y = "", 
        title = "Biodiversity Conservation",
        subtitle = "A") +
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5, size = 14),       
         plot.subtitle = element_text(hjust = 0, size = 14))
 
 ## GDP Prediction
 var_seq <- seq(min(biodiv_data_clean$Avg_GDP, na.rm = TRUE), 
                max(biodiv_data_clean$Avg_GDP, na.rm = TRUE), 
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Total_Threatened_Species, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$Avg_GDP <- var_seq
 
 mm <- model.matrix(~ Total_Threatened_Species + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(lm_median_b))
 se_link <- sqrt(diag(mm %*% vcov(lm_median_b) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 b_GDP <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = Avg_GDP, ymin = lower_ci, ymax = upper_ci), fill = fill_color, alpha = 0.2) +
   geom_line(data = pred_df, aes(x = Avg_GDP, y = fit), color = line_color) +
   geom_point(data = final_data, aes(x = Avg_GDP, y = Bio_Words_int), alpha = 0.5) +
   scale_y_log10() +
   labs(x = "Average GDP per Language", y = "Median Number of Words per Language (log10 Transformed)",
        subtitle = "D") +
   theme_minimal() + 
   theme(plot.subtitle = element_text(hjust = 0, size = 14))
 
 ## L1 Speakers
 var_seq <- seq(min(biodiv_data_clean$log10_Total_L1_Users, na.rm = TRUE), 
                max(biodiv_data_clean$log10_Total_L1_Users, na.rm = TRUE), 
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Total_Threatened_Species, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$log10_Total_L1_Users <- var_seq
 
 mm <- model.matrix(~ Total_Threatened_Species + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(lm_median_b))
 se_link <- sqrt(diag(mm %*% vcov(lm_median_b) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 b_L1 <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = log10_Total_L1_Users, ymin = lower_ci, ymax = upper_ci), fill = fill_color, alpha = 0.2) +
   geom_line(data = pred_df, aes(x = log10_Total_L1_Users, y = fit), color = line_color) +
   geom_point(data = final_data, aes(x = log10_Total_L1_Users, y = Bio_Words_int), alpha = 0.5) +
   scale_y_log10() +
   labs(x = "log10 Transformed Total L1 Users", y = "",
        subtitle = "G") +
   theme_minimal()
 
 ## Climate Change ##
 
 ## L1 Trendline
 var_seq <- seq(min(clim_data_clean$log10_Total_L1_Users, na.rm = TRUE), 
                max(clim_data_clean$log10_Total_L1_Users, na.rm = TRUE), 
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Avg_Climate_Vul, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$log10_Total_L1_Users <- var_seq
 
 mm <- model.matrix(~ Avg_Climate_Vul + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(lm_median_c))
 se_link <- sqrt(diag(mm %*% vcov(lm_median_c) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 c_L1 <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = log10_Total_L1_Users, ymin = lower_ci, ymax = upper_ci), fill = fill_color, alpha = 0.2) +
   geom_line(data = pred_df, aes(x = log10_Total_L1_Users, y = fit), color = line_color) +
   geom_point(data = final_data, aes(x = log10_Total_L1_Users, y = clim_Words_int), alpha = 0.5) +
   scale_y_log10() +
   labs(x = "log10 Transformed Total L1 Users", y = "",
        subtitle = "H") +
   theme_minimal() + 
   theme(plot.subtitle = element_text(hjust = 0, size = 14))
 
 ## GDP and HDI Points Only
 c_GDP <- ggplot(final_data, aes(x = Avg_GDP, y = clim_Words_int)) +
   geom_point(alpha = 0.5, color = point_color) +
   scale_y_log10() +
   labs(x = "Average GDP per Language", y = "",
        subtitle = "E") +
   theme_minimal() + 
   theme(plot.subtitle = element_text(hjust = 0, size = 14))
 
 c_HDI <- ggplot(final_data, aes(x = Avg_HDI, y = clim_Words_int)) +
   geom_point(alpha = 0.5, color = point_color) +
   scale_y_log10() +
   labs(x = "Average HDI per Language", y = "", 
        title = "Climate Change",
        subtitle = "B") +
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5, size = 14),       
         plot.subtitle = element_text(hjust = 0, size = 14))
 
 
 ## Public Health ##
  # GDP Prediction
 var_seq <- seq(min(PubHealth_clean$Avg_GDP, na.rm = TRUE), 
                max(PubHealth_clean$Avg_GDP, na.rm = TRUE), 
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Avg_GHS_Index, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$Avg_GDP <- var_seq
 
 mm <- model.matrix(~ Avg_GHS_Index + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(lm_median_p))
 se_link <- sqrt(diag(mm %*% vcov(lm_median_p) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 p_GDP <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = Avg_GDP, ymin = lower_ci, ymax = upper_ci), fill = "green", alpha = 0.2) +
   geom_line(data = pred_df, aes(x = Avg_GDP, y = fit), color = "darkgreen") +
   geom_point(data = final_data, aes(x = Avg_GDP, y = pub_Words_int), alpha = 0.5) +
   scale_y_log10() +
   labs(x = "Average GDP per Language", y = "",
        subtitle = "F") +
   theme_minimal() + 
   theme(plot.subtitle = element_text(hjust = 0, size = 14))
 
 ## HDI
 var_seq <- seq(min(PubHealth_clean$Avg_HDI, na.rm = TRUE), 
                max(PubHealth_clean$Avg_HDI, na.rm = TRUE), 
                length.out = 300)
 
 pred_df <- final_data %>%
   summarise(across(c(Avg_GHS_Index, Avg_GDP, Avg_HDI, log10_Total_L1_Users), ~mean(.x, na.rm = TRUE))) %>%
   slice(rep(1, 300))
 pred_df$Avg_HDI <- var_seq
 
 mm <- model.matrix(~ Avg_GHS_Index + Avg_GDP + Avg_HDI + log10_Total_L1_Users, data = pred_df)
 pred_link <- as.vector(mm %*% coef(lm_median_p))
 se_link <- sqrt(diag(mm %*% vcov(lm_median_p) %*% t(mm)))
 
 pred_df$fit <- exp(pred_link)
 pred_df$lower_ci <- exp(pred_link - 1.96 * se_link)
 pred_df$upper_ci <- exp(pred_link + 1.96 * se_link)
 
 p_HDI <- ggplot() +
   geom_ribbon(data = pred_df, aes(x = Avg_HDI, ymin = lower_ci, ymax = upper_ci), fill = fill_color, alpha = 0.2) +
   geom_line(data = pred_df, aes(x = Avg_HDI, y = fit), color = line_color) +
   geom_point(data = final_data, aes(x = Avg_HDI, y = pub_Words_int), alpha = 0.5) +
   scale_y_log10() +
   labs(x = "Average HDI per Language", y = "", 
        title = "Public Health",
        subtitle = "C") +
   theme_minimal() + 
   theme(plot.title = element_text(hjust = 0.5, size = 14),       
         plot.subtitle = element_text(hjust = 0, size = 14))
 
 ## L1 Points Only
 p_L1 <- ggplot(final_data, aes(x = log10_Total_L1_Users, y = pub_Words_int)) +
   geom_point(alpha = 0.5, color = point_color) +
   scale_y_log10() +
   labs(x = "log10 Transformed Total L1 Users", y = "",
        subtitle = "I") +
   theme_minimal() + 
   theme(plot.subtitle = element_text(hjust = 0, size = 14))
 
 
 ###  Combine all into 3x3 grid 
 combined_plot <- (b_HDI | c_HDI | p_HDI) /
   (b_GDP | c_GDP | p_GDP) /
   (b_L1 | c_L1 | p_L1)
 
 print(combined_plot)
 
 #### Analysis 3 - Average Slope ####
 BiodivCons_clean <- BiodivCons %>%
   mutate(
     date = dmy(date),  
     month = floor_date(date, unit = "month")
   )
 
 # Count monthly documents
 b_monthly_counts <- BiodivCons_clean %>%
   group_by(Category, ISO_639, month) %>%
   summarise(doc_count = n(), .groups = "drop")
 
 # Compute cumulative documents
 biodiversity_df <- b_monthly_counts %>%
   arrange(Category, ISO_639, month) %>%
   group_by(Category, ISO_639) %>%
   mutate(cumulative_docs = cumsum(doc_count)) %>%
   ungroup()
 
 # Filter out languages with fewer than 5 total documents
 language_counts <- BiodivCons_clean %>%
   group_by(ISO_639) %>%
   summarise(total_docs = n(), .groups = "drop")
 
 valid_languages <- language_counts %>%
   filter(total_docs >= 5) %>%
   pull(ISO_639)
 
 biodiversity_df_filtered <- biodiversity_df %>%
   filter(ISO_639 %in% valid_languages)
 
 # Calculate average slope (monthly growth) per Category × Language
 b_average_slopes <- biodiversity_df_filtered %>%
   group_by(Category, ISO_639) %>%
   filter(n() >= 2) %>%
   summarise(
     avg_slope = coef(lm(cumulative_docs ~ as.numeric(month)))[2],
     .groups = "drop")
 
 # Join metadata
 b_average_slopes <- b_average_slopes %>%
   inner_join(ethno_collapsed %>% dplyr::select(ISO_639, Total_L1_Users), by = "ISO_639") %>%
   left_join(language_metrics, by = "ISO_639")
 
 # Fit Gamma GLM with log link
 b_glm <- glm(avg_slope ~ Total_Threatened_Species + Avg_GDP + Avg_HDI + Total_L1_Users,
              data = b_average_slopes,
              family = Gamma(link = "log"))
 
 # Model summary
 summary(b_glm)
 
 
 ## Climate Change ##
 ClimateCh_clean <- ClimateCh %>%
   mutate(
     date = dmy(date),
     month = floor_date(date, unit = "month")
   )
 
 # Count documents per Category × Language × Month
 c_monthly_counts <- ClimateCh_clean %>%
   group_by(Category, ISO_639, month) %>%
   summarise(doc_count = n(), .groups = "drop")
 
 # Compute cumulative document counts
 clim_df <- c_monthly_counts %>%
   arrange(Category, ISO_639, month) %>%
   group_by(Category, ISO_639) %>%
   mutate(cumulative_docs = cumsum(doc_count)) %>%
   ungroup()
 
 # Filter out languages with fewer than 5 total documents
 language_counts <- ClimateCh_clean %>%
   group_by(ISO_639) %>%
   summarise(total_docs = n(), .groups = "drop")
 
 valid_languages <- language_counts %>%
   filter(total_docs >= 5) %>%
   pull(ISO_639)
 
 clim_df_filtered <- clim_df %>%
   filter(ISO_639 %in% valid_languages)
 
 # Calculate average monthly slope
 c_average_slopes <- clim_df_filtered %>%
   group_by(Category, ISO_639) %>%
   filter(n() >= 2) %>%
   summarise(
     avg_slope = coef(lm(cumulative_docs ~ as.numeric(month)))[2],
     .groups = "drop")
 
 # Join with metadata
 c_average_slopes <- c_average_slopes %>%
   inner_join(ethno_collapsed %>% dplyr::select(ISO_639, Total_L1_Users), by = "ISO_639") %>%
   left_join(language_metrics, by = "ISO_639")
 
 # Fit Gamma GLM with log link
 c_glm <- glm(avg_slope ~ Avg_Climate_Vul + Avg_GDP + Avg_HDI + Total_L1_Users,
              data = c_average_slopes,
              family = Gamma(link = "log"))
 
 # Summarise model
 summary(c_glm)
 
 PubHealth_clean <- PubHealth %>%
   mutate(
     date = dmy(date),
     month = floor_date(date, unit = "month")
   )
 
 # Count how many entries per language overall
 language_doc_counts <- PubHealth_clean %>%
   group_by(ISO_639) %>%
   summarise(total_docs = n(), .groups = "drop") %>%
   filter(total_docs >= 5)  # Only keep languages with 5+ docs
 
 # Filter to those languages only
 PubHealth_filtered <- PubHealth_clean %>%
   semi_join(language_doc_counts, by = "ISO_639")
 
 # Monthly counts
 p_monthly_counts <- PubHealth_filtered %>%
   group_by(Category, ISO_639, month) %>%
   summarise(doc_count = n(), .groups = "drop")
 
 # Cumulative docs
 pubh_df <- p_monthly_counts %>%
   arrange(Category, ISO_639, month) %>%
   group_by(Category, ISO_639) %>%
   mutate(cumulative_docs = cumsum(doc_count)) %>%
   ungroup()
 
 # Calculate average slope using linear regression
 p_average_slopes <- pubh_df %>%
   group_by(Category, ISO_639) %>%
   filter(n() >= 2) %>%
   summarise(
     avg_slope = coef(lm(cumulative_docs ~ as.numeric(month)))[2],
     .groups = "drop") 
 
 # Join language metadata
 p_average_slopes <- p_average_slopes %>%
   inner_join(ethno_collapsed %>% dplyr::select(ISO_639, Total_L1_Users), by = "ISO_639") %>%
   left_join(language_metrics, by = "ISO_639")
 
 # Fit Gamma GLM
 p_glm <- glm(avg_slope ~ Avg_GHS_Index + Avg_GDP + Avg_HDI + Total_L1_Users,
              data = p_average_slopes,
              family = Gamma(link = "log"))
 
 summary(p_glm)