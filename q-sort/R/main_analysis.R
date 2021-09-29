library(qmethod)
library(tidyverse)
library(psych)
library(paran)

# Importiere Daten

import.htmlq(<CSV-Datei von HTMLQ>)

# Scree-Plot

## ggplot Einstellungen

ggplot2::theme_set(
  theme_linedraw(base_family = "URWClassico", base_size = 18)
)

ggplot_output_path <- file.path(getwd())


unrotated <- prcomp(q_sortdata[[1]], scale = TRUE)

eigenvalues_df <- data.frame(Faktor = 1:ncol(unrotated$x),
                             Eigenvalue = (unrotated$sdev)^2)

set.seed(2021)

sort_data <- q_sortdata[[1]]
sort_path <- file.path("data", "q-sort")

# Parallelanalyse nach Horn

parallel_analysis <- paran(sort_data)

parallel_simvals <- parallel_analysis$SimEvs

colnames(parallel_simvals) <- 1:ncol(parallel_simvals)

parallel_eigenvalue_means <- parallel_simvals %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), names_to = "Faktor", values_to = "Eigenvalue") %>% 
  mutate(Faktor = as.double(Faktor)) %>% 
  group_by(Faktor) %>% 
  summarise(Eigenvalue = sum(Eigenvalue) / nrow(parallel_simvals)) %>% 
  head(10)

red <- "#AD002AFF"
blue <- "#00468BFF"

eigenvalues_df %>%
  filter(Faktor < 11) %>%
  ggplot(aes(x = Faktor,y = Eigenvalue, group = 1)) +
  geom_point(size = 3) +
  geom_line() +
  geom_line(data = parallel_eigenvalue_means, color = red, linetype = "dashed", size = 1.2) +
  ylab("Eigenwert") +
  scale_x_continuous(breaks = 1:10, minor_breaks = NULL) +
  scale_y_continuous(breaks = c(1, 5, 10, 15)) +
  geom_path(y = rep(1, 10), x = 1:10, linetype = "dashed", color = blue, size = 1.2) +
  annotate("text", x = 3.95, y = 0.5, label = "Kaiser-Guttmann-Kriterium", color = blue,
           family= theme_get()$text[["family"]],
           size = 6) +
  annotate("text", x = 3.9, y = 3.8, label = "Parallelanalyse nach Horn", color = red,
           family= theme_get()$text[["family"]],
           size = 6)

ggsave(file.path(ggplot_output_path, "scree-plot_eigenvalue.pdf"),
       width = 250, height =150, units = "mm",
       device = cairo_pdf)

# Nach Entscheidung

if (!file.exists(file.path(sort_path, "final", "q_sorts_data.csv"))) {
  write_csv2(q_results$dataset, file.path(sort_path, "final", "q_sorts_data.csv"))
}

factor_chars_unrotated <- qmethod(sort_data, nfactors = 22, rotation = "none")$f_char$characteristics

q_results_4 <- qmethod(sort_data, nfactors = 4, rotation = "varimax")
q_results_4_unrotated <- qmethod(sort_data, nfactors = 5, rotation = "none")

factor_chars_4 <- q_results_4$f_char$characteristics

q_results_2 <- qmethod(sort_data, nfactors = 2, rotation = "varimax")
q_results_2_unrotated <- qmethod(sort_data, nfactors = 2, rotation = "none")

factor_chars_2 <- q_results_2$f_char$characteristics

q_results_list <- list(q_results_2, q_results_2_unrotated, q_results_4, q_results_4_unrotated)

factor_chars_list <- list(factor_chars_unrotated, factor_chars_4, factor_chars_2)

factor1_dem <- questionnaire %>% filter(q_results_2$flagged[,1]) %>% 
  #Entferne Q-Sorts mit unklarer Zuordnung
  filter(!sid %in% c(4, 5))
factor2_dem <- questionnaire %>% filter(q_results_2$flagged[,2])

factor1_ids <- factor1_dem %>% pull("Sort-ID")
factor2_ids <- factor2_dem %>% pull("Sort-ID")



