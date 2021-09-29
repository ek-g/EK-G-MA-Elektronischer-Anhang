temp_data <- file.path("data", "all_links.RDS")

library(rvest)
library(stringr)
library(purrr)
library(tidyverse)

if(!file.exists(temp_data)) {
  
  ## DBFK
  
  dbfk_seite <- read_html("https://www.dbfk.de/de/veroeffentlichungen/Positionspapiere.php")
  
  dbfk_links <- dbfk_seite %>% 
    html_nodes(".download") %>% 
    html_attr("href") %>% 
    na.omit(.) %>% 
    paste0("https://www.dbfk.de/", .)
  
  dbfk_data <- list(
    url = dbfk_links,
    
    destfile = file.path("data",
                         "positionspapiere",
                         "dbfk",
                         basename(dbfk_links)))
  
  ## DPR
  
  dpr_seite <- read_html("https://deutscher-pflegerat.de/category/positionen/")
  
  dpr_links <- dpr_seite %>% 
    html_nodes(".cat-item a") %>% 
    html_attr("href") %>% 
    .[3:length(.)]
  
  dpr_html <- dpr_links %>% 
    map(read_html)
  
  dpr_href <- dpr_html %>% 
    map(html_nodes, ".more-link") %>% 
    map(html_attr, "href") %>% 
    unlist()
  
  dpr_html2 <- dpr_href %>% 
    map(read_html)
  
  dpr_finallinks <- dpr_html2 %>% 
    map(html_nodes, ".regular-button") %>% 
    map(html_attr, "href") %>% 
    unlist() %>% 
    na.omit(.)
  
  dpr_data <- list(url = dpr_finallinks,
                   
                   destfile = file.path("data",
                                        "positionspapiere",
                                        "dpr",
                                        basename(dpr_finallinks)))
  
  ## DGP
  
  dgp_seite <- read_html("https://dg-pflegewissenschaft.de/veroeffentlichungen/positionen-stellungnahmen/")
  
  dgp_links <- dgp_seite %>% 
    html_nodes("strong a") %>% 
    html_attr("href") %>% 
    str_remove_all(., "https://dg-pflegewissenschaft.de") %>% 
    paste0("https://dg-pflegewissenschaft.de", .)
  
  dgp_data <- list(
    url = dgp_links,
    
    destfile = file.path("data",
                         "positionspapiere",
                         "dgp",
                         basename(dgp_links)))
  
  ## DBVA 
  
  dbva_seite <- read_html("http://www.dbva.de/stellungnahmen.html")
  
  dbva_links <- dbva_seite %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    .[str_detect(., "\\.pdf$")] %>% 
    str_remove_all(., "^/") %>% 
    paste0("http://www.dbva.de/", .)
  
  dbva_data <- list(
    url = dbva_links,
    
    destfile = file.path("data",
                         "positionspapiere",
                         "dbva",
                         basename(dbva_links)))
  
  ## BeKD
  
  bekd_seite <- read_html("https://bekd.de/stellungnahmen/")
  
  bekd_links <- bekd_seite %>% 
    html_nodes(".vc_btn3-style-custom") %>% 
    html_attr("href") %>% 
    str_remove_all(., "https://bekd.de") %>% 
    paste0("https://bekd.de", .)
  
  bekd_data <- list(
    url = bekd_links,
    
    destfile = file.path("data",
                         "positionspapiere",
                         "bekd",
                         basename(bekd_links)))
  
  ## VPU
  
  vpu_seite <- read_html("https://www.vpuonline.de/newsletterpresse/pressemitteilungen-publikationen-in-fachzeitschriften/")
  
  vpu_links <- vpu_seite %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    na.omit(.) %>% 
    .[str_detect(., "\\.pdf$")] %>% 
    # zusätzliche Datei:
    c(., "https://www.vpuonline.de/de/pdf/presse/VPU_--_Pflege_an_Universitaetskliniken.pdf")
  
  vpu_data <- list(
    url = vpu_links,
    
    destfile = file.path("data",
                         "positionspapiere",
                         "vpu",
                         basename(vpu_links)))
  
  all_links <- c(dbfk_links, dpr_finallinks, dbva_links, bekd_links, vpu_links)
  all_links <- tibble(URL = all_links, Datei = basename(all_links))
  
  saveRDS(all_links, file = file.path("data", "all_links.RDS"))
} else all_links <- readRDS(temp_data)

## Download

# Nur herunterladen, wenn Daten nicht bereits exportiert sind

if(!file.exists(temp_data)) {
  
  dbfk_data %>% 
    pmap(download.file, method = "libcurl")
  
  dpr_data %>% 
    pmap(download.file, method = "libcurl")
  
  dbva_data %>% 
    pmap(download.file, method = "libcurl")
  
  bekd_data %>% 
    pmap(download.file, method = "libcurl")
  
  vpu_data %>% 
    pmap(download.file, method = "libcurl")
  
  dgp_data %>% 
    pmap(download.file, method = "libcurl")
  
}

rm(temp_data)

# Pflegekammer Stoppen.de

pflegekammer_stoppen_webseite <- "https://www.pflegekammer-stoppen.de/downloads.html"

pflegekammer_stoppen <- read_html(pflegekammer_stoppen_webseite )

pflegekammer_stoppen_links <- pflegekammer_stoppen %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  .[str_detect(., "\\.pdf$")] %>% 
  str_remove_all("\\.\\./?") %>% 
  paste0(dirname(pflegekammer_stoppen_webseite), "/", .) %>% 
  unique(.)

pflegekammer_stoppen_data <- list(
  url = pflegekammer_stoppen_links,
  
  destfile = file.path("data",
                       "positionspapiere",
                       "andere",
                       basename(pflegekammer_stoppen_links)))

pflegekammer_stoppen_data %>% 
  pmap(download.file, method = "libcurl")

# ver.di offizielle Position zu Pflegekammer

verdi_links <- c(
  "https://gesundheit-soziales.verdi.de/++file++58497de07713b840629c8c00/download/Pflegekammer_Position.pdf",
  "https://gesundheit-soziales.verdi.de/++file++5a81ac0ef1b4cd36e1856961/download/180206_Info_Pflegekammer.pdf",
  # manuell: "https://gesundheit-soziales.verdi.de/++file++5a81abd6f1b4cd36e1856959/download/180119_broschuere-pflegekammer%20BaWü%202018%20web.pdf",
  "https://mittelbaden.verdi.de/++file++576cfa3dbdf98d36d7e5b151/download/KHI%20Pflegekammer.pdf",
  "https://nds-bremen.verdi.de/++file++5672c0beaa698e1ac5000097/download/reader%20kammer%20nds%20medium.pdf",
  "https://gesundheit-soziales.verdi.de/++file++5c44a0f32d9efb3070ffb99b/download/2018-03-14%20V03-18-019%20Flyer%20Pflegekammer%20Brandenburg-Einzel-DL4b.pdf",
  "https://nds-bremen.verdi.de/++file++5d7a2873e999fb208541279b/download/_Nds%20Pflegendenvereinigung%20Sept.2019%20web.pdf"
)

verdi_data <- list(
  url = verdi_links,
  
  destfile = file.path("data",
                       "positionspapiere",
                       "verdi",
                       basename(verdi_links))
)

verdi_data %>% 
  pmap(download.file, method = "libcurl")

web_links <- c(pflegekammer_stoppen_links, verdi_links)
web_links <- tibble(URL = web_links, Datei = basename(web_links))

all_links <- readRDS(file.path("data", "temp", "all_links.RDS"))

all_links <- dplyr::bind_rows(all_links, web_links)