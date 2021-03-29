library(rjson)
library(stringr)
library(dplyr)

# ID's de todos os jogos regulares do Houston Rockets (HOU) 19/20
schedules <- c(15, 32, 45, 61, 69, 86, 93, 107, 129, 142, 157,
               169, 179, 193, 211, 224, 235, 259, 282, 303, 318, 
               333, 346, 356, 371, 386, 396, 419, 437, 453, 457,
               478, 488, 501, 520, 558, 566, 577, 600, 611, 633,
               645, 662, 675, 685, 698, 712, 722, 738, 752, 768,
               774, 790, 806, 825, 841, 853, 870, 892, 903, 927, 
               940, 953, 963, 1238, 1248, 1261, 1273, 1290, 1298,
               1304, 1315)
schedules <- paste0("002190", str_pad(as.character(schedules), 4, pad = "0"))

# Scrapping 
for (i in schedules){
  pbpURL <- paste0("http://data.nba.net/v2015/json/mobile_teams/nba/2019/scores/pbp/", i, "_full_pbp.json")
  check <- tryCatch(pbpData <- fromJSON(file = pbpURL, method = "C"), 
           warning = function(war) {
             message(paste("URL não encontrada no jogo", i))
             next
           },
           error = function(err) message("Pulando para o próximo jogo"),
           finally = function(f) print(paste("e: ", e)))
  
  pbpDataf <- as_tibble(matrix(unlist(pbpData$g$pd[[1]]$pla), ncol = 17, byrow = TRUE)) %>%
    mutate(qrt = "1st", .before = 1)
  for (j in 2:length(pbpData$g$pd)){
    pbpDataf <- bind_rows(pbpDataf, as_tibble(matrix(unlist(pbpData$g$pd[[j]]$pla), ncol = 17, byrow = TRUE)) %>%
                            mutate(qrt = paste0(j, "st"), .before = 1))
  }
  colnames(pbpDataf) <- c("qrt", "evt","cl","de","locX","locY","opt1","opt2","mtype","etype","opid","tid","pid","hs","vs","epid","oftid", "ord")
  pbpDataf %>% mutate(id = i) -> pbpDataf
  write.csv(pbpDataf, file = paste0("raw_data/", gsub("/","-",pbpData$g[3]$gcode),".csv"))
  message(paste0("Jogo ", i, ": ", gsub("/","-",pbpData$g[3]$gcode), " Importado com Sucesso!"))
}
