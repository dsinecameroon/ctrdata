## Stats compilation

files_stat <- list.files("stats/", pattern = "rds", full.names = T)

stats_all <- lapply(files_stat, readRDS)

names(stats_all) <- c(2022, 2023, 2024, 2025)

#
for(nam in names(stats_all[[1]])){

  out[[nam]] <- lapply(stats_all, \(x) x[[nam]])

  out[[nam]] <- rbindlist(out[[nam]], fill = T)
  }


saveRDS(out, "stats/stats_compiled.rds")
