set_apikey <- function(key, file_renviron = path_renviron,
                       restart = T) {

  if (!file.exists(file_renviron)) {
    file.create(file_renviron)
    con <- file(file_renviron)
    writeLines(sprintf("IVOLATILITY_API_KEY='%s'", key), con)
    close(con)
  } else {
    lines <- readLines(file_renviron)
    index <- which(grepl("IVOLATILITY_API_KEY='*'", gsub("\\s", "", lines)))
    lines[index] <- sprintf("IVOLATILITY_API_KEY='%s'", key)
    writeLines(lines, file_renviron)
  }

  if (restart) .rs.restartR()

}

get_apikey <- function(file_renviron = path_renviron()) {

  Sys.getenv("IVOLATILITY_API_KEY")

}

path_renviron <- function() file.path(getwd(), ".Renviron")
