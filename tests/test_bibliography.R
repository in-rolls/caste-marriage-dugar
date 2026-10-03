setwd(Sys.getenv("PROJECT_ROOT", unset = ".."))

normalize_bib <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  gsub("[^a-z0-9]", "", tolower(x))
}

test_that("the bibliography parses and matches verified publisher metadata", {
  expect_true(nzchar(Sys.which("pandoc")))
  parsed <- system2("pandoc", c("-f", "bibtex", "-t", "csljson", "ms/references.bib"), stdout = TRUE)
  expect_null(attr(parsed, "status"))
  bibliography <- jsonlite::fromJSON(paste(parsed, collapse = "\n"), simplifyVector = FALSE)
  metadata <- jsonlite::fromJSON("sources/bibliography_metadata.json", simplifyVector = FALSE)$records
  keys <- vapply(bibliography, `[[`, character(1), "id")
  expect_setequal(keys, names(metadata))
  expect_equal(anyDuplicated(keys), 0L)
  for (entry in bibliography) {
    source <- metadata[[entry$id]]
    expect_equal(tolower(entry$DOI), tolower(source$DOI), label = entry$id)
    expect_equal(normalize_bib(entry$title), normalize_bib(source$title[[1]]), label = entry$id)
    expect_equal(normalize_bib(sub("^The ", "", entry[["container-title"]])),
      normalize_bib(sub("^The ", "", source[["container-title"]][[1]])),
      label = entry$id
    )
    expect_equal(entry$volume, source$volume, label = entry$id)
    expect_equal(entry$issue, source$issue, label = entry$id)
    expect_equal(normalize_bib(entry$page), normalize_bib(source$page), label = entry$id)
    issue_date <- source[["published-print"]]
    if (is.null(issue_date)) issue_date <- source$published
    expect_equal(entry$issued[["date-parts"]][[1]][[1]],
      issue_date[["date-parts"]][[1]][[1]],
      label = entry$id
    )
    names_in_bib <- vapply(entry$author, function(a) paste(a$given, a$family), character(1))
    names_in_source <- vapply(source$author, function(a) paste(a$given, a$family), character(1))
    expect_equal(normalize_bib(names_in_bib), normalize_bib(names_in_source), label = entry$id)
  }
})

test_that("all manuscript citations resolve to the verified bibliography", {
  manuscript <- paste(readLines("ms/paper.Rmd", warn = FALSE), collapse = "\n")
  citations <- regmatches(manuscript, gregexpr("@[a-z][a-z0-9]+", manuscript))[[1]]
  keys <- names(jsonlite::fromJSON("sources/bibliography_metadata.json", simplifyVector = FALSE)$records)
  expect_gt(length(citations), 0L)
  expect_true(all(sub("^@", "", citations) %in% keys))
})
