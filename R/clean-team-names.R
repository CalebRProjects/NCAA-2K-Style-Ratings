to_torvik_names <- function(x) {
  s <- as.character(x)
  s <- gsub("\u00A0", " ", s, fixed = TRUE)
  s <- gsub("\\s+NCAA\\b", "", s)
  s <- gsub("-", " ", s)
  s <- gsub("\\s+", " ", s)
  s <- trimws(s)
  s <- sub("^Miami \\(OH\\)$", "Miami OH", s)
  s <- sub("^Miami \\(FL\\)$", "Miami FL", s)
  s <- sub("^Queens \\(NC\\)$", "Queens", s)
  s <- sub("^Albany \\(NY\\)$", "Albany", s)
  s <- sub("^Saint Francis \\(PA\\)$", "St. Francis PA", s)
  s <- sub("^St\\. Francis \\(PA\\)$", "St. Francis PA", s)
  s <- sub("^St\\. Francis \\(NY\\)$", "St. Francis NY", s)
  s <- sub("^Saint Mary'?s \\(CA\\)$", "Saint Mary's", s)
  s <- gsub("^Cal State ", "Cal St. ", s)
  s <- gsub("\\bState\\b", "St.", s)
  map <- c(
    "Brigham Young"="BYU","Maryland Baltimore County"="UMBC","Kansas City"="UMKC",
    "Nevada Las Vegas"="UNLV","Virginia Commonwealth"="VCU","Southern Methodist"="SMU",
    "Southern California"="USC","South Carolina Upstate"="USC Upstate",
    "Florida International"="FIU","Texas Rio Grande Valley"="UT Rio Grande Valley",
    "California Baptist"="Cal Baptist","Purdue Fort Wayne"="Fort Wayne",
    "Detroit Mercy"="Detroit","Prairie View"="Prairie View A&M",
    "Massachusetts Lowell"="UMass Lowell","Long Island University"="LIU Brooklyn",
    "Louisiana"="Louisiana Lafayette","Texas A&M Corpus Christi"="Texas A&M Corpus Chris"
  )
  i <- match(tolower(s), tolower(names(map)))
  s[!is.na(i)] <- unname(map[i[!is.na(i)]])
  s
}

