join_hdi_rating <- function(bt_data, hdi_data, year = 2023) {
  
  require(dplyr)
  require(stringr)
  
  normalize_name <- function(name) {
    name <- str_trim(name)
    name <- str_replace(name, "\\s+(Jr\\.?\\.?|Sr\\.?|III|II|IV|V)$", "")
    name <- str_replace_all(name, "[^[:alnum:]\\s]", "")
    name <- str_to_lower(name)
    name <- str_squish(name)
    return(name)
  }
  
  normalize_school <- function(school) {
    school <- str_trim(school)
    school_lower <- str_to_lower(str_replace_all(school, "\\.", ""))
    school_lower <- str_replace_all(school_lower, "-", " ")
    
    school_map <- c(
      "florida gulf coast" = "fgcu", "fgcu" = "fgcu",
      "northern illinois" = "niu", "niu" = "niu",
      "texas a&m corpus chris" = "am corpus christi",
      "am corpus christi" = "am corpus christi",
      "texas a&m commerce" = "tex am commerce",
      "tex am commerce" = "tex am commerce",
      "north carolina st" = "nc state", "nc state" = "nc state",
      "western kentucky" = "western ky", "western ky" = "western ky",
      "central michigan" = "central mich", "central mich" = "central mich",
      "southern illinois" = "southern ill", "southern ill" = "southern ill",
      "charleston southern" = "charleston so", "charleston so" = "charleston so",
      "mississippi valley st" = "mississippi val", "mississippi val" = "mississippi val",
      "central arkansas" = "central ark", "central ark" = "central ark",
      "northern arizona" = "northern ariz", "northern ariz" = "northern ariz",
      "prairie view a&m" = "prairie view", "prairie view" = "prairie view",
      "georgia southern" = "ga southern", "ga southern" = "ga southern",
      "southern california" = "usc", "usc" = "usc",
      "nicholls st" = "nicholls", "nicholls" = "nicholls",
      "purdue fort wayne" = "purdue fort wayne", "fort wayne" = "purdue fort wayne",
      "siu edwardsville" = "siue", "siue" = "siue",
      "tennessee martin" = "ut martin", "ut martin" = "ut martin",
      "college of charleston" = "col of charleston",
      "eastern illinois" = "eastern ill", "eastern ill" = "eastern ill",
      "southern" = "southern u", "southern u" = "southern u",
      "north carolina central" = "nc central", "nc central" = "nc central",
      "southern indiana" = "southern ind", "southern ind" = "southern ind",
      "lamar" = "lamar university", "lamar university" = "lamar",
      "umkc" = "kansas city", "kansas city" = "kansas city",
      "northwestern st" = "northwestern st", "northwestern state" = "northwestern st",
      "miami oh" = "miami ohio", "miami ohio" = "miami oh",
      "cal baptist" = "california baptist", "california baptist" = "cal baptist"
    )
    
    if (school_lower %in% names(school_map)) {
      return(school_map[school_lower])
    }
    return(school_lower)
  }
  
  get_all_nickname_variations <- function(first_name) {
    nick_map <- list(
      "christopher" = c("chris"), "chris" = c("christopher"),
      "anthony" = c("tony", "ace"), "tony" = c("anthony"), "ace" = c("anthony"),
      "william" = c("will"), "will" = c("william"),
      "robert" = c("rob"), "rob" = c("robert"),
      "michael" = c("mike"), "mike" = c("michael"),
      "daniel" = c("dan"), "dan" = c("daniel"),
      "matthew" = c("matt"), "matt" = c("matthew"),
      "benjamin" = c("ben"), "ben" = c("benjamin"),
      "nicholas" = c("nick"), "nick" = c("nicholas"),
      "alexander" = c("alex", "aj", "zan"), "alex" = c("alexander"), 
      "aj" = c("alexander"), "zan" = c("alexander"),
      "james" = c("jim", "jay"), "jim" = c("james"), "jay" = c("james"),
      "charles" = c("chuck"), "chuck" = c("charles"),
      "antonio" = c("tj"), "tj" = c("antonio"),
      "cameron" = c("cam"), "cam" = c("cameron"),
      "dominic" = c("dj"), "dj" = c("dominic"),
      "davonte" = c("ticket"), "ticket" = c("davonte"),
      "russell" = c("deuce"), "deuce" = c("russell"),
      "donald" = c("don", "don don"), "don" = c("donald"), "don don" = c("donald"),
      "quandre" = c("dre"), "dre" = c("quandre"),
      "eddie" = c("ed"), "ed" = c("eddie"),
      "ethan" = c("riggs"), "riggs" = c("ethan"),
      "jamil" = c("jj"), "jj" = c("jamil"),
      "jacob" = c("jake"), "jake" = c("jacob"),
      "jeffrey" = c("jeff"), "jeff" = c("jeffrey"),
      "jonathan" = c("jon"), "jon" = c("jonathan"),
      "joshua" = c("josh"), "josh" = c("joshua"),
      "patrick" = c("pat", "pd"), "pat" = c("patrick"), "pd" = c("patrick"),
      "quinton" = c("quentin"), "quentin" = c("quinton"),
      "trey" = c("tre"), "tre" = c("trey"),
      "trenton" = c("trent"), "trent" = c("trenton"),
      "tristan" = c("tristen"), "tristen" = c("tristan"),
      "tiger" = c("jordan"), "jordan" = c("tiger"),
      "jaqualon" = c("jq"), "jq" = c("jaqualon"),
      "daren" = c("dj"), "dj" = c("daren", "dominic"),
      "davian" = c("da"), "da" = c("davian"),
      "carl" = c("cj"), "cj" = c("carl"),
      "mitchel" = c("mitch"), "mitch" = c("mitchel", "mitchell"),
      "mitchell" = c("mitch"), 
      "bowyn" = c("bo"), "bo" = c("bowyn"),
      "yousouf" = c("youssouf"), "youssouf" = c("yousouf"),
      "nai" = c("naj"), "naj" = c("nai"),
      "amahrie" = c("ahmarie"), "ahmarie" = c("amahrie"),
      "christian" = c("chrisitan"), "chrisitan" = c("christian"),
      "varrick" = c("varick"), "varick" = c("varrick"),
      "derek" = c("derik"), "derik" = c("derek"),
      "darryl" = c("daryl"), "daryl" = c("darryl"),
      "bryson" = c("bryce"), "bryce" = c("bryson"),
      "nikolas" = c("niko"), "niko" = c("nikolas"),
      "jackson" = c("bb"), "bb" = c("jackson"),
      "jeremiah" = c("bear"), "bear" = c("jeremiah"),
      "dallan" = c("deebo"), "deebo" = c("dallan"),
      "desmond" = c("des"), "des" = c("desmond"),
      "javonte" = c("javonte"), "javontae" = c("javonte"),
      "mikeal" = c("mikeal"), "michael" = c("mikeal"),
      "hasan" = c("hasan"), "hassan" = c("hasan"),
      "wilguens" = c("wilguens"), "jr" = c("wilguens"),
      "timothy" = c("timoty"), "timoty" = c("timothy"),
      "nellyjunior" = c("nelly"), "nelly" = c("nellyjunior"),
      "rodolforufino" = c("rodolfo"), "rodolfo" = c("rodolforufino"),
      "louthm" = c("louth"), "louth" = c("louthm"),
      "quoiren" = c("quorien"), "quorien" = c("quoiren"),
      "ubongabasi" = c("ubong abasi"), "ubong abasi" = c("ubongabasi"),
      "tehshaundre" = c("teshaundre"), "teshaundre" = c("tehshaundre"),
      "demeiko" = c("demeiko"), 
      "juslin" = c("justin"), "justin" = c("juslin"),
      "jack" = c("jack"),
      "dk" = c("dk")
    )
    
    if (first_name %in% names(nick_map)) {
      return(nick_map[[first_name]])
    }
    return(character(0))
  }
  
  compound_name_map <- list(
    "william tavares" = "william tavares de brito",
    "aaron humphrey" = "aaron humphries",
    "ethan soares" = "ethan soares rodriguez",
    "frank anselemibe" = "frank anselem",
    "pavle kuzmaanovic" = "pavle kuzmanovic",
    "pedro lopezsanvicente" = "pedro lopez sanvicente",
    "promise c idiaru" = "promise idiaru",
    "carl daughtery" = "carl daugherty",
    "janko bulajic" = "janko buljic",
    "dirin birhiray" = "dirin bihiray",
    "jack medalie" = "jack medale",
    "jack di donna" = "jack didonna",
    "jack didonna" = "jack didonna",
    "eli djordjecvic" = "eli djordjevic",
    "justin bodo bodo" = "justin bodo",
    "james morrow" = "james marrow",
    "michael pajeaud" = "mike pajeaud",
    "deuce dean" = "russell dean",
    "tj madlock" = "antonio madlock",
    "don don ferguson" = "donald ferguson",
    "nelly junior joseph" = "nelly joseph",
    "nellyjunior joseph" = "nelly joseph",
    "rodolfo bolis" = "rodolfo rufino bolis",
    "riggs abner" = "ethan riggs abner",
    "jaqualon roberts" = "jaqualon jq roberts",
    "pd mccraney" = "patrick mccraney",
    "mitch williams" = "micah williams",
    "mitchell taylor" = "mitch taylor",
    "nai ashleyemory" = "naj ashleyemory",
    "demeiko anderson" = "demeiko anderson",
    "clarence jackson" = "clarence monzy jackson",
    "juslin bodo bodo" = "juslin bodo",
    "jackson bb washington" = "jackson washington",
    "jeremiah bear cherry" = "jeremiah cherry",
    "dallan deebo coleman" = "dallan coleman",
    "jesus carralero martin" = "jesus carralero",
    "mikeal brownjones" = "mikeal brown jones",
    "jefferson de la cruz monegro" = "jefferson monegro",
    "wilguens jr exacte" = "wilguens exacte",
    "derrick michael xzavierro" = "derrick xzavierro",
    "yanic konan niederhauser" = "yanic niederhauser",
    "tunde vahlberg fasasi" = "tunde fasasi",
    "aareyon munirjones" = "aareyon munir jones",
    "teddy washington jr" = "teddy washington",
    "jacolb fredsoncole" = "jacolb cole",
    "taylor bol bowen" = "taylor bowen",
    "day day thomas" = "dayday thomas",
    "hasan abdul hakim" = "hasan hakim",
    "jadin collinsroberts" = "jadin roberts",
    "zamoku welucheume" = "zamoku ume",
    "john mobley jr" = "john mobley",
    "mykol sanchezvega" = "mykol vega",
    "jaborri mcghee" = "jaborri mcghee",
    "troy mckoy jr" = "troy mckoy",
    "zocko littleton jr" = "zocko littleton",
    "phat phat brooks" = "phat brooks",
    "omar migueshibelji" = "omar hibelji",
    "theo pierrejustin" = "theo justin",
    "amsal delalic" = "amsal delalic",
    "andrija bukumirovic" = "andrija bukumirovic",
    "ruben prey" = "ruben prey",
    "mouhamadou cisse" = "mouhamadou cisse",
    "abdou khadre kebe" = "abdou kebe",
    "josue grullon" = "josue grullon",
    "matija zuzic" = "matija zuzic",
    "filip radakovic" = "filip radakovic",
    "abdulai fanta kabba" = "abdulai kabba",
    "duane rogers iii" = "duane rogers"
  )
  
  if (year == 2024) {
    hdi_prep <- hdi_data %>%
      mutate(
        Name_norm = normalize_name(`Full Name`),
        Team_norm = sapply(`2024-2025 School `, normalize_school)
      ) %>%
      select(Name, Team, Name_norm, Team_norm, Rating)
  } else {
    hdi_prep <- hdi_data %>%
      mutate(
        Name_norm = normalize_name(Name),
        Team_norm = sapply(Team, normalize_school)
      ) %>%
      select(Name, Team, Name_norm, Team_norm, Rating)
  }
  
  bt_prep <- bt_data %>%
    mutate(
      Name_norm = normalize_name(Name),
      Team_norm = sapply(Team, normalize_school),
      row_id = row_number()
    )
  
  result <- bt_prep %>%
    left_join(hdi_prep %>% select(Name_norm, Team_norm, Rating),
              by = c("Name_norm", "Team_norm"))
  
  for (idx in which(is.na(result$Rating))) {
    bt_name <- result$Name_norm[idx]
    bt_team <- result$Team_norm[idx]
    
    name_parts <- str_split(bt_name, " ")[[1]]
    if (length(name_parts) >= 2) {
      first_name <- name_parts[1]
      rest_of_name <- paste(name_parts[-1], collapse = " ")
      
      alternates <- get_all_nickname_variations(first_name)
      
      if (length(alternates) > 0) {
        for (alt in alternates) {
          alt_full <- paste(alt, rest_of_name)
          match <- hdi_prep %>%
            filter(Name_norm == alt_full, Team_norm == bt_team) %>%
            slice(1)
          
          if (nrow(match) > 0) {
            result$Rating[idx] <- match$Rating
            break
          }
        }
      }
    }
  }
  
  for (idx in which(is.na(result$Rating))) {
    bt_name <- result$Name_norm[idx]
    bt_team <- result$Team_norm[idx]
    
    if (bt_name %in% names(compound_name_map)) {
      mapped_name <- compound_name_map[[bt_name]]
      
      match <- hdi_prep %>%
        filter(Name_norm == mapped_name, Team_norm == bt_team) %>%
        slice(1)
      
      if (nrow(match) > 0) {
        result$Rating[idx] <- match$Rating
      }
    }
  }
  
  hdi_unique <- hdi_prep %>% group_by(Name_norm) %>% filter(n() == 1) %>%
    ungroup() %>% select(Name_norm, Rating)
  
  for (idx in which(is.na(result$Rating))) {
    match <- hdi_unique %>% filter(Name_norm == result$Name_norm[idx]) %>% slice(1)
    if (nrow(match) > 0) result$Rating[idx] <- match$Rating
  }
  
  result <- result %>% select(-Name_norm, -Team_norm, -row_id) %>%
    rename(HDI_Rating = Rating)
  
  matched <- sum(!is.na(result$HDI_Rating))
  unmatched_count <- nrow(bt_data) - matched
  
  return(result)
}
