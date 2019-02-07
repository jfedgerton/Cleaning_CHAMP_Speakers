## Clear working speace
rm(list = ls())
memory.limit(size=10000000000000)

## load packages
library('tidyverse')
library('igraph')

## load data 
Speaker_Data <- read.csv("C:/Users/jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Speaker Data/Speaker_Data.csv")
outstanding_contributors_v2 <- read.csv("C:/Users/jared/Dropbox/CHAMP-Net/Data/Show-Year CSVs/Speaker Data/outstanding_contributors_v2.csv")

## Clean node attributes
temp <- outstanding_contributors_v2 %>%
  distinct(Speaker_Match, .keep_all = T)
temp <- temp[sample(nrow(temp), 100),]

Speaker_Data <- Speaker_Data %>%
  mutate(first.name = trimws(first.name),
         last.name = trimws(last.name),
         first.name = toupper(first.name),
         last.name = toupper(last.name),
         first.name = gsub("'", "", first.name),
         last.name = gsub("'", "", last.name),
         first.name = gsub("\\.", "", first.name),
         last.name = gsub("\\.", "", last.name),
         New_Speaker5 = paste0(last.name, ", ", first.name))

for (i in 1:ncol(Speaker_Data))
{
  Speaker_Data[,i] <- trimws(as.character(Speaker_Data[,i]))
}

for (i in 1:ncol(outstanding_contributors_v2))
{
  outstanding_contributors_v2[,i] <- trimws(as.character(outstanding_contributors_v2[,i]))
}

Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "BUSH, GEORGE"] <- "BUSH, GEORGE W"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "CARLSON, GRETHEN"] <- "CARLSON, GRETCHEN"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "CHENEY, RICHARD"] <- "CHENEY, DICK"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "CLINTO, HILLARY"] <- "CLINTON, HILLARY"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "FRIST, SEN BILL"] <- "FRIST, BILL"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "HELPERIN, MARK"] <- "HALPERIN, MARK"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "HOROWITZ, DANIEL"] <- "HOROWITZ, DAN"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "HOYER, REP STENY"] <- "HOYER, STENY"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "HOYER, STENY"] <- "HOYER, STENY"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "ISSA, REP DARRELL"] <- "ISSA, DARRELL"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "KENDALL (KELLY), MEGYN"] <- "KENDALL, MEGYN"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "KENDALL, MEGYN"] <- "KENDALL, MEGYN"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "KENNEDY, EDWARD"] <- "KENNEDY, TED"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "KING, REP STEVE"] <- "KING, STEVE"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "KRISTOL, WILLIAM"] <- "KRISTOL, BILL"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "KUCINICH, REP DENNIS"] <- "KUCINICH, DENNIS"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "NORRIS, MICHELE"] <- "NORRIS, MICHELLE"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "OBAMA, SEN BARACK"] <- "OBAMA, BARACK"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "SENAY, DR EMILY"] <- "SENAY, EMILY"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "CLINTON, BILL"] <- "CLINTON, BILL"
Speaker_Data$New_Speaker5[Speaker_Data$New_Speaker5 == "CLINTON, WILLIAM J"] <- "CLINTON, BILL"
Speaker_Data$Speaker_Match <- Speaker_Data$New_Speaker5
sum(is.na(Speaker_Data$Speaker_Match))

fixed_contributors <- outstanding_contributors_v2 %>%
  mutate(Speaker_Match = as.character(Speaker_Match),
         New_Speaker5 = as.character(New_Speaker5)) %>%
  bind_rows(Speaker_Data) %>%
  arrange(Speaker_Match, gender, party) %>%
  dplyr::select(., New_Speaker5, Speaker_Match, gender, race, orientation, birth.country, party)  %>%
  distinct(New_Speaker5, .keep_all = T) %>%
  filter(!is.na(gender)) %>%
  filter(!is.na(party)) %>%
  filter(!is.na(orientation)) %>%
  filter(!is.na(race))
fixed_contributors$Speaker_Match[fixed_contributors$Speaker_Match == "GIULIANI, MAYOR RUDOLPH"] <- "GIULIANI, RUDOLPH"  
fixed_contributors$Speaker_Match[fixed_contributors$Speaker_Match == "GIULIANI, RUDY"] <- "GIULIANI, RUDOLPH"  

fixed_contributors <- fixed_contributors %>%
  mutate(Speaker_Match = gsub(" PRESIDENT ", " ", Speaker_Match),
         Speaker_Match = gsub(" SEN ", " ", Speaker_Match),
         Speaker_Match = gsub(" SENATOR ", " ", Speaker_Match),
         Speaker_Match = gsub(" REPRESENTATIVE ", " ", Speaker_Match),
         Speaker_Match = gsub(" CONGRESSMAN ", " ", Speaker_Match),
         Speaker_Match = gsub(" CONGRESSWOMAN ", " ", Speaker_Match),
         Speaker_Match = gsub(" SECRETARY ", " ", Speaker_Match),
         Speaker_Match = gsub(" SEC ", " ", Speaker_Match),
         Speaker_Match = gsub(" GOV ", " ", Speaker_Match),
         Speaker_Match = gsub(" GOVERNOR ", " ", Speaker_Match),
         Speaker_Match = gsub(" SPEAKER ", " ", Speaker_Match),
         Speaker_Match = gsub(" MAYOR ", " ", Speaker_Match))

fixed_contributors$Speaker_Number <- 1:nrow(fixed_contributors)






## Extract all the show files 
all_shows <- list.files(path = "CHAMP-Net/Data/Show-Year CSVs/formatted_data/2000 to 2010/Updated/Text Search", 
                        pattern = ".Rda", 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = FALSE, 
                        no.. = FALSE)
all_shows <- all_shows[!(all_shows %in% "Text_Search.Rda")]

## Merge in cleaned speaker data
output <- list()
for (i in 1:length(all_shows))
{
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/formatted_data/Network Data/Final Clean/", all_shows[i]))
  temp$New_Speaker5[temp$New_Speaker5 == "M, CYNTHIA"] <- "MCFADDEN, CYNTHIA"
  temp$New_Speaker5 <- trimws(as.character(temp$New_Speaker5))
  
  temp <- merge(temp, 
                fixed_contributors,
                by = "New_Speaker5")
  if (nrow(temp) > 0)
  {
    ## Additional cleans
    temp$Speaker_Match[temp$New_Speaker5 %in% c("ACHUTHAN, LAKSHAM", "ACHUTHAN, LACHMAN", 
                                                "ACHUTHAN, LACHMAN", "ACHUTHAN, LACKSHMAN",
                                                "ACHUTHAN, LACKSHMAN", "ACHUTHAN, LAKSHAM", 
                                                "ACHUTHAN, LAKSHAM", "ACHUTHAN, LAKSHAMAN", 
                                                "ACHUTHAN, LAKSHAMAN", "ACHUTHAN, LAKSHUMAN", 
                                                "ACHUTHAN, LAKSHUMAN")] <- "ACHUTHAN, LAKSHMAN" 
    temp$Speaker_Match[temp$New_Speaker5 %in% c("ACKER, TONYA")] <- "ACKER, TANYA" 
    temp$Speaker_Match[temp$New_Speaker5 %in% c("ADELMAN, KEN")] <- "ADELMAN, KENNETH" 
    temp$Speaker_Match[temp$New_Speaker5 %in% c("ADELMAN, KEN")] <- "ADELMAN, KENNETH" 
    temp$Speaker_Match[temp$New_Speaker5 == "AIKEN, JOHNATHAN"] <- "AIKEN, JONATHAN"
    temp$Speaker_Match[temp$New_Speaker5 == "MANPOUR, CHRISTIANA"] <- "AMANPOUR, CHRISTIANE"
    temp$Speaker_Match[temp$New_Speaker5 == "AMANPOUR, CHRISTAINE"] <- "AMANPOUR, CHRISTIANE"
    temp$Speaker_Match[temp$New_Speaker5 == "AMANPOUR, CHIRSTIANE"] <- "AMANPOUR, CHRISTIANE"
    temp$Speaker_Match[temp$New_Speaker5 == "AMANPOUR, CHRSTIANE"] <- "AMANPOUR, CHRISTIANE"
    temp$Speaker_Match[temp$New_Speaker5 == "AMANPOUR, CHRISIANE"] <- "AMANPOUR, CHRISTIANE"
    temp$Speaker_Match[temp$New_Speaker5 == "ANDERSON, PAGE"] <- "ANDERSEN, PAGE"
    temp$Speaker_Match[temp$New_Speaker5 == "ANDERSON, BROOK"] <- "ANDERSON, BROOKE"
    temp$Speaker_Match[temp$New_Speaker5 == "ANDERSON, BROOK,"] <- "ANDERSON, BROOKE"
    temp$Speaker_Match[temp$New_Speaker5 == "ANDERSON, BROOKEE"] <- "ANDERSON, BROOKE"
    temp$Speaker_Match[temp$New_Speaker5 == "ANDERSON, BROOKS"] <- "ANDERSON, BROOKE"
    temp$Speaker_Match[temp$New_Speaker5 == "ANDERSON, PAMELA LEE"] <- "ANDERSON, PAMELA"
    temp$Speaker_Match[temp$New_Speaker5 == "ANNAN, KOFFI"] <- "ANNAN, KOFI"
    temp$Speaker_Match[temp$New_Speaker5 == "ANNAN, KOFIA"] <- "ANNAN, KOFI"
    temp$Speaker_Match[temp$New_Speaker5 == "ARAFAT, YASIR"] <- "ARAFAT, YASSER"
    temp$Speaker_Match[temp$New_Speaker5 == "ARAFAT, YASSIR"] <- "ARAFAT, YASSER"
    temp$Speaker_Match[temp$New_Speaker5 == "ARPAIO, JOE"] <- "ARPAIO, JOESPH"
    temp$Speaker_Match[temp$New_Speaker5 == "ARPAIO, JOSEPH"] <- "ARPAIO, JOESPH"
    temp$Speaker_Match[temp$New_Speaker5 == "ARPAIO, SHERRIF JOSEPH"] <- "ARPAIO, JOESPH"
    temp$Speaker_Match[temp$New_Speaker5 == "ARPAIO, SHERRIFF JOE"] <- "ARPAIO, JOESPH"
    temp$Speaker_Match[temp$New_Speaker5 == "AYALON, DAN"] <- "AYALON, DANIEL"
    temp$Speaker_Match[temp$New_Speaker5 == "BACHMANN, MICHELLE"] <- "BACHMANN, MICHELE"
    temp$Speaker_Match[temp$New_Speaker5 == "BAHKTIAR, RUDI"] <- "BAKHTIAR, RUDI"
    temp$Speaker_Match[temp$New_Speaker5 == "BAKHTAIR, RUDI"] <- "BAKHTIAR, RUDI"
    temp$Speaker_Match[temp$New_Speaker5 == "BAKHTIAR, RUDY"] <- "BAKHTIAR, RUDI"
    temp$Speaker_Match[temp$New_Speaker5 == "BAKHTIAR, RUDY,"] <- "BAKHTIAR, RUDI"
    temp$Speaker_Match[temp$New_Speaker5 == "BAKTHIAR, RUDI"] <- "BAKHTIAR, RUDI"
    temp$Speaker_Match[temp$New_Speaker5 == "BEANUTH, RUDY VON"] <- "BAKHTIAR, RUDI"
    temp$Speaker_Match[temp$New_Speaker5 == "BARMAKIAN, JOE"] <- "BARMAKIAN, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "BARNETT, ERROLL"] <- "BARNETT, ERROL"
    temp$Speaker_Match[temp$New_Speaker5 == "BARRETT, VANESSA"] <- "BARNETT, VANESSA"
    temp$Speaker_Match[temp$New_Speaker5 == "BECKER, ROB"] <- "BECKER, ROBERT"
    temp$Speaker_Match[temp$New_Speaker5 == "BELICH, KATHY"] <- "BELICH, KATHI"
    temp$Speaker_Match[temp$New_Speaker5 == "BERNERO, VERG"] <- "BERNERO, VIRG"
    temp$Speaker_Match[temp$New_Speaker5 == "BERTINELLI, VALARIE"] <- "BERTINELLI, VALERIE"
    temp$Speaker_Match[temp$New_Speaker5 == "BIDEN, JO"] <- "BIDEN, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "BIDEN, JOE"] <- "BIDEN, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "BIDEN, JOESEPH"] <- "BIDEN, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "BIDEN, JOSEPH"] <- "BIDEN, SEN JOE"
    temp$Speaker_Match[temp$New_Speaker5 == "BITTERMAN, JIM"] <- "BITTERMANN, JIM"
    temp$Speaker_Match[temp$New_Speaker5 == "BLACK, CHARLES"] <- "BLACK, CHARLIE"
    temp$Speaker_Match[temp$New_Speaker5 == "BOEHLER, ERIC"] <- "BOEHLERT, ERIC"
    temp$Speaker_Match[temp$New_Speaker5 == "BOETTCHER, MICHAEL"] <- "BOETTCHER, MIKE"
    temp$Speaker_Match[temp$New_Speaker5 == "BOHANNAN, JIM"] <- "BOHANNON, JIM"
    temp$Speaker_Match[temp$New_Speaker5 == "BONADUCE, DAN"] <- "BONADUCE, DANNY"
    temp$Speaker_Match[temp$New_Speaker5 == "BOTEACH, SCHMULEY"] <- "BOTEACH, SHMULEY"
    temp$Speaker_Match[temp$New_Speaker5 == "BOYLE, GREG"] <- "BOYLE, FATHER GREG"
    temp$Speaker_Match[temp$New_Speaker5 == "BOYLE, GREGORY"] <- "BOYLE, FATHER GREG"
    temp$Speaker_Match[temp$New_Speaker5 == "BRAHIMI, RYME"] <- "BRAHIMI, RYM"
    temp$Speaker_Match[temp$New_Speaker5 == "BREMNER, ANN"] <- "BREMNER, ANNE"
    temp$Speaker_Match[temp$New_Speaker5 == "GIULIANI, RUDOLPH"] <- "GIULIANI, RUDY"
    temp$Speaker_Match[temp$New_Speaker5 == "BRINKLEY, DOUG"] <- "BRINKLEY, DOUGLAS"
    temp$Speaker_Match[temp$New_Speaker5 == "BRITTO, MARVETTE"] <- "BRITTO, MARVET"
    temp$Speaker_Match[temp$New_Speaker5 == "BROWN, HILLARY"] <- "BROWN, HILARY"
    temp$Speaker_Match[temp$New_Speaker5 == "BROWN, PAM"] <- "BROWN, PAMELA"
    temp$Speaker_Match[temp$New_Speaker5 == "BROWN, SHERROD"] <- "BROWN, SEN SHERROD"
    temp$Speaker_Match[temp$New_Speaker5 == "BROWN, TERELL"] <- "BROWN, TERRELL"
    temp$Speaker_Match[temp$New_Speaker5 == "BROWN, WILLY"] <- "BROWN, WILLIE"
    temp$Speaker_Match[temp$New_Speaker5 == "BROWNE, PATTI ANNE"] <- "BROWNE, PATTI ANN"
    temp$Speaker_Match[temp$New_Speaker5 == "BRZEZINKSI, MIKA"] <- "BRZEZINSKI, MIKA"
    temp$Speaker_Match[temp$New_Speaker5 == "BRZEZINSKI, MIZA"] <- "BRZEZINSKI, MIKA"
    temp$Speaker_Match[temp$New_Speaker5 == "BRZEZINSKY, MIKA"] <- "BRZEZINSKI, MIKA"
    temp$Speaker_Match[temp$New_Speaker5 == "BRZEZIINSKI, MIKA"] <- "BRZEZINSKI, MIKA"
    temp$Speaker_Match[temp$New_Speaker5 == "BRZEZENSKI, ZBIGNIEW"] <- "BRZEZINSKI, ZBIGNIEW"
    temp$Speaker_Match[temp$New_Speaker5 == "BRZEZINSKY, ZBIGNIEW"] <- "BRZEZINSKI, ZBIGNIEW"
    temp$Speaker_Match[temp$New_Speaker5 == "BRZEZINKSI, ZBIGNIEW"] <- "BRZEZINSKI, ZBIGNIEW"
    temp$Speaker_Match[temp$New_Speaker5 == "BURNS, NICK"] <- "BURNS, NICHOLAS"
    temp$Speaker_Match[temp$New_Speaker5 == "CALIFANO, JOE"] <- "CALIFANO, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "CAMEROTA, ALISON"] <- "CAMEROTA, ALISYN"
    temp$Speaker_Match[temp$New_Speaker5 == "CASAREZ, JEANNE"] <- "CASAREZ, JEAN"
    temp$Speaker_Match[temp$New_Speaker5 == "CHALABI, AHMA"] <- "CHALABI, AHMED"
    temp$Speaker_Match[temp$New_Speaker5 == "CHANCE, MATHEW"] <- "CHANCE, MATTHEW"
    temp$Speaker_Match[temp$New_Speaker5 == "CHANDRASAKARAN, RAJIV"] <- "CHANDRASEKARAN, RAJIV"
    temp$Speaker_Match[temp$New_Speaker5 == "CHENEY, LYNN"] <- "CHENEY, LYNNE"
    temp$Speaker_Match[temp$New_Speaker5 == "CHERNOFF, ALAN"] <- "CHERNOFF, ALLAN"
    temp$Speaker_Match[temp$New_Speaker5 == "CHETRY, KIERAN"] <- "CHETRY, KIRAN"
    temp$Speaker_Match[temp$New_Speaker5 == "CIRINCIONE, JOE"] <- "CIRINCIONE, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "CLINTON, BILL"] <- "CLINTON, BILL"
    temp$Speaker_Match[temp$New_Speaker5 == "CLINTON, WILLIAM J"] <- "CLINTON, BILL"
    temp$Speaker_Match[temp$New_Speaker5 == "COCHRAN, JOHN"] <- "COCHRAN, JOHNNIE"
    temp$Speaker_Match[temp$New_Speaker5 == "DAHLGREN, KRISTEN"] <- "DAHLGREEN, KRISTEN"
    temp$Speaker_Match[temp$New_Speaker5 == "DELACRUZ, VERONICA"] <- "DE LA CRUZ, VERONICA"
    temp$Speaker_Match[temp$New_Speaker5 == "CONTE, NATALI DEL"] <- "DEL CONTE, NATALI"
    temp$Speaker_Match[temp$New_Speaker5 == "CONTE, NATALIE DEL"] <- "DEL CONTE, NATALI"
    temp$Speaker_Match[temp$New_Speaker5 == "ENGEL, RICHARD"] <- "ENGAL, RICHARD"
    temp$Speaker_Match[temp$New_Speaker5 == "FEINBERG, KEN"] <- "FEINBERG, KENNETH"
    temp$Speaker_Match[temp$New_Speaker5 == "FIEGER, GEOFFREY"] <- "FIEGER, GEOGGREY"
    temp$Speaker_Match[temp$New_Speaker5 == "FIEGER, GEOFF"] <- "FIEGER, GEOGGREY"
    temp$Speaker_Match[temp$New_Speaker5 == "FLAY, BOB"] <- "FLAY, BOBBY"
    temp$Speaker_Match[temp$New_Speaker5 == "FOX, VINCENTE"] <- "FOX, VICENTE"
    temp$Speaker_Match[temp$New_Speaker5 == "FRATANGELO, DAWN"] <- "FRANTANGELO, DAWN"
    temp$Speaker_Match[temp$New_Speaker5 == "GHOSH, BOB"] <- "GHOSH, BOBBY"
    temp$Speaker_Match[temp$New_Speaker5 == "SANT, VAN"] <- "VAN SANT, PETER"
    temp$Speaker_Match[temp$New_Speaker5 == "GIULIANI, RUDOLPH"] <- "GIULIANI, RUDY"
    temp$Speaker_Match[temp$New_Speaker5 == "GIULIANI, RUDI"] <- "GIULIANI, RUDY"
    temp$Speaker_Match[temp$New_Speaker5 == "GIULIANI, MAYOR RUDY"] <- "GIULIANI, RUDY"
    temp$Speaker_Match[temp$New_Speaker5 == "GROSS, KEN"] <- "GROSS, KENNETH"
    temp$Speaker_Match[temp$New_Speaker5 == "GIULIANI, RUDY"] <- "GUILIANI, RUDOLPH"
    temp$Speaker_Match[temp$New_Speaker5 == "GIULIANI, MAYOR RUDOLPH"] <- "GUILIANI, RUDOLPH"
    temp$Speaker_Match[temp$New_Speaker5 == "HAGEL, CHUCK"] <- "HAGEL, SEN CHUCK"
    temp$Speaker_Match[temp$New_Speaker5 == "HASTER, DENNIS"] <- "HASTERT, REP DENNIS"
    temp$Speaker_Match[temp$New_Speaker5 == "HASTERT, DENNIS"] <- "HASTERT, REP DENNIS"
    temp$Speaker_Match[temp$New_Speaker5 == "HAZIM, SANTIAGO"] <- "HAZIM, SANTAIGO"
    temp$Speaker_Match[temp$New_Speaker5 == "HYMOWITZ, GREG"] <- "HYMOWITZ, GREGG"
    temp$Speaker_Match[temp$New_Speaker5 == "IMUS, DEIRDRE"] <- "IMUS, DEIDRE"
    temp$Speaker_Match[temp$New_Speaker5 == "JACKSON, JESSE"] <- "JACKSON SR, JESSE"
    temp$Speaker_Match[temp$New_Speaker5 == "JARRETT, GREG"] <- "JARRETT, GREGG"
    temp$Speaker_Match[temp$New_Speaker5 == "JINDAL, BOB"] <- "JINDAL, BOBBY"
    temp$Speaker_Match[temp$New_Speaker5 == "OHNSON, TIM"] <- "JOHNSON, DR TIM"
    temp$Speaker_Match[temp$New_Speaker5 == "JOHNSON, TIMOTHY"] <- "OHNSON, TIM"
    temp$Speaker_Match[temp$New_Speaker5 == "JOHNSTON, DAVE CAY"] <- "JOHNSTON, DAVID CAY"
    temp$Speaker_Match[temp$New_Speaker5 == "KENNEDY, JOHN F"] <- "KENNEDY, JOHN FITZGERALD"
    temp$Speaker_Match[temp$New_Speaker5 == "JEUNESSE, LA"] <- "LA JEUNESSE, WILLIAM"
    temp$Speaker_Match[temp$New_Speaker5 == "JEUNESSE, WILLIAM LA"] <- "LA JEUNESSE, WILLIAM"
    temp$Speaker_Match[temp$New_Speaker5 == "LAJEUNESSE, WILLIAM"] <- "LA JEUNESSE, WILLIAM"
    temp$Speaker_Match[temp$New_Speaker5 == "LEJEUNESSE, WILLIAMS"] <- "LA JEUNESSE, WILLIAM"
    temp$Speaker_Match[temp$New_Speaker5 == "LIDDY, GGORDON"] <- "LIDDY, G GORDON"
    temp$Speaker_Match[temp$New_Speaker5 == "LIEBERMAN, JOE"] <- "LIEBERMAN, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "MCCANN, JKELLY"] <- "MCCANN, J KELLY"
    temp$Speaker_Match[temp$New_Speaker5 == "NEJAME, MARK"] <- "NEJAME, MARC"
    temp$Speaker_Match[temp$New_Speaker5 == "NOTTINGHAM, DANLE"] <- "NOTTINGHAM, DANIELLE"
    temp$Speaker_Match[temp$New_Speaker5 == "PARDHAM, GEORGE"] <- "PARNAHM, GEORGE"
    temp$Speaker_Match[temp$New_Speaker5 == "PARNHAM, GEORGE"] <- "PARNAHM, GEORGE"
    temp$Speaker_Match[temp$New_Speaker5 == "PAUL, CHRISTI"] <- "PAUL, CHRISTIE"
    temp$Speaker_Match[temp$New_Speaker5 == "POLLACK, KEN"] <- "POLLACK, KENNETH"
    temp$Speaker_Match[temp$New_Speaker5 == "RAHMAN, HASSAN ABDEL"] <- "RAHMAN, HASSAN"
    temp$Speaker_Match[temp$New_Speaker5 == "ROSEN, HILLARY"] <- "ROSEN, HILARY"
    temp$Speaker_Match[temp$New_Speaker5 == "SALAZAR, STEVE"] <- "SALAZAR, STEVEN"
    temp$Speaker_Match[temp$New_Speaker5 == "SEIBERG, DAN"] <- "SEIBERG, DANIEL"
    temp$Speaker_Match[temp$New_Speaker5 == "SHRUM, ROBERT"] <- "SHRUM, ROB"
    temp$Speaker_Match[temp$New_Speaker5 == "SIEBERG, DAN"] <- "SIEBERG, DANIEL"
    temp$Speaker_Match[temp$New_Speaker5 == "HOLLEN, VAN"] <- "VAN HOLLEN, CHRIS"
    temp$Speaker_Match[temp$New_Speaker5 == "HOLLEN, VAN"] <- "HOLLEN, CHRIS VAN"
    temp$Speaker_Match[temp$New_Speaker5 == "SANT, VAN"] <- "VAN SANT, PETER"
    temp$Speaker_Match[temp$New_Speaker5 == "SANT, PETER VAN"] <- "VAN SANT, PETER"
    temp$Speaker_Match[temp$New_Speaker5 == "ZANDT, CLINT VAN"] <- "VAN ZANDT, CLINT"
    temp$Speaker_Match[temp$New_Speaker5 == "HEUVEL, KATRINA VANDEN"] <- "VANDEN HEUVEL, KATRINA"
    temp$Speaker_Match[temp$New_Speaker5 == "HEUVEL, VANDEN"] <- "VANDEN HEUVEL, KATRINA"
    temp$Speaker_Match[temp$New_Speaker5 == "ANCKEN, ERICK VON"] <- "VON ANCKEN, ERICK"
    temp$Speaker_Match[temp$New_Speaker5 == "WASSERMAN, DEBBIE"] <- "WASSERMAN SCHULTZ, DEBBIE"
    temp$Speaker_Match[temp$New_Speaker5 == "WASSERMAN, USDEBBIE"] <- "WASSERMAN SCHULTZ, DEBBIE"
    temp$Speaker_Match[temp$New_Speaker5 == "WILSON, JOE"] <- "WILSON, JOSEPH"
    temp$Speaker_Match[temp$New_Speaker5 == "CHENEY, RICHARD B"] <- "CHENEY, DICK"
    temp$Speaker_Match[temp$New_Speaker5 == "HALPERIN, MARK"] <- "HALPERIN, MARK"
    temp$Speaker_Match[temp$New_Speaker5 == "KENDALL, MYGAN"] <- "KENDALL, MEGYN"
    temp$Speaker_Match[temp$New_Speaker5 == "OBAMA, BARACK,"] <- "OBAMA, BARACK"
    temp$Speaker_Match[temp$New_Speaker5 == "BECK, GLEN"] <- "BECK, GLENN"
    temp$Speaker_Match[temp$New_Speaker5 == "KENNEDY, ED"] <- "KENNEDY, TED"
    subset_data <- temp %>%
      dplyr::select(Speaker_Match, gender, race, orientation, birth.country, party, year, fulldate, Text, voice, television, audio, announcer, Text_Extract) 
    subset_data$Show_Name <- substr(all_shows[i], 1, nchar(all_shows[i]) - 9)
    output[[i]] <- subset_data 
  }
}

## create a data frame of all dates
all_dates_check <- list()

for (i in 1:length(output))
{
  if (!is.null(output[[i]]))
  {
    all_dates_check[[i]] <- output[[i]] %>%
      distinct(fulldate) 
  }
}

check_all_dates <- do.call(rbind, all_dates_check)

check_all_dates <- check_all_dates %>%
  distinct(fulldate) %>%
  arrange(fulldate)

## Merge in all data
all_data <- do.call(rbind, output)

## subset the data by dates
subset_dates_list <- list()

for (i in 1:length(check_all_dates$fulldate))
{
  if (i %in% c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000))
  {
    print(paste0("The process is ", round(i/nrow(check_all_dates), 2), " complete."))
  }
  
  temp_date <- all_data %>%
    filter(fulldate == check_all_dates$fulldate[i]) %>%
    dplyr::select(Speaker_Match, fulldate, Show_Name) %>%
    distinct(Speaker_Match, fulldate, Show_Name)  
     
  
  subset_dates_list[[i]] <- temp_date
}


## create node data
node_att <- fixed_contributors %>%
  dplyr::select(Speaker_Match, gender, race, orientation, party) %>%
  distinct(Speaker_Match, .keep_all = T)

## impute missing node data with mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

node_att$gender[is.na(node_att$gender)]           <- getmode(node_att$gender)
node_att$race[is.na(node_att$race)]               <- getmode(node_att$race)
node_att$orientation[is.na(node_att$orientation)] <- getmode(node_att$orientation)
node_att$party[is.na(node_att$party)]             <- getmode(node_att$party)

## Create subset of data for node attributes
men <- node_att %>%
  filter(gender == 2)
lgbtq <- node_att %>%
  filter(orientation %in% c(1, 3))
black <- node_att %>%
  filter(race == 4)
arab <- node_att %>%
  filter(race == 2)
latino <- node_att %>%
  filter(race == 5)
other <- node_att %>%
  filter(race %in% c(1, 3, 6, 9))
republican <- node_att %>%
  filter(party  == 2)
democrat <- node_att %>%
  filter(party == 1)
independent <- node_att %>%
  filter(party == 3)
thirdparty <- node_att %>%
  filter(party == 4)


## Construct daily networks with loop
daily_networks <- list()  
for (i in 300:319)
{
  
  if (i %in% seq(from = 200, to = 4000, by = 475))
  {
    print(paste0("The process is ", round(i/nrow(check_all_dates), 2), " complete."))
  }
  
  ## create empty matrix
  all_speaker_mat <- matrix(0, nrow = length(unique(all_data$Speaker_Match)), ncol = length(unique(all_data$Speaker_Match)))
  colnames(all_speaker_mat) <- rownames(all_speaker_mat) <- sort(unique(all_data$Speaker_Match))
  date_subset <- subset_dates_list[[i]]
  
  ## loop through shows for coappearances
    for (z in 1:length(unique(date_subset$Show_Name)))
    {
    date_show_subset <- date_subset %>%
      filter(Show_Name == unique(date_subset$Show_Name)[z]) %>%
      distinct(Speaker_Match)
    ties <- expand.grid(date_show_subset$Speaker_Match, date_show_subset$Speaker_Match)
    ties <- ties %>%
      filter(ties$Var1 != ties$Var2) %>%
      mutate(Var1 = as.character(Var1),
             Var2 = as.character(Var2)) %>%
      arrange(Var1)
    all_speaker_mat[row.names(all_speaker_mat) %in% unique(ties$Var1), colnames(all_speaker_mat) %in% unique(ties$Var2)] <-  all_speaker_mat[row.names(all_speaker_mat) %in% ties$Var1, colnames(all_speaker_mat) %in% ties$Var2] + 1
    diag(all_speaker_mat) <- 0
    }
  
  ## Create network object
 # daily_networks[[i]] <- all_speaker_mat
  network_output <- igraph::graph_from_adjacency_matrix(all_speaker_mat, mode = "undirected", weighted = T)
  V(network_output)$gender <- "Female"
  V(network_output)$gender[V(network_output)$name %in% men$Speaker_Match] <- "Male"
  V(network_output)$LGBTQ <- "No"
  V(network_output)$LGBTQ[V(network_output)$name %in% lgbtq$Speaker_Match] <- "Yes"
  V(network_output)$race <- "White"
  V(network_output)$race[V(network_output)$name %in% black$Speaker_Match] <- "Black"
  V(network_output)$race[V(network_output)$name %in% latino$Speaker_Match] <- "Lanitx"
  V(network_output)$race[V(network_output)$name %in% arab$Speaker_Match] <- "Arab"
  V(network_output)$race[V(network_output)$name %in% other$Speaker_Match] <- "Other"
  V(network_output)$party <- "Unaffiliated"
  V(network_output)$party[V(network_output)$name %in% republican$Speaker_Match] <- "Republican"
  V(network_output)$party[V(network_output)$name %in% democrat$Speaker_Match] <- "Democrat"
  V(network_output)$party[V(network_output)$name %in% independent$Speaker_Match] <- "Independent"
  V(network_output)$party[V(network_output)$name %in% thirdparty$Speaker_Match] <- "Third Party"
  save(all_speaker_mat, file = paste0("CHAMP-Net/Data/Show-Year CSVs/Edgelist/Daily/", "Adjacency_Mat_", subset_dates_list[[i]]$fulldate[1], ".Rda"))
  save(network_output, file = paste0("CHAMP-Net/Data/Show-Year CSVs/Edgelist/Daily/", "Network_", subset_dates_list[[i]]$fulldate[1], ".RData"))
}  
save(all_data, file = "CHAMP-Net/Data/Show-Year CSVs/Edgelist/all_data.Rda")

plot(network_output)
library('RColorBrewer')
plot(delete.vertices(simplify(network_output), degree(network_output)==0), edge.arrow.size=.3, edge.color="orange",
     vertex.color="gray50", vertex.frame.color="gray50",
     vertex.label = NA, edge.curved=.4, vertex.size = 4)

between_check <- betweenness(network_output)


all_networks <- list.files(path = "CHAMP-Net/Data/Show-Year CSVs/Edgelist/Daily/", 
                        pattern = ".RData", 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = FALSE, 
                        no.. = FALSE)

for (i in 1:length(all_networks))
{
  load(paste0("CHAMP-Net/Data/Show-Year CSVs/Edgelist/Daily/", all_networks[i]))
  V(network_output)$party <- "Unaffiliated"
  V(network_output)$
}