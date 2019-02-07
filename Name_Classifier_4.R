#####################################
####    Fix Remaining Speakers   ####
#####################################

clean_names <- function(input){
  require('tidyverse')
  input$New_Speaker2 <- input$New_Speaker
  input$New_Speaker2[input$New_Speaker2 == "BENTSEN, SEN."] <- "BENSON, LLOYD"
  input$New_Speaker2[input$New_Speaker2 == "BENTSEN, SEN. LLOYD"] <- "BENSON, LLOYD"
  input$New_Speaker2[input$New_Speaker2 == "BENTSEN, SEN. LLOYD"] <- "BENSON, LLOYD"
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = '"', replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = ' DR\\.', replacement = "")
  input$New_Speaker2[input$New_Speaker2 == "ALLEN, SEN."] <- "ALLEN, GEORGE"
  input$New_Speaker2[input$New_Speaker2 == "AYOTTE, SEN."] <- "AYOTTE, KELLY"
  input$New_Speaker2[input$New_Speaker2 == "AVOTTE, SEN. KELLY"] <- "AYOTTE, KELLY"
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = ' DOCTOR', replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = "'", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " GOV\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " GOVERNOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " SEN\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " SENATOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " STATE", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " MAYOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " MAY\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REV\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REVEREND", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " PRES\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " PRESIDENT", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " SGT\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " SERGEANT", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " LT\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " LIEUTENANT", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " GEN\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " GENERAL", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " DR\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " DOCTOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " MAJ\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " MAJOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ATTY", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ATTORNEY", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ADM\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ADMIRAL", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ADM\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ADMIRAL", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " STATE ", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REP\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REPRESENTATIVE", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " VOICE", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " CAPT\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " CAPTAIN", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REAR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " FRMR\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " FORMER", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " CMDR.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " COMMANDER", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REAR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " BRIG\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " BRIGADE", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " DEP\\. CHIEFCOL\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " COL\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " COLONEL", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = ",REP\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " RETIRED", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " RET\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " PROFESSOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " PROF\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = "ABERCROMBE, NEIL", replacement = "ABERCROMBIE, NEIL")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " AMBASSADOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " FMR\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " AMB\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " DETECTIVE", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " DET\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ASST.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ASSISTANT", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = "\\. ", replacement = "\\.")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = "\\.", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REPUBLICAN", replacement = "\\.")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " DEMOCRAT", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " JUDGE", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " FATHER", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ASSOCJUSTICE", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " ARMY CORPORAL", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " CARDINAL", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " PAKISTANI PRESIDENT", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " SHERIFF", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " PASTOR", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " CHIEF", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " CHEF", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " OFFICER", replacement = "")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " OF ", replacement = " ")
  input$New_Speaker2 <- str_replace_all(string = input$New_Speaker2, pattern = " REP ", replacement = " ")
  
  input$New_Speaker2[input$New_Speaker2 == "LOUISE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "OBAMA,"] <- "OBAMA, BARACK"
  input$New_Speaker2[input$New_Speaker2 == "BENNET, SEN."] <- "BENNET, MICHAEL"
  input$New_Speaker2[input$New_Speaker2 == "APOLLO,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CATWOMAN,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DETECTIVE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CHRIS,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "STEVE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "EAGLE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == ",  "] <- NA
  input$New_Speaker2[input$New_Speaker2 == ","] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TEACHER,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "STANLEY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "KENNY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "M,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "S,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "I,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "NOMAD,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "JODY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "LESLIE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "URSULA,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DONNA,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TOMBSTONE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TO,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "N, STEVE DUNLEAVY"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "VOICE, COMPUTER"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "NORM,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "NORM,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "T, JOHN"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "HARRY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "HANK,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ARTIE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "FRED,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CREOLE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ED,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MARGARET,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DEEANN,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "GENIA,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DANIEL,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "RUBY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MARY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DAVID,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == ", CLIP FROM"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "FRIEDMAN, THOMAS L"] <- "FRIEDMAN, THOMAS"
  input$New_Speaker2[input$New_Speaker2 == "PELLEY, SCOT"] <- "PELLEY, SCOTT"
  input$New_Speaker2[input$New_Speaker2 == "PELLEY,"] <- "PELLEY, SCOTT"
  input$New_Speaker2[input$New_Speaker2 == "PELLEY, SCOTTY"] <- "PELLEY, SCOTT"
  input$New_Speaker2[input$New_Speaker2 == "ANCHOR, SCOTT PELLEY CBS NEWS"] <- "PELLEY, SCOTT"
  input$New_Speaker2[input$New_Speaker2 == "CBS, SCOTT PELLEY"] <- "PELLEY, SCOTT"
  input$New_Speaker2[input$New_Speaker2 == "PELLEY,"] <- "PELLEY, SCOTT"
  input$New_Speaker2[input$New_Speaker2 == ","] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DONNELL, NORAH"] <- "ODONNELL, NORAH"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, GOVERNOR BOB"] <- "MCDONNELL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, GOV BOB"] <- "MCDONNELL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, PAUL ODWYER BOB"] <- "MCDONNELL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, VOICE OF GOVERNOR BOB"] <- "MCDONNELL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, FORMER GOVERNOR BOB"] <- "MCDONNELL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, FORMER GOV BOB"] <- "MCDONNELL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, VOICE OF MATTHEW"] <- "MCDONNELL,MATTHEW"
  input$New_Speaker2[input$New_Speaker2 == "ODONNELL, BROTHER PAUL"] <- "ODONNELL, PAUL"
  input$New_Speaker2[input$New_Speaker2 == "BLITZER,"] <- "BLITZER, WOLF"
  input$New_Speaker2[input$New_Speaker2 == "MCDONNELL, VIRGINIA GOVERNOR BOB"] <- "MCDONNELL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "ODONNELL, COMMANDER EDWARD"] <- "ODONNELL, ED"
  input$New_Speaker2[input$New_Speaker2 == "ODONNELL, EDWARD"] <- "ODONNELL, ED"
  input$New_Speaker2[input$New_Speaker2 == "KING,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "SCHACHER,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "SCHACHER, SAMATHA"] <- "SCHACHER, SAMANTHA"
  input$New_Speaker2[input$New_Speaker2 == "SCHACHER, SAMANTA"] <- "SCHACHER, SAMANTHA"
  input$New_Speaker2[input$New_Speaker2 == "RICO,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "HOGAN, B"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "NSYNC,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "STREISAND, BARBARA"] <- "STREISAND, BARBRA"
  input$New_Speaker2[input$New_Speaker2 == "STREISAND,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "LAVIOLETTE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "EDWARDS, J"] <- "EDWARDS, JOHN"
  input$New_Speaker2[input$New_Speaker2 == "EDWARDS, J"] <- "EDWARDS, JOHN"
  input$New_Speaker2[input$New_Speaker2 == "MESERVE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == ", DR SANJAY"] <- "GUPTA, DR SANJAY"
  input$New_Speaker2[input$New_Speaker2 == "GUPTA, DR SANJAY"] <- "GUPTA, DR SANJAY"
  input$New_Speaker2[input$New_Speaker2 == "BRIDGET,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "(PH), LOUISE DE MIRANDA"] <- NA
  input$New_Speaker2[input$New_Speaker2 == ", SGT"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "SPOKESBOT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "NIQUE, MO"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MALVEAUX,"] <- "MALVEAUX, SUZANNE"
  input$New_Speaker2[input$New_Speaker2 == "(PH), MICHAEL MCCARDEL"] <- "MCCARDEL, MICHAEL"
  input$New_Speaker2[input$New_Speaker2 == "JOHNSON,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == ", MARCO"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CLANCY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == ", HE APPLIED FOR THE GREATEST NUMBER SO"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "A, MOST STUDENTS SAY THEY CANT WAIT UNTIL SPRING BREAK"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "HOLDER,"] <- "HOLDER, TAMARA"
  input$New_Speaker2[input$New_Speaker2 == "ALTMAN,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "GIBSON,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DOBBS,"] <- "DOBBS, LOU"
  input$New_Speaker2[input$New_Speaker2 == "BUCHI, MARIO"] <- "BUCHI, MARIO"
  input$New_Speaker2[input$New_Speaker2 == "KIRSCHHOCH, MARY ANN"] <- "KIRSCHHOCH, MARY ANNE"
  input$New_Speaker2[input$New_Speaker2 == "KIRSCHHOCH, M"] <- "KIRSCHHOCH, MARY ANNE"
  input$New_Speaker2[input$New_Speaker2 == "OREILLY,"] <- "OREILLY, BILL"
  input$New_Speaker2[input$New_Speaker2 == "REILLY,"] <- "OREILLY, BILL"
  input$New_Speaker2[input$New_Speaker2 == "ROVE,"] <- "ROVE, KARL"
  input$New_Speaker2[input$New_Speaker2 == "OREILLY,"] <- "OREILLY, BILL"
  input$New_Speaker2[input$New_Speaker2 == "REILLY, O"] <- "OREILLY, BILL"
  input$New_Speaker2[input$New_Speaker2 == "BARNES,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "PERINO,"] <- "PERINO, DANA"
  input$New_Speaker2[input$New_Speaker2 == "BECKEL,"] <- "BECKEL, BOB"
  input$New_Speaker2[input$New_Speaker2 == "GUTFELD"] <- "GUTFELD, GREG" 
  input$New_Speaker2[input$New_Speaker2 == "GUTFELD,"] <- "GUTFELD, GREG" 
  input$New_Speaker2[input$New_Speaker2 == "GUTFELD, DANA PERINO GREG"] <- "GUTFELD, GREG" 
  input$New_Speaker2[input$New_Speaker2 == "GUTFELD, KIMBERLY GUILFOYLE GREG"] <- "GUTFELD, GREG" 
  input$New_Speaker2[input$New_Speaker2 == "NAPOLITANO,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "BOLLING,"] <- "BOLLING, ERIC"
  input$New_Speaker2[input$New_Speaker2 == "BOLLING"] <- "BOLLING, ERIC"
  input$New_Speaker2[input$New_Speaker2 == "BILA, ERIC BOLLING JEDEDIAH"] <- "BILA, JEDEDIAH"
  input$New_Speaker2[input$New_Speaker2 == "GUILFOYLE,"] <- "GUILFOYLE, KIMBERLY"
  input$New_Speaker2[input$New_Speaker2 == "WILLIAMS,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "KILMEADE,"] <- "KILMEADE, BRIAN"
  input$New_Speaker2[input$New_Speaker2 == "KILMEADEE,"] <- "KILMEADE, BRIAN"
  input$New_Speaker2[input$New_Speaker2 == "KILMEADE,"] <- "KILMEADE, BRIAN"
  input$New_Speaker2[input$New_Speaker2 == "KILMEADE, BRAIN"] <- "KILMEADE, BRIAN"
  input$New_Speaker2[input$New_Speaker2 == "KILMEADEE, BRIAN"] <- "KILMEADE, BRIAN"
  input$New_Speaker2[input$New_Speaker2 == "CROWLEY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DONNELL, LAWRENCE"] <- "ODONNELL, LAWRENCE"
  input$New_Speaker2[input$New_Speaker2 == "(PH), ALEXA SWIT"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TODD,"] <- "TODD, CHUCK"
  input$New_Speaker2[input$New_Speaker2 == "ANNOUNCER,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "GRAPHICS,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CONTENT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MALE, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "FEMALE, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "QUESTION,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "RUSSERT, MR"] <- "RUSSERT, TIM"
  input$New_Speaker2[input$New_Speaker2 == "TEXT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "WOMAN,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "GREGORY, MR"] <- "GREGORY, DAVID"
  input$New_Speaker2[input$New_Speaker2 == "REPORTER, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MAN, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MAN,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ACTOR, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "OBAMA,"] <- "OBAMA, BARRACK"
  input$New_Speaker2[input$New_Speaker2 == "CLIP, VIDEO"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "REPORTER,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "WOMAN, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CLINTON, SEN HILLARY RODHAM"] <- "CLINTON, HILLARY"
  input$New_Speaker2[input$New_Speaker2 == "STATION,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "OPERATOR,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CHILD, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ACTOR,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "GIRL,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "REPORTER, UNIDENTIFIED MALE"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ANDERSON,"] <- "ANDERSON, COOPER"
  input$New_Speaker2[input$New_Speaker2 == "CARVILLE, MR"] <- "CARVILLE, JAMES"
  input$New_Speaker2[input$New_Speaker2 == "MEMBER, AUDIENCE"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "COLLINS,"] <- "COLLINS, SUSAN"
  input$New_Speaker2[input$New_Speaker2 == "CREDITS,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "BIDEN, SEN"] <- "BIDEN, JOE"
  input$New_Speaker2[input$New_Speaker2 == "BUSH, GW"] <- "BUSH, GEORGE W"
  input$New_Speaker2[input$New_Speaker2 == "POWELL,"] <- "POWELL, COLIN"
  input$New_Speaker2[input$New_Speaker2 == "SNOW,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "STORM,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "HILL,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "BOY,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DREW, DR"] <- "DREW, PINSKY"
  input$New_Speaker2[input$New_Speaker2 == "GIRL, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "SANTORUM, SEN"] <- "SANTORUM, RICK"
  input$New_Speaker2[input$New_Speaker2 == "RICE,"] <- "RICE, SUSAN"
  input$New_Speaker2[input$New_Speaker2 == "LAUER,"] <- "LAUER, MATT"
  input$New_Speaker2[input$New_Speaker2 == "BUSH, GHW"] <- "BUSH, GEORGE HW"
  input$New_Speaker2[input$New_Speaker2 == "CARLSON,"] <- "CARLSON, TUCKER"
  input$New_Speaker2[input$New_Speaker2 == "NARRATOR, AD"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ROVE, MR"] <- "ROVE, KARL"
  input$New_Speaker2[input$New_Speaker2 == "US,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "OFFICER,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "COURIC,"] <- "COURIC, KATIE"
  input$New_Speaker2[input$New_Speaker2 == "ACTRESS,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "PILOT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CHILD,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TI"] <- "TI"
  input$New_Speaker2[input$New_Speaker2 == "OBAMA, BARAK"] <- "OBAMA, BARACK"
  input$New_Speaker2[input$New_Speaker2 == "BOEHNER,"] <- "BOEHNER, JOHN"
  input$New_Speaker2[input$New_Speaker2 == "BUSH, PRES"] <- "BUSH, GEORGE W"
  input$New_Speaker2[input$New_Speaker2 == "LEMON,"] <- "LEMON, DON"
  input$New_Speaker2[input$New_Speaker2 == "ANNOUNCER, UNIDENTIFIED MAL"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "STUDENT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "AXELROD,"] <- "AXELROD, DAVID"
  input$New_Speaker2[input$New_Speaker2 == "NADER, MR"] <- "NADER, RALPH"
  input$New_Speaker2[input$New_Speaker2 == "BUSH, GEORGE H W"] <- "BUSH, GEORGE HW"
  input$New_Speaker2[input$New_Speaker2 == "PROTESTERS, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TRUMP,"] <- "TRUMP, DONALD"
  input$New_Speaker2[input$New_Speaker2 == "STING,"] <- "STING"
  input$New_Speaker2[input$New_Speaker2 == "VOICE,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "EDWARDS, SEN"] <- "EDWARDS, JOHN"
  input$New_Speaker2[input$New_Speaker2 == "ANNOUNCER, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "POLIZZI,"] <- "POLIZZI, NICOLE"
  input$New_Speaker2[input$New_Speaker2 == "HUCKABEE,"] <- "HUCKABEE, MIKE"
  input$New_Speaker2[input$New_Speaker2 == "GILLESPIE,"] <- "GILLESPIE, ED"
  input$New_Speaker2[input$New_Speaker2 == "T, MR"] <- "Tureaud, Laurence"
  input$New_Speaker2[input$New_Speaker2 == "CORKER,"] <- "CORKER, BOB"
  input$New_Speaker2[input$New_Speaker2 == "CORRESPONDENT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CORKER, SEN"] <- "CORKER, BOB"
  input$New_Speaker2[input$New_Speaker2 == "ORKER, SEN BOBO"] <- "CORKER, BOB"
  input$New_Speaker2[input$New_Speaker2 == "SHELBY, SEN"] <- "SHELBY, RICHARD"
  input$New_Speaker2[input$New_Speaker2 == "ONSCREEN,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ANCHOR, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "REPORT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MEMBER, UNIDENTIFIED BOARD"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "VELSHI,"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, WHERE IS THE STIMULUS PACKAGE MONEY GOING? ALI"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, AL"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "HARLOW, ALI VELSHI POPPY"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, ALII"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, VOICE OF ALI"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "ANCHOR, ALI VELSHI CNN"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "CARROLL, ALI VELSHI JASON"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "FOREMAN, ALI VELSHI TOM"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, MAX FOSTER ALI"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, NIC ROBERTSON ALI"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "CORRESPONDENT, ALI VELSHI CNN"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, ALL"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHII,"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHI, ALL"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "VELSHII,"] <- "VELSHI, ALI"
  input$New_Speaker2[input$New_Speaker2 == "HAGEL,"] <- "HAGEL, CHUCK"
  input$New_Speaker2[input$New_Speaker2 == "FIREFIGHTER,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "JR, REP HAROLD FORD"] <- "FORD, HAROLD"
  input$New_Speaker2[input$New_Speaker2 == "OPERATOR, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "WORKER, UNIDENTIFIED SOCIAL"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ASSAD,"] <- "ASSAD, BASHAR AL"
  input$New_Speaker2[input$New_Speaker2 == "DIRECTOR,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "DEMONSTRATORS,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "IMAN"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "PRIEBUS, MR"] <- "PRIEBUS, REINCE"
  input$New_Speaker2[input$New_Speaker2 == "SCALIA,"] <- "SCALIA, ANTONIN"
  input$New_Speaker2[input$New_Speaker2 == "MCCAIN, OF JOHN"] <- "MCCAIN, JOHN"
  
  input$New_Speaker2[input$New_Speaker2 == "MALE, UNIDENTIIFED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "HOST, RADIO"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TI,"] <- "TI"
  input$New_Speaker2[input$New_Speaker2 == "WHITNEY, DANIEL LAWRENCE LARRY THE CABLE GUY"] <- "WHITNEY, DANIEL LAWRENCE"
  input$New_Speaker2[input$New_Speaker2 == "BUSH, PRES GEORGE"] <- "BUSH, GEORGE W"
  input$New_Speaker2[input$New_Speaker2 == "CLINTON, B"] <- "CLINTON, BILL"
  input$New_Speaker2[input$New_Speaker2 == "KERREY, SEN"] <- "KERREY, BOB"
  input$New_Speaker2[input$New_Speaker2 == "SEBELIUS, SECY"] <- "SEBELIUS, KATHLEEN"
  input$New_Speaker2[input$New_Speaker2 == "J, LL COOL"] <- "LL COOL J"
  input$New_Speaker2[input$New_Speaker2 == "BEGALA, MR"] <- "BEGALA, PAUL"
  input$New_Speaker2[input$New_Speaker2 == "CITIZEN, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "BUSH, PRESIDENT GEORGE"] <- "BUSH, GEORGE W"
  input$New_Speaker2[input$New_Speaker2 == "LUGAR, SEN"] <- "LUGAR, DICK"
  input$New_Speaker2[input$New_Speaker2 == "YORKER, UNIDENTIFIED NEW"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "MATTHEWS, CHRIS MATTHEWS CHRIS"] <- "MATTHEWS, CHRIS"
  input$New_Speaker2[input$New_Speaker2 == "MYSTERY, SOLVE MY"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "RUBIO,"] <- "RUBIO, MARCO"
  input$New_Speaker2[input$New_Speaker2 == "RUBIO, SEN"] <- "RUBIO, MARCO"
  input$New_Speaker2[input$New_Speaker2 == "PATIENT,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TOURIST, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TOURIST, UNIDENTIFIED AMERICAN"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TOURIST, UNIDENTIFIED FEMALE"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TOURIST, UNIDENTIFIED CANADIAN"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TOURIST, UNIDENTIFIED CHINESE"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "TOURIST, UNIDENTIFIED MALE"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "SPECTER, SEN"] <- "SPECTER, ARLEN"
  input$New_Speaker2[input$New_Speaker2 == "HUTCHISON,"] <- "HUTCHISON, KAY BAILEY"
  input$New_Speaker2[input$New_Speaker2 == "FATHER,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "GATES, SECY"] <- "GATES, ROBERT"
  input$New_Speaker2[input$New_Speaker2 == "JUROR, UNIDENTIFIED"] <- "GATES, ROBERT"
  input$New_Speaker2[input$New_Speaker2 == "OFFICER, UNIDENTIFIED MALE POLICE"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "THERAPIST,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "NADLER, REP"] <- "NADLER, JERROLD"
  input$New_Speaker2[input$New_Speaker2 == "ONEILL, SECY"] <- "ONEILL, PAUL"
  input$New_Speaker2[input$New_Speaker2 == "PHYSICIAN,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "ANNOUNCER, COMMERCIAL"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CORNYN, SEN"] <- "CORNYN, JOHN"
  input$New_Speaker2[input$New_Speaker2 == "CNN, A"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "CHORUS,"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "GEITHNER,"] <- "GEITHNER, TIM"
  input$New_Speaker2[input$New_Speaker2 == "PARTICIPANT, UNIDENTIFIED"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "STEINBERG, STEVE CARELL AND DAVID"] <- "STEINBERG, DAVID"
  input$New_Speaker2[input$New_Speaker2 == ", HIGH THIS CONFLICT IS"] <- NA
  input$New_Speaker2[input$New_Speaker2 == "THUNE, MR"] <- "THUNE, JOHN"
  input$New_Speaker2[input$New_Speaker2 == "LANDRIEU, SEN"] <- "LANDRIEU, MARY"
  input$New_Speaker2[input$New_Speaker2 == "FEINGOLD, SEN"] <- "FEINGOLD, RUSS"
  input$New_Speaker2[input$New_Speaker2 == "SESSIONS,"] <- "SESSIONS, JEFF"
  input$New_Speaker2[input$New_Speaker2 == ", SANJAY"] <- "GUPTA, SANJAY"
  input$New_Speaker2[input$New_Speaker2 == "BLITZER,"] <- "BLITZER, WOLF"
  input$New_Speaker2[input$New_Speaker2 == "OBAMA,"] <- "OBAMA, BARACK"
  
  input$New_Speaker2[input$New_Speaker2 == "OBAMA, MS"] <- "OBAMA, MICHELLE"
  input$New_Speaker2[input$New_Speaker2 == "AUDIENCE, OBAMA AND"] <- "OBAMA, BARACK"
  input$New_Speaker2[input$New_Speaker2 == "BUSH, VICE"] <- "BUSH, GEORGE HW"
  input$New_Speaker2[input$New_Speaker2 == "ANNOUNCER,"] <- NA
  #input$New_Speaker2[input$New_Speaker2 == "ANNOUNCER,"] <- NA
  
  # for (i in 1:nrow(input)){
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("B", "OBAMA"), fixed=TRUE)) >= 2] <- "OBAMA, BARACK" 
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("OBAMA", "B"), fixed=TRUE)) >= 2] <- "OBAMA, BARACK" 
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("M", "OBAMA", "MI"), fixed=TRUE)) >= 2] <- "OBAMA, MICHELLE"  
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("H", "CLINTON"), fixed=TRUE)) >= 2] <- "CLINTON, HILLARY"  
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("B", "CLINTON", "PRE"), fixed=TRUE)) >= 2] <- "CLINTON, BILL"  
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("B", "CLINTON", "CH"), fixed=TRUE)) >= 2] <- "CLINTON, CHELSEA"  
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("BIDEN", "JO"), fixed=TRUE)) >= 2] <- "BIDEN, JOE"  
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("GEORGE", "H", "BUSH"), fixed=TRUE)) >= 3] <- "BUSH, GEORGE HW"
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("GORE", "A"), fixed=TRUE)) >= 2] <- "GORE, AL"
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, "UNIDENT", fixed=TRUE)) == 1] <- NA
  #  input[i,]$New_Speaker2[nchar(input[i,]$New_Speaker2) > 25] <- NA
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("GEORGE", "H", "BUSH"), fixed=TRUE)) >= 3] <- "BUSH, GEORGE HW"
  #  input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("BOEHNER", "JO", "REP"), fixed=TRUE)) >= 2] <- "BUSH, GEORGE HW"
  #  
  #  
  #  #if (input[i,]$New_Speaker2 != "BUSH, GEORGE HW"){
  #   # input[i,]$New_Speaker2[sum(grepl(input[i,]$New_Speaker2, c("GEORGE", "W", "BUSH"), fixed=TRUE)) >= 3] <- "BUSH, GEORGE W"
  #  #}
  #}
  #input$New_Speaker2[input$New_Speaker2 == "BUSH, USDENT GEORGE HERBERT WALKER"] <- "BUSH, GEORGE HW"
  #input$check <- 0
  #input$sub_string <- substr(input$New_Speaker2, nchar(input$New_Speaker2), input$New_Speaker2)
  #input$check[input$sub_string == ","] <- NA
  return(input)
}

