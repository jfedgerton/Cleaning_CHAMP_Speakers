show_fix_function <- function(input){
  input$show[input$show == "ABC"] <- "ABC"
  input$show[input$show == "CNBC/DOW JONES BUSINESS VIDEO INSIDE POLITICS"] <- "INSIDE_POLITICS"
  input$show[input$show == "ABC GOOD MORNING AMERICA"] <- "GMA"
  input$show[input$show == "ABC NEWS"] <- "ABC_NEWS"
  input$show[input$show == "ABC NEWS TRANSCRIPT"] <- "ABC_NEWS"
  input$show[input$show == "ABC NEWS TRANSCRIPTS"] <- "ABC_NEWS"
  input$show[input$show == "ABC NIGHTLINE"] <- "NIGHTLINE"
  input$show[input$show == "ABC NIGHTLINE ("] <- "NIGHTLINE"
  input$show[input$show == "ABC PRIMETIME LIVE"] <- "NIGHTLINE"
  input$show[input$show == "ABC PRIMETIME LIVE ("] <- "NIGHTLINE"
  input$show[input$show == "ABC THIS WEEK"] <- "THIS_WEEK"
  input$show[input$show == "ABC THIS WEEK ("] <- "THIS_WEEK"
  input$show[input$show == "ABC TRANSCRIPTS (AUSTRALIA)"] <- "ABC_AUSTRALIA"
  input$show[input$show == "ABC TURNING POINT ("] <- "TURNING_POINT"
  input$show[input$show == "ABC WORLD NEWS SATURDAY"] <- "WNT_SATURDAY"
  input$show[input$show == "ABC WORLD NEWS SUNDAY"] <- "WNT_SUNDAY"
  input$show[input$show == "ABC WORLD NEWS TONIGHT (SATURDAY)"] <- "WNT_SATURDAY"
  input$show[input$show == "ABC WORLD NEWS TONIGHT (SUNDAY)"] <- "WNT_SUNDAY"
  input$show[input$show == "AC"] <- "ANDERSON_COOPER"
  input$show[input$show == "ALL IN WITH CHRIS HAYES"] <- "ALL_IN_WITH_CHRIS_HAYES"
  input$show[input$show == "ALL THINGS CONSIDERED"] <- "ALL_THINGS_CONSIDERED"
  input$show[input$show == "AMERICAN PUBLIC MEDIA"] <- "APM"
  input$show[input$show == "ANDERSON COOPER"] <- "ANDERSON_COOPER"
  input$show[input$show == "APM MARKETPLACE MONEY"] <- "APMMM"
  input$show[input$show == "AROUND THE WORLD"] <- "ATW"
  input$show[input$show == "BLOOMBERG TV"] <- "BTV"
  input$show[input$show == "BLOOMBERG: TV"] <- "BTV"
  input$show[input$show == "BUSINESS WEEK"] <- "BUSINESS_WEEK"
  input$show[input$show == "BUSINESS WORLD"] <- "BUSINESS_WORLD"
  input$show[input$show == "BUSINESS WORLD (ABC)"] <- "BUSINESS_WORLD"
  input$show[input$show == "CANADA AM"] <- "CANADA_AM"
  input$show[input$show == "CAVUTO BUSINESS REPORT ("] <- "CBR"
  input$show[input$show == "CBS"] <- "CBS"
  input$show[input$show == "CBS EVENING NEWS ("] <- "EVENING_NEWS"
  input$show[input$show == "CBS NEWS SUNDAY MORNING"] <- "SUNDAY_MORNING"
  input$show[input$show == "CBS NEWS TRANSCRIPTS"] <- "NEWS_TRANSCRIPTS"
  input$show[input$show == "CBS SUNDAY MORNING"] <- "SUNDAY_MORNING"
  input$show[input$show == "CBS SUNDAY MORNING ("] <- "SUNDAY_MORNING"
  input$show[input$show == "CBS THIS MORNING"] <- "THIS_MORNING"
  input$show[input$show == "CHANNEL"] <- "Error"
  input$show[input$show == "CLOSING BELL"] <- "CLOSING_BELL"
  input$show[input$show == "CLOSING BELL WITH MARIA BARTIROMO"] <- "CLOSING_BELL"
  input$show[input$show == "CNBC NEWS TRANSCRIPTS"] <- "Error"
  input$show[input$show == "CNN & TIME"] <- "Error"
  input$show[input$show == "CNN AHEAD OF THE CURVE"] <- "AHEAD_OF_THE_CURVE"
  input$show[input$show == "CNN AMERICAN MORNING WITH PAULA ZAHN"] <- "AMERICAN_MORNING_PAULA_ZAHN"
  input$show[input$show == "CNN BUSINESS DAY"] <- "BUSINESS_DAY"
  input$show[input$show == "CNN CNN"] <- "Error"
  input$show[input$show == "CNN EVANS, NOVAK, HUNT & SHIELDS"] <- " EVANS_NOVAK_HUNT_SHIELDS"
  input$show[input$show == "CNN FIRST EVENING NEWS"] <- "FIRST_EVENING_NEWS"
  input$show[input$show == "CNN INTERNATIONAL"] <- "INTERNATIONAL"
  input$show[input$show == "CNN LATE EDITION PRIMETIME WITH WOLF BLITZER"] <- "PRIMETIME_WOLF_BLITZER"
  input$show[input$show == "CNN LATE EDITION WITH WOLF BLITZER"] <- "PRIMETIME_WOLF_BLITZER"
  input$show[input$show == "CNN LIVE EVENT/SPECIAL"] <- "LIVE_EVENT"
  input$show[input$show == "CNN LIVE ON LOCATION"] <- "LIVE_ON_LOCATION"
  input$show[input$show == "CNN LIVE TODAY"] <- "LIVE_TODAY"
  input$show[input$show == "CNN MONEY MORNING"] <- "MONEY_MORNING"
  input$show[input$show == "CNN MONEY WEEK"] <- "MONEY_WEEK"
  input$show[input$show == "CNN MONEYWEEK"] <- "MONEY_WEEK"
  input$show[input$show == "CNN MOVERS WITH JAN HOPKINS"] <- "JAN_HOPKINS"
  input$show[input$show == "CNN NEWS SITE"] <- "NEWS_SITE"
  input$show[input$show == "CNN NEWSNIGHT AARON BROWN"] <- "AARON_BROWN"
  input$show[input$show == "CNN NEWSSTAND"] <- "NEWSSTAND"
  input$show[input$show == "CNN NOVAK, HUNT & SHIELDS"] <- "EVANS_NOVAK_HUNT_SHIELDS"
  input$show[input$show == "CNN PEOPLE IN THE NEWS"] <- "PEOPLE_IN_THE_NEWS"
  input$show[input$show == "CNN SATURDAY EDITION"] <- "SATURDAY_EDITION"
  input$show[input$show == "CNN SATURDAY MORNING"] <- "SATURDAY_MORNING"
  input$show[input$show == "CNN SATURDAY MORNING NEWS"] <- "SATURDAY_MORNING"
  input$show[input$show == "CNN SATURDAY NIGHT"] <- "SATURDAY_NIGHT"
  input$show[input$show == "CNN SCIENCE & TECHNOLOGY WEEK"] <- "SCIENCE_TECHNOLOGY_WEEK"
  input$show[input$show == "CNN SCIENCE AND TECHNOLOGY WEEK"] <- "SCIENCE_TECHNOLOGY_WEEK"
  input$show[input$show == "CNN SHOWBIZ THIS WEEK"] <- "SHOWBIZ_THIS_WEEK"
  input$show[input$show == "CNN SHOWBIZ THIS WEEKEND"] <- "SHOWBIZ_THIS_WEEKEND"
  input$show[input$show == "CNN SHOWBIZ TODAY"] <- "SHOWBIZ_TODAY"
  input$show[input$show == "CNN SPORTING LIFE WITH JIM HUBER"] <- "JIM_HUBER"
  input$show[input$show == "CNN STREET SWEEP"] <- "STREET_SWEEP"
  input$show[input$show == "CNN STUDENT NEWS"] <- "STUDENT_NEWS"
  input$show[input$show == "CNN SUNDAY"] <- "SUNDAY"
  input$show[input$show == "CNN SUNDAY EVENING"] <- "SUNDAY_EVENING"
  input$show[input$show == "CNN SUNDAY MORNING"] <- "SUNDAY_MORNING"
  input$show[input$show == "CNN SUNDAY MORNING NEWS"] <- "SUNDAY_MORNING"
  input$show[input$show == "CNN SUNDAY NIGHT"] <- "SUNDAY_NIGHT"
  input$show[input$show == "CNN TAKE FIVE"] <- "TAKE_FIVE"
  input$show[input$show == "CNN TALKBACK LIVE"] <- "TALKBACK_LIVE"
  input$show[input$show == "CNN THE SPIN ROOM"] <- "SPIN_ROOM"
  input$show[input$show == "CNN THE SPIN ROOM CORRECTED COPY"] <- "SPIN_ROOM_CORRECTED_COPY"
  input$show[input$show == "CNN THE WORLD TODAY"] <- "WORLD_TODAY"
  input$show[input$show == "CNN TODAY"] <- "CNN_TODAY"
  input$show[input$show == "CNN TODAY BREAKING NEWS"] <- "BREAKING_NEWS"
  input$show[input$show == "CNN TODAY SPECIAL EVENT"] <- "SPECIAL_EVENT"
  input$show[input$show == "CNN TONIGHT"] <- "TONIGHT"
  input$show[input$show == "CNN WOLF BLITZER REPORTS"] <- "BLITZER_REPORTS"
  input$show[input$show == "CNN WORLD REPORT"] <- "WORLD_REPORT"
  input$show[input$show == "CNN WORLD TODAY"] <- "WORLD_TODAY"
  input$show[input$show == "CNN WORLDVIEW"] <- "WORLDVIEW"
  input$show[input$show == "CNN YOUR HEALTH"] <- "YOUR_HEALTH"
  input$show[input$show == "CNN YOUR MONEY"] <- "YOUR_MONEY"
  input$show[input$show == "CNN, THE WORLD TODAY"] <- "WORLD_TODAY"
  input$show[input$show == "COUNTDOWN"] <- "COUNTDOWN"
  input$show[input$show == "CTV TELEVISION, INC."] <- "TELEVISION"
  input$show[input$show == "DAY ONE"] <- "DAY_ONE"
  input$show[input$show == "DAY ONE (ABC"] <- "DAY_ONE"
  input$show[input$show == "DAY ONE THIS IS AN UNCORRECTED COPY. NOT A FINAL VERSION."] <- "Error"
  input$show[input$show == "EARLY AM"] <- "EARLY_AM"
  input$show[input$show == "ENCORE PRESENTATION OF CNN LATE EDITION WITH WOLF BLITZER"] <- "ENCORE_CNN_LATE_WOLF_BLITZER"
  input$show[input$show == "EUROPEAN CLOSING BELL"] <- "CLOSING_BELL"
  input$show[input$show == "EYEWTINESS"] <- "EYEWTINESS"
  input$show[input$show == "FARM WEEK"] <- "FARM_WEEK"
  input$show[input$show == "FARMING TODAY THIS WEEK"] <- "FARMING_TODAY_THIS_WEEK"
  input$show[input$show == "FIVE"] <- "FIVE"
  input$show[input$show == "FOX"] <- "FOX"
  input$show[input$show == "FOX NEWS"] <- "FOX_NEWS"
  input$show[input$show == "FOX NEWS NETWORK"] <- "NEWS_NETWORK"
  input$show[input$show == "FOX ON THE RECORD WITH GRETA VAN SUSTEREN"] <- "ON_THE_RECORD_VAN_SUSTEREN"
  input$show[input$show == "FOX ON THE RECORD WITH GRETA VAN SUSTEREN ("] <- "ON_THE_RECORD_VAN_SUSTEREN"
  input$show[input$show == "FOX SPECIAL REPORT WITH BRIT HUME"] <- "SPECIAL_REPORT_BRIT_HUME"
  input$show[input$show == "FOX SPECIAL REPORT WITH BRIT HUME ("] <- "SPECIAL_REPORT_BRIT_HUME"
  input$show[input$show == "GLOBAL BROADCAST DATABASE - ENGLISH"] <- "GBD_ENGLISH"
  input$show[input$show == "GOOD MORNING AMERICA"] <- "GMA"
  input$show[input$show == "GOOD MORNING AMERICA - ABC"] <- "GMA"
  input$show[input$show == "GOOD MORNING AMERICA ("] <- "GMA"
  input$show[input$show == "GOOD MORNING AMERICA WEEKEND EDITION ("] <- "GMA_WEEKEND_EDITION"
  input$show[input$show == "GREAT DAY SA"] <- "GREAT_DAY_SA"
  input$show[input$show == "GREENFIELD AT LARGE"] <- "GREENFIELD_AT_LARGE"
  input$show[input$show == "HARDBALL"] <- "HARDBALL"
  input$show[input$show == "HEALTH WEEK"] <- "HEALTH_WEEK"
  input$show[input$show == "INSIDE BUSINESS"] <- "INSIDE_BUSINESS"
  input$show[input$show == "INSIDERS"] <- "INSIDERS"
  input$show[input$show == "JOURNAL EDITORIAL REPORT"] <- "EDITORIAL_REPORT"
  input$show[input$show == "KAIT"] <- "KAIT"
  input$show[input$show == "KARE"] <- "KARE"
  input$show[input$show == "KATV"] <- "KATV"
  input$show[input$show == "KFSMMORN"] <- "KFSMMORN"
  input$show[input$show == "KHBS"] <- "KHBS"
  input$show[input$show == "KJRH"] <- "KJRH"
  input$show[input$show == "KOCO"] <- "KOCO"
  input$show[input$show == "KOKI"] <- "KOKI"
  input$show[input$show == "KOMO FIRST NEWS AT"] <- "KOMO_FIRST_NEWS_AT"
  input$show[input$show == "KSAT MORNING NEWS"] <- "KSAT_MORNING_NEWS"
  input$show[input$show == "KTHV"] <- "KTHV"
  input$show[input$show == "KXXV"] <- "KXXV"
  input$show[input$show == "LATE NEWS"] <- "LATE_NEWS"
  input$show[input$show == "LATE NEWSCHANNEL"] <- "LATE_NEWS"
  input$show[input$show == "LATER TODAY ("] <- "LATER_TODAY"
  input$show[input$show == "LEGAL VIEW WITH ASHLEIGH BANFIELD"] <- "ASHLEIGH_BANFIELD"
  input$show[input$show == "LIVE ON LOCATION"] <- "LIVE_ON_LOCATION"
  input$show[input$show == "LIVE TODAY"] <- "LIVE_TODAY"
  input$show[input$show == "LOCAL NEWS"] <- "LOCAL_NEWS"
  input$show[input$show == "LOU DOBBS THIS WEEK"] <- "LOU_DOBBS_THIS_WEEK"
  input$show[input$show == "MARKETPLACE ("] <- "MARKETPLACE"
  input$show[input$show == "MEET THE PRESS ("] <- "MTP"
  input$show[input$show == "M-F MIDDAY NEWS"] <- "M_F_MIDDAY_NEWS"
  input$show[input$show == "MORNING"] <- "MORNING"
  input$show[input$show == "MORNING EDITION"] <- "MORNING_EDITION"
  input$show[input$show == "MORNING NEWS"] <- "MORNING_NEWS"
  input$show[input$show == "MORNING SHOW"] <- "MORNING_SHOW"
  input$show[input$show == "MORNINGS ON TWO"] <- "MORING_ON_TWO"
  input$show[input$show == "NANCY GRACE"] <- "NANCY_GRACE"
  input$show[input$show == "NATIONAL PUBLIC RADIO"] <- "NPR"
  input$show[input$show == "NATIONAL PUBLIC RADIO (NPR)"] <- "NPR"
  input$show[input$show == "NBC"] <- "NBC"
  input$show[input$show == "NBC NEWS"] <- "NBC"
  input$show[input$show == "NBC NEWS TRANSCRIPTS"] <- "NBC"
  input$show[input$show == "NBC NIGHTLY NEWS"] <- "NIGHTLY_NEWS"
  input$show[input$show == "NBC NIGHTLY NEWS ("] <- "NIGHTLY_NEWS"
  input$show[input$show == "NBC NIGHTLY NEWS SPECIAL EDITION ("] <- "NIGHTLY_SPECIAL_EDITION"
  input$show[input$show == "NEV WEEK IN REVIEW"] <- "IN_REVIEW"
  input$show[input$show == "NEW DAY SATURDAY"] <- "NEW_DAY_SATURDAY"
  input$show[input$show == "NEW DAY SUNDAY"] <- "NEW_DAY_SUNDAY"
  input$show[input$show == "NEWS"] <- "NEWS"
  input$show[input$show == "NEWS AT"] <- "NEWS_AT"
  input$show[input$show == "NEWS AT NOON"] <- "NEWS_AT_NOON"
  input$show[input$show == "NEWS CENTER TONIGHT"] <- "NEWS_CENTER_TONIGHT"
  input$show[input$show == "NEWS FROM CNN"] <- "FROM_CNN"
  input$show[input$show == "NEWS M-F"] <- "NEWS_M_F"
  input$show[input$show == "NEWS SUN"] <- "NEWS_SUNDAY"
  input$show[input$show == "NEWS WATCH THIS WEEK"] <- "NEWS_WATCH_THIS_WEEK"
  input$show[input$show == "NEXT@CNN"] <- "NEXT_CNN"
  input$show[input$show == "NIGHTLINE"] <- "NIGHTLINE"
  input$show[input$show == "NIGHTLINE ("] <- "NIGHTLINE"
  input$show[input$show == "NIGHTLINE (ABC"] <- "NIGHTLINE"
  input$show[input$show == "NIGHTLINE HURRICANE ("] <- "NIGHTLINE_HURRICANE"
  input$show[input$show == "NIGHTLINE THIS IS AN UNCORRECTED COPY. NOT A FINAL VERSION."] <- "NIGHTLINE"
  input$show[input$show == "NIGHTLINE WEST COAST"] <- "NIGHTLINE_WEST_COAST"
  input$show[input$show == "ON THE RECORD WITH GRETA VAN SUSTEREN ("] <- "VAN_SUSTEREN"
  input$show[input$show == "PARKER SPITZER"] <- "PARKER_SPITZER"
  input$show[input$show == "PEOPLE IN THE NEWS"] <- "THE_NEWS"
  input$show[input$show == "POWER PLAY WITH DON MARTIN"] <- "DON_MARTIN"
  input$show[input$show == "PRIMETIME LIVE"] <- "PRIMETIME_LIVE"
  input$show[input$show == "PRIMETIME LIVE ("] <- "PRIMETIME_LIVE"
  input$show[input$show == "PRIMETIME LIVE (ABC"] <- "PRIMETIME_LIVE"
  input$show[input$show == "PRIMETIME LIVE THIS IS AN UNCORRECTED COPY. NOT A FINAL VERSION."] <- "PRIMETIME_LIVE"
  input$show[input$show == "PRIMETIME LIVE YOUNG WOMEN, PORN ("] <- "PRIMETIME_LIVE_PORN"
  input$show[input$show == "PRIMETIME LIVE YOUNG WOMEN, PORN AND ("] <- "PRIMETIME_LIVE_PORN"
  input$show[input$show == "RICK'S LIST"] <- "RICKS_LIST"
  input$show[input$show == "ROCK CENTER"] <- "ROCK_CENTER"
  input$show[input$show == "SAT. EARLY SHOW"] <- "SATURDA_EARLY_SHOW"
  input$show[input$show == "SATURDAY TODAY"] <- "SATURDAY_TODAY"
  input$show[input$show == "SATURDAY TODAY ("] <- "SATURDAY_TODAY"
  input$show[input$show == "SCARBOROUGH COUNTRY"] <- "SCARBOROUGH_COUNTRY"
  input$show[input$show == "SCIENCE & TECHNOLOGY WEEK"] <- "SCIENCE_TECHNOLOGY_WEEK"
  input$show[input$show == "SCIENCE AND TECHNOLOGY WEEK"] <- "SCIENCE_TECHNOLOGY_WEEK"
  input$show[input$show == "SHOWBIZ TONIGHT"] <- "SHOWBIZ_TONIGHT"
  input$show[input$show == "SIX"] <- "SIX"
  input$show[input$show == "SPEAKING OF FAITH ("] <- "SPEAKING_OF_FAITH"
  input$show[input$show == "SPORTSCENTER"] <- "SPORTSCENTER"
  input$show[input$show == "STATE OF THE UNION WITH JOHN KING"] <- "JOHN_KING"
  input$show[input$show == "SUNDAY MORNING"] <- "SUNDAY_MORNING"
  input$show[input$show == "SUNDAY MORNING ("] <- "SUNDAY_MORNING"
  input$show[input$show == "SUNDAY TODAY"] <- "SUNDAY_TODAY"
  input$show[input$show == "SUNDAY TODAY ("] <- "SUNDAY_TODAY"
  input$show[input$show == "TEN"] <- "TEN"
  input$show[input$show == "THE CAVUTO BUSINESS REPORT ("] <- "BUSINESS_REPORT"
  input$show[input$show == "THE CHRIS MATTHEWS SHOW  VARIOUS TIMES NBC"] <- "CHRIS_MATTHEWS"
  input$show[input$show == "THE EARLY SHOW"] <- "EARLY_SHOW"
  input$show[input$show == "THE ED SHOW WITH ED SCHULTZ"] <- "ED_SCHULTZ"
  input$show[input$show == "THE FIVE"] <- "THE_FIVE"
  input$show[input$show == "THE O'REILLY FACTOR"] <- "OREILLY_FACTOR"
  input$show[input$show == "THE O'REILLY FACTOR ("] <- "OREILLY_FACTOR"
  input$show[input$show == "THE RACHEL MADDOW SHOW"] <- "MADDOW_SHOW"
  input$show[input$show == "THE SATURDAY EARLY SHOW"] <- "SATURDAY_EARLY_SHOW"
  input$show[input$show == "THE SATURDAY EARLY SHOW ("] <- "SATURDAY_EARLY_SHOW"
  input$show[input$show == "THE SITUATION ROOM"] <- "SITUATION_ROOM"
  input$show[input$show == "THE TURNAROUND"] <- "THE_TURNAROUND"
  input$show[input$show == "THE WEEK IN REVIEW"] <- "THE_WEEK_IN_REVIEW"
  input$show[input$show == "THE WEEKEND REPORT (WEEKEND NEWS SAT.)"] <- "THE_WEEKEND_REPORT"
  input$show[input$show == "THE WORLD TODAY"] <- "WORLD_TODAY"
  input$show[input$show == 'THE WORLD TODAY ("ASK CNN")'] <- "WORLD_TODAY"
  input$show[input$show == "THIS WEEK"] <- "THIS_WEEK"
  input$show[input$show == "THIS WEEK ("] <- "THIS_WEEK"
  input$show[input$show == "THIS WEEK AT WAR"] <- "THIS_WEEK_AT_WAR"
  input$show[input$show == "THIS WEEK WITH BOB MUELLER"] <- "BOB_MUELLER"
  input$show[input$show == "THIS WEEK WITH DAVID BRINKLEY"] <- "DAVID_BRINKLEY"
  input$show[input$show == "THIS WEEK WITH DAVID BRINKLEY (ABC"] <- "DAVID_BRINKLEY"
  input$show[input$show == "THIS WEEK WITH DAVID BRINKLEY THIS IS AN UNCORRECTED COPY. NOT A FINAL VERSION."] <- "DAVID_BRINKLEY"
  input$show[input$show == "THIS WEEK WITH GEORG"] <- "THIS_WEEK_GEORGE_STEPHANOPOULOS"
  input$show[input$show == "THIS WEEK WITH GEORGE STEPHANOPOULOS - ABC"] <- "THIS_WEEK_GEORGE_STEPHANOPOULOS"
  input$show[input$show == "THIS WEEK WITH GEORGE STEPHANOPOULOS ("] <- "THIS_WEEK_GEORGE_STEPHANOPOULOS"
  input$show[input$show == "THIS WEEK WITH GEORGE STEPHANPOULOS ("] <- "THIS_WEEK_GEORGE_STEPHANOPOULOS"
  input$show[input$show == "THIS WEEK WITH SAM DONALDSON AND COKIE ROBERTS ("] <- "THIS_WEEK_DONALDSON_ROBERTS"
  input$show[input$show == "TODAY"] <- "TODAY"
  input$show[input$show == "TODAY ("] <- "TODAY"
  input$show[input$show == "TODAY IN FLORIDA"] <- "TODAY_IN_FLORIDA"
  input$show[input$show == "TURNING POINT"] <- "TURNING_POINT"
  input$show[input$show == "TURNING POINT (ABC)"] <- "TURNING_POINT"
  input$show[input$show == "UNGUARDED WITH RACHEL NICHOLS"] <- "RACHEL_NICHOLS"
  input$show[input$show == "UP CLOSE"] <- "UP_CLOSE"
  input$show[input$show == "UP CLOSE ("] <- "UP_CLOSE"
  input$show[input$show == "USA TONIGHT"] <- "USA_TONIGHT"
  input$show[input$show == "VERDICT WITH DAN ABRAMS"] <- "DAN_ABRAMS"
  input$show[input$show == "WALL ST WEEK FORTUNE"] <- "WEEK_FORTUNE"
  input$show[input$show == "WEEKEND EARLY START"] <- "EARLY_START"
  input$show[input$show == "WEEKEND EDITION SATURDAY"] <- "WEEKEND_EDITION_SATURDAY"
  input$show[input$show == "WEEKEND NEWS SATURDAY LATE - ABC"] <- "WEEKEND_NEWS_SATURDAY_LATE"
  input$show[input$show == "WEEKEND NEWS SATURDAY LATE WEEKEND NEWS SATURDAY LATE - ABC"] <- "WEEKEND_NEWS_SATURDAY_LATE"
  input$show[input$show == "WEEKEND NEWS SATURDAY LATEWEEKEND NEWS SATURDAY LATE - ABC"] <- "WEEKEND_NEWS_SATURDAY_LATE"
  input$show[input$show == "WOAI"] <- "WOAI"
  input$show[input$show == "WOLF"] <- "WOLF"
  input$show[input$show == "WOLF BLITZER REPORTS"] <- "WOLF_BLITZER_REPORTS"
  input$show[input$show == "WORLD NEWS NOW ("] <- "WORLD_NEWS_NOW"
  input$show[input$show == "WORLD NEWS SATURDAY"] <- "WORLD_NEWS_SATURDAY"
  input$show[input$show == "WORLD NEWS SUNDAY"] <- "WORLD_NEWS_SUNDAY"
  input$show[input$show == "WORLD NEWS THIS MORNING - ABC"] <- "WORLD_NEWS_THIS_MORNING"
  input$show[input$show == "WORLD NEWS THIS MORNING ("] <- "WORLD_NEWS_THIS_MORNING"
  input$show[input$show == "WORLD NEWS TONIGHT SATURDAY - ABC"] <- "WORLD_NEWS_TONIGHT_SATURDAY"
  input$show[input$show == "WORLD NEWS TONIGHT SATURDAY ("] <- "WORLD_NEWS_TONIGHT_SATURDAY"
  input$show[input$show == "WORLD NEWS TONIGHT SUNDAY - ABC"] <- "WORLD_NEWS_TONIGHT_SUNDAY"
  input$show[input$show == "WORLD NEWS TONIGHT SUNDAY ("] <- "WORLD_NEWS_TONIGHT_SUNDAY"
  input$show[input$show == "WORLD NEWS WITH DIANE SAWYER"] <- "DIANE_SAWYER"
  input$show[input$show == "WSJ EDITORIAL REPORT"] <- "EDITORIAL_REPORT"
  input$show[input$show == "YOUR BOTTOM LINE"] <- "YOUR_BOTTOM_LINE"
  input$show[input$show == "YOUR MONEY"] <- "YOUR_MONEY"
  input$show[input$show == "YOUR TOTAL HEALTH  VARIOUS TIMES NBC"] <- "YOUR_TOTAL_HEALTH"
  input$show[input$show == "YOUR WORLD TODAY"] <- "WORLD_TODAY"
  input$show[input$show == "YOUR WORLD WITH NEIL CAVUTO"] <- "NEIL_CAVUTO"
  input$show[input$show == "YOUR WORLD WITH NEIL CAVUTO ("] <- "NEIL_CAVUTO"
  input$show[input$show == "YOUR WORLD WITH NEIL CAVUTO ("] <- "NEIL_CAVUTO"
  input$show[input$show == "BECK"] <- "BECK"
  input$show[input$show == "CAMPBELL BROWN"] <- "CAMPBELL_BROWN"
  input$show[input$show == "CBS EVENING NEWS"] <- "EVENING_NEWS"
  input$show[input$show == "CBS EVENING NEWS, SATURDAY EDITION"] <- "EVENING_NEWS_SAT_EDITION"
  input$show[input$show == "CBS EVENING NEWS, SUNDAY EDITION"] <- "EVENING_NEWS_SUN_EDITION"
  input$show[input$show == "CBS FACE THE NATION"] <- "THE_NATION"
  input$show[input$show == "CBS MORNING NEWS"] <- "MORNING_NEWS"
  input$show[input$show == "CBS MORNING NEWS ("] <- "MORNING_NEWS"
  input$show[input$show == "CBS THE EARLY SHOW"] <- "EARLY_SHOW"
  input$show[input$show == "CHRIS MATTHEWS SHOW"] <- "CHRIS_MATTHEWS"
  input$show[input$show == 'CNN "INSIDE POLITICS"'] <- "INSIDE_POLITICS"
  input$show[input$show == "CNN BOTH SIDES WITH JESSE JACKSON"] <- "JESSE_JACKSON"
  input$show[input$show == "CNN BURDEN OF PROOF"] <- "BURDEN_OF_PROOF"
  input$show[input$show == "CNN CONNIE CHUNG TONIGHT"] <- "CONNIE_CHUNG"
  input$show[input$show == "CNN CROSSFIRE"] <- "CROSSFIRE"
  input$show[input$show == "CNN CROSSFIRE SUNDAY"] <- "CROSSFIRE_SUNDAY"
  input$show[input$show == "CNN DAYBREAK"] <- "DAYBREAK"
  input$show[input$show == "CNN DAYBREAK SATURDAY"] <- "DAYBREAK_SATURDAY"
  input$show[input$show == "CNN DIPLOMATIC LICENSE"] <- "DIPLOMATIC_LICENSE"
  input$show[input$show == "CNN EARTH MATTERS"] <- "EARTH_MATTERS"
  input$show[input$show == "CNN HE SAID/SHE SAID"] <- "HE_SAID_SHE_SAID"
  input$show[input$show == "CNN IN THE MONEY"] <- "IN_THE_MONEY"
  input$show[input$show == "CNN INSIDE POLITICS"] <- "INSIDE_POLITICS"
  input$show[input$show == "CNN LARRY KING LIVE"] <- "LARRY_KING"
  input$show[input$show == "CNN LARRY KING WEEKEND"] <- "LARRY_KING_WEEKEND"
  input$show[input$show == "CNN LIVE AT DAYBREAK"] <- "LIVE_DAYBREAK"
  input$show[input$show == "CNN LIVE THIS MORNING"] <- "LIVE_MORNING"
  input$show[input$show == "CNN LOU DOBBS MONEYLINE"] <- "DOBBS_MONEYLINE"
  input$show[input$show == "CNN MONEYLINE NEWS HOUR"] <- "MONEYLINE_NEWS_HOUR"
  input$show[input$show == "CNN MONEYLINE WEEKEND"] <- "MONEYLINE_WEEKEND"
  input$show[input$show == "CNN MONEYLINE WEEKEND EDITION"] <- "MONEYLINE_WEEKEND"
  input$show[input$show == "CNN NEWSROOM"] <- "NEWSROOM"
  input$show[input$show == "CNN NEWSROOM>Y"] <- "NEWSROOM"
  input$show[input$show == "CNN PINNACLE"] <- "PINNACLE"
  input$show[input$show == "CNN RELIABLE SOURCES"] <- "RELIABLE_SOURCES"
  input$show[input$show == "CROSSFIRE"] <- "CROSSFIRE"
  input$show[input$show == "CROSSFIRE SUNDAY"] <- "CROSSFIRE_SUNDAY"
  input$show[input$show == "DATELINE NBC"] <- "DATELINE"
  input$show[input$show == "DATELINE NBC ("] <- "DATELINE"
  input$show[input$show == "DR. DREW"] <- "DR_DREW"
  input$show[input$show == "EARLY START"] <- "EARLY_START"
  input$show[input$show == "EARLY START WITH ASHLEIGH BANFIELD AND ZORAIDA SAMBOLIN"] <- "EARLY_START_BANFIELD_SAMBOLIN"
  input$show[input$show == "EARLY START WITH JOHN BERMAN AND ZORADA SAMBOLIN"] <- "EARLY_START_BERMAN_SAMBOLIN"
  input$show[input$show == "EARLY START WITH JOHN BERMAN AND ZORAIDA SAMBOLIN"] <- "EARLY_START_BERMAN_SAMBOLIN"
  input$show[input$show == "EARTH MATTERS"] <- "EARTH_MATTERS"
  input$show[input$show == "ERIN BURNETT OUTFRONT"] <- "ERIN_BURNETT"
  input$show[input$show == "FACE THE NATION"] <- "FACE_THE_NATION"
  input$show[input$show == "FACE THE NATION ("] <- "FACE_THE_NATION"
  input$show[input$show == "FAREED ZAKARIA GPS"] <- "FAREED_ZAKARIA"
  input$show[input$show == "FOX FILES"] <- "FOX_FILES"
  input$show[input$show == "FOX HANNITY"] <- "HANNITY"
  input$show[input$show == "FOX HANNITY & COLMES"] <- "HANNITY_COLMES"
  input$show[input$show == "FOX HANNITY & COLMES ("] <- "HANNITY_COLMES"
  input$show[input$show == "FOX HANNITY &AMP"] <- "HANNITY_AMP"
  input$show[input$show == "FOX NEWS EDGE ("] <- "NEWS_EDGE"
  input$show[input$show == "FOX NEWS SUNDAY"] <- "NEWS_SUNDAY"
  input$show[input$show == "FOX NEWS SUNDAY ("] <- "NEWS_SUNDAY"
  input$show[input$show == "FOX NEWS WATCH"] <- "NEWS_WATCH"
  input$show[input$show == "FOX NEWS WATCH ("] <- "NEWS_WATCH"
  input$show[input$show == "FOX THE BIG STORY ("] <- "BIG_STORY"
  input$show[input$show == "FOX THE O'REILLY FACTOR"] <- "OREILLY_FACTOR"
  input$show[input$show == "FOX WIRE ("] <- "WIRE"
  input$show[input$show == "GLENN BECK"] <- "BECK"
  input$show[input$show == "HANNITY'S AMERICA"] <- "HANNITY_AMERICA"
  input$show[input$show == "HANNITY & COLMES ("] <- "HANNITY_COLMES"
  input$show[input$show == "INSIDE POLITICS"] <- "INSIDE_POLITICS"
  input$show[input$show == "INSIDE POLITICS SUNDAY"] <- "POLITICS_SUNDAY"
  input$show[input$show == "ISSUE NUMBER ONE"] <- "ISSUE_NUMBER_ONE"
  input$show[input$show == "JOHN KING, USA"] <- "JOHN_KING"
  input$show[input$show == "JUDY WOODRUFF'S INSIDE POLITICS"] <- "JUDY_WOODRUFF"
  input$show[input$show == "JUDY WOODRUFFS'S INSIDE POLITICS"] <- "JUDY_WOODRUFF"
  input$show[input$show == "LIVE FROM..."] <- "LIVE_FROM"
  input$show[input$show == "LOU DOBBS MONEYLINE"] <- "DOBBS_MONEYLINE"
  input$show[input$show == "LOU DOBBS TONIGHT"] <- "DOBBS_TONIGHT"
  input$show[input$show == "MARKETPLACE MORNING REPORT ("] <- "MORNING_REPORT"
  input$show[input$show == "MEET THE PRESS"] <- "THE_PRESS"
  input$show[input$show == "MELISSA-HARRIS-PERRY"] <- "HARRIS_PERRY"
  input$show[input$show == "MELISSA HARRIS-PERRY"] <- "HARRIS_PERRY"
  input$show[input$show == "MSNBC HARDBALL"] <- "MSNBC_HARDBALL"
  input$show[input$show == "PIERS MORGAN LIVE"] <- "MORGAN_LIVE"
  input$show[input$show == "PIERS MORGAN TONIGHT"] <- "MORGAN_TONIGHT"
  input$show[input$show == "POLITICS NATION"] <- "POLITICS_NATION"
  input$show[input$show == "RITA COSBY LIVE & DIR"] <- "RITA_COSBY"
  input$show[input$show == "STARTING POINT WITH SOLEDAD O'BRIEN"] <- "SOLEDAD_OBRIEN"
  input$show[input$show == "THE BELTWAY BOYS"] <- "BELTWAY_BOYS"
  input$show[input$show == "THE BELTWAY BOYS ("] <- "BELTWAY_BOYS"
  input$show[input$show == "THE BIG STORY ("] <- "BIG_STORY"
  input$show[input$show == "THE BIG STORY WITH JOHN GIBSON"] <- "JOHN_GIBSON"
  input$show[input$show == "THE BIG STORY WITH JOHN GIBSON ("] <- "JOHN_GIBSON"
  input$show[input$show == "THE CHRIS MATTHEWS SHOW ("] <- "CHRIS_MATTHEWS"
  input$show[input$show == "THE CHRIS MATTHEWS SHOW (VARIOUS TIMES) - NBC"] <- "CHRIS_MATTHEWS"
  input$show[input$show == "THE EARLY SHOW ("] <- "EARLY_SHOW"
  input$show[input$show == "THE ED SHOW"] <- "ED_SHOW"
  input$show[input$show == "THE KELLY FILE"] <- "KELLY_FILE"
  input$show[input$show == "THE LAST WORD WITH LAWRENCE O' DONNELL"] <- "LAWRENCE_ODONNELL"
  input$show[input$show == "THE LAST WORD WITH LAWRENCE O'DONNELL"] <- "LAWRENCE_ODONNELL"
  input$show[input$show == "UP LATE"] <- "UP_LATE"
  input$show[input$show == "UP WITH CHRIS HAYES"] <- "CHRIS_HAYES"
  input$show[input$show == "UP WITH STEVE KORNACKI"] <- "STEVE_KORNACKI"
  return(input)
}