library(CodeExtractoR)

links <- RJ_links[RJ_links$year == 2011 & RJ_links$issue_nr == 1,"pdf_href"]
api_key <- readLines("~/Studia/Semestr4/WarBady/api_key.txt")
for(link in links){
  # extract_code_from_pdf(link, api_key = api_key)
}

### ----- 001 -----
file.remove("RJ-2011-001.R")
extract_code_from_html("RJ-2011-001.html", console_char = ">")
# Werykt: 2

# 003
file.remove("RJ-2011-003.R")
extract_code_from_html("RJ-2011-003.html", console_char = ">")
# 005
file.remove("RJ-2011-005.R")
extract_code_from_html("RJ-2011-005.html", console_char = ">")
# 007 
file.remove("RJ-2011-007.R")
extract_code_from_html("RJ-2011-007.html", console_char = ">")
# 008 
file.remove("RJ-2011-008.R")
extract_code_from_html("RJ-2011-008.html", console_char = ">")
# 010 
file.remove("RJ-2011-010.R")
extract_code_from_html("RJ-2011-010.html", console_char = ">")
