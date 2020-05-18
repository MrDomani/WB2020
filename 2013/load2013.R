library(CodeExtractoR)

links <- RJ_links[RJ_links$year == 2013 & RJ_links$issue_nr == 1,][1:10,"pdf_href"]
api_key <- readLines("~/Studia/Semestr4/WarBady/api_key.txt")
for(link in links) extract_code_from_pdf(link, api_key = api_key)

# 002
file.remove("RJ-2013-002.R")
extract_code_from_html("RJ-2013-002.html", console_char = ">")

file.remove("RJ-2013-003.R")
extract_code_from_html("RJ-2013-003.html", console_char = ">")

file.remove("RJ-2013-004.R")
extract_code_from_html("RJ-2013-004.html", console_char = ">")

file.remove("RJ-2013-005.R")
extract_code_from_html("RJ-2013-005.html", console_char = ">")

file.remove("RJ-2013-006.R")
extract_code_from_html("RJ-2013-006.html", console_char = "")

file.remove("RJ-2013-009.R")
extract_code_from_html("RJ-2013-009.html", console_char = ">")
