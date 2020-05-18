library(CodeExtractoR)

links <- RJ_links[RJ_links$year == 2015 & RJ_links$issue_nr == 1,][1:10,"pdf_href"]
links <- links[6:10]
api_key <- readLines("~/Studia/Semestr4/WarBady/api_key.txt")
for(link in links) extract_code_from_pdf(link, api_key = api_key)

file.remove("RJ-2015-001.R")
extract_code_from_html("RJ-2015-001.html", console_char = ">")

file.remove("RJ-2015-003.R")
extract_code_from_html("RJ-2015-003.html", console_char = ">")

file.remove("RJ-2015-004.R")
extract_code_from_html("RJ-2015-004.html", console_char = ">")

file.remove("RJ-2015-005.R")
extract_code_from_html("RJ-2015-005.html", console_char = ">")

file.remove("RJ-2015-006.R")
extract_code_from_html("RJ-2015-006.html", console_char = ">")

file.remove("RJ-2015-007.R")
extract_code_from_html("RJ-2015-007.html", console_char = ">")

file.remove("RJ-2015-008.R")
extract_code_from_html("RJ-2015-008.html", console_char = ">")

file.remove("RJ-2015-010.R")
extract_code_from_html("RJ-2015-010.html", console_char = ">")
