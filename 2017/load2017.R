library(CodeExtractoR)

links <- RJ_links[RJ_links$year == 2017 & RJ_links$issue_nr == 1,]
links <- links[order(links$pdf_href),][16:20,"pdf_href"]
api_key <- readLines("~/Studia/Semestr4/WarBady/api_key.txt")
for(link in links) extract_code_from_pdf(link, api_key = api_key)

file.remove("RJ-2017-001.R")
extract_code_from_html("RJ-2017-001.html", console_char = ">")

file.remove("RJ-2017-003.R")
extract_code_from_html("RJ-2017-003.html", console_char = ">")

file.remove("RJ-2017-004.R")
extract_code_from_html("RJ-2017-004.html", console_char = ">")

file.remove("RJ-2017-008.R")
extract_code_from_html("RJ-2017-008.html", console_char = ">")

file.remove("RJ-2017-011.R")
extract_code_from_html("RJ-2017-011.html", console_char = ">")

file.remove("RJ-2017-014.R")
extract_code_from_html("RJ-2017-014.html", console_char = ">")

file.remove("RJ-2017-015.R")
extract_code_from_html("RJ-2017-015.html", console_char = ">")

file.remove("RJ-2017-017.R")
extract_code_from_html("RJ-2017-017.html", console_char = ">")

file.remove("RJ-2017-018.R")
extract_code_from_html("RJ-2017-018.html", console_char = ">")
