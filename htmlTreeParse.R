abs_doc = htmlTreeParse(abs.html[1], useInternal = TRUE)
txt = xpathApply(abs_doc, "//body//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)]", xmlValue)
txt_list = unlist(txt)[52]
