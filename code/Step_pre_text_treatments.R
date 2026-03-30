library(readtext)
library(stringr)
setwd("~/ARMELLONI_SLU/RiskAnalysis/LEK/interviews_round2_aug2025")
x <- readtext('summary_styles.docx')
doc.text=x$text
doc.text=tolower(doc.text)
doc.parts <- strsplit(doc.text, "\\id:")[[1]]
doc.parts=doc.parts[2:8]
styles=data.frame(text=doc.parts)
styles$id_I=substr(styles$text,2,2)
write.csv(styles,'C:/github/extreme_weather_risk/data/styles_desc.csv', row.names = F)
