text_treatment: can be moved, it serves only to extract a csv file from word files


z.style$f.style.2 -> z.style$f.style

styles[z,]$f.style -> styles[z,]$short_description

style.dataset[style.dataset$gear==i.gear[i],] -> gear.dataset[gear.dataset$gear==i.gear[i],] 



i.fisher=style.dataset[style.dataset$gear==i.gear[i],]
  i.fisher=unique(i.fisher$id)

goes as

i.fisher=gear.dataset[gear.dataset$gear==i.gear[i],] 
   i.fisher=unique(style.dataset[style.dataset$short_description %in% i.fisher$fishing_style,]$id_I)