

sFile = Pmisc::downloadIfOld(
	'https://www150.statcan.gc.ca/n1/en/tbl/csv/13100779-eng.zip?st=7Wam1ruF',
		file=tempfile(fileext='.zip'))

x = read.csv(sFile[2])

x = x[grep("Quebec", x$GEO), ]
x$dateEnd = gsub("Week of death, ending |[/].*", "", x$Week.of.death)
x$timeChar = paste(x$X...REF_DATE, x$dateEnd)
x$date = as.Date(x$timeChar, format = '%Y %B %d')

x$year = as.numeric(format(x$date, '%Y'))
x$month = as.numeric(format(x$date, '%m'))

qFile = Pmisc::downloadIfOld(
	'https://www.bdso.gouv.qc.ca/docs-ken/multimedia/ken_01662_698_fr.xlsx')
xQ = as.data.frame(readxl::read_xlsx(qFile, range='B25:L36', 
	col_names=FALSE, na='...'))
colnames(xQ) = 2010:2020
xQ$month = 1:12
xQ2 = reshape2::melt(xQ, id.var='month', variable.name='year',
	value.name = 'bdso')

xSc = aggregate(x[,'VALUE', drop=FALSE], 
	x[,c('year','month')],
	sum)
colnames(xSc) = gsub("VALUE", "statsCan", colnames(xSc))

xMerge = merge(xSc, xQ2)
xMerge = xMerge[order(xMerge$month, xMerge$year), ]
xMerge[xMerge$year > 2017,]