REXE=R
PANDOC=pandoc
XELATEX=xelatex
BIBER=biber
RM=rm
CP=cp
PANDOCCROSSREF=--filter=pandoc-crossref
SED=sed
pandocTo=latex 

all: Coronavirus_Data.html Coronavirus_Stan.html Coronavirus_Export.html 

data: Coronavirus_Data.html

wave2: Coronavirus_Data.html secondWave.pdf 	

DOCXTEMPLATE = $(REXE) --slave -e "base::cat(base::system.file('src','template.docx',package='Pmisc'))"

ODTTEMPLATE = $(REXE) --slave -e "base::cat(base::system.file('src','template.odt',package='Pmisc'))"
## 
##  temporary files to remove
## 

clean:
	$(RM) *.run.xml* *.blg *.out *.log *.aux *.nav *.toc *.vrb
	
## 
##  tmd file is markdown only to be converted to latex, 
##  include
##  if(any(commandArgs()=='mdToTex')) {
##    knitr::knit_hooks$set(plot=knitr::hook_plot_tex)  
##    mdToTex=TRUE
##  } else {
##    knitr::knit_hooks$set(plot=Pmisc::hook_plot_htmlsubfig)
##    mdToTex=FALSE
##  }
##  specify files for tex from tmd directly 
##  so as not to conflict with %.tex: %.md rule
## 
##  md to tex, convert from a tmd if it exists
##  get rid of subfig package, it conflicts with subcaption (or something else)

%.tex: %.tmd
	$(PANDOC) --standalone $(PANDOCCROSSREF) --biblatex $(pandocArgs) --from=markdown --to=$(pandocTo) $< | $(SED) 's/.usepackage.subfig.//' > $@
## 
##  Create tmd
## 

%.tmd: %.Rmd
	$(REXE) -e "base::library('base');knitr::knit('$<',output='$@',encoding='UTF-8')" mdToTex $(Rargs) > $@.log
## 
##  Bog standard markdown
## 

%.md: %.Rmd  
	$(REXE) -e "base::library('base');knitr::knit('$<',encoding='UTF-8')" $(Rargs) > $@.log
## 
##  spin
##  

%.Rmd: %.R
	$(REXE) -e "base::library('base');knitr::spin('$<',knit=FALSE,format='Rmd')" $(Rargs)
## 
##  tex bibliography
## 
##  biblatex

%.bbl: %.bcf
	$(BIBER) $<	

%.bcf: %.tex
	$(XELATEX) -interaction=nonstopmode $<
##  bibtex

%.aux: %.tex
	$(XELATEX) -interaction=nonstopmode $<

%.bst: %.aux
	$(BIBTEX) $<	
## 
##  pdf from tex
##  
##  change bbl to bst to use bibtex

%.pdf: %.tex %.bbl
	$(XELATEX) -interaction=nonstopmode $<;
	$(XELATEX) -interaction=nonstopmode $<
## 
##  html from md
## 

%.html: %.md
	$(PANDOC) --self-contained --mathml $(PANDOCCROSSREF) --filter=pandoc-citeproc $(pandocArgs) --to=html5 --output=$@ $<
##  
##  libreoffice document
## 

%.odt: %.md
	$(PANDOC) --standalone $(PANDOCCROSSREF) --filter=pandoc-citeproc $(pandocArgs) --output=$@ $<
##  --reference-doc=$$($(ODTTEMPLATE))  no currently working
## 
##  MS Word document
## 

%.docx: %.md
	$(PANDOC) --standalone $(PANDOCCROSSREF) --filter=pandoc-citeproc $(pandocArgs) --reference-doc=$$($(DOCXTEMPLATE)) --output=$@ $<
##  
##  rtf
##  

%.rtf: %.md
	$(PANDOC) --standalone $(PANDOCCROSSREF) --filter=pandoc-citeproc $(pandocArgs) --output=$@ $<
##  
##  plain text
##  

%.txt: %.md
	$(PANDOC) --standalone $(PANDOCCROSSREF) --filter=pandoc-citeproc $(pandocArgs) --to=plain --output=$@ $<
## 
##  latex from md
##  should only be used if there's no .tmd or .Rmd file
## 
## %.tex: %.md
## 	$(PANDOC) --standalone $(PANDOCCROSSREF) --biblatex $(pandocArgs) --from=markdown --to=$(pandocTo) --output=$@ $<