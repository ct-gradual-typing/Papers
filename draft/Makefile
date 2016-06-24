PDFLATEX = pdflatex
BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_wrap false -tex_show_meta false -picky_multiple_parses false
SKIM := skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

TexFileName := main
OTTFileName := grady
OTTFile := $(OTTFileName).ott
OTTGen := $(OTTFileName)-inc.tex
OTTOutputFile := $(TexFileName)-output.tex
OTTPrefix := Ott

Main := $(TexFileName).tex
References := references.bib 

PDF := $(TexFileName).pdf

all: pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
  endif

pdf : $(PDF)

$(OTTOutputFile) : $(OTTFile) $(Main)
	@echo "\n\n***OTT: Preprocessing L.ott in main.tex.***"
	@$(OTT) $(OTT_FLAGS) -i $(OTTFile)  -o $(OTTGen) -tex_name_prefix $(OTTPrefix) \
		-tex_filter $(Main) $(OTTOutputFile)

# Now this takes the full LaTex translation and compiles it using
# pdflatex.
$(PDF) :  $(OTTOutputFile) Makefile $(References)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(BIBTEX) $(TexFileName)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
