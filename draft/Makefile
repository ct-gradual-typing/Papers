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

core-grady-ott.tex : core-grady.tex core-grady/core-grady.ott
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott  -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter core-grady.tex core-grady-ott.tex

grady-ott.tex : grady.tex grady.ott
	@$(OTT) $(OTT_FLAGS) -i grady.ott  -o grady-inc.tex -tex_name_prefix Grady \
		-tex_filter grady.tex grady-ott.tex

proofs-ott.tex : proofs.tex grady.ott
	@$(OTT) $(OTT_FLAGS) -i grady.ott  -o grady-inc.tex -tex_name_prefix Grady \
		-tex_filter proofs.tex proofs-ott.tex

categorical-model-ott.tex : categorical-model.tex grady.ott
	@$(OTT) $(OTT_FLAGS) -i grady.ott  -o grady-inc.tex -tex_name_prefix Grady \
		-tex_filter categorical-model.tex categorical-model-ott.tex

gradual-typing-ott.tex : gradual-typing.tex grady.ott
	@$(OTT) $(OTT_FLAGS) -i grady.ott  -o grady-inc.tex -tex_name_prefix Grady \
		-tex_filter gradual-typing.tex gradual-typing-ott.tex

introduction-ott.tex : introduction.tex grady.ott
	@$(OTT) $(OTT_FLAGS) -i grady.ott  -o grady-inc.tex -tex_name_prefix Grady \
		-tex_filter introduction.tex introduction-ott.tex

$(OTTOutputFile) : grady.ott main.tex introduction-ott.tex gradual-typing-ott.tex core-grady-ott.tex \
	categorical-model-ott.tex proofs-ott.tex grady-ott.tex
	@$(OTT) $(OTT_FLAGS) -i grady.ott  -o grady-inc.tex -tex_name_prefix Grady \
		-tex_filter main.tex main-output.tex

# Now this takes the full LaTex translation and compiles it using
# pdflatex.
$(PDF) :  $(OTTOutputFile) Makefile $(References)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(BIBTEX) $(TexFileName)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex
