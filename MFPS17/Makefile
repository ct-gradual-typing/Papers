PDFLATEX = pdflatex
BIBTEX = bibtex
OTT = ott
OTT_FLAGS := -tex_wrap false -tex_show_meta false -picky_multiple_parses false
SKIM := skim_revert.sh
SKIMRevinPath := $(shell command -v $(SKIM) 2> /dev/null)

TexFileName := main
OTTOutputFile := main.tex
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

quick: quick-pdf
  # This is for my private machine.  It forces my PDF reader to reload.
  # It should not run unless "skim_revert.sh" is in your PATH.
  ifdef SKIMRevinPath
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
	@$(SKIM) $(PDF) &>/dev/null
  endif

pdf : $(PDF)

sl-grady-ott.tex : sl-grady.tex sl-grady/sl-grady.ott siek15-gradual/siek-gradual.ott
	@$(OTT) $(OTT_FLAGS) -i sl-grady/sl-grady.ott -o sl-grady-inc.tex -tex_name_prefix SLGrady \
		-tex_filter sl-grady.tex sl-grady-ott1.tex
	scripts/prepr_double_ott.sh sl-grady-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i siek15-gradual/siek-gradual.ott -o siek15-gradual-inc.tex -tex_name_prefix GSiek \
		-tex_filter sl-grady-ott1.tex sl-grady-ott.tex

proofs-ott.tex : proofs.tex sl-grady/sl-grady.ott siek15-gradual/siek-gradual.ott
	@$(OTT) $(OTT_FLAGS) -i sl-grady/sl-grady.ott  -o sl-grady-inc.tex -tex_name_prefix SLGrady \
		-tex_filter proofs.tex proofs-ott1.tex
	scripts/prepr_double_ott.sh proofs-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i siek15-gradual/siek-gradual.ott -o siek15-gradual-inc.tex -tex_name_prefix GSiek \
		-tex_filter proofs-ott1.tex proofs-ott.tex

categorical-model-ott.tex : categorical-model.tex sl-grady/sl-grady.ott siek15-gradual/siek-gradual.ott
	@$(OTT) $(OTT_FLAGS) -i sl-grady/sl-grady.ott  -o sl-grady-inc.tex -tex_name_prefix SLGrady \
		-tex_filter categorical-model.tex categorical-model-ott1.tex
	scripts/prepr_double_ott.sh categorical-model-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i siek15-gradual/siek-gradual.ott -o siek15-gradual-inc.tex -tex_name_prefix GSiek \
		-tex_filter categorical-model-ott1.tex categorical-model-ott.tex

categorical-interp-ott.tex : categorical-interp.tex sl-grady/sl-grady.ott siek15-gradual/siek-gradual.ott
	@$(OTT) $(OTT_FLAGS) -i sl-grady/sl-grady.ott  -o sl-grady-inc.tex -tex_name_prefix SLGrady \
		-tex_filter categorical-interp.tex categorical-interp-ott1.tex
	scripts/prepr_double_ott.sh categorical-interp-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i siek15-gradual/siek-gradual.ott -o siek15-gradual-inc.tex -tex_name_prefix GSiek \
		-tex_filter categorical-interp-ott1.tex categorical-interp-ott.tex

gradual-typing-ott.tex : gradual-typing.tex sl-grady/sl-grady.ott siek15-gradual/siek-gradual.ott
	@$(OTT) $(OTT_FLAGS) -i siek15-gradual/siek-gradual.ott -o siek15-gradual-inc.tex -tex_name_prefix GSiek \
		-tex_filter gradual-typing.tex gradual-typing-ott.tex

introduction-ott.tex : introduction.tex sl-grady/sl-grady.ott siek15-gradual/siek-gradual.ott
	@$(OTT) $(OTT_FLAGS) -i sl-grady/sl-grady.ott  -o sl-grady-inc.tex -tex_name_prefix SLGrady \
		-tex_filter introduction.tex introduction-ott1.tex
	scripts/prepr_double_ott.sh introduction-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i siek15-gradual/siek-gradual.ott -o siek15-gradual-inc.tex -tex_name_prefix GSiek \
		-tex_filter introduction-ott1.tex introduction-ott.tex

ott : sl-grady/sl-grady.ott introduction-ott.tex gradual-typing-ott.tex core-grady-ott.tex \
	categorical-model-ott.tex proofs-ott.tex sl-grady-ott.tex \
	Makefile $(References) categorical-interp-ott.tex

$(PDF) : main.tex sl-grady/sl-grady.ott introduction-ott.tex gradual-typing-ott.tex \
	categorical-model-ott.tex categorical-interp-ott.tex proofs-ott.tex sl-grady-ott.tex  \
	Makefile $(References) conclusion.tex
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(BIBTEX) $(TexFileName)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

quick-pdf : $(OTTOutputFile) Makefile $(References)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex *-ott1.tex
