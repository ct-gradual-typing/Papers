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

# results-ott.tex : results.tex surface-grady/surface-grady.ott core-grady/core-grady.ott 
# 	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott  -o surface-grady-inc.tex -tex_name_prefix SGrady \
# 		-tex_filter results.tex results-ott1.tex
# 	scripts/prepr_double_ott.sh results-ott1.tex
# 	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott  -o core-grady-inc.tex -tex_name_prefix CGrady \
# 		-tex_filter results-ott1.tex results-ott.tex

# proofs-gradual-guarantee-part-one-ott.tex : proofs-gradual-guarantee-part-one.tex surface-grady/surface-grady.ott \
# 			     		    core-grady/core-grady.ott
# 	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott -o surface-grady-inc.tex -tex_name_prefix SGrady \
# 		-tex_filter proofs-gradual-guarantee-part-one.tex proofs-gradual-guarantee-part-one-ott1.tex
# 	scripts/prepr_double_ott.sh proofs-gradual-guarantee-part-one-ott1.tex
# 		@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott -o core-grady-inc.tex -tex_name_prefix CGrady \
# 		-tex_filter proofs-gradual-guarantee-part-one-ott1.tex proofs-gradual-guarantee-part-one-ott.tex

surface-grady-ott.tex : surface-grady.tex surface-grady/surface-grady.ott
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott  -o surface-grady-inc.tex -tex_name_prefix SGrady \
		-tex_filter surface-grady.tex surface-grady-ott1.tex
	scripts/prepr_double_ott.sh surface-grady-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter surface-grady-ott1.tex surface-grady-ott.tex

core-grady-ott.tex : core-grady.tex core-grady/core-grady.ott surface-grady/surface-grady.ott
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott  -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter core-grady.tex core-grady-ott1.tex
	scripts/prepr_double_ott.sh core-grady-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott -o surface-grady-inc.tex -tex_name_prefix SGrady \
	-tex_filter core-grady-ott1.tex core-grady-ott.tex

categorical-model-ott.tex : categorical-model.tex core-grady/core-grady.ott surface-grady/surface-grady.ott
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott  -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter categorical-model.tex categorical-model-ott1.tex
	scripts/prepr_double_ott.sh categorical-model-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott -o surface-grady-inc.tex -tex_name_prefix SGrady \
	-tex_filter categorical-model-ott1.tex categorical-model-ott.tex

gradual-guarantee-ott.tex : gradual-guarantee.tex core-grady/core-grady.ott surface-grady/surface-grady.ott
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott -o surface-grady-inc.tex -tex_name_prefix SGrady \
	-tex_filter gradual-guarantee.tex gradual-guarantee-ott1.tex
	scripts/prepr_double_ott.sh gradual-guarantee-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott  -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter gradual-guarantee-ott1.tex gradual-guarantee-ott.tex

bounded-ott.tex : bounded.tex core-grady/core-grady.ott surface-grady/surface-grady.ott
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott  -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter bounded.tex bounded-ott1.tex
	scripts/prepr_double_ott.sh bounded-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott -o surface-grady-inc.tex -tex_name_prefix SGrady \
	-tex_filter bounded-ott1.tex bounded-ott.tex

proofs-ott.tex : proofs.tex surface-grady/surface-grady.ott core-grady/core-grady.ott 
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott  -o surface-grady-inc.tex -tex_name_prefix SGrady \
		-tex_filter proofs.tex proofs-ott1.tex
	scripts/prepr_double_ott.sh proofs-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter proofs-ott1.tex proofs-ott.tex

aux-results-ott.tex : aux-results.tex surface-grady/surface-grady.ott core-grady/core-grady.ott 
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott  -o surface-grady-inc.tex -tex_name_prefix SGrady \
		-tex_filter aux-results.tex aux-results-ott1.tex
	scripts/prepr_double_ott.sh aux-results-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter aux-results-ott1.tex aux-results-ott.tex

# aux-results-proofs-ott.tex : aux-results-proofs.tex surface-grady/surface-grady.ott core-grady/core-grady.ott 
# 	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott -o surface-grady-inc.tex -tex_name_prefix SGrady \
# 		-tex_filter aux-results-proofs.tex aux-results-proofs-ott1.tex
# 	scripts/prepr_double_ott.sh aux-results-proofs-ott1.tex
# 	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott -o core-grady-inc.tex -tex_name_prefix CGrady \
# 		-tex_filter aux-results-proofs-ott1.tex aux-results-proofs-ott.tex

introduction-ott.tex : introduction.tex core-grady/core-grady.ott surface-grady/surface-grady.ott
	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott  -o surface-grady-inc.tex -tex_name_prefix SGrady \
		-tex_filter introduction.tex introduction-ott1.tex
	scripts/prepr_double_ott.sh introduction-ott1.tex
	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott -o core-grady-inc.tex -tex_name_prefix CGrady \
		-tex_filter introduction-ott1.tex introduction-ott.tex

# related-work-ott.tex : related-work.tex core-grady/core-grady.ott surface-grady/surface-grady.ott
# 	@$(OTT) $(OTT_FLAGS) -i surface-grady/surface-grady.ott  -o surface-grady-inc.tex -tex_name_prefix SGrady \
# 		-tex_filter related-work.tex related-work-ott1.tex
# 	scripts/prepr_double_ott.sh related-work-ott1.tex
# 	@$(OTT) $(OTT_FLAGS) -i core-grady/core-grady.ott -o core-grady-inc.tex -tex_name_prefix CGrady \
# 		-tex_filter related-work-ott1.tex related-work-ott.tex

ott : main.tex introduction-ott.tex core-grady-ott.tex \
      surface-grady-ott.tex gradual-guarantee-ott.tex bounded-ott.tex categorical-model-ott.tex proofs-ott.tex aux-results-ott.tex \
      Makefile $(References)

$(PDF) : main.tex introduction-ott.tex core-grady-ott.tex \
	surface-grady-ott.tex conclusion.tex gradual-guarantee-ott.tex bounded-ott.tex categorical-model-ott.tex proofs-ott.tex aux-results-ott.tex \
	Makefile $(References)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(BIBTEX) $(TexFileName)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

quick-pdf : $(OTTOutputFile) Makefile $(References)
	$(PDFLATEX) -jobname=$(TexFileName) $(OTTOutputFile)

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg *.rel *.pdf *~ *.vtc *.out *.spl *-inc.tex *-ott1.tex
