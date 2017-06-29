# Comments that begin with ## will be shown from target help

## This is template about how to use make automate the workflow of R project

# Make sure that Rscript.exe and pandoc are in your path (see Readmd.md for directions)

.PHONY: list help all
help : 
	@echo "Showing Help Comments:"
	@echo
	@sed -n 's/^##//p' makefile
	@printf "\nList of all targets: "
	@$(MAKE) -s list

# List targets (http://stackoverflow.com/a/26339924/3429373)
list:
	@$(MAKE) -pRrq -f $(lastword $(MAKEFILE_LIST)) : 2>/dev/null | awk -v RS= -F: '/^# File/,/^# Finished Make data base/ {if ($$1 !~ "^[#.]") {print $$1}}' | sort | egrep -v -e '^[^[:alnum:]]' -e '^$@$$' | xargs

colleges.RData: colleges.R
	Rscript.exe colleges.R

HW3_Writeup.html: HW3_Writeup.Rmd colleges.RData
	Rscript.exe -e "rmarkdown::render('HW3_Writeup.Rmd')"

all: HW3_Writeup.html