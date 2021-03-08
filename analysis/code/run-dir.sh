echo "###################"
echo "run analysis directory"
echo "###################"

Rscript --vanilla "analysis/code/tabulate-tab1.R"
Rscript --vanilla "analysis/code/tabulate-tab2.R"
Rscript --vanilla "analysis/code/tabulate-tab3.R"
Rscript --vanilla "analysis/code/tabulate-tab4.R"

