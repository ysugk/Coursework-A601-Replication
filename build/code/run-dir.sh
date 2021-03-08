echo "###################"
echo "run build directory"
echo "###################"
Rscript --vanilla "build/code/download-wrds.R"
Rscript --vanilla "build/code/merge-fundq-ibes.R"
Rscript --vanilla "build/code/gen-event-window.R"
Rscript --vanilla "build/code/gen-daily-return.R"

cp build/output/* analysis/input/
