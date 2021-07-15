export PROJECT_PATH="/Users/nadir/Desktop/projects/postdoc/DATABASES/databases"
cd $PROJECT_PATH;
Rscript core/00_common/src/main.R --db_name=03_p53_eortc --class_name=Mappingp53_eortc --mapping --preprocessing
Rscript core/00_common/src/main_plot.R --db_name=03_p53_eortc -p
# !is.na(pCR) & !is.na(trt_arm) & totrt==6 & is.na(prog_trt_inelig) & is.na(miss_surg_info) & metstat==0 & is.na(surg_inelig)