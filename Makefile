run:
	Rscript src/update.R

historical:
	Rscript src/update.R --historical

update:
	git pull

upload:
	aws s3 cp --recursive out s3://data.vis4.net/dwd/ --acl public-read