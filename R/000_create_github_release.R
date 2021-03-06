# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Create a github release with the piggyback package. Using this
# package, I can upload data to an existing release.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

library(piggyback)

pb_releases()

# Create a zip file with all the data
dir_path <- tempdir()
tmpfile <- paste0(dir_path, "/data.zip")

zip(tmpfile, fs::dir_ls("data/", recurse = TRUE, type = "file"))
zip(tmpfile, here::here("manuscript", "essd", "nunataryuk_data_paper.zip"))

# Create a new release and upload the data zip file
pb_new_release(tag = "v0.0.3")
pb_upload(file = tmpfile, overwrite = TRUE, tag = "v0.0.3")

unlink(tmpfile)
