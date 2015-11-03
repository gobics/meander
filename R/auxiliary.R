get.rds.file.descriptor <- function(file.name)
{
    file.path(RDS_PATH, paste(c(file.name, RDS_FILE_EXTENSION), collapse = EXTENSION_SEPERATOR))
}
