# THIS FILE IS INTENDED TO STORE GLOBAL CONSTANTS NEEDED THROUGHOUT THE HOLE PROGRAMM
# SUCH AS BUT NOT LIMITED TO PATH NAMES DEFAULT FILE EXTENSIONS AND COLOR DESCRIPTORS
# DATE: 2015-11-01

# ******************************************************************************
# BEGIN     :   FILE CONSTANTS
# ******************************************************************************

EXTENSION_SEPERATOR = '.'

DATA_DIR_NAME = 'data'
DATA_PATH = file.path(system.file(package=getPackageName()), DATA_DIR_NAME)

HTML_TEMPLATE_DIR_NAME = 'template'
HTML_TEMPLATE_PATH = file.path(system.file(package=getPackageName()), HTML_TEMPLATE_DIR_NAME)

HTML_SUPPLY_DIR_NAME = 'supply'
HTML_SUPPLY_PATH = file.path(system.file(package = getPackageName()), HTML_SUPPLY_DIR_NAME)

ICON_DIR_NAME = 'icon'
ICON_PATH = file.path(system.file(package=getPackageName()), ICON_DIR_NAME)

RDS_PATH = DATA_PATH
RDS_FILE_EXTENSION = 'Rds'

# ******************************************************************************
# END     :   FILE CONSTANTS
# ******************************************************************************
