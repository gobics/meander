# THIS FILE IS INTENDED TO STORE GLOBAL CONSTANTS NEEDED THROUGHOUT THE HOLE PROGRAMM
# SUCH AS BUT NOT LIMITED TO PATH NAMES DEFAULT FILE EXTENSIONS AND COLOR DESCRIPTORS
# DATE: 2015-11-01

# ******************************************************************************
# BEGIN     :   GENERAL       
# ******************************************************************************

APPLICATION_TITLE = 'MeandeR'
MEANDER_VERSION = '1.0.0'
LICENCE_FILE = file.path( system.file( package = getPackageName() ), 'licence.txt' )
NCBI_TAX_URL = 'http://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id='

# ******************************************************************************
# > END     :   GENERAL       
# ******************************************************************************

# ******************************************************************************
# BEGIN     :   FILE CONSTANTS
# ******************************************************************************

EXTENSION_SEPERATOR = '.'

HELP.DIR = file.path(system.file(package=getPackageName()))

HELP.FILE.INTERFACE = file.path(HELP.DIR,'MeandeR_help.html')



DATA_DIR_NAME = 'data'
DATA_PATH = file.path(system.file(package=getPackageName()), DATA_DIR_NAME)


HTML_TEMPLATE_DIR_NAME = 'template'
HTML_TEMPLATE_PATH = file.path(system.file(package=getPackageName()), HTML_TEMPLATE_DIR_NAME)
HTML_RESULT_TEMPLATE = file.path(HTML_TEMPLATE_PATH, 'RESULTS.html')

HTML_SUPPLY_DIR_NAME = 'supply'
HTML_SUPPLY_PATH = file.path(system.file(package = getPackageName()), HTML_SUPPLY_DIR_NAME)

ICON_DIR_NAME = 'icon'
ICON_PATH = file.path(system.file(package=getPackageName()), ICON_DIR_NAME)

RDS_PATH = DATA_PATH
RDS_FILE_EXTENSION = 'Rds'

HTML_OUTPUT_SUBDIR = 'html'
ORTHOLOG_TABLE_SUBDIR	= 'orthologs'
PATHWAY_TABLE_SUBDIR	= 'pathways'

ORTHOLOG_TABLE_PATH = file.path(HTML_OUTPUT_SUBDIR, ORTHOLOG_TABLE_SUBDIR)
PATHWAY_TABLE_PATH = file.path(HTML_OUTPUT_SUBDIR, PATHWAY_TABLE_SUBDIR)

SVG_OUTPUT_SUBDIR = 'svg'

HOME_DIR = path.expand('~')
CONFIG_FILE_NAME = 'meander.conf'
CONFIG_DIRECTORY_NAME = '.meander'
CONFIG_DIRECTORY = file.path(HOME_DIR, CONFIG_DIRECTORY_NAME)
CONFIG_FILE = file.path(CONFIG_DIRECTORY, CONFIG_FILE_NAME)
CONFIG_TEMP_FILE = file.path(system.file(package=getPackageName()), CONFIG_FILE_NAME)
CONFIG_FILE_HASH = toupper('02ea73642806879826c9bc796812ad7a34fd55ef20d3be6af93884f041838163f03fdfa49d78d318d628349505878b468f0c894c92d09290081d49829abc42db')
CONFIG_FILE_COMMENT = '#'
CONFIG_FILE_ENTRY_PATTERN = '\\[(.+)\\][[:space:]]+(.*)'
CONFIG_FILE_ENTRY_FORMATTER = '[%s] %s'

LEFT_FRAME = 'TABLE_FRAME'
RIGHT_FRAME = 'MAP_FRAME'
KEY_FRAME = 'KEY_FRAME'
NEW_FRAME = '_blank'

OVERVIEW_TITLE = 'ALL_PATHWAYS'

TAX_SELECT_HELP_FILE = file.path( system.file( package = getPackageName() ), 'tax_select_help.txt' )
# ******************************************************************************
# > END     :   FILE CONSTANTS
# ******************************************************************************

# ******************************************************************************
# BEGIN     :   COLORS
# ******************************************************************************

# ORTHOLOG COLOR FLAGS
SHOULD_MAP_FLAG = 1
MAPPED_FLAG = 2
UP_REGULATED_FLAG = 4
SIGNIFICANT_FLAG = 8
COLOR_BLIND_FLAG = 16

COLOR_OFFSET = 1

DEFAULT_COLOR = '#808080'

ORTHOLOG_COLORS = c(
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#BBE000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#E08000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#00DB30',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#DB0030',
#COLOR BLIND MODE
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#BBE000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#00DB30',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#E08000',
    DEFAULT_COLOR,
    '#9080F0',
    '#80B0F0',
    '#DB0030'
)


SIGNIFICANT_UP_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[16]
INSIGNIFICANT_UP_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[8]
SIGNIFICANT_DOWN_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[12]
INSIGNIFICANT_DOWN_REGULATED_DEFAULT_COLOR = ORTHOLOG_COLORS[4]
MAPPED_AND_SHOULD_NOT_DEFAULT_COLOR = ORTHOLOG_COLORS[3]
NOT_MAPPED_AND_SHOULD_DEFAULT_COLOR = ORTHOLOG_COLORS[2]
NOT_MAPPED_AND_SHOULD_NOT_DEFAULT_COLOR = ORTHOLOG_COLORS[1]

SIGNIFICANT_UP_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[32] 
INSIGNIFICANT_UP_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[24]
SIGNIFICANT_DOWN_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[28]
INSIGNIFICANT_DOWN_REGULATED_COLORBLIND_COLOR = ORTHOLOG_COLORS[20]
MAPPED_AND_SHOULD_NOT_COLORBLIND_COLOR = ORTHOLOG_COLORS[19]
NOT_MAPPED_AND_SHOULD_COLORBLIND_COLOR = ORTHOLOG_COLORS[18]
NOT_MAPPED_AND_SHOULD_NOT_COLORBLIND_COLOR = ORTHOLOG_COLORS[17]

# ******************************************************************************
# > END     :   COLORS
# ******************************************************************************

# ******************************************************************************
# BEGIN     :   TAXONOMY
# ******************************************************************************

TAX_ROOT_ID = -1
MAX_RANK = 8

# ******************************************************************************
# > END     :   TAXONOMY
# ******************************************************************************

# ******************************************************************************
# BEGIN     :   %TEMPLATE%
# ******************************************************************************

# ******************************************************************************
# > END     :   %TEMPLATE%
# ******************************************************************************
