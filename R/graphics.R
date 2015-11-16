# **************************************************************************************************
#   CONSTANTS
# **************************************************************************************************

MINIMUM_SCREEN_WIDTH  = 1024
MINIMUM_SCREEN_HEIGHT = 768

MINIMUM_SCREEN_DIMENSION = c(MINIMUM_SCREEN_WIDTH, MINIMUM_SCREEN_HEIGHT)

DIMENSION_NAMES = c('width', 'height')

TCLTK.DIMENSION_SEPARATOR = 'x'
TCLTK.POSITION_SEPARATOR = '+'

TCLTK.REQUIRED_VERSION = '8.5'

# **************************************************************************************************
#   GLOBAL VARIABLES
# **************************************************************************************************

WINDOW_DECORATION_SIZE = c(0, 0)

# **************************************************************************************************
#   METHOD DEFINITIONS
# **************************************************************************************************

# -------------------------------------------------------------------------------------------------- 
#   BEGIN CLASS: GraphicalWindow
# -------------------------------------------------------------------------------------------------- 

GraphicalWindow.methodDefinition_determine.Window.InnerDimension <- function()
{
    dim - WINDOW_DECORATION_SIZE
}

GraphicalWindow.methodDefinition_determine.Window.CenteredPosition <- function()
{
    floor((determine.Screen.Dimension() - dim) / 2)
}

GraphicalWindow.methodDefinition_create <- function(...)
{
    geometry = parse.TCLTK.Geometry(determine.Window.CenteredPosition(), dim)
    
    tkwm.geometry(
        windowHandle,
        geometry
        )
}

GraphicalWindow.methodDefinition_init <- function(...)
{
    # ABSTRACT METHOD STUB    
}

GraphicalWindow.methodDefinition_setVisibility <- function(visible)
{
    if (visible)
        tkwm.deiconify(windowHandle)
    else
        tkwm.withdraw(windowHandle)
}

GraphicalWindow.methodDefinition_disable <- function()
{
    
}

GraphicalWindow.methodDefinition_initialize <- function(width = MINIMUM_SCREEN_WIDTH, height = MINIMUM_SCREEN_HEIGHT, visible = TRUE, ...)
{
    tclServiceMode(F)
    
    windowHandle <<- tktoplevel()$ID
  
    setVisibility(FALSE)
    
    dim <<- as.integer(c(width, height))
    names(dim) <<- DIMENSION_NAMES
   
    create(...)
    
    init(...)
    
    setVisibility(visible)
    
    tclServiceMode(T)
}

# -------------------------------------------------------------------------------------------------- 
#   END CLASS: GraphicalWindow
# -------------------------------------------------------------------------------------------------- 

# -------------------------------------------------------------------------------------------------- 
#   BEGIN CLASS: MainWindow
# -------------------------------------------------------------------------------------------------- 

MainWindow.methodDefinition_initialize <- function(title = '')
{
    callSuper(title = title)
}

MainWindow.methodDefinition_create <- function(title = '')
{
    callSuper()
    
    tkwm.title(windowHandle, title)
}

# -------------------------------------------------------------------------------------------------- 
#   END CLASS: MainWindow
# -------------------------------------------------------------------------------------------------- 

# **************************************************************************************************
#   CLASS DEFINITIONS 
# **************************************************************************************************

GraphicalWindow <- setRefClass(
    'GraphicalWindow',
    
    fields = list(
        windowHandle = 'character',
        dim = 'integer'
        ),
    
    methods = list(
        create     = GraphicalWindow.methodDefinition_create,
        init       = GraphicalWindow.methodDefinition_init,
        setVisibility = GraphicalWindow.methodDefinition_setVisibility,
        disable    = GraphicalWindow.methodDefinition_disable,
        determine.Window.InnerDimension = GraphicalWindow.methodDefinition_determine.Window.InnerDimension,
        determine.Window.CenteredPosition = GraphicalWindow.methodDefinition_determine.Window.CenteredPosition,
        initialize  = GraphicalWindow.methodDefinition_initialize
        )
)

MainWindow <- setRefClass(
    'MainWindow',
    
    contains = 'GraphicalWindow',
    
    fields = list(
        title = 'character'
        ),
    
    methods = list(
        create = MainWindow.methodDefinition_create,
        initialize = MainWindow.methodDefinition_initialize
    )
)

# **************************************************************************************************
#   AUXILIARY FUNCTIONS AND STATIC METHODS
# **************************************************************************************************

determine.WindowDecoration.Size <- function()
{
    testWindow = tktoplevel()
    
    coordinateString = as.character(tkwm.geometry(testWindow))
    
    innerPos = as.integer(strsplit(coordinateString, TCLTK.POSITION_SEPARATOR, fixed = T)[[1]][2:3])
    
    outerPos = c(as.integer(tkwinfo('rootx', testWindow)), as.integer(tkwinfo('rooty', testWindow)))

    tkdestroy(testWindow)
    
    outerPos - innerPos
}    

parse.TCLTK.Geometry <- function(position, dimension)
{
    bindStrings(
        dimension[1],
        TCLTK.DIMENSION_SEPARATOR,
        dimension[2],
        TCLTK.POSITION_SEPARATOR,
        position[1],
        TCLTK.POSITION_SEPARATOR,
        position[2]
        )
}
    
determine.Screen.Dimension <- function()
{
   c(
        as.integer(tkwinfo('screenwidth', '.')),
        as.integer(tkwinfo('screenheight', '.'))
        ) 
}

has.GUI_Support <- function()
{
    has.required.TCLTK.Version()
}

init.GUI <- function()
{
    if(!has.GUI_Support())
        ERROR$new('Graphical User Interface is not supported')$throw()
    
    WINDOW_DECORATION_SIZE <<- determine.WindowDecoration.Size()
}

start.GUI <- function()
{
    mw <- MainWindow$new(APPLICATION_TITLE)
}
# **************************************************************************************************
#   WRAPPER FUNCTIONS
# **************************************************************************************************

meandeR.frontend <- function()
{
    attemptExecution({
        init.GUI()  
        start.GUI()
        })
}
