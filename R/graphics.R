# **************************************************************************************************
#   CONSTANTS
# **************************************************************************************************

MINIMUM_SCREEN_WIDTH  = 1024
MINIMUM_SCREEN_HEIGHT = 768




# **************************************************************************************************
#   METHOD DEFINITIONS
# **************************************************************************************************


GraphicalWindow.methodDefinition_create <- function(...)
{
    
}

GraphicalWindow.methodDefinition_init <- function(...)
{
    
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

GraphicalWindow.methodDefinition_initialize <- function(...)
{
    print('INIT 2')
    
    
    tclServiceMode(F)
    
    windowHandle <<- tktoplevel()
    
    tkwm.withdraw(windowHandle)
   
    create(...)
    
    init(...)
     
    tkwm.deiconify(windowHandle)
    
    tclServiceMode(T)
}

MainWindow.methodDefinition_initialize <- function(title = '')
{
    print('INIT 1')
    
    callSuper(title = title)
}

MainWindow.methodDefinition_create <- function(title = '')
{
    print('INIT 3')
    
    tkwm.title(windowHandle, title)
}
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
#   AUXILIARY
# **************************************************************************************************

determine.WindowDecoration.Size <- function()
{
    testWindow = tktoplevel()
    
    coordinateString = as.character(tkwm.geometry(testWindow))
    
    innerPos = as.integer(strsplit(coordinateString, '+', fixed = T)[[1]][2:3])
    
    outerPos = c(as.integer(tkwinfo('rootx', testWindow)), as.integer(tkwinfo('rooty', testWindow)))

    outerPos - innerPos    
}    
