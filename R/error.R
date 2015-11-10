



# GIVEN AN OBJECT `obj` THAT WAS CREATED BY `obj = setRefClass(...)`
# THE FOLLWING WOULD BE RETURNED BY `class(obj)`
REFERENCE_CLASS.NAME_STRING = 'refObjectGenerator'

ERROR_CLASS.NAME_STRING = 'ERROR'

WARNING_CLASS.NAME_STRING = 'WARNING'

ERROR.NAME_ID_DELIMITER = ':'

ERROR.ID_PREFIX = 'ID_'

ERROR.REGEX_VALID_ERROR_STRING = bindStrings(
    '^[[:alpha:]\\.]+[[:alnum:]_\\.]*', ERROR.NAME_ID_DELIMITER,
    ERROR.ID_PREFIX,
    '[[:digit:]]+$'
)

ERROR.STACK_POINTER = as.integer(0)

ERROR.STACK = list()

# ************************************************************************************************** 
#   STATIC FIELDS AND METHODS
# ************************************************************************************************** 

handle.Error <- function(errorString)
{
    error = extract.ErrorFeatures(errorString$message)

    
    if (is.ErrorObject(error$Name) && is.ValidError(error$ID))
        handle.ErrorObject(error$ID)
    else
        stop(errorString)
}

extract.ErrorFeatures <- function(errorString)
{
    if (is.ValidErrorString(errorString))
        error = split.ErrorString(errorString)
    else
        error = list('', '')
    
    names(error) = c('Name', 'ID')
   
    error
}

is.ValidErrorString <- function(errorString)
{
    grepl(ERROR.REGEX_VALID_ERROR_STRING, errorString)
}
    
split.ErrorString <- function(errorString)
{
    as.list(unlist(
        strsplit(
                errorString,
                ERROR.NAME_ID_DELIMITER
        )
    ))
}

is.ErrorObject <- function(error)
{
    (nchar(error) > 0) &&
    exists(error) &&
    is.ReferenceClassObject(error) &&
    (is.ErrorClassObject(error) || is.ErrorSubclassObject(error))
}

is.ValidError <- function(error)
{
    
    error %in% names(ERROR.STACK)
}

is.ReferenceClassObject <- function(error)
{
    class(get(error))[1] == REFERENCE_CLASS.NAME_STRING
}

is.ErrorClassObject <- function(error)
{
    ERROR_CLASS.NAME_STRING == get(error)@className[1]
}

is.ErrorSubclassObject <- function(error)
{
    ERROR_CLASS.NAME_STRING %in% names(get(error)@generator$def@contains)   
}

handle.ErrorObject <- function(error)
{
    error = restore.ErrorObject(error)

    error$handle()
}

restore.ErrorObject <- function(errorID)
{
    error = ERROR.STACK[errorID][[1]]
    
    ERROR.STACK[errorID] <<- NULL
    
    error
}

obtain.IDString <- function(ID)
{
    bindStrings(ERROR.ID_PREFIX, ID)
}

# ************************************************************************************************** 
#   METHODS DEFINITIONS
# ************************************************************************************************** 

ERROR.methodDefinition_handle <- function() 
{
    print(
        toupper('The Answer to the Great Question of Life, the Universe and Everything ...')
    )
}

ERROR.methodDefinition_throw <- function()
{
    .self$addTo.ErrorStack()
    
    stop(.self$obtain.formattedThrowString())
}

ERROR.methodDefinition_addTo.ErrorStack <- function()
{
    ERROR.STACK[obtain.IDString(.self$ID)] <<- .self
}

ERROR.methodDefinition_obtain.formattedThrowString <- function()
{
    bindStrings(.self$get.ClassName(), ERROR.NAME_ID_DELIMITER, obtain.IDString(.self$ID))
}

StatusMessage.methodDefinition_initialize <- function(message = '', description = '', ...)
{
   initFields(message = message, description = description, ...)
}

ERROR.methodDefinition_initialize <- function(message = '', description = 'ERROR', ...)
{
    callSuper(message, description, ...)
     
    # INCREASE STACK POINTER
    ERROR.STACK_POINTER <<- as.integer(ERROR.STACK_POINTER + 1)

    initFields(ID = ERROR.STACK_POINTER)
}

# DUMMY-STUB
FATAL_ERROR.methodDefinition_handle <- function()
{
    stop(get.ClassName())
}

NOTIFICATION.methodDefinition_issue <- function()
{
    cat(
        sprintf('%s\n%s\n', toupper(description), message)
        )
}

WARNING.methodDefinition_initialize <- function(message = '', description = 'WARNING', ...)
{
    callSuper(message = message, description = description, ...)
}
# ************************************************************************************************** 
#   CLASS DEFINITIONS
# ************************************************************************************************** 

StatusMessage = setRefClass(
    'StatusMessage',
    
    fields = list(
        description = 'character',
        message     = 'character'
        ),
    
    methods = list(
        get.ClassName = function() {.self$getRefClass()@className[1]},
        initialize = StatusMessage.methodDefinition_initialize
        )
)

ERROR = setRefClass(
    ERROR_CLASS.NAME_STRING,
    
    contains = 'StatusMessage',
    
    fields = list(
        ID = 'integer'
        ),
    
    methods = list(
       handle = ERROR.methodDefinition_handle,
       throw  = ERROR.methodDefinition_throw,
       obtain.formattedThrowString = ERROR.methodDefinition_obtain.formattedThrowString,
       initialize = ERROR.methodDefinition_initialize,
       addTo.ErrorStack = ERROR.methodDefinition_addTo.ErrorStack
        )
)

FATAL_ERROR = setRefClass(
    'FATAL_ERROR',
    
    contains = ERROR_CLASS.NAME_STRING,
    
    methods = list(
       handle = FATAL_ERROR.methodDefinition_handle
        )
)

NOTIFICATION = setRefClass(
    'NOTIFICATION',
    
    contains = 'StatusMessage',
    
    methods = list(
        issue = NOTIFICATION.methodDefinition_issue
        )
)

WARNING = setRefClass(
    'WARNING',
    
    contains = 'NOTIFICATION',
    
    methods = list(
        initialize = WARNING.methodDefinition_initialize
        )
)

# ************************************************************************************************** 
#   WRAPPER FUNCTIONS
# ************************************************************************************************** 

attemptExecution <- function(expr, nof.Attempts = 1)
{
    index.Attempt = 0
    success = F
   
    suppressWarnings( 
    while(!success && index.Attempt < nof.Attempts) 
    {
        index.Attempt = index.Attempt + 1 
        
        tryCatch(
            {
                eval(expr = parse(text = expr))
                success = T
            },
            error = handle.Error
        )
    })
}
