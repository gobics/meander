

# GIVEN AN OBJECT `obj` THAT WAS CREATED BY `obj = setRefClass(...)`
# THE FOLLWING WOULD BE RETURNED BY `class(obj)`
REFERENCE_CLASS.NAME_STRING = 'refObjectGenerator'

ERROR_CLASS.NAME_STRING = 'ERROR'

WARNING_CLASS.NAME_STRING = 'WARNING'

ERROR.NAME_ID_DELIMITER = ':'

ERROR.ID_PREFIX = 'ID_'

ERROR.CALLING_FRAME_OFFSET = 11

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
    print(.self$get.ClassName())
    print(.self$description)
    print(.self$message)
#    print(.self$variables)
    lapply(.self$callStack, print) 
    
    
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

    #print(sys.nframe())
    
#     if (sys.nframe() > ERROR.CALLING_FRAME_OFFSET + 1)
#     {
#       callingFrame = sys.frame(- ERROR.CALLING_FRAME_OFFSET - 1)
#       
#       varNames = ls(envir = callingFrame)
#       
#       variables <<- lapply(varNames, get, envir = callingFrame)
#       names(variables) <<- varNames
#       
#       callStack <<- obtain.callStack(sys.nframe() - ERROR.CALLING_FRAME_OFFSET)
#     }
        
    # INCREASE STACK POINTER
    ERROR.STACK_POINTER <<- as.integer(ERROR.STACK_POINTER + 1)

    initFields(ID = ERROR.STACK_POINTER)
}

ERROR.methodDefinition_obtain.callStack <- function(peak)
{
    lapply(
        1 : peak,
        sys.call
        )
}

# DUMMY-STUB
FATAL_ERROR.methodDefinition_handle <- function()
{
    stop(.self$get.ClassName())
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

MY_WARNING.methodDefinition_initialize <- function(samples, message = '', description = 'WARNING', ...)
{
  #message = sprintf('%s %d\n',message,number)
  callSuper(message = sprintf(WARNING_MESSAGE_NOREPLACEMENT,message), description = description, ...)
}

MY_WARNING2.methodDefinition_initialize <- function(message = '', number = 4, description = 'WARNING', ...)
{
  message <<- sprintf('%s %d\n',message,number)
#  tk_messageBox(type = "ok",
#                message = ret, caption = "", default = "", ...)
}

WARNING.methodDefinition_issue <- function(TKFLAG = FALSE)
{
  callSuper()
    if (TKFLAG)
    {
      tk_messageBox(type = "ok", message = message, caption = "", default = "")
    }
}


WARNING_DIFFERENT_METHOD.methodDefinition_initialize <- function(rep.method, ori.method, message = '', description = 'WARNING', ...)
{
  #message = sprintf('%s %d\n',message,number)
  callSuper(message = sprintf(WARNING_MESSAGE_DIFFERENTMETHOD,rep.method,ori.method), description = description, ...)
}


WARNING_NO_REPLACEMENT.methodDefinition_initialize <- function(message = '', description = 'WARNING', ...)
{
  #message = sprintf('%s %d\n',message,number)
  callSuper(message = sprintf(WARNING_MESSAGE_NOREPLACEMENT,message), description = description, ...)
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
        ID = 'integer',
        variables = 'list',
        callStack = 'list'
        ),
    
    methods = list(
       handle = ERROR.methodDefinition_handle,
       throw  = ERROR.methodDefinition_throw,
       obtain.formattedThrowString = ERROR.methodDefinition_obtain.formattedThrowString,
       initialize = ERROR.methodDefinition_initialize,
       addTo.ErrorStack = ERROR.methodDefinition_addTo.ErrorStack,
       obtain.callStack = ERROR.methodDefinition_obtain.callStack
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
        initialize = WARNING.methodDefinition_initialize,
        issue = WARNING.methodDefinition_issue
        )
)

MY_WARNING = setRefClass(
  'MY_WARNING',
  
  contains = 'WARNING',
  
  methods = list(
    initialize = MY_WARNING.methodDefinition_initialize
  )
)

MY_WARNING2 = setRefClass(
  'MY_WARNIN2G',
  
  contains = 'WARNING',
  
  methods = list(
    initialize = MY_WARNING2.methodDefinition_initialize
  )
)

WARNING_DIFFERENT_METHOD = setRefClass(
  'WARNING_DIFFERENT_METHOD',
  
  contains = 'WARNING',
  
  methods = list(
    initialize = WARNING_DIFFERENT_METHOD.methodDefinition_initialize
  )
)

WARNING_NO_REPLACEMENT = setRefClass(
  'WARNING_NO_REPLACEMENT',
  
  contains = 'WARNING',
  
  methods = list(
    initialize = WARNING_NO_REPLACEMENT.methodDefinition_initialize
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
                expr
                success = TRUE
            },
            error = handle.Error
        )
    })
}
