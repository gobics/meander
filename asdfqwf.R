# how far do we have to go to access the calling frame of the Obj$new method
INIT_OBJECT_CALL_DEPTH = 4

retry <- function(a, b)
{
    a + b
}

handler <- function(fun.name, ...)
{
  
  deparse(substitute(...))
   
  #do.call(fun.name, list(...))
}

Exception = setRefClass(
    'Throwable',
    fields = list(
        description = 'character', 
        message     = 'character',
        log.file    = 'character'
    ),
    methods = list(
        handle = function()
        {
            write(...)
        },
        get.StackTrace.FormatString = function()
        {
            print(sys.call(-1))
            
            as.character(sys.call(-1))
            
        },
        get.Message.FormatString = function()
        {
            sprintf('%s\n%s\n%s\n', toupper(description), message)
        },
        get.VariableList.FormatString = function(var.list)
        {
            print(sapply(as.list(substitute(var.list))[-4L], deparse))
        },
        write = function(...)
        {
                cat(
                   get.StackTrace.FormatString(),
                   get.Message.FormatString(),
                   get.VariableList.FormatString(list(...)),
                   file = if (length(log.file) == 0) '' else log.file
                )
        }
        
    )
)
    
Error = setRefClass(
    'Error',
    contains = 'Throwable',
    methods = list(
        handle = function(...)
        {
            callSuper('handle', ...)
        }
    )
)

BBB = setRefClass(
    'BBB',
    fields = list(a = 'list'),
    methods = list(
        initialize = function(...)
        {
            call.frame = parent.frame(INIT_OBJECT_CALL_DEPTH)
            a <<- lapply(ls(call.frame), function(var.name) {get(var.name, envir = call.frame)})
            names(a) <<- ls(call.frame)
        }
    )
)
