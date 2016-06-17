# CONSTANTS
WINDOW_TITLE =  'meandeR'
TAX_ROOT     = -1
TAX_UNDEF    =  0
RANK_NAMES   =  c(
    'Species (or lower)',
    'Genus',
    'Family',
    'Order',
    'Class',
    'Phylum',
    'Kingdom',
    'Superkingdom',
    'no Rank'
    )
MAX_RANK = length( RANK_NAMES )
MAX_DISPLAYED_TAX   = 10
TAX_CUTOFF_DEGREE   = 1
TAX_CUTOFF_RATIO    = TAX_CUTOFF_DEGREE / 360
        
DIAMETER_LARGE  = 400
RIM_WIDTH       =  25
DIAMETER_SMALL  = DIAMETER_LARGE - 2 * RIM_WIDTH
HALO_WIDTH      = 3

TAX_TAG_PREFIX = '_'

PCA_CANVAS_SIZE = 400
PCA_DOT_RADIUS    = 20
PCA_AREA_SIZE   = PCA_CANVAS_SIZE - 2 * ( PCA_DOT_RADIUS + 1 )
PCA_COLORS = sapply( ( 1 : 2 ) / 2 , hsv, s = 0.6, v = 0.9 )

# LAYOUT
LAYOUT = list(
ELEMENTS = list(
        LAYOUT_HEADER  = list( col = 0, cspan = 5, row = 0, rspan = 1, sticky = 'new'  ),
        LAYOUT_PIE_1   = list( col = 0, cspan = 1, row = 2, rspan = 1, sticky = 'nsew' ),
        LAYOUT_PIE_2   = list( col = 2, cspan = 1, row = 2, rspan = 1, sticky = 'nsew' ),
        LAYOUT_PCA     = list( col = 4, cspan = 1, row = 2, rspan = 3, sticky = 'nsew' ),
        LAYOUT_BAR_1   = list( col = 0, cspan = 1, row = 4, rspan = 1, sticky = 'sew'  ),
        LAYOUT_BAR_2   = list( col = 2, cspan = 1, row = 4, rspan = 1, sticky = 'sew'  ),
        LAYOUT_FOOTER  = list( col = 0, cspan = 3, row = 6, rspan = 1, sticky = 'sew'  ),
        LAYOUT_CONTROL = list( col = 4, cspan = 1, row = 5, rspan = 2, sticky = 'se'   ),

        FILL_A = list( col = 0, cspan = 5, row = 1, rspan = 1, sticky = 'nsew' ),
        FILL_B = list( col = 0, cspan = 1, row = 3, rspan = 1, sticky = 'nsew' ),
        FILL_C = list( col = 2, cspan = 1, row = 3, rspan = 1, sticky = 'nsew' ),
        FILL_D = list( col = 0, cspan = 3, row = 5, rspan = 1, sticky = 'nsew' ),
        FILL_E = list( col = 1, cspan = 1, row = 2, rspan = 3, sticky = 'nsew' ),
        FILL_F = list( col = 3, cspan = 1, row = 2, rspan = 5, sticky = 'nsew' )
        ),

    PADDINGS = list( X = 10, Y = 10 ),

    WEIGHTS = list( X = c( 0, 1, 0, 1, 0 ), Y = c( 0, 1, 0, 1, 0, 1, 0 ) )
    )

#GLOBALS

ELES = new.env()
DATA = new.env()
VARS = new.env()

BRIGHTNESS = list(LIGHT = 1, DARK = 0.7)

tax.Select = 
function( obj.data, obj.refined, obj.config )
{
    rm( list = ls( envir = ELES ), envir = ELES )  # container for widgets and 
    rm( list = ls( envir = DATA ), envir = DATA )  # temporary global data
    rm( list = ls( envir = VARS ), envir = VARS )  # tcl variables 

    DATA$object = prepare.Object( obj.data, obj.refined, obj.config ) 

    DATA$tax.Current = TAX_ROOT
    DATA$tax.Marked = NULL
    DATA$tax.Selected = NULL 
    VARS$tax.Final = tclVar( 'NA' )
    DATA$is.expanded = list()

    DATA$node.Stack = list()
    for ( cond in rownames( DATA$object$map ) )
        DATA$node.Stack[[ cond ]] = DATA$object$table[ sample %in% which( DATA$object$map[ cond, ] ) ]
    
    init.Tree()

    tclServiceMode( FALSE )

    main.Window = tktoplevel()
    tkwm.title( main.Window, WINDOW_TITLE )

    build.Frame( main.Window )

    tclServiceMode( TRUE )

    # busy wait here because tkwait.visibility just doesnt work sometimes
    # it is necessary to know the window size because the count bar takes 
    # the size of the pie canvas and has length zero if not displayed
    while ( tclvalue( tkwinfo ( 'ismapped', main.Window ) ) != 1 ) {}

    set.Content( DATA$tax.Current )

    override.TclVarBlock( main.Window, VARS$tax.Final, TAX_ROOT )

    tcl( 'vwait', VARS$tax.Final )

    result = tclvalue( VARS$tax.Final )

    tkdestroy( main.Window )

    result
}

build.Frame = 
function(window)
{
    frame = tkframe( window )

    create.Children( frame )

    layout.Children(frame, LAYOUT)

    tkpack( frame, fill = 'both', expand = 1 )
}

layout.Children =
function(frame, layout)
{
        layout.Matrix = rbind( as.character( tkwinfo( 'children', frame ) ), layout$ELEMENTS, layout$PADDINGS$X, layout$PADDINGS$Y )

    apply( 
        layout.Matrix,
        2,
        function(x)
        {
            tkgrid(
                x[[ 1 ]], 
                column = x[[ 2 ]]$col,
                columnspan = x[[ 2 ]]$cspan,
                row = x[[ 2 ]]$row,
                rowspan = x[[ 2 ]]$rspan,
                sticky = x[[ 2 ]]$sticky,
                padx = x[[ 3 ]],
                pady = x[[ 4 ]]
            )
        }
    )    

    sapply( 
        
        which( layout$WEIGHTS$X != 0 ),
        
        function( index )
        
            tcl( 'grid', 'columnconfigure', frame, index - 1, '-weight', layout$WEIGHTS$X[ index ] )
    )

    sapply( 
        
        which( layout$WEIGHTS$Y != 0 ),
        
        function( index )
        
            tcl( 'grid', 'rowconfigure', frame, index - 1, '-weight', layout$WEIGHTS$Y[ index ] )
    )
}

#=================================================================================================== 
# CREATE FUNCTIONS
#=================================================================================================== 

create.Children =
function(frame)
{
    create.Header( frame )

    for ( cond in rownames( DATA$object$map ) )
        create.Pie.Frame( frame, cond )

    create.PCA.Frame( frame )

    for ( cond in rownames( DATA$object$map ) )
        create.Bar.Frame( frame, cond )

    create.Footer( frame )

    create.Control( frame )

    for ( filling in 9 : 14 )
    {
        tkframe( frame )
    }
}

create.Header =
function(parent)
{
    FONT_HEADER  = tkfont.create(family = 'verdana', size = 24, weight = 'bold')
    FONT_SUB_HEADER  = tkfont.create(family = 'verdana', size = 20, weight = 'bold')

    frame = tkframe( parent )

    # Title
    VARS$header.Taxonomy = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$header.Taxonomy, font = FONT_HEADER ),
        column = 0,
        row = 0,
        sticky = 'new'
    )

    # Subtitle
    VARS$header.Rank = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$header.Rank, font = FONT_SUB_HEADER ),
        column = 0,
        row = 1,
        sticky = 'sew'
    )

    tcl( 'grid', 'columnconfigure', frame, 0, '-weight', 1 )
}

create.Footer =
function(parent)
{
    FONT_LEAD  = tkfont.create( family = 'verdana', size = 18, weight = 'bold' )
    FONT_TEXT  = tkfont.create( family = 'verdana', size = 16 )
    FONT_URL  = tkfont.create( family = 'verdana', size = 16, underline = 1 )

    frame = ttklabelframe( parent, text = 'Info')

    # NCBI TaxId LEAD
    tkgrid(
        tklabel( frame, text = 'NCBI TaxID :', font = FONT_LEAD, anchor = 'e' ),
        column = 0,
        row = 0,
        sticky = 'ew',
        padx = 10
    )
    # NCBI TaxId TEXT
    VARS$footer.TaxID = tclVar( '' )
    footer.TaxID.Label = tklabel(
        frame,
        textvariable = VARS$footer.TaxID,
        font = FONT_URL,
        anchor = 'w',
        foreground = '#3090F0' 
        )
    tkgrid(
        footer.TaxID.Label,
        column = 1,
        row = 0,
        sticky = 'ew',
        pady = 10
    )
    tkbind(
        footer.TaxID.Label,
        '<Double-1>',
        function()
        {
            uid = tclvalue( VARS$footer.TaxID ) 

            if ( uid > 0 ) browseURL( bindStrings( NCBI_TAX_URL, uid ) )
        }
    )

    # NCBI Rank LEAD
    tkgrid(
        tklabel( frame, text = 'Rank :', font = FONT_LEAD, anchor = 'e' ),
        column = 0,
        row = 1,
        sticky = 'ew',
        padx = 10
    )
    # NCBI Rank TEXT
    VARS$footer.Rank = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$footer.Rank, font = FONT_TEXT, anchor = 'w' ),
        column = 1,
        row = 1,
        sticky = 'ew',
        pady = 10
    )

    # NCBI Name LEAD
    tkgrid(
        tklabel( frame, text = 'Taxonomy :', font = FONT_LEAD, anchor = 'e' ),
        column = 0,
        row = 2,
        sticky = 'ew',
        padx = 10
    )
    # NCBI TaxId TEXT
    VARS$footer.Name = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$footer.Name, font = FONT_TEXT, anchor = 'w' ),
        column = 1,
        row = 2,
        sticky = 'ew',
        pady = 10
    )

    tcl( 'grid', 'columnconfigure', frame, 1, '-weight', 1 )
}

create.Control =
function(parent)
{
    BUTTON_TEXT = c( 'Up', 'Down', 'Proceed', 'Help' )
    BUTTON_WIDTH = max( nchar( BUTTON_TEXT )  ) + 2

    FONT_BUTTON  = tkfont.create( family = 'verdana', size = 18 )

    frame = ttklabelframe( parent, text = 'Control' )

    # Down
    down.ButtonHandler = function()
    {
        if ( ! is.null( DATA$tax.Selected ) && ( get.Rank( DATA$tax.Selected ) > 1 ) ) 
        {
            DATA$tax.Current = DATA$tax.Selected

            DATA$tax.Selected = NULL
            DATA$tax.Marked = NULL


            set.Content( DATA$tax.Current )
        }
    }
    ELES$button.Down = tkbutton(
        frame,
        text = 'Down',
        width = BUTTON_WIDTH,
        font = FONT_BUTTON,
        command = down.ButtonHandler 
        )
    tkgrid(
        ELES$button.Down,
        column = 0,
        row = 0,
        padx = 10,
        pady = 10,
        sticky = 'nsew'
    )

    # Up
    up.ButtonHandler = function()
    {
        if ( TRUE )
        {
            DATA$tax.Current = DATA$tree[[ 1 ]][ tax.id == DATA$tax.Current, parent ]

            DATA$tax.Selected = NULL
            DATA$tax.Marked = NULL

            if ( get.Rank( DATA$tax.Current ) == MAX_RANK)
                tkconfigure( ELES$button.Up, state = 'disabled' )

            set.Content( DATA$tax.Current )
        }
    }
    ELES$button.Up = tkbutton(
        frame,
        text = 'Up',
        width = BUTTON_WIDTH,
        font = FONT_BUTTON,
        command = up.ButtonHandler 
        )
    tkgrid(
        ELES$button.Up,
        column = 0,
        row = 1,
        padx = 10,
        pady = 10,
        sticky = 'nsew'
    )

    # Proceed
    proceed.ButtonHandler = function()
    {
        if ( is.null( DATA$tax.Selected ) )
            tcl( 'set', VARS$tax.Final, DATA$tax.Current )
        else 
            tcl( 'set', VARS$tax.Final, DATA$tax.Selected )
    }
    tkgrid(
        tkbutton( frame, text = 'Proceed', width = BUTTON_WIDTH, font = FONT_BUTTON, command = proceed.ButtonHandler ),
        column = 1,
        row = 0,
        padx = 10,
        pady = 10,
        sticky = 'nsew'
    )

    # Help
    help.ButtonHandler = function()
    {
        browseURL( TAX_SELECT_HELP_FILE )
    }
    tkgrid(
        tkbutton( frame, text = 'Help', width = BUTTON_WIDTH, font = FONT_BUTTON, command = help.ButtonHandler ),
        column = 1,
        row = 1,
        padx = 10,
        pady = 10,
        sticky = 'nsew'
    )
}

create.PCA.Frame =
function(parent)
{
    FONT_VARIANCE_TEXT = tkfont.create( family = 'verdana', size = 16, weight = 'bold' )
    FONT_VARIANCE_VALUE = tkfont.create( family = 'monospace', size = 16, slant  = 'italic' )
    FONT_KEY_TEXT = tkfont.create( family = 'verdana', size = 16 )


    frame = tkframe( parent )

    # PCA Canvas
    PCA_Canvas.Frame = ttklabelframe( frame, text = 'Principal Components Analysis' )

    ELES$pca.Canvas = tkcanvas( PCA_Canvas.Frame, background = '#F0F0F0', width = PCA_CANVAS_SIZE, height = PCA_CANVAS_SIZE )
    tkgrid(
        ELES$pca.Canvas,
        column = 0,
        row = 0,
        columnspan = 3,
        sticky = 'new',
        padx = 5
    )

    tkgrid(
        tklabel( PCA_Canvas.Frame, text = 'Variance :', font = FONT_VARIANCE_TEXT ),
        column = 0,
        row = 1,
        sticky = 'sw',
        pady = 5
    )
    
    VARS$pca.Var_1 = tclVar( '' )
    tkgrid(
        tklabel( PCA_Canvas.Frame, textvariable = VARS$pca.Var_1, font = FONT_VARIANCE_VALUE, anchor = 'e' ),
        column = 1,
        row = 1,
        sticky = 'se',
        pady = 5
    )

    VARS$pca.Var_2 = tclVar( '' )
    tkgrid(
        tklabel( PCA_Canvas.Frame, textvariable = VARS$pca.Var_2, font = FONT_VARIANCE_VALUE, anchor = 'e' ),
        column = 2,
        row = 1,
        sticky = 'se',
        pady = 5
    )

    tkgrid(
        PCA_Canvas.Frame,
        column = 0,
        row = 0,
        stick = 'new'
    )

    # PCA Key
    PCA_Key.Frame = ttklabelframe( frame, text = 'PCA Key' )

    extended.Cond = c( rownames( DATA$object$map ), 'ignore' )

    type = c( 1 : nrow( DATA$object$map ), NA )

    for ( idx in 1 : length( type ) )
    {
        canvas = tkcanvas( PCA_Key.Frame, width = 21, height = 21 )
        tkgrid(
            canvas,
            column = 0,
            row = idx - 1,
            sticky = 'e',
            padx = 5,
            pady = 5
        )
        draw.Point( canvas, 11, 11, type[ idx ], '' )

        tkgrid(
            tklabel( PCA_Key.Frame, text = extended.Cond[ idx ], font = FONT_KEY_TEXT, anchor = 'w' ),
            column = 1,
            row = idx - 1,
            sticky = 'w',
            padx = 5,
            pady = 5
        )
    }

    tkgrid(
        PCA_Key.Frame,
        column = 0,
        row = 1,
        stick = 'sew'
    )
}

create.Bar.Frame =
function(parent, cond)
{
    FONT_LABEL_TEXT = tkfont.create( family = 'verdana', size = 14 )

    BAR_WIDTH = 250
    BAR_HEIGHT = 30

    frame = ttklabelframe( parent, text = bindStrings( 'Count Ratio ', '[', cond, ']' ) )
    
    if ( cond == 1 )
    {
        VARS$bar.Count_marked  = list()
        VARS$bar.Count_current = list()
        VARS$bar.Perc_marked   = list()
        VARS$bar.Perc_current  = list()

        ELES$bar.Canvas       = list()
    }

    VARS$bar.Count_marked[[ cond ]] = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$bar.Count_marked[[ cond ]], font = FONT_LABEL_TEXT ),
        row = 0,
        column = 0,
        sticky = 'nw',
        padx = 5,
        pady = 5
    )

    VARS$bar.Count_current[[ cond ]] = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$bar.Count_current[[ cond ]], font = FONT_LABEL_TEXT ),
        row = 0,
        column = 1,
        sticky = 'ne',
        padx = 5,
        pady = 5
    )

    VARS$bar.Perc_marked[[ cond ]] = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$bar.Perc_marked[[ cond ]], font = FONT_LABEL_TEXT ),
        row = 2,
        column = 0,
        sticky = 'sw',
        padx = 5,
        pady = 5
    )

    VARS$bar.Perc_current[[ cond ]] = tclVar( '' )
    tkgrid(
        tklabel( frame, textvariable = VARS$bar.Perc_current[[ cond ]], font = FONT_LABEL_TEXT ),
        row = 2,
        column = 1,
        sticky = 'se',
        padx = 5,
        pady = 5
    )

    ELES$bar.Canvas[[ cond ]] = tkcanvas( frame, height = BAR_HEIGHT, borderwidth = 3, relief = 'ridge' )
    tkgrid(
        ELES$bar.Canvas[[ cond ]],
        row = 1,
        column = 0,
        columnspan = 2,
        sticky = 'nsew',
        padx = 5,
        pady = 5
    )

    tkgrid.columnconfigure( frame, 0, weight = 1 )
    tkgrid.columnconfigure( frame, 1, weight = 1 )
}

create.Pie.Frame =
function(parent, cond)
{

    frame = ttklabelframe(
        parent,
        text = bindStrings( 'Taxonomic Composition ', '[', cond, ']' )
        )
    
    if ( cond == 1 )
    {
        ELES$pie.Canvas = list()
    }

    ELES$pie.Canvas[[ cond ]] = tkcanvas( frame, width = DIAMETER_LARGE + 2 * HALO_WIDTH, height = DIAMETER_LARGE + 2 * HALO_WIDTH )
    
    tkpack( ELES$pie.Canvas[[ cond ]] )
}

#=================================================================================================== 
# UPDATE FUNCTIONS
#=================================================================================================== 

update.OnEnter.HandlerFactory = 
function( active )
{
    force( active )

    function()
    {
        tclServiceMode( FALSE )

        taxTag = as.character( tkgettags( ELES$pie.Canvas[[ active ]], 'current' ) )[ 1 ] 

        DATA$tax.Marked = taxTag.DelPrefix( taxTag )

        for ( cond in get.Conditions() )
        {
            node = DATA$tree[[ cond ]][ tax.id == DATA$tax.Marked & parent == DATA$tax.Current ]

            tkitemconfigure(
                ELES$pie.Canvas[[ cond ]],
                taxTag,
                fill = node$color.Light,
                width = HALO_WIDTH,
                outline = node$color.Dark 
                )

            if ( ! is.null( DATA$tax.Selected ) )
                tkitemconfigure( ELES$bar.Canvas[[ cond ]], taxTag.AddPrefix( DATA$tax.Selected ), state = 'hidden' )

            tkitemconfigure( ELES$bar.Canvas[[ cond ]], taxTag, state = 'normal' )
        }

        set.Labels( DATA$tax.Marked )

        tclServiceMode( TRUE )
    }
}

update.OnLeave.HandlerFactory = 
function( active )
{
    force( active )

    function()
    {
        taxTag = as.character( tkgettags( ELES$pie.Canvas[[ active ]], 'current' ) )[ 1 ] 

        DATA$tax.Marked = taxTag.DelPrefix( taxTag )

        for ( cond in rownames( DATA$object$map ) )
        {
            node = DATA$tree[[ cond ]][ tax.id == DATA$tax.Marked & parent == DATA$tax.Current ]

            tkitemconfigure(
                ELES$pie.Canvas[[ cond ]], 
                taxTag, 
                fill = node$color.Dark, 
                width = 1,
                outline = '#000000'
                )

            tkitemconfigure( ELES$bar.Canvas[[ cond ]], taxTag, state = 'hidden' )

            if ( ! is.null( DATA$tax.Selected ) )
                tkitemconfigure( ELES$bar.Canvas[[ cond ]], taxTag.AddPrefix( DATA$tax.Selected ), state = 'normal' )

        }

        set.Labels( DATA$tax.Selected )

        DATA$tax.Marked = NULL
    }
}

set.Labels =
function( id )
{
    if ( is.null( id ) )
        for ( cond in get.Conditions() )
        {
            tcl(
                'set',
                VARS$bar.Count_marked[[ cond ]],
                ''
                )

            tcl(
                'set',
                VARS$bar.Perc_marked[[ cond ]],
                ''
                )
                
            tcl(
                'set',
                VARS$footer.Name,
                ''
                )
                
            tcl(
                'set',
                VARS$footer.Rank,
                ''
                )
                
            tcl(
                'set',
                VARS$footer.TaxID,
                ''
                )
        }
    else
        for ( cond in get.Conditions() )
        {
            node = DATA$tree[[ cond ]][ tax.id == id & parent == DATA$tax.Current ]

            tcl(
                'set',
                VARS$bar.Count_marked[[ cond ]],
                format.Count( node$size )
                )

            tcl(
                'set',
                VARS$bar.Perc_marked[[ cond ]],
                format.Perc( node$total )
                )

            tcl(
                'set',
                VARS$footer.Name,
                get.Name( id )
                )
                
            tcl(
                'set',
                VARS$footer.Rank,
                RANK_NAMES[ get.Rank( id ) ]
                )
                
            tcl(
                'set',
                VARS$footer.TaxID,
                id
                )
        }
}

update.Select.HandlerFactory =
function( switch )
{
    if (switch == 'on' )
        function()
        {
            if ( ! is.null( DATA$tax.Selected ) )
            {
                if ( DATA$tax.Selected == DATA$tax.Marked ) 
                    return()
    
                unselect()
            }

            DATA$tax.Selected = DATA$tax.Marked
            
            for ( cond in get.Conditions() )
            {
                node = DATA$tree[[ cond ]][ tax.id == DATA$tax.Selected ]

                tkmove(
                    ELES$pie.Canvas[[ cond ]],
                    taxTag.AddPrefix( DATA$tax.Selected ),
                    node$x.Off,
                    node$y.Off 
                    )
            }

            draw.PCA( DATA$tax.Selected )
        }
    else
        function()
        {
            unselect()

            draw.PCA( DATA$tax.Current )
        }
}

unselect = 
function()
{
    if ( is.null( DATA$tax.Selected ) )
        return()

    for ( cond in get.Conditions() )
    {
        node = DATA$tree[[ cond ]][ tax.id == DATA$tax.Selected ]

        tkmove(
            ELES$pie.Canvas[[ cond ]],
            taxTag.AddPrefix( DATA$tax.Selected ),
            -node$x.Off,
            -node$y.Off 
            )

        tkitemconfigure( 
            ELES$bar.Canvas[[ cond ]],
            taxTag.AddPrefix( DATA$tax.Selected ),
            state = 'hidden' 
            )
    }

    DATA$tax.Selected = NULL
}

#=================================================================================================== 
# TREE MODIFYING FUNCTIONS
#=================================================================================================== 

init.Tree =
function()
{
    DATA$tree = list()

    for ( cond in rownames( DATA$object$map ) )
        DATA$tree[[ cond ]] = data.table(
            tax.id = as.character( TAX_ROOT ),
            size   = DATA$object$table[
                parent == TAX_ROOT & sample %in% which( DATA$object$map[ cond, ] )
                , sum( counts ) 
                ],
            parent       = as.character( NA ),
            level        = 1,
            color.Light  = as.character( NA ),
            color.Dark   = as.character( NA ),
            total        = 1,
            extent       = as.numeric( NA ),
            start        = as.numeric( NA ),
            rank         = MAX_RANK,
            x.Off        = as.numeric( NA ),
            y.Off        = as.numeric( NA )
            )        
}

expand.Node = 
function( id )
{
    precursor = list()

    for ( cond in rownames( DATA$object$map ) )
    {
        precursor[[ cond ]] = DATA$node.Stack[[ cond ]][
            parent == id,
            sum(counts),
            by = tax.id
            ]

        setnames( precursor[[ cond ]], 'V1', 'size' )

        precursor[[ cond ]][ , parent := id ] 
        precursor[[ cond ]][ , level := size / DATA$tree[[ cond ]][ tax.id == id, size ] ]

        precursor[[ cond ]] = precursor[[ cond ]][ level >= TAX_CUTOFF_RATIO ]
    }

    shared.Tax = Reduce( intersect, lapply( precursor, function( table ) table[ , tax.id ] ) )

    for ( cond in rownames( DATA$object$map ) )
    {
        precursor[[ cond ]] = precursor[[ cond ]][ tax.id %in% shared.Tax ]

        precursor[[ cond ]] = precursor[[ cond ]][ order( tax.id ) ]

        precursor[[ cond ]] = head( precursor[[ cond ]], MAX_DISPLAYED_TAX )

        options( stringsAsFactors = FALSE )

        precursor[[ cond ]][ , c( 'color.Light', 'color.Dark' ) := get.Color.Pairs( .N ) ]

        precursor[[ cond ]] = rbind( precursor[[ cond ]], data.frame(
            tax.id = TAX_UNDEF, 
            size = DATA$tree[[ cond ]][ tax.id == id, size ] - precursor[[ cond ]][ , sum( size ) ], 
            parent = id , 
            level = 1 - precursor[[ cond ]][ , sum( level ) ],
            color.Light = '#A0A0A0',
            color.Dark = '#606060'
            ))

        tmp.Total = DATA$tree[[ cond ]][ tax.id == id, total ]
        precursor[[ cond ]][ , total := level * tmp.Total ]

        precursor[[ cond ]][ , extent := - level * 360 ]

        tmp.Start = c( 0, cumsum( precursor[[ cond ]][ 1 : (.N - 1 ), extent ]) )

        precursor[[ cond ]][ , start := ( 450 + tmp.Start ) %% 360 ]

        precursor[[ cond ]][ , rank := get.Rank( id ) - 1 ]

        precursor[[ cond ]][ , c( 'x.Off', 'y.Off' ) := compute.Offset(start, extent ) ]

        DATA$tree[[ cond ]] = rbind( DATA$tree[[ cond ]], precursor[[ cond]] )
    }

}

compute.Offset = 
function(start, extent)
{
        deg.Offset = ( start + extent / 2 ) %% 360
        rad.Offset = 2 * pi * deg.Offset / 360
        
        list(
            cos( rad.Offset ) * RIM_WIDTH,
            -sin( rad.Offset ) * RIM_WIDTH
            )
}

#=================================================================================================== 
# DRAW FUNCTIONS
#=================================================================================================== 

draw.Point =
function(canvas, x, y, type, i)
{
    DEFAULT_COLOR = '#DDDDDD'

    # TODO: export constants and integrate type and font

    POINT_SIZE = PCA_DOT_RADIUS
    
    point = tkcreate( canvas, 'oval', x - POINT_SIZE / 2, y - POINT_SIZE / 2, x + POINT_SIZE / 2, y + POINT_SIZE / 2, fill = if ( is.na( type ) ) DEFAULT_COLOR else PCA_COLORS[ type ],  outline = '#000000', tags = c( bindStrings( '_', i ), 'point' ) )
 
    tkcreate( canvas, 'text', x, y, text = i, tags = c( bindStrings( '_', i ), 'label' ))

}

draw.PCA =
function( id )
{
    tkdelete( ELES$pca.Canvas, 'all' )

    PCA = make.PCA( id )

    tcl( 'set', VARS$pca.Var_1, format.Perc( PCA$v[ 1 ] ) )
    tcl( 'set', VARS$pca.Var_2, format.Perc( PCA$v[ 2 ] ) )

    # shrink to fit
    x = PCA_AREA_SIZE * ( PCA$x - min( PCA$x ) ) / ( max( PCA$x) - min( PCA$x ) ) + PCA_DOT_RADIUS
    y = PCA_AREA_SIZE * ( PCA$y - min( PCA$y ) ) / ( max( PCA$y) - min( PCA$y ) ) + PCA_DOT_RADIUS

    type = as.integer( apply( DATA$object$map, 2, which ) )

    apply(
        cbind( x, y, type, 1 : length( x ) ),
        1,
        function( point )
        {
            draw.Point( ELES$pca.Canvas, point[ 1 ], point[ 2 ], point[ 3 ], point[ 4 ] )
        }
    )

    tkitembind(
        ELES$pca.Canvas,
        'point',
        '<Any-Enter>',
        function()
        {
            sample.Tag = as.character( tkgettags ( ELES$pca.Canvas, 'current' ) )[ 1 ]
            
            tkitemraise( ELES$pca.Canvas, sample.Tag )
        }
    )
}

set.Content =
function( id )
{
    tclServiceMode( FALSE )

    tkconfigure( ELES$button.Up, state = 'normal' )

    tkconfigure( ELES$button.Down, state = 'normal' )

    if ( ( ! id %in% DATA$is.expanded ) )
    {
        expand.Node( id )

        DATA$is.expanded = c(DATA$is.expanded, id)
    }

    do.Drawing( id )

    tcl( 'set', VARS$header.Taxonomy, get.Name( id ) )
    tcl( 'set', VARS$header.Rank, RANK_NAMES[ get.Rank( id ) ] )

    for ( cond in get.Conditions() )
    {
        tcl( 'set', VARS$bar.Count_current[[ cond ]], format.Count( DATA$tree[[ cond ]][ tax.id == id, size ] ) )
        tcl( 'set', VARS$bar.Perc_current[[ cond ]], format.Perc( DATA$tree[[ cond ]][ tax.id == id, total ] ) )
        set.Labels( NULL )
    }

    if ( get.Rank( id ) == MAX_RANK )
        tkconfigure( ELES$button.Up, state = 'disabled' )

    if ( get.Rank( id ) == 2 )
        tkconfigure( ELES$button.Down, state = 'disabled' )

    draw.PCA( DATA$tax.Current )

    tclServiceMode( TRUE )
}

draw.Piece = 
function( canvas, node )
{
    if ( node$extent == 0 )
        return ()

    center = as.integer( tkcget( canvas, '-width' ) ) / 2

    if ( node$extent == -360 )
        tkcreate(
            canvas,
            'oval',
            center - DIAMETER_SMALL / 2,
            center - DIAMETER_SMALL / 2,
            center + DIAMETER_SMALL / 2,
            center + DIAMETER_SMALL / 2,
            fill = node$color.Dark,
            tags = taxTag.AddPrefix( node$tax.id )
            )
    else
        tkcreate(
            canvas,
            'arc',
            center - DIAMETER_SMALL / 2,
            center - DIAMETER_SMALL / 2,
            center + DIAMETER_SMALL / 2,
            center + DIAMETER_SMALL / 2,
            start = node$start,
            extent = node$extent,
            fill = node$color.Dark,
            tags = taxTag.AddPrefix( node$tax.id )
            )
}

draw.Bars = 
function( cond, id )
{
    canvas = ELES$bar.Canvas[[ cond ]]

    height = as.integer( tkcget( canvas, '-height' ) )
    offset = as.integer( tkcget( canvas, '-borderwidth' ) ) + 1
    width = as.integer( tkwinfo( 'width', canvas ) ) - offset

    # create parent piece
    tkcreate(
        canvas,
        'rectangle',
        offset,
        offset,
        offset + width * DATA$tree[[ cond ]][ tax.id == id, total ],
        offset + height,
        width = 0,
        fill = '#000000',
        state = 'normal'
        )

    for ( node in DATA$tree[[ cond ]][ parent == id, tax.id ] )
        tkcreate(
            canvas,
            'rectangle',
            offset,
            offset,
            offset + width * DATA$tree[[ cond ]][ tax.id == node & parent == id, total ],
            offset + height,
            width = 0,
            fill = DATA$tree[[ cond ]][ tax.id == node & parent == id, color.Light ],
            state = 'hidden',
            tags = taxTag.AddPrefix( node )
            )
}

do.Drawing = 
function( id )
{
    for ( cond in rownames( DATA$object$map ) )
    {
        tkdelete( ELES$pie.Canvas[[ cond ]], 'all' )
        tkdelete( ELES$bar.Canvas[[ cond ]], 'all' )

        nodes = DATA$tree[[ cond ]][ parent == id ]
        
        for ( idx in 1 : nodes[ , .N ] )
            draw.Piece( ELES$pie.Canvas[[ cond ]], nodes[ idx ] ) 
        
        draw.Bars( cond, id )

        tkitembind(
            ELES$pie.Canvas[[ cond ]],
            'all',
            '<Any-Enter>',
            update.OnEnter.HandlerFactory( cond ) 
            )

        tkitembind(
            ELES$pie.Canvas[[ cond ]],
            'all',
            '<Any-Leave>',
            update.OnLeave.HandlerFactory( cond ) 
            )
        
        tkitembind(
            ELES$pie.Canvas[[ cond ]],
            bindStrings( '!', TAX_TAG_PREFIX, TAX_UNDEF ),
            '<1>',
            update.Select.HandlerFactory( 'on' )
            )

        tkitembind(
            ELES$pie.Canvas[[ cond ]],
            bindStrings( '!', TAX_TAG_PREFIX, TAX_UNDEF ),
            '<3>',
            update.Select.HandlerFactory( 'off' )
            )
    }
}

#=================================================================================================== 
# AUXILIARY FUNCTIONS
#=================================================================================================== 

prepare.Object =
function( data, refined, config)
{
    object = list()

    object$table = slot( refined, 'QuickDT' )[ , .( Sample, TaxID, Counts, Previous ) ]
    colnames( object$table ) = c( 'sample', 'tax.id', 'counts', 'parent' )

    object$data = data

    map = matrix(
        FALSE, 
        nrow = length( slot( config, 'ClassNames' ) ), 
        ncol = length( slot( config, 'ClassVec' ) )
        )

    dimnames(map) = list(
        conditions = slot( config, 'ClassNames' ),
        samples    = as.character( 1 : ncol( map ) )
        )

    map[ slot( config ,'ClassVec' ) + nrow( map ) * ( ( 1 : ncol( map ) ) -1 ) ] = TRUE
    
    object$map = map[ slot( config, 'SelectedClasses' ), ]

    object$names = readRDS( get.RdsFileDescriptor( 'tax.names' ) )

    object$config = config

    object
}

taxTag.AddPrefix = 
function( tax.id )
{
    bindStrings( TAX_TAG_PREFIX,  as.character( tax.id ) )
}

taxTag.DelPrefix =
function( tag.name )
{
    substring( tag.name, 2 )
}

get.Rank =
function( id )
{
    if ( id != TAX_UNDEF )
        DATA$tree[[ 1 ]][ tax.id == id, rank ]
    else
        DATA$tree[[ 1 ]][ tax.id == id & parent == DATA$tax.Current, rank ]
}

get.Conditions =
function()
{
    rownames( DATA$object$map )
}

format.Count =
function( counts )
{
    sprintf( '%1.2E', counts)
}

format.Perc =
function( perc )
{
    sprintf( '%05.2f %%', perc * 100 )
}

get.Name =
function( tax.id )
{
    DATA$object$names[ as.character( tax.id ) ]
}

get.Color.Pairs =
function( n )
{
    lapply(
        BRIGHTNESS,
        function( brightness )
            sapply( ( 1 : n ) / n , hsv, s = 1, v = brightness )
    )    
}

make.PCA = 
function( id )
{
    slot( DATA$object$config, 'SelectedTax' ) = as.integer( id )

    PCA = do.pca( create.matrix( DATA$object$data, DATA$object$config )[[ 1 ]] )
    
    list(
        x = PCA$x[ , 'PC1' ],
        y = PCA$x[ , 'PC2' ],
        v = ( PCA$sdev ^ 2 / sum( PCA$sdev ^ 2) )[ 1 : 2 ]
    )
}

#=================================================================================================== 
# DUMMY FUNCTIONS
#=================================================================================================== 

call.Wrapper = 
function()
{
    library( data.table )
    library( tcltk )

    tax.Select(
        readRDS( '/data/big.rds' ),
        readRDS( '/data/Refined.rds' ),
        readRDS( '/data/job.nonfig.rds' )
    )
}
