
FULL_CIRCLE_ANGLE = 2 * pi

#make editiable
MIN_RATIO_EXPAND = 0.01

RIGID_CUTOFF_MODE = TRUE

DEFAULT_COLOR = '#909090'

MIN_SIZE = 1
MAX_SIZE = 50

SHELL_WIDTH = MAX_SIZE * 1.25

LEVEL_WIDTH = MAX_SIZE

COLORING_THRESHOLD_RATIO = 0
COLORING_THRESHOLD_SHELL = 5 

COLORING_THRESHOLD_RADIUS = MAX_SIZE/5

TOTAL_PADDING = 20
BRANCH_PADDING = 5
LEAF_PADDING = 5
SHELL_PADDING = 10

draw.Overview = function (Object, radial.Flag = FALSE)
{
    samples.cond.A = which(Object@Job@Config@ClassVec == Object@Job@Config@SelectedClasses[1])

    samples.cond.B = which(Object@Job@Config@ClassVec == Object@Job@Config@SelectedClasses[2])

    table = obtain.Table(Object)

    forest = obtain.Forest(table, samples.cond.A, samples.cond.B)

    forest = obtain.Coordinates(forest, radial.Flag)

    forest$color = obtain.Colors(forest)

    forest$name = obtain.Names(forest)

    canvas = create.Canvas(forest)

    draw.Edges(forest, canvas)

    draw.Nodes(forest, canvas)
    
    forest
}

obtain.Forest = function (table, samples.cond.A, samples.cond.B)
{
    tree.A = obtain.Tree(table, samples.cond.A)
    tree.B = obtain.Tree(table, samples.cond.B)

    forest = merge.Forest(tree.A, tree.B)
}

obtain.Coordinates = function (forest, radial.Flag)
{
    if (radial.Flag)
    {
        # TODO - Sebastian: auto-generated method stub
    }
    else
    {
        forest$x = obtain.X_Coordinates(forest)

        forest$y = obtain.Y_Coordinates(forest)
    }

    forest 
}

obtain.X_Coordinates = function (forest)
{
    node.Nof = nrow(forest)

    pos = rep(-1, node.Nof)
    names(pos) = forest[, id]

    extent = rep(0, node.Nof)
    names(extent) = forest[, id]

    x = rep(0, node.Nof)
    names(x) = forest[, id]
    
    temp.Data = list(pos = pos, extent = extent, x = x)

    temp.Data = obtain.X_Coordinates.SubTree(forest, as.character(TAX_ROOT_ID), list(0), temp.Data)

    temp.Data$x
}

obtain.Y_Coordinates = function (forest)
{
    max.Radius = forest[shell > 0, max(radius), by = shell]$V1

    temp.Matrix = matrix(0, nrow = MAX_RANK, ncol = MAX_RANK)

    temp.Matrix[lower.tri(temp.Matrix)] = rep(max.Radius, times = (MAX_RANK - 1):0) * 2

    temp.Matrix = temp.Matrix + lower.tri(temp.Matrix) * SHELL_PADDING

    diag(temp.Matrix) = max.Radius + TOTAL_PADDING

    y.Per.Shell = c(TOTAL_PADDING, apply(temp.Matrix, 1, sum))

    times = diff(c(which(!duplicated(forest$shell)), nrow(forest) + 1))

    rep(y.Per.Shell, times = times)
}

obtain.X_Coordinates.SubTree = function (forest, node, pos.List, temp.Data)
{
    children = forest[parent == node]

    self = forest[id == node] 

    pos = pos.List[[1]]

    temp.Data$pos[node] = pos

    children.Nof = nrow(children)

    if (children.Nof == 0)
    {
        temp.Data$extent[node] = 2 * self$radius
    }
    else
    {
        for (child in 1:children.Nof)
        {
            temp.Data = obtain.X_Coordinates.SubTree(forest, children[child, id], c(child - 1, pos.List), temp.Data)
        }      

        temp.Data$extent[node] = max(
            sum(temp.Data$extent[children$id]) + (children.Nof - 1) * BRANCH_PADDING,
            2 * self$radius
            )
    }

    if (pos > 0)
    {
        ancestor = self$parent

        siblings = forest[parent == ancestor, id]

        left = names(temp.Data$pos[siblings])[temp.Data$pos[siblings] == pos - 1]

        temp.Data$x[node] = temp.Data$x[left] + temp.Data$extent[left] / 2 + temp.Data$extent[node] / 2 + BRANCH_PADDING
    }
    else
    {
        ancestor = node
   
        while(pos == 0)
        {
            ancestor = forest[id == ancestor, parent]

            pos = if (is.na(ancestor)) -1 else temp.Data$pos[ancestor]
        }

        if (is.na(ancestor))
            temp.Data$x[node] = temp.Data$extent[node] / 2 + TOTAL_PADDING
        else
        {
            siblings = forest[parent == forest[id == ancestor, parent], id]
            
            left = names(temp.Data$pos[siblings])[temp.Data$pos[siblings] == pos - 1]

            temp.Data$x[node] = temp.Data$x[left] + temp.Data$extent[left] / 2 + temp.Data$extent[node] / 2 + BRANCH_PADDING                  }
    }
        
    temp.Data
}

merge.Forest = function (tree.A, tree.B)
{
    forest = rbind(
        tree.A[, .(id, parent, shell)],
        tree.B[!(tree.B$id %in% tree.A$id), .(id, parent, shell)]
        )

    forest$size.A = 0
    forest$size.B = 0

    forest[id %in% tree.A$id, size.A:=tree.A$size]
    forest[id %in% tree.B$id, size.B:=tree.B$size]

    forest$radius = forest$size.A
    forest[size.A < size.B, radius:=size.B]

    forest[order(shell, parent, id)]
}

obtain.Tree = function (table, samples)
{
    tree = init.Tree(table[Sample %in% samples])

    tree = purge.Tree(tree)
}

init.Tree = function (table)
{
    root = data.table(
		id = TAX_ROOT_ID,
		parent = as.numeric(NA),
		rank = MAX_RANK + 1,
		count = table[Previous == TAX_ROOT_ID, sum(Counts)]
		)

    trunk = table[, sum(Counts), by = .(TaxID, Previous, Rank)] 

    rbindlist(list(
		root,
		trunk
		))
}

obtain.Table = function(Object)
{
    table = Object@DATA@Refined@QuickDT

    # get ranks
    for (rank in 1: MAX_RANK)
    {
        table$Rank[table$TaxID %in% Object@DATA@KEGG@TaxMat[, rank]] = rank
    }

    table
}

purge.Tree = function(tree) 
{
    tree$ratio = tree[, count / tree[id == TAX_ROOT_ID, count]]

    tree$size = obtain.Size(tree)

    tree$expand = tree[, ratio >= MIN_RATIO_EXPAND]
    
    keep = c(TAX_ROOT_ID, tree[expand == TRUE, id])

    tree = tree[parent %in% keep | id == TAX_ROOT_ID]

    if (RIGID_CUTOFF_MODE)
        tree = tree[ratio >= MIN_RATIO_EXPAND]

    tree$id = as.character(tree[, id])

    tree$parent = as.character(tree[, parent])

    tree$shell = tree[, MAX_RANK - rank + 1]

    tree[order(shell, parent, id), .(id, parent, shell, size)]
}

obtain.Size = function(tree)
{
    size = tree[, MAX_SIZE * sqrt(ratio)]
    
    size[size < MIN_SIZE] = MIN_SIZE
    
    size
}

obtain.Names = function(forest)
{
    readRDS(get.RdsFileDescriptor('pathway.names'))[forest$id]  
}

obtain.Colors = function(forest)
{

    forest$has.color = (forest$radius >= COLORING_THRESHOLD_RADIUS) & (forest$shell <= COLORING_THRESHOLD_SHELL) & (forest$id != TAX_ROOT_ID)
   
    color = rep(DEFAULT_COLOR, nrow(forest))
    names(color) = forest$id
    
    color[forest[has.color == TRUE, id]] = substring(rainbow(sum(forest$has.color)), 1, 7)
    
    color
}

extract.SubTree = function(tree, vertex)
{
    index = vertex
    
    fringe = as.list(tree[parent == vertex, id])
    
    while(length(fringe) > 0)
    {
        vertex = fringe[[1]]
        
        index = c(index, vertex)
        
        fringe[[1]] <- NULL
        
        fringe = c(fringe, as.list(tree[parent == vertex, id]))
    }
    
    tree[id %in% index]
}

create.Canvas = function(forest)
{
    min.Node.X = forest[x == min(x), id][1]
    max.Node.X = forest[x == max(x), id][1]

    fringe = forest[shell == max(shell)]

    top = tktoplevel()
    
    width = forest[id == max.Node.X, x] - forest[id == min.Node.X, x] + forest[id == max.Node.X, radius] + forest[id == min.Node.X, radius] + 2 * TOTAL_PADDING

    height = fringe[, max(y)] + fringe[, max(radius)] + TOTAL_PADDING

    canvas = tkcanvas(top, width = width, height = height, background = '#FFFFFF')
    
    tkgrid(canvas, row=0, column=0, rowspan=2)

    key = create.Key(top, forest)
    
    tkgrid(key, row=0, column=1)
    
    control = create.Control(top, forest)

    tkgrid(control, row = 1, column = 1)

    canvas
}

draw.Edges = function (forest, canvas)
{
    for (node in 2:nrow(forest))
    {
        tkcreate(canvas, 'line', forest[node, x], forest[node, y], forest[id == forest[node, parent], x], forest[id == forest[node, parent], y])
    }

}

draw.Nodes = function (forest, canvas)
{
    for (node in 2:nrow(forest))    #dont draw root node
    {
        current = forest[node]

        tkcreate(
            canvas,
            'arc',
            current$x - current$size.A,
            current$y - current$size.A,
            current$x + current$size.A,
            current$y + current$size.A,
            start = 90,
            extent = 180,
            fill = current$color
            )
        
        tkcreate(
            canvas,
            'arc',
            current$x - current$size.B,
            current$y - current$size.B,
            current$x + current$size.B,
            current$y + current$size.B,
            start = 270,
            extent = 180,
            fill = current$color
            )
    }
}

create.Key = function (window, forest)
{
    key.Frame = tkframe(window)

    forest[color != DEFAULT_COLOR, draw.KeyPairs(key.Frame, color, name)]
    
    key.Frame
}

draw.KeyPairs = function(frame, color, name)
{
	for (i in 1 : length(color))
	{
		box = tkframe(frame, background = color[i], width = 20, height = 20)
				
		text = tkentry(frame, textvariable = tclVar(name[i]), state = 'readonly', relief = 'flat')
		
		tkgrid(box, column = 0, row = i - 1, padx = 5, pady = 5)
		tkgrid(text, column = 1, row = i - 1, sticky = 'nsw', padx = 5, pady = 5)
	}
}

create.Control = function (window, forest)
{
    control.Frame = tkframe(window)

    save.Button = tkbutton(control.Frame, text = 'SAVE IMAGE', command = function() saveFile.Dialog(window, forest))

    export.Button = tkbutton(control.Frame, text = 'EXPORT KEY', command = function() exportKey.Dialog(forest))

    tkpack(save.Button, expand = 1, fill = 'both')

    tkpack(export.Button, expand = 1, fill = 'both')

    control.Frame
}

saveFile.Dialog = function(window, forest)
{
    epsFileName = tclvalue(tkgetSaveFile(
        initialfile = "condition_overview.eps",
        filetypes = "{{All files} *} {{Encapsulated PostScript Files} {.eps}}"
        ))

    if (!nchar(epsFileName))
        return()
    else
    {
        canvas = as.character(tkwinfo('children', window))[1]

        choice = tclvalue(tkmessageBox(title = 'Export Color Key', message = 'It is recommended to export the color key too, as it is not part of the image. Do you want to export the key file?', type = 'yesnocancel'))

        if (choice == 'cancel') 
            return()
        else if (choice == 'yes')
            exportKey.Dialog(forest, epsFileName)

        tkpostscript(canvas, file = epsFileName)
    }
}

exportKey.Dialog = function(forest, default.FileName = 'condition_overview')
{
    keyFileName = tclvalue(tkgetSaveFile(
        initialfile = paste(c(default.FileName, '_key.csv'), collapse = ''),
        filetypes = "{{All files} *} {{Comma-Separated Values Files} {.csv}}"
        ))

    if (!nchar(keyFileName))
        return()
    else
    {
        write.table(
            forest[color != DEFAULT_COLOR, .(color, name)],
            file = keyFileName,
            row.names = FALSE,
            col.names = FALSE,
            sep = ',',
            quote = FALSE
            )
    }
}
