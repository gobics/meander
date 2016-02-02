TABLE_COLUMN_OFFSET = 2

# @PARAMS
# data.frame : pathways.table             [ _df.rds ]
# data.frame : pathways.table.header      
#   - contains the variables: string, type for each column of pathway.table indicating
#    - string : the name of the column as it appears in the html table
#    - type   : the type of the column data, either (i) integer, (r) ratio, (f) double
# character  : kegg.pathways.IDs          [ data/keggmapnames.rds ]
# character  : kegg.pathways.names        [ data/pathway.names.Rds ]
# character  : kegg.orthologs.description [ data/ko_desc.rds ]
# integer    : orthologs.flag             [ Object@DATA@Refined@FlagVec ]
# double     : orthologs.p_value          [ _pval.rds ]
# matrix     : orthologs.pathways.map     [ Object@DATA@KEGG@KEGG2PATH ]
# string     : output.Dir
write.html.files = function(pathways.table, pathways.table.header, kegg.pathways.IDs, kegg.pathways.names, kegg.orthologs.description, orthologs.flag, orthologs.p_value, orthologs.pathways.map, output.Dir)
{
    # prepare pathways.table
    names(pathways.table) = pathways.table.header$string  
    for (col in 1:ncol(pathways.table))
    {
        if (pathways.table.header$type[col] == 'r')
        {
            pathways.table[is.na(pathways.table[, col]), col] = 0
            pathways.table[, col] = sprintf('%1.2f', round(pathways.table[,col], 2))
        }
        if (pathways.table.header$type[col] == 'f')
            pathways.table[, col] = sprintf('%1.2E', pathways.table[,col])
    }
    rownames(pathways.table) = gsub('map', '', kegg.pathways.IDs)
    pathways.table$Name = kegg.pathways.names[rownames(pathways.table)]
    pathways.table = pathways.table[, c(ncol(pathways.table), 2:ncol(pathways.table)- 1)]

    # prepare orthologs.table
    max.ko = nrow(orthologs.pathways.map)
    orthologs.table = data.frame(
        Name = kegg.orthologs.description[1:max.ko],
        Flag = orthologs.flag[1:max.ko], 
        P.Value = orthologs.p_value[1:max.ko],
        stringsAsFactors = FALSE,
        row.names = sprintf('K%05d', 1:max.ko)
        )
    orthologs.table$Name[orthologs.table$Name==''] = rownames(orthologs.table)[orthologs.table$Name=='']
    orthologs.table$P.Value = sprintf('%1.2E', orthologs.table$P.Value)

    # prepare map
    pathways.orthologs.map = t(orthologs.pathways.map == 1)
    dimnames(pathways.orthologs.map) = list(rownames(pathways.table), rownames(orthologs.table))

    # call next
    create.HTML.Output(pathways.table, orthologs.table,  pathways.orthologs.map, output.Dir)
}

create.HTML.Output = function(Pathways.Table, Orthologs.Table, Pathways.Orthologs.Map, output.Dir)
{
    prepare.Output.Dir(output.Dir)

    create.Output.Pathways(Pathways.Table, Orthologs.Table, Pathways.Orthologs.Map, output.Dir)

    create.Output.Orthologs(Pathways.Table, Orthologs.Table, Pathways.Orthologs.Map, output.Dir)
}

prepare.Output.Dir = function (output.Dir)
{
    dir.create(
        file.path(output.Dir, PATHWAY_TABLE_PATH),
        showWarnings = FALSE,
        recursive = TRUE
    )

    dir.create(
        file.path(output.Dir, ORTHOLOG_TABLE_PATH),
        showWarnings = FALSE,
        recursive = TRUE
    )

    file.copy(
        from = HTML_SUPPLY_PATH,
        to = file.path(output.Dir, HTML_OUTPUT_SUBDIR),
        recursive = TRUE
    )

    file.copy(
        from = HTML_RESULT_TEMPLATE,
        to = output.Dir
    )

    write.ColorKey.HTML(output.Dir)
}

create.Output.Pathways = function(Pathways.Table, Orthologs.Table, Pathways.Orthologs.Map, output.Dir)
{
    pathway.Caption = convertTo.HTML_StringArray(colnames(Pathways.Table)[-1])

    pathway.Names = Pathways.Table[1]

    pathway.Links = get.PathwayLinks(rownames(Pathways.Table))

    pathway.Values = Pathways.Table[, -1]

    pathway.HTML.Data = build.Pathways.HTML.Data(pathway.Names, pathway.Links, pathway.Values, rownames(Pathways.Table))

    column.nof = ncol(Pathways.Table) + TABLE_COLUMN_OFFSET

    write.Pathway.HTML(OVERVIEW_TITLE, NA, rep(TRUE, nrow(Pathways.Table)), pathway.Caption, pathway.HTML.Data, output.Dir, column.nof)

    for (ortholog in rownames(Orthologs.Table))
    {
        write.Pathway.HTML(ortholog, Orthologs.Table[ortholog, 3], Pathways.Orthologs.Map[, ortholog], pathway.Caption, pathway.HTML.Data, output.Dir, column.nof)
    }
}

create.Output.Orthologs = function(Pathways.Table, Orthologs.Table, Pathways.Orthologs.Map, output.Dir)
{
    for (pathway in rownames(Pathways.Table))
    {
        Orthologs.Subset = Orthologs.Table[Pathways.Orthologs.Map[pathway,],]

        pathway.name = Pathways.Table[pathway, 1]

        write.Ortholog.HTML(pathway, pathway.name, Orthologs.Subset, output.Dir)
    }
}

get.PathwayLinks = function(pathways.ID)
{
    link.Symbols = c(
        'M',
        'T',
        'K'
        )
    link.Prefixes = c(
        '../../svg/map',
        '../orthologs/',
        'http://www.genome.jp/dbget-bin/www_bget?map'
        )
    link.Suffixes = c(
        '.svg',
        '.html',
        ''
        )
    link.target = c(
        RIGHT_FRAME,
        RIGHT_FRAME,
        NEW_FRAME
        )

    separator = '&nbsp;'

    substitute = '%s'

    format.String = paste(apply(
        cbind(link.Symbols, link.Prefixes, link.Suffixes, link.target),
        1,
        function(link)
        {
            bindStrings(
                '<a href=\\"',
                link[2],
                substitute,
                link[3],
                '\\" target=\\"',
                link[4],
                '\\"><b>',
                link[1],
                '</b></a>'
                )
        }
        ), collapse = separator)

    sprintf(format.String, pathways.ID, pathways.ID, pathways.ID)
}

build.Pathways.HTML.Data = function(names, links, values, id)
{
    #values = format.Values(values)

    data = apply(
        cbind(names, links, values),
        1,
        convertTo.HTML_StringArray
        )

    names(data) = id

    data
}

write.Pathway.HTML = function(title, p.value, pathway.idx, caption, data, dir, column.nof)
{
    is.overview = is.na(p.value)

    file = file.path(dir, PATHWAY_TABLE_PATH, bindStrings(title, '.html'))
    # TODO

    head = paste(
        readLines(file.path(HTML_TEMPLATE_PATH, 'head.temp')),
        collapse = '\n'
        )

    tail = paste(
        if (is.overview)
            readLines(file.path(HTML_TEMPLATE_PATH, 'tail_overview.temp'))
        else
            readLines(file.path(HTML_TEMPLATE_PATH, 'tail.temp')),
        collapse = '\n'
        )


    keggLink = bindStrings(
        '\"<a href=\\\"http://www.genome.jp/dbget-bin/www_bget?ko+',
        title,
        '\\" target=\\\"_blank\\\">',
        title,
        '</a>\"'
        )

    cat(
        head,
        '\n\n',

        'var TABLE_DATA = [\n\t',
        paste(data[pathway.idx], collapse=',\n\t'),
        '\n];\n\n',

        'var TABLE_CAPTION = ',
		chartr('_', ' ', caption),
		';\n\n',

		'var TITLE = ',
		sprintf('\"%s\"', chartr('_', ' ', title)),
		';\n\n',

        'var KEGG_LINK = ',
        keggLink,
        ';\n\n',

        'var COLUMN_NOF = ',
        column.nof,
        ';\n\n',

		if (!is.overview)   'var P_VALUE = ',
		if (!is.overview)   sprintf('\"p-Value: %s\"', chartr('_', ' ', p.value)),
		if (!is.overview)   ';\n\n',

        tail,
        '\n',

        file = file
    )
}

UNIT_TEST_create.HTML.Output = function()
{
    output.Dir = path.expand('~/9796')
    
    np=100

    no=50    

    Pathways.Table = data.frame(names = sample(LETTERS, np, replace = TRUE), coverage = sample(100, np, replace = TRUE)/100, abundance = sample(10000, np), stringsAsFactors = FALSE)
    rownames(Pathways.Table) = sprintf('%05d', sample(10000, np))

    Orthologs.Table = data.frame(names = sample(LETTERS, no, replace = TRUE), flag = sample(15, no, replace = TRUE), p.value = sample(100, no, replace = TRUE)/10000, stringsAsFactors = FALSE)
    rownames(Orthologs.Table) = sprintf('K%05d', sample(10000, no))

    Pathways.Orthologs.Map = matrix(sample(c(TRUE, FALSE), no*np, replace = TRUE), nrow = np)
    dimnames(Pathways.Orthologs.Map) = list(rownames(Pathways.Table), rownames(Orthologs.Table))

    create.HTML.Output(Pathways.Table, Orthologs.Table, Pathways.Orthologs.Map, output.Dir)
}

write.Ortholog.HTML = function(pathway, pathway.name, Orthologs.Table, output.Dir)
{
    output.File = file.path(output.Dir, ORTHOLOG_TABLE_PATH, bindStrings(pathway, '.html'))

    table.FormatString = readLines(file.path(HTML_TEMPLATE_PATH, 'table.fmt'))

    diffExp.Idx = has.Flag(Orthologs.Table[, 2], SHOULD_MAP_FLAG, MAPPED_FLAG)

    diffExp.Table = Orthologs.Table[diffExp.Idx, ]
    misMap.Table = Orthologs.Table[!diffExp.Idx, ]

    diffExp.Table.Rows = build.Table.Rows(diffExp.Table[order(as.numeric(diffExp.Table[,3])), ])
    misMap.Table.Rows = build.Table.Rows(misMap.Table[order(misMap.Table[,2], decreasing = TRUE), ])

    cat(
        sprintf(table.FormatString, pathway.name, pathway.name, diffExp.Table.Rows, misMap.Table.Rows),
        file = output.File
        )
}

build.Table.Rows = function(table)
{
    rows = nrow(table)

    if (rows == 0)
        return('')

    row.FormatString = readLines(file.path(HTML_TEMPLATE_PATH, 'row.fmt'))

    paste(
        apply(
            cbind(1:rows, rownames(table), table),
            1,
            function(ortholog)
            {
                sprintf(
                    row.FormatString,
                    as.integer(ortholog[1]),
                    ortholog[2],
                    ortholog[3],
                    ORTHOLOG_COLORS[as.integer(ortholog[4]) + 1],
                    as.numeric(ortholog[5])
                    )
            }
            ),
        collapse = '\n'
        )
}

convertTo.HTML_StringArray = function(data)
{
    sprintf(
        '[\"%s\"]', 
        paste(
            if (!is.null(dim(data)))
                apply(data, 1, c)
            else 
                data, 
            collapse='\",\"'
        )
    )
}


write.ColorKey.HTML = function (output.Dir)
{
    output.File = file.path(output.Dir, HTML_OUTPUT_SUBDIR, 'COLOR_KEY.html')
    table.FormatString = readLines(file.path(HTML_TEMPLATE_PATH, 'key_table.fmt'))
    row.FormatString = readLines(file.path(HTML_TEMPLATE_PATH, 'key_row.fmt'))

    DESCRIPTION = matrix(
        c(
            ORTHOLOG_COLORS[16], 'Significant upregulated', ORTHOLOG_COLORS[3], 'Hit but should not',
            ORTHOLOG_COLORS[12], 'Significant downregulated', ORTHOLOG_COLORS[2], 'No Hit but should',
            ORTHOLOG_COLORS[8], 'Upregulated', ORTHOLOG_COLORS[1], 'No Hit and should not',
            ORTHOLOG_COLORS[4], 'Downregulated', '', ''
            ),
        byrow = TRUE,
        ncol = 4    
        )

    rows = paste(
        apply(
            DESCRIPTION,
            1,
            function(x)
            {
                sprintf(row.FormatString, x[1], x[2], x[3], x[4])
            }
            ),
        collapse = '\n'         
        )

    cat(
        sprintf(table.FormatString, rows),
        file = output.File
        )
}

