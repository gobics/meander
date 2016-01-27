function init()
{
	document.title = TITLE;
	if (document.getElementsByTagName("h3").length > 0)
    {
	    document.getElementsByTagName("h1")[0].innerHTML = KEGG_LINK;
        document.getElementsByTagName("h3")[0].innerHTML = P_VALUE;
    }	
    else
	    document.getElementsByTagName("h1")[0].innerHTML = TITLE;
        
	
	build_table(0, false);
}

function build_table(by, up)
{
	var table="";
	table += build_caption();
	table += build_sort(by, up);
	table += build_body();
	
	document.getElementsByTagName("table")[0].innerHTML = table;
}

function sort_table(by, up)
{
	var table= TABLE_DATA;
	
	table.sort(function(x,y) {return (x[by-1] - y[by-1]);});
	
	if (!up) table.reverse();
	
	build_table(by, up)
}

function build_caption()
{
	var row = "<tr>\n";
	
	row += "<th class=\"big\" colspan=\"3\">"+ CAPTION +"</th>\n";

	for (var col=0; col < TABLE_CAPTION.length; col++)
	{
		row += "<th width=100px>"+ TABLE_CAPTION[col] +"</th>\n"
	}
	
	row += "</tr>\n";
	
	return row;
}

function build_sort(by, up)
{
	var sort = "<tr class=\"sorting\">\n";
	
	for (var col=0; col < COLUMN_NOF; col++)
	{
		sort += "<td  style=\"text-align:center; font-weight:bold\" class=";

		switch (col)
		{
			case ORDINAL_COL:
				sort += "\"ordinal\">";
				sort += SUB_CAPTION[ORDINAL_COL];
				break;
			case PATHWAY_COL:
				sort += "\"pathway\">";
				sort += SUB_CAPTION[PATHWAY_COL];
				break;
			case LINK_COL:
				sort += "\"link\">";
				sort += SUB_CAPTION[LINK_COL];
				break;
			default:
				sort += "\"sorting\">";
				sort += "<a href=\"javascript:sort_table("+col+",true)\">"+((up && (col == by))?ICON_UP_ACT:ICON_UP_IN)+"</a>";
				sort += "<a href=\"javascript:sort_table("+col+",false)\">"+((!up && (col == by))?ICON_DOWN_ACT:ICON_DOWN_IN)+"</a>";
		}
		
		sort += "</td>\n";
	}
	
	sort += "</tr>\n";
	
	return sort;
}

function build_body()
{
	var body = "";
	
	var ROW_NOF = TABLE_DATA.length;
	
	for (var row=0; row < ROW_NOF; )
	{
		var current= TABLE_DATA[row];
		var col= 0;
		
		body += "<tr>\n"+
			"<td class=\"ordinal\">"+(++row)+".</td>\n"+
			"<td class=\"pathway\">"+current[col++]+"</td>\n"+
			"<td class=\"link\">"+current[col++]+"</td>\n";
			
		for (; col+ 1 < COLUMN_NOF;)
		{
			body += "<td>"+current[col++]+"</td>\n";
		}
			
		body +=	"</tr>\n"
	}
	
	return body;
}
