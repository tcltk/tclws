#
# Tcl Httpd local site templates
#
# SCCS: @(#) mypage.tcl 1.5 97/12/23 21:11:10
#

package provide mypage 1.0

namespace eval mypage {
    namespace export *
}

# mypage::contents
#
#	Define the site contents
#
# Arguments:
#	list	List of URL, labels for each page in the site
#
# Side Effects:
#	Records the site structure

proc mypage::contents {list} {
    variable contents $list
}

# mypage::header
#
#	Generate HTML for the standard page header
#
# Arguments:
#	title	The page title
#
# Results:
#	HTML for the page header.

proc mypage::header {title} {
    mypage::SetLevels $title
    set html [html::head $title]\n
    append html [html::bodyTag]\n

    append html "<table cellpadding=0 cellspacing=0 border=0 width=100%>\n"
    append html "<tr> \
	[html::cell align=left  "<a href=/><img src=/images/tclp.gif border=0 alt=\"Home\"></a>"] \
	[html::cell "" "<h2>$title</h2>"] \
	</tr>"
    append html [mypage::thinrule]
    append html "</table>"
    append html [html::font]\n

    return $html
}

# mypage::thinrule
#
#	Generate a thin horizontal rule, expect to be in full width table.
#
# Arguments:
#	bgcolor	(optional) color
#
# Results:
#	HTML for the table row containing the rule.

proc mypage::thinrule {{param {colspan=2}}} {
    set color [html::default thinrule.bgcolor $param]
    append html "<tr>[html::cell "$param $color" \
		"<img src=/images/Space.gif width=2 height=2>"]</tr>\n"
    return $html
}

# mypage::SetLevels
#
#	Map the page title to a hierarchy location in the site for the page.
#
# Arguments:
#	title	The page title
#
# Side Effects:
#	Sets the level namespace variables

proc mypage::SetLevels {title} {
    variable level
}

# mypage::footer
#
#	Generate HTML for the standard page footer
#
# Arguments:
#	none
#
# Results:
#	HTML for the page footer.

proc mypage::footer {} {
    variable contents
    if {![info exists contents]} {
        set contents {}
    }
    append html "<!-- contents = {$contents} -->\n"
    append html "<table cellpadding=0 cellspacing=2 border=0 width=100%>\n"
    append html [thinrule colspan=[expr {[llength $contents]/2}]]
    append html <tr>[html::cell "" [html::minorMenu $contents </font></td><td>[html::font]]]</tr>\n
    append html </table>\n
    append html "</body>\n</html>\n"
    return $html
}

# mypage::README_links
#
#	Generate optional links to distribution README files.
#
# Arguments:
#	none
#
# Results:
#	HTML for some links

proc mypage::README_links {} {
    set html ""
    foreach f [lsort [glob -nocomplain [file join [Doc_Root] links/*]]] {
        if {[file tail $f] == "CVS"} {
            continue
        }
	if {[file exists $f]} {
	    # Symlink is not broken
	    set t [file tail $f]
	    append html "<li><a href=/links/$t>$t</a>\n"
	}
    }
    if {[string length $html]} {
	set html <ul>$html</ul>
    }
    return $html
}
