#
# Faq generation tool
# (for use in .tml->.html files)
#
# Laurent Demailly
#
# SCCS: @(#) faq.tcl 1.5 97/12/23 21:13:33
#

package provide faq 1.2

global FAQ

proc FAQinit {{title ""}} {
    global FAQ
    if {[info exists FAQ]} {
	unset FAQ
    }
    set FAQ(section) 0
    set FAQ(question) 0
    set FAQ(currentSection) S0
    set FAQ(sections) [list S0]
    set result {}
    if {[string length $title]} {
	append result [FAQ_H1 $title]
    }
    append result "<!-- generated using @(#) faq.tcl 1.5 97/12/23 21:13:33 -->\n"
    return $result
}

proc FAQsection {section {ref ""}} {
    global FAQ
    if {$FAQ(section) == 0} {
	# Reset the section list, we have a FAQ with actual sections:
	set FAQ(sections) {}
    }
    incr FAQ(section)
    set label $FAQ(section)
    if {[string compare $ref ""] == 0} {
	set ref "S$label"
    }
    if {[info exists FAQ(label$ref)]} {
	error "ref \"$ref\" not unique!"
    }
    lappend FAQ(sections) $ref $section
    set FAQ(currentSection) $ref
    set FAQ($ref) {}
    set FAQ(label$ref) $label
    set FAQ(question) 0
    return $ref
}

proc FAQ {question answer {ref ""}} {
    global FAQ
    set sectionRef $FAQ(currentSection)
    incr FAQ(question)
    if {$FAQ(section)} {
	set label "$FAQ(section).$FAQ(question)"
    } else {
	set label "$FAQ(question)"
    }
    if {[string compare $ref ""] == 0} {
	set ref "Q$label"
    }
    if {[info exists FAQ(label$ref)]} {
	error "ref \"$ref\" not unique!"
    }
    lappend FAQ($sectionRef) $ref $question $answer
    set FAQ(label$ref) $label
    return $ref
}

proc FAQlink {ref {label ""}} {
    global FAQ
    if {[string compare $label ""] == 0} {
	set label %%FAQ$ref%%
    }
    return "<a href=\"#$ref\">$label</a>"
}

# Quotes chars that are special to regsub : To be implemented
proc FAQregQuote {str} {
    return $str
}

proc FAQlinkResolve {text} {
    global FAQ
    while {[regexp {%%FAQ(.+)%%} $text all ref]} {
	regsub "%%FAQ[FAQregQuote $ref]%%" $text [FAQregQuote $FAQ(label$ref)] text
    }
    return $text
}

proc FAQgenSec {ref section} {
    global FAQ page
    set result {}
    if {[info exists page(opendl)]} {
	append result "</dl>\n"
    }
    append result [FAQ_H2 "<a href=\"#I$ref\" name=\"$ref\">$FAQ(label$ref)</a>. $section"]
    append result "<dl>\n"
    set page(opendl) 1
    return $result
}

proc FAQgenQ {ref question answer} {
    global FAQ
    set result {}
    append result "<p><dt><a href=\"#I$ref\" name=\"$ref\">$FAQ(label$ref)</a>. <em>$question</em>\n"
    append result "<dd>[FAQlinkResolve $answer]\n"
    return $result
}
proc FAQgen {} {
    global FAQ page
    set result {}

    set hasSections $FAQ(section)
    # Table of content
    append result [FAQ_H2 "<a name=\"index\">FAQ Index</a>"]
    append result "<menu>\n"
    foreach {secRef section} $FAQ(sections) {
	if {$hasSections} {
	    append result "<li><a href=\"#$secRef\" name=\"I$secRef\">$FAQ(label$secRef)</a>.\
		    $section\n"
	    append result "<menu>\n"
	}
	foreach {qref question answer} $FAQ($secRef) {
	    append result "<li><a href=\"#$qref\" name=\"I$qref\">$FAQ(label$qref)</a>.\
		    $question\n"
	}
	if {$hasSections} {
	    append result "</menu>\n"
	}
    }
    append result "</menu>"

    # Actual content
    foreach {secRef section} $FAQ(sections) {
	if {$hasSections} {
	    append result [FAQgenSec $secRef $section]
	} else {
	    append result [FAQ_H2 "Questions and Answers"]
	    append result "<dl>\n"
	    set page(opendl) 1
	}
	foreach {qref question answer} $FAQ($secRef) {
	    append result [FAQgenQ $qref $question $answer]
	}
    }
    if {[info exists page(opendl)]} {
	append result "</dl>\n"
    }
    return $result
}

proc FAQ_H1 {title} {
    return "<H1>$title</H1>\n"
}

proc FAQ_H2 {title} {
    return "<H2>$title</H2>\n"
}
