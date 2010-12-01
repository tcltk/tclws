namespace eval ::log {

    proc log {level args} {
	::ns_log [string totitle $level] [join $args " "]
    }

    namespace export *
}

package provide log 2.4.0
