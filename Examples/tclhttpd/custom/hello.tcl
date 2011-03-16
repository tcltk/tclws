# Trivial application-direct URL for "/hello"
# The URLs under /hello are implemented by procedures that begin with "::hello::"

Direct_Url /hello	::hello::

namespace eval ::hello {
    variable x [clock format [clock seconds]]
}

# ::hello::/ --
#
#	This implements /hello/

proc ::hello::/ {args} {
    return "Hello, World!"
}

# ::hello::/there --
#
#	This implements /hello/there

proc ::hello::/there {args} {
    variable x
    return "Hello, World!<br>\nThe server started at $x"
}
