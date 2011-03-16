# Require WS::AOLserver and record version
# Note: All required packages must be in a lib directory.

namespace eval ::WS::AOLserver {
    
    variable logVersion [ns_ictl package require log]
    variable wsVersion  [ns_ictl package require WS::Server]
    variable version    [ns_ictl package require WS::AOLserver]
}

ns_register_filter preauth GET /*/wsdl ::ws_aolserver_redirect wsdl
ns_register_filter preauth POST /*/op  ::ws_aolserver_redirect op

proc ::ws_aolserver_redirect { why } {

    set urlv [split [ns_conn url] /]

    set new_url "[join [lrange $urlv 0 end-1] /]/index.tcl"
    ns_log Notice "WS::AOLserver::Redirect: from [lindex $urlv end] to '$new_url'"
    ns_rewriteurl $new_url

    return filter_ok
}
