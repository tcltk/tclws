#
# $Header: /cvsroot/aolserver/aolserver/examples/config/base.tcl,v 1.4 2007/08/01 21:35:26 michael_andrews Exp $
# $Name:  $
#
# base.tcl --
#
#     A simple example of a Tcl based AOLserver configuration file.
#
# Results:
#
#     HTTP (nssock):       
#
#         http://<address>:8000/
#
#     Server Page Root:      
#     
#         $AOLSERVER/servers/server1/pages
#
#     Server Access Log:
#
#         $AOLSERVER/servers/server1/modules/nslog/access.log
#
# Notes:
#
#     To start AOLserver, make sure you are in the AOLserver
#     installation directory, usually /usr/local/aolserver, and
#     execute the following command:
#
#     % bin/nsd -ft sample-config.tcl 
#

set server tclws
set home [file dirname [ns_info config]]
set pageRoot $home/servers/$server/pages


ns_section "ns/parameters"
    ns_param home $home
    ns_param logdebug true
    ns_param logusec true


ns_section "ns/mimetypes"
    ns_param default "*/*"
    ns_param .adp "text/html; charset=iso-8859-1"

ns_section "ns/encodings"
    ns_param adp "iso8859-1"

ns_section "ns/threads"
    ns_param stacksize [expr 128 * 1024]

ns_section "ns/servers"
    ns_param $server "$server"

ns_section "ns/server/$server"
    ns_param directoryfile "index.tcl,index.htm,index.html,index.adp"
    ns_param pageroot $pageRoot
    ns_param maxthreads 20
    ns_param minthreads 5
    ns_param maxconnections 20
    ns_param urlcharset "utf-8"
    ns_param outputcharset "utf-8"
    ns_param inputcharset "utf-8"
    ns_param enabletclpages true
    ns_param chunked true
    ns_param nsvbuckets 8
    ns_param errorminsize 514

ns_section "ns/server/$server/adp"
    ns_param map "/*.adp"
    ns_param   defaultparser  fancy ; # adp
    ns_param   cachesize      40

ns_section ns/server/${server}/adp/parsers
    ns_param   fancy    ".adp"

ns_section ns/server/${server}/tcl
ns_param   autoclose      "on" 
ns_param   debug          "true" ;# false
ns_param   nsvbuckets     "8"

ns_section "ns/server/$server/modules"
    ns_param nssock nssock.so
    ns_param nslog nslog.so
    ns_param nscp nscp.so
    ns_param nsrewrite nsrewrite.so
    ns_param tclws tcl

ns_section "ns/server/$server/module/nssock"
    ns_param location http://127.0.0.1:8080
    ns_param hostname 127.0.0.1:8080
    ns_param address 127.0.0.1
    ns_param port 8080

ns_section "ns/server/$server/module/nslog"
    ns_param rolllog true
    ns_param rollonsignal true
    ns_param rollhour 0
    ns_param maxbackup 2

ns_section "ns/server/$server/module/nscp"
    ns_param address "127.0.0.1"
    ns_param port 8081
    ns_param cpcmdlogging "false"

ns_section "ns/server/$server/module/nscp/users"
    ns_param user ":"
    ns_param user "tom:1.7MASVQIxdCA"

# Testing 
# Server url, Proxy & Redirects
ns_section "ns/server/${server}/redirects"
ns_param   404 "global/file-not-found.tcl"
#ns_param   403 "global/forbidden.html"
ns_param   500 "global/server-error.tcl"

