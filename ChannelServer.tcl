###############################################################################
##                                                                           ##
##  Copyright (c) 2008, Gerald W. Lester                                     ##
##  All rights reserved.                                                     ##
##                                                                           ##
##  Redistribution and use in source and binary forms, with or without       ##
##  modification, are permitted provided that the following conditions       ##
##  are met:                                                                 ##
##                                                                           ##
##    * Redistributions of source code must retain the above copyright       ##
##      notice, this list of conditions and the following disclaimer.        ##
##    * Redistributions in binary form must reproduce the above              ##
##      copyright notice, this list of conditions and the following          ##
##      disclaimer in the documentation and/or other materials provided      ##
##      with the distribution.                                               ##
##    * Neither the name of the Visiprise Software, Inc nor the names        ##
##      of its contributors may be used to endorse or promote products       ##
##      derived from this software without specific prior written            ##
##      permission.                                                          ##
##                                                                           ##
##  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS      ##
##  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT        ##
##  LIMITED  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       ##
##  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE           ##
##  COPYRIGHT OWNER OR  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     ##
##  INCIDENTAL, SPECIAL,  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    ##
##  BUT NOT LIMITED TO,  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        ##
##  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER         ##
##  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT       ##
##  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR  OTHERWISE) ARISING IN       ##
##  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF  ADVISED OF THE         ##
##  POSSIBILITY OF SUCH DAMAGE.                                              ##
##                                                                           ##
###############################################################################

package require Tcl 8.4
# XXX WS::Utils usable here? (provide dict, lassign)
if {![llength [info command dict]]} {
    package require dict
}
if {![llength [info command lassign]]} {
    proc lassign {inList args} {
        set numArgs [llength $args]
        set i -1
        foreach var $args {
            incr i
            uplevel 1 [list set $var [lindex $inList $i]]
        }
        return [lrange $inList $numArgs end]
    }
}

package require uri
package require base64
package require html

package provide WS::Channel 2.4.0

namespace eval ::WS::Channel {

    array set portInfo {}
    array set dataArray {}
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Channel::AddHandler
#
# Description : Register a handler for a url on a port.
#
# Arguments :
#       ports     -- The port to register the callback on
#       operation -- {} for WSDL callback, otherwise operation callback
#       callback  -- The callback prefix, two additionally arguments are lappended
#                    the callback: (1) the socket (2) the null string
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Channel::Listen must have been called for the port
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Channel::AddHandler {ports operation callback} {
    variable portInfo

    if {[llength $ports] == 2} {
        lassign $ports in out
        set portInfo(in) $in
        set portInfo(out) $out
        set portInfo(eof) [lindex [fconfigure $portInfo(out) -eofchar] end]
    } elseif {[llength $ports] == 1} {
        set portInfo(in) $ports
        set portInfo(out) $ports
        set portInfo(eof) [fconfigure $portInfo(out) -eofchar]
    } else {
        return -code error -errorcode [list ] "Invalid channel count {$ports}"
    }
    if {[string length $operation]} {
        set portInfo(op) $callback
    } else {
        set portInfo(wsdl) $callback
    }
    return;
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Channel::ReturnData
#
# Description : Store the information to be returned.
#
# Arguments :
#       socket  -- Socket data is for
#       type    -- Mime type of data
#       data    -- Data
#       code    -- Status code
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : A callback on the socket should be pending
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Channel::ReturnData {sock type data code} {
    variable dataArray

    foreach var {type data code} {
        dict set dataArray(reply) $var [set $var]
    }
    return;
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Channel::Start
#
# Description : Start listening on all ports (i.e. enter the event loop).
#
# Arguments : None
#
# Returns :   Value that event loop was exited with.
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions :
#        ::WS::Channel::Listen should have been called for one or more port.
#
#
# Original Author : Gerald W. Lester
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Channel::Start {} {
    variable portInfo
    variable dataArray

    while {1} {
        array unset dataArray
        set xml [read $portInfo(in)]
        if {[string length $xml]} {
            ##
            ## Call for an operation
            ##
            handler op $xml
        } else {
            ##
            ## Call for a WSDL
            ##
            handler wsdl {}
        }
    }
}



###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Channel::respond
#
# Description : Send response back to user.
#
# Arguments :
#       sock -- Socket to send reply on
#       code -- Code to send
#       body -- HTML body to send
#       head -- HTML header to send
#
# Returns :
#       Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Channel::respond {sock code body {head ""}} {
    puts -nonewline $sock "HTTP/1.0 $code ???\nContent-Type: text/html; charset=ISO-8859-1\nConnection: close\nContent-length: [string length $body]\n$head\n$body"
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Channel::handler
#
# Description : Handle a request.
#
# Arguments :
#       type        -- Request type
#       xml         -- XML
#
# Returns :
#       Nothing
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Gerald W. Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/28/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Channel::handler {type xml} {
    variable portInfo
    variable dataArray
    upvar #0 Httpd_Channel data


    set ::errorInfo {}
    set data(query) $xml
    set cmd $portInfo($type)
    lappend cmd _Channel {}
    puts "Calling {$cmd}"
    if {[catch {eval $cmd} msg]} {
        respond $portInfo(out) 404 Error $msg
    } else {
        set data [dict get $dataArray(reply) data]
        set reply "HTTP/1.0 [dict get $dataArray(reply) code] ???\n"
        append reply "Content-Type: [dict get $dataArray(reply) type]; charset=UTF-8\n"
        append reply "Connection: close\n"
        append reply "Content-length: [string length $data]\n"
        append reply "\n"
        append reply $data
        puts -nonewline $portInfo(out) $reply
    }
    puts -nonewline $portInfo(eof)

    return;
}
