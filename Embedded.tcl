###############################################################################
##                                                                           ##
##  Copyright (c) 2016-2020, Harald Oehlmann                                 ##
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

package require Tcl 8.6

package require uri
package require base64
package require html
package require log

# Emulate the log::logsubst command introduced in log 1.4
if {![llength [info command ::log::logsubst]]} {
	proc ::log::logsubst {level text} {
		if {[::log::lvIsSuppressed $level]} {
			return
		}
		::log::log $level [uplevel 1 [list subst $text]]
	}
}

package provide WS::Embeded 3.0.0

namespace eval ::WS::Embeded {

    set portInfo {}

    set portList [list]
    set forever {}

    variable returnCodeText [dict create 200 OK 404 "Not Found"\
	    500 "Internal Server Error" 501 "Not Implemented"]
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::AddHandler
#
# Description : Register a handler for a url on a port.
#
# Arguments :
#       port     -- The port to register the callback on
#       url      -- The URL to register the callback for
#       callback -- The callback prefix, two additionally arguments are lappended
#                   the callback: (1) the socket (2) the null string
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
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
proc ::WS::Embeded::AddHandler {port url callback} {
    variable portInfo

    dict set portInfo $port handlers $url $callback
    return;
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::GetValue
#
# Description : Get a value found in this module
#
# Arguments :
#       index    -- type of value to get. Possible values:
#                    -- isHTTPS : true, if https protocol is used.
#       sock     -- concerned socket. May be ommitted, if not relevant for value.
#
# Returns :     the distinct value
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
#
# Original Author : Harald Oehlmann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#   2.7.0  2020-10-26  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::GetValue {index {sock ""}} {
    variable portInfo

    switch -exact -- $index {
        isHTTPS { return [dict get $portInfo $sock $index] }
        default {return -code error "Unknown index '$index'"}
    }
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::AddHandlerAllPorts
#
# Description : Register a handler for a url on all "defined" ports.
#
# Arguments :
#       url      -- List of three elements:
#       callback -- The callback prefix, two additionally argumens are lappended
#                   the callback: (1) the socket (2) the null string
#
# Returns :     Nothing
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
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
proc ::WS::Embeded::AddHandlerAllPorts {url callback} {
    variable portList

    foreach port $portList {
        AddHandler $port $url $callback
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
# Procedure Name : ::WS::Embeded::Listen
#
# Description : Instruct the module to listen on a Port, security information.
#
# Arguments :
#       port     -- Port number to listen on
#       certfile -- Name of the certificate file or a pfx archive for twapi
#       keyfile  -- Name of the key file
#                   To use twapi TLS, specify a list with the following elements:
#                    -- "-twapi": Flag, that TWAPI TLS should be used
#                    -- password: password of PFX file passed by
#                       [::twapi::conceal]. The concealing makes sure that the
#                       password is not readable in the error stack trace
#                    -- ?subject?: optional search string in pfx file, if
#                       multiple certificates are included.
#       userpwds -- A list of username:password
#       realm    -- The security realm
#
# Returns :     socket handle
#
# Side-Effects :
#       None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Embeded::Listen must have been called for the port
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
proc ::WS::Embeded::Listen {port {certfile {}} {keyfile {}} {userpwds {}} {realm {}}} {
    variable portInfo
    variable portList

    lappend portList $port
    foreach key {port userpwds realm} {
        dict set portInfo $port $key [set $key]
    }
    if {![dict exists $portInfo $port handlers]} {
        dict set portInfo $port handlers {}
    }
    set authlist {}
	foreach up $userpwds {
        lappend authlist [base64::encode $up]
    }
	dict set portInfo $port auths $authlist
    
    ##
    ## Check if HTTPS protocol is used
    ##
    
    dict set portInfo $port isHTTPS [expr {$certfile ne ""}]

    if {$certfile ne "" } {
        if { [string is list $keyfile] && [lindex $keyfile 0] eq "-twapi"} {
            
            ##
            ## Use TWAPI TLS
            ##
            
            package require twapi_crypto
            
            # Decode parameters
            #
            # certfile is the pfx file name
            # keyfile is a list of:
            #   -twapi: fix element
            #   password of the pfx file, passed by twapi::conceal
            #   Optional Subject of the certificate, if there are multiple
            #       certificates contained.
            #       If not given, the first certificate is used.
            set pfxpassword [lindex $keyfile 1]
            set pfxsubject ""
            if {[llength $keyfile] > 2} {
                set pfxsubject [lindex $keyfile 2]
            }
            # Create certificate selection tring
            if {$pfxsubject eq ""} {
                set pfxselection any
            } else {
                set pfxselection [list subject_substring $pfxsubject)]
            }
            
            set hFile [open $certfile rb]
            set PFXCur [read $hFile]
            close $hFile
            # Set up the store containing the certificates
            # Import the PFX file and search the certificate.
            set certstore [twapi::cert_temporary_store -pfx $PFXCur\
                    -password $pfxpassword]
            set servercert [twapi::cert_store_find_certificate $certstore\
                    {*}$pfxselection]
            if {"" eq $servercert} {
                # There was no certificate included in the pfx file
                catch {twapi::cert_store_release $certstore}
                return -code error "no certificate found in file '$certfile'"
            }
            # The following is catched to clean-up in case of any error
            if {![catch {
                # Start the TLS socket with the credentials
                set creds [twapi::sspi_schannel_credentials \
                        -certificates [list $servercert]\
                        -protocols [list ssl3 tls1.1 tls1.2]]
                set creds [twapi::sspi_acquire_credentials \
                        -credentials $creds -package unisp -role server]
                set handle [::twapi::tls_socket\
                        -server [list ::WS::Embeded::accept $port]\
                        -credentials $creds $port]
            } errormsg errordict]} {
                # All ok, clear error flag
                unset errormsg
            }
            # Clean up certificate and certificate store
            if {[info exists servercert]} {
                catch {twapi::cert_release $servercert}
            }
            catch {twapi::cert_store_release $certstore}
            # Return error if happened above
            if {[info exists errormsg]} {
                dict unset errordict -level
                return -options  $errordict $errormsg
            }
        } else {
            
            ##
            ## Use TCL Package
            ##
            
            package require tls
    
            ::tls::init \
                -certfile $certfile \
                -keyfile  $keyfile \
                -require 0 \
                -request 0
            set handle [::tls::socket -server [list ::WS::Embeded::accept $port] $port]
        }
    } else {
        
        ##
        ## Use http protocol without encryption
        ##
        
        ::log::logsubst debug {socket -server [list ::WS::Embeded::accept $port] $port}
        set handle [socket -server [list ::WS::Embeded::accept $port] $port]
    }

    return $handle
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Embeded::ReturnData
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
proc ::WS::Embeded::ReturnData {sock type data code} {
    upvar #0 ::WS::Embeded::Httpd$sock dataDict

    foreach var {type data code} {
        dict set dataDict reply $var [set $var]
    }
    return;
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::respond
#
# Description : Send response back to user.
#
# Arguments :
#       sock -- Socket to send reply on
#       code -- Code to send
#       body -- HTML body to send
#       head -- Additional HTML headers to send
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
#   2.3.0  11/06/2012  H.Oehlmann   Separate head and body,
#                                   correct Content-length
#
#
###########################################################################
proc ::WS::Embeded::respond {sock code body {head ""}} {
    set body [encoding convertto iso8859-1 $body\r\n]
    chan configure $sock -translation crlf
    puts $sock "[httpreturncode $code]\nContent-Type: text/html; charset=ISO-8859-1\nConnection: close\nContent-length: [string length $body]"
    if {"" ne $head} {
	puts -nonewline $sock $head
    }
    # Separator head and body
    puts $sock ""
    chan configure $sock -translation binary
    puts -nonewline $sock $body
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::httpreturncode
#
# Description : Format the first line of a http return including the status code
#
# Arguments :
#       code -- numerical http return code
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
#       1  10/05/2012  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::httpreturncode {code} {
    variable returnCodeText
    if {[dict exist $returnCodeText $code]} {
	set textCode [dict get $returnCodeText $code]
    } else {
	set textCode "???"
    }
    return "HTTP/1.0 $code $textCode"
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::checkauth
#
# Description : Check to see if the user is allowed.
#
# Arguments :
#       port -- Port number
#       sock -- Incoming socket
#       ip   -- Requester's IP address
#       auth -- Authentication information
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
proc ::WS::Embeded::checkauth {port sock ip auth} {
    variable portInfo

    if { [dict exists $portInfo $port auths] &&
            0 != [llength [dict get $portInfo $port auths]] &&
            $auth ni [dict get $portInfo $port auths] } {
        set realm [dict get $portInfo $port realm]
        respond $sock 401 "" "WWW-Authenticate: Basic realm=\"$realm\"\n"
        ::log::logsubst warning {Unauthorized from $ip}
        return -code error
    }
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::handler
#
# Description : Handle a request.
#
# Arguments :
#       port        -- Port number
#       sock        -- Incoming socket
#       ip          -- Requester's IP address
#       auth        -- Authentication information
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
#   2.3.0  10/31/2012  G.Lester     bug fix for [68310fe3bd] -- correct encoding and data length
#   2.6.1  2020-10-22  H.Oehlmann   Do not pass parameter reqstring.
#                                   The corresponding value is found in global
#                                   array anyway.
#                                   Use charset handler of request decoding.
#   2.7.0  2020-10-26  H.Oehlmann   Pass additional port parameter to handle functions.
#                                   This helps to get isHTTPS status for WSDL.
#
#
###########################################################################
proc ::WS::Embeded::handler {port sock ip auth} {
    variable portInfo
    upvar #0 ::WS::Embeded::Httpd$sock dataDict

    if {[catch {checkauth $port $sock $ip $auth}]} {
        ::log::log warning {Auth Failed}
        return
    }

    set path "/[string trim [dict get $dataDict path] /]"
    if {[dict exists $portInfo $port handlers $path]} {
        set cmd [dict get $portInfo $port handlers $path]
        lappend cmd $sock $port
        # ::WS::Server::callOperation is called (for operations).
        # This routine reads our data by:
        #   upvar #0 ::WS::Embeded::Httpd$sock data
        # while only the array index (query) is used
        if {[catch {eval $cmd} msg]} {
            ::log::log error "Return 404 due to eval error: $msg"
            respond $sock 404 "Error: $msg"
        } else {
            set type [dict get $dataDict reply type]
            # This may modify the type variable, if encoding is not found
            set encoding [contentTypeParse 0 type]
            set data [encoding convertto $encoding [dict get $dataDict reply data]]
            set reply "[httpreturncode [dict get $dataDict reply code]]\n"
            append reply "Content-Type: $type\n"
            append reply "Connection: close\n"
            append reply "Content-length: [string length $data]\n"
            chan configure $sock -translation crlf
            puts $sock $reply
            chan configure $sock -translation binary
            puts -nonewline $sock $data
            ::log::log debug ok
        }
    } else {
        ::log::log warning "404 Error: URL not found"
        respond $sock 404 "URL not found"
    }

    return;
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::accept
#
# Description : Accept an incoming connection.
#
# Arguments :
#       port        -- Port number
#       sock        -- Incoming socket
#       ip          -- Requester's IP address
#       clientport  -- Requester's port number
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
#   2.3.0  10/31/2012  G.Lester     Bug fix [66fb3aeef5] -- correct header parsing
#   2.6.1  2020-10-22  H.Oehlmann   Honor received encoding.
#                                   Only pass request data by global array
#                                   to the handler.
#
#
###########################################################################
proc ::WS::Embeded::accept {port sock ip clientport} {

    upvar #0 ::WS::Embeded::Httpd$sock dataDict
    ::log::logsubst info {Receviced request on $port for $ip:$clientport}

    chan configure $sock -translation crlf
    if {1 == [catch {
        gets $sock line
        ::log::logsubst debug {Request is: $line}
        set auth {}
        set request {}
        while {[gets $sock temp] > 0 && ![eof $sock]} {
            if {[regexp {^([^:]*):(.*)$} $temp -> key data]} {
                dict set request header [string tolower $key] [string trim $data]
            }
        }
        if {[eof $sock]} {
            ::log::logsubst warning  {Connection closed from $ip}
            return
        }
        if {[dict exists $request header authorization]} {
            regexp -nocase {^basic +([^ ]+)$}\
                [dict get $request header authorization] -> auth
        }
        if {![regexp {^([^ ]+) +([^ ]+) ([^ ]+)$} $line -> method url version]} {
            ::log::logsubst warning  {Wrong request: $line}
            return
        }
        
        ##
        ## Process passed http method
        ##
        
        switch -exact -- $method {
            POST {
                ##
                ## This is all broken and needs to be fixed
                ##
                set data ""
                if {[dict exists $request header transfer-encoding]
                    && [dict get $request header transfer-encoding] eq "chunked"} {
                    # Receive chunked request body.
                    while {[scan [gets $sock line] %x length] == 1 && $length > 0} {
                        chan configure $sock -translation binary
                        append data [read $sock $length]
                        chan configure $sock -translation crlf
                    }
                } else {
                    # Receive non-chunked request body.
                    chan configure $sock -translation binary
                    set data [read $sock [dict get $request header content-length]]
                    chan configure $sock -translation crlf
                }
                set dataDict [uri::split $url]
                if {![dict exists $request header content-type]} {
                    ::log::logsubst warning  {Header missing: 'Content-Type' from $ip}
                    return
                }
                set contentType [dict get $request header content-type]
                set requestEncoding [contentTypeParse 1 contentType]
                dict set dataDict query [encoding convertfrom $requestEncoding $data]
                dict set dataDict headers $request
                dict set dataDict ipaddr $ip
                handler $port $sock $ip $auth
            }
            GET {
                set dataDict [uri::split $url]
                handler $port $sock $ip $auth
            }
            default {
                ::log::logsubst warning {Unsupported method '$method' from $ip}
                respond $sock 501 "Method not implemented"
            }
        }
    } msg]} {
        ::log::log error "Error: $msg"
        # catch this against an eventual closed socket
        catch {respond $sock 500 "Server Error"}
    }

    catch {flush $sock}
    catch {close $sock}
    unset -nocomplain dataDict
    return
}




###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Embeded::contentTypeParse
#
# Description : Parse a content-type value and get the encoding.
#               When receiving, only the encoding is required.
#               When sending, we have to correct the encoding, if not known
#               by TCL. Thus, the content-type string is changed.
#
# Arguments :
#       fReceiving  -- When receiving, we only need the extracted codepage.
#                       If sending, the content-type string must be modified,
#                       if the codepage is not found in tcl
#       contentTypeName --  The variable containing the content type string.
#
# Returns :
#       tcl encoding to apply
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Harald Oehlmann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#   2.6.1  2020-10-22  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Embeded::contentTypeParse {fReceiving contentTypeName} {

    upvar 1 $contentTypeName contentType

    ##
    ## Extract charset parameter from content-type header
    ##

    # content-type example content: text/xml;charset=utf-8
    set paramList [lassign [split $contentType ";"] typeOnly]
    foreach parameterCur $paramList {
        set parameterCur [string trim $parameterCur]
        # Check for 'charset="<data>', where data may contain '\"'
        if {[regexp -nocase {^charset\s*=\s*\"((?:[^""]|\\\")*)\"$}\
                $parameterCur -> requestEncoding]
        } {
            set requestEncoding [string map {{\"} \"} $requestEncoding]
            break
        } else {
            # check for 'charset=<data>'
            regexp -nocase {^charset\s*=\s*(\S+?)$}\
                    $parameterCur -> requestEncoding
            break
        }
    }

    ##
    ## Find the corresponding TCL encoding name
    ##

    if {[info exists requestEncoding]} {
        if {[llength [info commands ::http::CharsetToEncoding]]} {
            # Use private http package routine
            set requestEncoding [::http::CharsetToEncoding $requestEncoding]
            # Output is "binary" if not found
            if {$requestEncoding eq "binary"} {
                unset requestEncoding
            }
        } else {
            # Reduced version of the http package version only honoring ISO8859-x
            # and encoding names identical to tcl encoding names
            set requestEncoding [string tolower $requestEncoding]
            if {[regexp {iso-?8859-([0-9]+)} $requestEncoding -> num]} {
                set requestEncoding "iso8859-$num"
            }
            if {$requestEncoding ni [encoding names]} {
                unset requestEncoding
            }
        }
    }

    ##
    ## Output found encoding and eventually content type
    ##

    # If encoding was found, just return it
    if {[info exists requestEncoding]} {
        return $requestEncoding
    }

    # encoding was not found
    if {$fReceiving} {
        # This is the http default so use that
        ::log::logsubst information {Use default encoding as content type header has missing/unknown charset in '$contentType'}
        return iso8859-1
    }
    
    # When sending, be sure to cover all characters, so use utf-8
    # correct content-type string (upvar)
    ::log::logsubst information {Set send charset to utf-8 due missing/unknown charset in '$contentType'}
    if {[info exists typeOnly]} {
        set contentType "${typeOnly};charset=utf-8"
    } else {
        set contentType "text/xml;charset=utf-8"
    }
    return utf-8
}
