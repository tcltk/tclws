###############################################################################
##                                                                           ##
##  Copyright (c) 2006-2013, Gerald W. Lester                                ##
##  Copyright (c) 2008, Georgios Petasis                                     ##
##  Copyright (c) 2006, Visiprise Software, Inc                              ##
##  Copyright (c) 2006, Colin McCormack                                      ##
##  Copyright (c) 2006, Rolf Ade                                             ##
##  Copyright (c) 2001-2006, Pat Thoyts                                      ##
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
package require WS::Utils 2.3.2 ; # provides dict
package require html
package require log
package require tdom

package provide WS::Server 2.3.3

namespace eval ::WS::Server {
    array set ::WS::Server::serviceArr {}
    set ::WS::Server::procInfo {}
    set ::WS::Server::mode {}
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Server::Service
#
# Description : Declare a Web Service, the following URLs will exist
#                 /service/<ServiceName>
#                       Displays an HTML page describing the service
#                 /service/<ServiceName>/wsdl
#                       Returns a WSDL describing the service
#                 /service/<ServiceName>/op
#                       Invoke an operation
#
# Arguments : this procedure uses position independent arguments, they are:
#               -host           - The host name for this service.
#                                 Defaults to "ip:port" in embedded mode,
#                                 and to "localhost" otherwise.
#               -description    - The HTML description for this service
#               -service        - The service name (this will also be used for
#                                 the Tcl namespace of the procedures that implement
#                                 the operations.
#               -premonitor     - This is a command prefix to be called before
#                                 an operation is called.  The following arguments are
#                                 added to the command prefix:
#                                     PRE serviceName operationName operArgList
#               -postmonitor    - This is a command prefix to be called after
#                                 an operation is called.  The following arguments are
#                                 added to the command prefix:
#                                     POST serviceName operationName OK|ERROR results
#               -inheaders      - List of input header types.
#               -outheaders     - List of output header types.
#               -intransform    - Inbound (request) transform procedure.
#               -outtransform   - Outbound (reply) transform procedure.
#               -checkheader    - Command prefix to check headers.
#                                     If the call is not to be allowed, this command
#                                     should raise an error.
#                                     The signature of the command must be:
#                                       cmd \
#                                           service \
#                                           operation \
#                                           caller_ipaddr \
#                                           http_header_list \
#                                           soap_header_list
#               -mode           - Mode that service is running in.  Must be one of:
#                                   tclhttpd  -- running inside of tclhttpd or an
#                                                environment that supplies a
#                                                compatible Url_PrefixInstall
#                                                and Httpd_ReturnData commands
#                                   embedded  -- using the ::WS::Embedded package
#                                   aolserver -- using the ::WS::AolServer package
#                                   wub       -- using the ::WS::Wub package
#                                   wibble    -- running inside of wibble
#                                   rivet     -- running inside Apache Rivet (mod_rivet)
#                                   channel   -- use a channel pair, WSDL is return if no XML
#                                                otherwise an operation is called
#               -ports          - List of ports.  Only valid for embedded and channel mode
#                                   For chanel mode. Defaults to {stdin stdout}
#                                               NOTE -- a call should be to
#                                                      ::WS::Channel::Start to process data
#                                   For embedded mode. Default: 80
#                                               NOTE -- a call should be to
#                                                      ::WS::Embedded::Listen
#                                                      for each port in this
#                                                      list prior to this call
#               -prefix         - Path prefix used for the namespace and endpoint
#                                 Defaults to "/service/" plus the service name
#               -traceEnabled   - Boolean to enable/disable trace being passed back in exception
#                                 Defaults to "Y"
#               -docFormat      - Format of the documentation for operations ("text" or "html").
#                                 Defaults to "text"
#               -stylesheet     - The CSS stylesheet URL used in the HTML documentation
#
#
# Returns :     Nothing
#
# Side-Effects :        None
#
# Exception Conditions :
#       MISSREQARG -- Missing required arguments
#
# Pre-requisite Conditions : None
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::Service {args} {
    variable serviceArr
    variable procInfo
    variable mode

    ::log::log debug "Defining Service as $args"

    array set defaults {
        -description    {}
        -checkheader    {::WS::Server::ok}
        -inheaders      {}
        -outheaders     {}
        -intransform    {}
        -outtransform   {}
        -htmlhead       {TclHttpd Based Web Services}
        -author         {}
        -description    {}
        -mode           {tclhttpd}
        -ports          {80}
        -traceEnabled   {Y}
        -docFormat      {text}
        -stylesheet     {}
    }
    array set defaults $args
    if {[string equal $defaults(-mode) channel]} {
        set defaults(-ports) {stdin stdout}
        array set defaults $args
    }
    set requiredList {-service}
    set missingList {}
    foreach opt $requiredList {
        if {![info exists defaults($opt)]} {
            lappend missingList $opt
        }
    }
    if {[llength $missingList]} {
        return \
            -code error \
            -errorcode [list WSSERVER MISSREQARG $missingList] \
            "Missing required arguments '[join $missingList {,}]'"
    }
    set service $defaults(-service)
    if {![info exists defaults(-prefix)]} {
        set defaults(-prefix) /service/$service
    }
    # find default host
    if {![info exists defaults(-host)]} {
	switch -exact -- $defaults(-mode) {
	    embedded {
		set me [socket -server garbage_word -myaddr [info hostname] 0]
    		set defaults(-host) [lindex [fconfigure $me -sockname] 0]
	    	close $me
	    	if {0 !=[llength $defaults(-ports)] && 80 != [lindex $defaults(-ports) 0]} {
        	    append defaults(-host) ":[lindex $defaults(-ports) 0]"
	    	}
            }
	    default {
	    	set defaults(-host) localhost
            }
	}
    }

    set defaults(-uri) $service
    namespace eval ::$service {}
    set serviceArr($service) [array get defaults]
    if {![dict exists $procInfo $service operationList]} {
        dict set procInfo $service operationList {}
    }
    set mode $defaults(-mode)

    ##
    ## Install wsdl doc
    ##
    interp alias {} ::WS::Server::generateInfo_${service} \
                 {} ::WS::Server::generateInfo ${service}
    ::log::log debug "Installing Generate info for $service at $defaults(-prefix)"
    switch -exact -- $mode {
        embedded {
            package require WS::Embeded 2.1.3
            foreach port $defaults(-ports) {
                ::WS::Embeded::AddHandler $port $defaults(-prefix) ::WS::Server::generateInfo_${service}
            }
        }
        tclhttpd {
            ::Url_PrefixInstall $defaults(-prefix) ::WS::Server::generateInfo_${service}  \
                -thread 0
        }
        wub {
            package require WS::Wub
        }
        aolserver {
	    package require WS::AOLserver
        }
        rivet {
            package require Rivet
        }
        wibble {
            ##
            ## Define zone handler - get code from andy
            ##
            proc ::wibble::webservice {state} {
                dict with state options {}
                switch -exact -- $suffix {
                    "" - \
                    / {
                        ::WS::Server::generateInfo $name 0 response
                        sendresponse $response
                    } /op {
                        ::WS::Server::callOperation $name 0 [dict get $state request] response
                        sendresponse $response
                    } /wsdl {
                        ::WS::Server::generateWsdl $name 0 response
                        sendresponse $response
                    }
                    default {
                        ## Do nothing
                    }
                }
            }
            if {[package present Wibble] eq "0.1"} {
                proc ::WS::Wibble::ReturnData {responseDictVar type text status} {
                    upvar 1 $responseDictVar responseDict
                    dict set responseDict header content-type $type
                    dict set responseDict content $text
                    dict set responseDict status $status
                }
            } else {
                proc ::WS::Wibble::ReturnData {responseDictVar type text status} {
                    upvar 1 $responseDictVar responseDict
                    dict set responseDict header content-type "" $type
                    dict set responseDict content $text
                    dict set responseDict status $status
                }
            }

            ::wibble::handle $defaults(-prefix) webservice name $service
        }
        default {
            return \
                -code error \
                -errorcode [list WSSERVER UNSUPMODE $mode] \
                "-mode '$mode' not supported"
        }
    }


    ##
    ## Install wsdl
    ##
    interp alias {} ::WS::Server::generateWsdl_${service} \
                 {} ::WS::Server::generateWsdl ${service}
    ::log::log debug "Installing GenerateWsdl info for $service at $defaults(-prefix)/wsdl"
    switch -exact -- $mode {
        embedded {
            foreach port $defaults(-ports) {
                ::WS::Embeded::AddHandler $port $defaults(-prefix)/wsdl ::WS::Server::generateWsdl_${service}
            }
        }
        channel {
            package require WS::Channel
            ::WS::Channel::AddHandler $defaults(-ports) {} ::WS::Server::generateWsdl_${service}
        }
        tclhttpd {
            ::Url_PrefixInstall $defaults(-prefix)/wsdl ::WS::Server::generateWsdl_${service} \
                -thread 0
        }
        default {
            ## Do nothing
        }
    }

    ##
    ## Install operations
    ##
    interp alias {} ::WS::Server::callOperation_${service} \
                 {} ::WS::Server::callOperation ${service}
    ::log::log debug "Installing callOperation info for $service at $defaults(-prefix)/op"
    switch -exact -- $mode {
        embedded {
            foreach port $defaults(-ports) {
                ::WS::Embeded::AddHandler $port $defaults(-prefix)/op ::WS::Server::callOperation_${service}
            }
        }
        channel {
            package require WS::Channel
            ::WS::Channel::AddHandler $defaults(-ports) {op} ::WS::Server::callOperation_${service}
        }
        tclhttpd {
            ::Url_PrefixInstall $defaults(-prefix)/op ::WS::Server::callOperation_${service} \
                -thread 1
        }
        default {
            ## Do nothing
        }
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
# Procedure Name : ::WS::Server::ServiceProc
#
# Description : Register an operation for a service and declare the procedure to handle
#               the operations.
#
# Arguments :
#       ServiceName     -- Name of the service this operation is for
#       NameInfo        -- List of three elements:
#                               1) OperationName -- the name of the operation
#                               2) ReturnType    -- the type of the procedure return,
#                                                   this can be a simple or complex type
#                               3) Description   -- description of the return method
#       Arglist         -- List of argument definitions,
#                           each list element must be of the form:
#                               1) ArgumentName -- the name of the argument
#                               2) ArgumentTypeInfo -- A list of:
#                                     {type typeName comment commentString}
#                                          typeName can be any simple or defined type.
#                                          commentString is a quoted string describing the field.
#       Documentation   -- HTML describing what this operation does
#       Body            -- The tcl code to be called when the operation is invoked. . This
#                             code should return a dictionary with <OperationName>Result as a
#                             key and the operation's result as the value.
#
# Returns :     Nothing
#
# Side-Effects :
#       A procedure named "<ServiceName>::<OperationName>" defined
#       A type name with the name <OperationName>Result is defined.
#
# Exception Conditions : None
#
# Pre-requisite Conditions : ::WS::Server::Server must have been called for the ServiceName
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::ServiceProc {service nameInfo arglist documentation body} {
    variable procInfo

    set name [lindex $nameInfo 0]
    ::log::log debug "Defining operation $name for $service"
    set argOrder {}
    ::log::log debug "\targs are {$arglist}"
    foreach {arg data} $arglist {
        lappend argOrder $arg
    }
    if {![dict exists $procInfo $service op$name argList]} {
        set tmpList [dict get $procInfo $service operationList]
        lappend tmpList $name
        dict set procInfo $service operationList $tmpList
    }
    dict set procInfo $service op$name argList $arglist
    dict set procInfo $service op$name argOrder $argOrder
    dict set procInfo $service op$name docs $documentation
    dict set procInfo $service op$name returnInfo [lindex $nameInfo 1]
    set typeInfo [dict get $procInfo $service op$name returnInfo]
    ::WS::Utils::ServiceTypeDef Server $service ${name}Results [list ${name}Result $typeInfo]
    ::WS::Utils::ServiceTypeDef Server $service ${name}Request $arglist
    set tclArgList {}
    foreach {arg typeinfo} $arglist {
        lappend tclArgList [lindex $arg 0]
    }
    proc ::${service}::${name} $tclArgList $body

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::GetWsdl
#
# Description : Generates a WSDL for a registered service.
#
# Arguments :
#       serviceName     - The name of the service
#       urlPrefix       - (optional) Prefix to use for location; defaults to http://<host>
#
# Returns :
#       XML for the WSDL
#
# Side-Effects : None
#
# Exception Conditions :
#       WS SERVER UNKSERV - Unknown service name
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
#       1  07/06/2006  G.Lester     Initial version
#       2  12/12/2009  W.Kocjan     Support for services over SSL in Tclhttpd
#
#
###########################################################################
proc ::WS::Server::GetWsdl {serviceName {urlPrefix ""}} {
    variable serviceArr
    variable procInfo

    array set serviceData $serviceArr($serviceName)

    set operList [lsort -dictionary [dict get $procInfo $serviceName operationList]]
    ::log::log debug "Generating WSDL for $serviceName"
    if {![info exists serviceArr($serviceName)]} {
        set msg "Unknown service '$serviceName'"
        ::return \
            -code error \
            -errorCode [list WS SERVER UNKSERV $serviceName] \
            $msg
    }

    set msg {}
    set reply [::dom createDocument wsdl:definitions]
    $reply documentElement definition
    $definition setAttribute \
        xmlns:wsdl      "http://schemas.xmlsoap.org/wsdl/" \
        xmlns:http      "http://schemas.xmlsoap.org/wsdl/http/" \
        xmlns:mime      "http://schemas.xmlsoap.org/wsdl/mime/" \
        xmlns:xs         "http://www.w3.org/2001/XMLSchema" \
        xmlns:soap      "http://schemas.xmlsoap.org/wsdl/soap/" \
        xmlns:soapenc   "http://schemas.xmlsoap.org/soap/encoding/" \
        xmlns:${serviceName} "http://$serviceData(-host)$serviceData(-prefix)" \
        targetNamespace "http://$serviceData(-host)$serviceData(-prefix)"

    foreach topLevel {types} {
        $definition appendChild [$reply createElement wsdl:$topLevel $topLevel]
    }

    ##
    ## Messages
    ##

    ## Operations
    foreach oper $operList {
        $definition appendChild [$reply createElement wsdl:message input]
        $input setAttribute name ${oper}In
        $input appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${oper}Request
        $definition appendChild [$reply createElement wsdl:message output]
        $output setAttribute name ${oper}Out
        $output appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${oper}Results
    }

    ## Input headers
    foreach headerType $serviceData(-inheaders) {
        $definition appendChild [$reply createElement wsdl:message header]
        $header setAttribute name $headerType
        $header appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${headerType}
    }

    ## Output headers
    foreach headerType $serviceData(-outheaders) {
        $definition appendChild [$reply createElement wsdl:message header]
        $header setAttribute name $headerType
        $header appendChild [$reply createElement wsdl:part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${headerType}
    }

    ##
    ## Add the rest of the toplevels in
    ##
    foreach topLevel {portType binding service} {
        $definition appendChild [$reply createElement wsdl:$topLevel $topLevel]
    }

    ##
    ## Service
    ##
    $service setAttribute name $serviceName

    $service appendChild [$reply createElement wsdl:documentation documentation]
    $documentation appendChild [$reply createTextNode $serviceData(-description)]

    $service appendChild [$reply createElement wsdl:port port]
    $port setAttribute \
        name ${serviceName}Soap \
        binding ${serviceName}:${serviceName}Soap

    $port appendChild [$reply createElement soap:address address]

    if {$urlPrefix == ""} {
        set urlPrefix "http://$serviceData(-host)"
    }

    $address setAttribute  \
        location "$urlPrefix$serviceData(-prefix)/op"


    ##
    ## Bindings
    ##
    $binding setAttribute \
        name ${serviceName}Soap \
        type ${serviceName}:${serviceName}Soap
    $binding appendChild [$reply createElement soap:binding b2]
    $b2 setAttribute\
        style document \
        transport "http://schemas.xmlsoap.org/soap/http"

    foreach oper $operList {
        $binding appendChild [$reply createElement wsdl:operation operNode]
        $operNode setAttribute name $oper
        $operNode appendChild [$reply createElement soap:operation o2]
        $o2 setAttribute \
            soapAction $serviceName:$oper \
            style document
        ## Input message
        $operNode appendChild [$reply createElement wsdl:input input]
        $input appendChild [$reply createElement soap:body tmp]
        $tmp setAttribute use literal
        foreach headerType $serviceData(-inheaders) {
            $operNode appendChild [$reply createElement wsdl:header header]
            $header appendChild [$reply createElement soap:header tmp]
            $tmp setAttribute \
                use literal \
                message ${serviceName}:${headerType} \
                part $headerType
        }
        ## Output message
        $operNode appendChild [$reply createElement wsdl:output output]
        $output appendChild [$reply createElement soap:body tmp]
        $tmp setAttribute use literal
        foreach headerType $serviceData(-outheaders) {
            $operNode appendChild [$reply createElement wsdl:header header]
            $header appendChild [$reply createElement soap:header tmp]
            $tmp setAttribute \
                use literal \
                message ${serviceName}:${headerType} \
                part $headerType
        }
    }

    ##
    ## Ports
    ##
    $portType setAttribute name ${serviceName}Soap
    foreach oper $operList {
        $portType appendChild [$reply createElement wsdl:operation operNode]
        $operNode setAttribute name $oper
        $operNode appendChild [$reply createElement wsdl:input input]
        $input setAttribute message ${serviceName}:${oper}In
        $operNode appendChild [$reply createElement wsdl:output output]
        $output setAttribute message ${serviceName}:${oper}Out
    }

    ##
    ## Types
    ##
    GenerateScheme Server $serviceName $reply $types

    append msg \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$reply asXML  \
            -indent 4 \
            -escapeNonASCII \
            -doctypeDeclaration 0 \
        ]
    $reply delete
    return $msg
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateWsdl
#
# Description : Generates a WSDL for a registered service.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#       2  12/12/2009  W.Kocjan     Support for services over SSL in Tclhttpd
#
#
###########################################################################
proc ::WS::Server::generateWsdl {serviceName sock args} {
    variable serviceArr
    variable procInfo
    variable mode

    array set serviceData $serviceArr($serviceName)

    set operList [lsort -dictionary [dict get $procInfo $serviceName operationList]]
    ::log::log debug "Generating WSDL for $serviceName on $sock with {$args}"
    if {![info exists serviceArr($serviceName)]} {
        set msg "Unknown service '$serviceName'"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData \
                    $sock \
                    "text/html; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            embedded {
                ::WS::Embeded::ReturnData \
                    $sock \
                    "text/html; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            channel {
                ::WS::Channel::ReturnData \
                    $sock \
                    "text/xml; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            rivet {
                headers type text/html
                headers numeric 404
                puts "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>"
            }
	    aolserver {
                ::WS::AOLserver::ReturnData \
                    $sock \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            wibble  {
                upvar 1 [lindex $args 0] responseDict
                ::WS::Wibble::ReturnData responseDict text/html "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" 404
            }
            default {
                ## Do nothing
            }
        }
        return 1
    }


    switch -exact -- $mode {
        tclhttpd {
            upvar #0 ::Httpd$sock s

            set urlPrefix ""
            catch {
                set urlPrefix [lindex $s(self) 0]://$serviceData(-host)
                set urlPrefix [lindex $s(self) 0]://$s(mime,host)
            }
            set xml [GetWsdl $serviceName $urlPrefix]
            ::Httpd_ReturnData $sock "text/xml; charset=UTF-8" $xml 200
        }
        channel {
            set xml [GetWsdl $serviceName]
            ::WS::Channel::ReturnData $sock "text/xml; charset=UTF-8" $xml 200
        }
        embedded {
            set xml [GetWsdl $serviceName]
            ::WS::Embeded::ReturnData $sock "text/xml; charset=UTF-8" $xml 200
        }
        rivet {
            set xml [GetWsdl $serviceName]
            headers type text/xml
            headers numeric 200
            puts $xml
        }
        aolserver {
            set xml [GetWsdl $serviceName]
            ::WS::AOLserver::ReturnData $sock text/xml $xml 200
        }
        wibble  {
            set xml [GetWsdl $serviceName]
            upvar 1 [lindex $args 0] responseDict
            ::WS::Wibble::ReturnData responseDict text/xml $xml 200
        }
        default {
            ## Do nothing
        }
    }
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Server::GenerateScheme
#
# Description : Generate a scheme
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       doc         - The document to add the scheme to
#       parent      - The parent node of the scheme
#
# Returns :     nothing
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
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
#       1  02/15/2008  G.Lester     Made Scheme generation a utility
#       2  02/03/2008  G.Lester     Moved scheme generation into WS::Utils namespace
#
###########################################################################
proc ::WS::Server::GenerateScheme {mode serviceName doc parent} {
    variable serviceArr

    array set serviceData $serviceArr($serviceName)
    set targetNamespace "http://$serviceData(-host)$serviceData(-prefix)"
    return [::WS::Utils::GenerateScheme $mode $serviceName $doc $parent $targetNamespace]

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateInfo {service sock args} {
    variable serviceArr
    variable procInfo
    variable mode

    ::log::log debug "Generating HTML Documentation for $service on $sock with {$args}"
    if {![info exists serviceArr($service)]} {
        set msg "Unknown service '$service'"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData \
                    $sock \
                    "text/html; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            embedded {
                ::WS::Embeded::ReturnData \
                    $sock \
                    "text/html; charset=UTF-8" \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            channel {
                ::WS::Channel::ReturnData \
                    $sock \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            rivet {
                headers type "text/html; charset=UTF-8"
                headers numeric 404
                puts "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>"
            }
            aolserver {
                ::WS::AOLserver::ReturnData \
                    $sock \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            wibble  {
                upvar 1 [lindex $args 0] responseDict
                ::WS::Wibble::ReturnData responseDict \
                    text/html \
                    "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
                    404
            }
            default {
                ## Do nothing
            }
        }
        return 1
    }

    set menuList {
        {List of Operations}    {#TOC}
        {Operation Details}     {#OperDetails}
        {Simple Types}          {#SimpleTypeDetails}
        {Custom Types}          {#CustomTypeDetails}
    }

    ##
    ## Display Service General Information
    ##
    append msg [generateGeneralInfo $serviceArr($service) $menuList]

    ##
    ## Display TOC
    ##
    append msg [generateTocInfo $serviceArr($service) $menuList]

    ##
    ## Display Operations
    ##
    ::log::log debug "\tDisplay Operations"
    append msg [generateOperationInfo $serviceArr($service) $menuList]

    ##
    ## Display custom types
    ##
    ::log::log debug "\tDisplay custom types"
    append msg [generateCustomTypeInfo $serviceArr($service) $menuList]

    ##
    ## Display list of simple types
    ##
    ::log::log debug "\tDisplay list of simply types"
    append msg [generateSimpleTypeInfo $serviceArr($service) $menuList]

    ##
    ## All Done
    ##
    append msg [::html::end]
    switch -exact -- $mode {
        tclhttpd {
            ::Httpd_ReturnData $sock "text/html; charset=UTF-8" $msg 200
        }
        embedded {
            ::WS::Embeded::ReturnData $sock "text/html; charset=UTF-8" $msg 200
        }
        channel {
            ::WS::Channel::ReturnData $sock "text/html; charset=UTF-8" $msg 200
        }
        rivet {
            headers numeric 200
            headers type text/html
            puts $msg
        }
        aolserver {
            ::WS::AOLserver::ReturnData $sock text/html $msg 200
        }
        wibble  {
                upvar 1 [lindex $args 0] responseDict
                ::WS::Wibble::ReturnData responseDict text/html $msg 200
        }
        default {
            ## Do nothing
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
# Procedure Name : ::WS::Server::displayType
#
# Description : Formats the type, including a link to a complex type.
#
# Arguments :
#       serviceName     - The name of the service
#       type            - The type
#
# Returns : Formatted type information.
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::displayType {serviceName type} {
    set testType [string trimright $type {()}]
    if {([lindex [::WS::Utils::TypeInfo Server $serviceName $testType] 0] == 0) &&
        ([info exists ::WS::Utils::simpleTypes($testType)])} {
        set result $type
    } else {
        set result [format {<a href="#type_%1$s">%2$s</a>} $testType $type]
    }
    return $result
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::callOperation
#
# Description : Process the call to an operation.  If an error occurs, a standard
#               error packet is sent, otherwise the appropriate message type
#               is sent.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::callOperation {service sock args} {
    variable procInfo
    variable serviceArr
    variable mode

    switch -exact -- $mode {
        embedded {
            upvar #0 ::WS::Embeded::Httpd$sock data
            set inXML $data(query)
            #parray data
        }
        wibble {
            set requestDict [lindex $args 0]
            upvar 1 [lindex $args 1] responseDict
            set inXML [dict get $requestDict post xml ""]
        }
        default {
            upvar #0 Httpd$sock data
            set inXML $data(query)
        }
    }

    ::log::log debug "In ::WS::Server::callOperation {$service $sock $args}"
    array set serviceInfo $serviceArr($service)
    ::log::log debug "\tDocument is {$inXML}"

    set ::errorInfo {}
    set ::errorCode {}
    set ns $service

    set inTransform $serviceInfo(-intransform)
    set outTransform $serviceInfo(-outtransform)
    set first [string first {<} $inXML]
    if {$first > 0} {
        set inXML [string range $inXML $first end]
    }
    if {![string equal $inTransform  {}]} {
        set inXML [$inTransform REQUEST $inXML]
    }
    dom parse $inXML doc
    $doc documentElement top
    ::log::log debug [list $doc selectNodesNamespaces \
        [list ENV http://schemas.xmlsoap.org/soap/envelope/ \
              $service http://$serviceInfo(-host)$serviceInfo(-prefix)]]
    $doc selectNodesNamespaces \
        [list ENV http://schemas.xmlsoap.org/soap/envelope/ \
              $service http://$serviceInfo(-host)$serviceInfo(-prefix)]
    $doc documentElement rootNode


    ##
    ## Determine the name of the method being invoked.
    ##
    set top [$rootNode selectNodes /ENV:Envelope/ENV:Body/*]
    catch {$top localName} requestMessage
    set legacyRpcMode 0
    if {$requestMessage == ""} {
        # older RPC/Encoded clients need to try nodeName instead.
        # Python pySoap needs this.
        catch {$top nodeName} requestMessage
        set legacyRpcMode 1
    }
    ::log::log debug "requestMessage = {$requestMessage}"
    if {[string match {*Request} $requestMessage]} {
        set operation [string range $requestMessage 0 end-7]
    } else {
        # broken clients might not have sent the correct Document Wrapped name.
        # Python pySoap and Perl SOAP::Lite need this.
        set operation $requestMessage
        set legacyRpcMode 1
    }


    ##
    ## Check that the method exists.
    ##
    if {![dict exists $procInfo $service op$operation argList]} {
        set msg "Method $operation not found"
        ::log::log error $msg
        set ::errorInfo {}
        set ::errorCode [list Server UNKNOWN_METHOD $operation]
        set xml [generateError \
                    $serviceInfo(-traceEnabled) \
                    CLIENT \
                    $msg \
                    [list "errorCode" $::errorCode "stackTrace" $::errorInfo]]
        catch {$doc delete}
        ::log::log debug "Leaving @ error 1::WS::Server::callOperation $xml"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            embedded {
                ::WS::Embeded::ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            rivet {
                headers type text/xml
                headers numeric 500
                puts $xml
            }
            aolserver {
                ::WS::AOLserver::ReturnData $sock text/xml $xml 500
            }
            wibble  {
                ::WS::Wibble::ReturnData responseDict text/xml $xml 500
            }
            default {
                ## Do nothing
            }
        }
        return;
    }
    set baseName $operation
    set cmdName op$baseName
    set methodName "${ns}::$baseName"

    ##
    ## Parse the arguments for the method.
    ##
    set argInfo [dict get $procInfo $ns $cmdName argList]
    if {[catch {
        foreach pass [list 1 2 3] {
            set tclArgList {}
            set gotAnyArgs 0
            set argIndex 0
            foreach argName [dict get $procInfo $ns $cmdName argOrder] {
                set argType [string trim [dict get $argInfo $argName type]]
                set typeInfoList [::WS::Utils::TypeInfo Server $service $argType]
                if {$pass == 1} {
                    # access arguments by name using full namespace
                    set path $service:$argName
                    set node [$top selectNodes $path]
                } elseif {$pass == 2} {
                    # legacyRpcMode only, access arguments by unqualified name
                    set path $argName
                    set node [$top selectNodes $path]
                } else {
                    # legacyRpcMode only, access arguments by index
                    set path "legacy argument index $argIndex"
                    set node [lindex [$top childNodes] $argIndex]
                    incr argIndex
                }
                if {[string equal $node {}]} {
                    ::log::log debug "did not find argument for $argName using $path, leaving blank"
                    lappend tclArgList {}
                    continue
                }
                ::log::log debug "found argument $argName using $path, processing $node"
                set gotAnyArgs 1
                switch -exact -- $typeInfoList {
                    {0 0} {
                        ##
                        ## Simple non-array
                        ##
                        lappend tclArgList [$node asText]
                    }
                    {0 1} {
                        ##
                        ## Simple array
                        ##
                        set tmp {}
                        foreach row $node {
                            lappend tmp [$row asText]
                        }
                        lappend tclArgList $tmp
                    }
                    {1 0} {
                        ##
                        ## Non-simple non-array
                        ##
                        lappend tclArgList [::WS::Utils::convertTypeToDict Server $service $node $argType $top]
                    }
                    {1 1} {
                        ##
                        ## Non-simple array
                        ##
                        set tmp {}
                        set argType [string trimright $argType {()}]
                        foreach row $node {
                            lappend tmp [::WS::Utils::convertTypeToDict Server $service $row $argType $top]
                        }
                        lappend tclArgList $tmp
                    }
                    default {
                        ## Do nothing
                    }
                }
            }
            ::log::log debug "gotAnyArgs $gotAnyArgs, legacyRpcMode $legacyRpcMode"
            if {$gotAnyArgs || !$legacyRpcMode} break
        }
        ::log::log debug "finalargs $tclArgList"
    } errMsg]} {
        ::log::log error $errMsg
        set localerrorCode $::errorCode
        set localerrorInfo $::errorInfo
        set xml [generateError \
                    $serviceInfo(-traceEnabled) \
                    CLIENT \
                    "Error Parsing Arguments -- $errMsg" \
                    [list "errorCode" $localerrorCode "stackTrace" $localerrorInfo]]
        catch {$doc delete}
        ::log::log debug "Leaving @ error 3::WS::Server::callOperation $xml"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            embedded {
                ::WS::Embeded::ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            channel {
                ::WS::Channel::ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            rivet {
                headers type text/xml
                headers numeric 500
                puts $xml
            }
            aolserver {
                ::WS::AOLserver::ReturnData $sock text/xml $xml 500
            }
            wibble  {
                ::WS::Wibble::ReturnData responseDict text/xml $xml 500
            }
            default {
                ## Do nothing
            }
        }
        return;
    }

    ##
    ## Run the premonitor hook, if necessary.
    ##
    if {[info exists serviceInfo(-premonitor)] && [string length $serviceInfo(-premonitor)]} {
        set precmd $serviceInfo(-premonitor)
        lappend precmd PRE $service $operation $tclArgList
        catch $precmd
    }

    ##
    ## Convert the HTTP request headers.
    ##
    set headerList {}
    foreach headerType $serviceInfo(-inheaders) {
        if {[string equal $headerType {}]} {
            continue
        }
        foreach node [$top selectNodes data:$headerType] {
            lappend headerList [::WS::Utils::convertTypeToDict Server $service $node $headerType $top]
        }
    }

    ##
    ## Actually execute the method.
    ##
    if {[catch {
        set cmd $serviceInfo(-checkheader)
        switch -exact -- $mode {
            wibble  {
                lappend cmd \
                    $ns \
                    $baseName \
                    [dict get $requestDict peerhost] \
                    [dict keys [dict get $requestDict header]] \
                    $headerList
            }
            embedded {
                lappend cmd $ns $baseName $data(ipaddr) $data(headers) $headerList
            }
            default {
                lappend cmd $ns $baseName $data(ipaddr) $data(headerlist) $headerList
            }
        }
        eval $cmd
        set results [eval \$methodName $tclArgList]
        # generate a reply packet
        set xml [generateReply $ns $baseName $results]
        # regsub "<!DOCTYPE\[^>\]+>\n" $xml {} xml
        catch {$doc delete}
        set xml [string map {{<?xml version="1.0"?>} {<?xml version="1.0"  encoding="utf-8"?>}} $xml]
        if {![string equal $outTransform  {}]} {
            set xml [$outTransform REPLY $xml $operation $results]
        }
        if {[info exists serviceInfo(-postmonitor)] &&
            [string length $serviceInfo(-postmonitor)]} {
            set precmd $serviceInfo(-postmonitor)
            lappend precmd POST $service $operation OK $results
            catch $precmd
        }
        ::log::log debug "Leaving ::WS::Server::callOperation $xml"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData $sock "text/xml; charset=UTF-8" $xml 200
            }
            embedded {
                ::WS::Embeded::ReturnData $sock "text/xml; charset=UTF-8" $xml 200
            }
            channel {
                ::WS::Channel::ReturnData $sock "text/xml; charset=UTF-8" $xml 200
            }
            rivet {
                headers type text/xml
                headers numeric 200
                puts $xml
            }
            aolserver {
                ::WS::AOLserver::ReturnData $sock text/xml $xml 200
            }
            wibble  {
                ::WS::Wibble::ReturnData responseDict text/xml $xml 200
            }
            default {
                ## Do nothing
            }
        }
    } msg]} {
        ##
        ## Handle errors
        ##
        set localerrorCode $::errorCode
        set localerrorInfo $::errorInfo
        if {[info exists serviceInfo(-postmonitor)] &&
            [string length $serviceInfo(-postmonitor)]} {
            set precmd $serviceInfo(-postmonitor)
            lappend precmd POST $service $operation ERROR $msg
            catch $precmd
        }
        set xml [generateError \
                    $serviceInfo(-traceEnabled) \
                    CLIENT \
                    $msg \
                    [list "errorCode" $localerrorCode "stackTrace" $localerrorInfo]]
        catch {$doc delete}
        ::log::log debug "Leaving @ error 2::WS::Server::callOperation $xml"
        switch -exact -- $mode {
            tclhttpd {
                ::Httpd_ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            embedded {
                ::WS::Embeded::ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            channel {
                ::WS::Channel::ReturnData $sock "text/xml; charset=UTF-8" $xml 500
            }
            rivet {
                headers type text/xml
                headers numeric 500
                puts $xml
            }
            aolserver {
                ::WS::AOLserver::ReturnData $sock text/xml $xml 500
            }
            wibble  {
                ::WS::Wibble::ReturnData responseDict text/xml $xml 500
            }
            default {
                ## Do nothing
            }
        }
        return;
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
# Procedure Name : ::WS::Server::generateError
#
# Description : Generate a standard error packet
#
# Arguments :
#    includeTrace       - Boolean indicate if the trace is to be included.
#    faultcode          - The code describing the error
#    faultstring        - The string describing the error.
#    detail             - Optional details of error.
#
# Returns : XML formatted standard error packet
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateError {includeTrace faultcode faultstring detail} {
    ::log::log debug "Entering ::WS::Server::generateError $faultcode $faultstring {$detail}"
    set code [lindex $detail 1]
    switch -exact -- $code {
        "VersionMismatch" {
            set code "SOAP-ENV:VersionMismatch"
        }
        "MustUnderstand" {
            set code "SOAP-ENV:MustUnderstand"
        }
        "Client" {
            set code "SOAP-ENV:Client"
        }
        "Server" {
            set code "SOAP-ENV:Server"
        }
        default {
            ## Do nothing
        }
    }
    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute  \
        "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" \
        "xmlns:xsi"      "http://www.w3.org/1999/XMLSchema-instance" \
        "xmlns:xsd"      "http://www.w3.org/1999/XMLSchema" \
        "xmlns:SOAP-ENC" "http://schemas.xmlsoap.org/soap/encoding/"
    $env appendChild [$doc createElement "SOAP-ENV:Body" bod]
    $bod appendChild [$doc createElement "SOAP-ENV:Fault" flt]
    $flt appendChild [$doc createElement "SOAP-ENV:faultcode" fcd]
    $fcd appendChild [$doc  createTextNode $faultcode]
    $flt appendChild [$doc createElement "SOAP-ENV:faultstring" fst]
    $fst appendChild [$doc createTextNode $faultstring]

    if { $detail != {} } {
        $flt appendChild [$doc createElement "SOAP-ENV:detail" dtl0]
        $dtl0 appendChild [$doc createElement "e:errorInfo" dtl]
        $dtl setAttribute "xmlns:e" "urn:TclErrorInfo"

        foreach {detailName detailInfo} $detail {
            if {!$includeTrace && $detailName == "stackTrace"} {
                continue
            }
            $dtl appendChild [$doc createElement $detailName err]
            $err appendChild [$doc createTextNode $detailInfo]
        }
    }

    # serialize the DOM document and return the XML text
    append xml  \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$doc asXML -indent none -doctypeDeclaration 0]
    $doc delete
    ::log::log debug "Leaving (error) ::WS::Server::generateError $xml"
    return $xml
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : :WS::Server::generateReply
#
# Description : Generate the reply packet for an operation
#
# Arguments :
#    serviceName         - The name of the service
#    operation           - The name of the operation
#    results             - The results as a dictionary object
#
#
# Returns : The results as an XML formatted packet.
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateReply {serviceName operation results} {
    ::log::log debug "Entering ::WS::Server::generateReply $serviceName $operation {$results}"

    variable serviceArr

    array set serviceData $serviceArr($serviceName)

    if {[info exists ::Config(docRoot)] && [file exists [file join $::Config(docRoot) $serviceName $operation.css]]} {
        set replaceText [format {<?xml-stylesheet type="text/xsl" href="http://%s/css/%s/%s.css"?>}\
                                $serviceData(-host) \
                                $serviceName \
                                $operation]
        append replaceText "\n"
    } else {
        set replaceText {}
    }

    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute  \
        "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" \
        "xmlns:xsi"      "http://www.w3.org/1999/XMLSchema-instance" \
        "xmlns:xsd"      "http://www.w3.org/1999/XMLSchema" \
        "xmlns:SOAP-ENC" "http://schemas.xmlsoap.org/soap/encoding/" \
         xmlns:$serviceName "http://$serviceData(-host)$serviceData(-prefix)"
    if {[llength $serviceData(-outheaders)]} {
        $env appendChild [$doc createElement "SOAP-ENV:Header" header]
        foreach headerType $serviceData(-outheaders) {
            #$header appendChild [$doc createElement ${serviceName}:${headerType} part]
            #::WS::Utils::convertDictToType Server $serviceName $doc $part $results $headerType
            ::WS::Utils::convertDictToType Server $serviceName $doc $header $results $headerType
        }
    }
    $env appendChild [$doc createElement "SOAP-ENV:Body" body]
    $body appendChild [$doc createElement ${serviceName}:${operation}Results reply]

    ::WS::Utils::convertDictToType Server $serviceName $doc $reply $results ${serviceName}:${operation}Results

    append xml  \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$doc asXML -indent none -doctypeDeclaration 0]
    #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] $replaceText xml
    $doc delete

    ::log::log debug "Leaving ::WS::Server::generateReply $xml"
    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : :WS::Server::ok
#
# Description : Stub for header check
#
# Arguments :
#    serviceName         - The name of the service
#    operation           - The name of the operation
#    results             - The results as a dictionary object
#
#
# Returns : The results as an XML formatted packet.
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::ok {args} {
    return;
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateGeneralInfo {serviceInfo menuList} {
    variable procInfo

    ::log::log debug "\tDisplay Service General Information"
    array set serviceData $serviceInfo
    set service [dict get $serviceInfo -service]

    ::html::init
    ::html::author $serviceData(-author)
    if {[string equal $serviceData(-description) {}]} {
        ::html::description  "Automatically generated human readable documentation for '$service'"
    } else {
        ::html::description $serviceData(-description)
    }
    if {$serviceData(-stylesheet) != ""} {
        ::html::headTag "link rel=\"stylesheet\" type=\"text/css\" href=\"$serviceData(-stylesheet)\""
    }
    set head $serviceData(-htmlhead)
    set msg [::html::head $head]
    append msg [::html::bodyTag]

    array unset serviceData -service
    if {[info exists serviceData(-description)]} {
        set serviceData(-description) [::html::nl2br $serviceData(-description)]
    }
    set wsdl [format {<a href="%s/%s">WSDL(xml)</a>} $serviceData(-prefix) wsdl]
    append msg [::html::openTag center] [::html::h1 "$head -- $wsdl"] [::html::closeTag] \
               [::html::openTag table {border="2"}]

    foreach key [lsort -dictionary [array names serviceData]] {
        if {[string equal $serviceData($key) {}]} {
            append msg [::html::row [string range $key 1 end] {<i>N/A</i>}]
        } else {
            append msg [::html::row [string range $key 1 end] $serviceData($key)]
        }
    }
    append msg [::html::closeTag] \
               "\n<hr/>\n"

    return $msg
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateTocInfo {serviceInfo menuList} {
    variable procInfo

    ##
    ## Display TOC
    ##
    ::log::log debug "\tTOC"
    set service [dict get $serviceInfo -service]
    append msg [::html::h2 {<a id='TOC'>List of Operations</a>}]

    set operList {}
    foreach oper [lsort -dictionary [dict get $procInfo $service operationList]] {
        lappend operList $oper "#op_$oper"
    }
    append msg [::html::minorList $operList]

    append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
    append msg "\n<hr/>\n"

    return $msg
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateOperationInfo {serviceInfo menuList} {
    variable procInfo

    ##
    ## Display Operations
    ##
    ::log::log debug "\tDisplay Operations"
    set service [dict get $serviceInfo -service]
    set operList {}
    foreach oper [lsort -dictionary [dict get $procInfo $service operationList]] {
        lappend operList $oper "#op_$oper"
    }
    append msg [::html::h2 {<a id='OperDetails'>Operation Details</a>}]

    set docFormat [dict get $serviceInfo -docFormat]
    foreach {oper anchor} $operList {
        ::log::log debug "\t\tDisplaying '$oper'"
        append msg [::html::h3 "<a id='op_$oper'>$oper</a>"]

        append msg [::html::h4 {Description}] "\n"

        append msg [::html::openTag div {style="margin-left: 40px;"}]
        switch -exact -- $docFormat {
            "html" {
                append msg [dict get $procInfo $service op$oper docs]
            }
            "text" -
            default {
                append msg [::html::nl2br [::html::html_entities [dict get $procInfo $service op$oper docs]]]
            }
        }
        append msg [::html::closeTag]

        append msg "\n"

        append msg [::html::h4 {Inputs}] "\n"

        append msg [::html::openTag div {style="margin-left: 40px;"}]
        append msg [::html::openTag {table} {border="2"}]
        append msg [::html::hdrRow Name Type Description]
        foreach arg [dict get $procInfo $service op$oper argOrder] {
            ::log::log debug "\t\t\tDisplaying '$arg'"
            if {[dict exists $procInfo $service op$oper argList $arg comment]} {
                set comment [dict get $procInfo $service op$oper argList $arg comment]
            } else {
                set comment {}
            }
            append msg [::html::row \
                            $arg \
                            [displayType $service [dict get $procInfo $service op$oper argList $arg type]] \
                            $comment \
                       ]
        }
        append msg [::html::closeTag]
        append msg [::html::closeTag]

        ::log::log debug "\t\tReturns"
        append msg [::html::h4 {Returns}] "\n"

        append msg [::html::openTag div {style="margin-left: 40px;"}]
        append msg [::html::openTag {table} {border="2"}]
        append msg [::html::hdrRow Type Description]
        if {[dict exists $procInfo $service op$oper returnInfo comment]} {
            set comment [dict get $procInfo $service op$oper returnInfo comment]
        } else {
            set comment {}
        }
        append msg [::html::row \
                        [displayType $service [dict get $procInfo $service op$oper returnInfo type]] \
                        $comment \
                   ]
        append msg [::html::closeTag]
        append msg [::html::closeTag]

        append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
        append msg "\n<hr/>\n"
    }

    if {![llength $operList]} {
        append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
        append msg "\n<hr/>\n"
    }

    return $msg

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateCustomTypeInfo {serviceInfo menuList} {
    variable procInfo

    ##
    ## Display custom types
    ##
    ::log::log debug "\tDisplay custom types"
    set service [dict get $serviceInfo -service]
    append msg [::html::h2 {<a id='CustomTypeDetails'>Custom Types</a>}]

    set localTypeInfo [::WS::Utils::GetServiceTypeDef Server $service]
    foreach type [lsort -dictionary [dict keys $localTypeInfo]] {
        ::log::log debug "\t\tDisplaying '$type'"
        set typeOverloadArray($type) 1
        append msg [::html::h3 "<a id='type_$type'>$type</a>"]
        set typeDetails [dict get $localTypeInfo $type definition]
        append msg [::html::openTag {table} {border="2"}]
        append msg [::html::hdrRow Field Type]
        foreach part [lsort -dictionary [dict keys $typeDetails]] {
            ::log::log debug "\t\t\tDisplaying '$part'"
            append msg [::html::row \
                            $part \
                            [displayType $service [dict get $typeDetails $part type]]
                       ]
        }
        append msg [::html::closeTag]
    }

    append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
    append msg "\n<hr/>\n"

    return $msg
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::generateInfo
#
# Description : Generate an HTML description of the service, the operations
#               and all applicable type definitions.
#
# Arguments :
#       serviceName     - The name of the service
#       sock            - The socket to return the WSDL on
#       args            - not used
#
# Returns :
#       1 - On error
#       0 - On success
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Server::generateSimpleTypeInfo {serviceInfo menuList} {
    variable procInfo

    ##
    ## Display list of simple types
    ##
    ::log::log debug "\tDisplay list of simply types"
    set service [dict get $serviceInfo -service]
    append msg [::html::h2 {<a id='SimpleTypeDetails'>Simple Types</a>}]

    append msg "\n<br/>\n<center>" [::html::minorMenu $menuList] "</center>"
    set localTypeInfo [::WS::Utils::GetServiceSimpleTypeDef Server $service]
    foreach typeDetails [lsort -dictionary -index 0 $localTypeInfo] {
        set type [lindex $typeDetails 0]
        ::log::log debug "\t\tDisplaying '$type'"
        set typeOverloadArray($type) 1
        append msg [::html::h3 "<a id='type_$type'>$type</a>"]
        append msg [::html::openTag {table} {border="2"}]
        append msg [::html::hdrRow Attribute Value]
        foreach part [lsort -dictionary [dict keys [lindex $typeDetails 1]]] {
            ::log::log debug "\t\t\tDisplaying '$part'"
            append msg [::html::row \
                            $part \
                            [dict get [lindex $typeDetails 1] $part]
                       ]
        }
        append msg [::html::closeTag]
    }
    append msg "\n<hr/>\n"

    return $msg
}
