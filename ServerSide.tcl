###############################################################################
##                                                                           ##
##  Copyright (c) 2006, Visiprise Software, Inc                              ##
##  Copyright (c) 2006, Gerald W. Lester                                     ##
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

package require WS::Utils
package require Tcl 8.5
package require html
package require log
package require tdom

package provide WS::Server 1.1.0

namespace eval ::WS::Server {
    array set serviceArr {}
    set procInfo {}
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
# Arguments : this procedure uses position independed arguments, the are:
#               -host           - The host name for this serice
#                                       Defaults to "localhost"
#               -decription     - The HTML description for this service
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
#
#
# Returns :     Nothing
#
# Side-Effects :        None
#
# Exception Conditions :
#       MISSREQARG -- Missing required arguements
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

    ::log::log debug "Defining Service as $args"

    array set defaults {
        -host           localhost
        -description    {}
        -checkheader    {::WS::Server::ok}
        -inheaders      {}
        -outheaders     {}
        -htmlhead       {TclHttpd Based Web Services}
        -author         {}
        -description    {}
    }
    array set defaults $args
    set requiredList {-host -service}
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
            "Missing required arguements '[join $missingList {,}]'"
    }
    set service $defaults(-service)
    set defaults(-prefix) /service/$service
    set defaults(-uri) $service
    namespace eval ::$service {}
    set serviceArr($service) [array get defaults]
    if {![dict exists $procInfo $service operationList]} {
        dict set procInfo $service operationList {}
    }

    ##
    ## Install wsdl doc
    ##
    interp alias {} ::WS::Server::generateInfo_${service} \
                 {} ::WS::Server::generateInfo ${service}
    ::log::log debug "Installing Generate info for $service at $defaults(-prefix)"
    ::Url_PrefixInstall $defaults(-prefix) ::WS::Server::generateInfo_${service}  \
        -thread 0


    ##
    ## Install wsdl
    ##
    interp alias {} ::WS::Server::generateWsdl_${service} \
                 {} ::WS::Server::generateWsdl ${service}
    ::log::log debug "Installing GenerateWsdl info for $service at $defaults(-prefix)/wsdl"
    ::Url_PrefixInstall $defaults(-prefix)/wsdl ::WS::Server::generateWsdl_${service} \
        -thread 0

    ##
    ## Install operations
    ##
    interp alias {} ::WS::Server::callOperation_${service} \
                 {} ::WS::Server::callOperation ${service}
    ::log::log debug "Installing callOperation info for $service at $defaults(-prefix)/op"
    ::Url_PrefixInstall $defaults(-prefix)/op ::WS::Server::callOperation_${service} \
        -thread 1

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
#               the oeprations.
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
#       A proceedure named "<ServiceName>::<OperationName>" defined
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
    ::log::log debug "Defining opertaion $name for $service"
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
#
#
###########################################################################
proc ::WS::Server::GetWsdl {serviceName} {
    variable serviceArr
    variable procInfo

    array set serviceData $serviceArr($serviceName)

    set operList [lsort -dictionary [dict get $procInfo $serviceName operationList]]
    ::log::log debug "Generating WSDL for $serviceName"
    if {![info exists serviceArr($serviceName)]} {
        set msg "Unknown service '$serviceName'"
        :return \
            -code error \
            -errorCode [list WS SERVER UNKSERV $serviceName] \
            $msg
    }

    set msg {}
    set reply [::dom createDocument definitions]
    $reply documentElement definition
    $definition setAttribute \
        xmlns           "http://schemas.xmlsoap.org/wsdl/" \
        xmlns:http      "http://schemas.xmlsoap.org/wsdl/http/" \
        xmlns:mime      "http://schemas.xmlsoap.org/wsdl/mime/" \
        xmlns:s         "http://www.w3.org/2001/XMLSchema" \
        xmlns:soap      "http://schemas.xmlsoap.org/wsdl/soap/" \
        xmlns:soapenc   "http://schemas.xmlsoap.org/soap/encoding/" \
        xmlns:${serviceName} "http://$serviceData(-host)$serviceData(-prefix)" \
        targetNamespace "http://$serviceData(-host)$serviceData(-prefix)"

    foreach topLevel {types} {
        $definition appendChild [$reply createElement $topLevel $topLevel]
    }

    ##
    ## Messages
    ##

    ## Operations
    foreach oper $operList {
        $definition appendChild [$reply createElement message input]
        $input setAttribute name ${oper}In
        $input appendChild [$reply createElement part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${oper}Request
        $definition appendChild [$reply createElement message output]
        $output setAttribute name ${oper}Out
        $output appendChild [$reply createElement part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${oper}Results
    }

    ## Input headers
    foreach headerType $serviceData(-inheaders) {
        $definition appendChild [$reply createElement message header]
        $header setAttribute name $headerType
        $header appendChild [$reply createElement part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${headerType}
    }

    ## Output headers
    foreach headerType $serviceData(-outheaders) {
        $definition appendChild [$reply createElement message header]
        $header setAttribute name $headerType
        $header appendChild [$reply createElement part part]
        $part setAttribute \
            name parameters \
            element ${serviceName}:${headerType}
    }

    ##
    ## Add the rest of the toplevels in
    ##
    foreach topLevel {portType binding service} {
        $definition appendChild [$reply createElement $topLevel $topLevel]
    }

    ##
    ## Service
    ##
    $service setAttribute name $serviceName

    $service appendChild [$reply createElement documentation documentation]
    $documentation appendChild [$reply createTextNode $serviceData(-description)]

    $service appendChild [$reply createElement port port]
    $port setAttribute \
        name ${serviceName}Soap \
        binding ${serviceName}:${serviceName}Soap

    $port appendChild [$reply createElement soap:address address]
    $address setAttribute  \
        location "http://$serviceData(-host)$serviceData(-prefix)/op"


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
        $binding appendChild [$reply createElement operation operNode]
        $operNode setAttribute name $oper
        $operNode appendChild [$reply createElement soap:operation o2]
        $o2 setAttribute \
            soapAction $serviceName:$oper \
            style document
        ## Input message
        $operNode appendChild [$reply createElement input input]
        $input appendChild [$reply createElement soap:body tmp]
        $tmp setAttribute use literal
        foreach headerType $serviceData(-inheaders) {
            $operNode appendChild [$reply createElement header header]
            $header appendChild [$reply createElement soap:header tmp]
            $tmp setAttribute \
                use literal \
                message ${serviceName}:${headerType} \
                part $headerType
        }
        ## Output message
        $operNode appendChild [$reply createElement output output]
        $output appendChild [$reply createElement soap:body tmp]
        $tmp setAttribute use literal
        foreach headerType $serviceData(-outheaders) {
            $operNode appendChild [$reply createElement header header]
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
        $portType appendChild [$reply createElement operation operNode]
        $operNode setAttribute name $oper
        $operNode appendChild [$reply createElement input input]
        $input setAttribute message ${serviceName}:${oper}In
        $operNode appendChild [$reply createElement output output]
        $output setAttribute message ${serviceName}:${oper}Out
    }

    ##
    ## Types
    ##
    set localTypeInfo [::WS::Utils::GetServiceTypeDef Server $serviceName]
    array set typeArr {}
    foreach type [dict keys $localTypeInfo] {
        set typeArr($type) 1
    }
    $types appendChild [$reply createElement s:schema schema]
    $schema setAttribute \
        elementFormDefault qualified \
        targetNamespace "http://$serviceData(-host)$serviceData(-prefix)"

    foreach baseType [lsort -dictionary [array names typeArr]] {
        ::log::log debug "Outputing $baseType"
        $schema appendChild [$reply createElement s:element elem]
        $elem setAttribute name $baseType
        $elem setAttribute type ${serviceName}:${baseType}
        $schema appendChild [$reply createElement s:complexType comp]
        $comp setAttribute name $baseType
        $comp appendChild [$reply createElement s:sequence seq]
        set baseTypeInfo [dict get $localTypeInfo $baseType definition]
        ::log::log debug "\t parts {$baseTypeInfo}"
        foreach {field tmpTypeInfo} $baseTypeInfo {
            $seq appendChild  [$reply createElement s:element tmp]
            set tmpType [dict get $tmpTypeInfo type]
            ::log::log debug "Field $field of $tmpType"
            foreach {name value} [getTypeWSDLInfo $serviceName $field $tmpType] {
                $tmp setAttribute $name $value
            }
        }
    }

    append msg \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$reply asXML -indent none -doctypeDeclaration 0]
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
#
#
###########################################################################
proc ::WS::Server::generateWsdl {serviceName sock args} {
    variable serviceArr
    variable procInfo

    array set serviceData $serviceArr($serviceName)

    set operList [lsort -dictionary [dict get $procInfo $serviceName operationList]]
    ::log::log debug "Generating WSDL for $serviceName on $sock with {$args}"
    if {![info exists serviceArr($serviceName)]} {
        set msg "Unknown service '$serviceName'"
        ::Httpd_ReturnData \
            $sock \
            text/html \
            "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
            404
        return 1
    }


    set msg [GetWsdl $serviceName]
    ::Httpd_ReturnData $sock text/xml $msg 200
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Server::getTypeWSDLInfo
#
# Description : Return full type information usable for a WSDL
#
# Arguments :
#    serviceName        - The name of the service
#    field              - The field name
#    type               - The data type
#
# Returns : The type definition as a dictionary object
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
proc ::WS::Server::getTypeWSDLInfo {serviceName field type} {
    set typeInfo {maxOccurs 1 minOccurs 1 name * type *}
    dict set typeInfo name $field
    set typeList [::WS::Utils::TypeInfo Server $serviceName $type]
    if {[lindex $typeList 0] == 0} {
        dict set typeInfo type s:$type
    } else {
        dict set typeInfo type $serviceName:[string trim $type {()}]
    }
    if {[lindex $typeList 1]} {
        dict set typeInfo maxOccurs unbounded
    }

    return $typeInfo
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

    ::log::log debug "Generating HTML Documentation for $service on $sock with {$args}"
    if {![info exists serviceArr($service)]} {
        set msg "Unknown service '$service'"
        ::Httpd_ReturnData \
            $sock \
            text/html \
            "<html><head><title>Webservice Error</title></head><body><h2>$msg</h2></body></html>" \
            404
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
    ::Httpd_ReturnData $sock text/html $msg 200

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
# Returns : Formated type information.
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
        set result [format {<A HREF="#type_%1$s">%2$s</A>} $testType $type]
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

    upvar #0 Httpd$sock data

    ::log::log debug "In ::WS::Server::callOperation {$service $sock $args}"
    set inXML [string trim $data(query)]

    regsub "^<\\?.*?\\?>" $inXML "" inXML
    set inXML [encoding convertfrom utf-8 $inXML]

    array set serviceInfo $serviceArr($service)
    ::log::log debug "\tDocument is {$inXML}"

    set ::errorInfo {}
    set ::errorCode {}
    set ns $service
    dom parse $inXML doc
    $doc documentElement top
    ::log::log debug [list $doc selectNodesNamespaces \
        [list ENV http://schemas.xmlsoap.org/soap/envelope/ \
              $service http://$serviceInfo(-host)$serviceInfo(-prefix)]]
    $doc selectNodesNamespaces \
        [list ENV http://schemas.xmlsoap.org/soap/envelope/ \
              $service http://$serviceInfo(-host)$serviceInfo(-prefix)]
    $doc documentElement rootNode
    set top [$rootNode selectNodes /ENV:Envelope/ENV:Body/*]
    catch {$top localName} requestMessage
    ::log::log debug "requestMessage = {$requestMessage}"
    if {![string match {*Request} $requestMessage]} {
        set msg "Malformed Request -- not understood '$requestMessage'"
        set ::errorInfo {}
        set ::errorCode [list Server MALFORMED_REQUEST $doc]
        set xml [generateError \
                    CLIENT \
                    $msg \
                    [list "errorCode" $::errorCode "stackTrace" $::errorInfo]]
        ::Httpd_ReturnData $sock \
                         text/xml \
                         [encoding convertto utf-8 $xml] \
                         500
        return;
    }
    set operation [string range $requestMessage 0 end-7]
    if {![dict exists $procInfo $service op$operation argList]} {
        set msg "Method $operation not found"
        set ::errorInfo {}
        set ::errorCode [list Server UNKNOWN_METHOD $operation]
        set xml [generateError \
                    CLIENT \
                    $msg \
                    [list "errorCode" $::errorCode "stackTrace" $::errorInfo]]
        catch {$doc delete}
        ::log::log debug "Leaving @ error 1::WS::Server::callOperation $xml"
        ::Httpd_ReturnData $sock \
                         text/xml \
                         [encoding convertto utf-8 $xml] \
                         500
        return;
    }
    set baseName $operation
    set cmdName op$baseName
    set methodName "${ns}::$baseName"
    set tclArgList {}
    set argInfo [dict get $procInfo $ns $cmdName argList]
    if {[catch {
        foreach argName [dict get $procInfo $ns $cmdName argOrder] {
            set argType [dict get $argInfo $argName type]
            set typeInfoList [::WS::Utils::TypeInfo Server $service $argType]
            ::log::log debug "\tProcessing argName = {$argName} argType = {$argType} typeInfoList = {$typeInfoList}"
            set path $service:$argName
            set node [$top selectNodes $path]
            if {[string equal $node {}]} {
                lappend tclArgList {}
                continue
            }
            switch $typeInfoList {
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
                    lappend tclArgList [::WS::Utils::convertTypeToDict Server $service $node $argType]
                }
                {1 1} {
                    ##
                    ## Non-simple array
                    ##
                    set tmp {}
                    set argType [string trimright $argType {()}]
                    foreach row $node {
                        lappend tmp [::WS::Utils::convertTypeToDict Server $service $row $argType]
                    }
                    lappend tclArgList $tmp
                }
            }
        }
    } errMsg]} {
        set localerrorCode $::errorCode
        set localerrorInfo $::errorInfo
        set xml [generateError \
                    CLIENT \
                    "Error Parsing Arguments -- $errMsg" \
                    [list "errorCode" $localerrorCode "stackTrace" $localerrorInfo]]
        catch {$doc delete}
        ::log::log debug "Leaving @ error 3::WS::Server::callOperation $xml"
        ::Httpd_ReturnData $sock \
                         "text/xml; charset=UTF-8"\
                         [encoding convertto utf-8 $xml] \
                         500
        return;
    }
    if {[info exists serviceInfo(-premonitor)] && [string length $serviceInfo(-premonitor)]} {
        set precmd $serviceInfo(-premonitor)
        lappend precmd PRE $service $operation $tclArgList
        catch $precmd
    }
    set headerList {}
    foreach headerType $serviceInfo(-inheaders) {
        if {[string equal $headerType {}]} {
            continue
        }
        foreach node [$top selectNodes data:$headerType] {
            lappend headerList [::WS::Utils::convertTypeToDict Server $service $node $headerType]
        }
    }
    if {[catch {
        set cmd $serviceInfo(-checkheader)
        lappend cmd $ns $baseName $data(ipaddr) $data(headerlist) $headerList
        eval $cmd
        set results [eval \$methodName $tclArgList]
        # generate a reply packet
        set xml [generateReply $ns $baseName $results]
        # regsub "<!DOCTYPE\[^>\]+>\n" $xml {} xml
        catch {$doc delete}
        set xml [string map {{<?xml version="1.0"?>} {<?xml version="1.0"  encoding="utf-8"?>}} $xml]
        if {[info exists serviceInfo(-postmonitor)] &&
            [string length $serviceInfo(-postmonitor)]} {
            set precmd $serviceInfo(-postmonitor)
            lappend precmd POST $service $operation OK $results
            catch $precmd
        }
        ::log::log debug "Leaving ::WS::Server::callOperation $xml"
        ::Httpd_ReturnData $sock \
                         "text/xml; charset=UTF-8" \
                         [encoding convertto utf-8 $xml] \
                         200
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
                    CLIENT \
                    $msg \
                    [list "errorCode" $localerrorCode "stackTrace" $localerrorInfo]]
        catch {$doc delete}
        ::log::log debug "Leaving @ error 2::WS::Server::callOperation $xml"
        ::Httpd_ReturnData $sock \
                         "text/xml; charset=UTF-8"\
                         $xml \
                         500
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
#    faultcode          - The code describing the error
#    faultstring        - The string describing the error.
#    detail             - Optional details of error.  Defaults to the empty string.
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
proc ::WS::Server::generateError {faultcode faultstring {detail {}}} {
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
# Description : Generate the reply packet for an opearation
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

    ::WS::Utils::convertDictToType Server $serviceName $doc $reply $results ${operation}Results

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
    set head $serviceData(-htmlhead)
    set msg [::html::head $head]

    array unset serviceData -service
    if {[info exists serviceData(-description)]} {
        set serviceData(-description) [::html::nl2br $serviceData(-description)]
    }
    set wsdl [format {<A HREF="%s/%s">WSDL(xml)</A>} $serviceData(-prefix) wsdl]
    append msg [::html::openTag Center] [::html::h1 "$head -- $wsdl"] [::html::closeTag] \
               [::html::openTag Table {border=2}]

    foreach key [lsort -dictionary [array names serviceData]] {
        if {[string equal $serviceData($key) {}]} {
            append msg [::html::row [string range $key 1 end] {<I>N/A</I>}]
        } else {
            append msg [::html::row [string range $key 1 end] $serviceData($key)]
        }
    }
    append msg [::html::closeTag] \
               "\n<HR>\n"

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

    append msg "\n<BR>\n<CENTER>" [::html::minorMenu $menuList] "</CENTER>"
    append msg "\n<HR>\n"

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

    foreach {oper anchor} $operList {
        ::log::log debug "\t\tDisplaying '$oper'"
        append msg [::html::h3 "<a id='op_$oper'>$oper</a>"]

        append msg [::html::h4 {Description}] "\n<UL>\n"
        append msg [::html::nl2br [dict get $procInfo $service op$oper docs]]
        append msg "\n</UL>\n"

        append msg [::html::h4 {Inputs}] "\n<UL>\n"
        append msg [::html::openTag {Table} {border=2}]
        append msg [::html::hdrRow Name Type Description]
        foreach arg [lsort -dictionary [dict get $procInfo $service op$oper argOrder]] {
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
        append msg "\n</UL>\n"

        ::log::log debug "\t\tReturns"
        append msg [::html::h4 {Returns}] "\n<UL>\n"
        append msg [::html::openTag {Table} {border=2}]
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
        append msg "\n</UL>\n"

        append msg "\n<BR>\n<CENTER>" [::html::minorMenu $menuList] "</CENTER>"
        append msg "\n<HR>\n"
    }

    if {![llength $operList]} {
        append msg "\n<BR>\n<CENTER>" [::html::minorMenu $menuList] "</CENTER>"
        append msg "\n<HR>\n"
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
        append msg [::html::openTag {Table} {border=2}]
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

    append msg "\n<BR>\n<CENTER>" [::html::minorMenu $menuList] "</CENTER>"
    append msg "\n<HR>\n"

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

    append msg "\n<BR>\n<CENTER>" [::html::minorMenu $menuList] "</CENTER>"
    set localTypeInfo [::WS::Utils::GetServiceSimpleTypeDef Server $service]
    foreach typeDetails [lsort -dictionary -index 0 $localTypeInfo] {
        set type [lindex $typeDetails 0]
        ::log::log debug "\t\tDisplaying '$type'"
        set typeOverloadArray($type) 1
        append msg [::html::h3 "<a id='type_$type'>$type</a>"]
        append msg [::html::openTag {Table} {border=2}]
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
    append msg "\n<HR>\n"

    return $msg
}
