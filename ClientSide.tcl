###############################################################################
##                                                                           ##
##  Copyright (c) 2006-2008, Gerald W. Lester                                ##
##  Copyright (c) 2008, Georgios Petasis                                     ##
##  Copyright (c) 2006, Visiprise Software, Inc                              ##
##  Copyright (c) 2006, Arnulf Wiedemann                                     ##
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
#package require Tcl 8.5
if {![llength [info command dict]]} {
    package require dict
}
package require tdom 0.8
package require http 2
package require log
package require uri

catch {
    package require tls
    http::register https 443 ::tls::socket
}

package provide WS::Client 2.0.4

namespace eval ::WS::Client {
    ##
    ## serviceArr is indexed by service name and contains a dictionary that
    ## defines the service.  The dictionary has the following structure:
    ##   targetNamespace - the target namespace
    ##   operList - list of operations
    ##   objList  - list of operations
    ##   headers  - list of http headers
    ##   types    - dictionary of types
    ##   service  - dictionary containing general information about the service, formatted:
    ##      name     -- the name of the service
    ##      location -- the url
    ##      style    -- style of call (e.g. rpc/encoded, document/literal)
    ##
    ## For style of rpc/encoded, document/literal
    ##   operations - dictionary with information about the operations.  The key
    ##               is the operations name and each with the following structure:
    ##      soapRequestHeader -- list of SOAP Request Headers
    ##      soapReplyHeader   -- list of SOAP Reply Headers
    ##      action            -- SOAP Action Header
    ##      inputs            -- list of fields with type info
    ##      outputs           -- return type
    ##      style             -- style of call (e.g. rpc/encoded, document/literal)
    ##
    ## For style of rest
    ##   object - dictionary with informat about objects.  The key is the object
    ##            name each with the following strucutre:
    ##     operations -- dictionary with information about the operations.  The key
    ##                   is the operations name and each with the following structure:
    ##       inputs            --- list of fields with type info
    ##       outputs           --- return type
    ##
    ## Note -- all type information is formated suitable to be passed
    ##         to ::WS::Utils::ServiceTypeDef
    ##
    array set ::WS::Client::serviceArr {}
    set ::WS::Client::currentBaseUrl {}
    array set ::WS::Client::options {
        skipLevelWhenActionPresent 0
        suppressTargetNS 0
        allowOperOverloading 1
    }
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::SetOption
#
# Description : Set or get an option
#
# Arguments :
#       option  - Option to be set/retrieved
#       args    - Value to set the option to
#
# Returns : The value of the option
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
#       1  04/272009   G.Lester     Initial version
###########################################################################
proc ::WS::Client::SetOption {option args} {
    variable options

    if {[info exists options($option)} {
        if {[llength $args] == 0} {
            return $options($option)
        } elseif {[llength $args] == 1} {
            set options($option) [lindex $args 1]
        } else {
            return  -code error \
                    -errorcode [list WSCLIENT INVALDCNT $args] \
                    "Invalid number of values: '$args'"
        }
    } else {
        return  -code error \
                -errorcode [list WSCLIENT UNKOPT $option] \
                "Uknown option: '$option'"
    }
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::CreateService
#
# Description : Define a service
#
# Arguments :
#       serviceName - Service name to add namespace to
#       type        - The type of service, currently only REST is supported
#       url         - URL of namespace file to import
#       args        - Optional arguments:
#                       -header httpHeaderList
#
# Returns :     The local alias (tns)
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
#       1  04/14/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::CreateService {serviceName type url args} {
    variable serviceArr
    variable options

    dict set serviceArr($serviceName) types {}
    dict set serviceArr($serviceName) operList {}
    dict set serviceArr($serviceName) objList {}
    dict set serviceArr($serviceName) headers {}
    dict set serviceArr($serviceName) targetNamespace tns1 $url
    dict set serviceArr($serviceName) name $serviceName
    dict set serviceArr($serviceName) location $url
    dict set serviceArr($serviceName) style $type
    dict set serviceArr($serviceName) imports {}
    dict set serviceArr($serviceName) inTransform {}
    dict set serviceArr($serviceName) outTransform {}
    dict set serviceArr($serviceName) skipLevelWhenActionPresent $options(skipLevelWhenActionPresent)
    dict set serviceArr($serviceName) suppressTargetNS $options(suppressTargetNS)
    foreach {name value} $args {
        set name [string trimleft $name {-}]
        dict set serviceArr($serviceName) $name $value
    }

    if {[dict exists $serviceArr($serviceName) xns]} {
        foreach xnsItem [dict get $serviceArr($serviceName) xns] {
            lassign $xnsItem tns xns
            ::log::log debug [list Setting targetNamespae to $xns]
            dict set serviceArr($serviceName) targetNamespace $xns $tns
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
# Procedure Name : ::WS::Client::Config
#
# Description : Configure a service information
#
# Arguments :
#       serviceName - Service name to add namespace to
#       item        - The item to configure
#       value       - Optional, the new value
#
# Returns :     The value of the option
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
#       1  04/14/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::Config {serviceName item {value {}}} {
    variable serviceArr

    set serviceInfo $serviceArr($serviceName)
    switch -exact -- $item {
        suppressTargetNS -
        skipLevelWhenActionPresent -
        location -
        targetNamespace {
            ##
            ## Valid, so do nothing
            ##
        }
        default {
            return -code error "Uknown option '$item'"
        }
    }

    if {![string equal $value {}]} {
        dict set serviceInfo $item $value
        set serviceArr($serviceName) $serviceInfo
    }

    return [dict get $serviceInfo $item]

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::SetServiceTransforms
#
# Description : Define a service's transforms
#               Transform signature is:
#                   cmd serviceName operationName transformType xml {url {}} {argList {}}
#               where transformType is REQUEST or REPLY
#               and url and argList will only be present for transformType == REQUEST
#
# Arguments :
#       serviceName  - Service name to add namespace to
#       inTransform  - Input transform, defaults to {}
#       outTransform - Output transform, defaults to {}
#
# Returns :     None
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
#       1  04/14/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::SetServiceTransforms {serviceName {inTransform {}} {outTransform {}}} {
    variable serviceArr

    dict set serviceArr($serviceName) inTransform $inTransform
    dict set serviceArr($serviceName) outTransform $outTransform

    return;
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::GetServiceTransforms
#
# Description : Define a service's transforms
#
# Arguments :
#       serviceName  - Service name to add namespace to
#
# Returns :     List of two elements: inTransform outTransform
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
#       1  04/14/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::GetServiceTransforms {serviceName} {
    variable serviceArr

    set inTransform [dict get serviceArr($serviceName) inTransform]
    set outTransform [dict get serviceArr($serviceName) outTransform]

    return [list $inTransform $outTransform]
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DefineRestMethod
#
# Description : Define a method
#
# Arguments :
#       serviceName - Service name to add namespace to
#       methodName  - The name of the method to add
#       inputArgs   - List of input argument definitions where each argument
#                       definition is of the format: name typeInfo
#       returnType  - The type, if any returned by the procedure.  Format is:
#                       xmlTag typeInfo
#
#  where, typeInfo is of the format {type typeName comment commentString}
#
# Returns :     The current service definition
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
#       1  04/14/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::DefineRestMethod {serviceName objectName operationName inputArgs returnType {location {}}} {
    variable serviceArr

    if {[lsearch -exact  [dict get $serviceArr($serviceName) objList] $objectName] == -1} {
        dict lappend serviceArr($serviceName) objList $objectName
    }
    if {![llength $location]} {
        set location [dict get $serviceArr($serviceName) location]
    }

    if {![string equal $inputArgs {}]} {
        set inType $objectName.$operationName.Request
        ::WS::Utils::ServiceTypeDef Client $serviceName $inType $inputArgs
    } else {
        set inType {}
    }
    if {![string equal $returnType {}]} {
        set outType $objectName.$operationName.Results
        ::WS::Utils::ServiceTypeDef Client $serviceName $outType $returnType
    } else {
        set outType {}
    }

    dict set serviceArr($serviceName) object $objectName location $location
    dict set serviceArr($serviceName) object $objectName operation $operationName inputs $inType
    dict set serviceArr($serviceName) object $objectName operation $operationName outputs $outType

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::ImportNamespace
#
# Description : Import and additional namespace into the service
#
# Arguments :
#       serviceName - Service name to add namespace to
#       url         - URL of namespace file to import
#
# Returns :     The local alias (tns)
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
#       1  01/30/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::ImportNamespace {serviceName url} {
    variable serviceArr

    switch -exact -- [dict get [::uri::split $url] scheme] {
        file {
            upvar #0 [::uri::geturl $url] token
            set xml $token(data)
            unset token
        }
        http {
            set token [::http::geturl $url]
            ::http::wait $token
            set ncode [::http::ncode $token]
            set xml [::http::data $token]
            ::http::cleanup $token
            if {$ncode != 200} {
                return \
                    -code error \
                    -errorcode [list WS CLIENT HTTPFAIL $url] \
                    "HTTP get of import file failed '$url'"
            }
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKURLTYP $url] \
                "Unknown URL type '$url'"
        }
    }
    set tnsCount [llength [dict get $serviceArr($serviceName) targetNamespace]]
    set serviceInfo $serviceArr($serviceName)
    dict lappend serviceInfo imports $url
    ::WS::Utils::ProcessImportXml Client $url $xml $serviceName serviceInfo tnsCount
    set serviceArr($serviceName) $serviceInfo
    set result {}
    foreach pair [dict get $serviceArr($serviceName) targetNamespace] {
        if {[string equal [lindex $pair 1] $url]} {
            set result [lindex $pair 0]
        }
    }
    return $result
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::GetOperationList
#
# Description : Import and additional namespace into the service
#
# Arguments :
#       serviceName - Service name to add namespace to
#
# Returns :     A list of operations names.
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
#       1  01/30/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::GetOperationList {serviceName {object {}}} {
    variable serviceArr

    if {[string equal $object {}]} {
        return [dict get $serviceArr($serviceName) operList]
    } else {
        return [dict get $serviceArr($serviceName) operation $object inputs]
    }

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::AddInputHeader
#
# Description : Import and additional namespace into the service
#
# Arguments :
#       serviceName - Service name to of the oepration
#       operation   - name of operation to add an input header to
#       headerType  - the type name to add as a header
#       attrList    - list of name value pairs of attributes and their
#                     values to add to the XML
#
# Returns :     Nothing
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
#       1  01/30/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::AddInputHeader {serviceName operationName headerType {attrList {}}} {
    variable serviceArr

    set serviceInfo $serviceArr($serviceName)
    set soapRequestHeader [dict get $serviceInfo operation $operationName soapRequestHeader]
    lappend soapRequestHeader [list $headerType $attrList]
    dict set serviceInfo operation $operationName soapRequestHeader $soapRequestHeader
    set serviceArr($serviceName) $serviceInfo
    return ;

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::AddOutputHeader
#
# Description : Import and additional namespace into the service
#
# Arguments :
#       serviceName - Service name to of the oepration
#       operation   - name of operation to add an output header to
#       headerType  - the type name to add as a header
#       attrList    - list of name value pairs of attributes and their
#                     values to add to the XML
#
# Returns :     Nothing
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
#       1  01/30/2009  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::AddOutputHeader {serviceName operation headerType} {
    variable serviceArr

    set serviceInfo $serviceArr($serviceName)
    set soapReplyHeader [dict get $serviceInfo operation $operationName soapReplyHeader]
    lappend soapReplyHeader $headerType
    dict set serviceInfo operation $operationName soapReplyHeader $soapReplyHeader
    set serviceArr($serviceName) $serviceInfo
    return ;

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::LoadParsedWsdl
#
# Description : Load a saved service definition in
#
# Arguments :
#       serviceInfo - parsed service definition, as returned from
#                     ::WS::Client::ParseWsdl or ::WS::Client::GetAndParseWsdl
#       headers     - Extra headers to add to the HTTP request. This
#                       is a key value list argument. It must be a list with
#                       an even number of elements that alternate between
#                       keys and values. The keys become header field names.
#                       Newlines are stripped from the values so the header
#                       cannot be corrupted.
#                       This is an optional argument and defaults to {}.
#       serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns :     The name of the service loaded
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
#       1  07/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::LoadParsedWsdl {serviceInfo {headers {}} {serviceAlias {}}} {
    variable serviceArr

    if {[string length $serviceAlias]} {
        set serviceName $serviceAlias
    } else {
        set serviceName [dict get $serviceInfo name]
    }
    if {[llength $headers]} {
        dict set serviceInfo headers $headers
    }
    set serviceArr($serviceName) $serviceInfo

    if {[dict exists $serviceInfo types]} {
        foreach {typeName partList} [dict get $serviceInfo types] {
            set definition [dict get $partList definition]
            set xns [dict get $partList xns]
            if {[string equal [lindex [split $typeName {:}] 1] {}]} {
                ::WS::Utils::ServiceTypeDef Client $serviceName $typeName $definition tns1
            } else {
                #set typeName [lindex [split $typeName {:}] 1]
                ::WS::Utils::ServiceTypeDef Client $serviceName $typeName $definition $xns
            }
        }
    }

    if {[dict exists $serviceInfo simpletypes]} {
        foreach partList [dict get $serviceInfo simpletypes] {
            lassign $partList typeName definition
            if {[string equal [lindex [split $typeName {:}] 1] {}]} {
                ::WS::Utils::ServiceSimpleTypeDef Client $serviceName $typeName $definition tns1
            } else {
                set xns [lindex [split $typeName {:}] 0]
                #set typeName [lindex [split $typeName {:}] 1]
                ::WS::Utils::ServiceSimpleTypeDef Client $serviceName $typeName $definition $xns
            }
        }
    }

    return $serviceName
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::GetAndParseWsdl
#
# Description :
#
# Arguments :
#       url     - The url of the WSDL
#       headers     - Extra headers to add to the HTTP request. This
#                       is a key value list argument. It must be a list with
#                       an even number of elements that alternate between
#                       keys and values. The keys become header field names.
#                       Newlines are stripped from the values so the header
#                       cannot be corrupted.
#                       This is an optional argument and defaults to {}.
#       serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns : The parsed service definition
#
# Side-Effects : None
#
# Exception Conditions : None
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
proc ::WS::Client::GetAndParseWsdl {url {headers {}} {serviceAlias {}}} {
    variable currentBaseUrl

    set currentBaseUrl $url
    switch -exact -- [dict get [::uri::split $url] scheme] {
        file {
            upvar #0 [::uri::geturl $url] token
            set wsdlInfo [ParseWsdl $token(data) -headers $headers -serviceAlias $serviceAlias]
            unset token
        }
        http -
        https {
            if {[llength $headers]} {
                set token [::http::geturl $url -headers $headers]
            } else {
                set token [::http::geturl $url]
            }
            ::http::wait $token
            set wsdlInfo [ParseWsdl [::http::data $token] -headers $headers -serviceAlias $serviceAlias]
            ::http::cleanup $token
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKURLTYP $url] \
                "Unknown URL type '$url'"
        }
    }
    set currentBaseUrl {}

    return $wsdlInfo
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::ParseWsdl
#
# Description : Parse a WSDL
#
# Arguments :
#       wsdlXML - XML of the WSDL
#
# Optional Arguments:
#       -createStubs 0|1 - create stub routines for the service
#                               NOTE -- Webservice arguments are position
#                                       independent, thus the proc arguments
#                                       will be defined in alphabetical order.
#       -headers         - Extra headers to add to the HTTP request. This
#                          is a key value list argument. It must be a list with
#                          an even number of elements that alternate between
#                          keys and values. The keys become header field names.
#                          Newlines are stripped from the values so the header
#                          cannot be corrupted.
#                          This is an optional argument and defaults to {}.
#       -serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns : The parsed service definition
#
# Side-Effects : None
#
# Exception Conditions :None
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
proc ::WS::Client::ParseWsdl {wsdlXML args} {
    variable serviceArr

    array set defaults {
        -createStubs    0
        -headers        {}
        -serviceAlias   {}
    }

    array set defaults $args

    dom parse $wsdlXML tmpdoc
    $tmpdoc xslt $::WS::Utils::xsltSchemaDom wsdlDoc
    $tmpdoc delete
    $wsdlDoc documentElement wsdlNode
    set nsCount 1
    set targetNs [$wsdlNode getAttribute targetNamespace]
    dict set nsDict url $targetNs tns$nsCount
    foreach itemList [$wsdlNode attributes xmlns:*] {
        set ns [lindex $itemList 0]
        set url [$wsdlNode getAttribute xmlns:$ns]
        if {[dict exists $nsDict url $url]} {
            set tns [dict get $nsDict url $url]
        } else {
            ##
            ## Check for hardcoded namespaces
            ##
            switch -exact -- $url {
                http://schemas.xmlsoap.org/wsdl/ {
                    set tns w
                }
                http://schemas.xmlsoap.org/wsdl/soap/ {
                    set tns d
                }
                http://www.w3.org/2001/XMLSchema {
                    set tns xs
                }
                default {
                    set tns tns[incr nsCount]
                }
            }
            dict set nsDict url $url $tns
        }
        dict set nsDict tns $ns $tns
    }
    $wsdlDoc selectNodesNamespaces {
        w http://schemas.xmlsoap.org/wsdl/
        d http://schemas.xmlsoap.org/wsdl/soap/
        xs http://www.w3.org/2001/XMLSchema
    }
    if {[string length $defaults(-serviceAlias)]} {
        set serviceAlias $defaults(-serviceAlias)
    } else {
        set serviceAlias {}
    }

    set serviceInfo {}

    foreach serviceInfo [buildServiceInfo $wsdlNode $nsDict $serviceInfo $serviceAlias] {
        set serviceName [dict get $serviceInfo name]

        if {[llength $defaults(-headers)]} {
            dict set serviceInfo headers $defaults(-headers)
        }
        dict set serviceInfo types [::WS::Utils::GetServiceTypeDef Client $serviceName]
        dict set serviceInfo simpletypes [::WS::Utils::GetServiceSimpleTypeDef Client $serviceName]

        set serviceArr($serviceName) $serviceInfo

        if {$defaults(-createStubs)} {
            catch {namespace delete $serviceName}
            namespace eval $serviceName {}
            CreateStubs $serviceName
        }
    }

    $wsdlDoc delete

    return $serviceInfo

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::CreateStubs
#
# Description : Create stubs routines to make calls to Webservice Operations.
#               All routines will be create in a namespace that is the same
#               as the service name.  The procedure name will be the same
#               as the operation name.
#
#               NOTE -- Webservice arguments are position independent, thus
#                       the proc arguments will be defined in alphabetical order.
#
# Arguments :
#       serviceName     - The service to create stubs for
#
# Returns : A string describing the created procedures.
#
# Side-Effects : Existing namespace is deleted.
#
# Exception Conditions : None
#
# Pre-requisite Conditions : Service must have been defined.
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
proc ::WS::Client::CreateStubs {serviceName} {
    variable serviceArr

    namespace eval [format {::%s::} $serviceName] {}

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }

    set serviceInfo $serviceArr($serviceName)

    set procList {}

    foreach operationName [dict get $serviceInfo operList] {
        if {[dict get $serviceInfo operation $operationName cloned]} {
            continue
        }
        set procName [format {::%s::%s} $serviceName $operationName]
        set argList {}
        foreach inputHeaderTypeItem [dict get $serviceInfo operation $operationName soapRequestHeader] {
            set  inputHeaderType [lindex $inputHeaderTypeItem 0]
            if {[string equal $inputHeaderType {}]} {
                continue
            }
            set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
            set headerFields [dict keys [dict get $headerTypeInfo definition]]
            if {![string equal $headerFields {}]} {
                lappend argList [lsort -dictionary $headerFields]
            }
        }
        set inputMsgType [dict get $serviceInfo operation $operationName inputs]
        ## Petasis, 14 July 2008: If an input message has no elements, just do
        ## not add any arguments...
        set inputMsgTypeDefinition [::WS::Utils::GetServiceTypeDef Client $serviceName $inputMsgType]
        if {[dict exists $inputMsgTypeDefinition definition]} {
          set inputFields [dict keys [dict get $inputMsgTypeDefinition definition]]
         } else {
          ::log::log debug "no definition found for inputMsgType $inputMsgType"
          set inputFields {}
        }
        if {![string equal $inputFields {}]} {
            lappend argList [lsort -dictionary $inputFields]
        }
        set argList [join $argList]

        set body {
            set procName [lindex [info level 0] 0]
            set serviceName [string trim [namespace qualifiers $procName] {:}]
            set operationName [string trim [namespace tail $procName] {:}]
            set argList {}
            foreach var [namespace eval ::${serviceName}:: [list info args $operationName]] {
                lappend argList $var [set $var]
            }
            ::log::log debug [list ::WS::Client::DoCall $serviceName $operationName $argList]
            ::WS::Client::DoCall $serviceName $operationName $argList
        }
        proc $procName $argList $body
        append procList "\n\t[list $procName $argList]"
    }
    return "$procList\n"
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoRawCall
#
# Description : Call an operation of a web service
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object.
#                         This is for both the Soap Header and Body messages.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       The XML of the operation.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#
# Pre-requisite Conditions :    Service must have been defined.
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
proc ::WS::Client::DoRawCall {serviceName operationName argList {headers {}}} {
    variable serviceArr

    ::log::log debug "Entering ::WS::Client::DoRawCall {$serviceName $operationName $argList}"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $operationName]] \
            "Unknown operation '$operationName' for service '$serviceName'"
    }
    set url [dict get $serviceInfo location]
    set query [buildCallquery $serviceName $operationName $url $argList]
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[dict exists $serviceInfo operation $operationName action]} {
        lappend headers  SOAPAction [dict get $serviceInfo operation $operationName action]
    }
    if {[llength $headers]} {
        set token [::http::geturl $url -query $query -type text/xml -headers $headers]
    } else {
        set token [::http::geturl $url -query $query -type text/xml]
    }
    ::http::wait $token

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    if {![string equal [::http::status $token] ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        set errorInfo {}
        set results [::http::error $token]
        set hadError 1
    } else {
        set hadError 0
        set results [::http::data $token]
    }
    ::http::cleanup $token
    if {$hadError} {
        ::log::log debug "Leaving (error) ::WS::Client::DoRawCall"
        return \
            -code error \
            -errorcode $errorCode \
            -errorinfo $errorInfo \
            $results
    } else {
        ::log::log debug "Leaving ::WS::Client::DoRawCall with {$results}"
        return $results
    }

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoCall
#
# Description : Call an operation of a web service
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       The return value of the operation as a dictionary object.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#       others                  - as raised by called Operation
#
# Pre-requisite Conditions :    Service must have been defined.
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
proc ::WS::Client::DoCall {serviceName operationName argList {headers {}}} {
    variable serviceArr

    ::log::log debug "Entering ::WS::Client::DoCall {$serviceName $operationName $argList}"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $operationName]] \
            "Unknown operation '$operationName' for service '$serviceName'"
    } elseif {[dict get $serviceInfo operation $operationName cloned]} {
        return \
            -code error \
            -errorcode [list WS CLIENT MUSTCALLCLONE [list $serviceName $operationName]] \
            "Operation '$operationName' for service '$serviceName' is overloaded, you must call one of its clones."
    }

    set url [dict get $serviceInfo location]
    set query [buildCallquery $serviceName $operationName $url $argList]
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[dict exists $serviceInfo operation $operationName action]} {
        lappend headers  SOAPAction [dict get $serviceInfo operation $operationName action]
    }
    if {[llength $headers]} {
        ::log::log debug [list ::http::geturl $url -query $query -type text/xml -headers $headers]
        set token [::http::geturl $url -query $query -type text/xml -headers $headers]
    } else {
        ::log::log debug  [list ::http::geturl $url -query $query -type text/xml]
        set token [::http::geturl $url -query $query -type text/xml]
    }
    ::http::wait $token

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    ::log::log debug "\tReceived: $body"
    set httpStatus [::http::status $token]
    if {![string equal $httpStatus ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        ::log::log debug "\tHTTP error [array get $token]"
        set results [::http::error $token]
        if {[string equal $results {}] || [string equal $httpStatus eof]} {
            set results {Unexpected EOF received from Server}
            set errorCode [list WSCLIENT HTTPERROR UNEXPEOF]
        } else {
            set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        }
        set errorInfo {}
        set hadError 1
    } else {
        set outTransform [dict get $serviceInfo outTransform]
        if {![string equal $outTransform {}]} {
            set body [$outTransform $serviceName $operationName REPLY $body]
        }
        set hadError [catch {parseResults $serviceName $operationName $body} results]
        if {$hadError} {
            ::log::log debug "Reply was $body"
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }
    ::http::cleanup $token
    if {$hadError} {
        ::log::log debug "Leaving (error) ::WS::Client::DoCall"
        return \
            -code error \
            -errorcode $errorCode \
            -errorinfo $errorInfo \
            $results
    } else {
        ::log::log debug "Leaving ::WS::Client::DoCall with {$results}"
        return $results
    }

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoAsyncCall
#
# Description : Call an operation of a web service asynchronously
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       succesCmd       - A command prefix to be called if the operations
#                         does not raise an error.  The results, as a dictionary
#                         object are concatinated to the prefix.
#       errorCmd        - A command prefix to be called if the operations
#                         raises an error.  The error code and stack trace
#                         are concatinated to the prefix.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       None.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#       others                  - as raised by called Operation
#
# Pre-requisite Conditions :    Service must have been defined.
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
proc ::WS::Client::DoAsyncCall {serviceName operationName argList succesCmd errorCmd {headers {}}} {
    variable serviceArr

    ::log::log debug "Entering ::WS::Client::DoAsyncCall [list $serviceName $operationName $argList $succesCmd $errorCmd $headers]"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $operationName]] \
            "Unknown operation '$operationName' for service '$serviceName'"
    }
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    set url [dict get $serviceInfo location]
    set query [buildCallquery $serviceName $operationName $url $argList]
    if {[llength $headers]} {
        ::http::geturl $url \
            -query $query \
            -type text/xml \
            -headers $headers \
            -command [list ::WS::Client::asyncCallDone $serviceName $operationName $succesCmd $errorCmd]
    } else {
        ::http::geturl $url \
            -query $query \
            -type text/xml \
            -command [list ::WS::Client::asyncCallDone $serviceName $operationName $succesCmd $errorCmd]
    }
    ::log::log debug "Leaving ::WS::Client::DoAsyncCall"
    return;
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::List
#
# Description : List a Webservice's Operations.
#
#               NOTE -- Webservice arguments are position independent, thus
#                       the proc arguments will be defined in alphabetical order.
#
# Arguments :
#       serviceName     - The service to create stubs for
#
# Returns : A string describing the operations.
#
# Side-Effects : Existing namespace is deleted.
#
# Exception Conditions : None
#
# Pre-requisite Conditions : Service must have been defined.
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
#       1  10/11/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::List {serviceName} {
    variable serviceArr

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }

    set serviceInfo $serviceArr($serviceName)

    set procList {}

    foreach operationName [dict get $serviceInfo operList] {
        if {[dict get $serviceInfo operation $operationName cloned]} {
            continue
        }
        set procName $operationName
        set argList {}
        foreach inputHeaderTypeItem [dict get $serviceInfo operation $operationName soapRequestHeader] {
            set inputHeaderType [lindex $inputHeaderTypeItem 0]
            if {[string equal $inputHeaderType {}]} {
                continue
            }
            set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
            set headerFields [dict keys [dict get $headerTypeInfo definition]]
            if {![string equal $headerFields {}]} {
                lappend argList [lsort -dictionary $headerFields]
            }
        }
        set inputMsgType [dict get $serviceInfo operation $operationName inputs]
        set inputFields [dict keys [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputMsgType] definition]]
        if {![string equal $inputFields {}]} {
            lappend argList [lsort -dictionary $inputFields]
        }
        set argList [join $argList]

        append procList "\n\t$procName $argList"
    }
    return "$procList\n"
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::ListRest
#
# Description : List a Webservice's Operations.
#
#               NOTE -- Webservice arguments are position independent, thus
#                       the proc arguments will be defined in alphabetical order.
#
# Arguments :
#       serviceName     - The service to create stubs for
#
# Returns : A string describing the operations.
#
# Side-Effects : Existing namespace is deleted.
#
# Exception Conditions : None
#
# Pre-requisite Conditions : Service must have been defined.
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
#       1  10/11/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::ListRest {serviceName} {
    variable serviceArr

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }

    set serviceInfo $serviceArr($serviceName)

    set procList {}

    foreach object [dict get $serviceInfo objList] {
        foreach operationName [dict keys [dict get $serviceInfo object $object operations]] {
            set procName $operationName
            set argList {}
            foreach inputHeaderTypeItem [dict get $serviceInfo operation $operationName soapRequestHeader] {
                set inputHeaderType [lindex $inputHeaderTypeItem 0]
                if {[string equal $inputHeaderType {}]} {
                    continue
                }
                set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
                set headerFields [dict keys [dict get $headerTypeInfo definition]]
                if {![string equal $headerFields {}]} {
                    lappend argList [lsort -dictionary $headerFields]
                }
            }
            set inputMsgType [dict get $serviceInfo operation $operationName inputs]
            set inputFields [dict keys [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputMsgType] definition]]
            if {![string equal $inputFields {}]} {
                lappend argList [lsort -dictionary $inputFields]
            }
            set argList [join $argList]

            append procList "\n\t$object $procName $argList"
        }
    }
    return "$procList\n"
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::asyncCallDone
#
# Description : Called when an asynchronous call is complete.  This routine
#               will call either the success or error callback depending on
#               if the operation succeeded or failed -- assuming the callback
#               is defined.
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    succesCmd          - the command prefix to call if no error
#    errorCmd           - the command prefix to call on an error
#    token              - the token from the HTTP request
#
# Returns : Nothing
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
proc ::WS::Client::asyncCallDone {serviceName operationName succesCmd errorCmd token} {
    ::log::log debug "Entering ::WS::Client::asyncCallDone {$serviceName $operationName $succesCmd $errorCmd $token}"

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    if {![string equal [::http::status $token] ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        set hadError 1
        set errorInfo [::http::error $token]
    } else {
        set hadError [catch {parseResults $serviceName $operationName $body} results]
        if {$hadError} {
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }

    ##
    ## Call the appropriate callback
    ##
    if {$hadError} {
        set cmd $errorCmd
        lappend cmd $errorCode $errorInfo
    } else {
        set cmd $succesCmd
    }
    lappend cmd $results
    catch $cmd

    ##
    ## All done
    ##
    ::http::cleanup $token
    return;
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseResults
#
# Description : Convert the returned XML into a dictionary object
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    inXML              - the XML returned by the operation
#
# Returns : A dictionary object representing the results
#
# Side-Effects : None
#
# Exception Conditions :
#       WSCLIENT REMERR         - The remote end raised an exception, the third element of
#                                 the error code is the remote fault code.
#                                 Error info is set to the remote fault details.
#                                 The error message is the remote fault string;
#       WSCLIENT BADREPLY       - Badly formatted reply, the third element is a list of
#                                 what message type was received vs what was expected.
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
proc ::WS::Client::parseResults {serviceName operationName inXML} {
    variable serviceArr

    ::log::log debug "In parseResults $serviceName $operationName {$inXML}"

    set serviceInfo $serviceArr($serviceName)

    set expectedMsgType [dict get $serviceInfo operation $operationName outputs]
    set expectedMsgTypeBase [lindex [split $expectedMsgType {:}] end]

    dom parse $inXML doc
    $doc documentElement top
    set xns {
        ENV http://schemas.xmlsoap.org/soap/envelope/
        xsi "http://www.w3.org/2001/XMLSchema-instance"
        xs "http://www.w3.org/2001/XMLSchema"
    }
    foreach tmp [dict get $serviceInfo targetNamespace] {
        lappend xns [lindex $tmp 0] [lindex $tmp 1]
    }
    ::log::log debug "Using namespaces {$xns}"
    $doc selectNodesNamespaces $xns
    set body [$top selectNodes ENV:Body]
    set rootNode [$body childNodes]
    ::log::log debug "Have [llength $rootNode]"
    if {[llength $rootNode] > 1} {
        foreach tmp $rootNode {
            #puts "\t Got {[$tmp localName]} looking for {$expectedMsgTypeBase}"
            if {[string equal [$tmp localName] $expectedMsgTypeBase] ||
                [string equal [$tmp nodeName] $expectedMsgTypeBase] ||
                [string equal [$tmp localName] Fault] ||
                [string equal [$tmp nodeName] Fault]} {
                set rootNode $tmp
                break
            }
        }
    }
    if {([llength $rootNode] == 1) && ![string equal $rootNode {}]} {
        set rootName [$rootNode localName]
        if {[string equal $rootName {}]} {
            set rootName [$rootNode nodeName]
        }
    } else {
        set rootName {}
    }
    ::log::log debug "root name is {$rootName}"

    ##
    ## See if it is a standard error packet
    ##
    if {[string equal $rootName {Fault}]} {
        set faultcode {}
        set faultstring {}
        set detail {}
        foreach item {faultcode faultstring detail} {
            set tmpNode [$rootNode selectNodes ENV:$item]
            if {[string equal $tmpNode {}]} {
                set tmpNode [$rootNode selectNodes $item]
            }
            if {![string equal $tmpNode {}]} {
                if {[$tmpNode hasAttribute href]} {
                    set tmpNode [GetReferenceNode $top [$tmpNode getAttribute href]]
                }
                set $item [$tmpNode asText]
            }
        }
        $doc delete
        return \
            -code error \
            -errorcode [list WSCLIENT REMERR $faultcode] \
            -errorinfo $detail \
            $faultstring
    }

    ##
    ## Validated that it is the expected packet type
    ##
    if {![string equal $rootName $expectedMsgTypeBase]} {
        $doc delete
        return \
            -code error \
            -errorcode [list WSCLIENT BADREPLY [list $rootName $expectedMsgTypeBase]] \
            "Bad reply type, received '$rootName; but expected '$expectedMsgTypeBase'."
    }

    ##
    ## Convert the packet to a dictionary
    ##
    set results {}
    set headerRootNode [$top selectNodes ENV:Header]
    foreach outHeaderType [dict get $serviceInfo operation $operationName soapReplyHeader] {
        if {[string equal $outHeaderType {}]} {
            continue
        }
        set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $outputHeaderType] xns]
        set node [$headerRootNode selectNodes $xns:outHeaderType]
        if {[llength $outHeaderAttrs]} {
            ::WS::Utils::setAttr $node $outHeaderAttrs
        }
        ::log::log debug "Calling [list ::WS::Utils::convertTypeToDict Client $serviceName $node $outHeaderType $headerRootNode]"
        lappend results [::WS::Utils::convertTypeToDict Client $serviceName $node $outHeaderType $headerRootNode]
    }
    ::log::log debug "Calling [list ::WS::Utils::convertTypeToDict Client $serviceName $rootNode $expectedMsgType $body]"
    if {![string equal $rootName {}]} {
        lappend results [::WS::Utils::convertTypeToDict \
                         Client $serviceName $rootNode $expectedMsgType $body]
    }
    set results [join $results]
    $doc delete
    set ::errorCode {}
    set ::errorInfo {}

    return $results

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildCallquery
#
# Description : Build the XML request message for the call
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    url                - the URL of the operation
#    argList            - a dictionary object of the calling arguments
#
# Returns : The XML for the call
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
proc ::WS::Client::buildCallquery {serviceName operationName url argList} {
    variable serviceArr

    set serviceInfo $serviceArr($serviceName)

    set style [dict get $serviceInfo operation $operationName style]
    set suppressTargetNS [dict get $serviceInfo suppressTargetNS]
    if {$suppressTargetNS} {
        ::WS::Utils::SetOption suppressNS tns1
    } else {
        ::WS::Utils::SetOption suppressNS {}
    }

    switch -exact -- $style {
        document/literal {
            set xml [buildDocLiteralCallquery $serviceName $operationName $url $argList]
        }
        rpc/encoded {
            set xml [buildRpcEncodedCallquery $serviceName $operationName $url $argList]
        }
        default {
            return \
                -code error
                "Unsupported Style '$style'"
        }
    }

    ::WS::Utils::SetOption suppressNS {}
    set inTransform [dict get $serviceInfo inTransform]
    if {![string equal $inTransform {}]} {
        set query [$inTransform $serviceName $operationName REQUEST $xml $url $argList]
    }

    ::log::log debug "Leaving ::::WS::Client::buildCallquery with {$xml}"
    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildDocLiteralCallquery
#
# Description : Build the XML request message for the call
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    url                - the URL of the operation
#    argList            - a dictionary object of the calling arguments
#                         This is for both the Soap Header and Body messages.
#
# Returns : The XML for the call
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
proc ::WS::Client::buildDocLiteralCallquery {serviceName operationName url argList} {
    variable serviceArr

    ::log::log debug "Entering [info level 0]"
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo operation $operationName inputs]
    set url [dict get $serviceInfo location]
    set xnsList [dict get $serviceInfo targetNamespace]

    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute \
        "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" \
        "xmlns:SOAP-ENC" "http://schemas.xmlsoap.org/soap/encoding/" \
        "xmlns:xsi"      "http://www.w3.org/2001/XMLSchema-instance" \
        "xmlns:xs"      "http://www.w3.org/2001/XMLSchema"
    array set tnsArray {}
    array unset tnsArray *
    foreach xns $xnsList {
        set tns [lindex $xns 0]
        set target [lindex $xns 1]
        set tnsArray($target) $tns
        $env  setAttribute \
            xmlns:$tns $target
    }
    #parray tnsArray

    set firstHeader 1
    foreach inputHeaderTypeItem [dict get $serviceInfo operation $operationName soapRequestHeader] {
        lassign $inputHeaderTypeItem inputHeaderType attrList
        if {[string equal $inputHeaderType {}]} {
            continue
        }
        set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType] xns]
        if {[info exists tnsArray($xns)]} {
            set xns $tnsArray($xns)
        }
        if {$firstHeader} {
            $env appendChild [$doc createElement "SOAP-ENV:Header" header]
            set firstHeader 0
        }
        $header appendChild [$doc createElement $xns:$inputHeaderType headerData]
        if {[llength $attrList]} {
            ::WS::Utils::setAttr $headerData $attrList
        }
        ::WS::Utils::convertDictToType Client $serviceName $doc $headerData $argList $inputHeaderType
    }

    $env appendChild [$doc createElement "SOAP-ENV:Body" bod]
    set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $msgType] xns]
    if {[info exists tnsArray($xns)]} {
        set xns $tnsArray($xns)
    }
    set typeInfo [split $msgType {:}]
    if {[llength $typeInfo] != 1} {
        set xns [lindex $typeInfo 0]
        set msgType [lindex $typeInfo 1]
    }

    if {[dict get $serviceInfo skipLevelWhenActionPresent] && [dict exists $serviceInfo operation $operationName action]} {
        set reply $bod
    } else {
        ::log::log debug [list $bod appendChild \[$doc createElement $xns:$msgType reply\]]
        $bod appendChild [$doc createElement $xns:$msgType reply]
    }

    ::WS::Utils::convertDictToType Client $serviceName $doc $reply $argList $xns:$msgType

    append xml  \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$doc asXML -indent none -doctypeDeclaration 0]
    #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] {} xml
    $doc delete

    ::log::log debug "Leaving ::::WS::Client::buildDocLiteralCallquery with {$xml}"

    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildRpcEncodedCallquery
#
# Description : Build the XML request message for the call
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    url                - the URL of the operation
#    argList            - a dictionary object of the calling arguments
#                         This is for both the Soap Header and Body messages.
#
# Returns : The XML for the call
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
proc ::WS::Client::buildRpcEncodedCallquery {serviceName operationName url argList} {
    variable serviceArr

    ::log::log debug "Entering [info level 0]"
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo operation $operationName inputs]
    #set url [dict get $serviceInfo location]
    set xnsList [dict get $serviceInfo targetNamespace]
    #set action [dict get $serviceInfo operation $operationName action]

    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute \
        xmlns:SOAP-ENV "http://schemas.xmlsoap.org/soap/envelope/" \
        xmlns:xsi      "http://www.w3.org/2001/XMLSchema-instance" \
        xmlns:xs      "http://www.w3.org/2001/XMLSchema"

    foreach xns $xnsList {
        set tns [lindex $xns 0]
        set target [lindex $xns 1]
        $env setAttribute xmlns:$tns $target
    }

    set firstHeader 1
    foreach inputHeaderType [dict get $serviceInfo operation $operationName soapRequestHeader] {
        if {[string equal $inputHeaderType {}]} {
            continue
        }
        set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType] xns]
        if {$firstHeader} {
            $env appendChild [$doc createElement "SOAP-ENV:Header" header]
            set firstHeader 0
        }
        $header appendChild [$doc createElement $xns:$inputHeaderType headerData]
        ::WS::Utils::convertDictToEncodedType Client $serviceName $doc $headerData $argList $inputHeaderType
    }

    $env appendChild [$doc createElement "SOAP-ENV:Body" bod]
    set baseName [dict get $serviceInfo operation $operationName name]

    set callXns [dict get $serviceInfo operation $operationName xns]
    if {![string is space $callXns]} {
        $bod appendChild [$doc createElement $callXns:$baseName reply]
    } else {
        $bod appendChild [$doc createElement $baseName reply]
    }
    $reply  setAttribute \
        SOAP-ENV:encodingStyle "http://schemas.xmlsoap.org/soap/encoding/"

    ::WS::Utils::convertDictToEncodedType Client $serviceName $doc $reply $argList $msgType

    append xml  \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$doc asXML -indent none -doctypeDeclaration 0]
    #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] {} xml
    $doc delete
    ::log::log debug "Leaving ::::WS::Client::buildRpcEncodedCallquery with {$xml}"

    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildServiceInfo
#
# Description : Parse the WSDL into our internal representation
#
# Arguments :
#    wsdlNode   - The top node of the WSDL
#    results    - Inital definition. This is optional and defaults to no definition.
#    serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#
# Returns : The parsed WSDL
#
# Side-Effects : Defines Client mode types as specified by the WSDL
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
proc ::WS::Client::buildServiceInfo {wsdlNode tnsDict {serviceInfo {}} {serviceAlias {}}} {
    ##
    ## Need to refactor to foreach service parseService
    ##  Service drills down to ports, which drills down to bindings and messages
    ##
    ::log::log debug "Entering ::WS::Client::buildServiceInfo with doc $wsdlNode"

    ##
    ## Parse Service information
    ##
    set serviceNameList [$wsdlNode selectNodes w:service]
    if {[string length $serviceAlias] & ([llength $serviceNameList] > 1)} {
        return \
            -code error \
            -errorcode [list WS CLIENT MULTISVC] \
            "Can not specify alias when WSDL defines multiple services"
    } elseif {[llength $serviceNameList] == 0} {
        return \
            -code error \
            -errorcode [list WS CLIENT NOSVC] \
            "WSDL does not define any services"
    }


    foreach serviceNode $serviceNameList {
        lappend serviceInfo [parseService $wsdlNode $serviceNode $serviceAlias $tnsDict]
    }

    return $serviceInfo
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseService
#
# Description : Parse a service from a WSDL into our internal representation
#
# Arguments :
#    wsdlNode     - The top node of the WSDL
#    serviceNode  - The DOM node for the service.
#    serviceAlias - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name of the
#                       service in serviceInfo.
#    tnsDict       - Dictionary of URI to namespaces used
#
# Returns : The parsed service WSDL
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
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
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::parseService {wsdlNode serviceNode serviceAlias tnsDict} {
    variable serviceArr
    variable options

    if {[string length $serviceAlias]} {
        set serviceName $serviceAlias
    } else {
        set serviceName [$serviceNode getAttribute name]
    }
    set addressNodeList [$serviceNode getElementsByTagNameNS http://schemas.xmlsoap.org/wsdl/soap/ address]
    if {[llength $addressNodeList] == 1} {
        set addressNode [lindex $addressNodeList 0]
        set portNode [$addressNode parentNode]
        set location [$addressNode getAttribute location]
    } else {
        foreach addressNode $addressNodeList {
            set portNode [$addressNode parentNode]
            if {[$addressNode hasAttribute location]} {
                set location [$addressNode getAttribute location]
                break
            }
        }
    }
    if {![info exists location]} {
        return \
            -code error \
            -errorcode [list WS CLIENT NOSOAPADDR] \
            "Malformed WSDL -- No SOAP address node found."
    }

    set xns {}
    foreach url [dict keys [dict get $tnsDict url]] {
        lappend xns [list [dict get $tnsDict url $url] $url]
    }
    CreateService $serviceName WSDL $location xns $xns
    set serviceInfo $serviceArr($serviceName)
    dict set serviceInfo tnsList $tnsDict
    set bindingName [lindex [split [$portNode getAttribute binding] {:}] end]

    ##
    ## Parse types
    ##
    parseTypes $wsdlNode $serviceName serviceInfo

    ##
    ## Parse bindings
    ##
    parseBinding $wsdlNode $serviceName $bindingName serviceInfo

    ##
    ## All done, so return results
    ##
    dict unset serviceInfo tnsList
    dict set serviceInfo suppressTargetNS $options(suppressTargetNS)
    set serviceArr($serviceName) $serviceInfo
    return $serviceInfo
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseTypes
#
# Description : Parse the types for a service from a WSDL into
#               our internal representation
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceNode    - The DOM node for the service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#
# Returns : Nothing
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
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
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::parseTypes {wsdlNode serviceName serviceInfoVar} {
    ::log:::log debug "Entering [info level 0]"

    upvar 1 $serviceInfoVar serviceInfo


    set tnsCount [llength [dict keys [dict get $serviceInfo tnsList url]]]
    set baseUrl [dict get $serviceInfo location]
    foreach schemaNode [$wsdlNode selectNodes w:types/xs:schema] {
        ::WS::Utils::parseScheme Client $baseUrl $schemaNode $serviceName serviceInfo tnsCount
    }

    ::log:::log debug "Leaving [lindex [info level 0] 0]"
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseBinding
#
# Description : Parse the bindings for a service from a WSDL into our
#               internal representation
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceName    - The name service.
#    bindingName    - The name binding we are to parse.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#
# Returns : Nothing
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
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
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::parseBinding {wsdlNode serviceName bindingName serviceInfoVar} {
    ::log:::log debug "Entering [info level 0]"
    upvar 1 $serviceInfoVar serviceInfo
    variable options

    set bindQuery [format {w:binding[attribute::name='%s']} $bindingName]
    array set msgToOper {}
    foreach binding [$wsdlNode selectNodes $bindQuery] {
        array unset msgToOper *
        set portName [lindex [split [$binding  getAttribute type] {:}] end]
        ::log:::log debug "\t Processing binding '$bindingName' on port '$portName'"
        set operList [$binding selectNodes w:operation]
        set styleNode [$binding selectNodes d:binding]
        if {![info exists style]} {
            if {[catch {$styleNode getAttribute style} tmpStyle]} {
                set styleNode [$binding selectNodes {w:operation[1]/d:operation}]
                if {[string equal $styleNode {}]} {
                    ##
                    ## This binding is for a SOAP level other than 1.1
                    ##
                    ::log:::log debug "Skiping non-SOAP 1.1 binding [$binding asXML]"
                    continue
                }
                set style [$styleNode getAttribute style]
                #puts "Using style for first operation {$style}"
            } else {
                set style $tmpStyle
                #puts "Using style for first binding {$style}"
            }
            if {!([string equal $style document] || [string equal $style rpc])} {
                ::log:::log debug "Leaving [lindex [info level 0] 0] with error @1"
                return \
                    -code error \
                    -errorcode [list WSCLIENT UNSSTY $style] \
                    "Unsupported calling style: '$style'"
            }

            if {![info exists use]} {
                set use [[$binding selectNodes {w:operation[1]/w:input/d:body}] getAttribute use]
                if {!([string equal $style document] && [string equal $use literal]) &&
                    !([string equal $style rpc] && [string equal $use encoded])} {
                    ::log:::log debug "Leaving [lindex [info level 0] 0] with error @2"
                    return \
                        -code error \
                        -errorcode [list WSCLIENT UNSMODE $use] \
                        "Unsupported mode: $style/$use"
                }
            }
        }

        set style $style/$use

        ##
        ## Process each operation
        ##
        foreach oper $operList {
            set operName [$oper getAttribute name]
            set baseName $operName
            ::log:::log debug "\t Processing operation '$operName'"

            ##
            ## Check for overloading
            ##
            set inNode [$oper selectNodes w:input]
            if {[llength $inNode] == 1 && [$inNode hasAttribute name]} {
                set inName [$inNode getAttribute name]
            } else {
                set inName {}
            }
            if {[dict exists $serviceInfo operation $operName]} {
                if {!$options(allowOperOverloading)} {
                    return  -code error \
                            -errorcode [list WSCLIENT NOOVERLOAD $operName]
                }
                ##
                ## See if the existing operation needs to be cloned"
                ##
                set origType [lindex [split [dict get $serviceInfo operation $operName inputs] {:}] end]
                set newName ${operName}_${origType}
                if {![dict exists $serviceInfo operation $newName]} {
                    ##
                    ## Clone it
                    ##
                    dict set serviceInfo operation $baseName cloned 1
                    dict lappend serviceInfo operList $newName
                    dict set serviceInfo operation $newName [dict get $serviceInfo operation $operName]
                }
                set typeList [getTypesForPort $wsdlNode $serviceName $baseName $portName $inName serviceInfo $style]
                set operName ${operName}_[lindex [split [lindex $typeList 0] {:}] end]
                set cloneList [dict get $serviceInfo operation $baseName cloneList]
                lappend cloneList $operName
                dict set serviceInfo operation $baseName cloneList $cloneList
                dict set serviceInfo operation $operName isClone 1
            } else {
                set typeList [getTypesForPort $wsdlNode $serviceName $baseName $portName $inName serviceInfo $style]
                dict set serviceInfo operation $operName isClone 0
            }

            #puts "Processing operation $operName"
            set actionNode [$oper selectNodes d:operation]
            if {[string equal $actionNode {}]} {
                ::log:::log debug "Skiping operation with no action [$oper asXML]"
                continue
            }
            dict lappend serviceInfo operList $operName
            dict set serviceInfo operation $operName cloneList {}
            dict set serviceInfo operation $operName cloned 0
            dict set serviceInfo operation $operName name $baseName
            dict set serviceInfo operation $operName style $style
            catch {
                set action [$actionNode getAttribute soapAction]
                dict set serviceInfo operation $operName action $action
            }

            ##
            ## Get the input headers, if any
            ##
            set soapRequestHeaderList {{}}
            foreach inHeader [$oper selectNodes w:input/d:header] {
                ##set part [$inHeader getAttribute part]
                set tmp [$inHeader getAttribute use]
                if {![string equal $tmp $use]} {
                    ::log:::log debug "Leaving [lindex [info level 0] 0] with error @3"
                    return \
                        -code error \
                        -errorcode [list WSCLIENT MIXUSE $use $tmp] \
                        "Mixed usageage not supported!'"
                }
                set msgName [$inHeader getAttribute message]
                ::log:::log debug [list messageToType $wsdlNode $serviceName $baseName $msgName serviceInfo $style]
                set type [messageToType $wsdlNode $serviceName $baseName $msgName serviceInfo $style]
                lappend soapRequestHeaderList $type
            }
            dict set serviceInfo operation $operName soapRequestHeader $soapRequestHeaderList
            if {![dict exists [dict get $serviceInfo operation $operName] action]} {
                dict set serviceInfo operation $operName action $serviceName
            }

            ##
            ## Get the output header, if one
            ##
            set soapReplyHeaderList {{}}
            foreach outHeader [$oper selectNodes w:output/d:header] {
                ##set part [$outHeader getAttribute part]
                set tmp [$outHeader getAttribute use]
                if {![string equal $tmp $use]} {
                    ::log:::log debug "Leaving [lindex [info level 0] 0] with error @4"
                    return \
                        -code error \
                        -errorcode [list WSCLIENT MIXUSE $use $tmp] \
                        "Mixed usageage not supported!'"
                }
                set messagePath [$outHeader getAttribute message]
                set msgName [lindex [split $messagePath {:}] end]
                ::log:::log debug [list messageToType $wsdlNode $serviceName $baseName $msgName serviceInfo $style]
                set type [messageToType $wsdlNode $serviceName $baseName $msgName serviceInfo $style]
                lappend soapReplyHeaderList $type
            }
            dict set serviceInfo operation $operName soapReplyHeader $soapReplyHeaderList

            ##
            ## Validate that the input and output uses
            ##
            set inUse $use
            set outUse $use
            catch {set inUse [[$oper selectNodes w:input/d:body] getAttribute use]}
            catch {set outUse [[$oper selectNodes w:output/d:body] getAttribute use]}
            foreach tmp [list $inUse $outUse] {
                if {![string equal $tmp $use]} {
                    ::log:::log debug "Leaving [lindex [info level 0] 0] with error @5"
                    return \
                        -code error \
                        -errorcode [list WSCLIENT MIXUSE $use $tmp] \
                        "Mixed usageage not supported!'"
                }
            }
            set typeList [getTypesForPort $wsdlNode $serviceName $baseName $portName $inName serviceInfo $style]
            ::log:::log debug "\t Messages are {$typeList}"
            foreach type $typeList mode {inputs outputs} {
                dict set serviceInfo operation $operName $mode $type
            }
            ##
            ## Handle target namespace defined at WSDL level for older RPC/Encoded
            ##
            if {![dict exists $serviceInfo targetNamespace]} {
                catch {
                    #puts "attempting to get tragetNamespace"
                    dict set serviceInfo targetNamespace tns1 [[$oper selectNodes w:input/d:body] getAttribute namespace]
                }
            }
            set xns tns1
            dict set serviceInfo operation $operName xns $xns
        }
    }

    ::log:::log debug "Leaving [lindex [info level 0] 0]"
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::getTypesForPort
#
# Description : Get the types for a port.
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceNode    - The DOM node for the service.
#    operNode       - The DOM node for the operation.
#    portName       - The name of the port.
#    inName         - The name of the input message.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#   style           - style of call
#
# Returns : A list containing the input and output types
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
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
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::getTypesForPort {wsdlNode serviceName operName portName inName serviceInfoVar style} {
    ::log:::log debug "Enteringing [info level 0]"
    upvar 1 $serviceInfoVar serviceInfo

    set inType {}
    set outType {}

    #set portQuery [format {w:portType[attribute::name='%s']} $portName]
    #set portNode [lindex [$wsdlNode selectNodes $portQuery] 0]
    if {[string equal $inName {}]} {
        set operQuery [format {w:portType[attribute::name='%s']/w:operation[attribute::name='%s']} \
                        $portName $operName]
    } else {
        set operQuery [format {w:portType[attribute::name='%s']/w:operation[attribute::name='%s']/w:input[attribute::name='%s']/parent::*} \
                        $portName $operName $inName]
    }
    set operNode [$wsdlNode selectNodes $operQuery]

    set inputMsgNode [$operNode selectNodes {w:input}]
    if {![string equal $inputMsgNode {}]} {
        set inputMsgPath [$inputMsgNode getAttribute message]
        set inputMsg [lindex [split $inputMsgPath {:}] end]
        set inType [messageToType $wsdlNode $serviceName $operName $inputMsg serviceInfo $style]
    }

    set outputMsgNode [$operNode selectNodes {w:output}]
    if {![string equal $outputMsgNode {}]} {
        set outputMsgPath [$outputMsgNode getAttribute message]
        set outputMsg [lindex [split $outputMsgPath {:}] end]
        set outType [messageToType $wsdlNode $serviceName $operName $outputMsg serviceInfo $style]
    }

    ##
    ## Return the types
    ##
    ::log:::log debug "Leaving [lindex [info level 0] 0] with [list $inType $outType]"
    return [list $inType $outType]
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::messageToType
#
# Description : Get a type name from a message
#
# Arguments :
#    wsdlNode       - The top node of the WSDL
#    serviceName    - The name of the service.
#    operName       - The name of the operation.
#    msgName        - The name of the message.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#    style          - Style of call
#
# Returns : The requested type name
#
# Side-Effects : Defines Client mode types for the service as specified by the WSDL
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
#       1  08/06/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::messageToType {wsdlNode serviceName operName msgName serviceInfoVar style} {
    upvar 1 $serviceInfoVar serviceInfo
    ::log:::log debug "Enteringing [info level 0]"

    #puts "Message to Type $serviceName $operName $msgName"

    set msgQuery [format {w:message[attribute::name='%s']} $msgName]
    set msg [$wsdlNode selectNodes $msgQuery]
    switch -exact -- $style {
        document/literal {
            set partNode [$msg selectNodes w:part]
            set partNodeCount [llength $partNode]
            if {$partNodeCount == 1} {
                if {[$partNode hasAttribute element]} {
                    set type [::WS::Utils::getQualifiedType $serviceInfo [$partNode getAttribute element] tns1]
                }
            }
            if {($partNodeCount > 1) || ![info exist type]} {
                set tmpType {}
                foreach part [$msg selectNodes w:part] {
                    set partName [$part getAttribute name]
                    if {[$part hasAttribute type]} {
                        set partType [$part getAttribute type]
                    } else {
                        set partType [$part getAttribute element]
                    }
                    lappend tmpType $partName [list type [::WS::Utils::getQualifiedType $serviceInfo $partType tns1] comment {}]
                }
                set type tns1:$msgName
                dict set serviceInfo types $type $tmpType
                ::WS::Utils::ServiceTypeDef Client $serviceName $type $tmpType tns1
            } elseif {!$partNodeCount} {
                return \
                    -code error \
                    -errorcode [list WS CLIENT BADMSGSEC $msgName] \
                    "Invalid format for message '$msgName'"
            }
        }
        rpc/encoded {
            set tmpType {}
            foreach part [$msg selectNodes w:part] {
                set partName [$part getAttribute name]
                if {[$part hasAttribute type]} {
                    set partType [$part getAttribute type]
                } else {
                    set partType [$part getAttribute element]
                }
                lappend tmpType $partName [list type [::WS::Utils::getQualifiedType $serviceInfo $partType tns1] comment {}]
            }
            set type tns1:$msgName
            dict set serviceInfo types $type $tmpType
            ::WS::Utils::ServiceTypeDef Client $serviceName $type $tmpType tns1
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKSTY $style] \
                "Unknown style combination $style"
        }
    }

    ##
    ## Return the type name
    ##
    ::log:::log debug "Leaving [lindex [info level 0] 0] with {$type}"
    return $type
}

#---------------------------------------
#---------------------------------------

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoRawRestCall
#
# Description : Call an operation of a web service
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object.
#                         This is for both the Soap Header and Body messages.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       The XML of the operation.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#
# Pre-requisite Conditions :    Service must have been defined.
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
proc ::WS::Client::DoRawRestCall {serviceName objectName operationName argList {headers {}} {location {}}} {
    variable serviceArr

    ::log::log debug "Entering [info level 0]"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo object $objectName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOBJ [list $serviceName $objectName]] \
            "Unknown object '$objectName' for service '$serviceName'"
    }
    if {![dict exists $serviceInfo object $objectName operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $objectName $operationName]] \
            "Unknown operation '$operationName' for object '$objectName' of service '$serviceName'"
    }
    if {![string equal $location {}]} {
        set url $location
    } else {
        set url [dict get $serviceInfo object $objectName location]
    }
    set query [buildRestCallquery $serviceName $objectName $operationName $url $argList]
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[llength $headers]} {
        set token [::http::geturl $url -query $query -type text/xml -headers $headers]
    } else {
        set token [::http::geturl $url -query $query -type text/xml]
    }
    ::http::wait $token

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    if {![string equal [::http::status $token] ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        set errorInfo {}
        set results [::http::error $token]
        set hadError 1
    } else {
        set hadError 0
        set results [::http::data $token]
    }
    ::http::cleanup $token
    if {$hadError} {
        ::log::log debug "Leaving (error) ::WS::Client::DoRawRestCall"
        return \
            -code error \
            -errorcode $errorCode \
            -errorinfo $errorInfo \
            $results
    } else {
        ::log::log debug "Leaving ::WS::Client::DoRawRestCall with {$results}"
        return $results
    }

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoRestCall
#
# Description : Call an operation of a web service
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       The return value of the operation as a dictionary object.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#       others                  - as raised by called Operation
#
# Pre-requisite Conditions :    Service must have been defined.
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
proc ::WS::Client::DoRestCall {serviceName objectName operationName argList {headers {}} {location {}}} {
    variable serviceArr

    ::log::log debug "Entering [info level 0]"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo object $objectName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOBJ [list $serviceName $objectName]] \
            "Unknown object '$objectName' for service '$serviceName'"
    }
    if {![dict exists $serviceInfo object $objectName operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $objectName $operationName]] \
            "Unknown operation '$operationName' for object '$objectName' of service '$serviceName'"
    }
    if {![string equal $location {}]} {
        set url $location
    } else {
        set url [dict get $serviceInfo object $objectName location]
    }
    set query [buildRestCallquery $serviceName $objectName $operationName $url $argList]
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[llength $headers]} {
        set token [::http::geturl $url -query $query -type text/xml -headers $headers]
    } else {
        set token [::http::geturl $url -query $query -type text/xml]
    }
    ::http::wait $token

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    ::log::log debug "\tReceived: $body"
    set httpStatus [::http::status $token]
    set hadError 0
    set results {}
    if {![string equal $httpStatus ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        ::log::log debug "\tHTTP error [array get $token]"
        set results [::http::error $token]
        if {[string equal $results {}] || [string equal $httpStatus eof]} {
            set results {Unexpected EOF received from Server}
            set errorCode [list WSCLIENT HTTPERROR UNEXPEOF]
        } else {
            set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        }
        set errorInfo {}
        set hadError 1
    } else {
        set hadError [catch {parseRestResults $serviceName $objectName $operationName $body} results]
        if {$hadError} {
            ::log::log debug "Reply was [::http::data $token]"
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }
    ::http::cleanup $token
    if {$hadError} {
        ::log::log debug "Leaving (error) ::WS::Client::DoRestCall"
        return \
            -code error \
            -errorcode $errorCode \
            -errorinfo $errorInfo \
            $results
    } else {
        ::log::log debug "Leaving ::WS::Client::DoRestCall with {$results}"
        return $results
    }

}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::DoARestsyncCall
#
# Description : Call an operation of a web service asynchronously
#
# Arguments :
#       serviceName     - The name of the Webservice
#       operationName   - The name of the Operation to call
#       argList         - The arguements to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       succesCmd       - A command prefix to be called if the operations
#                         does not raise an error.  The results, as a dictionary
#                         object are concatinated to the prefix.
#       errorCmd        - A command prefix to be called if the operations
#                         raises an error.  The error code and stack trace
#                         are concatinated to the prefix.
#       headers         - Extra headers to add to the HTTP request. This
#                         is a key value list argument. It must be a list with
#                         an even number of elements that alternate between
#                         keys and values. The keys become header field names.
#                         Newlines are stripped from the values so the header
#                         cannot be corrupted.
#                         This is an optional argument and defaults to {}.
#
# Returns :
#       None.
#
# Side-Effects :        None
#
# Exception Conditions :
#       WSCLIENT HTTPERROR      - if an HTTP error occured
#       others                  - as raised by called Operation
#
# Pre-requisite Conditions :    Service must have been defined.
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
proc ::WS::Client::DoRestAsyncCall {serviceName objectName operationName argList succesCmd errorCmd {headers {}}} {
    variable serviceArr

    set svcHeaders [dict get $serviceArr($serviceName) headers]
    if {[llength $svcHeaders]} {
        lappend headers $svcHeaders
    }
    ::log::log debug "Entering ::WS::Client::DoAsyncRestCall [list $serviceName $objectName $operationName $argList $succesCmd $errorCmd $headers]"
    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    if {![dict exists $serviceInfo object $objectName operation $operationName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKOPER [list $serviceName $objectName $operationName]] \
            "Unknown operation '$operationName' for service '$serviceName'"
    }
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    set url [dict get $serviceInfo object $objectName location]
    set query [buildRestCallquery $serviceName $objectName $operationName $url $argList]
    if {[llength $headers]} {
        ::http::geturl $url \
            -query $query \
            -type text/xml \
            -headers $headers \
            -command [list ::WS::Client::asyncRestCallDone $serviceName $operationName $succesCmd $errorCmd]
    } else {
        ::http::geturl $url \
            -query $query \
            -type text/xml \
            -command [list ::WS::Client::asyncRestCallDone $serviceName $operationName $succesCmd $errorCmd]
    }
    ::log::log debug "Leaving ::WS::Client::DoAsyncRestCall"
    return;
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::buildRestCallquery
#
# Description : Build the XML request message for the call
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    url                - the URL of the operation
#    argList            - a dictionary object of the calling arguments
#                         This is for both the Soap Header and Body messages.
#
# Returns : The XML for the call
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
proc ::WS::Client::buildRestCallquery {serviceName objectName operationName url argList} {
    variable serviceArr

    ::log::log debug "Entering [info level 0]"
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo object $objectName operation $operationName inputs]
    set xnsList [dict get $serviceInfo targetNamespace]

    dom createDocument "request" doc
    $doc documentElement body
    $body setAttribute \
        "method"      $operationName
    foreach xns $xnsList {
        set tns [lindex $xns 0]
        set target [lindex $xns 1]
        $body  setAttribute \
            xmlns:$tns $target
    }

    set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $msgType] xns]

    ::log::log debug "calling [list ::WS::Utils::convertDictToType Client $serviceName $doc $body $argList $msgType]"
    set options [::WS::Utils::SetOption]
    ::WS::Utils::SetOption UseNS 0
    ::WS::Utils::SetOption genOutAttr 1
    ::WS::Utils::convertDictToType Client $serviceName $doc $body $argList $msgType
    foreach {option value} $options {
        ::WS::Utils::SetOption $option $value
    }

    append xml  \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$doc asXML -indent none -doctypeDeclaration 0]
    #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] {} xml
    $doc delete

    set inTransform [dict get $serviceInfo inTransform]
    if {![string equal $inTransform {}]} {
        set xml [$inTransform $serviceName $operationName REQUEST $xml $url $argList]
    }

    ::log::log debug "Leaving ::::WS::Client::buildRestCallquery with {$xml}"

    return $xml

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::parseRestResults
#
# Description : Convert the returned XML into a dictionary object
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    inXML              - the XML returned by the operation
#
# Returns : A dictionary object representing the results
#
# Side-Effects : None
#
# Exception Conditions :
#       WSCLIENT REMERR         - The remote end raised an exception, the third element of
#                                 the error code is the remote fault code.
#                                 Error info is set to the remote fault details.
#                                 The error message is the remote fault string;
#       WSCLIENT BADREPLY       - Badly formatted reply, the third element is a list of
#                                 what message type was received vs what was expected.
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
proc ::WS::Client::parseRestResults {serviceName objectName operationName inXML} {
    variable serviceArr

    ::log::log debug "In parseResults $serviceName $operationName {$inXML}"
    set serviceInfo $serviceArr($serviceName)
    set outTransform [dict get $serviceInfo outTransform]
    if {![string equal $outTransform {}]} {
        set inXML [$outTransform $serviceName $operationName REPLY $inXML]
    }
    set expectedMsgType [dict get $serviceInfo object $objectName operation $operationName outputs]
    dom parse $inXML doc
    $doc documentElement top
    set xns {}
    foreach tmp [dict get $serviceInfo targetNamespace] {
        lappend xns [lindex $tmp 0] [lindex $tmp 1]
    }
    ::log::log debug "Using namespaces {$xns}"
    set body $top
    set status [$body getAttribute status]

    ##
    ## See if it is a standard error packet
    ##
    if {![string equal $status {ok}]} {
        set faultstring {}
        if {[catch {set faultstring [[$body selectNodes error] asText]}]} {
            catch {set faultstring [[$body selectNodes error] asText]}
        }
        $doc delete
        return \
            -code error \
            -errorcode [list WSCLIENT REMERR $status] \
            -errorinfo {} \
            $faultstring
    }

    ##
    ## Convert the packet to a dictionary
    ##
    set results {}
    set options [::WS::Utils::SetOption]
    ::WS::Utils::SetOption UseNS 0
    ::WS::Utils::SetOption parseInAttr 1
    ::log::log debug "Calling [list ::WS::Utils::convertTypeToDict Client $serviceName $body $expectedMsgType $body]"
    if {![string equal $expectedMsgType {}]} {
        set node [$body childNodes]
        set nodeName [$node nodeName]
        if {![string equal $objectName $nodeName]} {
            return \
                -code error \
                -errorcode [list WSCLIENT BADRESPONSE [list $objectName $nodeName]] \
                -errorinfo {} \
                "Unexpected message type {$nodeName}, expected {$objectName}"
        }
        set results [::WS::Utils::convertTypeToDict \
                         Client $serviceName $node $expectedMsgType $body]
    }
    foreach {option value} $options {
        ::WS::Utils::SetOption $option $value
    }
    $doc delete

    return $results

}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::asyncRestobCallDone
#
# Description : Called when an asynchronous call is complete.  This routine
#               will call either the success or error callback depending on
#               if the operation succeeded or failed -- assuming the callback
#               is defined.
#
# Arguments :
#    serviceName        - the name of the service called
#    operationName      - the name of the operation called
#    succesCmd          - the command prefix to call if no error
#    errorCmd           - the command prefix to call on an error
#    token              - the token from the HTTP request
#
# Returns : Nothing
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
proc ::WS::Client::asyncRestCallDone {serviceName objectName operationName succesCmd errorCmd token} {
    ::log::log debug "Entering ::WS::Client::asyncCallDone {$serviceName $objectName $operationName $succesCmd $errorCmd $token}"

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    if {![string equal [::http::status $token] ok] ||
        ([::http::ncode $token] != 200 && [string equal $body {}])} {
        set errorCode [list WSCLIENT HTTPERROR [::http::code $token]]
        set hadError 1
        set errorInfo [::http::error $token]
    } else {
        set hadError [catch {parseRestResults $serviceName $objectName $operationName $body} results]
        if {$hadError} {
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }

    ##
    ## Call the appropriate callback
    ##
    if {$hadError} {
        set cmd $errorCmd
        lappend cmd $errorCode $errorInfo
    } else {
        set cmd $succesCmd
    }
    lappend cmd $results
    catch $cmd

    ##
    ## All done
    ##
    ::http::cleanup $token
    return;
}
