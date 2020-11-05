###############################################################################
##                                                                           ##
##  Copyright (c) 2016-2019, Harald Oehlmann                                 ##
##  Copyright (c) 2006-2013, Gerald W. Lester                                ##
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

package require Tcl 8.6
package require WS::Utils ; # logsubst
package require tdom 0.8
package require http 2
package require log
package require uri

package provide WS::Client 3.0.1

namespace eval ::WS::Client {
    # register https only if not yet registered
    if {[catch { http::unregister https } lPortCmd]} {
        # not registered -> register on my own
        if {[catch {
            package require tls
            http::register https 443 ::tls::socket
        } err]} {
            log::log warning "No TLS package: $err"
            if { [catch {
                package require twapi_crypto
                http::register https 443 ::twapi::tls_socket
                
            } Err] } {
                log::log warning "No https support. No TWAPI package: $err"
            }
        }
    } else {
        # Ok, was registered - reregister
        http::register https {*}$lPortCmd
    }
    unset -nocomplain err lPortCmd

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
        skipLevelOnReply 0
        skipHeaderLevel 0
        suppressTargetNS 0
        allowOperOverloading 1
        contentType {text/xml;charset=utf-8}
        UseNS {}
        parseInAttr {}
        genOutAttr {}
        valueAttrCompatiblityMode 1
        suppressNS {}
        useTypeNs {}
        nsOnChangeOnly {}
        noTargetNs 0
        errorOnRedefine 0
        inlineElementNS 1
        queryTimeout 60000
    }
    ##
    ## List of options which are copied to the service array
    ##
    set ::WS::Client::serviceLocalOptionsList {
        skipLevelWhenActionPresent
        skipLevelOnReply
        skipHeaderLevel
        suppressTargetNS
        allowOperOverloading
        contentType
        UseNS
        parseInAttr
        genOutAttr
        valueAttrCompatiblityMode
        suppressNS
        useTypeNs
        nsOnChangeOnly
        noTargetNs
        queryTimeout
    }

    ##
    ## List of options which are set and restored in the Utilities module
    ## when we do a call into the module
    ##
    set ::WS::Client::utilsOptionsList {
        UseNS
        parseInAttr
        genOutAttr
        valueAttrCompatiblityMode
        suppressNS
        useTypeNs
        nsOnChangeOnly
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
# Description : Set or get file global or default option.
#               Global option control the service creation process.
#               Default options are takren as defaults to new created services.
#
# Arguments :
#       -globalonly
#               - Return list of global options/values
#       -defaultonly
#               - Return list of default options/values
#       --
#       option  - Option to be set/retrieved
#                 Return all option/values if omitted
#       args    - Value to set the option to
#                 Return the value if not given
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
# 2.4.5    2017-12-04  H.Oehlmann   Return all current options if no argument
#                                   given. Options -globalonly or -defaultonly
#                                   limit this to options which are (not)
#                                   copied to the service.
#
###########################################################################
proc ::WS::Client::SetOption {args} {
    variable options
    variable serviceLocalOptionsList
    if {0 == [llength $args]} {
        return [array get options]
    }
    set args [lassign $args option]

    switch -exact -- $option {
        -globalonly {
            ##
            ## Return list of global options
            ##
            # A list convertible to a dict is build for performance reasons:
            # - lappend does not test existence for each element
            # - if a list is needed, dict build burden is avoided
            set res {}
            foreach option [array names options] {
                if {$option ni $serviceLocalOptionsList} {
                    lappend res $option $options($option)
                }
            }
            return $res
        }
        -defaultonly {
            ##
            ## Return list of default options
            ##
            set res {}
            foreach option [array names options] {
                if {$option in $serviceLocalOptionsList} {
                    lappend res $option $options($option)
                }
            }
            return $res
        }
        -- {
            ##
            ## End of options
            ##
            set args [lassign $args option]
        }
    }
    ##
    ## Check if given option exists
    ##
    if {![info exists options($option)]} {
        return  -code error \
                -errorcode [list WS CLIENT UNKOPT $option] \
                "Unknown option: '$option'"
    }
    ##
    ## Check if value is given
    ##
    switch -exact -- [llength $args] {
        0 {
            return $options($option)
        }
        1 {
            set value [lindex $args 0]
            set options($option) $value
            return $value
        }
        default {
            return  -code error \
                    -errorcode [list WS CLIENT INVALDCNT $args] \
                    "To many parameters: '$args'"
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
# 2.4.5    2017-12-04  H.Oehlmann   Use distinct list of option items, which are
#                                   copied to the service array. Not all options
#                                   are used in the service array.
#
###########################################################################
proc ::WS::Client::CreateService {serviceName type url target args} {
    variable serviceArr
    variable options
    variable serviceLocalOptionsList

    if {$options(errorOnRedefine) && [info exists serviceArr($serviceName)]} {
        return -code error "Service '$serviceName' already exists"
    } elseif {[info exists serviceArr($serviceName)]} {
        unset serviceArr($serviceName)
    }

    dict set serviceArr($serviceName) types {}
    dict set serviceArr($serviceName) operList {}
    dict set serviceArr($serviceName) objList {}
    dict set serviceArr($serviceName) headers {}
    dict set serviceArr($serviceName) targetNamespace tns1 $target
    dict set serviceArr($serviceName) name $serviceName
    dict set serviceArr($serviceName) location $url
    dict set serviceArr($serviceName) style $type
    dict set serviceArr($serviceName) imports {}
    dict set serviceArr($serviceName) inTransform {}
    dict set serviceArr($serviceName) outTransform {}
    foreach item $serviceLocalOptionsList {
        dict set serviceArr($serviceName) $item $options($item)
    }
    foreach {name value} $args {
        set name [string trimleft $name {-}]
        dict set serviceArr($serviceName) $name $value
    }

    ::log::logsubst debug {Setting Target Namespace tns1 as $target}
    if {[dict exists $serviceArr($serviceName) xns]} {
        foreach xnsItem [dict get $serviceArr($serviceName) xns] {
            lassign $xnsItem tns xns
            ::log::logsubst debug {Setting targetNamespace $tns for $xns}
            dict set serviceArr($serviceName) targetNamespace $tns $xns
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
#       serviceName - Service name to add namespace to.
#                     Return a list of items/values of default options if not
#                     given.
#       item        - The item to configure. Return a list of all items/values
#                     if not given.
#       value       - Optional, the new value. Return the value, if not given.
#
# Returns :     The value of the option or a list of item/value pairs.
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
# 2.4.5    2017-12-04  H.Oehlmann   Allow to set an option to the empty string.
#                                   Return all option/values, if called without
#                                   item. Return default items/values if no
#                                   service given.
#
###########################################################################
proc ::WS::Client::Config {args} {
    variable serviceArr
    variable options
    variable serviceLocalOptionsList

    set validOptionList $serviceLocalOptionsList
    lappend validOptionList location targetNamespace

    if {0 == [llength $args]} {
        # A list convertible to a dict is build for performance reasons:
        # - lappend does not test existence for each element
        # - if a list is needed, dict build burden is avoided
        set res {}
        foreach item $validOptionList {
            lappend res $item
            if {[info exists options($item)]} {
                lappend res $options($item)
            } else {
                lappend res {}
            }
        }
        return $res
    }
    set args [lassign $args serviceName]
    if {0 == [llength $args]} {
        set res {}
        foreach item $validOptionList {
            lappend res $item [dict get $serviceArr($serviceName) $item]
        }
        return $res
    }

    set args [lassign $args item]
    if { $item ni $validOptionList } {
        return -code error "Uknown option '$item' -- must be one of: [join $validOptionList {, }]"
    }

    switch -exact -- [llength $args] {
        0 {
            return [dict get $serviceArr($serviceName) $item]
        }
        1 {
            set value [lindex $args 0]
            dict set serviceArr($serviceName) $item  $value
            return $value
        }
        default {
            ::log::log debug "To many arguments arguments {$args}"
            return \
                -code error \
                -errorcode [list WS CLIENT INVARGCNT $args] \
                "To many arguments '$args'"
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

    return
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
#       serviceName   - Service name to add namespace to
#       objectName    - Name of the object
#       operationName - The name of the method to add
#       inputArgs     - List of input argument definitions where each argument
#                       definition is of the format: name typeInfo
#       returnType    - The type, if any returned by the procedure.  Format is:
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

    if {$inputArgs ne {}} {
        set inType $objectName.$operationName.Request
        ::WS::Utils::ServiceTypeDef Client $serviceName $inType $inputArgs
    } else {
        set inType {}
    }
    if {$returnType ne {}} {
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
# 2.4.1    2017-08-31  H.Oehlmann   Use utility function
#                                   ::WS::Utils::geturl_fetchbody for http call
#                                   which also follows redirects.
# 3.0.0    2020-10-26  H.Oehlmann   Add geturl timeout
#
#
###########################################################################
proc ::WS::Client::ImportNamespace {serviceName url} {
    variable serviceArr

    set serviceInfo $serviceArr($serviceName)
    switch -exact -- [dict get [::uri::split $url] scheme] {
        file {
            upvar #0 [::uri::geturl $url] token
            set xml $token(data)
            unset token
        }
        http -
        https {
            set xml [::WS::Utils::geturl_fetchbody $url\
                    -timeout [dict get $serviceInfo queryTimeout]]
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKURLTYP $url] \
                "Unknown URL type '$url'"
        }
    }
    set tnsCount [expr {[llength [dict get $serviceArr($serviceName) targetNamespace]]/2}]
    dict lappend serviceInfo imports $url
    ::WS::Utils::ProcessImportXml Client $url $xml $serviceName serviceInfo tnsCount
    set serviceArr($serviceName) $serviceInfo
    set result {}
    foreach {result target} [dict get $serviceArr($serviceName) targetNamespace] {
        if {$target eq $url} {
            break
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

    if {$object eq {}} {
        return [dict get $serviceArr($serviceName) operList]
    } else {
        return [list $object [dict get $serviceArr($serviceName) operation $object inputs] [dict get $serviceArr($serviceName) operation $object outputs]]
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
#       serviceName - Service name to of the operation
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
    return

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
# Description : Import any additional namespace into the service
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
    set soapReplyHeader [dict get $serviceInfo operation $operation soapReplyHeader]
    lappend soapReplyHeader $headerType
    dict set serviceInfo operation $operation soapReplyHeader $soapReplyHeader
    set serviceArr($serviceName) $serviceInfo
    return

}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Client::GetParsedWsdl
#
# Description : Get a service definition
#
# Arguments :
#       serviceName - Name of the service.
#
# Returns :     The parsed service information
#
# Side-Effects :        None
#
# Exception Conditions :        UNKSERVICE
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
proc ::WS::Client::GetParsedWsdl {serviceName} {
    variable serviceArr

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error "Unknown service '$serviceName'" \
            -errorcode [list UNKSERVICE $serviceName]
    }

    return $serviceArr($serviceName)
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
#   3.0.0  2020-10-30  H.Oehlmann   Smooth option migration.
#
#
###########################################################################
proc ::WS::Client::LoadParsedWsdl {serviceInfo {headers {}} {serviceAlias {}}} {
    variable serviceArr
    variable options
    variable serviceLocalOptionsList

    if {[string length $serviceAlias]} {
        set serviceName $serviceAlias
    } else {
        set serviceName [dict get $serviceInfo name]
    }
    if {$options(errorOnRedefine) && [info exists serviceArr($serviceName)]} {
        return -code error "Service '$serviceName' already exists"
    } elseif {[info exists serviceArr($serviceName)]} {
        unset serviceArr($serviceName)
    }

    if {[llength $headers]} {
        dict set serviceInfo headers $headers
    }
    set serviceArr($serviceName) $serviceInfo

    ##
    ## Copy any not present options from the default values
    ## This allows smooth migration, if a new version of the package define
    ## new options and the preparsed service of the old version was stored.
    ##
    
    foreach item $serviceLocalOptionsList {
        if {![dict exists $serviceArr($serviceName) $item]} {
            dict set serviceArr($serviceName) $item $options($item)
        }
    }

    if {[dict exists $serviceInfo types]} {
        foreach {typeName partList} [dict get $serviceInfo types] {
            set definition [dict get $partList definition]
            set xns [dict get $partList xns]
            set isAbstarct [dict get $partList abstract]
            if {[lindex [split $typeName {:}] 1] eq {}} {
                ::WS::Utils::ServiceTypeDef Client $serviceName $typeName $definition tns1 $isAbstarct
            } else {
                #set typeName [lindex [split $typeName {:}] 1]
                ::WS::Utils::ServiceTypeDef Client $serviceName $typeName $definition $xns $isAbstarct
            }
        }
    }

    if {[dict exists $serviceInfo simpletypes]} {
        foreach partList [dict get $serviceInfo simpletypes] {
            lassign $partList typeName definition
            if {[lindex [split $typeName {:}] 1] eq {}} {
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
#       url           - The url of the WSDL
#       headers       - Extra headers to add to the HTTP request. This
#                       is a key value list argument. It must be a list with
#                       an even number of elements that alternate between
#                       keys and values. The keys become header field names.
#                       Newlines are stripped from the values so the header
#                       cannot be corrupted.
#                       This is an optional argument and defaults to {}.
#       serviceAlias  - Alias (unique) name for service.
#                       This is an optional argument and defaults to the name
#                       of the service in serviceInfo.
#       serviceNumber - Number of service within the WSDL to assign the
#                       serviceAlias to. Only usable with a serviceAlias.
#                       First service (default) is addressed by value "1".
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
# 2.4.1    2017-08-31  H.Oehlmann   Use utility function
#                                   ::WS::Utils::geturl_fetchbody for http call
# 2.4.6    2017-12-07  H.Oehlmann   Added argument "serviceNumber".
# 3.0.0    2020-10-26  H.Oehlmann   Added query timeout
#
###########################################################################
proc ::WS::Client::GetAndParseWsdl {url {headers {}} {serviceAlias {}} {serviceNumber 1}} {
    variable currentBaseUrl
    variable options

    set currentBaseUrl $url
    switch -exact -- [dict get [::uri::split $url] scheme] {
        file {
            upvar #0 [::uri::geturl $url] token
            set wsdlInfo [ParseWsdl $token(data) -headers $headers -serviceAlias $serviceAlias -serviceNumber $serviceNumber]
            unset token
        }
        http -
        https {
            set largs {}
            if {[llength $headers]} {
                lappend largs -headers $headers
            }
            set body [::WS::Utils::geturl_fetchbody $url\
                    -timeout $options(queryTimeout) {*}$largs]
            set wsdlInfo [ParseWsdl $body -headers $headers -serviceAlias $serviceAlias -serviceNumber $serviceNumber]
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
# Description : Parse a WSDL and create the service. Create stubs if specified.
#
# Arguments :
#       wsdlXML - XML of the WSDL
#
# Optional Arguments:
#       -createStubs 0|1 - create stub routines for the service
#       -headers         - Extra headers to add to the HTTP request. This
#                          is a key value list argument. It must be a list with
#                          an even number of elements that alternate between
#                          keys and values. The keys become header field names.
#                          Newlines are stripped from the values so the header
#                          cannot be corrupted.
#                          This is an optional argument and defaults to {}.
#       -serviceAlias    - Alias (unique) name for service.
#                          This is an optional argument and defaults to the
#                          name of the service in serviceInfo.
#       -serviceNumber   - Number of service within the WSDL to assign the
#                          serviceAlias to. Only usable with a serviceAlias.
#                          First service (default) is addressed by value "1".
#
# NOTE -- Arguments are position independent.
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
# 2.4.4    2017-11-03  H.Oehlmann   Included ticket [dcce437d7a] with
#                                   solution by Wolfgang Winkler:
#                                   Search namespace prfix also in element
#                                   nodes and not only in definition node
#                                   of wsdl file.
# 2.4.4    2017-11-06  H.Oehlmann   Added check (for nested namespace prefix
#                                   case), that a namespace prefix is not
#                                   reused for another URI.
# 2.4.5    2017-11-24  H.Oehlmann   Added option "inlineElementNS" to activate
#                                   namespace definition search in element nodes
# 2.4.6    2017-12-07  H.Oehlmann   Added argument "-serviceNumber".
#
###########################################################################
proc ::WS::Client::ParseWsdl {wsdlXML args} {
    variable currentBaseUrl
    variable serviceArr
    variable options

    # Build the argument array with the following defaults
    array set argument {
        -createStubs    0
        -headers        {}
        -serviceAlias   {}
        -serviceNumber  1
    }
    array set argument $args

    set first [string first {<} $wsdlXML]
    if {$first > 0} {
        set wsdlXML [string range $wsdlXML $first end]
    }
    ::log::logsubst debug {Parsing WSDL: $wsdlXML}

    # save parsed document node to tmpdoc
    dom parse $wsdlXML tmpdoc
    # save transformed document handle in variable wsdlDoc
    $tmpdoc xslt $::WS::Utils::xsltSchemaDom wsdlDoc
    $tmpdoc delete
    # save top node in variable wsdlNode
    $wsdlDoc documentElement wsdlNode
    set nsCount 1
    set targetNs [$wsdlNode getAttribute targetNamespace]
    set ::WS::Utils::targetNs $targetNs
    ##
    ## Build the namespace prefix dict
    ##
    # nsDict contains two tables:
    # 1) Lookup URI, get internal prefix
    #   url <URI> <tns>
    # 2) Lookup wsdl namespace prefix, get internal namespace prefix
    #   tns <ns> <tns>
    # <URI>: unique ID, mostly URL
    # <ns>: namespace prefix used in wsdl
    # <tns> internal namespace prefix which allows to use predefined prefixes
    #   not to clash with the wsdl prefix in <ns>
    #   Predefined:
    #   - tns1 : targetNamespace
    #   - w: http://schemas.xmlsoap.org/wsdl/
    #   - d: http://schemas.xmlsoap.org/wsdl/soap/
    #   - xs: http://www.w3.org/2001/XMLSchema
    #
    # The top node
    # <wsdl:definitions
    #   targetNamespace="http://www.webserviceX.NET/">
    #   xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/ ...>
    # contains the target namespace and all namespace definitions
    dict set nsDict url $targetNs tns$nsCount

    $wsdlDoc selectNodesNamespaces {
        w http://schemas.xmlsoap.org/wsdl/
        d http://schemas.xmlsoap.org/wsdl/soap/
        xs http://www.w3.org/2001/XMLSchema
    }

    ##
    ## build list of namespace definition nodes
    ##
    ## the top node is always used
    set NSDefinitionNodeList [list $wsdlNode]

    ##
    ## get namespace definitions in element nodes
    ##
    ## Element nodes may declare namespaces inline like:
    ## <xs:element xmlns:q1="myURI" type="q1:MessageQ1"/>
    ## ticket [dcce437d7a]

    # This is only done, if option inlineElementNS is set in the default
    # options. Service dependent options may not be used at this stage,
    # as serviceArr is not created jet (Client::Config will fail) and the
    # service name is not known jet.
    if {$options(inlineElementNS)} {
        lappend NSDefinitionNodeList {*}[$wsdlDoc selectNodes {//xs:element}]
    }
    foreach elemNode $NSDefinitionNodeList {
        # Get list of xmlns attributes
        # This list looks for the example like: {{q1 q1 {}} ... }
        set xmlnsAttributes [$elemNode attributes xmlns:*]
        # Loop over found namespaces
        foreach itemList $xmlnsAttributes {
            set ns [lindex $itemList 0]
            set url [$elemNode getAttribute xmlns:$ns]

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
            ##
            ## Check if same namespace prefix was already assigned to a
            ## different URL
            ##
            # This may happen, if the element namespace prefix overwrites
            # a global one, like
            # <wsdl:definitions xmlns:q1="URI1" ...>
            #   <xs:element xmlns:q1="URI2" type="q1:MessageQ1"/>
            if { [dict exists $nsDict tns $ns] && $tns ne [dict get $nsDict tns $ns] } {
                ::log::logsubst debug {Namespace prefix '$ns' with different URI '$url': $nsDict}
                return \
                    -code error \
                    -errorcode [list WS CLIENT AMBIGNSPREFIX] \
                    "element namespace prefix '$ns' used again for different URI '$url'.\
                    Sorry, this is a current implementation limitation of TCLWS."
            }
            dict set nsDict tns $ns $tns
        }
    }

    if {[info exists currentBaseUrl]} {
        set url $currentBaseUrl
    } else {
        set url $targetNs
    }

    array unset ::WS::Utils::includeArr
    ::WS::Utils::ProcessIncludes $wsdlNode $url

    set serviceInfo {}

    foreach serviceInfo [buildServiceInfo $wsdlNode $nsDict $serviceInfo $argument(-serviceAlias) $argument(-serviceNumber)] {
        set serviceName [dict get $serviceInfo name]

        if {[llength $argument(-headers)]} {
            dict set serviceInfo headers $argument(-headers)
        }
        dict set serviceInfo types [::WS::Utils::GetServiceTypeDef Client $serviceName]
        dict set serviceInfo simpletypes [::WS::Utils::GetServiceSimpleTypeDef Client $serviceName]

        set serviceArr($serviceName) $serviceInfo

        if {$argument(-createStubs)} {
            catch {namespace delete $serviceName}
            namespace eval $serviceName {}
            CreateStubs $serviceName
        }
    }

    $wsdlDoc delete
    unset -nocomplain ::WS::Utils::targetNs

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
            if {$inputHeaderType eq {}} {
                continue
            }
            set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
            set headerFields [dict keys [dict get $headerTypeInfo definition]]
            if {$headerFields ne {}} {
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
          ::log::logsubst debug {no definition found for inputMsgType $inputMsgType}
          set inputFields {}
        }
        if {$inputFields ne {}} {
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
            ::log::logsubst debug {::WS::Client::DoCall $serviceName $operationName $argList}
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
#       argList         - The arguments to the operation as a dictionary object.
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
#       WS CLIENT HTTPERROR      - if an HTTP error occurred
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
# 2.4.1    2017-08-31  H.Oehlmann   Use utility function
#                                   ::WS::Utils::geturl_fetchbody for http call
#                                   which also follows redirects.
#   3.0.0  2020-10-26  H.Oehlmann   Added query timeout
#
#
###########################################################################
proc ::WS::Client::DoRawCall {serviceName operationName argList {headers {}}} {
    variable serviceArr

    ::log::logsubst debug {Entering [info level 0]}
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

    ##
    ## build query
    ##

    set url [dict get $serviceInfo location]
    SaveAndSetOptions $serviceName
    if {[catch {set query [buildCallquery $serviceName $operationName $url $argList]} err]} {
        RestoreSavedOptions $serviceName
        return -code error -errorcode $::errorCode -errorinfo $::errorInfo $err
    } else {
        RestoreSavedOptions $serviceName
    }
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[dict exists $serviceInfo operation $operationName action]} {
        lappend headers  SOAPAction [format {"%s"} [dict get $serviceInfo operation $operationName action]]
    }

    ##
    ## do http call
    ##

    set largs {}
    if {[llength $headers]} {
        lappend largs -headers $headers
    }
    set body [::WS::Utils::geturl_fetchbody $url\
            -query $query\
            -type [dict get $serviceInfo contentType]\
            -timeout [dict get $serviceInfo queryTimeout]\
            {*}$largs]

    ::log::logsubst debug {Leaving ::WS::Client::DoRawCall with {$body}}
    return $body

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
#       argList         - The arguments to the operation as a dictionary object
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
#       WS CLIENT HTTPERROR      - if an HTTP error occurred
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
# 2.4.1    2017-08-30  H.Oehlmann   Use ::WS::Utils::geturl_fetchbody to do
#                                   http call. This automates a lot and follows
#                                   redirects.
#   3.0.0  2020-10-26  H.Oehlmann   Added query timeout
#
#
###########################################################################
proc ::WS::Client::DoCall {serviceName operationName argList {headers {}}} {
    variable serviceArr

    ::log::logsubst debug {Entering [info level 0]}
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
    SaveAndSetOptions $serviceName
    if {[catch {set query [buildCallquery $serviceName $operationName $url $argList]} err]} {
        RestoreSavedOptions $serviceName
        return -code error -errorcode $::errorCode -errorinfo $::errorInfo "buildCallquery error -- $err"
    } else {
        RestoreSavedOptions $serviceName
    }
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    if {[dict exists $serviceInfo operation $operationName action]} {
        lappend headers  SOAPAction [format {"%s"} [dict get $serviceInfo operation $operationName action]]
    }
    ##
    ## Do the http request
    ##
    # This will directly return with correct error
    # side effect: sets the variable httpCode
    set largs {}
    if {[llength $headers]} {
        lappend largs -headers $headers
    }
    set body [::WS::Utils::geturl_fetchbody -codeok {200 500} -codevar httpCode $url\
            -query $query\
            -type [dict get $serviceInfo contentType]\
            -headers $headers\
            {*}$largs]
    # numerical http code was saved in variable httpCode

    ##
    ## Process body
    ##
    set outTransform [dict get $serviceInfo outTransform]
    if {$httpCode == 500} {
        ## Code 500 treatment
        if {$outTransform ne {}} {
            SaveAndSetOptions $serviceName
            catch {set body [$outTransform $serviceName $operationName REPLY $body]}
            RestoreSavedOptions $serviceName
        }
        set hadError [catch {parseResults $serviceName $operationName $body} results]
        if {$hadError} {
            lassign $::errorCode mainError subError
            if {$mainError eq {WSCLIENT} && $subError eq {NOSOAP}} {
                ::log::logsubst debug {\tHTTP error $body}
                set results $body
                set errorCode [list WSCLIENT HTTPERROR $body]
                set errorInfo {}
            } else {
                ::log::logsubst debug {Reply was $body}
                set errorCode $::errorCode
                set errorInfo $::errorInfo
            }
        }
    } else {
        if {$outTransform ne {}} {
            SaveAndSetOptions $serviceName
            catch {set body [$outTransform $serviceName $operationName REPLY $body]}
            RestoreSavedOptions $serviceName
        }
        SaveAndSetOptions $serviceName
        set hadError [catch {parseResults $serviceName $operationName $body} results]
        RestoreSavedOptions $serviceName
        if {$hadError} {
            ::log::logsubst debug {Reply was $body}
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }
    if {$hadError} {
        ::log::log debug "Leaving (error) ::WS::Client::DoCall"
        return \
            -code error \
            -errorcode $errorCode \
            -errorinfo $errorInfo \
            $results
    } else {
        ::log::logsubst debug {Leaving ::WS::Client::DoCall with {$results}}
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
# Procedure Name : ::WS::Client::FormatHTTPError
#
# Description : Format error after a http::geturl failure.
# A failure consists wether in the HTTP return code unequal to 200
# or in the status equal "error". Status "timeout" is untreated, as this
# http feature is not used in the package.
#
# Arguments :
#       tolken          - tolken of the http::geturl request
#
# Returns :
#       Error message
#
# Side-Effects :        None
#
# Pre-requisite Conditions :    HTTP failure must be present
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
#       1  06/02/2015  H.Oehlmann   Initial version
#
#
###########################################################################
proc ::WS::Client::FormatHTTPError {token} {
    if {[::http::status $token] eq {ok}} {
        if {[::http::size $token] == 0} {
            return "HTTP failure socket closed"
        }
        return "HTTP failure code [::http::ncode $token]"
    } else {
        return "HTTP error: [::http::error $token]"
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
#       argList         - The arguments to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       succesCmd       - A command prefix to be called if the operations
#                         does not raise an error.  The results, as a dictionary
#                         object are concatenated to the prefix.
#       errorCmd        - A command prefix to be called if the operations
#                         raises an error.  The error code and stack trace
#                         are concatenated to the prefix.
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
#       WS CLIENT HTTPERROR      - if an HTTP error occurred
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

    ::log::logsubst debug {Entering [info level 0]}
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
    SaveAndSetOptions $serviceName
    if {[catch {set query [buildCallquery $serviceName $operationName $url $argList]} err]} {
        RestoreSavedOptions $serviceName
        return -code error -errorcode $::errorCode -errorinfo $::errorInfo $err
    } else {
        RestoreSavedOptions $serviceName
    }
    set largs {}
    if {[llength $headers]} {
        lappend largs -headers $headers
    }
    ::log::logsubst info {::http::geturl $url \
            -query $query \
            -type [dict get $serviceInfo contentType] \
            -command [list ::WS::Client::asyncCallDone $serviceName $operationName $succesCmd $errorCmd]\
            -timeout [dict get $serviceInfo queryTimeout] \
            {*}$largs}
    ::http::geturl $url \
        -query $query \
        -type [dict get $serviceInfo contentType] \
        -command [list ::WS::Client::asyncCallDone $serviceName $operationName $succesCmd $errorCmd] \
        -timeout [dict get $serviceInfo queryTimeout] \
        {*}$largs
    ::log::logsubst debug {Leaving ::WS::Client::DoAsyncCall}
    return
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

    foreach operationName [lsort -dictionary [dict get $serviceInfo operList]] {
        if {[dict get $serviceInfo operation $operationName cloned]} {
            continue
        }
        set procName $operationName
        set argList {}
        foreach inputHeaderTypeItem [dict get $serviceInfo operation $operationName soapRequestHeader] {
            set inputHeaderType [lindex $inputHeaderTypeItem 0]
            if {$inputHeaderType eq {}} {
                continue
            }
            set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
            set headerFields [dict keys [dict get $headerTypeInfo definition]]
            if {$headerFields ne {}} {
                lappend argList [lsort -dictionary $headerFields]
            }
        }
        set inputMsgType [dict get $serviceInfo operation $operationName inputs]
        if {$inputMsgType ne {}} {
            set inTypeDef [::WS::Utils::GetServiceTypeDef Client $serviceName $inputMsgType]
            if {[dict exists $inTypeDef definition]} {
                set inputFields [dict keys [dict get $inTypeDef definition]]
                if {$inputFields ne {}} {
                    lappend argList [lsort -dictionary $inputFields]
                }
            }
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
                if {$inputHeaderType eq {}} {
                    continue
                }
                set headerTypeInfo [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType]
                set headerFields [dict keys [dict get $headerTypeInfo definition]]
                if {$headerFields ne {}} {
                    lappend argList [lsort -dictionary $headerFields]
                }
            }
            set inputMsgType [dict get $serviceInfo operation $operationName inputs]
            set inputFields [dict keys [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputMsgType] definition]]
            if {$inputFields ne {}} {
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
    ::log::logsubst debug {Entering [info level 0]}

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    ::log::logsubst info {\nReceived: $body}
    set results {}
    if {[::http::status $token] ne {ok} ||
        ( [::http::ncode $token] != 200 && $body eq {} )} {
        set errorCode [list WS CLIENT HTTPERROR [::http::code $token]]
        set hadError 1
        set results [FormatHTTPError $token]
        set errorInfo ""
    } else {
        SaveAndSetOptions $serviceName
        set hadError [catch {parseResults $serviceName $operationName $body} results]
        RestoreSavedOptions $serviceName
        if {$hadError} {
            set errorCode $::errorCode
            set errorInfo $::errorInfo
        }
    }
    ::http::cleanup $token

    ##
    ## Call the appropriate callback
    ##
    if {$hadError} {
        set cmd $errorCmd
        lappend cmd $errorCode $errorInfo
    } else {
        set cmd $succesCmd
    }
    if {$cmd ne ""} {
        lappend cmd $results
        if {[catch $cmd cmdErr]} {
            ::log::log error "Error invoking callback '$cmd': $cmdErr"
        }
    }
    ##
    ## All done
    ##
    return
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
#       WS CLIENT REMERR         - The remote end raised an exception, the third element of
#                                 the error code is the remote fault code.
#                                 Error info is set to the remote fault details.
#                                 The error message is the remote fault string.
#       WS CLIENT BADREPLY       - Badly formatted reply, the third element is a list of
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
# 2.4.2    2017-08-31  H.Oehlmann   The response node name may also be the
#                                   output name and not only the output type.
#                                   (ticket [21f41e22bc]).
# 2.4.3    2017-11-03  H.Oehlmann   Extended upper commit also to search
#                                   for multiple child nodes.
# 2.5.1    2018-05-14  H.Oehlmann   Add support to translate namespace prefixes
#                                   in attribute values or text values.
#                                   Translation dict "xnsDistantToLocalDict" is
#                                   passed to ::WS::Utils::convertTypeToDict
#                                   to translate abstract types.
#
###########################################################################
proc ::WS::Client::parseResults {serviceName operationName inXML} {
    variable serviceArr

    ::log::logsubst debug {Entering [info level 0]}

    set serviceInfo $serviceArr($serviceName)

    set expectedMsgType [dict get $serviceInfo operation $operationName outputs]
    set expectedMsgTypeBase [lindex [split $expectedMsgType {:}] end]

    set first [string first {<} $inXML]
    if {$first > 0} {
        set inXML [string range $inXML $first end]
    }
    # parse xml and save handle in variable doc and free it when out of scope
    dom parse $inXML doc

    # save top node handle in variable top and free it if out of scope
    $doc documentElement top

    set xns {
        ENV http://schemas.xmlsoap.org/soap/envelope/
        xsi "http://www.w3.org/2001/XMLSchema-instance"
        xs "http://www.w3.org/2001/XMLSchema"
    }
    foreach {prefixCur URICur} [dict get $serviceInfo targetNamespace] {
        lappend xns $prefixCur $URICur
    }
    ::log::logsubst debug {Using namespaces {$xns}}
    $doc selectNodesNamespaces $xns

    ##
    ## When arguments with tags are passed (example: abstract types),
    ## the upper "selectNodesNamespaces translation must be executed manually.
    ## Thus, we need a list of server namespace prefixes to our client namespace
    ## prefixes. (bug 584bfb77)
    ##
    # Example xml:
    # <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/"
    #   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    #   xmlns:xsd="http://www.w3.org/2001/XMLSchema"
    #   xmlns:tns="http://www.esri.com/schemas/ArcGIS/10.3">

    set xnsDistantToLocalDict {}
    foreach attributeCur [$top attributes] {
        # attributeCur is a list of "prefix local URI",
        # which is for xmlns tags: "prefix prefix {}".
        set attributeCur [lindex $attributeCur 0]
        # Check if this is a namespace prefix
        if { ! [$top hasAttribute "xmlns:$attributeCur"] } {continue}
        set URIServer [$top getAttribute "xmlns:$attributeCur"]
        # Check if it is included in xns
        foreach {prefixCur URICur} $xns {
            if {$URIServer eq $URICur} {
                dict set xnsDistantToLocalDict $attributeCur $prefixCur
                break
            }
        }
    }
    ::log::logsubst debug {Server to Client prefix dict: $xnsDistantToLocalDict}

    ##
    ## Get body tag
    ##
    set body [$top selectNodes ENV:Body]
    if {![llength $body]} {
        return \
            -code error \
            -errorcode [list WS CLIENT BADREPLY $inXML] \
            "Bad reply type, no SOAP envelope received in: \n$inXML"
    }
    ##
    ## Find the reply root node with the response.
    ##
    # <SOAP-ENV:Envelope...>
    #   <SOAP-ENV:Body>
    #     <i2:TestResponse id="ref-1" xmlns:i2=...> <-- this one
    #
    # WSDL 1.0: http://xml.coverpages.org/wsdl20000929.html
    # Chapter 2.4.2 (name optional) and 2.4.5 (default name)
    # The node name could be:
    # 1) an error node "Fault"
    # 2) equal to the WSDL name property of the output node
    # 3) if no name tag, equal to <Operation>Response
    # 4) the local output type name
    #
    # Possibility (2) "OutName" WSDL example:
    # <wsdl:portType...><wsdl:operation...>
    #   <wsdl:output name="{OutName}" message="tns:{OutMsgName}" />
    # This possibility is requested by ticket [21f41e22bc]
    #
    # Possibility (3) default name "{OperationName}Result" WSDL example:
    # <wsdl:portType...><wsdl:operation name="{OperationName}">
    #   <wsdl:output message="tns:{OutMsgName}" -> *** no name tag ***
    #
    # Possibility (4) was not found in wsdl 1.0 standard but was used as only
    # solution by TCLWS prior to 2.4.2.
    # The following sketch shows the location of the local output type name
    # "OutTypeName" in a WSDL file:
    # -> In WSDL portType output message name
    # <wsdl:portType...><wsdl:operation...>
    #   <wsdl:output message="tns:{OutMsgName}" />
    # -> then in message, use the element:
    # <wsdl:message name="{OutMsgName}">
    #   <wsdl:part name="..." element="tns:<{OutTypeName}>" />
    # -> The element "OutTypeName" is also find in a type definition:
    # <wsdl:types>
    #   <s:element name="{OutMsgName}">
    #     <s:complexType>...
    #
    # Build a list of possible names
    set nodeNameCandidateList [list Fault $expectedMsgTypeBase]
    # We check if the preparsed wsdl contains the name flag.
    # This is not the case, if it was parsed with tclws prior 2.4.2
    # *** ToDo *** This security may be removed on a major release
    if {[dict exists $serviceInfo operation $operationName outputsname]} {
        lappend nodeNameCandidateList [dict get $serviceInfo operation $operationName outputsname]
    }

    set rootNodeList [$body childNodes]
    ::log::logsubst debug {Have [llength $rootNodeList] node under Body}
    foreach rootNodeCur $rootNodeList {
        set rootNameCur [$rootNodeCur localName]
        if {$rootNameCur eq {}} {
            set rootNameCur [$rootNodeCur nodeName]
        }
        if {$rootNameCur in $nodeNameCandidateList} {
            set rootNode $rootNodeCur
            set rootName $rootNameCur
            ::log::logsubst debug {Result root name is '$rootName'}
            break
        }
        ::log::logsubst debug {Result root name '$rootNameCur' not in candidates '$nodeNameCandidateList'}
    }
    ##
    ## Exit if there is no such node
    ##
    if {![info exists rootName]} {
        return \
            -code error \
            -errorcode [list WS CLIENT BADREPLY [list $rootNameCur $expectedMsgTypeBase]] \
            "Bad reply type, received '$rootNameCur'; but expected '$expectedMsgTypeBase'."
    }

    ##
    ## See if it is a standard error packet
    ##
    if {$rootName eq {Fault}} {
        set faultcode {}
        set faultstring {}
        set detail {}
        foreach item {faultcode faultstring detail} {
            set tmpNode [$rootNode selectNodes ENV:$item]
            if {$tmpNode eq {}} {
                set tmpNode [$rootNode selectNodes $item]
            }
            if {$tmpNode ne {}} {
                if {[$tmpNode hasAttribute href]} {
                    set tmpNode [GetReferenceNode $top [$tmpNode getAttribute href]]
                }
                set $item [$tmpNode asText]
            }
        }
        $doc delete
        return \
            -code error \
            -errorcode [list WS CLIENT REMERR $faultcode] \
            -errorinfo $detail \
            $faultstring
    }

    ##
    ## Convert the packet to a dictionary
    ##
    set results {}
    set headerRootNode [$top selectNodes ENV:Header]
    if {[llength $headerRootNode]} {
        foreach outHeaderType [dict get $serviceInfo operation $operationName soapReplyHeader] {
            if {$outHeaderType eq {}} {
                continue
            }
            set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $outHeaderType] xns]
            set node [$headerRootNode selectNodes $outHeaderType]
            if {![llength $node]} {
                set node [$headerRootNode selectNodes $xns:$outHeaderType]
                if {![llength $node]} {
                    continue
                }
            }

            #if {[llength $outHeaderAttrs]} {
            #    ::WS::Utils::setAttr $node $outHeaderAttrs
            #}
            ::log::logsubst debug {Calling convertTypeToDict from header node type '$outHeaderType'}
            lappend results [::WS::Utils::convertTypeToDict Client $serviceName $node $outHeaderType $headerRootNode 0 $xnsDistantToLocalDict]
        }
    }
    ##
    ## Call Utility function to build result list
    ##
    if {$rootName ne {}} {
        ::log::log debug "Calling convertTypeToDict with root node"
        set bodyData [::WS::Utils::convertTypeToDict \
                     Client $serviceName $rootNode $expectedMsgType $body 0 $xnsDistantToLocalDict]
        if {![llength $bodyData] && ([dict get $serviceInfo skipLevelWhenActionPresent] || [dict get $serviceInfo skipLevelOnReply])} {
            ::log::log debug "Calling convertTypeToDict with skipped action level (skipLevelWhenActionPresent was set)"
            set bodyData [::WS::Utils::convertTypeToDict \
                         Client $serviceName $body $expectedMsgType $body 0 $xnsDistantToLocalDict]
        }
        lappend results $bodyData
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
    set inSuppressNs [::WS::Utils::SetOption suppressNS]
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
                -code error \
                "Unsupported Style '$style'"
        }
    }

    ::WS::Utils::SetOption suppressNS $inSuppressNs
    set inTransform [dict get $serviceInfo inTransform]
    if {$inTransform ne {}} {
        set xml [$inTransform $serviceName $operationName REQUEST $xml $url $argList]
    }

    ::log::logsubst debug {Leaving ::WS::Client::buildCallquery with {$xml}}
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

    ::log::logsubst debug {Entering [info level 0]}
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo operation $operationName inputs]
    set url [dict get $serviceInfo location]
    set xnsList [dict get $serviceInfo targetNamespace]

    # save the document in variable doc and free it if out of scope
    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute \
        "xmlns:SOAP-ENV" "http://schemas.xmlsoap.org/soap/envelope/" \
        "xmlns:SOAP-ENC" "http://schemas.xmlsoap.org/soap/encoding/" \
        "xmlns:xsi"      "http://www.w3.org/2001/XMLSchema-instance" \
        "xmlns:xs"       "http://www.w3.org/2001/XMLSchema"
    if {[dict exists $serviceInfo noTargetNs] && ![dict get $serviceInfo noTargetNs]} {
        $env setAttribute "xmlns" [dict get $xnsList tns1]
    }
    array unset tnsArray *
    array set tnsArray {
        "http://schemas.xmlsoap.org/soap/envelope/" "xmlns:SOAP-ENV"
        "http://schemas.xmlsoap.org/soap/encoding/" "xmlns:SOAP-ENC"
        "http://www.w3.org/2001/XMLSchema-instance" "xmlns:xsi"
        "http://www.w3.org/2001/XMLSchema" "xmlns:xs"
    }
    foreach {tns target} $xnsList {
        #set tns [lindex $xns 0]
        #set target [lindex $xns 1]
        set tnsArray($target) $tns
        $env  setAttribute \
            xmlns:$tns $target
    }
    #parray tnsArray

    set firstHeader 1
    foreach inputHeaderTypeItem [dict get $serviceInfo operation $operationName soapRequestHeader] {
        lassign $inputHeaderTypeItem inputHeaderType attrList
        if {$inputHeaderType eq {}} {
            continue
        }
        set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $inputHeaderType] xns]
        if {[info exists tnsArray($xns)]} {
            set xns $tnsArray($xns)
        }
        if {$firstHeader} {
            # side effect: save new node handle in variable header
            $env appendChild [$doc createElement "SOAP-ENV:Header" header]
            set firstHeader 0
        }
        if {[dict exists $serviceInfo skipHeaderLevel] && [dict get $serviceInfo skipHeaderLevel]} {
            set headerData $header
        } else {
            set typeInfo [split $inputHeaderType {:}]
            if {[llength $typeInfo] > 1} {
                set headerType $inputHeaderType
            } else {
                set headerType $xns:$inputHeaderType
            }
            $header appendChild [$doc createElement $headerType headerData]
            if {[llength $attrList]} {
                ::WS::Utils::setAttr $headerData $attrList
            }
        }
        ::WS::Utils::convertDictToType Client $serviceName $doc $headerData $argList $inputHeaderType
    }

    # side effect: save new element handle in variable bod
    $env appendChild [$doc createElement "SOAP-ENV:Body" bod]
    #puts "set xns \[dict get \[::WS::Utils::GetServiceTypeDef Client $serviceName $msgType\] xns\]"
    #puts "\t [::WS::Utils::GetServiceTypeDef Client $serviceName $msgType]"
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
        set forceNs 1
        set reply $bod
    } else {
        ::log::logsubst debug {$bod appendChild \[$doc createElement $xns:$msgType reply\]}
        $bod appendChild [$doc createElement $xns:$msgType reply]
        set forceNs 0
    }

    ::WS::Utils::convertDictToType Client $serviceName $doc $reply $argList $xns:$msgType $forceNs

    set encoding [lindex [split [lindex [split [dict get $serviceInfo contentType] {:}] end] {=}] end]
    set xml [format {<?xml version="1.0"  encoding="%s"?>} $encoding]
    append xml "\n" [$doc asXML -indent none -doctypeDeclaration 0]
    $doc delete

    ::log::logsubst debug {Leaving ::WS::Client::buildDocLiteralCallquery with {$xml}}

    return [encoding convertto $encoding $xml]

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

    ::log::logsubst debug {Entering [info level 0]}
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo operation $operationName inputs]
    set xnsList [dict get $serviceInfo targetNamespace]

    dom createDocument "SOAP-ENV:Envelope" doc
    $doc documentElement env
    $env setAttribute \
        xmlns:SOAP-ENV "http://schemas.xmlsoap.org/soap/envelope/" \
        xmlns:xsi      "http://www.w3.org/2001/XMLSchema-instance" \
        xmlns:xs       "http://www.w3.org/2001/XMLSchema"

    foreach {tns target} $xnsList {
        $env setAttribute xmlns:$tns $target
    }

    set firstHeader 1
    foreach inputHeaderType [dict get $serviceInfo operation $operationName soapRequestHeader] {
        if {$inputHeaderType eq {}} {
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
    # side effect: node handle is saved in variable reply
    if {![string is space $callXns]} {
        $bod appendChild [$doc createElement $callXns:$baseName reply]
    } else {
        $bod appendChild [$doc createElement $baseName reply]
    }
    $reply  setAttribute \
        SOAP-ENV:encodingStyle "http://schemas.xmlsoap.org/soap/encoding/"

    ::WS::Utils::convertDictToEncodedType Client $serviceName $doc $reply $argList $msgType

    set encoding [lindex [split [lindex [split [dict get $serviceInfo contentType] {;}] end] {=}] end]
    set xml [format {<?xml version="1.0"  encoding="%s"?>} $encoding]
    append xml "\n" [$doc asXML -indent none -doctypeDeclaration 0]
    $doc delete
    ::log::logsubst debug {Leaving ::WS::Client::buildRpcEncodedCallquery with {$xml}}

    return [encoding convertto $encoding $xml]

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
#    results    - Initial definition. This is optional and defaults to no definition.
#    serviceAlias - Alias (unique) name for service.
#                   This is an optional argument and defaults to the name of the
#                   service in serviceInfo.
#    serviceNumber - Number of service within the WSDL to assign the
#                    serviceAlias to. Only usable with a serviceAlias.
#                    First service (default) is addressed by value "1".
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
# 2.4.6    2017-12-07  H.Oehlmann   Added argument "serviceNumber"
#
#
###########################################################################
proc ::WS::Client::buildServiceInfo {wsdlNode tnsDict {serviceInfo {}} {serviceAlias {}} {serviceNumber 1}} {
    ##
    ## Need to refactor to foreach service parseService
    ##  Service drills down to ports, which drills down to bindings and messages
    ##
    ::log::logsubst debug {Entering [info level 0]}

    ##
    ## Parse Service information
    ##
    # WSDL snippet:
    #  <definitions ...>
    #    <service name="service1">
    #      ...
    #    </service>
    #    <service name="service2">
    #      ...
    #    </service>
    #  </definitions>
    # Without serviceAlias and serviceNumber, two services "service1" and
    # "service2" are created.
    # With serviceAlias = "SE" and serviceNumber=2, "service2" is created as
    # "SE".
    set serviceNameList [$wsdlNode selectNodes w:service]
    # Check for no service node
    if {[llength $serviceNameList] == 0} {
        return \
            -code error \
            -errorcode [list WS CLIENT NOSVC] \
            "WSDL does not define any services"
    }
    if {"" ne $serviceAlias} {
        if {$serviceNumber < 1 || $serviceNumber > [llength $serviceNameList]} {
            return \
                -code error \
                -errorcode [list WS CLIENT INVALDCNT] \
                "WSDL does not define service number $serviceNumber"
        }
        set serviceNameList [lrange $serviceNameList $serviceNumber-1 $serviceNumber-1]
    }

    foreach serviceNode $serviceNameList {
        lappend serviceInfo [parseService $wsdlNode $serviceNode $serviceAlias $tnsDict]
    }

    ::log::logsubst debug {Leaving ::WS::Client::buildServiceInfo with $serviceInfo}
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

    ::log::logsubst debug {Entering [info level 0]}
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
    if {[$wsdlNode hasAttribute targetNamespace]} {
        set target [$wsdlNode getAttribute targetNamespace]
    } else {
        set target $location
    }
    set tmpTargetNs $::WS::Utils::targetNs
    set ::WS::Utils::targetNs $target
    CreateService $serviceName WSDL $location $target xns $xns
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
    #dict unset serviceInfo tnsList
    dict set serviceInfo suppressTargetNS $options(suppressTargetNS)
    foreach {key value} [dict get $serviceInfo tnsList url] {
        dict set serviceInfo targetNamespace $value $key
    }
    set serviceArr($serviceName) $serviceInfo

    set ::WS::Utils::targetNs $tmpTargetNs

    ::log::logsubst debug {Leaving [lindex [info level 0] 0] with $serviceInfo}
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
    ::log::log debug "Entering [info level 0]"

    upvar 1 $serviceInfoVar serviceInfo


    set tnsCount [llength [dict keys [dict get $serviceInfo tnsList url]]]
    set baseUrl [dict get $serviceInfo location]
    foreach schemaNode [$wsdlNode selectNodes w:types/xs:schema] {
        ::log::log debug "Parsing node $schemaNode"
        ::WS::Utils::parseScheme Client $baseUrl $schemaNode $serviceName serviceInfo tnsCount
    }

    ::log::log debug "Leaving [lindex [info level 0] 0]"
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
# 2.4.2    2017-08-31  H.Oehlmann   Also set serviceArr operation members
#                                   inputsName and outputsName.
#
#
###########################################################################
proc ::WS::Client::parseBinding {wsdlNode serviceName bindingName serviceInfoVar} {
    ::log::log debug "Entering [info level 0]"
    upvar 1 $serviceInfoVar serviceInfo
    variable options

    set bindQuery [format {w:binding[attribute::name='%s']} $bindingName]
    array set msgToOper {}
    foreach binding [$wsdlNode selectNodes $bindQuery] {
        array unset msgToOper *
        set portName [lindex [split [$binding  getAttribute type] {:}] end]
        ::log::log debug "\t Processing binding '$bindingName' on port '$portName'"
        set operList [$binding selectNodes w:operation]
        set styleNode [$binding selectNodes d:binding]
        if {![info exists style]} {
            if {[catch {$styleNode getAttribute style} tmpStyle]} {
                set styleNode [$binding selectNodes {w:operation[1]/d:operation}]
                if {$styleNode eq {}} {
                    ##
                    ## This binding is for a SOAP level other than 1.1
                    ##
                    ::log::log debug "Skiping non-SOAP 1.1 binding [$binding asXML]"
                    continue
                }
                set style [$styleNode getAttribute style]
                #puts "Using style for first operation {$style}"
            } else {
                set style $tmpStyle
                #puts "Using style for first binding {$style}"
            }
            if {!($style eq {document} || $style eq {rpc} )} {
                ::log::log debug "Leaving [lindex [info level 0] 0] with error @1"
                return \
                    -code error \
                    -errorcode [list WS CLIENT UNSSTY $style] \
                    "Unsupported calling style: '$style'"
            }

            if {![info exists use]} {
                set use [[$binding selectNodes {w:operation[1]/w:input/d:body}] getAttribute use]
                if {!($style eq {document} && $use eq {literal} ) &&
                    !($style eq {rpc} && $use eq {encoded} )} {
                    ::log::log debug "Leaving [lindex [info level 0] 0] with error @2"
                    return \
                        -code error \
                        -errorcode [list WS CLIENT UNSMODE $use] \
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
            ::log::log debug "\t Processing operation '$operName'"

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
                            -errorcode [list WS CLIENT NOOVERLOAD $operName]
                }
                ##
                ## See if the existing operation needs to be cloned
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
                # typNameList contains inType inName outType outName
                set typeNameList [getTypesForPort $wsdlNode $serviceName $baseName $portName $inName serviceInfo $style]
                set operName ${operName}_[lindex [split [lindex $typeNameList 0] {:}] end]
                set cloneList [dict get $serviceInfo operation $baseName cloneList]
                lappend cloneList $operName
                dict set serviceInfo operation $baseName cloneList $cloneList
                dict set serviceInfo operation $operName isClone 1
            } else {
                set typeNameList [getTypesForPort $wsdlNode $serviceName $baseName $portName $inName serviceInfo $style]
                dict set serviceInfo operation $operName isClone 0
            }

            #puts "Processing operation $operName"
            set actionNode [$oper selectNodes d:operation]
            if {$actionNode eq {}} {
                ::log::log debug "Skiping operation with no action [$oper asXML]"
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
                if {[dict exists $serviceInfo soapActions $action]} {
                    set actionList [dict get $serviceInfo soapActions $action]
                } else {
                    set actionList {}
                }
                lappend actionList $operName
                dict set serviceInfo soapActions $action $actionList
            }

            ##
            ## Get the input headers, if any
            ##
            set soapRequestHeaderList {{}}
            foreach inHeader [$oper selectNodes w:input/d:header] {
                ##set part [$inHeader getAttribute part]
                set tmp [$inHeader getAttribute use]
                if {$tmp ne $use} {
                    ::log::log debug "Leaving [lindex [info level 0] 0] with error @3"
                    return \
                        -code error \
                        -errorcode [list WS CLIENT MIXUSE $use $tmp] \
                        "Mixed usage not supported!'"
                }
                set msgName [$inHeader getAttribute message]
                ::log::log debug [list messageToType $wsdlNode $serviceName $baseName $msgName serviceInfo $style]
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
                if {$tmp ne $use} {
                    ::log::log debug "Leaving [lindex [info level 0] 0] with error @4"
                    return \
                        -code error \
                        -errorcode [list WS CLIENT MIXUSE $use $tmp] \
                        "Mixed usage not supported!'"
                }
                set messagePath [$outHeader getAttribute message]
                set msgName [lindex [split $messagePath {:}] end]
                ::log::log debug [list messageToType $wsdlNode $serviceName $baseName $msgName serviceInfo $style]
                set type [messageToType $wsdlNode $serviceName $baseName $msgName serviceInfo $style]
                lappend soapReplyHeaderList $type
            }
            dict set serviceInfo operation $operName soapReplyHeader $soapReplyHeaderList

            ##
            ## Validate that the input and output uses are the same
            ##
            set inUse $use
            set outUse $use
            catch {set inUse [[$oper selectNodes w:input/d:body] getAttribute use]}
            catch {set outUse [[$oper selectNodes w:output/d:body] getAttribute use]}
            foreach tmp [list $inUse $outUse] {
                if {$tmp ne $use} {
                    ::log::log debug "Leaving [lindex [info level 0] 0] with error @5"
                    return \
                        -code error \
                        -errorcode [list WS CLIENT MIXUSE $use $tmp] \
                        "Mixed usage not supported!'"
                }
            }
            ::log::log debug "\t Input/Output types and names are {$typeNameList}"
            foreach {type name} $typeNameList mode {inputs outputs} {
                dict set serviceInfo operation $operName $mode $type
                # also set outputsname which is used to match it as alternate response node name
                dict set serviceInfo operation $operName ${mode}name $name
            }
            set inMessage [dict get $serviceInfo operation $operName inputs]
            if {[dict exists $serviceInfo inputMessages $inMessage] } {
                set operList [dict get $serviceInfo inputMessages $inMessage]
            } else {
                set operList {}
            }
            lappend operList $operName
            dict set serviceInfo inputMessages $inMessage $operList

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

    ::log::log debug "Leaving [lindex [info level 0] 0]"
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
# Returns : A list containing the input and output types and names
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
# 2.4.2    2017-08-31  H.Oehlmann   Extend return by names to verify this
#                                   as return output node name.
# 2.4.3    2017-11-03  H.Oehlmann   If name is not given, set the default
#                                   name of <OP>Request/Response given by the
#                                   WSDL 1.0 standard.
#
#
###########################################################################
proc ::WS::Client::getTypesForPort {wsdlNode serviceName operName portName inName serviceInfoVar style} {
    ::log::log debug "Entering [info level 0]"
    upvar 1 $serviceInfoVar serviceInfo

    set inType {}
    set outType {}

    #set portQuery [format {w:portType[attribute::name='%s']} $portName]
    #set portNode [lindex [$wsdlNode selectNodes $portQuery] 0]
    if {$inName eq {}} {
        set operQuery [format {w:portType[attribute::name='%s']/w:operation[attribute::name='%s']} \
                        $portName $operName]
    } else {
        set operQuery [format {w:portType[attribute::name='%s']/w:operation[attribute::name='%s']/w:input[attribute::name='%s']/parent::*} \
                        $portName $operName $inName]
    }
    ::log::log debug "\t operNode query is {$operQuery}"
    set operNode [$wsdlNode selectNodes $operQuery]
    if {$operNode eq {} && $inName ne {}} {
        set operQuery [format {w:portType[attribute::name='%s']/w:operation[attribute::name='%s']} \
                        $portName $operName]
        ::log::log debug "\t operNode query is {$operQuery}"
        set operNode [$wsdlNode selectNodes $operQuery]
    }

    set resList {}
    foreach sel {w:input w:output} defaultNameSuffix {Request Response} {
        set nodeList [$operNode selectNodes $sel]
        if {1 == [llength $nodeList]} {
            set nodeCur [lindex $nodeList 0]
            set msgPath [$nodeCur getAttribute message]
            set msgCur [lindex [split $msgPath {:}] end]
            # Append type
            lappend resList [messageToType $wsdlNode $serviceName $operName $msgCur serviceInfo $style]
            # Append name
            if {[$nodeCur hasAttribute name]} {
                lappend resList [$nodeCur getAttribute name]
            } else {
                # Build the default name according WSDL 1.0 as
                # <Operation>Request/Response
                lappend resList ${operName}$defaultNameSuffix
            }
        }
    }

    ##
    ## Return the types
    ##
    ::log::log debug "Leaving [lindex [info level 0] 0] with $resList"
    return $resList
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
    ::log::log debug "Entering [info level 0]"

    #puts "Message to Type $serviceName $operName $msgName"

    set msgQuery [format {w:message[attribute::name='%s']} $msgName]
    set msg [$wsdlNode selectNodes $msgQuery]
    if {$msg eq {} &&
        [llength [set msgNameList [split $msgName {:}]]] > 1} {
        set tmpMsgName [join [lrange $msgNameList 1 end] {:}]
        set msgQuery [format {w:message[attribute::name='%s']} $tmpMsgName]
        set msg [$wsdlNode selectNodes $msgQuery]
    }
    if {$msg eq {}} {
        return \
            -code error \
            -errorcode [list WS CLIENT BADMSGSEC $msgName] \
            "Can not find message '$msgName'"
    }
    switch -exact -- $style {
        document/literal {
            set partNode [$msg selectNodes w:part]
            set partNodeCount [llength $partNode]
            ::log::log debug  "partNodeCount = {$partNodeCount}"
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
    ::log::log debug "Leaving [lindex [info level 0] 0] with {$type}"
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
#       argList         - The arguments to the operation as a dictionary object.
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
#       WS CLIENT HTTPERROR      - if an HTTP error occurred
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
# 2.4.1    2017-08-31  H.Oehlmann   Use utility function
#                                   ::WS::Utils::geturl_fetchbody for http call
#                                   which also follows redirects.
# 3.0.0    2020-10-26  H.Oehlmann   Added query timeout
#
#
###########################################################################
proc ::WS::Client::DoRawRestCall {serviceName objectName operationName argList {headers {}} {location {}}} {
    variable serviceArr

    ::log::logsubst debug {Entering [info level 0]}
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

    ##
    ## build call query
    ##

    if {$location ne {}} {
        set url $location
    } else {
        set url [dict get $serviceInfo object $objectName location]
    }
    SaveAndSetOptions $serviceName
    if {[catch {set query [buildRestCallquery $serviceName $objectName $operationName $url $argList]} err]} {
        RestoreSavedOptions $serviceName
        return -code error -errorcode $::errorCode -errorinfo $::errorInfo $err
    } else {
        RestoreSavedOptions $serviceName
    }
    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }

    ##
    ## do http call
    ##

    set largs {}
    if {[llength $headers]} {
        lappend largs -headers $headers
    }
    set body [::WS::Utils::geturl_fetchbody $url\
            -query $query\
            -type [dict get $serviceInfo contentType]\
            -timeout [dict get $serviceInfo queryTimeout]\
            {*}$largs]

    ::log::logsubst debug {Leaving ::WS::Client::DoRawRestCall with {$body}}
    return $body

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
#       argList         - The arguments to the operation as a dictionary object
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
#       WS CLIENT HTTPERROR      - if an HTTP error occurred
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
# 2.4.1    2017-08-31  H.Oehlmann   Use utility function
#                                   ::WS::Utils::geturl_fetchbody for http call
#                                   which also follows redirects.
# 3.0.0    2020-10-26  H.Oehlmann   Added query timeout
#
#
###########################################################################
proc ::WS::Client::DoRestCall {serviceName objectName operationName argList {headers {}} {location {}}} {
    variable serviceArr

    ::log::logsubst debug {Entering [info level 0]}
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
    if {$location ne {}} {
        set url $location
    } else {
        set url [dict get $serviceInfo object $objectName location]
    }

    ##
    ## build call query
    ##

    SaveAndSetOptions $serviceName
    if {[catch {set query [buildRestCallquery $serviceName $objectName $operationName $url $argList]} err]} {
        RestoreSavedOptions $serviceName
        return -code error -errorcode $::errorCode -errorinfo $::errorInfo $err
    }
    RestoreSavedOptions $serviceName

    ##
    ## Do http call
    ##

    if {[dict exists $serviceInfo headers]} {
        set headers [concat $headers [dict get $serviceInfo headers]]
    }
    set largs {}
    if {[llength $headers]} {
        lappend largs -headers $headers
    }
    set body [::WS::Utils::geturl_fetchbody $url\
            -query $query\
            -type [dict get $serviceInfo contentType]\
            -timeout [dict get $serviceInfo queryTimeout]\
            {*}$largs]

    ##
    ## Parse results
    ##

    SaveAndSetOptions $serviceName
    if {[catch {
        parseRestResults $serviceName $objectName $operationName $body
    } results]} {
        RestoreSavedOptions $serviceName
        ::log::log debug "Leaving (error) ::WS::Client::DoRestCall"
        return -code error $results
    }
    RestoreSavedOptions $serviceName
    ::log::logsubst debug {Leaving ::WS::Client::DoRestCall with {$results}}
    return $results

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
#       argList         - The arguments to the operation as a dictionary object
#                         This is for both the Soap Header and Body messages.
#       succesCmd       - A command prefix to be called if the operations
#                         does not raise an error.  The results, as a dictionary
#                         object are concatenated to the prefix.
#       errorCmd        - A command prefix to be called if the operations
#                         raises an error.  The error code and stack trace
#                         are concatenated to the prefix.
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
#       WS CLIENT HTTPERROR      - if an HTTP error occurred
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
        set headers [concat $headers $svcHeaders]
    }
    ::log::logsubst debug {Entering [info level 0]}
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
    SaveAndSetOptions $serviceName
    if {[catch {set query [buildRestCallquery $serviceName $objectName $operationName $url $argList]} err]} {
        RestoreSavedOptions $serviceName
        return -code error -errorcode $::errorCode -errorinfo $::errorInfo $err
    } else {
        RestoreSavedOptions $serviceName
    }
    set largs {}
    if {[llength $headers]} {
        lappend largs -headers $headers
    }
    ::log::logsubst info {::http::geturl $url \
            -query $query \
            -type [dict get $serviceInfo contentType] \
            -command [list ::WS::Client::asyncRestCallDone $serviceName $operationName $succesCmd $errorCmd] \
            -timeout [dict get $serviceInfo queryTimeout]\
            {*}$largs}
    ::http::geturl $url \
            -query $query \
            -type [dict get $serviceInfo contentType] \
            -headers $headers \
            -command [list ::WS::Client::asyncRestCallDone $serviceName $operationName $succesCmd $errorCmd] \
            -timeout [dict get $serviceInfo queryTimeout]\
            {*}$largs
    ::log::log debug "Leaving ::WS::Client::DoAsyncRestCall"
    return
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

    ::log::logsubst debug {Entering [info level 0]}
    set serviceInfo $serviceArr($serviceName)
    set msgType [dict get $serviceInfo object $objectName operation $operationName inputs]
    set xnsList [dict get $serviceInfo targetNamespace]

    dom createDocument "request" doc
    $doc documentElement body
    $body setAttribute \
        "method"      $operationName
    foreach {tns target} $xnsList {
        #set tns [lindex $xns 0]
        #set target [lindex $xns 1]
        $body  setAttribute \
            xmlns:$tns $target
    }

    set xns [dict get [::WS::Utils::GetServiceTypeDef Client $serviceName $msgType] xns]

    ::log::logsubst debug {calling [list ::WS::Utils::convertDictToType Client $serviceName $doc $body $argList $msgType]}
    set options [::WS::Utils::SetOption]
    ::WS::Utils::SetOption UseNS 0
    ::WS::Utils::SetOption genOutAttr 1
    ::WS::Utils::SetOption valueAttr {}
    ::WS::Utils::convertDictToType Client $serviceName $doc $body $argList $msgType
    set encoding [lindex [split [lindex [split [dict get $serviceInfo contentType] {;}] end] {=}] end]
    foreach {option value} $options {
        ::WS::Utils::SetOption $option $value
    }

    set xml [format {<?xml version="1.0"  encoding="%s"?>} $encoding]
    append xml "\n" [$doc asXML -indent none -doctypeDeclaration 0]
    #regsub "<!DOCTYPE\[^>\]*>\n" [::dom::DOMImplementation serialize $doc] {} xml
    $doc delete
    set xml [encoding convertto $encoding $xml]

    set inTransform [dict get $serviceInfo inTransform]
    if {$inTransform ne {}} {
        set xml [$inTransform $serviceName $operationName REQUEST $xml $url $argList]
    }

    ::log::logsubst debug {Leaving ::WS::Client::buildRestCallquery with {$xml}}

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
#       WS CLIENT REMERR         - The remote end raised an exception, the third element of
#                                 the error code is the remote fault code.
#                                 Error info is set to the remote fault details.
#                                 The error message is the remote fault string.
#       WS CLIENT BADREPLY       - Badly formatted reply, the third element is a list of
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

    ::log::logsubst debug {Entering [info level 0]}
    set first [string first {<} $inXML]
    if {$first > 0} {
        set inXML [string range $inXML $first end]
    }
    set serviceInfo $serviceArr($serviceName)
    set outTransform [dict get $serviceInfo outTransform]
    if {$outTransform ne {}} {
        set inXML [$outTransform $serviceName $operationName REPLY $inXML]
    }
    set expectedMsgType [dict get $serviceInfo object $objectName operation $operationName outputs]
    # save parsed xml handle in variable doc
    dom parse $inXML doc
    # save top node handle in variable top
    $doc documentElement top
    set xns {}
    foreach tmp [dict get $serviceInfo targetNamespace] {
        lappend xns $tmp
    }
    ::log::logsubst debug {Using namespaces {$xns}}
    set body $top
    set status [$body getAttribute status]

    ##
    ## See if it is a standard error packet
    ##
    if {$status ne {ok}} {
        set faultstring {}
        if {[catch {set faultstring [[$body selectNodes error] asText]}]} {
            catch {set faultstring [[$body selectNodes error] asText]}
        }
        $doc delete
        return \
            -code error \
            -errorcode [list WS CLIENT REMERR $status] \
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
    ::log::logsubst debug {Calling ::WS::Utils::convertTypeToDict Client $serviceName $body $expectedMsgType $body}
    if {$expectedMsgType ne {}} {
        set node [$body childNodes]
        set nodeName [$node nodeName]
        if {$objectName ne $nodeName} {
            return \
                -code error \
                -errorcode [list WS CLIENT BADRESPONSE [list $objectName $nodeName]] \
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
    ::log::logsubst debug {Entering [info level 0]}

    ##
    ## Check for errors
    ##
    set body [::http::data $token]
    ::log::logsubst info {\nReceived: $body}
    if {[::http::status $token] ne {ok} ||
        ( [::http::ncode $token] != 200 && $body eq {} )} {
        set errorCode [list WS CLIENT HTTPERROR [::http::code $token]]
        set hadError 1
        set errorInfo [FormatHTTPError $token]
    } else {
        SaveAndSetOptions $serviceName
        if {[catch {set hadError [catch {parseRestResults $serviceName $objectName $operationName $body} results]} err]} {
            RestoreSavedOptions $serviceName
            return -code error -errorcode $::errorCode -errorinfo $::errorInfo $err
        } else {
            RestoreSavedOptions $serviceName
        }
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
    return
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
# Description : Save the global options of the utilities package and
#               set them for how this service needs them.
#
# Arguments :
#    serviceName        - the name of the service called
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
#       1  03/06/2012  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::SaveAndSetOptions {serviceName} {
    variable serviceArr
    variable utilsOptionsList

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    set savedDict {}
    foreach item $utilsOptionsList {
        if {[dict exists $serviceInfo $item] && [string length [set value [dict get $serviceInfo $item]]]} {
            dict set savedDict $item [::WS::Utils::SetOption $item]
            ::WS::Utils::SetOption $item $value
        }
    }
    dict set serviceArr($serviceName) UtilsSavedOptions $savedDict
    return
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Client::RestoreSavedOptions
#
# Description : Restore the saved global options of the utilities package.
#
# Arguments :
#    serviceName        - the name of the service called
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
#       1  03/06/2012  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Client::RestoreSavedOptions {serviceName} {
    variable serviceArr

    if {![info exists serviceArr($serviceName)]} {
        return \
            -code error \
            -errorcode [list WS CLIENT UNKSRV $serviceName] \
            "Unknown service '$serviceName'"
    }
    set serviceInfo $serviceArr($serviceName)
    set savedDict {}
    foreach {item value} [dict get $serviceInfo UtilsSavedOptions] {
        ::WS::Utils::SetOption $item $value
    }
    dict set serviceArr($serviceName) UtilsSavedOptions {}
    return
}
