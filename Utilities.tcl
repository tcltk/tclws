###############################################################################
##                                                                           ##
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

package require tdom 0.8
package require struct::set

package provide WS::Utils 3.1.0

namespace eval ::WS {}

namespace eval ::WS::Utils {
    set ::WS::Utils::typeInfo {}
    set ::WS::Utils::currentSchema {}
    array set ::WS::Utils::importedXref {}
	variable redirectArray
	array set redirectArray {}
    set nsList {
        w http://schemas.xmlsoap.org/wsdl/
        d http://schemas.xmlsoap.org/wsdl/soap/
        xs http://www.w3.org/2001/XMLSchema
    }

    # mapping of how the simple SOAP types should be serialized using YAJL into JSON.
    array set ::WS::Utils::simpleTypesJson {
        boolean "bool"
        float "number"
        double "double"
        integer "integer"
        int "integer"
        long "integer"
        short "integer"
        byte "integer"
        nonPositiveInteger "integer"
        negativeInteger "integer"
        nonNegativeInteger "integer"
        unsignedLong "integer"
        unsignedInt "integer"
        unsignedShort "integer"
        unsignedByte "integer"
        positiveInteger "integer"
        decimal "number"
    }

    array set ::WS::Utils::simpleTypes {
        anyType 1
        string 1
        boolean 1
        decimal 1
        float 1
        double 1
        duration 1
        dateTime 1
        time 1
        date 1
        gYearMonth 1
        gYear 1
        gMonthDay 1
        gDay 1
        gMonth 1
        hexBinary 1
        base64Binary 1
        anyURI 1
        QName 1
        NOTATION 1
        normalizedString 1
        token 1
        language 1
        NMTOKEN 1
        NMTOKENS 1
        Name 1
        NCName 1
        ID 1
        IDREF 1
        IDREFS 1
        ENTITY 1
        ENTITIES 1
        integer 1
        nonPositiveInteger 1
        negativeInteger 1
        long 1
        int 1
        short 1
        byte 1
        nonNegativeInteger 1
        unsignedLong 1
        unsignedInt 1
        unsignedShort 1
        unsignedByte 1
        positiveInteger 1
    }
    array set ::WS::Utils::options {
        UseNS 1
        StrictMode error
        parseInAttr 0
        genOutAttr 0
        valueAttrCompatiblityMode 1
        includeDirectory {}
        suppressNS {}
        useTypeNs 0
        nsOnChangeOnly 0
        anyType string
		queryTimeout 60000
    }

    set ::WS::Utils::standardAttributes {
        baseType
        comment
        example
        pattern
        length
        fixed
        maxLength
        minLength
        minInclusive
        maxInclusive
        enumeration
        type
    }

    dom parse {
<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema"
    version="1.0">

  <xsl:template priority="1" match="comment()"/>

  <xsl:template match="xs:choice">
      <xsl:apply-templates/>
  </xsl:template>

  <!-- Copy all the attributes and other nodes -->
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>
    } ::WS::Utils::xsltSchemaDom

    set currentNs {}

}




###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::GetCrossreference
#
# Description : Get the type cross reference information for a service.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service
#
# Returns : A dictionary of cross reference information
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
proc ::WS::Utils::GetCrossreference {mode service} {
    variable typeInfo

    array set crossreference {}

    dict for {type typeDict} [dict get $typeInfo $mode $service] {
        foreach {field fieldDict} [dict get $typeDict definition] {
            set fieldType [string trimright [dict get $fieldDict type] {()?}]
            incr crossreference($fieldType,count)
            lappend crossreference($fieldType,usedBy) $type.$field
        }
        if {![info exists crossreference($type,count) ]} {
            set crossreference($type,count) 0
            set crossreference($type,usedBy) {}
        }
    }

    return [array get crossreference]
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::SetOption
#
# Description : Define a type for a service.
#
# Arguments :
#       option        - option
#       value         - value (optional)
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
proc ::WS::Utils::SetOption {args} {
    variable options

    if {[llength $args] == 0} {
        ::log::log debug {Return all options}
        return [array get options]
    } elseif {[llength $args] == 1} {
        set opt [lindex $args 0]
        ::log::logsubst debug {One Option {$opt}}
        if {[info exists options($opt)]} {
            return $options($opt)
        } else {
            ::log::logsubst debug {Unknown option {$opt}}
            return \
                -code error \
                -errorcode [list WS CLIENT UNKOPTION $opt] \
                "Unknown option'$opt'"
        }
    } elseif {([llength $args] % 2) == 0} {
        ::log::log debug {Multiple option pairs}
        foreach {opt value} $args {
            if {[info exists options($opt)]} {
                ::log::logsubst debug {Setting Option {$opt} to {$value}}
                set options($opt) $value
            } else {
                ::log::logsubst debug {Unknown option {$opt}}
                return \
                    -code error \
                    -errorcode [list WS CLIENT UNKOPTION $opt] \
                    "Unknown option'$opt'"
            }
        }
    } else {
        ::log::logsubst debug {Bad number of arguments {$args}}
        return \
            -code error \
            -errorcode [list WS CLIENT INVARGCNT $args] \
            "Invalid argument count'$args'"
    }
    return
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::ServiceTypeDef
#
# Description : Define a type for a service.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this type definition is for
#       type            - The type to be defined/redefined
#       definition      - The definition of the type's fields.  This consist of one
#                         or more occurence of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is: {type typeName comment commentString}
#                           typeName can be any simple or defined type.
#                           commentString is a quoted string describing the field.
#       xns             - The namespace
#       abstract        - Boolean indicating if this is an abstract, and hence mutable type
#       version         - Version code for the custom type
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
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  07/06/2006  G.Lester     Initial version
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Utils::ServiceTypeDef {mode service type definition {xns {}} {abstract {false}} {version {}}} {
    ::log::logsubst debug {Entering [info level 0]}
    variable typeInfo

    if {![string length $xns]} {
        set xns $service
    }
    if {[llength [split $type {:}]] == 1} {
        set type $xns:$type
    }
    dict set typeInfo $mode $service $type definition $definition
    dict set typeInfo $mode $service $type xns $xns
    dict set typeInfo $mode $service $type abstract $abstract
    if {$version ne {}} {
        dict set typeInfo $mode $service $type version $version
    }
    return
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::MutableTypeDef
#
# Description : Define a mutable type for a service.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this type definition is for
#       type            - The type to be defined/redefined
#       fromSwitchCmd   - The cmd to determine the actaul type when converting
#                         from DOM to a dictionary.  The actual call will have
#                         the following arguments appended to the command:
#                           mode service type xns DOMnode
#       toSwitchCmd     - The cmd to determine the actual type when converting
#                         from a dictionary to a DOM.  The actual call will have
#                         the following arguments appended to the command:
#                           mode service type xns remainingDictionaryTree
#       xns             - The namespace
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
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  02/15/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Utils::MutableTypeDef {mode service type fromSwitchCmd toSwitchCmd {xns {}}} {
    variable mutableTypeInfo

    if {![string length $xns]} {
        set xns $service
    }
    set mutableTypeInfo([list $mode $service $type]) \
        [list $fromSwitchCmd $toSwitchCmd]
    return
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::ServiceSimpleTypeDef
#
# Description : Define a type for a service.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this type definition is for
#       type            - The type to be defined/redefined
#       definition      - The definition of the type's fields.  This consist of one
#                         or more occurance of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is list of name value:
#                           basetype typeName - any simple or defined type.
#                           comment commentString - a quoted string describing the field.
#                           pattern value
#                           length value
#                           fixed "true"|"false"
#                           maxLength value
#                           minLength value
#                           minInclusive value
#                           maxInclusive value
#                           enumeration value
#
#       xns             - The namespace
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
proc ::WS::Utils::ServiceSimpleTypeDef {mode service type definition {xns {tns1}}} {
    variable simpleTypes
    variable typeInfo

    ::log::logsubst debug {Entering [info level 0]}
    if {![dict exists $definition xns]} {
        set simpleTypes($mode,$service,$type) [concat $definition xns $xns]
    } else {
        set simpleTypes($mode,$service,$type) $definition
    }
    if {[dict exists $typeInfo $mode $service $type]} {
        ::log::logsubst debug {\t Unsetting typeInfo $mode $service $type}
        ::log::logsubst debug {\t Was [dict get $typeInfo $mode $service $type]}
        dict unset typeInfo $mode $service $type
    }
    return
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name :      ::WS::Utils::GetServiceTypeDef
#
# Description : Query for type definitions.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this query is for
#       type            - The type to be retrieved (optional)
#
# Returns :
#       If type not provided, a dictionary object describing all of the complex types
#       for the service.
#       If type provided, a dictionary object describing the type.
#         A definition consist of a dictionary object with the following key/values:
#           xns         - The namespace for this type.
#           definition  - The definition of the type's fields.  This consist of one
#                         or more occurance of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is: {type typeName comment commentString}
#                         Where field info is list of name value:
#                           basetype typeName - any simple or defined type.
#                           comment commentString - a quoted string describing the field.
#                           pattern value
#                           length value
#                           fixed "true"|"false"
#                           maxLength value
#                           minLength value
#                           minInclusive value
#                           maxInclusive value
#                           enumeration value
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    The service must be defined.
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
proc ::WS::Utils::GetServiceTypeDef {mode service {type {}}} {
    variable typeInfo
    variable simpleTypes

    set type [string trimright $type {()?}]
    set results {}
    if {[string equal $type {}]} {
        ::log::log debug "@1"
        set results [dict get $typeInfo $mode $service]
    } else {
        set typeInfoList [TypeInfo $mode $service $type]
        if {[string equal -nocase -length 3 $type {xs:}]} {
            set type [string range $type 3 end]
        }
        ::log::logsubst debug {Type = {$type} typeInfoList = {$typeInfoList}}
        if {[info exists simpleTypes($mode,$service,$type)]} {
            ::log::log debug "@2"
            set results $simpleTypes($mode,$service,$type)
        } elseif {[info exists simpleTypes($type)]} {
            ::log::log debug "@3"
            set results [list type xs:$type xns xs]
        } elseif {[dict exists $typeInfo $mode $service $service:$type]} {
            ::log::log debug "@5"
            set results [dict get $typeInfo $mode $service $service:$type]
        } elseif {[dict exists $typeInfo $mode $service $type]} {
            ::log::log debug "@6"
            set results [dict get $typeInfo $mode $service $type]
        }
    }

    return $results
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name :      ::WS::Utils::GetServiceSimpleTypeDef
#
# Description : Query for type definitions.
#
# Arguments :
#       mode            - Client|Server
#       service         - The name of the service this query is for
#       type            - The type to be retrieved (optional)
#
# Returns :
#       If type not provided, a dictionary object describing all of the simple types
#       for the service.
#       If type provided, a dictionary object describing the type.
#         A definition consist of a dictionary object with the following key/values:
#           xns         - The namespace for this type.
#           definition  - The definition of the type's fields.  This consist of one
#                         or more occurance of a field definition.  Each field definition
#                         consist of:  fieldName fieldInfo
#                         Where field info is: {type typeName comment commentString}
#                         Where field info is list of name value and any restrictions:
#                           basetype typeName - any simple or defined type.
#                           comment commentString - a quoted string describing the field.
#                           pattern value
#                           length value
#                           fixed "true"|"false"
#                           maxLength value
#                           minLength value
#                           minInclusive value
#                           maxInclusive value
#                           enumeration value
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    The service must be defined.
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
proc ::WS::Utils::GetServiceSimpleTypeDef {mode service {type {}}} {
    variable simpleTypes

    set type [string trimright $type {()?}]
    if {[string equal -nocase -length 3 $type {xs:}]} {
        return [::WS::Utils::GetServiceTypeDef $mode $service $type]
    }
    if {[string equal $type {}]} {
        set results {}
        foreach {key value} [array get simpleTypes $mode,$service,*] {
            lappend results [list [lindex [split $key {,}] end] $simpleTypes($key)]
        }
    } else {
        if {[info exists simpleTypes($mode,$service,$type)]} {
            set results $simpleTypes($mode,$service,$type)
        } elseif {[info exists simpleTypes($type)]} {
            set results [list type $type xns xs]
        } else {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKSMPTYP $type] \
                "Unknown simple type '$type'"
        }
    }

    return $results
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::ProcessImportXml
#
# Description : Parse the bindings for a service from a WSDL into our
#               internal representation
#
# Arguments :
#    mode           - The mode, Client or Server
#    baseUrl        - The URL we are processing
#    xml            - The XML string to parse
#    serviceName    - The name service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#    tnsCountVar    - The name of the variable containing the count of the
#                     namespace.
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
#   2.6.1  07/20/2018  A.Goth       Correct variable access problems. Ticket [7bb1cd7b43]
#                                   Corrected access of right document. Ticket [61fd346dc3]
#                                   Bugs introduced 2015-05-24 Checkin [9c7e118edb]
#          2018-09-03  H.Oehlmann   Replaced stderr error print by error log.
#
###########################################################################
proc ::WS::Utils::ProcessImportXml {mode baseUrl xml serviceName serviceInfoVar tnsCountVar} {
    ::log::logsubst debug {Entering [info level 0]}
    upvar 1 $serviceInfoVar serviceInfo
    upvar 1 $tnsCountVar tnsCount
    variable currentSchema
    variable nsList
    variable xsltSchemaDom

    set first [string first {<} $xml]
    if {$first > 0} {
        set xml [string range $xml $first end]
    }
    if {[catch {dom parse $xml tmpdoc}]} {
        set first [string first {?>} $xml]
        incr first 2
        set xml [string range $xml $first end]
        dom parse $xml tmpdoc
    }
    $tmpdoc xslt $xsltSchemaDom doc
    $tmpdoc delete
    $doc selectNodesNamespaces {
        w http://schemas.xmlsoap.org/wsdl/
        d http://schemas.xmlsoap.org/wsdl/soap/
        xs http://www.w3.org/2001/XMLSchema
    }
    $doc documentElement schema
    if {[catch {ProcessIncludes $schema $baseUrl} errMsg]} {
	::log::log error "Error processing include $schema $baseUrl: $errMsg"
    }

    set prevSchema $currentSchema
    set nodeList [$doc selectNodes -namespaces $nsList descendant::xs:schema]
    foreach node $nodeList {
        set currentSchema $node
        parseScheme $mode $baseUrl $node $serviceName serviceInfo tnsCount
    }

    set currentSchema $prevSchema
    $doc delete
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::ProcessIncludes
#
# Description : Replace all include nodes with the contents of the included url.
#
# Arguments :
#    rootNode - the root node of the document
#    baseUrl  - The URL being processed
#
# Returns : nothing
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
#       1  25/05/2006  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Utils::ProcessIncludes {rootNode baseUrl {includePath {}}} {
    variable xsltSchemaDom
    variable nsList
    variable options
    variable includeArr

    ::log::logsubst debug {Entering [info level 0]}

    set includeNodeList [concat \
                            [$rootNode selectNodes -namespaces $nsList descendant::xs:include] \
                            [$rootNode selectNodes -namespaces $nsList descendant::w:include] \
                            [$rootNode selectNodes -namespaces $nsList descendant::w:import] \
    ]
    set inXml [$rootNode asXML]
    set included 0
    foreach includeNode $includeNodeList {
        ::log::logsubst debug {\t Processing Include [$includeNode asXML]}
        if {[$includeNode hasAttribute schemaLocation]} {
            set urlTail [$includeNode getAttribute schemaLocation]
            set url [::uri::resolve $baseUrl  $urlTail]
        } elseif {[$includeNode hasAttribute location]} {
            set url [$includeNode getAttribute location]
            set urlTail [file tail [dict get [::uri::split $url] path]]
        } else {
            continue
        }
        if {[lsearch -exact $includePath $url] != -1} {
            log::logsubst warning {Include loop detected: [join $includePath { -> }]}
            continue
        } elseif {[info exists includeArr($url)]} {
            continue
        } else {
            set includeArr($url) 1
        }
        incr included
        ::log::logsubst info {\t Including {$url} from base {$baseUrl}}
        switch -exact -- [dict get [::uri::split $url] scheme] {
            file {
                upvar #0 [::uri::geturl $url] token
                set xml $token(data)
                unset token
            }
            https -
            http {
                set ncode -1
                catch {
                    ::log::logsubst info {[list ::http::geturl $url\
                            -timeout $options(queryTimeout)]}
                    set token [::http::geturl $url -timeout $options(queryTimeout)]
                    set ncode [::http::ncode $token]
                    set xml [::http::data $token]
                    ::log::logsubst info {Received Ncode = ($ncode), $xml}
                    ::http::cleanup $token
                }
                if {($ncode != 200) && [string equal $options(includeDirectory) {}]} {
                    return \
                        -code error \
                        -errorcode [list WS CLIENT HTTPFAIL $url $ncode] \
                        "HTTP get of import file failed '$url'"
                } elseif {($ncode != 200) && ![string equal $options(includeDirectory) {}]} {
                    set fn [file join  $options(includeDirectory) $urlTail]
                    set ifd  [open $fn r]
                    set xml [read $ifd]
                    close $ifd
                }
            }
            default {
                return \
                    -code error \
                    -errorcode [list WS CLIENT UNKURLTYP $url] \
                    "Unknown URL type '$url'"
            }
        }
        set parentNode [$includeNode parentNode]
        set nextSibling [$includeNode nextSibling]
        set first [string first {<} $xml]
        if {$first > 0} {
            set xml [string range $xml $first end]
        }
        dom parse $xml tmpdoc
        $tmpdoc xslt $xsltSchemaDom doc
        $tmpdoc delete
        set children 0
        set top [$doc documentElement]
        ::WS::Utils::ProcessIncludes $top $url [concat $includePath $baseUrl]
        foreach childNode [$top childNodes] {
            if {[catch {
                    #set newNode [$parentNode appendXML [$childNode asXML]]
                    #$parentNode removeChild $newNode
                    #$parentNode insertBefore $newNode $includeNode
                    $parentNode insertBefore $childNode $includeNode
                }]} {
                continue
            }
            incr children
        }
        $doc delete
        $includeNode delete
    }
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::TypeInfo
#
# Description : Return a list indicating if the type is simple or complex
#               and if it is a scalar or an array.  Also if it is optional
#
# Arguments :
#    type       - the type name, possibly with a () to specify it is an array
#
# Returns : A list of two elements, as follows:
#               0|1 - 0 means a simple type, 1 means a complex type
#               0|1 - 0 means a scalar, 1 means an array
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
#  2.3.0   10/16/2012  G. Lester    Corrected detection of service specific simple type.
#  2.3.0   10/31/2012  G. Lester    Corrected missing newline.
#
###########################################################################
proc ::WS::Utils::TypeInfo {mode service type {findOptional 0}} {
    variable simpleTypes
    variable typeInfo

    set type [string trim $type]
    set isOptional 0
    if {[string equal [string index $type end] {?}]} {
        set isOptional 1
        set type [string trimright $type {?}]
    }
    if {[string equal [string range $type end-1 end] {()}]} {
        set isArray 1
        set type [string range $type 0 end-2]
    } elseif {[string equal $type {array}]} {
        set isArray 1
    } else {
        set isArray 0
    }
    #set isNotSimple [dict exists $typeInfo $mode $service $type]
    #set isNotSimple [expr {$isNotSimple || [dict exists $typeInfo $mode $service $service:$type]}]
    lassign [split $type {:}] tns baseType
    set isNotSimple [expr {!([info exist simpleTypes($type)] || [info exist simpleTypes($baseType)] || [info exist simpleTypes($mode,$service,$type)] || [info exist simpleTypes($mode,$service,$baseType)] )}]
    if {$findOptional} {
        return [list $isNotSimple $isArray $isOptional]
    }
    return [list $isNotSimple $isArray]
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::CheckAndBuild::ValidateRequest
#
# Description : Given a schema validate a XML string given as parameter
#               using a XML schema description (in WS:: form) for
#               validation
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       xmlString   - The XML string to validate
#       tagName     - The name of the starting tag
#       typeName    - The type for the tag
#
# Returns :     1 if validation ok, 0 if not
#
# Side-Effects :
#       ::errorCode - cleared if validation ok
#                   - contains validation failure information if validation
#                       failed.
#
# Exception Conditions :
#       WS CHECK START_NODE_DIFFERS - Start node not what was expected
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/14/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to handle qualified XML
#
#
###########################################################################
proc ::WS::Utils::Validate {mode serviceName xmlString tagName typeName} {

    set first [string first {<} $xmlString]
    if {$first > 0} {
        set xmlString [string range $xmlString $first end]
    }
    dom parse $xmlString resultTree
    $resultTree documentElement currNode
    set nodeName [$currNode localName]
    if {![string equal $nodeName $tagName]} {
        return \
            -code error \
            -errorcode [list WS CHECK START_NODE_DIFFERS [list $tagName $nodeName]] \
            "start node differs expected: $tagName found: $nodeName"
    }
    set ::errorCode {}
    set result [checkTags $mode $serviceName $currNode $typeName]
    $resultTree delete
    return $result
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::BuildRequest
#
# Description : Given a schema check the body of a request handed in
#               as a XML string using a XML schema description (in WS:: form)
#               for validation
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       tagName     - The name of the starting tag
#       typeName    - The type for the tag
#       valueInfos  - The dictionary of the values
#
# Returns :     The body of the request as xml
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PUBLIC<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/13/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to generate qualified XML
#
###########################################################################
proc ::WS::Utils::BuildRequest {mode serviceName tagName typeName valueInfos} {
    upvar 1 $valueInfos values
    variable resultTree
    variable currNode

    set resultTree [::dom createDocument $tagName]
    set typeInfo [GetServiceTypeDef $mode $serviceName $typeName]
    $resultTree documentElement currNode
    if {[catch {buildTags $mode $serviceName $typeName $valueInfos $resultTree $currNode} msg]} {
        set tmpErrorCode $::errorCode
        set tmpErrorInfo $::errorInfo
        $resultTree delete
        return \
            -code error \
            -errorcode $tmpErrorCode \
            -errorinfo $tmpErrorInfo \
            $msg
    }
    set xml [$resultTree asXML]
    $resultTree delete
    return $xml
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Server::GenerateXsd
#
# Description : Generate a XSD.  NOTE -- does not write a file
#
# Arguments :
#       mode            - Client/Server
#       serviceName     - The service name
#       targetNamespace - Target namespace
#
# Returns :     XML of XSD
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    Service must exists
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
#       1  02/03/2008  G.Lester     Initial Version
#
###########################################################################
proc ::WS::Utils::GenerateXsd {mode serviceName targetNamespace} {
    set reply [::dom createDocument definitions]
    $reply documentElement definition

    GenerateScheme $mode $serviceName $reply {} $targetNamespace

    append msg \
        {<?xml version="1.0"  encoding="utf-8"?>} \
        "\n" \
        [$reply asXML  -indent 4 -escapeNonASCII -doctypeDeclaration 0]
    $reply delete
    return $msg
}

###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::Utils::GenerateScheme
#
# Description : Generate a scheme
#
# Arguments :
#       mode            - Client/Server
#       serviceName     - The service name
#       doc             - The document to add the scheme to
#       parent          - The parent node of the scheme
#       targetNamespace - Target namespace
#       version         - Requested service version
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
#       3  11/13/2018  J.Cone       Version support
#
###########################################################################
proc ::WS::Utils::GenerateScheme {mode serviceName doc parent targetNamespace {version {}}} {

    set localTypeInfo [GetServiceTypeDef $mode $serviceName]
    array set typeArr {}
    foreach type [dict keys $localTypeInfo] {
        set typeArr($type) 1
    }
    if {[string equal $parent {}]} {
        $doc documentElement schema
        $schema setAttribute \
            xmlns:xs         "http://www.w3.org/2001/XMLSchema"
    } else {
        $parent appendChild [$doc createElement xs:schema schema]
    }
    $schema setAttribute \
        elementFormDefault qualified \
        targetNamespace $targetNamespace

    foreach baseType [lsort -dictionary [array names typeArr]] {
        if {$version ne {} && [dict exists $localTypeInfo $baseType version]} {
            if {![check_version [dict get $localTypeInfo $baseType version] $version]} {
                continue
            }
        }
        ::log::logsubst debug {Outputing $baseType}
        $schema appendChild [$doc createElement xs:element elem]
        set name [lindex [split $baseType {:}] end]
        $elem setAttribute name $name
        $elem setAttribute type $baseType
        $schema appendChild [$doc createElement xs:complexType comp]
        $comp setAttribute name $name
        $comp appendChild [$doc createElement xs:sequence seq]
        set baseTypeInfo [dict get $localTypeInfo $baseType definition]
        ::log::logsubst debug {\t parts {$baseTypeInfo}}
        foreach {field tmpTypeInfo} $baseTypeInfo {
            if {$version ne {} && [dict exists $tmpTypeInfo version]} {
                if {![check_version [dict get $tmpTypeInfo version] $version]} {
                    continue
                }
            }
            $seq appendChild  [$doc createElement xs:element tmp]
            set tmpType [dict get $tmpTypeInfo type]
            ::log::logsubst debug {Field $field of $tmpType}
            foreach {name value} [getTypeWSDLInfo $mode $serviceName $field $tmpType] {
                $tmp setAttribute $name $value
            }
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
# Procedure Name : ::WS::Server::getTypeWSDLInfo
#
# Description : Return full type information usable for a WSDL
#
# Arguments :
#     mode        - Client/Server
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
#       2  02/03/2008  G.Lester     Moved  into WS::Utils namespace
#
###########################################################################
proc ::WS::Utils::getTypeWSDLInfo {mode serviceName field type} {
    set typeInfo {maxOccurs 1 minOccurs 1 name * type *}
    dict set typeInfo name $field
    set typeList [TypeInfo $mode $serviceName $type 1]
    if {[lindex $typeList 0] == 0} {
        dict set typeInfo type xs:[string trimright $type {()?}]
    } else {
        dict set typeInfo type $serviceName:[string trimright $type {()?}]
    }
    if {[lindex $typeList 1]} {
        dict set typeInfo maxOccurs unbounded
    }
    if {[lindex $typeList 2]} {
        dict set typeInfo minOccurs 0
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
# Procedure Name : ::WS::Utils::convertTypeToDict
#
# Description : Convert the XML, in DOM representation, to a dictionary object for
#               a given type.
#
# Arguments :
#    mode        - The mode, Client or Server
#    serviceName - The service name the type is defined in
#    node        - The base node for the type.
#    type        - The name of the type
#    root        - The root node of the document
#    isArray     - We are looking for array elements
#
# Returns : A dictionary object for a given type.
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
# 2.4.2    2018-05-14  H.Oehlmann   Add support to translate namespace prefixes
#                                   in attribute values or text values.
#                                   New parameter "xnsDistantToLocalDict".
#
###########################################################################
proc ::WS::Utils::convertTypeToDict {
        mode serviceName node type root {isArray 0} {xnsDistantToLocalDict {}}
} {
    variable typeInfo
    variable mutableTypeInfo
    variable options

    if {$options(valueAttrCompatiblityMode)} {
        set valueAttr {}
    } else {
        set valueAttr {::value}
    }
    set xsiNsUrl {http://www.w3.org/2001/XMLSchema-instance}
    ::log::logsubst debug {Entering [info level 0]}
    if {[dict exists $typeInfo $mode $serviceName $type]} {
        set typeName $type
    } elseif {[dict exists $typeInfo $mode $serviceName $serviceName:$type]} {
        set typeName $serviceName:$type
    } else {
        ##
        ## Assume this is a simple type
        ##
        set baseType [::WS::Utils::GetServiceTypeDef $mode $serviceName $type]
        if {[string equal $baseType {XML}]} {
            set results [$node asXML]
        } else {
            set results [$node asText]
        }
        return $results
    }
    set typeDefInfo [dict get $typeInfo $mode $serviceName $typeName]
    ::log::logsubst debug {\t type def = {$typeDefInfo}}
    set xns [dict get $typeDefInfo xns]
    if {[$node hasAttribute href]} {
        set node [GetReferenceNode $root [$node getAttribute href]]
    }
    ::log::logsubst debug {\t XML of node is [$node asXML]}
    if {[info exists mutableTypeInfo([list $mode $serviceName $typeName])]} {
        set type [(*)[lindex mutableTypeInfo([list $mode $serviceName $type]) 0] $mode $serviceName $typeName $xns $node]
        set typeDefInfo [dict get $typeInfo $mode $serviceName $typeName]
        ::log::logsubst debug {\t type def replaced with = {$typeDefInfo}}
    }
    set results {}
    #if {$options(parseInAttr)} {
    #    foreach attr [$node attributes] {
    #        if {[llength $attr] == 1} {
    #            dict set results $attr [$node getAttribute $attr]
    #        }
    #    }
    #}
    set partsList [dict keys [dict get $typeDefInfo definition]]
    ::log::logsubst debug {\t partsList is {$partsList}}
    set arrayOverride [expr {$isArray && ([llength $partsList] == 1)}]
    foreach partName $partsList {
        set partType [dict get $typeDefInfo definition $partName type]
        set partType [string trimright $partType {?}]
        if {[dict exists $typeDefInfo definition $partName allowAny] && [dict get $typeDefInfo definition $partName allowAny]} {
            set allowAny 1
        } else {
            set allowAny 0
        }
        if {[string equal $partName *] && [string equal $partType *]} {
            ##
            ## Type infomation being handled dynamically for this part
            ##
            set savedTypeInfo $typeInfo
            parseDynamicType $mode $serviceName $node $type
            set tmp [convertTypeToDict $mode $serviceName $node $type $root 0 $xnsDistantToLocalDict]
            foreach partName [dict keys $tmp] {
                dict set results $partName [dict get $tmp $partName]
            }
            set typeInfo $savedTypeInfo
            continue
        }
        set partXns $xns
        catch {set partXns  [dict get $typeInfo $mode $serviceName $partType xns]}
        set typeInfoList [TypeInfo $mode $serviceName $partType]
        set tmpTypeInfo [::WS::Utils::GetServiceTypeDef $mode $serviceName $partType]
        ::log::logsubst debug {\tpartName $partName partType $partType xns $xns typeInfoList $typeInfoList}
        ##
        ## Try for fully qualified name
        ##
        ::log::logsubst debug {Trying #1 [list $node selectNodes $partXns:$partName]}
        if {[catch {llength [set item [$node selectNodes $partXns:$partName]]} len] || ($len == 0)} {
            ::log::logsubst debug {Trying #2 [list $node selectNodes $xns:$partName]}
            if {[catch {llength [set item [$node selectNodes $xns:$partName]]} len] || ($len == 0)} {
                ##
                ## Try for unqualified name
                ##
                ::log::logsubst debug {Trying #3 [list $node selectNodes $partName]}
                if {[catch {llength [set item [$node selectNodes $partName]]} len] || ($len == 0)} {
                    ::log::log debug "Trying #4 -- search of children"
                    set item {}
                    set matchList [list $partXns:$partName  $xns:$partName $partName]
                    foreach childNode [$node childNodes] {
                        set nodeType [$childNode nodeType]
                        ::log::logsubst debug {\t\t Looking at {[$childNode localName],[$childNode nodeName]} ($allowAny,$isArray,$nodeType,$partName)}
                        # From SOAP1.1 Spec:
                        #    Within an array value, element names are not significant
                        # for distinguishing accessors. Elements may have any name.
                        # Here we don't need check the element name, just simple check
                        # it's a element node
                        if {$allowAny  || ($arrayOverride && [string equal $nodeType "ELEMENT_NODE"])} {
                            ::log::logsubst debug {\t\t Found $partName [$childNode asXML]}
                            lappend item $childNode
                        }
                    }
                    if {![string length $item]} {
                        ::log::log debug "\tSkipping"
                        continue
                    }
                } else {
                    ::log::logsubst debug {\t\t Found [llength $item] $partName}
                }
            } else {
                ::log::logsubst debug {\t\t Found [llength $item] $partName}
            }
        } else {
            ::log::logsubst debug {\t\t Found [llength $item] $partName}
        }
        set origItemList $item
        set newItemList {}
        foreach item $origItemList {
            if {[$item hasAttribute href]} {
                set oldXML [$item asXML]
                ::log::logsubst debug {\t\t Replacing: $oldXML}
                set item [GetReferenceNode $root [$item getAttribute href]]
                ::log::logsubst debug {\t\t With: [$item asXML]}
            }
            lappend newItemList $item
        }
        set item $newItemList
        set isAbstract false
        if {[dict exists $typeInfo $mode $serviceName $partType abstract]} {
            set isAbstract [dict get $typeInfo $mode $serviceName $partType abstract]
        }
        switch -exact -- $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                if {[dict exists $tmpTypeInfo base]} {
                    set baseType [dict get $tmpTypeInfo base]
                } else {
                    set baseType string
                }
                if {$options(parseInAttr)} {
                    foreach attrList [$item attributes] {
                        catch {
                            lassign $attrList attr nsAlias nsUrl
                            if {[string equal $nsUrl $xsiNsUrl]} {
                                set attrValue [$item getAttribute ${nsAlias}:$attr]
                                dict set results $partName ::$attr $attrValue
                            } elseif {![string equal $nsAlias {}]} {
                                set attrValue [$item getAttribute ${nsAlias}:$attr]
                                dict set results $partName $attr $attrValue
                            } else {
                                set attrValue [$item getAttribute $attr]
                                dict set results $partName $attr $attrValue
                            }
                        }
                    }
                    if {[string equal $baseType {XML}]} {
                        dict set results $partName $valueAttr [$item asXML]
                    } else {
                        dict set results $partName $valueAttr [$item asText]
                    }
                } else {
                    if {[string equal $baseType {XML}]} {
                        dict set results $partName [$item asXML]
                    } else {
                        dict set results $partName [$item asText]
                    }
                }
            }
            {0 1} {
                ##
                ## Simple array
                ##
                if {[dict exists $tmpTypeInfo base]} {
                    set baseType [dict get $tmpTypeInfo base]
                } else {
                    set baseType string
                }
                set tmp {}
                foreach row $item {
                    if {$options(parseInAttr)} {
                        set rowList {}
                        foreach attrList [$row attributes] {
                            catch {
                                lassign $attrList attr nsAlias nsUrl
                                if {[string equal $nsUrl $xsiNsUrl]} {
                                    set attrValue [$row getAttribute ${nsAlias}:$attr]
                                    lappend rowList ::$attr $attrValue
                                } elseif {![string equal $nsAlias {}]} {
                                    set attrValue [$row getAttribute ${nsAlias}:$attr]
                                    lappend rowList $attr $attrValue
                                } else {
                                    set attrValue [$row getAttribute $attr]
                                    lappend rowList $attr $attrValue
                                }
                            }
                        }
                        if {[string equal $baseType {XML}]} {
                            lappend rowList $valueAttr [$row asXML]
                        } else {
                            lappend rowList $valueAttr [$row asText]
                        }
                        lappend tmp $rowList
                    } else {
                        if {[string equal $baseType {XML}]} {
                            lappend tmp [$row asXML]
                        } else {
                            lappend tmp [$row asText]
                        }
                    }
                }
                dict set results $partName $tmp
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                if {$options(parseInAttr)} {
                    ## Translate an abstract type from the WSDL to a type given
                    ## in the response
                    ## Example xml response from bug 584bfb772:
                    ## <soap:Envelope ...
                    ##    xmlns:tns="http://www.esri.com/schemas/ArcGIS/10.3">
                    ##  <soap:Body>
                    ##    <tns:GetServerInfoResponse>
                    ##      <Result xsi:type="tns:MapServerInfo">
                    ##      <Name>Layers</Name>
                    ##      <Description></Description>
                    ##        <FullExtent xsi:type="tns:EnvelopeN">
                    ##
                    ## The element FullExtend gets type "tns:EnvelopeN".
                    ##
                    ## xnsDistantToLocalDict
                    if {$isAbstract && [$item hasAttributeNS {http://www.w3.org/2001/XMLSchema-instance} type]} {
                        # partType is now tns::EnvelopeN
                        set partType [XNSDistantToLocal $xnsDistantToLocalDict \
                                [$item getAttributeNS {http://www.w3.org/2001/XMLSchema-instance} type]]

                        # Remove this type attribute from the snippet.
                        # So, it is not handled in the loop below.
                        $item removeAttributeNS {http://www.w3.org/2001/XMLSchema-instance} type
                    }
                    foreach attrList [$item attributes] {
                        catch {
                            lassign $attrList attr nsAlias nsUrl
                            if {[string equal $nsUrl $xsiNsUrl]} {
                                set attrValue [$item getAttribute ${nsAlias}:$attr]
                                dict set results $partName ::$attr $attrValue
                            } elseif {![string equal $nsAlias {}]} {
                                set attrValue [$item getAttribute ${nsAlias}:$attr]
                                dict set results $partName $attr $attrValue
                            } else {
                                set attrValue [$item getAttribute $attr]
                                dict set results $partName $attr $attrValue
                            }
                        }
                    }
                    dict set results $partName $valueAttr [convertTypeToDict $mode $serviceName $item $partType $root 0 $xnsDistantToLocalDict]
                } else {
                    dict set results $partName [convertTypeToDict $mode $serviceName $item $partType $root  0 $xnsDistantToLocalDict]
                }
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set partType [string trimright $partType {()}]
                set tmp [list]
                foreach row $item {
                    if {$options(parseInAttr)} {
                        set rowList {}
                        if {$isAbstract && [$row hasAttributeNS {http://www.w3.org/2001/XMLSchema-instance} type]} {
                            set partType [$row getAttributeNS {http://www.w3.org/2001/XMLSchema-instance} type]
                            $row removeAttributeNS {http://www.w3.org/2001/XMLSchema-instance} type
                        }
                        foreach attrList [$row attributes] {
                            catch {
                                lassign $attrList attr nsAlias nsUrl
                                if {[string equal $nsUrl $xsiNsUrl]} {
                                    set attrValue [$row getAttribute ${nsAlias}:$attr]
                                    lappend rowList ::$attr $attrValue
                                } elseif {![string equal $nsAlias {}]} {
                                    set attrValue [$row getAttribute ${nsAlias}:$attr]
                                    lappend rowList $attr $attrValue
                                } else {
                                    set attrValue [$row getAttribute $attr]
                                    lappend rowList $attr $attrValue
                                }
                            }
                        }
                        lappend rowList $valueAttr [convertTypeToDict $mode $serviceName $row $partType $root 1 $xnsDistantToLocalDict]
                        lappend tmp $rowList
                    } else {
                        lappend tmp [convertTypeToDict $mode $serviceName $row $partType $root 1 $xnsDistantToLocalDict]
                    }
                }
                dict set results $partName $tmp
            }
            default {
                ##
                ## Placed here to shut up tclchecker
                ##
            }
        }
    }
    ::log::logsubst debug {Leaving ::WS::Utils::convertTypeToDict with result '$results'}
    return $results
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::XNSDistantToLocal
#
# Description : Get a reference node.
#
# Arguments :
#    xnsDistantToLocalDict - Dict to translate distant to local NS prefixes
#    typeDistant - Type string with possible distant namespace prefix
#
# Returns : type with local namespace prefix
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
# 2.4.2    2017-11-03  H.Oehlmann   Initial version
#
###########################################################################
proc ::WS::Utils::XNSDistantToLocal {xnsDistantToLocalDict type} {
    set collonPos [string first ":" $type]
    # check for namespace prefix present
    if {-1 < $collonPos} {
        set prefixDistant [string range $type 0 $collonPos-1]
        if {[dict exists $xnsDistantToLocalDict $prefixDistant]} {
            set type [dict get $xnsDistantToLocalDict $prefixDistant][string range $type $collonPos end]
            log::logsubst debug {Mapped distant namespace prefix '$prefixDistant' to type '$type'}
        } else {
            log::logsubst warning {Distant type '$type' does not have a known namespace prefix ([dict keys $xnsDistantToLocalDict])}
        }
    }
    return $type
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::GetReferenceNode
#
# Description : Get a reference node.
#
# Arguments :
#    root        - The root node of the document
#    root        - The root node of the document
#
# Returns : A node object.
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
#       1  08/19/2008  G.Lester     Initial version
#
#
###########################################################################
proc ::WS::Utils::GetReferenceNode {root id} {
    set id [string trimleft $id {#}]
    set node [$root selectNodes -cache yes [format {//*[@id="%s"]} $id]]
    if {[$node hasAttribute href]} {
        set node [GetReferenceNode $root [$node getAttribute href]]
    }
    return $node
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::convertDictToType
#
# Description : Convert a dictionary object into a XML DOM tree.
#
# Arguments :
#    mode        - The mode, Client or Server
#    service     - The service name the type is defined in
#    parent      - The parent node of the type.
#    doc         - The document
#    dict        - The dictionary to convert
#    type        - The name of the type
#    forceNs     - Force the response to use a namespace
#    enforceRequired - Boolean setting for enforcing required vars be set
#    version     - Requested service version
#
# Returns : None
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
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Utils::convertDictToType {mode service doc parent dict type {forceNs 0} {enforceRequired 0} {version {}}} {
    ::log::logsubst debug {Entering [info level 0]}
    # ::log::logsubst debug {  Parent xml: [$parent asXML]}
    variable typeInfo
    variable simpleTypes
    variable options
    variable standardAttributes
    variable currentNs

    if {!$options(UseNS)} {
        return [::WS::Utils::convertDictToTypeNoNs $mode $service $doc $parent $dict $type $enforceRequired $version]
    }

    if {$options(valueAttrCompatiblityMode)} {
        set valueAttr {}
    } else {
        set valueAttr {::value}
    }
    set typeInfoList [TypeInfo $mode $service $type]
    set type [string trimright $type {?}]
    ::log::logsubst debug {\t typeInfoList = {$typeInfoList}}
    if {[dict exists $typeInfo $mode $service $service:$type]} {
        set typeName $service:$type
    } else {
        set typeName $type
    }
    if {$version ne {} && [dict exists $typeInfo $mode $service $typeName version]} {
        if {![check_version [dict get $typeInfo $mode $service $typeName version] $version]} {
            return
        }
    }
    set itemList {}
    if {[lindex $typeInfoList 0] && [dict exists $typeInfo $mode $service $typeName definition]} {
        set itemList [dict get $typeInfo $mode $service $typeName definition]
        set xns [dict get $typeInfo $mode $service $typeName xns]
    } else {
        if {[info exists simpleTypes($mode,$service,$typeName)]} {
          set xns [dict get $simpleTypes($mode,$service,$typeName) xns]
        } elseif {[info exists simpleTypes($mode,$service,$currentNs:$typeName)]} {
          set xns [dict get $simpleTypes($mode,$service,$currentNs:$typeName) xns]
        } else {
          error "Simple type cannot be found: $typeName"
        }
        set itemList [list $typeName {type string}]
    }
    if {[info exists mutableTypeInfo([list $mode $service $typeName])]} {
        set typeName [(*)[lindex mutableTypeInfo([list $mode $service $type]) 0] $mode $service $type $xns $dict]
        set typeInfoList [TypeInfo $mode $service $typeName]
        if {[lindex $typeInfoList 0]} {
            set itemList [dict get $typeInfo $mode $service $typeName definition]
            set xns [dict get $typeInfo $mode $service $typeName xns]
        } else {
            if {[info exists simpleTypes($mode,$service,$typeName)]} {
              set xns [dict get $simpleTypes($mode,$service,$typeName) xns]
            } elseif {[info exists simpleTypes($mode,$service,$currentNs:$typeName)]} {
              set xns [dict get $simpleTypes($mode,$service,$currentNs:$typeName) xns]
            } else {
              error "Simple type cannot be found: $typeName"
            }
            set itemList [list $type {type string}]
        }
    }
    ::log::logsubst debug {\titemList is {$itemList} in $xns}
    set entryNs $currentNs
    if {!$forceNs} {
        set currentNs $xns
    }
    set fieldList {}
    foreach {itemName itemDef} $itemList {
        if {[dict exists $itemDef version] && ![check_version [dict get $itemDef version] $version]} {
            continue
        }
        set baseName [lindex [split $itemName {:}] end]
        lappend fieldList $itemName
        set itemType [dict get $itemDef type]
        ::log::logsubst debug {\t\titemName = {$itemName} itemDef = {$itemDef} itemType ={$itemType}}
        set typeInfoList [TypeInfo $mode $service $itemType 1]
        ::log::logsubst debug {Expr [list ![dict exists $dict $itemName] && ![dict exists $dict $baseName]]}
        if {![dict exists $dict $itemName] && ![dict exists $dict $baseName]} {
            ::log::logsubst debug {Neither {$itemName} nor {$baseName} are in dictionary {$dict}, skipping}
            # If required parameters are being enforced and this field is not optional, throw an error
            if {$enforceRequired && ![lindex $typeInfoList 2]} {
                error "Required field $itemName is missing from response"
            }
            continue
        } elseif {[dict exists $dict $baseName]} {
            set useName $baseName
        } else {
            set useName $itemName
        }
        set itemXns $xns
        set tmpInfo [GetServiceTypeDef $mode $service [string trimright $itemType {()?}]]
        if {$options(useTypeNs) && [dict exists $tmpInfo xns]} {
            set itemXns [dict get $tmpInfo xns]
        }
        set attrList {}
        if {$options(useTypeNs) && [string equal $itemXns xs]} {
            set itemXns $xns
        }
        if {$options(nsOnChangeOnly) && [string equal $itemXns $currentNs]} {
            set itemXns {}
        }
        foreach key [dict keys $itemDef] {
            if {[lsearch -exact $standardAttributes $key] == -1 && $key ne "isList" && $key ne "xns"} {
                lappend attrList $key [dict get $itemDef $key]
                ::log::logsubst debug {key = {$key} standardAttributes = {$standardAttributes}}
            }
        }
        ::log::logsubst debug {\t\titemName = {$itemName} itemDef = {$itemDef} typeInfoList = {$typeInfoList} itemXns = {$itemXns} tmpInfo = {$tmpInfo} attrList = {$attrList}}
        set isAbstract false
        set baseType [string trimright $itemType {()?}]
        if {$options(genOutAttr) && [dict exists $typeInfo $mode $service $baseType abstract]} {
            set isAbstract [dict get $typeInfo $mode $service $baseType abstract]
        }
        ::log::logsubst debug {\t\titemName = {$itemName} itemDef = {$itemDef} typeInfoList = {$typeInfoList} isAbstract = {$isAbstract}}
        # Strip the optional flag off the typeInfoList
        set typeInfoList [lrange $typeInfoList 0 1]
        switch -exact -- $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                if {[string equal $itemXns $options(suppressNS)] || [string equal $itemXns {}]} {
                    $parent appendChild [$doc createElement $itemName retNode]
                } else {
                    $parent appendChild [$doc createElement $itemXns:$itemName retNode]
                }
                if {$options(genOutAttr)} {
                    set resultValue {}
                    set dictList [dict keys [dict get $dict $useName]]
                    #::log::log debug "$useName <$dict> '$dictList'"
                    foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                        if {[string equal $attr $valueAttr]} {
                            set resultValue [dict get $dict $useName $attr]
                        } elseif {[string match {::*} $attr]} {
                            set baseAttr [string range $attr 2 end]
                            set attrValue [dict get $dict $useName $attr]
                            $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                        } else {
                            lappend attrList $attr [dict get $dict $useName $attr]
                        }
                    }
                } else {
                    set resultValue [dict get $dict $useName]
                }
                if {[dict exists $tmpInfo base] && [string equal [dict get $tmpInfo base] {XML}]} {
                    $retNode appendXML $resultValue
                } else {
                    $retNode appendChild [$doc createTextNode $resultValue]
                }
                if {[llength $attrList]} {
                    ::WS::Utils::setAttr $retNode $attrList
                }
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set dataList [dict get $dict $useName]
                #::log::log debug "\t\t [llength $dataList] rows {$dataList}"
                foreach row $dataList {
                    if {[string equal $itemXns $options(suppressNS)] || [string equal $itemXns {}]} {
                        $parent appendChild [$doc createElement $itemName retNode]
                    } else {
                        $parent appendChild [$doc createElement $itemXns:$itemName retNode]
                    }
                    if {$options(genOutAttr)} {
                        set dictList [dict keys $row]
                        ::log::logsubst debug {<$row> '$dictList'}
                        set resultValue {}
                        foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                            if {[string equal $attr $valueAttr]} {
                                set resultValue [dict get $row $attr]
                            } elseif {[string match {::*} $attr]} {
                                set baseAttr [string range $attr 2 end]
                                set attrValue [dict get $row $attr]
                                $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                            } else {
                                lappend attrList $attr [dict get $row $attr]
                            }
                        }
                    } else {
                        set resultValue $row
                    }
                    if {[dict exists $tmpInfo base] && [string equal [dict get $tmpInfo base] {XML}]} {
                        $retNode appendXML $resultValue
                    } else {
                        $retNode appendChild [$doc createTextNode $resultValue]
                    }
                    if {[llength $attrList]} {
                        ::WS::Utils::setAttr $retNode $attrList
                    }
                }
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                if {[string equal $itemXns $options(suppressNS)] || [string equal $itemXns {}]} {
                    $parent appendChild [$doc createElement $itemName retNode]
                } else {
                    $parent appendChild [$doc createElement $itemXns:$itemName retNode]
                }
                if {$options(genOutAttr)} {
                    #::log::log debug "Before 150 useName {$useName} dict {$dict}"
                    set dictList [dict keys [dict get $dict $useName]]
                    #::log::log debug "$useName <$dict> '$dictList'"
                    set resultValue {}
                    foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                        if {$isAbstract && [string equal $attr {::type}]} {
                            set itemType [dict get $dict $useName $attr]
                            $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:type $itemType
                            set itemType $itemXns:$itemType
                        } elseif {[string equal $attr $valueAttr]} {
                            set resultValue [dict get $dict $useName $attr]
                        } elseif {[string match {::*} $attr]} {
                            set baseAttr [string range $attr 2 end]
                            set attrValue [dict get $dict $useName $attr]
                            $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                        } else {
                            lappend attrList $attr [dict get $dict $useName $attr]
                        }
                    }
                } else {
                    set resultValue [dict get $dict $useName]
                }
                if {![string equal $currentNs $itemXns] && ![string equal $itemXns {}]} {
                    set tmpNs $currentNs
                    set currentNs $itemXns
                    convertDictToType $mode $service $doc $retNode $resultValue $itemType $forceNs $enforceRequired $version
                } else {
                    convertDictToType $mode $service $doc $retNode $resultValue $itemType $forceNs $enforceRequired $version
                }
                if {[llength $attrList]} {
                    ::WS::Utils::setAttr $retNode $attrList
                }
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set dataList [dict get $dict $useName]
                #::log::log debug "\t\t [llength $dataList] rows {$dataList}"
                foreach row $dataList {
                    if {[string equal $itemXns $options(suppressNS)] || [string equal $itemXns {}]} {
                        $parent appendChild [$doc createElement $itemName retNode]
                    } else {
                        $parent appendChild [$doc createElement $itemXns:$itemName retNode]
                    }
                    if {$options(genOutAttr)} {
                        set dictList [dict keys $row]
                        set resultValue {}
                        #::log::log debug "<$row> '$dictList'"
                        foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                            if {$isAbstract && [string equal $attr {::type}]} {
                                set tmpType [dict get $row $attr]
                                $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:type $tmpType
                                set tmpType $itemXns:$tmpType
                            } elseif {[string equal $attr $valueAttr]} {
                                set resultValue [dict get $row $attr]
                            } elseif {[string match {::*} $attr]} {
                                set baseAttr [string range $attr 2 end]
                                set attrValue [dict get $row $attr]
                                $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                            } else {
                                lappend attrList $attr [dict get $row $attr]
                            }
                        }
                    } else {
                        set resultValue $row
                    }
                    if {[string index $itemType end] eq {?}} {
                        set tmpType "[string trimright $itemType {()?}]?"
                    } else {
                        set tmpType [string trimright $itemType {()}]
                    }
                    if {![string equal $currentNs $itemXns] && ![string equal $itemXns {}]} {
                        set tmpNs $currentNs
                        set currentNs $itemXns
                        convertDictToType $mode $service $doc $retNode $resultValue $tmpType $forceNs $enforceRequired $version
                    } else {
                        convertDictToType $mode $service $doc $retNode $resultValue $tmpType $forceNs $enforceRequired $version
                    }
                    if {[llength $attrList]} {
                        ::WS::Utils::setAttr $retNode $attrList
                    }
                }
            }
            default {
                ##
                ## Placed here to shut up tclchecker
                ##
            }
        }
        #if {$options(genOutAttr)} {
        #    set dictList [dict keys $dict]
        #    foreach attr [lindex [::struct::set intersect3 $fieldList $dictList] end] {
        #        $parent setAttribute $attr [dict get $dict $attr]
        #    }
        #}
    }
    set currentNs $entryNs
    ::log::logsubst debug {Leaving ::WS::Utils::convertDictToType with xml: [$parent asXML]}
    return
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::convertDictToJson
#
# Description : Convert a dictionary object into a JSON tree.
#
# Arguments :
#    mode        - The mode, Client or Server
#    service     - The service name the type is defined in
#    doc         - The document (yajltcl)
#    dict        - The dictionary to convert
#    type        - The name of the type
#    enforceRequired - Boolean setting for enforcing required vars be set
#    version     - The requested service version
#
# Returns : None
#
# Side-Effects : None
#
# Exception Conditions : None
#
# Pre-requisite Conditions : None
#
# Original Author : Jeff Lawson
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  03/23/2011  J.Lawson     Initial version
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Utils::convertDictToJson {mode service doc dict type {enforceRequired 0} {version {}}} {
    ::log::logsubst debug {Entering [info level 0]}
    variable typeInfo
    variable simpleTypes
    variable simpleTypesJson
    variable options
    variable standardAttributes

    set typeInfoList [TypeInfo $mode $service $type]
    set type [string trimright $type {?}]
    if {[dict exists $typeInfo $mode $service $service:$type]} {
        set typeName $service:$type
    } else {
        set typeName $type
    }
    if {$version ne {} && [dict exists $typeInfo $mode $service $typeName version]} {
        if {![check_version [dict get $typeInfo $mode $service $typeName version] $version]} {
            return
        }
    }
    set itemList {}
    if {[lindex $typeInfoList 0] && [dict exists $typeInfo $mode $service $typeName definition]} {
        set itemList [dict get $typeInfo $mode $service $typeName definition]
        set xns [dict get $typeInfo $mode $service $typeName xns]
    } else {
        set xns $simpleTypes($mode,$service,$typeName)
        set itemList [list $typeName {type string}]
    }
    if {[info exists mutableTypeInfo([list $mode $service $typeName])]} {
        set typeName [(*)[lindex mutableTypeInfo([list $mode $service $type]) 0] $mode $service $type $xns $dict]
        set typeInfoList [TypeInfo $mode $service $typeName]
        if {[lindex $typeInfoList 0]} {
            set itemList [dict get $typeInfo $mode $service $typeName definition]
        } else {
            set itemList [list $type {type string}]
        }
    }
    ::log::logsubst debug {\titemList is {$itemList}}
    set fieldList {}
    foreach {itemName itemDef} $itemList {
        if {[dict exists $itemDef version] && ![check_version [dict get $itemDef version] $version]} {
            continue
        }
        lappend fieldList $itemName
        set itemType [dict get $itemDef type]
        ::log::logsubst debug {\t\titemName = {$itemName} itemDef = {$itemDef} itemType = {$itemType}}
        set typeInfoList [TypeInfo $mode $service $itemType 1]
        if {![dict exists $dict $itemName]} {
            if {$enforceRequired && ![lindex $typeInfoList 2]} {
                error "Required field $itemName is missing from response"
            }
            continue
        }

        if {[info exists simpleTypesJson([string trimright $itemType {()?}])]} {
            set yajlType $simpleTypesJson([string trimright $itemType {()?}])
        } else {
            set yajlType "string"
        }

        ::log::logsubst debug {\t\titemName = {$itemName} itemDef = {$itemDef} typeInfoList = {$typeInfoList}}
        set typeInfoList [lrange $typeInfoList 0 1]
        switch -- $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                set resultValue [dict get $dict $itemName]
                $doc string $itemName $yajlType $resultValue
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set dataList [dict get $dict $itemName]
                $doc string $itemName array_open
                foreach row $dataList {
                    $doc $yajlType $row
                }
                $doc array_close
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                $doc string $itemName map_open
                set resultValue [dict get $dict $itemName]
                convertDictToJson $mode $service $doc $resultValue $itemType $enforceRequired $version
                $doc map_close
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set dataList [dict get $dict $itemName]
                $doc string $itemName array_open
                if {[string index $itemType end] eq {?}} {
                    set tmpType "[string trimright $itemType {()?}]?"
                } else {
                    set tmpType [string trimright $itemType {()}]
                }
                foreach row $dataList {
                    $doc map_open
                    convertDictToJson $mode $service $doc $row $tmpType $enforceRequired $version
                    $doc map_close
                }
                $doc array_close
            }
        }
    }
    return
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::convertDictToTypeNoNs
#
# Description : Convert a dictionary object into a XML DOM tree.
#
# Arguments :
#    mode        - The mode, Client or Server
#    service     - The service name the type is defined in
#    parent      - The parent node of the type.
#    dict        - The dictionary to convert
#    type        - The name of the type
#    enforceRequired - Boolean setting for enforcing required vars be set
#    version     - The requested service version
#
# Returns : None
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
#       2  11/13/2018  J.Cone       Version support
#
#
###########################################################################
proc ::WS::Utils::convertDictToTypeNoNs {mode service doc parent dict type {enforceRequired 0} {version {}}} {
    ::log::logsubst debug {Entering [info level 0]}
    # ::log::log debug "  Parent xml: [$parent asXML]"
    variable typeInfo
    variable simpleTypes
    variable options
    variable standardAttributes
    variable currentNs

    if {$version ne {} && [dict exists $typeInfo $mode $service $type version]} {
        if {![check_version [dict get $typeInfo $mode $service $type version] $version]} {
            return
        }
    }
    if {$options(valueAttrCompatiblityMode)} {
        set valueAttr {}
    } else {
        set valueAttr {::value}
    }
    set typeInfoList [TypeInfo $mode $service $type]
    if {[lindex $typeInfoList 0]} {
        set itemList [dict get $typeInfo $mode $service $type definition]
        set xns [dict get $typeInfo $mode $service $type xns]
    } else {
        if {[info exists simpleTypes($mode,$service,$type)]} {
          set xns [dict get $simpleTypes($mode,$service,$type) xns]
        } elseif {[info exists simpleTypes($mode,$service,$currentNs:$type)]} {
          set xns [dict get $simpleTypes($mode,$service,$currentNs:$type) xns]
        } else {
          error "Simple type cannot be found: $type"
        }
        set itemList [list $type {type string}]
    }
    ::log::logsubst debug {\titemList is {$itemList}}
    foreach {itemName itemDef} $itemList {
        if {[dict exists $itemDef version] && ![check_version [dict get $itemDef version] $version]} {
            continue
        }
        ::log::logsubst debug {\t\titemName = {$itemName} itemDef = {$itemDef}}
        set itemType [dict get $itemDef type]
        set isAbstract false
        set baseType [string trimright $itemType {()?}]
        if {$options(genOutAttr) && [dict exists $typeInfo $mode $service $baseType abstract]} {
            set isAbstract [dict get $typeInfo $mode $service $baseType abstract]
        }
        set typeInfoList [TypeInfo $mode $service $itemType 1]
        if {![dict exists $dict $itemName]} {
            if {$enforceRequired && ![lindex $typeInfoList 2]} {
                error "Required field $itemName is missing from response"
            }
            continue
        }
        set attrList {}
        foreach key [dict keys $itemDef] {
            if {[lsearch -exact $standardAttributes $key] == -1 && $key ne "isList" && $key ne "xns"} {
                lappend attrList $key [dict get $itemDef $key]
                ::log::logsubst debug {key = {$key} standardAttributes = {$standardAttributes}}
            }
        }
        ::log::logsubst debug {\t\titemName = {$itemName} itemDef = {$itemDef} typeInfoList = {$typeInfoList}}
        set typeInfoList [lrange $typeInfoList 0 1]
        switch -exact -- $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                $parent appendChild [$doc createElement $itemName retNode]
                if {$options(genOutAttr)} {
                    set dictList [dict keys [dict get $dict $itemName]]
                    set resultValue {}
                    foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                        if {[string equal $attr $valueAttr]} {
                            set resultValue [dict get $dict $itemName $attr]
                        } elseif {[string match {::*} $attr]} {
                            set baseAttr [string range $attr 2 end]
                            set attrValue [dict get $dict $itemName $attr]
                            $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                        } else {
                            lappend attrList $attr [dict get $dict $itemName $attr]
                        }
                    }
                } else {
                    set resultValue [dict get $dict $itemName]
                }
                $retNode appendChild [$doc createTextNode $resultValue]
                if {[llength $attrList]} {
                    ::WS::Utils::setAttr $retNode $attrList
                }
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set dataList [dict get $dict $itemName]
                foreach row $dataList {
                    $parent appendChild [$doc createElement $itemName retNode]
                    if {$options(genOutAttr)} {
                        set dictList [dict keys $row]
                        set resultValue {}
                        foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                            if {[string equal $attr $valueAttr]} {
                                set resultValue [dict get $row $attr]
                            } elseif {[string match {::*} $attr]} {
                                set baseAttr [string range $attr 2 end]
                                set attrValue [dict get $row $attr]
                                $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                            } else {
                                lappend attrList $attr [dict get $row $attr]
                            }
                        }
                    } else {
                        set resultValue $row
                    }
                    $retNode appendChild [$doc createTextNode $resultValue]
                    if {[llength $attrList]} {
                        ::WS::Utils::setAttr $retNode $attrList
                    }
                }
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                $parent appendChild [$doc createElement $itemName retNode]
                if {$options(genOutAttr)} {
                    set dictList [dict keys [dict get $dict $itemName]]
                    set resultValue {}
                    foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                        if {$isAbstract && [string equal $attr {::type}]} {
                            # *** HaO: useName is never defined
                            set itemType [dict get $dict $useName $attr]
                            $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:type $itemType
                        } elseif {[string equal $attr $valueAttr]} {
                            set resultValue [dict get $dict $itemName $attr]
                        } elseif {[string match {::*} $attr]} {
                            set baseAttr [string range $attr 2 end]
                            set attrValue [dict get $dict $itemName $attr]
                            $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                        } else {
                            lappend attrList $attr [dict get $dict $itemName $attr]
                        }
                    }
                } else {
                    set resultValue [dict get $dict $itemName]
                }
                if {[llength $attrList]} {
                    ::WS::Utils::setAttr $retNode $attrList
                }
                convertDictToTypeNoNs $mode $service $doc $retNode $resultValue $itemType $enforceRequired $version
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set dataList [dict get $dict $itemName]
                set tmpType [string trimright $itemType {()}]
                foreach row $dataList {
                    $parent appendChild [$doc createElement $itemName retNode]
                    if {$options(genOutAttr)} {
                        set dictList [dict keys $row]
                        set resultValue {}
                        foreach attr [lindex [::struct::set intersect3 $standardAttributes $dictList] end] {
                            if {$isAbstract && [string equal $attr {::type}]} {
                                set tmpType [dict get $row $attr]
                                $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:type $tmpType
                            } elseif {[string equal $attr $valueAttr]} {
                                set resultValue [dict get $row $attr]
                            } elseif {[string match {::*} $attr]} {
                                set baseAttr [string range $attr 2 end]
                                set attrValue [dict get $row $attr]
                                $retNode setAttributeNS "http://www.w3.org/2001/XMLSchema-instance" xsi:$baseAttr $attrValue
                            } else {
                                lappend attrList $attr [dict get $row $attr]
                            }
                        }
                    } else {
                        set resultValue $row
                    }
                    if {[llength $attrList]} {
                        ::WS::Utils::setAttr $retNode $attrList
                    }
                    convertDictToTypeNoNs $mode $service $doc $retNode $resultValue $tmpType $enforceRequired $version
                }
            }
            default {
                ##
                ## Placed here to shut up tclchecker
                ##
            }
        }
    }
    # ::log::log debug "Leaving ::WS::Utils::convertDictToTypeNoNs with xml: [$parent asXML]"
    return
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::convertDictToEncodedType
#
# Description : Convert a dictionary object into a XML DOM tree with type
#               encoding.
#
# Arguments :
#    mode        - The mode, Client or Server
#    service     - The service name the type is defined in
#    parent      - The parent node of the type.
#    dict        - The dictionary to convert
#    type        - The name of the type
#
# Returns : None
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
proc ::WS::Utils::convertDictToEncodedType {mode service doc parent dict type} {
    ::log::logsubst debug {Entering [info level 0]}
    variable typeInfo
    variable options


    set typeInfoList [TypeInfo $mode $service $type]
    ::log::logsubst debug {\t typeInfoList = {$typeInfoList}}
    set type [string trimright $type {?}]
    if {[lindex $typeInfoList 0]} {
        set itemList [dict get $typeInfo $mode $service $type definition]
        set xns [dict get $typeInfo $mode $service $type xns]
    } else {
        if {[info exists simpleTypes($mode,$service,$type)]} {
          set xns [dict get $simpleTypes($mode,$service,$type) xns]
        } else {
          error "Simple type cannot be found: $type"
        }
        set itemList [list $type {type string}]
    }
    if {[info exists mutableTypeInfo([list $mode $service $type])]} {
        set type [(*)[lindex mutableTypeInfo([list $mode $service $type]) 0] $mode $service $type $xns $dict]
        set typeInfoList [TypeInfo $mode $service $type]
        if {[lindex $typeInfoList 0]} {
            set itemList [dict get $typeInfo $mode $service $type definition]
            set xns [dict get $typeInfo $mode $service $type xns]
        } else {
            if {[info exists simpleTypes($mode,$service,$type)]} {
              set xns [dict get $simpleTypes($mode,$service,$type) xns]
            } else {
              error "Simple type cannot be found: $type"
            }
            set itemList [list $type {type string}]
        }
    }
    ::log::logsubst debug {\titemList is {$itemList} in $xns}
    foreach {itemName itemDef} $itemList {
        set itemType [string trimright [dict get $itemList $itemName type] {?}]
        set typeInfoList [TypeInfo $mode $service $itemType]
        ::log::logsubst debug {\t\t Looking for {$itemName} in {$dict}}
        if {![dict exists $dict $itemName]} {
            ::log::log debug "\t\t Not found, skipping"
            continue
        }
        ::log::logsubst debug {\t\t Type info is {$typeInfoList}}
        switch -exact -- $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                if {[string equal $xns $options(suppressNS)]} {
                    $parent appendChild [$doc createElement $itemName retNode]
                } else {
                    $parent appendChild [$doc createElement $xns:$itemName retNode]
                }
                if {![string match {*:*} $itemType]} {
                    set attrType $xns:$itemType
                } else {
                    set attrType $itemType
                }
                $retNode setAttribute xsi:type $attrType
                set resultValue [dict get $dict $itemName]
                $retNode appendChild [$doc createTextNode $resultValue]
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set dataList [dict get $dict $itemName]
                set tmpType [string trimright $itemType {()}]
                if {![string match {*:*} $itemType]} {
                    set attrType $xns:$itemType
                } else {
                    set attrType $itemType
                }
                foreach resultValue $dataList {
                    if {[string equal $xns $options(suppressNS)]} {
                        $parent appendChild [$doc createElement $itemName retNode]
                    } else {
                        $parent appendChild [$doc createElement $xns:$itemName retNode]
                    }
                    $retNode setAttribute xsi:type $attrType
                    $retNode appendChild [$doc createTextNode $resultValue]
                }
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                if {[string equal $xns $options(suppressNS)]} {
                    $parent appendChild [$doc createElement $itemName retNode]
                } else {
                    $parent appendChild [$doc createElement $xns:$itemName retNode]
                }
                if {![string match {*:*} $itemType]} {
                    set attrType $xns:$itemType
                } else {
                    set attrType $itemType
                }
                $retNode setAttribute xsi:type $attrType
                convertDictToEncodedType $mode $service $doc $retNode [dict get $dict $itemName] $itemType
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set dataList [dict get $dict $itemName]
                set tmpType [string trimright $itemType {()}]
                if {![string match {*:*} $itemType]} {
                    set attrType $xns:$itemType
                } else {
                    set attrType $itemType
                }
                set attrType [string trim $attrType {()?}]
                $parent setAttribute xmlns:soapenc {http://schemas.xmlsoap.org/soap/encoding/}
                $parent setAttribute soapenc:arrayType [format {%s[%d]} $attrType [llength $dataList]]
                $parent setAttribute xsi:type soapenc:Array
                #set itemName [$parent nodeName]
                foreach item $dataList {
                    if {[string equal $xns $options(suppressNS)]} {
                        $parent appendChild [$doc createElement $itemName retNode]
                    } else {
                        $parent appendChild [$doc createElement $xns:$itemName retNode]
                    }
                    $retNode setAttribute xsi:type $attrType
                    convertDictToEncodedType $mode $service $doc $retNode $item $tmpType
                }
            }
            default {
                ##
                ## Placed here to shut up tclchecker
                ##
            }
        }
    }
    return
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseDynamicType
#
# Description : Parse the schema for a dynamically typed part.
#
# Arguments :
#    mode        - The mode, Client or Server
#    serviceName - The service name the type is defined in
#    node        - The base node for the type.
#    type        - The name of the type
#
# Returns : A dictionary object for a given type.
#
# Side-Effects : Type definitions added
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
proc ::WS::Utils::parseDynamicType {mode serviceName node type} {
    variable typeInfo
    variable nsList

    ::log::logsubst debug {Entering [info level 0]}

    foreach child [$node childNodes] {
        ::log::logsubst debug {\t Child $child is [$child nodeName]}
    }

    ##
    ## Get type being defined
    ##
    set schemeNode [$node selectNodes -namespaces $nsList xs:schema]
    set newTypeNode [$node selectNodes -namespaces $nsList  xs:schema/xs:element]
    set newTypeName [lindex [split [$newTypeNode getAttribute name] :] end]

    ##
    ## Get sibling node to scheme and add tempory type definitions
    ##
    ## type == sibing of temp type
    ## temp_type == newType of newType
    ##
    set tnsCountVar [llength [dict get $::WS::Client::serviceArr($serviceName) targetNamespace]]
    set tns tns$tnsCountVar
    set dataNode {}
    $schemeNode nextSibling dataNode
    if {![info exists dataNode] || ![string length $dataNode]} {
        $schemeNode previousSibling dataNode
    }
    set dataNodeNameList [split [$dataNode nodeName] :]
    set dataTnsName [lindex $dataNodeNameList 0]
    set dataNodeName [lindex $dataNodeNameList end]
    set tempTypeName 1_temp_type
    dict set typeInfo $mode $serviceName $tempTypeName [list  xns $tns definition [list $newTypeName [list type $newTypeName comment {}]]]
    dict set typeInfo $mode $serviceName $type [list xns $dataTnsName definition [list $dataNodeName [list type $tempTypeName comment {}]]]

    ##
    ## Parse the Scheme --gwl
    ##
    parseScheme $mode {} $schemeNode $serviceName typeInfo tnsCountVar

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
# Procedure Name : ::WS::Utils::parseScheme
#
# Description : Parse the types for a service from a Schema into
#               our internal representation
#
# Arguments :
#    mode        - The mode, Client or Server
#    SchemaNode       - The top node of the Schema
#    serviceNode    - The DOM node for the service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#    tnsCountVar -- variable name holding count of tns so far
#
# Returns : Nothing
#
# Side-Effects : Defines mode types for the service as specified by the Schema
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
proc ::WS::Utils::parseScheme {mode baseUrl schemaNode serviceName serviceInfoVar tnsCountVar} {
    ::log::logsubst debug {Entering [info level 0]}

    upvar 1 $tnsCountVar tnsCount
    upvar 1 $serviceInfoVar serviceInfo
    variable currentSchema
    variable nsList
    variable options
    variable unknownRef

    set currentSchema $schemaNode
    set tmpTargetNs $::WS::Utils::targetNs
    foreach attr [$schemaNode attributes] {
        set value {?}
        catch {set value [$schemaNode getAttribute $attr]}
        ::log::logsubst debug {Attribute $attr = $value}
    }
    if {[$schemaNode hasAttribute targetNamespace]} {
        set xns [$schemaNode getAttribute targetNamespace]
        ::log::logsubst debug {In Parse Scheme, found targetNamespace attribute with {$xns}}
        set ::WS::Utils::targetNs $xns
    } else {
        set xns $::WS::Utils::targetNs
    }
    ::log::logsubst debug {@3a {$xns} {[dict get $serviceInfo tnsList url]}}
    if {![dict exists $serviceInfo tnsList url $xns]} {
        set tns [format {tns%d} [incr tnsCount]]
        dict set serviceInfo targetNamespace $tns $xns
        dict set serviceInfo tnsList url $xns $tns
        dict set serviceInfo tnsList tns $tns $tns
    } else {
        set tns [dict get $serviceInfo tnsList url $xns]
    }
    ::log::logsubst debug {@3 TNS count for $xns is $tnsCount {$tns}}

    set prevTnsDict [dict get $serviceInfo tnsList tns]
    dict set serviceInfo tns {}
    foreach itemList [$schemaNode attributes xmlns:*] {
        set ns [lindex $itemList 0]
        set url [$schemaNode getAttribute xmlns:$ns]
        if {[dict exists $serviceInfo tnsList url $url]} {
            set tmptns [dict get $serviceInfo tnsList url $url]
        } else {
            ##
            ## Check for hardcoded namespaces
            ##
            switch -exact -- $url {
                http://schemas.xmlsoap.org/wsdl/ {
                    set tmptns w
                }
                http://schemas.xmlsoap.org/wsdl/soap/ {
                    set tmptns d
                }
                http://www.w3.org/2001/XMLSchema {
                    set tmptns xs
                }
                default {
                    set tmptns tns[incr tnsCount]
                }
            }
            dict set serviceInfo tnsList url $url $tmptns
        }
        dict set serviceInfo tnsList tns $ns $tmptns
    }

    ##
    ## Process the scheme in multiple passes to handle forward references and extensions
    ##
    set pass 1
    set lastUnknownRefCount 0
    array unset unknownRef
    while {($pass == 1) || ($lastUnknownRefCount != [array size unknownRef])} {
        ::log::logsubst debug  {Pass $pass over schema}
        incr pass
        set lastUnknownRefCount [array size unknownRef]
        array unset unknownRef

        foreach element [$schemaNode selectNodes -namespaces $nsList xs:import] {
            if {[catch {processImport $mode $baseUrl $element $serviceName serviceInfo tnsCount} msg]} {
                ::log::logsubst notice {Import failed due to: {$msg}.  Trace: $::errorInfo}
            }
        }

        foreach element [$schemaNode selectNodes -namespaces $nsList w:import] {
            if {[catch {processImport $mode $baseUrl $element $serviceName serviceInfo tnsCount} msg]} {
                ::log::logsubst notice {Import failed due to: {$msg}.  Trace: $::errorInfo}
            }
        }

        ::log::logsubst debug {Parsing Element types for $xns as $tns}
        foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:element] {
            ::log::logsubst debug {\tprocessing $element}
            if {[catch {parseElementalType $mode serviceInfo $serviceName $element $tns} msg]} {
                ::log::logsubst notice {Unhandled error: {$msg}.  Trace: $::errorInfo}
            }
        }

        ::log::logsubst debug {Parsing Attribute types for $xns as $tns}
        foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:attribute] {
            ::log::logsubst debug {\tprocessing $element}
            if {[catch {parseElementalType $mode serviceInfo $serviceName $element $tns} msg]} {
                ::log::logsubst notice {Unhandled error: {$msg}.  Trace: $::errorInfo}
            }
        }

        ::log::logsubst debug {Parsing Simple types for $xns as $tns}
        foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:simpleType] {
            ::log::logsubst debug {\tprocessing $element}
            if {[catch {parseSimpleType $mode serviceInfo $serviceName $element $tns} msg]} {
                ::log::logsubst notice {Unhandled error: {$msg}.  Trace: $::errorInfo}
            }
        }

        ::log::logsubst debug {Parsing Complex types for $xns as $tns}
        foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:complexType] {
            ::log::logsubst debug {\tprocessing $element}
            if {[catch {parseComplexType $mode serviceInfo $serviceName $element $tns} msg]} {
                ::log::logsubst notice {Unhandled error: {$msg}.  Trace: $::errorInfo}
            }
        }
    }

    set lastUnknownRefCount [array size unknownRef]
    foreach {unkRef usedByTypeList} [array get unknownRef] {
        foreach usedByType $usedByTypeList {
            switch -exact -- $options(StrictMode) {
                debug -
                warning {
                    ::log::logsubst $options(StrictMode) {Unknown type reference $unkRef in type $usedByType}
                }
                error -
                default {
                    ::log::logsubst error {Unknown type reference $unkRef in type $usedByType}
                }
            }
        }
    }

    if {$lastUnknownRefCount} {
        switch -exact -- $options(StrictMode) {
            debug -
            warning {
                set ::WS::Utils::targetNs $tmpTargetNs
                ::log::logsubst $options(StrictMode) {Found $lastUnknownRefCount forward type references: [join [array names unknownRef] {,}]}
            }
            error -
            default {
                set ::WS::Utils::targetNs $tmpTargetNs
                return \
                    -code error \
                    -errorcode [list WS $mode UNKREFS [list $lastUnknownRefCount]] \
                    "Found $lastUnknownRefCount forward type references: [join [array names unknownRef] {,}]"
            }
        }
    }



    ##
    ## Ok, one more pass to report errors
    ##
    set importNodeList [concat \
                            [$schemaNode selectNodes -namespaces $nsList xs:import] \
                            [$schemaNode selectNodes -namespaces $nsList w:import] \
    ]
    foreach element $importNodeList {
        if {[catch {processImport $mode $baseUrl $element $serviceName serviceInfo tnsCount} msg]} {
            switch -exact -- $options(StrictMode) {
                debug -
                warning {
                    ::log::logsubst $options(StrictMode) {Could not parse:\n [$element asXML]}
                    ::log::logsubst $options(StrictMode) {\t error was: $msg}
                }
                error -
                default {
                    set errorCode $::errorCode
                    set errorInfo $::errorInfo
                    ::log::logsubst error {Could not parse:\n [$element asXML]}
                    ::log::logsubst error {\t error was: $msg}
                    ::log::logsubst error {\t error info: $errorInfo}
                    ::log::logsubst error {\t error in: [lindex [info level 0] 0]}
                    ::log::logsubst error {\t error code: $errorCode}
                    set ::WS::Utils::targetNs $tmpTargetNs
                    return \
                        -code error \
                        -errorcode $errorCode \
                        -errorinfo $errorInfo \
                        $msg
                }
            }
        }
    }

    ::log::logsubst debug {Parsing Element types for $xns as $tns}
    foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:element] {
        ::log::logsubst debug {\tprocessing $element}
        if {[catch {parseElementalType $mode serviceInfo $serviceName $element $tns} msg]} {
            switch -exact -- $options(StrictMode) {
                debug -
                warning {
                    ::log::logsubst $options(StrictMode) {Could not parse:\n [$element asXML]}
                    ::log::logsubst $options(StrictMode) {\t error was: $msg}
                }
                error -
                default {
                    set errorCode $::errorCode
                    set errorInfo $::errorInfo
                    ::log::logsubst error {Could not parse:\n [$element asXML]}
                    ::log::logsubst error {\t error was: $msg}
                    ::log::logsubst error {\t error info: $errorInfo}
                    ::log::logsubst error {\t last element: $::elementName}
                    ::log::logsubst error {\t error in: [lindex [info level 0] 0]}
                    ::log::logsubst error {\t error code: $errorCode}
                    set ::WS::Utils::targetNs $tmpTargetNs
                    return \
                        -code error \
                        -errorcode $errorCode \
                        -errorinfo $errorInfo \
                        $msg
                }
            }
        }
    }

    ::log::logsubst debug {Parsing Attribute types for $xns as $tns}
    foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:attribute] {
        ::log::logsubst debug {\tprocessing $element}
        if {[catch {parseElementalType $mode serviceInfo $serviceName $element $tns} msg]} {
            switch -exact -- $options(StrictMode) {
                debug -
                warning {
                    ::log::logsubst $options(StrictMode) {Could not parse:\n [$element asXML]}
                    ::log::logsubst $options(StrictMode) {\t error was: $msg}
                }
                error -
                default {
                    set errorCode $::errorCode
                    set errorInfo $::errorInfo
                    ::log::logsubst error {Could not parse:\n [$element asXML]}
                    ::log::logsubst error {\t error was: $msg}
                    ::log::logsubst error {\t error info: $errorInfo}
                    ::log::logsubst error {\t error in: [lindex [info level 0] 0]}
                    ::log::logsubst error {\t error code: $errorCode}
                    ::log::logsubst error {\t last element: $::elementName}
                    set ::WS::Utils::targetNs $tmpTargetNs
                    return \
                        -code error \
                        -errorcode $errorCode \
                        -errorinfo $errorInfo \
                        $msg
                }
            }
        }
    }

    ::log::logsubst debug {Parsing Simple types for $xns as $tns}
    foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:simpleType] {
        ::log::logsubst debug {\tprocessing $element}
        if {[catch {parseSimpleType $mode serviceInfo $serviceName $element $tns} msg]} {
            switch -exact -- $options(StrictMode) {
                debug -
                warning {
                    ::log::logsubst $options(StrictMode) {Could not parse:\n [$element asXML]}
                    ::log::logsubst $options(StrictMode) {\t error was: $msg}
                }
                error -
                default {
                    set errorCode $::errorCode
                    set errorInfo $::errorInfo
                    ::log::logsubst error {Could not parse:\n [$element asXML]}
                    ::log::logsubst error {\t error was: $msg}
                    ::log::logsubst error {\t error info: $errorInfo}
                    ::log::logsubst error {\t error in: [lindex [info level 0] 0]}
                    ::log::logsubst error {\t error code: $errorCode}
                    set ::WS::Utils::targetNs $tmpTargetNs
                    return \
                        -code error \
                        -errorcode $errorCode \
                        -errorinfo $errorInfo \
                        $msg
                }
            }
        }
    }

    ::log::logsubst debug {Parsing Complex types for $xns as $tns}
    foreach element [$schemaNode selectNodes -namespaces $nsList child::xs:complexType] {
        ::log::logsubst debug {\tprocessing $element}
        if {[catch {parseComplexType $mode serviceInfo $serviceName $element $tns} msg]} {
            switch -exact -- $options(StrictMode) {
                debug -
                warning {
                    ::log::logsubst $options(StrictMode) {Could not parse:\n [$element asXML]}
                    ::log::logsubst $options(StrictMode) {\t error was: $msg}
                }
                error -
                default {
                    set errorCode $::errorCode
                    set errorInfo $::errorInfo
                    ::log::logsubst error {Could not parse:\n [$element asXML]}
                    ::log::logsubst error {\t error was: $msg}
                    ::log::logsubst error {\t error info: $errorInfo}
                    ::log::logsubst error {\t error in: [lindex [info level 0] 0]}
                    ::log::logsubst error {\t error code: $errorCode}
                    set ::WS::Utils::targetNs $tmpTargetNs
                    return \
                        -code error \
                        -errorcode $errorCode \
                        -errorinfo $errorInfo \
                        $msg
                }
            }
        }
    }

    set ::WS::Utils::targetNs $tmpTargetNs
    ::log::logsubst debug {Leaving :WS::Utils::parseScheme $mode $baseUrl $schemaNode $serviceName $serviceInfoVar $tnsCountVar}
    ::log::logsubst debug {Target NS is now: $::WS::Utils::targetNs}
    dict set serviceInfo tnsList tns $prevTnsDict
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::processImport
#
# Description : Parse the bindings for a service from a Schema into our
#               internal representation
#
# Arguments :
#    baseUrl        - The url of the importing node
#    importNode     - The node to import
#    serviceName    - The name service.
#    serviceInfoVar - The name of the dictionary containing the partially
#                     parsed service.
#    tnsCountVar    - The name of the variable containing the count of the
#                     namespace.
#
# Returns : Nothing
#
# Side-Effects : Defines mode types for the service as specified by the Schema
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
# 3.0.0    2020-10-26  H.Oehlmann   Added query timeout
# 3.1.0    2020-11-06  H.Oehlmann   Access namespace variable redirectArray
#                                   via variable command
#
#
###########################################################################
proc ::WS::Utils::processImport {mode baseUrl importNode serviceName serviceInfoVar tnsCountVar} {
    upvar 1 $serviceInfoVar serviceInfo
    upvar 1 $tnsCountVar tnsCount
    variable currentSchema
    variable importedXref
    variable options
	variable redirectArray

    ::log::logsubst debug {Entering [info level 0]}
    ##
    ## Get the xml
    ##
    set attrName schemaLocation
    if {![$importNode hasAttribute $attrName]} {
        set attrName namespace
        if {![$importNode hasAttribute $attrName]} {
            ::log::log debug "\t No schema location, existing"
            return \
                -code error \
                -errorcode [list WS CLIENT MISSCHLOC $baseUrl] \
                "Missing Schema Location in '$baseUrl'"
        }
    }
    set urlTail [$importNode getAttribute $attrName]
    set url [::uri::resolve $baseUrl  $urlTail]
    ::log::logsubst debug {Including $url}

    set lastPos [string last / $url]
    set testUrl [string range $url 0 [expr {$lastPos - 1}]]
    if { [info exists redirectArray($testUrl)] } {
        set newUrl $redirectArray($testUrl)
        append newUrl [string range $url $lastPos end]
        ::log::logsubst debug {newUrl = $newUrl}
        set url $newUrl
    }

    ::log::logsubst debug {\t Importing {$url}}

    ##
    ## Skip "known" namespace
    ##
    switch -exact -- $url {
        http://schemas.xmlsoap.org/wsdl/ -
        http://schemas.xmlsoap.org/wsdl/soap/ -
        http://www.w3.org/2001/XMLSchema {
            return
        }
        default {
            ##
            ## Do nothing
            ##
        }
    }

    ##
    ## Short-circuit infinite loop on inports
    ##
    if { [info exists importedXref($mode,$serviceName,$url)] } {
        ::log::logsubst debug {$mode,$serviceName,$url was already imported: $importedXref($mode,$serviceName,$url)}
        return
    }
    dict lappend serviceInfo imports $url
    set importedXref($mode,$serviceName,$url) [list $mode $serviceName $tnsCount]
    set urlScheme [dict get [::uri::split $url] scheme]
    ::log::logsubst debug {URL Scheme of {$url} is {$urlScheme}}
    switch -exact -- $urlScheme {
        file {
            ::log::logsubst debug {In file processor -- {$urlTail}}
            set fn [file join  $options(includeDirectory) [string range $urlTail 8 end]]
            set ifd  [open $fn r]
            set xml [read $ifd]
            close $ifd
            ProcessImportXml $mode $baseUrl $xml $serviceName $serviceInfoVar $tnsCountVar
        }
        https -
        http {
            ::log::log debug "In http/https processor"
            set ncode -1
            set token [geturl_followRedirects $url -timeout $options(queryTimeout)]
            set ncode [::http::ncode $token]
            ::log::log debug "returned code {$ncode}"
            set xml [::http::data $token]
            ::http::cleanup $token
            if {($ncode != 200) && [string equal $options(includeDirectory) {}]} {
                return \
                    -code error \
                    -errorcode [list WS CLIENT HTTPFAIL $url $ncode] \
                    "HTTP get of import file failed '$url'"
            } elseif {($ncode == 200) && ![string equal $options(includeDirectory) {}]} {
                set fn [file join  $options(includeDirectory) [file tail $urlTail]]
                ::log::logsubst info {Could not access $url -- using $fn}
                set ifd  [open $fn r]
                set xml [read $ifd]
                close $ifd
            }
            if {[catch {ProcessImportXml $mode $baseUrl $xml $serviceName $serviceInfoVar $tnsCountVar} err]} {
                ::log::logsubst info {Error during processing of XML: $err}
                #puts stderr "error Info: $::errorInfo"
            } else {
                #puts stderr "import successful"
            }
        }
        default {
            return \
                -code error \
                -errorcode [list WS CLIENT UNKURLTYP $url] \
                "Unknown URL type '$url'"
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
# Procedure Name : ::WS::Utils::parseComplexType
#
# Description : Parse a complex type declaration from the Schema into our
#               internal representation
#
# Arguments :
#    dictVar            - The name of the results dictionary
#    serviceName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::parseComplexType {mode dictVar serviceName node tns} {
    upvar 1 $dictVar results
    variable currentSchema
    variable nsList
    variable unknownRef
    variable defaultType

    ::log::logsubst debug {Entering [info level 0]}

    set isAbstractType false
    set defaultType string
    set typeName $tns:[$node getAttribute name]
    ::log::logsubst debug {Complex Type is $typeName}
    if {[$node hasAttribute abstract]} {
        set isAbstractType [$node getAttribute abstract]
        ::log::logsubst debug {\t Abstract type = $isAbstractType}
    }
    #if {[string length [::WS::Utils::GetServiceTypeDef $mode $serviceName $typeName]]} {
    #    ::log::log debug "\t Type $typeName is already defined -- leaving"
    #    return
    #}
    set partList {}
    set nodeFound 0
    array set attrArr {}
    set comment {}
    set middleNodeList [$node childNodes]
    foreach middleNode $middleNodeList {
        set commentNodeList [$middleNode selectNodes -namespaces $nsList xs:annotation]
        if {[llength $commentNodeList]} {
            set commentNode [lindex $commentNodeList 0]
            set comment [string trim [$commentNode asText]]
        }
        set middle [$middleNode localName]
        ::log::logsubst debug {Complex Type is $typeName, middle is $middle}
        #if {$isAbstractType && [string equal $middle attribute]} {
        #    ##
        #    ## Abstract type, so treat like an element
        #    ##
        #    set middle element
        #}

        switch -exact -- $middle {
            attribute -
            annotation {
                ##
                ## Do nothing
                ##
                continue
            }
            element {
                set nodeFound 1
                if {[$middleNode hasAttribute ref]} {
                    set partType [$middleNode getAttribute ref]
                    ::log::logsubst debug {\t\t has a ref of {$partType}}
                    if {[catch {
                        set refTypeInfo [split $partType {:}]
                        set partName [lindex $refTypeInfo end]
                        set refNS [lindex $refTypeInfo 0]
                        if {[string equal $refNS {}]} {
                            set partType $tns:$partType
                        }
                        ##
                        ## Convert the reference to the local tns space
                        ##
                        set partType [getQualifiedType $results $partType $tns $middleNode]
                        set refTypeInfo [GetServiceTypeDef $mode $serviceName $partType]
                        set refTypeInfo [dict get $refTypeInfo definition]
                        set tmpList [dict keys $refTypeInfo]
                        if {[llength $tmpList] == 1} {
                            ##
                            ## See if the reference is to an element or a type
                            ##
                            if {![dict exists $results elements $partType]} {
                                ##
                                ## To at type, so redefine the name
                                ##
                                set partName [lindex [dict keys $refTypeInfo] 0]
                            }
                            set partType [getQualifiedType $results [dict get $refTypeInfo $partName type] $tns $middleNode]
                        }
                        lappend partList $partName [list type $partType]
                    }]} {
                        lappend unknownRef($partType) $typeName
                        return \
                            -code error \
                            -errorcode [list WS $mode UNKREF [list $typeName $partType]] \
                            "Unknown forward type reference {$partType} in {$typeName}"
                    }
                } else {
                    set partName [$middleNode getAttribute name]
                    set partType [string trimright \
                        [getQualifiedType $results [$middleNode getAttribute type string:string] $tns $middleNode] {?}]
                    set partMax [$middleNode getAttribute maxOccurs 1]
                    if {$partMax <= 1} {
                        lappend partList $partName [list type $partType comment $comment]
                    } else {
                        lappend partList $partName [list type [string trimright ${partType} {()}]() comment $comment]
                    }
                }
            }
            extension {
                #set baseName [lindex [split [$middleNode getAttribute base] {:}] end]
                set tmp [partList $mode $middleNode $serviceName results $tns]
                if {[llength $tmp]} {
                    set nodeFound 1
                    set partList [concat $partList $tmp]
                }
            }
            choice -
            sequence -
            all {
                # set elementList [$middleNode selectNodes -namespaces $nsList xs:element]
                set partMax [$middleNode getAttribute maxOccurs 1]
                set tmp [partList $mode $middleNode $serviceName results $tns $partMax]
                if {[llength $tmp]} {
                    ::log::logsubst debug {\tadding {$tmp} to partslist}
                    set nodeFound 1
                    set partList [concat $partList $tmp]
                } elseif {!$nodeFound} {
                    ::WS::Utils::ServiceSimpleTypeDef $mode $serviceName $typeName [list base string comment $comment] $tns
                    return
                }
            # simpleType {
            #   $middleNode setAttribute name [$node getAttribute name]
            #   parseSimpleType $mode results $serviceName $middleNode $tns
            #   return
            # }
            }
            complexType {
                $middleNode setAttribute name $typeName
                parseComplexType $mode results $serviceName $middleNode $tns
            }
            simpleContent -
            complexContent {
                ##
                ## Save simple or complex content for abstract types, which
                ## may have content type with no fields. [Bug 584bfb77]
                ## Example xml type snippet:
                ##  <xs:complexType name="Envelope" abstract="true">
                ##    <xs:annotation><xs:documentation /></xs:annotation>
                ##    <xs:complexContent mixed="false">
                ##      <xs:extension base="Geometry" />
                ##    </xs:complexContent>
                ##  </xs:complexType>
                ##  ...
                ##  <xs:complexType name="Geometry">
                ##    <xs:annotation><xs:documentation /></xs:annotation>
                ##  </xs:complexType>

                set isComplexContent [expr {$middle eq "complexContent"}]
                ::log::logsubst debug {isComplexContent = $isComplexContent}

                ##
                ## Loop over the components of the type
                ##
                foreach child [$middleNode childNodes] {
                    set parent [$child parent]
                    set contentType [$child localName]
                    ::log::logsubst debug {Content Type is {$contentType}}
                    switch -exact -- $contentType {
                        restriction {
                            set nodeFound 1
                            set restriction $child
                            set element [$child selectNodes -namespaces $nsList xs:attribute]
                            set typeInfoList [list baseType [$restriction getAttribute base]]
                            array unset attrArr
                            foreach attr [$element attributes] {
                                if {[llength $attr] > 1} {
                                    set name [lindex $attr 0]
                                    set ref [lindex $attr 1]:[lindex $attr 0]
                                } else {
                                    set name $attr
                                    set ref $attr
                                }
                                catch {set attrArr($name) [$element getAttribute $ref]}
                            }
                            set partName item
                            set partType [getQualifiedType $results $attrArr(arrayType) $tns]
                            set partType [string map {{[]} {()}} $partType]
                            lappend partList $partName [list type [string trimright ${partType} {()?}]() comment $comment allowAny 1]
                            set nodeFound 1
                        }
                        extension {
                            ::log::logsubst debug {Calling partList for $contentType of $typeName}
                            if {[catch {set tmp [partList $mode $child $serviceName results $tns]} msg]} {
                                ::log::logsubst debug {Error in partList {$msg}, errorInfo: $errorInfo}
                            }
                            ::log::logsubst debug {partList for $contentType of $typeName is {$tmp}}
                            if {[llength $tmp]  && ![string equal [lindex $tmp 0] {}]} {
                                set nodeFound 1
                                set partList [concat $partList $tmp]
                            } elseif {[llength $tmp]} {
                                ##
                                ## Found extension, but it is an empty type
                                ##
                            } else {
                                ::log::log debug  "Unknown extension!"
                                return
                            }
                        }
                        default {
                            ##
                            ## Placed here to shut up tclchecker
                            ##
                        }
                    }
                }
            }
            restriction {
                if {!$nodeFound} {
                    parseSimpleType $mode results $serviceName $node $tns
                    return
                }
            }
            default {
                if {!$nodeFound} {
                    parseElementalType $mode results $serviceName $node $tns
                    return
                }
            }
        }
    }
    ::log::logsubst debug {at end of foreach {$typeName} with {$partList}}
    if {[llength $partList] || $isAbstractType} {
        #dict set results types $tns:$typeName $partList
        dict set results types $typeName $partList
        ::log::logsubst debug  {Defining $typeName as '$partList'}
        ##
        ## Add complex type definition, if:
        ##   *  there is a part list
        ##   *  or it is an abstract type announced as complex
        ##      (see xml snipped above about [Bug 584bfb77])
        ##      -> will set dict typeInfo client $service tns1:envelope {
        ##          definition {} xns tns1 abstract true}
        ##
        if {    ([llength $partList]  && ![string equal [lindex $partList 0] {}])
                || ($isAbstractType && [info exists isComplexContent] && $isComplexContent)
        } {
            ::WS::Utils::ServiceTypeDef $mode $serviceName $typeName $partList $tns $isAbstractType
        } else {
            ::WS::Utils::ServiceSimpleTypeDef $mode $serviceName $typeName [list base $defaultType comment {}] $tns
        }

    } elseif {!$nodeFound} {
        #puts "Defined $typeName as simple type"
        #::WS::Utils::ServiceTypeDef $mode $serviceName $typeName $partList $tns $isAbstractType
        ::WS::Utils::ServiceSimpleTypeDef $mode $serviceName $typeName [list base $defaultType comment {}] $tns
    } else {
        set xml [string trim [$node asXML]]
        return \
            -code error \
            -errorcode [list WS $mode BADCPXTYPDEF [list $typeName $xml]] \
            "Bad complex type definition for '$typeName' :: '$xml'"
    }
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::partList
#
# Description : Parse the list of parts of a type definition from the Schema into our
#               internal representation
#
# Arguments :
#    dictVar            - The name of the results dictionary
#    servcieName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::partList {mode node serviceName dictVar tns {occurs {}}} {
    variable currentSchema
    variable unknownRef
    variable nsList
    variable defaultType
    variable options
    variable simpleTypes
    upvar 1 $dictVar results

    set partList {}
    set middle [$node localName]
    ::log::logsubst debug {Entering [info level 0] -- for $middle}
    switch -exact -- $middle {
        anyAttribute -
        attribute {
            ##
            ## Do Nothing
            ##
        }
        element {
            catch {
                set partName [$node getAttribute name]
                set partType [string trimright [getQualifiedType $results [$node getAttribute type string] $tns $node] {?}]
                set partMax [$node getAttribute maxOccurs 1]
                if {$partMax <= 1} {
                    set partList [list $partName [list type $partType comment {}]]
                } else {
                    set partList [list $partName [list type [string trimright ${partType} {()}]() comment {}]]
                }
            }
        }
        extension {
            set baseName [getQualifiedType $results [$node getAttribute base string] $tns $node]
            set baseTypeInfo [TypeInfo Client $serviceName $baseName]
            ::log::logsubst debug {\t base name of extension is {$baseName} with typeinfo {$baseTypeInfo}}
            if {[lindex $baseTypeInfo 0]} {
                if {[catch {::WS::Utils::GetServiceTypeDef Client $serviceName $baseName}]} {
                    set baseQuery [format {child::*[attribute::name='%s']} $baseName]
                    set baseNode [$currentSchema selectNodes $baseQuery]
                    #puts "$baseQuery gave {$baseNode}"
                    set baseNodeType [$baseNode localName]
                    switch -exact -- $baseNodeType {
                        complexType {
                            parseComplexType $mode results $serviceName $baseNode $tns
                        }
                        element {
                            parseElementalType $mode results $serviceName $baseNode $tns
                        }
                        simpleType {
                            parseSimpleType $mode results $serviceName $baseNode $tns
                        }
                        default {
                            ##
                            ## Placed here to shut up tclchecker
                            ##
                        }
                    }
                }
                set baseInfo [GetServiceTypeDef $mode $serviceName $baseName]
                ::log::logsubst debug {\t baseInfo is {$baseInfo}}
                if {[llength $baseInfo] == 0} {
                    ::log::logsubst debug {\t Unknown reference '$baseName'}
                    set unknownRef($baseName) 1
                    return
                }
                catch {set partList [concat $partList [dict get $baseInfo definition]]}
            } else {
                ::log::logsubst debug {\t Simple type}
            }
            foreach elementNode [$node childNodes] {
                set tmp [partList $mode $elementNode $serviceName results $tns]
                if {[llength $tmp]} {
                    set partList [concat $partList $tmp]
                }
            }
        }
        choice -
        sequence -
        all {
            set elementList [$node selectNodes -namespaces $nsList xs:element]
            set elementsFound 0
            ::log::logsubst debug {\telement list is {$elementList}}
            foreach element $elementList {
                ::log::logsubst debug {\t\tprocessing $element ([$element nodeName])}
                set comment {}
                set additional_defininition_elements {}
                if {[catch {
                    set elementsFound 1
                    set attrName name
                    set isRef 0
                    if {![$element hasAttribute name]} {
                        set attrName ref
                        set isRef 1
                    }
                    set partName [$element getAttribute $attrName]
                    if {$isRef} {
                        set partType {}
                        set partTypeInfo {}
                        set partType [string trimright [getQualifiedType $results $partName $tns] {?}]
                        set partTypeInfo [::WS::Utils::GetServiceTypeDef $mode $serviceName $partType]
                        set partName [lindex [split $partName {:}] end]
                        ::log::logsubst debug {\t\t\t part name is {$partName} type is {$partTypeInfo}}
                        if {[dict exists $partTypeInfo definition $partName]} {
                            set partType [dict get $partTypeInfo definition $partName type]
                        }
                        ::log::logsubst debug {\t\t\t part name is {$partName} type is {$partType}}
                    } else {
                        ##
                        ## See if really a complex definition
                        ##
                        if {[$element hasChildNodes]} {
                            set isComplex 0; set isSimple 0
                            foreach child [$element childNodes] {
                                switch -exact -- [$child localName] {
                                  annotation {set comment [string trim [$child asText]]}
                                  simpleType {set isSimple  1}
                                  default    {set isComplex 1}
                                }
                            }
                            if {$isComplex} {
                                set partType $partName
                                parseComplexType $mode results $serviceName $element $tns
                            } elseif {$isSimple} {
                                set partType $partName
                                parseComplexType $mode results $serviceName $element $tns
                                if {[info exists simpleTypes($mode,$serviceName,$tns:$partName)]} {
                                  set additional_defininition_elements $simpleTypes($mode,$serviceName,$tns:$partName)
                                  set partType [dict get $additional_defininition_elements baseType]
                                }
                            } else {
                                set partType [getQualifiedType $results [$element getAttribute type string] $tns $element]
                            }
                        } else {
                            set partType [getQualifiedType $results [$element getAttribute type string] $tns $element]
                        }
                    }
                    if {[string length $occurs]} {
                        set partMax [$element getAttribute maxOccurs 1]
                        if {$partMax < $occurs} {
                            set partMax $occurs
                        }
                    } else {
                        set partMax [$element getAttribute maxOccurs 1]
                    }
                    if {$partMax <= 1} {
                        lappend partList $partName [concat [list type $partType comment $comment] $additional_defininition_elements]
                    } else {
                        lappend partList $partName [concat [list type [string trimright ${partType} {()?}]() comment $comment] $additional_defininition_elements]
                    }
                } msg]} {
                    ::log::logsubst error {\tError processing {$msg} for [$element asXML]}
                    if {$isRef} {
                        ::log::log error "\t\t Was a reference.  Additionally information is:"
                        ::log::logsubst error {\t\t\t part name is {$partName} type is {$partType} with {$partTypeInfo}}
                    }
                }
            }
            if {!$elementsFound} {
                set defaultType $options(anyType)
                return
            }
        }
        complexContent {
            set contentType [[$node childNodes] localName]
            switch -exact -- $contentType {
                restriction {
                    set restriction [$node selectNodes -namespaces $nsList xs:restriction]
                    set element [$node selectNodes -namespaces $nsList xs:restriction/xs:attribute]
                    set typeInfoList [list baseType [$restriction getAttribute base]]
                    array unset attrArr
                    foreach attr [$element attributes] {
                        if {[llength $attr] > 1} {
                            set name [lindex $attr 0]
                            set ref [lindex $attr 1]:[lindex $attr 0]
                        } else {
                            set name $attr
                            set ref $attr
                        }
                        catch {set attrArr($name) [$element getAttribute $ref]}
                    }
                    set partName item
                    set partType [getQualifiedType $results $attrArr(arrayType) $tns]
                    set partType [string map {{[]} {()}} $partType]
                    set partList [list $partName [list type [string trimright ${partType} {()?}]() comment {} allowAny 1]]
                }
                extension {
                    set extension [$node selectNodes -namespaces $nsList xs:extension]
                    set partList [partList $mode $extension $serviceName results $tns]
                }
                default {
                    ##
                    ## Placed here to shut up tclchecker
                    ##
                }
            }
        }
        simpleContent {
            foreach elementNode [$node childNodes] {
                set tmp [partList $mode $elementNode $serviceName results $tns]
                if {[llength $tmp]} {
                    set partList [concat $partList $tmp]
                }
            }
        }
        restriction {
            parseSimpleType $mode results $serviceName $node $tns
            return
        }
        default {
            parseElementalType $mode results $serviceName $node $tns
            return
        }
    }
    if {[llength $partList] == 0} {
        set partList {{}}
    }
    return $partList
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseElementalType
#
# Description : Parse an elemental type declaration from the Schema into our
#               internal representation
#
# Arguments :
#    dictVar            - The name of the results dictionary
#    servcieName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::parseElementalType {mode dictVar serviceName node tns} {

    upvar 1 $dictVar results
    variable importedXref
    variable nsList
    variable unknownRef

    ::log::logsubst debug {Entering [info level 0]}

    set attributeName name
    if {![$node hasAttribute $attributeName]} {
        set attributeName ref
    }
    set typeName [$node getAttribute $attributeName]
    if {[string length [::WS::Utils::GetServiceTypeDef $mode $serviceName $tns:$typeName]]} {
        ::log::logsubst debug {\t Type $tns:$typeName is already defined -- leaving}
        return
    }
    set typeType ""
    if {[$node hasAttribute type]} {
        set typeType [getQualifiedType $results [$node getAttribute type string] $tns $node]
    }
    ::log::logsubst debug {Elemental Type is $typeName}
    set partList {}
    set partType {}
    set isAbstractType false
    if {[$node hasAttribute abstract]} {
        set isAbstractType [$node getAttribute abstract]
        ::log::logsubst debug {\t Abstract type = $isAbstractType}
    }
    set elements [$node selectNodes -namespaces $nsList xs:complexType/xs:sequence/xs:element]
    ::log::logsubst debug {\t element list is {$elements} partList {$partList}}
    foreach element $elements {
        set ::elementName [$element asXML]
        ::log::logsubst debug {\t\t Processing element {[$element nodeName]}}
        set elementsFound 1
        set typeAttribute ""
        if {[$element hasAttribute ref]} {
            set partType [$element getAttribute ref]
            ::log::logsubst debug {\t\t has a ref of {$partType}}
            if {[catch {
                set refTypeInfo [split $partType {:}]
                set partName [lindex $refTypeInfo end]
                set refNS [lindex $refTypeInfo 0]
                if {[string equal $refNS {}]} {
                    set partType $tns:$partType
                }
                ##
                ## Convert the reference to the local tns space
                ##
                set partType  [getQualifiedType $results $partType $tns]
                set refTypeInfo [GetServiceTypeDef $mode $serviceName $partType]
                log::logsubst debug {looking up ref {$partType} got {$refTypeInfo}}
                if {![llength $refTypeInfo]} {
                    error "lookup failed"
                }
                if {[dict exists $refTypeInfo definition]} {
                    set refTypeInfo [dict get $refTypeInfo definition]
                }
                set tmpList [dict keys $refTypeInfo]
                if {[llength $tmpList] == 1} {
                    ##
                    ## See if the reference is to an element or a type
                    ##
                    if {![dict exists $results elements $partType]} {
                        ##
                        ## To at type, so redefine the name
                        ##
                        set partName [lindex [dict keys $refTypeInfo] 0]
                    }
                    if {[dict exists $refTypeInfo $partName type]} {
                        set partType [getQualifiedType $results [dict get $refTypeInfo $partName type] $tns]
                    } else {
                        ##
                        ## Not a simple element, so point type to type of same name as element
                        ##
                        set partType [getQualifiedType $results $partName $tns]
                    }
                }
            } msg]} {
                lappend unknownRef($partType) $typeName
                log::logsubst debug {Unknown ref {$partType,$typeName} error: {$msg} trace: $::errorInfo}
                return \
                    -code error \
                    -errorcode [list WS $mode UNKREF [list $typeName $partType]] \
                    "Unknown forward type reference {$partType} in {$typeName}"
            }
        } else {
            ::log::logsubst debug {\t\t\t has no ref has {[$element attributes]}}
            set childList [$element selectNodes -namespaces $nsList xs:complexType/xs:sequence/xs:element]
            ::log::logsubst debug {\t\t\ has no ref has [llength $childList]}
            if {[llength $childList]} {
                ##
                ## Element defines another element layer
                ##
                set partName [$element getAttribute name]
                set partType [getQualifiedType $results $partName $tns $element]
                parseElementalType $mode results $serviceName $element $tns
            } else {
                set partName [$element getAttribute name]
                if {[$element hasAttribute type]} {
                    set partType [getQualifiedType $results [$element getAttribute type] $tns $element]
                } else {
                    set partType xs:string
                }

            }
        }
        set partMax [$element getAttribute maxOccurs -1]
        ::log::logsubst debug {\t\t\t part is {$partName} {$partType} {$partMax}}

        if {[string equal $partMax -1]} {
            set partMax [[$element parent] getAttribute maxOccurs -1]
        }
        if {$partMax <= 1} {
            lappend partList $partName [list type $partType comment {}]
        } else {
            lappend partList $partName [list type [string trimright ${partType} {()?}]() comment {}]
        }
    }
    if {[llength $elements] == 0} {
        #
        # Validate this is not really a complex or simple type
        #
        set childList [$node childNodes]
        foreach childNode $childList {
            if {[catch {$childNode setAttribute name $typeName}]} {
                continue
            }
            set childNodeType [$childNode localName]
            switch -exact -- $childNodeType {
                complexType {
                    parseComplexType $mode results $serviceName $childNode $tns
                    return
                }
                element {
                    parseElementalType $mode results $serviceName $childNode $tns
                    return
                }
                simpleType {
                    parseSimpleType $mode results $serviceName $childNode $tns
                    return
                }
                default {
                    ##
                    ## Placed here to shut up tclchecker
                    ##
                }
            }
        }
        # have an element with a type only, so do the work here
        if {[$node hasAttribute type]} {
            set partType [getQualifiedType $results [$node getAttribute type] $tns $node]
        } elseif {[$node hasAttribute base]}  {
            set partType [getQualifiedType $results [$node getAttribute base] $tns $node]
        } else {
            set partType xs:string
        }
        set partMax [$node getAttribute maxOccurs 1]
        if {$partMax <= 1} {
            ##
            ## See if this is just a restriction on a simple type
            ##
            if {([lindex [TypeInfo $mode $serviceName $partType] 0] == 0) &&
                [string equal $tns:$typeName $partType]} {
                return
            } else {
                lappend partList $typeName [list type $partType comment {}]
            }
        } else {
            lappend partList $typeName [list type [string trimright ${partType} {()?}]() comment {}]
        }
    }
    if {[llength $partList]} {
        ::WS::Utils::ServiceTypeDef $mode $serviceName $tns:$typeName $partList $tns $isAbstractType
    } else {
        if {![dict exists $results types $tns:$typeName]} {
            set partList [list base string comment {} xns $tns]
            ::WS::Utils::ServiceSimpleTypeDef $mode $serviceName $tns:$typeName  $partList $tns
            dict set results simpletypes $tns:$typeName $partList
        }
    }
    dict set results elements $tns:$typeName 1

     ::log::log debug "\t returning"
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::parseSimpleType
#
# Description : Parse a simple type declaration from the Schema into our
#               internal representation
#
# Arguments :
#    dictVar            - The name of the results dictionary
#    servcieName        - The service name this type belongs to
#    node               - The root node of the type definition
#    tns                - Namespace for this type
#
# Returns : Nothing
#
# Side-Effects : Defines mode type as specified by the Schema
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
proc ::WS::Utils::parseSimpleType {mode dictVar serviceName node tns} {
    upvar 1 $dictVar results
    variable nsList

    ::log::logsubst debug {Entering [info level 0]}

    set typeName [$node getAttribute name]
    if {$typeName in {SAP_VALID_FROM}} {
        set foo 1
    }
    set isList no
    ::log::logsubst debug {Simple Type is $typeName}
    if {[string length [::WS::Utils::GetServiceTypeDef $mode $serviceName $tns:$typeName]]} {
        ::log::logsubst debug {\t Type $tns:$typeName is already defined -- leaving}
        return
    }
    #puts "Simple Type is $typeName"
    set restrictionNode [$node selectNodes -namespaces $nsList xs:restriction]
    if {[string equal $restrictionNode {}]} {
        set restrictionNode [$node selectNodes -namespaces $nsList xs:simpleType/xs:restriction]
    }
    if {[string equal $restrictionNode {}]} {
        set restrictionNode [$node selectNodes -namespaces $nsList xs:list/xs:simpleType/xs:restriction]
    }
    if {[string equal $restrictionNode {}]} {
        set restrictionNode [$node selectNodes -namespaces $nsList xs:list]
        set isList yes
    }
    if {[string equal $restrictionNode {}]} {
        set xml [string trim [$node asXML]]
        return \
            -code error \
            -errorcode [list WS $mode BADSMPTYPDEF [list $typeName $xml]] \
            "Bad simple type definition for '$typeName' :: \n'$xml'"
    }
    if {$isList} {
        set baseType [lindex [split [$restrictionNode getAttribute itemType] {:}] end]
    } else {
        set baseType [lindex [split [$restrictionNode getAttribute base] {:}] end]
    }
    set partList [list baseType $baseType xns $tns isList $isList]
    set enumList {}
    foreach item [$restrictionNode childNodes] {
        set itemName [$item localName]
        set value [$item getAttribute value]
        #puts "\t Item {$itemName} = {$value}"
        if {[string equal $itemName {enumeration}]} {
            lappend enumList $value
        } else {
            lappend partList $itemName $value
        }
        if {[$item hasAttribute fixed]} {
            lappend partList fixed [$item getAttribute fixed]
        }
    }
    if {[llength $enumList]} {
        lappend partList enumeration $enumList
    }
    if {![dict exists $results types $tns:$typeName]} {
        ServiceSimpleTypeDef $mode $serviceName $tns:$typeName $partList $tns
        dict set results simpletypes $tns:$typeName $partList
    } else {
        ::log::logsubst debug {\t type already exists as $tns:$typeName}
    }
}



###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::checkTags
#
# Description : Recursivly check the tags and values inside the tags
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       currNode    - The node to process
#       typeName    - The type name of the node
#
# Returns :     1 if ok, 0 otherwise
#
# Side-Effects :
#       ::errorCode - contains validation failure information if validation
#                       failed.
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/13/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to handle qualified XML
#
###########################################################################
proc ::WS::Utils::checkTags {mode serviceName currNode typeName} {

    ##
    ## Assume success
    ##
    set result 1

    ##
    ## Get the type information
    ##
    set typeInfoList [TypeInfo $mode $serviceName $typeName]
    set baseTypeName [string trimright $typeName {()?}]
    set typeName [string trimright $typeName {?}]
    set typeInfo [GetServiceTypeDef $mode $serviceName $baseTypeName]
    set isComplex [lindex $typeInfoList 0]
    set isArray [lindex $typeInfoList 1]

    if {$isComplex} {
        ##
        ## Is complex
        ##
        array set fieldInfoArr {}
        ##
        ## Build array of what is present
        ##
        foreach node [$currNode childNodes] {
            set localName [$node localName]
            lappend fieldInfoArr($localName) $node
        }
        ##
        ## Walk through each field and validate the information
        ##
        foreach {field fieldDef} [dict get $typeInfo definition] {
            array unset fieldInfoArr
            set fieldInfoArr(minOccurs) 0
            array set fieldInfoArr $fieldDef
            if {$fieldInfoArr(minOccurs) && ![info exists fieldInfoArr($field)]} {
                ##
                ## Fields was required but is missing
                ##
                set ::errorCode [list WS CHECK MISSREQFLD [list $typeName $field]]
                set result 0
            } elseif {$fieldInfoArr(minOccurs) &&
                      ($fieldInfoArr(minOccurs) > [llength $fieldInfoArr($field)])} {
                ##
                ## Fields was required and present, but not enough times
                ##
                set ::errorCode [list WS CHECK MINOCCUR [list $type $field]]
                set result 0
            } elseif {[info exists fieldInfoArr(maxOccurs)] &&
                      [string is integer fieldInfoArr(maxOccurs)] &&
                      ($fieldInfoArr(maxOccurs) < [llength $fieldInfoArr($field)])} {
                ##
                ## Fields was required and present, but too many times
                ##
                set ::errorCode [list WS CHECK MAXOCCUR [list $typeName $field]]
                set result 0
            } elseif {[info exists fieldInfoArr($field)]} {
                foreach node $fieldInfoArr($field) {
                    set result [checkTags $mode $serviceName $node $fieldInfoArr(type)]
                    if {!$result} {
                        break
                    }
                }
            }
            if {!$result} {
                break
            }
        }
    } else {
        ##
        ## Get the value
        ##
        set value [$currNode asText]
        set result [checkValue $mode $serviceName $baseTypeName $value]
    }

    return $result
}



###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::checkValue
#
# Description : Check a Value between tags of a XML document against the
#               type in the XML schema description
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The name of the service
#       type        - The type to check
#       value       - The value to check
#
# Returns :     1 if ok or 0 if checking not ok
#
# Side-Effects :
#       ::errorCode - contains validation failure information if validation
#                       failed.
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/14/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to handle qualified XML
#
###########################################################################
proc ::WS::Utils::checkValue {mode serviceName type value} {

    set result 0
    array set typeInfos {
        minLength 0
        maxLength -1
        fixed false
    }
    # returns indexes type, xns, ...
    array set typeInfos [GetServiceTypeDef $mode $serviceName $type]
    foreach {var value} [array get typeInfos] {
        set $var $value
    }
    set result 1

    if {$minLength >= 0 && [string length $value] < $minLength} {
        set ::errorCode [list WS CHECK VALUE_TO_SHORT [list $type $value $minLength $typeInfo]]
        set result 0
    } elseif {$maxLength >= 0 && [string length $value] > $maxLength} {
        set ::errorCode [list WS CHECK VALUE_TO_LONG [list $type $value $maxLength $typeInfo]]
        set result 0
    } elseif {[info exists enumeration] && ([lsearch -exact $enumeration $value] == -1)} {
        set errorCode [list WS CHECK VALUE_NOT_IN_ENUMERATION [list $type $value $enumeration $typeInfo]]
        set result 0
    } elseif {[info exists pattern] && (![regexp -- $pattern $value])} {
        set errorCode [list WS CHECK VALUE_NOT_MATCHES_PATTERN [list $type $value $pattern $typeInfo]]
        set result 0
    }

    return $result
}



###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::buildTags
#
# Description : Recursivly build the tags by checking the values to put
#               inside the tags and append to the dom tree resultTree
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       typeName    - The type for the tag
#       valueInfos  - The dictionary of the values
#       doc         - The DOM Document
#       currentNode - Node to append values to
#
# Returns :     nothing
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Arnulf Wiedemann
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  08/13/2006  A.Wiedemann  Initial version
#       2  08/18/2006  G.Lester     Generalized to generate qualified XML
#
###########################################################################
proc ::WS::Utils::buildTags {mode serviceName typeName valueInfos doc currentNode} {
    upvar 1 $valueInfos values

    ##
    ## Get the type information
    ##
    set baseTypeName [string trimright $typeName {()?}]
    set typeInfo [GetServiceTypeDef $mode $serviceName $baseTypeName]
    set typeName [string trimright $typeName {?}]
    set xns [dict get $typeInfo $mode $serviceName $typeName xns]

    foreach {field fieldDef} [dict get $typeInfo definition] {
        ##
        ## Get info about this field and its type
        ##
        array unset fieldInfoArr
        set fieldInfoArr(minOccurs) 0
        array set fieldInfoArr $fieldDef
        set typeInfoList [TypeInfo $mode $serviceName $fieldInfoArr(type)]
        set fieldBaseType [string trimright $fieldInfoArr(type) {()?}]
        set isComplex [lindex $typeInfoList 0]
        set isArray [lindex $typeInfoList 1]
        if {[dict exists $valueInfos $field]} {
            if {$isArray} {
                set valueList [dict get $valueInfos $field]
            } else {
                set valueList [list [dict get $valueInfos $field]]
            }
            set valueListLenght [llength $valueList]
        } else {
            set valueListLenght -1
        }

        if {$fieldInfoArr(minOccurs) && ![dict exists $valueInfos $field]} {
            ##
            ## Fields was required but is missing
            ##
            return \
                -errorcode [list WS CHECK MISSREQFLD [list $type $field]] \
                "Field '$field' of type '$typeName' was required but is missing"
        } elseif {$fieldInfoArr(minOccurs) &&
                  ($fieldInfoArr(minOccurs) > $valueListLenght)} {
            ##
            ## Fields was required and present, but not enough times
            ##
            set minOccurs $fieldInfoArr(minOccurs)
            return \
                -errorcode [list WS CHECK MINOCCUR [list $type $field $minOccurs $valueListLenght]] \
                "Field '$field' of type '$typeName' was required to occur $minOccurs time(s) but only occured $valueListLenght time(s)"
        } elseif {[info exists fieldInfoArr(maxOccurs)] &&
                  [string is integer fieldInfoArr(maxOccurs)] &&
                  ($fieldInfoArr(maxOccurs) < $valueListLenght)} {
            ##
            ## Fields was required and present, but too many times
            ##
            set minOccurs $fieldInfoArr(maxOccurs)
            return \
                -errorcode [list WS CHECK MAXOCCUR [list $typeName $field]] \
                "Field '$field' of type '$typeName' could only occur $minOccurs time(s) but occured $valueListLenght time(s)"
        } elseif {[dict exists $valueInfos $field]} {
            foreach value $valueList {
                $currentNode appendChild [$doc createElement $xns:$field retNode]
                if {$isComplex} {
                    buildTags $mode $serviceName $fieldBaseType $value $doc $retNode
                } else {
                    if {[info exists fieldInfoArr(enumeration)] &&
                        [info exists fieldInfoArr(fixed)] && $fieldInfoArr(fixed)} {
                        set value [lindex $fieldInfoArr(enumeration) 0]
                    }
                    if {[checkValue $mode $serviceName $fieldBaseType $value]} {
                        $retNode appendChild [$doc createTextNode $value]
                    } else {
                        set msg "Field '$field' of type '$typeName' "
                        switch -exact -- [lindex $::errorCode 2] {
                            VALUE_TO_SHORT {
                                append msg "value required to be $fieldInfoArr(minLength) long but is only [string length $value] long"
                            }
                            VALUE_TO_LONG {
                                append msg "value allowed to be only $fieldInfoArr(minLength) long but is [string length $value] long"
                            }
                            VALUE_NOT_IN_ENUMERATION {
                                append msg "value '$value' not in ([join $fieldInfoArr(enumeration) {, }])"
                            }
                            VALUE_NOT_MATCHES_PATTERN {
                                append msg "value '$value' does not match pattern: $fieldInfoArr(pattern)"
                            }
                            default {
                                ##
                                ## Placed here to shut up tclchecker
                                ##
                            }
                        }
                        return \
                            -errorcode $::errorCode \
                            $msg
                    }
                }
            }
        }
    }
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::getQualifiedType
#
# Description : Get a qualified type name from a local reference.
#               Thus return <Prefix>:<Type> which is in the global type list.
#               The <Prefix> is adjusted to point to the global type list.
#
# Arguments :
#       serviceInfo - service information dictionary
#       type        - type to get local qualified type on
#       tns         - current namespace
#       node        - optional XML item to search for xmlns:* attribute
#
# Returns :     nothing
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Gerald Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  02/24/2011  G. Lester    Initial version
#   2.6.2  2018-09-22  C. Werner    Added parameter "node" to first search a
#                                   namespace attribute "xmlns:yprefix>" in the
#                                   current node.
#
###########################################################################
proc ::WS::Utils::getQualifiedType {serviceInfo type tns {node {}}} {

    set typePartsList [split $type {:}]
    if {[llength $typePartsList] == 1} {
        # No namespace prefix given - use current prefix
        set result $tns:$type
    } else {
        lassign $typePartsList tmpTns tmpType
        # Search the namespace attribute in the current node for a node-local prefix.
        # Aim is to translate the node-local prefix to a global namespace prefix.
        # Example:
        # <xs:element name="A_O_S"
        #    type="x1:ArrayOfSomething"
        #    xmlns:x1="http://foo.org/bar" />
        #
        # Variable setup:
        # - type: x1:ArrayOfSomething
        # - tmpTns: x1
        # - tmpType: ArrayOfSomething
        # Return value:
        #   - <Prefix in serviceinfo which corresponds to namespace: "http://foo.org/bar">
        #   - plus ":ArrayOfSomething"
        if {$node ne {}} {
            set attr xmlns:$tmpTns
            if {[$node hasAttribute $attr]} {
                # There is a node-local attribute (Example: xmlns:x1) giving the node namespace
                set xmlns [$node getAttribute $attr]
                if {[dict exists $serviceInfo tnsList url $xmlns]} {
                    set result [dict get $serviceInfo tnsList url $xmlns]:$tmpType
                    ::log::logsubst debug {Got global qualified type '$result' from node-local qualified namespace '$xmlns'}
                    return $result
                } else {
                    # The node namespace (Ex: http://foo.org/bar) was not found as global prefix.
                    # Thus, the type is refused.
                    # HaO 2018-11-05 Opinion:
                    # Continuing here is IMHO not an option, as the prefix (Ex: x1) might have a
                    # different namespace on the global level which would lead to a misassignment.
                    #
                    # One day, we may support cascading namespace prefixes. Then, we may define
                    # the namespace here
                    set errMsg "Node local namespace URI '$xmlns' not found for type: '$type'"
                    ::log::log error $errMsg
                    return -code error $errMsg
                }
                # fail later if namespace not found
            }
        }
        if {[dict exists $serviceInfo tnsList tns $tmpTns]} {
            set result [dict get $serviceInfo tnsList tns $tmpTns]:$tmpType
        } elseif {[dict exists $serviceInfo types $type]} {
            set result $type
        } else {
            ::log::log error $serviceInfo
            ::log::logsubst error {Could not find tns '$tmpTns' in '[dict get $serviceInfo tnsList tns]' for type {$type}}
            return -code error "Namespace prefix of type '$type' not found."
        }
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
# Procedure Name : ::WS::Utils::GenerateTemplateDict
#
# Description : Generate a template dictionary object for
#               a given type.
#
# Arguments :
#    mode        - The mode, Client or Server
#    serviceName - The service name the type is defined in
#    type        - The name of the type
#    arraySize   - Number of elements to generate in an array.  Default is 2
#
# Returns : A dictionary object for a given type.  If any circular references
#           exist, they will have the value of <** Circular Reference **>
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
proc ::WS::Utils::GenerateTemplateDict {mode serviceName type {arraySize 2}} {
    variable generatedTypes

    ::log::logsubst debug {Entering [info level 0]}
    unset -nocomplain -- generatedTypes

    set result [_generateTemplateDict $mode $serviceName $type $arraySize]

    unset -nocomplain -- generatedTypes
    ::log::logsubst debug {Leaving [info level 0] with {$result}}

    return $result
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                            that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::_generateTemplateDict
#
# Description : Private procedure to generate a template dictionary.  This needs
#               setup work done by ::WS::Utils::GnerateTemplateDict
#
# Arguments :
#    mode        - The mode, Client or Server
#    serviceName - The service name the type is defined in
#    type        - The name of the type
#    arraySize   - Number of elements to generate in an array.
#
# Returns : A dictionary object for a given type.  If any circular references
#           exist, they will have the value of <** Circular Reference **>
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
proc ::WS::Utils::_generateTemplateDict {mode serviceName type arraySize {xns {}}} {
    variable typeInfo
    variable mutableTypeInfo
    variable options
    variable generatedTypes

    ::log::logsubst debug {Entering [info level 0]}
    set results {}

    ##
    ## Check for circular reference
    ##
    if {[info exists generatedTypes([list $mode $serviceName $type])]} {
        set results {<** Circular Reference **>}
        ::log::logsubst debug {Leaving [info level 0] with {$results}}
        return $results
    } else {
        set generatedTypes([list $mode $serviceName $type]) 1
    }

    set type [string trimright $type {?}]
    # set typeDefInfo [dict get $typeInfo $mode $serviceName $type]
    set typeDefInfo [GetServiceTypeDef $mode $serviceName $type]
    if {![llength $typeDefInfo]} {
      ## We failed to locate the type. try with the last known xns...
      set typeDefInfo [GetServiceTypeDef $mode $serviceName ${xns}:$type]
    }

    ::log::logsubst debug {\t type def = {$typeDefInfo}}
    set xns [dict get $typeDefInfo xns]

    ##
    ## Check for mutable type
    ##
    if {[info exists mutableTypeInfo([list $mode $serviceName $type])]} {
        set results {<** Mutable Type **>}
        ::log::logsubst debug {Leaving [info level 0] with {$results}}
        return $results
    }

    if {![dict exists $typeDefInfo definition]} {
      ## This is a simple type, simulate a type definition...
      if {![dict exists $typeDefInfo type]} {
        if {[dict exists $typeDefInfo baseType]} {
          dict set typeDefInfo type [dict get $typeDefInfo baseType]
        } else {
          dict set typeDefInfo type xs:string
        }
      }
      set typeDefInfo [dict create definition [dict create $type $typeDefInfo]]
    }
    set partsList [dict keys [dict get $typeDefInfo definition]]
    ::log::logsubst debug {\t partsList is {$partsList}}
    foreach partName $partsList {
        set partType [string trimright [dict get $typeDefInfo definition $partName type] {?}]
        set partXns $xns
        catch {set partXns  [dict get $typeInfo $mode $serviceName $partType xns]}
        set typeInfoList [TypeInfo $mode $serviceName $partType]
        set isArray [lindex $typeInfoList end]

        ::log::logsubst debug {\tpartName $partName partType $partType xns $xns typeInfoList $typeInfoList}
        switch -exact -- $typeInfoList {
            {0 0} {
                ##
                ## Simple non-array
                ##
                set msg {Simple non-array}
                ## Is there an enumenration?
                foreach attr {enumeration type comment} {
                  if {[dict exists $typeDefInfo definition $partName $attr]} {
                    set value [dict get $typeDefInfo definition $partName $attr]
                    set value [string map {\{ ( \} ) \" '} $value]
                    append msg ", $attr=\{$value\}"
                  }
                }
                dict set results $partName $msg
            }
            {0 1} {
                ##
                ## Simple array
                ##
                set tmp {}
                for {set row 1} {$row <= $arraySize} {incr row} {
                    lappend tmp [format {Simple array element #%d} $row]
                }
                dict set results $partName $tmp
            }
            {1 0} {
                ##
                ## Non-simple non-array
                ##
                dict set results $partName [_generateTemplateDict $mode $serviceName $partType $arraySize $xns]
            }
            {1 1} {
                ##
                ## Non-simple array
                ##
                set partType [string trimright $partType {()}]
                set tmp [list]
                set isRecursive [info exists generatedTypes([list $mode $serviceName $partType])]
                for {set row 1} {$row <= $arraySize} {incr row} {
                    if {$isRecursive} {
                        lappend tmp $partName {<** Circular Reference **>}
                    } else {
                        unset -nocomplain -- generatedTypes([list $mode $serviceName $partType])
                        lappend tmp [_generateTemplateDict $mode $serviceName $partType $arraySize $xns]
                    }
                }
                dict set results $partName $tmp
            }
            default {
                ##
                ## Placed here to shut up tclchecker
                ##
            }
        }
    }
    ::log::logsubst debug {Leaving [info level 0] with {$results}}
    return $results
}




###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::setAttr
#
# Description : Set attributes on a DOM node
#
# Arguments :
#       node        - node to set attributes on
#       attrList    - List of attribute name value pairs
#
# Returns :     nothing
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Gerald Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  02/24/2011  G. Lester    Initial version
#
###########################################################################
proc ::WS::Utils::setAttr {node attrList} {
    $node setAttribute {*}$attrList
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::geturl_followRedirects
#
# Description : fetch via http following redirects.
#               May not be used as asynchronous call with -command option.
#
# Arguments :
#       url        - target document url
#       args       - additional argument list to http::geturl call
#
# Returns :     http package token of received data
#
# Side-Effects :        Save final url in redirectArray to forward info to
#                       procedure "processImport".
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Gerald Lester
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  02/24/2011  G. Lester    Initial version
# 2.3.10   11/09/2015  H. Oehlmann  Allow only 5 redirects (loop protection)
# 3.1.0    2020-11-06  H.Oehlmann   Access namespace variable redirectArray
#                                   via variable command
#
###########################################################################
proc ::WS::Utils::geturl_followRedirects {url args} {
	variable redirectArray
    ::log::logsubst debug {[info level 0]}
    set initialUrl $url
    set finalUrl $url
    array set URI [::uri::split $url] ;# Need host info from here
    for {set loop 1} {$loop <=5} {incr loop} {
        ::log::logsubst info {[concat [list ::http::geturl $url] $args]}
        set token [http::geturl $url {*}$args]
        set ncode [::http::ncode $token]
        ::log::logsubst info {ncode = $ncode}
        if {![string match {30[12378]} $ncode]} {
            ::log::logsubst debug {initialUrl = $initialUrl, finalUrl = $finalUrl}
            if {![string equal $finalUrl {}]} {
                ::log::log debug "Getting initial URL directory"
                set lastPos [string last / $initialUrl]
                set initialUrlDir [string range $initialUrl 0 [expr {$lastPos - 1}]]
                set lastPos [string last / $finalUrl]
                set finalUrlDir [string range $finalUrl 0 [expr {$lastPos - 1}]]
                ::log::logsubst debug {initialUrlDir = $initialUrlDir, finalUrlDir = $finalUrlDir}
                set redirectArray($initialUrlDir) $finalUrlDir
            }
            return $token
        } elseif {![string match {20[1237]} $ncode]} {
            return $token
        }
        # http code announces redirect (3xx)
        array set meta [set ${token}(meta)]
        if {![info exist meta(Location)]} {
            ::log::log debug "Redirect http code without Location"
            return $token
        }
        array set uri [::uri::split $meta(Location)]
        unset meta
        array unset meta
        ::http::cleanup $token
        if { $uri(host) eq "" } {
            set uri(host) $URI(host)
        }
        # problem w/ relative versus absolute paths
        set url [eval ::uri::join [array get uri]]
        ::log::logsubst debug {url = $url}
        set finalUrl $url
    }
    # > 5 redirects reached -> exit with error
    return -errorcode [list WS CLIENT REDIRECTLIMIT $url] \
            -code error "http redirect limit exceeded for $url"
}
###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::geturl_fetchbody
#
# Description : fetch via http following redirects and return data or error
#
# Arguments :
#       ?-codeok list? - list of acceptable http codes.
#                       If not given, 200 is used
#       ?-codevar varname ? - Uplevel variable name to return current code
#                       value.
#       ?-bodyalwaysok bool? - If a body is delivered any ncode is ok
#       url        - target document url
#       args       - additional argument list to http::geturl call
#
# Returns :     fetched data
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
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
#       1  11/08/2015  H.Oehlmann   Initial version
#   3.0.0  2020-10-26  H.Oehlmann   Honor timeout and eof status
#
###########################################################################
proc ::WS::Utils::geturl_fetchbody {args} {
    set codeOkList {200}
    set codeVar ""
    set bodyAlwaysOk 0
    ::log::logsubst info {Entering [info level 0]}
    if {[lindex $args 0] eq "-codeok"} {
        set codeOkList [lindex $args 1]
        set args [lrange $args 2 end]
    }
    if {[lindex $args 0] eq "-codevar"} {
        set codeVar [lindex $args 1]
        set args [lrange $args 2 end]
    }
    if {[lindex $args 0] eq "-bodyalwaysok"} {
        set bodyAlwaysOk [lindex $args 1]
        set args [lrange $args 2 end]
    }

    set token [eval ::WS::Utils::geturl_followRedirects $args]
    switch -exact -- [::http::status $token] {
        ok {
            if {[::http::size $token] == 0} {
                ::log::log debug "\tHTTP error: no data"
                ::http::cleanup $token
                return -errorcode [list WS CLIENT NODATA [lindex $args 0]]\
                        -code error "HTTP failure socket closed"
            }
            if {$codeVar ne ""} {
                upvar 1 $codeVar ncode
            }
            set ncode [::http::ncode $token]
            set body [::http::data $token]
            ::http::cleanup $token
            if {$bodyAlwaysOk && $body ne ""
                || $ncode in $codeOkList
            } {
                # >> Fetch ok
                ::log::logsubst debug {\tReceived: $body}
                return $body
            }
            ::log::logsubst debug {\tHTTP error: Wrong code $ncode or no data}
            return -code error -errorcode [list WS CLIENT HTTPERROR $ncode]\
                    "HTTP failure code $ncode"
        }
        eof {
            set error "socket closed by server"
        }
        error {
            set error [::http::error $token]
        }
        timeout {
            set error "timeout"
        }
        default {
            set error "unknown http::status: [::http::status $token]"
        }
    }
    ::log::logsubst debug {\tHTTP error [array get $token]}
    
    ::http::cleanup $token
    return -errorcode [list WS CLIENT HTTPERROR $error]\
            -code error "HTTP error: $error"
}

###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::Utils::check_version
#
# Description : for a particular version code, check if the requested version
#   is allowd
#
# Arguments :
#       version - The specified version for the type, proc, etc
#       requestedVersion - The version being requested by the user
#
# Returns :     boolean - true if allowed, false if not
#
# Side-Effects :        None
#
# Exception Conditions :        None
#
# Pre-requisite Conditions :    None
#
# Original Author : Jonathan Cone
#
#>>END PRIVATE<<
#
# Maintenance History - as this file is modified, please be sure that you
#                       update this segment of the file header block by
#                       adding a complete entry at the bottom of the list.
#
# Version     Date     Programmer   Comments / Changes / Reasons
# -------  ----------  ----------   -------------------------------------------
#       1  10/23/2018  J. Cone       Initial version
#
###########################################################################
proc ::WS::Utils::check_version {version requestedVersion} {
    if {$version eq {} || $requestedVersion eq {}} {
        return 1
    }
    if {[regexp {^(\d+)([+-])?(\d+)?$} $version _ start modifier end]} {
        if {$start ne {} && $end ne {}} {
            return [expr {$requestedVersion >= $start && $requestedVersion <= $end}]
        } elseif {$start ne {}} {
            switch -- $modifier {
                "+" {
                    return [expr {$requestedVersion >= $start}]
                }
                "-" {
                    return [expr {$requestedVersion <= $start}]
                }
                "" {
                    return [expr {$requestedVersion == $start}]
                }
            }
        }
    }
    return 0
}

