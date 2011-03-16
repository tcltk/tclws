###############################################################################
##                                                                           ##
##  Copyright (c) 2006, Arnulf Wiedemann
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

package provide WS::CheckAndBuild 0.0.3

if {![llength [info command dict]]} {
    package require dict
}
package require tdom
package require log

namespace eval ::WS::CheckAndBuild {
        variable resultTree
        variable currNode
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
#       tagName     - The name of the starting tag
#       xmlString   - The XML string to validate
#       typeInfos   - The types infos
#
# Returns :     1 if valition ok, 0 if not
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
#       1  08/14/2006  A.Wiedemann  Initial version
#
#
###########################################################################
proc ::WS::CheckAndBuild::Validate {mode serviceName tagName xmlString typeInfos} {
        variable resultTree
        variable currNode

        set startInfos [dict get $typeInfos types $tagName]
        dom parse $xmlString resultTree
        $resultTree documentElement currNode
        set nodeName [$currNode nodeName]
        if {![string equal $nodeName $tagName]} {
                return \
                    -code error \
                    -errorcode [list WS CHECK START_NODE_DIFFERS [list $tagName $nodeName]] \
                    "start node differs expected: $tagName found: $nodeName"
        }
        return [checkTags $mode $serviceName $startInfos $typeInfos]
}


###########################################################################
#
# Public Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PUBLIC<<
#
# Procedure Name : ::WS::CheckAndBuild::BuildRequest
#
# Description : Given a schema check the body of a request handed in
#               as a XML string using a XML schema description (in WS:: form)
#               for validation
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       tagName     - The name of the starting tag
#       typeInfos   - The types infos
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
#
#
###########################################################################
proc ::WS::CheckAndBuild::BuildRequest {mode serviceName tagName typeInfos valueInfos} {
        upvar $valueInfos values
        variable resultTree
        variable currNode

        set startInfos [dict get $typeInfos types $tagName]
        set resultTree [::dom createDocument $tagName]
        $resultTree documentElement currNode
        buildTags $mode $serviceName $startInfos $typeInfos $valueInfos
        return [$resultTree asXML]
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::CheckAndBuild::buildValue
#
# Description : Check a Value to be put into a tag pair according to the
#               XML schema description
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The name of the service
#       key         - The element to handle
#       typeInfo    - The type info for the element to handle
#       valueInfos  - The name of the array with the values
#
# Returns :     The value or an error if checking not ok
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
#
#
###########################################################################
proc ::WS::CheckAndBuild::buildValue {mode serviceName key typeInfo valueInfos} {
        upvar $valueInfos values

        catch {unset typeInfos}
        array set typeInfos [list \
                minLength 0 \
                maxLength -1 \
                minOccurs 0 \
                maxOccurs -1 \
                fixed false \
                length -1 \
        ]
        array set typeInfos $typeInfo

        set val ""
        set gotVal 0
        if {[info exists values($key)]} {
                set val $values($key)
                set gotVal 0
        }
        set minLength $typeInfos(minLength)
        set maxLength $typeInfos(maxLength)
        set minOccurs $typeInfos(minOccurs)
        set maxOccurs $typeInfos(maxOccurs)
        set fixed $typeInfos(fixed)
        if {$minOccurs > 0} {
                if {!$gotVal} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_NOT_MATCHES_PATTERN [list $key $val $pattern]] \
                            "No value for '$key' which is mandatory typeInfo:$typeInfo:"
                }
        }
        if {$minLength >= 0} {
                if {[string length $val] < $minLength} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_TO_SHORT [list $key $val $minLength $typeInfo]] \
                            "Value for $key: '$val' is too short, minLength: $minLength:"
                }
        }
        if {$maxLength >= 0} {
                if {[string length $val] > $maxLength} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_TO_LONG [list $key $val $maxLength $typeInfo]] \
                            "Value for $key: '$val' is too long, maxLength: $maxLength:"
                }
        }
        set haveEnumeration 0
        set isOk 0
        set enumerationVals [list]
        set enumerationInfos [dict get $typeInfo enumeration]
        foreach {typeKey typeVal} $enumerationInfos {
                set haveEnumeration 1
                lappend enumerationVals $typeVal
                if {[string equal $val $typeVal]} {
                        set isOk 1
                }
        }
        if {$haveEnumeration && $fixed} {
                return [lindex $enumerationVals 0]
        }
        if {$haveEnumeration && ! $isOk} {
                return \
                    -code error \
                    -errorcode [list WS CHECK VALUE_NOT_IN_ENUMERATION [list $key $val $enumerationVals $typeInfo]] \
                    "Value for $key: '$val' is not in enumeration values: '$enumerationVals':"
        }
        if {[info exists typeInfos(pattern)]} {
                set pattern $typeInfos(pattern)
                if {! [regexp $pattern $val]} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_NOT_MATCHES_PATTERN [list $key $val $pattern $typeInfo]] \
                            "Value for $key: '$val' does not match pattern: '$pattern':"
                }
        }
        return $val
}


###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::CheckAndBuild::buildTags
#
# Description : Recursivly build the tags by checking the values to put
#               inside the tags and append to the dom tree resultTree
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       startInfos  - The infos for the current tag
#       typeInfos   - The types infos
#       valueInfos  - The name of the array with the values
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
#
#
###########################################################################
proc ::WS::CheckAndBuild::buildTags {mode serviceName startInfos typeInfos valueInfos} {
        upvar $valueInfos values
        variable resultTree
        variable currNode

        foreach {key value} $startInfos {
                lappend keyList $key
        }
        foreach entry $keyList {
                foreach {key dummy} $entry break
                if {[dict exists $startInfos $key type]} {
                        set allDone 0
                        if {[info exists ::WS::Parse::simpleTypes($key)]} {
                                if {![info exists ::WS::Parse::simpleTypes($mode,$serviceName,$key)]} {
                                        set typeInfo [list type $key]
                                        set val [buildValue $mode $serviceName $key $typeInfo $valueInfos]
                                        $currNode appendChild [$resultTree createElement $key node]
                                        $node appendChild [$resultTree createTextNode $val]
                                        set all_done 1
                                }
                        }
                        if {!$allDone} {
                                set typeName [dict get $startInfos $key type]
                                set typeName [string trimright $typeName "()"]
                                if {[dict exists $typeInfos types $typeName]} {
                                        set subStartInfos [dict get $typeInfos types $typeName]
                                        set saveNode $currNode
                                        $currNode appendChild [$resultTree createElement $key currNode]
                                        buildTags $mode $serviceName $subStartInfos $typeInfos $valueInfos
                                        set currNode $saveNode
                                } else {
                                        set simpleTypeInfos [::WS::Utils::GetServiceSimpleTypeDef $mode $serviceName $typeName]
                                        set val [buildValue $mode $serviceName $key $simpleTypeInfos $valueInfos]
                                        $currNode appendChild [$resultTree createElement $key node]
                                        $node appendChild [$resultTree createTextNode $val]
                                }
                        }
                } else {
                        return \
                            -code error \
                            -errorcode [list WS CHECK SIMPLE_TYPES2_NOT_IMPLEMENTED [list $key $startInfos]] \
                            "simple type 2 part not yet implemented (in handling key: $key startInfos: $startInfos:"
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
# Procedure Name : ::WS::CheckAndBuild::checkValue
#
# Description : Check a Value between tags of a XML document against the
#               type in the XML schema description
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The name of the service
#       key         - The element to handle
#       value       - The value to check
#       typeInfo    - The type info for the element to handle
#
# Returns :     1 if ok or 0 if checking not ok
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
#       1  08/14/2006  A.Wiedemann  Initial version
#
#
###########################################################################
proc ::WS::CheckAndBuild::checkValue {mode serviceName key value typeInfo} {

        catch {unset typeInfos}
        array set typeInfos [list \
                minLength 0 \
                maxLength -1 \
                minOccurs 0 \
                maxOccurs -1 \
                fixed false \
                length -1 \
        ]
        array set typeInfos $typeInfo
        set minLength $typeInfos(minLength)
        set maxLength $typeInfos(maxLength)
        set minOccurs $typeInfos(minOccurs)
        set maxOccurs $typeInfos(maxOccurs)
        set fixed $typeInfos(fixed)
        if {$minOccurs > 0} {
                if {[string length $value] == 0} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_NOT_MATCHES_PATTERN [list $key $value $pattern]] \
                            "No value for $key which is mandatory typeInfo:$typeInfo:"
                }
        }
        if {$minLength >= 0} {
                if {[string length $value] < $minLength} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_TO_SHORT [list $key $value $minLength $typeInfo]] \
                            "Value for $key: '$value' is too short, minLength: $minLength:"
                }
        }
        if {$maxLength >= 0} {
                if {[string length $value] > $maxLength} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_TO_LONG [list $key $value $maxLength $typeInfo]] \
                            "Value for $key: '$value' is too long, maxLength: $maxLength:"
                }
        }
        set haveEnumeration 0
        set isOk 0
        set enumerationVals [list]
        set enumerationInfos [dict get $typeInfo enumeration]
        foreach {typeKey typeVal} $enumerationInfos {
                set haveEnumeration 1
                lappend enumerationVals $typeVal
                if {[string equal $value $typeVal]} {
                        set isOk 1
                }
        }
        if {$haveEnumeration && ! $isOk} {
                return \
                    -code error \
                    -errorcode [list WS CHECK VALUE_NOT_IN_ENUMERATION [list $key $value $enumerationVals $typeInfo]] \
                    "Value for $key: '$value' is not in enumeration values: '$enumerationVals':"
        }
        if {[info exists typeInfos(pattern)]} {
                set pattern $typeInfos(pattern)
                if {! [regexp $pattern $value]} {
                        return \
                            -code error \
                            -errorcode [list WS CHECK VALUE_NOT_MATCHES_PATTERN [list $key $value $pattern $typeInfo]] \
                            "Value for $key: '$value' does not match pattern: '$pattern':"
                }
        }
        return 1
}



###########################################################################
#
# Private Procedure Header - as this procedure is modified, please be sure
#                           that you update this header block. Thanks.
#
#>>BEGIN PRIVATE<<
#
# Procedure Name : ::WS::CheckAndBuild::checkTags
#
# Description : Recursivly check the tags and values inside the tags
#
# Arguments :
#       mode        - Client/Server
#       serviceName - The service name
#       startInfos  - The infos for the current tag
#       typeInfos   - The types infos
#
# Returns :     1 if ok, 0 otherwise
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
#
#
###########################################################################
proc ::WS::CheckAndBuild::checkTags {mode serviceName startInfos typeInfos} {
        variable resultTree
        variable currNode

        foreach {key value} $startInfos {
                lappend keyList $key
        }
        set node [$currNode firstChild]
        set lastNode [$currNode lastChild]
        foreach entry $keyList {
                foreach {key dummy} $entry break
                if {[dict exists $startInfos $key type]} {
                        set allDone 0
                        if {[info exists ::WS::Parse::simpleTypes($key)]} {
                                if {![info exists ::WS::Parse::simpleTypes($mode,$serviceName,$key)]} {
                                        set typeInfo [list type $key]
                                        if {[$node hasChildNodes]} {
                                                set textNode [$node firstChild]
                                                set value [$textNode nodeValue]
                                        } else {
                                                # there is no text node so set value to empty
                                                set value ""
                                        }
                                        checkValue $mode $serviceName $key $value $typeInfo
                                        set all_done 1
                                }
                        }
                        if {!$allDone} {
                                set typeName [dict get $startInfos $key type]
                                set typeName [string trimright $typeName "()"]
                                if {[dict exists $typeInfos types $typeName]} {
                                        set subStartInfos [dict get $typeInfos types $typeName]
                                        set currNode $node
                                        checkTags $mode $serviceName $subStartInfos $typeInfos
                                        set node $currNode
                                } else {
                                        set simpleTypeInfos [::WS::Utils::GetServiceSimpleTypeDef $mode $serviceName $typeName]
                                        if {[$node hasChildNodes]} {
                                                set textNode [$node firstChild]
                                                set value [$textNode nodeValue]
                                        } else {
                                                # there is no text node so set value to empty
                                                set value ""
                                        }
                                        checkValue $mode $serviceName $key $value $simpleTypeInfos
                                }
                        }
                } else {
                        return \
                            -code error \
                            -errorcode [list WS CHECK SIMPLE_TYPES2_NOT_IMPLEMENTED [list $key $startInfos]] \
                            "simple type 2 part not yet implemented (in handling key: $key startInfos: $startInfos:"
                }
                if {$node != $lastNode} {
                        set node [$node nextSibling]
                }
        }
        return 1
}

