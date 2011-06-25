###############################################################################
##                                                                           ##
##  Copyright (c) 2006, Visiprise Software, Inc                              ##
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

package require WS::Server
package require WS::Utils

##
## Define the service
##
::WS::Server::Service \
    -service wsMathExample \
    -description  {Math Example - Tcl Web Services} \
    -host         $::Config(host):$::Config(port)


##
## Define any special types
##
::WS::Utils::ServiceTypeDef Server wsMathExample Term {
   `coef         {type float}
    powerTerms   {type PowerTerm()}
}
::WS::Utils::ServiceTypeDef Server wsMathExample PowerTerm {
    var          {type string}
    exponet      {type float}
}
::WS::Utils::ServiceTypeDef Server wsMathExample Variables {
    var          {type string}
    value        {type float}
}


##
## Define the operations available
##
::WS::Server::ServiceProc \
    wsMathExample \
    {Add {type string comment {Sum of two number}}} \
    {
        N1      {type double comment {First number to add}}
        N2      {type double comment {Second number to add}}
    } \
    {Add two numbers} {

    return [list AddResult [expr {$N1 + $N2}]]
}

::WS::Server::ServiceProc \
    wsMathExample \
    {Subtract {type string comment {Difference of two number}}} \
    {
        Minuend      {type double comment {Number to subtrack from}}
        Subtrahend   {type double comment {Number to be subtracked}}
    } \
    {Subtract one number from another} {

    return [list SubtractResult [expr {$Minuend - $Subtrahend}]]
}

::WS::Server::ServiceProc \
    wsMathExample \
    {Multiply {type string comment {Product of two number}}} \
    {
        N1      {type double comment {First number to multiply}}
        N2      {type double comment {Second number to multiply}}
    } \
    {Multiply two numbers} {

    return [list MultiplyResult [expr {$N1 * $N2}]]
}

::WS::Server::ServiceProc \
    wsMathExample \
    {Divide {type string comment {Quotient of two number}}} \
    {
        Dividend  {type double comment {Number that is being divided}}
        Divisor   {type double comment {Number dividing}}
    } \
    {Divide one number by another} {

    if {$Divisor == 0.0} {
        return \
            -code error \
            -errorcode [list MATH DIVBYZERO] \
            "Can not divide by zero"
    }

    return [list DivideResult [expr {$Dividend / $Divisor}]]
}

::WS::Server::ServiceProc \
    wsMathExample \
    {Sqrt {type string comment {Square root of a non-negative number}}} \
    {
        X  {type double comment {Number raised to the half power}}
    } \
    {The the square root of a number} {

    if {$X < 0.0} {
        return \
            -code error \
            -errorcode [list MATH RANGERR] \
            "Can not take the square root of a negative number, $X"
    }

    return [list SqrtResult [expr {sqrt($X)}]]
}

##
## Define the operations available
##
::WS::Server::ServiceProc \
     wsMathExample \
     {EvaluatePolynomial {type float comment {Result of evaluating a polynomial}}} \
     {
         varList       {type Variables() comment {The variables to be substitued into the polynomial}}
         polynomial    {type Term() comment {The polynomial}}
     } \
     {Evaluate a polynomial} {
     set equation {0 }
     foreach varDict $varList {
         set var dict get $varDict var
         set val dict get $varDict value
         set vars($var) $val
     }
     foreach term $polynomial {
         if {dict exists $term coef} {
             set coef dict get $term coef
         } else {
             set coef 1
         }
         append equation "+ ($coef"
         foreach pow dict get $term powerTerms {
             if {dict exists $pow exponet} {
                 set exp dict get $pow exponet
             } else {
                 set exp 1
             }
             append equation format { * pow($vars(%s),%s} [dict get $pow var $exp]
         }
         append equation ")"
     }
     set result expr $equation
     return list SimpleEchoResult $result
 }

