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

package require WS::Client

##
## Get Definition of the offered services
##
::WS::Client::GetAndParseWsdl http://localhost:8015/service/wsMathExample/wsdl

##
## Add two numbers
##
puts stdout "Calling Add via DoCalls!"
set inputs [list N1 12 N2 34]
set results [::WS::Client::DoCall wsMathExample Add $inputs]
puts stdout "\t Received: {$results}"

##
## Divide two numbers
##
puts stdout "Calling Divide via DoCalls!"
set inputs [list Dividend 34 Divisor 12]
set results [::WS::Client::DoCall wsMathExample Divide $inputs]
puts stdout "\t Received: {$results}"

##
## Multiply two numbers
##
puts stdout "Calling Multiply via DoCalls!"
set inputs [list N1 12.0 N2 34]
set results [::WS::Client::DoCall wsMathExample Multiply $inputs]
puts stdout "\t Received: {$results}"

##
## Subtract two numbers
##
puts stdout "Calling Subtract via DoCalls!"
set inputs [list Subtrahend 12 Minuend 34]
set results [::WS::Client::DoCall wsMathExample Subtract $inputs]
puts stdout "\t Received: {$results}"

##
## Sqrt a number
##
puts stdout "Calling Sqrt via DoCalls!"
set inputs [list X 12]
set results [::WS::Client::DoCall wsMathExample Sqrt $inputs]
puts stdout "\t Received: {$results}"



##
##  Set up to evaluate a polynomial
##
dict set term var X
dict set term value 2.0
dict lappend varList $term
dict set term var Y
dict set term value 3.0
dict lappend varList $term


set term {}
set powerTerm {}
dict set powerTerm coef 2.0
dict set term var X
dict set term pow 2.0
dict lappend terms $term
dict set term var Y
dict set term pow 3.0
dict lappend terms $term
dict set powerTerm powerTerms $terms


dict set powerTerm coef -2.0
dict set term var X
dict set term pow 3.0
dict lappend terms $term
dict set term var Y
dict set term pow 2.0
dict lappend terms $term
dict set powerTerm powerTerms $terms
dict lappend polynomial powerTerms $powerTerm


dict set input varList $varList
dict set input polynomial $polynomial
##
## Call service
##
puts stdout "Calling EvaluatePolynomial with {$input}"
set resultsDict [::WS::Client::DoCall wsMathExample EvaluatePolynomial $input]
puts stdout "Results are {$resultsDict}"
