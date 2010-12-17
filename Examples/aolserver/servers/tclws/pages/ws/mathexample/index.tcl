package require WS::AOLserver

::WS::AOLserver::Init


##
## Define the service
##
::WS::Server::Service \
    -service $service \
    -mode aolserver \
    -prefix $prefix \
    -description  {Math Example - Tcl Web Services} \
    -host         $host \
    -ports        $port

##
## Define the operations available
##
::WS::Server::ServiceProc \
    $service \
    {Add {type string comment {Sum of two number}}} \
    {
        N1      {type double comment {First number to add}}
        N2      {type double comment {Second number to add}}
    } \
    {Add two numbers} {

    return [list AddResult [expr {$N1 + $N2}]]
}

::WS::Server::ServiceProc \
    $service \
    {Subtract {type string comment {Difference of two number}}} \
    {
        Minuend      {type double comment {Number to subtrack from}}
        Subtrahend   {type double comment {Number to be subtracked}}
    } \
    {Subtract one number from another} {

    return [list SubtractResult [expr {$Minuend - $Subtrahend}]]
}

::WS::Server::ServiceProc \
    $service \
    {Multiply {type string comment {Product of two number}}} \
    {
        N1      {type double comment {First number to multiply}}
        N2      {type double comment {Second number to multiply}}
    } \
    {Multiply two numbers} {

    return [list MultiplyResult [expr {$N1 * $N2}]]
}

::WS::Server::ServiceProc \
    $service \
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

    return [list DivideResult [expr {$Dividend + $Divisor}]]
}

::WS::Server::ServiceProc \
    $service \
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

::WS::AOLserver::Return