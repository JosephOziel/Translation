USING: arrays command-line grouping io io.encodings.binary io.files kernel math math.functions math.parser multiline namespaces peg peg.ebnf random ranges sequences sets splitting strings ;
FROM: namespaces => set ;

IN: translation

! <PRIVATE

CONSTANT: mut-rate 1/3 ! rate of mutation

: num-muts ( length -- freq ) mut-rate * floor ;

ERROR: incorrect-start-end-codons program ; ! "Program should start and end with the proper codons."

: check ( code -- verified_code ) 
    dup
    [ "AUG" head? ]
    [ 3 tail* { "UGA" "UAA" "UAG" } member? ] ! maybe also add the two letter one
    bi and [ incorrect-start-end-codons ] unless ;

: process ( code -- processed ) "UACG" within ; ! 3 group

: mutation ( symbols -- mutated ) 
!    "" join
    dup length num-muts over length 4 - randoms [ dup 3 < [ 3 + ] when ] map
    [
        { 
          [ "UACG" random spin insert-nth ] ! insertion
          [ swap remove-nth ] ! deletion
          [ "UACG" random spin [ clone set-nth ] keep ] ! substitution
        } random call( x y -- z )
    ] each 3 group " " join ; 

: compose-all ( seq -- quot )
    [ ] [ compose ] reduce ;

: get-2 ( seq -- seq second first )
    2 cut* first2 ;

: get-1 ( seq -- seq first )
    1 cut* first ;

! No structure for the stack, since there will be no environment I think

: phe ( stack -- stack ) [ read1 suffix ] suffix ; ! read a character

: leu ( stack -- stack ) 1 suffix ; ! push 1 

: ile ( stack -- stack ) [ get-1 write1 ] suffix ; ! print character and pop it

: met ( stack -- stack ) 2 suffix ; ! push 2 ;

: val ( stack -- stack ) [ get-2 dip suffix ] suffix ; ! dip 

: ser ( stack -- stack ) get-2 swap [ call( stack -- stack ) ] curry times ; inline ! quote n repn

: pro ( stack -- stack ) [ get-2 compose suffix ] suffix ; ! compose

: thr ( stack -- stack ) [ get-1 drop ] suffix ; ! zap

: ala ( stack -- stack ) [ get-1 dup 2array append ] suffix ; ! dup

: tyr ( stack -- stack ) [ get-2 over 3array append ] suffix ; ! over

: his ( stack -- stack ) [ ] suffix ; ! push empty quote

: gln ( stack -- stack ) [ get-2 curry suffix ] suffix ; ! cons

: asn ( stack -- stack ) [ get-2 + suffix ] suffix ; ! +

: lys ( stack -- stack ) [ get-2 - suffix ] suffix ; ! -

: asp ( stack -- stack ) [ get-2 * suffix ] suffix ; ! *

: glu ( stack -- stack ) [ get-2 / suffix ] suffix ; ! /

: cys ( stack -- stack ) [ get-2 ^ suffix ] suffix ; ! ^

: trp ( stack -- stack ) [ get-1 >dec write ] suffix ; ! print number and pop it

: arg ( stack -- stack ) get-1 call( stack -- stack ) ; inline ! i/call

: gly ( stack -- stack ) [ get-2 swap 2array append ] suffix ; ! swap

! unknown codons are ignored
EBNF: parse-translation [=[
    start = "AUG"
    phe = ("UUU" | "UUC")                                 => [[ [ phe ] ]]
    leu = ("UUA" | "UUG" | "CUU" | "CUC" | "CUA" | "CUG") => [[ [ leu ] ]]
    ile = ("AUU" | "AUC" | "AUA")                         => [[ [ ile ] ]]
    met = "AUG"                                           => [[ [ met ] ]]
    val = ("GUU" | "GUC" | "GUA" | "GUG")                 => [[ [ val ] ]]
    ser = ("UCU" | "UCC" | "UCA" | "UCG" | "AGU" | "AGC") => [[ [ ser ] ]]
    pro = ("CCU" | "CCC" | "CCA" | "CCG")                 => [[ [ pro ] ]]
    thr = ("ACU" | "ACC" | "ACA" | "ACG")                 => [[ [ thr ] ]]
    ala = ("GCU" | "GCC" | "GCA" | "GCG")                 => [[ [ ala ] ]]
    tyr = ("UAU" | "UAC")                                 => [[ [ tyr ] ]]
    his = ("CAU" | "CAC")                                 => [[ [ his ] ]]
    gln = ("CAA" | "CAG")                                 => [[ [ gln ] ]]
    asn = ("AAU" | "AAC")                                 => [[ [ asn ] ]]
    lys = ("AAA" | "AAG")                                 => [[ [ lys ] ]]
    asp = ("GAU" | "GAC")                                 => [[ [ asp ] ]]
    glu = ("GAA" | "GAG")                                 => [[ [ glu ] ]]
    cys = ("UGU" | "UGC")                                 => [[ [ cys ] ]]
    trp = "UGG"                                           => [[ [ trp ] ]]
    arg = ("CGU" | "CGC" | "CGA" | "CGG" | "AGA" | "AGG") => [[ [ arg ] ]]
    gly = ("GGU" | "GGC" | "GGA" | "GGG")                 => [[ [ gly ] ]]
    unknown = ("U" | "A" | "G" | "C")+ ?[ length 3 <= ]?  => [[ [ ] ]]

    amino-acid = phe|leu|ile|met|val|ser|pro|thr|ala|tyr|his|gln|asn|lys|asp|glu|cys|trp|arg|gly|unknown
    space = " "
    code = start~ space~ (amino-acid space~)+ unknown~      => [[ compose-all ]]
]=]

! PRIVATE>

! maybe add debug version that prints stacl at the end too
MACRO: run-translation ( code -- quot )
    check process mutation parse-translation '[ { } @ drop flush ] ;

MACRO: run-translation-nomut ( code -- quot )
    check process 3 group " " join parse-translation '[ { } @ drop flush ] ;

: get-translation ( code -- result )
    [ run-translaton ] with-string-writer ; inline

<PRIVATE

: (run-translation) ( code -- )
    check process "nomut" get [ 3 group " " join ] [ mutation ] if parse-translation { } swap call( stack -- stack ) drop flush ;

PRIVATE>

! FIX BUG DOESN'T WORK. GIT CLONE FACTOR AND GET LATEST RELEASE
: translation-main ( -- )
    command-line get [ "-" ?head ] partition
    [ [ set ] each ] dip [
        read-contents (run-translation)
    ] [
        [ binary file-contents (run-translation) ] each
    ] if-empty ; 

MAIN: translation-main