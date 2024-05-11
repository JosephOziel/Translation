USING: arrays grouping kernel math math.functions multiline peg peg.ebnf random ranges sequences sets strings ;

IN: translation

! <PRIVATE

CONSTANT: mut-rate 1/3 ! rate of mutation

: num-muts ( length -- freq ) mut-rate * floor ;

: check ( code -- verified_code ) 
    dup
    [ 3 0 spin <slice> >string "AUG" = ]
    [ dup length over length 3 - spin <slice> >string { "UGA" "UAA" "UAG" } member? ] ! maybe also add the two letter one
    bi and swap '[ _ ] [ "Program should start and end with the proper codons." throw ] if
;

: process ( code -- processed ) "UACG" within ; ! 3 group

: mutation ( symbols -- mutated ) 
!    "" join
    dup length mut-rate * over length 4 - randoms [ dup 3 < [ 3 + ] when ] map
    [
        { 
          [ "UABG" random spin insert-nth ] ! insertion
          [ swap remove-nth ] ! deletion
          [ "UABG" random spin [ clone set-nth ] keep ] ! substitution
        } random call( x y -- z )
    ] each 3 group " " join
; 

: compose-all ( seq -- quot )
    [ ] [ compose ] reduce ;

EBNF: parse-translation [=[
    start = "AUG"
    phe = "UUU" | "UUC"                                 => [[ [ phe ] ]]
    leu = "UUA" | "UUG" | "CUU" | "CUC" | "CUA" | "CUG" => [[ [ leu ] ]]
    ile = "AUU" | "AUC" | "AUA"                         => [[ [ ile ] ]]
    met = "AUG"                                         => [[ [ met ] ]]
    val = "GUU" | "GUC" | "GUA" | "GUG"                 => [[ [ val ] ]]
    ser = "UCU" | "UCC" | "UCA" | "UCG" | "AGU" | "AGC" => [[ [ ser ] ]]
    pro = "CCU" | "CCC" | "CCA" | "CCG"                 => [[ [ pro ] ]]
    thr = "ACU" | "ACC" | "ACA" | "ACG"                 => [[ [ thr ] ]]
    ala = "GCU" | "GCC" | "GCA" | "GCG"                 => [[ [ ala ] ]]
    tyr = "UAU" | "UAC"                                 => [[ [ tyr ] ]]
    his = "CAU" | "CAC"                                 => [[ [ his ] ]]
    gln = "CAA" | "CAG"                                 => [[ [ gln ] ]]
    asn = "AAU" | "AAC"                                 => [[ [ asn ] ]]
    lys = "AAA" | "AAG"                                 => [[ [ lys ] ]]
    asp = "GAU" | "GAC"                                 => [[ [ asp ] ]]
    glu = "GAA" | "GAG"                                 => [[ [ glu ] ]]
    cys = "UGU" | "UGC"                                 => [[ [ cys ] ]]
    trp = "UGG"                                         => [[ [ trp ] ]]
    arg = "CGU" | "CGC" | "CGA" | "CGG" | "AGA" | "AGG" => [[ [ arg ] ]]
    gly = "GGU" | "GGC" | "GGA" | "GGG"                 => [[ [ gly ] ]]
    end = ("U" | "A" | "G" | "C")+ ?[ length 2 <= ]? 

    amino-acid = phe|leu|ile|met|val|ser|pro|thr|ala|tyr|his|gln|asn|lys|asp|glu|cys|trp|arg|gly
    space = " "
    code = start~ space~ (amino-acid space~)+ end~      => [[ compose-all ]]
]=]

! PRIVATE>