$fname
$vname
$tname

${tmeta(...)}
${vmeta(...)}
${fmeta(...)}
# like this
${tmeta(name)}
${tmeta(name(in(depth)))}
# ^ mirrors #[derive(adhoc(name(in(depth))))]

$tattrs
$vattrs
$fattrs
${tattrs attrname, attrname, ...}
${tattrs ! attrname, attrname, ...}
${tattrs = attrname, attrname, ...}

$keyword
${keyword}
${keyword ...}
# as applicable

$( )
# automatic iteration determination by scanning for expansions eg $fname

${when }


expressions

false
true



actually expressions and substitutions have the same keyword namespace
and the distinction is semantic
