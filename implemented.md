EXPANSIONS

$fname $vname $tname
$ftype        $ttype

${tmeta(...)} ${vmeta(...)} ${fmeta(...)}
  like this:
${tmeta(name)}
${tmeta(name) as ty}
${tmeta(name(in(depth)))}
  ^ mirrors #[derive(adhoc(name(in(depth))))]

$tattrs $vattrs $fattrs
${tattrs attrname, attrname, ...}
${tattrs ! attrname, attrname, ...}
${tattrs = attrname, attrname, ...}

$keyword
${keyword}
${keyword ...}
# as applicable

$( )
   repetition automatically tells what to loop over
   by scanning for expansions eg $fname

$( ${when CONDITION} ... )
${if ... { ... } else if COND2 { ... } else { ... }}
${for fields { ... }


EXPRESSIONS

false
true
not(...)
all(...)
any(...)
