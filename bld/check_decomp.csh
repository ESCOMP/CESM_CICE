#!/bin/csh

set res = gx1v6
set n = 0
set nmax = 400
set file = $res.$nmax

echo "check_decomp on $res from $n to $nmax pes:" >! $file
echo "" >>& $file

while ($n < $nmax)

 @ n++
 set config = `./generate_cice_decomp.pl -res $res -nproc $n`

 if ($config[1] >= 0) then
    echo $res $n : $config  >>& $res.$nmax
 endif

end
