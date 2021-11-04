
IF NOT CONNECTED(TRIM("mgcam ")) THEN CONNECT -db camil    -ld mgcam    -S 17600 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("mgcad ")) THEN CONNECT -db ems2cad  -ld mgcad    -S 17608 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("mgdis ")) THEN CONNECT -db ems2dis  -ld mgdis    -S 17610 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("mgfis ")) THEN CONNECT -db ems2fis  -ld mgfis    -S 17612 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("movmov")) THEN CONNECT -db mov2mov  -ld movmov   -S 17654 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("movdis")) THEN CONNECT -db mov2dis  -ld movdis   -S 17648 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("movfis")) THEN CONNECT -db mov2fis  -ld movfis   -S 17650 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("movind")) THEN CONNECT -db mov2ind  -ld movind   -S 17652 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("ems5  ")) THEN CONNECT -db ems5     -ld ems5     -S 17622 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("emsca ")) THEN CONNECT -db emsca    -ld emsca    -S 17626 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("emscom")) THEN CONNECT -db emscom   -ld emscom   -S 17628 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("emsfnd")) THEN CONNECT -db emsfnd   -ld emsfnd   -S 17634 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("emsfin")) THEN CONNECT -db emsfin   -ld emsfin   -S 17632 -H zemina -N tcp.
IF NOT CONNECTED(TRIM("movfin")) THEN CONNECT -db movfin   -ld movfin   -S 17656 -H zemina -N tcp.
