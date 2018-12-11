#!/bin/bash
# create pstex and pstex_t files from the fig files in this directory

for file in *.fig
do
    if [ -f $file ] ; then
        name=${file%\.*}
        fig2dev -L pstex -m 0.45 $file $name.pstex
        fig2dev -L pstex_t -m 0.45 $file $name.pstex_t_2

        echo "\begin{picture}(0,0)%" > $name.pstex_t
        echo "\includegraphics{$name.pstex}%" >> $name.pstex_t
        echo "\end{picture}%" >> $name.pstex_t

        cat $name.pstex_t_2 >> $name.pstex_t
    fi ;
done

latex figConvert.tex
latex figConvert.tex
latex figConvert.tex

count=1
for file in *.fig
do
    if [ -f $file ] ; then
        name=${file%\.*}
        dvips -p$count -n1 -E -o ../finishedFigs/$name.eps figConvert.dvi
        epstopdf ../finishedFigs/$name.eps
        let count++
    fi ;
done

rm *.pstex*
