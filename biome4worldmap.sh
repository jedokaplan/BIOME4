#!/usr/bin/env bash

gmt gmtset MAP_FRAME_TYPE plain
gmt gmtset MAP_FRAME_PEN thinnest,black
gmt gmtset PS_MEDIA a2
gmt gmtset FONT_ANNOT 7p,Helvetica,black
gmt gmtset FONT_LABEL 7p,Helvetica,black
gmt gmtset FONT_TITLE 9p,Helvetica,black
gmt gmtset MAP_TITLE_OFFSET 0

naturalearth=/home/terraces/datasets/naturalearth

ocean=$naturalearth/ne_50m_ocean_blocks.gmt
lakes=$naturalearth/ne_50m_lakes.gmt

legend=biome4legend.cpt

# ---

output=biome4worldmap.ps

infile=${1}

gmt psbasemap -R-169/191/-90/90 -JQ25 -B0 -B+t"6000 BP vegetation" -P -K > $output

gmt grdimage $infile?biome -R -J -C$legend -nn -O -P -K >> $output

gmt psxy $lakes -R -J -Gslategray1 -O -P -K >> $output
 
gmt psxy $ocean -R -J -Gslategray1 -B0 -O -P -K >> $output

gmt psscale -R -J -DJMR+r+o0.3/0 -C$legend -L0.1 -By+l"        BIOMES" -O -P >> $output

gmt psconvert -A -Tf -Z $output
