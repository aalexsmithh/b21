#!/bin/bash

#
# TS_NAME="Jacob Errington"
# TS_ID="260636023"
# TS_DEPT="Office of Student Life and Learning"
# TS_SUNDAY="2018-01-foo"
# TS_SATURDAY="2018-01-bar"
# TS_RATE="15"

TS_SIG="$TS_NAME                    $(date "+%d %b %Y")"

# TS_IN_SU=""
# TS_OUT_SU=""
TS_OFF_SU=""
#TS_TOT_SU=1

# TS_IN_MO=""
# TS_OUT_MO=""
TS_OFF_MO=""
#TS_TOT_MO=2

# TS_IN_TU=""
# TS_OUT_TU=""
TS_OFF_TU=""
#TS_TOT_TU=3

# TS_IN_WE=""
# TS_OUT_WE=""
TS_OFF_WE=""
#TS_TOT_WE=4

# TS_IN_TH=""
# TS_OUT_TH=""
TS_OFF_TH=""
#TS_TOT_TH=5

# TS_IN_FR=""
# TS_OUT_FR=""
TS_OFF_FR=""
#TS_TOT_FR=6

# TS_IN_SA=""
# TS_OUT_SA=""
TS_OFF_SA=""
#TS_TOT_SA=7

TS_TOTAL_H=$(
    echo "$TS_TOT_SU + $TS_TOT_MO + $TS_TOT_TU + $TS_TOT_WE" \
        "+ $TS_TOT_TH + $TS_TOT_FR + $TS_TOT_SA" | bc
)

TS_TOTAL_MONEY=$(echo "$TS_RATE * $TS_TOTAL_H" | bc)

convert "${TS_BASE}" \
    -fill black \
    -pointsize  48\
    -font 'DejaVu-Sans' \
    -annotate +1000+695 "$TS_NAME" \
    -annotate +2320+695 "$TS_ID" \
    -annotate +1000+900 "$TS_DEPT" \
    -annotate +1250+1125 "$TS_SUNDAY" \
    -annotate +2250+1125 "$TS_SATURDAY" \
    -annotate +450+2635 "$TS_SIG" \
    \
    -annotate +1300+1570 "$TS_IN_SU" \
    -annotate +1300+1695 "$TS_IN_MO" \
    -annotate +1300+1820 "$TS_IN_TU" \
    -annotate +1300+1935 "$TS_IN_WE" \
    -annotate +1300+2060 "$TS_IN_TH" \
    -annotate +1300+2185 "$TS_IN_FR" \
    -annotate +1300+2300 "$TS_IN_SA" \
    \
    -annotate +1545+1570 "$TS_OUT_SU" \
    -annotate +1545+1695 "$TS_OUT_MO" \
    -annotate +1545+1820 "$TS_OUT_TU" \
    -annotate +1545+1935 "$TS_OUT_WE" \
    -annotate +1545+2060 "$TS_OUT_TH" \
    -annotate +1545+2185 "$TS_OUT_FR" \
    -annotate +1545+2300 "$TS_OUT_SA" \
    \
    -annotate +1795+1570 "$TS_OFF_SU" \
    -annotate +1795+1695 "$TS_OFF_MO" \
    -annotate +1795+1820 "$TS_OFF_TU" \
    -annotate +1795+1935 "$TS_OFF_WE" \
    -annotate +1795+2060 "$TS_OFF_TH" \
    -annotate +1795+2185 "$TS_OFF_FR" \
    -annotate +1795+2300 "$TS_OFF_SA" \
    \
    -annotate +2075+1570 "$TS_TOT_SU" \
    -annotate +2075+1695 "$TS_TOT_MO" \
    -annotate +2075+1820 "$TS_TOT_TU" \
    -annotate +2075+1935 "$TS_TOT_WE" \
    -annotate +2075+2060 "$TS_TOT_TH" \
    -annotate +2075+2185 "$TS_TOT_FR" \
    -annotate +2075+2300 "$TS_TOT_SA" \
    \
    -annotate +2075+2440 "$TS_TOTAL_H h" \
    -annotate +2075+2560 "${TS_RATE} \$/h" \
    -annotate +2075+2670 "\$ $TS_TOTAL_MONEY" \
    "${OUTNAME}"
