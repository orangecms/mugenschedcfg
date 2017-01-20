set terminal pdf enh size 29.7,21 font "Arial,6"
set output "scheduling_plans.pdf"
unset key
plot \
 "scheduling_plans" using ($4)     : ($2      ) : (0)         : (0)   : ($3) : yticlabel (1) with vectors nohead lc variable lw 10, \
 "scheduling_plans" using ($4)     : ($2 - 0.4) : (1 + $5-$4) : (0)   : ($3) with vectors nohead lc variable lw 2,\
 "scheduling_plans" using ($4)     : ($2 + 0.4) : (1 + $5-$4) : (0)   : ($3) with vectors nohead lc variable lw 2,\
 "scheduling_plans" using ($4)     : ($2 - 0.4) : (0)         : (0.8) : ($3) with vectors nohead lc variable lw 2,\
 "scheduling_plans" using (1 + $5) : ($2 - 0.4) : (0)         : (0.8) : ($3) with vectors nohead lc variable lw 2;
