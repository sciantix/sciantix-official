set terminal pngcairo size 1400,1000 enhanced font 'Arial,10'
set output 'UN_M7_codex_results/codex_final_3_best_rizk_near_dv0.5_diagnostics.png'
set multiplot layout 2,2 title 'codex_final_3_best_rizk_near_dv0.5'
set grid
set key outside
set xlabel 'T [K]'
set ylabel 'swelling [%]'
plot 'UN_M7_codex_results/codex_final_3_best_rizk_near_dv0.5_diagnostics.dat' using 1:2 with linespoints title 'dislocation/P2', \
     'UN_M7_codex_results/codex_final_3_best_rizk_near_dv0.5_diagnostics.dat' using 1:3 with linespoints title 'bulk'
set ylabel 'R_d [nm]'
plot 'UN_M7_codex_results/codex_final_3_best_rizk_near_dv0.5_diagnostics.dat' using 1:4 with linespoints title 'R_d'
set ylabel 'N_d [m^-3]'
set logscale y
plot 'UN_M7_codex_results/codex_final_3_best_rizk_near_dv0.5_diagnostics.dat' using 1:5 with linespoints title 'N_d'
unset logscale y
set ylabel 'pressure ratio'
plot 'UN_M7_codex_results/codex_final_3_best_rizk_near_dv0.5_diagnostics.dat' using 1:6 with linespoints title 'p_d/p_d,eq', \
     'UN_M7_codex_results/codex_final_3_best_rizk_near_dv0.5_diagnostics.dat' using 1:7 with linespoints title 'p_b/p_b,eq', \
     1 with lines dt 2 title 'equilibrium', \
     3 with lines dt 3 title 'free factor'
unset multiplot
