create_project planAhead . -part xc6slx150fgg484-2
close [ open ./Pano-generated.ucf w ]
add_files -fileset constrs_1 -norecurse ../Pano.ucf ./Pano-generated.ucf
add_files -norecurse ../Pano.v
add_files -norecurse ../coregen/MIG.xco
add_files -norecurse ../ise/progmem.bmm
set_property target_constrs_file ./Pano-generated.ucf [current_fileset -constrset]

generate_target  {synthesis simulation instantiation_template}  [get_files ../coregen/MIG.xco]

set_property strategy MapTiming [get_runs impl_1]

# Prevent BufferCC from being turned into a LUT + FF
set_property -name {steps.xst.args.More Options} -value {-shreg_min_size 3} -objects [get_runs synth_1]

update_compile_order -fileset sources_1
update_compile_order -fileset sim_1
