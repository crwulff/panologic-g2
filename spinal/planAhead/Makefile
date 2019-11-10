
project:
	planAhead -source planAhead.tcl -mode batch
	# This will have generated the MIG design. Apply patches
	cd ../coregen && cat *.patch | patch -p3

progmem:
	data2mem -bm planAhead.runs/impl_1/progmem_bd.bmm -bt planAhead.runs/impl_1/Pano.bit -bd ../sw/progmem.mem -o b planAhead.runs/impl_1/Pano_ram.bit

clean:
	rm -rf planAhead.runs planAhead.data planAhead.srcs *.jou *.log planAhead.ppr planAhead.cache
