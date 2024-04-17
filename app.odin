package main

import ncpu "emulator/cpu"

// This main has no purpose other than to make `odin test` work (by importing the package)

main :: proc() {
	bus := ncpu.new_bus()
	cpu := ncpu.new_cpu(&bus)
	ncpu.reset_interupt(&cpu)
}
