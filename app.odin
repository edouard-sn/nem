package main

import nem_cpu "emulator/cpu"
import nem_bus "emulator/bus"

main :: proc() {
	bus := nem_bus.new_bus()
	cpu := nem_cpu.new_cpu(&bus)

	nem_cpu.reset_interupt(&cpu)
}
