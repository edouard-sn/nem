package main

import nem_cpu "emulator/cpu"

main :: proc() {
	cpu := nem_cpu.new_cpu()
	nem_cpu.reset_interupt(&cpu)
}
