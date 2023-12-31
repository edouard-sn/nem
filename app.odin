package main

import "emulator/cpu"
import "emulator/ines"

main :: proc() {
	cpu := cpu.new_cpu()
	_ = ines.load_rom("nestest.nes")


	cpu.registers.flags |= {.Carry}
}
