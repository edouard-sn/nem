package main

import nbus "emulator/bus"
import ncpu "emulator/cpu"
import nines "emulator/rom"

import "core:fmt"

main :: proc() {
	bus := nbus.new_bus()

	cpu := ncpu.new_cpu(&bus)
	ncpu.reset_interupt(&cpu)

	rom, ok := nines.load_rom("./nestest.nes")
	if !ok {
		fmt.printf("Failed to load rom\n")
		return
	}

	copy(bus.prg_rom, rom.prg_rom)
	copy(bus.prg_rom[0x4000:], rom.prg_rom)

	cpu.registers.program_counter = 0x0C000

	cpu.cycles = 7
	for {
		ncpu.execute_instruction(&cpu)
	}

}
