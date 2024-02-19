package main

import nbus "emulator/bus"
import ncpu "emulator/cpu"

import "core:fmt"

// This main has no purpose other than to make `odin test` work (by importing the package)

main :: proc() {
	bus := nbus.new_bus()
	cpu := ncpu.new_cpu(&bus)
	ncpu.reset_interupt(&cpu)
}
