package main

import "emulator/cpu"
import "core:fmt"

main :: proc() {
    cpu := cpu.new_cpu()
    cpu.registers.flags |= {.Carry}
    // fmt.printf("%d", cpu.registers.flags in {.Carry})
}