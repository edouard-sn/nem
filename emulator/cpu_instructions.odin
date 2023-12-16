package emulator

// Sets the Negative or Zero flag depending on nb
zero_or_neg_flags :: proc(registers: ^CPURegisters, nb: u8) {
    if (nb == 0) {
        registers.flags |= {.Zero}
    } else if (i8(nb) < 0) {
        // NOTE: Might have to check internally like ((1 << 8) & nb) for negative
        registers.flags |= {.Negative}
    } 
}


tax_instruction :: proc(cpu: ^CPU) {
    cpu.registers.x = cpu.registers.accumulator
    
    zero_or_neg_flags(&cpu.registers, cpu.registers.x)
}

txa_instruction :: proc(cpu: ^CPU) {
    cpu.registers.accumulator = cpu.registers.x
    
    zero_or_neg_flags(&cpu.registers, cpu.registers.accumulator)
}

tay_instruction :: proc(cpu: ^CPU) {
    cpu.registers.y = cpu.registers.accumulator
    
    zero_or_neg_flags(&cpu.registers, cpu.registers.y)
}

tya_instruction :: proc(cpu: ^CPU) {
    cpu.registers.accumulator = cpu.registers.y
    
    zero_or_neg_flags(&cpu.registers, cpu.registers.accumulator)
}

inx_instruction :: proc(cpu: ^CPU) {
    cpu.registers.x += 1

    zero_or_neg_flags(&cpu.registers, cpu.registers.x)
}

dex_instruction :: proc(cpu: ^CPU) {
    cpu.registers.x -= 1

    zero_or_neg_flags(&cpu.registers, cpu.registers.x)
}


iny_instruction :: proc(cpu: ^CPU) {
    cpu.registers.y += 1

    zero_or_neg_flags(&cpu.registers, cpu.registers.y)
}

dey_instruction :: proc(cpu: ^CPU) {
    cpu.registers.y -= 1

    zero_or_neg_flags(&cpu.registers, cpu.registers.y)
}

