package cpu

@(private)
AddressModes :: enum {
    Accumulator,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Immediate,
    Implied,
    Indirect,
    XIndirect,
    IndirectY,
    Relative,
    Zeropage,
    ZeropageX,
    ZeropageY,
}

@(private)
_set_mask :: #force_inline proc(flags: ^CPUFlagRegistry, mask: CPUFlagRegistry, value := true) {
    flags^ = value ? (flags^ | mask) : (flags^ & ~mask)
}

// Sets the Negative or Zero flag depending on nb
@(private)
_zero_or_neg_flags :: #force_inline proc(flags: ^CPUFlagRegistry, nb: u8) {
    _set_mask(flags, {.Zero}, nb == 0)
    _set_mask(flags, {.Negative}, i8(nb) < 0)
}

// Implied instructions

tax_instruction :: proc(cpu: ^CPU) {
    cpu.registers.x = cpu.registers.accumulator
    
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

txa_instruction :: proc(cpu: ^CPU) {
    cpu.registers.accumulator = cpu.registers.x
    
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

tay_instruction :: proc(cpu: ^CPU) {
    cpu.registers.y = cpu.registers.accumulator
    
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

tya_instruction :: proc(cpu: ^CPU) {
    cpu.registers.accumulator = cpu.registers.y
    
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

inx_instruction :: proc(cpu: ^CPU) {
    cpu.registers.x += 1

    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

dex_instruction :: proc(cpu: ^CPU) {
    cpu.registers.x -= 1

    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

iny_instruction :: proc(cpu: ^CPU) {
    cpu.registers.y += 1

    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

dey_instruction :: proc(cpu: ^CPU) {
    cpu.registers.y -= 1

    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

php_instruction :: proc(cpu: ^CPU) {
    assert(push_u8_on_stack(cpu, transmute(u8)(cpu.registers.flags | {.Break, .Bit5})))
}

nop_instruction :: proc(cpu: ^CPU) {
}

pla_instruction :: proc(cpu: ^CPU) {
    assert(push_u8_on_stack(cpu, cpu.registers.accumulator))
    
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

pha_instruction :: proc(cpu: ^CPU) {
    cpu.registers.accumulator = pull_u8_from_stack(cpu)
    
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

plp_instruction :: proc(cpu: ^CPU) {
    break_bit5_save := (cpu.registers.flags & {.Break, .Bit5})

    cpu.registers.flags = transmute(CPUFlagRegistry)pull_u8_from_stack(cpu)
    cpu.registers.flags |= break_bit5_save 

    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

clc_instruction :: proc(cpu: ^CPU) {
    cpu.registers.flags &= ~{.Carry}
}

sec_instruction :: proc(cpu: ^CPU) {
    cpu.registers.flags |= {.Carry}
}

sed_instruction :: proc(cpu: ^CPU) {
    cpu.registers.flags |= {.Decimal}
}

sei_instruction :: proc(cpu: ^CPU) {
    cpu.registers.flags |= {.Interupt}
}

cli_instruction :: proc(cpu: ^CPU) {
    cpu.registers.flags &= ~{.Interupt}
}

clv_instruction :: proc(cpu: ^CPU) {
    cpu.registers.flags &= ~{.Overflow}
}

cld_instruction :: proc(cpu: ^CPU) {
    cpu.registers.flags &= ~{.Decimal}
}

txs_instruction :: proc(cpu: ^CPU) {
    cpu.registers.stack_pointer = cpu.registers.x
}

tsx_instruction :: proc(cpu: ^CPU) {
    cpu.registers.x = cpu.registers.stack_pointer
}

lsr_instruction :: proc(cpu: ^CPU, data: ^u8) {
    _set_mask(&cpu.registers.flags, {.Carry}, (data^ & 1 != 0))
    (data^) >>= 1

    cpu.registers.flags &= ~{.Negative}
    
    _set_mask(&cpu.registers.flags, {.Zero}, (data^ == 0))

    _zero_or_neg_flags(&cpu.registers.flags, data^)
}

asl_instruction :: proc(cpu: ^CPU, data: ^u8) {
    _set_mask(&cpu.registers.flags, {.Carry}, (data^ & 0b10000000 != 0))
    (data^) <<= 1
    _zero_or_neg_flags(&cpu.registers.flags, data^)
}

ora_instruction :: proc(cpu: ^CPU, data: ^u8) {
    // TODO: + add 1 cycle if page boundary crossed
    cpu.registers.accumulator |= data^
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

and_instruction :: proc(cpu: ^CPU, data: ^u8) {
    // TODO: + add 1 cycle if page boundary crossed
    cpu.registers.accumulator &= data^
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

eor_instruction :: proc(cpu: ^CPU, data: ^u8) {
    cpu.registers.accumulator ~= data^
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

_cmp_instruction :: proc(flags: ^CPUFlagRegistry, lhs: u8, rhs: u8) {
    _set_mask(flags, {.Carry}, lhs >= rhs)
    sub := lhs - rhs
    _zero_or_neg_flags(flags, sub)
}

cmp_instruction :: proc(cpu: ^CPU, data: ^u8) {
    _cmp_instruction(&cpu.registers.flags, cpu.registers.accumulator, data^)
}

cmx_instruction :: proc(cpu: ^CPU, data: ^u8) {
    _cmp_instruction(&cpu.registers.flags, cpu.registers.x, data^)
}

cmy_instruction :: proc(cpu: ^CPU, data: ^u8) {
    _cmp_instruction(&cpu.registers.flags, cpu.registers.y, data^)
}

sta_instruction :: proc(cpu: ^CPU, data: ^u8) {
    data^ = cpu.registers.accumulator
}

stx_instruction :: proc(cpu: ^CPU, data: ^u8) {
    data^ = cpu.registers.x
}

sty_instruction :: proc(cpu: ^CPU, data: ^u8) {
    data^ = cpu.registers.y
}

lda_instruction :: proc(cpu: ^CPU, data: ^u8) {
    cpu.registers.accumulator = data^
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

ldy_instruction :: proc(cpu: ^CPU, data: ^u8) {
    cpu.registers.y = data^
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

ldx_instruction :: proc(cpu: ^CPU, data: ^u8) {
    cpu.registers.x = data^
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

rol_instruction :: proc(cpu: ^CPU, data: ^u8) {    
    old_carry := .Carry in cpu.registers.flags

    _set_mask(&cpu.registers.flags, {.Carry}, (data^ & 0b10000000 != 0))
    (data^) <<= 1

    if (old_carry) {
        (data^) |= 0b00000001 
    } else {
        (data^) &= 0b11111110 
    }

    _zero_or_neg_flags(&cpu.registers.flags, data^)
}

ror_instruction :: proc(cpu: ^CPU, data: ^u8) {    
    old_carry := .Carry in cpu.registers.flags

    _set_mask(&cpu.registers.flags, {.Carry}, (data^ & 0b10000000 != 0))
    (data^) >>= 1

    if (old_carry) {
        (data^) |= 0b10000000 
    } else {
        (data^) &= 0b01111111 
    }

    _zero_or_neg_flags(&cpu.registers.flags, data^)
}

rti_instruction :: proc(cpu: ^CPU) {    
    cpu.registers.flags = transmute(CPUFlagRegistry)pull_u8_from_stack(cpu)
    cpu.registers.program_counter = pull_u16_from_stack(cpu)
}

rts_instruction :: proc(cpu: ^CPU) {
    cpu.registers.program_counter = pull_u16_from_stack(cpu) + 1
}

adc_instruction :: proc(cpu: ^CPU, data: ^u8) {
    addition: u16 = u16(cpu.registers.accumulator) + u16(data^) + u16(.Carry in cpu.registers.flags) 

    _set_mask(&cpu.registers.flags, {.Carry}, addition > 0xFF)

    // Shenanigans https://stackoverflow.com/questions/29193303/6502-emulation-proper-way-to-implement-adc-and-sbc
    overflow : bool = ((((u16(~(cpu.registers.accumulator ~ data^))) & ((u16(cpu.registers.accumulator) ~ addition))) & 0b10000000) > 0)
    _set_mask(&cpu.registers.flags, {.Overflow}, overflow)

    cpu.registers.accumulator = u8(addition & 0xFF)
    _zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

sbc_instruction :: proc(cpu: ^CPU, data: ^u8) {
    complemented := ~(data^) // -data - 1
    adc_instruction(cpu, &complemented)
}

inc_instruction :: proc(cpu: ^CPU, data: ^u8) {
    (data^) += 1
    _zero_or_neg_flags(&cpu.registers.flags, data^)
}

dec_instruction :: proc(cpu: ^CPU, data: ^u8) {
    (data^) -= 1
    _zero_or_neg_flags(&cpu.registers.flags, data^)
}

jmp_instruction :: proc(cpu: ^CPU, address: u16) {
    cpu.registers.program_counter = address
}

jsr_instruction :: proc(cpu: ^CPU, address: u16) {
    stack_pc := cpu.registers.program_counter + 2

    assert(push_u8_on_stack(cpu, u8(stack_pc & 0xFF00) >> 8))
    assert(push_u8_on_stack(cpu, u8(stack_pc & 0x00FF)))

    cpu.registers.program_counter = address
}

bit_instruction :: proc (cpu: ^CPU, data: ^u8) {
    and_result := cpu.registers.accumulator & data^
    
    _set_mask(&cpu.registers.flags, {.Negative}, (data^ & (1 << 7)) > 0) // N = 7th bit of data
    _set_mask(&cpu.registers.flags, {.Overflow}, (data^ & (1 << 6)) > 0) // V = 6th bit of data
    _set_mask(&cpu.registers.flags, {.Zero}, (and_result == 0))
}

bpl_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Negative not_in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}

bmi_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Negative in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}


bne_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Zero not_in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}

beq_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Zero in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}

bcc_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Carry not_in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}

bcs_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Carry in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}

bvc_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Overflow in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}

bvs_instruction :: proc (cpu: ^CPU, data: ^u8) {
    offset := data^
    if (.Overflow in cpu.registers.flags) {
        cpu.registers.program_counter += u16(offset)
    }
}

brk_instruction :: proc (cpu: ^CPU) {
    stack_pc := cpu.registers.program_counter + 2
    assert(push_u8_on_stack(cpu, u8(stack_pc & 0xFF00) >> 8))
    assert(push_u8_on_stack(cpu, u8(stack_pc & 0x00FF)))

    //maybe
    cpu.registers.program_counter += 1
}