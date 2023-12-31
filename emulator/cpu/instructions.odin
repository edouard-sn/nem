package cpu

import "core:fmt"

@(private)
AddressMode :: enum {
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
	ZeroPage,
	ZeroPageX,
	ZeroPageY,
}

// NOTE: Would it be worth it to remove the use of union so we can make the array const?
InstructionHandle :: union { 
	proc(cpu: ^CPU), // No data processing
	proc(cpu: ^CPU, data: ^byte), // Data processing
	proc(cpu: ^CPU, address: u16), // Direct address access
}

Instruction :: struct {
	handle:              InstructionHandle,
	mode:                AddressMode,
	cycles:              uint,
	cycle_page_crossed:  bool,
}

instruction_handles := [0xFF]Instruction {
	0x00 = Instruction{handle = brk_instruction, cycles = 7, mode = .Implied},
	0x01 = Instruction{handle = ora_instruction, cycles = 6, mode = .XIndirect},
	0x05 = Instruction{handle = ora_instruction, cycles = 3, mode = .ZeroPage},
	0x06 = Instruction{handle = asl_instruction, cycles = 5, mode = .ZeroPage},
	0x08 = Instruction{handle = php_instruction, cycles = 3, mode = .Implied},
	0x09 = Instruction{handle = ora_instruction, cycles = 2, mode = .Immediate},
	0x0A = Instruction{handle = asl_instruction, cycles = 2, mode = .Accumulator},
	0x0D = Instruction{handle = ora_instruction, cycles = 4, mode = .Absolute},
	0x0E = Instruction{handle = asl_instruction, cycles = 6, mode = .Absolute},
	0x10 = Instruction{handle = bpl_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x11 = Instruction{handle = ora_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x15 = Instruction{handle = ora_instruction, cycles = 4, mode = .ZeroPageX},
	0x16 = Instruction{handle = asl_instruction, cycles = 6, mode = .ZeroPageX},
	0x18 = Instruction{handle = clc_instruction, cycles = 2, mode = .Implied},
	0x19 = Instruction{handle = ora_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x1D = Instruction{handle = ora_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x1E = Instruction{handle = asl_instruction, cycles = 7, mode = .AbsoluteX},
	0x20 = Instruction{handle = jsr_instruction, cycles = 6, mode = .Absolute},
	0x21 = Instruction{handle = and_instruction, cycles = 6, mode = .XIndirect},
	0x24 = Instruction{handle = bit_instruction, cycles = 3, mode = .ZeroPage},
	0x25 = Instruction{handle = and_instruction, cycles = 3, mode = .ZeroPage},
	0x26 = Instruction{handle = rol_instruction, cycles = 5, mode = .ZeroPage},
	0x28 = Instruction{handle = plp_instruction, cycles = 4, mode = .Implied},
	0x29 = Instruction{handle = and_instruction, cycles = 2, mode = .Immediate},
	0x2A = Instruction{handle = rol_instruction, cycles = 2, mode = .Accumulator},
	0x2C = Instruction{handle = bit_instruction, cycles = 4, mode = .Absolute},
	0x2D = Instruction{handle = and_instruction, cycles = 4, mode = .Absolute},
	0x2E = Instruction{handle = rol_instruction, cycles = 6, mode = .Absolute},
	0x30 = Instruction{handle = bmi_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x31 = Instruction{handle = and_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x35 = Instruction{handle = and_instruction, cycles = 4, mode = .ZeroPageX},
	0x36 = Instruction{handle = rol_instruction, cycles = 6, mode = .ZeroPageX},
	0x38 = Instruction{handle = sec_instruction, cycles = 2, mode = .Implied},
	0x39 = Instruction{handle = and_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x3D = Instruction{handle = and_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x3E = Instruction{handle = rol_instruction, cycles = 7, mode = .AbsoluteX},
	0x40 = Instruction{handle = rti_instruction, cycles = 6, mode = .Implied},
	0x41 = Instruction{handle = eor_instruction, cycles = 6, mode = .XIndirect},
	0x45 = Instruction{handle = eor_instruction, cycles = 3, mode = .ZeroPage},
	0x46 = Instruction{handle = lsr_instruction, cycles = 5, mode = .ZeroPage},
	0x48 = Instruction{handle = pha_instruction, cycles = 3, mode = .Implied},
	0x49 = Instruction{handle = eor_instruction, cycles = 2, mode = .Immediate},
	0x4A = Instruction{handle = lsr_instruction, cycles = 2, mode = .Accumulator},
	0x4C = Instruction{handle = jmp_instruction, cycles = 3, mode = .Absolute},
	0x4D = Instruction{handle = eor_instruction, cycles = 4, mode = .Absolute},
	0x4E = Instruction{handle = lsr_instruction, cycles = 6, mode = .Absolute},
	0x50 = Instruction{handle = bvc_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x51 = Instruction{handle = eor_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x55 = Instruction{handle = eor_instruction, cycles = 4, mode = .ZeroPageX},
	0x56 = Instruction{handle = lsr_instruction, cycles = 6, mode = .ZeroPageX},
	0x58 = Instruction{handle = cli_instruction, cycles = 2, mode = .Implied},
	0x59 = Instruction{handle = eor_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x5D = Instruction{handle = eor_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x5E = Instruction{handle = lsr_instruction, cycles = 7, mode = .AbsoluteX},
	0x60 = Instruction{handle = rts_instruction, cycles = 6, mode = .Implied},
	0x61 = Instruction{handle = adc_instruction, cycles = 6, mode = .XIndirect},
	0x65 = Instruction{handle = adc_instruction, cycles = 3, mode = .ZeroPage},
	0x66 = Instruction{handle = ror_instruction, cycles = 5, mode = .ZeroPage},
	0x68 = Instruction{handle = pla_instruction, cycles = 4, mode = .Implied},
	0x69 = Instruction{handle = adc_instruction, cycles = 2, mode = .Immediate},
	0x6A = Instruction{handle = ror_instruction, cycles = 2, mode = .Accumulator},
	0x6C = Instruction{handle = jmp_instruction, cycles = 5, mode = .Indirect},
	0x6D = Instruction{handle = adc_instruction, cycles = 4, mode = .Absolute},
	0x6E = Instruction{handle = ror_instruction, cycles = 6, mode = .Absolute},
	0x70 = Instruction{handle = bvs_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x71 = Instruction{handle = adc_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x75 = Instruction{handle = adc_instruction, cycles = 4, mode = .ZeroPageX},
	0x76 = Instruction{handle = ror_instruction, cycles = 6, mode = .ZeroPageX},
	0x78 = Instruction{handle = sei_instruction, cycles = 2, mode = .Implied},
	0x79 = Instruction{handle = adc_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x7D = Instruction{handle = adc_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x7E = Instruction{handle = ror_instruction, cycles = 7, mode = .AbsoluteX},
	0x81 = Instruction{handle = sta_instruction, cycles = 6, mode = .XIndirect},
	0x84 = Instruction{handle = sty_instruction, cycles = 3, mode = .ZeroPage},
	0x85 = Instruction{handle = sta_instruction, cycles = 3, mode = .ZeroPage},
	0x86 = Instruction{handle = stx_instruction, cycles = 3, mode = .ZeroPage},
	0x88 = Instruction{handle = dey_instruction, cycles = 2, mode = .Implied},
	0x8A = Instruction{handle = txa_instruction, cycles = 2, mode = .Implied},
	0x8C = Instruction{handle = sty_instruction, cycles = 4, mode = .Absolute},
	0x8D = Instruction{handle = sta_instruction, cycles = 4, mode = .Absolute},
	0x8E = Instruction{handle = stx_instruction, cycles = 4, mode = .Absolute},
	0x90 = Instruction{handle = bcc_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x91 = Instruction{handle = sta_instruction, cycles = 6, mode = .IndirectY},
	0x94 = Instruction{handle = sty_instruction, cycles = 4, mode = .ZeroPageX},
	0x95 = Instruction{handle = sta_instruction, cycles = 4, mode = .ZeroPageX},
	0x96 = Instruction{handle = stx_instruction, cycles = 4, mode = .ZeroPageY},
	0x98 = Instruction{handle = tya_instruction, cycles = 2, mode = .Implied},
	0x99 = Instruction{handle = sta_instruction, cycles = 5, mode = .AbsoluteY},
	0x9A = Instruction{handle = txs_instruction, cycles = 2, mode = .Implied},
	0x9D = Instruction{handle = sta_instruction, cycles = 5, mode = .AbsoluteX},
	0xA0 = Instruction{handle = ldy_instruction, cycles = 2, mode = .Immediate},
	0xA1 = Instruction{handle = lda_instruction, cycles = 6, mode = .XIndirect},
	0xA2 = Instruction{handle = ldx_instruction, cycles = 2, mode = .Immediate},
	0xA4 = Instruction{handle = ldy_instruction, cycles = 3, mode = .ZeroPage},
	0xA5 = Instruction{handle = lda_instruction, cycles = 3, mode = .ZeroPage},
	0xA6 = Instruction{handle = ldx_instruction, cycles = 3, mode = .ZeroPage},
	0xA8 = Instruction{handle = tay_instruction, cycles = 2, mode = .Implied},
	0xA9 = Instruction{handle = lda_instruction, cycles = 2, mode = .Immediate},
	0xAA = Instruction{handle = tax_instruction, cycles = 2, mode = .Implied},
	0xAC = Instruction{handle = ldy_instruction, cycles = 4, mode = .Absolute},
	0xAD = Instruction{handle = lda_instruction, cycles = 4, mode = .Absolute},
	0xAE = Instruction{handle = ldx_instruction, cycles = 4, mode = .Absolute},
	0xB0 = Instruction{handle = bcs_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0xB1 = Instruction{handle = lda_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0xB4 = Instruction{handle = ldy_instruction, cycles = 4, mode = .ZeroPageX},
	0xB5 = Instruction{handle = lda_instruction, cycles = 4, mode = .ZeroPageX},
	0xB6 = Instruction{handle = ldx_instruction, cycles = 4, mode = .ZeroPageY},
	0xB8 = Instruction{handle = clv_instruction, cycles = 2, mode = .Implied},
	0xB9 = Instruction{handle = lda_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xBA = Instruction{handle = tsx_instruction, cycles = 2, mode = .Implied},
	0xBC = Instruction{handle = ldy_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xBD = Instruction{handle = lda_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xBE = Instruction{handle = ldx_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xC0 = Instruction{handle = cpy_instruction, cycles = 2, mode = .Immediate},
	0xC1 = Instruction{handle = cmp_instruction, cycles = 6, mode = .XIndirect},
	0xC4 = Instruction{handle = cpy_instruction, cycles = 3, mode = .ZeroPage},
	0xC5 = Instruction{handle = cmp_instruction, cycles = 3, mode = .ZeroPage},
	0xC6 = Instruction{handle = dec_instruction, cycles = 5, mode = .ZeroPage},
	0xC8 = Instruction{handle = iny_instruction, cycles = 2, mode = .Implied},
	0xC9 = Instruction{handle = cmp_instruction, cycles = 2, mode = .Immediate},
	0xCA = Instruction{handle = dex_instruction, cycles = 2, mode = .Implied},
	0xCC = Instruction{handle = cpy_instruction, cycles = 4, mode = .Absolute},
	0xCD = Instruction{handle = cmp_instruction, cycles = 4, mode = .Absolute},
	0xCE = Instruction{handle = dec_instruction, cycles = 6, mode = .Absolute},
	0xD0 = Instruction{handle = bne_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0xD1 = Instruction{handle = cmp_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0xD5 = Instruction{handle = cmp_instruction, cycles = 4, mode = .ZeroPageX},
	0xD6 = Instruction{handle = dec_instruction, cycles = 6, mode = .ZeroPageX},
	0xD8 = Instruction{handle = cld_instruction, cycles = 2, mode = .Implied},
	0xD9 = Instruction{handle = cmp_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xDD = Instruction{handle = cmp_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xDE = Instruction{handle = dec_instruction, cycles = 7, mode = .AbsoluteX},
	0xE0 = Instruction{handle = cpx_instruction, cycles = 2, mode = .Immediate},
	0xE1 = Instruction{handle = sbc_instruction, cycles = 6, mode = .XIndirect},
	0xE4 = Instruction{handle = cpx_instruction, cycles = 3, mode = .ZeroPage},
	0xE5 = Instruction{handle = sbc_instruction, cycles = 3, mode = .ZeroPage},
	0xE6 = Instruction{handle = inc_instruction, cycles = 5, mode = .ZeroPage},
	0xE8 = Instruction{handle = inx_instruction, cycles = 2, mode = .Implied},
	0xE9 = Instruction{handle = sbc_instruction, cycles = 2, mode = .Immediate},
	0xEA = Instruction{handle = nop_instruction, cycles = 2, mode = .Implied},
	0xEC = Instruction{handle = cpx_instruction, cycles = 4, mode = .Absolute},
	0xED = Instruction{handle = sbc_instruction, cycles = 4, mode = .Absolute},
	0xEE = Instruction{handle = inc_instruction, cycles = 6, mode = .Absolute},
	0xF0 = Instruction{handle = beq_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0xF1 = Instruction{handle = sbc_instruction, cycles = 5, mode = .IndirectY , cycle_page_crossed = true},
	0xF5 = Instruction{handle = sbc_instruction, cycles = 4, mode = .ZeroPageX},
	0xF6 = Instruction{handle = inc_instruction, cycles = 6, mode = .ZeroPageX},
	0xF8 = Instruction{handle = sed_instruction, cycles = 2, mode = .Implied},
	0xF9 = Instruction{handle = sbc_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xFD = Instruction{handle = sbc_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xFE = Instruction{handle = inc_instruction, cycles = 7, mode = .AbsoluteX},
}

AddressingHelper :: struct {
	handle: proc(^CPU, ^[]byte) -> u16,
	bytes:  uint,
}
addressing_helpers := [AddressMode] AddressingHelper  {
	.Implied = {
		handle = proc(cpu: ^CPU, _: ^[]byte) -> u16 {return 0},
		bytes = 0,
	},
	.Accumulator = {
		handle = proc(cpu: ^CPU, _: ^[]byte) -> u16 {return 0},
		bytes = 0,
	},
	.Immediate = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return immediate(data)},
		bytes = 1,
	},
	.ZeroPage = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return zeropage(data)},
		bytes = 1,
	},
	.ZeroPageX = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return zeropage(data, cpu.registers.x)},
		bytes = 1,
	},
	.ZeroPageY = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return zeropage(data, cpu.registers.y)},
		bytes = 1,
	},
	.Relative = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return relative(cpu, data)},
		bytes = 1,
	},
	.Absolute = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return absolute(data)},
		bytes = 2,
	},
	.AbsoluteX = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return absolute(data, cpu.registers.x)},
		bytes = 2,
	},
	.AbsoluteY = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return absolute(data, cpu.registers.y)},
		bytes = 2,
	},
	.Indirect = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return absolute(data)},
		bytes = 2,
	},
	.XIndirect = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return x_zp_indirect(cpu, data)},
		bytes = 1,
	},
	.IndirectY = {
		handle = proc(cpu: ^CPU, data: ^[]byte) -> u16 {return zp_indirect_y(cpu, data)},
		bytes = 1,
	},
}

execute_instruction :: proc(cpu: ^CPU, data: ^[]byte) {
	op_code := data[0]
	instruction := instruction_handles[op_code]
	cpu.cycles += 1

	data^ = data[1:]
	if (instruction.handle != nil) {
		addressing := addressing_helpers[instruction.mode]
		address := addressing.handle(cpu, data)
		data^ = data[addressing.bytes:]

		cpu.registers.program_counter += u16(addressing.bytes)
		// Pages on the 6502 are 256 bytes long. If the address crosses a page boundary, we add an extra cycle when needed.
		if (instruction.cycle_page_crossed && (address & 0xFF00) != (cpu.registers.program_counter & 0xFF00)) {
			cpu.cycles +=  1
		}

		switch ins in instruction.handle {
			case proc(^CPU):
				ins(cpu) 
			case proc(^CPU, u16): 
				ins(cpu, address)
			case proc(^CPU, ^byte): 
				ins(cpu, &cpu.memory.raw[address])
		}
		cpu.cycles += instruction.cycles
	}
	 else {
		fmt.eprintf("Unknown OP-code encountered: [%02d]", op_code)
	}
}


@(private)
_set_mask :: #force_inline proc(flags: ^CPUFlagRegistry, mask: CPUFlagRegistry, value := true) {
	flags^ = value ? (flags^ | mask) : (flags^ & ~mask)
}

// Sets the Negative or Zero flag depending on nb
@(private)
_zero_or_neg_flags :: #force_inline proc(flags: ^CPUFlagRegistry, nb: byte) {
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
	assert(push_byte_on_stack(cpu, transmute(byte)(cpu.registers.flags | {.Break, .Bit5})))
}

nop_instruction :: proc(cpu: ^CPU) {
}

pla_instruction :: proc(cpu: ^CPU) {
	assert(push_byte_on_stack(cpu, cpu.registers.accumulator))

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

pha_instruction :: proc(cpu: ^CPU) {
	cpu.registers.accumulator = pull_byte_from_stack(cpu)

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

plp_instruction :: proc(cpu: ^CPU) {
	break_bit5_save := (cpu.registers.flags & {.Break, .Bit5})

	cpu.registers.flags = transmute(CPUFlagRegistry)pull_byte_from_stack(cpu)
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

lsr_instruction :: proc(cpu: ^CPU, data: ^byte) {
	_set_mask(&cpu.registers.flags, {.Carry}, (data^ & 1 != 0))
	(data^) >>= 1

	cpu.registers.flags &= ~{.Negative}

	_zero_or_neg_flags(&cpu.registers.flags, data^)
}

asl_instruction :: proc(cpu: ^CPU, data: ^byte) {
	_set_mask(&cpu.registers.flags, {.Carry}, (data^ & 0b1000_0000 != 0))
	(data^) <<= 1
	_zero_or_neg_flags(&cpu.registers.flags, data^)
}

ora_instruction :: proc(cpu: ^CPU, data: ^byte) {
	// TODO: + add 1 cycle if page boundary crossed
	cpu.registers.accumulator |= data^
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

and_instruction :: proc(cpu: ^CPU, data: ^byte) {
	// TODO: + add 1 cycle if page boundary crossed
	cpu.registers.accumulator &= data^
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

eor_instruction :: proc(cpu: ^CPU, data: ^byte) {
	cpu.registers.accumulator ~= data^
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

_cmp_instruction :: proc(flags: ^CPUFlagRegistry, lhs: byte, rhs: byte) {
	_set_mask(flags, {.Carry}, lhs >= rhs)
	sub := lhs - rhs
	_zero_or_neg_flags(flags, sub)
}

cmp_instruction :: proc(cpu: ^CPU, data: ^byte) {
	_cmp_instruction(&cpu.registers.flags, cpu.registers.accumulator, data^)
}

cpx_instruction :: proc(cpu: ^CPU, data: ^byte) {
	_cmp_instruction(&cpu.registers.flags, cpu.registers.x, data^)
}

cpy_instruction :: proc(cpu: ^CPU, data: ^byte) {
	_cmp_instruction(&cpu.registers.flags, cpu.registers.y, data^)
}

sta_instruction :: proc(cpu: ^CPU, data: ^byte) {
	data^ = cpu.registers.accumulator
}

stx_instruction :: proc(cpu: ^CPU, data: ^byte) {
	data^ = cpu.registers.x
}

sty_instruction :: proc(cpu: ^CPU, data: ^byte) {
	data^ = cpu.registers.y
}

lda_instruction :: proc(cpu: ^CPU, data: ^byte) {
	cpu.registers.accumulator = data^
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

ldy_instruction :: proc(cpu: ^CPU, data: ^byte) {
	cpu.registers.y = data^
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

ldx_instruction :: proc(cpu: ^CPU, data: ^byte) {
	cpu.registers.x = data^
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

rol_instruction :: proc(cpu: ^CPU, data: ^byte) {
	old_carry := .Carry in cpu.registers.flags

	_set_mask(&cpu.registers.flags, {.Carry}, (data^ & 0b1000_0000 != 0))
	(data^) <<= 1

	if (old_carry) {
		(data^) |= 0b0000_0001
	}
	 else {
		(data^) &= 0b1111_1110
	}

	_zero_or_neg_flags(&cpu.registers.flags, data^)
}

ror_instruction :: proc(cpu: ^CPU, data: ^byte) {
	old_carry := .Carry in cpu.registers.flags

	_set_mask(&cpu.registers.flags, {.Carry}, (data^ & 0b1000_0000 != 0))
	(data^) >>= 1

	if (old_carry) {
		(data^) |= 0b1000_0000
	}
	 else {
		(data^) &= 0b0111_1111
	}

	_zero_or_neg_flags(&cpu.registers.flags, data^)
}

rti_instruction :: proc(cpu: ^CPU) {
	cpu.registers.flags = transmute(CPUFlagRegistry)pull_byte_from_stack(cpu)
	cpu.registers.program_counter = pull_u16_from_stack(cpu)
}

rts_instruction :: proc(cpu: ^CPU) {
	cpu.registers.program_counter = pull_u16_from_stack(cpu) + 1
}

adc_instruction :: proc(cpu: ^CPU, data: ^byte) {
	addition: u16 = u16(cpu.registers.accumulator) + u16(data^) + u16(.Carry in cpu.registers.flags)

	_set_mask(&cpu.registers.flags, {.Carry}, addition > 0xFF)

	// Shenanigans https://stackoverflow.com/questions/29193303/6502-emulation-proper-way-to-implement-adc-and-sbc
	overflow: bool =
		((((u16(~(cpu.registers.accumulator ~ data^))) & ((u16(cpu.registers.accumulator) ~ addition))) & 0b1000_0000) >
			0)
	_set_mask(&cpu.registers.flags, {.Overflow}, overflow)

	cpu.registers.accumulator = byte(addition & 0xFF)
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

sbc_instruction :: proc(cpu: ^CPU, data: ^byte) {
	complemented := ~(data^) // -data - 1
	adc_instruction(cpu, &complemented)
}

inc_instruction :: proc(cpu: ^CPU, data: ^byte) {
	(data^) += 1
	_zero_or_neg_flags(&cpu.registers.flags, data^)
}

dec_instruction :: proc(cpu: ^CPU, data: ^byte) {
	(data^) -= 1
	_zero_or_neg_flags(&cpu.registers.flags, data^)
}

jmp_instruction :: proc(cpu: ^CPU, address: u16) {
	cpu.registers.program_counter = address
}

jsr_instruction :: proc(cpu: ^CPU, address: u16) {
	stack_pc := cpu.registers.program_counter + 2

	assert(push_byte_on_stack(cpu, byte(stack_pc & 0xFF00) >> 8))
	assert(push_byte_on_stack(cpu, byte(stack_pc & 0x00FF)))

	cpu.registers.program_counter = address
}

bit_instruction :: proc(cpu: ^CPU, data: ^byte) {
	and_result := cpu.registers.accumulator & data^

	_set_mask(&cpu.registers.flags, {.Negative}, (data^ & (1 << 7)) > 0) // N = 7th bit of data
	_set_mask(&cpu.registers.flags, {.Overflow}, (data^ & (1 << 6)) > 0) // V = 6th bit of data
	_set_mask(&cpu.registers.flags, {.Zero}, (and_result == 0))
}

bpl_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Negative not_in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}

bmi_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Negative in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}


bne_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Zero not_in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}

beq_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Zero in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}

bcc_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Carry not_in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}

bcs_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Carry in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}

bvc_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Overflow in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}

bvs_instruction :: proc(cpu: ^CPU, data: ^byte) {
	if (.Overflow in cpu.registers.flags) {
		offset := data^
		cpu.registers.program_counter += u16(offset)
	}
}

brk_instruction :: proc(cpu: ^CPU) {
	stack_pc := cpu.registers.program_counter + 2
	assert(push_byte_on_stack(cpu, byte(stack_pc & 0xFF00) >> 8))
	assert(push_byte_on_stack(cpu, byte(stack_pc & 0x00FF)))

	//maybe
	cpu.registers.program_counter += 1
}
