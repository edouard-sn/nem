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
	name:				string,

	// Instruction procedure logic
	handle:             InstructionHandle,
	
	// How do we get the data needed
	mode:               AddressMode,

	// How many cycles does this instruction take
	cycles:             uint,

	// Does this instruction take an extra cycle if the page is crossed
	cycle_page_crossed: bool,

	// Is PC changed by this instruction
	changes_pc:	    	bool,
}

instruction_handles := [0xFF]Instruction {
	0x00 = Instruction{name="BRK", handle = brk_instruction, cycles = 7, mode = .Implied},
	0x01 = Instruction{name="ORA", handle = ora_instruction, cycles = 6, mode = .XIndirect},
	0x05 = Instruction{name="ORA", handle = ora_instruction, cycles = 3, mode = .ZeroPage},
	0x06 = Instruction{name="ASL", handle = asl_instruction, cycles = 5, mode = .ZeroPage},
	0x08 = Instruction{name="PHP", handle = php_instruction, cycles = 3, mode = .Implied},
	0x09 = Instruction{name="ORA", handle = ora_instruction, cycles = 2, mode = .Immediate},
	0x0A = Instruction{name="ASL", handle = asl_instruction, cycles = 2, mode = .Accumulator},
	0x0D = Instruction{name="ORA", handle = ora_instruction, cycles = 4, mode = .Absolute},
	0x0E = Instruction{name="ASL", handle = asl_instruction, cycles = 6, mode = .Absolute},
	0x10 = Instruction{name="BPL", handle = bpl_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x11 = Instruction{name="ORA", handle = ora_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x15 = Instruction{name="ORA", handle = ora_instruction, cycles = 4, mode = .ZeroPageX},
	0x16 = Instruction{name="ASL", handle = asl_instruction, cycles = 6, mode = .ZeroPageX},
	0x18 = Instruction{name="CLC", handle = clc_instruction, cycles = 2, mode = .Implied},
	0x19 = Instruction{name="ORA", handle = ora_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x1D = Instruction{name="ORA", handle = ora_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x1E = Instruction{name="ASL", handle = asl_instruction, cycles = 7, mode = .AbsoluteX},
	0x20 = Instruction{name="JSR", handle = jsr_instruction, cycles = 6, mode = .Absolute, changes_pc = true},
	0x21 = Instruction{name="AND", handle = and_instruction, cycles = 6, mode = .XIndirect},
	0x24 = Instruction{name="BIT", handle = bit_instruction, cycles = 3, mode = .ZeroPage},
	0x25 = Instruction{name="AND", handle = and_instruction, cycles = 3, mode = .ZeroPage},
	0x26 = Instruction{name="ROL", handle = rol_instruction, cycles = 5, mode = .ZeroPage},
	0x28 = Instruction{name="PLP", handle = plp_instruction, cycles = 4, mode = .Implied},
	0x29 = Instruction{name="AND", handle = and_instruction, cycles = 2, mode = .Immediate},
	0x2A = Instruction{name="ROL", handle = rol_instruction, cycles = 2, mode = .Accumulator},
	0x2C = Instruction{name="BIT", handle = bit_instruction, cycles = 4, mode = .Absolute},
	0x2D = Instruction{name="AND", handle = and_instruction, cycles = 4, mode = .Absolute},
	0x2E = Instruction{name="ROL", handle = rol_instruction, cycles = 6, mode = .Absolute},
	0x30 = Instruction{name="BMI", handle = bmi_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x31 = Instruction{name="AND", handle = and_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x35 = Instruction{name="AND", handle = and_instruction, cycles = 4, mode = .ZeroPageX},
	0x36 = Instruction{name="ROL", handle = rol_instruction, cycles = 6, mode = .ZeroPageX},
	0x38 = Instruction{name="SEC", handle = sec_instruction, cycles = 2, mode = .Implied},
	0x39 = Instruction{name="AND", handle = and_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x3D = Instruction{name="AND", handle = and_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x3E = Instruction{name="ROL", handle = rol_instruction, cycles = 7, mode = .AbsoluteX},
	0x40 = Instruction{name="RTI", handle = rti_instruction, cycles = 6, mode = .Implied, changes_pc=true},
	0x41 = Instruction{name="EOR", handle = eor_instruction, cycles = 6, mode = .XIndirect},
	0x45 = Instruction{name="EOR", handle = eor_instruction, cycles = 3, mode = .ZeroPage},
	0x46 = Instruction{name="LSR", handle = lsr_instruction, cycles = 5, mode = .ZeroPage},
	0x48 = Instruction{name="PHA", handle = pha_instruction, cycles = 3, mode = .Implied},
	0x49 = Instruction{name="EOR", handle = eor_instruction, cycles = 2, mode = .Immediate},
	0x4A = Instruction{name="LSR", handle = lsr_instruction, cycles = 2, mode = .Accumulator},
	0x4C = Instruction{name="JMP", handle = jmp_instruction, cycles = 3, mode = .Absolute, changes_pc = true},
	0x4D = Instruction{name="EOR", handle = eor_instruction, cycles = 4, mode = .Absolute},
	0x4E = Instruction{name="LSR", handle = lsr_instruction, cycles = 6, mode = .Absolute},
	0x50 = Instruction{name="BVC", handle = bvc_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x51 = Instruction{name="EOR", handle = eor_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x55 = Instruction{name="EOR", handle = eor_instruction, cycles = 4, mode = .ZeroPageX},
	0x56 = Instruction{name="LSR", handle = lsr_instruction, cycles = 6, mode = .ZeroPageX},
	0x58 = Instruction{name="CLI", handle = cli_instruction, cycles = 2, mode = .Implied},
	0x59 = Instruction{name="EOR", handle = eor_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x5D = Instruction{name="EOR", handle = eor_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x5E = Instruction{name="LSR", handle = lsr_instruction, cycles = 7, mode = .AbsoluteX},
	0x60 = Instruction{name="RTS", handle = rts_instruction, cycles = 6, mode = .Implied, changes_pc=true},
	0x61 = Instruction{name="ADC", handle = adc_instruction, cycles = 6, mode = .XIndirect},
	0x65 = Instruction{name="ADC", handle = adc_instruction, cycles = 3, mode = .ZeroPage},
	0x66 = Instruction{name="ROR", handle = ror_instruction, cycles = 5, mode = .ZeroPage},
	0x68 = Instruction{name="PLA", handle = pla_instruction, cycles = 4, mode = .Implied},
	0x69 = Instruction{name="ADC", handle = adc_instruction, cycles = 2, mode = .Immediate},
	0x6A = Instruction{name="ROR", handle = ror_instruction, cycles = 2, mode = .Accumulator},
	0x6C = Instruction{name="JMP", handle = jmp_instruction, cycles = 5, mode = .Indirect},
	0x6D = Instruction{name="ADC", handle = adc_instruction, cycles = 4, mode = .Absolute},
	0x6E = Instruction{name="ROR", handle = ror_instruction, cycles = 6, mode = .Absolute},
	0x70 = Instruction{name="BVS", handle = bvs_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x71 = Instruction{name="ADC", handle = adc_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0x75 = Instruction{name="ADC", handle = adc_instruction, cycles = 4, mode = .ZeroPageX},
	0x76 = Instruction{name="ROR", handle = ror_instruction, cycles = 6, mode = .ZeroPageX},
	0x78 = Instruction{name="SEI", handle = sei_instruction, cycles = 2, mode = .Implied},
	0x79 = Instruction{name="ADC", handle = adc_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0x7D = Instruction{name="ADC", handle = adc_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0x7E = Instruction{name="ROR", handle = ror_instruction, cycles = 7, mode = .AbsoluteX},
	0x81 = Instruction{name="STA", handle = sta_instruction, cycles = 6, mode = .XIndirect},
	0x84 = Instruction{name="STY", handle = sty_instruction, cycles = 3, mode = .ZeroPage},
	0x85 = Instruction{name="STA", handle = sta_instruction, cycles = 3, mode = .ZeroPage},
	0x86 = Instruction{name="STX", handle = stx_instruction, cycles = 3, mode = .ZeroPage},
	0x88 = Instruction{name="DEY", handle = dey_instruction, cycles = 2, mode = .Implied},
	0x8A = Instruction{name="TXA", handle = txa_instruction, cycles = 2, mode = .Implied},
	0x8C = Instruction{name="STY", handle = sty_instruction, cycles = 4, mode = .Absolute},
	0x8D = Instruction{name="STA", handle = sta_instruction, cycles = 4, mode = .Absolute},
	0x8E = Instruction{name="STX", handle = stx_instruction, cycles = 4, mode = .Absolute},
	0x90 = Instruction{name="BCC", handle = bcc_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0x91 = Instruction{name="STA", handle = sta_instruction, cycles = 6, mode = .IndirectY},
	0x94 = Instruction{name="STY", handle = sty_instruction, cycles = 4, mode = .ZeroPageX},
	0x95 = Instruction{name="STA", handle = sta_instruction, cycles = 4, mode = .ZeroPageX},
	0x96 = Instruction{name="STX", handle = stx_instruction, cycles = 4, mode = .ZeroPageY},
	0x98 = Instruction{name="TYA", handle = tya_instruction, cycles = 2, mode = .Implied},
	0x99 = Instruction{name="STA", handle = sta_instruction, cycles = 5, mode = .AbsoluteY},
	0x9A = Instruction{name="TXS", handle = txs_instruction, cycles = 2, mode = .Implied},
	0x9D = Instruction{name="STA", handle = sta_instruction, cycles = 5, mode = .AbsoluteX},
	0xA0 = Instruction{name="LDY", handle = ldy_instruction, cycles = 2, mode = .Immediate},
	0xA1 = Instruction{name="LDA", handle = lda_instruction, cycles = 6, mode = .XIndirect},
	0xA2 = Instruction{name="LDX", handle = ldx_instruction, cycles = 2, mode = .Immediate},
	0xA4 = Instruction{name="LDY", handle = ldy_instruction, cycles = 3, mode = .ZeroPage},
	0xA5 = Instruction{name="LDA", handle = lda_instruction, cycles = 3, mode = .ZeroPage},
	0xA6 = Instruction{name="LDX", handle = ldx_instruction, cycles = 3, mode = .ZeroPage},
	0xA8 = Instruction{name="TAY", handle = tay_instruction, cycles = 2, mode = .Implied},
	0xA9 = Instruction{name="LDA", handle = lda_instruction, cycles = 2, mode = .Immediate},
	0xAA = Instruction{name="TAX", handle = tax_instruction, cycles = 2, mode = .Implied},
	0xAC = Instruction{name="LDY", handle = ldy_instruction, cycles = 4, mode = .Absolute},
	0xAD = Instruction{name="LDA", handle = lda_instruction, cycles = 4, mode = .Absolute},
	0xAE = Instruction{name="LDX", handle = ldx_instruction, cycles = 4, mode = .Absolute},
	0xB0 = Instruction{name="BCS", handle = bcs_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0xB1 = Instruction{name="LDA", handle = lda_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0xB4 = Instruction{name="LDY", handle = ldy_instruction, cycles = 4, mode = .ZeroPageX},
	0xB5 = Instruction{name="LDA", handle = lda_instruction, cycles = 4, mode = .ZeroPageX},
	0xB6 = Instruction{name="LDX", handle = ldx_instruction, cycles = 4, mode = .ZeroPageY},
	0xB8 = Instruction{name="CLV", handle = clv_instruction, cycles = 2, mode = .Implied},
	0xB9 = Instruction{name="LDA", handle = lda_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xBA = Instruction{name="TSX", handle = tsx_instruction, cycles = 2, mode = .Implied},
	0xBC = Instruction{name="LDY", handle = ldy_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xBD = Instruction{name="LDA", handle = lda_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xBE = Instruction{name="LDX", handle = ldx_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xC0 = Instruction{name="CPY", handle = cpy_instruction, cycles = 2, mode = .Immediate},
	0xC1 = Instruction{name="CMP", handle = cmp_instruction, cycles = 6, mode = .XIndirect},
	0xC4 = Instruction{name="CPY", handle = cpy_instruction, cycles = 3, mode = .ZeroPage},
	0xC5 = Instruction{name="CMP", handle = cmp_instruction, cycles = 3, mode = .ZeroPage},
	0xC6 = Instruction{name="DEC", handle = dec_instruction, cycles = 5, mode = .ZeroPage},
	0xC8 = Instruction{name="INY", handle = iny_instruction, cycles = 2, mode = .Implied},
	0xC9 = Instruction{name="CMP", handle = cmp_instruction, cycles = 2, mode = .Immediate},
	0xCA = Instruction{name="DEX", handle = dex_instruction, cycles = 2, mode = .Implied},
	0xCC = Instruction{name="CPY", handle = cpy_instruction, cycles = 4, mode = .Absolute},
	0xCD = Instruction{name="CMP", handle = cmp_instruction, cycles = 4, mode = .Absolute},
	0xCE = Instruction{name="DEC", handle = dec_instruction, cycles = 6, mode = .Absolute},
	0xD0 = Instruction{name="BNE", handle = bne_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0xD1 = Instruction{name="CMP", handle = cmp_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0xD5 = Instruction{name="CMP", handle = cmp_instruction, cycles = 4, mode = .ZeroPageX},
	0xD6 = Instruction{name="DEC", handle = dec_instruction, cycles = 6, mode = .ZeroPageX},
	0xD8 = Instruction{name="CLD", handle = cld_instruction, cycles = 2, mode = .Implied},
	0xD9 = Instruction{name="CMP", handle = cmp_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xDD = Instruction{name="CMP", handle = cmp_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xDE = Instruction{name="DEC", handle = dec_instruction, cycles = 7, mode = .AbsoluteX},
	0xE0 = Instruction{name="CPX", handle = cpx_instruction, cycles = 2, mode = .Immediate},
	0xE1 = Instruction{name="SBC", handle = sbc_instruction, cycles = 6, mode = .XIndirect},
	0xE4 = Instruction{name="CPX", handle = cpx_instruction, cycles = 3, mode = .ZeroPage},
	0xE5 = Instruction{name="SBC", handle = sbc_instruction, cycles = 3, mode = .ZeroPage},
	0xE6 = Instruction{name="INC", handle = inc_instruction, cycles = 5, mode = .ZeroPage},
	0xE8 = Instruction{name="INX", handle = inx_instruction, cycles = 2, mode = .Implied},
	0xE9 = Instruction{name="SBC", handle = sbc_instruction, cycles = 2, mode = .Immediate},
	0xEA = Instruction{name="NOP", handle = nop_instruction, cycles = 2, mode = .Implied},
	0xEC = Instruction{name="CPX", handle = cpx_instruction, cycles = 4, mode = .Absolute},
	0xED = Instruction{name="SBC", handle = sbc_instruction, cycles = 4, mode = .Absolute},
	0xEE = Instruction{name="INC", handle = inc_instruction, cycles = 6, mode = .Absolute},
	0xF0 = Instruction{name="BEQ", handle = beq_instruction, cycles = 2, mode = .Relative, cycle_page_crossed = true},
	0xF1 = Instruction{name="SBC", handle = sbc_instruction, cycles = 5, mode = .IndirectY, cycle_page_crossed = true},
	0xF5 = Instruction{name="SBC", handle = sbc_instruction, cycles = 4, mode = .ZeroPageX},
	0xF6 = Instruction{name="INC", handle = inc_instruction, cycles = 6, mode = .ZeroPageX},
	0xF8 = Instruction{name="SED", handle = sed_instruction, cycles = 2, mode = .Implied},
	0xF9 = Instruction{name="SBC", handle = sbc_instruction, cycles = 4, mode = .AbsoluteY, cycle_page_crossed = true},
	0xFD = Instruction{name="SBC", handle = sbc_instruction, cycles = 4, mode = .AbsoluteX, cycle_page_crossed = true},
	0xFE = Instruction{name="INC", handle = inc_instruction, cycles = 7, mode = .AbsoluteX},
}

AddressingHelper :: struct {
	handle: proc(_: ^CPU, _: []byte) -> u16,
	bytes:  uint,
}
addressing_helpers := [AddressMode]AddressingHelper {
	.Implied = {handle = proc(cpu: ^CPU, _: []byte) -> u16 {return 0}, bytes = 0},
	.Accumulator = {handle = proc(cpu: ^CPU, _: []byte) -> u16 {return u16(cpu.registers.accumulator)}, bytes = 0},
	.Immediate = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return immediate(data)}, bytes = 1},
	.ZeroPage = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return zeropage(data)}, bytes = 1},
	.ZeroPageX = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return zeropage(data, cpu.registers.x)}, bytes = 1},
	.ZeroPageY = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return zeropage(data, cpu.registers.y)}, bytes = 1},
	.Relative = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return relative(cpu, data)}, bytes = 1},
	.Absolute = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return absolute(data)}, bytes = 2},
	.AbsoluteX = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return absolute(data, cpu.registers.x)}, bytes = 2},
	.AbsoluteY = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return absolute(data, cpu.registers.y)}, bytes = 2},
	.Indirect = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return absolute(data)}, bytes = 2},
	.XIndirect = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return x_zp_indirect(cpu, data)}, bytes = 1},
	.IndirectY = {handle = proc(cpu: ^CPU, data: []byte) -> u16 {return zp_indirect_y(cpu, data)}, bytes = 1},
}

execute_instruction :: proc(cpu: ^CPU) {
	op_code := cpu.memory.raw[cpu.registers.program_counter]
	data := cpu.memory.raw[cpu.registers.program_counter + 1:]
	instruction := instruction_handles[op_code]

	if (instruction.handle != nil) {
		addressing := addressing_helpers[instruction.mode]
		address := addressing.handle(cpu, data)

		// Pages on the 6502 are 256 bytes long. If the address crosses a page boundary, we add an extra cycle when needed.
		if (instruction.cycle_page_crossed && (address & 0xFF00) != (cpu.registers.program_counter & 0xFF00)) {
			cpu.cycles += 1
		}

		switch ins in instruction.handle {
			case proc(_: ^CPU):
				ins(cpu)
			case proc(_: ^CPU, _: u16):
				ins(cpu, address)
			case proc(_: ^CPU, _: ^byte):
				if instruction.mode == .Immediate {
					value := u8(address)
					ins(cpu, &value)
				} else  if instruction.mode == .Accumulator {
					ins(cpu, &cpu.registers.accumulator)
				}
				 else {
					ins(cpu, &cpu.memory.raw[address])
				}
		}
		cpu.cycles += instruction.cycles

		if (instruction.changes_pc == false) {
			cpu.registers.program_counter += u16(addressing.bytes) + 1
		}
	}
	 else {
		fmt.assertf(false, "Unknown OP-code encountered: [%02d]", op_code)
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
	push_byte_on_stack(cpu, transmute(byte)(cpu.registers.flags | {.Break, .Bit5}))
}

nop_instruction :: proc(cpu: ^CPU) {
}

pla_instruction :: proc(cpu: ^CPU) {
	push_byte_on_stack(cpu, cpu.registers.accumulator)

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
		((((u16(~(cpu.registers.accumulator ~ data^))) & ((u16(cpu.registers.accumulator) ~ addition))) &
				0b1000_0000) >
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

	push_byte_on_stack(cpu, byte(stack_pc & 0xFF00 >> 8))
	push_byte_on_stack(cpu, byte(stack_pc & 0x00FF))

	cpu.registers.program_counter = address
}

bit_instruction :: proc(cpu: ^CPU, data: ^byte) {
	and_result := cpu.registers.accumulator & data^

	_set_mask(&cpu.registers.flags, {.Negative}, (data^ & (1 << 7)) > 0) // N = 7th bit of data
	_set_mask(&cpu.registers.flags, {.Overflow}, (data^ & (1 << 6)) > 0) // V = 6th bit of data
	_set_mask(&cpu.registers.flags, {.Zero}, (and_result == 0))
}

_branch_instruction :: #force_inline proc(cpu: ^CPU, address: u16, condition: bool) {
	if (condition) {
		cpu.registers.program_counter = address
	}
}

bpl_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Negative not_in cpu.registers.flags)
}

bmi_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Negative in cpu.registers.flags)
}

bne_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Zero not_in cpu.registers.flags)
}

beq_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Zero in cpu.registers.flags)
}

bcc_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Carry not_in cpu.registers.flags)
}

bcs_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Carry in cpu.registers.flags)
}

bvc_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Overflow in cpu.registers.flags)
}

bvs_instruction :: proc(cpu: ^CPU, address: u16) {
	_branch_instruction(cpu, address, .Overflow in cpu.registers.flags)
}

brk_instruction :: proc(cpu: ^CPU) {
	stack_pc := cpu.registers.program_counter + 2

	push_byte_on_stack(cpu, byte(stack_pc & 0xFF00 >> 8))
	push_byte_on_stack(cpu, byte(stack_pc & 0x00FF))

	cpu.registers.program_counter += 1
}
