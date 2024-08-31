package cpu

// NOTE: Would it be worth it to remove the use of union so we can make the array const?
InstructionHandle :: union {
	proc(cpu: ^CPU), // No data processing
	proc(cpu: ^CPU, data: byte), // Read-only
	proc(cpu: ^CPU, address: u16), // Direct address access and write operations
}

Instruction :: struct {
	// Instruction name
	name:               string,

	// Instruction procedure logic
	handle:             InstructionHandle,

	// How do we get the data needed
	mode:               AddressMode,

	// How many cycles does this instruction take
	cycles:             uint,

	// Does this instruction take an extra cycle if the page is crossed
	cycle_page_crossed: bool,

	// Is PC changed by this instruction
	changes_pc:         bool,
	official:           bool,
}

@(private)
_set_mask :: #force_inline proc(flags: ^ProcStatus, mask: ProcStatus, value := true) {
	flags^ = value ? (flags^ | mask) : (flags^ & ~mask)
}

// Sets the Negative or Zero flag depending on nb
@(private)
_zero_or_neg_flags :: #force_inline proc(flags: ^ProcStatus, nb: byte) {
	_set_mask(flags, {.Zero}, nb == 0)
	_set_mask(flags, {.Negative}, i8(nb) < 0)
}

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

pha_instruction :: proc(cpu: ^CPU) {
	push_byte_on_stack(cpu, cpu.registers.accumulator)
}

pla_instruction :: proc(cpu: ^CPU) {
	cpu.registers.accumulator = pull_byte_from_stack(cpu)

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

plp_instruction :: proc(cpu: ^CPU) {
	break_bit5_save := (cpu.registers.flags & {.Break, .Bit5})
	cpu.registers.flags = transmute(ProcStatus)pull_byte_from_stack(cpu) & ~{.Break, .Bit5}
	cpu.registers.flags |= break_bit5_save
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
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

lsr_instruction :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 1 != 0))
	cpu->write(address, data >> 1)

	cpu.registers.flags &= ~{.Negative}

	_zero_or_neg_flags(&cpu.registers.flags, cpu->read(address))
}

asl_instruction :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 0b1000_0000 != 0))
	data <<= 1
	cpu->write(address, data)
	_zero_or_neg_flags(&cpu.registers.flags, data)
}

ora_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator |= data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

and_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator &= data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

eor_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator ~= data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

_cmp_instruction :: proc(flags: ^ProcStatus, lhs: byte, rhs: byte) {
	_set_mask(flags, {.Carry}, lhs >= rhs)
	sub := lhs - rhs
	_zero_or_neg_flags(flags, sub)
}

cmp_instruction :: proc(cpu: ^CPU, data: byte) {
	_cmp_instruction(&cpu.registers.flags, cpu.registers.accumulator, data)
}

cpx_instruction :: proc(cpu: ^CPU, data: byte) {
	_cmp_instruction(&cpu.registers.flags, cpu.registers.x, data)
}

cpy_instruction :: proc(cpu: ^CPU, data: byte) {
	_cmp_instruction(&cpu.registers.flags, cpu.registers.y, data)
}

sta_instruction :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.accumulator)
}

stx_instruction :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.x)
}

sty_instruction :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.y)
}

lda_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

ldy_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.y = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

ldx_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.x = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

rol_instruction :: proc(cpu: ^CPU, address: u16) {
	old_carry := .Carry in cpu.registers.flags

	data := cpu->read(address)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 0b1000_0000 != 0))
	data <<= 1

	if (old_carry) {
		data |= 0b0000_0001
	} else {
		data &= 0b1111_1110
	}

	cpu->write(address, data)
	_zero_or_neg_flags(&cpu.registers.flags, data)
}

ror_instruction :: proc(cpu: ^CPU, address: u16) {
	old_carry := .Carry in cpu.registers.flags
	data := cpu->read(address)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 0b0000_0001 != 0))
	data >>= 1

	if (old_carry) {
		data |= 0b1000_0000
	} else {
		data &= 0b0111_1111
	}
	cpu->write(address, data)

	_zero_or_neg_flags(&cpu.registers.flags, data)
}

rti_instruction :: proc(cpu: ^CPU) {
	// ignore break and bit5 inline lol
	cpu.registers.flags =
		(transmute(ProcStatus)pull_byte_from_stack(cpu) & ~{.Break, .Bit5}) | (cpu.registers.flags & {.Break, .Bit5})
	cpu.registers.program_counter = pull_u16_from_stack(cpu)
}

rts_instruction :: proc(cpu: ^CPU) {
	cpu.registers.program_counter = pull_u16_from_stack(cpu) + 1
}

adc_instruction :: proc(cpu: ^CPU, data: byte) {
	addition := u16(cpu.registers.accumulator) + u16(data) + u16(.Carry in cpu.registers.flags)
	_set_mask(&cpu.registers.flags, {.Carry}, addition > 0xFF)

	// Shenanigans https://stackoverflow.com/questions/29193303/6502-emulation-proper-way-to-implement-adc-and-sbc
	overflow: bool =
		((((u16(~(cpu.registers.accumulator ~ data))) & ((u16(cpu.registers.accumulator) ~ addition))) & 0b1000_0000) >
			0)
	_set_mask(&cpu.registers.flags, {.Overflow}, overflow)
	cpu.registers.accumulator = byte(addition & 0xFF)
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

sbc_instruction :: proc(cpu: ^CPU, data: byte) {
	complemented := ~data // -data - 1
	adc_instruction(cpu, complemented)
}

inc_instruction :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	data += 1
	cpu->write(address, data)
	_zero_or_neg_flags(&cpu.registers.flags, data)
}

dec_instruction :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	data -= 1
	cpu->write(address, data)
	_zero_or_neg_flags(&cpu.registers.flags, data)
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

bit_instruction :: proc(cpu: ^CPU, data: byte) {
	and_result := cpu.registers.accumulator & data

	_set_mask(&cpu.registers.flags, {.Negative}, (data & (1 << 7)) > 0) // N = 7th bit of data
	_set_mask(&cpu.registers.flags, {.Overflow}, (data & (1 << 6)) > 0) // V = 6th bit of data
	_set_mask(&cpu.registers.flags, {.Zero}, (and_result == 0))
}

_branch_instruction :: #force_inline proc(cpu: ^CPU, address: u16, condition: bool) {
	if (condition) {
		old_pc := cpu.registers.program_counter + 2
		cpu.registers.program_counter = address
		// Branching takes 1 cycle
		cpu.cycles += 1
		// If the branch jumps to a different page, add an extra cycle
		if ((address & 0xFF00) != (old_pc & 0xFF00)) {
			cpu.cycles += 1
		}
	} else {
		cpu.registers.program_counter += 2
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
	_branch_instruction(cpu, address, .Overflow not_in cpu.registers.flags)
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

jam_instruction :: proc(cpu: ^CPU) {
	// TODO: Will change JAM behavior

	panic("NES jammed :(")
}

slo_instruction :: proc(cpu: ^CPU, address: u16) {
	asl_instruction(cpu, address)
	ora_instruction(cpu, cpu->read(address))
}

anc_instruction :: proc(cpu: ^CPU, data: byte) {
	and_instruction(cpu, data)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 0b1000_0000 != 0))
}

rla_instruction :: proc(cpu: ^CPU, address: u16) {
	rol_instruction(cpu, address)
	and_instruction(cpu, cpu->read(address))
}

sre_instruction :: proc(cpu: ^CPU, address: u16) {
	lsr_instruction(cpu, address)
	eor_instruction(cpu, cpu->read(address))
}

alr_instruction :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	and_instruction(cpu, data)
	_set_mask(&cpu.registers.flags, {.Carry}, (cpu.registers.accumulator & 1 != 0))
	cpu.registers.accumulator >>= 1
	cpu.registers.flags &= ~{.Negative}
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

rra_instruction :: proc(cpu: ^CPU, address: u16) {
	ror_instruction(cpu, address)
	adc_instruction(cpu, cpu->read(address))
}

arr_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator &= data
	cpu.registers.accumulator >>= 1
	cpu.registers.accumulator |= u8(.Carry in cpu.registers.flags) << 7

	_set_mask(&cpu.registers.flags, {.Negative}, .Carry in cpu.registers.flags)
	_set_mask(&cpu.registers.flags, {.Zero}, cpu.registers.accumulator == 0)
	_set_mask(&cpu.registers.flags, {.Carry}, ((cpu.registers.accumulator >> 6) & 1) > 0) // Old 7th bit goes into carry

	// http://forum.6502.org/viewtopic.php?f=4&t=3493&start=105#p44620
	overflow := ((cpu.registers.accumulator >> 5) & 1) ~ ((cpu.registers.accumulator >> 6) & 1)
	_set_mask(&cpu.registers.flags, {.Overflow}, overflow > 0)
}

sax_instruction :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.x & cpu.registers.accumulator)
}

xaa_instruction :: proc(cpu: ^CPU, data: byte) {
	MAGIC :: #config(XAA_MAGIC, u8(0xEE))
	cpu.registers.accumulator = ((cpu.registers.accumulator | MAGIC) & cpu.registers.x) & data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

ahx_instruction :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu->write(address, (cpu.registers.accumulator & cpu.registers.x & ah))
}

tas_instruction :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu.registers.stack_pointer = cpu.registers.accumulator & cpu.registers.x
	cpu->write(address, cpu.registers.stack_pointer & ah)
}

shy_instruction :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu->write(address, cpu.registers.y & ah)
}

shx_instruction :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu->write(address, cpu.registers.x & ah)
}

lax_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator = data
	cpu.registers.x = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

las_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.x = data & cpu.registers.stack_pointer
	cpu.registers.accumulator = cpu.registers.x
	cpu.registers.stack_pointer = cpu.registers.x
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

dcp_instruction :: proc(cpu: ^CPU, address: u16) {
	dec_instruction(cpu, address)
	cmp_instruction(cpu, cpu->read(address))
}

sbx_instruction :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.x = cpu.registers.accumulator & cpu.registers.x - data
	cmp_instruction(cpu, cpu.registers.x)
}

isb_instruction :: proc(cpu: ^CPU, address: u16) {
	inc_instruction(cpu, address)
	sbc_instruction(cpu, cpu->read(address))
}
