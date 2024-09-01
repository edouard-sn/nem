package emulator

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

cpu_instruction_tax :: proc(cpu: ^CPU) {
	cpu.registers.x = cpu.registers.accumulator

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

cpu_instruction_txa :: proc(cpu: ^CPU) {
	cpu.registers.accumulator = cpu.registers.x

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_tay :: proc(cpu: ^CPU) {
	cpu.registers.y = cpu.registers.accumulator

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

cpu_instruction_tya :: proc(cpu: ^CPU) {
	cpu.registers.accumulator = cpu.registers.y

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_inx :: proc(cpu: ^CPU) {
	cpu.registers.x += 1

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

cpu_instruction_dex :: proc(cpu: ^CPU) {
	cpu.registers.x -= 1

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

cpu_instruction_iny :: proc(cpu: ^CPU) {
	cpu.registers.y += 1

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

cpu_instruction_dey :: proc(cpu: ^CPU) {
	cpu.registers.y -= 1

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

cpu_instruction_php :: proc(cpu: ^CPU) {
	cpu_stack_push(cpu, transmute(byte)(cpu.registers.flags | {.Break, .Bit5}))
}

cpu_instruction_nop :: proc(cpu: ^CPU) {
}

cpu_instruction_pha :: proc(cpu: ^CPU) {
	cpu_stack_push(cpu, cpu.registers.accumulator)
}

cpu_instruction_pla :: proc(cpu: ^CPU) {
	cpu.registers.accumulator = cpu_stack_pull(cpu)

	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_plp :: proc(cpu: ^CPU) {
	break_bit5_save := (cpu.registers.flags & {.Break, .Bit5})
	cpu.registers.flags = transmute(ProcStatus)cpu_stack_pull(cpu) & ~{.Break, .Bit5}
	cpu.registers.flags |= break_bit5_save
}

cpu_instruction_clc :: proc(cpu: ^CPU) {
	cpu.registers.flags &= ~{.Carry}
}

cpu_instruction_sec :: proc(cpu: ^CPU) {
	cpu.registers.flags |= {.Carry}
}

cpu_instruction_sed :: proc(cpu: ^CPU) {
	cpu.registers.flags |= {.Decimal}
}

cpu_instruction_sei :: proc(cpu: ^CPU) {
	cpu.registers.flags |= {.Interupt}
}

cpu_instruction_cli :: proc(cpu: ^CPU) {
	cpu.registers.flags &= ~{.Interupt}
}

cpu_instruction_clv :: proc(cpu: ^CPU) {
	cpu.registers.flags &= ~{.Overflow}
}

cpu_instruction_cld :: proc(cpu: ^CPU) {
	cpu.registers.flags &= ~{.Decimal}
}

cpu_instruction_txs :: proc(cpu: ^CPU) {
	cpu.registers.stack_pointer = cpu.registers.x
}

cpu_instruction_tsx :: proc(cpu: ^CPU) {
	cpu.registers.x = cpu.registers.stack_pointer
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

cpu_instruction_lsr :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 1 != 0))
	cpu->write(address, data >> 1)

	cpu.registers.flags &= ~{.Negative}

	_zero_or_neg_flags(&cpu.registers.flags, cpu->read(address))
}

cpu_instruction_asl :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 0b1000_0000 != 0))
	data <<= 1
	cpu->write(address, data)
	_zero_or_neg_flags(&cpu.registers.flags, data)
}

cpu_instruction_ora :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator |= data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_and :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator &= data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_eor :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator ~= data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction__cmp :: proc(flags: ^ProcStatus, lhs: byte, rhs: byte) {
	_set_mask(flags, {.Carry}, lhs >= rhs)
	sub := lhs - rhs
	_zero_or_neg_flags(flags, sub)
}

cpu_instruction_cmp :: proc(cpu: ^CPU, data: byte) {
	cpu_instruction__cmp(&cpu.registers.flags, cpu.registers.accumulator, data)
}

cpu_instruction_cpx :: proc(cpu: ^CPU, data: byte) {
	cpu_instruction__cmp(&cpu.registers.flags, cpu.registers.x, data)
}

cpu_instruction_cpy :: proc(cpu: ^CPU, data: byte) {
	cpu_instruction__cmp(&cpu.registers.flags, cpu.registers.y, data)
}

cpu_instruction_sta :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.accumulator)
}

cpu_instruction_stx :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.x)
}

cpu_instruction_sty :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.y)
}

cpu_instruction_lda :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_ldy :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.y = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.y)
}

cpu_instruction_ldx :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.x = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

cpu_instruction_rol :: proc(cpu: ^CPU, address: u16) {
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

cpu_instruction_ror :: proc(cpu: ^CPU, address: u16) {
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

cpu_instruction_rti :: proc(cpu: ^CPU) {
	// ignore break and bit5 inline lol
	cpu.registers.flags =
		(transmute(ProcStatus)cpu_stack_pull(cpu) & ~{.Break, .Bit5}) | (cpu.registers.flags & {.Break, .Bit5})
	cpu.registers.program_counter = cpu_stack_pull_u16(cpu)
}

cpu_instruction_rts :: proc(cpu: ^CPU) {
	cpu.registers.program_counter = cpu_stack_pull_u16(cpu) + 1
}

cpu_instruction_adc :: proc(cpu: ^CPU, data: byte) {
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

cpu_instruction_sbc :: proc(cpu: ^CPU, data: byte) {
	complemented := ~data // -data - 1
	cpu_instruction_adc(cpu, complemented)
}

cpu_instruction_inc :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	data += 1
	cpu->write(address, data)
	_zero_or_neg_flags(&cpu.registers.flags, data)
}

cpu_instruction_dec :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	data -= 1
	cpu->write(address, data)
	_zero_or_neg_flags(&cpu.registers.flags, data)
}

cpu_instruction_jmp :: proc(cpu: ^CPU, address: u16) {
	cpu.registers.program_counter = address
}

cpu_instruction_jsr :: proc(cpu: ^CPU, address: u16) {
	stack_pc := cpu.registers.program_counter + 2

	cpu_stack_push(cpu, byte(stack_pc & 0xFF00 >> 8))
	cpu_stack_push(cpu, byte(stack_pc & 0x00FF))

	cpu.registers.program_counter = address
}

cpu_instruction_bit :: proc(cpu: ^CPU, data: byte) {
	and_result := cpu.registers.accumulator & data

	_set_mask(&cpu.registers.flags, {.Negative}, (data & (1 << 7)) > 0) // N = 7th bit of data
	_set_mask(&cpu.registers.flags, {.Overflow}, (data & (1 << 6)) > 0) // V = 6th bit of data
	_set_mask(&cpu.registers.flags, {.Zero}, (and_result == 0))
}

cpu_instruction__branch :: #force_inline proc(cpu: ^CPU, address: u16, condition: bool) {
	if (condition) {
		old_pc := cpu.registers.program_counter + 2
		cpu.registers.program_counter = address
		// Branching takes 1 cycle
		cpu_tick(cpu, 1)
		// If the branch jumps to a different page, add an extra cycle
		if ((address & 0xFF00) != (old_pc & 0xFF00)) {
			cpu_tick(cpu, 1)
		}
	} else {
		cpu.registers.program_counter += 2
	}
}

cpu_instruction_bpl :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Negative not_in cpu.registers.flags)
}

cpu_instruction_bmi :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Negative in cpu.registers.flags)
}

cpu_instruction_bne :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Zero not_in cpu.registers.flags)
}

cpu_instruction_beq :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Zero in cpu.registers.flags)
}

cpu_instruction_bcc :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Carry not_in cpu.registers.flags)
}

cpu_instruction_bcs :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Carry in cpu.registers.flags)
}

cpu_instruction_bvc :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Overflow not_in cpu.registers.flags)
}

cpu_instruction_bvs :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction__branch(cpu, address, .Overflow in cpu.registers.flags)
}

cpu_instruction_brk :: proc(cpu: ^CPU) {
	stack_pc := cpu.registers.program_counter + 2

	cpu_stack_push(cpu, byte(stack_pc & 0xFF00 >> 8))
	cpu_stack_push(cpu, byte(stack_pc & 0x00FF))

	cpu.registers.program_counter += 1
}

cpu_instruction_jam :: proc(cpu: ^CPU) {
	// TODO: Will change JAM behavior

	panic("NES jammed :(")
}

cpu_instruction_slo :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction_asl(cpu, address)
	cpu_instruction_ora(cpu, cpu->read(address))
}

cpu_instruction_anc :: proc(cpu: ^CPU, data: byte) {
	cpu_instruction_and(cpu, data)
	_set_mask(&cpu.registers.flags, {.Carry}, (data & 0b1000_0000 != 0))
}

cpu_instruction_rla :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction_rol(cpu, address)
	cpu_instruction_and(cpu, cpu->read(address))
}

cpu_instruction_sre :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction_lsr(cpu, address)
	cpu_instruction_eor(cpu, cpu->read(address))
}

cpu_instruction_alr :: proc(cpu: ^CPU, address: u16) {
	data := cpu->read(address)
	cpu_instruction_and(cpu, data)
	_set_mask(&cpu.registers.flags, {.Carry}, (cpu.registers.accumulator & 1 != 0))
	cpu.registers.accumulator >>= 1
	cpu.registers.flags &= ~{.Negative}
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_rra :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction_ror(cpu, address)
	cpu_instruction_adc(cpu, cpu->read(address))
}

cpu_instruction_arr :: proc(cpu: ^CPU, data: byte) {
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

cpu_instruction_sax :: proc(cpu: ^CPU, address: u16) {
	cpu->write(address, cpu.registers.x & cpu.registers.accumulator)
}

cpu_instruction_xaa :: proc(cpu: ^CPU, data: byte) {
	MAGIC :: #config(XAA_MAGIC, u8(0xEE))
	cpu.registers.accumulator = ((cpu.registers.accumulator | MAGIC) & cpu.registers.x) & data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_ahx :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu->write(address, (cpu.registers.accumulator & cpu.registers.x & ah))
}

cpu_instruction_tas :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu.registers.stack_pointer = cpu.registers.accumulator & cpu.registers.x
	cpu->write(address, cpu.registers.stack_pointer & ah)
}

cpu_instruction_shy :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu->write(address, cpu.registers.y & ah)
}

cpu_instruction_shx :: proc(cpu: ^CPU, address: u16) {
	ah := u8((address + 1) >> 8)
	cpu->write(address, cpu.registers.x & ah)
}

cpu_instruction_lax :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.accumulator = data
	cpu.registers.x = data
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.accumulator)
}

cpu_instruction_las :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.x = data & cpu.registers.stack_pointer
	cpu.registers.accumulator = cpu.registers.x
	cpu.registers.stack_pointer = cpu.registers.x
	_zero_or_neg_flags(&cpu.registers.flags, cpu.registers.x)
}

cpu_instruction_dcp :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction_dec(cpu, address)
	cpu_instruction_cmp(cpu, cpu->read(address))
}

cpu_instruction_sbx :: proc(cpu: ^CPU, data: byte) {
	cpu.registers.x = cpu.registers.accumulator & cpu.registers.x - data
	cpu_instruction_cmp(cpu, cpu.registers.x)
}

cpu_instruction_isb :: proc(cpu: ^CPU, address: u16) {
	cpu_instruction_inc(cpu, address)
	cpu_instruction_sbc(cpu, cpu->read(address))
}
