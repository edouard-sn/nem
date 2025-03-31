package emulator

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

AddressingHelper :: struct {
	handle: proc(_: ^CPU) -> (address: u16, page_crossed: bool),
	bytes:  uint,
}

addressing_helpers := [AddressMode]AddressingHelper {
	.Implied = {handle = proc(cpu: ^CPU) -> (u16, bool) {return 0, false}, bytes = 0},
	.Accumulator = {handle = proc(cpu: ^CPU) -> (u16, bool) {return u16(cpu.registers.accumulator), false}, bytes = 0},
	.Immediate = {handle = proc(cpu: ^CPU) -> (u16, bool) {return immediate(cpu)}, bytes = 1},
	.ZeroPage = {handle = proc(cpu: ^CPU) -> (u16, bool) {return zeropage(cpu)}, bytes = 1},
	.ZeroPageX = {handle = proc(cpu: ^CPU) -> (u16, bool) {return zeropage(cpu, cpu.registers.x)}, bytes = 1},
	.ZeroPageY = {handle = proc(cpu: ^CPU) -> (u16, bool) {return zeropage(cpu, cpu.registers.y)}, bytes = 1},
	.Relative = {handle = proc(cpu: ^CPU) -> (u16, bool) {return relative(cpu)}, bytes = 1},
	.Absolute = {handle = proc(cpu: ^CPU) -> (u16, bool) {return absolute(cpu)}, bytes = 2},
	.AbsoluteX = {handle = proc(cpu: ^CPU) -> (u16, bool) {return absolute(cpu, cpu.registers.x)}, bytes = 2},
	.AbsoluteY = {handle = proc(cpu: ^CPU) -> (u16, bool) {return absolute(cpu, cpu.registers.y)}, bytes = 2},
	.Indirect = {handle = proc(cpu: ^CPU) -> (u16, bool) {return indirect(cpu)}, bytes = 2},
	.XIndirect = {handle = proc(cpu: ^CPU) -> (u16, bool) {return x_zp_indirect(cpu)}, bytes = 1},
	.IndirectY = {handle = proc(cpu: ^CPU) -> (u16, bool) {return zp_indirect_y(cpu)}, bytes = 1},
}


@(private = "file")
x_zp_indirect :: proc(cpu: ^CPU) -> (u16, bool) {
	operand_addr := cpu.registers.program_counter + 1
	temp_address := (u16(bus_read_byte(cpu.bus, operand_addr)) + u16(cpu.registers.x))

	// Wrap both access around zero page zone
	lo := bus_read_byte(cpu.bus, temp_address & 0xFF)
	hi := bus_read_byte(cpu.bus, (temp_address + 1) & 0xFF)

	return (u16(hi) << 8 | u16(lo)), false
}

@(private = "file")
zp_indirect_y :: proc(cpu: ^CPU) -> (u16, bool) {
	operand_addr := cpu.registers.program_counter + 1
	temp_address := bus_read_byte(cpu.bus, operand_addr)

	lo := bus_read_byte(cpu.bus, u16(temp_address))
	hi := bus_read_byte(cpu.bus, u16(temp_address + 1))
	no_offset := (u16(hi) << 8 | u16(lo))
	target := no_offset + u16(cpu.registers.y)

	return target, cpu.registers.y > (0xFF - lo)
}

@(private = "file")
indirect :: proc(cpu: ^CPU) -> (u16, bool) {
	operand_addr := cpu.registers.program_counter + 1

	lo := bus_read_byte(cpu.bus, u16(operand_addr))
	hi := bus_read_byte(cpu.bus, u16(operand_addr + 1))
	address := (u16(hi) << 8 | u16(lo))

	target_lo := bus_read_byte(cpu.bus, address)
	target_hi := bus_read_byte(cpu.bus, ((address + 1) & 0xFF) | (address & 0xFF00)) // Wrap over page
	target := (u16(target_hi) << 8) | u16(target_lo)

	return target, false
}

@(private = "file")
zeropage :: #force_inline proc(cpu: ^CPU, indexed_value: byte = 0) -> (u16, bool) {
	operand_addr := cpu.registers.program_counter + 1
	// Wrap around zeropage, no page boundary cross
	return (u16(bus_read_byte(cpu.bus, operand_addr)) + u16(indexed_value)) & 0xFF, false
}

@(private = "file")
immediate :: #force_inline proc(cpu: ^CPU) -> (u16, bool) {
	operand_addr := cpu.registers.program_counter + 1
	return u16(bus_read_byte(cpu.bus, operand_addr)), false
}

@(private = "file")
absolute :: #force_inline proc(cpu: ^CPU, indexed_value: byte = 0) -> (u16, bool) {
	operand_addr := cpu.registers.program_counter + 1
	lo := bus_read_byte(cpu.bus, operand_addr)
	hi := bus_read_byte(cpu.bus, operand_addr + 1)

	return (u16(hi) << 8 | u16(lo)) + u16(indexed_value), indexed_value > (0xFF - lo)
}

@(private = "file")
relative :: #force_inline proc(cpu: ^CPU) -> (u16, bool) {
	operand_addr := cpu.registers.program_counter + 1
	offset := i16(i8(bus_read_byte(cpu.bus, operand_addr)))
	return u16(i16(cpu.registers.program_counter + 2) + offset), false
}
