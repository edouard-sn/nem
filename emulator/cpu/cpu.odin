// Emulates a 6502 NES cpu
package cpu

import "core:fmt"

CPU :: struct {
	registers: CPURegisters,
	memory:    CPUMemory,
	cycles:    uint,
}

@(private)
CPURegisters :: struct {
	program_counter: u16,
	stack_pointer:   byte,
	accumulator:     byte,
	x:               byte,
	y:               byte,
	flags:           CPUFlagRegistry,
}

@(private)
CPUFlagRegistry :: bit_set[enum {
	Carry,
	Zero,
	Interupt, // IRQ disable
	Decimal, // (BCD for arithmetics)
	Break,
	Bit5,
	Overflow,
	Negative,
};byte]


@(private)
CPU_MEMORY_SIZE :: 0x10000

@(private)
CPUMemory :: struct {
	raw:              ^[CPU_MEMORY_SIZE]byte,

	//  memory slices
	ram:              []byte,
	ram_map:          struct {
		zero_page: []byte,
		stack:     []byte,
		ram:       []byte,
		mirrors:   []byte,
	},
	io_registers:     []byte,
	io_registers_map: struct {
		first:   []byte,
		mirrors: []byte,
		second:  []byte,
	},
	expansion_rom:    []byte,
	sram:             []byte,
	prg_rom:          struct {
		lower: []byte,
		upper: []byte,
	},
}

dump_registers :: proc(cpu: ^CPU) {
	fmt.printf("PC: %x\n", cpu.registers.program_counter)
	fmt.printf("SP: %x\n", cpu.registers.stack_pointer)
	fmt.printf("A: %x\n", cpu.registers.accumulator)
	fmt.printf("X: %x\n", cpu.registers.x)
	fmt.printf("Y: %x\n", cpu.registers.y)
	fmt.printf("Flags: %v\n", cpu.registers.flags)
}

new_cpu :: proc() -> CPU {
	cpu: CPU
	memory := &cpu.memory

	memory.raw = new([CPU_MEMORY_SIZE]byte)

	// CPU RAM map init
	memory.ram = memory.raw[0x0000:0x2000]

	memory.ram_map.zero_page = memory.raw[0x0000:0x0100]
	memory.ram_map.stack = memory.raw[0x0100:0x0200]
	memory.ram_map.ram = memory.raw[0x0200:0x0800]
	memory.ram_map.mirrors = memory.raw[0x0800:0x2000]

	cpu.registers.stack_pointer = 0xFF // 0x01FF - 0x0100

	// CPU IO Registers map init
	memory.io_registers = memory.raw[0x2000:0x401F]
	memory.io_registers_map.first = memory.raw[0x2000:0x2008]
	memory.io_registers_map.mirrors = memory.raw[0x2008:0x4000]
	memory.io_registers_map.second = memory.raw[0x4000:0x4020]

	// CPU Expansion ROM map init
	memory.expansion_rom = memory.raw[0x4020:0x6000]

	// CPU Expansion SRAM map init
	memory.sram = memory.raw[0x6000:0x8000]

	// CPU Expansion PRG ROM map init
	memory.prg_rom.lower = memory.raw[0x8000:0xC000]
	memory.prg_rom.upper = memory.raw[0xC000:0x10000]

	return cpu
}

read_byte :: proc(cpu: ^CPU, address: u16) -> byte {
	return cpu.memory.raw[address]
}

read_u16 :: proc(cpu: ^CPU, address: u16) -> u16 {
	return u16(cpu.memory.raw[address]) | (u16(cpu.memory.raw[address + 1]) << 8)
}

write_u16 :: proc(cpu: ^CPU, address: u16, value: u16) {
	cpu.memory.raw[address] = u8(value & 0xFF)
	cpu.memory.raw[address + 1] = u8(value >> 8)
}

push_byte_on_stack :: #force_inline proc(cpu: ^CPU, value: byte) -> bool {
	fmt.assertf(
		cpu.registers.stack_pointer >= size_of(byte), 
		"Stack overflow! %x won't be pushed to the stack. size_of(byte) = %d. stack_pointer = %x\n",
		value,
		size_of(byte),
		cpu.registers.stack_pointer,
	)

	stack := cpu.memory.ram_map.stack
	stack[cpu.registers.stack_pointer] = value
	cpu.registers.stack_pointer -= size_of(byte)

	return true
}

pull_byte_from_stack :: #force_inline proc(cpu: ^CPU) -> byte {
	fmt.assertf(
		(cpu.registers.stack_pointer < 0xFF), 
		"Stack underflow! No value will be pulled from the stack, value will be 0. stack_pointer = %x\n",
		cpu.registers.stack_pointer,
	)

	cpu.registers.stack_pointer += size_of(byte)
	value := cpu.memory.ram_map.stack[cpu.registers.stack_pointer]
	return value
}

pull_u16_from_stack :: #force_inline proc(cpu: ^CPU) -> u16 {
	return u16(pull_byte_from_stack(cpu)) | (u16(pull_byte_from_stack(cpu)) << 8)
}

x_zp_indirect :: proc(cpu: ^CPU, data: []byte) -> u16 {
	temp_address := u16(data[0]) + u16(cpu.registers.x)

	lo := cpu.memory.raw[temp_address]
	hi := cpu.memory.raw[temp_address + 1]

	return u16(hi) << 8 | u16(lo)
}

zp_indirect_y :: proc(cpu: ^CPU, data: []byte) -> u16 {
	temp_address := data[0]

	lo := cpu.memory.raw[temp_address]
	hi := cpu.memory.raw[temp_address + 1]

	return (u16(hi) << 8 | u16(lo)) + u16(cpu.registers.y)
}

zeropage :: #force_inline proc(data: []byte, indexed_value: byte = 0) -> u16 {
	return u16(data[0]) + u16(indexed_value)
}

immediate :: #force_inline proc(data: []byte) -> u16 {
	return u16(data[0])
}

absolute :: #force_inline proc(data: []byte, indexed_value: byte = 0) -> u16 {
	lo := data[0]
	hi := data[1]

	return u16(hi) << 8 | u16(lo) + u16(indexed_value)
}

relative :: #force_inline proc(cpu: ^CPU, data: []byte) -> u16 {
	offset := data[0]
	return u16(i16(cpu.registers.program_counter) + i16(i8(offset)))
}

reset_interupt :: proc(cpu: ^CPU) {
	cpu.registers.flags = {}
	cpu.registers.x = 0
	cpu.registers.accumulator = 0
	cpu.registers.program_counter = read_u16(cpu, 0xFFFC)
}
