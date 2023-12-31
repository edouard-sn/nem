// Emulates a 6502 NES cpu
package cpu

import "core:fmt"

CPU :: struct {
	registers: CPURegisters,
	memory:    CPUMemory,
	cycles:	   uint,
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

new_cpu :: proc() -> CPU {
	cpu: CPU
	memory := &cpu.memory

	memory.raw = new([CPU_MEMORY_SIZE]byte)

	// CPU RAM map init
	memory.ram = memory.raw[0x0000:0x07FF]

	memory.ram_map.zero_page = memory.raw[0x0000:0x00FF]
	memory.ram_map.stack = memory.raw[0x0100:0x01FF]
	memory.ram_map.ram = memory.raw[0x0200:0x07FF]
	memory.ram_map.mirrors = memory.raw[0x0800:0x1FFF]

	cpu.registers.stack_pointer = 0xFF // 0x01FF - 0x0100

	// CPU IO Registers map init
	memory.io_registers = memory.raw[0x2000:0x401F]
	memory.io_registers_map.first = memory.raw[0x2000:0x2007]
	memory.io_registers_map.mirrors = memory.raw[0x2008:0x3FFF]
	memory.io_registers_map.second = memory.raw[0x4000:0x401F]

	// CPU Expansion ROM map init
	memory.expansion_rom = memory.raw[0x4020:0x5FFF]

	// CPU Expansion SRAM map init
	memory.sram = memory.raw[0x6000:0x7FFF]

	// CPU Expansion PRG ROM map init
	memory.prg_rom.lower = memory.raw[0x8000:0xBFFF]
	memory.prg_rom.upper = memory.raw[0xC000:0x10000]

	return cpu
}

read_byte :: proc(cpu: ^CPU, address: u16) -> byte {
	return cpu.memory.raw[address]
}

read_u16 :: proc(cpu: ^CPU, address: u16) -> u16 {
	return u16(cpu.memory.raw[address]) | u16(cpu.memory.raw[address + 1] << 8)
}

push_byte_on_stack :: proc(cpu: ^CPU, value: byte) -> bool {
	when ODIN_DEBUG 
	{
		if (cpu.registers.stack_pointer - size_of(byte) >= 0) {
			fmt.printf(
				"Stack overflow! %x won't be pushed to the stack. size_of(byte) = %d. stack_pointer = %x",
				value,
				size_of(byte),
				cpu.registers.stack_pointer,
			)
			return false
		}
	} else {
		fmt.assertf(cpu.registers.stack_pointer - size_of(byte) >= 0, "Stack overflow :(")
		return false
	}

	stack := cpu.memory.ram_map.stack
	cpu.registers.stack_pointer -= size_of(byte)
	stack[cpu.registers.stack_pointer] = value

	return true
}

pull_byte_from_stack :: proc(cpu: ^CPU) -> byte {
	when ODIN_DEBUG 
	{
		if (size_of(byte) > cpu.registers.stack_pointer) {
			fmt.printf(
				"Stack underflow! No value will be pulled from the stack, value will be 0. stack_pointer = %x",
				cpu.registers.stack_pointer,
			)
			return 0
		}
	} else {
		fmt.assertf(size_of(byte) > cpu.registers.stack_pointer, "Stack underflow :(")
	}

	value := cpu.memory.ram_map.stack[cpu.registers.stack_pointer]
	cpu.registers.stack_pointer += size_of(byte)
	return value
}

pull_u16_from_stack :: proc(cpu: ^CPU) -> u16 {
	when ODIN_DEBUG 
	{
		if (size_of(u16) > cpu.registers.stack_pointer) {
			fmt.printf(
				"Stack underflow! No value will be pulled from the stack, value will be 0. stack_pointer = %x",
				cpu.registers.stack_pointer,
			)
			return 0
		}
	} else {
		fmt.assertf(size_of(u16) > cpu.registers.stack_pointer, "Stack underflow :(")
		return 0
	}

	return u16(pull_byte_from_stack(cpu)) | u16(pull_byte_from_stack(cpu) << 8)
}

x_zp_indirect :: proc(cpu: ^CPU, data: ^[]byte) -> u16 {
	temp_address := u16(data[0]) + u16(cpu.registers.x)

	lo := cpu.memory.raw[temp_address]
	hi := cpu.memory.raw[temp_address + 1]

	return u16(hi << 8) | u16(lo)
}

zp_indirect_y :: proc(cpu: ^CPU, data: ^[]byte) -> u16 {
	temp_address := data[0]

	lo := cpu.memory.raw[temp_address]
	hi := cpu.memory.raw[temp_address + 1]

	return (u16(hi << 8) | u16(lo)) + u16(cpu.registers.y)
}

zeropage :: #force_inline proc(data: ^[]byte, indexed_value: byte = 0) -> u16 {
	return u16(data[0]) + u16(indexed_value)
}

immediate :: #force_inline proc(data: ^[]byte) -> u16 {
	return u16(data[0])
}

absolute :: #force_inline proc(data: ^[]byte, indexed_value: byte = 0) -> u16 {
	lo := data[0]
	hi := data[1]

	return u16(hi << 8) | u16(lo) + u16(indexed_value)
}

relative :: #force_inline proc(cpu: ^CPU, data: ^[]byte) -> u16 {
	offset := data[0]
	return u16(i16(cpu.registers.program_counter) + i16(offset))
}

reset_interupt :: proc(cpu: ^CPU) {
	cpu.registers.flags = {}
	cpu.registers.x = 0
	cpu.registers.accumulator = 0
	cpu.registers.program_counter = read_u16(cpu, 0xFFFC)
}

load :: proc(cpu: ^CPU, program: []byte) {}

// process :: proc(cpu: ^CPU, data: []byte) {
// 	for b in data {
// 		execute_instruction(cpu, &data)
// 		cpu.registers.program_counter += 1
// 	}
// }
