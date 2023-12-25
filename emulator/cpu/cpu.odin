// Emulates a 6502 NES cpu
package cpu 

import "core:fmt"

CPU :: struct {
    registers: CPURegisters,
    memory: CPUMemory,
}

@(private)
CPURegisters :: struct {
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,

    flags: CPUFlagRegistry,
}

@(private)
CPUFlagRegistry :: bit_set[enum{
    Carry,
    Zero, 
    Interupt, // IRQ disable
    Decimal, // (BCD for arithmetics)
    Break,
    Bit5,
    Overflow,
    Negative,
}; u8]


@(private)
CPU_MEMORY_SIZE :: 0x10000

@(private)
CPUMemory :: struct {
    raw: ^[CPU_MEMORY_SIZE]u8,

    //  memory slices

    ram: []u8,
    ram_map :  struct {
        zero_page: []u8,
        stack: []u8,
        ram: []u8,    
        mirrors: []u8,
    },

    io_registers: []u8,
    io_registers_map: struct {
        first: []u8,
        mirrors: []u8,
        second: []u8,
    },

    expansion_rom: []u8,
    sram: []u8,

    prg_rom : struct {
        lower: []u8,
        upper: []u8,
    },
}

new_cpu :: proc() -> CPU {
    cpu: CPU
    memory := &cpu.memory

    memory.raw = new([CPU_MEMORY_SIZE]u8)

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

read_u8 :: proc(cpu: ^CPU, address: u16) -> u8 {
    return cpu.memory.raw[address]
}

read_u16 :: proc(cpu: ^CPU, address: u16) -> u16 {
    return u16(cpu.memory.raw[address] << 8) | u16(cpu.memory.raw[address + 1])
}

push_u8_on_stack :: proc(cpu: ^CPU, value: u8) -> bool {
    when ODIN_DEBUG {
        if (cpu.registers.stack_pointer - size_of(u8) >= 0) {
            fmt.printf("Stack overflow! %x won't be pushed to the stack. size_of(u8) = %d. stack_pointer = %x", value, size_of(u8), cpu.registers.stack_pointer)
            return false
        }
    } else {
        fmt.assertf(cpu.registers.stack_pointer - size_of(u8) >= 0, "Stack overflow :(")
        return false
    }

    stack := cpu.memory.ram_map.stack
    cpu.registers.stack_pointer -= size_of(u8)
    stack[cpu.registers.stack_pointer] = value

    return true
}

pull_u8_from_stack :: proc(cpu: ^CPU) -> u8 {
    when ODIN_DEBUG {
        if (size_of(u8) > cpu.registers.stack_pointer) {
            fmt.printf("Stack underflow! No value will be pulled from the stack, value will be 0. stack_pointer = %x", cpu.registers.stack_pointer)
            return 0
        }
    } else {
        fmt.assertf(size_of(u8) > cpu.registers.stack_pointer, "Stack underflow :(")
    }

    value := cpu.memory.ram_map.stack[cpu.registers.stack_pointer]
    cpu.registers.stack_pointer += size_of(u8)
    return value
}

pull_u16_from_stack :: proc(cpu: ^CPU) -> u16 {
    when ODIN_DEBUG {
        if (size_of(u16) > cpu.registers.stack_pointer) {
            fmt.printf("Stack underflow! No value will be pulled from the stack, value will be 0. stack_pointer = %x", cpu.registers.stack_pointer)
            return 0
        }
    } else {
        fmt.assertf(size_of(u16) > cpu.registers.stack_pointer, "Stack underflow :(")
        return 0
    }

    return u16(pull_u8_from_stack(cpu)) | u16(pull_u8_from_stack(cpu) << 8)
}

x_zp_indirect :: proc(cpu: ^CPU, data: []u8) -> u16{
    temp_address := u16(data[0]) + u16(cpu.registers.x)

    lo := cpu.memory.raw[temp_address]
    hi := cpu.memory.raw[temp_address + 1]

    return u16(hi << 8) | u16(lo)
}

zp_indirect_y :: proc(cpu: ^CPU, data: []u8) -> u16{
    temp_address := data[0]

    lo := cpu.memory.raw[temp_address]
    hi := cpu.memory.raw[temp_address + 1]

    return (u16(hi << 8) | u16(lo)) + u16(cpu.registers.y)
}

zeropage :: #force_inline proc(data: []u8, indexed_value:u8 = 0) -> u16 {
    return u16(data[0]) + u16(indexed_value)
}

immediate :: #force_inline proc(data: []u8) -> u16 {
    return u16(data[0])
}

absolute :: #force_inline proc(data: []u8, indexed_value:u8 = 0) -> u16 {
    lo := data[0]
    hi := data[1]

    return u16(hi << 8) | u16(lo) + u16(indexed_value)
}

relative :: #force_inline proc(cpu: ^CPU, data: []u8) -> u16 {
    offset := data[0]
    return u16(i16(cpu.registers.program_counter) + i16(offset))
}

InstructionHandle :: #type proc(^CPU, []u8)
instruction_handles := [?]InstructionHandle{
    0x00 = proc(cpu: ^CPU,     _: []u8) {brk_instruction(cpu)},
    0x01 = proc(cpu: ^CPU, opers: []u8) {ora_instruction(cpu, &cpu.memory.raw[x_zp_indirect(cpu, opers)])},
    0x05 = proc(cpu: ^CPU, opers: []u8) {ora_instruction(cpu, &cpu.memory.raw[zeropage(opers)])},
    0x06 = proc(cpu: ^CPU, opers: []u8) {asl_instruction(cpu, &cpu.memory.raw[zeropage(opers)])},
    0x08 = proc(cpu: ^CPU,     _: []u8) {php_instruction(cpu)},
    0x09 = proc(cpu: ^CPU, opers: []u8) {ora_instruction(cpu, &opers[1])},
    0x0A = proc(cpu: ^CPU,     _: []u8) {asl_instruction(cpu, &cpu.registers.accumulator)},
    0x0D = proc(cpu: ^CPU,     _: []u8) {ora_instruction(cpu, &cpu.registers.accumulator)},
    0x0E = proc(cpu: ^CPU, opers: []u8) {asl_instruction(cpu, &cpu.memory.raw[absolute(opers)])},
    0x10 = proc(cpu: ^CPU, opers: []u8) {bpl_instruction(cpu, &cpu.memory.raw[relative(cpu, opers)])},
    0x11 = proc(cpu: ^CPU, opers: []u8) {ora_instruction(cpu, &cpu.memory.raw[zp_indirect_y(cpu, opers)])},
    0x15 = proc(cpu: ^CPU, opers: []u8) {ora_instruction(cpu, &cpu.memory.raw[zeropage(opers, cpu.registers.x)])},
    0x16 = proc(cpu: ^CPU, opers: []u8) {asl_instruction(cpu, &cpu.memory.raw[zeropage(opers, cpu.registers.x)])},
    0x18 = proc(cpu: ^CPU,     _: []u8) {clc_instruction(cpu)},
    0x19 = proc(cpu: ^CPU, opers: []u8) {ora_instruction(cpu, &cpu.memory.raw[absolute(opers, cpu.registers.y)])},
    0x1D = proc(cpu: ^CPU, opers: []u8) {ora_instruction(cpu, &cpu.memory.raw[absolute(opers, cpu.registers.x)])},
    0x1E = proc(cpu: ^CPU, opers: []u8) {asl_instruction(cpu, &cpu.memory.raw[absolute(opers, cpu.registers.x)])},
    0x20 = proc(cpu: ^CPU, opers: []u8) {jsr_instruction(cpu, absolute(opers))},
    0x21 = proc(cpu: ^CPU, opers: []u8) {and_instruction(cpu, &cpu.memory.raw[x_zp_indirect(cpu, opers)])},
    0x24 = proc(cpu: ^CPU, opers: []u8) {bit_instruction(cpu, &cpu.memory.raw[zeropage(opers)])},
    0x25 = proc(cpu: ^CPU, opers: []u8) {and_instruction(cpu, &cpu.memory.raw[zeropage(opers)])},
    0x26 = proc(cpu: ^CPU, opers: []u8) {rol_instruction(cpu, &cpu.memory.raw[zeropage(opers)])},
    0x28 = proc(cpu: ^CPU, opers: []u8) {plp_instruction(cpu)},
}

execute :: proc(cpu: ^CPU, data: []u8) {
    op_code := data[0]
    handle := instruction_handles[op_code]

    if (handle != nil) {
        operands := data[1:]
        handle(cpu, operands)
    } else {
        fmt.eprintf("Unknown OP-code encountered: [%02d]", op_code)
    }
}