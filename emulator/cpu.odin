package emulator 

CPURegisters :: struct {
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    flags: u8
}

CPU_MEMORY_SIZE :: 0x10000
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
    }
}

CPU :: struct {
    registers: CPURegisters,
    memory: CPUMemory
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

consume_instruction :: proc(cpu: ^CPU, instruction: u8[]) {
    // TODO: ...   
}