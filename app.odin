package main

import "emulator"

// This main has no purpose other than to make `odin test` work (by importing the package)

main :: proc() {
	_ = emulator.new_console()
}
