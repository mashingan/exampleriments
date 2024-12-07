from std/strutils import parseInt, parseEnum, splitWhitespace
from std/sugar import dump

const input {.strdefine.} = "input.txt"

const pixelLength = 40

type
  Register = object
    value: int
    cycle: Natural
    signalCycle: array[0..5, Natural]
    pixels: array[0..5, string]
    signalCount: Natural
    totalSignal: int

  Instruction = enum
    Noop = (1, "noop")
    Addx = (2, "addx")

proc calculate(reg: var Register, instrline: sink string) =
  let instr = instrline.splitWhitespace
  let instruct = instr[0].parseEnum[:Instruction] 

  template checkAndAddSignal(value: int) =
    let b = reg.signalCount <= reg.signalCycle.high and
            reg.cycle >= reg.signalCycle[reg.signalCount]
    if b:
      #dump value * reg.signalCycle[reg.signalCount]
      reg.totalSignal += value * reg.signalCycle[reg.signalCount]
      inc reg.signalCount

  template drawPixel =
    let cycle = (reg.cycle mod pixelLength) - 1
    let value = reg.value
    let chardraw =
      if cycle in [value-1, value, value+1]: '#'
      else: '.'
    reg.pixels[(reg.cycle-1) div pixelLength] &= chardraw

  if instruct == Noop:
    inc reg.cycle
    reg.value.checkAndAddSignal
    drawPixel
  else:
    let prevValue = reg.value
    for _ in 1 .. 2:
      inc reg.cycle
      drawPixel
    reg.value += parseInt(instr[1])
    prevValue.checkAndAddSignal

proc main =
  var reg: Register
  reg.signalCycle = [Natural 20, 60, 100, 140, 180, 220]
  reg.value = 1
  for instr in input.lines:
    reg.calculate instr

  dump reg.totalSignal
  for s in reg.pixels:
    echo s

main()
