/*
 * Dummy file to start a Chisel project.
 *
 * Author: Martin Schoeberl (martin@jopdesign.com)
 *
 */

package empty

import chisel3._
import chisel3.util._

class FSR_once extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(1.W))
    val state_out = Output(UInt(128.W))
  })
  io.state_out := ((io.state(91) ^ (~(io.state(85) & io.state(70))) ^ io.state(
    47
  ) ^ io.state(0) ^ io.key) ## io.state(127, 1))
}
class FSR_640(ROUNDS: Int) extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val state_out = Output(UInt(128.W))
  })
  val state_temp = Wire(Vec(ROUNDS + 1, UInt((128).W)))
  state_temp(0) := io.state
  for (i <- 1 until ROUNDS + 1) {
    val once = Module(new FSR_once())
    once.io.key := io.key((i - 1) % 128)
    once.io.state := state_temp(i - 1)
    state_temp(i) := once.io.state_out
  }
  io.state_out := state_temp(ROUNDS)
}
class tick() extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(1.W))
    val out_tick = Output(UInt(1.W))
  })
  val temp_reg = Reg(UInt(1.W))
  temp_reg := io.in
  io.out_tick := io.in & ~temp_reg
}
// this will perform the xor with framebit
class frame_bit_init() extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(3.W))
    val state = Input(UInt(3.W))
    val state_out = Output(UInt(3.W))
  })
  io.state_out := (io.state(0) ^ io.in(0)) ## (io.state(1) ^ io.in(1)) ## (io
    .state(2) ^ io.in(2))
}

class init_key() extends Module {
  val io = IO(new Bundle {
    val state_out = Output(UInt(128.W))
    val key = Input(UInt(128.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
  })
  val p_start_reg = Reg(UInt(1.W))
  val p1024 = Module(new FSR_N_Reg())
  p1024.io.steps := 8.U
  p1024.io.start := p_start_reg
  p1024.io.key := io.key
  io.state_out := p1024.io.state_out
  p1024.io.state := 0.U
  io.done := 0.U
  when(io.start === 1.U) {
    p_start_reg := 1.U
    io.done := 0.U
  }.elsewhen(p1024.io.done === 1.U) {
    p_start_reg := 0.U
    io.done := 1.U
  }
}

// This performs initialization once (32 bits)
// This means this module must be started 3 times to do all 96 bits of nonce
// the variables going in are: key, state, nonce (32 bits), and start. The output is init_output (state with one 32 bit nonce mixed)
// when sending a start signal, the value inside is a tick, so no immediate need to turn the start signal off until the end

class initialization_tinyJAMBU_once() extends Module {
  val io = IO(new Bundle {
    val key = Input(UInt(128.W))
    val state = Input(UInt(128.W))
    val start = Input(UInt(1.W))
    val nonce = Input(UInt(32.W))
    val single_initialization_out = Output(UInt(128.W))
    val done = Output(UInt(1.W))
  })

  val state = Wire(UInt(128.W))
  state := io.state
  val state_out = Wire(UInt(128.W))

  val start_FSR = Reg(UInt(1.W))
  val temp_nonce_state = Wire(UInt(32.W))

  val output_of_whole_frame = Wire(UInt(128.W))
  start_FSR := 0.U

  // apply framebits
  val single_frame_bit = Module(new frame_bit_init())
  val output_of_3_bit_frame = Wire(UInt(3.W))
  single_frame_bit.io.in := 1.U
  single_frame_bit.io.state := state(38, 36)
  output_of_3_bit_frame := single_frame_bit.io.state_out
  // recombine state with framebits
  output_of_whole_frame := state(127, 39) ## output_of_3_bit_frame ## state(
    35,
    0
  )
  // use tick to start FSR
  val sig_edge = Module(new tick())
  val start_sig = Wire(UInt(1.W))
  sig_edge.io.in := start_FSR
  start_sig := sig_edge.io.out_tick

  val inst_FSR = Module(new FSR_N_Reg())
  inst_FSR.io.start := start_sig
  state_out := inst_FSR.io.state_out
  inst_FSR.io.state := state
  inst_FSR.io.key := io.key
  inst_FSR.io.steps := 6.U

  // initialize other variables

  io.single_initialization_out := 0.U
  temp_nonce_state := 0.U
  io.done := 0.U

  // start the FSR - uses start_FSR to use the tick module
  when(io.start === 1.U) {
    start_FSR := 1.U
    io.done := 0.U
  }.elsewhen(inst_FSR.io.done === 1.U) {
// if the fsr is done do other stuff
// reset FSR for future runs
    start_FSR := 0.U

// perform 32 bit xor with state and nonce
    temp_nonce_state := output_of_whole_frame(127, 96) ^ io.nonce

// recombine state and put it at output
    io.single_initialization_out := temp_nonce_state ## state_out(95, 0)
    io.done := 1.U
  }
}
// plan is to run the init_once three times when given a start signal
// every iteration, change the output of that "once" module to be the input
// once done, assert a done signal
// don't forget to reset the internal start signal to 0. also initialize the count when starting
class initialization_tinyJAMBU() extends Module {
  val io = IO(new Bundle {
    val key = Input(UInt(128.W))
    val nonce = Input(UInt(96.W))
    val state = Input(UInt(128.W))
    val state_out = Output(UInt(128.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
  })
  // start_tick_gen gets in start from this module
  // it outputs to the behavioral section

  val start_init_tick = Wire(UInt(1.W))
  val loop_counter = Reg(UInt(2.W))
  val init_once_input = Reg(UInt(128.W))

  val init_once_output = Wire(UInt(128.W))
  val init_once_start_reg = Reg(UInt(1.W))
  val init_inst_once = Module(new initialization_tinyJAMBU_once())
  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start
  start_init_tick := tick_gen.io.out_tick

  init_inst_once.io.state := init_once_input
  init_inst_once.io.start := init_once_start_reg
  init_once_output := init_inst_once.io.single_initialization_out
  init_inst_once.io.key := io.key

  init_inst_once.io.nonce := 0.U
  io.state_out := 0.U
  io.done := 0.U
//   start
  when(start_init_tick === 1.U) {
    init_once_start_reg := 1.U
    loop_counter := 2.U
    // give init_once the state
    init_once_input := io.state
    // give init_once the nonce; should be 31,0 then 63,32 then 96,64
    init_inst_once.io.nonce := io.nonce(31, 0)
    io.done := 0.U
  }
  when(loop_counter > 0.U) {
    // if init_once is done
    when(init_inst_once.io.done === 1.U) {
      // only when loop is done does the counter decrease and the state register get recycled
      loop_counter := loop_counter - 1.U
      init_once_input := init_once_output
      when(loop_counter === 1.U) {
        init_inst_once.io.nonce := io.nonce(63, 32)
      }.elsewhen(loop_counter === 0.U) {
        init_inst_once.io.nonce := io.nonce(95, 64)
      }
      //   start init_once
      init_once_start_reg := 1.U
    }
    // can do something here if the fsr is not done, but probably don't need to
  }.elsewhen(loop_counter === 0.U) {
    io.state_out := init_once_output
    io.done := 1.U
  }
}
class FSR_N_Reg() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val steps = Input(UInt(10.W))
    val start = Input(UInt(1.W))
    val state_out = Output(UInt(128.W))
    val done = Output(UInt(1.W))
  })
  val temp_state = Reg(UInt(128.W))
  val temp_state_out = Wire(UInt(128.W))
  val temp_count = Reg(UInt(10.W))
  // initialize FSR_128
  val FSR_128 = Module(new FSR_640(128))
  val edge_detect = Module(new tick())
  edge_detect.io.in := io.start

  FSR_128.io.key := io.key
  FSR_128.io.state := temp_state
  temp_state_out := temp_state
  // meybe reginit?

  // initialize temp variables
  // temp_count := io.steps
  io.state_out := temp_state_out
  io.done := RegInit(0.U)
  // temp_state := io.state
  when(edge_detect.io.out_tick === 1.U) {
    io.done := 0.U
    temp_state := io.state
    temp_state_out := temp_state
    temp_count := io.steps
    // update the temp variables
  }.elsewhen(temp_count > 0.U) {
    temp_state := temp_state_out
    temp_state_out := FSR_128.io.state_out
    temp_count := temp_count - 1.U
    // update done when temp_count reaches 0
  }.elsewhen(temp_count === 0.U) {
    io.done := 1.U
  }.otherwise {
    io.done := 0.U
    temp_count := io.steps
    io.state_out := temp_state_out
    temp_state := io.state
  }
}
object FSR_640Main extends App {
  println("Generating the FSR hardware")
  // emitVerilog(new FSR_N_Reg(), Array("--target-dir", "generated"))
  // emitVerilog(new FSR_640(640), Array("--target-dir", "generated/640"))
  // emitVerilog(new FSR_640(1024), Array("--target-dir", "generated/1024"))
  emitVerilog(
    new initialization_tinyJAMBU_once(),
    Array("--target-dir", "whole")
  )
  emitVerilog(
    new initialization_tinyJAMBU(),
    Array("--target-dir", "generated/whole")
  )
  emitVerilog(
    new init_key(),
    Array("--target-dir", "generated/whole")
  )
}
