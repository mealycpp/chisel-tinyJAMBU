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
class FSR_128_N(ROUNDS: Int) extends Module {
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
  io.state_out := (io.state(2) ^ io.in(2)) ## (io.state(1) ^ io.in(1)) ## (io
    .state(0) ^ io.in(0))
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
  val start_tick = Module(new tick())
  val start_init_key = Reg(UInt(1.W))
  start_tick.io.in := io.start
  start_init_key := start_tick.io.out_tick

  io.done := 0.U
  when(start_init_key === 1.U) {
    p_start_reg := 1.U
    io.done := 0.U
  }.elsewhen(p1024.io.done === 1.U) {
    p_start_reg := 0.U
    io.done := 1.U
  }
}

class whole_frame_bit() extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(3.W))
    val state = Input(UInt(128.W))
    val out = Output(UInt(128.W))
  })
  val inst_frame_bit_init = Module(new frame_bit_init())
  inst_frame_bit_init.io.in := io.in
  inst_frame_bit_init.io.state := io.state(38, 36)
  io.out := io.state(127, 39) ## inst_frame_bit_init.io.state_out(2, 0) ## io
    .state(35, 0)
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

  val temp_nonce_state = Wire(UInt(32.W))

  // apply framebits
  val output_of_whole_frame = Wire(UInt(128.W))
  val single_frame_bit = Module(new frame_bit_init())
  val output_of_3_bit_frame = Wire(UInt(3.W))
  single_frame_bit.io.in := 1.U
  single_frame_bit.io.state := state(38, 36)
  output_of_3_bit_frame := single_frame_bit.io.state_out
  // recombine state with framebits
  output_of_whole_frame := state(127, 39) ## output_of_3_bit_frame(
    2,
    0
  ) ## state(
    35,
    0
  )
  // use tick to start FSR
  // this can be combined with init_once_edge to save a clock cycle
  val start_FSR = Reg(UInt(1.W))
  start_FSR := 0.U
  val fsr_sig_edge = Module(new tick())
  val start_sig = Wire(UInt(1.W))
  fsr_sig_edge.io.in := start_FSR
  start_sig := fsr_sig_edge.io.out_tick

  val inst_FSR = Module(new FSR_N_Reg())
  inst_FSR.io.start := start_sig
  state_out := inst_FSR.io.state_out
  inst_FSR.io.state := output_of_whole_frame
  inst_FSR.io.key := io.key
  inst_FSR.io.steps := 5.U

  val init_once_edge = Module(new tick())
  val start_init = Wire(UInt(1.W))
  init_once_edge.io.in := io.start
  start_init := init_once_edge.io.out_tick
  // initialize other variables

  io.single_initialization_out := 0.U
  temp_nonce_state := 0.U
  io.done := 0.U

  // start the FSR - uses start_FSR to use the tick module
  when(start_init === 1.U) {
    start_FSR := 1.U
    io.done := 0.U
  }.elsewhen(inst_FSR.io.done === 1.U) {
// if the fsr is done do other stuff
// reset FSR for future runs
    start_FSR := 0.U

// perform 32 bit xor with state and nonce
    temp_nonce_state := state_out(127, 96) ^ io.nonce(31, 0)

// recombine state and put it at output
    io.single_initialization_out := temp_nonce_state(31, 0) ## state_out(95, 0)
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
    // val state = Input(UInt(128.W))
    val state_out = Output(UInt(128.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
  })
  // start_tick_gen gets in start from this module
  // it outputs to the behavioral section

  val init_key = Module(new init_key())
  val state = Wire(UInt(128.W))
  val start_key = Wire(UInt(1.W))
  start_key := 0.U
  val done_key = Wire(UInt(1.W))
  state := 0.U
  init_key.io.key := io.key
  init_key.io.start := start_key
  done_key := init_key.io.done
  state := init_key.io.state_out

  val start_key_init_tick = Wire(UInt(1.W))
  val tick_key_gen = Module(new tick())
  tick_key_gen.io.in := io.start
  start_key_init_tick := tick_key_gen.io.out_tick

  val start_init_tick = Wire(UInt(1.W))
  val tick_init_gen = Module(new tick())
  tick_init_gen.io.in := done_key
  start_init_tick := tick_init_gen.io.out_tick

  val init_inst_once = Module(new initialization_tinyJAMBU_once())
  val init_once_input = Reg(UInt(128.W))
  val init_once_output = Wire(UInt(128.W))
  val init_once_start_reg = Reg(UInt(1.W))
  init_inst_once.io.state := init_once_input
  init_inst_once.io.start := init_once_start_reg
  init_once_output := init_inst_once.io.single_initialization_out
  init_inst_once.io.key := io.key
  init_inst_once.io.nonce := io.nonce(31, 0)

  val done = Reg(UInt(1.W))
  io.state_out := init_once_output
  io.done := done
  done := 0.U
  val loop_counter = Reg(UInt(2.W))
  // For some reason, this locks done to 0
  // loop_counter := 2.U

//   start
  when(start_init_tick === 1.U) {
    start_key := 0.U
    loop_counter := 2.U
    // give init_once the state
    init_once_input := state
    // give init_once the nonce; should be 31,0 then 63,32 then 96,64
    init_inst_once.io.nonce := io.nonce(31, 0)
    init_once_start_reg := 1.U
    done := 0.U
  }
    .elsewhen(start_key_init_tick === 1.U) {
      // assumes that the variables going into init_key are ready; if not, then they need to be set here
      state := init_key.io.state_out
      init_key.io.key := io.key
      start_key := 1.U
      done := 0.U
    }
    .elsewhen(loop_counter > 0.U) {
      // if init_once is done
      when(init_inst_once.io.done === 1.U) {
        // only when loop is done does the counter decrease and the state register get recycled
        loop_counter := loop_counter - 1.U
        init_once_input := init_once_output
        when(loop_counter === 1.U) {
          init_inst_once.io.nonce := io.nonce(63, 32)
        }
        //   start init_once
        init_once_start_reg := 1.U
      }.otherwise {
        init_once_start_reg := 0.U
        done := 0.U
      }
    }
    .elsewhen(loop_counter === 0.U) {
      init_inst_once.io.nonce := io.nonce(95, 64)
      // actually need to wait one more loop until output is correct
      when(init_inst_once.io.done === 1.U) {
        io.state_out := init_once_output
        init_once_start_reg := 0.U
        done := 1.U
      }
    }
    .otherwise {
      done := 0.U
    }
}

// adlen_left should be between 1 and 3; 0 and 4 means there shouldn't be a partial block
// technically can use 2 bits, but using 3 is more readable
// also tecnically the partial block can be as small as a single bit, but to match the c implmenetation, it uses a byte
class process_partial_once() extends Module {
  val io = IO(new Bundle {
    val ad = Input(UInt(32.W))
    val adlen_left = Input(UInt(3.W))
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val start = Input(UInt(1.W))
    val state_out = Output(UInt(128.W))
    val done = Output(UInt(1.W))
  })
  val frame = Module(new whole_frame_bit())
  val frame_out = Wire(UInt(128.W))
  frame.io.in := 3.U
  frame.io.state := io.state
  frame_out := frame.io.out

  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start

  val inst_fsr = Module(new FSR_N_Reg())
  val fsr_out = Wire(UInt(129.W))
  inst_fsr.io.state := frame_out
  fsr_out := inst_fsr.io.state_out
  inst_fsr.io.key := io.key
  inst_fsr.io.steps := 5.U
  inst_fsr.io.start := tick_gen.io.out_tick

  val number_xor = Wire(UInt(128.W))

  when(inst_fsr.io.done === 1.U) {
    // 0 left should never happen
    when(io.adlen_left === 1.U) {
      // take first eight bits
      number_xor := fsr_out(127, 96 + 8) ## (
        fsr_out((96 + 8) - 1, 96) ^ io.ad(7, 0)
      ) ## fsr_out(95, 0)
      io.state_out := number_xor(127, 34) ## (number_xor(
        33,
        32
      ) ^ 1.U) ## number_xor(31, 0)
    }
      .elsewhen(io.adlen_left === 2.U) {
        // take up to second eight bits
        number_xor := fsr_out(127, 96 + 16) ## (
          fsr_out((96 + 16) - 1, 96) ^ io.ad(15, 0)
        ) ## fsr_out(95, 0)
        io.state_out := number_xor(127, 34) ## (number_xor(
          33,
          32
        ) ^ 2.U) ## number_xor(31, 0)
      }
      .elsewhen(io.adlen_left === 3.U) {
        // take up to third eight bits (max)
        number_xor := fsr_out(127, 96 + 24) ## (
          fsr_out((96 + 24) - 1, 96) ^ io.ad(23, 0)
        ) ## fsr_out(95, 0)
        io.state_out := number_xor(127, 34) ## (number_xor(
          33,
          32
        ) ^ 3.U) ## number_xor(31, 0)

      }
      .otherwise {
        number_xor := 0.U
        io.state_out := 0.U
        io.done := 0.U
      }
    io.done := 1.U
  }.otherwise {
    number_xor := 0.U
    io.state_out := 0.U
    io.done := 0.U
  }
}
// processes 32 bits
class process_AD_once() extends Module {
  val io = IO(new Bundle {
    val ad = Input(UInt(32.W))
    val key = Input(UInt(128.W))
    val state = Input(UInt(128.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
    val state_out = Output(UInt(128.W))
  })
  val frame_bit_inst = Module(new whole_frame_bit())
  val frame_state_out = Wire(UInt(128.W))
  frame_bit_inst.io.in := 3.U
  frame_bit_inst.io.state := io.state
  frame_state_out := frame_bit_inst.io.out

  val inst_FSR = Module(new FSR_N_Reg())
  val fsr_done = Wire(UInt(1.W))
  val fsr_start = Reg(UInt(1.W))
  fsr_start := 0.U
  val fsr_state_out = Wire(UInt(128.W))
  fsr_done := inst_FSR.io.done
  inst_FSR.io.key := io.key
  inst_FSR.io.start := fsr_start
  inst_FSR.io.state := frame_state_out
  inst_FSR.io.steps := 5.U
  fsr_state_out := inst_FSR.io.state_out

  val tick_gen = Module(new tick())
  val start_tick = Wire(UInt(1.W))
  tick_gen.io.in := io.start
  start_tick := tick_gen.io.out_tick

  // start whole module
  // some wires may not be needed, kept for readability
  when(start_tick === 1.U) {
    fsr_start := 1.U
    io.done := 0.U
    io.state_out := 0.U
  }.elsewhen(fsr_done === 1.U) {
    fsr_start := 0.U
    // 32 bit directions might not be needed, but used here just in case
    io.state_out := (fsr_state_out(127, 96) ^ io
      .ad(31, 0)) ## fsr_state_out(95, 0)
    io.done := 1.U
  }.otherwise {
    fsr_start := 0.U
    io.state_out := 0.U
    io.done := 0.U
  }
}
// assume there's a fifo inputting to ad
// also someone should provide some info about whether there's a partial block

// adlen is how many 8 bit chunks of data
// let this module control the fifo for ad. The fifo will read out data when requested and it should happen adlen / 4 times. For the last read, if it's a partial block (adlen % 4 == 1) then use the partial block operation
// software will load data into the fifo
// since there are fifos, they can be done in parallel for message and ad with a mux to choose which
// make the ad automatic; automatically take data into process_ad
// each chunk is a ratio, total amount is capacity - make it 16; keep in mind that the whole system is at same clock speed, so loading will be one at a time

// This has the process once modules
// it will connect the appropriate data to them
// it will contain a fifo for data
// this will work until ad_len is 0

// will the fifo be inside or outside this modulead_fifo
// who gives out the ready signal
// how to resolve having adlen and a fifo - they provide the same info
// how to resolve the queue's ready with the module's ready
// how to handle allowing pushing into queue

// main problem is when one cycle of process_ad happens, how to get or wait for more data for the next cycle

// keep getting back to the problem of how once is different than this module
// why not just give the start signal 3 times for the once modules?

// right now, the architecture is like this: 2 modules for performing one iteration
// ad coming in the form of a fifo
// when the queue is empty, put the result out

class process_AD() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val ad = Decoupled(UInt(32.W))
    val adlen = Input(UInt(32.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
    val state_out = Output(UInt(128.W))
  })

  val adlen_left = Reg(UInt(32.W))
  val state_reg = Reg(UInt(128.W))
  val ad_fifo = Queue(io.ad, 16)

  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start

  val inst_once = Module(new process_AD_once())

  val inst_partial = Module(new process_partial_once())

  // val first_fsr :: second_fsr :: Nil = Enum(2)
  // val state_reg = RegInit(first_fsr)
  inst_partial.io.adlen_left := adlen_left

  when (tick_gen.io.out_tick === 1.U) {
    adlen_left := io.adlen
    state_reg := io.state
    // pass the required data to both partial and whole block
    inst_once.io.key := io.key
    inst_once.io.state := state_reg
    inst_once.io.ad := io.ad

    inst_partial.io.key := io.key
    inst_partial.io.state := state_reg
    inst_partial.io.ad := io.ad

    // start needs to be controlled, so drive it in the when statements
    when (adlen_left >= 4.U) {
      inst_once.io.start := 1.U
    }
    .elsewhen(adlen_left < 4.U) {
      inst_partial.io.start := 1.U
    }
  }
  // // This line below doesn't work because we need to know who was running
  // when (inst_once.io.done === 1.U || inst_partial.io.done === 1.U) {
  //   adlen_left := adlen_left - 4.U
    
  //   when (adlen_left >= 4) {
  //     inst_once.io.start := 1.U
  //   }
  //   .elsewhen(adlen_left < 4) {
  //     inst_partial.io.start := 1.U
  //   }

  when (inst_once.io.done === 1.U) {
    adlen_left := adlen_left - 4.U
  }
  .elsewhen (inst_partial.io.done === 1.U) {
    adlen_left := 0.U

  }
  .otherwise {
    adlen_left := 0.U
    inst_once.io.start := 0.U
    inst_partial.io.start := 0.U
  }
}

class process_AD_streaming() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val ad = Decoupled(UInt(32.W))
    val adlen = Input(UInt(10.W))
    val fifo_ad_in = Input(UInt(1.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
    val state_out = Output(UInt(128.W))
  })
// https://stackoverflow.com/questions/41122480/chisel-language-how-to-best-use-queues
  // change depth from 2 to 16
  val ad_fifo = Queue(io.ad, 16)

  val start_process_AD = Module(new tick())
  start_process_AD.io.in := io.start
  // start_process_AD.io.out_tick
  // repeat process_AD_once() while there is data
  val ad_count = Reg(UInt(10.W))
  ad_count := io.adlen

  val process_once = Module(new process_AD_once())
  val process_state_in = Reg(UInt(128.W))
  process_state_in := io.state
  process_once.io.ad := ad_fifo.deq()
  process_once.io.key := io.key
  process_once.io.start := start_process_AD.io.out_tick
  process_once.io.state := process_state_in
  process_once.io.start := 0.U

  io.state_out := process_once.io.state_out
  when(ad_fifo.ready === true.B) {
    process_once.io.ad := ad_fifo.deq()
    process_once.io.start := 1.U
  } .elsewhen(process_once.io.done === 1.U) {
    process_state_in := process_once.io.state_out
    when (ad_fifo.ready === true.B) {
      process_once.io.ad := ad_fifo.deq()
      process_once.io.start := 1.U
    }
  }
  .otherwise {}
}

class encrypt_once() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val state_out = Output(UInt(128.W))
    val message = Input(UInt(32.W))
    val ciphertext_out = Output(UInt(32.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
  })
  val inst_frame = Module(new frame_bit_init())
  val frame_out = Wire(UInt(128.W))
  inst_frame.io.in := 5.U
  inst_frame.io.state := io.state
  frame_out := inst_frame.io.state_out
  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start

  val inst_fsr = Module(new FSR_N_Reg())
  inst_fsr.io.key := io.key
  inst_fsr.io.state := frame_out
  inst_fsr.io.steps := 8.U
  inst_fsr.io.start := tick_gen.io.out_tick
  val temp_partial_state = Wire(UInt(32.W))

  when(inst_fsr.io.done === 1.U) {
    // The state_out value should be "stored" in a wire after xoring with the message, but since they don't overlap, they don't need to come from a single wire
    temp_partial_state := inst_fsr.io.state_out(127, 96) ^ io.message(31, 0)
    io.ciphertext_out := inst_fsr.io.state_out(95, 64) ^ io.message(31, 0)
    io.state_out := temp_partial_state(127, 96) ## inst_fsr.io.state_out(
      95,
      0
    )
    io.done := 1.U
  }
    .otherwise {
      inst_fsr.io.start := 0.U
      temp_partial_state := 0.U
    }
}

class partial_encrypt_once() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val state_out = Output(UInt(128.W))
    val message = Input(UInt(32.W))
    val ciphertext_out = Output(UInt(32.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
    val mlen_left = Input(UInt(3.W))
  })
  // lines below should be converted to a function or something since they are reused in basically all modules
  val inst_frame = Module(new frame_bit_init())
  val frame_out = Wire(UInt(128.W))
  inst_frame.io.in := 5.U
  inst_frame.io.state := io.state
  frame_out := inst_frame.io.state_out
  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start
  val inst_fsr = Module(new FSR_N_Reg())
  inst_fsr.io.key := io.key
  inst_fsr.io.state := frame_out
  inst_fsr.io.steps := 8.U
  inst_fsr.io.start := tick_gen.io.out_tick
  val temp_partial_state = Wire(UInt(32.W))

  when(inst_fsr.io.done === 1.U) {
    // should simplify to one statement
    // if one statement, replace the '1' with the corresponding variable (mlen_left)
    // again, min size is 8 bits (one byte)
    when(io.mlen_left === 1.U) {
      // this nasty line performs xor with message (32 bits) and mlen_left then recombines it with the rest of the state
      // some kind of partial assignment into state would be better, but that is not quite allowed
      io.state_out := (inst_fsr.io.state_out(127, 96 + (1 * 8)) ## (inst_fsr.io
        .state_out((96 - 1) + (1 * 8), 96) ^ io.message(
        (1 * 8) - 1,
        0
      )) ## inst_fsr.io.state_out(95, 34) ## (inst_fsr.io.state_out(
        33,
        32
      ) ^ 1.U) ## inst_fsr.io.state_out(31, 0))
      io.ciphertext_out := inst_fsr.io.state_out((64 - 1) + (1 * 8), 64) ^ io
        .message(1 * 8, 0)
    }.elsewhen(io.mlen_left === 2.U) {
      io.state_out := (inst_fsr.io.state_out(127, 96 + (2 * 8)) ## (inst_fsr.io
        .state_out((96 - 2) + (2 * 8), 96) ^ io.message(
        (2 * 8) - 1,
        0
      )) ## inst_fsr.io.state_out(95, 34) ## (inst_fsr.io.state_out(
        33,
        32
      ) ^ 2.U) ## inst_fsr.io.state_out(31, 0))
      io.ciphertext_out := inst_fsr.io.state_out((64 - 2) + (2 * 8), 64) ^ io
        .message(2 * 8, 0)
    }.elsewhen(io.mlen_left === 3.U) {
      io.state_out := (inst_fsr.io.state_out(127, 96 + (3 * 8)) ## (inst_fsr.io
        .state_out((96 - 3) + (3 * 8), 96) ^ io.message(
        (3 * 8) - 1,
        0
      )) ## inst_fsr.io.state_out(95, 34) ## (inst_fsr.io.state_out(
        33,
        32
      ) ^ 3.U) ## inst_fsr.io.state_out(31, 0))
      io.ciphertext_out := inst_fsr.io.state_out((64 - 3) + (3 * 8), 64) ^ io
        .message(3 * 8, 0)
    }.otherwise {}
    io.done := 1.U
  }
    .otherwise {
      inst_fsr.io.start := 0.U
      temp_partial_state := 0.U
      io.done := 0.U
    }
}

class finalization() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val tag = Output(UInt(64.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
  })
  val inst_frame = Module(new whole_frame_bit())
  val temp_state = Reg(UInt(128.W))
  temp_state := io.state
  inst_frame.io.state := temp_state
  inst_frame.io.in := 7.U
  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start
  val inst_fsr = Module(new FSR_N_Reg())
  inst_fsr.io.start := tick_gen.io.out_tick
  inst_fsr.io.state := inst_frame.io.out
  inst_fsr.io.key := io.key
  inst_fsr.io.steps := 8.U

  val first_fsr :: second_fsr :: Nil = Enum(2)
  val state_reg = RegInit(first_fsr)

  val temp_tag1 = Wire(UInt(32.W))
  val temp_tag2 = Wire(UInt(32.W))

  when(state_reg === first_fsr) {
    inst_frame.io.state := temp_state
    // start fsr, then wait for done
    when(tick_gen.io.out_tick === 1.U) {
      inst_fsr.io.start := 1.U
      io.done := 0.U
      state_reg := first_fsr
    }
      .elsewhen(inst_fsr.io.done === 1.U) {
        temp_tag1(31, 0) := inst_fsr.io.state_out(95, 64)
        // this line below automatically goes into inst_frame
        temp_state := inst_fsr.io.state_out
        inst_fsr.io.steps := 5.U
        inst_fsr.io.start := 1.U
        state_reg := second_fsr
      }
  }
    .elsewhen(state_reg === second_fsr) {
      when(inst_fsr.io.done === 1.U) {
        temp_tag2(63, 32) := inst_fsr.io.state_out(95, 64)
        io.tag := temp_tag1(63, 32) ## temp_tag1(31, 0)
        inst_fsr.io.start := 0.U
        io.done := 1.U
      }
    }
    .otherwise {}
}

// verification uses the same steps as finalization with an extra step to compare tags
class verification extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    // val tag
    // val start
    // val done
    val verified = Output(UInt(1.W))
  })
}
class decrypt_once() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val state_out = Output(UInt(128.W))
    val ciphertext = Input(UInt(32.W))
    val message_out = Output(UInt(32.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
  })
  val inst_frame = Module(new frame_bit_init())
  val frame_out = Wire(UInt(128.W))
  inst_frame.io.in := 5.U
  inst_frame.io.state := io.state
  frame_out := inst_frame.io.state_out
  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start

  val inst_fsr = Module(new FSR_N_Reg())
  inst_fsr.io.key := io.key
  inst_fsr.io.state := frame_out
  inst_fsr.io.steps := 8.U
  inst_fsr.io.start := tick_gen.io.out_tick

  when(inst_fsr.io.done === 1.U) {
    // The state_out value should be "stored" in a wire after xoring with the message, but since they don't overlap, they don't need to come from a single wire

    io.message_out := inst_fsr.io.state_out(95, 64) ^ io.ciphertext(31, 0)
    // not sure if xor with output (message_out) is allowed
    // if chisel treats the output as a connection that's ready to use, this should work
    // minor simplification here by putting the xor ciphertext as a scala variable; not sure if this works
    // if this doesn't work, just substitute the variable with what it equals to
    val xor_temp = inst_fsr.io.state_out(95, 64) ^ io.ciphertext(31, 0)
    io.state_out := (xor_temp) ## inst_fsr.io.state_out(
      95,
      0
    )
    io.done := 1.U
  }
    .otherwise {
      inst_fsr.io.start := 0.U
      io.done := 0.U
    }

}

class partial_decrypt_once() extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(128.W))
    val key = Input(UInt(128.W))
    val state_out = Output(UInt(128.W))
    val ciphertext = Input(UInt(32.W))
    val c_len_left = Input(UInt(3.W))
    val message_out = Output(UInt(32.W))
    val start = Input(UInt(1.W))
    val done = Output(UInt(1.W))
  })
  val inst_frame = Module(new frame_bit_init())
  val frame_out = Wire(UInt(128.W))
  inst_frame.io.in := 5.U
  inst_frame.io.state := io.state
  frame_out := inst_frame.io.state_out
  val tick_gen = Module(new tick())
  tick_gen.io.in := io.start
  val inst_fsr = Module(new FSR_N_Reg())
  inst_fsr.io.key := io.key
  inst_fsr.io.state := frame_out
  inst_fsr.io.steps := 8.U
  inst_fsr.io.start := tick_gen.io.out_tick
  val temp_partial_state = Wire(UInt(32.W))

  // ideally this isn't needed, but it's used due to partial reading for xor with 1.U
  val number_xor = Wire(UInt(128.W))

  when(inst_fsr.io.done === 1.U) {
    when(io.c_len_left === 1.U) {
      // take first eight bits

      io.message_out := inst_fsr.io.state_out(64 + (8 * 1), 64) ^ io.ciphertext(
        31,
        0
      )
      number_xor := (inst_fsr.io.state_out(127, 96 + (8 * 1)) ## (inst_fsr.io
        .state_out(96 + (8 * 1) - 1, 96) ^ io.message_out(31, 0)) ## inst_fsr.io
        .state_out(96 - 1, 0))
      io.state_out := number_xor(127, 34) ## number_xor(
        33,
        32
      ) ^ 1.U ## number_xor(31, 0)
      // number_xor := inst_fsr.io.state_out(127, 96 + (8 * 1)) ## (
      //   inst_fsr.io.state_out((96 + (8 * 1)) - 1, 96) ^ io.ciphertext((8 - 1), 0)
      // ) ## inst_fsr.io.state_out(95, 0)
      // io.state_out := number_xor(127, 34) ## (number_xor(
      //   33,
      //   32
      // ) ^ 1.U) ## number_xor(31, 0)
    }
      .elsewhen(io.c_len_left === 2.U) {
        // take up to second eight bits

        io.message_out := inst_fsr.io.state_out(64 + (8 * 2), 64) ^ io
          .ciphertext(31, 0)
        number_xor := (inst_fsr.io
          .state_out(127, 96 + (8 * 2)) ## (inst_fsr.io.state_out(
          96 + (8 * 2) - 1,
          96
        ) ^ io.message_out(31, 0)) ## inst_fsr.io.state_out(96 - 1, 0))
        io.state_out := number_xor(127, 34) ## number_xor(
          33,
          32
        ) ^ 1.U ## number_xor(31, 0)
      }
      .elsewhen(io.c_len_left === 3.U) {
        // take up to third eight bits (max)

        io.message_out := inst_fsr.io.state_out(64 + (8 * 3), 64) ^ io
          .ciphertext(31, 0)
        number_xor := (inst_fsr.io
          .state_out(127, 96 + (8 * 3)) ## (inst_fsr.io.state_out(
          96 + (8 * 3) - 1,
          96
        ) ^ io.message_out(31, 0)) ## inst_fsr.io.state_out(96 - 1, 0))
        io.state_out := number_xor(127, 34) ## number_xor(
          33,
          32
        ) ^ 1.U ## number_xor(31, 0)
      }
      .otherwise {
        number_xor := 0.U
        io.state_out := 0.U
        io.done := 0.U
      }
    io.done := 1.U
  }
    .otherwise {
      inst_fsr.io.start := 0.U
      temp_partial_state := 0.U
      number_xor := 0.U
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
  val FSR_128 = Module(new FSR_128_N(128))
  val edge_detect = Module(new tick())
  edge_detect.io.in := io.start

  FSR_128.io.key := io.key
  FSR_128.io.state := temp_state
  temp_state_out := temp_state
  // meybe reginit?

  // initialize io variables
  io.state_out := temp_state_out
  io.done := RegInit(0.U)
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
  }
}
object FSR_640Main extends App {
  println("Generating the FSR hardware")
  emitVerilog(new FSR_N_Reg(), Array("--target-dir", "generated"))
  // emitVerilog(new FSR_640(640), Array("--target-dir", "generated/640"))
  // emitVerilog(new FSR_640(1024), Array("--target-dir", "generated/1024"))
  emitVerilog(
    new initialization_tinyJAMBU_once(),
    Array("--target-dir", "generated/whole")
  )
  emitVerilog(
    new initialization_tinyJAMBU(),
    Array("--target-dir", "generated/whole")
  )
  emitVerilog(
    new init_key(),
    Array("--target-dir", "generated/whole")
  )
  emitVerilog(
    new process_AD_once(),
    Array("--target-dir", "generated/whole")
  )
  emitVerilog(
    new process_partial_once(),
    Array("--target-dir", "generated/whole")
  )
  // emitVerilog(
  //   new process_AD(),
  //   Array("--target-dir", "generated/whole")
  // )
}
