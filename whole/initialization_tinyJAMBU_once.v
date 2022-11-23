module frame_bit_init(
  input  [2:0] io_state,
  output [2:0] io_state_out
);
  wire [1:0] _io_state_out_T_6 = {io_state[2],io_state[1]}; // @[FSR_640.scala 55:44]
  wire  _io_state_out_T_9 = io_state[0] ^ 1'h1; // @[FSR_640.scala 56:15]
  assign io_state_out = {_io_state_out_T_6,_io_state_out_T_9}; // @[FSR_640.scala 55:72]
endmodule
module tick(
  input   clock,
  input   io_in,
  output  io_out_tick
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg  temp_reg; // @[FSR_640.scala 44:21]
  assign io_out_tick = io_in & ~temp_reg; // @[FSR_640.scala 46:24]
  always @(posedge clock) begin
    temp_reg <= io_in; // @[FSR_640.scala 45:12]
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  temp_reg = _RAND_0[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module FSR_once(
  input  [127:0] io_state,
  input          io_key,
  output [127:0] io_state_out
);
  wire  _io_state_out_T_7 = io_state[91] ^ ~(io_state[85] & io_state[70]) ^ io_state[47]; // @[FSR_640.scala 19:69]
  wire  _io_state_out_T_10 = _io_state_out_T_7 ^ io_state[0] ^ io_key; // @[FSR_640.scala 21:19]
  assign io_state_out = {_io_state_out_T_10,io_state[127:1]}; // @[FSR_640.scala 21:29]
endmodule
module FSR_640(
  input  [127:0] io_state,
  input  [127:0] io_key,
  output [127:0] io_state_out
);
  wire [127:0] once_io_state; // @[FSR_640.scala 32:22]
  wire  once_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_1_io_state; // @[FSR_640.scala 32:22]
  wire  once_1_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_1_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_2_io_state; // @[FSR_640.scala 32:22]
  wire  once_2_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_2_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_3_io_state; // @[FSR_640.scala 32:22]
  wire  once_3_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_3_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_4_io_state; // @[FSR_640.scala 32:22]
  wire  once_4_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_4_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_5_io_state; // @[FSR_640.scala 32:22]
  wire  once_5_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_5_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_6_io_state; // @[FSR_640.scala 32:22]
  wire  once_6_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_6_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_7_io_state; // @[FSR_640.scala 32:22]
  wire  once_7_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_7_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_8_io_state; // @[FSR_640.scala 32:22]
  wire  once_8_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_8_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_9_io_state; // @[FSR_640.scala 32:22]
  wire  once_9_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_9_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_10_io_state; // @[FSR_640.scala 32:22]
  wire  once_10_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_10_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_11_io_state; // @[FSR_640.scala 32:22]
  wire  once_11_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_11_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_12_io_state; // @[FSR_640.scala 32:22]
  wire  once_12_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_12_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_13_io_state; // @[FSR_640.scala 32:22]
  wire  once_13_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_13_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_14_io_state; // @[FSR_640.scala 32:22]
  wire  once_14_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_14_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_15_io_state; // @[FSR_640.scala 32:22]
  wire  once_15_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_15_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_16_io_state; // @[FSR_640.scala 32:22]
  wire  once_16_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_16_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_17_io_state; // @[FSR_640.scala 32:22]
  wire  once_17_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_17_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_18_io_state; // @[FSR_640.scala 32:22]
  wire  once_18_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_18_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_19_io_state; // @[FSR_640.scala 32:22]
  wire  once_19_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_19_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_20_io_state; // @[FSR_640.scala 32:22]
  wire  once_20_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_20_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_21_io_state; // @[FSR_640.scala 32:22]
  wire  once_21_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_21_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_22_io_state; // @[FSR_640.scala 32:22]
  wire  once_22_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_22_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_23_io_state; // @[FSR_640.scala 32:22]
  wire  once_23_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_23_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_24_io_state; // @[FSR_640.scala 32:22]
  wire  once_24_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_24_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_25_io_state; // @[FSR_640.scala 32:22]
  wire  once_25_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_25_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_26_io_state; // @[FSR_640.scala 32:22]
  wire  once_26_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_26_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_27_io_state; // @[FSR_640.scala 32:22]
  wire  once_27_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_27_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_28_io_state; // @[FSR_640.scala 32:22]
  wire  once_28_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_28_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_29_io_state; // @[FSR_640.scala 32:22]
  wire  once_29_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_29_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_30_io_state; // @[FSR_640.scala 32:22]
  wire  once_30_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_30_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_31_io_state; // @[FSR_640.scala 32:22]
  wire  once_31_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_31_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_32_io_state; // @[FSR_640.scala 32:22]
  wire  once_32_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_32_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_33_io_state; // @[FSR_640.scala 32:22]
  wire  once_33_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_33_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_34_io_state; // @[FSR_640.scala 32:22]
  wire  once_34_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_34_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_35_io_state; // @[FSR_640.scala 32:22]
  wire  once_35_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_35_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_36_io_state; // @[FSR_640.scala 32:22]
  wire  once_36_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_36_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_37_io_state; // @[FSR_640.scala 32:22]
  wire  once_37_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_37_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_38_io_state; // @[FSR_640.scala 32:22]
  wire  once_38_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_38_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_39_io_state; // @[FSR_640.scala 32:22]
  wire  once_39_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_39_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_40_io_state; // @[FSR_640.scala 32:22]
  wire  once_40_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_40_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_41_io_state; // @[FSR_640.scala 32:22]
  wire  once_41_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_41_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_42_io_state; // @[FSR_640.scala 32:22]
  wire  once_42_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_42_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_43_io_state; // @[FSR_640.scala 32:22]
  wire  once_43_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_43_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_44_io_state; // @[FSR_640.scala 32:22]
  wire  once_44_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_44_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_45_io_state; // @[FSR_640.scala 32:22]
  wire  once_45_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_45_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_46_io_state; // @[FSR_640.scala 32:22]
  wire  once_46_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_46_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_47_io_state; // @[FSR_640.scala 32:22]
  wire  once_47_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_47_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_48_io_state; // @[FSR_640.scala 32:22]
  wire  once_48_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_48_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_49_io_state; // @[FSR_640.scala 32:22]
  wire  once_49_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_49_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_50_io_state; // @[FSR_640.scala 32:22]
  wire  once_50_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_50_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_51_io_state; // @[FSR_640.scala 32:22]
  wire  once_51_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_51_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_52_io_state; // @[FSR_640.scala 32:22]
  wire  once_52_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_52_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_53_io_state; // @[FSR_640.scala 32:22]
  wire  once_53_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_53_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_54_io_state; // @[FSR_640.scala 32:22]
  wire  once_54_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_54_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_55_io_state; // @[FSR_640.scala 32:22]
  wire  once_55_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_55_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_56_io_state; // @[FSR_640.scala 32:22]
  wire  once_56_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_56_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_57_io_state; // @[FSR_640.scala 32:22]
  wire  once_57_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_57_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_58_io_state; // @[FSR_640.scala 32:22]
  wire  once_58_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_58_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_59_io_state; // @[FSR_640.scala 32:22]
  wire  once_59_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_59_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_60_io_state; // @[FSR_640.scala 32:22]
  wire  once_60_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_60_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_61_io_state; // @[FSR_640.scala 32:22]
  wire  once_61_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_61_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_62_io_state; // @[FSR_640.scala 32:22]
  wire  once_62_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_62_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_63_io_state; // @[FSR_640.scala 32:22]
  wire  once_63_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_63_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_64_io_state; // @[FSR_640.scala 32:22]
  wire  once_64_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_64_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_65_io_state; // @[FSR_640.scala 32:22]
  wire  once_65_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_65_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_66_io_state; // @[FSR_640.scala 32:22]
  wire  once_66_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_66_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_67_io_state; // @[FSR_640.scala 32:22]
  wire  once_67_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_67_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_68_io_state; // @[FSR_640.scala 32:22]
  wire  once_68_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_68_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_69_io_state; // @[FSR_640.scala 32:22]
  wire  once_69_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_69_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_70_io_state; // @[FSR_640.scala 32:22]
  wire  once_70_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_70_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_71_io_state; // @[FSR_640.scala 32:22]
  wire  once_71_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_71_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_72_io_state; // @[FSR_640.scala 32:22]
  wire  once_72_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_72_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_73_io_state; // @[FSR_640.scala 32:22]
  wire  once_73_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_73_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_74_io_state; // @[FSR_640.scala 32:22]
  wire  once_74_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_74_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_75_io_state; // @[FSR_640.scala 32:22]
  wire  once_75_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_75_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_76_io_state; // @[FSR_640.scala 32:22]
  wire  once_76_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_76_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_77_io_state; // @[FSR_640.scala 32:22]
  wire  once_77_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_77_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_78_io_state; // @[FSR_640.scala 32:22]
  wire  once_78_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_78_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_79_io_state; // @[FSR_640.scala 32:22]
  wire  once_79_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_79_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_80_io_state; // @[FSR_640.scala 32:22]
  wire  once_80_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_80_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_81_io_state; // @[FSR_640.scala 32:22]
  wire  once_81_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_81_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_82_io_state; // @[FSR_640.scala 32:22]
  wire  once_82_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_82_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_83_io_state; // @[FSR_640.scala 32:22]
  wire  once_83_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_83_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_84_io_state; // @[FSR_640.scala 32:22]
  wire  once_84_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_84_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_85_io_state; // @[FSR_640.scala 32:22]
  wire  once_85_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_85_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_86_io_state; // @[FSR_640.scala 32:22]
  wire  once_86_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_86_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_87_io_state; // @[FSR_640.scala 32:22]
  wire  once_87_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_87_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_88_io_state; // @[FSR_640.scala 32:22]
  wire  once_88_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_88_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_89_io_state; // @[FSR_640.scala 32:22]
  wire  once_89_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_89_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_90_io_state; // @[FSR_640.scala 32:22]
  wire  once_90_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_90_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_91_io_state; // @[FSR_640.scala 32:22]
  wire  once_91_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_91_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_92_io_state; // @[FSR_640.scala 32:22]
  wire  once_92_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_92_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_93_io_state; // @[FSR_640.scala 32:22]
  wire  once_93_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_93_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_94_io_state; // @[FSR_640.scala 32:22]
  wire  once_94_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_94_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_95_io_state; // @[FSR_640.scala 32:22]
  wire  once_95_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_95_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_96_io_state; // @[FSR_640.scala 32:22]
  wire  once_96_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_96_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_97_io_state; // @[FSR_640.scala 32:22]
  wire  once_97_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_97_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_98_io_state; // @[FSR_640.scala 32:22]
  wire  once_98_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_98_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_99_io_state; // @[FSR_640.scala 32:22]
  wire  once_99_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_99_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_100_io_state; // @[FSR_640.scala 32:22]
  wire  once_100_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_100_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_101_io_state; // @[FSR_640.scala 32:22]
  wire  once_101_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_101_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_102_io_state; // @[FSR_640.scala 32:22]
  wire  once_102_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_102_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_103_io_state; // @[FSR_640.scala 32:22]
  wire  once_103_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_103_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_104_io_state; // @[FSR_640.scala 32:22]
  wire  once_104_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_104_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_105_io_state; // @[FSR_640.scala 32:22]
  wire  once_105_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_105_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_106_io_state; // @[FSR_640.scala 32:22]
  wire  once_106_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_106_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_107_io_state; // @[FSR_640.scala 32:22]
  wire  once_107_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_107_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_108_io_state; // @[FSR_640.scala 32:22]
  wire  once_108_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_108_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_109_io_state; // @[FSR_640.scala 32:22]
  wire  once_109_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_109_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_110_io_state; // @[FSR_640.scala 32:22]
  wire  once_110_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_110_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_111_io_state; // @[FSR_640.scala 32:22]
  wire  once_111_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_111_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_112_io_state; // @[FSR_640.scala 32:22]
  wire  once_112_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_112_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_113_io_state; // @[FSR_640.scala 32:22]
  wire  once_113_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_113_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_114_io_state; // @[FSR_640.scala 32:22]
  wire  once_114_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_114_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_115_io_state; // @[FSR_640.scala 32:22]
  wire  once_115_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_115_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_116_io_state; // @[FSR_640.scala 32:22]
  wire  once_116_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_116_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_117_io_state; // @[FSR_640.scala 32:22]
  wire  once_117_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_117_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_118_io_state; // @[FSR_640.scala 32:22]
  wire  once_118_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_118_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_119_io_state; // @[FSR_640.scala 32:22]
  wire  once_119_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_119_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_120_io_state; // @[FSR_640.scala 32:22]
  wire  once_120_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_120_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_121_io_state; // @[FSR_640.scala 32:22]
  wire  once_121_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_121_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_122_io_state; // @[FSR_640.scala 32:22]
  wire  once_122_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_122_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_123_io_state; // @[FSR_640.scala 32:22]
  wire  once_123_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_123_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_124_io_state; // @[FSR_640.scala 32:22]
  wire  once_124_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_124_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_125_io_state; // @[FSR_640.scala 32:22]
  wire  once_125_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_125_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_126_io_state; // @[FSR_640.scala 32:22]
  wire  once_126_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_126_io_state_out; // @[FSR_640.scala 32:22]
  wire [127:0] once_127_io_state; // @[FSR_640.scala 32:22]
  wire  once_127_io_key; // @[FSR_640.scala 32:22]
  wire [127:0] once_127_io_state_out; // @[FSR_640.scala 32:22]
  FSR_once once ( // @[FSR_640.scala 32:22]
    .io_state(once_io_state),
    .io_key(once_io_key),
    .io_state_out(once_io_state_out)
  );
  FSR_once once_1 ( // @[FSR_640.scala 32:22]
    .io_state(once_1_io_state),
    .io_key(once_1_io_key),
    .io_state_out(once_1_io_state_out)
  );
  FSR_once once_2 ( // @[FSR_640.scala 32:22]
    .io_state(once_2_io_state),
    .io_key(once_2_io_key),
    .io_state_out(once_2_io_state_out)
  );
  FSR_once once_3 ( // @[FSR_640.scala 32:22]
    .io_state(once_3_io_state),
    .io_key(once_3_io_key),
    .io_state_out(once_3_io_state_out)
  );
  FSR_once once_4 ( // @[FSR_640.scala 32:22]
    .io_state(once_4_io_state),
    .io_key(once_4_io_key),
    .io_state_out(once_4_io_state_out)
  );
  FSR_once once_5 ( // @[FSR_640.scala 32:22]
    .io_state(once_5_io_state),
    .io_key(once_5_io_key),
    .io_state_out(once_5_io_state_out)
  );
  FSR_once once_6 ( // @[FSR_640.scala 32:22]
    .io_state(once_6_io_state),
    .io_key(once_6_io_key),
    .io_state_out(once_6_io_state_out)
  );
  FSR_once once_7 ( // @[FSR_640.scala 32:22]
    .io_state(once_7_io_state),
    .io_key(once_7_io_key),
    .io_state_out(once_7_io_state_out)
  );
  FSR_once once_8 ( // @[FSR_640.scala 32:22]
    .io_state(once_8_io_state),
    .io_key(once_8_io_key),
    .io_state_out(once_8_io_state_out)
  );
  FSR_once once_9 ( // @[FSR_640.scala 32:22]
    .io_state(once_9_io_state),
    .io_key(once_9_io_key),
    .io_state_out(once_9_io_state_out)
  );
  FSR_once once_10 ( // @[FSR_640.scala 32:22]
    .io_state(once_10_io_state),
    .io_key(once_10_io_key),
    .io_state_out(once_10_io_state_out)
  );
  FSR_once once_11 ( // @[FSR_640.scala 32:22]
    .io_state(once_11_io_state),
    .io_key(once_11_io_key),
    .io_state_out(once_11_io_state_out)
  );
  FSR_once once_12 ( // @[FSR_640.scala 32:22]
    .io_state(once_12_io_state),
    .io_key(once_12_io_key),
    .io_state_out(once_12_io_state_out)
  );
  FSR_once once_13 ( // @[FSR_640.scala 32:22]
    .io_state(once_13_io_state),
    .io_key(once_13_io_key),
    .io_state_out(once_13_io_state_out)
  );
  FSR_once once_14 ( // @[FSR_640.scala 32:22]
    .io_state(once_14_io_state),
    .io_key(once_14_io_key),
    .io_state_out(once_14_io_state_out)
  );
  FSR_once once_15 ( // @[FSR_640.scala 32:22]
    .io_state(once_15_io_state),
    .io_key(once_15_io_key),
    .io_state_out(once_15_io_state_out)
  );
  FSR_once once_16 ( // @[FSR_640.scala 32:22]
    .io_state(once_16_io_state),
    .io_key(once_16_io_key),
    .io_state_out(once_16_io_state_out)
  );
  FSR_once once_17 ( // @[FSR_640.scala 32:22]
    .io_state(once_17_io_state),
    .io_key(once_17_io_key),
    .io_state_out(once_17_io_state_out)
  );
  FSR_once once_18 ( // @[FSR_640.scala 32:22]
    .io_state(once_18_io_state),
    .io_key(once_18_io_key),
    .io_state_out(once_18_io_state_out)
  );
  FSR_once once_19 ( // @[FSR_640.scala 32:22]
    .io_state(once_19_io_state),
    .io_key(once_19_io_key),
    .io_state_out(once_19_io_state_out)
  );
  FSR_once once_20 ( // @[FSR_640.scala 32:22]
    .io_state(once_20_io_state),
    .io_key(once_20_io_key),
    .io_state_out(once_20_io_state_out)
  );
  FSR_once once_21 ( // @[FSR_640.scala 32:22]
    .io_state(once_21_io_state),
    .io_key(once_21_io_key),
    .io_state_out(once_21_io_state_out)
  );
  FSR_once once_22 ( // @[FSR_640.scala 32:22]
    .io_state(once_22_io_state),
    .io_key(once_22_io_key),
    .io_state_out(once_22_io_state_out)
  );
  FSR_once once_23 ( // @[FSR_640.scala 32:22]
    .io_state(once_23_io_state),
    .io_key(once_23_io_key),
    .io_state_out(once_23_io_state_out)
  );
  FSR_once once_24 ( // @[FSR_640.scala 32:22]
    .io_state(once_24_io_state),
    .io_key(once_24_io_key),
    .io_state_out(once_24_io_state_out)
  );
  FSR_once once_25 ( // @[FSR_640.scala 32:22]
    .io_state(once_25_io_state),
    .io_key(once_25_io_key),
    .io_state_out(once_25_io_state_out)
  );
  FSR_once once_26 ( // @[FSR_640.scala 32:22]
    .io_state(once_26_io_state),
    .io_key(once_26_io_key),
    .io_state_out(once_26_io_state_out)
  );
  FSR_once once_27 ( // @[FSR_640.scala 32:22]
    .io_state(once_27_io_state),
    .io_key(once_27_io_key),
    .io_state_out(once_27_io_state_out)
  );
  FSR_once once_28 ( // @[FSR_640.scala 32:22]
    .io_state(once_28_io_state),
    .io_key(once_28_io_key),
    .io_state_out(once_28_io_state_out)
  );
  FSR_once once_29 ( // @[FSR_640.scala 32:22]
    .io_state(once_29_io_state),
    .io_key(once_29_io_key),
    .io_state_out(once_29_io_state_out)
  );
  FSR_once once_30 ( // @[FSR_640.scala 32:22]
    .io_state(once_30_io_state),
    .io_key(once_30_io_key),
    .io_state_out(once_30_io_state_out)
  );
  FSR_once once_31 ( // @[FSR_640.scala 32:22]
    .io_state(once_31_io_state),
    .io_key(once_31_io_key),
    .io_state_out(once_31_io_state_out)
  );
  FSR_once once_32 ( // @[FSR_640.scala 32:22]
    .io_state(once_32_io_state),
    .io_key(once_32_io_key),
    .io_state_out(once_32_io_state_out)
  );
  FSR_once once_33 ( // @[FSR_640.scala 32:22]
    .io_state(once_33_io_state),
    .io_key(once_33_io_key),
    .io_state_out(once_33_io_state_out)
  );
  FSR_once once_34 ( // @[FSR_640.scala 32:22]
    .io_state(once_34_io_state),
    .io_key(once_34_io_key),
    .io_state_out(once_34_io_state_out)
  );
  FSR_once once_35 ( // @[FSR_640.scala 32:22]
    .io_state(once_35_io_state),
    .io_key(once_35_io_key),
    .io_state_out(once_35_io_state_out)
  );
  FSR_once once_36 ( // @[FSR_640.scala 32:22]
    .io_state(once_36_io_state),
    .io_key(once_36_io_key),
    .io_state_out(once_36_io_state_out)
  );
  FSR_once once_37 ( // @[FSR_640.scala 32:22]
    .io_state(once_37_io_state),
    .io_key(once_37_io_key),
    .io_state_out(once_37_io_state_out)
  );
  FSR_once once_38 ( // @[FSR_640.scala 32:22]
    .io_state(once_38_io_state),
    .io_key(once_38_io_key),
    .io_state_out(once_38_io_state_out)
  );
  FSR_once once_39 ( // @[FSR_640.scala 32:22]
    .io_state(once_39_io_state),
    .io_key(once_39_io_key),
    .io_state_out(once_39_io_state_out)
  );
  FSR_once once_40 ( // @[FSR_640.scala 32:22]
    .io_state(once_40_io_state),
    .io_key(once_40_io_key),
    .io_state_out(once_40_io_state_out)
  );
  FSR_once once_41 ( // @[FSR_640.scala 32:22]
    .io_state(once_41_io_state),
    .io_key(once_41_io_key),
    .io_state_out(once_41_io_state_out)
  );
  FSR_once once_42 ( // @[FSR_640.scala 32:22]
    .io_state(once_42_io_state),
    .io_key(once_42_io_key),
    .io_state_out(once_42_io_state_out)
  );
  FSR_once once_43 ( // @[FSR_640.scala 32:22]
    .io_state(once_43_io_state),
    .io_key(once_43_io_key),
    .io_state_out(once_43_io_state_out)
  );
  FSR_once once_44 ( // @[FSR_640.scala 32:22]
    .io_state(once_44_io_state),
    .io_key(once_44_io_key),
    .io_state_out(once_44_io_state_out)
  );
  FSR_once once_45 ( // @[FSR_640.scala 32:22]
    .io_state(once_45_io_state),
    .io_key(once_45_io_key),
    .io_state_out(once_45_io_state_out)
  );
  FSR_once once_46 ( // @[FSR_640.scala 32:22]
    .io_state(once_46_io_state),
    .io_key(once_46_io_key),
    .io_state_out(once_46_io_state_out)
  );
  FSR_once once_47 ( // @[FSR_640.scala 32:22]
    .io_state(once_47_io_state),
    .io_key(once_47_io_key),
    .io_state_out(once_47_io_state_out)
  );
  FSR_once once_48 ( // @[FSR_640.scala 32:22]
    .io_state(once_48_io_state),
    .io_key(once_48_io_key),
    .io_state_out(once_48_io_state_out)
  );
  FSR_once once_49 ( // @[FSR_640.scala 32:22]
    .io_state(once_49_io_state),
    .io_key(once_49_io_key),
    .io_state_out(once_49_io_state_out)
  );
  FSR_once once_50 ( // @[FSR_640.scala 32:22]
    .io_state(once_50_io_state),
    .io_key(once_50_io_key),
    .io_state_out(once_50_io_state_out)
  );
  FSR_once once_51 ( // @[FSR_640.scala 32:22]
    .io_state(once_51_io_state),
    .io_key(once_51_io_key),
    .io_state_out(once_51_io_state_out)
  );
  FSR_once once_52 ( // @[FSR_640.scala 32:22]
    .io_state(once_52_io_state),
    .io_key(once_52_io_key),
    .io_state_out(once_52_io_state_out)
  );
  FSR_once once_53 ( // @[FSR_640.scala 32:22]
    .io_state(once_53_io_state),
    .io_key(once_53_io_key),
    .io_state_out(once_53_io_state_out)
  );
  FSR_once once_54 ( // @[FSR_640.scala 32:22]
    .io_state(once_54_io_state),
    .io_key(once_54_io_key),
    .io_state_out(once_54_io_state_out)
  );
  FSR_once once_55 ( // @[FSR_640.scala 32:22]
    .io_state(once_55_io_state),
    .io_key(once_55_io_key),
    .io_state_out(once_55_io_state_out)
  );
  FSR_once once_56 ( // @[FSR_640.scala 32:22]
    .io_state(once_56_io_state),
    .io_key(once_56_io_key),
    .io_state_out(once_56_io_state_out)
  );
  FSR_once once_57 ( // @[FSR_640.scala 32:22]
    .io_state(once_57_io_state),
    .io_key(once_57_io_key),
    .io_state_out(once_57_io_state_out)
  );
  FSR_once once_58 ( // @[FSR_640.scala 32:22]
    .io_state(once_58_io_state),
    .io_key(once_58_io_key),
    .io_state_out(once_58_io_state_out)
  );
  FSR_once once_59 ( // @[FSR_640.scala 32:22]
    .io_state(once_59_io_state),
    .io_key(once_59_io_key),
    .io_state_out(once_59_io_state_out)
  );
  FSR_once once_60 ( // @[FSR_640.scala 32:22]
    .io_state(once_60_io_state),
    .io_key(once_60_io_key),
    .io_state_out(once_60_io_state_out)
  );
  FSR_once once_61 ( // @[FSR_640.scala 32:22]
    .io_state(once_61_io_state),
    .io_key(once_61_io_key),
    .io_state_out(once_61_io_state_out)
  );
  FSR_once once_62 ( // @[FSR_640.scala 32:22]
    .io_state(once_62_io_state),
    .io_key(once_62_io_key),
    .io_state_out(once_62_io_state_out)
  );
  FSR_once once_63 ( // @[FSR_640.scala 32:22]
    .io_state(once_63_io_state),
    .io_key(once_63_io_key),
    .io_state_out(once_63_io_state_out)
  );
  FSR_once once_64 ( // @[FSR_640.scala 32:22]
    .io_state(once_64_io_state),
    .io_key(once_64_io_key),
    .io_state_out(once_64_io_state_out)
  );
  FSR_once once_65 ( // @[FSR_640.scala 32:22]
    .io_state(once_65_io_state),
    .io_key(once_65_io_key),
    .io_state_out(once_65_io_state_out)
  );
  FSR_once once_66 ( // @[FSR_640.scala 32:22]
    .io_state(once_66_io_state),
    .io_key(once_66_io_key),
    .io_state_out(once_66_io_state_out)
  );
  FSR_once once_67 ( // @[FSR_640.scala 32:22]
    .io_state(once_67_io_state),
    .io_key(once_67_io_key),
    .io_state_out(once_67_io_state_out)
  );
  FSR_once once_68 ( // @[FSR_640.scala 32:22]
    .io_state(once_68_io_state),
    .io_key(once_68_io_key),
    .io_state_out(once_68_io_state_out)
  );
  FSR_once once_69 ( // @[FSR_640.scala 32:22]
    .io_state(once_69_io_state),
    .io_key(once_69_io_key),
    .io_state_out(once_69_io_state_out)
  );
  FSR_once once_70 ( // @[FSR_640.scala 32:22]
    .io_state(once_70_io_state),
    .io_key(once_70_io_key),
    .io_state_out(once_70_io_state_out)
  );
  FSR_once once_71 ( // @[FSR_640.scala 32:22]
    .io_state(once_71_io_state),
    .io_key(once_71_io_key),
    .io_state_out(once_71_io_state_out)
  );
  FSR_once once_72 ( // @[FSR_640.scala 32:22]
    .io_state(once_72_io_state),
    .io_key(once_72_io_key),
    .io_state_out(once_72_io_state_out)
  );
  FSR_once once_73 ( // @[FSR_640.scala 32:22]
    .io_state(once_73_io_state),
    .io_key(once_73_io_key),
    .io_state_out(once_73_io_state_out)
  );
  FSR_once once_74 ( // @[FSR_640.scala 32:22]
    .io_state(once_74_io_state),
    .io_key(once_74_io_key),
    .io_state_out(once_74_io_state_out)
  );
  FSR_once once_75 ( // @[FSR_640.scala 32:22]
    .io_state(once_75_io_state),
    .io_key(once_75_io_key),
    .io_state_out(once_75_io_state_out)
  );
  FSR_once once_76 ( // @[FSR_640.scala 32:22]
    .io_state(once_76_io_state),
    .io_key(once_76_io_key),
    .io_state_out(once_76_io_state_out)
  );
  FSR_once once_77 ( // @[FSR_640.scala 32:22]
    .io_state(once_77_io_state),
    .io_key(once_77_io_key),
    .io_state_out(once_77_io_state_out)
  );
  FSR_once once_78 ( // @[FSR_640.scala 32:22]
    .io_state(once_78_io_state),
    .io_key(once_78_io_key),
    .io_state_out(once_78_io_state_out)
  );
  FSR_once once_79 ( // @[FSR_640.scala 32:22]
    .io_state(once_79_io_state),
    .io_key(once_79_io_key),
    .io_state_out(once_79_io_state_out)
  );
  FSR_once once_80 ( // @[FSR_640.scala 32:22]
    .io_state(once_80_io_state),
    .io_key(once_80_io_key),
    .io_state_out(once_80_io_state_out)
  );
  FSR_once once_81 ( // @[FSR_640.scala 32:22]
    .io_state(once_81_io_state),
    .io_key(once_81_io_key),
    .io_state_out(once_81_io_state_out)
  );
  FSR_once once_82 ( // @[FSR_640.scala 32:22]
    .io_state(once_82_io_state),
    .io_key(once_82_io_key),
    .io_state_out(once_82_io_state_out)
  );
  FSR_once once_83 ( // @[FSR_640.scala 32:22]
    .io_state(once_83_io_state),
    .io_key(once_83_io_key),
    .io_state_out(once_83_io_state_out)
  );
  FSR_once once_84 ( // @[FSR_640.scala 32:22]
    .io_state(once_84_io_state),
    .io_key(once_84_io_key),
    .io_state_out(once_84_io_state_out)
  );
  FSR_once once_85 ( // @[FSR_640.scala 32:22]
    .io_state(once_85_io_state),
    .io_key(once_85_io_key),
    .io_state_out(once_85_io_state_out)
  );
  FSR_once once_86 ( // @[FSR_640.scala 32:22]
    .io_state(once_86_io_state),
    .io_key(once_86_io_key),
    .io_state_out(once_86_io_state_out)
  );
  FSR_once once_87 ( // @[FSR_640.scala 32:22]
    .io_state(once_87_io_state),
    .io_key(once_87_io_key),
    .io_state_out(once_87_io_state_out)
  );
  FSR_once once_88 ( // @[FSR_640.scala 32:22]
    .io_state(once_88_io_state),
    .io_key(once_88_io_key),
    .io_state_out(once_88_io_state_out)
  );
  FSR_once once_89 ( // @[FSR_640.scala 32:22]
    .io_state(once_89_io_state),
    .io_key(once_89_io_key),
    .io_state_out(once_89_io_state_out)
  );
  FSR_once once_90 ( // @[FSR_640.scala 32:22]
    .io_state(once_90_io_state),
    .io_key(once_90_io_key),
    .io_state_out(once_90_io_state_out)
  );
  FSR_once once_91 ( // @[FSR_640.scala 32:22]
    .io_state(once_91_io_state),
    .io_key(once_91_io_key),
    .io_state_out(once_91_io_state_out)
  );
  FSR_once once_92 ( // @[FSR_640.scala 32:22]
    .io_state(once_92_io_state),
    .io_key(once_92_io_key),
    .io_state_out(once_92_io_state_out)
  );
  FSR_once once_93 ( // @[FSR_640.scala 32:22]
    .io_state(once_93_io_state),
    .io_key(once_93_io_key),
    .io_state_out(once_93_io_state_out)
  );
  FSR_once once_94 ( // @[FSR_640.scala 32:22]
    .io_state(once_94_io_state),
    .io_key(once_94_io_key),
    .io_state_out(once_94_io_state_out)
  );
  FSR_once once_95 ( // @[FSR_640.scala 32:22]
    .io_state(once_95_io_state),
    .io_key(once_95_io_key),
    .io_state_out(once_95_io_state_out)
  );
  FSR_once once_96 ( // @[FSR_640.scala 32:22]
    .io_state(once_96_io_state),
    .io_key(once_96_io_key),
    .io_state_out(once_96_io_state_out)
  );
  FSR_once once_97 ( // @[FSR_640.scala 32:22]
    .io_state(once_97_io_state),
    .io_key(once_97_io_key),
    .io_state_out(once_97_io_state_out)
  );
  FSR_once once_98 ( // @[FSR_640.scala 32:22]
    .io_state(once_98_io_state),
    .io_key(once_98_io_key),
    .io_state_out(once_98_io_state_out)
  );
  FSR_once once_99 ( // @[FSR_640.scala 32:22]
    .io_state(once_99_io_state),
    .io_key(once_99_io_key),
    .io_state_out(once_99_io_state_out)
  );
  FSR_once once_100 ( // @[FSR_640.scala 32:22]
    .io_state(once_100_io_state),
    .io_key(once_100_io_key),
    .io_state_out(once_100_io_state_out)
  );
  FSR_once once_101 ( // @[FSR_640.scala 32:22]
    .io_state(once_101_io_state),
    .io_key(once_101_io_key),
    .io_state_out(once_101_io_state_out)
  );
  FSR_once once_102 ( // @[FSR_640.scala 32:22]
    .io_state(once_102_io_state),
    .io_key(once_102_io_key),
    .io_state_out(once_102_io_state_out)
  );
  FSR_once once_103 ( // @[FSR_640.scala 32:22]
    .io_state(once_103_io_state),
    .io_key(once_103_io_key),
    .io_state_out(once_103_io_state_out)
  );
  FSR_once once_104 ( // @[FSR_640.scala 32:22]
    .io_state(once_104_io_state),
    .io_key(once_104_io_key),
    .io_state_out(once_104_io_state_out)
  );
  FSR_once once_105 ( // @[FSR_640.scala 32:22]
    .io_state(once_105_io_state),
    .io_key(once_105_io_key),
    .io_state_out(once_105_io_state_out)
  );
  FSR_once once_106 ( // @[FSR_640.scala 32:22]
    .io_state(once_106_io_state),
    .io_key(once_106_io_key),
    .io_state_out(once_106_io_state_out)
  );
  FSR_once once_107 ( // @[FSR_640.scala 32:22]
    .io_state(once_107_io_state),
    .io_key(once_107_io_key),
    .io_state_out(once_107_io_state_out)
  );
  FSR_once once_108 ( // @[FSR_640.scala 32:22]
    .io_state(once_108_io_state),
    .io_key(once_108_io_key),
    .io_state_out(once_108_io_state_out)
  );
  FSR_once once_109 ( // @[FSR_640.scala 32:22]
    .io_state(once_109_io_state),
    .io_key(once_109_io_key),
    .io_state_out(once_109_io_state_out)
  );
  FSR_once once_110 ( // @[FSR_640.scala 32:22]
    .io_state(once_110_io_state),
    .io_key(once_110_io_key),
    .io_state_out(once_110_io_state_out)
  );
  FSR_once once_111 ( // @[FSR_640.scala 32:22]
    .io_state(once_111_io_state),
    .io_key(once_111_io_key),
    .io_state_out(once_111_io_state_out)
  );
  FSR_once once_112 ( // @[FSR_640.scala 32:22]
    .io_state(once_112_io_state),
    .io_key(once_112_io_key),
    .io_state_out(once_112_io_state_out)
  );
  FSR_once once_113 ( // @[FSR_640.scala 32:22]
    .io_state(once_113_io_state),
    .io_key(once_113_io_key),
    .io_state_out(once_113_io_state_out)
  );
  FSR_once once_114 ( // @[FSR_640.scala 32:22]
    .io_state(once_114_io_state),
    .io_key(once_114_io_key),
    .io_state_out(once_114_io_state_out)
  );
  FSR_once once_115 ( // @[FSR_640.scala 32:22]
    .io_state(once_115_io_state),
    .io_key(once_115_io_key),
    .io_state_out(once_115_io_state_out)
  );
  FSR_once once_116 ( // @[FSR_640.scala 32:22]
    .io_state(once_116_io_state),
    .io_key(once_116_io_key),
    .io_state_out(once_116_io_state_out)
  );
  FSR_once once_117 ( // @[FSR_640.scala 32:22]
    .io_state(once_117_io_state),
    .io_key(once_117_io_key),
    .io_state_out(once_117_io_state_out)
  );
  FSR_once once_118 ( // @[FSR_640.scala 32:22]
    .io_state(once_118_io_state),
    .io_key(once_118_io_key),
    .io_state_out(once_118_io_state_out)
  );
  FSR_once once_119 ( // @[FSR_640.scala 32:22]
    .io_state(once_119_io_state),
    .io_key(once_119_io_key),
    .io_state_out(once_119_io_state_out)
  );
  FSR_once once_120 ( // @[FSR_640.scala 32:22]
    .io_state(once_120_io_state),
    .io_key(once_120_io_key),
    .io_state_out(once_120_io_state_out)
  );
  FSR_once once_121 ( // @[FSR_640.scala 32:22]
    .io_state(once_121_io_state),
    .io_key(once_121_io_key),
    .io_state_out(once_121_io_state_out)
  );
  FSR_once once_122 ( // @[FSR_640.scala 32:22]
    .io_state(once_122_io_state),
    .io_key(once_122_io_key),
    .io_state_out(once_122_io_state_out)
  );
  FSR_once once_123 ( // @[FSR_640.scala 32:22]
    .io_state(once_123_io_state),
    .io_key(once_123_io_key),
    .io_state_out(once_123_io_state_out)
  );
  FSR_once once_124 ( // @[FSR_640.scala 32:22]
    .io_state(once_124_io_state),
    .io_key(once_124_io_key),
    .io_state_out(once_124_io_state_out)
  );
  FSR_once once_125 ( // @[FSR_640.scala 32:22]
    .io_state(once_125_io_state),
    .io_key(once_125_io_key),
    .io_state_out(once_125_io_state_out)
  );
  FSR_once once_126 ( // @[FSR_640.scala 32:22]
    .io_state(once_126_io_state),
    .io_key(once_126_io_key),
    .io_state_out(once_126_io_state_out)
  );
  FSR_once once_127 ( // @[FSR_640.scala 32:22]
    .io_state(once_127_io_state),
    .io_key(once_127_io_key),
    .io_state_out(once_127_io_state_out)
  );
  assign io_state_out = once_127_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_io_state = io_state; // @[FSR_640.scala 29:24 30:17]
  assign once_io_key = io_key[0]; // @[FSR_640.scala 33:26]
  assign once_1_io_state = once_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_1_io_key = io_key[1]; // @[FSR_640.scala 33:26]
  assign once_2_io_state = once_1_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_2_io_key = io_key[2]; // @[FSR_640.scala 33:26]
  assign once_3_io_state = once_2_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_3_io_key = io_key[3]; // @[FSR_640.scala 33:26]
  assign once_4_io_state = once_3_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_4_io_key = io_key[4]; // @[FSR_640.scala 33:26]
  assign once_5_io_state = once_4_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_5_io_key = io_key[5]; // @[FSR_640.scala 33:26]
  assign once_6_io_state = once_5_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_6_io_key = io_key[6]; // @[FSR_640.scala 33:26]
  assign once_7_io_state = once_6_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_7_io_key = io_key[7]; // @[FSR_640.scala 33:26]
  assign once_8_io_state = once_7_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_8_io_key = io_key[8]; // @[FSR_640.scala 33:26]
  assign once_9_io_state = once_8_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_9_io_key = io_key[9]; // @[FSR_640.scala 33:26]
  assign once_10_io_state = once_9_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_10_io_key = io_key[10]; // @[FSR_640.scala 33:26]
  assign once_11_io_state = once_10_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_11_io_key = io_key[11]; // @[FSR_640.scala 33:26]
  assign once_12_io_state = once_11_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_12_io_key = io_key[12]; // @[FSR_640.scala 33:26]
  assign once_13_io_state = once_12_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_13_io_key = io_key[13]; // @[FSR_640.scala 33:26]
  assign once_14_io_state = once_13_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_14_io_key = io_key[14]; // @[FSR_640.scala 33:26]
  assign once_15_io_state = once_14_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_15_io_key = io_key[15]; // @[FSR_640.scala 33:26]
  assign once_16_io_state = once_15_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_16_io_key = io_key[16]; // @[FSR_640.scala 33:26]
  assign once_17_io_state = once_16_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_17_io_key = io_key[17]; // @[FSR_640.scala 33:26]
  assign once_18_io_state = once_17_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_18_io_key = io_key[18]; // @[FSR_640.scala 33:26]
  assign once_19_io_state = once_18_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_19_io_key = io_key[19]; // @[FSR_640.scala 33:26]
  assign once_20_io_state = once_19_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_20_io_key = io_key[20]; // @[FSR_640.scala 33:26]
  assign once_21_io_state = once_20_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_21_io_key = io_key[21]; // @[FSR_640.scala 33:26]
  assign once_22_io_state = once_21_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_22_io_key = io_key[22]; // @[FSR_640.scala 33:26]
  assign once_23_io_state = once_22_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_23_io_key = io_key[23]; // @[FSR_640.scala 33:26]
  assign once_24_io_state = once_23_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_24_io_key = io_key[24]; // @[FSR_640.scala 33:26]
  assign once_25_io_state = once_24_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_25_io_key = io_key[25]; // @[FSR_640.scala 33:26]
  assign once_26_io_state = once_25_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_26_io_key = io_key[26]; // @[FSR_640.scala 33:26]
  assign once_27_io_state = once_26_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_27_io_key = io_key[27]; // @[FSR_640.scala 33:26]
  assign once_28_io_state = once_27_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_28_io_key = io_key[28]; // @[FSR_640.scala 33:26]
  assign once_29_io_state = once_28_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_29_io_key = io_key[29]; // @[FSR_640.scala 33:26]
  assign once_30_io_state = once_29_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_30_io_key = io_key[30]; // @[FSR_640.scala 33:26]
  assign once_31_io_state = once_30_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_31_io_key = io_key[31]; // @[FSR_640.scala 33:26]
  assign once_32_io_state = once_31_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_32_io_key = io_key[32]; // @[FSR_640.scala 33:26]
  assign once_33_io_state = once_32_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_33_io_key = io_key[33]; // @[FSR_640.scala 33:26]
  assign once_34_io_state = once_33_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_34_io_key = io_key[34]; // @[FSR_640.scala 33:26]
  assign once_35_io_state = once_34_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_35_io_key = io_key[35]; // @[FSR_640.scala 33:26]
  assign once_36_io_state = once_35_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_36_io_key = io_key[36]; // @[FSR_640.scala 33:26]
  assign once_37_io_state = once_36_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_37_io_key = io_key[37]; // @[FSR_640.scala 33:26]
  assign once_38_io_state = once_37_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_38_io_key = io_key[38]; // @[FSR_640.scala 33:26]
  assign once_39_io_state = once_38_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_39_io_key = io_key[39]; // @[FSR_640.scala 33:26]
  assign once_40_io_state = once_39_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_40_io_key = io_key[40]; // @[FSR_640.scala 33:26]
  assign once_41_io_state = once_40_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_41_io_key = io_key[41]; // @[FSR_640.scala 33:26]
  assign once_42_io_state = once_41_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_42_io_key = io_key[42]; // @[FSR_640.scala 33:26]
  assign once_43_io_state = once_42_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_43_io_key = io_key[43]; // @[FSR_640.scala 33:26]
  assign once_44_io_state = once_43_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_44_io_key = io_key[44]; // @[FSR_640.scala 33:26]
  assign once_45_io_state = once_44_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_45_io_key = io_key[45]; // @[FSR_640.scala 33:26]
  assign once_46_io_state = once_45_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_46_io_key = io_key[46]; // @[FSR_640.scala 33:26]
  assign once_47_io_state = once_46_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_47_io_key = io_key[47]; // @[FSR_640.scala 33:26]
  assign once_48_io_state = once_47_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_48_io_key = io_key[48]; // @[FSR_640.scala 33:26]
  assign once_49_io_state = once_48_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_49_io_key = io_key[49]; // @[FSR_640.scala 33:26]
  assign once_50_io_state = once_49_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_50_io_key = io_key[50]; // @[FSR_640.scala 33:26]
  assign once_51_io_state = once_50_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_51_io_key = io_key[51]; // @[FSR_640.scala 33:26]
  assign once_52_io_state = once_51_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_52_io_key = io_key[52]; // @[FSR_640.scala 33:26]
  assign once_53_io_state = once_52_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_53_io_key = io_key[53]; // @[FSR_640.scala 33:26]
  assign once_54_io_state = once_53_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_54_io_key = io_key[54]; // @[FSR_640.scala 33:26]
  assign once_55_io_state = once_54_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_55_io_key = io_key[55]; // @[FSR_640.scala 33:26]
  assign once_56_io_state = once_55_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_56_io_key = io_key[56]; // @[FSR_640.scala 33:26]
  assign once_57_io_state = once_56_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_57_io_key = io_key[57]; // @[FSR_640.scala 33:26]
  assign once_58_io_state = once_57_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_58_io_key = io_key[58]; // @[FSR_640.scala 33:26]
  assign once_59_io_state = once_58_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_59_io_key = io_key[59]; // @[FSR_640.scala 33:26]
  assign once_60_io_state = once_59_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_60_io_key = io_key[60]; // @[FSR_640.scala 33:26]
  assign once_61_io_state = once_60_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_61_io_key = io_key[61]; // @[FSR_640.scala 33:26]
  assign once_62_io_state = once_61_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_62_io_key = io_key[62]; // @[FSR_640.scala 33:26]
  assign once_63_io_state = once_62_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_63_io_key = io_key[63]; // @[FSR_640.scala 33:26]
  assign once_64_io_state = once_63_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_64_io_key = io_key[64]; // @[FSR_640.scala 33:26]
  assign once_65_io_state = once_64_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_65_io_key = io_key[65]; // @[FSR_640.scala 33:26]
  assign once_66_io_state = once_65_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_66_io_key = io_key[66]; // @[FSR_640.scala 33:26]
  assign once_67_io_state = once_66_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_67_io_key = io_key[67]; // @[FSR_640.scala 33:26]
  assign once_68_io_state = once_67_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_68_io_key = io_key[68]; // @[FSR_640.scala 33:26]
  assign once_69_io_state = once_68_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_69_io_key = io_key[69]; // @[FSR_640.scala 33:26]
  assign once_70_io_state = once_69_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_70_io_key = io_key[70]; // @[FSR_640.scala 33:26]
  assign once_71_io_state = once_70_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_71_io_key = io_key[71]; // @[FSR_640.scala 33:26]
  assign once_72_io_state = once_71_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_72_io_key = io_key[72]; // @[FSR_640.scala 33:26]
  assign once_73_io_state = once_72_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_73_io_key = io_key[73]; // @[FSR_640.scala 33:26]
  assign once_74_io_state = once_73_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_74_io_key = io_key[74]; // @[FSR_640.scala 33:26]
  assign once_75_io_state = once_74_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_75_io_key = io_key[75]; // @[FSR_640.scala 33:26]
  assign once_76_io_state = once_75_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_76_io_key = io_key[76]; // @[FSR_640.scala 33:26]
  assign once_77_io_state = once_76_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_77_io_key = io_key[77]; // @[FSR_640.scala 33:26]
  assign once_78_io_state = once_77_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_78_io_key = io_key[78]; // @[FSR_640.scala 33:26]
  assign once_79_io_state = once_78_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_79_io_key = io_key[79]; // @[FSR_640.scala 33:26]
  assign once_80_io_state = once_79_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_80_io_key = io_key[80]; // @[FSR_640.scala 33:26]
  assign once_81_io_state = once_80_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_81_io_key = io_key[81]; // @[FSR_640.scala 33:26]
  assign once_82_io_state = once_81_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_82_io_key = io_key[82]; // @[FSR_640.scala 33:26]
  assign once_83_io_state = once_82_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_83_io_key = io_key[83]; // @[FSR_640.scala 33:26]
  assign once_84_io_state = once_83_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_84_io_key = io_key[84]; // @[FSR_640.scala 33:26]
  assign once_85_io_state = once_84_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_85_io_key = io_key[85]; // @[FSR_640.scala 33:26]
  assign once_86_io_state = once_85_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_86_io_key = io_key[86]; // @[FSR_640.scala 33:26]
  assign once_87_io_state = once_86_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_87_io_key = io_key[87]; // @[FSR_640.scala 33:26]
  assign once_88_io_state = once_87_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_88_io_key = io_key[88]; // @[FSR_640.scala 33:26]
  assign once_89_io_state = once_88_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_89_io_key = io_key[89]; // @[FSR_640.scala 33:26]
  assign once_90_io_state = once_89_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_90_io_key = io_key[90]; // @[FSR_640.scala 33:26]
  assign once_91_io_state = once_90_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_91_io_key = io_key[91]; // @[FSR_640.scala 33:26]
  assign once_92_io_state = once_91_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_92_io_key = io_key[92]; // @[FSR_640.scala 33:26]
  assign once_93_io_state = once_92_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_93_io_key = io_key[93]; // @[FSR_640.scala 33:26]
  assign once_94_io_state = once_93_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_94_io_key = io_key[94]; // @[FSR_640.scala 33:26]
  assign once_95_io_state = once_94_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_95_io_key = io_key[95]; // @[FSR_640.scala 33:26]
  assign once_96_io_state = once_95_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_96_io_key = io_key[96]; // @[FSR_640.scala 33:26]
  assign once_97_io_state = once_96_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_97_io_key = io_key[97]; // @[FSR_640.scala 33:26]
  assign once_98_io_state = once_97_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_98_io_key = io_key[98]; // @[FSR_640.scala 33:26]
  assign once_99_io_state = once_98_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_99_io_key = io_key[99]; // @[FSR_640.scala 33:26]
  assign once_100_io_state = once_99_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_100_io_key = io_key[100]; // @[FSR_640.scala 33:26]
  assign once_101_io_state = once_100_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_101_io_key = io_key[101]; // @[FSR_640.scala 33:26]
  assign once_102_io_state = once_101_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_102_io_key = io_key[102]; // @[FSR_640.scala 33:26]
  assign once_103_io_state = once_102_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_103_io_key = io_key[103]; // @[FSR_640.scala 33:26]
  assign once_104_io_state = once_103_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_104_io_key = io_key[104]; // @[FSR_640.scala 33:26]
  assign once_105_io_state = once_104_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_105_io_key = io_key[105]; // @[FSR_640.scala 33:26]
  assign once_106_io_state = once_105_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_106_io_key = io_key[106]; // @[FSR_640.scala 33:26]
  assign once_107_io_state = once_106_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_107_io_key = io_key[107]; // @[FSR_640.scala 33:26]
  assign once_108_io_state = once_107_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_108_io_key = io_key[108]; // @[FSR_640.scala 33:26]
  assign once_109_io_state = once_108_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_109_io_key = io_key[109]; // @[FSR_640.scala 33:26]
  assign once_110_io_state = once_109_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_110_io_key = io_key[110]; // @[FSR_640.scala 33:26]
  assign once_111_io_state = once_110_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_111_io_key = io_key[111]; // @[FSR_640.scala 33:26]
  assign once_112_io_state = once_111_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_112_io_key = io_key[112]; // @[FSR_640.scala 33:26]
  assign once_113_io_state = once_112_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_113_io_key = io_key[113]; // @[FSR_640.scala 33:26]
  assign once_114_io_state = once_113_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_114_io_key = io_key[114]; // @[FSR_640.scala 33:26]
  assign once_115_io_state = once_114_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_115_io_key = io_key[115]; // @[FSR_640.scala 33:26]
  assign once_116_io_state = once_115_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_116_io_key = io_key[116]; // @[FSR_640.scala 33:26]
  assign once_117_io_state = once_116_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_117_io_key = io_key[117]; // @[FSR_640.scala 33:26]
  assign once_118_io_state = once_117_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_118_io_key = io_key[118]; // @[FSR_640.scala 33:26]
  assign once_119_io_state = once_118_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_119_io_key = io_key[119]; // @[FSR_640.scala 33:26]
  assign once_120_io_state = once_119_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_120_io_key = io_key[120]; // @[FSR_640.scala 33:26]
  assign once_121_io_state = once_120_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_121_io_key = io_key[121]; // @[FSR_640.scala 33:26]
  assign once_122_io_state = once_121_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_122_io_key = io_key[122]; // @[FSR_640.scala 33:26]
  assign once_123_io_state = once_122_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_123_io_key = io_key[123]; // @[FSR_640.scala 33:26]
  assign once_124_io_state = once_123_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_124_io_key = io_key[124]; // @[FSR_640.scala 33:26]
  assign once_125_io_state = once_124_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_125_io_key = io_key[125]; // @[FSR_640.scala 33:26]
  assign once_126_io_state = once_125_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_126_io_key = io_key[126]; // @[FSR_640.scala 33:26]
  assign once_127_io_state = once_126_io_state_out; // @[FSR_640.scala 29:24 35:19]
  assign once_127_io_key = io_key[127]; // @[FSR_640.scala 33:26]
endmodule
module FSR_N_Reg(
  input          clock,
  input  [127:0] io_state,
  input  [127:0] io_key,
  input          io_start,
  output [127:0] io_state_out,
  output         io_done
);
`ifdef RANDOMIZE_REG_INIT
  reg [127:0] _RAND_0;
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_REG_INIT
  wire [127:0] FSR_128_io_state; // @[FSR_640.scala 492:23]
  wire [127:0] FSR_128_io_key; // @[FSR_640.scala 492:23]
  wire [127:0] FSR_128_io_state_out; // @[FSR_640.scala 492:23]
  wire  edge_detect_clock; // @[FSR_640.scala 493:27]
  wire  edge_detect_io_in; // @[FSR_640.scala 493:27]
  wire  edge_detect_io_out_tick; // @[FSR_640.scala 493:27]
  reg [127:0] temp_state; // @[FSR_640.scala 488:23]
  reg [9:0] temp_count; // @[FSR_640.scala 490:23]
  wire [9:0] _temp_count_T_1 = temp_count - 10'h1; // @[FSR_640.scala 513:30]
  wire  _T_2 = temp_count == 10'h0; // @[FSR_640.scala 515:25]
  wire [127:0] _GEN_2 = temp_count > 10'h0 ? FSR_128_io_state_out : temp_state; // @[FSR_640.scala 498:18 510:32 512:20]
  wire  _GEN_4 = temp_count > 10'h0 ? 1'h0 : _T_2; // @[FSR_640.scala 503:11 510:32]
  FSR_640 FSR_128 ( // @[FSR_640.scala 492:23]
    .io_state(FSR_128_io_state),
    .io_key(FSR_128_io_key),
    .io_state_out(FSR_128_io_state_out)
  );
  tick edge_detect ( // @[FSR_640.scala 493:27]
    .clock(edge_detect_clock),
    .io_in(edge_detect_io_in),
    .io_out_tick(edge_detect_io_out_tick)
  );
  assign io_state_out = edge_detect_io_out_tick ? temp_state : _GEN_2; // @[FSR_640.scala 504:41 507:20]
  assign io_done = edge_detect_io_out_tick ? 1'h0 : _GEN_4; // @[FSR_640.scala 504:41 505:13]
  assign FSR_128_io_state = temp_state; // @[FSR_640.scala 497:20]
  assign FSR_128_io_key = io_key; // @[FSR_640.scala 496:18]
  assign edge_detect_clock = clock;
  assign edge_detect_io_in = io_start; // @[FSR_640.scala 494:21]
  always @(posedge clock) begin
    if (edge_detect_io_out_tick) begin // @[FSR_640.scala 504:41]
      temp_state <= io_state; // @[FSR_640.scala 506:16]
    end else if (temp_count > 10'h0) begin // @[FSR_640.scala 510:32]
      if (!(edge_detect_io_out_tick)) begin // @[FSR_640.scala 504:41]
        if (temp_count > 10'h0) begin // @[FSR_640.scala 510:32]
          temp_state <= FSR_128_io_state_out; // @[FSR_640.scala 512:20]
        end
      end
    end
    if (edge_detect_io_out_tick) begin // @[FSR_640.scala 504:41]
      temp_count <= 10'h5; // @[FSR_640.scala 508:16]
    end else if (temp_count > 10'h0) begin // @[FSR_640.scala 510:32]
      temp_count <= _temp_count_T_1; // @[FSR_640.scala 513:16]
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {4{`RANDOM}};
  temp_state = _RAND_0[127:0];
  _RAND_1 = {1{`RANDOM}};
  temp_count = _RAND_1[9:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module initialization_tinyJAMBU_once(
  input          clock,
  input          reset,
  input  [127:0] io_key,
  input  [127:0] io_state,
  input          io_start,
  input  [31:0]  io_nonce,
  output [127:0] io_single_initialization_out,
  output         io_done
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  wire [2:0] single_frame_bit_io_state; // @[FSR_640.scala 124:32]
  wire [2:0] single_frame_bit_io_state_out; // @[FSR_640.scala 124:32]
  wire  fsr_sig_edge_clock; // @[FSR_640.scala 141:28]
  wire  fsr_sig_edge_io_in; // @[FSR_640.scala 141:28]
  wire  fsr_sig_edge_io_out_tick; // @[FSR_640.scala 141:28]
  wire  inst_FSR_clock; // @[FSR_640.scala 146:24]
  wire [127:0] inst_FSR_io_state; // @[FSR_640.scala 146:24]
  wire [127:0] inst_FSR_io_key; // @[FSR_640.scala 146:24]
  wire  inst_FSR_io_start; // @[FSR_640.scala 146:24]
  wire [127:0] inst_FSR_io_state_out; // @[FSR_640.scala 146:24]
  wire  inst_FSR_io_done; // @[FSR_640.scala 146:24]
  wire  init_once_edge_clock; // @[FSR_640.scala 153:30]
  wire  init_once_edge_io_in; // @[FSR_640.scala 153:30]
  wire  init_once_edge_io_out_tick; // @[FSR_640.scala 153:30]
  wire [2:0] output_of_3_bit_frame = single_frame_bit_io_state_out; // @[FSR_640.scala 125:35 128:25]
  wire [91:0] _output_of_whole_frame_T_2 = {io_state[127:39],output_of_3_bit_frame}; // @[FSR_640.scala 130:43]
  reg  start_FSR; // @[FSR_640.scala 139:22]
  wire  start_init = init_once_edge_io_out_tick; // @[FSR_640.scala 154:24 156:14]
  wire  _T_1 = inst_FSR_io_done; // @[FSR_640.scala 167:31]
  wire [127:0] state_out = inst_FSR_io_state_out; // @[FSR_640.scala 118:23 148:13]
  wire [31:0] _temp_nonce_state_T_2 = state_out[127:96] ^ io_nonce; // @[FSR_640.scala 173:44]
  wire [31:0] _GEN_1 = inst_FSR_io_done ? _temp_nonce_state_T_2 : 32'h0; // @[FSR_640.scala 160:20 167:40 173:22]
  wire [31:0] temp_nonce_state = start_init ? 32'h0 : _GEN_1; // @[FSR_640.scala 160:20 164:28]
  wire [127:0] _io_single_initialization_out_T_2 = {temp_nonce_state,state_out[95:0]}; // @[FSR_640.scala 176:61]
  wire [127:0] _GEN_2 = inst_FSR_io_done ? _io_single_initialization_out_T_2 : 128'h0; // @[FSR_640.scala 159:32 167:40 176:34]
  frame_bit_init single_frame_bit ( // @[FSR_640.scala 124:32]
    .io_state(single_frame_bit_io_state),
    .io_state_out(single_frame_bit_io_state_out)
  );
  tick fsr_sig_edge ( // @[FSR_640.scala 141:28]
    .clock(fsr_sig_edge_clock),
    .io_in(fsr_sig_edge_io_in),
    .io_out_tick(fsr_sig_edge_io_out_tick)
  );
  FSR_N_Reg inst_FSR ( // @[FSR_640.scala 146:24]
    .clock(inst_FSR_clock),
    .io_state(inst_FSR_io_state),
    .io_key(inst_FSR_io_key),
    .io_start(inst_FSR_io_start),
    .io_state_out(inst_FSR_io_state_out),
    .io_done(inst_FSR_io_done)
  );
  tick init_once_edge ( // @[FSR_640.scala 153:30]
    .clock(init_once_edge_clock),
    .io_in(init_once_edge_io_in),
    .io_out_tick(init_once_edge_io_out_tick)
  );
  assign io_single_initialization_out = start_init ? 128'h0 : _GEN_2; // @[FSR_640.scala 164:28 159:32]
  assign io_done = start_init ? 1'h0 : _T_1; // @[FSR_640.scala 164:28 166:13]
  assign single_frame_bit_io_state = io_state[38:36]; // @[FSR_640.scala 127:37]
  assign fsr_sig_edge_clock = clock;
  assign fsr_sig_edge_io_in = start_FSR; // @[FSR_640.scala 143:22]
  assign inst_FSR_clock = clock;
  assign inst_FSR_io_state = {_output_of_whole_frame_T_2,io_state[35:0]}; // @[FSR_640.scala 133:5]
  assign inst_FSR_io_key = io_key; // @[FSR_640.scala 150:19]
  assign inst_FSR_io_start = fsr_sig_edge_io_out_tick; // @[FSR_640.scala 142:23 144:13]
  assign init_once_edge_clock = clock;
  assign init_once_edge_io_in = io_start; // @[FSR_640.scala 155:24]
  always @(posedge clock) begin
    start_FSR <= init_once_edge_io_out_tick; // @[FSR_640.scala 154:24 156:14]
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  start_FSR = _RAND_0[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
