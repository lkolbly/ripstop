`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 10/12/2015 02:52:16 PM
// Design Name: 
// Module Name: sw_led
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////

module logistic_map(
    input clk,
    input rst,

    // x must be held until out_valid is true
    input[63:0] x,
    // x_valid must be deasserted after a single cycle
    input x_valid,

    output[63:0] out,
    output out_valid
);

    // The logistic map is:
    // x[n] = r * x[n-1] * (1 - x[n-1])
    // We will choose r = 3.58

    wire one_minus_x_out_tvalid, one_minus_x_out_tready;
    wire[63:0] one_minus_x_out_tdata;

    floating_point_sub one_minus_x (
        .aclk(clk),
        .aresetn(~rst),

        .s_axis_a_tvalid(1),
        .s_axis_a_tready(),
        .s_axis_a_tdata(64'h3FF0000000000000), // 1 in binary

        .s_axis_b_tvalid(x_valid),
        .s_axis_b_tready(),
        .s_axis_b_tdata(x),

        .m_axis_result_tvalid(one_minus_x_out_tvalid),
        .m_axis_result_tready(one_minus_x_out_tready),
        .m_axis_result_tdata(one_minus_x_out_tdata)
    );

    wire times_x_out_tvalid, times_x_out_tready;
    wire[63:0] times_x_out_tdata;

    floating_point_mult times_x (
        .aclk(clk),
        .aresetn(~rst),

        .s_axis_a_tvalid(one_minus_x_out_tvalid),
        .s_axis_a_tready(),
        .s_axis_a_tdata(x),

        .s_axis_b_tvalid(one_minus_x_out_tvalid),
        .s_axis_b_tready(one_minus_x_out_tready),
        .s_axis_b_tdata(one_minus_x_out_tdata),

        .m_axis_result_tvalid(times_x_out_tvalid),
        .m_axis_result_tready(times_x_out_tready),
        .m_axis_result_tdata(times_x_out_tdata)
    );

    floating_point_mult times_r (
        .aclk(clk),
        .aresetn(~rst),

        .s_axis_a_tvalid(1),
        .s_axis_a_tready(),
        .s_axis_a_tdata(64'h400CA3D70A3D70A4), // 3.58 in binary

        .s_axis_b_tvalid(times_x_out_tvalid),
        .s_axis_b_tready(times_x_out_tready),
        .s_axis_b_tdata(times_x_out_tdata),

        .m_axis_result_tvalid(out_valid),
        .m_axis_result_tready(1),
        .m_axis_result_tdata(out)
    );

endmodule


module logistic_map_runner(
    input clk,
    input rst,
    output[63:0] result,
    output[31:0] fxp_result
);

    reg[5:0] x_starting_cnt_q, x_starting_cnt_d;
    reg x_valid_q, x_valid_d;
    reg[63:0] x_q, x_d;

    wire lm_result_valid;
    wire[63:0] lm_result;

    logistic_map lm(
        .clk(clk),
        .rst(rst),
        .x(x_q),
        .x_valid(x_valid_q),
        .out(lm_result),
        .out_valid(lm_result_valid)
    );

    reg[31:0] fxp_result_q, fxp_result_d;
    wire[31:0] fxp_result_raw;

    floating_point_to_fxp fxp_converter (
        .aclk(clk),
        .aresetn(~rst),

        .s_axis_a_tvalid(lm_result_valid),
        .s_axis_a_tready(),
        .s_axis_a_tdata(lm_result),

        .m_axis_result_tvalid(fxp_result_valid),
        .m_axis_result_tready(1),
        .m_axis_result_tdata(fxp_result_raw)
    );

    assign result = x_q;
    assign fxp_result = fxp_result_q;

    always @(*) begin
        if (lm_result_valid) begin
            x_valid_d <= 1;
            x_d <= lm_result;
            x_starting_cnt_d <= x_starting_cnt_q;
        end else if (x_starting_cnt_q == 1) begin
            x_valid_d <= 1;
            x_d <= x_q;
            x_starting_cnt_d <= 0;
        end else if (x_starting_cnt_q > 1) begin
            x_valid_d <= 0;
            x_d <= x_q;
            x_starting_cnt_d <= x_starting_cnt_q - 1;
        end else begin
            x_valid_d <= 0;
            x_d <= x_q;
            x_starting_cnt_d <= x_starting_cnt_q;
        end

        if (fxp_result_valid) begin
            fxp_result_d <= fxp_result_raw;
        end else begin
            fxp_result_d <= fxp_result_q;
        end
    end

    always @(posedge clk) begin
        if (rst) begin
            x_valid_q <= 0;
            x_q <= 64'h3FE0000000000000;
            x_starting_cnt_q <= 20; // Wait 20 cycles for FPUs to come up from reset
                                    // (yes I know this is massive overkill)
            fxp_result_q <= 0;
        end else begin
            x_valid_q <= x_valid_d;
            x_q <= x_d;
            x_starting_cnt_q <= x_starting_cnt_d;
            fxp_result_q <= fxp_result_d;
        end
    end

endmodule


module pwm(
    input clk,
    input rst,

    input[15:0] duty_cycle,
    output signal
);

    reg[15:0] counter_q, counter_d;

    assign signal = counter_q < duty_cycle;

    always @(*) begin
        counter_d = counter_q + 1;
    end

    always @(posedge clk) begin
        counter_q <= counter_d;
    end

endmodule


// This module stolen from https://github.com/cseed/fpga-playground/blob/master/arty/ddr3/main.v
module ddr_tester(
	    output [13:0]    ddr3_sdram_addr,
	    output [2:0]     ddr3_sdram_ba,
	    output 	     ddr3_sdram_cas_n,
	    output [0:0]     ddr3_sdram_ck_n,
	    output [0:0]     ddr3_sdram_ck_p,
	    output [0:0]     ddr3_sdram_cke,
	    output [0:0]     ddr3_sdram_cs_n,
	    output [1:0]     ddr3_sdram_dm,
	    inout [15:0]     ddr3_sdram_dq,
	    inout [1:0]      ddr3_sdram_dqs_n,
	    inout [1:0]      ddr3_sdram_dqs_p,
	    output [0:0]     ddr3_sdram_odt,
	    output 	     ddr3_sdram_ras_n,
	    output 	     ddr3_sdram_reset_n,
	    output 	     ddr3_sdram_we_n,
	    input 	     sys_resetn,
	    input 	     sys_clock,
        input clk_out1,
        input clk_out2,
	    output reg [3:0] led);

   // user interface signals
   wire 		  ui_clk;
   wire 		  ui_clk_sync_rst;
   wire 		  mmcm_locked;
   wire 		  app_sr_active;
   wire 		  app_ref_ack;
   wire 		  app_zq_ack;
   // Slave Interface Write Address Ports
   wire [27:0] 		  s_axi_awaddr = 28'b0;
   wire [7:0] 		  s_axi_awlen = 8'b0; // 1
   wire [2:0] 		  s_axi_awsize = 3'b010; // 4 bytes
   wire [1:0] 		  s_axi_awburst = 2'b0; // fixed
   reg 			  s_axi_awvalid;
   wire 		  s_axi_awready;
   // Slave Interface Write Data Ports
   reg [31:0] 		  s_axi_wdata = 32'hcafe_beef;
   reg [3:0] 		  s_axi_wstrb = 4'b1111;
   reg 			  s_axi_wlast;
   reg 			  s_axi_wvalid;
   wire 		  s_axi_wready;
   // Slave Interface Write Response Ports
   reg 			  s_axi_bready;
   wire [3:0] 		  s_axi_bid;
   wire [1:0] 		  s_axi_bresp;
   wire 		  s_axi_bvalid;
   // Slave Interface Read Address Ports
   wire [27:0] 		  s_axi_araddr = 28'b0;
   wire [7:0] 		  s_axi_arlen = 8'b0; // 1
   wire [2:0] 		  s_axi_arsize = 3'b010; // 4 bytes
   wire [1:0] 		  s_axi_arburst = 2'b0; // fixed
   reg 			  s_axi_arvalid;
   wire 		  s_axi_arready;
   // Slave Interface Read Data Ports
   reg 			  s_axi_rready;
   wire [3:0] 		  s_axi_rid;
   wire [31:0] 		  s_axi_rdata;
   wire [1:0] 		  s_axi_rresp;
   wire 		  s_axi_rlast;
   wire 		  s_axi_rvalid;
   

   mig_7series_0 mig_inst(
			  .ddr3_dq(ddr3_sdram_dq),
			  .ddr3_dqs_n(ddr3_sdram_dqs_n),
			  .ddr3_dqs_p(ddr3_sdram_dqs_p),
			  .ddr3_addr(ddr3_sdram_addr),
			  .ddr3_ba(ddr3_sdram_ba),
			  .ddr3_ras_n(ddr3_sdram_ras_n),
			  .ddr3_cas_n(ddr3_sdram_cas_n),
			  .ddr3_we_n(ddr3_sdram_we_n),
			  .ddr3_reset_n(ddr3_sdram_reset_n),
			  .ddr3_ck_p(ddr3_sdram_ck_p),
			  .ddr3_ck_n(ddr3_sdram_ck_n),
			  .ddr3_cke(ddr3_sdram_cke),
			  .ddr3_cs_n(ddr3_sdram_cs_n),
			  .ddr3_dm(ddr3_sdram_dm),
			  .ddr3_odt(ddr3_sdram_odt),
              .init_calib_complete(),
			  .sys_clk_i(clk_out1),
			  .clk_ref_i(clk_out2),
			  // User Interface
			  .ui_clk(ui_clk),
			  .ui_clk_sync_rst(ui_clk_sync_rst),
			  .mmcm_locked(mmcm_locked),
			  .aresetn(1), // FIXME
			  .app_sr_req(0),
			  .app_ref_req(0),
			  .app_zq_req(0),
			  .app_sr_active(app_sr_active),
			  .app_ref_ack(app_ref_ack),
			  .app_zq_ack(app_zq_ack),
			  // Slave Interface Write Address Ports
			  .s_axi_awid(4'b0),
			  .s_axi_awaddr(s_axi_awaddr),
			  .s_axi_awlen(s_axi_awlen),
			  .s_axi_awsize(s_axi_awsize),
			  .s_axi_awburst(s_axi_awburst),
			  .s_axi_awlock(1'b0), // normal access
			  .s_axi_awcache(4'b0011),
			  .s_axi_awprot(3'b0),
			  .s_axi_awqos(4'b0),
			  .s_axi_awvalid(s_axi_awvalid),
			  .s_axi_awready(s_axi_awready),
			  // Slave Interface Write Data Ports
			  .s_axi_wdata(s_axi_wdata),
			  .s_axi_wstrb(s_axi_wstrb),
			  .s_axi_wlast(s_axi_wlast),
			  .s_axi_wvalid(s_axi_wvalid),
			  .s_axi_wready(s_axi_wready),
			  // Slave Interface Write Response Ports
			  .s_axi_bready(s_axi_bready),
			  .s_axi_bid(s_axi_bid),
			  .s_axi_bresp(s_axi_bresp),
			  .s_axi_bvalid(s_axi_bvalid),
			  // Slave Interface Read Address Ports
			  .s_axi_arid(4'b0),
			  .s_axi_araddr(s_axi_araddr),
			  .s_axi_arlen(s_axi_arlen),
			  .s_axi_arsize(s_axi_arsize),
			  .s_axi_arburst(s_axi_arburst),
			  .s_axi_arlock(1'b0),
			  .s_axi_arcache(4'b0011),
			  .s_axi_arprot(3'b0),
			  .s_axi_arqos(4'b0),
			  .s_axi_arvalid(s_axi_arvalid),
			  .s_axi_arready(s_axi_arready),
			  // Slave Interface Read Data Ports
			  .s_axi_rready(s_axi_rready),
			  .s_axi_rid(s_axi_rid),
			  .s_axi_rdata(s_axi_rdata),
			  .s_axi_rresp(s_axi_rresp),
			  .s_axi_rlast(s_axi_rlast),
			  .s_axi_rvalid(s_axi_rvalid),
			  
			  .sys_rst(sys_resetn));

   wire 		  clk = ui_clk;
   wire 		  clk_resetn = !ui_clk_sync_rst;

   reg [31:0] 		  state;
   
   always @(posedge clk) begin
      if (!clk_resetn) begin
	 s_axi_awvalid <= 0;
	 s_axi_wlast <= 0;
	 s_axi_wvalid <= 0;
	 s_axi_bready <= 0;
	 s_axi_arvalid <= 0;
	 s_axi_rready <= 0;
	 state <= 0;
	 led <= 4'b0;
      end else begin
	 case (state)
	   0: begin
	      s_axi_awvalid <= 1;
	      state <= 1;
	   end
	   
	   1: begin
	      if (s_axi_awready) begin
		 s_axi_awvalid <= 0;
		 state <= 2;
	      end
	   end
	   
	   2: begin
	      s_axi_wlast <= 1;
	      s_axi_wvalid <= 1;
	      state <= 3;
	   end
	   
	   3: begin
	      if (s_axi_wready) begin
		 s_axi_wvalid <= 0;
		 state <= 4;
	      end
	   end
	   
	   4: begin
	      if (s_axi_bvalid) begin
		 s_axi_bready <= 1;

		 if (s_axi_bresp == 2'b00)
		   state <= 5;
		 else
		   state <= 100;
	      end
	   end
	   
	   5: begin
	      s_axi_bready <= 0;
	      
	      led[0] <= 1; // write finished
	      state <= 6;
	   end
	   
	   6: begin
	      s_axi_arvalid <= 1;
	      state <= 7;
	   end

	   7: begin
	      if (s_axi_arready) begin
		 s_axi_arvalid <= 0;
		 state <= 8;
	      end
	   end

	   8: begin
	      if (s_axi_rvalid) begin
		 s_axi_rready <= 1;

		 if (s_axi_rresp == 2'b00)
		   state <= 9;
		 else
		   state <= 100;
	      end
	   end

	   9: begin
	      s_axi_rready <= 0;
	      
	      led[1] <= 1; // read finished
	      
	      if (s_axi_rdata == 32'hcafe_beef)
		led[3] <= 1;
	      
	      state <= 100;
	   end
	   
	   100: begin
	      s_axi_bready <= 0;
	      s_axi_rready <= 0;
	   end
	   
	 endcase
      end
   end
   
endmodule



module sw_btn_led(
    input sys_clock,
    input [3:0]sw,
    input [3:0]btn,
    output [3:0]led,

    input uart_txd_in,
    output uart_rxd_out,

    //Inouts
    inout  [15:0] ddr3_dq,
    inout  [1:0]  ddr3_dqs_n,
    inout  [1:0]  ddr3_dqs_p,
    // Outputs
    output [13:0] ddr3_addr,
    output [2:0]  ddr3_ba,
    output ddr3_ras_n,
    output ddr3_cas_n,
    output ddr3_we_n,
    output ddr3_reset_n,
    output [0:0] ddr3_ck_p,
    output [0:0] ddr3_ck_n,
    output [0:0] ddr3_cke,
    output [0:0] ddr3_cs_n,
    output [1:0] ddr3_dm,
    output [0:0] ddr3_odt
);

    wire clk;
    wire rst;

    //assign clk = sys_clock;
    assign rst = btn[0];

       wire 		  locked;
       wire clk_out1, clk_out2;
   clk_wiz_0 clk_wiz_inst(
			  .clk_out1(clk_out1),
			  .clk_out2(clk_out2),
              .clk_out3(clk),
			  .resetn(~rst),
			  .locked(locked),
			  .clk_in1(sys_clock));

    //assign led = sw | btn;

    reg[31:0] duty_cycle_q, duty_cycle_d;
    reg[31:0] duty_cycle_target_q, duty_cycle_target_d;
    reg[28:0] target_update_cnt_q, target_update_cnt_d;
    wire[31:0] fxp_result;

    logistic_map_runner(
        .clk(clk),
        .rst(rst),
        .result(),
        .fxp_result(fxp_result)
    );

    pwm led_pwm(
        .clk(clk),
        .rst(rst),
        .duty_cycle(duty_cycle_q[31:16]),
        .signal()//led[0])
    );

    pwm led1_pwm(
        .clk(clk),
        .rst(rst),
        .duty_cycle(16'h0fff),
        .signal()//led[1])
    );

    pwm led2_pwm(
        .clk(clk),
        .rst(rst),
        .duty_cycle(16'h1fff),
        .signal()//led[2])
    );

    pwm led3_pwm(
        .clk(clk),
        .rst(rst),
        .duty_cycle(16'h3fff),
        .signal()//led[3])
    );

  /*mcu_subsystem mcu_subsystem_i
       (.BRAM_PORTB_0_addr(0),
        .BRAM_PORTB_0_clk(clk),
        .BRAM_PORTB_0_din(0),
        .BRAM_PORTB_0_dout(),
        .BRAM_PORTB_0_en(0),
        .BRAM_PORTB_0_rst(rst),
        .BRAM_PORTB_0_we(0),

        .DEBUG_0_capture(0),
        .DEBUG_0_clk(clk),
        .DEBUG_0_disable(1),
        .DEBUG_0_reg_en(0),
        .DEBUG_0_rst(rst),
        .DEBUG_0_shift(0),
        .DEBUG_0_tdi(0),
        .DEBUG_0_tdo(),
        .DEBUG_0_update(0),

        .INTERRUPT_0_ack(),
        .INTERRUPT_0_address(0),
        .INTERRUPT_0_interrupt(0),

        .M01_AXI_0_araddr(M01_AXI_0_araddr),
        .M01_AXI_0_arburst(M01_AXI_0_arburst),
        .M01_AXI_0_arcache(M01_AXI_0_arcache),
        .M01_AXI_0_arlen(M01_AXI_0_arlen),
        .M01_AXI_0_arlock(M01_AXI_0_arlock),
        .M01_AXI_0_arprot(M01_AXI_0_arprot),
        .M01_AXI_0_arqos(M01_AXI_0_arqos),
        .M01_AXI_0_arready(M01_AXI_0_arready),
        .M01_AXI_0_arregion(M01_AXI_0_arregion),
        .M01_AXI_0_arsize(M01_AXI_0_arsize),
        .M01_AXI_0_arvalid(M01_AXI_0_arvalid),

        .M01_AXI_0_awaddr(M01_AXI_0_awaddr),
        .M01_AXI_0_awburst(M01_AXI_0_awburst),
        .M01_AXI_0_awcache(M01_AXI_0_awcache),
        .M01_AXI_0_awlen(M01_AXI_0_awlen),
        .M01_AXI_0_awlock(M01_AXI_0_awlock),
        .M01_AXI_0_awprot(M01_AXI_0_awprot),
        .M01_AXI_0_awqos(M01_AXI_0_awqos),
        .M01_AXI_0_awready(M01_AXI_0_awready),
        .M01_AXI_0_awregion(M01_AXI_0_awregion),
        .M01_AXI_0_awsize(M01_AXI_0_awsize),
        .M01_AXI_0_awvalid(M01_AXI_0_awvalid),

        .M01_AXI_0_bready(M01_AXI_0_bready),
        .M01_AXI_0_bresp(M01_AXI_0_bresp),
        .M01_AXI_0_bvalid(M01_AXI_0_bvalid),

        .M01_AXI_0_rdata(M01_AXI_0_rdata),
        .M01_AXI_0_rlast(M01_AXI_0_rlast),
        .M01_AXI_0_rready(M01_AXI_0_rready),
        .M01_AXI_0_rresp(M01_AXI_0_rresp),
        .M01_AXI_0_rvalid(M01_AXI_0_rvalid),

        .M01_AXI_0_wdata(M01_AXI_0_wdata),
        .M01_AXI_0_wlast(M01_AXI_0_wlast),
        .M01_AXI_0_wready(M01_AXI_0_wready),
        .M01_AXI_0_wstrb(M01_AXI_0_wstrb),
        .M01_AXI_0_wvalid(M01_AXI_0_wvalid),

        .UART_0_rxd(uart_txd_in),
        .UART_0_txd(uart_rxd_out),

        .clk(clk),
        .interrupt_0(),
        .rst(rst),
        .rstn(~rst));*/

/*
module ddr_tester(
	    output [13:0]    ddr3_sdram_addr,
	    output [2:0]     ddr3_sdram_ba,
	    output 	     ddr3_sdram_cas_n,
	    output [0:0]     ddr3_sdram_ck_n,
	    output [0:0]     ddr3_sdram_ck_p,
	    output [0:0]     ddr3_sdram_cke,
	    output [0:0]     ddr3_sdram_cs_n,
	    output [1:0]     ddr3_sdram_dm,
	    inout [15:0]     ddr3_sdram_dq,
	    inout [1:0]      ddr3_sdram_dqs_n,
	    inout [1:0]      ddr3_sdram_dqs_p,
	    output [0:0]     ddr3_sdram_odt,
	    output 	     ddr3_sdram_ras_n,
	    output 	     ddr3_sdram_reset_n,
	    output 	     ddr3_sdram_we_n,
	    input 	     sys_resetn,
	    input 	     sys_clock,
	    output reg [3:0] led);
*/

    ddr_tester ddr_tester(
        .ddr3_sdram_addr(ddr3_addr),
        .ddr3_sdram_ba(ddr3_ba),
        .ddr3_sdram_cas_n(ddr3_cas_n),
        .ddr3_sdram_ck_n(ddr3_ck_n),
        .ddr3_sdram_ck_p(ddr3_ck_p),
        .ddr3_sdram_cke(ddr3_cke),
        .ddr3_sdram_cs_n(ddr3_cs_n),
        .ddr3_sdram_dm(ddr3_dm),
        .ddr3_sdram_dq(ddr3_dq),
        .ddr3_sdram_dqs_n(ddr3_dqs_n),
        .ddr3_sdram_dqs_p(ddr3_dqs_p),
        .ddr3_sdram_odt(ddr3_odt),
        .ddr3_sdram_ras_n(ddr3_ras_n),
        .ddr3_sdram_reset_n(ddr3_reset_n),
        .ddr3_sdram_we_n(ddr3_we_n),
        .sys_resetn(~rst),
        .sys_clock(clk),
        .clk_out1(clk_out1),
        .clk_out2(clk_out2),
        .led(led)
    );

    /*mig_7series_0 u_mig_7series_0 (
        // Memory interface ports
        .ddr3_addr                      (ddr3_addr),
        .ddr3_ba                        (ddr3_ba),
        .ddr3_cas_n                     (ddr3_cas_n),
        .ddr3_ck_n                      (ddr3_ck_n),
        .ddr3_ck_p                      (ddr3_ck_p),
        .ddr3_cke                       (ddr3_cke),
        .ddr3_ras_n                     (ddr3_ras_n),
        .ddr3_reset_n                   (ddr3_reset_n),
        .ddr3_we_n                      (ddr3_we_n),
        .ddr3_dq                        (ddr3_dq),
        .ddr3_dqs_n                     (ddr3_dqs_n),
        .ddr3_dqs_p                     (ddr3_dqs_p),
        .ddr3_cs_n                      (ddr3_cs_n),
        .ddr3_dm                        (ddr3_dm),
        .ddr3_odt                       (ddr3_odt),

        .init_calib_complete            (),

        // Application interface ports
        .ui_clk                         (ui_clk),  // output			ui_clk
        .ui_clk_sync_rst                (ui_clk_sync_rst),  // output			ui_clk_sync_rst
        .mmcm_locked                    (mmcm_locked),  // output			mmcm_locked
        .app_sr_req                     (app_sr_req),  // input			app_sr_req
        .app_ref_req                    (app_ref_req),  // input			app_ref_req
        .app_zq_req                     (app_zq_req),  // input			app_zq_req
        .app_sr_active                  (app_sr_active),  // output			app_sr_active
        .app_ref_ack                    (app_ref_ack),  // output			app_ref_ack
        .app_zq_ack                     (app_zq_ack),  // output			app_zq_ack

        // Slave Interface Write Address Ports
        .s_axi_awid                     (s_axi_awid),  // input [3:0]			s_axi_awid
        .s_axi_awaddr                   (s_axi_awaddr),  // input [27:0]			s_axi_awaddr
        .s_axi_awlen                    (s_axi_awlen),  // input [7:0]			s_axi_awlen
        .s_axi_awsize                   (s_axi_awsize),  // input [2:0]			s_axi_awsize
        .s_axi_awburst                  (s_axi_awburst),  // input [1:0]			s_axi_awburst
        .s_axi_awlock                   (s_axi_awlock),  // input [0:0]			s_axi_awlock
        .s_axi_awcache                  (s_axi_awcache),  // input [3:0]			s_axi_awcache
        .s_axi_awprot                   (s_axi_awprot),  // input [2:0]			s_axi_awprot
        .s_axi_awqos                    (s_axi_awqos),  // input [3:0]			s_axi_awqos
        .s_axi_awvalid                  (s_axi_awvalid),  // input			s_axi_awvalid
        .s_axi_awready                  (s_axi_awready),  // output			s_axi_awready

        // Slave Interface Write Data Ports
        .s_axi_wdata                    (s_axi_wdata),  // input [127:0]			s_axi_wdata
        .s_axi_wstrb                    (s_axi_wstrb),  // input [15:0]			s_axi_wstrb
        .s_axi_wlast                    (s_axi_wlast),  // input			s_axi_wlast
        .s_axi_wvalid                   (s_axi_wvalid),  // input			s_axi_wvalid
        .s_axi_wready                   (s_axi_wready),  // output			s_axi_wready

        // Slave Interface Write Response Ports
        .s_axi_bid                      (s_axi_bid),  // output [3:0]			s_axi_bid
        .s_axi_bresp                    (s_axi_bresp),  // output [1:0]			s_axi_bresp
        .s_axi_bvalid                   (s_axi_bvalid),  // output			s_axi_bvalid
        .s_axi_bready                   (s_axi_bready),  // input			s_axi_bready

        // Slave Interface Read Address Ports
        .s_axi_arid                     (s_axi_arid),  // input [3:0]			s_axi_arid
        .s_axi_araddr                   (s_axi_araddr),  // input [27:0]			s_axi_araddr
        .s_axi_arlen                    (s_axi_arlen),  // input [7:0]			s_axi_arlen
        .s_axi_arsize                   (s_axi_arsize),  // input [2:0]			s_axi_arsize
        .s_axi_arburst                  (s_axi_arburst),  // input [1:0]			s_axi_arburst
        .s_axi_arlock                   (s_axi_arlock),  // input [0:0]			s_axi_arlock
        .s_axi_arcache                  (s_axi_arcache),  // input [3:0]			s_axi_arcache
        .s_axi_arprot                   (s_axi_arprot),  // input [2:0]			s_axi_arprot
        .s_axi_arqos                    (s_axi_arqos),  // input [3:0]			s_axi_arqos
        .s_axi_arvalid                  (s_axi_arvalid),  // input			s_axi_arvalid
        .s_axi_arready                  (s_axi_arready),  // output			s_axi_arready

        // Slave Interface Read Data Ports
        .s_axi_rid                      (s_axi_rid),  // output [3:0]			s_axi_rid
        .s_axi_rdata                    (s_axi_rdata),  // output [127:0]			s_axi_rdata
        .s_axi_rresp                    (s_axi_rresp),  // output [1:0]			s_axi_rresp
        .s_axi_rlast                    (s_axi_rlast),  // output			s_axi_rlast
        .s_axi_rvalid                   (s_axi_rvalid),  // output			s_axi_rvalid
        .s_axi_rready                   (s_axi_rready),  // input			s_axi_rready

        // System Clock Ports
        .sys_clk_i                       (clk),

        .aresetn                        (~rst),

        // Reference Clock Ports
        .clk_ref_i                      (clk),
        .sys_rst                        (rst)
    );*/

    always @(*) begin
        if (target_update_cnt_q == 0) begin
            duty_cycle_target_d = fxp_result;
        end else begin
            duty_cycle_target_d = duty_cycle_target_q;
        end

        target_update_cnt_d = target_update_cnt_q + 1;

        if (duty_cycle_q > duty_cycle_target_q) begin
            duty_cycle_d = duty_cycle_q - 1;
        end else begin
            duty_cycle_d = duty_cycle_q + 1;
        end
    end

    always @(posedge clk) begin
        if (rst) begin
            duty_cycle_q <= 0;
            duty_cycle_target_q <= 0;
            target_update_cnt_q <= 1;
        end else begin
            duty_cycle_q <= duty_cycle_d;
            duty_cycle_target_q <= duty_cycle_target_d;
            target_update_cnt_q <= target_update_cnt_d;
        end
    end

endmodule
