`include "lib/defines.vh"



module IF(
    // 如果没有驱动元件连接到 wire 型变量，缺省值一般为 "Z"，即高阻态
    input wire clk,
    input wire rst,
    input wire [`StallBus-1:0] stall,

    // input wire flush,
    // input wire [31:0] new_pc,

    
    input wire [`BR_WD-1:0] br_bus, 
    output wire [`IF_TO_ID_WD-1:0] if_to_id_bus, // 33 位

   
    */
    output wire inst_sram_en,            // 存储器使能信号， 1 表示被使用
    output wire [3:0] inst_sram_wen,     // 指示是否写入数据以及写入哪些字节，一共四位，每一位表示存储器中的一个字节
    output wire [31:0] inst_sram_addr,   // 在取指阶段，将PC值通过此信号传给指令存储器获取指令
    output wire [31:0] inst_sram_wdata   // 写的内容
);
    reg [31:0] pc_reg; 
    reg ce_reg; // 该值为 1 时，允许取指
    wire [31:0] next_pc;
    wire br_e;
    wire [31:0] br_addr;

    assign {
        br_e,
        br_addr
    } = br_bus;


    always @ (posedge clk) begin
        if (rst) begin
            pc_reg <= 32'hbfbf_fffc;
        end
        else if (stall[0]==`NoStop) begin
            pc_reg <= next_pc;
        end
    end

    always @ (posedge clk) begin
        if (rst) begin // 是否复位
            ce_reg <= 1'b0;
        end
        else if (stall[0]==`NoStop) begin
            ce_reg <= 1'b1;
        end
    end
    // always @(posedge clk) begin
    //     $display("time : %0t, brbus = %h, if_to_id_bus = %h", $time, br_bus, if_to_id_bus);
    // end

    assign next_pc = br_e ? br_addr 
                   : pc_reg + 32'h4;

    
    assign inst_sram_en = ce_reg;
    assign inst_sram_wen = 4'b0;
    assign inst_sram_addr = pc_reg;
    assign inst_sram_wdata = 32'b0;
    assign if_to_id_bus = {
        ce_reg, // 1
        pc_reg  // 32
    };

endmodule