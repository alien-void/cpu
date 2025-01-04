`include "lib/defines.vh"
module MEM(
    input wire clk,
    input wire rst,
    
    input wire [`StallBus-1:0] stall,

    input wire [`EX_TO_MEM_WD-1:0] ex_to_mem_bus,

    
    output wire [`MEM_TO_ID_WD-1:0] mem_to_id_bus,   //+

    
    input wire [`LOAD_SRAM_DATA_WD-1:0] load_sram_data, //+
    input wire [`STORE_SRAM_DATA_WD-1:0] store_sram_data, //+

    input wire [31:0] data_sram_rdata,

    output wire [`MEM_TO_WB_WD-1:0] mem_to_wb_bus
);

    reg [`EX_TO_MEM_WD-1:0] ex_to_mem_bus_r;

    always @ (posedge clk) begin
        if (rst) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        end
        // else if (flush) begin
        //     ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        // end
        else if (stall[3]==`Stop && stall[4]==`NoStop) begin
            ex_to_mem_bus_r <= `EX_TO_MEM_WD'b0;
        end
        else if (stall[3]==`NoStop) begin
            ex_to_mem_bus_r <= ex_to_mem_bus;
        end
    end

    wire [31:0] mem_pc;
    wire data_ram_en;
    wire [3:0] data_ram_wen;
    wire sel_rf_res;
    wire rf_we;
    wire [4:0] rf_waddr;
    wire [31:0] rf_wdata;
    wire [31:0] ex_result;
    wire [31:0] mem_result;

    assign {
        mem_pc,         // 75:44
        data_ram_en,    // 43
        data_ram_wen,   // 42:39
        sel_rf_res,     // 38         
        rf_we,          // 37
        rf_waddr,       // 36:32
        ex_result       // 31:0
    } =  ex_to_mem_bus_r;


    

    reg [`LOAD_SRAM_DATA_WD-1:0] load_sram_data_r;   //+
    reg [`STORE_SRAM_DATA_WD-1:0] store_sram_data_r;   //+  

    always @ (posedge clk) begin
        if (rst) begin
            load_sram_data_r <= `LOAD_SRAM_DATA_WD'b0;      //如果复位（rst）信号为高，则将这两个寄存器的值清零。
            store_sram_data_r <= `STORE_SRAM_DATA_WD'b0;
        end
      
        else if (stall[2]==`Stop && stall[3]==`NoStop) begin    //如果 stall[2] 为 Stop 且 stall[3] 为 NoStop，表示暂停当前阶段的操作，将寄存器值清零。
            load_sram_data_r <= `LOAD_SRAM_DATA_WD'b0;
            store_sram_data_r <= `STORE_SRAM_DATA_WD'b0;
        end
        else if (stall[2]==`NoStop) begin                 //如果流水线没有暂停（stall[2] == NoStop），就将来自外部的数据（load_sram_data和store_sram_data`）传递到寄存器中。
            load_sram_data_r <= load_sram_data;
            store_sram_data_r <= store_sram_data;
        end
    end

    wire inst_sb, inst_sh, inst_sw;
    wire inst_lb, inst_lh, inst_lw, inst_lbu, inst_lhu;

    assign {
        inst_sb,
        inst_sh,
        inst_sw
    } = store_sram_data_r;

    assign {
        inst_lb,
        inst_lh,
        inst_lw,
        inst_lbu,
        inst_lhu
    } = load_sram_data_r;

    wire [7:0]  b_data;
    wire [15:0] h_data;
    wire [31:0] w_data;

    assign b_data = data_ram_wen[3] ? data_sram_rdata[31:24] : 
                    data_ram_wen[2] ? data_sram_rdata[23:16] :
                    data_ram_wen[1] ? data_sram_rdata[15: 8] : 
                    data_ram_wen[0] ? data_sram_rdata[ 7: 0] : 8'b0;
    assign h_data = data_ram_wen[2] ? data_sram_rdata[31:16] :
                    data_ram_wen[0] ? data_sram_rdata[15: 0] : 16'b0;
    assign w_data = data_sram_rdata;

    assign mem_result = inst_lb ? {{24{b_data[7]}},b_data} :
                        inst_lbu ? {{24{1'b0}},b_data} :
                        inst_lh ? {{16{h_data[15]}},h_data} :
                        inst_lhu ? {{16{1'b0}},h_data} :
                        inst_lw ? w_data : 32'b0; 

    assign mem_result = data_ram_en ? data_sram_rdata : 32'b0;

   

    assign rf_wdata = sel_rf_res ? mem_result : ex_result;

    assign mem_to_wb_bus = {
        mem_pc,     // 69:38
        rf_we,      // 37
        rf_waddr,   // 36:32
        rf_wdata    // 31:0
    };

    
    assign mem_to_id_bus = {
        rf_we,     
        rf_waddr,   
        rf_wdata  
    };



endmodule