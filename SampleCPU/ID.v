`include "lib/defines.vh"
module ID(
    input wire clk,      // 时钟信号
    input wire rst,      //复位信号
    // input wire flush,
    input wire [`StallBus-1:0] stall,    //控制信号，用于控制流水线停顿
    
    output wire stallreq,    //控制是否需要停顿

    input wire [`IF_TO_ID_WD-1:0] if_to_id_bus,    //来自指令获取阶段（IF 阶段）的信号，包括指令和 PC 值等

    input wire [31:0] inst_sram_rdata,     //从指令存储器读取的 32 位指令数据

    input wire [`WB_TO_RF_WD-1:0] wb_to_rf_bus,//来自写回阶段（WB 阶段）的信号

    output wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,    //解码阶段传递给执行阶段的数据总线

    output wire [`BR_WD-1:0] br_bus    //分支信息
);

    reg [`IF_TO_ID_WD-1:0] if_to_id_bus_r;    //一个寄存器，用于保存来自 IF 阶段的指令和 PC 等信息
    wire [31:0] inst;  //当前指令（从 inst_sram_rdata 中读取）
    wire [31:0] id_pc;   //当前指令的程序计数器值（PC）
    wire ce;   //来自 if_to_id_bus_r 的一个控制信号
 
    wire wb_rf_we;     //写回使能
    wire [4:0] wb_rf_waddr;//写回寄存器地址
    wire [31:0] wb_rf_wdata;   //写回数据

    always @ (posedge clk) begin
        if (rst) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;        
        end
        // else if (flush) begin
        //     ic_to_id_bus <= `IC_TO_ID_WD'b0;
        // end
        else if (stall[1]==`Stop && stall[2]==`NoStop) begin
            if_to_id_bus_r <= `IF_TO_ID_WD'b0;
        end
        else if (stall[1]==`NoStop) begin
            if_to_id_bus_r <= if_to_id_bus;
        end
    end
    
    assign inst = inst_sram_rdata;
    assign {
        ce,
        id_pc
    } = if_to_id_bus_r;
    assign {
        wb_rf_we,
        wb_rf_waddr,
        wb_rf_wdata
    } = wb_to_rf_bus;

    wire [5:0] opcode;  //操作码
    wire [4:0] rs,rt,rd,sa;//源寄存器,目标寄存器,移位量,函数码
    wire [5:0] func;
    wire [15:0] imm;   //立即数字段
    wire [25:0] instr_index;   //指令索引
    wire [19:0] code;
    wire [4:0] base;
    wire [15:0] offset;
    wire [2:0] sel;   //用于选择一些小范围的字段，例如立即数中的低位。

    wire [63:0] op_d, func_d;
    wire [31:0] rs_d, rt_d, rd_d, sa_d;

    wire [2:0] sel_alu_src1;  //控制 ALU 的输入来源，选择来自寄存器、立即数或其他来源的数据。
    wire [3:0] sel_alu_src2;
    wire [11:0] alu_op;//定义了 ALU 操作的类型，指示当前操作是加法、减法、与、或等。

    wire data_ram_en;
    wire [3:0] data_ram_wen;
    
    wire rf_we;    //控制寄存器文件是否写入
    wire [4:0] rf_waddr;   //控制写回寄存器的地址
    wire sel_rf_res;
    wire [2:0] sel_rf_dst;   //控制将结果写入哪个寄存器

    wire [31:0] rdata1, rdata2;

    regfile u_regfile(       //实现了两个寄存器的读取和一个寄存器的写入，支持同步操作，并且通过控制信号（we）来控制是否执行写操作。
    	.clk    (clk    ),
        .raddr1 (rs ),
        .rdata1 (rdata1 ),
        .raddr2 (rt ),
        .rdata2 (rdata2 ),
        .we     (wb_rf_we     ),
        .waddr  (wb_rf_waddr  ),
        .wdata  (wb_rf_wdata  )
    );

    assign opcode = inst[31:26];    //将 32 位指令（inst）中的不同部分提取出来，分配到对应的信号变量中。
    assign rs = inst[25:21];
    assign rt = inst[20:16];
    assign rd = inst[15:11];
    assign sa = inst[10:6];
    assign func = inst[5:0];
    assign imm = inst[15:0];
    assign instr_index = inst[25:0];
    assign code = inst[25:6];
    assign base = inst[25:21];
    assign offset = inst[15:0];
    assign sel = inst[2:0];

    wire inst_ori, inst_lui, inst_addiu, inst_beq;  //逻辑 OR 指令,加载上半字指令,加法立即数指令,等于跳转指令

    wire op_add, op_sub, op_slt, op_sltu;
    wire op_and, op_nor, op_or, op_xor;
    wire op_sll, op_srl, op_sra, op_lui;

    decoder_6_64 u0_decoder_6_64(//解码器
    	.in  (opcode  ),
        .out (op_d )
    );

    decoder_6_64 u1_decoder_6_64(
    	.in  (func  ),
        .out (func_d )
    );
    
    decoder_5_32 u0_decoder_5_32(
    	.in  (rs  ),
        .out (rs_d )
    );

    decoder_5_32 u1_decoder_5_32(
    	.in  (rt  ),
        .out (rt_d )
    );

    
    assign inst_ori     = op_d[6'b00_1101];     //如果 op_d 的前 6 位（即操作码）等于 6'b00_1101，那么 inst_ori 就会被赋值为 1，表示当前指令是 ORI 指令。
    assign inst_lui     = op_d[6'b00_1111];
    assign inst_addiu   = op_d[6'b00_1001];
    assign inst_beq     = op_d[6'b00_0100];



    // rs to reg1        //控制ALU（算术逻辑单元）的操作数选择
    assign sel_alu_src1[0] = inst_ori | inst_addiu;

    // pc to reg1
    assign sel_alu_src1[1] = 1'b0;

    // sa_zero_extend to reg1
    assign sel_alu_src1[2] = 1'b0;

    
    // rt to reg2
    assign sel_alu_src2[0] = 1'b0;
    
    // imm_sign_extend to reg2
    assign sel_alu_src2[1] = inst_lui | inst_addiu;

    // 32'b8 to reg2
    assign sel_alu_src2[2] = 1'b0;

    // imm_zero_extend to reg2
    assign sel_alu_src2[3] = inst_ori;



    assign op_add = inst_addiu;
    assign op_sub = 1'b0;
    assign op_slt = 1'b0;
    assign op_sltu = 1'b0;
    assign op_and = 1'b0;
    assign op_nor = 1'b0;
    assign op_or = inst_ori;
    assign op_xor = 1'b0;
    assign op_sll = 1'b0;
    assign op_srl = 1'b0;
    assign op_sra = 1'b0;
    assign op_lui = inst_lui;

    assign alu_op = {op_add, op_sub, op_slt, op_sltu,
                     op_and, op_nor, op_or, op_xor,
                     op_sll, op_srl, op_sra, op_lui};



    // load and store enable
    assign data_ram_en = 1'b0;

    // write enable
    assign data_ram_wen = 1'b0;



    // regfile store enable
    assign rf_we = inst_ori | inst_lui | inst_addiu;



    // store in [rd]
    assign sel_rf_dst[0] = 1'b0;
    // store in [rt] 
    assign sel_rf_dst[1] = inst_ori | inst_lui | inst_addiu;   //选择 rt 寄存器作为目标寄存器地址
    // store in [31]
    assign sel_rf_dst[2] = 1'b0;

    // sel for regfile address
    assign rf_waddr = {5{sel_rf_dst[0]}} & rd 
                    | {5{sel_rf_dst[1]}} & rt
                    | {5{sel_rf_dst[2]}} & 32'd31;

    // 0 from alu_res ; 1 from ld_res
    assign sel_rf_res = 1'b0;     //从 ALU 还是从内存读取数据

    assign id_to_ex_bus = {   //总线,从 ID 阶段传递到 EX 阶段的数据
        id_pc,          // 158:127
        inst,           // 126:95
        alu_op,         // 94:83
        sel_alu_src1,   // 82:80
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        rdata1,         // 63:32
        rdata2          // 31:0
    };


    wire br_e;    //分支使能信号，用于指示是否执行分支
    wire [31:0] br_addr;    //如果是分支，计算分支目标地址
    wire rs_eq_rt;
    wire rs_ge_z;
    wire rs_gt_z;
    wire rs_le_z;
    wire rs_lt_z;
    wire [31:0] pc_plus_4;
    assign pc_plus_4 = id_pc + 32'h4;

    assign rs_eq_rt = (rdata1 == rdata2);

    assign br_e = inst_beq & rs_eq_rt;
    assign br_addr = inst_beq ? (pc_plus_4 + {{14{inst[15]}},inst[15:0],2'b0}) : 32'b0;

    assign br_bus = {    总线传递分支相关的信息
        br_e,
        br_addr
    };
    


endmodule