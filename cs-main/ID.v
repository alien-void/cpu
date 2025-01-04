`include "lib/defines.vh"
module ID(
    input wire clk,   //时钟信号
    input wire rst,   //重置信号
    // input wire flush,
    input wire [`StallBus-1:0] stall,    //用于控制流水线暂停的信号
    
    output wire stallreq,                   

    input wire [`IF_TO_ID_WD-1:0] if_to_id_bus,    //从 IF 阶段传递到 ID 阶段的数据总线

    input wire [31:0] inst_sram_rdata,     // 从 SRAM 读取的指令数据

    input wire [`WB_TO_RF_WD-1:0] wb_to_rf_bus,    //从 WB 阶段传递到寄存器文件的数据

   //处理数据相关
    input wire [`EX_TO_ID_WD-1:0] ex_to_id_bus,    //+
    input wire [`MEM_TO_ID_WD-1:0] mem_to_id_bus,  //+
   

    
    // 增加处理内存连线
    output wire [`LOAD_SRAM_DATA_WD-1:0] load_sram_data,   //用于内存加载和存储的数据总线+
    output wire [`STORE_SRAM_DATA_WD-1:0] store_sram_data,//+
   
    output wire [`ID_TO_EX_WD-1:0] id_to_ex_bus,  //从 ID 阶段传递到 EX 阶段的数据总线。

    output wire [`BR_WD-1:0] br_bus   // 用于分支的控制信号。
);

    // 接收 IF 段的数据
    reg [`IF_TO_ID_WD-1:0] if_to_id_bus_r;    //一个寄存器，用于保存来自 IF 阶段的指令和 PC 等信息
    wire [31:0] inst;  //当前指令（从 inst_sram_rdata 中读取）
    wire [31:0] id_pc;   //当前指令的程序计数器值（PC）
    wire ce;   //来自 if_to_id_bus_r 的一个控制信号
 

    wire wb_rf_we;     //写回使能
    wire [4:0] wb_rf_waddr;//写回寄存器地址
    wire [31:0] wb_rf_wdata;   //写回数据

   
    wire ex_rf_we;     //+
    wire [4:0] ex_rf_waddr;  //+
    wire [31:0] ex_rf_wdata; //+

    wire mem_rf_we;     //+
    wire [4:0] mem_rf_waddr;  //+
    wire [31:0] mem_rf_wdata;  //+

    wire [31:0] tdata1, tdata2; // 临时数据

    assign {              //+
        ex_rf_we,
        ex_rf_waddr,
        ex_rf_wdata
    } = ex_to_id_bus;

    assign {              //+
        mem_rf_we,
        mem_rf_waddr,
        mem_rf_wdata
    } = mem_to_id_bus;

    

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
    // rf register file
    assign {
        wb_rf_we,
        wb_rf_waddr,
        wb_rf_wdata
    } = wb_to_rf_bus; // 38 位

    wire [5:0] opcode;
    wire [4:0] rs,rt,rd,sa; 
    // 功能码
    wire [5:0] func; 
    wire [15:0] imm;
    wire [25:0] instr_index;
    wire [19:0] code;
    wire [4:0] base;
    wire [15:0] offset; 
    wire [2:0] sel; // 选择信号，用于多路复用器选择输入

    wire [63:0] op_d, func_d;
    wire [31:0] rs_d, rt_d, rd_d, sa_d;

    wire [2:0] sel_alu_src1;
    wire [3:0] sel_alu_src2;
    wire [11:0] alu_op;

    wire data_ram_en;
    wire [3:0] data_ram_wen;
    
    wire rf_we;
    wire [4:0] rf_waddr;
    wire sel_rf_res;
    wire [2:0] sel_rf_dst;

    wire [31:0] rdata1, rdata2;

    
    assign tdata1 = ((ex_rf_we && rs == ex_rf_waddr) ? ex_rf_wdata : 32'b0) | ((mem_rf_we && rs == mem_rf_waddr) ? mem_rf_wdata : 32'b0) |((wb_rf_we && rs == wb_rf_waddr) ? wb_rf_wdata : 32'b0) |(((ex_rf_we && rs == ex_rf_waddr) || (mem_rf_we && rs == mem_rf_waddr) || (wb_rf_we && rs == wb_rf_waddr)) ? 32'b0 : rdata1);
    assign tdata2 = ((ex_rf_we && rt == ex_rf_waddr) ? ex_rf_wdata : 32'b0) | ((mem_rf_we && rt == mem_rf_waddr) ? mem_rf_wdata : 32'b0) |((wb_rf_we && rt == wb_rf_waddr) ? wb_rf_wdata : 32'b0) | (((ex_rf_we && rt == ex_rf_waddr) || (mem_rf_we && rt == mem_rf_waddr) || (wb_rf_we && rt == wb_rf_waddr)) ? 32'b0 : rdata2);  //判断是否数据冲突（地址是否相等）

    
    regfile u_regfile(
    	.clk    (clk    ), 
        .raddr1 (rs ),
        .rdata1 (rdata1 ),
        .raddr2 (rt ),        
        .rdata2 (rdata2 ),          
        .we     (wb_rf_we     ),
        .waddr  (wb_rf_waddr  ),
        .wdata  (wb_rf_wdata  )
    );
    

   

    assign opcode = inst[31:26];
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



    // 添加运算指令+
    // 算数
    wire inst_add, inst_addi, inst_addiu, inst_addu, inst_sub, inst_subu;
    wire inst_slt, inst_slti, inst_sltiu, inst_sltu, inst_mul, inst_mult, inst_multu;
    
    // 逻辑
    wire inst_and, inst_nor, inst_ori, inst_xor, inst_andi, inst_or, inst_xori, inst_lui;
    
   
   
    wire inst_sll, inst_srl, inst_sra, inst_sllv, inst_srlv, inst_srav;
    
   
    wire inst_mfhi, inst_mflo, inst_mthi, inst_mtlo;

    
        // 跳转指令
    wire inst_jr, inst_jalr, inst_j, inst_jal;
        // 分支指令
    wire inst_b, inst_bal, inst_beq, inst_bgez, inst_bgezal;
    wire inst_bgtz, inst_blez, inst_bltz, inst_bltzal, inst_bne;
        // 跳转指令
    
    // 加载指令
    wire inst_lb, inst_lbu, inst_lh, inst_lhu, inst_lw, inst_lwl, inst_lwr;
    // 存储指令+
    wire inst_sb, inst_sh, inst_sw, inst_swl, inst_swr;


   
    wire op_add, op_sub, op_slt, op_sltu;
    wire op_and, op_nor, op_or, op_xor;
    wire op_sll, op_srl, op_sra, op_lui;

    decoder_6_64 u0_decoder_6_64(
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

    // 添加运算指令
    // 算术运算指令+
    assign inst_add     = op_d[6'b00_0000] & func_d[6'b10_0000];    //操作码，功能码
    assign inst_addu    = op_d[6'b00_0000] & func_d[6'b10_0001];
    assign inst_addiu   = op_d[6'b00_1001];
    assign inst_sub     = op_d[6'b00_0000] & func_d[6'b10_0010];
    assign inst_subu    = op_d[6'b00_0000] & func_d[6'b10_0011];
    assign inst_slt     = op_d[6'b00_0000] & func_d[6'b10_1010]; 
    assign inst_sltu    = op_d[6'b00_0000] & func_d[6'b10_1011];
    assign inst_addi    = op_d[6'b00_1000];
    assign inst_slti    = op_d[6'b00_1010];
    assign inst_sltiu   = op_d[6'b00_1011];
   
    assign inst_mul     = op_d[6'b01_1100] & func_d[6'b00_0010];
    assign inst_mult    = op_d[6'b00_0000] & func_d[6'b01_1000];
    assign inst_multu   = op_d[6'b00_0000] & func_d[6'b01_1001];

    // 逻辑运算指令
    assign inst_and     = op_d[6'b00_0000] & func_d[6'b10_0100];
    assign inst_nor     = op_d[6'b00_0000] & func_d[6'b10_0111];
    assign inst_xor     = op_d[6'b00_0000] & func_d[6'b10_0110];
    assign inst_ori     = op_d[6'b00_1101];
    assign inst_or      = op_d[6'b00_0000] & func_d[6'b10_0101];
    assign inst_andi    = op_d[6'b00_1100];
    assign inst_xori    = op_d[6'b00_1110];
    assign inst_lui     = op_d[6'b00_1111];

    // 逻辑移动指令
    assign inst_sll     = op_d[6'b00_0000] & func_d[6'b00_0000];
    assign inst_srl     = op_d[6'b00_0000] & func_d[6'b00_0010];
    assign inst_sra     = op_d[6'b00_0000] & func_d[6'b00_0011];
    assign inst_sllv    = op_d[6'b00_0000] & func_d[6'b00_0100];
    assign inst_srlv    = op_d[6'b00_0000] & func_d[6'b00_0110];
    assign inst_srav    = op_d[6'b00_0000] & func_d[6'b00_0111];

    // 移动操作指令
    assign inst_mfhi    = op_d[6'b00_0000] & func_d[6'b01_0000];
    assign inst_mflo    = op_d[6'b00_0000] & func_d[6'b01_0010];
    assign inst_mthi    = op_d[6'b00_0000] & func_d[6'b01_0001];
    assign inst_mtlo    = op_d[6'b00_0000] & func_d[6'b01_0011];
    
    // 跳转指令
    assign inst_jr      = op_d[6'b00_0000] & func_d[6'b00_1000];
    assign inst_jalr    = op_d[6'b00_0000] & func_d[6'b00_1001];
    assign inst_j       = op_d[6'b00_0100];
    assign inst_jal     = op_d[6'b00_0011];

    // 分支指令
    // assign inst_b       = op_d[6'b00_0100];
    assign inst_beq     = op_d[6'b00_0100];
    assign inst_bne     = op_d[6'b00_0101];

    // 加载指令
    assign inst_lb      = op_d[6'b10_0000];
    assign inst_lbu     = op_d[6'b10_0100];
    assign inst_lh      = op_d[6'b10_0001];
    assign inst_lhu     = op_d[6'b10_0101];
    assign inst_lw      = op_d[6'b10_0011];
    assign inst_lwl     = op_d[6'b10_0010];
    assign inst_lwr     = op_d[6'b10_0110];

    // 存储指令+
    assign inst_sb      = op_d[6'b10_1000];
    assign inst_sh      = op_d[6'b10_1001];
    assign inst_sw      = op_d[6'b10_1011];
    assign inst_swl     = op_d[6'b10_1010];
    assign inst_swr     = op_d[6'b10_1110];

    
    // 内存数据传送+
    assign load_sram_data = {
        inst_lb,
        inst_lh,
        inst_lw,
        inst_lbu,
        inst_lhu
    };

    assign store_sram_data = {
        inst_sb,
        inst_sh,
        inst_sw
        
    };
    
   
    assign sel_alu_src1[0] = inst_ori   | inst_addiu | inst_and   //+
                            | inst_or   | inst_xor   | inst_nor
                            | inst_andi | inst_xori  | inst_sllv
                            | inst_srlv | inst_srav  | inst_add
                            | inst_addu | inst_sub   | inst_subu
                            | inst_slt   | inst_sltu | inst_addi
                            | inst_addiu | inst_slti | inst_sltiu
                            | inst_mul  | inst_mult  | inst_multu
                            | inst_mthi | inst_mtlo  | inst_jr
                            | inst_jalr | inst_lb | inst_lbu
                            | inst_lh | inst_lhu | inst_lw
                            | inst_sb | inst_sh | inst_sw
                            ;


    assign sel_alu_src1[1] = inst_jal;    //+


    assign sel_alu_src1[2] = inst_sll | inst_srl | inst_sra ;  //+
                           


    assign sel_alu_src2[0] = inst_and | inst_or | inst_xor     //+
                            | inst_nor | inst_sll | inst_srl
                            | inst_sra | inst_sllv | inst_srlv
                            | inst_srav | inst_add | inst_addu 
                            | inst_sub   | inst_subu | inst_slt
                            | inst_sltu | inst_addiu
                            | inst_slti | inst_sltiu | inst_mul
                            | inst_mult | inst_multu
                            ;
    
    
    assign sel_alu_src2[1] = inst_lui | inst_addiu | inst_addi   //+
                            | inst_slti | inst_sltiu
                            | inst_lb | inst_lbu | inst_lh
                            | inst_lhu | inst_lw | inst_sw;

    
    assign sel_alu_src2[2] = inst_jal;   //+

    
    assign sel_alu_src2[3] = inst_ori | inst_andi | inst_xori;   //+

    


    // 添加运算指令
    assign op_add = inst_addiu | inst_add | inst_addu | inst_addi | inst_jal | inst_lw | inst_sw;
    assign op_sub = inst_sub | inst_subu;
    assign op_slt = inst_slt | inst_slti;
    assign op_sltu = inst_sltu | inst_sltiu;
    assign op_and = inst_and | inst_andi;
    assign op_nor = inst_nor;
    assign op_or  = inst_ori | inst_or;
    assign op_xor = inst_xor | inst_xori;
    assign op_sll = inst_sll | inst_sllv;
    assign op_srl = inst_srl | inst_srlv;
    assign op_sra = inst_sra | inst_srav;   
    assign op_lui = inst_lui;

    assign alu_op = {op_add, op_sub, op_slt, op_sltu,
                     op_and, op_nor, op_or, op_xor,
                     op_sll, op_srl, op_sra, op_lui};



 
    assign data_ram_en = inst_lb | inst_lbu | inst_lh | inst_lhu | inst_lw
                          | inst_sb | inst_sh | inst_sw;

   
    assign data_ram_wen = inst_sw ? 4'b1111 : 4'b0000;
    
    assign rf_we = inst_ori | inst_lui | inst_addiu
                    | inst_add | inst_addu | inst_sub
                    | inst_subu | inst_slt | inst_sltu
                    | inst_addi | inst_slti | inst_sltiu
                    | inst_mul | inst_mult | inst_multu
                    | inst_and | inst_nor | inst_or
                    | inst_xor | inst_sll | inst_srl
                    | inst_sra | inst_sllv | inst_srlv
                    | inst_srav | inst_mfhi
                    | inst_mflo | inst_jalr | inst_jal
                    | inst_lb | inst_lbu | inst_lh
                    | inst_lhu | inst_lw;  
                    


    // 寄存器选择
    // store in [rd]
    assign sel_rf_dst[0] = inst_add | inst_addu | inst_sub
                        | inst_subu | inst_slt | inst_sltu
                        | inst_mul | inst_and | inst_or
                        | inst_xor | inst_nor | inst_sll
                        | inst_srl | inst_sra | inst_sllv
                        | inst_srlv | inst_srav /*| inst_nop*/
                        | inst_mfhi | inst_mflo | inst_jalr;
    // store in [rt] 
    assign sel_rf_dst[1] = inst_ori | inst_lui | inst_addiu
                        | inst_andi | inst_slti | inst_sltiu
                        | inst_andi | inst_xori | inst_lb
                        | inst_lbu | inst_lh | inst_lhu
                        | inst_lw | inst_addi;
    // store in [31]
    assign sel_rf_dst[2] = inst_jal;

    // sel for regfile address
    assign rf_waddr = {5{sel_rf_dst[0]}} & rd 
                    | {5{sel_rf_dst[1]}} & rt
                    | {5{sel_rf_dst[2]}} & 32'd31;

    // 0 from alu_res ; 1 from ld_res
    assign sel_rf_res = inst_lw;
                    


    // 将data数据进行修改，防止出现X态
    assign id_to_ex_bus = {
        id_pc,          // 158:127
        inst,           // 126:95
        alu_op,         // 94:83
        sel_alu_src1,   // 82:80 ALU Source 1 Selector
        sel_alu_src2,   // 79:76
        data_ram_en,    // 75
        data_ram_wen,   // 74:71
        rf_we,          // 70
        rf_waddr,       // 69:65
        sel_rf_res,     // 64
        tdata1,         // 63:32  
        tdata2          // 31:0
    };



    wire br_e;              // 分支使能信号，表示是否执行分支
    wire [31:0] br_addr;    // 分支跳转的目标地址
    wire rs_eq_rt;          
    wire rs_ge_z;           
    wire rs_gt_z;           
    wire rs_le_z;           
    wire rs_lt_z;           
    wire [31:0] pc_plus_4; 


    assign pc_plus_4 = id_pc + 32'h4;

    assign rs_eq_rt = (tdata1 == tdata2);

    assign br_e = (inst_beq & rs_eq_rt) |
              inst_jr |
              inst_j |
              inst_jal |
              (inst_bne & ~rs_eq_rt);
    assign br_addr = inst_beq ? (pc_plus_4 + {{14{inst[15]}}, offset, 2'b0}) :
                 inst_jr  ? tdata1 :
                 inst_jal ? {pc_plus_4[31:28], instr_index, 2'b0} :
                 inst_bne ? (pc_plus_4 + {{14{inst[15]}}, offset, 2'b0}) : 
                 32'b0;


    assign br_bus = {     //总线传递分支相关的信息
        br_e,
        br_addr
    };
    


endmodule